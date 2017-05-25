library(tidyverse)
library(stringr)
library(lubridate)
library(ggthemes)
library(scales)

read_func <- function(x){
    read_csv(x) %>% mutate(scrape_date = ymd(str_extract(x, '[0-9]{8}')))
}

price_func <- function(x,y){
    detail_price <- as.numeric(gsub(',', '', str_extract(x, '[0-9]+.*$')))
    listing_price <- as.numeric(gsub('[,|USD|\\s]', '', str_extract(y, 'USD\\s[[:graph:]]+\\s')))
    listing_price <- ifelse(is.na(listing_price), 0, listing_price)
    price <- ifelse(detail_price == 0 | is.na(detail_price), listing_price, detail_price)
}

csv_list <- list.files('data', full.names = TRUE, pattern = '\\.csv$')

raw_dat <- map_df(csv_list, read_func)

dat <- raw_dat %>%
    as_tibble(select(category_id, subcategory_id, subcategory2_id, listing_link-href, detail_name, 
                     detail_qty_sold, listing_price, detail_price, listing_vendor, scrape_date)) %>%
    mutate(qty_sold = str_extract(detail_qty_sold, '[0-9]{1,5} sold since.*20[0-9]{2}'),
           quantity = as.numeric(str_extract(qty_sold, '^[0-9]{1,5}')),
           date_str = str_extract(qty_sold, '[A-Z].*20[0-9]{2}'),
           listing_date = mdy(date_str),
           price = price_func(detail_price, listing_price),
           days_for_sale = as.numeric(difftime(scrape_date, listing_date, units = c("days"))+1),
           max_quantity = replace(quantity, quantity == 0, 1),
           sales_in_period = price*quantity,
           max_sales_in_period = price*max_quantity,
           avg_daily_sales = sales_in_period/days_for_sale,
           max_avg_daily_sales = max_sales_in_period/days_for_sale) %>%
    select(product_category = category_id, product_subcategory = subcategory_id, 
           product_name = subcategory2_id, product_listing = detail_name, listing_date, scrape_date, 
           quantity, max_quantity, price, days_for_sale, sales_in_period, max_sales_in_period, 
           avg_daily_sales, max_avg_daily_sales)


sum_dat <- dat %>%
    summarise(total_avg_daily_sales = sum(avg_daily_sales),
              total_annual_sales = total_avg_daily_sales*365,
              max_total_avg_daily_sales = sum(max_avg_daily_sales),
              max_total_annual_sales = max_total_avg_daily_sales*365)


p_cat_plot <- dat %>%
    group_by(product_category) %>%
    summarise(avg_annual_sales = sum((avg_daily_sales + max_avg_daily_sales)/2)*365) %>%
    ggplot(aes(x = reorder(product_category, avg_annual_sales), y = avg_annual_sales/1000000)) +
    geom_bar(stat = 'identity', fill = 'dodgerblue4', alpha = 0.85) +
    ggtitle("Annual sales by product category") +
    theme_tufte(base_family = 'Tahoma') +
    theme(panel.grid.major = element_line(size = 0.1, color = "grey"),
          axis.text.y = element_text(size = 11),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5, face = 'bold')) +
    scale_y_continuous(labels = dollar_format(suffix = 'M')) +
    coord_flip()

p_cat_plot


p_subcat_plot <- dat %>%
    filter(product_category == 'Drugs & Chemicals') %>%
    group_by(product_subcategory) %>%
    summarise(avg_annual_sales = sum((avg_daily_sales + max_avg_daily_sales)/2)*365) %>%
    ggplot(aes(x = reorder(product_subcategory, avg_annual_sales), y = avg_annual_sales/1000000)) +
    geom_bar(stat = 'identity', fill = 'dodgerblue4', alpha = 0.85) +
    ggtitle("Drugs & Chemicals - Annual sales by product subcategory") +
    theme_tufte(base_family = 'Tahoma') +
    theme(panel.grid.major = element_line(size = 0.1, color = "grey"),
          axis.text.y = element_text(size = 11),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5, face = 'bold')) +
    scale_y_continuous(labels = dollar_format(suffix = 'M')) +
    coord_flip()

p_subcat_plot


product_plot <- dat %>%
    filter(product_category == 'Drugs & Chemicals' & product_subcategory == 'Psychedelics') %>%
    group_by(product_name) %>%
    summarise(avg_annual_sales = sum((avg_daily_sales + max_avg_daily_sales)/2)*365) %>%
    ggplot(aes(x = reorder(product_name, avg_annual_sales), y = avg_annual_sales/1000000)) +
    geom_bar(stat = 'identity', fill = 'dodgerblue4', alpha = 0.85) +
    ggtitle("Psychedelics - Annual sales by product") +
    theme_tufte(base_family = 'Tahoma') +
    theme(panel.grid.major = element_line(size = 0.1, color = "grey"),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5, face = 'bold')) +
    scale_y_continuous(labels = dollar_format(suffix = 'M')) +
    coord_flip()

product_plot 


top10_plot <- dat %>%
    mutate(subcat_product = paste(product_subcategory, product_name, sep = " - ")) %>%
    group_by(subcat_product) %>%
    summarise(avg_annual_sales = sum((avg_daily_sales + max_avg_daily_sales)/2)*365) %>%
    top_n(n = 10, wt = avg_annual_sales) %>%
    ggplot(aes(x = reorder(subcat_product, avg_annual_sales), y = avg_annual_sales/1000000)) +
    geom_bar(stat = 'identity', fill = 'dodgerblue4', alpha = 0.85) +
    ggtitle("Top 10 products by annual sales") +
    theme_tufte(base_family = 'Tahoma') +
    theme(panel.grid.major = element_line(size = 0.1, color = "grey"),
          axis.text.y = element_text(size = 11),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          plot.title = element_text(hjust = 0.5, face = 'bold')) +
    scale_y_continuous(labels = dollar_format(suffix = 'M')) +
    coord_flip()

top10_plot



