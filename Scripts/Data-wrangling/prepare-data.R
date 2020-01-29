#-------------------------------------------------------------------------------------------------
# Script to prepare the datasets for modeling
#
#
#                                                                       Antonio C. da Silva Júnior 
#                                                                              juniorssz@gmail.com
#                                                           https://www.linkedin.com/in/juniorssz/
#                                                                    https://github.com/juniorssz/
#                                                                   https://juniorssz.netlify.com/
#                                                                2020-Jan-21 | Curitiba/PR - Brazil
#--------------------------------------------------------------------------------------------------
# Requirements:
source("Scripts/Data-wrangling/get-data-from-athena.R")
require(dplyr)
require(stringr)
require(stringi)
require(ggplot2)

#--------------------------------------------------------------------------------------------------
# Utilities:

## Function to check if IDs are unique:
is_unique_id <- function(var_id, df) {
  out <- length(unique(df[, var_id])) == nrow(df)
  return(out)
}

## Function to check the seller status is "ready" or "documentation"
is_ready_or_doc <- function() {
  out <- all(df_sellers$ss_status %in% c("ready", "documentation"))
  return(out)
}

## Function to count missing values
count_missing_values <- function(df) {
  df <- as.data.frame(df)
  out <- data.frame(var = names(df))
  out$n_missing <- NA
  out$var <- as.character(out$var)
  for (i in 1:nrow(out)) {
    var_name <- out[i,1]
    out$n_missing[i] <- length(df[is.na(df[var_name]),1])
  }
  out <- out %>%
    arrange(desc(n_missing))
  return(out)
}
#--------------------------------------------------------------------------------------------------
# Config and constants:
options(scipen=999)
DATE_OF_CUT <- Sys.Date()
START_DATE <- DATE_OF_CUT - 180
PERIOD_IN_DAYS <- as.numeric(DATE_OF_CUT - START_DATE)
MAIN_COLOUR = "#0C29D0"
SEC_COLOUR = "#0DC78B"

#--------------------------------------------------------------------------------------------------
# Preparing data about sellers:

df_sellers <- get_sellers_deals()

## Keeping only sellers with more than 180 days working
df_sellers <- df_sellers %>%
  filter(as.Date(ss_created_at) <= START_DATE)

## Transform all blank values to NA:
df_sellers <- df_sellers %>%
  mutate_all(na_if, "")

## Replacing NA to zero in numeric variables
df_sellers <- df_sellers %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))


### Checking status:
if(!is_ready_or_doc()) {
  print("ERROR: Check if the seller status is only ready or documentation")
}

### Splitting ss_about to create seller_stage variable:
df_sellers <- df_sellers %>%
  rowwise() %>%
  mutate(seller_stage = ifelse(!is.na(str_locate(ss_about, "-")[1]), 
                               str_sub(ss_about, 1, str_locate(ss_about, "-")[1]-1), 
                               NA)) %>%
  mutate(seller_stage = ifelse(is.na(seller_stage), ss_about, seller_stage))
df_sellers$seller_stage <- str_trim(df_sellers$seller_stage)
df_sellers$ss_about <- NULL


### Identifying the top 3 seller_stage:
top_3_seller_stages <- df_sellers %>%
  group_by(seller_stage) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

top_3_seller_stages <- top_3_seller_stages[(1:3),]
top_3_seller_stages <- top_3_seller_stages$seller_stage


### Simplifying seller_stages into 4 classes:
df_sellers <- df_sellers %>%
  mutate(seller_stage = ifelse(seller_stage %in% top_3_seller_stages, seller_stage, "Outro"))
rm(top_3_seller_stages)


### Transform the states to upper case:
df_sellers$sa_state <- str_to_upper(df_sellers$sa_state)
df_sellers$sa_state <- str_trim(df_sellers$sa_state)


### Creating the variable region
df_sellers <- df_sellers %>%
  mutate(region = ifelse(sa_state %in% c("RS", "SC", "PR"), "Sul", 
                         ifelse(sa_state %in% c("SP", "RJ", "MG", "ES"), "Sudeste", 
                                ifelse(sa_state %in% c("MS", "GO", "DF", "MT"), "Centro-oeste/DF", 
                                       ifelse(sa_state %in% c("BA", "SE", "PE", "AL", "PB", "RN", "MA", "PI", "CE"), "Nordeste", 
                                              ifelse(sa_state %in% c("AC", "AM", "RO", "RR", "AP", "PA", "TO"), "Norte", NA))))))


### Identifying the top 3 states:
top_3_states <- df_sellers %>%
  group_by(sa_state) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

top_3_states <- top_3_states[(1:3),]
top_3_states <- top_3_states$sa_state


### Simplifying states into 4 classes:
df_sellers <- df_sellers %>%
  mutate(sa_state = ifelse(sa_state %in% top_3_states, sa_state, "Outro"))
rm(top_3_states)


### Capitalize and remove accentuation from cities:
df_sellers$sa_city <- str_to_title(df_sellers$sa_city)
df_sellers$sa_city <- stri_trans_general(df_sellers$sa_city, "Latin-ASCII")
df_sellers$sa_city <- str_trim(df_sellers$sa_city)


### Identifying the top 3 cities:
top_3_cities <- df_sellers %>%
  group_by(sa_city) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

top_3_cities <- top_3_cities[(1:3),]
top_3_cities <- top_3_cities$sa_city


### Simplifying cities into 4 classes:
df_sellers <- df_sellers %>%
  mutate(sa_city = ifelse(sa_city %in% top_3_cities, sa_city, "Outra"))
rm(top_3_cities)


### Creating the variable total_shipping_days:
df_sellers <- df_sellers %>%
  mutate(total_shipping_days = sf_shipping_days + sf_extra_transit_time)

df_sellers$sf_shipping_days <- NULL
df_sellers$sf_extra_transit_time <- NULL

### Creating the variable is_churned:
df_sellers <- df_sellers %>%
  mutate(is_churned = ifelse(lc_current_type %in% c("Cancelamento"), 1, 0)) %>%
  mutate(is_churned = ifelse(ss_status %in% c("documentation"), 1, is_churned))

### Creating the variable cashflow_ranges:
df_sellers <- df_sellers %>%
  mutate(ds_cashflow = ifelse(ds_cashflow == 0 | is.na(ds_cashflow), "Desconhecido", ds_cashflow))

cashflow <- df_sellers %>%
  filter(ds_cashflow != "Desconhecido")
cashflow_median <- median(as.numeric(cashflow$ds_cashflow))
cashflow_quantile3 <- as.numeric(quantile(as.numeric(cashflow$ds_cashflow), 0.75))
cashflow_max <- max(as.numeric(cashflow$ds_cashflow))

cashflow$cashflow_ranges <- cut(as.numeric(cashflow$ds_cashflow), breaks = c(1, cashflow_median, cashflow_quantile3, cashflow_max))
df_sellers <- df_sellers %>%
  left_join((cashflow %>%
               select(ss_id, cashflow_ranges)), by = "ss_id")
df_sellers$cashflow_ranges <- as.character(df_sellers$cashflow_ranges)
df_sellers <- df_sellers %>%
  mutate(cashflow_ranges = ifelse(is.na(cashflow_ranges), ds_cashflow, cashflow_ranges))
rm(cashflow, cashflow_max, cashflow_median, cashflow_quantile3)


### Simplifying df_revenue into 4 classes:

df_sellers <- df_sellers %>%
  mutate(df_revenue = str_to_title(str_trim(df_revenue)))
range_1 <- c("De 500k A 3.6mi Ano (40 A 300k Mês)",
             "De 480k A 3.6mi Ano (40k A 300k Mês)",
             "De 500k A 3.6mi Ano (40 A 300k Mes)",
             "Entre R$500k Aa E R$3.600mi Aa (Entre R$40k Am E R$300kam)")
value_1 <- "De 500k a 3.6mi ano"

range_2 <- c("De 60 A 500k Ano (5 A 40k Mês)",
             "De 60k A 480k Ano (5k A 40k Mês)",
             "De 60 A 500k Ano (5 A 40k Mes)",
             "Entre R$ 60k Aa E R$500k Am (Entre R$5k E R$40k Am)")
value_2 <- "De 60k a 500k ano"

range_3 <- c("Até 60k Ano (5k Mês)",
             "Até R$ 60k Aa (Até R$ 5k Am)",
             "De 60 A 500k Ano (5 A 40k Mes)")
value_3 <- "Até 60k ano"

range_4 <- c("Acima De 3.6mi Ano (300k Mês)",
             "De 3.6mi A 8.4mi Ano (300k A 800k Mês)",
             "De 8.4mi A 72mi Ano (800k A 6mi Mês)",
             "Acima De 3.6mi Ano (300k Mes)",
             "Acima De R$3.600mi Aa (Acima De R$300k Am)",
             "Acima De 72mi Ano (6mi Mês)")
value_4 <- "Acima de 3.6mi ano"

all <- c(value_1, value_2, value_3, value_4)

df_sellers <- df_sellers %>%
  mutate(df_revenue = ifelse(df_revenue %in% range_1, value_1, df_revenue)) %>%
  mutate(df_revenue = ifelse(df_revenue %in% range_2, value_2, df_revenue)) %>%
  mutate(df_revenue = ifelse(df_revenue %in% range_3, value_3, df_revenue)) %>%
  mutate(df_revenue = ifelse(df_revenue %in% range_4, value_4, df_revenue)) %>%
  mutate(df_revenue = ifelse(df_revenue %in% all, df_revenue, "Desconhecido"))

rm(all, range_1, range_2, range_3, range_4, value_1, value_2, value_3, value_4)


### Identifying the top 5 behavior profile:

df_sellers$df_behavior_profile <- str_trim(df_sellers$df_behavior_profile)
df_sellers$df_behavior_profile <- str_to_title(df_sellers$df_behavior_profile)
df_sellers$df_behavior_profile <- stri_trans_general(df_sellers$df_behavior_profile, "Latin-ASCII")
df_sellers$df_behavior_profile <- str_replace_all(df_sellers$df_behavior_profile, " ", "")
df_sellers$df_behavior_profile <- str_replace_all(df_sellers$df_behavior_profile, ",", "/")
df_sellers <- df_sellers %>%
  mutate(df_behavior_profile = ifelse(df_behavior_profile %in% c("None", "", NA),
                                      "Outro",
                                      df_behavior_profile))

top_5_behavior <- df_sellers %>%
  group_by(df_behavior_profile) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

top_5_behavior <- top_5_behavior[(1:5),]
top_5_behavior <- top_5_behavior$df_behavior_profile

### Simplifying behavior profile into 5 classes:
df_sellers <- df_sellers %>%
  mutate(df_behavior_profile = ifelse(df_behavior_profile %in% top_5_behavior, df_behavior_profile, "Outro"))
rm(top_5_behavior)


### Checking id_persona:
df_sellers$df_id_persona <- str_trim(df_sellers$df_id_persona)
df_sellers$df_id_persona <- str_to_title(df_sellers$df_id_persona)
df_sellers <- df_sellers %>%
  mutate(df_id_persona = ifelse(df_id_persona %in% c("None", "", NA),
                                "Desconhecido",
                                df_id_persona))

### Creating the variable portfolio_reg_rate
df_sellers <- df_sellers %>%
  mutate(df_portfolio_cadastravel = as.numeric(df_portfolio_cadastravel)) %>%
  mutate(df_portfolio_cadastravel = ifelse(is.na(df_portfolio_cadastravel), 0, df_portfolio_cadastravel)) %>%
  mutate(sp_n_products = as.numeric(sp_n_products)) %>%
  mutate(sp_n_products = ifelse(is.na(sp_n_products), 0, sp_n_products)) %>%
  mutate(portfolio_reg_rate = ifelse(df_portfolio_cadastravel > 0,
                                     sp_n_products / df_portfolio_cadastravel,
                                     1))

## Replacing NA to zero in numeric variables
df_sellers <- df_sellers %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

count_missing_values(df_sellers)

#--------------------------------------------------------------------------------------------------
# Preparing data about orders:

df_orders <- get_orders()
df_orderitems <- get_order_items()

## Keeping only orders related to df_sellers:
df_orders <- df_orders %>%
  filter(so_seller_id %in% df_sellers$ss_id)

df_orderitems <- df_orderitems %>%
  filter(so_seller_id %in% df_sellers$ss_id)

## Transform all blank values to NA:
df_orders <- df_orders %>%
  mutate_all(na_if, "")

df_orderitems <- df_orderitems %>%
  mutate_all(na_if, "")


## Aggregating items by orders:
df_orderitems$oi_price <- as.numeric(df_orderitems$oi_price)
df_orderitems$oi_freight_value <- as.numeric(df_orderitems$oi_freight_value)

df_orders_group <- df_orderitems %>%
  group_by(oi_seller_order_id) %>%
  summarise(n_items = n(), 
            oi_price = sum(oi_price), 
            oi_freight_value = sum(oi_freight_value)) %>%
  arrange(desc(n_items))

## Keeping only orders related to df_orders_group
df_orders <- df_orders %>%
  filter(so_id %in% df_orders_group$oi_seller_order_id)

df_orders_group <- df_orders_group %>%
  filter(oi_seller_order_id %in% df_orders$so_id)

# Combining data
df_orders <- df_orders %>%
  left_join(df_orders_group, by = c("so_id" = "oi_seller_order_id"))

rm(df_orders_group)

## Creating the variable order_relative_day (considering start_date as the first day)
df_orders <- df_orders %>%
  mutate(order_relative_day = as.numeric(as.Date(so_purchase_timestamp) - as.Date(START_DATE)))

## Checking order status
df_orders %>%
  group_by(so_status) %>%
  summarise(n = n(),
            shipped = sum(ifelse(!is.na(so_delivered_carrier_date), 1, 0)),
            delivered = sum(ifelse(!is.na(so_delivered_customer_date), 1, 0))) %>%
  arrange(desc(n))

## Filling delivered_carrier_date
df_orders <- df_orders %>%
  mutate(so_delivered_carrier_date = ifelse((so_status == "delivered" | so_status == "shipped") & is.na(so_delivered_carrier_date),
                                            so_shipping_limit_date,
                                            so_delivered_carrier_date))

## Filling delivered_customer_date
df_orders <- df_orders %>%
  mutate(so_delivered_customer_date = ifelse(so_status == "delivered" & is.na(so_delivered_customer_date),
                                            so_estimated_delivery_date,
                                            so_delivered_customer_date))

## Replacing status
new_status <- function(d_carrier, d_custom) {
  if(is.na(d_custom)) {
    if(is.na(d_carrier)) {
      out <- "in progress"
    } else {
      out <- "shipped"
    }
  } else {
    out <-"delivered"
  }
  return(out)
}
df_orders <- df_orders %>%
  rowwise() %>%
  mutate(order_status = new_status(so_delivered_carrier_date, so_delivered_customer_date))

# # Checking so_status == shipped & order_status == "delivered"
# View(df_orders %>%
#   filter(so_status == "shipped", order_status == "delivered"))
# # to keep!

# ## Checking so_status == delivered & order_status == "shipped"
# View(df_orders %>%
#        filter(so_status == "delivered", order_status == "shipped"))
# # to remove!
df_orders <- df_orders %>%
  filter(!(so_status == "delivered" & order_status == "shipped"))

## Creating the variables order_in_period, order_in_firsthalf, order_in_lasthalf
df_orders <- df_orders %>%
  mutate(order_in_period = ifelse(order_relative_day %in% 1:PERIOD_IN_DAYS, 1, 0),
         order_in_firsthalf = ifelse(order_relative_day %in% 1:(PERIOD_IN_DAYS/2), 1, 0),
         order_in_lasthalf = ifelse(order_relative_day > (PERIOD_IN_DAYS/2), 1, 0))

## Calculating days late (shipping and delivery)
df_orders$so_shipping_limit_date <- as.Date(df_orders$so_shipping_limit_date)
df_orders$so_delivered_carrier_date <- as.Date(df_orders$so_delivered_carrier_date)
df_orders$so_estimated_delivery_date <- as.Date(df_orders$so_estimated_delivery_date)
df_orders$so_delivered_customer_date <- as.Date(df_orders$so_delivered_customer_date)

df_orders <- df_orders %>%
  mutate(shipping_late_days = as.numeric(so_delivered_carrier_date - so_shipping_limit_date),
         delivery_late_days = as.numeric(so_delivered_customer_date - so_estimated_delivery_date))

df_orders <- df_orders %>%
  mutate(shipping_late_days = ifelse(shipping_late_days < 0, 0, shipping_late_days),
         delivery_late_days = ifelse(delivery_late_days < 0, 0, delivery_late_days),
         shipping_late = ifelse(shipping_late_days > 0, 1, 0),
         delivery_late = ifelse(delivery_late_days > 0, 1, 0))


# ## Suspension and cancellation
# 
# View(df_orders %>%
#   filter(so_cancelation_status == "canceled") %>%
#   group_by(so_cancelation_reason) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n)))
# 
# View(df_orders %>%
#        filter(so_cancelation_status == "suspended") %>%
#        group_by(so_suspension_reason) %>%
#        summarise(n = n()) %>%
#        arrange(desc(n)))

# df_orders <- df_orders %>%
#   mutate(suspended = ifelse(so_cancelation_status %in% c("suspended"), 1, 0),
#          canceled = ifelse(so_cancelation_status %in% c("canceled"), 1, 0))


# write.csv(df_sellers, "Temp/df_sellers.csv", row.names = F)
# write.csv(df_orderitems, "Temp/df_orderitems.csv", row.names = F)
# write.csv(df_orders, "Temp/df_orders.csv", row.names = F)

# library(data.table)
# df_sellers <- fread("Temp/df_sellers.csv")
# df_orderitems <- fread("Temp/df_orderitems.csv")
# df_orders <- fread("Temp/df_orders.csv")

#--------------------------------------------------------------------------------------------------
# Preparing data about sellers - round 2:

## Grouping orders by seller
df_orders_sellers <- df_orders %>%
  group_by(so_seller_id) %>%
  summarise(first_order = min(as.Date(so_purchase_timestamp)),
            last_order = max(as.Date(so_purchase_timestamp)),
            total_orders = n(),
            total_items = sum(n_items),
            total_price = sum(oi_price),
            total_freight = sum(oi_freight_value),
            total_workeddays = n_distinct(as.Date(so_purchase_timestamp)),
            total_delivered = sum(ifelse(order_status == "delivered", 1, 0)),
            total_shipped = sum(ifelse(order_status == "shipped", 1, 0)),
            total_inprogress = sum(ifelse(order_status == "in progress", 1, 0)),
            total_shippedlate = sum(shipping_late),
            total_avgshippedlate_days = mean(shipping_late_days),
            total_deliverylate = sum(delivery_late),
            total_avgdeliverylate_days = mean(delivery_late_days),
            total_suspended = sum(ifelse(so_suspension_reason == "suspended", 1, 0)),
            total_canceled = sum(ifelse(so_suspension_reason == "canceled", 1, 0)),
            period_orders = sum(order_in_period),
            period_items = sum(ifelse(order_in_period == 1, n_items, 0)),
            period_price = sum(ifelse(order_in_period == 1, oi_price, 0)),
            period_freight = sum(ifelse(order_in_period == 1, oi_freight_value, 0)),
            period_workeddays = n_distinct(ifelse(order_in_period == 1, as.Date(so_purchase_timestamp), NA))-1,
            period_delivered = sum(ifelse(order_in_period == 1 & order_status == "delivered", 1, 0)),
            period_shipped = sum(ifelse(order_in_period == 1 & order_status == "shipped", 1, 0)),
            period_inprogress = sum(ifelse(order_in_period == 1 & order_status == "in progress", 1, 0)),
            period_shippedlate = sum(ifelse(order_in_period == 1, shipping_late, 0)),
            period_avgshippedlate_days = mean(ifelse(order_in_period == 1, shipping_late_days, 0)),
            period_deliverylate = sum(ifelse(order_in_period == 1, delivery_late, 0)),
            period_avgdeliverylate_days = mean(ifelse(order_in_period == 1, delivery_late_days, 0)),
            period_suspended = sum(ifelse(order_in_period == 1 & so_suspension_reason == "suspended", 1, 0)),
            period_canceled = sum(ifelse(order_in_period == 1 & so_suspension_reason == "canceled", 1, 0)),
            firsthalf_orders = sum(order_in_firsthalf),
            firsthalf_items = sum(ifelse(order_in_firsthalf == 1, n_items, 0)),
            firsthalf_price = sum(ifelse(order_in_firsthalf == 1, oi_price, 0)),
            firsthalf_freight = sum(ifelse(order_in_firsthalf == 1, oi_freight_value, 0)),
            firsthalf_workeddays = n_distinct(ifelse(order_in_firsthalf == 1, as.Date(so_purchase_timestamp), 0))-1,
            firsthalf_delivered = sum(ifelse(order_in_firsthalf == 1 & order_status == "delivered", 1, 0)),
            firsthalf_shipped = sum(ifelse(order_in_firsthalf == 1 & order_status == "shipped", 1, 0)),
            firsthalf_inprogress = sum(ifelse(order_in_firsthalf == 1 & order_status == "in progress", 1, 0)),
            firsthalf_shippedlate = sum(ifelse(order_in_firsthalf == 1, shipping_late, 0)),
            firsthalf_avgshippedlate_days = mean(ifelse(order_in_firsthalf == 1, shipping_late_days, 0)),
            firsthalf_deliverylate = sum(ifelse(order_in_firsthalf == 1, delivery_late, 0)),
            firsthalf_avgdeliverylate_days = mean(ifelse(order_in_firsthalf == 1, delivery_late_days, 0)),
            firsthalf_suspended = sum(ifelse(order_in_firsthalf == 1 & so_suspension_reason == "suspended", 1, 0)),
            firsthalf_canceled = sum(ifelse(order_in_firsthalf == 1 & so_suspension_reason == "canceled", 1, 0)),
            lasthalf_orders = sum(order_in_lasthalf),
            lasthalf_items = sum(ifelse(order_in_lasthalf == 1, n_items, 0)),
            lasthalf_price = sum(ifelse(order_in_lasthalf == 1, oi_price, 0)),
            lasthalf_freight = sum(ifelse(order_in_lasthalf == 1, oi_freight_value, 0)),
            lasthalf_workeddays = n_distinct(ifelse(order_in_lasthalf == 1, as.Date(so_purchase_timestamp), 0))-1,
            lasthalf_delivered = sum(ifelse(order_in_lasthalf == 1 & order_status == "delivered", 1, 0)),
            lasthalf_shipped = sum(ifelse(order_in_lasthalf == 1 & order_status == "shipped", 1, 0)),
            lasthalf_inprogress = sum(ifelse(order_in_lasthalf == 1 & order_status == "in progress", 1, 0)),
            lasthalf_shippedlate = sum(ifelse(order_in_lasthalf == 1, shipping_late, 0)),
            lasthalf_avgshippedlate_days = mean(ifelse(order_in_lasthalf == 1, shipping_late_days, 0)),
            lasthalf_deliverylate = sum(ifelse(order_in_lasthalf == 1, delivery_late, 0)),
            lasthalf_avgdeliverylate_days = mean(ifelse(order_in_lasthalf == 1, delivery_late_days, 0)),
            lasthalf_suspended = sum(ifelse(order_in_lasthalf == 1 & so_suspension_reason == "suspended", 1, 0)),
            lasthalf_canceled = sum(ifelse(order_in_lasthalf == 1 & so_suspension_reason == "canceled", 1, 0)),)

## Creating the variables gmv
df_orders_sellers <- df_orders_sellers %>%
  mutate(total_gmv = total_price + total_freight,
         period_gmv = period_price + period_freight,
         firsthalf_gmv = firsthalf_price + firsthalf_freight,
         lasthalf_gmv = lasthalf_price + lasthalf_freight)

## Creating the variables freightratio
df_orders_sellers <- df_orders_sellers %>%
  mutate(total_freightratio = ifelse(total_price > 0, total_freight / total_price, 0),
         period_freightratio = ifelse(period_price > 0, period_freight / period_price, 0),
         firsthalf_freightratio = ifelse(firsthalf_price > 0, firsthalf_freight / firsthalf_price, 0),
         lasthalf_freightratio = ifelse(lasthalf_price > 0, lasthalf_freight / lasthalf_price, 0))

## Creating the variables avgticket
df_orders_sellers <- df_orders_sellers %>%
  mutate(total_avgticket = ifelse(total_items > 0, total_price / total_items, 0),
         period_avgticket = ifelse(period_items > 0, period_price / period_items, 0),
         firsthalf_avgticket = ifelse(firsthalf_items > 0, firsthalf_price / firsthalf_items, 0),
         lasthalf_avgticket = ifelse(lasthalf_items > 0, lasthalf_price / lasthalf_items, 0))

## Combining data
df_sellers <- df_sellers %>%
  left_join(df_orders_sellers, by = c("ss_id" = "so_seller_id"))

## Replacing NA to zero in numeric variables
df_sellers <- df_sellers %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

## Creating the variable active_days
df_sellers <- df_sellers %>%
  mutate(active_days = as.numeric(DATE_OF_CUT - as.Date(ss_created_at))) %>%
  mutate(active_days = ifelse(is.na(active_days), 0, active_days))



## Analyzing the best way to find the response variable

## Blocked sellers
df_sellers <- df_sellers %>%
  mutate(ss_last_blocked_on = as.Date(ss_last_blocked_on))

## Creating the variable blocked_date
df_sellers <- df_sellers %>%
  mutate(blocked_date = ifelse(ss_blocked, ss_last_blocked_on, NA))

## Create the variable days_blocked
df_sellers <- df_sellers %>%
  mutate(blocked_days = as.numeric(DATE_OF_CUT - blocked_date)) %>%
  mutate(blocked_days = ifelse(ss_blocked == F, 0, blocked_days))

## Comparing with is_churned
df_sellers %>%
  filter(blocked_days >= PERIOD_IN_DAYS) %>%
  group_by(is_churned) %>%
  summarise(n = n(),
            avg_orders_period = mean(period_orders))
# Entre os sellers bloqueados a mais de 180 dias, 3070 estão considerados oficialmente churn
# Por outra visão, 1133 sellers não considerados oficialmente churn estão bloqueados há pelo menos 180 dias
# e obviamente ficaram sem realizar pedidos durante este período

# df_sellers %>%
#   filter(is_churned == 0, blocked_days >= PERIOD_IN_DAYS) %>%
#   group_by(ss_blocked_reason) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# df_sellers %>%
#   filter(is_churned == 0, blocked_days >= 0) %>%
#   group_by(ss_blocked_reason) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# View(df_sellers %>%
#   filter(is_churned == 0, ss_blocked_reason == "churn"))


df_sellers %>%
  filter(blocked_days >= PERIOD_IN_DAYS/2) %>%
  group_by(is_churned) %>%
  summarise(n = n(),
            avg_orders_lasthalf = mean(lasthalf_orders))
# Entre os sellers bloqueados a mais de 90 dias, 3275 estão considerados oficialmente churn
# Por outra visão, 1246 sellers não considerados oficialmente churn estão bloqueados há mais de 90 dias
# e obviamente ficaram sem realizar pedidos durante este período
(3275 + 1246) - (3070 + 1133)
# Mudando a faixa de >= 180 dias para >= 90, a variação é de somente 318 sellers


## Creating the variables noorders_lasthalf and noorders_period
df_sellers <- df_sellers %>%
  mutate(noorders_lasthalf = ifelse(lasthalf_orders == 0, 1, 0),
         noorders_period = ifelse(period_orders == 0, 1, 0))



table(df_sellers$noorders_lasthalf)
prop.table(table(df_sellers$noorders_lasthalf))

table(df_sellers$noorders_period)
prop.table(table(df_sellers$noorders_period))

table(df_sellers$is_churned)
prop.table(table(df_sellers$is_churned))

table(df_sellers$noorders_lasthalf)
table(df_sellers$noorders_lasthalf, df_sellers$is_churned)


## Creating the variables is_churned_block180 and is_churned_noordersperiod
df_sellers <- df_sellers %>%
  mutate(is_churned_block180 = ifelse(is_churned == 1 | blocked_days >= PERIOD_IN_DAYS, 1, 0),
         is_churned_noordersperiod = ifelse(is_churned == 1 | noorders_period == 1, 1, 0))

table(df_sellers$is_churned_block180)

table(df_sellers$is_churned_noordersperiod)
prop.table(table(df_sellers$is_churned_noordersperiod))


## Replacing NA to zero in numeric variables
df_sellers <- df_sellers %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))


write.csv(df_sellers, "Temp/df_sellers.csv", row.names = F)

#--------------------------------------------------------------------------------------------------
# Exploring data about sellers:


## origin

### is_churned
prop.table(table(df_sellers$ss_origin, df_sellers$is_churned), 1)
# 78% da origem v1 é churn

### noorderslasthalf
prop.table(table(df_sellers$ss_origin, df_sellers$noorders_lasthalf), 1)
# 86% da origem v1 é churn


### noordersperiod
prop.table(table(df_sellers$ss_origin, df_sellers$noorders_period), 1)
# 85% da origem v1 é churn



## plan_type

### is_churned
prop.table(table(df_sellers$ss_plan_type, df_sellers$is_churned), 1)
# brand, branded_seller e mid possuem baixo percentual de churn

### noorderslasthalf
prop.table(table(df_sellers$ss_plan_type, df_sellers$noorders_lasthalf), 1)
# branded_seller possui percentua de churn mais significativo
# small possui mais de 60% de churn

### noordersperiod
prop.table(table(df_sellers$ss_plan_type, df_sellers$noorders_period), 1)
# Similar ao anterior




## City

### is_churned
prop.table(table(df_sellers$sa_city, df_sellers$is_churned), 1)
# Todas as cidades com percentual parecido de churn

### noorderslasthalf
prop.table(table(df_sellers$sa_city, df_sellers$noorders_lasthalf), 1)
# Todas as cidades com percentual parecido de churn

### noordersperiod
prop.table(table(df_sellers$sa_city, df_sellers$noorders_period), 1)
# Todas as cidades com percentual parecido de churn



## state

### is_churned
prop.table(table(df_sellers$sa_state, df_sellers$is_churned), 1)
# Todas os estados com percentual parecido de churn

### noorderslasthalf
prop.table(table(df_sellers$sa_state, df_sellers$noorders_lasthalf), 1)
# Todas os estados com percentual parecido de churn

### noordersperiod
prop.table(table(df_sellers$sa_state, df_sellers$noorders_period), 1)
# Todas os estados com percentual parecido de churn



## region

### is_churned
prop.table(table(df_sellers$region, df_sellers$is_churned), 1)
# Todas as regiões com percentual parecido de churn

### noorderslasthalf
prop.table(table(df_sellers$region, df_sellers$noorders_lasthalf), 1)
# Região norte com 86% de churn se destaca entre as demais

### noordersperiod
prop.table(table(df_sellers$region, df_sellers$noorders_period), 1)
# Região norte com 83% de churn se destaca entre as demais




## has_withdraw_rejection

### is_churned
prop.table(table(df_sellers$ss_has_withdraw_rejection, df_sellers$is_churned), 1)
# Sem diferença significante

### noorderslasthalf
prop.table(table(df_sellers$ss_has_withdraw_rejection, df_sellers$noorders_lasthalf), 1)
# TRUE possue 83% de churn

### noordersperiod
prop.table(table(df_sellers$ss_has_withdraw_rejection, df_sellers$noorders_period), 1)
# TRUE possue 76% de churn




## nps last_score

### is_churned
prop.table(table(df_sellers$np_last_score, df_sellers$is_churned), 1)
# Quanto maior a nota do NPS, menor a probabilidade de churn

### noorderslasthalf
prop.table(table(df_sellers$np_last_score, df_sellers$noorders_lasthalf), 1)
# Quanto maior a nota do NPS, menor a probabilidade de churn, porem com menor intensidade que o caso anterior

### noordersperiod
prop.table(table(df_sellers$np_last_score, df_sellers$noorders_period), 1)
# Quanto maior a nota do NPS, menor a probabilidade de churn



## n_products

### is_churned
cor(df_sellers[c("sp_n_products", "is_churned")])
# fraca correlação negativa (10%)

### noorderslasthalf
cor(df_sellers[c("sp_n_products", "noorders_lasthalf")])
# fraca correlação negativa (20%)

### noordersperiod
cor(df_sellers[c("sp_n_products", "noorders_period")])
# fraca correlação negativa (20%)




## cashflow

### is_churned
prop.table(table(df_sellers$cashflow_ranges, df_sellers$is_churned), 1)
# 2 categorias com menor probabilidade de churn

### noorderslasthalf
prop.table(table(df_sellers$cashflow_ranges, df_sellers$noorders_lasthalf), 1)
# 1 categoria com maior probabilidade de churn

### noordersperiod
prop.table(table(df_sellers$cashflow_ranges, df_sellers$noorders_period), 1)
# 1 categoria com maior probabilidade de churn



## revenue

### is_churned
prop.table(table(df_sellers$df_revenue, df_sellers$is_churned), 1)
# Nenhuma categoria se destaca

### noorderslasthalf
prop.table(table(df_sellers$df_revenue, df_sellers$noorders_lasthalf), 1)
# A categoria "desconhecido" possui 70% de churn

### noordersperiod
prop.table(table(df_sellers$df_revenue, df_sellers$noorders_period), 1)
# A categoria "desconhecido" possui 66% de churn




## behavior profile

### is_churned
prop.table(table(df_sellers$df_behavior_profile, df_sellers$is_churned), 1)
# Nenhuma categoria se destaca

### noorderslasthalf
prop.table(table(df_sellers$df_behavior_profile, df_sellers$noorders_lasthalf), 1)
# Nenhuma categoria se destaca

### noordersperiod
prop.table(table(df_sellers$df_behavior_profile, df_sellers$noorders_period), 1)
# Nenhuma categoria se destaca




## persona

### is_churned
prop.table(table(df_sellers$df_id_persona, df_sellers$is_churned), 1)
# Nenhuma categoria se destaca

### noorderslasthalf
prop.table(table(df_sellers$df_id_persona, df_sellers$noorders_lasthalf), 1)
# Personas 155 e 156 possuem maior probabilidade de churn

### noordersperiod
prop.table(table(df_sellers$df_id_persona, df_sellers$noorders_period), 1)
# Personas 155 e 156 possuem maior probabilidade de churn




## seller_stage

### is_churned
prop.table(table(df_sellers$seller_stage, df_sellers$is_churned), 1)
# Ativação possui maior probabilidade de churn

### noorderslasthalf
prop.table(table(df_sellers$seller_stage, df_sellers$noorders_lasthalf), 1)
# Ativação possui maior probabilidade de churn

### noordersperiod
prop.table(table(df_sellers$seller_stage, df_sellers$noorders_period), 1)
# Ativação possui maior probabilidade de churn


## orders

### is_churned
cor(df_sellers %>%
      select(total_orders,
             period_orders,
             firsthalf_orders,
             lasthalf_orders,
             is_churned))
# fraca correlação negativa (12%)

### noorderslasthalf
cor(df_sellers %>%
      select(total_orders,
             period_orders,
             firsthalf_orders,
             lasthalf_orders,
             noorders_lasthalf))
# fraca correlação negativa (20%)

### noordersperiod
cor(df_sellers %>%
      select(total_orders,
             period_orders,
             firsthalf_orders,
             lasthalf_orders,
             noorders_period))
# fraca correlação negativa (20%)



## gmv

### is_churned
cor(df_sellers %>%
      select(total_gmv,
             period_gmv,
             firsthalf_gmv,
             lasthalf_gmv,
             is_churned))
# fraca correlação negativa (14%)

### noorderslasthalf
cor(df_sellers %>%
      select(total_gmv,
             period_gmv,
             firsthalf_gmv,
             lasthalf_gmv,
             noorders_lasthalf))
# fraca correlação negativa (25%)

### noordersperiod
cor(df_sellers %>%
      select(total_gmv,
             period_gmv,
             firsthalf_gmv,
             lasthalf_gmv,
             noorders_period))
# fraca correlação negativa (25%)



## worked days

### is_churned
cor(df_sellers %>%
      select(total_workeddays,
             period_workeddays,
             firsthalf_workeddays,
             lasthalf_workeddays,
             is_churned))
# fraca correlação negativa (35%)

### noorderslasthalf
cor(df_sellers %>%
      select(total_workeddays,
             period_workeddays,
             firsthalf_workeddays,
             lasthalf_workeddays,
             noorders_lasthalf))
# correlação negativa moderada (66%)

### noordersperiod
cor(df_sellers %>%
      select(total_workeddays,
             period_workeddays,
             firsthalf_workeddays,
             lasthalf_workeddays,
             noorders_period))
# fraca correlação negativa (60%)




## shipped late

### is_churned
cor(df_sellers %>%
      select(total_shippedlate,
             period_shippedlate,
             firsthalf_shippedlate,
             lasthalf_shippedlate,
             is_churned))
# fraca correlação negativa (12%)

### noorderslasthalf
cor(df_sellers %>%
      select(total_shippedlate,
             period_shippedlate,
             firsthalf_shippedlate,
             lasthalf_shippedlate,
             noorders_lasthalf))
# fraca correlação negativa(20%)

### noordersperiod
cor(df_sellers %>%
      select(total_shippedlate,
             period_shippedlate,
             firsthalf_shippedlate,
             lasthalf_shippedlate,
             noorders_period))
# fraca correlação negativa(20%)




## delivery late

### is_churned
cor(df_sellers %>%
      select(total_deliverylate,
             period_deliverylate,
             firsthalf_deliverylate,
             lasthalf_deliverylate,
             is_churned))
# fraca correlação negativa (10%)

### noorderslasthalf
cor(df_sellers %>%
      select(total_deliverylate,
             period_deliverylate,
             firsthalf_deliverylate,
             lasthalf_deliverylate,
             noorders_lasthalf))
# fraca correlação negativa(15%)

### noordersperiod
cor(df_sellers %>%
      select(total_deliverylate,
             period_deliverylate,
             firsthalf_deliverylate,
             lasthalf_deliverylate,
             noorders_period))
# fraca correlação negativa(15%)




## suspended

### is_churned
cor(df_sellers %>%
      select(total_suspended,
             period_suspended,
             firsthalf_suspended,
             lasthalf_suspended,
             is_churned))
# not work

### noorderslasthalf
cor(df_sellers %>%
      select(total_suspended,
             period_suspended,
             firsthalf_suspended,
             lasthalf_suspended,
             noorders_lasthalf))
# not work

### noordersperiod
cor(df_sellers %>%
      select(total_suspended,
             period_suspended,
             firsthalf_suspended,
             lasthalf_suspended,
             noorders_period))
# not work



## canceled

### is_churned
cor(df_sellers %>%
      select(total_canceled,
             period_canceled,
             firsthalf_canceled,
             lasthalf_canceled,
             is_churned))
# not work

### noorderslasthalf
cor(df_sellers %>%
      select(total_canceled,
             period_canceled,
             firsthalf_canceled,
             lasthalf_canceled,
             noorders_lasthalf))
# not work

### noordersperiod
cor(df_sellers %>%
      select(total_canceled,
             period_canceled,
             firsthalf_canceled,
             lasthalf_canceled,
             noorders_period))
# not work




## avg ticket

### is_churned
cor(df_sellers %>%
      select(total_avgticket,
             period_avgticket,
             firsthalf_avgticket,
             lasthalf_avgticket,
             is_churned))
# fraca correlação negativa (15%)

### noorderslasthalf
cor(df_sellers %>%
      select(total_avgticket,
             period_avgticket,
             firsthalf_avgticket,
             lasthalf_avgticket,
             noorders_lasthalf))
# fraca correlação negativa (30%)

### noordersperiod
cor(df_sellers %>%
      select(total_avgticket,
             period_avgticket,
             firsthalf_avgticket,
             lasthalf_avgticket,
             noorders_period))
# fraca correlação negativa (35%)




## freight ratio

### is_churned
cor(df_sellers %>%
      select(total_freightratio,
             period_freightratio,
             firsthalf_freightratio,
             lasthalf_freightratio,
             is_churned))
# fraca correlação negativa (35%)

### noorderslasthalf
cor(df_sellers %>%
      select(total_freightratio,
             period_freightratio,
             firsthalf_freightratio,
             lasthalf_freightratio,
             noorders_lasthalf))
# correlação negativa moderada (70%)

### noordersperiod
cor(df_sellers %>%
      select(total_freightratio,
             period_freightratio,
             firsthalf_freightratio,
             lasthalf_freightratio,
             noorders_period))
# correlação negativa moderada (63%)



## active days

### is_churned
cor(df_sellers %>%
      select(active_days,
             is_churned))
# correlação positiva moderada (44%)

### noorderslasthalf
cor(df_sellers %>%
      select(active_days,
             noorders_lasthalf))
# fraca correlação positiva (34%)

### noordersperiod
cor(df_sellers %>%
      select(active_days,
             noorders_period))
# correlação negativa moderada (40%)




## blocked days

### is_churned
cor(df_sellers %>%
      select(blocked_days,
             is_churned))
# correlação positiva moderada (68%)

### noorderslasthalf
cor(df_sellers %>%
      select(blocked_days,
             noorders_lasthalf))
# fraca correlação positiva (62%)

### noordersperiod
cor(df_sellers %>%
      select(blocked_days,
             noorders_period))
# correlação negativa moderada (68%)



## Considering noorders_lasthalf as response:

df_simulation <- df_sellers %>%
  select(noorders_lasthalf,
         ss_origin,
         ss_plan_type,
         ss_has_withdraw_rejection,
         sp_n_products,
         cashflow_ranges,
         df_revenue,
         df_id_persona,
         seller_stage,
         firsthalf_workeddays,
         active_days,
         blocked_days)


## Simulate a logistic regression
fit_simulate <- glm(formula = noorders_lasthalf ~., 
                   data = df_simulation, 
                   family = binomial(link = 'logit'))

summary(fit_simulate)



fit_simulate2 <- step(fit_simulate, direction = "both", data = df_simulation, k = 2)
summary(fit_simulate2)



## Diagnosis

library(statmod)
library(pROC)

get_res_pred <- function(model) {
  res <- qres.binom(model)
  pred <- predict(model)
  out <- data.frame(res, pred)
}

plot_fit_resid <- function(df_res_pred, GGPLOT_COLOR) {
  plt <- ggplot(df_res_pred, aes(x = pred, y = res)) +
    geom_point(color = GGPLOT_COLOR, alpha = 0.6, size = 2) +
    stat_smooth(method = "loess", color = "red") +
    labs(x = "Fitted values", y = "Residuals")
  print(plt)
}

plot_qqplot <- function(df_res_pred, GGPLOT_COLOR) {
  ggplot(df_res_pred, aes(sample = res)) +
    stat_qq(color = GGPLOT_COLOR, alpha = 0.6, size = 2) +
    stat_qq_line(color = "red") +
    labs(x = "Theorical quantiles", y = "Sample quantiles")
}

get_predicts_response <- function(model, df_test) {
  out <- predict(model, newdata = df_test, type = 'response')
}

plot_fit_histogram <- function(df, GGPLOT_COLOR) {
  plt <- ggplot(df, aes(x = fitted)) +
    geom_histogram(bins = 30, fill = GGPLOT_COLOR, alpha = 0.6, color = GGPLOT_COLOR) +
    labs(x = "Valores ajustados", y = "Frequência")
  plt
}

get_predicts_response <- function(model, df_test) {
  out <- predict(model, newdata = df_test, type = 'response')
}

plot_confusionMatrix <- function(df_test, threshold) {
  df_test$churn <- factor(df_test$noorders_lasthalf, labels = c("not churn", "churn"))
  tab_pred <- table(ifelse(df_test$fitted < threshold, 'not churn', 'churn'), df_test$churn)
  tab_pred <- as.data.frame(tab_pred)
  names(tab_pred) <- c("Predict", "Actual", "Freq")

  threshold <- round(threshold, 2)
  plt_title <- str_c("Confusion Matrix (threshold: ", threshold, ")", sep = "")

  plt <- ggplot(tab_pred, aes(x = Actual, y = Predict)) +
    geom_tile(fill = "white", color = MAIN_COLOUR) +
    geom_text(aes(x = Actual, y = Predict, label = Freq), color = MAIN_COLOUR) +
    theme(legend.position = "none") +
    ggtitle(plt_title)
  plt
}


MODEL <- fit_simulate2
DATASET <- df_simulation
DATASET_TRAIN <- df_simulation
DATASET_TEST <- df_simulation
df_res_pred <- get_res_pred(MODEL)
plot_fit_resid(df_res_pred, MAIN_COLOUR)
plot_qqplot(df_res_pred, MAIN_COLOUR)
#hnp(MODEL)
rm(df_res_pred)


## Prediction

DATASET_TEST$fitted <- get_predicts_response(MODEL, DATASET_TEST)
plot_fit_histogram(DATASET_TEST, MAIN_COLOUR)
roc_curve <- roc(DATASET_TEST$noorders_lasthalf, DATASET_TEST$fitted, plot=F, ci=T, ci.sp = T)
prev <- prop.table(table(DATASET$noorders_lasthalf))[2] # prevalência
threshold <- as.numeric(max(coords(roc_curve, x = "best", best.method = "youden", best.weights=c(1, prev), transpose = F)[1]))
plot_ggroc(DATASET_TEST, GGPLOT_COLOR)
coords(roc_curve, x = threshold, ret = c("sensitivity", "specificity", "accuracy"), transpose = F)
plot_confusionMatrix(DATASET_TEST, threshold)



summary(fit_simulate2)




