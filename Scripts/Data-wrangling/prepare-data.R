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
require(lubridate)
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
# Config:
date_of_cut <- Sys.Date()
start_date <- date_of_cut - 180
period_in_days <- as.numeric(date_of_cut - start_date)
options(scipen=999)
main_colour = "#0C29D0"
sec_colour = "#0DC78B"

#--------------------------------------------------------------------------------------------------
# Preparing data about sellers:

df_sellers <- get_sellers()
# df_deal_sellers <- get_deals_sellers()

## Keeping only sellers with more than 180 days working
df_sellers <- df_sellers %>%
  filter(as.Date(ss_created_at) <= start_date)

## Transform all blank values to NA:
df_sellers <- df_sellers %>%
  mutate_all(na_if, "")

# df_deal_sellers <- df_deal_sellers %>%
#   mutate_all(na_if, "")

## Replacing NA to zero in numeric variables
df_sellers <- df_sellers %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

# df_deal_sellers <- df_deal_sellers %>%
#   mutate_if(is.numeric, ~replace(., is.na(.), 0))

# ### Combining the datasets:
# if(is_unique_id("ss_id", df_sellers) & is_unique_id("ds_id_seller", df_deal_sellers)) {
#   df_sellers <- df_sellers %>%
#     left_join(df_deal_sellers, by = c("ss_id" = "ds_id_seller"))
# } else {
#   print("ERROR: Check if the datasets have unique IDs")
# }

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

# ### Creating the variable cashflow_ranges:
# df_sellers <- df_sellers %>%
#   mutate(ds_cashflow = ifelse(ds_cashflow == 0 | is.na(ds_cashflow), "Desconhecido", ds_cashflow))
# 
# cashflow <- df_sellers %>%
#   filter(ds_cashflow != "Desconhecido")
# cashflow_median <- median(as.numeric(cashflow$ds_cashflow))
# cashflow_quantile3 <- as.numeric(quantile(as.numeric(cashflow$ds_cashflow), 0.75))
# cashflow_max <- max(as.numeric(cashflow$ds_cashflow))
# 
# cashflow$cashflow_ranges <- cut(as.numeric(cashflow$ds_cashflow), breaks = c(1, cashflow_median, cashflow_quantile3, cashflow_max))
# df_sellers <- df_sellers %>%
#   left_join((cashflow %>%
#                select(ss_id, cashflow_ranges)), by = "ss_id")
# df_sellers$cashflow_ranges <- as.character(df_sellers$cashflow_ranges)
# df_sellers <- df_sellers %>%
#   mutate(cashflow_ranges = ifelse(is.na(cashflow_ranges), ds_cashflow, cashflow_ranges))
# rm(cashflow, cashflow_max, cashflow_median, cashflow_quantile3)
# 
# 
# ### Simplifying df_revenue into 4 classes:
# 
# df_sellers <- df_sellers %>%
#   mutate(df_revenue = str_to_title(str_trim(df_revenue)))
# range_1 <- c("De 500k A 3.6mi Ano (40 A 300k Mês)",
#              "De 480k A 3.6mi Ano (40k A 300k Mês)",
#              "De 500k A 3.6mi Ano (40 A 300k Mes)",
#              "Entre R$500k Aa E R$3.600mi Aa (Entre R$40k Am E R$300kam)")
# value_1 <- "De 500k a 3.6mi ano"
# 
# range_2 <- c("De 60 A 500k Ano (5 A 40k Mês)",
#              "De 60k A 480k Ano (5k A 40k Mês)",
#              "De 60 A 500k Ano (5 A 40k Mes)",
#              "Entre R$ 60k Aa E R$500k Am (Entre R$5k E R$40k Am)")
# value_2 <- "De 60k a 500k ano"
# 
# range_3 <- c("Até 60k Ano (5k Mês)",
#              "Até R$ 60k Aa (Até R$ 5k Am)",
#              "De 60 A 500k Ano (5 A 40k Mes)")
# value_3 <- "Até 60k ano"
# 
# range_4 <- c("Acima De 3.6mi Ano (300k Mês)",
#              "De 3.6mi A 8.4mi Ano (300k A 800k Mês)",
#              "De 8.4mi A 72mi Ano (800k A 6mi Mês)",
#              "Acima De 3.6mi Ano (300k Mes)",
#              "Acima De R$3.600mi Aa (Acima De R$300k Am)",
#              "Acima De 72mi Ano (6mi Mês)")
# value_4 <- "Acima de 3.6mi ano"
# 
# all <- c(value_1, value_2, value_3, value_4)
# 
# df_sellers <- df_sellers %>%
#   mutate(df_revenue = ifelse(df_revenue %in% range_1, value_1, df_revenue)) %>%
#   mutate(df_revenue = ifelse(df_revenue %in% range_2, value_2, df_revenue)) %>%
#   mutate(df_revenue = ifelse(df_revenue %in% range_3, value_3, df_revenue)) %>%
#   mutate(df_revenue = ifelse(df_revenue %in% range_4, value_4, df_revenue)) %>%
#   mutate(df_revenue = ifelse(df_revenue %in% all, df_revenue, "Desconhecido"))
# 
# rm(all, range_1, range_2, range_3, range_4, value_1, value_2, value_3, value_4)
# 
# 
# ### Identifying the top 5 behavior profile:
# 
# df_sellers$df_behavior_profile <- str_trim(df_sellers$df_behavior_profile)
# df_sellers$df_behavior_profile <- str_to_title(df_sellers$df_behavior_profile)
# df_sellers$df_behavior_profile <- stri_trans_general(df_sellers$df_behavior_profile, "Latin-ASCII")
# df_sellers$df_behavior_profile <- str_replace_all(df_sellers$df_behavior_profile, " ", "")
# df_sellers$df_behavior_profile <- str_replace_all(df_sellers$df_behavior_profile, ",", "/")
# df_sellers <- df_sellers %>%
#   mutate(df_behavior_profile = ifelse(df_behavior_profile %in% c("None", "", NA),
#                                       "Outro",
#                                       df_behavior_profile))
# 
# top_5_behavior <- df_sellers %>%
#   group_by(df_behavior_profile) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# top_5_behavior <- top_5_behavior[(1:5),]
# top_5_behavior <- top_5_behavior$df_behavior_profile
# 
# ### Simplifying behavior profile into 5 classes:
# df_sellers <- df_sellers %>%
#   mutate(df_behavior_profile = ifelse(df_behavior_profile %in% top_5_behavior, df_behavior_profile, "Outro"))
# rm(top_5_behavior)
# 
# 
# ### Identifying the top 5 persona:
# 
# df_sellers$df_id_persona <- str_trim(df_sellers$df_id_persona)
# df_sellers$df_id_persona <- str_to_title(df_sellers$df_id_persona)
# df_sellers <- df_sellers %>%
#   mutate(df_id_persona = ifelse(df_id_persona %in% c("None", "", NA),
#                                 "Outro",
#                                 df_id_persona))
# 
# top_5_persona <- df_sellers %>%
#   group_by(df_id_persona) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# top_5_persona <- top_5_persona[(1:5),]
# top_5_persona <- top_5_persona$df_id_persona
# 
# ### Simplifying persona into 5 classes:
# df_sellers <- df_sellers %>%
#   mutate(df_id_persona = ifelse(df_id_persona %in% top_5_persona, df_id_persona, "Outro"))
# rm(top_5_persona)
# 
# 
# ### Creating the variable portfolio_reg_rate
# df_sellers <- df_sellers %>%
#   mutate(df_portfolio_cadastravel = as.numeric(df_portfolio_cadastravel)) %>%
#   mutate(df_portfolio_cadastravel = ifelse(is.na(df_portfolio_cadastravel), 0, df_portfolio_cadastravel)) %>%
#   mutate(sp_n_products = as.numeric(sp_n_products)) %>%
#   mutate(sp_n_products = ifelse(is.na(sp_n_products), 0, sp_n_products)) %>%
#   mutate(portfolio_reg_rate = ifelse(df_portfolio_cadastravel > 0,
#                                      sp_n_products / df_portfolio_cadastravel,
#                                      1))


#--------------------------------------------------------------------------------------------------
# Preparing data about orders:

df_orderitems <- get_order_item()

## Keeping only orders related to df_sellers:
df_orderitems <- df_orderitems %>%
  filter(so_seller_id %in% df_sellers$ss_id)

## Transform all blank values to NA:
df_orderitems <- df_orderitems %>%
  mutate_all(na_if, "")

## Aggregating orders by items:
df_orderitems$oi_price <- as.numeric(df_orderitems$oi_price)
df_orderitems$oi_freight_value <- as.numeric(df_orderitems$oi_freight_value)

  df_orders <- df_orderitems %>%
    group_by(so_order_id, so_seller_id, so_purchase_timestamp) %>%
    summarise(n_items = n(), oi_price = sum(oi_price), oi_freight_value = sum(oi_freight_value)) %>%
    arrange(desc(n_items))

  
## Creating the variable order_relative_day (considering start_date as the first day)
df_orders <- df_orders %>%
  mutate(order_relative_day = as.numeric(as.Date(so_purchase_timestamp) - as.Date(start_date)))


## Creating the variable order_relative_day_rev:
df_orders <- df_orders %>%
  mutate(order_relative_day_rev = as.numeric(as.Date(date_of_cut) - as.Date(so_purchase_timestamp)))

#--------------------------------------------------------------------------------------------------
# Exploring data about sellers:

## Grouping orders by seller
df_orders_sellers <- df_orders %>%
  group_by(so_seller_id) %>%
  summarise(first_order = min(as.Date(so_purchase_timestamp)),
            last_order = max(as.Date(so_purchase_timestamp)),
            total_orders = n(),
            orders_period = sum(ifelse(order_relative_day %in% (1:period_in_days), 1, 0)),
            orders_first_half = sum(order_relative_day %in% (1:(period_in_days/2))),
            orders_last_half = sum(order_relative_day > period_in_days/2),
            total_items = sum(n_items),
            items_period = sum(ifelse(order_relative_day %in% (1:period_in_days), n_items, 0)),
            items_first_half = sum(ifelse(order_relative_day %in% (1:(period_in_days/2)), n_items, 0)),
            items_last_half = sum(ifelse(order_relative_day > period_in_days/2, n_items, 0)),
            worked_days = n_distinct(so_purchase_timestamp),
            workeddays_period = n_distinct(ifelse(order_relative_day %in% (1:period_in_days), so_purchase_timestamp, NA))-1,
            workeddays_first_half = n_distinct(ifelse(order_relative_day %in% (1:(period_in_days/2)), so_purchase_timestamp, 0))-1,
            workeddays_last_half = n_distinct(ifelse(order_relative_day > period_in_days/2, so_purchase_timestamp, 0))-1,
            total_price = sum(oi_price),
            price_period = sum(ifelse(order_relative_day %in% (1:period_in_days), oi_price, 0)),
            price_first_half = sum(ifelse(order_relative_day %in% (1:(period_in_days/2)), oi_price, 0)),
            price_last_half = sum(ifelse(order_relative_day > period_in_days/2, oi_price, 0)),
            total_freight = sum(oi_freight_value),
            freight_period = sum(ifelse(order_relative_day %in% (1:period_in_days), oi_freight_value, 0)),
            freight_first_half = sum(ifelse(order_relative_day %in% (1:(period_in_days/2)), oi_freight_value, 0)),
            freight_last_half = sum(ifelse(order_relative_day > period_in_days/2, oi_freight_value, 0)))

## Creating the variables GMV
df_orders_sellers <- df_orders_sellers %>%
  mutate(total_gmv = total_price + total_freight,
         gmv_period = price_period + freight_period,
         gmv_first_half = price_first_half + freight_first_half,
         gmv_last_half = price_last_half + freight_last_half)

## Creating the variables freight_ratio
df_orders_sellers <- df_orders_sellers %>%
  mutate(total_freight_ratio = ifelse(total_price > 0, total_freight / total_price, 0),
         freight_ratio_period = ifelse(price_period > 0, freight_period / price_period, 0),
         freight_ratio_first_half = ifelse(price_first_half > 0, freight_first_half / price_first_half, 0),
         freight_ratio_last_half = ifelse(price_last_half > 0, freight_last_half / price_last_half, 0))

## Creating the variables avg_ticket
df_orders_sellers <- df_orders_sellers %>%
  mutate(total_avg_ticket = ifelse(total_items > 0, total_price / total_items, 0),
         avg_ticket_period = ifelse(items_period > 0, price_period / items_period, 0),
         avg_ticket_first_half = ifelse(items_first_half > 0, price_period / items_period, 0),
         avg_ticket_last_half = ifelse(items_last_half > 0, price_period / items_period, 0))

## Removing items, price and freight variables
df_orders_sellers <- df_orders_sellers %>%
  select(-total_price,
         -total_freight,
         -total_items,
         -price_period,
         -freight_period,
         -items_period,
         -price_first_half,
         -freight_first_half,
         -items_first_half,
         -price_last_half,
         -freight_last_half,
         -items_last_half)













## Combining with df_sellers
df_orders_sellers <- df_orders_sellers %>%
  select(-ss_created_at)

df_sellers <- df_sellers %>%
  left_join(df_orders_sellers, by = c("ss_id" = "so_seller_id"))

# ## Replacing the variable is_churned
# df_sellers <- df_sellers %>%
#   mutate(is_churned = ifelse(lc_current_type %in% c("Cancelamento"), 1, 0)) %>%
#   mutate(is_churned = ifelse(ss_status %in% c("documentation"), 1, is_churned))

## Creating the variable active_days
df_sellers <- df_sellers %>%
  mutate(active_days = as.numeric(date_of_cut - as.Date(ss_created_at))) %>%
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
  mutate(days_blocked = as.numeric(date_of_cut - blocked_date)) %>%
  mutate(days_blocked = ifelse(is.na(days_blocked), 0, days_blocked))

## Comparing with is_churned
df_sellers %>%
  filter(days_blocked >= 180) %>%
  group_by(is_churned) %>%
  summarise(n = n(),
            avg_orders_last180days = mean(n_orders_last_180_days))
# Entre os sellers bloqueados a mais de 180 dias, 3590 estão considerados oficialmente churn
# Por outra visão, 1214 sellers não considerados oficialmente churn estão bloqueados há mais de 180 dias

df_sellers %>%
  filter(days_blocked >= 90) %>%
  group_by(is_churned) %>%
  summarise(n = n(),
            avg_orders_last90days = mean(n_orders_last_90_days))
# Entre os sellers bloqueados a mais de 90 dias, 3832 estão considerados oficialmente churn
# Por outra visão, 1356 sellers não considerados oficialmente churn estão bloqueados há mais de 180 dias
(3832 + 1356) - (3590 + 1214)
# Mudando a faixa de >= 180 dias para >= 90, a variação é de somente 384 sellers


## Comparing is_churned x orders_last_90_days
df_sellers %>%
  group_by(is_churned) %>%
  summarise(n = n(), 
            n_orders_last90days = sum(n_orders_last_90_days)) %>%
  mutate(orders_rate = n_orders_last90days / n)
# Considerando toda base:
# Média de 3,6 pedidos por seller nos últimos 90 dias quando is_churned = 1
# Média de 71,5 pedidos por sellers nos último 90 dias quando is_churned = 0


## Comparing is_churned x orders_last_180_days
df_sellers %>%
  group_by(is_churned) %>%
  summarise(n = n(), 
            n_orders_last180days = sum(n_orders_last_180_days)) %>%
  mutate(orders_rate = n_orders_last180days / n)
# Considerando toda base:
# Média de 9,9 pedidos por seller nos últimos 90 dias quando is_churned = 1
# Média de 123 pedidos por sellers nos últimos 90 dias quando is_churned = 0






df_sellers %>%
  filter(n_orders_last_90_days == 0, n_orders_first_90_days > 0) %>%
  group_by(is_churned) %>%
  summarise(n = n())
  




