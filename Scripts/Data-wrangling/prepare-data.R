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
options(scipen=999)

#--------------------------------------------------------------------------------------------------
# Preparing data about sellers:

df_sellers <- get_sellers()
df_deal_sellers <- get_deals_sellers()

## Transform all blank values to NA:
df_sellers <- df_sellers %>%
  mutate_all(na_if, "")

df_deal_sellers <- df_deal_sellers %>%
  mutate_all(na_if, "")

### Combining the datasets:
if(is_unique_id("ss_id", df_sellers) & is_unique_id("ds_id_seller", df_deal_sellers)) {
  df_sellers <- df_sellers %>%
    left_join(df_deal_sellers, by = c("ss_id" = "ds_id_seller"))
} else {
  print("ERROR: Check if the datasets have unique IDs")
}

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


### Creating the variable is_churned:
count_missing_values(df_sellers)
df_sellers <- df_sellers %>%
  mutate(is_churned = ifelse(lc_current_type %in% c("Cancelamento"), 1, 0))


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


### Identifying the top 5 persona:

df_sellers$df_id_persona <- str_trim(df_sellers$df_id_persona)
df_sellers$df_id_persona <- str_to_title(df_sellers$df_id_persona)
df_sellers <- df_sellers %>%
  mutate(df_id_persona = ifelse(df_id_persona %in% c("None", "", NA),
                                      "Outro",
                                      df_id_persona))

top_5_persona <- df_sellers %>%
  group_by(df_id_persona) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

top_5_persona <- top_5_persona[(1:5),]
top_5_persona <- top_5_persona$df_id_persona

### Simplifying persona into 5 classes:
df_sellers <- df_sellers %>%
  mutate(df_id_persona = ifelse(df_id_persona %in% top_5_persona, df_id_persona, "Outro"))
rm(top_5_persona)


### Creating the variable portfolio_reg_rate
df_sellers <- df_sellers %>%
  mutate(df_portfolio_cadastravel = as.numeric(df_portfolio_cadastravel)) %>%
  mutate(df_portfolio_cadastravel = ifelse(is.na(df_portfolio_cadastravel), 0, df_portfolio_cadastravel)) %>%
  mutate(sp_n_products = as.numeric(sp_n_products)) %>%
  mutate(sp_n_products = ifelse(is.na(sp_n_products), 0, sp_n_products)) %>%
  mutate(portfolio_reg_rate = ifelse(df_portfolio_cadastravel > 0,
                                     sp_n_products / df_portfolio_cadastravel,
                                     1))

rm(df_deal_sellers)

#--------------------------------------------------------------------------------------------------
# Preparing data about orders:

df_orderitems <- get_order_item()


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


## Getting ss_created_at from df_sellers:
df_orders <- df_orders %>%
  left_join(df_sellers %>%
              select(ss_id, ss_created_at), by = c("so_seller_id" = "ss_id"))


## Creating the variable order_relative_day:
df_orders$so_purchase_timestamp <- as.Date(df_orders$so_purchase_timestamp)
df_orders$ss_created_at <- as.Date(df_orders$ss_created_at)

df_orders <- df_orders %>%
  mutate(order_relative_day = as.integer(so_purchase_timestamp - ss_created_at))
  

## Creating the variable order_relative_month: 
df_orders <- df_orders %>%
  mutate(order_relative_month = as.integer(order_relative_day / 30))
  
  
## Creating the variable order_relative_day_rev:
df_orders <- df_orders %>%
  mutate(order_relative_day_rev = as.integer(date_of_cut - so_purchase_timestamp))


## Creating the variable order_relative_month_rev:
df_orders <- df_orders %>%
  mutate(order_relative_month_rev = as.integer(order_relative_day_rev / 30))



