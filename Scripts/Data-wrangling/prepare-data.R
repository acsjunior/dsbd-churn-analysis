#-------------------------------------------------------------------------------------------------
# Script to prepare the dataset for modeling
#
#
#                                                                       Antonio C. da Silva Júnior 
#                                                                              juniorssz@gmail.com
#                                                           https://www.linkedin.com/in/juniorssz/
#                                                                    https://github.com/juniorssz/
#                                                                   https://juniorssz.netlify.com/
#                                                                2020-Feb-20 | Curitiba/PR - Brazil
#--------------------------------------------------------------------------------------------------
# Requirements:
source("Scripts/Data-wrangling/get-data-from-athena.R")
require(tidyverse)
require(lubridate)
require(stringi)
require(caTools)

#--------------------------------------------------------------------------------------------------
# Config and constants:
options(scipen=999)
DATE_OF_CUT <- Sys.Date()
PERIOD_IN_DAYS <- 180
START_DATE <- DATE_OF_CUT - PERIOD_IN_DAYS
MAIN_COLOUR = "#0C29D0"
SEC_COLOUR = "#0DC78B"

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
  n_df <- nrow(df)
  out <- data.frame(var = names(df))
  out$n_missing <- NA
  out$f_missing <- NA
  out$var <- as.character(out$var)
  for (i in 1:nrow(out)) {
    var_name <- out[i,1]
    out$n_missing[i] <- length(df[is.na(df[var_name]),1])
    out$f_missing[i] <- round(out$n_missing[i] / n_df,4)
  }
  out <- out %>%
    arrange(desc(n_missing))
  return(out)
}

#--------------------------------------------------------------------------------------------------
# Loading data
df_sellers <- get_sellers_deals()
df_orders <- get_orders()
df_orderitems <- get_order_items()

#--------------------------------------------------------------------------------------------------
# Sampling data
set.seed(1986)
smp <- sample.split(df_orders$so_id, SplitRatio = .1)
df_orders <- subset(df_orders, smp == T)

smp <- sample.split(df_orderitems$oi_id, SplitRatio = .1)
df_orderitems <- subset(df_orderitems, smp == T)


#--------------------------------------------------------------------------------------------------
# Initial treatment

## Keeping only sellers with more than 180 days working
df_sellers <- df_sellers %>%
  filter(as.Date(ss_created_at) <= START_DATE)

df_orders <- df_orders %>%
  filter(so_seller_id %in% df_sellers$ss_id)

df_orderitems <- df_orderitems %>%
  filter(oi_seller_order_id %in% df_orders$so_id)


## Transform all blank values to NA:
df_sellers <- df_sellers %>%
  mutate_all(na_if, "")

df_orders <- df_orders %>%
  mutate_all(na_if, "")

df_orderitems <- df_orderitems %>%
  mutate_all(na_if, "")


## Replacing NA to zero in numeric variables
df_sellers <- df_sellers %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

df_orders <- df_orders %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

df_orderitems <- df_orderitems %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))



#--------------------------------------------------------------------------------------------------
# Preparing data about sellers

## Getting first and last order
df_sellers_orders <- df_orders %>%
  group_by(so_seller_id) %>%
  summarise(first_order = min(as.Date(so_purchase_timestamp)),
            last_order = max(as.Date(so_purchase_timestamp)),
            total_orders = n())

df_sellers <- df_sellers %>%
  left_join(df_sellers_orders, by = c("ss_id" = "so_seller_id"))

rm(df_sellers_orders)


## Creating the variable working_range
df_sellers <- df_sellers %>%
  mutate(working_range = as.numeric(last_order - first_order)+1)


## Replacing NA to zero in numeric variables
df_sellers <- df_sellers %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))


## Checking status:
if(!is_ready_or_doc()) {
  print("ERROR: Check if the seller status is only ready or documentation")
}


## Splitting ss_about to create seller_stage variable:
df_sellers <- df_sellers %>%
  rowwise() %>%
  mutate(seller_stage = ifelse(!is.na(str_locate(ss_about, "-")[1]), 
                               str_sub(ss_about, 1, str_locate(ss_about, "-")[1]-1), 
                               NA)) %>%
  mutate(seller_stage = ifelse(is.na(seller_stage), ss_about, seller_stage))
df_sellers$seller_stage <- str_trim(df_sellers$seller_stage)
df_sellers$ss_about <- NULL


## Transform the states to upper case:
df_sellers$sa_state <- str_to_upper(df_sellers$sa_state)
df_sellers$sa_state <- str_trim(df_sellers$sa_state)


## Creating the variable region
df_sellers <- df_sellers %>%
  mutate(region = ifelse(sa_state %in% c("RS", "SC", "PR"), "Sul", 
                         ifelse(sa_state %in% c("SP", "RJ", "MG", "ES"), "Sudeste", 
                                ifelse(sa_state %in% c("MS", "GO", "DF", "MT"), "Centro-oeste/DF", 
                                       ifelse(sa_state %in% c("BA", "SE", "PE", "AL", "PB", "RN", "MA", "PI", "CE"), "Nordeste", 
                                              ifelse(sa_state %in% c("AC", "AM", "RO", "RR", "AP", "PA", "TO"), "Norte", NA))))))


## Capitalize and remove accentuation from cities:
df_sellers$sa_city <- str_to_title(df_sellers$sa_city)
df_sellers$sa_city <- stri_trans_general(df_sellers$sa_city, "Latin-ASCII")
df_sellers$sa_city <- str_trim(df_sellers$sa_city)


## Creating the variable total_shipping_days:
df_sellers <- df_sellers %>%
  mutate(total_shipping_days = sf_shipping_days + sf_extra_transit_time)

df_sellers$sf_shipping_days <- NULL
df_sellers$sf_extra_transit_time <- NULL


## Converting ds_cashflow to numeric
df_sellers$ds_cashflow <- as.numeric(df_sellers$ds_cashflow)
df_sellers$df_portfolio_cadastravel <- as.numeric(df_sellers$df_portfolio_cadastravel)
df_sellers$df_portfolio_lojista <- as.numeric(df_sellers$df_portfolio_lojista)


## Replacing NA to zero in numeric variables
df_sellers <- df_sellers %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))


## Creating the variable is_churned

df_sellers <- df_sellers %>%
  mutate(churn_date = ifelse(!is.na(last_order),
                             as.Date(last_order) + (PERIOD_IN_DAYS / 2) - 1,
                             as.Date(ss_created_at) + (PERIOD_IN_DAYS / 2) - 1)) %>%
  mutate(churn_date = as.Date(as.POSIXct.Date(churn_date)),
         inactivity_days = ifelse(!is.na(last_order),
                                  as.numeric(DATE_OF_CUT - last_order),
                                  as.numeric(DATE_OF_CUT - as.Date(ss_created_at)))) %>%
  mutate(is_churned = ifelse(churn_date < DATE_OF_CUT, 1, 0))













         












