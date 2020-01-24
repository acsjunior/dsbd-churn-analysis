#--------------------------------------------------------------------------------------------------
# Script to get data from AWS Athena
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
source("Scripts/Conn/conn-athena.R")
library(readr)
library(stringr)
library(dplyr)

#--------------------------------------------------------------------------------------------------
# Functions to get data:

get_data_by_sql <- function(sql) {
  out <- str_replace_all(sql, "[\r\n\t]", " ")
  out <- get_data(out)
  return(out)
}

get_sellers <- function() {
  sql <- read_file("Scripts/SQL/sellers.sql")
  out <- get_data_by_sql(sql)
  return(out)
}

get_sellers_block_history <- function() {
  sql <- read_file("Scripts/SQL/sellers-block-history.sql")
  out <- get_data_by_sql(sql)
  return(out)
}

get_sellers_nps_history <- function() {
  sql <- read_file("Scripts/SQL/sellers-nps-history.sql")
  out <- get_data_by_sql(sql)
  return(out)
}

get_sellers_churn_history <- function() {
  sql <- read_file("Scripts/SQL/sellers-churn-history.sql")
  out <- get_data_by_sql(sql)
  return(out)
}

get_deals_sellers <- function() {
  sql <- read_file("Scripts/SQL/deals-sellers.sql")
  out <- get_data_by_sql(sql)
  return(out)
}

get_issues_claims <- function() {
  sql <- read_file("Scripts/SQL/issues-claims.sql")
  out <- get_data_by_sql(sql)
  return(out)
}

get_order_item <- function() {
  sql <- read_file("Scripts/SQL/orderitems.sql")
  out <- get_data_by_sql(sql)
  return(out)
}