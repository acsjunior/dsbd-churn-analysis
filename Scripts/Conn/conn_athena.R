#-----------------------------------------------------------------------------------------------------------------------------------
# Script to get data from AWS Athena
#
#                                                                                                                         A. Júnior 
#                                                                                                          antonio.junior@olist.com
#                                                                                                                      BS&A | Olist
#                                                                                                2019-Nov-22 | Curitiba/PR - Brazil
#-----------------------------------------------------------------------------------------------------------------------------------

get_credentials <- function() {
  AWS_CREDENTIALS <- fromJSON("Scripts/Conn/aws_credentials.json")
  return(AWS_CREDENTIALS)
}

get_conn <- function() {

  AWS_CREDENTIALS <- get_credentials()

  conn <- dbConnect(RAthena::athena(),
                   aws_access_key_id = AWS_CREDENTIALS$access_key,
                   aws_secret_access_key = AWS_CREDENTIALS$secret_key,
                   s3_staging_dir = AWS_CREDENTIALS$s3_dir)

  return(conn)
}

get_data <- function(query) {
  res <- dbSendQuery(get_conn(), query)
  df <- as.data.frame(dbFetch(res))
  return(df)
}


