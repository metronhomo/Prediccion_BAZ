library(dplyr)
library(stringr)

duplicated2 <- function(df){ 
  duplicated(df) | duplicated(df, fromLast = TRUE)}

datos <- readRDS("../../output/SFTP/crédito y captación.RDS")
names(datos) <- gsub(" ", "_", names(datos))

datos$credit_or_savings <- as.integer(is.na(datos$zip_code))
datos$zip_code2 <- as.character(datos$zip_code)
idx <- is.na(datos$zip_code)
datos$zip_code2[idx] <- datos$cred_zip_code[idx]
datos2 <- datos %>%
  mutate(zip_code = gsub("\"", "", zip_code2),
         zip_code = str_pad(zip_code, 5, pad = "0"),
         gender = gsub("(.*)M(.*)", "M", as.character(gender)),
         gender = gsub("(.*)F(.*)", "F", gender),
         credit_or_savings = gsub("1", "credit", credit_or_savings),
         credit_or_savings = gsub("0", "savings", credit_or_savings),
         savings_aplication_date = as.Date(savings_aplication_date,"%d/%m/%y"),
         age_C=cut(age,
                   breaks=c(0,17,25,35,45,55,65,Inf),
                   labels=c(NA,"18-25","26-35","36-45","46-55","56-65",">65"))) %>%
  select(-zip_code2, -cred_zip_code)
idx <- datos2$zip_code == "00000"
datos2$zip_code[idx] <- NA

rm(datos)

CPs <- read.csv("../../data/Geo/CPdescarga.txt", sep = "|", colClasses = "character")
Centroides_CP <- readRDS("../../output/Geo/Centroides_CP.RDS")

idx <- !(datos2$zip_code %in% Centroides_CP$CP)
sum(idx) #Número de clientes cuyo CP no está en la base de información

Clientes_sin_info_CP <- datos2[idx, c("id", "zip_code", "credit_or_savings")]

tbl <- Clientes_sin_info_CP %>%
  group_by(zip_code) %>% 
  mutate(n_credit = sum(credit_or_savings == "credit"),
         n_savings = sum(credit_or_savings == "savings"),
         n = n()) %>% 
  ungroup() %>% 
  select(-credit_or_savings, -id) %>% 
  distinct() %>% 
  arrange(desc(n)) %>% 
  mutate(p_credit = n_credit/n,
         p_savings = n_savings/n)# %>% 
#   left_join(select(datos2, zip_code, state)) %>% 
#   mutate(state = gsub("\"", "", state))

saveRDS(tbl, "../../output/postal_code_google_api/CPs_sin_info.RDS")

# CPs faltantes con Google ------------------------------------------------

# http://stackoverflow.com/questions/3257441/geocoding-in-r-with-google-maps

library(RCurl)
library(RJSONIO)
library(parallel)

construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

gGeoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- construct.geocode.url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    return(c(lat, lng, "OK"))
  } else {
    return(c(NA,NA, x$status))
  }
}

getGeoCode <- function(address, verbose = FALSE){
  tryCatch(gGeoCode(address, verbose), 
           error = function(err){
             # error handler picks up where error was generated
             print(paste("MY_ERROR:  ",err))
             f <- "error"
             return(f)
           })}


gGeoCode("Mexico 56367")

zip_codes <- tbl$zip_code[!is.na(tbl$zip_code)]

zips <- paste("Mexico codigo postal", zip_codes)
#zips <- head(zips, 30)
zips_geocode <- mclapply(zips, function(x) getGeoCode(x, TRUE), mc.cores = 3)
names(zips_geocode) <- zip_codes[1:length(zips_geocode)]

zips_geocode_dataframe <- as.data.frame(t(as.data.frame(zips_geocode)))
names(zips_geocode_dataframe) <- c("lat", "lon", "status")
zips_geocode_dataframe$CP <- substr(zips, nchar(zips)-4, nchar(zips))
row.names(zips_geocode_dataframe) <- NULL

zips_geocode_dataframe_status_ok <- zips_geocode_dataframe %>% 
  filter(status == 'OK')

zips_geocode_dataframe_status_ok_not_repeated <- zips_geocode_dataframe_status_ok %>% 
  filter(lat != 20.0082646, lon != -100.7184702)

saveRDS(zips_geocode_dataframe, 
        "../../output/postal_code_google_api/zips_geocode_dataframe_all.RDS")

saveRDS(zips_geocode_dataframe_status_ok_not_repeated, 
        "../../output/postal_code_google_api/zips_geocode_dataframe_status_ok_not_repeated.RDS")

## Faltantes por límite de query:

zips_geocode_dataframe_missing <- zips_geocode_dataframe %>% filter(status == 'OVER_QUERY_LIMIT')

saveRDS(zips_geocode_dataframe_missing, 
        "../../output/postal_code_google_api/zips_geocode_dataframe_missing.RDS")

zips_miss <- zips_geocode_dataframe_missing$CP


zips <- paste("Mexico codigo postal", zips_miss)
zips_geocode <- mclapply(zips, function(x) getGeoCode(x, TRUE), mc.cores = 3)
names(zips_geocode) <- zip_codes[1:length(zips_geocode)]

zips_geocode_dataframe <- as.data.frame(t(as.data.frame(zips_geocode)))
names(zips_geocode_dataframe) <- c("lat", "lon", "status")
zips_geocode_dataframe$CP <- substr(zips, nchar(zips)-4, nchar(zips))
row.names(zips_geocode_dataframe) <- NULL

zips_geocode_dataframe_status_ok <- zips_geocode_dataframe %>% 
  filter(status == 'OK')

zips_geocode_dataframe_status_ok_not_repeated <- zips_geocode_dataframe_status_ok %>% 
  filter(lat != 20.0082646, lon != -100.7184702)

zips_geocode_dataframe_status_ok_not_repeated2 <- rbind(readRDS('../../output/postal_code_google_api/zips_geocode_dataframe_status_ok_not_repeated.RDS'),
                                                        zips_geocode_dataframe_status_ok_not_repeated)

saveRDS(zips_geocode_dataframe_status_ok_not_repeated2, 
        file = '../../output/postal_code_google_api/zips_geocode_dataframe_status_ok_not_repeated2.RDS')

