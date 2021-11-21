library(stringr)
library(lubridate)
library(dplyr)

## range of site IDs to loop through (not all ids match a site)
seq <- 1:117

## loop through each ID
for (s in seq){
  tryCatch({
    
  s_yr <- 2009 ## define start year
  e_yr <- 2021 ## define end year
  
## make it a full date
  start_date <- paste0(min(as.numeric(s_yr)), "-01-01")

  ## use today as last day to fetch
  end_date <- Sys.Date()

  ## make a data frame out of the dates
  d8_df <- data.frame(date = seq(
    from=as.POSIXct(start_date, tz="UTC"),
    to=as.POSIXct(end_date, tz="UTC"),
    by="day"
  )  )
  
  d8 <- d8_df %>% 
    transmute(year = year(date),
              month = sprintf('%02d', month(date)),
              day = sprintf('%02d', day(date))) %>% 
    transmute(date = paste0(day, '-', month, '-', year))  
  
  ## create string of dates
  all_dayz <- unique(d8$date)
  all_dayz <- all_dayz[NROW(all_dayz):1]
  
  
  site <- list()
for (d in all_dayz){
  tryCatch({
  
df <- read.csv(file = paste0('http://solarheatdata.eu/modules/sol/histdata.asp?anlaeg=', s, "&fromdate=", d,"&todate=",d ,"&results=hours&csv=1"), 
              header = FALSE, sep = ';')

names(df) <- c('date', 'solar_heat_MWh', 'solar_heat_production_Whm2', 'solar_radiation_Whm2')

df$site_num <- s
# 
# t <- read.table(paste0('http://solarheatdata.eu/getpage.asp?vid=', s, '&page=prod&_=1634885606484'))
# 
# nam <- str_sub(t$V3[1], 7, -1)
# 
# df$site_name <- nam

site[[d]] <- df

print(paste0(s, '_', d))
flush.console()
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
  dat <- do.call(rbind, site)
  
  write.csv(dat, paste0("outputs/", s, '.csv'))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

