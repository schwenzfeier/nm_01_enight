
setwd('~/Documents/projects/nm_01_enight')

library(rgdal)
library(broom)
library(dplyr)



### READ IN 2020 PRES RESULTS
na.zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

library(readxl)
tab_names <- excel_sheets(path = 'FED Results Precinct_pres_2020.xlsx')
dat_list <- lapply(tab_names, function(x) read_excel(path = 'FED Results Precinct_pres_2020.xlsx', sheet = x,  skip = 5, na = c('', '*')))
dat_counties <- list()
for(i in 1:length(dat_list)){
  dat_counties[[i]] <- rep(tab_names[[i]], nrow(dat_list[[i]]))
}

dat <- do.call("rbind", dat_list)
dat$county <- unlist(dat_counties)
dat$office <- 'President'
dat$year <- 2020
dat$dem <- na.zero(as.numeric(dat$`JOSEPH R BIDEN AND KAMALA D HARRIS`))
dat$rep <- na.zero(as.numeric(dat$`DONALD J TRUMP AND MIKE PENCE`))


dat$total_votes <- dat$dem + dat$rep + 
  na.zero(as.numeric(dat$`JO JORGENSEN AND JEREMY "SPIKE" COHEN`)) + 
  na.zero(as.numeric(dat$`SHEILA "SAMM" TITTLE AND DAVID CARL SANDIGE`)) + 
               na.zero(as.numeric(dat$`HOWIE HAWKINS AND ANGELA NICOLE WALKER`)) + 
                            na.zero(as.numeric(dat$`GLORIA LA RIVA AND SUNIL FREEMAN`)) 

dat_pres_20 <- dat

### READ IN 2020 HOUSE RESULTS
tab_names <- excel_sheets(path = 'FED Results Precinct_house_2020.xlsx')
dat_list <- lapply(tab_names, function(x) read_excel(path = 'FED Results Precinct_house_2020.xlsx', sheet = x,  skip = 5, na = c('', '*')))
dat_counties <- list()
for(i in 1:length(dat_list)){
  dat_counties[[i]] <- rep(tab_names[[i]], nrow(dat_list[[i]]))
}

dat <- do.call("rbind", dat_list)
dat$county <- unlist(dat_counties)
dat$office <- 'House'
dat$year <- 2020
dat$dem <- na.zero(as.numeric(dat$`DEB HAALAND`))
dat$rep <- na.zero(as.numeric(dat$`MICHELLE GARCIA HOLMES`))

dat$total_votes <- dat$dem + dat$rep 

dat_house_20 <- dat


#################################
### READ IN 2021 HOUSE RESULTS
#################################
now_dat <- read.csv('~/Downloads/nm_01_hand_entered - Sheet1.csv')


### JOIN 2GETHER
dat_house_20$dem_house_20 <- dat_house_20$dem
dat_house_20$rep_house_20 <- dat_house_20$rep
dat_house_20$total_house_20 <- dat_house_20$total_votes

dat_pres_20$dem_pres_20 <- dat_pres_20$dem
dat_pres_20$rep_pres_20 <- dat_pres_20$rep
dat_pres_20$total_pres_20 <- dat_pres_20$total_votes

dat <- dat_house_20 %>% select(county, Precinct, dem_house_20, rep_house_20, total_house_20) %>%
  left_join(dat_pres_20 %>% select(county, Precinct, dem_pres_20, rep_pres_20, total_pres_20) )
dat$dem_house_2way <- dat$dem_house_20 / (dat$dem_house_20 + dat$rep_house_20)
dat$dem_pres_2way <- dat$dem_pres_20 / (dat$dem_pres_20 + dat$rep_pres_20)

# drop precincts labeled "TOTALS"
dat <- dat %>% filter(Precinct != 'TOTALS')

# plot
ggplot(dat) + geom_point(aes(x = dem_pres_2way, y = dem_house_2way)) + 
  geom_abline(aes(intercept = 0, slope = 1), col = 'red') + 
  xlim(0,1) + ylim(0,1)

sum(dat$dem_house_20)/sum(dat$total_house_20) # 58% 2way Haaland
sum(dat$dem_pres_20)/sum(dat$total_pres_20) # 61% 2way Biden

                          
    
# ##########################
# # READ IN SHAPEFILE
# ##########################
# dat <- readOGR( 
#   dsn= "new_mexico_precincts" , 
#   layer="new_mexico_precincts",
#   verbose=FALSE
# )
# 
# 
# 
# dat <- tidy(dat, region = "NAME10")
# 
# # Plot it
# library(ggplot2)
# ggplot() +
#   geom_polygon(data = dat, aes( x = long, y = lat, group = group), fill="#69b3a2", color="white") +
#   theme_void() 


                          