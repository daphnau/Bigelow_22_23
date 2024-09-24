dir()
####### Good Luck#######

##home###
setwd("C:/Users/dafna/Desktop/Bigelow_22_23/data/raw_data")
##work##
setwd("C:/Users/Daphnau/Box/Daphna Uni/Bigelow/all toghther/ORIGINAL DATA")
dir()
install.packages(tidyr)
library(tidyr)
library(lubridate)
library(dplyr)
library(readxl)
library(ggplot2)
library(reshape2)
library(reshape2) 
library(gridExtra)
# install.packages("bigleaf")
library(bigleaf)
#work####SOURCE IS LOADING A FUNCTION FOR ROUNDING TIMES###
source("C:/Users/Daphnau/Box/Daphna Uni/Bigelow/source/round.POSIXct.R")
##home#######SOURCE IS LOADING A FUNCTION FOR ROUNDING TIMES###
source("C:/Users/dafna/Box/Daphna Uni/Bigelow/source/round.POSIXct.R")


##sap flow ### 2022###
custom_headers_ <- c("time", "Average_SWP",  "Average_PP","Average_DF") 
sf_data_2022 <- read.csv('MB_SapFlow_SpeciesAverage_2022.csv', header = FALSE, col.names = custom_headers_, sep = ',')

#####changing column 1 to date and time####
doy<- floor(sf_data_2022$time)
min_fraction<-sf_data_2022$time-doy
min_minutes<-round((min_fraction*24)*60, digits = 0)
hrs <- as.integer(floor(min_minutes/60))
mins <- min_minutes %% 60
time_stamp<- paste0(hrs,":",mins, ":00")
date_stamp<- as.Date(x= (doy-1),origin = "2022-01-01")
sf_data_2022$date_time<-ymd_hms(paste0(date_stamp," ", time_stamp))
sf_data_2022$date_time_round<- round.POSIXct(sf_data_2022$date_time,"30 min")

##sap flow### 2023
sf_data_2023 <- read.csv('MB_SapFlow_SpeciesAverage_2023.csv', header = FALSE, col.names = custom_headers_, sep = ',')
#####changing column 1 to date and time####
doy<- floor(sf_data_2023$time)
min_fraction<-sf_data_2023$time-doy
min_minutes<-round((min_fraction*24)*60, digits = 0)
hrs <- as.integer(floor(min_minutes/60))
mins <- min_minutes %% 60
time_stamp<- paste0(hrs,":",mins, ":00")
date_stamp<- as.Date(x= (doy-1),origin = "2023-01-01")
sf_data_2023$date_time<-ymd_hms(paste0(date_stamp," ", time_stamp))
sf_data_2023$date_time_round<- round.POSIXct(sf_data_2023$date_time,"30 min")

###marging 2022 2023###
sf_data <- bind_rows(sf_data_2022, sf_data_2023)

column_index <- which(names(sf_data) == "date_time_round")

names(sf_data)[column_index] <- "Time"

###AVARAGE SAPFLOW FOT THREE SP.###
sf_data<- sf_data%>%
  mutate(mean_sap_flow = rowMeans(select(., Average_SWP, Average_PP, Average_PP ), na.rm = TRUE))

##########################################################################################

####TOWER DATA####
tower_data_2022 <- read.csv('GapfilledPartitionedFluxes_US-Mtb_HH_202201010000_202301010000.csv', sep = ',',
                            na.strings = c("-9999", "-9999.000"))
tower_data_2023 <- read.csv('GapfilledPartitionedFluxes_US-Mtb_HH_202301010000_202401010000.csv', sep = ',',
                            na.strings = c("-9999", "-9999.000"))
tower_data_2022$TIMESTAMP_START <- ymd_hm(tower_data_2022$TIMESTAMP_START)
tower_data_2023$TIMESTAMP_START <- ymd_hm(tower_data_2023$TIMESTAMP_START)      

tower_data_2022$TA_1_3_1[!is.finite(tower_data_2022$TA_1_3_1) | tower_data_2022$TA_1_3_1 == -9999.000] <- NA
tower_data_2023[tower_data_2023 == -9999.000] <- NA

tower_data<- bind_rows(tower_data_2022, tower_data_2023)
column_index <- which(names(tower_data) == "TIMESTAMP_START")

names(tower_data)[column_index] <- "Time"
#############################################################################################

tower_data$top_soil_swc_N <- rowMeans(tower_data[, c("SWC_1_1_1", "SWC_1_2_1", "SWC_1_3_1")], na.rm = TRUE)
tower_data$top_soil_swc_E <- rowMeans(tower_data[, c("SWC_2_1_1", "SWC_2_2_1", "SWC_2_3_1")], na.rm = TRUE)
tower_data$top_soil_swc_S <- rowMeans(tower_data[, c("SWC_3_1_1", "SWC_3_2_1", "SWC_3_3_1")], na.rm = TRUE)

tower_data<- tower_data%>%
  mutate(mean_swc_average = rowMeans(select(., top_soil_swc_N, top_soil_swc_E, top_soil_swc_S ), na.rm = TRUE))


latent_heat_of_vaporization_J_per_kg <- 2.45 * 10^6  # Approximately 2.45 MJ/kg

# Calculate ET based on LE
tower_data$ET <- tower_data$LE / latent_heat_of_vaporization_J_per_kg

tower_data$ET_ <-tower_data$ET*3600

tower_data$ET_mm_h <- LE.to.ET(tower_data$LE, Tair = tower_data$TA_1_4_1)
tower_data$ET_mm_day <- tower_data$ET_mm_h * 24
####
# Convert ET from mm/h to µmol H2O m⁻² s⁻¹
# 1 mm/h of ET is equivalent to 277.78 µmol H2O m⁻² s⁻¹
tower_data$ET_umol <- tower_data$ET_mm_h * 277.78

# Calculate Water Use Efficiency (WUEe)
tower_data$WUEe <- tower_data$GPP / tower_data$ET_umol
tower_data$WUE <- tower_data$GPP / tower_data$ET_

tower_data_selected<- tower_data %>% select(Time, TA_1_3_1, TA_1_4_1,RH_1_3_1, RH_1_4_1,VPD_1_3_1,
                                             VPD_1_4_1,PPFD_IN,P_1_1_1,P_2_1_1,mean_swc_average,
                                             GPP,ET_mm_day,ET,ET_mm_h,WUEe,WUE )


###THERMAL DATA###
##############adding thremal to sapflow and tower data####sf_flux_daytime_mo
dir()
thermal_2022_AD <- read.csv('thermal_AD__22.csv', sep = ','   )    
thermal_2023_AD <- read.csv('thermal_AD__23.csv', sep = ','   )
thermal_ad<-rbind(thermal_2022_AD,thermal_2023_AD)

thermal_ad_22_23 <- thermal_ad %>%
  select(filename, roi_temp_mean, roi_temp_sd, roi_temp_min, roi_temp_max, datetime) %>%
  mutate(Time = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))


###########################################################

###PRI DATA####
file1 <- read_excel("06-02574 11Oct22-1455_file1.xlsx")
file1$`06-02574` <- as.POSIXct(file1$`06-02574`, origin = "1970-01-01", tz = "UTC")

file2 <- read_excel("06-02574 26May23-1259_file2.xlsx")
file2$`06-02574` <- as.POSIXct(file2$`06-02574`, origin = "1970-01-01", tz = "UTC")

file3 <- read_excel("06-02574 04Dec23-1114_file3.xlsx")
file3$`06-02574` <- as.POSIXct(file3$`06-02574`, origin = "1970-01-01", tz = "UTC")

merged_data <- bind_rows(file1, file2, file3)


merged_data <- merged_data %>%
  mutate(Date = as.Date(`06-02574`),
         Time = format(as.POSIXct(`06-02574`), format = "%H:%M:%S"))

##calculating reflectance (P) for PRI by dividing radiance (from the sun)/ Irradiance (reflected)###

###convert to numeric###
print(class(merged_data$`Port 3...10`))

columns_to_numeric <- c("Port 1...2", "Port 1...3","Port 1...4","Port 1...5", "Port 2...6","Port 2...7","Port 2...8","Port 2...9","Port 3...10",
                        "Port 3...11","Port 3...12", "Port 3...13","Port 4...14","Port 4...15","Port 4...16","Port 4...17","Port 5...18",
                        "Port 5...19", "Port 5...20", "Port 5...21","Port 6...22","Port 6...23","Port 6...24","Port 6...25",
                        "Port 7...26","Port 7...27","Port 8...28","Port 8...29")

###create numeric table - md (merged_data)####
md <- merged_data %>%
  mutate(across(all_of(columns_to_numeric), as.numeric))
###check###
print(class(md$`Port 3...10`)) 

#########PORT4 OVER PORT 3##############
####reflectance for 532mn####
md <- md %>%
  mutate(P532_PORT4 =  `Port 4...14` / `Port 3...10`)
###reflectence for 570mn###

md <- md %>%
  mutate(P570_PORT4 =  `Port 4...15` / `Port 3...11`)
###calculate PRI FOR PORT 4#####
md <- md %>%
  mutate(PRI_PORT4 = (P532_PORT4-P570_PORT4)/ (P532_PORT4+P570_PORT4))

###PORT6 OVER PORT 3###########

####reflectance for 532mn####
md <- md %>%
  mutate(P532_PORT6 =  `Port 6...22`/ `Port 3...10`)
###reflectence for 570mn###

md <- md %>%
  mutate(P570_PORT6 =  `Port 6...23` / `Port 3...11`)
###calculate PRI FOR PORT 4#####
md <- md %>%
  mutate(PRI_PORT6 = (md$P532_PORT6-md$P570_PORT6)/ (md$P532_PORT6+md$P570_PORT6))

###NDVI_PORT5##
#################################################################################################
####reflectance for 810mn####
md <- md %>%
  mutate(P810_PORT5 =  `Port 5...19` / `Port 1...3`)
###reflectence for 650mn###

md <- md %>%
  mutate(P650_PORT5 =  `Port 5...18` / `Port 1...2`)
###calculate NDVI_PORT5#####
md <- md %>%
  mutate(NDVI_PORT5 = (`P810_PORT5`-`P650_PORT5`)/ (`P810_PORT5`+`P650_PORT5`))

###NDVI_PORT2##
#################################################################################################
####reflectance for 810mn####
md <- md %>%
  mutate(P810_PORT2 =  `Port 2...7` / `Port 1...3`)
###reflectence for 650mn###

md <- md %>%
  mutate(P650_PORT2 =  `Port 2...6` / `Port 1...2`)
###calculate NDVI_PORT2#####
md <- md %>%
  mutate(NDVI_PORT2 = (`P810_PORT2`-`P650_PORT2`)/ (`P810_PORT2`+`P650_PORT2`))    

#################
###NIRv_PORT5##
#################################################################################################
md <- md %>%
  mutate(NIRv_PORT5 = (md$NDVI_PORT5*md$P810_PORT5))

###NIRv_PORT2##
#################################################################################################
md <- md %>%
  mutate(NIRv_PORT2 = (md$NDVI_PORT2*md$P810_PORT2))

##################################
srs_data<- md %>%
  mutate(Time = as.POSIXct(md$`06-02574`), 
         minute = lubridate::minute(md$`06-02574`)) %>%
  filter(minute == 0 | minute == 30)

f_srs_data<- srs_data%>%
  dplyr::select(Date, Time,P532_PORT4,P570_PORT4, PRI_PORT4,P532_PORT6, P570_PORT6, 
                PRI_PORT6, P810_PORT5, P650_PORT5, NDVI_PORT5 , P810_PORT2 , P650_PORT2 , 
                NDVI_PORT2 , NIRv_PORT5 , NIRv_PORT2 ) 


################rgb############
##2022##
setwd('C:/Users/dafna/Desktop/Bigelow_22_23/data/raw_data')
load('VI.data.Rdata')
table22_larg<-do.call(rbind,VI.data)
table22_larg$ID<-row.names(table22_larg)
table22_larg$ID <- as.numeric(as.character(table22_larg$ID))
table22_larg$ID <- floor(table22_larg$ID)


load('VI.data23.Rdata')
table23_larg<-do.call(rbind,VI.data)
table23_larg$ID<-row.names(table23_larg)
table23_larg$ID <- as.numeric(as.character(table23_larg$ID))
table23_larg$ID <- floor(table23_larg$ID)

table22_larg <- table22_larg %>%
  mutate(GRVI =  (g.av-r.av)/ (g.av+r.av))
table22_larg <- table22_larg %>%
  mutate(GCC =  (g.av)/ (r.av+g.av+b.av))
table22_larg$origin <- "table22"

table23_larg <- table23_larg %>%
  mutate(GRVI =  (g.av-r.av)/ (g.av+r.av))
table23_larg <- table23_larg %>%
  mutate(GCC =  (g.av)/ (r.av+g.av+b.av))
table23_larg$origin <- "table23"

RGB_22_23_raw <- rbind(table22_larg, table23_larg)
RGB_22_23_raw<- RGB_22_23_raw  %>%
  filter(ID == 1)
################
##CREATING NORMELIZED VALUE###
start_date <- as.Date("2022-05-27")
end_date <- as.Date("2022-10-31")
# Trim the table to start from the specified date
table22_trimmed <- subset(table22_larg, date >= start_date& date <= end_date)

##NORMELIZE 2022##
min_gcc_value_22 <- min(table22_trimmed$GCC, na.rm = TRUE)
max_gcc_value_22 <- max(table22_trimmed$GCC, na.rm = TRUE)
min_grvi_value_22 <- min(table22_trimmed$GRVI, na.rm = TRUE)
max_grvi_value_22 <- max(table22_trimmed$GRVI, na.rm = TRUE)
table22_trimmed$normalized_gcc <- (table22_trimmed$GCC - min_gcc_value_22) / (max_gcc_value_22 - min_gcc_value_22)
table22_trimmed$normalized_grvi <- (table22_trimmed$GRVI - min_grvi_value_22) / (max_grvi_value_22 - min_grvi_value_22)


####NORMELIZED2023###
##NORMELIZE 2022##
min_gcc_value_23 <- min(table23_larg$GCC, na.rm = TRUE)
max_gcc_value_23 <- max(table23_larg$GCC, na.rm = TRUE)
min_grvi_value_23 <- min(table23_larg$GRVI, na.rm = TRUE)
max_grvi_value_23 <- max(table23_larg$GRVI, na.rm = TRUE)
table23_larg$normalized_gcc <- (table23_larg$GCC - min_gcc_value_23) / (max_gcc_value_23 - min_gcc_value_23)
table23_larg$normalized_grvi <- (table23_larg$GRVI - min_grvi_value_23) / (max_grvi_value_23 - min_grvi_value_23)

RGB_22 <- table22_trimmed %>%
  filter(ID == 1)

RGB_23 <- table23_larg %>%
  filter(ID == 1)



RGB_22_23_all <- rbind(RGB_22, RGB_23)

RGB_22_23 <- RGB_22_23_all %>%
  select(date, doy, GRVI, GCC, normalized_gcc, normalized_grvi) %>%
  mutate(Time = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))


# Save RGB_22_23 as a CSV file
write.csv(RGB_22_23, file = "RGB_with_normalization.csv", row.names = FALSE)

# Save RGB_22_23_raw as a CSV file
write.csv(RGB_22_23_raw, file = "RGB_raw.csv", row.names = FALSE)

# Save RGB_22_23_raw as a CSV file
write.csv(thermal_ad_22_23, file = "Thermal_allday.csv", row.names = FALSE)

# Save RGB_22_23_raw as a CSV file
write.csv(tower_data_selected, file = "tower_selected.csv", row.names = FALSE)

# Save RGB_22_23_raw as a CSV file
write.csv( f_srs_data, file = "SRS_selected.csv", row.names = FALSE)

# Save RGB_22_23_raw as a CSV file
write.csv(sf_data, file = "sap_flow.csv", row.names = FALSE)

thermal_tower <- inner_join(thermal_ad_22_23, tower_data_selected, by = "Time")

# Then, join the result with f_srs_data
thrmal_tower_srs <- inner_join(thermal_tower, f_srs_data, by = "Time")

# Then, join the result with f_srs_data
thrmal_tower_srs_sf <- inner_join(thrmal_tower_srs, sf_data, by = "Time")

# Finally, join the result with sf_data
all_data <- inner_join(thrmal_tower_srs_sf, RGB_22_23, by = "Time")


# Save RGB_22_23_raw as a CSV file
write.csv(thrmal_tower_srs_sf , file = "thrmal_tower_srs_sf ", row.names = FALSE)
write.csv(all_data , file = "all_data ", row.names = FALSE)
