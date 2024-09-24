
setwd("C:/Users/dafna/Desktop/Bigelow_22_23/data/clean_data")
library(tidyr)
library(lubridate)
library(dplyr)
library(readxl)
library(ggplot2)
library(reshape2)
library(reshape2) 
library(gridExtra)



SF<-read.csv('sap_flow.csv', header = TRUE,  sep = ',')
THERMAL<-read.csv('Thermal_allday.csv', header = TRUE,  sep = ',')
TOWER<-read.csv('tower_selected.csv', header = TRUE,  sep = ',')
SRS<-read.csv('SRS_selected.csv', header = TRUE,  sep = ',')
RGB<-read.csv('RGB_with_normalization.csv', header = TRUE,  sep = ',')

all_together <- read.csv('all_data', header = TRUE,  sep = ',')
TSRT<- read.csv('thrmal_tower_srs_sf', header = TRUE,  sep = ',')

TSRT <- TSRT %>%
  mutate(
    tdiffcan_air =  roi_temp_mean-TA_1_3_1,  # Subtracting two columns
    sf_3 = rowMeans(cbind(Average_SWP, Average_PP, Average_DF), na.rm = TRUE)  # Calculating row-wise means
  )



#####each one saparatly###

TSRT$Time <- as.POSIXct(TSRT$Time)  # Or use as.Date if it's only a date
###all together, monthly weekly and daily###
TSRT$Year <- year(TSRT$Time)
TSRT$Month <- month(TSRT$Time)
TSRT$Week <- week(TSRT$Time)
TSRT$date <- format(TSRT$Time, "%Y-%m-%d") 
TSRT$doy <- yday(TSRT$Time)
TSRT$hour <- format(TSRT$Time, "%H:%M:%S") 


TSRT <- TSRT %>%
  mutate(Season = case_when(
    (doy >= 60 & doy <= 91) ~ "Spring",            # March 1st (DOY 60) to April 1st (DOY 91)
    (doy >= 121 & doy <= 171) ~ "Pre-monsoon",     # May 1st (DOY 121) to June 20th (DOY 171)
    (doy >= 172 & doy <= 253) ~ "Monsoon",         # June 21st (DOY 172) to September 10th (DOY 253)
    (doy >= 254 & doy <= 304) ~ "After-monsoon",   # September 11th (DOY 254) to October 31st (DOY 304)
    TRUE ~ "Winter"                                # All other days are Winter
  ))


TSRT_daytime<- TSRT %>%
  filter(hour >= "06:00:00" & hour <= "18:00:00")

TSRT_daytime_AFTER37_23<- TSRT_daytime %>%
  filter(Week >= 36 & Year =="2023")

#TSRT_daytime_AFTER37<- TSRT_daytime %>%
# filter(Week >= 37)

integration=  TSRT_daytime %>%
  group_by(Week, Year) %>%
  summarise(PRI_PORT4_90= quantile(PRI_PORT4,probs =0.90, na.rm = TRUE),
            PRI_PORT6_90= quantile(PRI_PORT6,probs =0.90, na.rm = TRUE),
            m_gpp=mean(GPP, na.rm=TRUE),
            m_et= mean(ET, na.rm=TRUE),
            M_WUE=mean(WUEe, na.rm=TRUE),
            m_diff_Air_can=mean(tdiffcan_air, na.rm= TRUE),
            mean_t= mean(TA_1_4_1, na.rm=TRUE),
            mean_canopy_t= mean( roi_temp_mean, na.rm=TRUE),
            m_sapflow=mean(sf_3, na.rm=TRUE),
            N= n(),
            se_PRI = sd(PRI_PORT4, na.rm = TRUE) / sqrt(N),  
            se_m_diff = sd(tdiffcan_air, na.rm = TRUE) / sqrt(N),
            se_m_sapflow = sd(sf_3, na.rm = TRUE) / sqrt(N),)
integration$date <- as.Date(integration$date)            


weekly_diff <- ggplot(integration, aes(x = Week, y = m_diff_Air_can, color = as.factor(Year))) +
  geom_line(linewidth=1.3) +
  geom_ribbon(aes(ymin =  m_diff_Air_can - se_m_diff , ymax =  m_diff_Air_can+ se_m_diff , fill = as.factor(Year)), alpha = 0.5) +
  labs(x = "week", y = "mean canopy temp- mean air temp ", title = "") +
  scale_color_manual(values = c("2022"= "blue", "2023"= "darksalmon")) +
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightcoral")) + 
  # scale_x_continuous(breaks = 26:44) + # Assuming 'month_name' is available
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.y = element_text(color = "black"),
        panel.grid = element_blank(),
        
        axis.text = element_text(size = 14)) 
weekly_diff

weekly_pri <- ggplot(integration, aes(x = Week, y = PRI_PORT6_90, color = as.factor(Year))) +
  geom_line(linewidth=1.3) +
  geom_ribbon(aes(ymin =  PRI_PORT6_90 - se_PRI , ymax =  PRI_PORT6_90+ se_PRI , fill = as.factor(Year)), alpha = 0.5) +
  labs(x = "week", y = "90 % quantile PRI ", title = "") +
  scale_color_manual(values = c("2022"= "blue", "2023"= "darksalmon")) +
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightcoral")) + 
  # scale_x_continuous(breaks = 26:44) + # Assuming 'month_name' is available
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.y = element_text(color = "black"),
        panel.grid = element_blank(),
        
        axis.text = element_text(size = 14)) 
weekly_pri 





####################
after37=  TSRT_daytime_AFTER37_23 %>%
  group_by(date) %>%
  summarise(PRI_PORT4_90= quantile(PRI_PORT4,probs =0.90, na.rm = TRUE),
            PRI_PORT6_90= quantile(PRI_PORT6,probs =0.90, na.rm = TRUE),
            m_gpp=mean(GPP, na.rm=TRUE),
            m_et= mean(ET, na.rm=TRUE),
            M_WUE=mean(WUEe, na.rm=TRUE),
            m_diff_Air_can=mean(tdiffcan_air, na.rm= TRUE),
            mean_t= mean(TA_1_4_1, na.rm=TRUE),
            mean_canopy_t= mean( roi_temp_mean, na.rm=TRUE),
            m_sapflow=mean(sf_3, na.rm=TRUE),
            N= n(),
            se_PRI = sd(PRI_PORT4, na.rm = TRUE) / sqrt(N),  
            se_m_diff = sd(tdiffcan_air, na.rm = TRUE) / sqrt(N),
            se_m_sapflow = sd(sf_3, na.rm = TRUE) / sqrt(N),)



integration_filtered <- after37 %>%
  filter(date != as.Date("2023-10-03")) 

library(scales)
integration_filtered$date <- as.Date(integration_filtered$date)

ggplot(integration_filtered, aes(x = date)) +
  
  geom_ribbon(aes(ymin = m_diff_Air_can - se_m_diff, ymax = m_diff_Air_can +  se_m_diff), fill = "green3", alpha = 0.2) +
  geom_ribbon(aes(ymin = m_sapflow - se_m_sapflow, ymax = m_sapflow + se_m_sapflow), fill = "blue3", alpha = 0.2) +
  geom_ribbon(aes(ymin = (PRI_PORT4_90 * 10) - (se_PRI * 10), ymax = (PRI_PORT4_90 * 10) + (se_PRI * 10)), fill = "red", alpha = 0.2) +
  
  geom_line(aes(y = m_diff_Air_can, color = "tdiff", group = 1), size = 1) +  # Line for tdiff
  geom_line(aes(y = m_sapflow, color = "Sap Flow", group = 1), size = 1) +  # Line for sap flow
  geom_line(aes(y = PRI_PORT4_90 * 10, color = "PRI", group = 1), size = 1) +  # Line for PRI
  
  scale_y_continuous(
    name = "Primary Y-Axis",  
    sec.axis = sec_axis(~./10, name = "PRI Values", breaks = seq(-1, 0, by = 0.2))  # Scale PRI and set breaks for the desired range
  ) +
  
  scale_x_date(
    date_breaks = "10 days",  # Breaks every 10 days
    date_labels = "%d %b"  # Format as "01 Sep", "10 Sep", etc.
  ) +
  
  scale_color_manual(
    values = c("tdiff" = "green3", "Sap Flow" = "blue3", "PRI" = "red")
  ) +
  
  # Labels and theme
  labs(x = "Date", color = "Legend") +
  theme_minimal() + 
  theme(panel.background = element_rect(fill = "white"),
        axis.text.y = element_text(color = "black"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12))








ggplot(integration_filtered, aes(x = date)) +
  
  geom_ribbon(aes(ymin = m_diff_Air_can - se_m_diff, ymax = m_diff_Air_can + se_m_diff), fill = "green3", alpha = 0.2) +
  geom_ribbon(aes(ymin = m_sapflow - se_m_sapflow, ymax = m_sapflow + se_m_sapflow), fill = "blue3", alpha = 0.2) +
  geom_ribbon(aes(ymin = (PRI_PORT4_90 * 10) - (se_PRI * 10), ymax = (PRI_PORT4_90 * 10) + (se_PRI * 10)), fill = "red", alpha = 0.2) +
  geom_line(aes(y = m_diff_Air_can, color = "tdiff"), size = 1) +  # Line for tdiff
  geom_line(aes(y = m_sapflow, color = "Sap Flow"), size = 1) +  # Line for sap flow
  geom_line(aes(y = PRI_PORT4_90 * 10, color = "PRI"), size = 1) +  # Line for PRI
  scale_y_continuous(
    name = "Primary Y-Axis",  
    sec.axis = sec_axis(~./10, name = "PRI Values",, limits = c(-1, 0))  # Scale PRI back for secondary y-axis
  ) +
  
  scale_color_manual(
    values = c("tdiff" = "green3", "Sap Flow" = "blue3", "PRI" = "red")
  ) +
  
  # Labels and theme
  labs(x = "Date", color = "Legend") +
  theme_minimal() + theme(panel.background = element_rect(fill = "white"),
                          axis.text.y = element_text(color = "black"),
                          panel.grid = element_blank(),
                          axis.text = element_text(size = 12)) 
theme(legend.position = "right")



TSRT_daytime_AFTER37<- TSRT_daytime %>%
  filter(Week >= 36 )



hourly_daytime= TSRT_daytime %>%
  group_by(Week, hour,Year) %>%
  summarise(PRI_PORT4_90= quantile(PRI_PORT4,probs =0.90, na.rm = TRUE),
            PRI_PORT6_90= quantile(PRI_PORT6,probs =0.90, na.rm = TRUE),
            m_gpp=mean(GPP, na.rm=TRUE),
            m_et= mean(ET, na.rm=TRUE),
            M_WUE=mean(WUEe, na.rm=TRUE),
            m_diff_Air_can=mean(tdiffcan_air, na.rm= TRUE),
            mean_t= mean(TA_1_4_1, na.rm=TRUE),
            mean_canopy_t= mean( roi_temp_mean, na.rm=TRUE),
            m_sapflow=mean(sf_3, na.rm=TRUE),
            N= n(),
            se_PRI = sd(PRI_PORT4, na.rm = TRUE) / sqrt(N),  
            se_m_diff = sd(tdiffcan_air, na.rm = TRUE) / sqrt(N),
            se_m_sapflow = sd(sf_3, na.rm = TRUE) / sqrt(N))


hourly_daytime_week35<- hourly_daytime %>%
  filter(Week == "35" )


hourly_daytime_week40<- hourly_daytime %>%
  filter(Week == "41" ) 


sf_35 <- ggplot(hourly_daytime_week35, aes(x = hour, y = m_sapflow,color = as.factor(Year), group = Year)) +
  geom_line(linewidth=1.3) +
  geom_ribbon(aes(ymin =  m_sapflow -  se_m_sapflow, ymax =  m_sapflow +   se_m_sapflow, fill = as.factor(Year)), alpha = 0.5) +
  labs(x = "hour", y = "Sap flow ", title = "WEEK 35") +
  scale_color_manual(values = c("2022"= "blue", "2023"= "darksalmon")) +
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightcoral")) + 
  scale_x_discrete(breaks = c("06:00:00", "09:00:00", "12:00:00", "15:00:00", "18:00:00")) +  # Show only every 3 hours
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.y = element_text(color = "black"),
        panel.grid = element_blank(),
        
        axis.text = element_text(size = 12)) 
sf_35


sf_40 <- ggplot(hourly_daytime_week40, aes(x = hour, y = m_sapflow,color = as.factor(Year), group = Year)) +
  geom_line(linewidth=1.3) +
  geom_ribbon(aes(ymin =  m_sapflow -  se_m_sapflow, ymax =  m_sapflow +   se_m_sapflow, fill = as.factor(Year)), alpha = 0.5) +
  labs(x = "hour", y = "sap flow ", title = "WEEK 40") +
  scale_color_manual(values = c("2022"= "blue", "2023"= "darksalmon")) +
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightcoral")) + 
  scale_x_discrete(breaks = c("06:00:00", "09:00:00", "12:00:00", "15:00:00", "18:00:00")) +  # Show only every 3 hours
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.y = element_text(color = "black"),
        panel.grid = element_blank(),
        
        axis.text = element_text(size = 12)) 
sf_40



DIFF_35 <- ggplot(hourly_daytime_week35, aes(x = hour, y = m_diff_Air_can,color = as.factor(Year), group = Year)) +
  geom_line(linewidth=1.3) +
  geom_ribbon(aes(ymin =  m_diff_Air_can -se_m_diff, ymax =  m_diff_Air_can +se_m_diff, fill = as.factor(Year)), alpha = 0.5) +
  labs(x = "hour", y = "Tdiff ", title = "WEEK 35") +
  scale_color_manual(values = c("2022"= "blue", "2023"= "darksalmon")) +
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightcoral")) + 
  scale_x_discrete(breaks = c("06:00:00", "09:00:00", "12:00:00", "15:00:00", "18:00:00")) +  # Show only every 3 hours
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.y = element_text(color = "black"),
        panel.grid = element_blank(),
        
        axis.text = element_text(size = 12)) +
  ylim(-3,3)
DIFF_35


DIFF_40 <- ggplot(hourly_daytime_week40, aes(x = hour, y = m_diff_Air_can,color = as.factor(Year), group = Year)) +
  geom_line(linewidth=1.3) +
  geom_ribbon(aes(ymin =  m_diff_Air_can -se_m_diff, ymax =  m_diff_Air_can +se_m_diff, fill = as.factor(Year)), alpha = 0.5) +
  labs(x = "hour", y = "Tdiff ", title = "WEEK 40") +
  scale_color_manual(values = c("2022"= "blue", "2023"= "darksalmon")) +
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightcoral")) + 
  scale_x_discrete(breaks = c("06:00:00", "09:00:00", "12:00:00", "15:00:00", "18:00:00")) +  # Show only every 3 hours
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.y = element_text(color = "black"),
        panel.grid = element_blank(),
        
        axis.text = element_text(size = 12)) +
  ylim(-3,3)
DIFF_40


all_together<- all_together %>%
  mutate(
    tdiffcan_air =  roi_temp_mean-TA_1_3_1,  # Subtracting two columns
    sf_3 = rowMeans(cbind(Average_SWP, Average_PP, Average_DF), na.rm = TRUE)  # Calculating row-wise means
  )



#####each one saparatly###

all_together$Time <- as.POSIXct(all_together$Time)  # Or use as.Date if it's only a date
###all together, monthly weekly and daily###
all_together$Year <- year(all_together$Time)
all_together$Month <- month(all_together$Time)
all_together$Week <- week(all_together$Time)
all_together$date <- format(all_together$Time, "%Y-%m-%d") 
all_togetherdoy <- yday(all_together$Time)
all_together$hour <- format(all_together$Time, "%H:%M:%S") 



#############################
weekly=  all_together %>%
  group_by(Week,Year) %>%
  summarise(PRI_PORT4_90= quantile(PRI_PORT4,probs =0.90, na.rm = TRUE),
            PRI_PORT6_90= quantile(PRI_PORT6,probs =0.90, na.rm = TRUE),
            m_gpp=mean(GPP, na.rm=TRUE),
            m_et= mean(ET, na.rm=TRUE),
            m_WUE=mean(WUEe, na.rm=TRUE),
            Tdiff=mean(tdiffcan_air, na.rm= TRUE),
            mean_t= mean(TA_1_4_1, na.rm=TRUE),
            mean_canopy_t= mean( roi_temp_mean, na.rm=TRUE),
            m_sapflow=mean(sf_3, na.rm=TRUE),
            NDVI_PORT2_90= quantile(NDVI_PORT2,probs =0.90, na.rm = TRUE),
            NDVI_PORT5_90= quantile(NDVI_PORT5,probs =0.90, na.rm = TRUE),
            GCC_N= quantile(normalized_gcc,probs =0.90, na.rm = TRUE),
            GCC= quantile(GCC,probs =0.90, na.rm = TRUE),
            N= n(),
            se_PRI = sd(PRI_PORT4, na.rm = TRUE) / sqrt(N),  
            se_m_sapflow = sd(sf_3, na.rm = TRUE) / sqrt(N),
            se_GPP = sd(GPP, na.rm = TRUE) / sqrt(N),  
            se_ET = sd(ET, na.rm = TRUE) / sqrt(N),
            se_WUE = sd(WUEe, na.rm = TRUE) / sqrt(N),  
            se_Tdiff= sd(tdiffcan_air, na.rm = TRUE) / sqrt(N),
            se_NDVI2 = sd(NDVI_PORT2, na.rm = TRUE) / sqrt(N),
            se_NDVI5 = sd(NDVI_PORT5, na.rm = TRUE) / sqrt(N),
            se_GCCn = sd(normalized_gcc, na.rm = TRUE) / sqrt(N),  
            se_GCC= sd(GCC, na.rm = TRUE) / sqrt(N)
            
            )

weekly$Week <- as.numeric(as.character(weekly$Week))

weekly_diff <- ggplot(weekly, aes(x = Week, y =  Tdiff, color = as.factor(Year))) +
  geom_line(linewidth=1.3) +
  geom_ribbon(aes(ymin =  Tdiff -se_Tdiff, ymax =  Tdiff+ se_Tdiff, fill = as.factor(Year)), alpha = 0.5) +
  labs(x = "week", y = "mean canopy temp- mean air temp ", title = "") +
  scale_color_manual(values = c("2022"= "blue", "2023"= "darksalmon")) +
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightcoral")) + 
 # scale_x_continuous(breaks = 26:44) + # Assuming 'month_name' is available
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.y = element_text(color = "black"),
        panel.grid = element_blank(),
        
        axis.text = element_text(size = 12)) 
weekly_diff

se_Tdiff



weekly_GCC <- ggplot(weekly, aes(x = Week, y =  PRI_PORT4_90, color = as.factor(Year))) +
  geom_line(linewidth=1.3) +
  geom_ribbon(aes(ymin =  PRI_PORT4_90-se_PRI, ymax =  PRI_PORT4_90+ se_PRI, fill = as.factor(Year)), alpha = 0.5) +
  labs(x = "week", y = "mean canopy temp- mean air temp ", title = "") +
  scale_color_manual(values = c("2022"= "blue", "2023"= "darksalmon")) +
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightcoral")) + 
  # scale_x_continuous(breaks = 26:44) + # Assuming 'month_name' is available
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.y = element_text(color = "black"),
        panel.grid = element_blank(),
        
        axis.text = element_text(size = 12)) 

weekly_GCC 



weekly_PRI <- ggplot(weekly, aes(x = Week, y =  GCC_N, color = as.factor(Year))) +
  geom_line(linewidth=1.3) +
  geom_ribbon(aes(ymin =  GCC_N -se_GCCn, ymax =  GCC_N+ se_GCCn, fill = as.factor(Year)), alpha = 0.5) +
  labs(x = "week", y = "mean canopy temp- mean air temp ", title = "") +
  scale_color_manual(values = c("2022"= "blue", "2023"= "darksalmon")) +
  scale_fill_manual(values = c("2022" = "lightblue", "2023" = "lightcoral")) + 
  # scale_x_continuous(breaks = 26:44) + # Assuming 'month_name' is available
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),
        axis.text.y = element_text(color = "black"),
        panel.grid = element_blank(),
        
        axis.text = element_text(size = 12)) 

weekly_GCC 








all_togetheragter_36_23<- all_together %>%
  filter(Week >= 36 & Year =="2023")

after36=  all_togetheragter_36_23 %>%
  group_by(date) %>%
  summarise(PRI_PORT4_90= quantile(PRI_PORT4,probs =0.90, na.rm = TRUE),
            PRI_PORT6_90= quantile(PRI_PORT6,probs =0.90, na.rm = TRUE),
            m_gpp=mean(GPP, na.rm=TRUE),
            m_et= mean(ET, na.rm=TRUE),
            m_WUE=mean(WUEe, na.rm=TRUE),
            Tdiff=mean(tdiffcan_air, na.rm= TRUE),
            mean_t= mean(TA_1_4_1, na.rm=TRUE),
            mean_canopy_t= mean( roi_temp_mean, na.rm=TRUE),
            m_sapflow=mean(sf_3, na.rm=TRUE),
            NDVI_PORT2_90= quantile(NDVI_PORT2,probs =0.90, na.rm = TRUE),
            NDVI_PORT5_90= quantile(NDVI_PORT5,probs =0.90, na.rm = TRUE),
            GCC_N= quantile(normalized_gcc,probs =0.90, na.rm = TRUE),
            GCC= quantile(GCC,probs =0.90, na.rm = TRUE),
            N= n(),
            se_PRI = sd(PRI_PORT4, na.rm = TRUE) / sqrt(N),  
            se_m_sapflow = sd(sf_3, na.rm = TRUE) / sqrt(N),
            se_GPP = sd(GPP, na.rm = TRUE) / sqrt(N),  
            se_ET = sd(ET, na.rm = TRUE) / sqrt(N),
            se_WUE = sd(WUEe, na.rm = TRUE) / sqrt(N),  
            se_Tdiff= sd(tdiffcan_air, na.rm = TRUE) / sqrt(N),
            se_NDVI2 = sd(NDVI_PORT2, na.rm = TRUE) / sqrt(N),
            se_NDVI5 = sd(NDVI_PORT5, na.rm = TRUE) / sqrt(N),
            se_GCCn = sd(normalized_gcc, na.rm = TRUE) / sqrt(N),  
            se_GCC= sd(GCC, na.rm = TRUE) / sqrt(N)
            
  )


ggplot(after36, aes(x =date)) +
  
  geom_ribbon(aes(ymin = Tdiff- se_Tdiff, ymax = Tdiff +  se_Tdiff), fill = "green3", alpha = 0.2) +
  geom_ribbon(aes(ymin = m_sapflow- se_m_sapflow, ymax = m_sapflow + se_m_sapflow), fill = "blue3", alpha = 0.2) +
  geom_ribbon(aes(ymin = m_et-se_ET, ymax =m_et + se_ET), fill = "blue", alpha = 0.2) +
  geom_ribbon(aes(ymin = (PRI_PORT4_90 * 10) - (se_PRI * 10), ymax = (PRI_PORT4_90 * 10) + (se_PRI * 10)), fill = "red", alpha = 0.2) +
  geom_ribbon(aes(ymin = (GCC_N * 10) - (se_GCCn * 10), ymax = (GCC_N * 10) + (se_GCCn * 10)), fill = "pink", alpha = 0.2) +
  
  geom_line(aes(y = Tdiff, color = "tdiff"), size = 1) +  # Line for tdiff
  geom_line(aes(y = m_et, color = "ET"), size = 1) +  # Line for tdiff
  geom_line(aes(y = m_sapflow, color = "Sap Flow"), size = 1) +  # Line for sap flow
  geom_line(aes(y = PRI_PORT4_90 * 10, color = "PRI"), size = 1) +  # Line for PRI
  geom_line(aes(y = GCC_N * 10, color = "GCC"), size = 1) +  # Line for PRI
  scale_y_continuous(
    name = "Primary Y-Axis",  
    sec.axis = sec_axis(~./10, name = "PRI Values")  # Scale PRI back for secondary y-axis
  ) +
  
  scale_color_manual(
    values = c("tdiff" = "green3", "Sap Flow" = "blue3", "PRI" = "red", GCC="pink", ET="orange")
  ) +
  
  # Labels and theme
  labs(x = "week", color = "Legend") +
  theme_minimal() + theme(panel.background = element_rect(fill = "white"),
                          axis.text.y = element_text(color = "black"),
                          panel.grid = element_blank(),
                          axis.text = element_text(size = 12)) 
theme(legend.position = "right")









ggplot(weekly, aes(x =Week)) +
  
  geom_ribbon(aes(ymin = Tdiff- se_Tdiff, ymax = Tdiff +  se_Tdiff), fill = "green3", alpha = 0.2) +
  geom_ribbon(aes(ymin = m_sapflow- se_m_sapflow, ymax = m_sapflow + se_m_sapflow), fill = "blue3", alpha = 0.2) +
  geom_ribbon(aes(ymin = m_et-se_ET, ymax =m_et + se_ET), fill = "blue", alpha = 0.2) +
  geom_ribbon(aes(ymin = (PRI_PORT4_90 * 10) - (se_PRI * 10), ymax = (PRI_PORT4_90 * 10) + (se_PRI * 10)), fill = "red", alpha = 0.2) +
  geom_ribbon(aes(ymin = (GCC_N * 10) - (se_GCCn * 10), ymax = (GCC_N * 10) + (se_GCCn * 10)), fill = "pink", alpha = 0.2) +
  
  geom_line(aes(y = Tdiff, color = "tdiff"), size = 1) +  # Line for tdiff
  geom_line(aes(y = m_et, color = "ET"), size = 1) +  # Line for tdiff
  geom_line(aes(y = m_sapflow, color = "Sap Flow"), size = 1) +  # Line for sap flow
  geom_line(aes(y = PRI_PORT4_90 * 10, color = "PRI"), size = 1) +  # Line for PRI
  geom_line(aes(y = GCC_N * 10, color = "GCC"), size = 1) +  # Line for PRI
  scale_y_continuous(
    name = "Primary Y-Axis",  
    sec.axis = sec_axis(~./10, name = "PRI Values")  # Scale PRI back for secondary y-axis
  ) +
  
  scale_color_manual(
    values = c("tdiff" = "green3", "Sap Flow" = "blue3", "PRI" = "red", GCC="pink", ET="orange")
  ) +
  
  # Labels and theme
  labs(x = "week", color = "Legend") +
  theme_minimal() + theme(panel.background = element_rect(fill = "white"),
                          axis.text.y = element_text(color = "black"),
                          panel.grid = element_blank(),
                          axis.text = element_text(size = 12)) 
theme(legend.position = "right")


                                                             
