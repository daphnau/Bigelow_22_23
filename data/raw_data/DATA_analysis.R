SF<-read.csv('sap_flow.csv', header = TRUE,  sep = ',')
THERMAL<-read.csv('Thermal_allday.csv', header = TRUE,  sep = ',')
TOWER<-read.csv('tower_selected.csv', header = TRUE,  sep = ',')
SRS<-read.csv('SRS_selected.csv', header = TRUE,  sep = ',')
RGB<-read.csv('RGB_with_normalization.csv', header = TRUE,  sep = ',')

all_together <- read.csv('all_data', header = TRUE,  sep = ',')
THERMAL_SF_SRS_TOWER<- read.csv('all_data', header = TRUE,  sep = ',')

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