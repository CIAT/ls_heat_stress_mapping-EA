library(tidyverse)

varLS <- c("HURS", "TASMAX")

data <- read_csv("D:/jymutua/ls-heat-stress-mapping - EA/data/historical/weather_data_1981_2010.csv")

ensemble <- data %>%
  group_by(SITE_ID, LONG, LAT, CL_VARIABLE, LAYER_NAME, YEAR, MONTH, DAY) %>%
  summarize(VALUE = mean(VAR, na.rm = TRUE))

stationLS <- unique(ensemble$SITE_ID)

for (var in varLS){
  
  ensemble_split<- subset(ensemble, SITE_ID %in% stationLS[81:169])
  
  ensemble_split <- filter(ensemble_split, CL_VARIABLE==var)
  
  write.csv(ensemble_split , file = paste0("D:/jymutua/ls-heat-stress-mapping - EA/data/historical/", var, "_ensemble_1981_2010_s81-s169.csv"), row.names = TRUE)
  
}
