path <- "data/heat_stress/"
time_line <- "historical"
x <- list.files(path, pattern = time_line, full.names = T) %>% 
  tibble(data=.) %>% 
  filter(stringr::str_detect(data, '.shp')) %>% 
  pull()
# y <- read_sf(x[1])
read_heat <- function(x){
  
  name_heat <- str_remove(x, pattern = "data/heat_stress/historical_avg_frequencies_") %>%
    str_remove(pattern = ".shp")
  
  spatial_information <- read_sf(x) %>% 
    as_tibble()
  # print(colnames(spatial_information))  
  
  spatial_information <- spatial_information %>% 
    dplyr::select(-geometry) %>% 
    dplyr::select(4)
}


y <- purrr::map(.x = x, .f = read_heat) %>% 
  bind_cols()

vector_file <- read_sf(x[1]) %>% 
  dplyr::select(-bf_cttl)
variables <- colnames

information <- bind_cols(vector_file, y) %>% 
  tidyr::pivot_longer(cols = variables) #%>% 
# mutate(value = value/100)

library(RColorBrewer)
myPalette <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))

ggplot() +
  geom_sf(data =information, aes(fill = value),  colour = "black") +
  theme_bw() +
  scale_fill_gradientn(colours = myPalette(20))  +
  facet_wrap(~ name, ncol = 3) +
  ggtitle("Historical Heat Stress")
