library(tidyverse)
library(lubridate)
library(here)

#### Read data

mortality_data = read.csv(
  here("data/mortality_data_2000-2019.csv"),
  colClasses = c("numeric", "character", "character", "character", "numeric")) %>% 
  as_tibble()

arron_coords = read_csv(
  here("data/arrondissements_coords.csv"),
  col_types = cols(col_character(), col_character(), col_double(), col_double())
)

temp_data = read.csv2(
  here("data/temp_2000_2019.csv"),
  colClasses = rep("character", 7)
) %>% 
  as_tibble()



### Transform data

grids_temp = temp_data %>% select(GRID_NO, LATITUDE, LONGITUDE) %>% distinct() %>% 
  mutate(lat = as.numeric(LATITUDE),
         lon = as.numeric(LONGITUDE))



distances = lapply(1:nrow(arron_coords), function(i){
  distances_i = (grids_temp$lat - arron_coords[i,]$lat)^2 + (grids_temp$lon - arron_coords[i,]$lon)^2
  out_i = tibble(NIS_CODE = arron_coords$NIS_Code[i], GRID_NO = grids_temp$GRID_NO, dist = distances_i)
  return(out_i)
}) %>% 
  bind_rows()



arron_grid_mapping = distances %>% 
  group_by(NIS_CODE) %>% 
  filter(dist == min(dist)) %>% 
  ungroup()



arron_temp_data = temp_data %>% 
  inner_join(
    arron_grid_mapping %>% 
      select(NIS_CODE, GRID_NO)) %>% 
  left_join(arron_coords %>% 
              select(NIS_CODE = NIS_Code, Nom_arrondissement)) %>% 
  mutate(
    date = ymd(DAY),
    TEMPERATURE_MAX = as.numeric(TEMPERATURE_MAX),
    TEMPERATURE_MIN = as.numeric(TEMPERATURE_MIN),
    TEMPERATURE_AVG = as.numeric(TEMPERATURE_AVG)
  ) %>% 
  filter(date >= ymd("2000-01-01")) %>% 
  select(date, Nom_arrondissement, NIS_CODE, GRID_NO, TEMPERATURE_MAX, TEMPERATURE_MIN, TEMPERATURE_AVG) %>% 
  arrange(date, NIS_CODE)




temp_percetiles_year_arron = arron_temp_data %>% 
  mutate(YEAR = year(date)) %>% 
  group_by(YEAR, NIS_CODE, Nom_arrondissement) %>% 
  summarize(
    temp_95_percentile = quantile(TEMPERATURE_MAX, .95),
    temp_97_percentile = quantile(TEMPERATURE_MAX, .97),
    temp_98_percentile = quantile(TEMPERATURE_MAX, .98),
    temp_99_percentile = quantile(TEMPERATURE_MAX, .99)
  )




heat_wave_day_def = arron_temp_data %>% 
  arrange(Nom_arrondissement, NIS_CODE, date) %>%  
  group_by(Nom_arrondissement, NIS_CODE) %>% 
  mutate(
    MAX_TEMP_day_plus_1 = lead(TEMPERATURE_MAX, n = 1),
    MAX_TEMP_day_plus_2 = lead(TEMPERATURE_MAX, n = 2),
    MAX_TEMP_day_plus_3 = lead(TEMPERATURE_MAX, n = 3),
    MAX_TEMP_day_plus_4 = lead(TEMPERATURE_MAX, n = 4)) %>% 
  mutate(YEAR = year(date)) %>% 
  left_join(temp_percetiles_year_arron %>% 
              select(YEAR, NIS_CODE, Nom_arrondissement, temp_heat_wave_threshold = temp_95_percentile)) %>% 
  mutate(heat_wave = ifelse(
    TEMPERATURE_MAX >= temp_heat_wave_threshold &
      MAX_TEMP_day_plus_1 >= temp_heat_wave_threshold & 
      MAX_TEMP_day_plus_2 >= temp_heat_wave_threshold & 
      MAX_TEMP_day_plus_3 >= temp_heat_wave_threshold & 
      MAX_TEMP_day_plus_4 >= temp_heat_wave_threshold, 
    1, 0))




heat_wave_week = heat_wave_day_def %>% 
  mutate(isoweek = lubridate::isoweek(date)) %>% 
  group_by(NIS_CODE, Nom_arrondissement, YEAR, isoweek) %>% 
  summarize(heat_wave_week = as.integer(sum(heat_wave) > 0)) %>% 
  ungroup()



mortality_heat_wave = heat_wave_week %>% 
  mutate(YEAR_WEEK = paste0(YEAR, "-", sprintf("%03s", isoweek))) %>% 
  select(YEAR_WEEK, NIS_CODE, Nom_arrondissement, heat_wave_week) %>% 
  inner_join(
    mortality_data,
    by = c("NIS_CODE" = "ARRON", "YEAR_WEEK" = "YEAR_WEEK")
  ) %>% 
  select(YEAR, YEAR_WEEK, NIS_CODE, Nom_arrondissement, COD, n_deaths = N_MASK, heat_wave_week)


write_csv(mortality_heat_wave, file = here("out/mortality_heat_wave_R.csv"))


# arron_temp_data %>% 
#   ggplot() +
#   geom_line(aes(date, TEMPERATURE_AVG)) +
#   facet_wrap(~GRID_NO)
# 
# 
# 
# arron_temp_data %>% 
#   ggplot() +
#   geom_line(aes(date, TEMPERATURE_AVG)) +
#   facet_wrap(~Nom_arrondissement)
# 
# 
# 
# arron_temp_data %>% 
#   mutate(month = month(date)) %>% 
#   filter(month %in% c(5, 6, 7, 8)) %>% 
#   group_by(Nom_arrondissement) %>% 
#   mutate(ix = 1:n()) %>% 
#   ggplot() +
#   geom_line(aes(ix, TEMPERATURE_AVG)) +
#   facet_wrap(~Nom_arrondissement)








