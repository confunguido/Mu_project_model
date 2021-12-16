##===============================#
## Figure1
##===============================#
rm(list=ls())
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(lubridate)
library(sf)
library(raster)
library(rgdal)
library(deSolve)
library(reshape2)
library(Hmisc)
library(readxl)
library(tidyverse)
library(scales)
library(readxl)
library(broom)
rm(list = ls())

scale_factor_map <- 3


data_rt = read.delim('epidata/Bogota_Reproduction_Number.csv', fileEncoding  = "latin1" , skip = 5, sep = ";") %>%
  mutate(DateStart = parse_date_time(Fecha_Inicio_Ventana, orders = c("dmy", "dmy HMS")))%>%
  mutate(Date = parse_date_time(Fecha_Fin_Ventana, orders = c("dmy", "dmy HMS")))%>%
  mutate(R_mean = `Mean.R.`, R_std = `Std.R.`)%>%
  mutate(R_mean = as.numeric(str_replace(R_mean, ",", "\\."))) %>%
  filter(grepl(".*Bogot",Localidad)) %>%
  mutate(Date = as.Date(Date))

bog_deaths = read_delim('epidata/Covid_Bogota_cases_saludata.csv', delim = ';')  %>%
  filter(ESTADO == "Fallecido") %>% group_by(FECHA_DIAGNOSTICO) %>% 
  summarise(Deaths = n()) %>%
  rename(Date = FECHA_DIAGNOSTICO) %>% filter(Date < as.Date("2021-08-01"))

bog_cases = read_delim('epidata/Covid_Bogota_cases_saludata.csv',delim = ';')  %>%
  group_by(FECHA_DIAGNOSTICO) %>% summarise(Cases = n()) %>%
  rename(Date = FECHA_DIAGNOSTICO) %>% filter(Date < as.Date("2021-08-01"))

localidad_sec = read_csv('data/Localidad_Unidad_Catastral.csv')
localidad_sec_list = read_csv('data/Bogota_localidades_ID.csv') %>%
  filter(Localidad_ID != 20) %>%
  left_join(localidad_sec, by = c("Localidad_ID" = "Localidad"))

localidad_list = read_csv('data/Bogota_localidades_ID.csv')

pop_localidad = read_csv('data/bogota_population_data_sec.csv') %>%
  left_join(localidad_sec_list, by = c("Zone" = "SCACODIGO")) %>%
  group_by(Localidad_ID) %>%
  summarise(Pop = sum(Pop, na.rm = T)) %>%
  ungroup() %>%
  drop_na() %>%
  left_join(localidad_list,by = "Localidad_ID")


bog_loc_cases = read_delim('epidata/Covid_Bogota_cases_saludata.csv',delim = ';')  %>%
  group_by(FECHA_DIAGNOSTICO, LOCALIDAD_ASIS) %>% summarise(Cases = n()) %>%
  rename(Date = FECHA_DIAGNOSTICO) %>%
  arrange(Date) %>%
  mutate(CumCases = cumsum(Cases)) %>%
  ungroup() %>%
  mutate(Localidad_name = tolower(iconv(LOCALIDAD_ASIS,from="UTF-8",to="ASCII//TRANSLIT"))) %>%
  left_join(pop_localidad, by = c('Localidad_name' = 'Localidad')) %>%
  mutate(incidence_rt = Cases / Pop * 100000,
         cumincidence_rt = CumCases / Pop * 100000)








##### ------  Figure 1
date_breaks_plot <- function(x) seq.Date(from = min(bog_cases$Date), 
                                         to = max(bog_cases$Date), 
                                         by = "60 days")


coeff <- 60
p1_A <-
  ggplot() +
  geom_line(data = bog_cases, aes (x = Date, y = Cases/coeff), colour = "black", size = 0.5 * scale_factor_map) +
  geom_line(data = bog_deaths, aes (x = Date, y = Deaths), colour = "darkred", size = 0.5 * scale_factor_map) +
  cowplot::theme_cowplot(font_size = 15 * scale_factor_map) +
  scale_y_continuous(name = "Daily number of deaths",
                     sec.axis = sec_axis(~.*coeff, name="Daily number of cases"))  +
  theme(axis.title.y = element_text(color = "black"),
        axis.title.y.right = element_text(color = "darkred"),
        axis.text.y.right = element_text(color = "darkred")) +
  labs(colour = "", colour = "", x = "") +
  theme(legend.position = "none") +
  theme(legend.position = c(0.2, 0.8), legend.background = element_blank()) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = date_breaks_plot) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text.x = element_text(angle = 38, vjust = 1, hjust = 1)) 

p1_B <-
  ggplot(data_rt %>% filter(Date < as.Date("2021-07-31"))) +
  geom_line(aes (x = Date, y = R_mean), size = 0.5 * scale_factor_map) +
  labs(y = "Effective reproduction number (Rt)", colour = "", x = "") +
  geom_hline(yintercept = 1, linetype = 2) +
  cowplot::theme_cowplot(font_size = 15 * scale_factor_map) +
  theme(axis.text.x = element_text(angle = 38, vjust = 1, hjust = 1)) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = date_breaks_plot)



# link_data <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
# int       <- read.csv(text = getURL(link_data))
# int <- int %>% filter(sub_region_1 == "Bogota")
# int$date <- as.Date(as.character(int$date))
# int <- int %>% filter(date < as.Date("2021-08-01"))
# saveRDS(int, 'epidata/mobilityBog.RDS')
int <- readRDS('epidata/mobilityBog.RDS')
p1_C <-
  ggplot(int) +
  geom_line(aes(x= date, y = transit_stations_percent_change_from_baseline, 
                colour = "Transit"), size = 0.5 * scale_factor_map) +
  geom_line(aes(x= date, y = residential_percent_change_from_baseline, 
                colour = "Residential"), size = 0.5 * scale_factor_map) +
  theme(axis.title.y = element_text(color = "black"),
        axis.title.y.right = element_text(color = "darkred"),
        axis.text.y.right = element_text(color = "darkred")) +
  labs(colour = "", colour = "", x = "", y = "Mobility % of change from baseline") +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = date_breaks_plot) +
  cowplot::theme_cowplot(font_size = 15 * scale_factor_map) +
  theme(axis.text.x = element_text(angle = 38, vjust = 1, hjust = 1)) +
  theme(legend.position = c(0.6, 0.95), legend.background = element_blank()) +
  geom_hline(yintercept = 0, linetype = 2) +
  scale_colour_manual(values = c("black", "blue")) 


bog_loc_cases = read_delim('epidata/Covid_Bogota_cases_saludata.csv',delim = ';')  %>%
  group_by(FECHA_DIAGNOSTICO, LOCALIDAD_ASIS) %>% summarise(Cases = n()) %>%
  rename(Date = FECHA_DIAGNOSTICO) %>%
  arrange(Date) %>%
  mutate(CumCases = cumsum(Cases)) %>%
  ungroup() %>%
  mutate(Localidad_name = tolower(iconv(LOCALIDAD_ASIS,from="UTF-8",to="ASCII//TRANSLIT"))) %>%
  left_join(pop_localidad, by = c('Localidad_name' = 'Localidad')) %>%
  mutate(incidence_rt = Cases / Pop * 100000,
         cumincidence_rt = CumCases / Pop * 100000)

bog_locc <- rbind(
  bog_loc_cases %>% filter(Date < as.Date("2020-08-01")) %>%
    group_by(LOCALIDAD_ASIS) %>% summarise(cum = sum(Cases)) %>% mutate(period = "Mar-Aug 2020"),
  bog_loc_cases %>% filter(Date > as.Date("2020-08-01"), Date < as.Date("2021-02-28")) %>%
    group_by(LOCALIDAD_ASIS) %>% summarise(cum = sum(Cases)) %>% mutate(period = "Sep 2020 - Feb 2021"),
  bog_loc_cases %>% filter(Date > as.Date("2021-02-28"), Date < as.Date("2021-07-31")) %>%
    group_by(LOCALIDAD_ASIS) %>% summarise(cum = sum(Cases)) %>% mutate(period = "Mar-Jul 2021"))
bog_locc$period <- factor(bog_locc$period, levels = unique(bog_locc$period)) 


pop_locs <- read_excel("data/poblacion_localidades_bogota.xlsx") %>% 
  mutate(code = as.numeric(COD_LOC)) %>% rename(LOCALIDAD_ASIS = NOM_LOC) %>% rename(pop = TOTAL) %>%
  dplyr::select(-"AÑO")
pop_locs$LOCALIDAD_ASIS[pop_locs$LOCALIDAD_ASIS == "Ciudad Bolivar"] <-  "Ciudad Bolívar"
bog_locc <-  merge(bog_locc, pop_locs, by = "LOCALIDAD_ASIS", all = TRUE)
bog_locc <- bog_locc %>% rename (id  = COD_LOC) %>% mutate(cum_per_pop = (cum/pop) * 100e5)

localidad_shp = rgdal::readOGR('data/localidades_bogota/poligonos-localidades.shp')
localidad_shp = localidad_shp[localidad_shp$Identificad != 20,]
localidad_fort <- tidy(localidad_shp, region = "Identificad")
localidad_fort <- plyr::join(localidad_fort, bog_locc, by = "id", type = "full")

p1_D <-
  ggplot() +
  geom_polygon(data = localidad_fort, aes(fill = cum,  x = long, y = lat, group = group), 
               color="gray30", size = 0.5 * scale_factor_map) +
  facet_wrap(vars(period)) +
  theme_void(10 * scale_factor_map) +
  labs(x = "", y = "", fill = "Cases per 100,000 population") +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, 
                       # trans='log10',
                       na.value="gray80") +
  theme(strip.text.x = element_text(size = 15 * scale_factor_map)) +
  guides(fill = guide_colourbar(barwidth = 1* scale_factor_map, barheight = 25 * scale_factor_map,
                                title.position = "left", title.vjust = -1)) +
  theme(legend.position="right",
        legend.direction = "vertical",
        legend.title = element_text(size = 15 * scale_factor_map, angle = 90),
        legend.text = element_text(size = 15 * scale_factor_map),
        legend.justification = "left",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10))

vac_data <- read_csv('data/vacc_data_20210912.csv')
vac_data_d <- vac_data %>% 
  filter(date > as.Date ("2021-02-14"), date < as.Date ("2021-07-02"))

p1_E <-
  ggplot(vac_data_d) +
  geom_line(aes (x = date, y = cum_d1/sum(pop_localidad$Pop)*100, colour = "First Dose"), size = 0.5 * scale_factor_map) +
  geom_line(aes (x = date, y = cum_d2/sum(pop_localidad$Pop)*100, colour = "Second Dose"), size = 0.5 * scale_factor_map) +
  labs(y = "Vaccination coverage (%)", colour = "", x = "") +
  scale_x_date(date_labels = "%b %d %Y", breaks = "2 weeks") +
  scale_colour_manual(values = c("black", "blue")) +
  cowplot::theme_cowplot(font_size = 15 * scale_factor_map) +
  theme(axis.text.x = element_text(angle = 38, vjust = 1, hjust = 1)) +
  theme(legend.position = c(0.2, 0.8), legend.background = element_blank()) 
  

p1_Top <- plot_grid(p1_A, p1_B, p1_C, nrow = 1, rel_widths = c(1, 0.85, 0.85), scale = 0.9,
                    labels= c("A", "B", "C"), label_size = 20 * scale_factor_map)
p1_Btm <- plot_grid(p1_D, p1_E, nrow = 1, rel_widths = c(1.85,0.85), scale = 0.9,
                    labels= c("D", "E"), label_size = 20 * scale_factor_map)


png(file = "figures/science_paper/Fig1.jpeg",  
    width = 1200 *scale_factor_map, height = 800 * scale_factor_map)
plot_grid  (p1_Top, p1_Btm, nrow = 2)
dev.off()



