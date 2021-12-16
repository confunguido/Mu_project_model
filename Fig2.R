rm(list = ls())




library(reshape2)
library(Hmisc)
library(readxl)
library(tidyverse)
library(scales)
library(readxl)

scale_factor_map <- 3


dat <- read_excel("data/Bogotá_HUSI_DISAN_variants.xlsx", sheet = "bogota")
start_date <- as.Date("2021/03/25")
end_date   <- as.Date("2021/07/29")

dates <- seq(start_date, end_date, by = 7)

dd <- bind_rows (data.frame(var = "Alpha", sampling = dat$sampling, week=dat$week, pos = dat$Alpha, total = dat$Total,  (binconf(dat$Alpha, dat$Total))),
                 data.frame(var = "Gamma", sampling = dat$sampling, week=dat$week, pos = dat$Gamma, total = dat$Total, (binconf(dat$Gamma, dat$Total))),
                 data.frame(var = "Mu", sampling = dat$sampling, week=dat$week, pos = dat$Mu, total = dat$Total, (binconf(dat$Mu, dat$Total))),
                 data.frame(var = "Others", sampling = dat$sampling, week=dat$week, pos = dat$Others, total = dat$Total, (binconf(dat$Others, dat$Total))))
dd$date <- dates

levels_variants <- c("Alpha", "Gamma", "Iota", "Lambda", "Mu", "Other")
palette_varians <- c("#e31a1c", "#bf812d", "#66bd63", "#8073ac", "#35978f", "#c2a5cf")

bg <- read_excel("Bogotá_HUSI_DISAN_variants.xlsx", sheet = "bogota-all")
bg <- bg %>% mutate (Other = Other + B.1.625) %>% dplyr::select(-B.1.625)
bg$date <- dates
bg <- melt (bg, id.vars = c("week", "total", "date"), variable.name = "variant")
bg$prop <- bg$value /bg$total
bg$variant <- factor(bg$variant, levels = levels_variants)


col <- read_excel("Bogotá_HUSI_DISAN_variants.xlsx" ,sheet = "colombia")
col <- col %>% mutate (Other = Other + B.1.625 + Delta) %>% dplyr::select(-B.1.625, - Delta)
col$date <- dates
col <- melt (col, id.vars = c("week", "total", "date"), variable.name = "variant")
col$prop <- col$value/col$total
col$variant <- factor(col$variant, levels = levels_variants)


wrld <- read_excel("Bogotá_HUSI_DISAN_variants.xlsx" ,sheet = "world")
wrld$date <- dates
wrld <- melt (wrld, id.vars = c("week", "total", "date"), variable.name = "variant")
wrld$prop <- wrld$value/wrld$total


# ggplot(dd) +
#   geom_pointrange(aes (x= date, y=PointEst, ymin = Lower, ymax= Upper, colour = sampling)) +
#   facet_wrap(vars(var)) +
#   labs(x = "epi-weeks 2021", y = "prevalence") +
#   theme_linedraw() +
#   scale_x_date("week")
# 
# rownames(dd) <- NULL



# write.csv(dd, "data/variants_ic.csv")

date_breaks_plot <- function(x) seq.Date(from = min(bg$date), 
                                         to = max(bg$date), 
                                         by = "21 days")
p1 <- 
  ggplot(bg %>% filter (variant != "Delta")) +
  geom_bar(aes (x= (date), y=prop, fill = variant), 
           width = 7, stat = "identity", alpha = 0.8, colour ="white", size = .3 * scale_factor_map) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = date_breaks_plot) +
  cowplot::theme_cowplot(font_size = 15 * scale_factor_map) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs (fill ="", x = "", y ="Proportion of variants in Bogotá") + #, x = "EpiWeek") +
  theme(axis.text.x = element_text(angle = 38, vjust = 1, hjust = 1)) +
  scale_fill_manual(values = palette_varians)  +
  theme(legend.position="right",
        legend.direction = "vertical",
        legend.justification = "top",
        legend.margin=margin(10,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  guides(fill = guide_legend( ncol = 1, byrow = FALSE)) 

p2 <- 
  ggplot(col %>% filter (variant != "Delta")) +
  geom_bar(aes (x= (date), y=prop, fill = variant), 
           width = 7, stat = "identity", alpha = 0.8, colour ="white", size = .3 * scale_factor_map) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = date_breaks_plot) +
  cowplot::theme_cowplot(font_size = 15 * scale_factor_map) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs (fill ="", x = "", y ="Proportion of variants in Colombia") + #, x = "EpiWeek") +
  theme(axis.text.x = element_text(angle = 38, vjust = 1, hjust = 1)) +
  scale_fill_manual(values = palette_varians)  +
  theme(legend.position="right",
        legend.direction = "vertical",
        legend.justification = "top",
        legend.margin=unit(-.5,"cm")) +
  guides(fill = guide_legend( ncol = 1, byrow = FALSE))


wrld$country <- as.character(wrld$variant)
levels_country <- c(sort(unique(wrld$country[wrld$country != "Other"])), "Other")
wrld$country <- factor(wrld$country, levels = levels_country)

p3 <- 
  ggplot(wrld ) +
  geom_bar(aes (x= (date), y=prop, fill = country), 
           width = 7, stat = "identity", alpha = 0.8, colour ="white", size = .3 * scale_factor_map) +
  cowplot::theme_cowplot(font_size = 15 * scale_factor_map) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = date_breaks_plot) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs (fill ="", x = "", y ="Proportion of Mu variant by country") + #, x = "EpiWeek") +
  theme(axis.text.x = element_text(angle = 38, vjust = 1, hjust = 1)) +
  scale_fill_brewer(palette = "Paired")  +
  theme(legend.position="right",
        legend.direction = "vertical",
        legend.justification = "top",
        legend.margin=margin(-10,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  guides(fill = guide_legend( ncol = 1, byrow = FALSE))







###### Colombia Map
library(broom)
library(ggspatial)
library(rgdal)

shp <- readOGR(dsn = file.path("depto/depto.shp"), stringsAsFactors = FALSE)
colm <- tidy(shp, region = "NOMBRE_DPT")
colm$adminx <- epitrix::clean_labels(colm$id)
colm$adminx[colm$adminx == "santafe_de_bogota_d_c"] <- "bogota"

colmu <- read_excel("data/Bogotá_HUSI_DISAN_variants.xlsx", sheet = "raw-col")
colmu <- colmu %>% dplyr::select(Municipio, Cat_lineage) %>% 
  filter (Cat_lineage == "Mu") %>% rename(adminx = Municipio) %>% 
  group_by(adminx) %>% summarise(num_mu = n()) %>% 
  mutate(adminx = epitrix::clean_labels(adminx)) %>% as_tibble()

colm2 <- merge (colm, colmu, by = "adminx", all = TRUE) %>% as_tibble()
colm2 <- plyr::join(colm, colmu, type = "full")

pcol <-
  ggplot() +
  geom_polygon(data = colm2, aes(fill = num_mu,  x = long, y = lat, group = group), 
               color="white", size = 0.2 * scale_factor_map) +
  theme_void(10 * scale_factor_map) +
  labs(fill = "Cumulative number of Mu sequences in Colombia", x = "") +
  scale_fill_distiller(palette = "GnBu", direction = 1, trans='log10', na.value="gray80", space = "Lab") +
  coord_cartesian(xlim=c(5e5, 1.75e6), ylim = c(0, 1.8e6)) +
  guides(fill = guide_colourbar(barwidth = 1* scale_factor_map, barheight = 25 * scale_factor_map,
                                title.position = "left", title.vjust = -1)) +
  theme(legend.position="right",
        legend.direction = "vertical",
        legend.title = element_text(size = 15 * scale_factor_map, angle = 90),
        legend.text = element_text(size = 15 * scale_factor_map),
        legend.justification = "left",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  geom_segment(aes(x = 1.01e6, y = 1.01e6, xend = 1.5e6, yend = 1.6e6),
               arrow = arrow(length = unit(0.2* scale_factor_map, "cm")), size = 1 * scale_factor_map) +
  annotate(geom="text", x=1.55e6, y=1.65e6, label="Bogotá",color="black", size = 5 * scale_factor_map)


#####  Worldmap

library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(tidyverse)

vu <- read_excel("data/Base datos vuelo.xls") 
names(vu) <- epitrix::clean_labels(names(vu)) 
vu <- vu %>% mutate(fecha_utc = as.Date(fecha_utc))
intairp <- read_excel("data/Base datos vuelo.xls", sheet = "airports")
intairp <- intairp %>% filter(iata_code %in% unique(vu$origen)) %>% 
  dplyr::select(name, iata_code, coordinates, iso_country, continent) %>%
  filter(!continent %in% c("AS", "AF"))


n_pass_int  <- vu %>% 
  filter (fecha_utc > as.Date("2020-01-01") , fecha_utc < as.Date("2021-06-29")) %>% 
  group_by(origen) %>% summarise(n_pass = sum(pasajeros)) %>% rename (iata_code = origen) %>%
  filter(n_pass >10)
n_pass_int <- merge(n_pass_int, intairp, by = "iata_code")
n_pass_int$lon <- as.numeric(gsub(",.*$","",n_pass_int$coordinates))
n_pass_int$lat <- as.numeric(gsub(".*,", "",n_pass_int$coordinates))

alpha2 <- read_excel("data/Base datos vuelo.xls", sheet = "alpha2") 
n_pass_int <- merge(n_pass_int, alpha2, by = "iso_country")



world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world %>% filter(region_un != "Antarctica")
world$adminx <- epitrix::clean_labels(world$admin)

wm <- read_excel("data/Bogotá_HUSI_DISAN_variants.xlsx", sheet = "world-mu")
wm$adminx <- epitrix::clean_labels(wm$country)
wm <- wm %>% select(adminx, total) %>% rename(num_mu = total)

world2 <- merge(world, wm, by = "adminx", all = TRUE)

pw <-
ggplot(data = world2) +
  geom_sf(aes(fill = num_mu), colour = "white",size = .2 * scale_factor_map) +
  theme(text = element_text(size = 14 * scale_factor_map)) +
  coord_sf(xlim = c(-135, 135), ylim = c(-45, 75) ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank()) +
  scale_fill_distiller(palette = "GnBu", direction = 1, trans='log10', na.value="gray80", space = "Lab") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
  theme(panel.background = element_rect(fill = "white")) +
  annotate(geom="text", x=-125, y=0, label="Colombia",color="black", size = 5 * scale_factor_map)+
  geom_segment(data = n_pass_int, aes(x = lon,  y = lat, xend = -75 , yend = 5, 
                                      # alpha = n_pass , 
                                      size = n_pass * scale_factor_map), 
               colour = "purple", 
               arrow = arrow(length = unit(0.02* scale_factor_map, "cm"))) +
  guides(
         fill = guide_colourbar(barwidth = 25 * scale_factor_map, barheight = 1* scale_factor_map,
                                 title.position = "top", title.vjust = -1)) +
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        legend.title = element_text(size = 15 * scale_factor_map),
        legend.text = element_text(size = 15* scale_factor_map),
        legend.justification = "left",
        legend.margin=margin(0,0,0,0),
        legend.box = "horizontal",
        legend.box.margin=margin(-10,-10,-10,-10)) +
  geom_segment(aes(x = -80, y = 5, xend = -100, yend = 0),
               arrow = arrow(length = unit(0.2* scale_factor_map, "cm")), size = 1 * scale_factor_map) +
  labs(fill = "Cumulative number of Mu sequences worldwide", x = "", y = "") +
  scale_size(guide = 'none')
  

airports <- read_excel("data/Base datos vuelo.xls", sheet = "iso")
airports_bra <- airports %>% filter(country == "Brazil")
airports_bra <- airports_bra$code
fbra <- vu %>% filter(origen %in% airports_bra) 
fother <- vu %>% filter(!origen %in% airports_bra) %>% mutate(origin = "Others")
nf_bra <- fbra %>% group_by(fecha_utc) %>% 
  summarise(n_flights = n(), n_passangers = sum(pasajeros)) %>% mutate(origin = "Brazil")
nf_others <- fother %>% group_by(fecha_utc) %>% 
  summarise(n_flights = n(), n_passangers = sum(pasajeros)) %>%
  mutate(origin = "Others")

nf_all <- rbind (nf_bra, nf_others)  %>%
  filter(fecha_utc > as.Date("2021-01-11") , fecha_utc < as.Date("2021-07-31"))

p3 <-
  ggplot(nf_all) +
  geom_line(aes(x = fecha_utc, y = n_passangers, colour = origin), size = 3) +
  cowplot::theme_cowplot(font_size = 15 * scale_factor_map) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs (fill ="", x = "", y ="Daily number of international flight\npassangers arriving to Colombia", colour = "") + #, x = "EpiWeek") +
  theme(axis.text.x = element_text(angle = 38, vjust = 1, hjust = 1)) +
  scale_fill_brewer(palette = "Paired")  +
  theme(legend.position="right",
        legend.direction = "vertical",
        legend.justification = "top",
        legend.margin=margin(-10,0,0,0),
        legend.box.margin=margin(-10,-10,-10,-10)) +
  guides(fill = guide_legend (ncol = 1, byrow = FALSE)) +
  scale_colour_manual(values = c("red", "blue")) +
  scale_y_continuous(trans = "log10") +
  theme(legend.position = c(0.9, 0.7), legend.background = element_blank()) +
  scale_x_date(date_labels = "%b %d %Y",  breaks = "3 weeks") 



library(cowplot)
library(RColorBrewer)

pA <- plot_grid(p2, p1, pcol, nrow = 1, labels= c("A", "B", "C"), 
                label_size = 20 * scale_factor_map, scale = 0.95, rel_widths = c(1, 0.95, 0.95))
pB <- plot_grid(pw, p3, nrow = 1, labels= c("D", "E"), 
                label_size = 20 * scale_factor_map, scale = 0.9, 
                rel_widths = c(1.4, 0.8), 
                rel_heights = c(1.4, 0.8))
all <- plot_grid(pA, pB, nrow = 2, rel_heights = c(1, 1.1))

png(file = "figs/Fig2.png",  width = 1100 *scale_factor_map, height = 900 * scale_factor_map)
all
dev.off()






