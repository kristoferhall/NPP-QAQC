# KM Hall
# 20190326
#
# testing NPP daily data and program to look at speeding it up a bit.


library(tidyverse)
library(lubridate)
library(plyr)


# load AJ_AllQuads.csv as tibble (read_csv) versus read.csv
path <- "/Users/kris/Documents/SEV/NPP/NPP_QAQC_revision_2019/NPP_QAQC/data/data_other/AJ_AllQuads.csv"

system.time(aq_t <- read_csv(path, guess_max = 3000000))
# user  system elapsed 
# 3.480   0.323   3.865 

str(aq_t)

# format some variables
aq_t$date <- mdy(aq_t$date)
aq_t$year <- as.character(aq_t$year)
aq_t$season <- as.character(aq_t$season)
aq_t$web <- as.character(aq_t$web)
aq_t$quad <- as.character(aq_t$quad)



# ---
system.time(aq_b <- read.csv(path, strip.white = TRUE, stringsAsFactors = FALSE))
str(aq_b)
aq_b$date <- mdy(aq_b$date)

# variables that should be character format
aq_b$web <- as.character(aq_b$web)
aq_b$quad <- as.character(aq_b$quad)





### 
aq_charvar <- c("site", "web", "plot", "quad", "treatment", "kartez", "dataset", "subplot", "transect")

# THIS WORKS
for (i in seq_along(aq_t)) {
  if (is_character(aq_t[[i]])) {
    x <- aq_t %>% 
      dplyr::group_by(aq_t[[i]]) %>% 
      dplyr::summarise(count = n()) %>% 
      dplyr::arrange(-count)
    print(paste0(aq_t[[i]], ":"))
    print(x, n=1000)
  } else next
}
###



names(aq_t)
str(aq_t)

# look at unique combos in all.quads
combos_unique <-
  aq_t %>% 
    select(site, plot, quad, web, treatment, subplot, transect) %>% 
    unique() %>% 
    arrange(site)
site_unique <- 
  aq_t %>% 
  count(site)


# all combos listed in .Rmd doc
# Create data.frame with all site-treatment-web-plot-subplot-quad-transect combination
all.quad.codes <- join_all(type = "full", dfs = list(
  # Fert has plots 1-20, with quads 1-4 for each plot
  siteF = expand.grid(site = "F", plot = as.factor(1:20), quad = 1:4),
  # Creosote core has webs 1-5, plots N, S, E, W for each web, and quads 1 and 3 for each plot
  siteC = expand.grid(site = "C", web = 1:5, plot = c("N", "S", "E", "W"), quad = c(1,3)),
  # Black grama core has webs 1 and 5, with plots N, S, E, W, NE for each web, and quads 1 and 3 for each plot
  # Black grama core also has web 4, with plots N, S, E, W, NW, NE for each web, and quads 1 and 3 for each plot
  siteG = rbind(expand.grid(site = "G", treatment = "C", web = c(1,5), plot = c("N", "S", "E", "W", "NE"), quad = c(1,3)), expand.grid(site = "G", treatment = "C", web = 4, plot = c("NW", "S", "E", "W", "NE"), quad = c(1,3))), 
  # Blue grama core has webs 1-5, plots N, E, W for each web, and quads 1 and 3 for each plot
  siteB = expand.grid(site = "B", web = 1:5, plot = c("N", "E", "W"), quad = c(1,3)),
  # burned black grama core has quads 1:15, 26:40 
  siteGB = expand.grid(site = "G", treatment = "B", quad = c(1:15,26:40)),
  # mixed grass has burned and unburned treatments has quads 1:15, 26:40 
  siteMG = expand.grid(site = "MG", treatment = c(NA, "B"), quad = c(1:15,26:40)),
  # burned mixed shrub has burned and unburned treatments with quads 1:40 
  siteMS = expand.grid(site = "MS", treatment = c(NA, "B"), quad = 1:40),
  # monsoon has plots 1-13, with subplots A, B for each plot, and quads 1-2 for each subplot 
  siteM = expand.grid(site = "M", plot = as.factor(1:13), subplot = c("A", "B"), quad = 1:2),
  # warming has plots 1-40, with quads 1-2 for each plot 
  siteW = expand.grid(site = "W", plot = as.factor(1:40), quad = 1:2),
  # pinyon juniper core has plots 1-2, with transects V, R, S, N for each plot, and quads 1-5 for each transect 
  # pinyon juniper core also has plot 3, with transects A, B, C, D, and quads 1-5 for each transect 
  siteP = rbind(expand.grid(site = "P", plot = as.factor(1:2), transect = c("V", "R", "S", "N"), quad = 1:10), expand.grid(site = "P", plot = as.factor(3), transect = c("A", "B", "C", "D"), quad = 1:5))))

all.quad.codes$treatment[is.na(all.quad.codes$treatment)] <- "C"





core_creosote <- crossing(site = "core_creosote", web = as.character(1:5), plot = c("N", "S", "E", "W"), quad = c("1", "3"))
core_black_1 <- crossing(site = "core_black", web = c("1", "5"), plot = c("N", "S", "E", "W", "NE"), quad = c("1", "3"))
core_black_2 <- crossing(site = "core_black", web = "4", plot = c("S", "E", "W", "NE", "NW"), quad = c("1", "3"))
core_pj_1 <- crossing(site = "core_pj", transect = c("V", "R", "S", "N"), quad = as.character(c(1:10)))
core_pj_2 <- crossing(site = "core_pj", transect = c("A", "B", "C", "D"), quad = as.character(c(1:5)))
tst_combo <- core_creosote %>% 
  full_join(core_black_1) %>% 
  full_join(core_black_2) %>% 
  full_join(core_pj_1) %>% 
  full_join(core_pj_2)

# ---
dfs = list(
  # Fert has plots 1-20, with quads 1-4 for each plot
  siteF = expand.grid(site = "F", plot = as.factor(1:20), quad = 1:4),
  # Creosote core has webs 1-5, plots N, S, E, W for each web, and quads 1 and 3 for each plot
  siteC = expand.grid(site = "C", web = 1:5, plot = c("N", "S", "E", "W"), quad = c(1,3)),
  # Black grama core has webs 1 and 5, with plots N, S, E, W, NE for each web, and quads 1 and 3 for each plot
  # Black grama core also has web 4, with plots N, S, E, W, NW, NE for each web, and quads 1 and 3 for each plot
  siteG = rbind(expand.grid(site = "G", treatment = "C", web = c(1,5), plot = c("N", "S", "E", "W", "NE"), quad = c(1,3)), expand.grid(site = "G", treatment = "C", web = 4, plot = c("NW", "S", "E", "W", "NE"), quad = c(1,3))), 
  # Blue grama core has webs 1-5, plots N, E, W for each web, and quads 1 and 3 for each plot
  siteB = expand.grid(site = "B", web = 1:5, plot = c("N", "E", "W"), quad = c(1,3)),
  # burned black grama core has quads 1:15, 26:40 
  siteGB = expand.grid(site = "G", treatment = "B", quad = c(1:15,26:40)),
  # mixed grass has burned and unburned treatments has quads 1:15, 26:40 
  siteMG = expand.grid(site = "MG", treatment = c(NA, "B"), quad = c(1:15,26:40)),
  # burned mixed shrub has burned and unburned treatments with quads 1:40 
  siteMS = expand.grid(site = "MS", treatment = c(NA, "B"), quad = 1:40),
  # monsoon has plots 1-13, with subplots A, B for each plot, and quads 1-2 for each subplot 
  siteM = expand.grid(site = "M", plot = as.factor(1:13), subplot = c("A", "B"), quad = 1:2),
  # warming has plots 1-40, with quads 1-2 for each plot 
  siteW = expand.grid(site = "W", plot = as.factor(1:40), quad = 1:2),
  # pinyon juniper core has plots 1-2, with transects V, R, S, N for each plot, and quads 1-5 for each transect 
  # pinyon juniper core also has plot 3, with transects A, B, C, D, and quads 1-5 for each transect 
  siteP = rbind(expand.grid(site = "P", plot = as.factor(1:2), transect = c("V", "R", "S", "N"), quad = 1:10), expand.grid(site = "P", plot = as.factor(3), transect = c("A", "B", "C", "D"), quad = 1:5)))




str(all.quad.codes)
str(combos_unique)

all.quad.codes$site <- as.character(all.quad.codes$site)
all.quad.codes$plot <- as.character(all.quad.codes$plot)
all.quad.codes$quad <- as.character(all.quad.codes$quad)
all.quad.codes$web <- as.character(all.quad.codes$web)
all.quad.codes$treatment <- as.character(all.quad.codes$treatment)
all.quad.codes$subplot <- as.character(all.quad.codes$subplot)
all.quad.codes$transect <- as.character(all.quad.codes$transect)

aqc_no_match <- combos_unique %>% anti_join(all.quad.codes)
