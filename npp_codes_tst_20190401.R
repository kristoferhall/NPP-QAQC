# KM Hall
# 20190401
#
# Testing all.quads unique combinations of site, plot, quad, web, treatment, subplot, and transect 
# against what is produced as valid combinations in the all.quads.codes dataset in the daily
# program.


library(tidyverse)
library(lubridate)
# library(plyr)


# load AJ_AllQuads.csv as tibble (read_csv) versus read.csv
path <- "/Users/kris/Documents/SEV/NPP/NPP_QAQC_revision_2019/NPP_QAQC/data/data_other/AJ_AllQuads.csv"

system.time(aq <- read_csv(path, guess_max = 3000000))
# user  system elapsed 
# 3.480   0.323   3.865 

str(aq)

# format some variables
aq$date <- mdy(aq$date)
aq$year <- as.character(aq$year)
aq$season <- as.character(aq$season)
aq$web <- as.character(aq$web)
aq$quad <- as.character(aq$quad)


# get unique combinations from historical all.quads historical data
aq_unique <-
  aq %>% 
  select(site, plot, quad, web, treatment, subplot, transect) %>% 
  unique() %>% 
  arrange(site)

aq_site_unique <- 
  aq %>% 
  dplyr::count(site)


# list of valid combinations listed in the daily program
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

aqc_site_trt_combos <-
  all.quad.codes %>% 
  dplyr::count(site, treatment)
# TODO: Figure out better rules for filling in treatment with "C".  Checks of site-treatment will only be looking at 
# "C" and "B".  NAs for sites shouldn't necessarily be filled in with "C" because this is incorrect treatment for many 
# sites.  Or, I need to better understand how data is being processed.  


# need to format variables before trying to join
# TODO: should rewrite so that we don't have factors in the data
str(all.quad.codes)

all.quad.codes$site <- as.character(all.quad.codes$site)
all.quad.codes$plot <- as.character(all.quad.codes$plot)
all.quad.codes$quad <- as.character(all.quad.codes$quad)
all.quad.codes$web <- as.character(all.quad.codes$web)
all.quad.codes$treatment <- as.character(all.quad.codes$treatment)
all.quad.codes$subplot <- as.character(all.quad.codes$subplot)
all.quad.codes$transect <- as.character(all.quad.codes$transect)






# As of 20190401 testing, all.quad.codes has 582 obs. & aq_unique has 1224 obs.


# What is in all.quads.codes that is not in aq_unique?
aqc_no_aq <- all.quad.codes %>% 
  anti_join(aq_unique) %>% 
  arrange(site, plot, quad, web, treatment, subplot, transect)    # 156 obs - all come out as treatment = "C"?
aq_no_aqc <- aq_unique %>%
  anti_join(all.quad.codes)  %>% 
  arrange(site, plot, quad, web, treatment, subplot, transect)    # 798 obs



# In talking with Lauren today (20190401), she mentioned that there is a new all.quads dataset to replace AJ_AllQuads.csv.
# The new dataset is: NPP_quad_20190323.csv
# Need to compare this to AJ_AllQuads 
path_new <- "/Users/kris/Documents/SEV/NPP/NPP_QAQC_revision_2019/NPP_QAQC/data/data_other/NPP_quad_20190323.csv"

system.time(aq_new <- read_csv(path_new, guess_max = 3000000))
spec(aq_new)

aq_new$Year <- as.character(aq_new$Year)
aq_new$Season <- as.character(aq_new$Season)
aq_new$Web <- as.character(aq_new$Web)
aq_new$Block <- as.character(aq_new$Block)
aq_new$Quad <- as.character(aq_new$Quad)
aq_new$Observation <- as.character(aq_new$Observation)

names(aq_new)

# rename a few variables for naming consistency 
aq_new <- 
  aq_new %>% 
  dplyr::rename(Collection_Date = collection.date, Field_Sheet_Name = field.sheet.name, Collector = collector)

aq_new_unique <-
  aq_new %>% 
  select(Site, Plot, Quad, Web, Treatment, Subplot, Transect) %>% 
  unique() %>% 
  arrange(Site, Plot, Quad, Web, Treatment, Subplot, Transect)

names(aq_new)
aq_new_sites_unique <- 
  aq_new %>% 
  dplyr::count(Site)

nrow(aq_new)
nrow(aq)




aq_new$Collection_Date <- as.Date(aq_new$Collection_Date, format = "%d/%m/%Y")

str(aq_new)





# Look at species list ----
master.species.list.file <- "/Users/kris/Documents/SEV/NPP/NPP_QAQC_revision_2019/NPP_QAQC/data/data_other/SevilletaSpeciesList_AJH.csv"

master.species.list <- read_csv(master.species.list.file)

species_list_grass_flag <- 
  master.species.list %>% 
  select(kartez, g_f) %>% 
  dplyr::rename(species = kartez)





# Look at differences between AJ_AllQuads and NPP_quad_20190323 ----
dim(aq)
dim(aq_new)

names(aq)
names(aq_new)

str(aq)
str(aq_new)

aq %>% 
  summarize(Earliest_date = min(date, na.rm = TRUE),
            Latest_date   = max(date, na.rm = TRUE))

aq_new %>% 
  summarize(Earliest_date = min(Collection_Date, na.rm = TRUE),
            Latest_date   = max(Collection_Date, na.rm = TRUE))


# TODO: 
# Will want to make sure metadata lists 1/10/1999 as the earliest collection date instead of the previously earliest
# collection date of 2/2/1999.


test_plot <-
  aq_new %>% 
  filter(Collection_Date == "2018-10-05" & Site == "grassland_burn" & Treatment == "B" & Quad == "1")

test_plot %>% 
  summarize(Total_Cover = sum(Cover * Count))

aq_new2 <- 
  aq_new %>% 
  mutate(Cover1 = Cover * Count)

cover_totals <- 
  aq_new2 %>% 
  dplyr::group_by(Year, Season, Collection_Date, Site, Treatment, Transect, Web, Block, Plot, Subplot, Quad) %>% 
  dplyr::summarize(Total_Cover = sum(Cover1, na.rm = TRUE))
summary(cover_totals$Total_Cover)

cover_totals_bad <- 
  cover_totals %>% 
  filter(Total_Cover > 100.00) %>% 
  dplyr::arrange(Site, Treatment, Web, Block, Plot, Subplot, Quad, Collection_Date)

View(cover_totals_bad %>% ungroup() %>% 
  dplyr::select("Site") %>% 
  unique())

# look at a couple of examples where cover has been > 100% on multiple occasions
tst <- 
  aq_new %>% 
  filter(Site == "core_blue" & Treatment == "C" & Web == "1" & Plot == "E" & Quad == "1")

tst_cover <- 
  tst %>% 
  group_by(Year, Season, Collection_Date, Site, Treatment, Web, Block, Plot, Subplot, Quad) %>% 
  summarize(Total_Cover = sum(Cover * Count, na.rm = TRUE)) %>% 
  arrange(Year, Season)
# NOTE: There are some pretty crazy shifts in total_cover in this example.  Is that typical?


tst2 <- 
  aq_new %>% 
  filter(Site == "core_creosote" & Treatment == "C" & Web == "4" & Plot == "W" & Quad == "3")

tst2_cover <- 
  tst2 %>% 
  group_by(Year, Season, Collection_Date, Site, Treatment, Web, Block, Plot, Subplot, Quad) %>% 
  summarize(Total_Cover = sum(Cover * Count, na.rm = TRUE)) %>% 
  arrange(Year, Season)
# NOTE: There are some pretty crazy shifts in total_cover in this example.  Is that typical?





# Need to figure out what treatments are appropriate ----
site_trt_combos <-
  aq_new %>% 
  count(Site, Treatment)
# TODO: Need to figure out the proper Treatment combinations to be checking on.  Otherwise, it won't be possible
# to do Site-treatment checks.
# Possible that data has been processed wrong, given that NAs replaced with "C"


# let's look at the most recent year of data for Site-Treatment
aq18_new <-
  aq_new %>% 
  filter(Year >= "2018")

site_trt_combos18 <- 
  aq18_new %>% 
  count(Site, Treatment)
# Same deal.  Lots of Treatments.  



