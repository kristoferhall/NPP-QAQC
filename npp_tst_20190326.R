# KM Hall
# 20190326
#
# testing NPP daily data and program to look at speeding it up a bit.


library(tidyverse)
library(data.table)
library(lubridate)


# load AJ_AllQuads.csv as tibble (read_csv) versus read.csv
path <- "/Users/kris/Documents/SEV/NPP/NPP_QAQC_revision_2019/NPP_QAQC/data/data_other/AJ_AllQuads.csv"

system.time(aq_t <- read_csv(path, guess_max = 3000000))
# user  system elapsed 
# 3.480   0.323   3.865 

str(aq_t)
aq_t$date <- mdy(aq_t$date)

# variables that should be character format
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
for (i in seq_along(aq)) {
  if (is_character(aq[[i]])) {
    x <- aq %>% 
      dplyr::group_by(aq[[i]]) %>% 
      dplyr::summarise(count = n()) %>% 
      dplyr::arrange(-count)
    print(paste0(aq_charvar[[i]], ":"))
    print(x, n=1000)
  } else next
}
###
