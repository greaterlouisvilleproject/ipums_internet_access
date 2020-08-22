# Libraries
library(tidyverse)
library(survey)

setwd("~/Documents/pers/ipums_internet_access")

# # Read in data from IPUMS
# df <- read_csv("mpi_13_18_raw.csv",
#                col_types = cols(
#                  YEAR = col_double(),
#                  SAMPLE = col_double(),
#                  SERIAL = col_double(),
#                  CBSERIAL = col_double(),
#                  HHWT = col_double(),
#                  CLUSTER = col_double(),
#                  STATEFIP = col_double(),
#                  METRO = col_double(),
#                  MET2013 = col_double(),
#                  PUMA = col_double(),
#                  STRATA = col_double(),
#                  GQ = col_double(),
#                  FARM = col_double(),
#                  OWNCOST = col_double(),
#                  RENTGRS = col_double(),
#                  HHINCOME = col_double(),
#                  LINGISOL = col_double(),
#                  BEDROOMS = col_double(),
#                  CINETHH = col_double(),
#                  CILAPTOP = col_double(),
#                  CISMRTPHN = col_double(),
#                  CITABLET = col_double(),
#                  CIHAND = col_double(),
#                  CIOTHCOMP = col_double(),
#                  PERNUM = col_double(),
#                  PERWT = col_double(),
#                  SEX = col_double(),
#                  AGE = col_double(),
#                  RACE = col_double(),
#                  RACED = col_double(),
#                  HISPAN = col_double(),
#                  HISPAND = col_double(),
#                  CITIZEN = col_double(),
#                  HCOVANY = col_double(),
#                  EDUC = col_double(),
#                  EDUCD = col_double(),
#                  EMPSTAT = col_double(),
#                  EMPSTATD = col_double(),
#                  INCINVST = col_double(),
#                  INCRETIR = col_double(),
#                  POVERTY = col_double(),
#                  DIFFMOB = col_double(),
#                  DIFFCARE = col_double()
#                )
# )
#
# df_lou <- df %>%
#   filter(MET2013 == 31140) %>%
#   write_csv("louisville_mpi.csv") #louisville MSA in 2018 in order to test

df <- read_csv("louisville_mpi.csv",
               col_types = cols(
                 YEAR = col_double(),
                 SAMPLE = col_double(),
                 SERIAL = col_double(),
                 CBSERIAL = col_double(),
                 HHWT = col_double(),
                 CLUSTER = col_double(),
                 STATEFIP = col_double(),
                 METRO = col_double(),
                 MET2013 = col_double(),
                 PUMA = col_double(),
                 STRATA = col_double(),
                 GQ = col_double(),
                 FARM = col_double(),
                 OWNCOST = col_double(),
                 RENTGRS = col_double(),
                 HHINCOME = col_double(),
                 LINGISOL = col_double(),
                 BEDROOMS = col_double(),
                 CINETHH = col_double(),
                 CILAPTOP = col_double(),
                 CISMRTPHN = col_double(),
                 CITABLET = col_double(),
                 CIHAND = col_double(),
                 CIOTHCOMP = col_double(),
                 PERNUM = col_double(),
                 PERWT = col_double(),
                 SEX = col_double(),
                 AGE = col_double(),
                 RACE = col_double(),
                 RACED = col_double(),
                 HISPAN = col_double(),
                 HISPAND = col_double(),
                 CITIZEN = col_double(),
                 HCOVANY = col_double(),
                 EDUC = col_double(),
                 EDUCD = col_double(),
                 EMPSTAT = col_double(),
                 EMPSTATD = col_double(),
                 INCINVST = col_double(),
                 INCRETIR = col_double(),
                 POVERTY = col_double(),
                 DIFFMOB = col_double(),
                 DIFFCARE = col_double()
               ))

# recode internet, computer, smartphone, and tablet to be binary
df <- df %>%
  mutate(
    #deprived of internet acess = 1
    int_acc = case_when(
      CINETHH == 0 ~ NA_real_,
      CINETHH == 1 ~ 0,
      CINETHH == 2 ~ 0,
      CINETHH == 3 ~ 1
    ),
    computer = case_when(
      CILAPTOP == 0 ~ NA_real_,
      CILAPTOP == 1 ~ 1,
      CILAPTOP == 2 ~ 0
    ),
    smartphone = case_when(
      CISMRTPHN == 0 ~ NA_real_,
      CISMRTPHN == 1 ~ 0,
      CISMRTPHN == 2 ~ 1
    ),
    tablet = case_when(
      CITABLET == 0 ~ NA_real_,
      CITABLET == 1 ~ 1,
      CITABLET == 2 ~ 0
    ),
    #deprived of computer or tablet = 1
    comp_or_tab = case_when(
      computer == 1 | tablet == 1 ~ 0,
      computer == 0 & tablet == 0 ~ 1,
      TRUE ~ NA_real_
    )
  )

#@ need to filter to just Jefferson County. Currently including all of metro area

df <- df %>%
  filter(PUMA %in% c("1701", "1702", "1703", "1704", "1705", "1706"))

# These questions are usually only NA for group quarters
# All the NA rows are the same for all categories in the Louisville sample
df_na <- df %>%
  filter(!is.na(int_acc) & !is.na(computer))

##Section on Children
df_ch <- df_na %>%
  filter(AGE > 4 & AGE < 19 & YEAR == 2018)

svy_df <- svydesign(ids = ~ 1, weights = ~PERWT, data = df_ch)

int_tbl <- svyby(~int_acc, ~YEAR, design = svy_df, svymean)
comp_or_tab_tbl <- svyby(~comp_or_tab, ~YEAR, design = svy_df, svymean)

int_tbl_count <- svyby(~int_acc, ~YEAR, design = svy_df, svytotal)
comp_or_tab_tbl_count <- svyby(~comp_or_tab, ~YEAR, design = svy_df, svytotal)


##Making a map
int_tbl <- svyby(~int_acc, ~YEAR + PUMA, design = svy_df, svymean)
comp_or_tab_tbl <- svyby(~comp_or_tab, ~YEAR + PUMA, design = svy_df, svymean)

int_tbl_count <- svyby(~int_acc, ~YEAR + PUMA, design = svy_df, svytotal)
comp_or_tab_tbl_count <- svyby(~comp_or_tab, ~YEAR + PUMA, design = svy_df, svytotal)

library(ggplot2)
library(rgdal)
library(sf)

ky_shp <- readOGR("cb_2016_21_puma10_500k", layer = "cb_2016_21_puma10_500k", GDAL1_integer64_policy = TRUE)
ky_shp@data$PUMA <- as.numeric(as.character(ky_shp@data$PUMACE10))
ky_sf <- st_as_sf(ky_shp)

jeff_co_sf <- ky_sf %>% 
  filter(ky_sf$PUMA %in% c("1701", "1702", "1703", "1704", "1705", "1706"))

int_puma <- tibble(
  PUMA = int_tbl$PUMA,
  int_per = int_tbl$int_acc * 100,
  int_num = formattable::comma(round(int_tbl_count$int_acc, -2), digits = 0),
  ct_per = comp_or_tab_tbl$comp_or_tab * 100,
  ct_num = formattable::comma(round(comp_or_tab_tbl_count$comp_or_tab, -2), digits = 0)
)

jeff_co_sf <- full_join(jeff_co_sf, int_puma, by = "PUMA")

ggplot(jeff_co_sf) + 
  geom_sf(aes(fill=ct_per)) +
  geom_sf_label(aes(label = ct_num, fontface = "bold")) +
  scale_fill_gradient(low = "blue", high = "purple", name = "Percent") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank()) +
  labs(title = "Children ages 5-18 in households without a Computer or Tablet",
       caption = "Map is shaded by percent of children without access in each Public Use Microdata Area and 
       the number of children without acess is given by the label rounded to the nearest 100 \n
       Greater Louisville Project Analysis of Census Microdata from IPUMs")

ggplot(jeff_co_sf) + 
  geom_sf(aes(fill=int_per)) +
  geom_sf_text(aes(label = int_num, fontface = "bold")) +
  scale_fill_gradient(low = "blue", high = "purple", name = "Percent") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.border = element_blank()) +
  labs(title = "Children ages 5-18 in households without Internet Access",
       caption = "Map is shaded by percent of children without access in each Public Use Microdata Area and 
       the number of children without acess is given by the label rounded to the nearest 100 \n
       Greater Louisville Project Analysis of Census Microdata from IPUMs")

# google API key required for ggmap
# library(ggmap)
# lou_basemap <- get_map(location=c(lon = -85.7585, lat = 38.2527), zoom=11, maptype = 'terrain-background', source = 'stamen')


library(tmap)
tm_shape(jeff_co_sf) +
  tm_polygons("int_per", style="quantile", title="Children Ages 5-18 Without Home Internet Access, 2018 \n(Percent)", palette = "Blues")




#past section on children

df18 <- df %>%
  filter(YEAR == 2018)

svy_df18 <- svydesign(ids = ~ 1, weights = ~PERWT, data = df18)

svytotal(~comp_or_tab, svy_df18, na.rm = TRUE)

int_tbl <- svyby(~int_acc, ~YEAR, design = svy_df, svymean)
comp_tbl <- svyby(~computer, ~YEAR, design = svy_df, svymean)
comp_int_tbl <- svyby(~comp_and_int, ~YEAR, design = svy_df, svymean)
smart_int_tbl <- svyby(~smart_or_int, ~YEAR, design = svy_df, svymean)
smart_and_int_tbl <- svyby(~smart_and_int, ~YEAR, design = svy_df, svymean)


#smart phone and tablet data starts in 2016
df_na_16 <- df %>%
  filter(!is.na(smartphone) & !is.na(tablet))

svy_df_smart <- svydesign(ids = ~ 1, weights = ~PERWT, data = df_na_16)

smart_tbl <- svyby(~smartphone, ~YEAR, design = svy_df_smart, svymean)
tab_tbl <- svyby(~tablet, ~YEAR, design = svy_df, svymean)
comp_tab_tbl <- svyby(~comp_or_tab, ~YEAR, design = svy_df, svymean)

# Other characteristics to cross with internet access --------------------------------

df <- df %>%
  mutate(poverty = if_else(POVERTY < 100 & POVERTY != 000, 1, 0),
         under65 = if_else(AGE < 65, 1, 0))

df_na <- df %>%
  filter(!is.na(int_acc) & !is.na(computer))

svy_df <- svydesign(ids = ~ 1, weights = ~PERWT, data = df_na)

int_pov_tbl <- svyby(~int_acc, ~YEAR+poverty, design = svy_df, svymean)
int_age_tbl <-svyby(~int_acc, ~YEAR+under65, design = svy_df, svymean)
int_race_tbl <- svyby(~int_acc, ~YEAR+RACE, design = svy_df, svymean)

int_pov_tbl <- int_pov_tbl %>%
  mutate(Poverty = if_else(poverty == 0, "Above Poverty Line", "Below Poverty Line"),
         int = int_acc * 100,
         se100 = se * 100)

plt_by <- function(df, group_var, title_text = "Internet Access at Home") {
  group_var <- enquo(group_var)

  plt <- ggplot(data = df, aes(x = YEAR, y = int, group = !!group_var, colour = !!group_var)) +
    geom_errorbar(aes(ymin = int - (1.96 * se100), ymax = int + 1.96 * se100), width = .1) +
    geom_point() +
    geom_line() +
    theme_bw() +
    labs(title = title_text, x = "Year", y = "Percent") +
    theme(legend.position = "bottom")

  plt
}

plt_pov <- plt_by(int_pov_tbl, Poverty)

int_race_tbl <- int_race_tbl %>%
  filter(RACE < 3) %>% #Louisville's racial groups other than White and Black have small populations leading to low sample sizes
  mutate(Race = if_else(RACE == 1, "White", "Black"),
         int = int_acc * 100,
         se100 = se * 100)

plt_race <- plt_by(int_race_tbl, Race, title_text = "Internet Access")
plt_race

int_age_tbl <- int_age_tbl %>%
  mutate(Age = if_else(under65 == 1, "Under 65", "Over 65"),
         int = int_acc * 100,
         se100 = se * 100)

plt_age <- plt_by(int_age_tbl, Age, title_text = "Internet Access")
plt_age

int_tbl <- int_tbl %>%
  mutate(int = int_acc * 100,
         se100 = se * 100)

plt_int <- ggplot(int_tbl, aes(x = YEAR, y = int)) +
  geom_errorbar(aes(ymin = int - (1.96 * se100), ymax = int + 1.96 * se100), width = .1) +
  geom_line() +
  geom_point() +
  theme_bw() +
  labs(title = "Household Internet Access", x = "Year", y = "Percent") +
  theme(legend.position = "bottom")

# Next step is graphing the PUMAs






