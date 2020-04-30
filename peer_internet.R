# Libraries
library(tidyverse)
library(survey)

# Read in data from IPUMS
df <- read_csv("~/Documents/pers/mpitools/mpi_13_18_raw.csv",
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
               )
)

df_city <- df %>%
  filter(MET2013 %in% c(24340, 41180, 36420, 46140, 24860, 28940, 13820, 31140, 26900, 
                        28140, 36540, 24660, 16740, 18140, 17140, 34980, 32820, 27260, 
                        39580, 19380, 40060)) %>%
  mutate(city = case_when(
    MET2013 == 24340 ~ "Grand Rapids",
    MET2013 == 41180 ~ "St. Louis",
    MET2013 == 36420 ~ "Oklahoma City",
    MET2013 == 46140 ~ "Tulsa",
    MET2013 == 24860 ~ "Greenville",
    MET2013 == 28940 ~ "Knoxville",
    MET2013 == 13820 ~ "Birmingham",
    MET2013 == 31140 ~ "Louisville",
    MET2013 == 26900 ~ "Indianapolis",
    MET2013 == 28140 ~ "Kansas City",
    MET2013 == 36540 ~ "Omaha",
    MET2013 == 24660 ~ "Greensboro",
    MET2013 == 16740 ~ "Charlotte",
    MET2013 == 18140 ~ "Columbus",
    MET2013 == 17140 ~ "Cincinnati",
    MET2013 == 34980 ~ "Nashville",
    MET2013 == 32820 ~ "Memphis",
    MET2013 == 27260 ~ "Jacksonville",
    MET2013 == 39580 ~ "Raleign",
    MET2013 == 19380 ~ "Dayton",
    MET2013 == 40060 ~ "Richmond",
    TRUE ~ NA_character_
  )) %>%
  filter(!(city %in% c("Jacksonville", "Raleigh", "Dayton", "Richmond"))) # cities from old peer list

# df_tulsa <- df %>%
#   filter(STATEFIP == 40 &
#            PUMA %in% c(100, 1201, 1202, 1203, 1204, 1301, 1302, 1501, 1601,
#                        1100, 1200, 1000, 900, 600, 800))

# recode internet, computer, smartphone, and tablet to be binary
df <- df_city %>%
  mutate(
    int_acc = case_when(
      CINETHH == 0 ~ NA_real_,
      CINETHH == 1 ~ 1,
      CINETHH == 2 ~ 1,
      CINETHH == 3 ~ 0
    ),
    computer = case_when(
      CILAPTOP == 0 ~ NA_real_,
      CILAPTOP == 1 ~ 1,
      CILAPTOP == 2 ~ 0
    ),
    smartphone = case_when(
      CISMRTPHN == 0 ~ NA_real_,
      CISMRTPHN == 1 ~ 1,
      CISMRTPHN == 2 ~ 0
    ),
    tablet = case_when(
      CITABLET == 0 ~ NA_real_,
      CITABLET == 1 ~ 1,
      CITABLET == 2 ~ 0
    ),
    comp_or_tab = case_when(
      computer == 1 | tablet == 1 ~ 1,
      computer == 0 & tablet == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    comp_and_int = case_when(
      computer == 1 & int_acc == 1 ~ 1,
      computer == 0 | int_acc == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    smart_and_int = case_when(
      smartphone == 1 & int_acc == 1 ~ 1,
      smartphone == 0 | int_acc == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    smart_or_int = case_when(
      smartphone == 1 | int_acc == 1 ~ 1,
      smartphone == 0 & int_acc == 0 ~ 0,
      TRUE ~ NA_real_
    )
  )

# These questions are usually only NA for group quarters
# All the NA rows are the same for all categories in the Louisville sample
df_na <- df %>%
  filter(!is.na(int_acc) & !is.na(computer))

svy_df <- svydesign(ids = ~ 1, weights = ~PERWT, data = df_na)

int_tbl <- svyby(~int_acc, ~YEAR+city, design = svy_df, svymean)
comp_tbl <- svyby(~computer, ~YEAR+city, design = svy_df, svymean)

library(glptools)
library(classInt)
library(ggthemes)

int_2018 <- int_tbl %>%
  filter(YEAR == 2018) %>%
  mutate(current = 1)

plt1 <- ranking(int_2018, int_acc)
