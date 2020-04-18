# Libraries
library(tidyverse)
library(survey)

# Read in data from IPUMS
df <- read_csv("./mpitools/mpi_13_18_raw.csv",
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

df_18_lou <- df %>%
  filter(MET2013 == 31140) %>%
  write_csv("louisville_mpi.csv") #louisville MSA in 2018 in order to test

df <- read_csv("louisville_mpi.csv")

# recode internet, computer, smartphone, and tablet to be binary
df <- df_18_lou %>%
  mutate(
    int_acc = case_when(
      CINETHH == 0 ~ NA_real_,
      CINETHH == 1|2 ~ 1,
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
    )
  )



