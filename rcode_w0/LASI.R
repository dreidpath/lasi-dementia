library(haven)
library(tibble)
library(dplyr)
library(ggplot2)

dta <- read_dta("LASI-Pilot_all.dta")
dta$hhid_char <- as.character(dta$hhid)
hhid_tbl <- as_tibble(with(dta, table(hhid)))
hhid_tbl <-  select(hhid_tbl, hhid_char = hhid, hhsize = n)
  
dta <- merge(dta, hhid_tbl, by = "hhid_char")
dta <- dta %>% filter(hhsize ==2, ragender == 1)

dta %>% ggplot(aes(x = r0bmi, y = s0bmi)) + geom_point()

dta %>% lm(r0bmi ~ s0bmi, data = .) %>% summary()
