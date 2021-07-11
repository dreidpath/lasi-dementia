library(haven)
library(tibble)
library(dplyr)
library(ggplot2)


# Serial subtract 7 function
cogscores <- function(df_){
  attach(df_)
  
  # Episodic memory
  ht505[ht505 > 10] <- 0
  ht520[ht520 > 20] <- 0
  em <- ht505 + ht520
  
  # Modified MMSE
  dd <- ifelse(ht501_day == 1, 1, 0)
  mm <- ifelse(ht501_month == 1, 1, 0)
  yy <- ifelse(ht501_year == 1, 1, 0)
  day <- ifelse(ht502 == 1, 1, 0)
  pm <- ifelse(ht503 == 1, 1, 0)
  countback <- 3-ht512
  mmmse <- dd + mm + yy + day + pm + countback
  
  # Serial subtraction of 7
  ss7 <- 0
  ss7[ht513_value == 93 & ht514 == 86 & ht515 == 79 & ht515 == 72 & ht515 == 65] <- 5
  ss7[ht513_value == 93 & ht514 == 86 & ht515 == 79 & ht515 == 72 & ht515 != 65] <- 4
  ss7[ht513_value == 93 & ht514 == 86 & ht515 == 79 & ht515 != 72] <- 3
  ss7[ht513_value == 93 & ht514 == 86 & ht515 != 79] <- 2
  ss7[ht513_value == 93 & ht514 != 86] <- 1
  ss7[ht513 == 2] <- 0
  
  return( list(em = em, mmse = mmmse, ss7 = ss7))
}



# Load the RAND Harmonised data
hdta <- read_dta("H_LASI.dta")
hdta$hhid_char <- as.character(hdta$hhid)

hdta <- hdta %>% filter(!is.na(s0agey), r0agey>=45) # There is a spouse and age>=45


# Load the full data
dta <- read_dta("LASI-Pilot_all.dta")
dta$hhid_char <- as.character(dta$hhid)

## Calculate the cognition scores in the full data
ttmp <- cogscores(dta)
dta$em <- ttmp$em
dta$mmse <- ttmp$mmse
dta$ss7 <- ttmp$ss7

rm(ttmp)

dta <- dta[dta$hhid_char %in% hdta$hhid_char, ]
hdta <- hdta[hdta$hhid_char %in% dta$hhid_char, ]  %>% arrange(hhidpn)
dta <- select(dta, hhidpn = as.double(prim_key), em, mmse, ss7, cogproxy1 = ht601,
              cogproxy2 = ht602, cogproxy3 = ht603, cogproxy4 = ht604,
              cogproxy5 = ht605, cogproxy6 = ht606) %>% arrange(hhidpn)

hdta <- cbind(hdta, select(dta, em, mmse, ss7, cogproxy1, cogproxy2,
                           cogproxy3, cogproxy4, cogproxy5, cogproxy6))
rm(dta)
rm(cogscores)

