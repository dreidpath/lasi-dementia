#library(haven)
library(tibble)
library(dplyr)
library(ggplot2)


# Serial subtract 7 function
cogscores <- function(df_, impute=F, k=3){
  tmpdf_ <- select(df_, ht505, ht520, ht501_day, ht501_month, 
                   ht501_year, ht502, ht503, ht512, ht513, ht513_value, ht514, ht515, ht516, ht517)
  
  if(impute == T){
    tmpdf_ <- VIM::kNN(tmpdf_, variable = names(tmpdf_) , 
        k = k ,  dist_var =  names(tmpdf_),  
        weights = NULL ,  numFun = median ,  catFun = maxCat ,  
        impNA = TRUE ,  addRandom = FALSE ,  mixed = NULL ,  
        mixed.constant = NULL ) 
  }

  attach(tmpdf_)
  
  
  
  # Episodic memory
  ht505[ht505 > 10] <- 0
  ht520[ht520 > 20] <- 0
  stm <- ht505  # Short term memory (recall 0-10 words immediately) )
  ltm <- ht520  # Long term memory (recall 0-10 words in 5 minutes)
  em <- stm + ltm
  
  # Modified MMSE
  dd <- ifelse(ht501_day == 1, 1, 0) # today's day 
  mm <- ifelse(ht501_month == 1, 1, 0) # today's month 
  yy <- ifelse(ht501_year == 1, 1, 0) # today's year 
  day <- ifelse(ht502 == 1, 1, 0)  # today's day of the week
  pm <- ifelse(ht503 == 1, 1, 0)  # India's Prime Minister
  countback <- 3-ht512
  mmmse <- dd + mm + yy + day + pm + countback
  
  # Serial subtraction of 7
  ss7 <- 0
  ss7[ht513_value == 93 & ht514 == 86 & ht515 == 79 & ht516 == 72 & ht517 == 65] <- 5
  ss7[ht513_value == 93 & ht514 == 86 & ht515 == 79 & ht516 == 72 & ht517 != 65] <- 4
  ss7[ht513_value == 93 & ht514 == 86 & ht515 == 79 & ht516 != 72] <- 3
  ss7[ht513_value == 93 & ht514 == 86 & ht515 != 79] <- 2
  ss7[ht513_value == 93 & ht514 != 86] <- 1
  ss7[ht513 == 2] <- 0
  
  return( tibble(stm = stm, ltm = ltm, em = em, mmse = mmmse, ss7 = ss7))
}



# Load the RAND Harmonised data
# hdta <- read_dta("./data/H_LASI.dta")
load("./data/H_LASI.RData")
hdta$hhid_char <- as.character(hdta$hhid)

hdta <- hdta %>% filter(s0agey>=45, r0agey>=45) # The spouse and respondent have age>=45


# Load the full data
# dta <- read_dta("./data/LASI-Pilot_all.dta")
load("./data/LASI-Pilot_all.Rdata")
dta$hhid_char <- as.character(dta$hhid)

## Calculate the cognition scores in the full data
ttmp <- cogscores(dta, impute=T)  # With imputation of missing data
# Double Imputation 
ttmp <- VIM::kNN(ttmp, variable= c( "stm","ltm","em","mmse","ss7" ) , 
                     k= 3 ,  dist_var= c( "stm","ltm","em","mmse","ss7" ) ,  
                     weights= NULL ,  numFun= median ,  catFun= maxCat ,  
                     impNA= TRUE ,  addRandom= FALSE ,  mixed= NULL ,  
                     mixed.constant= NULL ) 


dta$stm <- ttmp$stm
dta$ltm <- ttmp$ltm
dta$em <- ttmp$em
dta$mmse <- ttmp$mmse
dta$ss7 <- ttmp$ss7

rm(ttmp)

dta <- dta[dta$prim_key %in% hdta$hhidpn, ]
hdta <- hdta[hdta$hhid_char %in% dta$hhid_char, ]  %>% arrange(hhidpn)
dta <- select(dta, hhidpn = prim_key, stm, ltm, em, mmse, ss7, cogproxy1 = ht601)
dta$hhidpn <- as.double(dta$hhidpn)
dta <-  arrange(dta, hhidpn)

hdta <- cbind(hdta, select(dta, stm, ltm, em, mmse, ss7)) #, cogproxy1, cogproxy2,
                           #cogproxy3, cogproxy4, cogproxy5, cogproxy6))
rm(dta)
rm(cogscores)

source("H_LASI_factors.R")

sdta <- hdta %>% select(rs0hhidpn = s0hhidpn, sstm = stm, sltm = ltm, 
                        sem = em, smmse = mmse, sss7 = ss7) %>% #, 
                        # scogproxy1 = cogproxy1, scogproxy2 = cogproxy2, 
                        # scogproxy3 = cogproxy3, scogproxy4 = cogproxy4, 
                        # scogproxy5 = cogproxy5, scogproxy6 = cogproxy6)  
                 arrange(rs0hhidpn)

spouse.dta <- cbind(hdta,sdta)

spouse.dta <- spouse.dta  %>% filter(s0agey >= r0agey) # The respondent is younger than the spouse
spouse.dta$age_diff <- spouse.dta$s0agey - spouse.dta$r0agey

## Touch up the cognitive functioning data
spouse.dta$cogfunc <- spouse.dta$stm + spouse.dta$ltm + spouse.dta$mmse + spouse.dta$ss7
spouse.dta$scogfunc <- spouse.dta$sstm + spouse.dta$sltm + spouse.dta$smmse + spouse.dta$sss7

