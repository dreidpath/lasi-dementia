# This file recodes variables and creates factors for the analysis.
# The chosen variables is based on: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4111300/

hdta$rage_cat <- hdta$r0agey
hdta$sage_cat <- hdta$s0agey

hdta$rcentage <- hdta$r0agey - 45  # Center age at 45
hdta$scentage <- hdta$s0agey - 45  # Center age at 45

hdta$rcentage2 <- hdta$rcentage^2  # Square of centered age
hdta$scentage2 <- hdta$scentage^2  # Square of centered age

hdta$reduc_yrs <- hdta$raedyrs  # Education in years
hdta$seduc_yrs <- hdta$saedyrs  # Education in years

hdta$hhspend_log <- log(hdta$h0ctot) # log household expenditure
hdta$children <- hdta$r0child
hdta$radl <- factor(hdta$r0adlwa>0,
                    levels = c(F, T),
                    labels = c("None", "Some")) # number of ADL issues: bath, dress, eat, bed, walk
hdta$sadl <- factor(hdta$s0adlwa>0,
                    levels = c(F, T),
                    labels = c("None", "Some")) # number of ADL issues: bath, dress, eat, bed, walk

hdta$rdistress <- hdta$r0cesdd  # Center of Epidemiologic Studies-Depression (CESD) Scale
hdta$sdistress <- hdta$s0cesdd  # Center of Epidemiologic Studies-Depression (CESD) Scale

## Factors

# Gender
hdta$gender <- factor(hdta$ragender,
                        levels = 1:2,
                        labels = c("male", "female"))
# Age categories respondent
hdta$rage_cat <- cut(hdta$rage_cat,
                        breaks = c(44, 50, 60, 70, 120),
                        labels = c("45-49", "50-59", "60-69", "70+"))

# Age categories spouse
hdta$sage_cat <- cut(hdta$sage_cat,
                     breaks = c(44, 50, 60, 70, 120),
                     labels = c("45-49", "50-59", "60-69", "70+"))


# Uban Rural
hdta$residency <- factor(hdta$r0lvreg,
                    levels = 1:2,
                    labels = c("urban", "rural"))

# State
hdta$state <- factor(hdta$r0state,
                     levels = 1:4,
                     labels = c("punjab", "rajasthan", "kerala", "karnataka"))

# Cowbelt
hdta$latitude <- NA
hdta$latitude[hdta$r0state<=2] <- "North"
hdta$latitude[hdta$r0state>=3] <- "South"


# hdta$latitude <- cut(hdta$r0state,
#                     breaks = c(0, 2, 5),
#                     labels = c("north", "south"))

# Education respondent
hdta$reducation <- hdta$raeduc
hdta$reducation[hdta$raedyrs==0] <- 0
hdta$reducation[hdta$reducation>2] <- 2
hdta$reducation <- factor(hdta$reducation,
                     levels = 0:2,
                     labels = c("no school" ,"<high school", "high school+"))

# Education respondent spouse   ***********
hdta$seducation <- hdta$s0educ
hdta$seducation[hdta$s0edyrs==0] <- 0
hdta$seducation[hdta$seducation>2] <- 2
hdta$seducation <- factor(hdta$seducation,
                          levels = 0:2,
                          labels = c("no school" ,"<high school", "high school+"))


# Quintiles of household expenditure / Socioeconomic Position
hdta$sep_q5 <- cut(log(hdta$h0ctot),
                breaks = quantile(log(hdta$h0ctot), probs = seq(0, 1, by = 1/5),  include.lowest= TRUE),
                labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
    
# Caste respondent
hdta$rcaste <- hdta$racaste
hdta$rcaste[hdta$rcaste>3] <- 4
hdta$rcaste <- factor(hdta$rcaste,
                     levels = 1:4,
                     labels = c("s.caste", "s.tribe", "obc", "other"))

# Caste spouse
hdta$scaste <- hdta$s0caste
hdta$scaste[hdta$scaste>3] <- 4
hdta$scaste <- factor(hdta$scaste,
                      levels = 1:4,
                      labels = c("s.caste", "s.tribe", "obc", "other"))


# BMI respondent
hdta$rbmi <- cut(hdta$r0bmi, breaks=c(11, 18.5, 25, 30, 50),
                     labels=c("Under",
                              "Normal",
                              "Over",
                              "Obese"))

hdta$rbmi <- relevel(hdta$rbmi, "Normal")

# BMI spouse
hdta$sbmi <- cut(hdta$s0bmi, breaks=c(11, 18.5, 25, 30, 50),
                 labels=c("Under",
                          "Normal",
                          "Over",
                          "Obese"))

hdta$sbmi <- relevel(hdta$sbmi, "Normal")


# Cardiovascular problem indicator respondent
hdta$rcardio <- as.integer(with(hdta, (r0hibpe == 1 | r0diabe == 1 | r0hearte == 1 | r0stroke == 1)))
hdta$rcardio[is.na(hdta$rcardio)] <- 2 
hdta$rcardio <- factor(hdta$rcardio,
                      levels = 0:2,
                      labels = c("no", "yes", "missing"))


# Cardiovascular problem indicator spouse
hdta$scardio <- as.integer(with(hdta, (s0hibpe == 1 | s0diabe == 1 | s0hearte == 1 | s0stroke == 1)))
hdta$scardio[is.na(hdta$scardio)] <- 2 
hdta$scardio <- factor(hdta$scardio,
                       levels = 0:2,
                       labels = c("no", "yes", "missing"))


# Smoking
hdta$hhsmoke <- 0
hdta$hhsmoke[hdta$r0smokev == 1 & hdta$s0smokev == 1] <- 3
hdta$hhsmoke[hdta$r0smokev == 1 & hdta$s0smokev == 0] <- 1
hdta$hhsmoke[hdta$r0smokev == 0 & hdta$s0smokev == 1] <- 2
hdta$hhsmoke <- factor(hdta$hhsmoke,
                      levels = 0:3,
                      labels = c("none", "respondent", "spouse", "both"))

# Vigorous physical activity respondent
hdta$rvigorous <- hdta$r0vgactx_l
hdta$rvigorous[hdta$rvigorous == 2] <- 1  # More than once per week
hdta$rvigorous[hdta$rvigorous == 3] <- 5  # Less than 1 per week
hdta$rvigorous[hdta$rvigorous == 4] <- 5  # Less than 1 per week
hdta$rvigorous[is.na(hdta$rvigorous)] <- 5  # Missing replaced by hardly ever

hdta$rvigorous <- factor(hdta$rvigorous,
                        levels = c(1,5),
                        labels = c(">1 per week", "<1 per week"))


# Vigorous physical activity spouse
hdta$svigorous <- hdta$s0vgactx_l
hdta$svigorous[hdta$svigorous == 2] <- 1  # More than once per week
hdta$svigorous[hdta$svigorous == 3] <- 5  # Less than 1 per week
hdta$svigorous[hdta$svigorous == 4] <- 5  # Less than 1 per week
hdta$svigorous[is.na(hdta$svigorous)] <- 5  # Missing replaced by hardly ever

hdta$svigorous <- factor(hdta$svigorous,
                         levels = c(1,5),
                         labels = c(">1 per week", "<1 per week"))

