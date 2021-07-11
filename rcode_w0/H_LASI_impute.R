# Impute missing data

hdta$r0state <- factor(hdta$r0state,
                       levels = 1:4,
                       labels = c("Punjab", "Rajasthan", "Kerala", "Karnakata"))

hdta$ragender <- factor(hdta$ragender,
                        levels = 1:2,
                        labels = c("Male", "Female"))

hdta$raeduc <- factor(hdta$raeduc,
                      levels = 1:4,
                      labels = c("<HS", "HS Grad", "Some College", "College+"))

hdta$r0mstat <- factor(hdta$r0mstat,
                       levels = c(1,4,5,7,8),
                       labels = c("Married", "Separated", "Divorced", "Widowed", "Never Married"))

hdta$s0mstat <- factor(hdta$s0mstat,
                       levels = c(1,4,5,7,8),
                       labels = c("Married", "Separated", "Divorced", "Widowed", "Never Married"))

hdta$r0lvreg <- factor(hdta$r0lvreg,
                       levels = 1:2,
                       labels = c("Urban", "Rural"))

hdta$s0hlthlm <- factor(hdta$s0hlthlm,
                        levels = 0:1,
                        labels = c("No", "Yes"))

hdta$r0hlthlm <- factor(hdta$r0hlthlm,
                        levels = 0:1,
                        labels = c("No", "Yes"))

hdta$r0hibpe <- factor(hdta$r0hibpe,
                       levels = 0:1,
                       labels = c("No", "Yes"))

hdta$r0diabe <- factor(hdta$r0diabe,
                       levels = 0:1,
                       labels = c("No", "Yes"))

hdta$r0cancre <- factor(hdta$r0cancre,
                        levels = 0:1,
                        labels = c("No", "Yes"))

hdta$r0lunge <- factor(hdta$r0lunge,
                       levels = 0:1,
                       labels = c("No", "Yes"))

hdta$r0hearte <- factor(hdta$r0hearte,
                        levels = 0:1,
                        labels = c("No", "Yes"))

hdta$r0stroke <- factor(hdta$r0stroke,
                        levels = 0:1,
                        labels = c("No", "Yes"))

hdta$r0arthre <- factor(hdta$r0arthre,
                        levels = 0:1,
                        labels = c("No", "Yes"))

hdta$r0psyche <- factor(hdta$r0psyche,
                        levels = 0:1,
                        labels = c("No", "Yes"))

hdta$r0psyche <- factor(hdta$r0psyche,
                        levels = 0:1,
                        labels = c("No", "Yes"))

hdta$s0hibpe <- factor(hdta$s0hibpe,
                       levels = 0:1,
                       labels = c("No", "Yes"))

hdta$s0diabe <- factor(hdta$s0diabe,
                       levels = 0:1,
                       labels = c("No", "Yes"))

hdta$s0cancre <- factor(hdta$s0cancre,
                        levels = 0:1,
                        labels = c("No", "Yes"))

hdta$s0lunge <- factor(hdta$s0lunge,
                       levels = 0:1,
                       labels = c("No", "Yes"))

hdta$s0hearte <- factor(hdta$s0hearte,
                        levels = 0:1,
                        labels = c("No", "Yes"))

hdta$s0stroke <- factor(hdta$s0stroke,
                        levels = 0:1,
                        labels = c("No", "Yes"))

hdta$s0arthre <- factor(hdta$s0arthre,
                        levels = 0:1,
                        labels = c("No", "Yes"))

hdta$s0psyche <- factor(hdta$s0psyche,
                        levels = 0:1,
                        labels = c("No", "Yes"))

hdta$r0smokev <- factor(hdta$r0smokev,
                        levels = 0:1,
                        labels = c("No", "Yes"))

hdta$s0smokev <- factor(hdta$s0smokev,
                        levels = 0:1,
                        labels = c("No", "Yes"))

hdta$r0smoken <- factor(hdta$r0smoken,
                        levels = 0:1,
                        labels = c("No", "Yes"))

hdta$s0smoken <- factor(hdta$s0smoken,
                        levels = 0:1,
                        labels = c("No", "Yes"))

hdta$cogproxy3 <- factor(hdta$cogproxy3,
                         levels = 1:2,
                         labels = c("Yes", "No"))

hdta$cogproxy4 <- factor(hdta$cogproxy4,
                         levels = 1:2,
                         labels = c("Yes", "No"))

hdta$cogproxy5 <- factor(hdta$cogproxy5,
                         levels = 1:2,
                         labels = c("Yes", "No"))

hdta$cogproxy6 <- factor(hdta$cogproxy6,
                         levels = 1:2,
                         labels = c("Yes", "No"))

log_h0itot1 <- log(h0itot+1)
log_h0ctotb <- log(h0ctotb)
log_h0ctot <- log(h0ctot)

fml <- r0state + r0agey + s0agey + ragender + raedyrs + raeduc + r0mstat + h0hhresp + h0hhres + r0adla + s0adla + r0adlwa + s0adlwa + r0adlam + s0adlam +
  r0depresd + r0effortd + r0sleeprd + r0whappyd + r0floned + r0fsadd + r0goingd + r0enlifed + r0cesdd + r0cesdmd + s0depresd + s0effortd + 
  s0sleeprd + s0whappyd + s0floned + s0fsadd + s0goingd + s0enlifed + s0cesdd + s0cesdmd + r0hibpe + r0diabe + r0cancre + r0lunge + r0hearte +
  r0stroke + r0arthre + r0psyche + s0hibpe + s0diabe + s0cancre + s0lunge + s0hearte + s0stroke + s0arthre + s0psyche + r0adla + s0adla + 
  r0adlwa + s0adlwa + r0adlam + s0adlam + r0vgactx_l + s0vgactx_l + r0mdactx_l + s0mdactx_l + r0height + r0weight + s0height + s0weight +
  r0smokev + s0smokev + r0smoken + s0smoken + r0child + h0ctot + log_h0ctot + h0ctotb + log_h0ctotb + h0itot + log_h0itot1 + em + mmse + ss7 +
  cogproxy1 +  cogproxy2 +  cogproxy3 +  cogproxy4 +  cogproxy5 +  cogproxy6 ~ 1 + (1|hhid)


imp <- jomoImpute(hdta, formula=fml, n.burn=5000, n.iter=100, m=100)

