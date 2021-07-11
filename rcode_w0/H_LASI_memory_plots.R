# LTM (Long term memory score)
cor_em <- paste("r=",as.character(round(with(spouse.dta, cor(ltm, sltm)),2)))
p <- ggplot(spouse.dta, aes(x = jitter(ltm, 1.5), y = jitter(sltm, 1.5))) +
  geom_point() +
  #  geom_smooth(method = "loess") +
  geom_smooth(method = "lm", color = "Black") +
  theme_classic() +
  xlab("Short term memory score") +
  ylab("Spouse's short term memory score") +
  annotate("text", x=9, y=0, label = cor_em)
ggExtra::ggMarginal(p, type = "histogram")


# STM (Short term memory score)
cor_em <- paste("r=",as.character(round(with(spouse.dta, cor(stm, sstm)),2)))
p <- ggplot(spouse.dta, aes(x = jitter(stm, 1.5), y = jitter(sstm, 1.5))) +
  geom_point() +
  #  geom_smooth(method = "loess") +
  geom_smooth(method = "lm") +
  theme_classic() +
  xlab("Shor term memory score") +
  ylab("Spouse's short term memory score") +
  annotate("text", x=9, y=0, label = cor_em)
ggExtra::ggMarginal(p, type = "histogram")


# EM (Total memory score)
cor_em <- paste("r=",as.character(round(with(spouse.dta, cor(em, sem)),2)))
p <- ggplot(spouse.dta, aes(x = jitter(em, 1.5), y = jitter(sem, 1.5))) +
  geom_point() +
#  geom_smooth(method = "loess") +
  geom_smooth(method = "lm") +
  theme_classic() +
  xlab("Total memory score") +
  ylab("Spouse's total memory score") +
  annotate("text", x=18, y=0, label = cor_em)
ggExtra::ggMarginal(p, type = "histogram")
