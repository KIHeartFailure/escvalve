
ProjectTemplate::reload.project()

dataass <- mice::complete(imp, 7)

# dataass <- dataass[edataforimp$num_nation != "LATVIA", ]

# check assumptions for cox models ----------------------------------------

mod <- coxph(formula(paste0("Surv(outtime_hosphf, out_deathcvhosphf == 1) ~ valve + ", 
                            paste0(modvarscox, collapse = "+"))),
  data = dataass
)

testpat <- cox.zph(mod)
print(sig <- testpat$table[testpat$table[, 3] < 0.05, ])

x11()
plot(testpat[1], resid = F, ylim = c(-4, 4))
plot(testpat[6], resid = F, ylim = c(-4, 4))
plot(testpat[7], resid = F, ylim = c(-4, 4))

library(splines)
mod <- coxph(Surv(outtime_hosphf, out_deathhosphf == 1) ~ valve + num_dmgender +
ns(num_age, 4) + ns(num_dmBmi, 4) + d_Efcomb_cat +  num_opNyha_cat + d_HFdiagnosis + 
  d_dmHF_cat + num_dmEtio_c1 + num_dmAfib_c1 + d_cad + num_dmDiab_c1 + num_dmPvd +
  num_dmCopd + num_dmStroke + d_opCKDEPI_cat + ns(num_dmBpm, 4) + ns(num_dmBp1, 4) + ns(num_opSod, 4) +
  ns(num_opHb, 4) + ns(num_opNt, 4) + ns(num_opLvdd, 4),
             data = dataass
)

termplot(mod, terms = 3, ask = F, rug = T) # age 
termplot(mod, terms = 4, ask = F, rug = T) # bmi
termplot(mod, terms = 17, ask = F, rug = T) # EF
termplot(mod, terms = 18, ask = F, rug = T) # bpm
termplot(mod, terms = 19, ask = F, rug = T) # bp1
termplot(mod, terms = 20, ask = F, rug = T) # 
termplot(mod, terms = 21, ask = F, rug = T) #  
termplot(mod, terms = 22, ask = F, rug = T) #  


# 
ormod <- glm(valve == "MR" ~ num_dmgender +
               ns(num_age, 4) + ns(num_dmBmi, 4) + d_Efcomb_cat +  num_opNyha_cat + d_HFdiagnosis + 
               d_dmHF_cat + num_dmEtio_c1 + num_dmAfib_c1 + d_cad + num_dmDiab_c1 + num_dmPvd +
               num_dmCopd + num_dmStroke + d_opCKDEPI_cat + ns(num_dmBpm, 4) + ns(num_dmBp1, 4) + ns(num_opSod, 4) +
               ns(num_opHb, 4) + ns(num_opNt, 4) + ns(num_opLvdd, 4),
                       family = binomial(link = "logit"), data = dataass %>% filter(valve %in% c("MR", "No VHD"))
)
termplot(ormod, terms = 2, ask = F, rug = T) # age
termplot(ormod, terms = 3, ask = F, rug = T) # BMI 
termplot(ormod, terms = 16, ask = F, rug = T) 
termplot(ormod, terms = 17, ask = F, rug = T) 
termplot(ormod, terms = 18, ask = F, rug = T) 
termplot(ormod, terms = 19, ask = F, rug = T) 
termplot(ormod, terms = 20, ask = F, rug = T) 
termplot(ormod, terms = 21, ask = F, rug = T) 

ormodmr <- glm(formula(paste0("valve == 'MR' ~ ", paste0(modvars, collapse = " + "))),
             family = binomial(link = "logit"), data = dataass %>% filter(valve %in% c("MR", "No VHD"))
)
ormodtr <- glm(formula(paste0("valve == 'TR' ~ ", paste0(modvars, collapse = " + "))),
             family = binomial(link = "logit"), data = dataass %>% filter(valve %in% c("TR", "No VHD"))
)
ormodmtr <- glm(formula(paste0("valve == 'MR+TR' ~ ", paste0(modvars, collapse = " + "))),
             family = binomial(link = "logit"), data = dataass %>% filter(valve %in% c("MR+TR", "No VHD"))
)


# multicollinearit

car::vif(ormodmr)
car::vif(ormodtr)
car::vif(ormodmtr)

# Outliers
kontmodvars <- edata %>% 
  select(!!!syms(modvars)) %>%
  select(where(is.numeric))
nams <- colnames(kontmodvars)
for(i in 1:ncol(kontmodvars)){
  x11()
  plot(kontmodvars[, i], xlab = nams[i])
}

# nt och lvdd outliers 