
# keep original imputed data (rsdataimp) just in case
imp.org <- imp

# Convert to Long
long <- mice::complete(imp, action = "long", include = TRUE)

long <- long %>%
  mutate(
    num_age_cat = factor(case_when(
      is.na(num_age) ~ NA_real_,
      num_age < 65 ~ 1,
      num_age >= 65 ~ 2
    ), levels = 1:2, labels = c("<65", ">=65")),
    num_dmBmi_cat = factor(case_when(
      is.na(num_dmBmi) ~ NA_real_,
      num_dmBmi < 25 ~ 1,
      num_dmBmi >= 25 ~ 2
    ), levels = 1:2, labels = c("<25", ">=25")),
    num_dmBp1_cat = factor(case_when(
      is.na(num_dmBp1) ~ NA_real_,
      num_dmBp1 < 100 ~ 1,
      num_dmBp1 >= 100 ~ 2
    ), levels = 1:2, labels = c("<100", ">=100")),
    num_dmBpm_cat = factor(case_when(
      is.na(num_dmBpm) ~ NA_real_,
      num_dmBpm < 70 ~ 1,
      num_dmBpm >= 70 ~ 2
    ), levels = 1:2, labels = c("<70", ">=70")),
    num_opSod_cat = factor(
      case_when(
        is.na(num_opSod) ~ NA_real_,
        num_opSod < 135 ~ 2,
        num_opSod >= 135 ~ 1
      ),
      levels = 1:2,
      labels = c(">=135", "<135")
    ),
    num_opHb_cat = factor(
      case_when(
        is.na(num_opHb) | is.na(num_dmgender) ~ NA_real_,
        num_opHb < 13 & num_dmgender == "Male" ~ 2,
        num_opHb < 12 & num_dmgender == "Female" ~ 2,
        TRUE ~ 1
      ),
      levels = 1:2,
      labels = c(">=13/12 (w/m)", "<13/12 (w/m)")
    ),
    num_opLvdd_cat = factor(
      case_when(
        is.na(num_opLvdd) ~ NA_real_,
        num_opLvdd < 60 ~ 1,
        num_opLvdd >= 60 ~ 2
      ),
      levels = 1:2,
      labels = c("<60", ">=60")
    ),
    num_opNt_cat = factor(
      case_when(
        is.na(num_opNt) ~ NA_real_,
        num_opNt < mednt ~ 1,
        num_opNt >= mednt ~ 2
      ),
      levels = 1:2,
      labels = c("<median", ">=median")
    ),
    d_opCKDEPI_cat = factor(
      case_when(
        is.na(d_opCKDEPI) ~ NA_real_,
        d_opCKDEPI < 60 ~ 2,
        d_opCKDEPI >= 60 ~ 1
      ),
      levels = 1:2,
      labels = c(">=60", "<60")
    )
  )

# Convert back to mids object
imp <- as.mids(long)

impsurv <- miceadds::subset_datlist(imp, expr_subset = edata$survpop == T)
