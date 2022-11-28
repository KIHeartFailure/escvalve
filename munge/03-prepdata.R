mednt <- median(edata$num_opNt, na.rm = T)

edata <- edata %>%
  mutate(
    valve = factor(case_when(
      num_opMitReg == "No" & num_opTriCus == "No" ~ 0,
      num_opMitReg == "Yes" & num_opTriCus == "No" ~ 1,
      num_opMitReg == "No" & num_opTriCus == "Yes" ~ 2,
      num_opMitReg == "Yes" & num_opTriCus == "Yes" ~ 3
    ), levels = 0:3, labels = c(
      "No VHD",
      "MR",
      "TR",
      "MR+TR"
    )),
    d_Efcomb = coalesce(num_opEf, num_dmEflp),
    d_Efcomb_cat = factor(
      case_when(
        is.na(d_Efcomb) ~ NA_real_,
        d_Efcomb <= 40 ~ 1,
        d_Efcomb <= 49 ~ 2,
        d_Efcomb >= 50 ~ 3
      ),
      levels = 1:3,
      labels = c("<=40", "41-49", ">=50")
    ),

    # Outliers
    num_opNt = ifelse(num_opNt > 745000, NA, num_opNt),
    num_opLvdd = ifelse(num_opLvdd > 900, NA, num_opLvdd),
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
    d_dmHF_history = case_when(
      is.na(num_dmHF) ~ NA_character_,
      num_dmHF %in% c("Yes with previous hospitalisation", "Yes without previous hospitalisation") ~ "Yes",
      TRUE ~ "No"
    ),
    d_dmHF_cat = case_when(
      is.na(num_dmHF) ~ NA_character_,
      num_dmHF == "Yes with previous hospitalisation" ~ "Previous HF hosp",
      TRUE ~ "No previous HF hosp"
    ),
    d_HFdiagnosis = case_when(
      num_dmHF == "No" ~ "<12mo",
      num_dmMonth %in% c("< 6 months", "6 - 12 months") ~ "<12mo",
      num_dmMonth %in% c("> 12 months") ~ ">12mo"
    ),
    d_dmThy = case_when(
      num_dmThy == "No" ~ "No",
      num_dmThy %in% c("Hypothyroidism", "Hyperthyroidism") ~ "Yes"
    ),
    num_dmEtio_cat = factor(
      case_when(
        is.na(num_dmEtio) ~ NA_real_,
        num_dmEtio %in% c(
          "Ischemic heart disease documented by coronary angiography",
          "Ischemic heart disease not documented by coronary angiography"
        ) ~ 1,
        num_dmEtio %in% c("Hypertension") ~ 2,
        num_dmEtio %in% c("Dilated cardiomyopathy") ~ 3,
        num_dmEtio %in% c("Valve disease") ~ 4,
        TRUE ~ 5
      ),
      levels = 1:5,
      labels = c(
        "IHD", "Hypertension",
        "Dilated cardiomyopathy", "Valve disease", "Other"
      )
    ),
    num_dmEtio_cat2 = factor(
      case_when(
        is.na(num_dmEtio) ~ NA_real_,
        num_dmEtio %in% c("Valve disease") ~ 1,
        TRUE ~ 0
      ),
      levels = 0:1,
      labels = c(
        "Other", "Valve disease"
      )
    ),
    num_dmEtio_c1 = relevel(num_dmEtio_c1, ref = "Non-ischemic heart disease"),
    num_opNyha_cat = factor(case_when(
      is.na(num_opNyha) ~ NA_real_,
      num_opNyha %in% c("NYHA I", "NYHA II") ~ 1,
      num_opNyha %in% c("NYHA III", "NYHA IV") ~ 2,
    ), levels = 1:2, labels = c("I-II", "III-IV")),
    d_cad = factor(case_when(
      is.na(num_dmCabg) | is.na(num_dmPci) | is.na(num_dmMi) ~ NA_real_,
      num_dmCabg == "Yes" | num_dmPci == "Yes" | num_dmMi == "Yes" ~ 1,
      TRUE ~ 0
    ), levels = 0:1, labels = c("No", "Yes")),
    # eGFR according to CKD-EPI 2021 https://www.nejm.org/doi/full/10.1056/NEJMoa2102953
    tmp_k = if_else(num_dmgender == "Female", 0.7, 0.9),
    tmp_a = if_else(num_dmgender == "Female", -0.241, -0.302),
    tmp_add = if_else(num_dmgender == "Female", 1.012, 1),
    d_opCKDEPI = 142 * pmin(num_opCre / tmp_k, 1)^tmp_a * pmax(num_opCre / tmp_k, 1)^-1.200 * 0.9938^num_age * tmp_add,
    d_opCKDEPI = if_else(d_opCKDEPI == Inf, NA_real_, d_opCKDEPI),
    d_opCKDEPI_cat = factor(
      case_when(
        is.na(d_opCKDEPI) ~ NA_real_,
        d_opCKDEPI < 60 ~ 2,
        d_opCKDEPI >= 60 ~ 1
      ),
      levels = 1:2,
      labels = c(">=60", "<60")
    ),
    d_rasiarni_prior = case_when(
      is.na(num_mdACEp) | is.na(num_mdATp) ~ NA_character_,
      num_mdACEp == "Yes" | num_mdATp == "Yes" | num_mdARNIp == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    d_rasiarni_visit = case_when(
      is.na(num_mdACEh) | is.na(num_mdATh) ~ NA_character_,
      num_mdACEh == "Yes" | num_mdATh == "Yes" | num_mdARNIh == "Yes" ~ "Yes",
      TRUE ~ "No"
    ),
    # Outcomes
    enddtm = coalesce(num_f1DeathDt, num_f1contDt),
    startdtm = num_dmVisitdt,
    outtime_death = as.numeric(enddtm - startdtm),
    outtime_death = ifelse(!survpop, NA, outtime_death),
    out_death = case_when(
      !survpop ~ NA_real_,
      num_f1vital == "Alive" ~ 0,
      num_f1vital == "Dead" ~ 1
    ),
    out_deathcv = case_when(
      is.na(out_death) ~ NA_real_,
      num_f1DeathCs %in% c("Cardiac", "Vascular") ~ 1,
      TRUE ~ 0
    ), # pats with missing info are NOT included in CV

    out_deathnoncv = case_when(
      is.na(out_death) ~ NA_real_,
      num_f1DeathCs == c("Non cardiovascular") ~ 1,
      TRUE ~ 0
    ), # pats with missing info are NOT included in nonCV

    # All-cause hosp
    out_hosp = case_when(
      num_f1lost != "No" | !survpop ~ NA_real_,
      num_f1hosp1 == "Yes" |
        num_f1hosp2 == "Yes" |
        num_f1hosp3 == "Yes" |
        num_f1hosp4 == "Yes" |
        num_f1hosp5 == "Yes" ~ 1,
      TRUE ~ 0
    ),
    out_hospdtm = coalesce(
      num_f1hosp1dt, num_f1hosp2dt, num_f1hosp3dt,
      num_f1hosp4dt, num_f1hosp5dt
    ),
    outtime_hosp = as.numeric(out_hospdtm - startdtm),
    outtime_hospmissing = case_when(
      out_hosp == 1 & is.na(outtime_hosp) ~ 1,
      out_hosp == 1 ~ 0
    ),
    outtime_hosp = ifelse(out_hosp == 1 & is.na(outtime_hosp), outtime_death / 2, outtime_hosp),
    outtime_hosp = pmin(outtime_hosp, outtime_death, na.rm = TRUE),
    outtime_hosp = ifelse(outtime_hosp < 0, outtime_death, outtime_hosp),
    outtime_hosp = ifelse(!survpop, NA, outtime_hosp),

    # HF hosp
    out_hosphf = case_when(
      num_f1lost != "No" | !survpop ~ NA_real_,
      num_f1hosp1cs == "HF" |
        num_f1hosp2cs == "HF" |
        num_f1hosp3cs == "HF" |
        num_f1hosp4cs == "HF" |
        num_f1hosp5cs == "HF" ~ 1,
      TRUE ~ 0
    ),
    out_hosphfdtm = case_when(
      num_f1hosp1cs == "HF" ~ num_f1hosp1dt,
      num_f1hosp2cs == "HF" ~ num_f1hosp2dt,
      num_f1hosp3cs == "HF" ~ num_f1hosp3dt,
      num_f1hosp4cs == "HF" ~ num_f1hosp4dt,
      num_f1hosp5cs == "HF" ~ num_f1hosp5dt
    ),
    outtime_hosphf = as.numeric(out_hosphfdtm - startdtm),
    outtime_hosphf = ifelse(out_hosphf == 1 & is.na(outtime_hosphf), outtime_death / 2, outtime_hosphf),
    outtime_hosphf = pmin(outtime_hosphf, outtime_death, na.rm = TRUE),
    outtime_hosphf = ifelse(outtime_hosphf < 0, outtime_death, outtime_hosphf),
    outtime_hosphf = ifelse(!survpop, NA, outtime_hosphf),

    # all-cause death or hf hosp
    out_deathhosphf = ifelse(out_hosphf == 1, 1, out_death),
    # cv death or hf hosp
    out_deathcvhosphf = ifelse(out_hosphf == 1, 1, out_deathcv)
  ) %>%
  mutate(across(where(is.character), as.factor))
