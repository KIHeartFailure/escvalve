edata <- edata %>%
  mutate(
    valve = factor(case_when(
      num_opAorSte == "No" & num_opAorReg == "No" & num_opMitReg == "No" & num_opTriCus == "No" ~ 0,
      num_opAorSte == "Yes" & num_opAorReg == "No" & num_opMitReg == "No" & num_opTriCus == "No" ~ 1,
      num_opAorSte == "No" & num_opAorReg == "Yes" & num_opMitReg == "No" & num_opTriCus == "No" ~ 2,
      num_opAorSte == "No" & num_opAorReg == "No" & num_opMitReg == "Yes" & num_opTriCus == "No" ~ 3,
      num_opAorSte == "No" & num_opAorReg == "No" & num_opMitReg == "No" & num_opTriCus == "Yes" ~ 4,
      TRUE ~ 5
    ), levels = 0:5, labels = c(
      "No VHD",
      "Aortic stenosis",
      "Aortic regurgitation",
      "Mitral regurgitation",
      "Tricuspid regurgitation",
      "Combined VHD"
    )),
    valve2 = factor(case_when(
      valve == "Combined VHD" ~ 3,
      valve == "No VHD" ~ 1,
      TRUE ~ 2
    ), levels = 1:3, labels = c("No VHD", "Single VHD", "Combined VHD")),
    num_dmEfl_cat = factor(
      case_when(
        num_dmEflp < 40 ~ 1,
        num_dmEflp <= 49 ~ 2,
        num_dmEflp >= 50 ~ 3
      ),
      levels = 1:3,
      labels = c("<40", "40-49", ">=50")
    ),
    num_age_cat = factor(case_when(
      num_age < 65 ~ 1,
      num_age >= 65 ~ 2
    ), levels = 1:2, labels = c("<65", ">=65")),
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
    d_anemia = case_when(
      is.na(num_opHb) | is.na(num_dmgender) ~ NA_character_,
      num_opHb < 13 & num_dmgender == "Male" ~ "Yes",
      num_opHb < 12 & num_dmgender == "Female" ~ "Yes",
      TRUE ~ "No"
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
    # eGFR according to CKD-EPI 2021 https://www.nejm.org/doi/full/10.1056/NEJMoa2102953
    tmp_k = if_else(num_dmgender == "Female", 0.7, 0.9),
    tmp_a = if_else(num_dmgender == "Female", -0.241, -0.302),
    tmp_add = if_else(num_dmgender == "Female", 1.012, 1),
    d_opCKDEPI = 142 * pmin(num_opCre / tmp_k, 1)^tmp_a * pmax(num_opCre / tmp_k, 1)^-1.200 * 0.9938^num_age * tmp_add,
    d_opCKDEPI = if_else(d_opCKDEPI == Inf, NA_real_, d_opCKDEPI),
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
    out_death = case_when(
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
      num_f1lost != "No" ~ NA_real_,
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

    # HF hosp
    out_hosphf = case_when(
      num_f1lost != "No" ~ NA_real_,
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

    # all-cause death or hf hosp
    out_deathhosphf = ifelse(out_hosphf == 1, 1, out_death),
    # cv death or hf hosp
    out_deathcvhosphf = ifelse(out_hosphf == 1, 1, out_deathcv)
  )
