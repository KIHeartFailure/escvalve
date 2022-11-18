

# Primary criteria --------------------------------------------------------

flow <- c(paste0("Number of patients in ESC "), nrow(esc))

edata <- esc %>%
  filter(num_dmPtype == "Outpatient")
flow <- rbind(flow, c("Outpatients", nrow(edata)))

edata <- edata %>%
  filter(!is.na(num_opEcho) & num_opEcho == "Performed")
flow <- rbind(flow, c("Echo-Doppler performed", nrow(edata)))

edata <- edata %>%
  filter(!is.na(num_opMitReg) & !is.na(num_opAorSte) & !is.na(num_opAorReg) & !is.na(num_opTriCus))
flow <- rbind(flow, c("Non-missing values for 4 valve variables", nrow(edata)))

edata <- edata %>%
  filter(num_opAorSte == "No" & num_opAorReg == "No")
flow <- rbind(flow, c("Exclude patients with aortic stenosis and regurgitation", nrow(edata)))

edata <- edata %>%
  mutate(
    enddtm = coalesce(num_f1DeathDt, num_f1contDt),
    startdtm = num_dmVisitdt,
    outtime_death = as.numeric(enddtm - startdtm),
    survpop = num_f1lost == "No" & outtime_death >= 0 & !is.na(outtime_death)
  )
flow <- rbind(flow, c(
  ". Not lost to follow-up (long-term outcome population)",
  nrow(edata %>% filter(survpop))
))

colnames(flow) <- c("Criteria", "N")
