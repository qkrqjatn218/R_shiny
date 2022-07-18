library(data.table);library(magrittr);library(jstable);library(lubridate)
#setwd("~/ShinyApps/koyus/tb")

#readxl::read_excel("KTBC_EpidemiologicalSpectrum연구_결과.xlsx", sheet = 4) %>% saveRDS("KTBC_EpidemiologicalSpectrum.rds")
#aws.s3::s3saveRDS(readRDS("KTBC_EpidemiologicalSpectrum.rds"), "data/KTBC_EpidemiologicalSpectrum.rds", bucket = "zarathu")

a <- data.table(aws.s3::s3readRDS("data/KTBC_EpidemiologicalSpectrum.rds", bucket = "zarathu"))
a[, prev_TB := as.integer(!is.na(과거결핵진단받은년도))]

name.old <- names(a)

a <- data.table(a, check.names = T)
names(name.old) <- names(a)


## variable list
varlist <- list(
  Base = c("Epidemiologic.Spectrum.Group", "연령", "성별", "BMI", "Rurality.읍면.거주.", "결혼여부", "동거가족", "직업", "흡연", "음주력"),
  Comorbity = c(setdiff(grep("동반질환_", names(a), value = T), c("동반질환_장기이식한경우장기서술", "동반질환_악성종양동반시장기서술", "동반질환_자가면역질환동반시질환서술")),
                "prev_TB", grep("EPTB", names(a), value = T)),
  Delay = c("Episcore_calculation", "Time_Presentation_delay_양수만", "Time_Healthcare_delay_양수만", "Time_Overall_delay_양수만", "Time_treatment_period_days_양수만"),
  Event = c("Mortality_All_cause",  "Mortality_TB_realted_결핵관련사망", "Mortality_Non_TB_realted_결핵비관련사망"),
  Day = "치료기간"
)



out <- a[,  .SD, .SDcols = c(unlist(varlist))]


## Factor

#names(out)[sapply(out, function(x){length(table(x))}) <= 6]
factor_vars <- names(out)[sapply(out, function(x){length(table(x))}) <= 6]
out[, (factor_vars) := lapply(.SD, factor), .SDcols = factor_vars]
conti_vars <- setdiff(names(out), c(factor_vars))
out[, (conti_vars) := lapply(.SD, as.numeric), .SDcols = conti_vars]


## Label
out.label <- mk.lev(out)
#vars.01 <- names(out)[sapply(lapply(out, levels), function(x){identical(x, c("0", "1"))})]

#for (v in vars.01){
#  out.label[variable == v, val_label := c("No", "Yes")]
#}

for (v in names(out)){
  out.label[variable == v, var_label := name.old[v]]
}

