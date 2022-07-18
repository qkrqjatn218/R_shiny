library(data.table);library(magrittr);library(jstable);library(lubridate)
#setwd("~/ShinyApps/koyus/tb")

#readxl::read_excel("KTBC_EpidemiologicalSpectrum����_���.xlsx", sheet = 4) %>% saveRDS("KTBC_EpidemiologicalSpectrum.rds")
#aws.s3::s3saveRDS(readRDS("KTBC_EpidemiologicalSpectrum.rds"), "data/KTBC_EpidemiologicalSpectrum.rds", bucket = "zarathu")

a <- data.table(aws.s3::s3readRDS("data/KTBC_EpidemiologicalSpectrum.rds", bucket = "zarathu"))
a[, prev_TB := as.integer(!is.na(���Ű������ܹ����⵵))]

name.old <- names(a)

a <- data.table(a, check.names = T)
names(name.old) <- names(a)


## variable list
varlist <- list(
  Base = c("Epidemiologic.Spectrum.Group", "����", "����", "BMI", "Rurality.����.����.", "��ȥ����", "���Ű���", "����", "����", "���ַ�"),
  Comorbity = c(setdiff(grep("������ȯ_", names(a), value = T), c("������ȯ_����̽��Ѱ����⼭��", "������ȯ_�Ǽ����絿�ݽ���⼭��", "������ȯ_�ڰ��鿪��ȯ���ݽ���ȯ����")),
                "prev_TB", grep("EPTB", names(a), value = T)),
  Delay = c("Episcore_calculation", "Time_Presentation_delay_�����", "Time_Healthcare_delay_�����", "Time_Overall_delay_�����", "Time_treatment_period_days_�����"),
  Event = c("Mortality_All_cause",  "Mortality_TB_realted_���ٰ��û��", "Mortality_Non_TB_realted_���ٺ���û��"),
  Day = "ġ��Ⱓ"
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
