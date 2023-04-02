library(here)

source("scripts/data_assembly/master_dataframe.R")

# NEE
t.test(NEE_umol_s_m2 ~ treatment, data = NEE)
# Welch Two Sample t-test, p = 0.1112

t.test(NEE_umol_s_m2 ~ treatment, data = filter(NEE, site == "WILL")) # p-value = 0.1664
t.test(NEE_umol_s_m2 ~ treatment, data = filter(NEE, site == "DRYAS")) # p-value = 0.427
t.test(NEE_umol_s_m2 ~ treatment, data = filter(NEE, site == "MEAD")) # p-value = 0.3755

# GEP
t.test(GEP_umol_s_m2 ~ treatment, data = GEP)
# Welch Two Sample t-test, p = 0.07929 (*)

t.test(GEP_umol_s_m2 ~ treatment, data = filter(GEP, site == "WILL")) # p-value = 0.1482
t.test(GEP_umol_s_m2 ~ treatment, data = filter(GEP, site == "DRYAS")) # p-value = 0.2035
t.test(GEP_umol_s_m2 ~ treatment, data = filter(GEP, site == "MEAD")) # p-value = 0.07604 (*)

# ER
t.test(ER_umol_s_m2 ~ treatment, data = ER)
# Welch Two Sample t-test, p-value = 0.09691

t.test(ER_umol_s_m2 ~ treatment, data = filter(ER, site == "WILL")) # p-value = 0.2175
t.test(ER_umol_s_m2 ~ treatment, data = filter(ER, site == "DRYAS")) # p-value = 0.08392 (*)
t.test(ER_umol_s_m2 ~ treatment, data = filter(ER, site == "MEAD")) # p-value = 0.0001343**
