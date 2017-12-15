
d = read.table("./data/drought/pdsisc.monthly.maps.1900-2099.r2.5x2.5.EnsAvg14Models.TP2.ipe=2.nc.output.batch", header=FALSE)
#d2 = d %>% filter(d$V3 != "No_data") %>% select(V3:V11)
#plot(t(d2[5,]), type="l")
droughtPeriods = c("1950-99","2016-25","2026-35","2036-45","2046-55","2056-65","2066-75","2076-85","2086-95")
