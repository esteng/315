require(ggplot2)
require(reshape2)

myac_path = "/Users/Elias/Documents/models/saturation/ac/"
ac_path = "/Users/Elias/Documents/models_ac_original/"
myam_path = "/Users/Elias/Documents/models/saturation/am/"
am_path = "/Users/Elias/Documents/models_am_original/"
myciu_path = "/Users/Elias/Documents/models/saturation/ciu/"
ciu_path = "/Users/Elias/Documents/models_ciu_original/"


all_ac = read.csv(paste(myac_path,"cycles", sep = "") , header = FALSE, sep = " ")
all_am = read.csv(paste(myam_path,"cycles", sep = "") , header = FALSE, sep = " ")
all_ciu = read.csv(paste(myciu_path,"cycles", sep = "") , header = FALSE, sep = " ")

aogam = read.csv(paste(am_path,"cycles", sep = "") , header = FALSE, sep = " ")
aogac = read.csv(paste(ac_path,"cycles", sep = "") , header = FALSE, sep = " ")
aogciu = read.csv(paste(ciu_path,"cycles", sep = "") , header = FALSE, sep = " ")
all_ciu$V2 = NULL
all_ac$V2 = NULL
all_am$V2 = NULL

aogam$V2 = NULL
aogac$V2 = NULL
aogciu$V2 = NULL

totals = data.frame(new_am = all_am, new_ac = all_ac, new_ciu =all_ciu)
totals2 = data.frame( old_am = aogam,old_ac = aogac,old_ciu = aogciu)

names = c("AM_1",  "AC_1", "CIU_1")
names2  = c("AM_0","AC_0", "CIU_0")
colnames(totals) = names
colnames(totals2) = names2
name_levels = names[order(names)]

t = as.numeric(totals[1,])

ggplot() + 
  geom_bar(stat = "identity", aes(x = colnames(totals)[2], y = mean(totals$`AC_1`), fill = "new")) +
  geom_errorbar(stat = "identity", aes(x = colnames(totals)[2], ymax = mean(totals$`AC_1`) + sd(totals$`AC_1`), 
                                       ymin = mean(totals$`AC_1`) - sd(totals$`AC_1`)))+
  geom_bar(stat = "identity", aes(x = colnames(totals2)[2], y = mean(totals2$`AC_0`), fill = "original"))+
  geom_errorbar(stat = "identity", aes(x = colnames(totals2)[2], ymax = mean(totals2$`AC_0`) + sd(totals2$`AC_0`), 
                                       ymin = mean(totals2$`AC_0`) - sd(totals2$`AC_0`)))+
  geom_bar(stat = "identity", aes(x = colnames(totals)[1], y = mean(totals$`AM_1`), fill = "new"))+
  geom_errorbar(stat = "identity", aes(x = colnames(totals)[1], ymax = mean(totals$`AM_1`) + sd(totals$`AM_1`), 
                                       ymin = mean(totals$`AM_1`) - sd(totals$`AM_1`)))+
  geom_bar(stat = "identity", aes(x = colnames(totals2)[1], y = mean(totals2$`AM_0`), fill = "original"))+
  geom_errorbar(stat = "identity", aes(x = colnames(totals2)[1], ymax = mean(totals2$`AM_0`) + sd(totals2$`AM_0`), 
                                       ymin = mean(totals2$`AM_0`) - sd(totals2$`AM_0`)))+
  geom_bar(stat = "identity", aes(x = colnames(totals)[3], y = mean(totals$`CIU_1`), fill = "new"))+
  geom_errorbar(stat = "identity", aes(x = colnames(totals)[3], ymax = mean(totals$`CIU_1`) + sd(totals$`CIU_1`), 
                                       ymin = mean(totals$`CIU_1`) - sd(totals$`CIU_1`)))+
  geom_bar(stat = "identity", aes(x = colnames(totals2)[3], y = mean(totals2$`CIU_0`), fill = "original"))+
  geom_errorbar(stat = "identity", aes(x = colnames(totals2)[3], ymax = mean(totals2$`CIU_0`) + sd(totals2$`CIU_0`), 
                                       ymin = mean(totals2$`CIU_0`) - sd(totals2$`CIU_0`)))+
  labs(title = "cycles to consensus",x ="algorithm", y = "mean cycles")

