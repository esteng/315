ac_path = "/Users/Elias/Documents/models/cycle/ac/"
am_path = "/Users/Elias/Documents/models/cycle/am/"
ciu_path = "/Users/Elias/Documents/models/cycle/ciu/"

am1_data = read.csv(paste(am_path,"s1counts",sep=""), header = FALSE, sep = " ")
ac1_data = read.csv(paste(ac_path,"s1counts",sep=""), header = FALSE, sep = " ")
ciu1_data = read.csv(paste(ciu_path,"s1counts",sep=""), header = FALSE, sep = " ")
am2_data = read.csv(paste(am_path,"s2counts",sep=""), header = FALSE, sep = " ")
ac2_data = read.csv(paste(ac_path,"s2counts",sep=""), header = FALSE, sep = " ")
ciu2_data = read.csv(paste(ciu_path,"s2counts",sep=""), header = FALSE, sep = " ")
am1_data$V21 = NULL
am1_data$mean = rowMeans(am1_data)
am1_data$ID <-seq.int(nrow(am1_data))
am2_data$V21 = NULL
am2_data$mean = rowMeans(am2_data)
am2_data$ID<-seq.int(nrow(am2_data))
ac1_data$V21 = NULL
ac1_data$mean = rowMeans(ac1_data)
ac1_data$ID<-seq.int(nrow(ac1_data))
ac2_data$V21 = NULL
ac2_data$mean = rowMeans(ac2_data)
ac2_data$ID <-seq.int(nrow(ac2_data))
ciu1_data$V21 = NULL
ciu1_data$mean = rowMeans(ciu1_data)
ciu1_data$ID <-seq.int(nrow(ciu1_data))
ciu2_data$V21 = NULL
ciu2_data$mean = rowMeans(ciu2_data)
ciu2_data$ID <-seq.int(nrow(ciu2_data))




ggplot()+
  geom_line(data=am1_data, aes( x = am1_data$ID, y = am1_data$mean, colour = "S1, AM"))+
  geom_line(data=am2_data, aes( x = am2_data$ID, y = am2_data$mean, colour = "S2, AM"))+
  geom_line(data=ac1_data, aes( x = ac1_data$ID, y = ac1_data$mean, colour = "S1, AC"))+
  geom_line(data=ac2_data, aes( x = ac2_data$ID, y = ac2_data$mean, colour = "S2, AC"))+
  geom_line(data=ciu1_data, aes( x = ciu1_data$ID, y = ciu1_data$mean, colour = "S1, CIU"))+
  geom_line(data=ciu2_data, aes( x = ciu2_data$ID, y = ciu2_data$mean, colour = "S2, CIU"))+
  labs(title = "S1, S2 by algorithm", y = "# agents", x = "cycle")

