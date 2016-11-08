require(ggplot2)


df0 = read.csv('/Users/Elias/315/A2/results/averages/all_averages_0.csv', header= TRUE)
df10 = read.csv('/Users/Elias/315/A2/results/averages/all_averages_10.csv', header= TRUE)
df20 = read.csv('/Users/Elias/315/A2/results/averages/all_averages_20.csv', header= TRUE)
df30 = read.csv('/Users/Elias/315/A2/results/averages/all_averages_30.csv', header= TRUE)
df40 = read.csv('/Users/Elias/315/A2/results/averages/all_averages_40.csv', header= TRUE)
df50 = read.csv('/Users/Elias/315/A2/results/averages/all_averages_50.csv', header= TRUE)
df60 = read.csv('/Users/Elias/315/A2/results/averages/all_averages_60.csv', header= TRUE)
df70 = read.csv('/Users/Elias/315/A2/results/averages/all_averages_70.csv', header= TRUE)
df75 = read.csv('/Users/Elias/315/A2/results/averages/all_averages_75.csv', header= TRUE)
df80 = read.csv('/Users/Elias/315/A2/results/averages/all_averages_80.csv', header= TRUE)
df85 = read.csv('/Users/Elias/315/A2/results/averages/all_averages_85.csv', header= TRUE)
df90 = read.csv('/Users/Elias/315/A2/results/averages/all_averages_90.csv', header= TRUE)
df95 = read.csv('/Users/Elias/315/A2/results/averages/all_averages_95.csv', header= TRUE)
df100 = read.csv('/Users/Elias/315/A2/results/averages/all_averages_100.csv', header= TRUE)

ggplot()+
  geom_line(data=df0, aes(x=df0$ID, y=df0$percent, colour = "0%"))+
  geom_line(data=df10, aes(x=df10$ID, y=df10$percent, colour = "10%"))+
  geom_line(data=df20, aes(x=df20$ID, y=df20$percent, colour = "20%"))+
  geom_line(data=df30, aes(x=df30$ID, y=df30$percent, colour = "30%"))+
  geom_line(data=df40, aes(x=df40$ID, y=df40$percent, colour = "40%"))+
  geom_line(data=df50, aes(x=df50$ID, y=df50$percent, colour = "50%"))+
  geom_line(data=df60, aes(x=df60$ID, y=df60$percent, colour = "60%"))+
  geom_line(data=df70, aes(x=df70$ID, y=df70$percent, colour = "70%"))+
  geom_line(data=df75, aes(x=df75$ID, y=df75$percent, colour = "75%"))+
  geom_line(data=df80, aes(x=df80$ID, y=df80$percent, colour = "80%"))+
  geom_line(data=df85, aes(x=df85$ID, y=df85$percent, colour = "85%"))+
  geom_line(data=df90, aes(x=df90$ID, y=df90$percent, colour = "90%"))+
  geom_line(data=df95, aes(x=df95$ID, y=df95$percent, colour = "96%"))+
  geom_line(data=df100, aes(x=df100$ID, y=df100$percent, colour = "100%"))




