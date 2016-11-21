require(ggplot2)
winners = read.csv("/Users/Elias/315/A3/results/winners", header = FALSE)

ggplot(winners)+
  geom_bar(stat="identity", aes(x = 0, y = winners$V1, fill = "no winner"))+
  geom_bar(stat="identity", aes(x = 1, y = winners$V2, fill = "selfish"))+
  geom_bar(stat="identity", aes(x = 2, y = winners$V3, fill = "traitor"))+
  geom_bar(stat="identity", aes(x = 3, y = winners$V4, fill = "ethnocentric"))+
  geom_bar(stat="identity", aes(x = 4, y = winners$V5, fill = "humanitarian"))+
  labs(x= "", y = "frequency")
  
