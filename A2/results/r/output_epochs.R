require(ggplot2)

df = read.csv('/Users/Elias/315/A2/results/all_epochs.csv', header= TRUE)
learnability = c(0, 10, 20, 30, 40, 50, 60, 70, 75, 80,85, 90,95, 100)
yscale = c(25, 50, 75, 100, 125)
ggplot()+
  geom_line(aes(x=learnability, y=colMeans(df)))+
  geom_point(aes(x=learnability, y=colMeans(df)))+
  geom_errorbar(aes(x=learnability, ymin=colMeans(df)-apply(df, 2, sd), ymax=colMeans(df)+apply(df, 2, sd)))+
  scale_x_continuous(breaks= learnability)+
  scale_y_continuous(breaks = yscale, labels = yscale)+
  labs(y = "output epochs")

