require(ggplot2)
my_mean_strategies = read.csv("/Users/Elias/315/A3/results/my_mean_strategies.csv", header = TRUE)
my_mean_strategies$ID = seq.int(nrow(my_mean_strategies))
ggplot(my_mean_strategies)+
  geom_line(aes(x = my_mean_strategies$ID, y = my_mean_strategies$X1, colour = "selfish"))+
  geom_line(aes(x = my_mean_strategies$ID, y = X2, colour = "traitor"))+
  geom_line(aes(x=ID, y = X3, colour = "ethnocentric"))+
  geom_line(aes(x =ID, y = X4, colour = "humanitarian"))+
  labs(x = "cycle", y = "frequency")+
  ylim(0,1200)
  
  

orig_mean_strategies = read.csv("/Users/Elias/315/A3/results/orig_mean_strategies.csv", header = TRUE)
orig_mean_strategies$ID = seq.int(nrow(orig_mean_strategies))
ggplot(orig_mean_strategies)+
  geom_line(aes(x = ID, y = X1, colour = "selfish"))+
  geom_line(aes(x = ID, y = X2, colour = "traitor"))+
  geom_line(aes(x=ID, y = X3, colour = "ethnocentric"))+
  geom_line(aes(x =ID, y = X4, colour = "humanitarian"))+
  labs(x = "cycle", y = "frequency")+
  ylim(0,1200)

var.test(orig_mean_strategies$X3,my_mean_strategies$X3)
t.test(orig_mean_strategies$X3,my_mean_strategies$X3,var.equal=FALSE, paired = FALSE)

var.test(orig_mean_strategies$X2,my_mean_strategies$X2)
t.test(orig_mean_strategies$X2,my_mean_strategies$X2,var.equal=FALSE, paired = FALSE)

var.test(orig_mean_strategies$X1,my_mean_strategies$X1)
t.test(orig_mean_strategies$X1,my_mean_strategies$X1,var.equal=FALSE, paired = FALSE)

var.test(orig_mean_strategies$X4,my_mean_strategies$X4)
t.test(orig_mean_strategies$X4,my_mean_strategies$X4,var.equal=FALSE, paired = FALSE)
