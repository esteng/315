require(ggplot2)
df1 = read.csv('/Users/Elias/315/A2/results/my_win_freq.csv', header = TRUE)
df2 = read.csv('/Users/Elias/315/A2/results/orig_win_freq.csv', header = TRUE)
xlabs = c(10,20,30,40,50,60,70,80,90,100)



ggplot()+
  geom_line( aes(x = xlabs, y = apply(df1, 2, sum), colour = "direc. error"))+
  geom_line( aes(x = xlabs, y = apply(df2, 2, sum), colour = "abs. error"))+
  labs(y = '# wins', x='learnability')+
  scale_x_continuous(breaks= xlabs)

  
