require(ggplot2)
require(Hmisc)

path = "/Users/Elias/Documents/models_ac/"


drops = c("means","sd","ID")

#S1 data
df = read.csv(paste(path,"s1counts", sep = ""), header=TRUE, sep  = " ")
df$X = NULL
df$ID= NULL
total = df

total$ID<-seq.int(nrow(total))
total$sd = apply(df,1,sd)
total$means = rowMeans(df)
total$stderr = total$sd/sqrt(20)
total$upper = total$means + 1.96*total$stderr
total$lower = total$means - 1.96*total$stderr

limits = aes(x=total$ID, ymax = total$means+total$sd, ymin = total$means-total$sd)
#S1 plot
ggplot(total, aes(x = total$ID, y = total$means)) + 
  geom_line(aes(x = total$ID, y = total$means, colour = "mean"))+ 
  geom_line(aes(x = total$ID , y = total$lower , colour = 'lower CI'))+
  geom_line(aes(x = total$ID, y = total$upper , colour = "upper CI")) +
  labs(title = "Mean counts of S1 with AC algorithm", x = "trial #", y = "# s1 units")


#S2 data
df_s2 = read.csv(paste(path,"s2counts", sep = ""), header=TRUE, sep  = " ")
df_s2$X = NULL
df_s2$ID= NULL
total_s2 = df_s2

total_s2$ID<-seq.int(nrow(total_s2))
total_s2$sd = apply(df_s2,1,sd)
total_s2$means = rowMeans(df_s2)
total_s2$stderr = total_s2$sd/sqrt(20)
total_s2$upper = total_s2$means + 1.96*total_s2$stderr
total_s2$lower = total_s2$means - 1.96*total_s2$stderr

limits = aes(x=total_s2$ID, ymax = total_s2$means+total_s2$sd, ymin = total_s2$means-total_s2$sd)
#S1 plot
ggplot(total_s2, aes(x = total_s2$ID, y = total_s2$means)) + 
  geom_line(aes(x = total_s2$ID, y = total_s2$means, colour = "mean"))+ 
  geom_line(aes(x = total_s2$ID , y = total_s2$lower , colour = 'lower CI'))+
  geom_line(aes(x = total_s2$ID, y = total_s2$upper , colour = "upper CI")) +
  labs(title = "Mean counts of S2 with AC algorithm", x = "trial #", y = "# s2 units")


#X data
df2 = read.csv(paste(path,"xcounts", sep = ""), header= TRUE, sep = " ")
df2$X = NULL
total_x = df2

total_x$ID<-seq.int(nrow(total_x))
total_x$sd = apply(df2,1,sd)
total_x$means = rowMeans(df2)
total_x$stderr = total_x$sd/sqrt(20)
total_x$upper = total_x$means + 1.96*total_x$stderr
total_x$lower = total_x$means - 1.96*total_x$stderr

#X COUNT PLOT
ggplot(total_x, aes(x = total_x$ID, y = total_x$means)) + 
  geom_line(aes(x = total_x$ID, y = total_x$means, colour = "mean"))+ 
  geom_line(aes(x = total_x$ID , y = total_x$lower , colour = 'lower CI'))+
  geom_line(aes(x = total_x$ID, y = total_x$upper , colour = "upper CI")) +
  labs(title = "Mean counts of X with AC algorithm", x = "trial #", y = "# X units")

#UDATA
df_u = read.csv(paste(path,"ucounts", sep = ""), header=TRUE, sep  = " ")
df_u$X = NULL
df_u$ID= NULL
total_u = df_u

total_u$ID<-seq.int(nrow(total_u))
total_u$sd = apply(df_u,1,sd)
total_u$means = rowMeans(df_u)
total_u$stderr = total_u$sd/sqrt(20)
total_u$upper = total_u$means + 1.96*total_u$stderr
total_u$lower = total_u$means - 1.96*total_u$stderr

limits = aes(x=total_u$ID, ymax = total_u$means+total_u$sd, ymin = total_u$means-total_u$sd)
#S1 plot
ggplot(total_u, aes(x = total_u$ID, y = total_u$means)) + 
  geom_line(aes(x = total_u$ID, y = total_u$means, colour = "mean"))+ 
  geom_line(aes(x = total_u$ID , y = total_u$lower , colour = 'lower CI'))+
  geom_line(aes(x = total_u$ID, y = total_u$upper , colour = "upper CI")) +
  labs(title = "Mean counts of U with AC algorithm", x = "trial #", y = "# u units")


