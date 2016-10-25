require(ggplot2)
require(Hmisc)

myac_path = "/Users/Elias/Documents/models_ac/"
ac_path = "/Users/Elias/Documents/models_ac_original/"
myam_path = "/Users/Elias/Documents/models_am/"
am_path = "/Users/Elias/Documents/models_am_original/"
myciu_path = "/Users/Elias/Documents/models_ciu/"
ciu_path = "/Users/Elias/Documents/models_ciu_original/"
paths = c(myac_path,ac_path,myam_path,am_path, myciu_path,ciu_path)
names = c('myac','ac', 'myam', 'am', 'myciu', 'ciu')

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
  labs(title = "Mean counts of S1 with AM algorithm", x = "trial #", y = "# s1 units")


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
  labs(title = "Mean counts of S2 with AM algorithm", x = "trial #", y = "# s2 units")


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
  labs(title = "Mean counts of X with AM algorithm", x = "trial #", y = "# X units")

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
  labs(title = "Mean counts of U with AM algorithm", x = "trial #", y = "# u units")

#Y DATA
df_y = read.csv(paste(path,"ycounts", sep = ""), header=TRUE, sep  = " ")
df_y$X = NULL
df_y$ID= NULL
total_y = df_y

total_y$ID<-seq.int(nrow(total_y))
total_y$sd = apply(df_y,1,sd)
total_y$means = rowMeans(df_y)
total_y$stderr = total_y$sd/sqrt(20)
total_y$upper = total_y$means + 1.96*total_y$stderr
total_y$lower = total_y$means - 1.96*total_y$stderr


#Aggregate plot
ggplot(total_u) + 
  geom_line(aes(x = total_u$ID, y = total_u$means, colour = "mean"))+ 
  geom_line(aes(x = total_u$ID , y = total_u$lower , colour = 'lower CI u'))+
  geom_line(aes(x = total_u$ID, y = total_u$upper , colour = "upper CI u")) +
  geom_line(aes(x = total_x$ID, y = total_x$means, colour = "mean"))+ 
  geom_line(aes(x = total_x$ID , y = total_x$lower , colour = 'lower CI x'))+
  geom_line(aes(x = total_x$ID, y = total_x$upper , colour = "upper CI x")) +
  geom_line(aes(x = total_u$ID, y = total_y$means, colour = "mean"))+ 
  geom_line(aes(x = total_y$ID , y = total_y$lower , colour = 'lower CI y'))+
  geom_line(aes(x = total_y$ID, y = total_y$upper , colour = "upper CI y")) +
  
  labs(title = "Mean counts of U, X, Y with AM algorithm", x = "trial #", y = "# units")

#mean # of solvers, treatment and control
control_x_am = read.csv(paste(myam_path,"xcounts_control", sep = ""), header = FALSE, sep = " ")
control_x_ac = read.csv(paste(myac_path,"xcounts_control", sep = ""), header = FALSE, sep = " ")
control_x_ciu = read.csv(paste(myciu_path,"xcounts_control", sep = ""), header = FALSE, sep = " ")
control_x_ac$V21 = NULL
control_x_am$V21 = NULL
control_x_ciu$V21 = NULL
x_am = read.csv(paste(myam_path,"xcounts", sep = ""), header = FALSE, sep = " ")
x_ac = read.csv(paste(myac_path,"xcounts", sep = ""), header = FALSE, sep = " ")
x_ciu = read.csv(paste(myciu_path,"xcounts", sep = ""), header = FALSE, sep = " ")
x_ac$V21 = NULL
x_am$V21 = NULL
x_ciu$V21 = NULL
N = 1101
M = 771
solver_total = data.frame(x_am = double(N), x_ac = double(N), x_ciu = double(N))
control_solver_total = data.frame(control_x_am = double(M), control_x_ac = double(M), control_x_ciu = double(M))
solver_total$x_am = rowMeans(x_am)
solver_total$x_am_sd = apply(x_am,1,sd)
solver_total$stderr = solver_total$x_am_sd/sqrt(20)
solver_total$upper = solver_total$x_am + 1.96*solver_total$stderr
solver_total$lower = solver_total$x_am - 1.96*solver_total$stderr
solver_total$ID<-seq.int(nrow(solver_total))

control_solver_total$ID<-seq.int(nrow(control_solver_total))
control_solver_total$x_am = rowMeans(control_x_am)
control_solver_total$x_am_sd = apply(control_x_am,1,sd)
control_solver_total$stderr = control_solver_total$x_am_sd/sqrt(20)
control_solver_total$upper = control_solver_total$x_am + 1.96*control_solver_total$stderr
control_solver_total$lower = control_solver_total$x_am - 1.96*control_solver_total$stderr


zeros = rep(0,330)
cxam = c(zeros, control_solver_total$x_am)
cxam_up = c(zeros, control_solver_total$upper)
cxam_low = c(zeros, control_solver_total$lower)
solver_total$c_x_am = cxam
solver_total$c_xam_up = cxam_up
solver_total$c_xam_low = cxam_low



ggplot(solver_total) + 
  geom_line(aes(x = solver_total$ID, y = solver_total$x_am, colour = "Tmt mean")) + 
  geom_line(aes(x = solver_total$ID, y = solver_total$lower , colour = "Tmt CI")) +
  geom_line(aes(x = solver_total$ID, y = solver_total$upper , colour = "Tmt CI")) +
  geom_line(aes(x = solver_total$ID, y = solver_total$c_x_am, colour = "Ctrl mean"))+
  geom_line(aes(x = solver_total$ID, y = solver_total$c_xam_up , colour = "Ctrl CI")) +
  geom_line(aes(x = solver_total$ID, y = solver_total$c_xam_low , colour = "Ctrl CI")) +
  labs(title = "AM", x = "cycle", y = "# solvers")

solver_total$x_ac = rowMeans(x_ac)
solver_total$x_ac_sd = apply(x_ac,1,sd)
solver_total$ac_stderr = solver_total$x_ac_sd/sqrt(20)
solver_total$ac_upper = solver_total$x_ac + 1.96*solver_total$ac_stderr
solver_total$ac_lower = solver_total$x_ac - 1.96*solver_total$ac_stderr


control_solver_total$x_ac = rowMeans(control_x_ac)
control_solver_total$x_ac_sd = apply(control_x_ac,1,sd)
control_solver_total$ac_stderr = control_solver_total$x_ac_sd/sqrt(20)
control_solver_total$ac_upper = control_solver_total$x_ac + 1.96*control_solver_total$ac_stderr
control_solver_total$ac_lower = control_solver_total$x_ac - 1.96*control_solver_total$ac_stderr

cxac = c(zeros, control_solver_total$x_ac)
cxac_up = c(zeros, control_solver_total$ac_upper)
cxac_low = c(zeros, control_solver_total$ac_lower)
solver_total$c_x_ac = cxac
solver_total$c_xac_up = cxac_up
solver_total$c_xac_low = cxac_low

ggplot(solver_total) + 
  geom_line(aes(x = solver_total$ID, y = solver_total$x_ac, colour = "Tmt mean")) + 
  geom_line(aes(x = solver_total$ID, y = solver_total$ac_lower , colour = "Tmt CI")) +
  geom_line(aes(x = solver_total$ID, y = solver_total$ac_upper , colour = "Tmt CI")) +
  geom_line(aes(x = solver_total$ID, y = solver_total$c_x_ac, colour = "Ctrl mean"))+
  geom_line(aes(x = solver_total$ID, y = solver_total$c_xac_up , colour = "Ctrl CI")) +
  geom_line(aes(x = solver_total$ID, y = solver_total$c_xac_low , colour = "Ctrl CI")) +
  labs(title = "AC", x = "cycle", y = "# solvers")


solver_total$x_ciu = rowMeans(x_ciu)
solver_total$x_ciu_sd = apply(x_ciu,1,sd)
solver_total$ciu_stderr = solver_total$x_ciu_sd/sqrt(20)
solver_total$ciu_upper = solver_total$x_ciu + 1.96*solver_total$ciu_stderr
solver_total$ciu_lower = solver_total$x_ciu - 1.96*solver_total$ciu_stderr


control_solver_total$x_ciu = rowMeans(control_x_ciu)
control_solver_total$x_ciu_sd = apply(control_x_ciu,1,sd)
control_solver_total$ciu_stderr = control_solver_total$x_ciu_sd/sqrt(20)
control_solver_total$ciu_upper = control_solver_total$x_ciu + 1.96*control_solver_total$ciu_stderr
control_solver_total$ciu_lower = control_solver_total$x_ciu - 1.96*control_solver_total$ciu_stderr

cxciu = c(zeros, control_solver_total$x_ciu)
cxciu_up = c(zeros, control_solver_total$ciu_upper)
cxciu_low = c(zeros, control_solver_total$ciu_lower)
solver_total$c_x_ciu = cxciu
solver_total$c_xciu_up = cxciu_up
solver_total$c_xciu_low = cxciu_low

ggplot(solver_total) + 
  geom_line(aes(x = solver_total$ID, y = solver_total$x_ciu, colour = "Tmt mean")) + 
  geom_line(aes(x = solver_total$ID, y = solver_total$ciu_lower , colour = "Tmt CI")) +
  geom_line(aes(x = solver_total$ID, y = solver_total$ciu_upper , colour = "Tmt CI")) +
  geom_line(aes(x = solver_total$ID, y = solver_total$c_x_ciu, colour = "Ctrl mean"))+
  geom_line(aes(x = solver_total$ID, y = solver_total$c_xciu_up , colour = "Ctrl CI")) +
  geom_line(aes(x = solver_total$ID, y = solver_total$c_xciu_low , colour = "Ctrl CI")) +
  labs(title = "CIU", x = "cycle", y = "# solvers")





