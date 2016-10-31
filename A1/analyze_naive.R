require(ggplot2)

myac_path = "/Users/Elias/Documents/models/persist/ac/"
myam_path = "/Users/Elias/Documents/models/persist/am/"
myciu_path = "/Users/Elias/Documents/models/persist/ciu/"
ac_path = "/Users/Elias/Documents/models/cycle/ac/"
am_path = "/Users/Elias/Documents/models/cycle/am/"
ciu_path = "/Users/Elias/Documents/models/cycle/ciu/"

new_am = read.csv(paste(myam_path, "countx-newbies", sep = ""), header = FALSE, sep = " ")
new_ac = read.csv(paste(myac_path, "countx-newbies", sep = ""), header = FALSE, sep = " ")
new_ciu = read.csv(paste(myciu_path, "countx-newbies", sep = ""), header = FALSE, sep = " ")
new_am$V21 = NULL
new_ac$V21 = NULL
new_ciu$V21 = NULL
am = read.csv(paste(am_path, "xcounts", sep = ""), header = FALSE, sep = " ")
ac = read.csv(paste(ac_path, "xcounts", sep = ""), header = FALSE, sep = " ")
ciu = read.csv(paste(ciu_path, "xcounts", sep = ""), header = FALSE, sep = " ")
am$V21 = NULL
ac$V21 = NULL
ciu$V21 = NULL
N = 275
M = 1101

new_total = data.frame(new_am = double(N), new_ac = double(N), new_ciu = double(N))
new_total$new_am = rowMeans(new_am)
new_total$new_ac = rowMeans(new_ac)
new_total$new_ciu = rowMeans(new_ciu)
new_total$am_sd = apply(new_am,1,sd)
new_total$stderr = new_total$am_sd/sqrt(20)
new_total$upper = new_total$new_am + 1.96*new_total$stderr
new_total$lower = new_total$new_am - 1.96*new_total$stderr
new_total$ID <-seq.int(nrow(new_total))

solver_total = data.frame(x_am = double(M), x_ac = double(M), x_ciu = double(M))
solver_total$x_am = rowMeans(am)
solver_total$x_am_sd = apply(am,1,sd)
solver_total$stderr = solver_total$x_am_sd/sqrt(20)
solver_total$upper = solver_total$x_am + 1.96*solver_total$stderr
solver_total$lower = solver_total$x_am - 1.96*solver_total$stderr
solver_total$ID<-seq.int(nrow(solver_total))


ggplot() + 
  geom_line(data = solver_total, aes(x = solver_total$ID, y = solver_total$x_am, colour = "orig. mean")) + 
  geom_line(data = solver_total, aes(x = solver_total$ID, y = solver_total$lower , colour = "orig. CI")) +
  geom_line(data = solver_total,aes(x = solver_total$ID, y = solver_total$upper , colour = "orig. CI")) +
  geom_line(data = new_total, aes(x = new_total$ID, y = new_total$new_am, colour = "newbie mean"))+
  geom_line(data = new_total, aes(x = new_total$ID, y =new_total$upper , colour = "newbie CI")) +
  geom_line(data = new_total, aes(x = new_total$ID, y =new_total$lower , colour = "newbie CI")) +
  labs(title = "AM", x = "cycle", y = "# solvers")


new_total$ac_sd = apply(new_ac,1,sd)
new_total$ac_stderr = new_total$ac_sd/sqrt(20)
new_total$ac_upper = new_total$new_ac + 1.96*new_total$ac_stderr
new_total$ac_lower = new_total$new_ac - 1.96*new_total$ac_stderr

solver_total$x_ac = rowMeans(ac)
solver_total$x_ac_sd = apply(ac,1,sd)
solver_total$ac_stderr = solver_total$x_ac_sd/sqrt(20)
solver_total$ac_upper = solver_total$x_ac + 1.96*solver_total$ac_stderr
solver_total$ac_lower = solver_total$x_ac - 1.96*solver_total$ac_stderr

ggplot() + 
  geom_line(data = solver_total, aes(x = solver_total$ID, y = solver_total$x_ac, colour = "orig. mean")) + 
  geom_line(data = solver_total, aes(x = solver_total$ID, y = solver_total$ac_lower , colour = "orig. CI")) +
  geom_line(data = solver_total,aes(x = solver_total$ID, y = solver_total$ac_upper , colour = "orig. CI")) +
  geom_line(data = new_total, aes(x = new_total$ID, y = new_total$new_ac, colour = "newbie mean"))+
  geom_line(data = new_total, aes(x = new_total$ID, y =new_total$ac_upper , colour = "newbie CI")) +
  geom_line(data = new_total, aes(x = new_total$ID, y =new_total$ac_lower , colour = "newbie CI")) +
  labs(title = "AC", x = "cycle", y = "# solvers")

new_total$ciu_sd = apply(new_ciu,1,sd)
new_total$ciu_stderr = new_total$ciu_sd/sqrt(20)
new_total$ciu_upper = new_total$new_ciu + 1.96*new_total$ciu_stderr
new_total$ciu_lower = new_total$new_ciu - 1.96*new_total$ciu_stderr

solver_total$x_ciu = rowMeans(ciu)
solver_total$x_ciu_sd = apply(ciu,1,sd)
solver_total$ciu_stderr = solver_total$x_ciu_sd/sqrt(20)
solver_total$ciu_upper = solver_total$x_ciu + 1.96*solver_total$ciu_stderr
solver_total$ciu_lower = solver_total$x_ciu - 1.96*solver_total$ciu_stderr

ggplot() + 
  geom_line(data = solver_total, aes(x = solver_total$ID, y = solver_total$x_ciu, colour = "orig. mean")) + 
  geom_line(data = solver_total, aes(x = solver_total$ID, y = solver_total$ciu_lower , colour = "orig. CI")) +
  geom_line(data = solver_total,aes(x = solver_total$ID, y = solver_total$ciu_upper , colour = "orig. CI")) +
  geom_line(data = new_total, aes(x = new_total$ID, y = new_total$new_ciu, colour = "newbie mean"))+
  geom_line(data = new_total, aes(x = new_total$ID, y =new_total$ciu_upper , colour = "newbie CI")) +
  geom_line(data = new_total, aes(x = new_total$ID, y =new_total$ciu_lower , colour = "newbie CI")) +
  labs(title = "CIU", x = "cycle", y = "# solvers")


