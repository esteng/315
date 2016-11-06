require(ggplot2)

my_50_path = "/Users/Elias/315/A2/results/my_learnability=50/"
my_100_path = "/Users/Elias/315/A2/results/my_learnability=100/"
orig_50_path = "/Users/Elias/315/A2/results/original_learnability=50/"
orig_100_path = "/Users/Elias/315/A2/results/original_learnability=100/"

my_50 = read.csv(paste(my_50_path, "train-errors0_clean", sep = ""), header = FALSE)
my_50$ID <-seq.int(nrow(my_50))

my_100 = read.csv(paste(my_100_path, "train-errors0_clean", sep = ""), header = FALSE)
my_100$ID <-seq.int(nrow(my_100))

orig_50 = read.csv(paste(orig_50_path, "train-errors0_clean", sep = ""), header = FALSE)
orig_50$ID <-seq.int(nrow(orig_50))

orig_100 = read.csv(paste(orig_100_path, "train-errors0_clean", sep = ""), header = FALSE)
orig_100$ID <-seq.int(nrow(orig_100))

ggplot()+
  geom_line(data = my_50, aes(x=my_50$ID, y=my_50$V1, colour = 'new 50%'))+
  geom_point(data=my_50, aes(x=my_50$ID, y=my_50$V2, colour = 'new 50%'))+
  geom_line(data = my_100, aes(x=my_100$ID, y=my_100$V1, colour = 'new 100%'))+
  geom_point(data=my_100, aes(x=my_100$ID, y=my_100$V2, colour = 'new 100%'))+
  geom_line(data = orig_50, aes(x=orig_50$ID, y=orig_50$V1, colour = 'orig 50%'))+
  geom_point(data=orig_50, aes(x=orig_50$ID, y=orig_50$V2, colour = 'orig 50%'))+
  geom_line(data = orig_100, aes(x=orig_100$ID, y=orig_100$V1, colour = 'orig 100%'))+
  geom_point(data=orig_100, aes(x=orig_100$ID, y=orig_100$V2, colour = 'orig 100%'))


