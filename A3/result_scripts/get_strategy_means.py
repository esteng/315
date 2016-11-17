import csv




all_means = []


with open("../results/original/strategies0") as f:
    lines = f.readlines()
for line in lines:
    split = line.split(" ")[0:-1]
    all_means.append([float(x) for x in split])

for i in range(1,20):
    with open("../results/original/strategies{}".format(i)) as f1:

        lines = f1.readlines()
    for j,line in enumerate(lines):
        split = line.split(" ")[0:-1]
        all_means[j] = [ (float(x)+float(y)) for x,y in zip(all_means[j], split)]


            
f2 = open("../results/orig_mean_strategies.csv", "w")
f2cw = csv.writer(f2)
f2cw.writerow([str(x) for x in range(1,5)]    )
for m in all_means:
    f2cw.writerow([m[i]/20 for i in range(4)])

