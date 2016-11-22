import csv
strat_counts = {}

def get_last_strategy(file):
    with open(file) as f1:
        lines =f1.readlines()
        last = lines[-1].split(" ")[-2]
        print(last)
        try:
            strat_counts[last]+=1
        except KeyError:
            strat_counts[last] = 1

for i in range(0,20):
    path = "/Users/Elias/315/A3/results/original/strategies{}".format(i)
    get_last_strategy(path)


f2= open("/Users/Elias/315/A3/results/winners",'w')
fieldnames = [str(x) for x in range(5)]
f2cw = csv.DictWriter(f2, fieldnames)
f2cw.writerow(strat_counts)