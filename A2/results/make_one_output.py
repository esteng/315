import csv
# 1  2  3  4  5  6  7  8  9  10  11  12  13 
# 10 20 30 40 50 60 70 75 80 85  90  95  100
all_lines =[]

for i in range(10, 71, 10):
    r = 0
    with open("/Users/Elias/315/A2/results/my_learnability={}/output-epochs".format(i)) as f1:
        lines = f1.readlines()
        newlines = []
        for line in lines:
            newlines.append(line.strip())
        all_lines.append(newlines)
print(all_lines)
for i in range(75, 101, 5):
        r = 0
        with open("/Users/Elias/315/A2/results/my_learnability={}/output-epochs".format(i)) as f1:
            lines = f1.readlines()
            newlines = []
            for line in lines:
                newlines.append(line.strip())
            all_lines.append(newlines)

f2 = open("/Users/Elias/315/A2/results/all_epochs.csv", 'w')
f2cw= csv.writer(f2)
f2cw.writerow([x*10 for x in range(1, 8)]+[y for y in range(75, 101, 5)])

while len(all_lines[0])>0:
    towrite = []
    for s in all_lines:
        towrite.append(s.pop(0))
    f2cw.writerow(towrite)   


