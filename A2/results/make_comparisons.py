import csv
import os


all_my_lines = []
for i in range(10, 101, 10):
    with open("/Users/Elias/315/A2/results/my_learnability/my_learnability={}/outcomes".format(i)) as f1:
        lines = [x.strip().replace("quit","0").replace("win","1") for x in f1.readlines()]
        all_my_lines.append(lines)

print(all_my_lines)
f2 = open("my_win_freq.csv", "w")
f2cw = csv.writer(f2)
f2cw.writerow([x for x in range(10, 101, 10)])
while len(all_my_lines[0]) > 0:
    towrite = []
    for s in all_my_lines:
        towrite.append(s.pop(0))
    f2cw.writerow(towrite)


all_orig_lines = []
for i in range(10, 101, 10):
    with open("/Users/Elias/315/A2/results/original_learnability/original_learnability={}/outcomes".format(i)) as f1:
        lines = [x.strip().replace("quit","0").replace("win","1").replace("stop","0") for x in f1.readlines()]
        all_orig_lines.append(lines)


f3 = open("orig_win_freq.csv", "w")
f3cw = csv.writer(f3)
f3cw.writerow([x for x in range(10, 101, 10)])
while len(all_orig_lines[0]) > 0:
    towrite = []
    for s in all_orig_lines:
        towrite.append(s.pop(0))
    f3cw.writerow(towrite)