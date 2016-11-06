import csv
import os


all_my_lines = []
for folder in os.walk("/Users/Elias/315/A2/results/my_learnability"):
    for f in folder[2]:
        if f == "outcomes" and int(folder[0][-2:])%10 == 0:
            with open(os.path.join(folder[0].strip(), f.strip())) as f1:
                lines = [x.strip() for x in f1.readlines()]
                all_my_lines.append(lines)
f2 = open("my_win_freq.csv", "w")
f2cw = csv.writer(f2)
while len(all_my_lines[0]) > 0:
    towrite = []
    for s in all_my_lines:
        towrite.append(s.pop(0))
    f2cw.writerow(towrite)


all_orig_lines = []
for folder in os.walk("/Users/Elias/315/A2/results/original_learnability"):
    for f in folder[2]:
        if f == "outcomes" and int(folder[0][-2:])%10 == 0:
            with open(os.path.join(folder[0].strip(), f.strip())) as f1:
                lines = [x.strip() for x in f1.readlines()]
                all_orig_lines.append(lines)


f3 = open("orig_win_freq.csv", "w")
f3cw = csv.writer(f3)