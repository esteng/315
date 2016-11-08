import csv
import os
import sys


## plan
## make train-errors clean (remove h's)
## for each file:
## put into 2d array
## take averages of columns (if empty then 0)
## put these averages into a global array as columns with header of learnability
def get_count(list):
    c = 0
    for l in list:
        if len(l)>0:
            c+=1
    return c




all_averages = []

def process_file(i):
    f2 = open("/Users/Elias/315/A2/results/averages/all_averages_{}.csv".format(i), "w")
    f2cw = csv.writer(f2)
    total = []
    averages = []
    with open("/Users/Elias/315/A2/results/my_learnability/my_learnability={}/train-errors".format(i)) as f1:
        lines = [x.replace("h", '') for x in f1.readlines()]
    for line in lines:
        split = line.split(" ")
        cleansplit = [x for x in split if x != " " and x != "\n" and x != ""]
        total.append(cleansplit)
    
    #now each row is 1 network
    #pop one off of each, average
    epoch = 0
    c = get_count(total)
    while c > 0:
        tot = 0
        for lst in total:
            try:
                value = float(lst.pop(0))
            except:
                value = 0
            tot+=value
        averages.append(tot/c) 
        epoch+=1  
        c = get_count(total)
    f2cw.writerow(["ID","percent"])
    for i,t in enumerate(averages):
        f2cw.writerow([i,t])


for i in range(0, 70, 10):
    process_file(i)
for i in range(70, 101, 5):
    process_file(i)






