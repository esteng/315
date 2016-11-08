# make em all clean
import os
import csv

def clean(file):
    total = []
    f = open(file+"_clean", "w")
    fcw = csv.writer(f)
    with open(file) as f1:
        lines = [x.replace("h", "") for x in f1.readlines()]
    for line in lines:
        split = line.split(" ")
        cleansplit = [x for x in split if x != " " and x != "\n" and x != ""]
        total.append(cleansplit)
    fcw.writerow(total[0])    
