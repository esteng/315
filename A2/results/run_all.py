import sys
import os


for i in range(1,11):
    if i in [5, 10]:
        continue
    os.system("mkdir /Users/Elias/315/A2/results/original_learnability={}0".format(i))

for i in range(7,10):
    os.system("mkdir /Users/Elias/315/A2/results/original_learnability={}5".format(i))