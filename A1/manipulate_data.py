import re
import csv 

def manipulate_grid(path, file, n):
    lines2 = []
    with open(path + "/" + file) as f1:
        lines = f1.readlines()
    for line in lines:
        line2= re.split(" ", line.strip())
        line2 = [int(x.strip()) for x in line2]
        lines2.append(line2)
    f2 = open(path+"/"+file+".csv", 'w')
    f2cw = csv.writer(f2)
    # f2cw.writerow(["replication_{}".format(x).strip() for x in range(20)])
    f2cw.writerow(['id','average{}'.format(n)])
    for i,line in enumerate(lines2):
        avg = sum(line)/len(line)


        f2cw.writerow([i,avg])


manipulate_grid("/Users/Elias/Documents/models", 'scounts', 0)

manipulate_grid("/Users/Elias/Documents/models", "xcounts", 1)

manipulate_grid("/Users/Elias/Documents/models", "ucounts", 2)