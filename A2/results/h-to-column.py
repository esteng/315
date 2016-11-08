import os 
import csv
import sys
import itertools


def convert_file(file):
    cwd = os.path.dirname(os.path.realpath(__file__))

    with open(os.path.join(cwd, file)) as f1:
        lines = f1.readlines()

    f2 = open(os.path.join(cwd, file+"_clean"), "w")
    f2cw = csv.writer(f2)
    all_clean = []
    towrite = None

    for i,line in enumerate(reversed(lines)):
        if line.strip() in ["h", "H"]:
            towrite = lambda x: x+.46
            continue
        else:
            if towrite is not None:
                all_clean.append([float(line), towrite(float(line))])
            else:   
                all_clean.append([float(line), "NA"])
            towrite = None

    for l in reversed(all_clean):
        f2cw.writerow(l)
if __name__=="__main__":
    file = sys.argv[1]
    convert_file(file)