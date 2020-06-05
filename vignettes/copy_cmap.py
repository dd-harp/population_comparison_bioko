import csv
import matplotlib.cm
import sys
cmap = sys.argv[1]
v = matplotlib.cm.get_cmap(cmap)
wr = csv.writer(open(cmap + ".csv", "w"))
for col in v.colors:
	wr.writerow(map(str, col))