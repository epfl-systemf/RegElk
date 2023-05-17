import pandas as pd
from matplotlib import pyplot as plt
import csv
import sys

columns = ["Size", "OCaml", "JS"]
data = pd.read_csv(sys.argv[1]+".csv", names=columns)
plt.plot(data.Size.values,data.OCaml.values,label="OCaml")
plt.plot(data.Size.values,data.JS.values,label="JS")
plt.legend(loc="upper left")
plt.title(sys.argv[1])
plt.ylabel("s")
if (len(sys.argv) >= 3):
    plt.xlabel(sys.argv[2])
plt.savefig(sys.argv[1]+".jpg")
plt.show()
