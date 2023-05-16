import pandas as pd
from matplotlib import pyplot as plt
import csv
import sys

columns = ["Size", "OCaml", "JS"]
data = pd.read_csv(sys.argv[1], names=columns)
plt.plot(data.Size,data.OCaml,label="OCaml")
plt.plot(data.Size,data.JS,label="JS")
plt.legend(loc="upper left")
plt.show()