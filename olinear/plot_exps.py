import pandas as pd
from matplotlib import pyplot as plt
import csv
import sys

columns = ["Size", "OCaml", "JS"]
data = pd.read_csv(sys.argv[1]+".csv", names=columns)
figure, axis = plt.subplots(3,1)

figure.set_figwidth(16)
figure.set_figheight(16)

axis[0].plot(data.Size.values,data.OCaml.values,label="OCaml",color='b')
axis[1].plot(data.Size.values,data.OCaml.values,label="OCaml",color='b')
axis[0].plot(data.Size.values,data.JS.values,label="JS",color='m')
axis[2].plot(data.Size.values,data.JS.values,label="JS",color='m')

axis[0].legend(loc="upper left")
axis[1].legend(loc="upper left")
axis[2].legend(loc="upper left")


figure.suptitle(sys.argv[1])

plt.ylabel("s")

if (len(sys.argv) >= 3):
    plt.xlabel(sys.argv[2])
    
plt.savefig(sys.argv[1]+".jpg",dpi=500)

plt.show()
