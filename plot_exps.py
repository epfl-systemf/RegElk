import pandas as pd
from matplotlib import pyplot as plt
import csv
import sys

columns_ocaml = ["Size", "OCaml"]
data_ocaml = pd.read_csv("exps/"+sys.argv[1]+"_ocaml.csv", names=columns_ocaml)

columns_js = ["Size", "JS"]
data_js = pd.read_csv("exps/"+sys.argv[1]+"_js.csv", names=columns_js)

figure, axis = plt.subplots(3,1)

figure.set_figwidth(16)
figure.set_figheight(16)

axis[0].plot(data_ocaml.Size.values,data_ocaml.OCaml.values,label="OCaml",color='b')
axis[1].plot(data_ocaml.Size.values,data_ocaml.OCaml.values,label="OCaml",color='b')
axis[0].plot(data_js.Size.values,data_js.JS.values,label="JS",color='m')
axis[2].plot(data_js.Size.values,data_js.JS.values,label="JS",color='m')

axis[0].legend(loc="upper left")
axis[1].legend(loc="upper left")
axis[2].legend(loc="upper left")


figure.suptitle(sys.argv[1])

plt.ylabel("s")

if (len(sys.argv) >= 3):
    plt.xlabel(sys.argv[2])
    
plt.savefig("exps/"+sys.argv[1]+".pdf",dpi=500)

plt.show()
