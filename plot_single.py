import pandas as pd
from matplotlib import pyplot as plt
import csv
import sys

columns = ["Size", "Measure"]
data = pd.read_csv("exps/"+sys.argv[1]+".csv", names=columns)

plt.plot(data.Size.values,data.Measure.values,label=sys.argv[2],color='b')

plt.legend(loc="upper left")

plt.suptitle(sys.argv[1])

plt.ylabel("s")
    
plt.savefig("exps/"+sys.argv[1]+".pdf",dpi=500)

plt.show()
