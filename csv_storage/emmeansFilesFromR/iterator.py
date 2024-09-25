import pandas as pd
import os

attributeList = ['perceived_attractiveness',
 'feminine',
 'happy',
 'intelligence',
 'trustworthy',
 'unusual',
 'sociability']

for attr in attributeList:
    for beautification in ["original","beautified"]:
        print("%s : %s"%(attr.split("_")[-1].title(),beautification.title()))
        fileName = "%s_emmContrasts_%s.csv"%(attr,beautification)
        df = pd .read_csv(fileName)
        print(df)
        input()
        os.system("clear")
