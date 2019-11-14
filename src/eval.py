import os
l=["processing started","downloading article and contributor information","parsing article text","forking threads to download of images and contributor information on them","precompiling table columns","joining threads to download the images and contributor information on them","preparing for PDF generation","preparing images for LaTeX document","generating PDF file. LaTeX run 1 of 4","generating PDF file. LaTeX run 2 of 4","generating PDF file. LaTeX run 3 of 4","generating PDF file. LaTeX run 4 of 4","finished"] 
def f(x):
 t=open(x).read()
 d={}
 for ll in l:
   for tt in t.split("\n"):
     if ll in tt: d.update({ll:float(tt.split("(")[1].split(")")[0].strip("s"))})
 start=d["processing started"]
 end=d["finished"]
 e={}
 for k,v in d.items():
   e.update({k:(v-start)/(end-start)})
 return e
ddd={}
for ll in l:
  ddd.update({ll:0})
done=0
for x in os.listdir("./tests"):
  try:
   dd=f("./tests/"+x)
   li=[]
   for ll in l:
     li+=[str(dd[ll])]
     ddd.update({ll:ddd[ll]+dd[ll]})
   #print " ".join(li)  
   done+=1
  except:
   pass
eee={}
for k,v in ddd.items():
  eee.update({k: v/(done*1.0)})

for ll in l:
  print (ll,eee[ll])
#plot "data" u 0:1 w lines, "data" u 0:1 w lines,  "data" u 0:2 w lines, "data" u 0:3 w lines, "data" u 0:4 w lines, "data" u 0:5 w lines, "data" u 0:6 w lines, "data" u 0:7 w lines, "data" u 0:8 w lines, "data" u 0:9 w lines, "data" u 0:10 w lines, "data" u 0:11 w lines, "data" u 0:12 w lines, "data" u 0:13 w lines

