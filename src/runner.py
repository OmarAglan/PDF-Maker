import multiprocessing
import os
import sys
import time
import random
import subprocess
global lo
lo=multiprocessing.Lock()

def getnext(l):
  l.acquire()
  d=os.listdir(".")
  tf=open("1.sh")
  t=tf.read().split("\n")[:-1]
  tf.close()
  try:
    ff=open("lockfile")
    lf=ff.read()
    ff.close()
    lf=lf.split("\n")[:-1]
  except:
    lf=[]
  for x in t:
     y=x.split(" ")
     if (y[5] not in d) and (y[5] not in lf):
        lf+=[y[5]]
        s=""
        for g in lf:
          s+=g+"\n"
        ff=open("lockfile","w")
        lf=ff.write(s)
        ff.close() 
        l.release()
        return True,x
  l.release()
  return False,None
  
n=int(sys.argv[1])

class Runnable:
  def __init__(self):
     pass
  def __call__(self,l,n):
    self.n=n
    self.l=l
    while True:
      b,d=getnext(self.l)
      if not b: 
        return
      fn=d.split(" ")[5]
      print (fn,self.n)
      subprocess.run(d.split(" "))


for i in range(n):
  r=Runnable()
  t=multiprocessing.Process(target=r, args=(lo, i))
  t.start()


