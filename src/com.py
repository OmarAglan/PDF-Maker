import os
def comment(s):
  start=True
  c=False
  a=False
  acu=""
  for x in s:
    if a :
      acu=acu+x
      continue
    if x==" " and  start:
      continue
    if x=="-" and start:
      c=True
      start=False
      continue
    if x=="-" and c:
      a=True
    else:
      return ""
  return acu

def countspaces(s):
  counter=0
  for x in s:
    if x==" ":
      counter=counter+1
    else:
      break
  return counter
  
def lastspace(s,n):
  index=n
  for i,x in enumerate(s):
    if x==" " and i!=0:
      index=i
    if i>=n:
      return index
  return index
    
def breakline(s,n):
  acu=[]
  while True:
    if len(s)<=n:
      return acu+[s]
    else:
      i=lastspace(s,n)
      acu=acu+[s[:i]]
      s=s[i:]
def gets(t):
  acu=[]
  for s in t.split("\n"):
    if comment(s)!="":
      sp=countspaces(s)
      l=breakline(comment(s),80-2-sp)
      for x in l:
        acu=acu+ [(" "*sp)+"--"+"".join(x)]
    else:
      acu=acu+[s]
  return "\n".join(acu)
def puts(t):
  localacu=[]
  acu=[]
  for s in t.split("\n"):
    if comment(s)!="":
      sp=countspaces(s)
      localacu=localacu+[comment(s)]
    else:
      if localacu!=[]:
        acu=acu+[(" "*sp)+"--"+"".join(localacu)]
        localacu=[]
      acu=acu+[s]
  if localacu!=[]:
    acu=acu+[(" "*sp)+"--"+"".join(localacu)]
  return "\n".join(acu)

for f in os.listdir("."):
  if f.split(".")[-1]=="hs":
    handle=open(f)
    t=handle.read()
    handle.close
    s= (gets(puts(t)))
    t=open(f,"w").write(s)
