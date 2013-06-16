inf = open('MainP.prof', 'r')
lines = inf.readlines()[32:]
lines1 = ['\t'.join(filter((lambda x:x!=''), x.rstrip().split())) for x in lines]
# lines1 = [x.rstrip().split() for x in lines]
for line in lines1:
  print line
