#!python

import os
import math
import shutil
import subprocess

# this is inclusive of the max!
def float_seq(mn, mx, steps):
  step_size = (mx-mn) / steps
  return [mn + step_size*i for i in range(steps+1)]


# 4500 is max, 5000 is too large for memory sometimes
points = [100] + [x*500 for x in range(1,10)]
dims = range(3, 11)
d3_radii = float_seq(0.05, 0.5, 20)

def sphere_vol(n, r):
  p = math.pow(math.pi, n/2.0)
  g = math.gamma(n/2.0 + 1)
  return (p / g) * math.pow(r, n)

def sphere_radius(n, vol):
  p = math.pow(math.pi, n/2.0)
  g = math.gamma(n/2.0 + 1)
  rn = vol * g / p
  return math.pow(rn, 1.0/n)

def frags_3d(r):
  cmd = "library(methods);library(utils);library(devtools);load_all('~/Projects/headkicker'); cat(exp.frags(3, %s, 1))"
  devnull = open(os.devnull, 'w')
  output = subprocess.check_output(["Rscript", "-e", cmd % (r)], 
                                   stderr=devnull)
  return float(output)

def f2r(f, d):
  cmd = "library(methods);library(utils);library(devtools);load_all('~/Projects/headkicker'); cat(radius.for.frags(%s, 1, %s))"
  devnull = open(os.devnull, 'w')
  output = subprocess.check_output(["Rscript", "-e", cmd % (d, f)], 
                                   stderr=devnull)
  return float(output)

def run_exp(dd, N, rr):
  print "d:", dd, "p:", N, "r:", rr
  cmd = "run -timedemo %s %s %s" % (dd, N, rr)
  if os.name != 'nt':
    ret = os.system('sbt "' + cmd + '"')
  else:
    ret = os.system('sbt "' + cmd + '"')
  os.system("cat gcevents.log >> gc_events.out")
  if ret != 0: exit()

def mv(src, dst):
  try:
    shutil.move(src, dst)
  except:
    pass
    
if __name__=='__main__':
  mv('all_times.csv', 'all_times.csv.old')
  mv('proc_list.out', 'proc_list.out.old')
  mv('temp_list.out', 'temp_list.out.old')
  mv('gc_events.out', 'gc_events.out.old')
  mv('screen_list.out', 'screen_list.out.old')
  
  for d in dims:
    for r3d in d3_radii:
      f = frags_3d(r3d)
      rr = f2r(f, d)
      for N in points:
        run_exp(d, N, rr)
  
