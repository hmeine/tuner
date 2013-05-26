#!python

import os
import math
import shutil
import subprocess

# 4500 is max, 5000 is too large
points = [100] + [x*500 for x in range(1,10)]
dims = range(3, 11)
dims = [4]
d3_radii = [0.05 + 0.005 * i for i in range(0, 10)] + \
           [0.1 + 0.05 * i for i in range(0, 9)]

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

def run_exp(dd, rr):
  for N in points:
    print "d:", dd, "p:", N, "r:", rr
    cmd = "run -timedemo %s %s %s" % (dd, N, rr)
    if os.name != 'nt':
      ret = os.system('sbt "' + cmd + '"')
    else:
      ret = os.system('sbt "' + cmd + '"')
    if ret != 0: break

try:
  shutil.move('static_times.csv', 'static_times.csv.old')
  shutil.move('drawing_times.csv', 'drawing_times.csv.old')
except:
  pass

for d in dims:
  for r3d in d3_radii:
    f = frags_3d(r3d)
    rr = f2r(f, 3)
    run_exp(d, rr)

