#!python

import os
import math
import shutil

dims = [3, 4, 5, 6, 7, 8]
points = [pow(2, x) for x in range(10,21)]
base_r = [x/10.0 for x in range(1,6)] + [x/20.0 for x in range(1,11,2)]
exp_count = 3

def sphere_vol(n, r):
  p = math.pow(math.pi, n/2.0)
  g = math.gamma(n/2.0 + 1)
  return (p / g) * math.pow(r, n)

def sphere_radius(n, vol):
  p = math.pow(math.pi, n/2.0)
  g = math.gamma(n/2.0 + 1)
  rn = vol * g / p
  return math.pow(rn, 1.0/n)

def run_exp(dd, rr):
  for N in points:
    print "d:", dd, "p:", N, "r:", rr
    cmd = "run -timedemo %s %s %s" % (dd, N, rr)
    if os.name != 'nt':
      ret = os.system('sbt "' + cmd + '"')
    else:
      ret = os.system('sbt "' + cmd + '"')
    if ret != 0: break

shutil.move('static_times.csv', 'static_times.csv.old')
shutil.move('drawing_times.csv', 'drawing_times.csv.old')

for d in dims:
  for r in sorted(base_r):
    #d3vol = sphere_vol(3, r)
    #rr = sphere_radius(d, d3vol)
    vol = 2.0 * r
    if d == 3: rr = r
    else:      rr = sphere_radius(d-2, vol)
    run_exp(d, rr)
    #print d, r, rr

