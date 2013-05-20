#!python

import os
import math
import shutil

STEPS = 10
#points = [pow(2, x) for x in range(10,21)]
points = [10, 20, 30]

# these are ranges
small_r_ranges = [
  (3,0.05,0.10),
  (4,0.1110274,0.1888459),
  (5,0.1820622,0.2813588),
  (6,0.2566508,0.3725323),
  (7,0.3314793,0.4599944),
  (8,0.4050211,0.5434151),
]

# these are actual numbers
large_r = [
  (3,0.15),
  (3,0.20),
  (3,0.25),
  (3,0.30),
  (3,0.35),
  (3,0.40),
  (3,0.45),
  (3,0.50),
  (4,0.2585059),
  (4,0.3237339),
  (4,0.3861447),
  (4,0.4466526),
  (4,0.5058640),
  (4,0.5642333),
  (4,0.6221396),
  (4,0.6799346),
  (5,0.3646144),
  (5,0.4394349),
  (5,0.5088121),
  (5,0.5742420),
  (5,0.6365387),
  (5,0.6961351),
  (5,0.7532204),
  (5,0.8078192),
  (6,0.4661741),
  (6,0.5489244),
  (6,0.6253015),
  (6,0.6977744),
  (6,0.7680074),
  (6,0.8373636),
  (6,0.9072191),
  (6,0.9793229),
  (7,0.5610659),
  (7,0.6490777),
  (7,0.7296070),
  (7,0.8056319),
  (7,0.8790900),
  (7,0.9514502),
  (7,1.0239579),
  (7,1.0976875),
  (8,0.6502349),
  (8,0.7424750),
  (8,0.8267239),
  (8,0.9066609),
  (8,0.9850360),
  (8,1.0647049),
  (8,1.1500879),
  (8,1.252687)
]

small_r = [(d, minr+x*(maxr-minr)/STEPS) for x in range(STEPS+1)
           for d, minr, maxr in small_r_ranges]

dim_r = sorted(small_r + large_r)

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

for d, rr in dim_r:
  run_exp(d, rr)

