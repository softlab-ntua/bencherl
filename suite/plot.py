#!/usr/bin/env python
# -*- coding: utf-8 -*-

import itertools, math, re, sys, uuid
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np

class Latency(object):
  """
  The latency statistics at a node
  """
  
  # The possible attributes of the statistics
  attrs = set(["messages", "average", "median"])
  # The attribute that is used for the plots
  default = "median"
  
  def __init__(self, **kwargs):
    for k in Latency.attrs:
      v = kwargs[k]
      setattr(self, k, v)

  def __str__(self):
    xs = []
    for x in Latency.attrs:
      k = getattr(self, x)
      xs.append("%s: %s" % (x.capitalize(), k))
    return ", ".join(xs)

class Conf(object):
  """
  A configuration scenario
  """
  
  # The possible attributes of a configuration
  attrs = set(["schedulers", "servers", "routers", "clients", "client_processes"])
  
  def __init__(self, **kwargs):
    self.id = uuid.uuid4()
    self.latencies = {}
    for k in Conf.attrs:
      v = kwargs[k]
      setattr(self, k, v)
  
  def __str__(self):
    xs = []
    for x in Conf.attrs:
      k = getattr(self, x)
      xs.append("%s: %s" % (x.capitalize(), k))
    return ", ".join(xs)
  
  def add_latency(self, node, lncy):
    self.latencies[node] = lncy
  
  def get_latency(self):
    """
    For now, returns the average median latency of the nodes
    """
    xs = [getattr(l, Latency.default) for l in self.latencies.values()]
    return sum(xs) / float(len(xs))

def reg_find(pat, s):
  return re.search(pat, s, re.I).group(1)

def pp_combo(xs, vs):
  s = []
  for x, v in zip(xs, vs):
    s.append("%s: %s" % (x.capitalize(), v))
  return ", ".join(s)

def fname_combo(xs, vs):
  s = []
  for x, v in zip(xs, vs):
    s.append("%s_%s" % (x, v))
  return "_".join(s)

### Main program

if len(sys.argv) < 2:
  print "Too few arguments..."
  print "Usage: ./plot.py statistics-file"
  sys.exit(1)

# Parse the file with the statistics
curr, confs = None, {}
with open(sys.argv[1]) as f:
  for l in f.readlines():
    if l.startswith("# Conf:"):
      # Declaration of a configuration
      ks = { "schedulers" : int(reg_find(r'(\d+) scheduler\(s\)', l))
           , "servers" : int(reg_find(r'(\d+) server\(s\)', l))
           , "routers" : int(reg_find(r'(\d+) router\(s\)', l))
           , "clients" : int(reg_find(r'(\d+) client\(s\)', l))
           , "client_processes" : int(reg_find(r'(\d+) client processes', l))
           }
      c = Conf(**ks)
      confs[c.id] = c
      curr = c
    else:
      # Latency results at a node
      [node, msgs, avg, mdn] = l.strip().split(" ")
      ks = { "messages" : int(msgs.strip())
           , "average" : float(avg.strip())
           , "median" : float(mdn.strip())
           }
      lncy = Latency(**ks)
      c.add_latency(node.strip(), lncy)
  f.close()

# Get all the possible combinations of n-1 dimensions
combos = itertools.combinations(Conf.attrs, len(Conf.attrs) - 1)
for cmb in combos:
  # Group the configurations that have the same values on the selected attributes
  d = {}
  for cnf in confs.values():
    key = []
    for attr in cmb:
      key.append(getattr(cnf, attr))
    tkey = tuple(key)
    d[tkey] = [cnf.id] if not tkey in d else [cnf.id] + d[tkey]
  
  for key in d:
    cs = d[key]
    if len(cs) > 1:
      # Find the varying dimension
      (xdim,) = Conf.attrs - set(cmb)
      # Gather the data from the related configurations
      data = []
      for cid in cs:
        c = confs[cid]
        v = getattr(c, xdim), c.get_latency()
        data.append(v)
      x, y = zip(*sorted(data))
      # Create the figure
      fig = plt.figure()
      ax = fig.add_subplot(111)
      ax.set_xlabel(xdim.capitalize())
      ax.set_ylabel(Latency.default.capitalize() + " latency (in ms)")
      ax.set_title(pp_combo(cmb, key))
      line, = ax.plot(x, y, '-o', lw=2, ms=8)
      # Set the xaxis ticks
      xmin, xmax = min(x), max(x)
      xstep = (xmax - xmin) / (len(x) - 1)
      plt.xticks(np.arange(xmin, xmax + xstep, xstep))
      ax.xaxis.set_major_formatter(ticker.FormatStrFormatter('%0.0f'))
      ax.yaxis.set_major_formatter(ticker.FormatStrFormatter('%0.1f'))
      plt.xticks(fontsize=9)
      plt.yticks(fontsize=9)
      #plt.show()
      plt.savefig(fname_combo(cmb, key))
