#!/usr/bin/env python
# -*- coding: utf-8 -*-

import itertools, math, re, sys, uuid
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np

class Throughput(object):
  """
  The throughput statistics at a node
  """

  axis_title = "Throughtput (in messages / sec)"

  def __init__(self, exectime):
    self.exectime = exectime
    self.v = None

  def set_messages(self, msgs):
    self.messages = msgs
    self.v = float(msgs) / float(self.exectime)

  def __str__(self):
    return "Throughtput: %0.2lf messages/sec" % self.v

class Latency(object):
  """
  The latency statistics at a node
  """

  # The possible attributes of the statistics
  attrs = set(["messages", "average", "median"])
  # The attribute that is used for the plots
  default = "median"
  unitFactors = {"microsecs" : 0.001, "millisecs" : 1.0}
  defaultUnit = "millisecs"

  def __init__(self, **kwargs):
    for k in Latency.attrs:
      (tp, v) = kwargs[k]
      if tp == None:
        vv = v
      else:
        vv = v * Latency.unitFactors[tp]
      setattr(self, k, vv)

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
    return "\n".join(xs)
  
  def add_latency(self, node, lncy):
    self.latencies[node] = lncy
  
  def get_latency(self):
    """
    For now, returns the average median latency of the nodes
    """
    xs = [getattr(l, Latency.default) for l in self.latencies.values()]
    return sum(xs) / float(len(xs))

  def add_throughtput(self, thrpt):
    self.throughput = thrpt

  def get_throughput(self):
    if self.throughput.v == None:
      msgs = sum([l.messages for l in self.latencies.values()])
      self.throughput.set_messages(msgs)
    return self.throughput.v

def reg_find_conf(pat, s):
  return re.search(pat, s, re.I).group(1)

def pp_combo(xs, vs):
  s = []
  for x, v in zip(xs, vs):
    s.append("%s: %s" % (x.capitalize(), v))
  return ", ".join(s)

def fname_combo(xs, vs, postfix):
  s = []
  for x, v in zip(xs, vs):
    s.append("%s_%s" % (x, v))
  s.append(postfix)
  return "_".join(s)

def create_single_graph(xaxis_label, yaxis_label, title, x, y, fname):
  """
  Create a graph for the configurations that vary in one dimension
  """
  fig = plt.figure()
  ax = fig.add_subplot(111)
  ax.set_xlabel(xaxis_label)
  ax.set_ylabel(yaxis_label)
  ax.set_title(title)
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
  plt.savefig(fname)

def create_overall_graph(yaxis_label, x, y, lbls, fname):
  """
  Create a graph for all the configurations
  """
  fig = plt.figure()
  ax = fig.add_subplot(111)
  ax.set_xlabel("Configuration")
  ax.set_ylabel(yaxis_label)
  line, = ax.plot(x, y, '--o', lw=2, ms=8)
  ax.set_xticks(x)
  ax.set_xticklabels(lbls)
  ax.yaxis.set_major_formatter(ticker.FormatStrFormatter('%0.1f'))
  plt.xticks(fontsize=9)
  plt.yticks(fontsize=9)
  plt.tight_layout()
  #plt.show()
  plt.savefig(fname)

### Main program

if len(sys.argv) < 2:
  print "Too few arguments..."
  print "Usage: ./plot.py statistics-file"
  sys.exit(1)

# Parse the file with the statistics
curr, confs, added = None, {}, {}
with open(sys.argv[1]) as f:
  for l in f.readlines():
    # Declaration of a configuration
    if l.startswith("# Conf:"):
      ks = { "schedulers" : int(reg_find_conf(r'(\d+) scheduler\(s\)', l))
           , "servers" : int(reg_find_conf(r'(\d+) server\(s\)', l))
           , "routers" : int(reg_find_conf(r'(\d+) router\(s\)', l))
           , "clients" : int(reg_find_conf(r'(\d+) client\(s\)', l))
           , "client_processes" : int(reg_find_conf(r'(\d+) client processes', l))
           }
      c = Conf(**ks)
      added_key = str(c)
      if added_key in added:
        old_id = added[added_key]
        confs.pop(old_id, None)
      added[added_key] = c.id
      confs[c.id] = c
      curr = c
    # Comment
    elif l.startswith("#"):
      pass
    # Execution time
    elif l.startswith("*"):
      t = Throughput(float(reg_find_conf(r'([\d\.]+) secs', l)))
      c.add_throughtput(t)
    # Latency results at a node
    else:
      mtch = re.search(r"'(.+)'\s+(\d+)\s+([\d\.]+)\s+(\w+)\s+([\d\.]+)\s+(\w+)", l, re.I)
      ks = { "messages" :
               (None, int(mtch.group(2)))
           , "average" :
               (mtch.group(4), float(mtch.group(3)))
           , "median" :
               (mtch.group(6), float(mtch.group(5)))
           }
      lncy = Latency(**ks)
      c.add_latency(mtch.group(1), lncy)
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
      # Gather the latency & throughpur data from the related configurations
      data_lat, data_thr = [], []
      for cid in cs:
        c = confs[cid]
        v_lat = getattr(c, xdim), c.get_latency()
        data_lat.append(v_lat)
        v_thr = getattr(c, xdim), c.get_throughput()
        data_thr.append(v_thr)
      x_lat, y_lat = zip(*sorted(data_lat))
      x_thr, y_thr = zip(*sorted(data_thr))
      # Create the latency figure
      xaxis_label = xdim.capitalize()
      yaxis_label = Latency.default.capitalize() + " latency (in " + Latency.defaultUnit + ")"
      title = pp_combo(cmb, key)
      fname = fname_combo(cmb, key, "latency")
      create_single_graph(xaxis_label, yaxis_label, title, x_lat, y_lat, fname)
      # Create the throughput figure
      xaxis_label = xdim.capitalize()
      yaxis_label = Throughput.axis_title
      title = pp_combo(cmb, key)
      fname = fname_combo(cmb, key, "throughput")
      create_single_graph(xaxis_label, yaxis_label, title, x_thr, y_thr, fname)

cfs = sorted(confs.values(), key=lambda x: x.clients, reverse=False)
# Gather the latency data from all the configurations
x = np.arange(0, len(confs), 1)
lbls, y = [], []
for c in cfs:
  lbls.append(str(c))
  y.append(c.get_latency())
# Create the latency figure of all the configurations
yaxis_label = Latency.default.capitalize() + " latency (in " + Latency.defaultUnit + ")"
create_overall_graph(yaxis_label, x, y, lbls, "overall_latency.png")

# Gather the throughtput data from all the configurations
x = np.arange(0, len(confs), 1)
lbls, y = [], []
for c in cfs:
  lbls.append(str(c))
  y.append(c.get_throughput())
# Create the throughput figure of all the configurations
yaxis_label = Throughput.axis_title
create_overall_graph(yaxis_label, x, y, lbls, "overall_throughput.png")
