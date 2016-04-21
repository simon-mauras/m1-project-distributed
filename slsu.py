#!/usr/bin/env python3

import os, re

relpath = os.path.relpath(".", os.path.expanduser("~"))

def hosts():
  with open("bin/.hosts.erlang", "r") as hostsfile:
    for line in hostsfile.readlines():
      host = re.fullmatch(r"'((?:[0-9]{1,3}\.){3}[0-9]{1,3})'.", line.rstrip())
      if host is not None:
        yield(host.group(1))

for host in hosts():
  command = "cd %s && nohup make NAME=agent$$@%s agent" % (relpath, host)
  os.system("ssh -t %s '%s'" % (host, command))
