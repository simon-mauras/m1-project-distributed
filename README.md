# M1 Project : Distributed Systems

## Deployment

### Create agents

On each machine, run ```make agent```

### Create hosts file

```
nano bin/.hosts.erlang  # Cf below
```
The syntax is the following
```
'server1.foo.org'.
'server2.foo.org'.
```

### Launch the monitor

Finally, (on the machine with the hosts file)

```
make monitor            # Only once...
```
You can now have fun with erlang's shell
```
nodes().
Pid = global:whereis_name(agent).
monitor:ask_graph(Pid, "graph.dot").
monitor:ask_job(Pid, jobs, wait, [2000]).
receive ok -> ok end.
monitor:stop().
```
