# M1 Project : Distributed Systems

## Deployment

### Using python script

Fill the hosts file (```bin/.hosts.erlang```) with the name of the servers you want to use as nodes. Make sure that you can access those nodes using ssh without password (e.g. public key authorized). Then run the python script ```slsu.py```.

### Create agents

On each machine, run ```make agent```.

You can rename the node using for example ```make NAME=node@192.168.0.1```.

If you want to run two independant instances of the middleware at the same time, you can change the cookie of the erlang nodes with ```make COOKIE=passphrase```. To nodes will be able to connect to one other if and only if their cookies are identical.

### Create hosts file

Update the hosts file (```bin/.hosts.erlang```) using to the following syntax.
```
'192.168.0.1'.
'192.168.0.2'.
```
One possible error is that the agents you deployed don't answer to request if their name is the default one or because of the DNS server. If it happens, please use IP adresses and rename the agents.

### Launch the monitor

Finally you can merge the agents you just deployed in a coherent topology. The hosts file must be on the computer on which you we run ```make monitor```.

You can now have fun with erlang's shell
```
nodes().
Pid = global:whereis_name(agent).
monitor:ask_graph(Pid, "graph.dot").
monitor:ask_job(Pid, jobs, wait, [2000]).
receive ok -> ok end.
monitor:stop().
```
