.SH
Resource Location and Authentication
.PP
At this time, 
.UX
lacks good network authentication and resource location.
The only example of a real name server in widespread use is the
internet name server.
As 
.UX
moves toward a distributed systems environment,
questions of distributed resource location become important.
X at this time does little to solve this problem,
relying on either command line arguments or an environment variable to
specify the host and display you want the application to use.
In reality, it should be closely tied to the user's name, since
the name of a machine is basically irrelevant as users often move.
X seems to highlight some issues in the future design of such servers
that may not be widely appreciated.
.PP
The model used to best describe distributed computing goes under the
name of the ``client/server'' model.
That is, a client program connects to a ``server'' which provides a service
somewhere in the network.
The additional twist is that the window system is a ``server'' in this
model, and other network services may become ``clients'' of the X server.
For example,
one can envision using services that want to interact with the user's display.
The result is that the ``name'' 
of the X server must somehow propagate
through such service requests, along with whatever authentication information
may be required to connect the X server in the future.
This ``cascaded'' services problem has not been well explored.
.PP
The access control currently in X requires no authentication, but is
only adequate for workstations, and fails badly in an environment
which includes timesharing systems.
X can be told to only accept connections from a list of machines.
Unfortunately, if any of them are timesharing machines,
and you allow access from
that machine, then anyone on that machine may manipulate your display
arbitrarily.
This has the unfortunate side effect of making it trivial to
write password grabbers (across the net!) or otherwise disturb the
display if access is left open.
.PP
The ``name'' of the user's
display server also comes and goes with some frequency,
as each time you log out, any previously authenticated connection
information needs to be invalidated, so no background process from a previous
user will disturb the user's display.
It is also not uncommon that a single user may  use multiple displays,
possibly on multiple machines simultaneously.
This might be common, for example, in a laboratory environment.
Interesting questions arise as to which display to use on what machine.
(For example, the user may initiate a request on a black and white display
that really works better on a color display; which display on what machine
should be used?)
We do not believe these issues, 
in particular the transient and cascading nature of such display services and
authentication information,
have been properly taken into account in the design of resource location
and authentication
servers.
