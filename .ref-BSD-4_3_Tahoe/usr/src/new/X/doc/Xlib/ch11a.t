.NH
Other X Server Functions
.XS
Other X Server Functions
.XE
.NH 2
Access Control
.PP
.IN "Access Control"
.IN "Internet Addresses"
.IN "DECnet Addresses"
.IN "Authentication"
X does not provide any protection on a per-window basis.
If you find out the resource id of a resource, you can manipulate it.
To provide some minimal level of protection, however,
connections are only permitted from machines you trust.
This is adequate on single user workstations, but obviously
breaks down on timesharing machines.
In the long run, it is expected that X will use a real network
authentication server, so we have only minimal facilities for the
time being.
Both DECnet and TCP domains are possible to use.
.PP
.IN "Default Protection"
On a VAX, the initial set of hosts allowed to open connections consists
.IN "File" "/etc/X?.hosts"
of:
.sp
   \(bu the host the window system is running on
   \(bu each host listed in \fI/etc/X?.hosts\fP, where `?' is the number of the
display.
.sp
This file should consist of host names separated by newlines.
DECnet nodes must terminate in ``::'' to distinguish them from internet hosts.
.FD
.IN "Definitions" "XAddHost"
.IN "XAddHost"
.IN "File" "<sys/socket.h>"
#include <sys/socket.h>
XAddHost (host)
	struct in_addr *host;	/* network address */
.FN
Add the specified host to the list of hosts allowed to open connections
to this display.
The display hardware must be on the same host as the program issuing the
command.
.FD
.IN "Definitions" "XAddNode"
.IN "XAddNode"
.IN "File" "<netdnet/dn.h>"
#include <netdnet/dn.h>
XAddNode (node)
	struct dn_naddr *node;	/* network address */
.FN
Add the specified DECnet node to the list of nodes allowed to open connections
to this display.
The display hardware must be on the same host as the program issuing the
command.
.FD
.IN "Definitions" "XRemoveHost"
.IN "XRemoveHost"
.IN "File" "<sys/socket.h>"
#include <sys/socket.h>
XRemoveHost (host)
	struct in_addr *host;	/* network address */
.FN
Remove the specified host from the list of hosts allowed to open
connections to the display.  
The display hardware must be on the same host as the
client process.
If you remove your machine from the access list, no new connections
can be made.
There is no way back from this call short of logout!
.FD
.IN "Definitions" "XRemoveNode"
.IN "XRemoveNode"
.IN "File" "<netdnet/dn.h>"
#include <netdnet/dn.h>
XRemoveNode (node)
	struct dn_naddr *node;	/* network address */
.FN
Remove the specified DECnet node from the list of nodes allowed to open
connections to the display.  
The display hardware must be on the same node as the
client process.
If you remove your machine from the access list, no new connections
can be made.
There is no way back from this call short of logout!
.FD
.IN "Definitions" "XGetHosts"
.IN "XGetHosts"
.IN "File" "<sys/socket.h>"
#include <sys/socket.h>
struct in_addr *XGetHosts (nhosts)
	int *nhosts;	/* RETURN */
.FN
Returns the current list of hosts allowed to open connections.
This allows a program to find out what machines can make connections.
Space is allocated and function returns a pointer to an array of hosts in the list.
The number of hosts are returned in the \fInhosts\fP argument.
This memory should be freed when no longer in use.
.FD
.IN "Definitions" "XGetNodes"
.IN "XGetNodes"
.IN "File" "<netdnet/dn.h>"
#include <netdnet/dn.h>
struct dn_naddr *XGetNodes (nnodes)
	int *nnodes;	/* RETURN */
.FN
Returns the current list of DECnet nodes allowed to open connections.
This allows a program to find out what machines can make connections.
Space is allocated and function returns a pointer to an array of hosts in the list.
The number of nodes are returned in the \fInodes\fP argument.
This memory should be freed when no longer in use.
