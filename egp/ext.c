
/* ext.c */

/* EGP User Process, ISI 23-Jun-84 */

/* external variable definitions */

#include "include.h"

/* int impsock;			/* IMP socket handle */


int tracing;		/* log errors, route changes &/or packets */

int n_interfaces;		/* # internet interfaces */
int n_remote_nets;		/* # remote nets via internal non-routing
				   gateways */
struct rthash rt_interior;	/* routes interior to my autonomous system */
struct rthash nethash[ROUTEHASHSIZ];	/* exterior routes advised by EGP 
						neighbor - see rt_table.h */
struct interface *ifnet;	/* direct internet interface list */
int terminate;			/* terminate EGP process - set by
				   egpallcease(); tested by egpstunacq() and 
				   egpacq() */
int	s;			/* socket for ioctl calls installing routes,
				   set in main() */
int	install;		/* if TRUE install route in kernel,
				   it is set by main() after kernel routes
				   initially read and tested in rt_table.c */
int	rt_default_status;	/* NULL (no default) | INSTALLED (install 
				   needs to be true also) | NOTINSTALLED */
u_short	mysystem;		/* autonomous system number */
int	nneigh;			/* number of trusted egp neighbors */
int	maxacq;			/* maximum number neighbors to be acquired */
int	n_acquired;		/* number neighbors acquired */
int	egpsleep;		/* No. seconds between egpjob() wakeups,
				   recomputed when neighbor acquired or ceased
				 */
struct	egpngh	*egpngh;	/* start egp neighbor state table linked list
				 */
u_short	egprid_h;		/* sequence number of received egp packet
				   in host byte order - all ids in internal
				   tables are in host byte order */

int	rt_maxage;		/* maximum allowed age of any route since last
				   updated by an NR message */
int	maxpollint;		/* maximum poll interval of acquired neighbors
				   set in egpstime(), used in rt_NRupdate() */
