/* rt_init.c */

/* EGP User Process, ISI 23-Jun-84 */

/* rt_init(), rt_readkernel(), rt_ifinit(), rt_dumbinit(fp),
 * rt_NRadvise_init()
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/mbuf.h>			/* route.h */

#include <netinet/in.h>
#include <netinet/in_systm.h>

#include <stdio.h>
#include <errno.h>

#define KERNEL 1		/* to ensure all <net/route.h> compiled */
#include <net/route.h>

/* definitions from routed */

#include "trace_egp.h"
#include "if.h"
#include "rt_table.h"

/* new definitions */

#include "defs.h"

#include <nlist.h>


/* rt_init() initializes exterior and interior routing tables.
 */

extern  struct  rthash	nethash[],
			rt_interior;

rt_init()
{
	register struct rthash *rh;


	for (rh = nethash; rh < &nethash[ROUTEHASHSIZ]; rh++)	/* interior */
		rh->rt_forw = rh->rt_back = (struct rt_entry *)rh;

	rt_interior.rt_forw = (struct rt_entry *)&rt_interior;	/* exterior */
	rt_interior.rt_back = (struct rt_entry *)&rt_interior;

}


/* rt_readkernel() initializes the EGP routing tables from the current kernel
 * routing tables through /dev/kmem.
 * This is necessary to ensure consistency when the EGP process is terminated
 * and restarted while the supporting host continues to run. This may be done
 * if configuration information needs to be changed.
 */

struct nlist nl[] = {
#define	N_RTHOST		0
	{ "_rthost" },
#define N_RTNET			1
	{ "_rtnet"},
	{ "" },
};


rt_readkernel()
{
	int oldinstall;

/* Defined in <net/route.h>
 *
 *	struct mbuf *rtnet[RTHASHSIZ];
 */

extern	int	install;	/* if TRUE install route in kernel */

/*  ..routed/table.h redefines some of the elements of 
 * struct rtentry defined in <net/route.h>. In order for the code from routed
 * to be able to be utilized part of the kernel structure has been redefined 
 * with a preceding k_.  
 */

	struct k_rtentry {
		u_long	k_rt_hash;		/* to speed lookups */
		struct	sockaddr k_rt_dst;	/* key */
		struct	sockaddr k_rt_gateway;	/* value */
		short	k_rt_flags;		/* up/down?, host/net */
	} *rt;

	int 	i,
		kmem;
	struct 	mbuf 	*next,
			m_buf;


	nlist("/vmunix", nl);
	if ((nl[N_RTHOST].n_value == 0)||(nl[N_RTNET].n_value == 0)) {
		printf("rt_readkernel: rthost and rtnet not in namelist\n");
		quit();
	}

	kmem = open("/dev/kmem", 0);
	if (kmem < 0) {
		p_error("rt_readkernel: /dev/kmem");
		quit();
	}

/* read table of net hash chains */

    if (lseek(kmem, (long)nl[N_RTNET].n_value, 0) == -1
	|| read(kmem, (char *)rtnet, sizeof (rtnet))
	 != sizeof (rtnet)) {
	    printf("rt_readkernel: error reading kmem\n");
	    quit();
    }

/* read all routing entries of each network hash chain */

	TRACE_RT("rt_readkernel: Initial indirect routes read from kernel");
	TRACE_RT(" (if any):\n");

	for(i=0; i<RTHASHSIZ; i++) {
					/* Read each route entry from net hash
					 * chain i and copy into EGP route 
					 * table, the chain is 
					 * terminated by a null pointer */ 

		for( next=rtnet[i]; next!=NULL; next=m_buf.m_next) {
			if (lseek(kmem, (long)next, 0) == -1
			    || read(kmem, (char *)&m_buf, 
					MMINOFF + sizeof(struct k_rtentry))
			    != MMINOFF + sizeof (struct k_rtentry)) {
				p_error("rt_readkernel: read");
				quit();
			}
			rt = mtod(&m_buf, struct k_rtentry *);

					/* ignore direct routes */
			if( !(rt->k_rt_flags & RTF_GATEWAY) ) continue;

					/* ignore non-internet addresses */
			if( rt->k_rt_gateway.sa_family != AF_INET ) continue;

					/* add route to table of exterior
					 * routes, if the route is in fact an
					 * interior route it will be deleted
					 * by rt_dumbinit() */

			oldinstall = install;
			install = FALSE;  /* don't install routes in kernel */
				/* metric unknown - set largest reachable */
			rt_add(EXTERIOR, &rt->k_rt_dst, &rt->k_rt_gateway,
				HOPCNT_INFINITY - 1, 0);
			install = oldinstall;

		}
	}
	close(kmem);
	return;
}



/* rt_ifinit() initializes the interior routing table with direct nets as
 * per the interface table.
 *
 * External variables:
 *	rt_interior - start interior routes linked list
 *	ifnet - start interface table
 *	install - if FALSE don't install routes in kernel
 */

rt_ifinit()
{
	register  struct 	interface *ifp;
	int oldinstall = install;

/* Add direct nets to interior route table as per interface list */

	install = FALSE;		/* routes for interfaces already
					 * installed by kernel */
	TRACE_RT("rt_ifinit: interior routes for direct interfaces:\n");
	for(ifp=ifnet; ifp!=NULL; ifp=ifp->int_next) {
		rt_add(INTERIOR, &ifp->int_addr, &ifp->int_addr, 0,
							RTS_INTERFACE);
	}
	install = oldinstall;
}				


/* rt_dumbinit() reads the initialization file EGPINITFILE to
 * initialize: 
 * - the interior route table with routes to known non-routing gateways on a
 * shared net, these routes are static and not updated;
 * - a default gateway prior to an EGP neighbor being acquired and polled;
 *
 * EGPINITFILE relevant format is:
 * net name gateway name metric value
 * defaultgateway name
 *
 * where name is either a symbolic name in /etc/hosts or /etc/networks or an
 * internet address in dot notation and value is the distance in gateway hops
 * from this gateway to the specified net.
 *
 * External variables:
 *	rt_interior - start interior routes linked list
 *	n_remote_nets - number remote nets in interior route table, set here
 *	install - if TRUE install routes in kernel
 *	s - socket for ioctl
 *	rt_default_status - default route status
 */


rt_dumbinit(fp)
	FILE *fp;
{
	char keyword[MAXHOSTNAMELENGTH+1];
	char netname[MAXHOSTNAMELENGTH+1];
	char gname[MAXHOSTNAMELENGTH+1];
	struct 	sockaddr_in	netaddr,
				gateway,
				defaultdst;
	struct	rt_entry *rt;
	int	metric,
		c,
		error = FALSE,
		old_install;


	bzero( (char *)&netaddr, sizeof(netaddr) );
	bzero( (char *)&gateway, sizeof(gateway) );
	bzero( (char *)&defaultdst, sizeof(defaultdst) );

	TRACE_RT("rt_dumbinit: non-routing gateway routes (if any):\n");

	rewind(fp);

	while( fscanf(fp, "%s", keyword) != EOF) { /* read first word of line
						   and compare to key words */
	    if( strcmp(keyword, "net") == 0) {
		if( fscanf(fp, "%s gateway %s metric %d", netname, gname, 
				&metric) != 3) error = TRUE;
		else if( !getnetorhostname("net", netname, &netaddr)) {
		    printf("rt_dumbinit: invalid net name or");
		    printf(" address %s\n", netname);
		    error = TRUE;
		}
		else if( !getnetorhostname("host", gname, &gateway)) {
		    printf("rt_dumbinit: invalid gateway name or");
		    printf(" address %s\n", gname);
		    error = TRUE;
		}

		else {		/* Initialize routings tables with
				 * non-routing gateway */

		    n_remote_nets++;
				/* first delete any old route in exterior 
				 * route table read from kernel.
				 */
		    old_install = install;
		    rt = rt_ext_lookup(&netaddr);
		    if( rt != NULL ) {
			if( equal(&rt->rt_router, &gateway))
				install = FALSE;	/* already in kernel*/
			rt_delete(rt);
		    }
#ifndef	notdef
		    /* don't install, we route through a different interface */
		    install = FALSE;	
#endif
				/* add to interior routing table */
		    rt_add(INTERIOR, &netaddr, &gateway, metric, RTS_PASSIVE);
		    install = old_install;
		}

	    } /* end "net" entry */

	    else if( strcmp(keyword, "defaultgateway") == 0) {
	    	if( fscanf(fp, "%s", gname) != 1) error = TRUE;
		else if( !getnetorhostname("host", gname, &gateway)) {
			    printf("rt_dumbinit: invalid gateway name or");
			    printf(" address %s\n", gname);
			    error = TRUE;
		}

		else {		/* Initialize default gateway */

				/* check if a kernel entry exists for default
				 * route, if not add new default else change 
				 * to new default */
		    defaultdst.sin_family = AF_INET;
		    defaultdst.sin_addr.s_addr = DEFAULTNET;
		    metric = HOPCNT_INFINITY - 1;
		    rt = rt_ext_lookup(&defaultdst);
		    if( rt == NULL )
			rt_add(EXTERIOR, &defaultdst, &gateway, metric,
								RTS_PASSIVE);
		    else {
			rt_change(rt, &gateway, metric);
			rt->rt_state |= RTS_PASSIVE;
		    }
		    rt_default_status = INSTALLED;
		}

	    }	/* end "defaultgateway" entry */

	    do c = fgetc(fp); while (c != '\n' && c != EOF);  /* next line */

	} /*end while*/

	if(error) {
	    printf("rt_dumbinit: %s: initialization error\n", EGPINITFILE);
	    quit();
	}
	TRACE_RT("rt_dumbinit: commence EGP route updates:\n\n");
}


/* rt_NRadvise_init() reads the initialization file EGPINITFILE to
 * initialize user specified nets to be advised in Network Reachability
 * messages. If any such nets are specified, only those specified are advised,
 * else the nets to be specified are computed in accord with normal stub 
 * gateway NR messages. Any such nets should be either direct nets or nets
 * reached via non-routing gateways reported in EGPINITFILE, if the net
 * is not one of these it is ignored.
 *
 * EGPINITFILE relevant format is:
 * egpnetsreachable name name .... name
 *
 * External variables:
 *	rt_interior - start interior routes linked list
 */

rt_NRadvise_init(fp)
	FILE *fp;
{
	char keyword[MAXHOSTNAMELENGTH+1];
	char netname[MAXHOSTNAMELENGTH+1];
	struct sockaddr_in netaddr;
	struct	rt_entry *rt;
	int	notadvise_set = FALSE,
		c,
		error = FALSE,
		morenets;

	bzero( (char *)&netaddr, sizeof(netaddr) );
	netaddr.sin_family = AF_INET;

	rewind(fp);
			/* read first word of line and compare to key words */
	while(fscanf(fp, "%s", keyword) != EOF) {
	    if( strcmp(keyword, "egpnetsreachable") == 0) {

				/* initially flag all interior routes as not
				 * to be advised in NR messages, this is only 
				 * done for first occurrence of 
				 * "egpnetsreachable" */
		if( !notadvise_set ) {
		    for(rt = rt_interior.rt_forw;
				 rt != (struct rt_entry *)&rt_interior;
							rt = rt->rt_forw)
			rt->rt_state |= RTS_NOTADVISENR;
		    notadvise_set = TRUE;
		}

/* read successive net strings into destination until end of line */

		morenets = TRUE;
		while ( (c = fgetc(fp)) != EOF && morenets) {
		    switch (c) {
			case '\n':
			    ungetc(c, fp);	/* end line read below */

			case '#':		/* start comment */
			    morenets = FALSE;
			    break;

			case ' ':
			case '\t':
			    break;		/* next character */

			default:
			    ungetc(c, fp);
			    fscanf(fp, "%s", netname);
			    if( !getnetorhostname( "net", netname, &netaddr)){
				printf("rt_NRadvise_init: invalid net name ");
				printf("or address %s\n", netname);
				error = TRUE;
			    }
			    else {
				/* search interior route table for net and
				 * set to be advised in NR messages */
				for(rt = rt_interior.rt_forw;
					rt != (struct rt_entry *)&rt_interior;
							rt = rt->rt_forw )
				    if( equal(&rt->rt_dst, &netaddr)) {
					rt->rt_state &= ~RTS_NOTADVISENR;
					printf("advise %s, ",
					    inet_ntoa(((struct sockaddr_in *)
						&rt->rt_dst)->sin_addr));
					printf("gateway %s\n",
					    inet_ntoa(((struct sockaddr_in *)
						&rt->rt_router)->sin_addr));
					break;
				    }
			    }
		    }			/* end switch c */
		}			/* end while morenets */
	    }				/* end if "egpnetsreachable" */

	    do c = fgetc(fp); while (c != '\n' && c != EOF);  /* next line */
    
	} 				/*end while*/

	if(error) {
	  printf("rt_NRadvise_init: %s: initialization error\n", EGPINITFILE);
	  quit();
	}

}
