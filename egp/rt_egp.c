/* @(#)rt_egp.c	1.4 2/22/89 */

/* EGP User Process, ISI 23-Jun-84 */

/* rt_NRnets(), rt_NRupdate() */

#include "include.h"


/* rt_NRnets() prepares the network part of the EGP Network
 * Reachability message with respect to the shared network of the EGP neighbor
 * This only includes the networks in the interior 
 * routing table (direct networks and remote networks of non-routing gateways)
 * that are allowed to be advised, as required for a stub gateway.
 * This routine
 * checks the status of routes and if down sets the distance as unreachable.
 * Returns the length of the EGP NR packet in octets or ERROR if an error
 * has occurred.
 */

extern	struct	rthash	rt_interior;	/* interior routing table */

rt_NRnets( nrpkt, sharednet_addr)
	struct  egpnr  *nrpkt;		/* start of NR message */
	u_long	sharednet_addr;		/* internet address of interface to
					   shared net of EGP neighbor */
{
	struct  rt_entry  *rt;
				/* temporary linked list for ordering nets */
	struct  net_order {
			struct net_order *next;
			u_long	net,			/* internet address */
				gateway;		/* internet address */
			int	distance;
	} *start_net,				/* start ordered net list */
	  *free_net;				/* next free memeory block */
reg	struct	net_order *net_pt,	/* new net to be ordered */
			  	  *this_net;	/* current search point */
	int	shared_net,
		n_bytes;
	u_long	current_gw;
reg	u_char	*nrp;			/* next octet of NR message */
	u_char	*n_distance,
		*distance,
		*n_nets;


	shared_net = inet_netof( sharednet_addr);

/* Reorder the interior routes as required for the NR message with respect to
 * the given shared net. Uses a temporary linked list terminated by NULL
 * pointer. The first element of the list is a dummy so insertions can be done
 * before the first true entry. The route status is checked and if down the
 * distance is set as unreachable.  The required order groups nets by gateway
 * and in order of increasing metric. This gateway is listed first (with all
 * nets not reached by gateways on the shared net) and then neighbor gateways
 * on the shared net, in any order. As there are few nets to be reported by a
 * stub gateway, each route is copied from the interior routing table and
 * inserted in the temporary reordered list using a linear search.
 */

					/* temporary linked list */
	start_net = (struct net_order *) malloc(
		(n_interfaces + n_remote_nets) * sizeof( struct net_order));
	if( start_net == NULL) {
		printf("rt_NRnets: malloc: out of memory\n");
		return(ERROR);
	}

	start_net->next = NULL;
	start_net->gateway = sharednet_addr;	/*ensures first gateway listed
						  is self */
	free_net = start_net + 1;	/* first element dummy to simplify 
					   insertion code */

/* check all routes of interior route table */

	for( rt = rt_interior.rt_forw; rt != (struct rt_entry *)&rt_interior;
					rt = rt->rt_forw ) {

	    if( inet_netof( sock_inaddr(&rt->rt_dst)) == shared_net)
		continue;			/* ignore shared net */
					/* ignore nets not to be advised */
	    if( rt->rt_state & RTS_NOTADVISENR ||
		(rt->rt_flags & RTF_UP) == 0) continue;

	    this_net = free_net++;
	    this_net->net = sock_inaddr( &rt->rt_dst);
					/* assign gw on shared net */
	    if( inet_netof( sock_inaddr( &rt->rt_router)) != shared_net)
		this_net->gateway = sharednet_addr;	/* gw is self */
	    else					/* gw is neighbor */
		this_net->gateway = sock_inaddr( &rt->rt_router);
						/* check net reachable */
	    this_net->distance = rt->rt_metric;

/* insert net in ordered list */

	    for(net_pt = start_net; ; net_pt = net_pt->next) {
		if( net_pt->next == NULL) break;	/* end list */
					/* next net reached by same gw */
		if( this_net->gateway == net_pt->next->gateway) {
			if( this_net->distance <= net_pt->next->distance)
			    break;
		}
					/* current net reached by same gw */
		else if( this_net->gateway == net_pt->gateway)
			break;

	    }
					/* insert this net after search net */
	    this_net->next = net_pt->next;
	    net_pt->next = this_net;
	}				/* end for all interior routes */

/* copy nets into NR message */

	nrpkt->en_igw = 0;		/* init # interior gateways */
	nrp = (u_char *)(nrpkt +1);	/* start nets part NR msg */
	current_gw = 0;			/* ensure first gateway addr copied */

					/* for each net */
	for(net_pt = start_net->next; net_pt != NULL; net_pt = net_pt->next) {

	    if( net_pt->gateway != current_gw) {	/* new gateway */
		current_gw = net_pt->gateway;

		if( in_isa( current_gw)) n_bytes = 3;
		else if( in_isb( current_gw)) n_bytes = 2;
		else n_bytes = 1;
		bcopy( (u_char *)&current_gw+4-n_bytes, nrp, n_bytes);
		nrp += n_bytes;

		nrpkt->en_igw++;
		n_distance = nrp++;
		*n_distance = 1;
		distance = nrp++;
		*distance = net_pt->distance;
		n_nets = nrp++;
		*n_nets = 1;
	    }
	    else if( net_pt->distance != *distance) {	/* new distance */
		(*n_distance)++;
		distance = nrp++;
		*distance = net_pt->distance;
		n_nets = nrp++;
		*n_nets = 1;
	    }
	    else (*n_nets)++;
							/* copy net addr */
	    if( in_isa( net_pt->net)) n_bytes = 1;
	    else if( in_isb( net_pt->net)) n_bytes = 2;
	    else n_bytes = 3;
	    bcopy(&net_pt->net, nrp, n_bytes);
	    nrp += n_bytes;
	}					/* end for each net */

	free( (char *)start_net);

	return( nrp - (u_char *)nrpkt);		/* length of NR message */

} /* end rt_NRnets */

	
/* rt_NRupdate() updates the exterior routing tables on receipt of an NR
 * message from an EGP neighbor. It first checks for valid NR counts before
 * updating the routing tables.
 * Returns 1 if error in NR message data, 0 otherwise.
*/

#define CLAA 1
#define CLAB 2
#define CLAC 3

extern int rt_maxage;			/* maximum age of any route without an
					   NR update */
extern int rt_default_status;		/* whether default installed in kernel
					 */

rt_NRupdate(ngp, nrp, egplen)
	struct egpngh  *ngp;			/* pointer to neighbor state
								      table */
	struct  egpnr  *nrp;			/* pointer to EGP NR packet */
	int		egplen;			/* length EGP NR packet */
{
	register  u_char  *nrb;
	struct sockaddr_in	destination,
				gateway;
	u_char	gw[4];				/* gateway internet address */
	int	gw_class,
		net_class,
		ng,
		nd,
		nn,
		n_gw,
		n_dist,
		n_net,
		dist,
		state,
		checkingNR = TRUE,
		change = FALSE;
	long	time;
	char	*strtime;
	struct rt_entry *rt;


	bzero( (char *)&destination, sizeof( destination));
	bzero( (char *)&gateway, sizeof( gateway));
	gateway.sin_family = AF_INET;
	destination.sin_family = AF_INET;

/* check class of shared net */

	*(u_long *)gw = nrp->en_net;		/* set net part of gateways */
	if( 	 in_isa( gw[0])) gw_class = CLAA;
	else if( in_isb( gw[0])) gw_class = CLAB;
	else /*if( in_isc( gw[0]))*/ gw_class = CLAC;
	/* else
		return(1);		/* NR message error */

	n_gw = nrp->en_igw + nrp->en_egw;

/* First check NR message for valid counts, then repeat and update routing
   tables */

repeat:	nrb = (u_char *)nrp + sizeof( struct egpnr);  /* start first gw */

	for( ng = 0; ng < n_gw; ng++) {		/* all gateways */

	    switch( gw_class) {		/* fill gateway local address */
		case CLAA:	gw[1] = *nrb++;
		case CLAB:	gw[2] = *nrb++;
		case CLAC:	gw[3] = *nrb++;
	    }
	    gateway.sin_addr.s_addr = *(u_long *)gw;
	    n_dist = *nrb++;

	    for( nd = 0; nd < n_dist; nd++) {	/* all distances this
								gateway */
		dist = *nrb++;
		n_net = *nrb++;

		for( nn = 0; nn < n_net; nn++) {  /* all nets this distance */

		    state = 0;
						/* copy destination net */
		    if( in_isa( *nrb))	    net_class = CLAA;
		    else if( in_isb( *nrb)) net_class = CLAB;
		    else {
			net_class = CLAC;
			if( !in_isc( *nrb))
			    state = RTS_NOTINSTALL;
		    }

		    destination.sin_addr.s_addr = 0;	/* zero unused bytes*/
		    bcopy( nrb, &destination.sin_addr.s_addr, net_class);
		    /* don't install bogus nets, but trace them */
		    if( *nrb == 0 || *nrb == 127)
			state = RTS_NOTINSTALL;
		    nrb += net_class;

		    if( checkingNR) {		/* first check counts only */
			if( nrb > (u_char *)nrp + egplen +1)
			    return(1);		/* erroneous counts in NR */
		    }

		    else {			/* update routing table */

						/* ignore myself */
			if( gateway.sin_addr.s_addr == ngp->ng_myaddr)
			    state = RTS_NOTINSTALL;
						/* check for internal route */
			if( (rt = rt_int_lookup( &destination)) != NULL) {
#ifdef notdef
			    if( gateway.sin_addr.s_addr
					== sock_inaddr( &rt->rt_router) )
				continue;	/*ignore same route */
			    else
#endif
				state = RTS_NOTINSTALL;
			}
						/* check for external route */
			rt = rt_ext_lookup( &destination);
			if( rt == NULL) {	/* new route */
			    rt_add(EXTERIOR, &destination, &gateway, dist,
								       state);
			    change = TRUE;
			}
			else {			/* existing route */

			    if( equal( &rt->rt_router, &gateway)){ /*same gw*/
				    if( rt_change( rt, &gateway, dist))
					change = TRUE;
			    }
			    else {		/* different gateway */
				if(    (dist < rt->rt_metric)
				    || (dist >= rt->rt_metric 
					&& rt->rt_timer > maxpollint
					&& !(rt->rt_state & RTS_CHANGED))
					) {
					rt_change( rt, &gateway, dist);
					change = TRUE;

/* note that this ensures that at least one NR message has been received
without the original route as the route timer is reset at the next age
increment after the NR message was received. To exceed maxpollint at least
one more route age increment (1 minute) must elapse, which gives time for
an NR poll retransmission */

				}
			    }
			}	/* end else existing route */
		    }		/* end else update routing table */
		}		/* end for all nets */
	    }			/* end for all distances */
	}			/* end for all gateways */

	if( checkingNR) {
	    if( nrb != (u_char *)nrp + egplen)
		return(1);			/* erroneous counts */
	    else
		checkingNR = FALSE;
		goto repeat;
	}

	if( rt_default_status == INSTALLED) {
	    rt_default("DELETE");
	    change = TRUE;
	}
	if( change && tracing & TR_RT) {
	    getod(&time);
	    strtime = ctime(&time);
	    printf("rt_NRupdate: above routes updated %s\n", strtime);
	}
	return(0);

}  /* end rt_NRupdate */
