/*	rt_table.c
 */

/* EGP User Process, ISI 23-Jun-84 */

/*
 * Routing table management routines.
 * Modified from Routing Table Management Daemon tables.c
 */

#include "include.h"

extern	int	install;		/* if 1 call kernel, it is set by
					 * main() after kernel routes 
					 * initially read and read in table2.c
					 */
/*
 * Lookup dst in the exterior route tables for an exact match.
 */
struct rt_entry *
rt_ext_lookup(dst)
	struct sockaddr_in *dst;
{
	register struct rt_entry *rt;
	register struct rthash *rh;
	register unsigned hash;

	if (dst->sin_family != AF_INET)
		return (0);
	hash = inet_netof(dst->sin_addr);
	rh = &nethash[hash % ROUTEHASHSIZ];

	for (rt = rh->rt_forw; rt != (struct rt_entry *)rh; rt = rt->rt_forw){
		if (rt->rt_hash != hash)
			continue;
		if (equal(&rt->rt_dst, dst))
			return (rt);
	}
	return (0);
}


/* rt_int_lookup() looks for exact destination match in internal routing table
 */

extern struct rthash rt_interior;

struct rt_entry *
rt_int_lookup( dst)
	struct  sockaddr *dst;
{
	struct rt_entry *rt;

	for( rt = rt_interior.rt_forw; rt != (struct rt_entry *)&rt_interior;
							rt = rt->rt_forw)
		if( equal( &rt->rt_dst, dst))
			return(rt);

	return(NULL);
}


/* Add a route to either the interior or exterior routing tables
 */

rt_add( table, dst, gate, metric, state)
	int table;			/* interior or exterior table */
	struct sockaddr *dst, *gate;
	int metric, state;
{
	register struct rt_entry *rt;
	struct rthash *rh;
	unsigned hash;

	if (dst->sa_family != AF_INET)
		return;
	hash = inet_netof( in_addr_ofs( dst));
	if( table == EXTERIOR)
		rh = &nethash[hash % ROUTEHASHSIZ];	/* start ext. list */
	else
		rh = (struct rthash *)rt_interior.rt_back; /* end int. list */

	rt = (struct rt_entry *)malloc( sizeof( *rt));
	if (rt == 0) {
		printf(" rt_add: malloc: out of memory\n");
		return;
	}
	rt->rt_hash = hash;
	rt->rt_dst = *dst;
				/* set local part of dest addr zero for nets*/
	if( inet_lnaof( in_addr_ofs(&rt->rt_dst)) != 0)
	    in_addr_ofs( &rt->rt_dst)
	    = inet_makeaddr( inet_netof( in_addr_ofs(&rt->rt_dst)),
								INADDR_ANY);
	rt->rt_router = *gate;
	rt->rt_metric = metric;
	rt->rt_timer = 0;
	rt->rt_flags = RTF_UP;
	rt->rt_state = state | RTS_CHANGED;
	rt->rt_ifp = if_withnet(&rt->rt_router);
	if( table == EXTERIOR) {
		rt->rt_flags |= RTF_GATEWAY;	/* for exterior routing table
						all routes are gateways, with
						EGP we dont necessarily know
						the meaning of the metric */
		TRACE_ACTION(ADD EXT, rt)
	}
	else {
		if (metric)			/* interior routing uses hop 
						count as metric => interfaces
						have zero metric */
			rt->rt_flags |= RTF_GATEWAY;
		TRACE_ACTION(ADD INT, rt)
	}
	insque(rt, rh);
	/*
	 * If the ioctl fails because the gateway is unreachable
	 * from this host, discard the entry.  This should only
	 * occur because of an incorrect entry in /etc/egp_gateways.
	 */
	if (install && !(rt->rt_state & RTS_NOTINSTALL)) {
		if( ioctl(s, SIOCADDRT, (char *)&rt->rt_rt) < 0) {
			p_error("rt_add: SIOCADDRT");
			if (errno == ENETUNREACH) {
				TRACE_ACTION(DELETE, rt);
				remque(rt);
				free((char *)rt);
			}
		}
	}
}


/* change a route &/or note that update received.
 * returns 1 if change made
 */

rt_change(rt, gate, metric)
	struct rt_entry *rt;
	struct sockaddr *gate;
	short metric;
{
	int doioctl = 0, metricchanged = 0;
	struct rtentry oldroute;

	rt->rt_state |= RTS_CHANGED;		/* ensures route age reset */

	if (!equal(&rt->rt_router, gate))
		doioctl++;
	if (metric != rt->rt_metric) {
		metricchanged++;
		rt->rt_metric = metric;
	}
	if (doioctl) {
		oldroute = rt->rt_rt;
		rt->rt_router = *gate;
		if (install && !(rt->rt_state & RTS_NOTINSTALL) ) {
			if (ioctl(s, SIOCADDRT, (char *)&rt->rt_rt) < 0)
				p_error("rt_change: SIOCADDRT");
			if (ioctl(s, SIOCDELRT, (char *)&oldroute) < 0)
				p_error("rt_change: SIOCDELRT");
		}
	}
	if (doioctl || metricchanged) {
		TRACE_ACTION(CHANGE, rt);
		return(1);
	}

	return(0);
}

/* delete route from exterior routing table.
 */
rt_delete(rt)
	struct rt_entry *rt;
{

	TRACE_ACTION(DELETE EXT, rt);
	if (install && !(rt->rt_state & RTS_NOTINSTALL)
		    && ioctl(s, SIOCDELRT, (char *)&rt->rt_rt) < 0)
		p_error("rt_delete: SIOCDELRT");
	remque(rt);
	free((char *)rt);
}


/* rt_ifupdate() sets the status (up/down) of all routes in the interior 
 * routing table according to the status of the  associated output interface.
 */

rt_ifupdate() {
	register struct rt_entry *rt;

	for( rt= rt_interior.rt_forw; rt != (struct rt_entry *)&rt_interior;
							rt = rt->rt_forw) {
		if( rt->rt_ifp->int_flags & IFF_UP)
			rt->rt_flags |=	RTF_UP;
		else
			rt->rt_flags &= ~RTF_UP;
	}
}


/* rt_default() adds or deletes default route in kernel.
 * Called by egpstngh() to delete default after first egp neighbor acquired
 * and by quit() to add default route before terminating.
 */

extern int rt_default_status;	/* default route status */
extern int install;		/* if TRUE install routes in kernel */


rt_default(cmd)
	char *cmd;
{
	struct rt_entry *rt;
	struct sockaddr_in	defaultdst;

	bzero( (char *)&defaultdst, sizeof( defaultdst));
	defaultdst.sin_family = AF_INET;
	defaultdst.sin_addr.s_addr = DEFAULTNET;

	rt = rt_ext_lookup( &defaultdst);
	if(rt == NULL) {
		printf( "rt_default: no default route\n");
	}
	else if( strcmp( cmd, "DELETE") == 0) {
		TRACE_ACTION(DELETE DEFAULT, rt);
		rt_default_status = NOTINSTALLED;
		if (install)
			if (ioctl(s, SIOCDELRT, (char *)&rt->rt_rt) < 0)
				p_error("rt_default: SIOCDELRT");
	}
	else if( strcmp( cmd, "ADD") == 0) {
		TRACE_ACTION(ADD DEFAULT, rt);
		rt_default_status = INSTALLED;
		if (install)
			if (ioctl(s, SIOCADDRT, (char *)&rt->rt_rt) < 0)
				p_error("rt_default: SIOCADDRT");
	}
	return;
}


/* rt_time() increments the age of all routes in the exterior routing table
 * If any routes are older than rt_maxage (set in egpstime() ) the routes are
 * deleted.
 */

rt_time()
{
	struct rthash	*rh;
	struct rt_entry	*rt,
			*old_rt;
		
	static	int	last_time;
	
		int	time,
			old_routes = FALSE;
		char	*strtime;	

	getod(&time);

	for( rh = nethash; rh < &nethash[ROUTEHASHSIZ]; rh++) {

	    for( rt = rh->rt_forw; rt != (struct rt_entry *)rh;
							rt = rt->rt_forw) {
		if( rt->rt_state & RTS_CHANGED) {	/* recently updated */
			rt->rt_state &= ~RTS_CHANGED;
			rt->rt_timer = 0;
		}
		else if( !(rt->rt_state & RTS_PASSIVE) ) /* dont age passive
							 routes e.g. default*/
			rt->rt_timer += time - last_time;

		if(rt->rt_timer >= rt_maxage) {		/* route too old */
			old_routes = TRUE;
			old_rt = rt;
			rt = rt->rt_back;
			TRACE_RT("OLD: ");
			rt_delete(old_rt);
		}
	    }
	}
					/* if no acq. egp neigh. and old
					routes delete, reinstall default
					route */
	if( old_routes) {
		if( n_acquired == 0 && rt_default_status == NOTINSTALLED ) {
		    TRACE_RT("rt_time: no acquired EGP neighbors\n");
		    rt_default("ADD");
		}
		if( tracing & TR_RT) {
		    strtime = ctime(&time);
		    printf("rt_time: above old routes deleted %s\n", strtime);
		}
	}

	last_time = time;
	return;
}


/* rt_unreach() deletes all routes for a specified gateway */

rt_unreach( gateway)
	u_long	gateway;		/* internet address of gateway */
{
	struct rthash	*rh;
	struct rt_entry	*rt,
			*unreach_rt;

	for( rh = nethash; rh < &nethash[ROUTEHASHSIZ]; rh++) {

	    for( rt = rh->rt_forw; rt != (struct rt_entry *)rh;
							rt = rt->rt_forw) {

		if( sock_inaddr(&rt->rt_router) == gateway
		    && !(rt->rt_state & RTS_PASSIVE) ) {  /* e.g. don't delete
							     default route */
			unreach_rt = rt;
			rt = rt->rt_back;
			TRACE_RT("GW UNREACH: ");
			rt_delete(unreach_rt);
		}
	    }
	}
	return;
}


/* change the routing tables in response to an ICMP redirect message */

rt_redirect(dst, gateway)
	struct sockaddr	*dst,
			*gateway;
{
	int	saveinstall;
	struct	rt_entry  *rt;

					/* check gateway directly reachable */
	if( if_withnet( gateway) == NULL)
		return;			/* bad ICMP redirect gateway addr */

	saveinstall = install;
	install = FALSE;		/* dont update kernel routing tables
					as redirect has already done so */

					/* interior routing table is for
					dumb gateways internal to my aut. sys.
					hence I should never get redirects
					for these nets */
	if( rt_int_lookup( dst) != NULL) {
		TRACE_EXT("rt_redirect: dest. %s in internal table of",
				inet_ntoa( sock_inaddr( dst)));
		TRACE_EXT(" dumb routes => init error?\n");
	}
	TRACE_RT("REDIRECT:");
	if( rt = rt_ext_lookup( dst) )
		rt_change( rt, gateway, rt->rt_metric);
	else
		rt_add( EXTERIOR, dst, gateway, HOPCNT_INFINITY - 1, 0);

	install = saveinstall;

	return;
}
	
