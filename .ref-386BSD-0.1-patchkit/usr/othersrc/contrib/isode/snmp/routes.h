/* routes.h - support for MIB support of the routing tables */

/* 
 * $Header: /f/osi/snmp/RCS/routes.h,v 7.2 91/02/22 09:43:53 mrose Interim $
 *
 * Contributed by NYSERNet Inc.  This work was partially supported by the
 * U.S. Defense Advanced Research Projects Agency and the Rome Air Development
 * Center of the U.S. Air Force Systems Command under contract number
 * F30602-88-C-0016.
 *
 *
 * $Log:	routes.h,v $
 * Revision 7.2  91/02/22  09:43:53  mrose
 * Interim 6.8
 * 
 * Revision 7.1  91/01/08  12:48:42  mrose
 * update
 * 
 * Revision 7.0  89/11/23  22:23:22  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#ifdef	BSD44
#include <sys/param.h>
#endif
#include <sys/mbuf.h>
#include <net/route.h>

/*  */

#define	METRIC_NONE	(-1)			/* ipRouteMetric[1234] */

#define	TYPE_OTHER	1			/* ipRouteType */
#define	TYPE_DIRECT	3
#define	TYPE_REMOTE	4

#define	PROTO_OTHER	1			/* ipRouteProto */
#define	PROTO_ICMP	4
#define	PROTO_ESIS	10

/*  */

struct rtetab {
#define	RT_SIZE		20			/* object instance */
    unsigned int   rt_instance[RT_SIZE + 1];
    int	    rt_insize;

    int	    rt_magic;				/* for multiple routes to the
						   same destination */

    struct rtentry rt_rt;			/* from kernel */

    union sockaddr_un rt_dst;			/* key */
    union sockaddr_un rt_gateway;		/* value */

    struct rtetab *rt_next;
};

extern struct rtetab *rts_inet;
#ifdef	BSD44
extern struct rtetab *rts_iso;
#endif


int	get_routes ();
struct rtetab *get_rtent ();
