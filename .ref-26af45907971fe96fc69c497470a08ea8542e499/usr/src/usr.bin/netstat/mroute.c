/*
 * Copyright (c) 1989 Stephen Deering
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Stephen Deering of Stanford University.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mroute.c	7.2 (Berkeley) %G%
 */

/*
 * Print DVMRP multicast routing structures and statistics.
 *
 * MROUTING 1.0
 */

#include <sys/param.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <sys/protosw.h>

#include <netinet/in.h>
#include <netinet/igmp.h>
#define KERNEL 1
#include <netinet/ip_mroute.h>
#undef KERNEL

#include <stdio.h>
#include <stdlib.h>
#include "netstat.h"

void
mroutepr(mrpaddr, mrtaddr, vifaddr)
	u_long mrpaddr, mrtaddr, vifaddr;
{
	u_int mrtproto;
	struct mrt *mrttable[MRTHASHSIZ];
	struct vif viftable[MAXVIFS];
	register struct mrt *mrt;
	struct mrt smrt;
	register struct vif *v;
	register vifi_t vifi;
	register struct in_addr *grp;
	register int i, n;
	register int banner_printed;
	register int saved_nflag;

	if (mrpaddr == 0) {
		printf("ip_mrtproto: symbol not in namelist\n");
		return;
	}

	kread(mrpaddr, (char *)&mrtproto, sizeof(mrtproto));
	switch (mrtproto) {

	case 0:
		printf("no multicast routing compiled into this system\n");
		return;

	case IGMP_DVMRP:
		break;

	default:
		printf("multicast routing protocol %u, unknown\n", mrtproto);
		return;
	}

	if (mrtaddr == 0) {
		printf("mrttable: symbol not in namelist\n");
		return;
	}
	if (vifaddr == 0) {
		printf("viftable: symbol not in namelist\n");
		return;
	}

	saved_nflag = nflag;
	nflag = 1;

	kread(vifaddr, (char *)&viftable, sizeof(viftable));
	banner_printed = 0;
	for (vifi = 0, v = viftable; vifi < MAXVIFS; ++vifi, ++v) {
		if (v->v_lcl_addr.s_addr == 0)
			continue;

		if (!banner_printed) {
			printf("\nVirtual Interface Table\n%s%s",
			    " Vif   Threshold   Local-Address   ",
			    "Remote-Address   Groups\n");
			banner_printed = 1;
		}

		printf(" %2u       %3u      %-15.15s",
		    vifi, v->v_threshold, routename(v->v_lcl_addr.s_addr));
		printf(" %-15.15s\n", (v->v_flags & VIFF_TUNNEL) ?
		    routename(v->v_rmt_addr.s_addr) : "");

		n = v->v_lcl_grps_n;
		grp = (struct in_addr *)malloc(n * sizeof(*grp));
		if (grp == NULL) {
			printf("v_lcl_grps_n: malloc failed\n");
			return;
		}
		kread((u_long)v->v_lcl_grps, (caddr_t)grp, n * sizeof(*grp));
		for (i = 0; i < n; ++i)
			printf("%51s %-15.15s\n",
			    "", routename((grp++)->s_addr));
		free(grp);
	}
	if (!banner_printed)
		printf("\nVirtual Interface Table is empty\n");

	kread(mrtaddr, (char *)&mrttable, sizeof(mrttable));
	banner_printed = 0;
	for (i = 0; i < MRTHASHSIZ; ++i) {
		for (mrt = mrttable[i]; mrt != NULL; mrt = mrt->mrt_next) {
			if (!banner_printed) {
				printf("\nMulticast Routing Table\n%s",
				    " Hash  Origin-Subnet  In-Vif  Out-Vifs\n");
				banner_printed = 1;
			}

			kread((u_long)mrt, (char *)&smrt, sizeof(*mrt));
			mrt = &smrt;
			printf(" %3u   %-15.15s  %2u   ",
			    i, netname(mrt->mrt_origin.s_addr,
			    ntohl(mrt->mrt_originmask.s_addr)),
			    mrt->mrt_parent);
			for (vifi = 0; vifi < MAXVIFS; ++vifi)
				if (VIFM_ISSET(vifi, mrt->mrt_children))
					printf(" %u%c",
					    vifi,
					    VIFM_ISSET(vifi, mrt->mrt_leaves) ?
					    '*' : ' ');
			printf("\n");
		}
	}
	if (!banner_printed)
		printf("\nMulticast Routing Table is empty\n");

	printf("\n");
	nflag = saved_nflag;
}


void
mrt_stats(mrpaddr, mstaddr)
	u_long mrpaddr, mstaddr;
{
	u_int mrtproto;
	struct mrtstat mrtstat;

	if(mrpaddr == 0) {
		printf("ip_mrtproto: symbol not in namelist\n");
		return;
	}

	kread(mrpaddr, (char *)&mrtproto, sizeof(mrtproto));
	switch (mrtproto) {
	    case 0:
		printf("no multicast routing compiled into this system\n");
		return;

	    case IGMP_DVMRP:
		break;

	    default:
		printf("multicast routing protocol %u, unknown\n", mrtproto);
		return;
	}

	if (mstaddr == 0) {
		printf("mrtstat: symbol not in namelist\n");
		return;
	}

	kread(mstaddr, (char *)&mrtstat, sizeof(mrtstat));
	printf("multicast routing:\n");
	printf(" %10u multicast route lookup%s\n",
	  mrtstat.mrts_mrt_lookups, plural(mrtstat.mrts_mrt_lookups));
	printf(" %10u multicast route cache miss%s\n",
	  mrtstat.mrts_mrt_misses, plurales(mrtstat.mrts_mrt_misses));
	printf(" %10u group address lookup%s\n",
	  mrtstat.mrts_grp_lookups, plural(mrtstat.mrts_grp_lookups));
	printf(" %10u group address cache miss%s\n",
	  mrtstat.mrts_grp_misses, plurales(mrtstat.mrts_grp_misses));
	printf(" %10u datagram%s with no route for origin\n",
	  mrtstat.mrts_no_route, plural(mrtstat.mrts_no_route));
	printf(" %10u datagram%s with malformed tunnel options\n",
	  mrtstat.mrts_bad_tunnel, plural(mrtstat.mrts_bad_tunnel));
	printf(" %10u datagram%s with no room for tunnel options\n",
	  mrtstat.mrts_cant_tunnel, plural(mrtstat.mrts_cant_tunnel));
}
