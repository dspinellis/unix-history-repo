/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tuba_table.c	7.4 (Berkeley) %G%
 */
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/proc.h>
#include <sys/mbuf.h>
#include <sys/socket.h>
#include <sys/socketvar.h>
#include <sys/domain.h>
#include <sys/protosw.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <sys/kernel.h>

#include <net/if.h>
#include <net/af.h>
#include <net/radix.h>

#include <netiso/iso.h>
#include <netiso/tuba_table.h>

int	tuba_table_size;
struct	tuba_cache **tuba_table;
struct	radix_node_head *tuba_tree;
extern	int arpt_keep, arpt_prune;	/* use same values as arp cache */

void
tuba_timer()
{
	int s = splnet();
	int	i;
	register struct	tuba_cache *tc;
	long	timelimit = time.tv_sec - arpt_keep;

	timeout(tuba_timer, (caddr_t)0, arpt_prune * hz);
	for (i = tuba_table_size; i > 0; i--)
		if ((tc = tuba_table[i]) && (tc->tc_refcnt == 0) &&
		    (tc->tc_time < timelimit)) {
			tuba_table[i] = 0;
			rn_delete((caddr_t)&tc->tc_addr, (caddr_t)0,
					tuba_tree->rnh_treetop);
			free((caddr_t)tc, M_RTABLE);
		}
	splx(s);
}

tuba_table_init()
{
	rn_inithead((void **)&tuba_tree, 40);
	timeout(tuba_timer, (caddr_t)0, arpt_prune * hz);
}

int
tuba_lookup(isoa, wait)
	register struct iso_addr *isoa;
{
	struct radix_node *rn, *rn_match();
	register struct tuba_cache *tc;
	struct tuba_cache **new;
	int dupentry = 0, sum_a = 0, sum_b = 0, old_size, i;
	char EID[7];

	if (isoa->isoa_len < 7)
		return (0);
	bcopy(isoa->isoa_genaddr + isoa->isoa_len - 7, EID + 1, EID[0] = 6);
	if ((rn = rn_match((caddr_t)EID, tuba_tree->rnh_treetop)) &&
	    ((rn->rn_flags & RNF_ROOT) == 0)) {
		tc = (struct tuba_cache *)rn;
		tc->tc_time = time.tv_sec;
		return (tc->tc_index);
	}
	if ((tc = (struct tuba_cache *)malloc(sizeof(*tc), M_RTABLE, wait))
		== NULL)
		return (0);
	bzero((caddr_t)tc, sizeof (*tc));
	bcopy((caddr_t)EID, (caddr_t)&tc->tc_addr, 1 + EID[0]);
	rn_insert((caddr_t)&tc->tc_addr, tuba_tree->rnh_treetop,
		&dupentry, tc->tc_nodes);
	if (dupentry)
		panic("tuba_lookup 1");
	tc->tc_time = time.tv_sec;
	for (i = EID[0]; i > 0; ) {
		(i & 1 ? sum_a : sum_b) += EID[i];
		i--;
	}
	REDUCE(tc->tc_sum_in, (sum_a << 8) + sum_b);
	HTONS(tc->tc_sum_in);
	for (i = tuba_table_size; i > 0; i--)
		if (tuba_table[i] == 0)
			break;
	if (i) {
		tc->tc_index = i;
		REDUCE(tc->tc_sum_out, tc->tc_sum_in + (0xffff ^ tc->tc_index));
		tuba_table[i] = tc;
		return (i);
	}
	old_size = tuba_table_size;
	if (tuba_table_size == 0)
		tuba_table_size = 15;
	if (tuba_table_size > 0x7fff)
		return (0);
	tuba_table_size = 1 + 2 * tuba_table_size;
	i = (tuba_table_size + 1) * sizeof(tc);
	new = (struct tuba_cache **)malloc((unsigned)i, M_RTABLE, wait);
	if (new == 0) {
		tuba_table_size = old_size;
		rn_delete((caddr_t)&tc->tc_addr, (caddr_t)0, tuba_tree);
		free((caddr_t)tc, M_RTABLE);
		return (0);
	}
	bzero((caddr_t)new, (unsigned)i);
	if (tuba_table) {
		bcopy((caddr_t)tuba_table, (caddr_t)new, i >> 1);
		free((caddr_t)tuba_table, M_RTABLE);
	}
	tuba_table = new;
	tuba_table[tc->tc_index = tuba_table_size] = tc;
	REDUCE(tc->tc_sum_out, tc->tc_sum_in + (0xffff ^ tc->tc_index));
	return (tc->tc_index);
}
