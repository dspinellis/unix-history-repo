/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tuba_table.h	7.9 (Berkeley) %G%
 */

struct tuba_cache {
	struct	radix_node tc_nodes[2];		/* convenient lookup */
	int	tc_refcnt;
	int	tc_time;			/* last looked up */
	int	tc_flags;
#define TCF_PERM	1
	int	tc_index;
	u_short	tc_sum;				/* cksum of nsap inc. length */
	u_short	tc_ssum;			/* swab(tc_sum) */
	struct	sockaddr_iso tc_siso;		/* for responding */
};

#define ADDCARRY(x)  (x >= 65535 ? x -= 65535 : x)
#define REDUCE(a, b) { union { u_short s[2]; long l;} l_util; long x; \
	l_util.l = (b); x = l_util.s[0] + l_util.s[1]; ADDCARRY(x); \
	if (x == 0) x = 0xffff; a = x;}
#define SWAB(a, b) { union { u_char c[2]; u_short s;} s; u_char t; \
	s.s = (b); t = s.c[0]; s.c[0] = s.c[1]; s.c[1] = t; a = s.s;}

#ifdef KERNEL
extern	int	tuba_table_size;
extern	struct	tuba_cache **tuba_table;
extern	struct	radix_node_head *tuba_tree;
#endif
