/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tuba_table.h	7.5 (Berkeley) %G%
 */

struct tuba_cache {
	struct	radix_node tc_nodes[2];		/* convenient lookup */
	int	tc_refcnt;
	int	tc_time;			/* last looked up */
	int	tc_flags;
#define TCF_PERM	1
	int	tc_index;
	u_short	tc_sum_in;			/* for inbound cksum */
	u_short	tc_sum_out;			/* for outbound cksum */
	struct	iso_addr tc_addr;		/* so you can respond */
	char	tc_EID[7];			/* what counts for lookups */
};

#define ADDCARRY(x)  (x >= 65535 ? x -= 65535 : x)
#define REDUCE(a, b) { union { u_short s[2]; long l;} l_util; long x; \
	l_util.l = (b); x = l_util.s[0] + l_util.s[1]; ADDCARRY(x); a = x;}

#ifdef KERNEL
extern int	tuba_table_size;
extern struct	tuba_cache **tuba_table;
extern struct	radix_node_head *tuba_tree;
#endif
