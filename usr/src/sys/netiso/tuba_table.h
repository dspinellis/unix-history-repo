/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tuba_table.h	7.3 (Berkeley) %G%
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
	struct	iso_addr tc_addr;
};

#define ICKSUM(a, b) ((a = (b) % 0xffff), (a == 0 ? a = 0xffff : a))

#ifdef KERNEL
extern int	tuba_table_size;
extern struct	tuba_cache **tuba_table;
extern struct	radix_node_head *tuba_tree;
#endif
