/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)radix.h	7.1 (Berkeley) %G%
 */

/*
 * Radix search tree node layout.
 */

struct radix_node {
	struct	radix_mask *rn_mklist;	/* list of masks contained in subtree */
	struct	radix_node *rn_p;	/* parent */
	short	rn_b;			/* bit offset; -1-index(netmask) */
	char	rn_bmask;		/* node: mask for bit test*/
	u_char	rn_flags;		/* enumerated next */
#define RNF_NORMAL	1		/* leaf contains normal route */
#define RNF_ROOT	2		/* leaf is root leaf for tree */
#define RNF_ACTIVE	4		/* This node is alive (for rtfree) */
	union {
		struct {			/* leaf only data: */
			char 	*rn_Key;	/* object of search */
			char	*rn_Mask;	/* netmask, if present */
			struct	radix_node *rn_Dupedkey;
		} rn_leaf;
		struct {			/* node only data: */
			int	rn_Off;		/* where to start compare */
			struct	radix_node *rn_L;/* progeny */
			struct	radix_node *rn_R;/* progeny */
		}rn_node;
	}		rn_u;
#ifdef RN_DEBUG
	int rn_info;
	struct radix_node *rn_twin;
	struct radix_node *rn_ybro;
#endif
};

#define rn_dupedkey rn_u.rn_leaf.rn_Dupedkey
#define rn_key rn_u.rn_leaf.rn_Key
#define rn_mask rn_u.rn_leaf.rn_Mask
#define rn_off rn_u.rn_node.rn_Off
#define rn_l rn_u.rn_node.rn_L
#define rn_r rn_u.rn_node.rn_R
/*
 * Annotations to tree concerning potential routes applying to subtrees.
 */
struct radix_mask {
	short	rm_b;			/* bit offset; -1-index(netmask) */
	char	rm_unused;		/* cf. rn_bmask */
	u_char	rm_flags;		/* cf. rn_flags */
	struct	radix_mask *rm_mklist;	/* more masks to try */
	char	*rm_mask;		/* the mask */
	int	rm_refs;		/* # of references to this struct */
};

#ifndef KERNEL
char *malloc();
#define Bcmp(a, b, n) bcmp(((char *)(a)), ((char *)(b)), (n))
#define Malloc(p, t, n) (p = (t) malloc((unsigned int)(n)))
#define Bzero(p, n) bzero((char *)(p), (int)(n));
#define Free(p) free((char *)p);
#define min(a, b) ((a) < (b) ? (a) : (b))
#ifndef RTF_UP
/*
 * probably just testing here . . .
 */
struct rtentry {
	int	rt_unused;
};
#endif
#else
#define Bcmp(a, b, n) bcmp(((caddr_t)(a)), ((caddr_t)(b)), (n))
#define Malloc(p, t, n) (p = (t) malloc((n), M_RTABLE, M_DONTWAIT))
#define Bzero(p, n) bzero((caddr_t)(p), (int)(n));
#define Free(p) free((caddr_t)p);
#endif KERNEL

struct nrtentry {
	struct	radix_node nrt_nodes[2];
	struct	rtentry nrt_rt;
};

#define MAXKEYLEN 32

extern struct radix_node_head {
	struct	radix_node_head *rnh_next;
	struct	radix_node *rnh_treetop;
	int	rnh_af;
	struct	radix_node rnh_upper;
	struct	nrtentry rnh_nrt;
} *radix_node_head;

extern struct radix_mask *rn_mkfreelist;

#define MKGet(m) {\
	if (rn_mkfreelist) {\
		m = rn_mkfreelist; \
		rn_mkfreelist = (m)->rm_mklist; \
	} else \
		Malloc(m, struct radix_mask *, sizeof (*(m))); }\

#define MKFree(m) { (m)->rm_mklist = rn_mkfreelist; rn_mkfreelist = (m);}
