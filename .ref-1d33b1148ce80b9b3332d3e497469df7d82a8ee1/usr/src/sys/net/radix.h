/*
 * Copyright (c) 1988, 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)radix.h	8.2 (Berkeley) %G%
 */

#ifndef _RADIX_H_
#define	_RADIX_H_

/*
 * Radix search tree node layout.
 */

struct radix_node {
	struct	radix_node *rn_mklist;	/* list of masks contained in subtree */
	struct	radix_node *rn_p;	/* parent */
	short	rn_b;			/* bit offset; -1-index(netmask) */
	char	rn_bmask;		/* node: mask for bit test*/
	u_char	rn_flags;		/* enumerated next */
#define RNF_NORMAL	1		/* leaf contains normal route */
#define RNF_ROOT	2		/* leaf is root leaf for tree */
#define RNF_ACTIVE	4		/* This node is alive (for rtfree) */
	union {
		struct {			/* leaf only data: */
			caddr_t	rn_Key;	/* object of search */
			caddr_t	rn_Mask;	/* netmask, if present */
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

struct radix_node_head {
	struct	radix_node *rnh_treetop;
	int	rnh_addrsize;		/* permit, but not require fixed keys */
	int	rnh_pktsize;		/* permit, but not require fixed keys */
	struct	radix_node *(*rnh_addaddr)	/* add based on sockaddr */
		__P((void *v, void *mask,
		     struct radix_node_head *head, struct radix_node nodes[]));
	struct	radix_node *(*rnh_addpkt)	/* add based on packet hdr */
		__P((void *v, void *mask,
		     struct radix_node_head *head, struct radix_node nodes[]));
	struct	radix_node *(*rnh_deladdr)	/* remove based on sockaddr */
		__P((void *v, void *mask, struct radix_node_head *head));
	struct	radix_node *(*rnh_delpkt)	/* remove based on packet hdr */
		__P((void *v, void *mask, struct radix_node_head *head));
	struct	radix_node *(*rnh_matchaddr)	/* locate based on sockaddr */
		__P((void *v, struct radix_node_head *head));
	struct	radix_node *(*rnh_lookup)	/* locate based on sockaddr */
		__P((void *v, void *mask, struct radix_node_head *head));
	struct	radix_node *(*rnh_lookup)	/* locate based on sockaddr */
		__P((void *v, void *mask, struct radix_node_head *head));
	struct	radix_node *(*rnh_matchpkt)	/* locate based on packet hdr */
		__P((void *v, struct radix_node_head *head));
	int	(*rnh_walktree)			/* traverse tree */
		__P((struct radix_node_head *head, int (*f)(), void *w));
/* mapping stuff */
	struct	radix_index_table  {
		short	limit;
		short	offset;
	}	*rnh_table;			/* how to re-order the bits */
	int	rnh_offset;			/* for martialed keys */
	int	(*rnh_bits_matched)		/* used in matching, insert */
		__P((void *trial, void *ref,
		     struct radix_node_head *head, int masklen));
	int	(*rnh_set_mask)			/* used in insertion */
		__P((int index,
		     struct radix_node *rn, struct radix_node_head *rnh));
/* the treetop itself */
	struct	radix_node rnh_nodes[3];	/* empty tree for common case */
};


#ifndef KERNEL
#define Bcmp(a, b, n) bcmp(((char *)(a)), ((char *)(b)), (n))
#define Bcopy(a, b, n) bcopy(((char *)(a)), ((char *)(b)), (unsigned)(n))
#define Bcopy(a, b, n) bcopy(((char *)(a)), ((char *)(b)), (unsigned)(n))
#define Bzero(p, n) bzero((char *)(p), (int)(n));
#define R_Malloc(p, t, n) (p = (t) malloc((unsigned int)(n)))
#define Free(p) free((char *)p);
#else
#define Bcmp(a, b, n) bcmp(((caddr_t)(a)), ((caddr_t)(b)), (unsigned)(n))
#define Bcopy(a, b, n) bcopy(((caddr_t)(a)), ((caddr_t)(b)), (unsigned)(n))
#define Bzero(p, n) bzero((caddr_t)(p), (unsigned)(n));
#define R_Malloc(p, t, n) (p = (t) malloc((unsigned long)(n), M_RTABLE, M_DONTWAIT))
#define Free(p) free((caddr_t)p, M_RTABLE);
#endif /*KERNEL*/
#endif /*KERNEL*/
#endif /*KERNEL*/

void	 rn_init __P((void));
int	 rn_inithead __P((void **, int));
int	 rn_refines __P((void *, void *));
int	 rn_walktree __P((struct radix_node_head *, int (*)(), void *));
struct radix_node
	 *rn_addmask __P((void *, int, int)),
	 *rn_addroute __P((void *, void *, struct radix_node_head *,
			struct radix_node [2])),
	 *rn_delete __P((void *, void *, struct radix_node_head *)),
	 *rn_insert __P((void *, struct radix_node_head *, int *,
			struct radix_node [2])),
	 *rn_match __P((void *, struct radix_node_head *)),
	 *rn_newpair __P((void *, int, struct radix_node[2])),
	 *rn_search __P((void *, struct radix_node *)),
	 *rn_search_unmapped __P((void *, struct radix_node_head *));

int	 rn_set_unmapped_mask
		__P((int, struct radix_node *, struct radix_node_head *)),
	 rn_set_mapped_mask
		__P((int, struct radix_node *, struct radix_node_head *)),
	 rn_mapped_bits_matched
		__P((void *, void *, struct radix_node_head *, int)),
	 rn_unmapped_bits_matched
		__P((void *, void *, struct radix_node_head *, int));
					

#endif /* _RADIX_H_ */
