/*
 * Copyright (c) 1988, 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)radix.c	8.5 (Berkeley) %G%
 */

/*
 * Routines to build and maintain radix trees for routing lookups.
 */
#ifndef RNF_NORMAL
#include <sys/param.h>
#ifdef	KERNEL
#include <sys/systm.h>
#include <sys/malloc.h>
#define	M_DONTWAIT M_NOWAIT
#ifdef	KERNEL
#include <sys/domain.h>
#else
#include <stdlib.h>
#endif
#include <sys/syslog.h>
#include <net/radix.h>
#endif

#include <net/radix.h>

int	max_keylen;
struct radix_node_head *mask_rnhead;
static char *rn_zeros, *rn_ones;

#define rn_masktop (mask_rnhead->rnh_treetop)
#undef Bcmp
#define Bcmp(a, b, l) (l == 0 ? 0 : bcmp((caddr_t)(a), (caddr_t)(b), (u_long)l))
/*
 * The data structure for the keys is a radix tree with one way
 * branching removed.  The index rn_b at an internal node n represents a bit
 * position to be tested.  The tree is arranged so that all descendants
 * of a node n have keys whose bits all agree up to position rn_b - 1.
 * (We say the index of n is rn_b.)
 *
 * There is at least one descendant which has a one bit at position rn_b,
 * and at least one with a zero there.
 *
 * A route is determined by a pair of key and mask.  We require that the
 * bit-wise logical and of the key and mask to be the key.
 * We define the index of a route to associated with the mask to be
 * the first bit number in the mask where 0 occurs (with bit number 0
 * representing the highest order bit).
 * 
 * We say a mask is normal if every bit is 0, past the index of the mask.
 * If a node n has a descendant (k, m) with index(m) == index(n) == rn_b,
 * and m is a normal mask, then the route applies to every descendant of n.
 * If the index(m) < rn_b, this implies the trailing last few bits of k
 * before bit b are all 0, (and hence consequently true of every descendant
 * of n), so the route applies to all descendants of the node as well.
 * This version of the code assumes that all masks are normal,
 * and consequently the only thing that needs to be stored about a mask
 * is its length.  This version also permits mapped, and fixed length
 * elements to be entered into the tree.
 */

struct radix_node *
rn_search(v_arg, top)
	void *v_arg;
	struct radix_node *top;
{
	register struct radix_node *x;
	register caddr_t v;

	for (x = top, v = v_arg; x->rn_b >= 0;) {
		if (x->rn_bmask & v[x->rn_off])
			x = x->rn_r;
		else
			x = x->rn_l;
	}
	return (x);
};

static char search_bits[] = {0x80, 0x40, 0x20, 0x10, 0x8, 0x4, 0x2, 0x1};

struct radix_node *
rn_search_unmapped(v_arg, head)
	void *v_arg;
	struct radix_node_head *head;
{
	register struct radix_node *x = head->rnh_treetop;
	register char *v;
	register int index;

	for (v = v_arg; x->rn_b >= 0;) {
		index = x->rn_b + head->rnh_offset;
		if (search_bits[index & 7] & v[index >> 3])
			x = x->rn_r;
		else
			x = x->rn_l;
	}
	return (x);
};


/*
 * This routine is used elsewhere in the kernel concerning
 * best matches for interfaces and is no longer used in the
 * radix code.  
 *
 */
int 
rn_refines(m_arg, n_arg)
	void *m_arg, *n_arg;
{
	register caddr_t m = m_arg, n = n_arg;
	register caddr_t lim, lim2 = lim = n + *(u_char *)n;
	int longer = (*(u_char *)n++) - (int)(*(u_char *)m++);
	int masks_are_equal = 1;

	if (longer > 0)
		lim -= longer;
	while (n < lim) {
		if (*n & ~(*m))
			return 0;
		if (*n++ != *m++)
			masks_are_equal = 0;
			
	}
	while (n < lim2)
		if (*n++)
			return 0;
	if (masks_are_equal && (longer < 0))
		for (lim2 = m - longer; m < lim2; )
			if (*m++)
				return 1;
	return (!masks_are_equal);
}
/* Begin bits.c */
static int low_bits[] = {1, 3, 7, 15, 31, 63, 127, 255};
static int mask_bits[] = {1, 2, 4, 8, 16, 32, 64, 128};

struct radix_node *
rn_lookup(v_arg, m_arg, head)
	void *v_arg, *m_arg;
	struct radix_node_head *head;
{
	register struct radix_node *x;
	caddr_t netmask = 0;
#define x1(a,n) ( a > ((1 << (n + 1)) - 1) ? n+1 : n)
#define x2(a,n) (( a > ((1 << (2 + n)) - 1)) ? x1(a,n+2) : x1(a,n))
#define x4(a,n) (( a > ((1 << (4 + n)) - 1)) ? x2(a,n+4) : x2(a,n))
#define x8(a,n) (( a > ((1 << (8 + n)) - 1)) ? x4(a,n+8) : x4(a,n))
#define x16(a,n) ((a > (((1 << n) - 1)+(65535 << n))) ? x8(a,n+16) : x8(a,n))

	if (m_arg) {
		if ((x = rn_addmask(m_arg, 1, head->rnh_treetop->rn_off)) == 0)
			return (0);
		netmask = x->rn_key;
	}
	x = rn_match(v_arg, head);
	if (x && netmask) {
		while (x && x->rn_mask != netmask)
			x = x->rn_dupedkey;
	}
	return x;
}

static int
rn_satsifies_leaf(trial, leaf, skip)
	char *trial;
	register struct radix_node *leaf;
	int skip;
{
	register char *cp = trial, *cp2 = leaf->rn_key, *cp3 = leaf->rn_mask;
	char *cplim;
	int length = min(*(u_char *)cp, *(u_char *)cp2);

	if (cp3 == 0)
		cp3 = rn_ones;
	else
		length = min(length, *(u_char *)cp3);
	cplim = cp + length; cp3 += skip; cp2 += skip;
	for (cp += skip; cp < cplim; cp++, cp2++, cp3++)
		if ((*cp ^ *cp2) & *cp3)
			return 0;
	return 1;
}

int
rn_mapped_bits_matched(t_a, r_a, rnh, masklen)
	void	*t_a;			/* Under scrutiny, assumed mapped */
	void	*r_a;			/* being compared to, not mapped */
	struct	radix_node_head *rnh;	/* has offset for ref, map for trial */
	int	masklen;		/* only need this many bits to match*/	
{
	register struct radix_index_table *table = rnh->rnh_table;
	int matched = 0, k, l;
	u_char	*trial = t_a;		/* Under scrutiny, assumed mapped */
	u_char	*ref = r_a;		/* being compared to, not mapped */

#ifdef utterly_straightforward_way_of_doing_this
	for (; table->limit; table++) {
		for (; matched < table->limit; matched++) {
			if (matched >= masklen - 1)
				return (matched);
			k = matched + table->offset;
			l = matched + rnh->rnh_offset;
			/* is bit l of ref == bit k of trial */
			if (((ref[l >> 3] >> (7 - (l & 7))) ^
			     (trial[k >> 3] >> (7 - (k & 7)))) & 1)
				return (matched);
		}
	}
#else
	int test_info, trial_bits, ref_bits, limit, sum_bits, delta;

	for (; table->limit; table++) {
		limit = MIN(masklen, table->limit);
		while (matched < limit) {
			k = matched + table->offset;
			l = matched + rnh->rnh_offset;
			trial_bits = 7 - (k & 7);
			ref_bits = 7 - (l & 7);
			delta = MIN(MIN(trial_bits, ref_bits), limit - matched);
			sum_bits = trial_bits + ref_bits;

			test_info = ((int)ref[k >> 3]) << ref_bits;
			test_info ^= ((int)trial[l >> 3]) << trial_bits;
			test_info &= low_bits[sum_bits];
			test_info &= ~low_bits[sum_bits - delta];
			if (test_info != 0) {
				int count, mask = mask_bits[sum_bits];
				for (count = delta; count >= 0; count--) {
					if (mask & test_info)
						return (matched);
					matched++; mask >>= 1;
				}
				printf("Bits_matched: should never get here!");
			}
			matched += delta;
			if (matched >= masklen)
				return (matched);
		}
	}
#endif
	return (matched);
}

#if defined(IPPROTO_IP) && defined(IPVERSION) /* #include <netinet/i{n,p}.h>" */
int ip_mapped_bits_matched
	(void *t_a, void *r_a, struct radix_node_head *rnh, int masklen)
{
	struct ip *trial = t_a;
	struct sockaddr_in *ref = r_a;

	u_long bits = trial->sin_addr.s_addr ^ ip->ip_dst.s_adddr;
	if (bits == 0) return (32); 	/* expected case !*/
	bits = ntohl(bits);
	bits =  x16(bits, 0);
	return bits;
}
#endif

rn_mapped_set_mask(index, rn, rnh)
	int index;
	register struct radix_node *rn;
	register struct radix_node_head *rnh;
{
	register struct radix_index_table *table;

	for (table = rnh->rnh_table; table->limit && index < table->limit;)
		table++;
	if (table->limit) {
		index += table->offset;
		rn->rn_bmask = mask_bits[7 - (index & 7)];
		rn->rn_off = (index >> 3);
	}
}

rn_unmapped_bits_matched(t_a, r_a, rnh, masklen)
	void	*t_a;			/* Under scrutiny, assumed mapped */
	void	*r_a;			/* being compared to, not mapped */
	struct	radix_node_head *rnh;	/* has offset for ref, map for trial */
	int	masklen;		/* only need this many bits to match*/	
{
	register u_char *cp1, *cp2, *limit;
	int offset = rnh->rnh_offset >> 3;/* XXX: off must be n * 8 */
	int matched, test_info;
	u_char	*trial = t_a;		/* Under scrutiny, assumed mapped */
	u_char	*ref = r_a;		/* being compared to, not mapped */

	cp1 = trial + offset;
	limit = cp1 + ((masklen + 7) >> 3);
	for (cp2 = ref + offset; cp1 < limit;)
		if (*cp1++ != *cp2++) {
			test_info = cp1[-1] ^ cp2[-1];
			matched = (cp1 - trial - offset) << 3;
			for (; test_info; matched--)
				test_info >>= 1;
			if (matched > masklen)
				matched = masklen;
			return (matched);
		}
	return (masklen);
}

rn_unmapped_set_mask(index, rn, rnh)
	int index;
	register struct radix_node *rn;
	register struct radix_node_head *rnh;
{
	index += rnh->rnh_offset;
	rn->rn_bmask = mask_bits[7 - (index & 7)];
	rn->rn_off = (index >> 3);
}
/* End bits.c */

struct radix_node *
rn_match(v_arg, head)
	void *v_arg;
	struct radix_node_head *head;
{
	register char *cp = (char *)(v_arg);
	register struct radix_node *m, *t = head->rnh_treetop;
	struct radix_node *saved_t, *top = t;
	int bits_matched, rn_b;

	/*
	 * Open code rn_search(v, top) to avoid overhead of extra
	 * subroutine call.
	 */
	while (t->rn_b >= 0)
		if (t->rn_bmask & cp[t->rn_off])
			t = t->rn_r;
		else
			t = t->rn_l;
	bits_matched = (*head->rnh_bits_matched)
				(v_arg, t->rn_key, head, -1 - t->rn_b);
	rn_b = -1 - bits_matched;	/* XXX: compatability */
	/*
	 * Check this node, and any other duped keys.
	 * We want to match the most specific possible mask, so
	 * duplicates are sorted longest mask to shortest.
	 */
	if (t->rn_mask)
		vlen = *(u_char *)t->rn_mask;
	for (saved_t = t; t; t = t->rn_dupedkey)
		/* if (bits_matched >= mask_index) */
		if (rn_b <= t->rn_b) {
			/*
			 * This extra grot is in case we are explicitly asked
			 * to look up the default.  Ugh!
			 */
			off = min(t->rn_off, matched_off);
			mstart = maskedKey + off;
			if ((t->rn_flags & RNF_ROOT) && t->rn_dupedkey)
				t = t->rn_dupedkey;
			return (t);
		}
	/* start searching up the tree */
	t = saved_t;
	do {
		t = t->rn_p;
		for (m = t->rn_mklist; m; m = m->rn_mklist)
			/* if (bits_matched >= mask_index) */
			if (rn_b <= m->rn_b)
				return (m);
	} while (t != top);
	return 0;
};
		
#ifdef RN_DEBUG
int	rn_nodenum;
struct	radix_node *rn_clist;
int	rn_saveinfo;
int	rn_debug =  1;
#endif

struct radix_node *
rn_newpair1(v, b, nodes, rnh)
	void *v;
	int b;
	struct radix_node nodes[2];
	struct radix_node_head *rnh;
{
	register struct radix_node *tt = nodes, *t = tt + 1;
	if (rnh == 0)
		panic("rn_newpair1");
	t->rn_b = b; t->rn_l = tt;
	(*rnh->rnh_set_mask)(b, t, rnh);
	tt->rn_b = -1; tt->rn_key = (caddr_t)v; tt->rn_p = t;
	tt->rn_flags = t->rn_flags = RNF_ACTIVE;
#ifdef RN_DEBUG
	tt->rn_info = rn_nodenum++; t->rn_info = rn_nodenum++;
	tt->rn_twin = t; tt->rn_ybro = rn_clist; rn_clist = tt;
#endif
	return t;
}

#define DEFAULT(a, b) (a > 0 ? a : b)
#define VLEN(v, h) ((DEFAULT(h->rnh_addrsize, *(u_char *)v) << 3) - h->rnh_offset)

struct radix_node *
rn_insert(v_arg, head, dupentry, nodes)
	void *v_arg;
	register struct radix_node_head *head;
	int *dupentry;
	struct radix_node nodes[2];
{
	caddr_t v = (caddr_t)v_arg, cp = (head->rnh_offset >> 3) + v;
	register struct radix_node *t = rn_search_unmapped(v, head);
	register struct radix_node *p, *x;
	int b, vlen = VLEN(v, head);
    	/*
	 * Find first bit at which v and t->rn_key differ
	 */
	b = rn_unmapped_bits_matched(v, t->rn_key, head, vlen);
	if (b == vlen) {
		*dupentry = 1;
		return t;
	}
	/*
	 * Find appropriate node after which to insert new key
	 */
	p = t;
	do {
		x = p;
		p = x->rn_p;
        } while (b <= p->rn_b);

#ifdef RN_DEBUG
	if (rn_debug)
		printf("Going In:\n"), traverse(p);
#endif
	t = rn_newpair1(v, b, nodes, head);
	/*
	 * If we went to the left during the matching process,
	 * the spliced-in node will still be on the left.
	 */
	if (p->rn_l == x)
		p->rn_l = t;
	else
		p->rn_r = t;
	t->rn_p = p;
	/*
	 * If the first bit of the input string that didn't match
	 * was set, put the new leaf to the right of the new node. 
	 */
	if (cp[b >> 3] & search_bits[b & 7]) {
		t->rn_r = nodes; t->rn_l = x;
	} else
		t->rn_r = x;
	x->rn_p = t;
#ifdef RN_DEBUG
	if (rn_debug)
		printf("Coming out:\n"), traverse(p);
#endif
	return (nodes);
}

/*
 * Here mostly for compatability's sake with
 * the previous networking code, which expects to find masks.
 */
struct radix_node *
rn_masksubr(n_arg, v_arg, skip, head, len_p)
	void *n_arg, *v_arg;
	int skip, *len_p;
	struct radix_node_head *head;
{
	caddr_t netmask = (caddr_t)n_arg, v = v_arg;
	register struct radix_node *x = rn_masktop;
	register caddr_t cp, cplim;
	register int b, mlen, j;
	int maskduplicated;
	struct radix_node *saved_x;

	if (head == 0)
		head = mask_rnhead;
	if (netmask == 0) {
		if (*len_p)
			*len_p = VLEN(v, head);
		return 0;
	}
	if (m0 < last_zeroed)
		Bzero(addmask_key + m0, last_zeroed - m0);
	*addmask_key = last_zeroed = mlen;
	x = rn_search(addmask_key, rn_masktop);
	if (Bcmp(addmask_key, x->rn_key, mlen) != 0)
		x = 0;
	if (x || search)
		return (x);
	if (skip > 0)
		for (j = skip << 3; j > ((unsigned)x->rn_b);)
			x = x->rn_r;
	x = rn_search(netmask, x);
	mlen = *(u_char *)netmask;
	if ((skip > mlen) ||
	    Bcmp(netmask + skip, x->rn_key + skip, mlen - skip) == 0)
		goto found;
	R_Malloc(x, struct radix_node *, max_keylen + 2 * sizeof (*x));
	if (x == 0)
		return (0);
	saved_x = x;
	Bzero(x, max_keylen + 2 * sizeof (*x));
	if (skip > 1) 
		Bcopy(rn_ones, cp + 1, skip - 1);
	maskduplicated = 0;
	x = rn_insert(cp, mask_rnhead, &maskduplicated, x);
	mlen = rn_unmapped_bits_matched(cp, rn_ones, head, mlen << 3);
	if (maskduplicated) {
		printf("rn_addmask1: impossible dup");
		Free(saved_x);
	} else {
		x->rn_b = -1 - mlen;
	}
	b += (cp - netmask) << 3;
found:
	if (len_p)
		*len_p = (-1 - x->rn_b) - head->rnh_offset;
	return (x);
}

static int	/* XXX: arbitrary ordering for non-contiguous masks */
rn_lexobetter(m_arg, n_arg)
	void *m_arg, *n_arg;
{
	register u_char *mp = m_arg, *np = n_arg, *lim;

	if (*mp > *np)
		return 1;  /* not really, but need to check longer one first */
	if (*mp == *np) 
		for (lim = mp + *mp; mp < lim;)
			if (*mp++ > *np++)
				return 1;
	return 0;
}

static struct radix_mask *
rn_new_radix_mask(tt, next)
	register struct radix_node *tt;
	register struct radix_mask *next;
{
	register struct radix_mask *m;

	MKGet(m);
	if (m == 0) {
		log(LOG_ERR, "Mask for route not entered\n");
		return (0);
	}
	Bzero(m, sizeof *m);
	m->rm_b = tt->rn_b;
	m->rm_flags = tt->rn_flags;
	if (tt->rn_flags & RNF_NORMAL)
		m->rm_leaf = tt;
	else
		m->rm_mask = tt->rn_mask;
	m->rm_mklist = next;
	tt->rn_mklist = m;
	return m;
}

struct radix_node *
rn_addroute(v_arg, n_arg, head, treenodes)
	void *v_arg, *n_arg;
	struct radix_node_head *head;
	struct radix_node treenodes[2];
{
	caddr_t v = (caddr_t)v_arg, netmask = (caddr_t)n_arg;
	register struct radix_node *m, *us = treenodes;
	struct radix_node *t, *tt, *x, *base, *top = head->rnh_treetop;
	struct radix_node *s /*sibling*/, *p /*parent*/, **mp;
	short b = 0, b_leaf = 0;
	int masklen, masklen_leaf, mlen, keyduplicated = 0;

	/*
	 * This version assumes contiguous masks and only cares about
	 * the index of the mask, if present.
	 */
	(void) rn_masksubr(v_arg, n_arg, (head->rnh_offset >> 3), head, &masklen);
	masklen_leaf = -1 - masklen;
	base = tt = rn_insert(v, head, &keyduplicated, treenodes);
	t = p = tt->rn_p;
	/*
	 * Deal with duplicated keys: attach node to previous instance
	 * We sort the nodes so that the longest mask comes first.
	 */
	if (keyduplicated) {
		/*
		 * Examine each node and continue past any where the
		 * mask length is greater than the new one;
		 * since we are storing -1 - masklength, the sense
		 * of the test is reversed.
		 */
		for (; tt && (tt->rn_b <= masklen_leaf);
					x = tt, tt = tt->rn_dupedkey)
			if (tt->rn_mask == netmask)
				return (0);  /* mask is also duplicated */
		if (tt == base) {
			/* link in at head of list */
			us->rn_dupedkey = tt;
			us->rn_p = p;
			if (p->rn_l == tt)
				p->rn_l = us; else p->rn_r = us;
			base = us;
		} else {
			us->rn_dupedkey = x->rn_dupedkey;
			x->rn_dupedkey = us;
		}
#ifdef RN_DEBUG
		t=tt+1; tt->rn_info = rn_nodenum++; t->rn_info = rn_nodenum++;
		tt->rn_twin = t; tt->rn_ybro = rn_clist; rn_clist = tt;
#endif
		t = saved_tt;
		us->rn_key = (caddr_t) v;
		us->rn_flags = RNF_ACTIVE;
	}
	us->rn_b = masklen_leaf;
	us->rn_mask = netmask;
	/*
	 * Annotate tree about masks.
	 */
			MKGet(m);
			if (m) {
				Bzero(m, sizeof *m);
				m->rm_b = x->rn_b;
				m->rm_mask = x->rn_mask;
				x->rn_mklist = t->rn_mklist = m;
			}

		}
	    }
	} else if (s->rn_mklist) {
		/*
		 * Skip over masks whose index is > that of new node
		 */
		for (mp = &(s->rn_mklist); m = *mp; mp = &m->rn_mklist)
			if (m->rn_b >= b_leaf)
				break;
		p->rn_mklist = m; *mp = 0;
	}
on2:
	/* Add new route to highest possible ancestor's list */
	if ((netmask == 0) || (masklen > p->rn_b ))
		return us; /* can't lift at all */
	do {
		x = t;
		t = t->rn_p;
	} while (masklen <= t->rn_b && x != top);
	/*
	 * Search through routes associated with node to
	 * insert new route according to index.
	 * For nodes of equal index, place more specific
	 * masks first.
	 */
	cplim = netmask + mlen;
	for (mp = &x->rn_mklist; m = *mp; mp = &m->rn_mklist) {
		if (m->rn_b > masklen_leaf)
			continue;
		if (m->rn_b < masklen_leaf)
			break;
		if (m->rn_b == masklen_leaf) {
			printf("rn_addroute: impossible duplicate mask\n");
			return us;
		}
	}
	*mp = us;
	us->rn_mklist = m;
	return us;
}

struct radix_node *
rn_delete(v_arg, n_arg, head)
	void *v_arg, *n_arg;
	struct radix_node_head *head;
{
	register struct radix_node *t, *x, *tt;
	struct radix_node *dupedkey;
	caddr_t v, netmask;
	int b, head_off, vlen;

	v = v_arg;
	x = head->rnh_treetop;
	tt = rn_search(v, x);
	head_off = x->rn_off;
	vlen =  *(u_char *)v;
	if (tt == 0 ||
	    Bcmp(v + head_off, tt->rn_key + head_off, vlen - head_off))
		return (0);
	/*
	 * Check for possiblity of key being duped in tree.
	 */
	if (dupedkey = tt->rn_dupedkey) {
		if (netmask_arg) 
			netmask = rn_search(netmask_arg, rn_masktop)->rn_key;
		else
			netmask = 0;
		while (tt->rn_mask != netmask)
			if ((tt = tt->rn_dupedkey) == 0)
				return (0);
	}
	return (rn_delete1(tt, head));
}

struct radix_node *
rn_delete1(rn, head)
	struct radix_node *rn;
	struct radix_node_head *head;
{
	register struct radix_node *t, *p, *x, *tt;
	struct radix_node *m, *saved_m, **mp;
	struct radix_node *dupedkey, *base, *top = head->rnh_treetop;
	int b, head_off = head->rnh_offset >> 3, masklen, masklen_leaf;
	int vlen = VLEN(v_arg, head) >> 3;
	caddr_t v = v_arg;

	base = tt = rn_search_unmapped(v_arg, head);
	if (tt == 0 || Bcmp(v + head_off, tt->rn_key + head_off, vlen))
		return (0);

	p = t = tt->rn_p;
	(void) rn_masksubr(v_arg, n_arg, head_off, head, &masklen);
	masklen_leaf = -1 - masklen;
		while (tt->rn_b != masklen_leaf)
			if ((tt = tt->rn_dupedkey) == 0)
				return (0);
	}
	/*
	 * Delete our route from mask lists.
	 */
	if (tt->rn_mask == 0 || masklen > t->rn_b)
		goto on1; /* Wasn't lifted at all */
	do {
		x = p;
		p = p->rn_p;
	} while (masklen <= p->rn_b && x != top);
	for (mp = &x->rn_mklist; m = *mp; mp = &m->rn_mklist)
		if (m == tt) {
			*mp = tt->rn_mklist;
			break;
		}
	if (m == 0)
		printf("rn_delete: couldn't find our annotation\n");
on1:
	/*
	 * Eliminate us from tree
	 */
	if (tt->rn_flags & RNF_ROOT)
		return (0);
#ifdef RN_DEBUG
	/* Get us out of the creation list */
	for (t = rn_clist; t && t->rn_ybro != tt; t = t->rn_ybro) {}
	if (t) t->rn_ybro = tt->rn_ybro;
#endif
	if (dupedkey) {
		if (tt == base) {
			x = dupedkey; x->rn_p = t;
			if (t->rn_l == tt) t->rn_l = x; else t->rn_r = x;
		} else {
			/* find node in front of tt on the chain */
			else printf("rn_delete: couldn't find us\n");
		}
		x = tt + 1;
		if  (x->rn_flags & RNF_ACTIVE) {
		/* Find inactive node to clober among this chain.  */
			for (t = base; t; t = x->rn_dupedkey)
				if ((t[1].rn_flags & RNF_ACTIVE) == 0)
					break;
			if (t == 0) {
				printf("rn_delete: lost inactive node");
				return (0);
			}
			t++;
			goto clobber;
		}
		goto out;
	}
	if (t->rn_l == tt) x = t->rn_r; else x = t->rn_l;
	p = t->rn_p;
	if (p->rn_r == t) p->rn_r = x; else p->rn_l = x;
	x->rn_p = p;
	/*
	 * Demote routes attached to us.
	 */
	if (t->rn_mklist) {
		if (x->rn_b >= 0) {
			for (mp = &x->rn_mklist; (m = *mp);)
				mp = &m->rn_mklist;
			*mp = t->rn_mklist;
		}
	}
	/*
	 * We may be holding an active internal node in the tree.
	 */
	x = tt + 1;
	if (t != x) {
clobber:
#ifndef RN_DEBUG
		*t = *x;
#else
		b = t->rn_info; *t = *x; t->rn_info = b;
#endif
		t->rn_l->rn_p = t; t->rn_r->rn_p = t;
		p = x->rn_p;
		if (p->rn_l == x) p->rn_l = t; else p->rn_r = t;
	}
out:
	tt->rn_flags &= ~RNF_ACTIVE;
	tt[1].rn_flags &= ~RNF_ACTIVE;
	return (tt);
}

int
rn_walktree(h, f, w)
	struct radix_node_head *h;
	register int (*f)();
	void *w;
{
	int error;
	struct radix_node *base, *next;
	register struct radix_node *rn = h->rnh_treetop;
	/*
	 * This gets complicated because we may delete the node
	 * while applying the function f to it, so we need to calculate
	 * the successor node in advance.
	 */
	/* First time through node, go left */
	while (rn->rn_b >= 0)
		rn = rn->rn_l;
	for (;;) {
		base = rn;
		/* If at right child go back up, otherwise, go right */
		while (rn->rn_p->rn_r == rn && (rn->rn_flags & RNF_ROOT) == 0)
			rn = rn->rn_p;
		/* Find the next *leaf* since next node might vanish, too */
		for (rn = rn->rn_p->rn_r; rn->rn_b >= 0;)
			rn = rn->rn_l;
		next = rn;
		/* Process leaves */
		while (rn = base) {
			base = rn->rn_dupedkey;
			if (!(rn->rn_flags & RNF_ROOT) && (error = (*f)(rn, w)))
				return (error);
		}
		rn = next;
		if (rn->rn_flags & RNF_ROOT)
			return (0);
	}
	/* NOTREACHED */
}

int
rn_inithead(head, off)
	void **head;
	int off;
{
	register struct radix_node_head *rnh;
	register struct radix_node *t, *tt, *ttt;
	if (*head)
		return (1);
	R_Malloc(rnh, struct radix_node_head *, sizeof (*rnh));
	if (rnh == 0)
		return (0);
	Bzero(rnh, sizeof (*rnh));
	*head = rnh;
	rnh->rnh_offset = off;
	t = rn_newpair1(rn_zeros, 0, rnh->rnh_nodes, rnh);
	ttt = rnh->rnh_nodes + 2;
	t->rn_r = ttt;
	t->rn_p = t;
	tt = t->rn_l;
	tt->rn_flags = t->rn_flags = RNF_ROOT | RNF_ACTIVE;
	tt->rn_b = -1;
	*ttt = *tt;
	ttt->rn_key = rn_ones;
	rnh->rnh_treetop = t;
	rnh->rnh_addaddr = rn_addroute;
	rnh->rnh_deladdr = rn_delete;
	rnh->rnh_matchaddr = rn_match;
	rnh->rnh_lookup = rn_lookup;
	rnh->rnh_walktree = rn_walktree;
	rnh->rnh_bits_matched = rn_unmapped_bits_matched;
	rnh->rnh_set_mask = rn_unmapped_set_mask;
	return (1);
}

void
rn_init()
{
	char *cp, *cplim;
#ifdef KERNEL
	struct domain *dom;

	for (dom = domains; dom; dom = dom->dom_next)
		if (dom->dom_maxrtkey > max_keylen)
			max_keylen = dom->dom_maxrtkey;
#endif
	if (max_keylen == 0) {
		printf("rn_init: radix functions require max_keylen be set\n");
		return;
	}
	R_Malloc(rn_zeros, char *, 3 * max_keylen);
	if (rn_zeros == NULL)
		panic("rn_init");
	Bzero(rn_zeros, 2 * max_keylen);
	rn_ones = cp = rn_zeros + max_keylen;
	while (cp < cplim)
		*cp++ = -1;
	if (rn_inithead((void **)&mask_rnhead, 0) == 0)
		panic("rn_init 2");
}
