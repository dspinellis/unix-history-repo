/*-
 * Copyright (c) 1982, 1986, 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 *
 *	@(#)tty_subr.c	7.13 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/buf.h>
#include <sys/ioctl.h>
#include <sys/proc.h>
#include <sys/tty.h>
#include <sys/clist.h>

char	cwaiting;
struct	cblock *cfree, *cfreelist;
int	cfreecount, nclist;

#define setquote(cp) \
	setbit(((char *)((int)(cp)&~CROUND)+sizeof(struct cblock *)), \
		(int)(cp)&CROUND)
#define isquote(cp) \
	isset(((char *)((int)(cp)&~CROUND)+sizeof(struct cblock *)), \
		(int)(cp)&CROUND)
#define cbptr(x) ((struct cblock *)(x))

/*
 * Initialize clist by freeing all character blocks.
 */
cinit()
{
	register int ccp;
	register struct cblock *cp;

	ccp = (int) cfree;
	ccp = (ccp + CROUND) & ~CROUND;
	for(cp = (struct cblock *) ccp; cp < &cfree[nclist - 1]; cp++) {
		cp->c_next = cfreelist;
		cfreelist = cp;
		cfreecount += CBSIZE;
	}
}

/*
 * Character list get/put
 */
getc(p)
	register struct clist *p;
{
	register struct cblock *bp;
	register int c, s;

	s = spltty();
	if (p->c_cc <= 0) {
		c = -1;
		p->c_cc = 0;
		p->c_cf = p->c_cl = NULL;
	} else {
		c = *p->c_cf & 0377;
		if (isquote(p->c_cf))
			c |= TTY_QUOTE;
		p->c_cf++;
		if (--p->c_cc<=0) {
			bp = cbptr(p->c_cf-1);
			bp = cbptr((int)bp & ~CROUND);
			p->c_cf = NULL;
			p->c_cl = NULL;
			bp->c_next = cfreelist;
			cfreelist = bp;
			cfreecount += CBSIZE;
			if (cwaiting) {
				wakeup(&cwaiting);
				cwaiting = 0;
			}
		} else if (((int)p->c_cf & CROUND) == 0){
			bp = cbptr(p->c_cf);
			bp--;
			p->c_cf = bp->c_next->c_info;
			bp->c_next = cfreelist;
			cfreelist = bp;
			cfreecount += CBSIZE;
			if (cwaiting) {
				wakeup(&cwaiting);
				cwaiting = 0;
			}
		}
	}
	splx(s);
	return (c);
}

/*
 * copy clist to buffer.
 * return number of bytes moved.
 */
q_to_b(q, cp, cc)
	register struct clist *q;
	register char *cp;
	int cc;
{
	register struct cblock *bp;
	register int s, nc;
	char *acp;

	if (cc <= 0)
		return (0);
	s = spltty();
	if (q->c_cc <= 0) {
		q->c_cc = 0;
		q->c_cf = q->c_cl = NULL;
		splx(s);
		return (0);
	}
	acp = cp;

	while (cc) {
		nc = sizeof (struct cblock) - ((int)q->c_cf & CROUND);
		nc = min(nc, cc);
		nc = min(nc, q->c_cc);
		(void) bcopy(q->c_cf, cp, (unsigned)nc);
		q->c_cf += nc;
		q->c_cc -= nc;
		cc -= nc;
		cp += nc;
		if (q->c_cc <= 0) {
			bp = cbptr(q->c_cf - 1);
			bp = cbptr((int)bp & ~CROUND);
			q->c_cf = q->c_cl = NULL;
			bp->c_next = cfreelist;
			cfreelist = bp;
			cfreecount += CBSIZE;
			if (cwaiting) {
				wakeup(&cwaiting);
				cwaiting = 0;
			}
			break;
		}
		if (((int)q->c_cf & CROUND) == 0) {
			bp = cbptr(q->c_cf);
			bp--;
			q->c_cf = bp->c_next->c_info;
			bp->c_next = cfreelist;
			cfreelist = bp;
			cfreecount += CBSIZE;
			if (cwaiting) {
				wakeup(&cwaiting);
				cwaiting = 0;
			}
		}
	}
	splx(s);
	return (cp-acp);
}

/*
 * Return count of contiguous characters
 * in clist starting at q->c_cf.
 * Stop counting if flag&character is non-null.
 */
ndqb(q, flag)
	register struct clist *q;
	int flag;
{
	register int cc, s;

	s = spltty();
	if (q->c_cc <= 0) {
		cc = -q->c_cc;
		goto out;
	}
	cc = ((int)q->c_cf + CBSIZE) & ~CROUND;
	cc -= (int)q->c_cf;
	if (q->c_cc < cc)
		cc = q->c_cc;
	if (flag) {
		register char *p, *end;

		p = q->c_cf;
		end = p;
		end += cc;
		while (p < end) {
			if (*p & flag) {
				cc = (int)p;
				cc -= (int)q->c_cf;
				break;
			}
			p++;
		}
	}
out:
	splx(s);
	return (cc);
}

/*
 * Flush cc bytes from q.
 */
ndflush(q, cc)
	register struct clist *q;
	register int cc;
{
	register struct cblock *bp;
	char *end;
	int rem, s;

	s = spltty();
	if (q->c_cc <= 0)
		goto out;
	while (cc>0 && q->c_cc) {
		bp = cbptr((int)q->c_cf & ~CROUND);
		if ((int)bp == (((int)q->c_cl-1) & ~CROUND)) {
			end = q->c_cl;
		} else {
			end = (char *)((int)bp + sizeof (struct cblock));
		}
		rem = end - q->c_cf;
		if (cc >= rem) {
			cc -= rem;
			q->c_cc -= rem;
			q->c_cf = bp->c_next->c_info;
			bp->c_next = cfreelist;
			cfreelist = bp;
			cfreecount += CBSIZE;
			if (cwaiting) {
				wakeup(&cwaiting);
				cwaiting = 0;
			}
		} else {
			q->c_cc -= cc;
			q->c_cf += cc;
			if (q->c_cc <= 0) {
				bp->c_next = cfreelist;
				cfreelist = bp;
				cfreecount += CBSIZE;
				if (cwaiting) {
					wakeup(&cwaiting);
					cwaiting = 0;
				}
			}
			break;
		}
	}
	if (q->c_cc <= 0) {
		q->c_cf = q->c_cl = NULL;
		q->c_cc = 0;
	}
out:
	splx(s);
}


putc(c, p)
	int c;
	register struct clist *p;
{
	register struct cblock *bp;
	register char *cp;
	register int s;

	s = spltty();
	if ((cp = p->c_cl) == NULL || p->c_cc < 0 ) {	/* no cblocks yet */
		if ((bp = cfreelist) == NULL) {
			splx(s);
			return (-1);
		}
		cfreelist = bp->c_next;
		cfreecount -= CBSIZE;
		bp->c_next = NULL;
		bzero(bp->c_quote, CBQSIZE);
		p->c_cf = cp = bp->c_info;
	} else if (((int)cp & CROUND) == 0) {
		bp = cbptr(cp) - 1;	/* pointer arith */
		if ((bp->c_next = cfreelist) == NULL) {
			splx(s);
			return (-1);
		}
		bp = bp->c_next;
		cfreelist = bp->c_next;
		cfreecount -= CBSIZE;
		bp->c_next = NULL;
		cp = bp->c_info;
	}
	if (c&TTY_QUOTE)
		setquote(cp);
	*cp++ = c;
	p->c_cc++;
	p->c_cl = cp;
	splx(s);
	return (0);
}

/*
 * copy buffer to clist.
 * return number of bytes not transfered.
 */
b_to_q(cp, cc, q)
	register char *cp;
	struct clist *q;
	register int cc;
{
	register char *cq;
	register struct cblock *bp;
	register int s, nc;
	int acc;

	if (cc <= 0)
		return (0);
	acc = cc;
	s = spltty();
	if ((cq = q->c_cl) == NULL || q->c_cc < 0) {
		if ((bp = cfreelist) == NULL) 
			goto out;
		cfreelist = bp->c_next;
		cfreecount -= CBSIZE;
		bzero(bp->c_quote, CBQSIZE);
		bp->c_next = NULL;
		q->c_cf = cq = bp->c_info;
	}

	while (cc) {
		if (((int)cq & CROUND) == 0) {
			bp = cbptr(cq) - 1;
			if ((bp->c_next = cfreelist) == NULL) 
				goto out;
			bp = bp->c_next;
			cfreelist = bp->c_next;
			cfreecount -= CBSIZE;
			bzero(bp->c_quote, CBQSIZE);
			bp->c_next = NULL;
			cq = bp->c_info;
		}
		nc = min(cc, sizeof (struct cblock) - ((int)cq & CROUND));
		(void) bcopy(cp, cq, (unsigned)nc);
		cp += nc;
		cq += nc;
		cc -= nc;
	}
out:
	q->c_cl = cq;
	q->c_cc += acc - cc;
	splx(s);
	return (cc);
}

/*
 * Given a non-NULL pointter into the list (like c_cf which
 * always points to a real character if non-NULL) return the pointer
 * to the next character in the list or return NULL if no more chars.
 *
 * Callers must not allow getc's to happen between nextc's so that the
 * pointer becomes invalid.  Note that interrupts are NOT masked.
 */
char *
nextc(p, cp, c)
	register struct clist *p;
	register char *cp;
	register int *c;
{

	if (p->c_cc && ++cp != p->c_cl) {
		if (((int)cp & CROUND) == 0) {
			cp = (cbptr(cp))[-1].c_next->c_info;
		}
		*c = *cp;
		if (isquote(cp))
			*c |= TTY_QUOTE;
		return (cp);
	}
	return (0);
}

/*
 * Remove the last character in the list and return it.
 */
unputc(p)
	register struct clist *p;
{
	register struct cblock *bp;
	register int c, s;
	struct cblock *obp;

	s = spltty();
	if (p->c_cc <= 0)
		c = -1;
	else {
		c = *--p->c_cl;
		if (isquote(p->c_cl))
			c |= TTY_QUOTE;
		if (--p->c_cc <= 0) {
			bp = cbptr(p->c_cl);
			bp = cbptr((int)bp & ~CROUND);
			p->c_cl = p->c_cf = NULL;
			bp->c_next = cfreelist;
			cfreelist = bp;
			cfreecount += CBSIZE;
		} else if (p->c_cl == (cbptr((int)p->c_cl & ~CROUND))->c_info) {
			p->c_cl = (char *)((int)p->c_cl & ~CROUND);

			bp = cbptr(p->c_cf);
			bp = cbptr((int)bp & ~CROUND);
			while (bp->c_next != cbptr(p->c_cl))
				bp = bp->c_next;
			obp = bp;
			p->c_cl = (char *)(bp + 1);
			bp = bp->c_next;
			bp->c_next = cfreelist;
			cfreelist = bp;
			cfreecount += CBSIZE;
			obp->c_next = NULL;
		}
	}
	splx(s);
	return (c);
}

/*
 * Put the chars in the from que
 * on the end of the to que.
 */
catq(from, to)
	struct clist *from, *to;
{
#ifdef notdef
	char bbuf[CBSIZE*4];
#endif
	register int s, c;

	s = spltty();
	if (to->c_cc == 0) {
		*to = *from;
		from->c_cc = 0;
		from->c_cf = NULL;
		from->c_cl = NULL;
		splx(s);
		return;
	}
	splx(s);
#ifdef notdef
	while (from->c_cc > 0) {
		c = q_to_b(from, bbuf, sizeof bbuf);
		(void) b_to_q(bbuf, c, to);
	}
#endif
	/* XXX - FIX */
	while ((c = getc(from)) >= 0)
		putc(c, to);
}

#ifdef unneeded
/*
 * Integer (short) get/put using clists.
 */
typedef	u_short word_t;

getw(p)
	register struct clist *p;
{
	register int s, c;
	register struct cblock *bp;

	if (p->c_cc <= 1)
		return(-1);
	if (p->c_cc & 01) {
		c = getc(p);
#if BYTE_ORDER == LITTLE_ENDIAN
		return (c | (getc(p)<<8));
#else
		return (getc(p) | (c<<8));
#endif
	}
	s = spltty();
#if BYTE_ORDER == LITTLE_ENDIAN 
	c = (((u_char *)p->c_cf)[0] << 8) | ((u_char *)p->c_cf)[1];
#else
	c = (((u_char *)p->c_cf)[1] << 8) | ((u_char *)p->c_cf)[0];
#endif
	p->c_cf += sizeof (word_t);
	p->c_cc -= sizeof (word_t);
	if (p->c_cc <= 0) {
		bp = cbptr(p->c_cf-1);
		bp = cbptr((int)bp & ~CROUND);
		p->c_cf = NULL;
		p->c_cl = NULL;
		bp->c_next = cfreelist;
		cfreelist = bp;
		cfreecount += CBSIZE;
		if (cwaiting) {
			wakeup(&cwaiting);
			cwaiting = 0;
		}
	} else if (((int)p->c_cf & CROUND) == 0) {
		bp = cbptr(p->c_cf);
		bp--;
		p->c_cf = bp->c_next->c_info;
		bp->c_next = cfreelist;
		cfreelist = bp;
		cfreecount += CBSIZE;
		if (cwaiting) {
			wakeup(&cwaiting);
			cwaiting = 0;
		}
	}
	splx(s);
	return (c);
}

putw(c, p)
	register struct clist *p;
	word_t c;
{
	register int s;
	register struct cblock *bp;
	register char *cp;

	s = spltty();
	if (cfreelist==NULL) {
		splx(s);
		return(-1);
	}
	if (p->c_cc & 01) {
#if BYTE_ORDER == LITTLE_ENDIAN
		(void) putc(c, p);
		(void) putc(c>>8, p);
#else
		(void) putc(c>>8, p);
		(void) putc(c, p);
#endif
	} else {
		if ((cp = p->c_cl) == NULL || p->c_cc < 0 ) {
			if ((bp = cfreelist) == NULL) {
				splx(s);
				return (-1);
			}
			cfreelist = bp->c_next;
			cfreecount -= CBSIZE;
			bp->c_next = NULL;
			p->c_cf = cp = bp->c_info;
		} else if (((int)cp & CROUND) == 0) {
			bp = cbptr(cp) - 1;
			if ((bp->c_next = cfreelist) == NULL) {
				splx(s);
				return (-1);
			}
			bp = bp->c_next;
			cfreelist = bp->c_next;
			cfreecount -= CBSIZE;
			bp->c_next = NULL;
			cp = bp->c_info;
		}
#if defined(vax)
		*(word_t *)cp = c;
#else
		((u_char *)cp)[0] = c>>8;
		((u_char *)cp)[1] = c;
#endif
		p->c_cl = cp + sizeof (word_t);
		p->c_cc += sizeof (word_t);
	}
	splx(s);
	return (0);
}
#endif /* unneeded */
