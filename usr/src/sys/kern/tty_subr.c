/*	tty_subr.c	3.3	%H%	*/

#include "../h/param.h"
#include "../h/tty.h"
#include "../h/systm.h"
#include "../h/conf.h"
#include "../h/buf.h"

struct cblock {
	struct cblock *c_next;
	char	c_info[CBSIZE];
};

struct	cblock	cfree[NCLIST];
int	cbad;
struct	cblock	*cfreelist;

/*
 * Character list get/put
 */
getc(p)
register struct clist *p;
{
	register struct cblock *bp;
	register int c, s;

	s = spl6();
	if (p->c_cc <= 0) {
		c = -1;
		p->c_cc = 0;
		p->c_cf = p->c_cl = NULL;
	} else {
		c = *p->c_cf++ & 0377;
		if (--p->c_cc<=0) {
			bp = (struct cblock *)(p->c_cf-1);
			bp = (struct cblock *) ((int)bp & ~CROUND);
			p->c_cf = NULL;
			p->c_cl = NULL;
			bp->c_next = cfreelist;
			cfreelist = bp;
		} else if (((int)p->c_cf & CROUND) == 0){
			bp = (struct cblock *)(p->c_cf);
			bp--;
			p->c_cf = bp->c_next->c_info;
			bp->c_next = cfreelist;
			cfreelist = bp;
		}
	}
	splx(s);
	return(c);
}

/*
 * copy clist to buffer.
 * return number of bytes moved.
 */
q_to_b(q, cp, cc)
register struct clist *q;
register char *cp;
{
	register struct cblock *bp;
	register int s;
	char *acp;

	if (cc <= 0)
		return(0);
	s = spl6();
	if (q->c_cc <= 0) {
		q->c_cc = 0;
		q->c_cf = q->c_cl = NULL;
		return(0);
	}
	acp = cp;
	cc++;

	while (--cc) {
		*cp++ = *q->c_cf++;
		if (--q->c_cc <= 0) {
			bp = (struct cblock *)(q->c_cf-1);
			bp = (struct cblock *)((int)bp & ~CROUND);
			q->c_cf = q->c_cl = NULL;
			bp->c_next = cfreelist;
			cfreelist = bp;
			break;
		}
		if (((int)q->c_cf & CROUND) == 0) {
			bp = (struct cblock *)(q->c_cf);
			bp--;
			q->c_cf = bp->c_next->c_info;
			bp->c_next = cfreelist;
			cfreelist = bp;
		}
	}
	splx(s);
	return(cp-acp);
}

/*
 * Return count of contiguous characters
 * in clist starting at q->c_cf.
 * Stop counting if flag&character is non-null.
 */
ndqb(q, flag)
register struct clist *q;
{
register cc;
int s;

	s = spl6();
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
				cc = (int)p - (int)q->c_cf;
				break;
			}
			p++;
		}
	}
out:
	splx(s);
	return(cc);
}

/*
 * Update clist to show that cc characters
 * were removed.  It is assumed that cc < CBSIZE.
 */
ndflush(q, cc)
register struct clist *q;
register cc;
{
register s;

	if (cc == 0)
		return;
	s = spl6();
	if (q->c_cc < 0) {
		if (q->c_cf != NULL) {
			q->c_cc += cc;
			q->c_cf += cc;
			goto out;
		}
		q->c_cc = 0;
		goto out;
	}
	if (q->c_cc == 0) {
		goto out;
	}
	if (cc > CBSIZE || cc <= 0) {
		cbad++;
		goto out;
	}
	q->c_cc -= cc;
	q->c_cf += cc;
	if (((int)q->c_cf & CROUND) == 0) {
		register struct cblock *bp;

		bp = (struct cblock *)(q->c_cf) -1;
		if (bp->c_next) {
			q->c_cf = bp->c_next->c_info;
		} else {
			q->c_cf = q->c_cl = NULL;
		}
		bp->c_next = cfreelist;
		cfreelist = bp;
	} else
	if (q->c_cc == 0) {
		register struct cblock *bp;
		q->c_cf = (char *)((int)q->c_cf & ~CROUND);
		bp = (struct cblock *)(q->c_cf);
		bp->c_next = cfreelist;
		cfreelist = bp;
		q->c_cf = q->c_cl = NULL;
	}
out:
	splx(s);
}

/*
 * Put character c in queue p.
 */
putc(c, p)
register struct clist *p;
{
	register struct cblock *bp;
	register char *cp;
	register s;

	s = spl6();
	if ((cp = p->c_cl) == NULL || p->c_cc < 0 ) {
		if ((bp = cfreelist) == NULL) {
			splx(s);
			return(-1);
		}
		cfreelist = bp->c_next;
		bp->c_next = NULL;
		p->c_cf = cp = bp->c_info;
	} else if (((int)cp & CROUND) == 0) {
		bp = (struct cblock *)cp - 1;
		if ((bp->c_next = cfreelist) == NULL) {
			splx(s);
			return(-1);
		}
		bp = bp->c_next;
		cfreelist = bp->c_next;
		bp->c_next = NULL;
		cp = bp->c_info;
	}
	*cp++ = c;
	p->c_cc++;
	p->c_cl = cp;
	splx(s);
	return(0);
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
	register s, acc;

	if (cc <= 0)
		return(0);
	acc = cc;
	s = spl6();
	if ((cq = q->c_cl) == NULL || q->c_cc < 0) {
		if ((bp = cfreelist) == NULL) 
			goto out;
		cfreelist = bp->c_next;
		bp->c_next = NULL;
		q->c_cf = cq = bp->c_info;
	}

	while (cc) {
		if (((int)cq & CROUND) == 0) {
			bp = (struct cblock *) cq - 1;
			if ((bp->c_next = cfreelist) == NULL) 
				goto out;
			bp = bp->c_next;
			cfreelist = bp->c_next;
			bp->c_next = NULL;
			cq = bp->c_info;
		}
		*cq++ = *cp++;
		cc--;
	}
out:
	q->c_cl = cq;
	q->c_cc += acc-cc;
	splx(s);
	return(cc);
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
nextc(p, cp)
register struct clist *p;
register char *cp;
{

	if (p->c_cc && ++cp != p->c_cl) {
		if (((int)cp & CROUND) == 0)
			return (((struct cblock *)cp)[-1].c_next->c_info);
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

	s = spl6();
	if (p->c_cc <= 0)
		c = -1;
	else {
		c = *--p->c_cl;
		if (--p->c_cc <= 0) {
			bp = (struct cblock *)p->c_cl;
			bp = (struct cblock *)((int)bp & ~CROUND);
			p->c_cl = p->c_cf = NULL;
			bp->c_next = cfreelist;
			cfreelist = bp;
		} else if (((int)p->c_cl & CROUND) == sizeof(bp->c_next)) {
			p->c_cl = (char *)((int)p->c_cl & ~CROUND);
			bp = (struct cblock *)p->c_cf;
			bp = (struct cblock *)((int)bp & ~CROUND);
			while (bp->c_next != (struct cblock *)p->c_cl)
				bp = bp->c_next;
			obp = bp;
			p->c_cl = (char *)(bp + 1);
			bp = bp->c_next;
			bp->c_next = cfreelist;
			cfreelist = bp;
			obp->c_next = NULL;
		}
	}
	splx(s);
	return (c);
}

/*
 * Put the chars in the from que
 * on the end of the to que.
 *
 * SHOULD JUST USE q_to_b AND THEN b_to_q HERE.
 */
catq(from, to)
struct clist *from, *to;
{
	register c;

	while ((c = getc(from)) >= 0)
		(void) putc(c, to);
}

/*
 * Initialize clist by freeing all character blocks, then count
 * number of character devices. (Once-only routine)
 */
cinit()
{
	register int ccp;
	register struct cblock *cp;
	register struct cdevsw *cdp;

	ccp = (int)cfree;
	ccp = (ccp+CROUND) & ~CROUND;
	for(cp=(struct cblock *)ccp; cp <= &cfree[NCLIST-1]; cp++) {
		cp->c_next = cfreelist;
		cfreelist = cp;
	}
	ccp = 0;
	for(cdp = cdevsw; cdp->d_open; cdp++)
		ccp++;
	nchrdev = ccp;
}

/*
 * integer (2-byte) get/put
 * using clists
 */
/*
getw(p)
register struct clist *p;
{
	register int s;

	if (p->c_cc <= 1)
		return(-1);
	s = getc(p);
	return(s | (getc(p)<<8));
}
*/

putw(c, p)
register struct clist *p;
{
	register s;

	s = spl6();
	if (cfreelist==NULL) {
		splx(s);
		return(-1);
	}
	(void) putc(c, p);
	(void) putc(c>>8, p);
	splx(s);
	return(0);
}
