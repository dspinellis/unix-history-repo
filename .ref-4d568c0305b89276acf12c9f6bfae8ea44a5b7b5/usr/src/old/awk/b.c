/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)b.c	4.4 (Berkeley) %G%";
#endif /* not lint */

#include "awk.def"
#include "stdio.h"
#include "awk.h"

extern node *op2();
extern struct fa *cgotofn();
#define MAXLIN 256
#define NCHARS 128
#define NSTATES 256

#define type(v)	v->nobj
#define left(v)	v->narg[0]
#define right(v)	v->narg[1]
#define parent(v)	v->nnext

#define LEAF	case CCL: case NCCL: case CHAR: case DOT:
#define UNARY	case FINAL: case STAR: case PLUS: case QUEST:

/* encoding in tree nodes:
	leaf (CCL, NCCL, CHAR, DOT): left is index, right contains value or pointer to value
	unary (FINAL, STAR, PLUS, QUEST): left is child, right is null
	binary (CAT, OR): left and right are children
	parent contains pointer to parent
*/

struct fa {
	int cch;
	struct fa *st;
};

int	*state[NSTATES];
int	*foll[MAXLIN];
char	chars[MAXLIN];
int	setvec[MAXLIN];
node	*point[MAXLIN];

int	setcnt;
int	line;
int	maxfoll;  /* index of highest foll[] entry set by cfoll() */


struct fa *makedfa(p)	/* returns dfa for tree pointed to by p */
node *p;
{
	node *p1;
	struct fa *fap;
	p1 = op2(CAT, op2(STAR, op2(DOT, (node *) 0, (node *) 0), (node *) 0), p);
		/* put DOT STAR in front of reg. exp. */
	p1 = op2(FINAL, p1, (node *) 0);		/* install FINAL node */

	line = 0;
	penter(p1);	/* enter parent pointers and leaf indices */
	point[line] = p1;	/* FINAL node */
	setvec[0] = 1;		/* for initial DOT STAR */
	cfoll(p1);	/* set up follow sets */
	fap = cgotofn();
	freetr(p1);	/* add this when alloc works */
	return(fap);
}

penter(p)	/* set up parent pointers and leaf indices */
node *p;
{
	switch(type(p)) {
		LEAF
			left(p) = (node *) line;
			point[line++] = p;
			break;
		UNARY
			penter(left(p));
			parent(left(p)) = p;
			break;
		case CAT:
		case OR:
			penter(left(p));
			penter(right(p));
			parent(left(p)) = p;
			parent(right(p)) = p;
			break;
		default:
			error(FATAL, "unknown type %d in penter\n", type(p));
			break;
	}
}

freetr(p)	/* free parse tree and follow sets */
node *p;
{
	switch(type(p)) {
		LEAF
			foll_free((int) left(p));
			xfree(p);
			break;
		UNARY
			freetr(left(p));
			xfree(p);
			break;
		case CAT:
		case OR:
			freetr(left(p));
			freetr(right(p));
			xfree(p);
			break;
		default:
			error(FATAL, "unknown type %d in freetr", type(p));
			break;
	}
}
char *cclenter(p)
register char *p;
{
	register i, c;
	char *op;

	op = p;
	i = 0;
	while ((c = *p++) != 0) {
		if (c == '-' && i > 0 && chars[i-1] != 0) {
			if (*p != 0) {
				c = chars[i-1];
				while (c < *p) {
					if (i >= MAXLIN)
						overflo();
					chars[i++] = ++c;
				}
				p++;
				continue;
			}
		}
		if (i >= MAXLIN)
			overflo();
		chars[i++] = c;
	}
	chars[i++] = '\0';
	dprintf("cclenter: in = |%s|, out = |%s|\n", op, chars, NULL);
	xfree(op);
	return(tostring(chars));
}

overflo()
{
	error(FATAL, "regular expression too long\n");
}

cfoll(v)		/* enter follow set of each leaf of vertex v into foll[leaf] */
register node *v;
{
	register i;
	int prev;
	int *add();

	maxfoll = -1;
	switch(type(v)) {
		LEAF
			setcnt = 0;
			for (i=1; i<=line; i++)
				setvec[i] = 0;
			follow(v);
			if (notin(foll, ( (int) left(v))-1, &prev)) {
				foll[(int) left(v)] = add(setcnt);
			}
			else
				foll[ (int) left(v)] = foll[prev];
			if ((int)left(v) > maxfoll)
				maxfoll = (int)left(v);
			break;
		UNARY
			cfoll(left(v));
			break;
		case CAT:
		case OR:
			cfoll(left(v));
			cfoll(right(v));
			break;
		default:
			error(FATAL, "unknown type %d in cfoll", type(v));
	}
}

first(p)			/* collects initially active leaves of p into setvec */
register node *p;		/* returns 0 or 1 depending on whether p matches empty string */
{
	register b;

	switch(type(p)) {
		LEAF
			if (setvec[(int) left(p)] != 1) {
				setvec[(int) left(p)] = 1;
				setcnt++;
			}
			if (type(p) == CCL && (*(char *) right(p)) == '\0')
				return(0);		/* empty CCL */
			else return(1);
		case FINAL:
		case PLUS:
			if (first(left(p)) == 0) return(0);
			return(1);
		case STAR:
		case QUEST:
			first(left(p));
			return(0);
		case CAT:
			if (first(left(p)) == 0 && first(right(p)) == 0) return(0);
			return(1);
		case OR:
			b = first(right(p));
			if (first(left(p)) == 0 || b == 0) return(0);
			return(1);
	}
	error(FATAL, "unknown type %d in first\n", type(p));
	return(-1);
}

follow(v)
node *v;		/* collects leaves that can follow v into setvec */
{
	node *p;

	if (type(v) == FINAL)
		return;
	p = parent(v);
	switch (type(p)) {
		case STAR:
		case PLUS:	first(v);
				follow(p);
				return;

		case OR:
		case QUEST:	follow(p);
				return;

		case CAT:	if (v == left(p)) {	/* v is left child of p */
					if (first(right(p)) == 0) {
						follow(p);
						return;
					}
				}
				else		/* v is right child */
					follow(p);
				return;
		case FINAL:	if (setvec[line] != 1) {
					setvec[line] = 1;
					setcnt++;
				}
				return;
	}
}

member(c, s)	/* is c in s? */
register char c, *s;
{
	while (*s)
		if (c == *s++)
			return(1);
	return(0);
}

notin(array, n, prev)		/* is setvec in array[0] thru array[n]? */
int **array;
int *prev; {
	register i, j;
	int *ptr;
	for (i=0; i<=n; i++) {
		ptr = array[i];
		if (*ptr == setcnt) {
			for (j=0; j < setcnt; j++)
				if (setvec[*(++ptr)] != 1) goto nxt;
			*prev = i;
			return(0);
		}
		nxt: ;
	}
	return(1);
}

int *add(n) {		/* remember setvec */
	int *ptr, *p;
	register i;
	if ((p = ptr = (int *) malloc((n+1)*sizeof(int))) == NULL)
		overflo();
	*ptr = n;
	dprintf("add(%d)\n", n, NULL, NULL);
	for (i=1; i <= line; i++)
		if (setvec[i] == 1) {
			*(++ptr) = i;
			dprintf("  ptr = %o, *ptr = %d, i = %d\n", ptr, *ptr, i);
		}
	dprintf("\n", NULL, NULL, NULL);
	return(p);
}

struct fa *cgotofn()
{
	register i, k;
	register int *ptr;
	char c;
	char *p;
	node *cp;
	int j, n, s, ind, numtrans;
	int finflg;
	int curpos, num, prev;
	struct fa *where[NSTATES];

	int fatab[257];
	struct fa *pfa;

	char index[MAXLIN];
	char iposns[MAXLIN];
	int sposns[MAXLIN];
	int spmax, spinit;

	char symbol[NCHARS];
	char isyms[NCHARS];
	char ssyms[NCHARS];
	int ssmax, ssinit;

	for (i=0; i<=line; i++) index[i] = iposns[i] = setvec[i] = 0;
	for (i=0; i<NCHARS; i++)  isyms[i] = symbol[i] = 0;
	setcnt = 0;
	/* compute initial positions and symbols of state 0 */
	ssmax = 0;
	ptr = state[0] = foll[0];
	spinit = *ptr;
	for (i=0; i<spinit; i++) {
		curpos = *(++ptr);
		sposns[i] = curpos;
		iposns[curpos] = 1;
		cp = point[curpos];
		dprintf("i = %d, spinit = %d, curpos = %d\n", i, spinit, curpos);
		switch (type(cp)) {
			case CHAR:
				k = (int) right(cp);
				if (isyms[k] != 1) {
					isyms[k] = 1;
					ssyms[ssmax++] = k;
				}
				break;
			case DOT:
				for (k=1; k<NCHARS; k++) {
					if (k != HAT) {
						if (isyms[k] != 1) {
							isyms[k] = 1;
							ssyms[ssmax++] = k;
						}
					}
				}
				break;
			case CCL:
				for (p = (char *) right(cp); *p; p++) {
					if (*p != HAT) {
						if (isyms[*p] != 1) {
							isyms[*p] = 1;
							ssyms[ssmax++] = *p;
						}
					}
				}
				break;
			case NCCL:
				for (k=1; k<NCHARS; k++) {
					if (k != HAT && !member(k, (char *) right(cp))) {
						if (isyms[k] != 1) {
							isyms[k] = 1;
							ssyms[ssmax++] = k;
						}
					}
				}
		}
	}
	ssinit = ssmax;
	n = 0;
	for (s=0; s<=n; s++)  {
	dprintf("s = %d\n", s, NULL, NULL);
		ind = 0;
		numtrans = 0;
		finflg = 0;
		if (*(state[s] + *state[s]) == line) {		/* s final? */
			finflg = 1;
			goto tenter;
		}
		spmax = spinit;
		ssmax = ssinit;
		ptr = state[s];
		num = *ptr;
		for (i=0; i<num; i++) {
			curpos = *(++ptr);
			if (iposns[curpos] != 1 && index[curpos] != 1) {
				index[curpos] = 1;
				sposns[spmax++] = curpos;
			}
			cp = point[curpos];
			switch (type(cp)) {
				case CHAR:
					k = (int) right(cp);
					if (isyms[k] == 0 && symbol[k] == 0) {
						symbol[k] = 1;
						ssyms[ssmax++] = k;
					}
					break;
				case DOT:
					for (k=1; k<NCHARS; k++) {
						if (k != HAT) {
							if (isyms[k] == 0 && symbol[k] == 0) {
								symbol[k] = 1;
								ssyms[ssmax++] = k;
							}
						}
					}
					break;
				case CCL:
					for (p = (char *) right(cp); *p; p++) {
						if (*p != HAT) {
							if (isyms[*p] == 0 && symbol[*p] == 0) {
								symbol[*p] = 1;
								ssyms[ssmax++] = *p;
							}
						}
					}
					break;
				case NCCL:
					for (k=1; k<NCHARS; k++) {
						if (k != HAT && !member(k, (char *) right(cp))) {
							if (isyms[k] == 0 && symbol[k] == 0) {
								symbol[k] = 1;
								ssyms[ssmax++] = k;
							}
						}
					}
			}
		}
		for (j=0; j<ssmax; j++) {	/* nextstate(s, ssyms[j]) */
			c = ssyms[j];
			symbol[c] = 0;
			setcnt = 0;
			for (k=0; k<=line; k++) setvec[k] = 0;
			for (i=0; i<spmax; i++) {
				index[sposns[i]] = 0;
				cp = point[sposns[i]];
				if ((k = type(cp)) != FINAL)
					if (k == CHAR && c == (int) right(cp)
					 || k == DOT
					 || k == CCL && member(c, (char *) right(cp))
					 || k == NCCL && !member(c, (char *) right(cp))) {
						ptr = foll[sposns[i]];
						num = *ptr;
						for (k=0; k<num; k++) {
							if (setvec[*(++ptr)] != 1
								&& iposns[*ptr] != 1) {
								setvec[*ptr] = 1;
								setcnt++;
							}
						}
					}
			} /* end nextstate */
			if (notin(state, n, &prev)) {
				if (n >= NSTATES) {
					dprintf("cgotofn: notin; state = %d, n = %d\n", state, n, NULL);
					overflo();
				}
				state[++n] = add(setcnt);
				dprintf("	delta(%d,%o) = %d", s,c,n);
				dprintf(", ind = %d\n", ind+1, NULL, NULL);
				fatab[++ind] = c;
				fatab[++ind] = n;
				numtrans++;
			}
			else {
				if (prev != 0) {
					dprintf("	delta(%d,%o) = %d", s,c,prev);
					dprintf(", ind = %d\n", ind+1, NULL, NULL);
					fatab[++ind] = c;
					fatab[++ind] = prev;
					numtrans++;
				}
			}
		}
	tenter:
		if ((pfa = (struct fa *) malloc((numtrans + 1) * sizeof(struct fa))) == NULL)
			overflo();
		where[s] = pfa;
		if (finflg)
			pfa->cch = -1;		/* s is a final state */
		else
			pfa->cch = numtrans;
		pfa->st = 0;
		for (i=1, pfa += 1; i<=numtrans; i++, pfa++) {
			pfa->cch = fatab[2*i-1];
			pfa->st = (struct fa *) fatab[2*i];
		}
	}
	for (i=0; i<=n; i++) {
		/* N.b. state[0] == foll[0], not separately allocated */
		if (i>0)
			xfree(state[i]);       /* free state[i] */
		pfa = where[i];
		pfa->st = where[0];
		dprintf("state %d: (%o)\n", i, pfa, NULL);
		dprintf("	numtrans = %d,	default = %o\n", pfa->cch, pfa->st, NULL);
		for (k=1; k<=pfa->cch; k++) {
			(pfa+k)->st = where[ (int) (pfa+k)->st];
			dprintf("	char = %o,	nextstate = %o\n",(pfa+k)->cch, (pfa+k)->st, NULL);
		}
	}
	pfa = where[0];
	if ((num = pfa->cch) < 0)
		return(where[0]);
	for (pfa += num; num; num--, pfa--)
		if (pfa->cch == HAT) {
			return(pfa->st);
		}
	return(where[0]);
}

match(pfa, p)
register struct fa *pfa;
register char *p;
{
	register count;
	char c;
	if (p == 0) return(0);
	if (pfa->cch == 1) {		/* fast test for first character, if possible */
		c = (++pfa)->cch;
		do
			if (c == *p) {
				p++;
				pfa = pfa->st;
				goto adv;
			}
		while (*p++ != 0);
		return(0);
	}
   adv: if ((count = pfa->cch) < 0) return(1);
	do {
		for (pfa += count; count; count--, pfa--)
			if (pfa->cch == *p) {
				break;
			}
		pfa = pfa->st;
		if ((count = pfa->cch) < 0) return(1);
	} while(*p++ != 0);
	return(0);
}

/*
 * Free foll[i], taking into account identical foll[] entries.
 * This is necessary because cfoll() uses the same physical follow set for
 * several foll[] entries when the set is identical.  Called by freetr().
 */
foll_free(i)
int i;
{
	register int j;
	int *p = foll[i];
	if (p==NULL) return;
	for (j=0; j<=maxfoll; j++) 
		if (foll[j]==p) foll[j]=NULL;
	xfree(p);
}
