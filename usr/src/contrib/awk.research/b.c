/****************************************************************
Copyright (C) AT&T 1993
All Rights Reserved

Permission to use, copy, modify, and distribute this software and
its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appear in all
copies and that both that the copyright notice and this
permission notice and warranty disclaimer appear in supporting
documentation, and that the name of AT&T or any of its entities
not be used in advertising or publicity pertaining to
distribution of the software without specific, written prior
permission.

AT&T DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
IN NO EVENT SHALL AT&T OR ANY OF ITS ENTITIES BE LIABLE FOR ANY
SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.
****************************************************************/

/* lasciate ogni speranza, voi ch'entrate. */

#define	DEBUG

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "awk.h"
#include "y.tab.h"

#define	HAT	(NCHARS-1)	/* matches ^ in regular expr */
				/* NCHARS is 2**n */
#define MAXLIN 512

#define type(v)		(v)->nobj
#define left(v)		(v)->narg[0]
#define right(v)	(v)->narg[1]
#define parent(v)	(v)->nnext

#define LEAF	case CCL: case NCCL: case CHAR: case DOT: case FINAL: case ALL:
#define UNARY	case STAR: case PLUS: case QUEST:

/* encoding in tree Nodes:
	leaf (CCL, NCCL, CHAR, DOT, FINAL, ALL):
		left is index, right contains value or pointer to value
	unary (STAR, PLUS, QUEST): left is child, right is null
	binary (CAT, OR): left and right are children
	parent contains pointer to parent
*/


uchar	chars[MAXLIN];
int	setvec[MAXLIN];
int	tmpset[MAXLIN];
Node	*point[MAXLIN];

int	rtok;		/* next token in current re */
int	rlxval;
uchar	*rlxstr;
uchar	*prestr;	/* current position in current re */
uchar	*lastre;	/* origin of last re */

static	int setcnt;
static	int poscnt;

uchar	*patbeg;
int	patlen;

#define	NFA	20	/* cache this many dynamic fa's */
fa	*fatab[NFA];
int	nfatab	= 0;	/* entries in fatab */

fa *makedfa(uchar *s, int anchor)	/* returns dfa for reg expr s */
{
	int i, use, nuse;
	fa *pfa;

	if (compile_time)	/* a constant for sure */
		return mkdfa(s, anchor);
	for (i = 0; i < nfatab; i++)	/* is it there already? */
		if (fatab[i]->anchor == anchor && strcmp(fatab[i]->restr,s) == 0) {
			fatab[i]->use++;
			return fatab[i];
	}
	pfa = mkdfa(s, anchor);
	if (nfatab < NFA) {	/* room for another */
		fatab[nfatab] = pfa;
		fatab[nfatab]->use = 1;
		nfatab++;
		return pfa;
	}
	use = fatab[0]->use;	/* replace least-recently used */
	nuse = 0;
	for (i = 1; i < nfatab; i++)
		if (fatab[i]->use < use) {
			use = fatab[i]->use;
			nuse = i;
		}
	freefa(fatab[nuse]);
	fatab[nuse] = pfa;
	pfa->use = 1;
	return pfa;
}

fa *mkdfa(uchar *s, int anchor)	/* does the real work of making a dfa */
				/* anchor = 1 for anchored matches, else 0 */
{
	Node *p, *p1;
	fa *f;

	p = reparse(s);
	p1 = op2(CAT, op2(STAR, op2(ALL, NIL, NIL), NIL), p);
		/* put ALL STAR in front of reg.  exp. */
	p1 = op2(CAT, p1, op2(FINAL, NIL, NIL));
		/* put FINAL after reg.  exp. */

	poscnt = 0;
	penter(p1);	/* enter parent pointers and leaf indices */
	if ((f = (fa *) calloc(1, sizeof(fa) + poscnt*sizeof(rrow))) == NULL)
		overflo("out of space for fa");
	f->accept = poscnt-1;	/* penter has computed number of positions in re */
	cfoll(f, p1);	/* set up follow sets */
	freetr(p1);
	if ((f->posns[0] = (int *) calloc(1, *(f->re[0].lfollow)*sizeof(int))) == NULL)
			overflo("out of space in makedfa");
	if ((f->posns[1] = (int *) calloc(1, sizeof(int))) == NULL)
		overflo("out of space in makedfa");
	*f->posns[1] = 0;
	f->initstat = makeinit(f, anchor);
	f->anchor = anchor;
	f->restr = tostring(s);
	return f;
}

int makeinit(fa *f, int anchor)
{
	register int i, k;

	f->curstat = 2;
	f->out[2] = 0;
	f->reset = 0;
	k = *(f->re[0].lfollow);
	xfree(f->posns[2]);			
	if ((f->posns[2] = (int *) calloc(1, (k+1)*sizeof(int))) == NULL)
		overflo("out of space in makeinit");
	for (i=0; i<=k; i++) {
		(f->posns[2])[i] = (f->re[0].lfollow)[i];
	}
	if ((f->posns[2])[1] == f->accept)
		f->out[2] = 1;
	for (i=0; i<NCHARS; i++)
		f->gototab[2][i] = 0;
	f->curstat = cgoto(f, 2, HAT);
	if (anchor) {
		*f->posns[2] = k-1;	/* leave out position 0 */
		for (i=0; i<k; i++) {
			(f->posns[0])[i] = (f->posns[2])[i];
		}

		f->out[0] = f->out[2];
		if (f->curstat != 2)
			--(*f->posns[f->curstat]);
	}
	return f->curstat;
}

void penter(Node *p)	/* set up parent pointers and leaf indices */
{
	switch (type(p)) {
	LEAF
		left(p) = (Node *) poscnt;
		if (poscnt >= MAXLIN)
			overflo("leaf index overflow in penter");
		point[poscnt++] = p;
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
	default:	/* can't happen */
		ERROR "unknown type %d in penter", type(p) FATAL;
		break;
	}
}

void freetr(Node *p)	/* free parse tree */
{
	switch (type(p)) {
	LEAF
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
	default:	/* can't happen */
		ERROR "unknown type %d in freetr", type(p) FATAL;
		break;
	}
}

/* in the parsing of regular expressions, metacharacters like . have */
/* to be seen literally;  \056 is not a metacharacter. */

hexstr(char **pp)	/* find and eval hex string at pp, return new p */
{
	char *p;
	int n = 0;

	for (p = *pp; isxdigit(*p); p++) {
		if (isdigit(*p))
			n = 16 * n + *p - '0';
		else if (*p >= 'a' && *p <= 'f')
			n = 16 * n + *p - 'a' + 10;
		else if (*p >= 'A' && *p <= 'F')
			n = 16 * n + *p - 'A' + 10;
	}
	*pp = p;
	return n;
}

#define isoctdigit(c) ((c) >= '0' && (c) <= '8')	/* multiple use of arg */

quoted(char **pp)	/* pick up next thing after a \\ */
			/* and increment *pp */
{
	char *p = *pp;
	int c;

	if ((c = *p++) == 't')
		c = '\t';
	else if (c == 'n')
		c = '\n';
	else if (c == 'f')
		c = '\f';
	else if (c == 'r')
		c = '\r';
	else if (c == 'b')
		c = '\b';
	else if (c == '\\')
		c = '\\';
	else if (c == 'x') {	/* hexadecimal goo follows */
		c = hexstr(&p);
	} else if (isoctdigit(c)) {	/* \d \dd \ddd */
		int n = c - '0';
		if (isoctdigit(*p)) {
			n = 8 * n + *p++ - '0';
			if (isoctdigit(*p))
				n = 8 * n + *p++ - '0';
		}
		c = n;
	} /* else */
		/* c = c; */
	*pp = p;
	return c;
}

uchar *cclenter(uchar *p)	/* add a character class */
{
	register int i, c, c2;
	uchar *op;

	op = p;
	i = 0;
	while ((c = *p++) != 0) {
		if (c == '\\') {
			c = quoted(&p);
		} else if (c == '-' && i > 0 && chars[i-1] != 0) {
			if (*p != 0) {
				c = chars[i-1];
				c2 = *p++;
				if (c2 == '\\')
					c2 = quoted(&p);
				while (c < c2) {
					if (i >= MAXLIN-1)
						overflo("character class too big");
					chars[i++] = ++c;
				}
				continue;
			}
		}
		if (i >= MAXLIN-1)
			overflo("character class too big");
		chars[i++] = c;
	}
	chars[i++] = '\0';
	dprintf( ("cclenter: in = |%s|, out = |%s|\n", op, chars) );
	xfree(op);
	return(tostring(chars));
}

void overflo(uchar *s)
{
	ERROR "regular expression too big: %.30s...", s FATAL;
}

void cfoll(fa *f, Node *v)	/* enter follow set of each leaf of vertex v into lfollow[leaf] */
{
	register int i;
	register int *p;

	switch (type(v)) {
	LEAF
		f->re[(int) left(v)].ltype = type(v);
		f->re[(int) left(v)].lval = (long) right(v);	/* assumes ptr & long fit */
		for (i=0; i<=f->accept; i++)
			setvec[i] = 0;
		setcnt = 0;
		follow(v);	/* computes setvec and setcnt */
		if ((p = (int *) calloc(1, (setcnt+1)*sizeof(int))) == NULL)
			overflo("out of space building follow set");
		f->re[(int) left(v)].lfollow = p;
		*p = setcnt;
		for (i = f->accept; i >= 0; i--)
			if (setvec[i] == 1) *++p = i;
		break;
	UNARY
		cfoll(f,left(v));
		break;
	case CAT:
	case OR:
		cfoll(f,left(v));
		cfoll(f,right(v));
		break;
	default:	/* can't happen */
		ERROR "unknown type %d in cfoll", type(v) FATAL;
	}
}

first(Node *p)	/* collects initially active leaves of p into setvec */
		/* returns 0 or 1 depending on whether p matches empty string */
{
	register int b;

	switch (type(p)) {
	LEAF
		if (setvec[(int) left(p)] != 1) {
			setvec[(int) left(p)] = 1;
			setcnt++;
		}
		if (type(p) == CCL && (*(uchar *) right(p)) == '\0')
			return(0);		/* empty CCL */
		else return(1);
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
	ERROR "unknown type %d in first", type(p) FATAL;	/* can't happen */
	return(-1);
}

void follow(Node *v)	/* collects leaves that can follow v into setvec */
{
	Node *p;

	if (type(v) == FINAL)
		return;
	p = parent(v);
	switch (type(p)) {
	case STAR:
	case PLUS:
		first(v);
		follow(p);
		return;

	case OR:
	case QUEST:
		follow(p);
		return;

	case CAT:
		if (v == left(p)) {	/* v is left child of p */
			if (first(right(p)) == 0) {
				follow(p);
				return;
			}
		} else		/* v is right child */
			follow(p);
		return;
	}
}

member(int c, uchar *s)	/* is c in s? */
{
	while (*s)
		if (c == *s++)
			return(1);
	return(0);
}

match(fa *f, uchar *p)	/* shortest match ? */
{
	register int s, ns;

	s = f->reset ? makeinit(f,0) : f->initstat;
	if (f->out[s])
		return(1);
	do {
		if (ns=f->gototab[s][*p])
			s = ns;
		else
			s = cgoto(f,s,*p);
		if (f->out[s])
			return(1);
	} while (*p++ != 0);
	return(0);
}

pmatch(fa *f, uchar *p)	/* longest match, for sub */
{
	register int s, ns;
	register uchar *q;
	int i, k;

	s = f->reset ? makeinit(f,1) : f->initstat;
	patbeg = p;
	patlen = -1;
	do {
		q = p;
		do {
			if (f->out[s])		/* final state */
				patlen = q-p;
			if (ns=f->gototab[s][*q])
				s = ns;
			else
				s = cgoto(f,s,*q);
			if (s == 1)	/* no transition */
				if (patlen >= 0) {
					patbeg = p;
					return(1);
				}
				else
					goto nextin;	/* no match */
		} while (*q++ != 0);
		if (f->out[s])
			patlen = q-p-1;	/* don't count $ */
		if (patlen >= 0) {
			patbeg = p;
			return(1);
		}
	nextin:
		s = 2;
		if (f->reset) {
			for (i = 2; i <= f->curstat; i++)
				xfree(f->posns[i]);
			k = *f->posns[0];			
			if ((f->posns[2] = (int *) calloc(1, (k+1)*sizeof(int))) == NULL)
				overflo("out of space in pmatch");
			for (i = 0; i <= k; i++)
				(f->posns[2])[i] = (f->posns[0])[i];
			f->initstat = f->curstat = 2;
			f->out[2] = f->out[0];
			for (i = 0; i < NCHARS; i++)
				f->gototab[2][i] = 0;
		}
	} while (*p++ != 0);
	return (0);
}

nematch(fa *f, uchar *p)	/* non-empty match, for sub */
{
	register int s, ns;
	register uchar *q;
	int i, k;

	s = f->reset ? makeinit(f,1) : f->initstat;
	patlen = -1;
	while (*p) {
		q = p;
		do {
			if (f->out[s])		/* final state */
				patlen = q-p;
			if (ns = f->gototab[s][*q])
				s = ns;
			else
				s = cgoto(f,s,*q);
			if (s == 1)	/* no transition */
				if (patlen > 0) {
					patbeg = p;
					return(1);
				} else
					goto nnextin;	/* no nonempty match */
		} while (*q++ != 0);
		if (f->out[s])
			patlen = q-p-1;	/* don't count $ */
		if (patlen > 0 ) {
			patbeg = p;
			return(1);
		}
	nnextin:
		s = 2;
		if (f->reset) {
			for (i = 2; i <= f->curstat; i++)
				xfree(f->posns[i]);
			k = *f->posns[0];			
			if ((f->posns[2] = (int *) calloc(1, (k+1)*sizeof(int))) == NULL)
				overflo("out of state space");
			for (i = 0; i <= k; i++)
				(f->posns[2])[i] = (f->posns[0])[i];
			f->initstat = f->curstat = 2;
			f->out[2] = f->out[0];
			for (i = 0; i < NCHARS; i++)
				f->gototab[2][i] = 0;
		}
		p++;
	}
	return (0);
}

Node *reparse(uchar *p)	/* parses regular expression pointed to by p */
{			/* uses relex() to scan regular expression */
	Node *np;

	dprintf( ("reparse <%s>\n", p) );
	lastre = prestr = p;	/* prestr points to string to be parsed */
	rtok = relex();
	if (rtok == '\0')
		ERROR "empty regular expression" FATAL;
	np = regexp();
	if (rtok != '\0')
		ERROR "syntax error in regular expression %s at %s", lastre, prestr FATAL;
	return(np);
}

Node *regexp(void)	/* top-level parse of reg expr */
{
	return (alt(concat(primary())));
}

Node *primary(void)
{
	Node *np;

	switch (rtok) {
	case CHAR:
		np = op2(CHAR, NIL, (Node *) rlxval);
		rtok = relex();
		return (unary(np));
	case ALL:
		rtok = relex();
		return (unary(op2(ALL, NIL, NIL)));
	case DOT:
		rtok = relex();
		return (unary(op2(DOT, NIL, NIL)));
	case CCL:
		np = op2(CCL, NIL, (Node*) cclenter(rlxstr));
		rtok = relex();
		return (unary(np));
	case NCCL:
		np = op2(NCCL, NIL, (Node *) cclenter(rlxstr));
		rtok = relex();
		return (unary(np));
	case '^':
		rtok = relex();
		return (unary(op2(CHAR, NIL, (Node *) HAT)));
	case '$':
		rtok = relex();
		return (unary(op2(CHAR, NIL, NIL)));
	case '(':
		rtok = relex();
		if (rtok == ')') {	/* special pleading for () */
			rtok = relex();
			return unary(op2(CCL, NIL, (Node *) tostring("")));
		}
		np = regexp();
		if (rtok == ')') {
			rtok = relex();
			return (unary(np));
		}
		else
			ERROR "syntax error in regular expression %s at %s", lastre, prestr FATAL;
	default:
		ERROR "illegal primary in regular expression %s at %s", lastre, prestr FATAL;
	}
	return 0;	/*NOTREACHED*/
}

Node *concat(Node *np)
{
	switch (rtok) {
	case CHAR: case DOT: case ALL: case CCL: case NCCL: case '$': case '(':
		return (concat(op2(CAT, np, primary())));
	}
	return (np);
}

Node *alt(Node *np)
{
	if (rtok == OR) {
		rtok = relex();
		return (alt(op2(OR, np, concat(primary()))));
	}
	return (np);
}

Node *unary(Node *np)
{
	switch (rtok) {
	case STAR:
		rtok = relex();
		return (unary(op2(STAR, np, NIL)));
	case PLUS:
		rtok = relex();
		return (unary(op2(PLUS, np, NIL)));
	case QUEST:
		rtok = relex();
		return (unary(op2(QUEST, np, NIL)));
	default:
		return (np);
	}
}

relex(void)		/* lexical analyzer for reparse */
{
	register int c;
	uchar cbuf[MAXLIN];
	int clen, cflag;

	switch (c = *prestr++) {
	case '|': return OR;
	case '*': return STAR;
	case '+': return PLUS;
	case '?': return QUEST;
	case '.': return DOT;
	case '\0': prestr--; return '\0';
	case '^':
	case '$':
	case '(':
	case ')':
		return c;
	case '\\':
		rlxval = quoted(&prestr);
		return CHAR;
	default:
		rlxval = c;
		return CHAR;
	case '[': 
		clen = 0;
		if (*prestr == '^') {
			cflag = 1;
			prestr++;
		}
		else
			cflag = 0;
		for ( ; clen < MAXLIN-1; ) {
			if ((c = *prestr++) == '\\') {
				cbuf[clen++] = '\\';
				if ((c = *prestr++) == '\0')
					ERROR "nonterminated character class %.20s...", lastre FATAL;
				cbuf[clen++] = c;
			} else if (c == ']') {
				cbuf[clen] = 0;
				rlxstr = tostring(cbuf);
				if (cflag == 0)
					return CCL;
				else
					return NCCL;
			} else if (c == '\n') {
				ERROR "newline in character class %.20s...", lastre FATAL;
			} else if (c == '\0') {
				ERROR "nonterminated character class %.20s", lastre FATAL;
			} else
				cbuf[clen++] = c;
		}
		if (clen >= MAXLIN-1)
			ERROR "character class %.20s... too long", cbuf FATAL;
	}
	/* can't happen */
	return 0;
}

int cgoto(fa *f, int s, int c)
{
	register int i, j, k;
	register int *p, *q;

	for (i = 0; i <= f->accept; i++)
		setvec[i] = 0;
	setcnt = 0;
	/* compute positions of gototab[s,c] into setvec */
	p = f->posns[s];
	for (i = 1; i <= *p; i++) {
		if ((k = f->re[p[i]].ltype) != FINAL) {
			if (k == CHAR && c == f->re[p[i]].lval
				|| k == DOT && c != 0 && c != HAT
				|| k == ALL && c != 0
				|| k == CCL && member(c, (uchar *) f->re[p[i]].lval)
				|| k == NCCL && !member(c, (uchar *) f->re[p[i]].lval) && c != 0 && c != HAT) {
					q = f->re[p[i]].lfollow;
					for (j = 1; j <= *q; j++) {
						if (setvec[q[j]] == 0) {
							setcnt++;
							setvec[q[j]] = 1;
						}
					}
				}
		}
	}
	/* determine if setvec is a previous state */
	tmpset[0] = setcnt;
	j = 1;
	for (i = f->accept; i >= 0; i--)
		if (setvec[i]) {
			tmpset[j++] = i;
		}
	/* tmpset == previous state? */
	for (i = 1; i <= f->curstat; i++) {
		p = f->posns[i];
		if ((k = tmpset[0]) != p[0])
			goto different;
		for (j = 1; j <= k; j++)
			if (tmpset[j] != p[j])
				goto different;
		/* setvec is state i */
		f->gototab[s][c] = i;
		return i;
	  different:;
	}

	/* add tmpset to current set of states */
	if (f->curstat >= NSTATES-1) {
		f->curstat = 2;
		f->reset = 1;
		for (i = 2; i < NSTATES; i++)
			xfree(f->posns[i]);
	} else
		++(f->curstat);
	for (i = 0; i < NCHARS; i++)
		f->gototab[f->curstat][i] = 0;
	xfree(f->posns[f->curstat]);
	if ((p = (int *) calloc(1, (setcnt+1)*sizeof(int))) == NULL)
		overflo("out of space in cgoto");

	f->posns[f->curstat] = p;
	f->gototab[s][c] = f->curstat;
	for (i = 0; i <= setcnt; i++)
		p[i] = tmpset[i];
	if (setvec[f->accept])
		f->out[f->curstat] = 1;
	else
		f->out[f->curstat] = 0;
	return f->curstat;
}


void freefa(fa *f)	/* free a finite automaton */
{
	register int i;

	if (f == NULL)
		return;
	for (i = 0; i <= f->curstat; i++)
		xfree(f->posns[i]);
	for (i = 0; i <= f->accept; i++) {
		xfree(f->re[i].lfollow);
		if (f->re[i].ltype == CCL || f->re[i].ltype == NCCL)
			xfree(f->re[i].lval);
	}
	xfree(f->restr);
	xfree(f);
}
