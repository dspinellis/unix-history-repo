#include "sh.h"

/*
 * syntax
 *	empty
 *	syn0
 */
syntax(p1, p2)
	register struct shvar2 *p1, *p2;
{

	while (p1 != p2)
		if (any(p1->value[0], ";&\n"))
			p1 = p1->next;
		else
			return (syn0(p1, p2));
	return (0);
}

/*
 * syn0
 *	syn1
 *	syn1 & syntax
 */
syn0(p1, p2)
	struct shvar2 *p1, *p2;
{
	register struct shvar2 *p;
	register *t, *t1;
	int l;

	l = 0;
	for (p = p1; p != p2; p = p->next)
		switch (p->value[0]) {
			case '(':
				l++;
				continue;
			case ')':
				l--;
				if (l < 0)
					seterr("Too many )'s");
				continue;
			case '&':
				if (l != 0)
					break;
				t1 = syn1(p1, p);
				if (t1[DTYP] == TLST) {
					t = calloc(2, 5);
					t[DTYP] = TPAR;
					t[DFLG] = FAND|FPRS|FINT;
					t[DSPR] = t1;
					t1 = t;
				} else
					t1[DFLG] =| FAND|FPRS|FINT;
				t = calloc(2, 4);
				t[DTYP] = TLST;
				t[DFLG] = 0;
				t[DLEF] = t1;
				t[DRIT] = syntax(p, p2);
				return(t);
		}
	if (l == 0)
		return (syn1(p1, p2));
	seterr("Too many ('s");
	return(0);
}

/*
 * syn1
 *	syn2
 *	syn2 ; syn1
 */
syn1(p1, p2)
	struct shvar2 *p1, *p2;
{
	register struct shvar *p;
	register *t;
	int l;

	l = 0;
	for (p = p1; p != p2; p = p->next)
		switch (p->value[0]) {
			case '(':
				l++;
				continue;
			case ')':
				l--;
				continue;
			case ';':
			case '\n':
				if (l != 0)
					break;
				t = calloc(2, 4);
				t[DTYP] = TLST;
				t[DLEF] = syn2(p1, p);
				t[DRIT] = syntax(p->next, p2);
				return (t);
		}
	return (syn2(p1, p2));
}

/*
 * syn2
 *	syn3
 *	syn3 | syn2
 *	syn3 |* syn2
 */
syn2(p1, p2)
	struct shvar2 *p1, *p2;
{
	register struct shvar2 *p, *pn;
	register int *t;
	int l;

	l = 0;
	for (p = p1; p != p2; p = p->next)
		switch (p->value[0]) {
			case '(':
				l++;
				continue;
			case ')':
				l--;
				continue;
			case '|':
			case '^':
				if (l != 0)
					continue;
				t = calloc(2, 4);
				t[DTYP] = TFIL;
				t[DLEF] = syn3(p1, p);
				pn = p->next;
				if (pn != p2 && pn->value[0] == '*') {
					t[DFLG] = FDIAG;
					if (pn->value[1] != 0)
						pn->value++;
					else
						p = pn;
				}
				t[DRIT] = syn2(p->next, p2);
				return (t);
		}
	return (syn3(p1, p2));
}

/*
 * syn3
 *	( syn0 ) [ < in  ] [ > out ]
 *	word word* [ < in ] [ > out ]
 */
syn3(p1, p2)
	struct shvar2 *p1, *p2;
{
	register struct shvar2 *p;
	struct shvar *lp, *rp;
	register int *t;
	int n, l, c;

	n = 0;
	l = 0;
	for (p = p1; p != p2; p = p->next)
		switch (p->value[0]) {
			case '(':
				l++;
				continue;
			case ')':
				l--;
				continue;
			case '>':
			case '<':
				if (l != 0)
					continue;
				if (p->next == p2)
					continue;
				if (any(p->next->value[0], "<>(*"))
					continue;
				n--;
				continue;
			default:
				if (l != 0)
					continue;
				n++;
				continue;
		}
	if (n < 0)
		n = 0;
	t = calloc(2, 5 + n + 1);
	n = 0;
	if (p2->value[0] == ')')
		t[DFLG] = FPAR;
	lp = 0;
	rp = 0;
	l = 0;
	for (p = p1; p != p2; p = p->next) {
		c = p->value[0];
		switch (c) {
			case '(':
				if (l == 0) {
					if (lp != 0)
						seterr("Badly placed (");
					lp = p->next;
				}
				l++;
				continue;
			case ')':
				l--;
				if (l == 0)
					rp = p;
				continue;
			case '>':
				if (l != 0)
					continue;
				if (p->next != p2) {
					p = p->next;
					if (p->value[0] == '>')
						t[DFLG] =| FCAT;
					else
						p = p->prev;
				}
				if (p->next != p2) {
					p = p->next;
					if (p->value[0] == '*') {
						t[DFLG] =| FDIAG;
						if (p->value[1]) {
							p->value++;
							p = p->prev;
						}
					} else
						p = p->prev;
				}
			case '<':
				if (l != 0)
					continue;
				if (p->next == p2) {
					seterr("Missing file for redirect");
					continue;
				}
				p = p->next;
				if (any(p->value[0], "<>(")) {
					seterr("Syntax error in redirection");
					continue;
				}
				if (c == '<') {
					if (t[DLEF] != 0) {
						seterr("Multiple < redirect");
						continue;
					}
					t[DLEF] = p->value;
					continue;
				}
				if (t[DRIT] != 0) {
					seterr("Multiple output redirect");
					continue;
				}
				t[DRIT] = p->value;
				continue;
			default:
				if (l != 0)
					continue;
				t[DCOM + n] = p->value;
				n++;
				continue;
		}
	}
	if (lp != 0) {
		if (n != 0)
			seterr("Only redirect allowed outside ()'s");
		t[DTYP] = TPAR;
		t[DSPR] = syn0(lp, rp);
	} else {
		if (n == 0)
			seterr("Inappropriate empty command");
		t[DCOM + n] = 0;
		t[DTYP] = TCOM;
	}
	return (t);
}

freesyn(t)
	register int *t;
{
	register char **v;

	if (t == 0)
		return;
	switch (t[DTYP]) {
		case TCOM:
			for (v = &t[DCOM]; *v; v++)
				xfree(*v);
			goto lr;
		case TPAR:
			freesyn(t[DSPR]);
lr:
			xfree(t[DLEF]);
			xfree(t[DRIT]);
			break;
		case TFIL:
		case TLST:
			freesyn(t[DLEF]);
			freesyn(t[DRIT]);
			break;
	}
	cfree(t);
}
