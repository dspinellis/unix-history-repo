/* Copyright (c) 1979 Regents of the University of California */
#include "sh.h"

/*
 * C Shell
 */

doset(v)
	register char **v;
{
	register char *p;
	char *vp, op;
	bool hadsub;
	int subscr;

	v++;
	p = *v++;
	if (p == 0) {
		prvars();
		return;
	}
	do {
		hadsub = 0;
		for (vp = p; letter(*p); p++)
			continue;
		if (vp == p)
			goto setsyn;
		if (*p == '[') {
			hadsub++;
			p = getinx(p, &subscr);
		}
		if (op = *p) {
			*p++ = 0;
			if (*p == 0 && *v && **v == '(')
				p = *v++;
		} else if (*v && eq(*v, "=")) {
			op = '=', v++;
			if (*v)
				p = *v++;
		}
		if (op && op != '=')
setsyn:
			bferr("Syntax error");
		if (eq(p, "(")) {
			register char **e = v;

			if (hadsub)
				goto setsyn;
			for (;;) {
				if (!*e)
					bferr("Missing )");
				if (**e == ')')
					break;
				e++;
			}
			p = *e, *e = 0, set1(vp, saveblk(v), &shvhed), *e = p;
			v = e + 1;
		} else if (hadsub)
			asx(vp, subscr, savestr(p));
		else
			set(vp, savestr(p));
	} while (p = *v++);
}

char *
getinx(cp, ip)
	register char *cp;
	register int *ip;
{

	*ip = 0;
	*cp++ = 0;
	while (*cp && digit(*cp))
		*ip = *ip * 10 + *cp++ - '0';
	if (*cp++ != ']')
		bferr("Subscript error");
	return (cp);
}

asx(vp, subscr, p)
	char *vp;
	int subscr;
	char *p;
{
	register struct varent *v = getvx(vp, subscr);

	xfree(v->vec[subscr - 1]);
	v->vec[subscr - 1] = globone(p);
}

struct varent *
getvx(vp, subscr)
{
	register struct varent *v = adrof(vp);

	if (v == 0)
		udvar(vp);
	if (subscr < 1 || subscr > blklen(v->vec))
		bferr("Subscript out of range");
	return (v);
}

char	plusplus[2] = { '1', 0 };


dolet(v)
	char **v;
{
	register char *p;
	char *vp, c, op;
	bool hadsub;
	int subscr;

	v++;
	p = *v++;
	if (p == 0) {
		prvars();
		return;
	}
	do {
		hadsub = 0;
		for (vp = p; letter(*p); p++)
			continue;
		if (vp == p)
			goto letsyn;
		if (*p == '[') {
			hadsub++;
			p = getinx(p, &subscr);
		}
		if (*p == 0 && *v)
			p = *v++;
		if (op = *p)
			*p++ = 0;
		else
			goto letsyn;
		vp = savestr(vp);
		if (op == '=') {
			c = '=';
			p = xset(p, &v);
		} else {
			c = *p++;
			if (any(c, "+-")) {
				if (c != op || *p)
					goto letsyn;
				p = plusplus;
			} else {
				if (any(op, "<>")) {
					if (c != op)
						goto letsyn;
					c = *p++;
letsyn:
					bferr("Syntax error");
				}
				if (c != '=')
					goto letsyn;
				p = xset(p, &v);
			}
		}
		if (op == '=')
			if (hadsub)
				asx(vp, subscr, p);
			else
				set(vp, p);
		else
			if (hadsub)
#ifndef V6
				/* avoid bug in vax CC */
				{
					struct varent *gv = getvx(vp, subscr);

					asx(vp, subscr, operate(op, gv->vec[subscr - 1], p));
				}
#else
				asx(vp, subscr, operate(op, getvx(vp, subscr)->vec[subscr - 1], p));
#endif
			else
				set(vp, operate(op, value(vp), p));
		xfree(vp);
		if (c != '=')
			xfree(p);
	} while (p = *v++);
}

char *
xset(cp, vp)
	char *cp, ***vp;
{
	register char *dp;

	if (*cp) {
		dp = savestr(cp);
		--(*vp);
		xfree(**vp);
		**vp = dp;
	}
	return (putn(exp(vp)));
}

char *
operate(op, vp, p)
	char op, *vp, *p;
{
	char opr[2];
	char *vec[5];
	register char **v = vec;
	char **vecp = v;
	register int i;

	if (op != '=') {
		if (*vp)
			*v++ = vp;
		opr[0] = op;
		opr[1] = 0;
		*v++ = opr;
		if (op == '<' || op == '>')
			*v++ = opr;
	}
	*v++ = p;
	*v++ = 0;
	i = exp(&vecp);
	if (*vecp)
		bferr("Expression syntax");
	return (putn(i));
}

onlyread(cp)
	char *cp;
{
	extern char end[];

	return (cp < end);
}

xfree(cp)
	char *cp;
{
	extern char end[];

	if (cp >= end && cp < (char *) &cp)
		cfree(cp);
}

char *
savestr(s)
	register char *s;
{

	if (s == 0)
		s = "";
	return (strcpy(calloc(1, strlen(s) + 1), s));
}

static	char *putp;
 
char *
putn(n)
	register int n;
{
	static char number[15];

	putp = number;
	if (n < 0) {
		n = -n;
		*putp++ = '-';
	}
	if (sizeof (int) == 2 && n == -32768) {
		*putp++ = '3';
		n = 2768;
#ifdef pdp11
	}
#else
	} else if (sizeof (int) == 4 && n == -2147483648) {
		*putp++ = '2';
		n = 147483648;
	}
#endif
	putn1(n);
	*putp = 0;
	return (savestr(number));
}

putn1(n)
	register int n;
{
	if (n > 9)
		putn1(n / 10);
	*putp++ = n % 10 + '0';
}

getn(cp)
	register char *cp;
{
	register int n;
	int sign;

	sign = 0;
	if (cp[0] == '+' && cp[1])
		cp++;
	if (*cp == '-') {
		sign++;
		cp++;
		if (!digit(*cp))
			goto badnum;
	}
	n = 0;
	while (digit(*cp))
		n = n * 10 + *cp++ - '0';
	if (*cp)
		goto badnum;
	return (sign ? -n : n);
badnum:
	bferr("Badly formed number");
	return (0);
}

char *
value(var)
	char *var;
{

	return (value1(var, &shvhed));
}

char *
value1(var, head)
	char *var;
	struct varent *head;
{
	register struct varent *vp;

	vp = adrof1(var, head);
	return (vp == 0 || vp->vec[0] == 0 ? "" : vp->vec[0]);
}

static	struct varent *shprev;

struct varent *
adrof(var)
	char *var;
{

	return (adrof1(var, &shvhed));
}

struct varent *
madrof(pat, head)
	char *pat;
	struct varent *head;
{
	register struct varent *vp;

	shprev = head;
	for (vp = shprev->link; vp != 0; vp = vp->link) {
		if (Gmatch(vp->name, pat))
			return (vp);
		shprev = vp;
	}
	return (0);
}

struct varent *
adrof1(var, head)
	char *var;
	struct varent *head;
{
	register struct varent *vp;
	int cmp;

	shprev = head;
	for (vp = shprev->link; vp != 0; vp = vp->link) {
		cmp = strcmp(vp->name, var);
		if (cmp == 0)
			return (vp);
		else if (cmp > 0)
			return (0);
		shprev = vp;
	}
	return (0);
}

/*
 * The caller is responsible for putting value in a safe place
 */
set(var, value)
	char *var, *value;
{
	register char **vec = (char **) calloc(2, sizeof (char **));

	vec[0] = onlyread(value) ? savestr(value) : value;
	set1(var, vec, &shvhed);
}

set1(var, vec, head)
	char *var, **vec;
	struct varent *head;
{

	register char **oldv = vec;

	gflag = 0; rscan(oldv, tglob);
	if (gflag) {
		vec = glob(oldv);
		if (vec == 0) {
			bferr("No match");
			blkfree(oldv);
			return;
		}
		blkfree(oldv);
		gargv = 0;
	}
	setq(var, vec, head);
}

setq(var, vec, head)
	char *var, **vec;
	struct varent *head;
{
	register struct varent *vp;

	vp = adrof1(var, head);
	if (vp == 0) {
		vp = (struct varent *) calloc(1, sizeof *vp);
		vp->name = savestr(var);
		vp->link = shprev->link;
		shprev->link = vp;
	}
	if (vp->vec)
		blkfree(vp->vec);
	scan(vec, trim);
	vp->vec = vec;
}

unset(v)
	register char *v[];
{

	unset1(v, &shvhed);
}

unset1(v, head)
	register char *v[];
	struct varent *head;
{
	register char *var;
	register struct varent *vp;
	register int cnt;

	v++;
	while (var = *v++) {
		cnt = 0;
		while (vp = madrof(var, head))
			unsetv1(vp->name, head), cnt++;
/*
		if (cnt == 0)
			setname(var), bferr("No match");
*/
	}
}

unsetv(var)
	char *var;
{

	unsetv1(var, &shvhed);
}

unsetv1(var, head)
	char *var;
	struct varent *head;
{
	register struct varent *vp;

	vp = adrof1(var, head);
	if (vp == 0)
		udvar(var);
	vp = shprev->link;
	shprev->link = vp->link;
	blkfree(vp->vec);
	xfree(vp->name);
	xfree(vp);
}

setNS(cp)
	char *cp;
{

	set(cp, "");
}

shift(v)
	register char **v;
{
	register struct varent *argv;
	register char *name;

	v++;
	name = *v;
	if (name == 0)
		name = "argv";
	else
		strip(name);
	argv = adrof(name);
	if (argv == 0)
		udvar(name);
	if (argv->vec[0] == 0)
		bferr("No more words");
	lshift(argv->vec, 1);
}

deletev(cp)
	register char *cp;
{

	if (adrof(cp))
		unsetv(cp);
}
