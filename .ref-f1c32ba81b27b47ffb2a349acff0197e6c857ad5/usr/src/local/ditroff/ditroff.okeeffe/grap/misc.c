#ifndef lint
static char sccsid[] = "@(#)misc.c	1.1 (CWI) 85/07/19";
#endif lint
#include <stdio.h>
#include "grap.h"
#include "y.tab.h"

int	nnum	= 0;	/* number of saved numbers */
double	num[MAXNUM];

int	just;		/* current justification mode (RJUST, etc.) */
int	sizeop;		/* current optional operator for size change */
double	sizexpr;	/* current size change expression */

savenum(n, f)	/* save f in num[n] */
	int n;
	double f;
{
	num[n] = f;
	nnum = n+1;
	if (nnum >= MAXNUM)
		yyerror("too many numbers");
}

setjust(j)
{
	just |= j;
}

setsize(op, expr)
	int op;
	double expr;
{
	sizeop = op;
	sizexpr = expr;
}

char *tostring(s)
	register char *s;
{
	register char *p;

	p = malloc(strlen(s)+1);
	if (p == NULL)
		fatal("out of space in tostring on %s", s);
	strcpy(p, s);
	return(p);
}

range(pt)	/* update the range for point pt */
	Point pt;
{
	Obj *p = pt.obj;

	if (!(p->coord & XFLAG)) {
		if (pt.x > p->pt1.x)
			p->pt1.x = pt.x;
		if (pt.x < p->pt.x)
			p->pt.x = pt.x;
	}
	if (!(p->coord & YFLAG)) {
		if (pt.y > p->pt1.y)
			p->pt1.y = pt.y;
		if (pt.y < p->pt.y)
			p->pt.y = pt.y;
	}
}

halfrange(p, side, val)	/* record max and min for one direction */
	Obj *p;
	int side;
	double val;
{
	if (!(p->coord&XFLAG) && (side == LEFT || side == RIGHT)) {
		if (val < p->pt.y)
			p->pt.y = val;
		if (val > p->pt1.y)
			p->pt1.y = val;
	} else if (!(p->coord&YFLAG) && (side == TOP || side == BOT)) {
		if (val < p->pt.x)
			p->pt.x = val;
		if (val > p->pt1.x)
			p->pt1.x = val;
	}
}


Obj *lookup(s, inst)	/* find s in objlist, install if inst */
	char *s;
	int inst;
{
	Obj *p;
	int found = 0;

	for (p = objlist; p; p = p->next)
		if (strcmp(s, p->name) == 0) {
			found = 1;
			break;
		}
	if (p == NULL && inst != 0) {
		p = (Obj *) calloc(1, sizeof(Obj));
		if (p == NULL)
			fatal("out of space in lookup");
		p->name = tostring(s);
		p->type = NAME;
		p->pt = ptmax;
		p->pt1 = ptmin;
		p->fval = 0.0;
		p->next = objlist;
		objlist = p;
	}
	dprintf("lookup(%s,%d) = %d\n", s, inst, found);
	return p;
}

double getvar(p)	/* return value of variable */
	Obj *p;
{
	return p->fval;
}

double setvar(p, f)	/* set value of variable to f */
	Obj *p;
	double f;
{
	if (strcmp(p->name, "pointsize") == 0) {	/* kludge */
		pointsize = f;
		ps_set = 1;
	}
	p->type = VARNAME;
	return p->fval = f;
}

Point makepoint(s, x, y)	/* make a Point */
	Obj *s;
	double x, y;
{
	Point p;
	
	dprintf("makepoint: %s, %g,%g\n", s->name, x, y);
	p.obj = s;
	p.x = x;
	p.y = y;
	return p;
}

Attr *makefattr(type, fval)	/* set double in attribute */
	int type;
	double fval;
{
	return makeattr(type, fval, (char *) 0, 0, 0);
}

Attr *makesattr(s, a)		/* make an Attr cell containing s */
	char *s;
	int a;	/* fake */
{
	Attr *ap = makeattr(STRING, sizexpr, s, just, sizeop);
	just = sizeop = 0;
	sizexpr = 0.0;
	return ap;
}

Attr *makeattr(type, fval, sval, just, op)
	int type;
	double fval;
	char *sval;
	int just, op;
{
	Attr *a;

	a = (Attr *) malloc(sizeof(Attr));
	if (a == NULL)
		fatal("out of space in makeattr");
	a->type = type;
	a->fval = fval;
	a->sval = sval;
	a->just = just;
	a->op = op;
	a->next = NULL;
	return a;
}

Attr *addattr(a1, ap)	/* add attr ap to end of list a1 */
	Attr *a1, *ap;
{
	Attr *p;

	if (a1 == 0)
		return ap;
	if (ap == 0)
		return a1;
	for (p = a1; p->next; p = p->next)
		;
	p->next = ap;
	return a1;
}

freeattr(ap)	/* free an attribute list */
	Attr *ap;
{
	Attr *p;

	while (ap) {
		p = ap->next;	/* save next */
		if (ap->sval)
			free(ap->sval);
		free(ap);
		ap = p;
	}
}

char *slprint(stringlist)	/* print strings from stringlist */
	Attr *stringlist;
{
	int ntext, n, last_op, last_just;
	double last_fval;
	static char buf[1000];
	Attr *ap;

	buf[0] = '\0';
	last_op = last_just = 0;
	last_fval = 0.0;
	for (ntext = 0, ap = stringlist; ap != NULL; ap = ap->next)
		ntext++;
	sprintf(buf, "box invis wid 0 ht %d*textht", ntext);
	n = strlen(buf);
	for (ap = stringlist; ap != NULL; ap = ap->next) {
		if (ap->op == 0) {	/* propagate last value */
			ap->op = last_op;
			ap->fval = last_fval;
		} else {
			last_op = ap->op;
			last_fval = ap->fval;
		}
		sprintf(buf+n, " \"%s\"", ps_set || ap->op ? sizeit(ap) : ap->sval);
		if (ap->just)
			last_just = ap->just;
		if (last_just)
			strcat(buf+n, juststr(last_just));
		n = strlen(buf);
	}
	return buf;	/* watch it:  static */
}

char *juststr(j)	/* convert RJUST, etc., into string */
	int j;
{
	static char buf[50];

	buf[0] = '\0';
	if (j & RJUST)
		strcat(buf, " rjust");
	if (j & LJUST)
		strcat(buf, " ljust");
	if (j & ABOVE)
		strcat(buf, " above");
	if (j & BELOW)
		strcat(buf, " below");
	return buf;	/* watch it:  static */
}
