#ifndef lint
static char sccsid[] = "@(#)circgen.c	3.1 (CWI) 85/07/30";
#endif lint

#include	<stdio.h>
#include	"pic.h"
#include	"y.tab.h"

obj *circgen(type)
{
	static float rad[2] = { HT2, WID2 };
	static float rad2[2] = { HT2, HT2 };
	static float x0, y0, x1, y1, x2, y2;
	int i, at, t, invis, ddtype, with;
	float xwith, ywith;
	float r, r2, ddval;
	obj *p, *ppos;
	Attr *ap;

	at = invis = ddtype = 0;
	with = xwith = ywith = 0;
	t = (type == CIRCLE) ? 0 : 1;
	if (type == CIRCLE)
		r = r2 = getfval("circlerad");
	else if (type == ELLIPSE) {
		r = getfval("ellipsewid") / 2;
		r2 = getfval("ellipseht") / 2;
	}
	for (i = 0; i < nattr; i++) {
		ap = &attr[i];
		switch (ap->a_type) {
		case TEXTATTR:
			savetext(ap->a_sub, ap->a_val.p);
			break;
		case RADIUS:
			r = ap->a_val.f;
			break;
		case DIAMETER:
		case WIDTH:
			r = ap->a_val.f / 2;
			break;
		case HEIGHT:
			r2 = ap->a_val.f / 2;
			break;
		case SAME:
			r = rad[t];
			r2 = rad2[t];
			break;
		case WITH:
			with = ap->a_val.i;
			break;
		case AT:
			ppos = ap->a_val.o;
			curx = ppos->o_x;
			cury = ppos->o_y;
			at++;
			break;
		case INVIS:
			invis = INVIS;
			break;
		case DOT:
		case DASH:
			ddtype = ap->a_type==DOT ? DOTBIT : DASHBIT;
			if (ap->a_sub == DEFAULT)
				ddval = getfval("dashwid");
			else
				ddval = ap->a_val.f;
			break;
		}
	}
	if (type == CIRCLE)
		r2 = r;	/* probably superfluous */
	if (with) {
		switch (with) {
		case NORTH:	ywith = -r2; break;
		case SOUTH:	ywith = r2; break;
		case EAST:	xwith = -r; break;
		case WEST:	xwith = r; break;
		case NE:	xwith = -r * 0.707; ywith = -r2 * 0.707; break;
		case SE:	xwith = -r * 0.707; ywith = r2 * 0.707; break;
		case NW:	xwith = r * 0.707; ywith = -r2 * 0.707; break;
		case SW:	xwith = r * 0.707; ywith = r2 * 0.707; break;
		}
		curx += xwith;
		cury += ywith;
	}
	if (!at) {
		if (isright(hvmode))
			curx += r;
		else if (isleft(hvmode))
			curx -= r;
		else if (isup(hvmode))
			cury += r2;
		else
			cury -= r2;
	}
	p = makenode(type, 2);
	p->o_val[0] = rad[t] = r;
	p->o_val[1] = rad2[t] = r2;
	if (r <= 0 || r2 <= 0) {
		yyerror("%s has invalid radius %g\n", (type==CIRCLE) ? "circle" : "ellipse", r<r2 ? r : r2);
	}
	p->o_attr = invis | ddtype;
	extreme(curx+r, cury+r2);
	extreme(curx-r, cury-r2);
	if (type == CIRCLE)
		dprintf("C %g %g %g\n", curx, cury, r);
	if (type == ELLIPSE)
		dprintf("E %g %g %g %g\n", curx, cury, r, r2);
	if (isright(hvmode))
		curx += r;
	else if (isleft(hvmode))
		curx -= r;
	else if (isup(hvmode))
		cury += r2;
	else
		cury -= r2;
	return(p);
}
