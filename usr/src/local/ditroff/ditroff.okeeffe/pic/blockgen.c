#ifndef lint
static char sccsid[] = "@(#)blockgen.c	1.1 (CWI) 85/07/19";
#endif lint

#include	<stdio.h>
#include	"pic.h"
#include	"y.tab.h"


struct pushstack stack[20];
int	nstack	= 0;

struct obj *leftthing(c)	/* called for {... or [... */
	int c;
{
	struct obj *p;

	stack[nstack].p_x = curx;
	stack[nstack].p_y = cury;
	stack[nstack].p_hvmode = hvmode;
	if (c == '[') {
		curx = cury = 0;
		stack[nstack].p_xmin = xmin;
		stack[nstack].p_xmax = xmax;
		stack[nstack].p_ymin = ymin;
		stack[nstack].p_ymax = ymax;
		xmin = ymin = 30000;
		xmax = ymax = -30000;
		p = makenode(BLOCK, 7);
		p->o_val[4] = nobj;	/* 1st item within [...] */
		if (p->o_nobj != nobj-1)
			fprintf(stderr, "nobjs wrong%d %d\n", p->o_nobj, nobj);
	}
	else
		p = NULL;
	nstack++;
	return(p);
}

struct obj *rightthing(p, c)	/* called for ... ] or ... } */
	struct obj *p;
{
	struct obj *q;

	nstack--;
	curx = stack[nstack].p_x;
	cury = stack[nstack].p_y;
	hvmode = stack[nstack].p_hvmode;
	if (c == '}') {
		q = makenode(MOVE, 0);
		dprintf("M %g %g\n", curx, cury);
	} else {
		q = makenode(BLOCKEND, 7);
		q->o_val[4] = p->o_nobj + 1;	/* back pointer */
		p->o_val[5] = q->o_nobj - 1;	/* forward pointer */
		p->o_val[0] = xmin; p->o_val[1] = ymin;
		p->o_val[2] = xmax; p->o_val[3] = ymax;
		p->o_dotdash = q->o_dotdash = (int) stack[nstack+1].p_symtab;
		xmin = stack[nstack].p_xmin;
		ymin = stack[nstack].p_ymin;
		xmax = stack[nstack].p_xmax;
		ymax = stack[nstack].p_ymax;
	}
	return(q);
}

struct obj *blockgen(p, type, q)	/* handles [...] */
	struct obj *p, *q;
	int type;
{
	static float prevh = HT;
	static float prevw = WID;	/* golden mean, sort of */
	int i, invis, at, ddtype;
	float ddval;
	int with;
	float h, w, xwith, ywith;
	float x0, y0, x1, y1, cx, cy;
	struct obj *ppos;

	invis = at = 0;
	with = xwith = ywith = 0;
	ddtype = ddval = 0;
	w = p->o_val[2] - p->o_val[0];
	h = p->o_val[3] - p->o_val[1];
	cx = (p->o_val[2] + p->o_val[0]) / 2;	/* geom ctr of [] wrt local orogin */
	cy = (p->o_val[3] + p->o_val[1]) / 2;
	dprintf("cx,cy=%g,%g\n", cx, cy);
	for (i = 0; i < nattr; i++) {
		switch (attr[i].a_type) {
		case HEIGHT:
			h = attr[i].a_val.f;
			break;
		case WIDTH:
			w = attr[i].a_val.f;
			break;
		case SAME:
			h = prevh;
			w = prevw;
			break;
		case WITH:
			with = attr[i].a_val.i;	/* corner */
			break;
		case PLACE:	/* actually with position ... */
			ppos = attr[i].a_val.o;
			xwith = cx - ppos->o_x;
			ywith = cy - ppos->o_y;
			with = PLACE;
			break;
		case AT:
			ppos = attr[i].a_val.o;
			curx = ppos->o_x;
			cury = ppos->o_y;
			at++;
			break;
		case INVIS:
			invis = INVIS;
			break;
		case LJUST: case RJUST: case CENTER: case SPREAD: case FILL: case ABOVE: case BELOW:
			savetext(attr[i].a_type, attr[i].a_val.p);
			break;
		}
	}
	if (with) {
		switch (with) {
		case NORTH:	ywith = -h / 2; break;
		case SOUTH:	ywith = h / 2; break;
		case EAST:	xwith = -w / 2; break;
		case WEST:	xwith = w / 2; break;
		case NE:	xwith = -w / 2; ywith = -h / 2; break;
		case SE:	xwith = -w / 2; ywith = h / 2; break;
		case NW:	xwith = w / 2; ywith = -h / 2; break;
		case SW:	xwith = w / 2; ywith = h / 2; break;
		}
		curx += xwith;
		cury += ywith;
	}
	if (!at) {
		if (isright(hvmode))
			curx += w / 2;
		else if (isleft(hvmode))
			curx -= w / 2;
		else if (isup(hvmode))
			cury += h / 2;
		else
			cury -= h / 2;
	}
	x0 = curx - w / 2;
	y0 = cury - h / 2;
	x1 = curx + w / 2;
	y1 = cury + h / 2;
	extreme(x0, y0);
	extreme(x1, y1);
	p->o_x = curx;
	p->o_y = cury;
	p->o_nt1 = ntext1;
	p->o_nt2 = ntext;
	ntext1 = ntext;
	p->o_val[0] = w;
	p->o_val[1] = h;
	p->o_val[2] = cx;
	p->o_val[3] = cy;
	p->o_val[5] = q->o_nobj - 1;		/* last item in [...] */
	p->o_ddval = ddval;
	p->o_attr = invis;
	dprintf("[] %g %g %g %g at %g %g, h=%g, w=%g\n", x0, y0, x1, y1, curx, cury, h, w);
	if (isright(hvmode))
		curx = x1;
	else if (isleft(hvmode))
		curx = x0;
	else if (isup(hvmode))
		cury = y1;
	else
		cury = y0;
	for (i = 0; i <= 5; i++)
		q->o_val[i] = p->o_val[i];
	stack[nstack+1].p_symtab = NULL;	/* so won't be found again */
	blockadj(p);	/* fix up coords for enclosed blocks */
	prevh = h;
	prevw = w;
	return(p);
}

blockadj(p)	/* adjust coords in block starting at p */
	struct obj *p;
{
	struct obj *q;
	float dx, dy;
	int n, lev;

	dx = p->o_x - p->o_val[2];
	dy = p->o_y - p->o_val[3];
	n = p->o_nobj + 1;
	q = objlist[n];
	dprintf("into blockadj: dx,dy=%g,%g\n", dx, dy);
	for (lev = 1; lev > 0; n++) {
		p = objlist[n];
		if (p->o_type == BLOCK)
			lev++;
		else if (p->o_type == BLOCKEND)
			lev--;
		dprintf("blockadj: type=%d o_x,y=%g,%g;", p->o_type, p->o_x, p->o_y);
		p->o_x += dx;
		p->o_y += dy;
		dprintf(" becomes %g,%g\n", p->o_x, p->o_y);
		switch (p->o_type) {	/* other absolute coords */
		case LINE:
		case ARROW:
		case SPLINE:
			p->o_val[0] += dx;
			p->o_val[1] += dy;
			break;
		case ARC:
			p->o_val[0] += dx;
			p->o_val[1] += dy;
			p->o_val[2] += dx;
			p->o_val[3] += dy;
			break;
		}
	}
}
