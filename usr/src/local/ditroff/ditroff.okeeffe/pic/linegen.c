#ifndef lint
static char sccsid[] = "@(#)linegen.c	3.1 (CWI) 85/07/30";
#endif lint

#include	<stdio.h>
#include	"pic.h"
#include	"y.tab.h"

obj *linegen(type)
{
	static float prevdx = HT;
	static float prevdy = 0;
	static float prevw = HT10;
	static float prevh = HT5;
	int i, j, some, head, ddtype, invis, chop;
	float ddval, chop1, chop2, x0, y0, x1, y1;
	double sin(), cos(), atan2(), theta;
	float defx, defy;
	obj *p, *ppos;
	static int xtab[] = { 1, 0, -1, 0 };	/* R=0, U=1, L=2, D=3 */
	static int ytab[] = { 0, 1, 0, -1 };
	float dx[50], dy[50];
	int ndxy;
	float nx, ny;
	Attr *ap;

	nx = curx;
	ny = cury;
	defx = getfval("linewid");
	defy = getfval("lineht");
	prevh = getfval("arrowht");
	prevw = getfval("arrowwid");
	dx[0] = dy[0] = ndxy = some = head = invis = 0;
	chop = chop1 = chop2 = 0;
	ddtype = ddval = 0;
	for (i = 0; i < nattr; i++) {
		ap = &attr[i];
		switch (ap->a_type) {
		case TEXTATTR:
			savetext(ap->a_sub, ap->a_val.p);
			break;
		case HEAD:
			head += ap->a_val.i;
			break;
		case INVIS:
			invis = INVIS;
			break;
		case CHOP:
			if (chop++ == 0)
				chop1 = chop2 = ap->a_val.f;
			else
				chop2 = ap->a_val.f;
			break;
		case DOT:
		case DASH:
			ddtype = ap->a_type==DOT ? DOTBIT : DASHBIT;
			if (ap->a_sub == DEFAULT)
				ddval = getfval("dashwid");
			else
				ddval = ap->a_val.f;
			break;
		case SAME:
			dx[ndxy] = prevdx;
			dy[ndxy] = prevdy;
			some++;
			break;
		case LEFT:
			dx[ndxy] -= (ap->a_sub==DEFAULT) ? defx : ap->a_val.f;
			some++;
			hvmode = L_DIR;
			break;
		case RIGHT:
			dx[ndxy] += (ap->a_sub==DEFAULT) ? defx : ap->a_val.f;
			some++;
			hvmode = R_DIR;
			break;
		case UP:
			dy[ndxy] += (ap->a_sub==DEFAULT) ? defy : ap->a_val.f;
			some++;
			hvmode = U_DIR;
			break;
		case DOWN:
			dy[ndxy] -= (ap->a_sub==DEFAULT) ? defy : ap->a_val.f;
			some++;
			hvmode = D_DIR;
			break;
		case HEIGHT:	/* length of arrowhead */
			prevh = ap->a_val.f;
			break;
		case WIDTH:	/* width of arrowhead */
			prevw = ap->a_val.f;
			break;
		case TO:
			if (some) {
				nx += dx[ndxy];
				ny += dy[ndxy];
				ndxy++;
				dx[ndxy] = dy[ndxy] = some = 0;
			}
			ppos = attr[i].a_val.o;
			dx[ndxy] = ppos->o_x - nx;
			dy[ndxy] = ppos->o_y - ny;
			some++;
			break;
		case BY:
			if (some) {
				nx += dx[ndxy];
				ny += dy[ndxy];
				ndxy++;
				dx[ndxy] = dy[ndxy] = some = 0;
			}
			ppos = ap->a_val.o;
			dx[ndxy] = ppos->o_x;
			dy[ndxy] = ppos->o_y;
			some++;
			break;
		case THEN:	/* turn off any previous accumulation */
			if (some) {
				nx += dx[ndxy];
				ny += dy[ndxy];
				ndxy++;
				dx[ndxy] = dy[ndxy] = some = 0;
			}
			break;
		case FROM:
		case AT:
			ppos = ap->a_val.o;
			nx = curx = ppos->o_x;
			ny = cury = ppos->o_y;
			break;
		}
	}
	if (some) {
		nx += dx[ndxy];
		ny += dy[ndxy];
		ndxy++;
		defx = dx[ndxy-1];
		defy = dy[ndxy-1];
	} else {
		defx *= xtab[hvmode];
		defy *= ytab[hvmode];
		dx[ndxy] = defx;
		dy[ndxy] = defy;
		ndxy++;
		nx += defx;
		ny += defy;
	}
	prevdx = defx;
	prevdy = defy;
	if (chop) {
		if (chop == 1 && chop1 == 0)	/* just said "chop", so use default */
			chop1 = chop2 = getfval("circlerad");
		theta = atan2(dy[0], dx[0]);
		x0 = chop1 * cos(theta);
		y0 = chop1 * sin(theta);
		curx += x0;
		cury += y0;
		dx[0] -= x0;
		dy[0] -= y0;

		theta = atan2(dy[ndxy-1], dx[ndxy-1]);
		x1 = chop2 * cos(theta);
		y1 = chop2 * sin(theta);
		nx -= x1;
		ny -= y1;
		dx[ndxy-1] -= x1;
		dy[ndxy-1] -= y1;
		dprintf("chopping %g %g %g %g; cur=%g,%g end=%g,%g\n",
			x0, y0, x1, y1, curx, cury, nx, ny);
	}
	p = makenode(type, 5 + 2 * ndxy);
	curx = p->o_val[0] = nx;
	cury = p->o_val[1] = ny;
	if (head || type == ARROW) {
		p->o_nhead = getfval("arrowhead");
		p->o_val[2] = prevw;
		p->o_val[3] = prevh;
		if (head == 0)
			head = HEAD2;	/* default arrow head */
	}
	p->o_attr = head | invis | ddtype;
	p->o_val[4] = ndxy;
	nx = p->o_x;
	ny = p->o_y;
	for (i = 0, j = 5; i < ndxy; i++, j += 2) {
		p->o_val[j] = dx[i];
		p->o_val[j+1] = dy[i];
		if (type == LINE)
			extreme(nx += dx[i], ny += dy[i]);
		else if (type == SPLINE && i < ndxy-1) {
			/* to compute approx extreme of spline at p,
			/* compute midway between p-1 and p+1,
			/* then go 3/4 from there to p */
			float ex, ey, xi, yi, xi1, yi1;
			xi = nx + dx[i]; yi = ny + dy[i];	/* p */
			xi1 = xi + dx[i+1]; yi1 = yi + dy[i+1];	/* p+1 */
			ex = (nx+xi1)/2; ey = (ny+yi1)/2;	/* midway */
			ex += 0.75*(xi-ex); ey += 0.75*(yi-ey);
			extreme(ex, ey);
			nx = xi; ny = yi;
		}
			
	}
	p->o_ddval = ddval;
	if (dbg) {
		printf("S or L from %g %g to %g %g with %d elements:\n", p->o_x, p->o_y, curx, cury, ndxy);
		for (i = 0, j = 5; i < ndxy; i++, j += 2)
			printf("%g %g\n", p->o_val[j], p->o_val[j+1]);
	}
	extreme(p->o_x, p->o_y);
	extreme(curx, cury);
	return(p);
}
