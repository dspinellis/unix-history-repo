#ifndef lint
static char sccsid[] = "@(#)misc.c	1.1 (CWI) 85/07/19";
#endif lint

#include	<stdio.h>
#include	"pic.h"
#include	"y.tab.h"

setdir(n)	/* set direction from n */
	int n;
{
	switch (n) {
	case UP:	hvmode = U_DIR; break;
	case DOWN:	hvmode = D_DIR; break;
	case LEFT:	hvmode = L_DIR; break;
	case RIGHT:	hvmode = R_DIR; break;
	}
	return(hvmode);
}

float getcomp(p, t)	/* return component of a position */
	struct obj *p;
	int t;
{
	switch (t) {
	case DOTX:
		return p->o_x;
	case DOTY:
		return p->o_y;
	case DOTWID:
		switch (p->o_type) {
		case BOX:
		case BLOCK:
			return p->o_val[0];
		case CIRCLE:
		case ELLIPSE:
			return 2 * p->o_val[0];
		case LINE:
		case ARROW:
			return p->o_val[0] - p->o_x;
		}
	case DOTHT:
		switch (p->o_type) {
		case BOX:
		case BLOCK:
			return p->o_val[1];
		case CIRCLE:
		case ELLIPSE:
			return 2 * p->o_val[1];
		case LINE:
		case ARROW:
			return p->o_val[1] - p->o_y;
		}
	case DOTRAD:
		switch (p->o_type) {
		case CIRCLE:
		case ELLIPSE:
			return p->o_val[0];
		}
	}
}

makeattr(type, val)	/* add attribute type and val */
	int type;
	YYSTYPE val;
{
	if (type == 0 && val.i == 0) {	/* clear table for next stat */
		nattr = 0;
		return;
	}
	dprintf("attr %d:  %d %d\n", nattr, type, val.i);
	attr[nattr].a_type = type;
	attr[nattr].a_val = val;
	nattr++;
}

printexpr(f)	/* print expression for debugging */
	float f;
{
	dprintf("%g\n", f);
}

printpos(p)	/* print position for debugging */
	struct obj *p;
{
	dprintf("%g, %g\n", p->o_x, p->o_y);
}

char *tostring(s)
	register char *s;
{
	register char *p;

	p = malloc(strlen(s)+1);
	if (p == NULL) {
		yyerror("out of space in tostring on %s", s);
		exit(1);
	}
	strcpy(p, s);
	return(p);
}

struct obj *makepos(x, y)	/* make a osition cell */
	float x, y;
{
	struct obj *p;

	p = makenode(PLACE, 0);
	p->o_x = x;
	p->o_y = y;
	return(p);
}

struct obj *makebetween(f, p1, p2)	/* make position between p1 and p2 */
	float f;
	struct obj *p1, *p2;
{
	struct obj *p;

	dprintf("fraction = %.2f\n", f);
	p = makenode(PLACE, 0);
	p->o_x = p1->o_x + f * (p2->o_x - p1->o_x);
	p->o_y = p1->o_y + f * (p2->o_y - p1->o_y);
	return(p);
}

struct obj *getpos(p, corner)	/* find position of point */
	struct obj *p;
	int corner;
{
	float x, y, x1, y1;

	dprintf("getpos %o %d\n", p, corner);
	x = p->o_x;
	y = p->o_y;
	x1 = p->o_val[0];
	y1 = p->o_val[1];
	switch (p->o_type) {
	case PLACE:
		break;
	case BOX:
	case BLOCK:
		switch (corner) {
		case NORTH:	y += y1 / 2; break;
		case SOUTH:	y -= y1 / 2; break;
		case EAST:	x += x1 / 2; break;
		case WEST:	x -= x1 / 2; break;
		case NE:	x += x1 / 2; y += y1 / 2; break;
		case SW:	x -= x1 / 2; y -= y1 / 2; break;
		case SE:	x += x1 / 2; y -= y1 / 2; break;
		case NW:	x -= x1 / 2; y += y1 / 2; break;
		case START:
			if (p->o_type == BLOCK)
				return getpos(objlist[(int)p->o_val[2]], START);
		case END:
			if (p->o_type == BLOCK)
				return getpos(objlist[(int)p->o_val[3]], END);
		}
		break;
	case CIRCLE:
	case ELLIPSE:
		switch (corner) {
		case NORTH:	y += y1; break;
		case SOUTH:	y -= y1; break;
		case EAST:	x += x1; break;
		case WEST:	x -= x1; break;
		case NE:	x += 0.707 * x1; y += 0.707 * y1; break;
		case SE:	x += 0.707 * x1; y -= 0.707 * y1; break;
		case NW:	x -= 0.707 * x1; y += 0.707 * y1; break;
		case SW:	x -= 0.707 * x1; y -= 0.707 * y1; break;
		}
		break;
	case LINE:
	case SPLINE:
	case ARROW:
	case MOVE:
		switch (corner) {
		case START:	break;	/* already in place */
		case END:	x = x1; y = y1; break;
		case CENTER:	x = (x+x1)/2; y = (y+y1)/2; break;
		case NORTH:	if (y1 > y) { x = x1; y = y1; } break;
		case SOUTH:	if (y1 < y) { x = x1; y = y1; } break;
		case EAST:	if (x1 > x) { x = x1; y = y1; } break;
		case WEST:	if (x1 < x) { x = x1; y = y1; } break;
		}
		break;
	case ARC:
		switch (corner) {
		case START:
			if (p->o_attr & CW_ARC) {
				x = p->o_val[2]; y = p->o_val[3];
			} else {
				x = x1; y = y1;
			}
			break;
		case END:
			if (p->o_attr & CW_ARC) {
				x = x1; y = y1;
			} else {
				x = p->o_val[2]; y = p->o_val[3];
			}
			break;
		}
		break;
	}
	dprintf("getpos returns %g %g\n", x, y);
	return(makepos(x, y));
}

struct obj *gethere(n)	/* make a place for curx,cury */
{
	dprintf("gethere %g %g\n", curx, cury);
	return(makepos(curx, cury));
}

struct obj *getlast(n, t)	/* find n-th previous occurrence of type t */
	int n, t;
{
	int i, k;
	struct obj *p;

	k = n;
	for (i = nobj-1; i >= 0; i--) {
		p = objlist[i];
		if (p->o_type == BLOCKEND) {
			i = p->o_val[4];
			continue;
		}
		if (p->o_type != t)
			continue;
		if (--k > 0)
			continue;	/* not there yet */
		dprintf("got a last of x,y= %g,%g\n", p->o_x, p->o_y);
		return(p);
	}
	yyerror("there is no %dth last", n);
	return(NULL);
}

struct obj *getfirst(n, t)	/* find n-th occurrence of type t */
	int n, t;
{
	int i, k;
	struct obj *p;

	k = n;
	for (i = 0; i < nobj; i++) {
		p = objlist[i];
		if (p->o_type == BLOCK && t != BLOCK) {	/* skip whole block */
			i = p->o_val[5] + 1;
			continue;
		}
		if (p->o_type != t)
			continue;
		if (--k > 0)
			continue;	/* not there yet */
		dprintf("got a first of x,y= %g,%g\n", p->o_x, p->o_y);
		return(p);
	}
	yyerror("there is no %dth ", n);
	return(NULL);
}

struct obj *getblock(p, s)	/* find variable s in block p */
	struct obj *p;
	char *s;
{
	struct symtab *stp;

	if (p->o_type != BLOCK) {
		yyerror(".%s is not in that block", s);
		return(NULL);
	}
	for (stp = (struct symtab *) p->o_dotdash; stp != NULL; stp = stp->s_next)
		if (strcmp(s, stp->s_name) == 0) {
			dprintf("getblock found x,y= %g,%g\n",
				(stp->s_val.o)->o_x, (stp->s_val.o)->o_y);
			return(stp->s_val.o);
		}
	yyerror("there is no .%s in that []", s);
	return(NULL);
}

struct obj *fixpos(p, x, y)
	struct obj *p;
	float x, y;
{
	dprintf("fixpos returns %g %g\n", p->o_x + x, p->o_y + y);
	return makepos(p->o_x + x, p->o_y + y);
}

struct obj *makenode(type, n)
	int type, n;
{
	struct obj *p;
	int i;

	p = (struct obj *) malloc(sizeof(struct obj) + (n-1)*sizeof(float));
	if (p == NULL) {
		yyerror("out of space in makenode\n");
		exit(1);
	}
	p->o_type = type;
	p->o_count = n;
	p->o_nobj = nobj;
	p->o_mode = hvmode;
	p->o_x = curx;
	p->o_y = cury;
	p->o_nt1 = ntext1;
	p->o_nt2 = ntext;
	ntext1 = ntext;	/* ready for next caller */
	p->o_attr = p->o_dotdash = p->o_ddval = 0;
	for (i = 0; i < n; i++)
		p->o_val[i] = 0;
	if (nobj >= MAXOBJ) {
		yyerror("objlist overflow\n");
		exit(1);
	}
	objlist[nobj++] = p;
	return(p);
}

extreme(x, y)	/* record max and min x and y values */
	float x, y;
{
	if (x > xmax)
		xmax = x;
	if (y > ymax)
		ymax = y;
	if (x < xmin)
		xmin = x;
	if (y < ymin)
		ymin = y;
}
