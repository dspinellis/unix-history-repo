#ifndef lint
static char sccsid[] = "@(#)textgen.c	1.1 (CWI) 85/07/19";
#endif lint

#include	<stdio.h>
#include	"pic.h"
#include	"y.tab.h"

struct obj *textgen(s)
{
	int i, type;
	float dx, dy;
	struct obj *p, *ppos;

	type = 'C';
	dx = dy = 0;
	for (i = 0; i < nattr; i++)
		switch (attr[i].a_type) {
		case LEFT:
			dx -= attr[i].a_val.f;
			break;
		case RIGHT:
			dx += attr[i].a_val.f;
			break;
		case UP:
			dy += attr[i].a_val.f;
			break;
		case DOWN:
			dy -= attr[i].a_val.f;
			break;
		case AT:
			ppos = attr[i].a_val.o;
			curx = ppos->o_x;
			cury = ppos->o_y;
			break;
		case LJUST:
			type = 'L';
			break;
		case RJUST:
			type = 'R';
			break;
		case SPREAD:
			type = 'S';
			break;
		case FILL:
			type = 'F';
			break;
		case ABOVE:
			type = 'A';
			break;
		case BELOW:
			type = 'B';
			break;
		}
	dprintf("T %c %s\n", type, s);
	extreme(curx, cury);
	curx += dx;
	cury += dy;
	extreme(curx, cury);
	p = makenode(TEXT, 2);
	p->o_val[0] = p->o_val[1] = 0;
	p->o_attr = s;	/* kludge into funny place to avoid coercion */
	p->o_dotdash = type;
	return(p);
}
