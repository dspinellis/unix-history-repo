#ifndef lint
static char *sccsid ="piece.c	(CWI)	1.1	85/03/01";
#endif
#include "ideal.h"
#include "y.tab.h"

void linecall (linefax)
LINEPTR linefax;
{
	printf ("...line %f %f %f %f\n",
		linefax->x0, linefax->y0,
		linefax->x1, linefax->y1
	);
}

void circcall (circfax)
CIRCPTR circfax;
{
	printf ("...circle %f %f %f\n",
	circfax->x0, circfax->y0, circfax->r
	);
}

void arccall (arcfax)
ARCPTR arcfax;
{
	printf ("...arc %f %f %f %f %f %f %f %f %f\n",
	arcfax->x0, arcfax->y0,
	arcfax->x1, arcfax->y1,
	arcfax->x2, arcfax->y2,
	arcfax->theta1, arcfax->theta2,

	fabs(arcfax->radius)
	);
}

void textcall (textfax)
TEXTPTR textfax;
{
	switch (textfax->command) {
	case LEFT:
		printf ("...left %f %f '%s\n",
			textfax->x0,
			textfax->y0,
			textfax->string
		);
		break;
	case CENTER:
		printf ("...center %f %f '%s\n",
			textfax->x0,
			textfax->y0,
			textfax->string
		);
		break;
	case RIGHT:
		printf ("...right %f %f '%s\n",
			textfax->x0,
			textfax->y0,
			textfax->string
		);
		break;
	default:
		fprintf (stderr, "ideal: textcall: can't happen\n");
		break;
	}
}

void boundscall (maxx, maxy, minx, miny)
float maxx, maxy;
float minx, miny;
{
	printf ("...maxx %f\n", maxx);
	printf ("...maxy %f\n", maxy);
	printf ("...minx %f\n", minx);
	printf ("...miny %f\n", miny);
}

void splcall (knotlist)
EXPRPTR knotlist;
{
	printf ("...spline %f %f\n",
		Re(((INTLPTR) knotlist->expr)),
		Im(((INTLPTR) knotlist->expr))
	);
	knotlist = knotlist->next;
	while (knotlist) {
		printf ("...knot %f %f\n",
			Re(((INTLPTR) knotlist->expr)),
			Im(((INTLPTR) knotlist->expr))
		);
		knotlist = knotlist->next;
	}
	printf ("...endspline\n");
}
