/*	hgraph.c	1.5	(Berkeley) 83/09/19
 *
 *     This file contains the graphics routines for converting gremlin
 * pictures to troff input.
 */

#include "gprint.h"


#define  MAXVECT	50
#define  pi		3.14159265358979324
#define  twopi		(2.0 * pi)
#define  len(a, b)	sqrt((b.x-a.x) * (b.x-a.x) + (b.y-a.y) * (b.y-a.y))


extern int style[];	/* line and character styles */
extern int thick[];
extern char *tfont[];
extern char *tsize[];


extern double scale;		/* imports from main.c */
extern double troffscale;
extern point();
extern int linethickness;
extern int linmod;
extern int lastx;
extern int lasty;
extern int lastyline;
extern int ytop;
extern int ybottom;
extern int xleft;
extern int xright;


/*----------------------------------------------------------------------------*
 | Routine:	HGPrintElt (element_pointer)
 |
 | Results:	examines a picture element and calls the appropriate
 |		routine(s) to print them according to their type.
 |		After the picture is drawn, current position is (lastx, lasty).
 *----------------------------------------------------------------------------*/

HGPrintElt(element)
ELT *element;
{
    register POINT *p1;
    register POINT *p2;
    register int length;

    if ( !DBNullelt(element) ) {
	p1 = element->ptlist;		/* p1 always has first point */
        if (TEXT(element->type)) {
            HGSetFont(element->brushf, element->size);
            HGPutText(element->type, *p1, element->textpt);
        } else {
	    HGSetBrush(element->brushf);	/* graphics need brush set */
            switch (element->type) {

                 case ARC:  p2 = PTNextPoint(p1);
			    doarc(*p1, *p2, element->size);
                            break;

               case CURVE:  tmove(p1);
			    printf("\\D'g");
                            while (!Nullpoint((p1 = PTNextPoint(p1)))) {
                                dx((double) p1->x);
                                dy((double) p1->y);
                            }  /* end while */;
			    putchar('\'');
                            break;

              case VECTOR:  length = 1;		/* keep track of line length */
			    tmove(p1);	   /* so single lines don't get long */
                            while (!Nullpoint((p1 = PTNextPoint(p1)))) {
				printf("\\D'l");
                                dx((double) p1->x);
                                dy((double) p1->y);
				putchar('\'');
				if (length++ > MAXVECT) {
				    tmove (p1);
				    length = 1;
				}
                            }  /* end while */
                            break;
            }  /* end switch */
        }  /* end else Text */
    }  /* end if */
}  /* end PrintElt */


/*----------------------------------------------------------------------------*
 | Routine:	HGPutText (justification, position_point, string)
 |
 | Results:	given the justification, a point to position with, and a
 |		string to put, HGPutText first sends the string into a
 |		diversion, moves to the positioning point, then outputs local
 |		vertical and horizontal motions as needed to justify the text.
 |		After all motions are done, the diversion is printed out.
 *----------------------------------------------------------------------------*/

HGPutText(justify,pnt,string)
int justify;
POINT pnt;
char string[];
{
    int savelasty = lasty;		/* vertical motion for text is to be */
					/*   ignored.  save current y here */

    printf(".nr g8 \\n(.d\n", string);	/* save current vertical position. */
    printf(".ds g9 \"%s", string);	/* define string containing the text. */
    tmove(&pnt);			/* move to positioning point */
    switch (justify) {
					/* local vertical motions */
					/* (the numbers here are used to be */
					/* somewhat compatible with gprint) */
        case CENTLEFT:
        case CENTCENT:
       case CENTRIGHT:	printf("\\v'0.85n'");		/* down half */
			break;

	 case TOPLEFT:
	 case TOPCENT:
        case TOPRIGHT:	printf("\\v'1.7n'");		/* down whole */
    }

    switch (justify) {
					/* local horizontal motions */
	 case BOTCENT:
        case CENTCENT:
	 case TOPCENT:	printf("\\h'-\\w'\\*(g9'u/2u'");	/* back half */
			break;

        case BOTRIGHT:
       case CENTRIGHT:
        case TOPRIGHT:	printf("\\h'-\\w'\\*(g9'u'");		/* back whole */
    }
    printf("\\*(g9\n");			/* now print the text. */
    printf(".sp |\\n(g8u");		/* restore vertical position */
    lasty = savelasty;		/* vertical position is restored to */
				/*   what it was before text was printed */
} /* end HGPutText */


/*----------------------------------------------------------------------------*
 | Routine:	doarc (center_point, start_point, angle)
 |
 | Results:	produces either drawarc command or a drawcircle command
 |		depending on the angle needed to draw through.
 *----------------------------------------------------------------------------*/

doarc (cp, sp, angle)
POINT cp;
POINT sp;
int angle;
{
	double radius = len(cp, sp);
	double radians;


	if (angle) {		/* arc with angle */
	    tmove (&sp);		/* starting point first */
	    printf("\\D'a");
	    dx((double) cp.x);		/* move to center */
	    dy((double) cp.y);

	    radians = acos((sp.x - cp.x) / radius);	  /* angle of ending */
	    if (cp.y - sp.y < 0.0)			 /* point calculated */
		radians = twopi - radians;		 /* from start point */
	    radians += ((double) angle) * (pi / 180.0);	  /* and arc's angle */
	    if (radians > twopi) radians -= twopi;

	    dx(cp.x + cos(radians) * radius);	/* move to ending point */
	    dy(cp.y - sin(radians) * radius);

	} else {		/* a full circle (angle == 0) */
	    cp.x -= radius;
	    tmove(&cp);			/* move to the left point first */
					/* draw circle with given diameter */
	    printf("\\D'c %du", (int) ((radius + radius) * troffscale));
	}
	putchar('\'');		/* finish the command */
}


/*----------------------------------------------------------------------------*
 | Routine:	HGSetFont (font_number, Point_size)
 |
 | Results:	ALWAYS outputs a .ft and .ps directive to troff.  This is
 |		done because someone may change stuff inside a text string.
 *----------------------------------------------------------------------------*/

HGSetFont(font, size)
int font, size;
{
    cr();
    printf(".ft %s\n.ps %s\n", tfont[font-1], tsize[size-1]);
}


/*----------------------------------------------------------------------------*
 | Routine:	HGSetBrush (line_mode)
 |
 | Results:	generates the troff commands to set up the line width and
 |		style of subsequent lines.  Does nothing if no change is needed.
 |
 | Side Efct:	sets "linmode" and "linethicknes"
 *----------------------------------------------------------------------------*/

HGSetBrush(mode)
int mode;
{
    if (linmod != style[--mode]) {
	cr();
	printf ("\\D's %du'",linmod = style[mode]);
    }
    if (linethickness != thick[mode]) {
	cr();
	printf ("\\D't %du'", linethickness = thick[mode]);
    }
}


/*----------------------------------------------------------------------------*
 | Routine:	dx (x_destination)
 |
 | Results:	scales and outputs a number for delta x (with a leading space)
 |		given "lastx" and x_destination.
 |
 | Side Efct:	resets "lastx" to x_destination.
 *----------------------------------------------------------------------------*/

dx(x)
double x;
{
    register int ix = (int) (x * troffscale);

    printf(" %du", ix - lastx);
    lastx = ix;
}


/*----------------------------------------------------------------------------*
 | Routine:	dy (y_destination)
 |
 | Results:	scales and outputs a number for delta y (with a leading space)
 |		given "lastyline" and y_destination.
 |
 | Side Efct:	resets "lastyline" to y_destination.  Since "line" vertical
 |		motions don't affect "page" ones, "lasty" isn't updated.
 *----------------------------------------------------------------------------*/

dy(y)
double y;
{
    register int iy = (int) (y * troffscale);

    printf(" %du", iy - lastyline);
    lastyline = iy;
}


/*----------------------------------------------------------------------------*
 | Routine:	tmove (point_pointer)
 |
 | Results:	produces horizontal and vertical moves for troff given the
 |		pointer of a point to move to.
 *----------------------------------------------------------------------------*/

tmove(ptr)
POINT *ptr;
{
    register int ix = (int) (ptr->x * troffscale);
    register int iy = (int) (ptr->y * troffscale);
    register int dx;
    register int dy;

    cr();
    if (dy = iy - lasty) {
	printf(".sp %du\n", dy);
    }
    lastyline = lasty = iy;		/* lasty is always set to current */
    if (dx = ix - lastx) {
	printf("\\h'%du'", dx);
	lastx = ix;
    }
}


/*----------------------------------------------------------------------------*
 | Routine:	cr ( )
 |
 | Results:	breaks the output line up to not overrun troff with lines that
 |		are too long.
 |
 | Side Efct:	sets "lastx" to "xleft" for troff's return to left margin
 *----------------------------------------------------------------------------*/

cr()
{
    putchar('\n');
    lastx = xleft;
}
