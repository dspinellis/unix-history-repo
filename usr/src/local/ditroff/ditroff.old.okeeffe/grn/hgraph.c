/*	hgraph.c	1.1	(Berkeley) 83/07/21
 *
 *     This file contains the graphics routines for converting gremlin
 * pictures to troff input.
 */

#include "gprint.h"

#define MAXVECT 50

/* line and character styles */

extern int style[];
extern int thick[];
extern char *tfont[];
extern char *tsize[];


/* variables used to print from font file */

extern cfont;
extern csize;
extern char *devdir ;

/* imports from main.c */

extern double scale;
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
        if (TEXT(element->type)) {
            HGSetFont(element->brushf, element->size);
            HGPutText(element->type, *(element->ptlist), element->textpt);
        } else {
	    tmove(p1 = element->ptlist);	/* always move to start first */
	    HGSetBrush(element->brushf);	/* p1 always has first point */
            switch (element->type) {

                 case ARC:  p2 = PTNextPoint(p1);
			    printf("\\D'a");

				/* stuff... */
				printf(" 0 0 0 0'");

                            break;

               case CURVE:  printf("\\D'g");
                            while (!Nullpoint((p1 = PTNextPoint(p1)))) {
                                dx((double) p1->x);
                                dy((double) p1->y);
                            }  /* end while */;
			    putchar('\'');
                            break;

              case VECTOR:  length = 1;		/* keep track of line length */
					   /* so single lines don't get long */
                            while (!Nullpoint((p1 = PTNextPoint(p1)))) {
				printf("\\D'l");
                                dx((double) p1->x);
                                dy((double) p1->y);
				putchar('\'');
				if (length++ > MAXVECT) {
				    tmove (p1);
				    length = 1;
				}
                            }  /* end while */;
                            break;
            }  /* end switch */;
        }  /* end else Text */
    }  /* end if */
}  /* end PrintElt */


/*----------------------------------------------------------------------------*
 | Routine:	HGPutText (justification, position_point, string)
 |
 | Results:	
 |
 | Side Efct:	
 |
 | Bugs:	
 *----------------------------------------------------------------------------*/

HGPutText(justify,pnt,string)
int justify;
POINT pnt;
char string[];

/* This routine is used to calculate the proper starting position for a
 * text string (based on justification, size and font), and prints it 
 * character by character.
 */

{
    switch (justify) {

	case BOTLEFT:
			break;
	case BOTCENT:
			break;
       case BOTRIGHT:
			break;
       case CENTLEFT:
			break;
       case CENTCENT:
			break;
      case CENTRIGHT:
			break;
	case TOPLEFT:
			break;
	case TOPCENT:
			break;
       case TOPRIGHT:
			break;
    }
    HGplotch(string);
} /* end HGPutText */;



HGSetFont(font, size)
int font, size;
{
    int i;
    char c, string[100];

    if (font != cfont) cfont = font;
    if (size != csize) csize = size;
    /* and whatever... */
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
	printf ("\\D's %du'",linmod = style[mode]);
    }
    if (linethickness != thick[mode]) {
	printf ("\\D't %du'", linethickness = thick[mode]);
    }
}


HGplotch(string)
char *string;
{
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
 | Results:	produces horizontal and vertical moves for troff given
 |		the point pointer.
 |
 | Bugs:	Notice that this is identical to "dx" and "dy" in value
 |		output.  This is because troff does NOT understand spaces
 |		in \h or \v commands (!)
 *----------------------------------------------------------------------------*/

tmove(ptr)
POINT *ptr;
{
    register int ix = (int) (ptr->x * troffscale);
    register int iy = (int) (ptr->y * troffscale);

    cr();
    if (iy - lasty) {
	printf(".sp %du\n", iy - lasty);
    }
    lastyline = lasty = iy;
    if (ix - lastx) {
	printf("\\h'%du'", ix - lastx);
	lastx = ix;
    }
}


/*----------------------------------------------------------------------------*
 | Routine:	cr
 |
 | Results:	breaks the output line up to not overrun troff with lines that
 |		are too long.  Outputs a ".sp -1" also to keep the vertical
 |		spacing correct.
 |
 | Side Efct:	sets "lastx" to "leftpoint" for troff's return to left margin
 *----------------------------------------------------------------------------*/

cr()
{
    putchar('\n');
    lastx = xleft;
}
