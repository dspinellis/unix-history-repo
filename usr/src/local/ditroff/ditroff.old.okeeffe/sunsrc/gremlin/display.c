/*
 * @(#)display.c	1.2	%G%
 *
 * This file contains routines to implement the higher level display
 * driver routines for the SUN Gremlin picture editor.
 *
 * Mark Opperman (opcode@monet.BERKELEY)
 *
 */

#include <suntool/tool_hs.h>
#include "gremlin.h"

/* imports from graphics.c */

extern GRArc();
extern GRClear();
extern GRCurve();
extern GRCurrentSetOn();		/* force display of current set */
extern GRDisplayJustify();
extern GRNewElement(); 
extern GRPutText(); 
extern GRSetCurve(); 
extern GRSetLineStyle();
extern GRSetStippleStyle();
extern GRSetTextPos(); 
extern GRStippleFill(); 
extern GRVector();

extern curve_set;			/* true if spline pre-computed */

/* imports from main.c */

extern struct pixwin *pix_pw;
extern struct rect pix_size;
extern struct pixrect *cset_pr;
extern struct pixrect *scratch_pr;
extern ELT *cset;
extern SHOWPOINTS;
extern SUN_XORIGIN;
extern SUN_YORIGIN;

/* imports from long*.c */

extern LGShowPoints();

/* locals */

int minsunx, maxsunx, minsuny, maxsuny;


/*
 * This routine displays an arbitrary element type 
 * using the parameters stored with the element.  
 * Elements are drawn by Exclusive Oring the screen.
 */
DISScreenAdd(element, mask)
register ELT *element;
int mask;
{
    register POINT *p0, *p1, *p2;
    POINT point;
    register x, y, width, height;

    if (DBNullelt(element))
	return;

    /* clear scratch_pr */
    pr_rop(scratch_pr, 0, 0, pix_size.r_width, pix_size.r_height,
						PIX_SRC, NULL, 0, 0);

    /* determine bounds for this element */
    minsunx = maxsunx = dbx_to_win(element->ptlist->x);
    minsuny = maxsuny = dby_to_win(element->ptlist->y);

    if (TEXT(element->type)) {
	GRSetTextPos(element->textpt, element->type, element->brushf,
			element->size, element->ptlist, &point);
	GRPutText(element->textpt, element->brushf, element->size, 
			&point);
	if (mask & csetmask)	/* display justification marker */
	    GRDisplayJustify(element);
    }
    else {
	switch (element->type) {
	     case ARC:  
		p1 = element->ptlist;
		p2 = PTNextPoint(p1);
		/* angle is stored in size */
		GRArc(p1, p2, (float) element->size, element->brushf);
		break;
	    case CURVE:  
		if (!curve_set)
		    GRSetCurve(element->ptlist);
		GRCurve(element->brushf);
		curve_set = 0;
		break;
	    case POLYGON:
		if (element->brushf != 0) {	/* bordered polygon */
		    p0 = p1 = element->ptlist;
		    p2 = PTNextPoint(p1);
		    GRSetLineStyle(element->brushf);

		    while (!Nullpoint(p2)) {
			GRVector(p1->x, p1->y, p2->x, p2->y);
			p1 = p2;
			p2 = PTNextPoint(p2);
		    }

		    /* if last point not specified, join end points */
		    if ((p0->x != p1->x) || (p0->y != p1->y))
			    GRVector(p1->x, p1->y, p0->x, p0->y);
		}
		else {		/* unbordered: find min/max */
		    p0 = element->ptlist;

		    while (!Nullpoint(p0)) {
			MINMAX(minsunx, maxsunx, dbx_to_win(p0->x));
			MINMAX(minsuny, maxsuny, dby_to_win(p0->y));
			p0 = PTNextPoint(p0);
		    }
		}

		GRSetStippleStyle(element->size);
		GRStippleFill(element->ptlist);
		break;
	    case VECTOR:  
		p1 = element->ptlist;
		p2 = PTNextPoint(p1);
		GRSetLineStyle(element->brushf);

		while (!Nullpoint(p2)) {
		    GRVector(p1->x, p1->y, p2->x, p2->y);
		    p1 = p2;
		    p2 = PTNextPoint(p2);
		}
		break;
	}
    }

    x = minsunx - 8;
    y = minsuny - 8;
    width = maxsunx + 8 - x;
    height = maxsuny + 8 - y;

    if (mask & pixmask)
	pw_write(pix_pw, x, y, width, height, PIX_SRC ^ PIX_DST, 
						    scratch_pr, x, y);
 
    if (mask & csetmask)
	pr_rop(cset_pr, x, y, width, height, PIX_SRC ^ PIX_DST, 
						    scratch_pr, x, y);
}  /* end DISScreenAdd */


/*
 * This routine erases an arbitrary element type by redrawing the 
 * element with XOR.  This is the same as drawing the element.
 */
DISScreenErase(element, mask)
register ELT *element;
register mask;
{
    DISScreenAdd(element, mask);
}  /* end ScreenErase */ 


/*
 * This routine clears the current set pixrect.
 */
DISClearSetDisplay()
{
    register ELT *elist;

    GRCurrentSetOn();

    if (SHOWPOINTS)
	LGShowPoints();

    elist = cset;
    while (!DBNullelt(elist)) {
	if (TEXT(elist->type))		/* turn off text handle */
	    GRDisplayJustify(elist);
	elist = DBNextofSet(elist);
    }

    GRClear(csetmask);
}  /* end DISClearSetDisplay */
