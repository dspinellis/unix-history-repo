/*
 * @(#)long1.c	1.2	%G%
 *
 * Routines to implement "long" commands in the SUN Gremlin picture editor.
 *
 * Mark Opperman (opcode@monet.BERKELEY)
 *
 */

/*
 * This file contains routines to implement the long text commands
 * of the gremlin PICTURE editor.
 */

#include <sunwindow/rect.h>
#include "gremlin.h"
#include <ctype.h>

/* imports from graphics files */

extern GRBlankPoints();
extern GRDisplayPoint();
extern GRErasePoint();
extern GRfontfound();
extern GROpenFont();
extern GRSetTextPos();
extern curve_set;		/* TRUE if spline points pre-computed */

/* imports from point.c */

extern POINT *PTMakeTextPoints();

/* imports from display.c */

extern DISClearSetDisplay();
extern DISScreenAdd();
extern DISScreenErase();

/* imports from database files */

extern ELT *DBCreateElt();
extern DBGravitate();
extern DBChangeBrush();
extern DBChangeFont();
extern DBChangeSize();
extern DBChangeStipple();
extern DBChangeText();
extern DBChangeJustify();
extern DBAddSet();
extern DBClearSet();
extern POINT *PTMakePoint();
extern PTDeletePoint();

/* imports from undodb.c */

extern UNRembMod();

/* imports from short.c */

extern SHUpdate();

/* imports from menu.c  */

extern HiArtMode;
extern HiLineStyle;
extern HiBrush[];
extern MNHighLt();
extern MNUnHighLt();
extern HiFont[];
extern HiSize[];
extern HiStipple[];

/* imports from text.c */

extern TxKillLine();
extern TxMsgOK();
extern text_getvalue();

/* imports from C */

extern char *malloc();
extern char *strcpy();

/* imports from main.c */

extern ELT *PICTURE;                /* current PICTURE database      */
extern ELT *cset;                   /* current set database          */
extern Artmode;			/* indication of point display size */
extern CBRUSH, CSIZE, CFONT;        /* current brush, size, font     */
extern CJUST;                       /* current text justification    */
extern CSTIPPLE;		    /* current stipple pattern	     */
extern Alignment;                   /* point alignment indicator     */
extern float PX, PY;                /* cursor coordinates            */
extern float Lastx, Lasty;          /* previous cursor coordinates   */
extern SEQ;                         /* point sequence number         */
extern POINT *POINTLIST, *BACKPOINT;/* accumulated point list        */
extern Adjustment;                  /* point adjustment mode         */
extern GravityOn;                   /* gravity mode flag             */
extern CHANGED;                     /* PICTURE changed flag          */
extern SymbolicLines;
extern Gridsize;

extern SUN_XORIGIN;
extern SUN_YORIGIN;
extern struct rect pix_size;

/* locals */

int SHOWPOINTS;			/* TRUE if current set reference points on */

static char badarg[] = "bad args";
static char noset[] = "no current set";
static char delmsg[] = "can't delete any more points";

#define BADNUM 0x7fffffff	/* largest positive 32-bit integer */

/*
 * This routine trys to interpret the string starting at
 * line+index as an integral numeric parameter.  The function
 * returns the numeric equivalent or the largest possible
 * integer if there is some error in interpreting the string.
 */
GetNumParm(line, index)
register char *line;
register int *index;
{
    char num[20];
    register sign = 1;
    register i;
    int result;

    for (i=0; (*(line + *index) == ' '); ++i)	/* skip blanks */
	++(*index);

    if (*(line + *index) == '-') {		/* negative number */
	sign = -1;
	++(*index);
    }

    for (i=0; !Delimiter(*(line + *index)); ++i) {
	num[i] = *(line + *index);
	if (!isdigit(num[i])) 
	    return(BADNUM);
	++(*index);
    } 

    if (i == 0) 
	return(BADNUM);

    num[i] = '\0';
    (void) sscanf(num, "%d", &result);
    return(result * sign);
}  /* end GetNumParm */


/*
 * This routine accepts coordinates from the text terminal
 * and creates and displays a point from them by passing them
 * along to LGPoint.
 */
LGOPoint()
{
    int index, xcoord, ycoord;
    char buf[TEXT_BUFMAX];

    text_getvalue(&buf[0]);
    TxKillLine();
    index = 0;
    xcoord = GetNumParm(buf, &index);
    if (xcoord == BADNUM) {
	error(badarg);
	return;
    }

    ++index;
    ycoord = GetNumParm(buf, &index);
    if (ycoord == BADNUM) {
	error(badarg);
	return;
    }

    PX = xcoord;
    PY = ycoord;
    LGPoint();
}  /* end LGOPoint */


/*
 * This routine accepts cursor coordinates (global PX & PY) and then
 * creates and displays points according to the current adjustment and 
 * alignment modes.  Note that alignment and gravity are mutually exclusive
 * and adjustment takes precedence over either.
 */
LGPoint()
{
    ELT *temp;
    POINT *p1;
    float signx = 1.0;
    float signy = 1.0;

    temp = DBInit();
    if (GravityOn) 
	DBGravitate (PX, PY, &PX, &PY, &p1, &temp, PICTURE, FALSE);

    if (DBNullelt(temp)) {   /* no gravity in effect */
	/* Round to nearest alignment boundary */
	if (PX < 0) {
	    signx = -1.0;
	    PX = -PX;
	}
	if (PY < 0) {
	    signy = -1.0;
	    PY = -PY;
	}

	PX = (float) (((int) (PX / Alignment + 0.5)) * Alignment) * signx;
	PY = (float) (((int) (PY / Alignment + 0.5)) * Alignment) * signy;
    }

    if (SEQ > 0) {    /* this isn't the first point */
	switch (Adjustment) {
	    case HORZ:
		PY = Lasty;
		break;
	    case VERT:
		PX = Lastx;
		break;
	    case MAN:
		if (fabs(PX - Lastx) > fabs(PY - Lasty)) 
		    PY = Lasty;
		else   
		    PX = Lastx;
		break;
	}
    }

    if (SEQ >= MAXPOINTS) {
	error("too many points");
	return;
    }

    GRDisplayPoint(PX, PY, SEQ);
    (void) PTMakePoint(PX, PY, &POINTLIST);
    Lastx = PX;
    Lasty = PY;

    ++SEQ;
}  /* end LGPoint */


/*
 * Clear all points on from Showpoints command.
 */
CSP()
{
    if (SHOWPOINTS)
	LGShowPoints();
}


/*
 * This routine deletes all points from the POINTLIST and 
 * clears them from the display also.
 */
CP()
{
    POINT *temp;

    while (!Nullpoint(BACKPOINT)) {
	temp = PTNextPoint(BACKPOINT);
	free ((char *) BACKPOINT);
	BACKPOINT = temp;
    }

    GRBlankPoints(POINTLIST);
    BACKPOINT = POINTLIST;
    POINTLIST = PTInit();
    SEQ = 0;
}  /* end CP */


/*
 * Clear all displayed points.
 */
LGClearPoints()
{
    CP();
    CSP();
}  /* end LGClearPoints */


/*
 * This routine removes the last point from the POINTLIST
 * and erases it from the screen.
 */
LGDeletePoint()
{
    POINT *pt1, *pt2, *pt3; 

    if (SEQ == 0) {
	error("no point");
	return;
    }

    pt2 = pt3 = POINTLIST;
    while (!Nullpoint(pt3)) {	/* find last point and pointer to it */
	pt1 = pt2;
	pt2 = pt3;
	pt3 = PTNextPoint(pt3);
    }

    SEQ--;
    GRErasePoint(pt2->x, pt2->y, SEQ);
    PTDeletePoint(pt2, &POINTLIST);
    if (SEQ > 0) {	/* pt1 points to last one of them */
	Lastx = pt1->x;
	Lasty = pt1->y;
    }
}  /* end LGDeletePoint */


/*
 *  This routine causes the positioning points of the current set
 *  to be displayed.
 */
LGShowPoints()
{
    register ELT *elt;
    register POINT *p1;
    register pno;

    if (DBNullelt(cset)) {
	error(noset);
	return;
    }

    elt = cset;
    while (!DBNullelt(elt)) {
	p1 = elt->ptlist;
	pno = 0;

	while (!Nullpoint(p1)) {
	    GRDisplayPoint(p1->x, p1->y, pno);
	    p1 = PTNextPoint(p1);
	    pno++;
	}

	elt = DBNextofSet(elt);
    }
    SHOWPOINTS = !SHOWPOINTS;
} /* end LGShowPoints */


/*
 *  This routine handles the two forms of the TEXT command.
 *  From the text subwindow, when a RETURN is pressed, the text
 *  buffer is copied to the LAST point layed down, the text is
 *  consumed, and that point is eaten.  This provides a convenient
 *  method of entering several TEXT elements at many locations
 *  in the picture.
 *  From the menu subwindow, the traditional Gremlin TEXT command
 *  is implemented.  One or two points may be specified, and all
 *  points are consumed at the end of the command.
 */
static
LGTextDisplay(oldway)
int oldway;
{
    register ELT *elt;
    char buf[TEXT_BUFMAX];
    POINT pos, ppnt, *p1;
    char *text;

    if (SEQ == 0) {
	error("not enough points");
	return;
    }

    text_getvalue(&buf[0]);

    if (*buf == '\0') {		/* no text */
	error("empty string");
	return;
    }

    GROpenFont(CFONT, CSIZE);
    if (!GRfontfound(CFONT, CSIZE)) {
	error("can't open font file");
	return;
    }

    UNForget();
    text = malloc((unsigned) strlen(buf) + 1);
    (void) strcpy(text, buf);
    DISClearSetDisplay();
    DBClearSet();

    if (oldway == TRUE) {	/* one or two points OK */
	ppnt.x = POINTLIST->x;
	ppnt.y = POINTLIST->y;
	if (SEQ > 1) {
	    p1 = PTNextPoint(POINTLIST);
	    ppnt.x = (ppnt.x + p1->x) / 2;
	    ppnt.y = (ppnt.y + p1->y) / 2;
	}
    }
    else {			/* find last point */
	p1 = POINTLIST;
	while (!Nullpoint(PTNextPoint(p1)))
	    p1 = PTNextPoint(p1);
	ppnt.x = p1->x;
	ppnt.y = p1->y;
    }

    GRSetTextPos(text, CJUST, CFONT, CSIZE, &ppnt, &pos);
    p1 = PTMakeTextPoints(text, CFONT, CSIZE, &ppnt, &pos);
    elt = DBCreateElt(CJUST, p1, CFONT, CSIZE, text, &PICTURE);

    DISScreenAdd(elt, pixmask | csetmask);
    DBAddSet(elt);

    if (oldway == TRUE)
	CP();
    else
	LGDeletePoint();

    TxKillLine();
    CHANGED = TRUE;
}  /* end LGTextDisplay */


/* 
 *  This routine implements the TEXT command from the menu subwindow.
 */
LGText()
{
    LGTextDisplay(TRUE);		/* the old way of doing text entry */
}  /* end LGText */


LGTextSW()
{
    LGTextDisplay(FALSE);		/* the new way of doing text entry */
}  /* end LGTextSW */


/*
 * This routine sets the current brush to that specified in the parameter.
 */
LGBrush(brush)
{
    MNUnHighLt(HiBrush[CBRUSH-1]);
    CBRUSH = brush;
    MNHighLt(HiBrush[CBRUSH-1]);
}  /* end LGBrush */


LGBrush1()
{
    LGBrush(1);
}


LGBrush2()
{
    LGBrush(2);
}


LGBrush3()
{
    LGBrush(3);
}


LGBrush4()
{
    LGBrush(4);
}


LGBrush5()
{
    LGBrush(5);
}


LGBrush6()
{
    LGBrush(6);
}


/*
 * This routine causes the elements in the current set
 * to be redrawn using the new brush.
 */
LGMBrush(brush)
int brush;
{
    register ELT *elt;

    if (DBNullelt(cset)) {
	error(noset);
	return;
    }

    UNForget();
    CSP();

    elt = cset;
    while (!DBNullelt(elt)) {
	if (!TEXT(elt->type)) {
	    DISScreenErase(elt, pixmask | csetmask);
	    DBChangeBrush(elt, brush, &PICTURE);
	    curve_set = TRUE;	/* no need to re-compute spline points */
	    DISScreenAdd(elt, pixmask | csetmask);
	} 
	elt = DBNextofSet(elt);
    }

    CP();
    CHANGED = TRUE;
} /* end LGMBrush */


LGMBrush1()
{
    LGMBrush(1);
}


LGMBrush2()
{
    LGMBrush(2);
}


LGMBrush3()
{
    LGMBrush(3);
}


LGMBrush4()
{
    LGMBrush(4);
}


LGMBrush5()
{
    LGMBrush(5);
}


LGMBrush6()
{
    LGMBrush(6);
}


/*
 * This routine causes text elements in the current set
 * to be redrawn using the new justification mode.
 * mode is 1 - 9 for tl, tc, tr, cl, cc, cl, bl, bc, br
 */
LGMJustify(just)
int just;
{
    register ELT *elt;

    if (DBNullelt(cset)) {
	error(noset);
	return;
    }

    UNForget();
    CSP();

    elt = cset;
    while (!DBNullelt(elt)) {
	if (TEXT(elt->type)) {
	    DISScreenErase(elt, pixmask | csetmask);
	    DBChangeJustify(elt, just, &PICTURE);
	    DISScreenAdd(elt, pixmask | csetmask);
	} 
	elt = DBNextofSet(elt);
    }

    CP();
    CHANGED = TRUE;
} /* end LGMJustify */


/*
 * This routine causes the text elements in the current set
 * to be redrawn using the new font.
 */
LGMFont(font)
int font;
{
    register ELT *elt;

    if (DBNullelt(cset)) {
	error(noset);
	return;
    }

    UNForget();
    CSP();

    elt = cset;
    while (!DBNullelt(elt)) {
	if (TEXT(elt->type)) {
	    GROpenFont(font, elt->size);
	    if (!GRfontfound(font, elt->size)) {
		error("can't open font file");
	    }
	    else {
		DISScreenErase(elt, pixmask | csetmask);
		TxMsgOK();
		DBChangeFont(elt, font, &PICTURE);
		DISScreenAdd(elt, pixmask | csetmask);
	    }
	}
	elt = DBNextofSet(elt);
    }

    CP();
    CHANGED = TRUE;
}  /* end LGMFont */


LGMFont1()
{
    LGMFont(1);
}


LGMFont2()
{
    LGMFont(2);
}


LGMFont3()
{
    LGMFont(3);
}


LGMFont4()
{
    LGMFont(4);
}


/*
 * This routine causes the text elements in the current set
 * to be redrawn using the new size.
 */
LGMSize(size)
int size;
{
    register ELT *elt;

    if (DBNullelt(cset)) {
	error(noset);
	return;
    }

    UNForget();
    CSP();

    elt = cset;
    while (!DBNullelt(elt)) {
	if (TEXT(elt->type)) {
	    GROpenFont(elt->brushf, size);
	    if (!GRfontfound(elt->brushf, size)) {
		error("can't open font file");
	    }
	    else {
		DISScreenErase(elt, pixmask | csetmask);
		TxMsgOK();
		DBChangeSize(elt, size, &PICTURE);
		DISScreenAdd(elt, pixmask | csetmask);
	    }
	}
	elt = DBNextofSet(elt);
    }

    CP();
    CHANGED = TRUE;
}  /* end LGMFize */


LGMSize1()
{
    LGMSize(1);
}


LGMSize2()
{
    LGMSize(2);
}


LGMSize3()
{
    LGMSize(3);
}


LGMSize4()
{
    LGMSize(4);
}


/*
 * This routine causes the polygon elements in the current set
 * to be redrawn using the new stipple.
 */
LGMStipple(stipple)
int stipple;
{
    register ELT *elt;

    if (DBNullelt(cset)) {
	error(noset);
	return;
    }

    UNForget();
    CSP();

    elt = cset;
    while (!DBNullelt(elt)) {
	if (elt->type == POLYGON) {
	    DISScreenErase(elt, pixmask | csetmask);
	    TxMsgOK();
	    DBChangeStipple(elt, stipple, &PICTURE);
	    DISScreenAdd(elt, pixmask | csetmask);
	}
	elt = DBNextofSet(elt);
    }

    CP();
    CHANGED = TRUE;
}  /* end LGMStipple */


LGMStipple1()
{
    LGMStipple(1);
}


LGMStipple2()
{
    LGMStipple(2);
}


LGMStipple3()
{
    LGMStipple(3);
}


LGMStipple4()
{
    LGMStipple(4);
}


LGMStipple5()
{
    LGMStipple(5);
}


LGMStipple6()
{
    LGMStipple(6);
}


LGMStipple7()
{
    LGMStipple(7);
}


LGMStipple8()
{
    LGMStipple(8);
}


/* 
 * This routine allows modification of text by replacing 
 * an existing string with a new one, appropriately repositioned
 */
LGMText()
{
    register ELT *elt;
    char buf[TEXT_BUFMAX];

    if (DBNullelt(cset)) {
	error(noset);
	return;
    }

    text_getvalue(&buf[0]);
    if (*buf == '\0') {		/* no text */
	error("empty string");
	return;
    }

    UNForget();
    CSP();

    elt = cset;
    while (!DBNullelt(elt)) {
        if (TEXT(elt->type)) {
            DISScreenErase(elt, pixmask | csetmask);
            TxMsgOK();
	    DBChangeText(elt, buf, &PICTURE);
            DISScreenAdd(elt, pixmask | csetmask);
        }
        elt = DBNextofSet(elt);
    }

    CP();
    TxKillLine();
    CHANGED = TRUE;
}  /* end LGMText */


/*
 * This routine modifies the element which contains the point
 * closest to the first of two specified points so that that point
 * coincides with the second of the points (if specified).
 *
 * Note: it implies knowledge of the database representation by modifying
 *       the element directly.
 */
LGMPoint()
{
    ELT *elt;
    POINT *p1, *p2, *p3, *p4;
    float x1, y1;
    int length;

    if (SEQ < 1) {
	error("no point specified");
	return;
    }

    /* find point */
    DBGravitate(POINTLIST->x, POINTLIST->y, &x1, &y1, &p1, &elt, cset, TRUE);

    if (DBNullelt(elt) || TEXT(elt->type)) {
	error("can't find a good element");
	return;
    }

    if (SEQ == 1) {		/* wants to delete a point */
	length = PTListLength(elt);
	if (((elt->type == POLYGON) && (length == 3)) || (length == 2)) {
	    error(delmsg);
	    return;
	}
    }

    /* now OK to do whatever */
    UNForget();
    CSP();

    DBClearSet();
    GRClear(csetmask);
    DISScreenErase(elt, pixmask);
    UNRembMod(elt, &PICTURE);
    if (SEQ > 1) {		/* move a point, not delete */
	p2 = PTNextPoint(POINTLIST);
	p1->x = p2->x;
	p1->y = p2->y;
	p2 = PTNextPoint(p2);
	if (!Nullpoint(p2)) {
	    p3 = PTInit();
	    while (!Nullpoint(p2)) {
		p4 = PTMakePoint(p2->x, p2->y, &p3);
		p2 = PTNextPoint(p2);
	    }
	    p4->nextpt = p1->nextpt;
	    p1->nextpt = p3;
	}
    }
    else {
	PTDeletePoint(p1, &(elt->ptlist));
    }

    DISScreenAdd(elt, pixmask | csetmask);
    DBAddSet(elt);

    CP();
    CHANGED = TRUE;
}  /* end LGMPoint */


/*
 * This routine allows users to leave gripe messages or report
 * bugs to the maintainer.  Mail is invoked via the defined constant GRIPE.
 */
LGGripe()
{
    TxPutMsg("mail gripes to opcode@monet");
}  /* end LGGripe */


/*
 * This routine controls the size of the point that is displayed.
 * The sizes available are Artmode in which a small (3 x 3) point is displayed
 * with no number and regular (non-Artmode).
 */
LGLittlePoint()
{
    register POINT *plist;
    register sp;
    register i = 0;

    GRBlankPoints(POINTLIST);
    if ((sp = SHOWPOINTS) != 0)		/* turn off show points */
	CSP();
    Artmode = !Artmode;

    plist = POINTLIST;
    while (!Nullpoint(plist)) {
	GRDisplayPoint(plist->x, plist->y, i++);
	plist = PTNextPoint(plist);
    }

    if (sp != 0)			/* turn on show points */
	LGShowPoints();

    if (Artmode)
	MNUnHighLt(HiArtMode);
    else
	MNHighLt(HiArtMode);
}  /* end LGLittlePoint */


/*
 * This routine looks at the command line for parameters to set
 * the current Font.
 */
LGFont(font)
int font;
{
    MNUnHighLt(HiFont[CFONT-1]);
    CFONT = font;
    MNHighLt(HiFont[CFONT-1]);
}  /* end LGFont */


LGFont1()
{
    LGFont(1);
}


LGFont2()
{
    LGFont(2);
}


LGFont3()
{
    LGFont(3);
}


LGFont4()
{
    LGFont(4);
}


/*
 * This routine changes the current character size.
 */
LGSize(size)
int size;
{
    MNUnHighLt(HiSize[CSIZE-1]);
    CSIZE = size;
    MNHighLt(HiSize[CSIZE-1]);
}  /* end LGSize */


LGSize1()
{
    LGSize(1);
}


LGSize2()
{
    LGSize(2);
}


LGSize3()
{
    LGSize(3);
}


LGSize4()
{
    LGSize(4);
}


/*
 * This routine changes the current stipple pattern.
 */
LGStipple(stipple)
int stipple;
{
    MNUnHighLt(HiStipple[CSTIPPLE-1]);
    CSTIPPLE = stipple;
    MNHighLt(HiStipple[CSTIPPLE-1]);
}  /* end LGStipple */


LGStipple1()
{
    LGStipple(1);
}


LGStipple2()
{
    LGStipple(2);
}


LGStipple3()
{
    LGStipple(3);
}


LGStipple4()
{
    LGStipple(4);
}


LGStipple5()
{
    LGStipple(5);
}


LGStipple6()
{
    LGStipple(6);
}


LGStipple7()
{
    LGStipple(7);
}


LGStipple8()
{
    LGStipple(8);
}


/*
 * Toggle line style
 */
LGLineStyle()
{
    if (SymbolicLines = !SymbolicLines)
	MNUnHighLt(HiLineStyle);
    else
	MNHighLt(HiLineStyle);

    SHUpdate();
}  /* end LGLineStyle */


LGPan()
{
    if (SEQ < 1) {
	error("need one point");
	return;
    }

    LGdopan(POINTLIST->x, POINTLIST->y);
}


/*
 * Make (wx, wy) center of Gremlin window.
 */
LGdopan(wx, wy)
float wx, wy;
{
    float cx, cy;
    register tx, ty;

    CP();						/* eat points first */

    cx = SUN_XORIGIN + (pix_size.r_width >> 1);		/* window x center */
    cy = SUN_YORIGIN - (pix_size.r_height >> 1);	/* window y center */

    tx = (int) (wx - cx);				/* x translation */
    ty = (int) (wy - cy);				/* y translation */

    tx += (tx < 0) ? -1 : 1;				/* fudge factor */
    ty += (ty < 0) ? -1 : 1;

    SUN_XORIGIN += (tx / Gridsize) * Gridsize;
    SUN_YORIGIN += (ty / Gridsize) * Gridsize;

    SHUpdate();
}  /* end LGPan */


/*
 * Pan to absolute center of picture.
 * Invoked by the middle button on the PAN icon.
 */
LGMPan()
{
    register ELT *elt;
    register POINT *point;
    float minx, miny, maxx, maxy;

    if (DBNullelt(PICTURE)) {
	error("empty picture");
	return;
    }

    elt = PICTURE;
    minx = maxx = elt->ptlist->x;
    miny = maxy = elt->ptlist->y;

    while (!DBNullelt(elt)) {
	point = elt->ptlist;

	while (!Nullpoint(point)) {
	    MINMAX(minx, maxx, point->x);
	    MINMAX(miny, maxy, point->y);
	    point = PTNextPoint(point);
	}

	elt = DBNextElt(elt);
    }

    LGdopan(maxx - ((maxx - minx) / 2.0), maxy - ((maxy - miny) / 2.0));
}
