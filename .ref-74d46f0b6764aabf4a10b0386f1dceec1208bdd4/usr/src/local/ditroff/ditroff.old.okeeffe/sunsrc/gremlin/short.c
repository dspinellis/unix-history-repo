/*
 * @(#)short.c	1.2	%G%
 *
 * Routines for the "short" commands of the SUN Gremlin picture editor.
 *
 * Mark Opperman (opcode@monet.BERKELEY)
 *
 */

#include <suntool/tool_hs.h>
#include "icondata.h"
#include "gremlin.h"

/* imports from graphics files */

extern GRClear();
extern GRCurrentSet();
extern GRCurrentSetOn();
extern GRCurrentSetOff();
extern GRDisplayPoint();
extern GRSetCurve();
extern GRSetTextPos();

/* imports from undodb.c */

extern UNForget();

/* imports from display.c */

extern DISClearSetDisplay();
extern DISScreenAdd();
extern DISScreenErase();

/* imports from database files */

extern ELT *DBCopy();
extern ELT *DBCreateElt(); 

extern POINT *PTMakePoint();
extern POINT *PTMakeTextPoints();

extern DBDelete();
extern DBClearElt();
extern DBXform();
extern DBBounded();
extern DBAddSet();
extern DBClearSet();

/* imports from long.c */

extern LGIncludeSet();
extern CP();			/* clear points */
extern CSP();			/* clear show points */

/* imports from menu.c  */

extern MNHighLt();
extern MNUnHighLt();

extern HiBrush[];
extern HiBuffer[];
extern HiFont[];
extern HiMode[];
extern HiSize[];

int adj[4] = { 0, 0, 1, 2 };

/* imports from help.c */

extern help();

/* imports from text.c */

extern TxMsgOK();
extern TxPutMsg();
extern text_putvalue();
extern text_restorebuf();

/* imports from C */

extern char *malloc();

/* imports from main.c */

extern ELT *PICTURE;                /* current PICTURE database      */
extern ELT *cset;                   /* current set database          */
extern ELT *MEN[];                  /* pointers for user symbols     */
extern ELT arhead;                  /* arrow head template           */

extern POINT MENPOINT[];            /* pointers used fo user symbols */
extern POINT *POINTLIST;            /* accumulated point list        */

extern CBRUSH;        		    /* current brush                 */
extern Gridon;                      /* grid mode flag                */
extern SEQ;                         /* point sequence number         */
extern Adjustment;                  /* point adjustment mode         */
extern GravityOn;                   /* gravity mode flag             */
extern CHANGED;                     /* PICTURE changed flag          */
extern CsetOn;			    /* current set displayed on	     */

extern (*lastcommand)();	    /* previous command */
extern lasttext;		    /* TRUE if previous command wants text */
extern struct pixwin *pix_pw;

extern SHUpdate(), SHAgain(), SHDrawArc(), SHDrawCurve(), SHCopy(),
       SHDefineSet(), SHErase(), SHSetArea(), SHGravity(),
       SHGrid(), SHRotate(), SHScale(), SHTranslate(),
       SHDrawVector(), SHMAdjust(), SHBox(), SHArrow(),
       SHSave1(), SHSave2(), SHSave3(), SHSave4();

extern LGUndo();

#define twoPi 6.2832

static char nopnt[18] = "not enough points";
static char noset[15] = "no current set";

/* 
 * The following two arrays define the short commands and the routines
 * that process them.
 */
static char shcmds[] = { '\14', '.', '1', '2', '3', '4', '?', 'a', 'b',
                          'c', 'd', 'e', 'f', 'g', 'q', 'r',
                          's', 't', 'u', 'v', 'w', 'x', 'z', '\0'};

static (*(shrtns[]))() = {
    SHUpdate,			/* redraw screen */
    SHAgain,			/* repeat last command */
    SHSave1,                    /* save user symbol */
    SHSave2,                    /* save user symbol */
    SHSave3,                    /* save user symbol */
    SHSave4,                    /* save user symbol */
    help,			/* help screen */
    SHDrawArc,			/* draw arc */
    SHDrawCurve,		/* draw curve */
    SHCopy,			/* copy current set */
    SHDefineSet,		/* define current set */
    SHErase,			/* erase elements */
    SHSetArea,			/* select area for current set */
    SHGravity,			/* gravity */
    SHGrid,			/* toggle grid display */
    SHRotate,			/* rotate current set */
    SHScale,			/* scale current set */
    SHTranslate,		/* translate current set */
    LGUndo,			/* undo last command */
    SHDrawVector,		/* draw vectors */
    SHArrow,                    /* arrowhead */
    SHBox,                      /* rectangle from two points */
    SHMAdjust			/* manhattan adjust */
};
  

/*
 * SHLookup searches a table of characters to find one that matches a
 * the given character.
 * If c is a valid command character, its index is returned;
 * if c is the null command, -2 is returned, else -1 is returned.
 */
SHLookup(c, table)
char c;				/* char to be looked up */
register char table[];		/* pointer to the valid commands */
{
    register index;

    if ((c == ' ') || (c == '\0')) 
	return(-2);

    for (index=0; table[index] != '\0'; index++) {
        if (table[index] == c) 
	    return(index);
        if (table[index] > c) 
	    return(-1);
    }

    return(-1);
}  /* end SHLookup */


/*
 * This routine reads in, looks up, and executes a short command.
 */
SHCommand(command)
register char *command;
{
    register index;

    if ((index = SHLookup(*command, shcmds)) == -2) 
	return;

    if (index >= 0) {
	GRCurrentSetOn();
	TxMsgOK();
	(*(shrtns[index]))();
    }
    else
	error("no such command");
}  /* end SHCommand */


/*
 * Repeat previous command.
 */
SHAgain()
{
    if (lasttext)
	text_restorebuf();
    (*lastcommand)();
}


/*
 * This routine creates and displays a VECTOR element from the
 * points previously specified.
 */
SHDrawVector()
{
    register POINT *p1, *p2;
    register ELT *e1;
    POINT *plist;
    char *txt;

    if (SEQ < 2) {      /* not enough points */
	error(nopnt);
	return;
    }

    UNForget();

    DISClearSetDisplay();
    DBClearSet();

    plist = PTInit();
    p1 = POINTLIST;
    (void) PTMakePoint(p1->x, p1->y, &plist);
    p2 = PTNextPoint(p1);

    while (!Nullpoint(p2)) {
	(void) PTMakePoint(p2->x, p2->y, &plist);
	p1 = p2;
	p2 = PTNextPoint(p1);
    }

    txt = malloc(1);
    *txt = '\0';
    e1 = DBCreateElt(VECTOR, plist, CBRUSH, 0, txt, &PICTURE);
    DISScreenAdd(e1, pixmask | csetmask);
    DBAddSet(e1);

    CP();
    CHANGED = TRUE;
}  /* end SHDrawVector */


/*
 * This routine creates and displays an ARC element based on the 
 * points previously defined.  If 3 or more points are defined, the
 * extent of the arc is calculated as the angle formed by the 
 * respective lines through the second and third points and the first
 * point.  If only 2 points are specified, a full circle is drawn.
 */
SHDrawArc()
{
    register POINT *p1, *p2;
    register ELT *e1;
    POINT *plist;
    char *txt;
    float a1, a2, angle, radius;

    if (SEQ < 2) {       /* not enough points */
	error(nopnt);
	return;
    }

    UNForget();

    plist = PTInit();
    p1 = POINTLIST;
    p2 = PTNextPoint(p1);
    radius = sqrt((double) ((p2->x - p1->x) * (p2->x - p1->x) +
			    (p2->y - p1->y) * (p2->y - p1->y)));
    if (radius == 0.0) {
	error("zero radius");
	return;
    }

    CSP();

    if (SEQ == 2) {       /* draw full circle */
	angle = 0;
        /* Add extra positioning points  */
	(void) PTMakePoint(p1->x, p1->y, &plist);
	(void) PTMakePoint(p2->x, p2->y, &plist);
	(void) PTMakePoint(p1->x, p1->y + radius, &plist);
	(void) PTMakePoint(p1->x, p1->y - radius, &plist);
	(void) PTMakePoint(p1->x + radius, p1->y, &plist);
	(void) PTMakePoint(p1->x - radius, p1->y, &plist);
    }
    else {
	(void) PTMakePoint(POINTLIST->x, POINTLIST->y, &plist);
	p1 = PTNextPoint(POINTLIST);
	(void) PTMakePoint(p1->x, p1->y, &plist);
	p2 = PTNextPoint(p1);
	a1 =  atan2((p1->x - POINTLIST->x), (p1->y - POINTLIST->y));
	a2 =  atan2((p2->x - POINTLIST->x), (p2->y - POINTLIST->y));
	angle = a1 - a2;
	if (angle < 0.0) 
	    angle += twoPi;

	/* Set second point to lie on arc */
	(void) PTMakePoint((radius * sin(a2) + POINTLIST->x),
			   (radius * cos(a2) + POINTLIST->y), &plist);
	angle *= 360.0/twoPi;      /* convert to degrees */
    }

    DISClearSetDisplay();
    DBClearSet();

    txt = malloc(1);
    *txt = '\0';
    e1 = DBCreateElt(ARC, plist, CBRUSH, (int) (angle + 0.5), txt, &PICTURE);

    DISScreenAdd(e1, pixmask | csetmask);
    DBAddSet(e1);

    CP();
    CHANGED = TRUE;
}  /* end SHDrawARc */


/*
 * Draw curve object.
 */
SHDrawCurve()
{
    register POINT *p1;
    register ELT *e1;
    POINT *plist;
    char *txt;

    if (SEQ < 2) {
	error("need at least 2 points");
	return;
    }

    plist = PTInit();
    p1 = POINTLIST;

    do {
	(void) PTMakePoint(p1->x, p1->y, &plist);
	p1 = PTNextPoint(p1);
    } while (!Nullpoint(p1));

    if (GRSetCurve(plist) != 0) {
	error("too many consecutive knots at same place");
	return;
    }

    UNForget();

    txt = malloc(1);
    *txt = '\0';
    e1 = DBCreateElt(CURVE, plist, CBRUSH, 0, txt, &PICTURE);

    DISClearSetDisplay();
    DBClearSet();
    DISScreenAdd(e1, pixmask | csetmask);
    DBAddSet(e1);

    CP();
    CHANGED = TRUE;
}  /* end SHDrawCurve */


/*
 * This routine erases selected elements from the screen and deletes
 * them from the picture database.  
 */
SHErase()
{
    register ELT *e1;

    if (DBNullelt(cset)) {
	error(noset);
	return;
    }

    UNForget();

    fasterase();

    while (!DBNullelt(cset)) {   /* delete elements in current set */
/*
	DISScreenErase(cset, pixmask | csetmask);
*/
	e1 = DBNextofSet(cset);
	DBDelete(cset, &PICTURE);
	cset = e1;
    }

    CHANGED = TRUE;
}  /* end SHErase */


/*
 * This routine toggles the gravity mode.
 */
SHGravity()
{
    if (GravityOn = !GravityOn)
	MNHighLt(HiMode[3]);
    else
	MNUnHighLt(HiMode[3]);
}  /* End GravityOn */


/*
 * This routine toggles the display of the grid.
 */
SHGrid()
{
    if (Gridon = !Gridon)
	GRDisplayGrid();
    else
	GRBlankGrid();
}  /* end SHGrid */


/*
 * Manhattan Adjust -
 * This routine toggles the adjustment mode.
 */
SHMAdjust()
{
    if (Adjustment == MAN) {
	MNUnHighLt(HiMode[adj[MAN]]);
	Adjustment = NOADJ;
    }
    else {
	if (Adjustment != NOADJ)
	    MNUnHighLt(HiMode[adj[Adjustment]]);
	MNHighLt(HiMode[adj[MAN]]);
	Adjustment = MAN;
    }
}  /* end SHMAdjust */


/*
 * This routine defines the current set based upon previously
 * defined points to select elements.  If no points are specified
 * the entire picture becomes the current set.  In this case, the
 * old current set is not erased (optimization) and any elements
 * not in it are added.  Otherwise, the new current set can not be
 * guaranteed to contain the old current set, and so it is erased
 * and set to empty before adding the new elements.
 */
SHDefineSet()
{
    if (SEQ > 0) {		/* redefine current set */
	DISClearSetDisplay();
	DBClearSet();
    }

    CSP();
    LGIncludeSet();
}  /* end SHDefineSet */


/*
 * This routine defines the current set by selecting all elements
 * bounded by a rectangle whose diagonal is defined by specifed points.
 */
SHSetArea()
{
    if (SEQ < 2) {
	error(nopnt);
	return;
    }

    if (DBNullelt(PICTURE))
	return;

    DISClearSetDisplay();
    DBClearSet();

    SHMSetArea();
}  /* end SHSetArea */


/*
 * This routine ADDS to the current set all elements bounded by a 
 * rectangle whose diagonal is defined by two specifed points.
 */
SHMSetArea()
{
    register ELT *e1;
    float x1, y1, x2, y2;

    if (SEQ < 2) {
	error(nopnt);
	return;
    }

    if (DBNullelt(PICTURE))
	return;

    x1 = POINTLIST->x;
    y1 = POINTLIST->y;
    x2 = PTNextPoint(POINTLIST)->x;
    y2 = PTNextPoint(POINTLIST)->y;
    e1 = PICTURE;

    while (!DBNullelt(e1)) {
	if (DBBounded(e1, x1, y1, x2, y2)) {
	    DISScreenAdd(e1, csetmask);
	    DBAddSet(e1);
	}
	e1 = DBNextElt(e1);
    }

    CP();
}  /* end SHMSetArea */


/*
 * This routine translates the elements in the current set as defined
 * by points.  The translation is accomplished by defining a transformation
 * matrix and calling DBXform.
 */
SHTranslate()
{
    register ELT *e1;
    register POINT *p1;
    float xmat[3][2];

    if (DBNullelt(cset)) {
	error(noset);
	return;
    }

    if (SEQ < 2) {      /* not enough points */
	error(nopnt);
	return;
    }

    UNForget();

    p1 = PTNextPoint(POINTLIST);
    xmat[0][0] = xmat[1][1] = 1;    /* set up translation matrix */
    xmat[1][0] = xmat[0][1] = 0;
    xmat[2][0] = p1->x - POINTLIST->x;
    xmat[2][1] = p1->y - POINTLIST->y;
    e1 = cset;

    fasterase();

    while (!DBNullelt(e1)) {
	/*
	DISScreenErase(e1, pixmask | csetmask);
	*/
	TxMsgOK();
	DBXform(e1, xmat, &PICTURE);
	DISScreenAdd(e1, pixmask | csetmask);
	e1 = DBNextofSet(e1);
    }

    CP();
    CHANGED = TRUE;
}  /* end SHTranslate */


/*
 * This routine copies the elements in the current set as defined
 * by points.  To copy, the current set pointer is cleared so that new
 * elements as added by DBCopy can be used to comprise the new current
 * set.  A pointer is maintained to the old current set which is traversed
 * to determine the elements to be copied.  This process continues for all
 * points specified.
 *
 * NOTE:  This assumes that the DBClearSet routine does not alter the
 *        pointers between elements in the set (which is currently true),
 *        and must be changed it this does not hold.
 */
SHCopy()
{
    register ELT *e1, *e2;
    register POINT *p1, *p2;
    float xmat[3][2];

    if (DBNullelt(cset)) {
	error(noset);
	return;
    }

    if (SEQ < 2) {      /* not enough points */
	error(nopnt);
	return;
    }

    UNForget();

    p1 = POINTLIST;
    p2 = PTNextPoint(POINTLIST);

    while (!Nullpoint(p2)) {
	xmat[0][0] = xmat[1][1] = 1;    /* set up translation matrix */
	xmat[1][0] = xmat[0][1] = 0;
	xmat[2][0] = p2->x - p1->x;
	xmat[2][1] = p2->y - p1->y;
	e1 = cset;

	DISClearSetDisplay();
	DBClearSet();    /* Dependent on Clearset preserving pointers */

	while (!DBNullelt(e1)) {
	    e2 = DBCopy(e1, xmat, &PICTURE);
	    DISScreenAdd(e2, pixmask | csetmask);
	    DBAddSet(e2);
	    e1 = DBNextofSet(e1);
	}

	p1 = p2;
	p2 = PTNextPoint(p2);
    }

    CP();
    CHANGED = TRUE;
}  /* end SHCopy */


/*
 * This routine rotates the elements in the current set as defined
 * by points.  The rotation is accomplished by defining a transformation
 * matrix and calling DBXform.
 */
SHRotate()
{
    register ELT *elt;
    register POINT *p1, *p2;
    POINT pos;
    float xmat[3][2], angle, s, c;

    if (DBNullelt(cset)) {
	error(noset);
	return;
    }

    if (SEQ < 3) {      /* not enough points */
	error(nopnt);
	return;
    }

    UNForget();

    p1 = PTNextPoint(POINTLIST);  /* calculate rotation angle */
    p2 = PTNextPoint(p1);
    angle = (float) atan2((p2->x - POINTLIST->x), (p2->y - POINTLIST->y)) -
	    (float) atan2((p1->x - POINTLIST->x), (p1->y - POINTLIST->y));
    s = (float) sin(angle);      
    c = (float) cos(angle);      

    /* Define transformation matrix to translate set to origin, rotate,
      and translate back. */

    xmat[0][0] = c;
    xmat[0][1] = -s;
    xmat[1][0] = s;
    xmat[1][1] = c;
    xmat[2][0] = (-c) * POINTLIST->x - s * POINTLIST->y + POINTLIST->x;
    xmat[2][1] = (-c) * POINTLIST->y + s * POINTLIST->x + POINTLIST->y;

    elt = cset;
    /* DISClearSetDisplay(); */

    fasterase();

    while (!DBNullelt(elt)) {
	/* DISScreenErase(elt, pixmask); */
	TxMsgOK();
	DBXform(elt, xmat, &PICTURE);

	if (TEXT(elt->type)) {
	    GRSetTextPos(elt->textpt, elt->type, elt->brushf, elt->size, 
						elt->ptlist, &pos);
	    elt->ptlist = PTMakeTextPoints(elt->textpt, elt->brushf, elt->size, 
						elt->ptlist, &pos);
	}

	DISScreenAdd(elt, pixmask | csetmask);
	elt = DBNextofSet(elt);
    }

    CP();
    CHANGED = TRUE;
}  /* end SHRotate */


/*
 * This routine scales the elements in the current set as defined
 * by points.  The scaling is accomplished by defining a transformation
 * matrix and calling DBXform.
 */
SHScale()
{
    register ELT *elt;
    register POINT *p1, *p2;
    POINT pos;
    float xmat[3][2], d1, d2, scalex, scaley;

    if (DBNullelt(cset)) {
	error(noset);
	return;
    }

    if (SEQ < 3) {      /* not enough points */
	error(nopnt);
	return;
    }

    UNForget();

    p1 = PTNextPoint(POINTLIST);
    p2 = PTNextPoint(p1);
    d1 = sqrt(pow((p1->x - POINTLIST->x), 2.0) + 
	      pow((p1->y - POINTLIST->y), 2.0));
    d2 = sqrt( pow((p2->x - POINTLIST->x), 2.0) + 
	      pow((p2->y - POINTLIST->y), 2.0));

    if (d1 == 0) {
	error("infinite scale");
	return;
    }

    scalex = scaley = d2 / d1;

    /* create transformation matrix to translate set to origin, 
       performaing the scaling and translating back */

    xmat[0][0] = scalex;
    xmat[1][1] = scaley;
    xmat[1][0] = xmat[0][1] = 0;
    xmat[2][0] = - POINTLIST->x * (scalex - 1.0);
    xmat[2][1] = - POINTLIST->y * (scaley - 1.0);

    elt = cset;
    fasterase();
    /* DISClearSetDisplay(); */

    while (!DBNullelt(elt)) {
	/* DISScreenErase(elt, pixmask); */
	TxMsgOK();
	DBXform(elt, xmat, &PICTURE);

	if (TEXT(elt->type)) {
	    GRSetTextPos(elt->textpt, elt->type, elt->brushf, 
					    elt->size, elt->ptlist, &pos);
	    elt->ptlist = PTMakeTextPoints(elt->textpt, elt->brushf, 
					    elt->size, elt->ptlist, &pos);
	}

	DISScreenAdd(elt, pixmask | csetmask);
	elt = DBNextofSet(elt);
    }

    CP();
    CHANGED = TRUE;
}  /* end SHScale */


/*
 * This routine redraws the graphics screen by clearing the screen ,
 * redisplaying the menu and adding each element back to the display.
 */
SHUpdate()
{
    register ELT *e1;
    register POINT *plist;
    POINT pos;
    register i;

    GRClear(pixmask | csetmask);

    if (Gridon)
	GRDisplayGrid();

    e1 = PICTURE;
    while (!DBNullelt(e1)) {
	if (DBInCset(e1))
	    DISScreenAdd(e1, pixmask | csetmask);
	else
	    DISScreenAdd(e1, pixmask);
	e1 = DBNextElt(e1);
    }

    CsetOn = 1;

    i = 0;
    plist = POINTLIST;
    while (!Nullpoint(plist)) {
	GRDisplayPoint(plist->x, plist->y, i++);
	plist = PTNextPoint(plist);
    }
}  /* end SHUpdate */


/*
 * This local routine stores the current set in the specified
 * user symbol.
 */
static 
savemen(sym)
register sym;
{
    register ELT *elt;
    float xmat[3][2];

    xmat[0][0] = xmat[1][1] = 1;	/* set up copy transformation */
    xmat[0][1] = xmat[1][0] = 0;	/* matrix for no transformation */
    xmat[2][0] = xmat[2][1] = 0;

    while (!DBNullelt(MEN[sym])) {	/* clear out existing symbols */
	elt = DBNextElt(MEN[sym]);
	DBClearElt(MEN[sym]);     
	MEN[sym] = elt;
    }

    elt = cset;				/* copy current set to symbol */

    while (!DBNullelt(elt)) {
	(void) DBCopy(elt, xmat, &(MEN[sym]));
	elt = DBNextofSet(elt);
    }

    if (SEQ == 0) {			/* no positioning points */
	MENPOINT[sym].x = 0;
	MENPOINT[sym].y = 0;
    }
    else {
	MENPOINT[sym].x = POINTLIST->x;
	MENPOINT[sym].y = POINTLIST->y;
    }

    if (!DBNullelt(MEN[sym]))
	MNHighLt(HiBuffer[sym]);
    else
	MNUnHighLt(HiBuffer[sym]);

    CP();
    CHANGED = TRUE;
}  /* end savemen */


/*
 * This routine saves the current set in user symbol 1 by
 * calling savemen.
 */
SHSave1()
{
    savemen(0);
}


/*
 * This routine saves the current set in user symbol 2 by
 * calling savemen.
 */
SHSave2()
{
    savemen(1);
}


/*
 * This routine saves the current set in user symbol 3 by
 * calling savemen.
 */
SHSave3()
{
    savemen(2);
}


/*
 * This routine saves the current set in user symbol 4 by
 * calling savemen.
 */
SHSave4()
{
    savemen(3);
}


/*
 * This routine creates and displays a rectangle whose diagonal is
 * defined by two points.  The routine uses the coordinates of these
 * points to define a VECTOR element with the appropriate vertices.
 */
SHBox()
{
    register POINT *p1, *p2;
    register ELT *e1;
    POINT *plist;
    char *txt;
    
    if (SEQ < 2) {
	error("not enough points");
	return;
    }

    UNForget();

    p1 = POINTLIST;
    p2 = PTNextPoint(p1);
    plist = PTInit();   /* create points for vector elements which define 
			   the rectangle */
    (void) PTMakePoint(p1->x, p1->y, &plist);
    (void) PTMakePoint(p1->x, p2->y, &plist);
    (void) PTMakePoint(p2->x, p2->y, &plist);
    (void) PTMakePoint(p2->x, p1->y, &plist);
    (void) PTMakePoint(p1->x, p1->y, &plist);   /* close rectangle */
    txt = malloc(1);
    *txt = '\0';

    DISClearSetDisplay();
    DBClearSet();
    e1 = DBCreateElt(VECTOR, plist, CBRUSH, 0, txt, &PICTURE);
    DISScreenAdd(e1, pixmask | csetmask);
    DBAddSet(e1);

    CP();
    CHANGED = TRUE;
}  /* end SHBox */


/*
 * This routine draws arrow heads by 'copying' the arrow head template
 * into the picture appropriately transformed.
 */
SHArrow()
{
    register ELT *e1;
    POINT p1;
    register POINT *p2;
    float xmat[3][2], angle, s, c;

    if (SEQ < 2) {      /* not enough points */
	error(nopnt);
	return;
    }

    UNForget();

    p1.x = POINTLIST->x - 1;
    p1.y = POINTLIST->y;
    p2 = PTNextPoint(POINTLIST);
    angle = (float) atan2((p2->x - POINTLIST->x),(p2->y - POINTLIST->y)) -
	    (float) atan2((p1.x - POINTLIST->x),(p1.y - POINTLIST->y));
    s = (float) sin(angle);      
    c = (float) cos(angle);      

   /* Define transformation matrix to translate element from origin
      and rotate. */

    xmat[0][0] = c;
    xmat[0][1] = -s;
    xmat[1][0] = s;
    xmat[1][1] = c;
    xmat[2][0] = POINTLIST->x;
    xmat[2][1] = POINTLIST->y;

    DISClearSetDisplay();	/* the new current set */
    DBClearSet();		/* clear old set in preparation to make */
    arhead.brushf = CBRUSH;
    e1 = DBCopy(&arhead, xmat, &PICTURE);
    DISScreenAdd(e1, pixmask | csetmask);
    DBAddSet(e1);

    CP();
    CHANGED = TRUE;
}  /* end SHArrow */

/*
 * Turn off any "showpoints" and erase the current set by XORing
 * the cset pixrect with the picture.  ONLY to be used when the
 * current set will be redrawn immediately afterwards.
 */
fasterase()
{
    CSP();			/* clear show points */

    GRCurrentSetOff();		/* erase current set */
    CsetOn = 1;			/* ON, the current must be redrawn */
    GRClear(csetmask);
}
