/* @(#)short.c	1.3	%G%
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 *
 *      This file contains routines to implement the short commands
 * of the gremlin picture editor.
 *
 */

#include "gremlin.h"
#include "grem2.h"
#include <signal.h>
#include <sgtty.h>

/* imports from graphics files */

extern GRVector(), GRPutText();
extern GRCurve();
extern GRClear();
extern int GRArc(); 
extern charxsize, charysize;

/* imports from display.c */

extern DISScreenAdd(), DISScreenErase();
extern DISDisplaySet(), DISEraseSet(), DISClearSetDisplay();

/* imports from database files */

extern ELT *DBInit(), *DBCreateElt(); 
extern DBDelete(), DBGravitate(), DBClearElt();
extern ELT *DBCopy();
extern DBXform(), DBBounded();
extern DBAddSet(), DBClearSet();
extern POINT *PTInit(), *PTMakePoint();
extern PTDeletePoint();

/* imports from long.c */

extern LGIncludeSet();

/* imports from menu.c  */

extern MNHighLt(), MNUnHighLt();
extern HiMen[], HiFont[], HiBrush[], HiMode[];

int adj[4] = { 0, 0, 1, 2 };

/* imports from textio.c */

extern TxMsgOK(), TxPutString(), TxGetLine(), TxRedisplay(), TxPutMsg();
extern char TxGetChar();
extern TXFIELD TAlign, TAdjust, TBrush, TFont, TGravity, TCSize;
extern TXFIELD TEdit, TJustmode;

/* imports from c */

extern char *malloc();
extern char *sprintf();

/* imports from main.c */

extern globinit();                  /* global initialization routine */
extern ELT *PICTURE;                /* current PICTURE database      */
extern ELT *cset;                   /* current set database          */
extern CBRUSH, CSIZE, CFONT;        /* current brush, size, font     */
extern CJUST;                       /* text justification mode       */
extern Gridon;                      /* grid mode flag                */
extern Orientation;                 /* orientation of workspace      */
extern Alignment;                   /* point alignment indicator     */
extern float PX, PY;                /* cursor coordinates            */
extern float Lastx, Lasty;          /* previous cursor coordinates   */
extern SEQ;                         /* point sequence number         */
extern char *Editfile;              /* current edit file             */
extern POINT *POINTLIST;            /* accumulated point list        */
extern Adjustment;                  /* point adjustment mode         */
extern GravityOn;                   /* gravity mode flag             */
extern Consume;                     /* point clear flag              */
extern CHANGED;                     /* PICTURE changed flag          */
extern ELT *MEN[];                  /* pointers for user symbols     */
extern POINT MENPOINT[];            /* pointers used fo user symbols */
extern cmdbuf[];                    /* line buffer for commands      */
extern ELT arhead;                  /* arrow head template           */
extern char *textpos[], *dispmode[];/* text positioning modes        */
extern int textmode[];              /* text positioning              */


extern SHUpdate(), SHDrawArc(), SHDrawCurve(), SHCopy(), SHRedis(),
       SHDefineSet(), SHErase(), SHSetArea(), SHGravity(),
       SHGrid(), SHRotate(), SHScale(), SHTranslate(), SHShellEsc(),
       SHDrawVector(), SHMAdjust(), SHBox(), SHArrow(),
       SHSave1(), SHSave2(), SHSave3(), SHSave4();

/* The following two arrays define the short commands and the routines
 * that process them.
 */
static char shcmds[] = { '\14', '!', '1', '2', '3', '4', 'a', 'b',
                          'c', 'd', 'e', 'f', 'g', 'l', 'q', 'r',
                          's', 't', 'v', 'w', 'x', 'z', '\0'};
static (*(shrtns[]))() = {
    SHUpdate,			/* redraw screen */
    SHShellEsc,                 /* shell escape */
    SHSave1,                    /* save user symbol */
    SHSave2,                    /* save user symbol */
    SHSave3,                    /* save user symbol */
    SHSave4,                    /* save user symbol */
    SHDrawArc,			/* draw arc */
    SHDrawCurve,		/* draw curve */
    SHCopy,			/* copy current set */
    SHDefineSet,		/* define current set */
    SHErase,			/* erase elements */
    SHSetArea,			/* select area for current set */
    SHGravity,			/* gravity */
    SHRedis,                    /* text screen redisplay */
    SHGrid,			/* toggle grid display */
    SHRotate,			/* rotate current set */
    SHScale,			/* scale current set */
    SHTranslate,		/* translate current set */
    SHDrawVector,		/* draw vectors */
    SHArrow,                    /* arrowhead */
    SHBox,                      /* rectangle from two points */
    SHMAdjust};			/* manhattan adjust */
  

int
SHLookup(c, table)
char c;				/* Pointer to a char to be looked up */
char table[];			/* Pointer to an array of characters
				 * which are the valid commands.  The array
				 * must be sorted by ascii value.
				 */

/*---------------------------------------------------------
 *	SHLookup searches a table of characters to find one that matches a
 *	the given character.
 *
 *      Results: If c is a valid command character, its index is 
 * returned, otherwise, if c is the null command, -2 is returned
 * else -1 is returned.
 *
 *	Side Effects:	None.
 *
 * (Modified from software written by John Ousterhout for the caesar
 *  program)
 *---------------------------------------------------------
 */

{
    int index;

    if ((c == ' ') || (c == '\0')) return(-2);
    for (index=0; table[index] != '\0'; index++)
    {
        if (table[index] == c) return(index);
        if (table[index] > c) return(-1);
    }
    return(-1);
}  /* end SHLookup */


SHCommand(command)
char *command;

/*---------------------------------------------------------
 *	This routine reads in, looks up, and executes a long command.
 *
 *	Results:	None.
 *
 *	Side Effects:
 *	Depends on the command that is invoked.
 *---------------------------------------------------------
 */

{
    int index;
    index = SHLookup(*command, shcmds);
    if (index == -2) return;
    if (index >= 0)
    {

	(*(shrtns[index]))();
    }
    else
    {
	error("no such command");
    }
}  /* end SHCommand */

static char nopnt[18] = "not enough points";
static char noset[15] = "no current set";

SHDrawVector()
/*
 *      This routine creates and displays a VECTOR element from the
 * points previously specified.
 */

{
	ELT *e1;
	POINT *plist, *p1, *p2;
	char *txt;

	if (SEQ < 2)        /* not enough points */
	{
		error(nopnt);
		return;
	}
	DBClearSet();         /* Clear current set in preparation */
	DISClearSetDisplay(); /* for making this the new one */
	GRsetwmask(linemask | setmask);
	plist = PTInit();
	p1 = POINTLIST;
	(void) PTMakePoint(p1->x, p1->y, &plist);
	p2 = PTNextPoint(p1);
	while ( !Nullpoint(p2) )
	{
		(void) PTMakePoint(p2->x, p2->y, &plist);
		GRVector(p1, p2, CBRUSH);
		p1 = p2;
		p2 = PTNextPoint(p1);
	}  /* end while */;
	txt = malloc(1);
	*txt = '\0';
	e1 = DBCreateElt(VECTOR, plist, CBRUSH, 0, txt, &PICTURE);
	DBAddSet(e1);
	CHANGED = TRUE;
}  /* end SHDrawVector */

#define twoPi 6.2832

SHDrawArc()
/*
 *      This routine creates and displays an ARC element based on the 
 * points previously defined.  If 3 or more points are defined, the
 * extent of the arc is calculated as the angle formed by the 
 * respective lines through the second and third points and the first
 * point.  If only 2 points are specified, a full circle is drawn.
 */

{
	ELT *e1;
	POINT *plist, *p1, *p2;
	char *txt;
	double a1, a2, angle, radius;
	int stat;

	if (SEQ < 2)         /* not enough points */
	{
		error(nopnt);
		return;
	}
	plist = PTInit();
	p1 = POINTLIST;
	p2 = PTNextPoint(p1);
	radius = sqrt( pow((p2->x - p1->x), 2.0)
	             + pow((p2->y - p1->y), 2.0));
	if (SEQ == 2)         /* draw full circle */
	{
		angle = 0;
         /* Add extra positioning points  */
		(void) PTMakePoint(p1->x, p1->y, &plist);
		(void) PTMakePoint(p2->x, p2->y, &plist);
		(void) PTMakePoint(p1->x, p1->y + radius, &plist);
		(void) PTMakePoint(p1->x, p1->y - radius, &plist);
		(void) PTMakePoint(p1->x + radius, p1->y, &plist);
		(void) PTMakePoint(p1->x - radius, p1->y, &plist);
	}  /* end if */
	else
	{
		(void) PTMakePoint(POINTLIST->x, POINTLIST->y, &plist);
		p1 = PTNextPoint(POINTLIST);
		(void) PTMakePoint(p1->x, p1->y, &plist);
		p2 = PTNextPoint(p1);
		a1 =  atan2((p1->x - POINTLIST->x), (p1->y - POINTLIST->y));
		a2 =  atan2((p2->x - POINTLIST->x), (p2->y - POINTLIST->y));
		angle = a1 - a2;
		if (angle < 0.0) angle += twoPi;

		       /* Set second point to lie on arc */
		(void) PTMakePoint((radius * sin(a2) + POINTLIST->x),
		                    (radius * cos(a2) + POINTLIST->y),
		                     &plist);

		angle *= 360.0/twoPi;      /* convert to degrees */
	};
	p1 = PTNextPoint(POINTLIST);
	DBClearSet();         /* Clear current set, this element becomes */
	DISClearSetDisplay(); /* the new current set                     */
	GRsetwmask(linemask | setmask);
	stat = GRArc(POINTLIST, p1, angle, CBRUSH);
	if (stat > -1)
	{
		txt = malloc(1);
		*txt = '\0';
		e1 = DBCreateElt(ARC, plist, CBRUSH, (int) (angle + 0.5),
	                 	txt, &PICTURE);
		DBAddSet(e1);
		CHANGED = TRUE;
	}
}  /* end SHDrawARc */


SHDrawCurve()
/*
 *      This routine creates and displays a curve using points previously
 * defined.  This is dependent on DISCurve which is not yet implemented.
 */

{
	ELT *e1;
	char *txt;
	POINT *p1, *p2, *plist;
	int stat;

	stat = 0;
	if (SEQ < 2)       /* not enough points */
	{
		error("need at least 2 points");
		return;
	}
	plist = PTInit();
	p1 = POINTLIST;
	(void) PTMakePoint(p1->x, p1->y, &plist);
	p2 = PTNextPoint(p1);
	while ( !Nullpoint(p2) )
	{
		(void) PTMakePoint(p2->x, p2->y, &plist);
		p1 = p2;
		p2 = PTNextPoint(p1);
	}  /* end while */;
	DBClearSet();         /* Clear current set, this element */
	DISClearSetDisplay(); /* the new current set             */
	GRsetwmask(linemask | setmask);
	stat = GRCurve(plist, CBRUSH);
	if (stat != 0)   /* bad knots */
	{
		error("too many consecutive knots at same place");
		return;
	}
	txt = malloc(1);
	*txt = '\0';
	e1 = DBCreateElt(CURVE, plist, CBRUSH, 0, txt, &PICTURE);
	DBAddSet(e1);
	CHANGED = TRUE;
}  /* end SHDrawCurve */

SHErase()
/*
 *     This routine erases selected elements from the screen and deletes
 * them from the picture database.  
 */

{
	ELT *e1;

	while ( !DBNullelt(cset) )     /* delete elements in current set */
	{
		DISScreenErase(cset, (linemask | setmask));
		e1 = DBNextofSet(cset);
		DBDelete(cset, &PICTURE);
		cset = e1;
	};
	CHANGED = TRUE;
}  /* end SHErase */


SHGravity()
/*
 *      This routine toggles the gravity mode.
 */

{
	if (GravityOn) 
	{
		MNUnHighLt(HiMode[3]);
		GravityOn = FALSE;
		TxPutString(&TGravity, "OFF");
	}
	else    
	{
		MNHighLt(HiMode[3], hicolor);
		GravityOn = TRUE;
		TxPutString(&TGravity, " ON");
	}
	Consume = FALSE;
}  /* End GravityOn */

SHGrid()
/*
 *      This routine toggles the display of the grid 
 */

{
	if (Gridon)
	{
		Gridon = FALSE;
		GRBlankGrid();
	}
	else
	{
		Gridon = TRUE;
		GRDisplayGrid();
	}
	Consume = FALSE;
}  /* end SHGrid */


SHMAdjust()
/*
 * Manhattan Adjust -
 *      This routine toggles the adjustment mode.
 */

{
	if (Adjustment == MAN)
	{
		MNUnHighLt(HiMode[adj[MAN]]);
		Adjustment = NOADJ;
		TxPutString(&TAdjust, "NO ADJUSTMENT");
	}
	else    
	{
		MNUnHighLt(HiMode[adj[Adjustment]]);
		MNHighLt(HiMode[adj[MAN]], hicolor);
		Adjustment = MAN;
		TxPutString(&TAdjust, "  MANHATTAN  ");
	}
	Consume = FALSE;
}


SHDefineSet()
/*
 *      This routine defines the current set based upon previously
 * defined points to select elements.  The action is performed by
 * clearing the current set and calling LGIncludeSet.
 */

{
	DBClearSet();
	DISClearSetDisplay();
	LGIncludeSet("");
}  /* end SHDefineSet */

SHSetArea()
/*
 *      This routine defines the current set by selecting all elements
 * bounded by a rectangle whose diagonal is defined by specifed points.
 */

{
	ELT *e1;
	POINT *p1;
	float x1, y1, x2, y2;

	if (SEQ < 2)
	{
		error(nopnt);
		return;
	}
	DBClearSet();
	DISClearSetDisplay();
	x1 = POINTLIST->x;
	y1 = POINTLIST->y;
	p1 = PTNextPoint(POINTLIST);
	x2 = p1->x;
	y2 = p1->y;
	e1 = PICTURE;
	while ( !DBNullelt(e1) )
	{
		if ( DBBounded(e1, x1, y1, x2, y2) )
		{
			DBAddSet(e1);
			DISDisplaySet(e1);
		}  /* end if */
		e1 = DBNextElt(e1);
	}  /* end while */
}  /* end SHSetArea */


SHTranslate()
/*
 *      This routine translates the elements in the current set as defined
 * by points.  The translation is accomplished by defining a transformation
 * matrix and calling DBXform.
 */

{
	ELT *e1;
	POINT *p1;
	float xmat[3][2];

	if (SEQ < 2)        /* not enough points */
	{
		error(nopnt);
		return;
	}
	if (DBNullelt(cset))
	{
		error(noset);
		return;
	}
	p1 = PTNextPoint(POINTLIST);
	xmat[0][0] = xmat[1][1] = 1;    /* set up translation matrix */
	xmat[1][0] = xmat[0][1] = 0;
	xmat[2][0] = p1->x - POINTLIST->x;
	xmat[2][1] = p1->y - POINTLIST->y;
	e1 = cset;
	while ( !DBNullelt(e1) )
	{
		DISScreenErase(e1, (linemask | setmask));
		TxMsgOK();
		DBXform(e1, xmat, &PICTURE);
		DISScreenAdd(e1, (linemask | setmask));
		e1 = DBNextofSet(e1);
	}  /* end while */
	CHANGED = TRUE;
}  /* end SHTranslate */


SHCopy()
/*
 *      This routine copies the elements in the current set as defined
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

{
	ELT *e1, *e2;
	POINT *p1, *p2;
	float xmat[3][2];

	if (SEQ < 2)        /* not enough points */
	{
		error(nopnt);
		return;
	}
	if (DBNullelt(cset))
	{
		error(noset);
		return;
	}
	p1 = POINTLIST;
	p2 = PTNextPoint(POINTLIST);
	while ( !Nullpoint(p2) )
	{
		xmat[0][0] = xmat[1][1] = 1;    /* set up translation matrix */
		xmat[1][0] = xmat[0][1] = 0;
		xmat[2][0] = p2->x - p1->x;
		xmat[2][1] = p2->y - p1->y;
		DISClearSetDisplay();
		e1 = cset;
		DBClearSet();    /* Dependent on Clearset preserving pointers */
		while ( !DBNullelt(e1) )
		{
			e2 = DBCopy(e1, xmat, &PICTURE);
			DBAddSet(e2);
			DISScreenAdd(e2, (linemask | setmask));
			e1 = DBNextofSet(e1);
		}  /* end while ! null elt */
		p1 = p2;
		p2 = PTNextPoint(p2);
	}  /* end while ! null point */
	CHANGED = TRUE;
}  /* end SHCopy */


SHRotate()
/*
 *      This routine rotates the elements in the current set as defined
 * by points.  The rotation is accomplished by defining a transformation
 * matrix and calling DBXform.
 */

{
	ELT *e1;
	POINT pos, *p1, *p2;
	float xmat[3][2], angle, s, c;
	int i, j;

	if (SEQ < 3)        /* not enough points */
	{
		error(nopnt);
		return;
	}
	if (DBNullelt(cset))
	{
		error(noset);
		return;
	}
	p1 = PTNextPoint(POINTLIST);  /* calculate rotation angle */
	p2 = PTNextPoint(p1);
	angle = (float) atan2((p2->x - POINTLIST->x),(p2->y - POINTLIST->y))
	       -(float) atan2((p1->x - POINTLIST->x),(p1->y - POINTLIST->y));
	s = (float) sin(angle);      
	c = (float) cos(angle);      

   /* Define transformation matrix to translate set to origin, rotate,
      and translate back.                                              */

	xmat[0][0] = c;
	xmat[0][1] = -s;
	xmat[1][0] = s;
	xmat[1][1] = c;
	xmat[2][0] = (-c) * POINTLIST->x - s * POINTLIST->y + POINTLIST->x;
	xmat[2][1] = (-c) * POINTLIST->y + s * POINTLIST->x + POINTLIST->y;
	e1 = cset;
	while ( !DBNullelt(e1) )
	{
		DISScreenErase(e1, (linemask | setmask));
		TxMsgOK();
		DBXform(e1, xmat, &PICTURE);
		if (TEXT(e1->type))
		{
			GRsetwmask(textmask | setmask);
			p1 = e1->ptlist;
			GRPutText(e1->type, p1, e1->brushf,
				  e1->size,e1->textpt, &pos);
			i= strlen(e1->textpt);
			p2 = PTInit();
			(void) PTMakePoint(p1->x, p1->y, &p2);
                   	    /* add extra positioning points */
			(void) PTMakePoint(pos.x, pos.y, &p2);   
			(void) PTMakePoint(pos.x + i * charxsize / 2, 
					   pos.y, &p2);
			(void) PTMakePoint(pos.x + i * charxsize, 
					   pos.y, &p2);
			e1->ptlist = p2;
		}  /* end if TEXT */
		else
			DISScreenAdd(e1, (linemask | setmask));
		e1 = DBNextofSet(e1);
	}  /* end while */
	CHANGED = TRUE;
}  /* end SHRotate */


SHScale()
/*
 *      This routine scales the elements in the current set as defined
 * by points.  The scaling is accomplished by defining a transformation
 * matrix and calling DBXform.
 */

{
	ELT *e1;
	POINT pos, *p1, *p2;
	float xmat[3][2], d1, d2, scalex, scaley;
	int i, j;

	if (SEQ < 3)        /* not enough points */
	{
		error(nopnt);
		return;
	}
	if (DBNullelt(cset))
	{
		error(noset);
		return;
	}
	p1 = PTNextPoint(POINTLIST);
	p2 = PTNextPoint(p1);
	d1 = sqrt( pow((p1->x - POINTLIST->x), 2.0)
	         + pow((p1->y - POINTLIST->y), 2.0));
	d2 = sqrt( pow((p2->x - POINTLIST->x), 2.0)
	         + pow((p2->y - POINTLIST->y), 2.0));
	if (d1 == 0)
	{
		error("infinite scale");
		return;
	}
	scalex = scaley = d2 / d1;

    /* create transformation matrix to translate set to origin, 
       performaing the scaling and translating back               */

	xmat[0][0] = scalex;
	xmat[1][1] = scaley;
	xmat[1][0] = xmat[0][1] = 0;
	xmat[2][0] = - POINTLIST->x * (scalex - 1.0);
	xmat[2][1] = - POINTLIST->y * (scaley - 1.0);
	e1 = cset;
	while ( !DBNullelt(e1) )
	{
		DISScreenErase(e1, (linemask | setmask));
		TxMsgOK();
		DBXform(e1, xmat, &PICTURE);
		if (TEXT(e1->type))
		{
			GRsetwmask(textmask | setmask);
			p1 = e1->ptlist;
			GRPutText(e1->type, p1, e1->brushf,
				  e1->size,e1->textpt, &pos);
			i= strlen(e1->textpt);
			p2 = PTInit();
			(void) PTMakePoint(p1->x, p1->y, &p2);
                   	/* add extra positioning points */
			(void) PTMakePoint(pos.x, pos.y, &p2);   
			(void) PTMakePoint(pos.x + i * charxsize / 2, 
					   pos.y, &p2);
			(void) PTMakePoint(pos.x + i * charxsize, 
					   pos.y, &p2);
			e1->ptlist = p2;
		}  /* end if TEXT */
		else
			DISScreenAdd(e1, (linemask | setmask));
		e1 = DBNextofSet(e1);
	}  /* end while */
	CHANGED = TRUE;
}  /* end SHScale */


SHUpdate()
/*
 *      This routine redraws the graphics screen by clearing the screen ,
 * redisplaying the menu and adding each element back to the display.
 */

{
	ELT *e1;
	int i;

	GRClear(linemask | setmask | textmask);
	MNDisplayMenu();
	e1 = PICTURE;
	while ( !DBNullelt(e1) )
	{
		DISScreenAdd(e1, linemask);
		e1 = DBNextElt(e1);
	};
	e1 = cset;
	while ( !DBNullelt(e1) )
	{
		DISScreenAdd(e1, setmask);
		e1 = DBNextofSet(e1);
	};
	if ( Adjustment != NOADJ) MNHighLt(HiMode[adj[Adjustment]], hicolor);
	if (GravityOn) MNHighLt(HiMode[3], hicolor); /* re highlight  */
	MNHighLt(HiFont[CFONT-1], hicolor);          /* selected menu */
	MNHighLt(HiBrush[CBRUSH-1], hicolor);        /* menu attributes */
	for (i = 0; i < NUSER; ++i)
		if ( !DBNullelt(MEN[i]) ) MNHighLt(HiMen[i], hicolor);
	Consume = FALSE;
}  /* end SHUpdate */


SHRedis()
/*
 *      This routine is used to redisplay the text screen.
 * It clears the screen, then redisplays the text and each of the 
 * defined fields.
 */

{
	char string[4];

	TxRedisplay();
	TxPutString(&TEdit, Editfile);
	(void) sprintf(string,"%1d",CBRUSH);
	TxPutString(&TBrush,string);
	(void) sprintf(string, "%1d",CFONT);
	TxPutString(&TFont,string);
	(void) sprintf(string, "%1d",CSIZE);
	TxPutString(&TCSize,string);
	TxPutString(&TJustmode,dispmode[CJUST]);
	(void) sprintf(string, "%3d",Alignment);
	TxPutString(&TAlign,string);
	(void) sprintf(string,"%1d",CBRUSH);
	TxPutString(&TBrush,string);
	if (GravityOn) TxPutString(&TGravity, " ON");
	else TxPutString(&TGravity, "OFF");
	switch(Adjustment)
	{
	  case NOADJ: TxPutString(&TAdjust, "NO ADJUSTMENT");
	              break;
	   case HORZ: TxPutString(&TAdjust, " HORIZONTAL  ");
	              break;
	   case VERT: TxPutString(&TAdjust, "  VERTICAL   ");
	              break;
	    case MAN: TxPutString(&TAdjust, "  MANHATTAN  ");
	              break;
	}  /* end switch */;
        Consume = FALSE;
}  /* end SHRedis */


SHShellEsc()
/* 
 *      This routine performs a shell escape through the c 'system'
 * function.  It first retrieves the command line through TxGetLine.
 */

{
	char line[80];

	TxGetLine("!",line,80);
	TxClose();     /* Restore text terminal to 'normal' state */
	(void) system(line);  /* do command */
          /* allow time for user to digest command */
	printf("Type <cr> to continue");
	(void) fflush(stdout);
	line[1] = TxGetChar();
	SHRedis();  /* reclaim terminal */
}  /* end ShellEsc */


static savemen(sym)
int sym;
/*
 *      This local routine stores the current set in the specified
 * user symbol.
 */

{
	ELT *elist;
	float xmat[3][2];

	xmat[0][0] = xmat[1][1] = 1;  /* set up copy transformation */
	xmat[0][1] = xmat[1][0] = 0;  /* matrix for no              */
	xmat[2][0] = xmat[2][1] = 0;  /* transformation             */
	while ( !DBNullelt(MEN[sym]) )
	{                             /* clear out existing symbol */
		elist = DBNextElt(MEN[sym]);
		DBClearElt(MEN[sym]);     
		MEN[sym] = elist;
	};
	elist = cset;          /* copy current set to symbol */
	while ( !DBNullelt(elist) )
	{
		(void) DBCopy(elist, xmat, &(MEN[sym]));
		elist = DBNextofSet(elist);
	}  /* end while */;
	if (SEQ == 0)     /* no positioning points */
	{
		MENPOINT[sym].x = 0;
		MENPOINT[sym].y = 0;
	}
	else
	{
		MENPOINT[sym].x = POINTLIST->x;
		MENPOINT[sym].y = POINTLIST->y;
	}
	if ( !DBNullelt(MEN[sym]) ) MNHighLt(HiMen[sym], hicolor);
	else     MNUnHighLt(HiMen[sym]);
	CHANGED = TRUE;
}  /* end savemen */

SHSave1()
/*
 *      This routine saves the current set in user symbol 1 by
 * calling savemen.
 */

{
	savemen(0);
}

SHSave2()
/*
 *      This routine saves the current set in user symbol 2 by
 * calling savemen.
 */

{
	savemen(1);
}

SHSave3()
/*
 *      This routine saves the current set in user symbol 3 by
 * calling savemen.
 */

{
	savemen(2);
}

SHSave4()
/*
 *      This routine saves the current set in user symbol 4 by
 * calling savemen.
 */

{
	savemen(3);
}



SHBox()
/*
 *      This routine creates and displays a rectangle whose diagonal is
 * defined by two points.  The routine uses the coordinates of these
 * points to define a VECTOR element with the appropriate vertices.
 */

{
	POINT *plist, *p1, *p2;
	char *txt;
	ELT *e1;
	
	if (SEQ < 2)
	{
		error("not enough points");
		return;
	}
	p1 = POINTLIST;
	p2 = PTNextPoint(p1);
	plist = PTInit();   /* create points for vector element which defines 
	                       the rectangle                                 */
	(void) PTMakePoint(p1->x, p1->y, &plist);
	(void) PTMakePoint(p1->x, p2->y, &plist);
	(void) PTMakePoint(p2->x, p2->y, &plist);
	(void) PTMakePoint(p2->x, p1->y, &plist);
	(void) PTMakePoint(p1->x, p1->y, &plist);   /* close rectangle */
	txt = malloc(1);
	*txt = '\0';
	DBClearSet();         /* clear old set in preparation to make */
	DISClearSetDisplay(); /* the new current set */
	e1 = DBCreateElt(VECTOR, plist, CBRUSH, 0, txt, &PICTURE);
	DBAddSet(e1);
	DISScreenAdd(e1, (linemask | setmask));
	CHANGED = TRUE;
}  /* end LGBox */

SHArrow()
/*
 *      This routine draws arrow heads by 'copying' the arrow head template
 * into the picture appropriately transformed.
 */

{
	ELT *e1;
	POINT p1, *p2;
	float xmat[3][2], angle, s, c;

	if (SEQ < 2)        /* not enough points */
	{
		error(nopnt);
		return;
	}
	p1.x = POINTLIST->x - 1;
	p1.y = POINTLIST->y;
	p2 = PTNextPoint(POINTLIST);
	angle = (float) atan2((p2->x - POINTLIST->x),(p2->y - POINTLIST->y))
	       -(float) atan2((p1.x - POINTLIST->x),(p1.y - POINTLIST->y));
	s = (float) sin(angle);      
	c = (float) cos(angle);      

   /* Define transformation matrix to translate element from origin
      and rotate.                                                   */

	xmat[0][0] = c;
	xmat[0][1] = -s;
	xmat[1][0] = s;
	xmat[1][1] = c;
	xmat[2][0] = POINTLIST->x;
	xmat[2][1] = POINTLIST->y;

	DBClearSet();         /* clear old set in preparation to make */
	DISClearSetDisplay(); /* the new current set */
	arhead.brushf = CBRUSH;
	e1 = DBCopy(&arhead, xmat, &PICTURE);
	DBAddSet(e1);
	DISScreenAdd(e1, (linemask | setmask));
	CHANGED = TRUE;
}  /* end SHArrow */
