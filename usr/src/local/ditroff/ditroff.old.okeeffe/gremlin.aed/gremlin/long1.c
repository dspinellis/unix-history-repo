/* @(#)long1.c	1.2	%G%
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 *
 *      This file contains routines to implement the long text commands
 * of the gremlin PICTURE editor.
 *
 */

#include "gremlin.h"
#include "grem2.h"
#include <ctype.h>

/* imports from config.c */

extern char GMailCommand[];

/* imports from graphics files */

extern GRVector(), GRArc(), GRPutText();
extern GRDisplayPoint(), GRDeletePoint(), GRBlankPoints();
extern charxsize, charysize;
extern artmode;   /* indication of point display size */

/* imports from display.c */

extern DISScreenAdd(), DISScreenErase();
extern DISDisplaySet(), DISEraseSet(), DISClearSetDisplay();

/* imports from database files */

extern ELT *DBInit(), *DBCreateElt(), *DBRead();
extern DBDelete(), DBSetGravitate(), DBGravitate(), DBClearElt();
extern ELT *DBCopy();
extern DBXform(), DBChangeBrush();
extern DBAddSet(), DBClearSet();
extern POINT *PTInit(), *PTMakePoint();
extern PTDeletePoint();

/* imports from undodb.c */

extern UNELT *unlist, *unback;
extern UNRembMod();

/* imports from short.c */

extern SHUpdate();

/* imports from menu.c  */

extern MNHighLt(), MNUnHighLt();
extern MNInitMenu();
extern HiMen[], HiFont[], HiBrush[], HiMode[];

/* imports from textio.c */

extern TxMsgOK(), TxPutString();
extern TXFIELD TAlign, TAdjust, TBrush, TFont, TGravity, TCSize;
extern TXFIELD TEdit, TJustmode;

/* imports from c */

extern char *malloc();
extern char *strcpy(), *sprintf();

/* imports from main.c */

extern ELT *PICTURE;                /* current PICTURE database      */
extern ELT *cset;                   /* current set database          */
extern CBRUSH, CSIZE, CFONT;        /* current brush, size, font     */
extern CJUST;                       /* current text justification    */
extern Gridon;                      /* grid mode flag                */
extern Orientation;                 /* orientation of workspace      */
extern Alignment;                   /* point alignment indicator     */
extern float PX, PY;                /* cursor coordinates            */
extern float Lastx, Lasty;          /* previous cursor coordinates   */
extern SEQ;                         /* point sequence number         */
extern char *Editfile;              /* current edit file             */
extern POINT *POINTLIST, *BACKPOINT;/* accumulated point list        */
extern Adjustment;                  /* point adjustment mode         */
extern GravityOn;                   /* gravity mode flag             */
extern Consume;                     /* point clear flag              */
extern CHANGED;                     /* PICTURE changed flag          */
extern ELT *MEN[];                  /* pointers for user symbols     */
extern POINT MENPOINT[];            /* pointers used fo user symbols */
extern cmdbuf[];                    /* line buffer for commands      */
extern char *lines[], *fonts[];     /* line and character styles     */
extern int lnum[], fnum[];

/*      The following is available to the outside world */

int bang;


/*      The following are defined to allow creation of the command
 * lookup table.
 */

extern LGAlign(), LGBrush(), LGClearPoints(), LGGripe(), LGLittlePoint(),
       LGDeletePoint(), LGEdit(), LGFont(), LGIncludeSet(), LGSize(), LGSave(),
       LGJust(), LGMenu(), LGPoint(), LGPath(), LGQuit(), LGRead(), LGHAdjust(),
       LGMBrush(), LGMFont(), LGMSize(), LGMText(), LGMPoint(), LGMirror(),
       LGOrient(), LGVAdjust(), LGText(), LGUndo(), LGWrite(), LGOpoint(),
       LGShowPoints();

/* The following two arrays define the long commands and the routines
 * that process them.
 */
static char *lcmds[] = {
    "align",
    "brush",
    "buffer",
    "clearpoints",
    "deletepoint",
    "edit",
    "font",
    "gripe",
    "hadjust",
    "includeset",
    "justificaion",
    "littlepoint",
    "mbrush",
    "mfont",
    "mirror",
    "mpoint",
    "msize",
    "mtext",
    "orient",
    "path",
    "point",
    "quit",
    "read",
    "size",
    "saveset",
    "showpoints",
    "text",
    "undo",
    "vadjust",
    "write",
    "zpoint",
    NULL};
static (*(lrtns[]))() = {
    LGAlign,			/* align */
    LGBrush,			/* set brush */
    LGMenu,			/* select user set buffer */
    LGClearPoints,		/* clear points */
    LGDeletePoint,		/* delete a point */
    LGEdit,			/* edit new file */
    LGFont,			/* set font */
    LGGripe,			/* leave a gripe or bug */
    LGHAdjust,                  /* horizontal adjust */
    LGIncludeSet,		/* add to set */
    LGJust,			/* text justification */
    LGLittlePoint,		/* point size */
    LGMBrush,			/* modify brush */
    LGMFont,			/* modify font */
    LGMirror,			/* mirror current set */
    LGMPoint,			/* move point */
    LGMSize,			/* modify size */
    LGMText,			/* modify text string */
    LGOrient,			/* change picture orientation */
    LGPath,			/* set path or toggle search mode */
    LGOpoint,			/* obtain point from terminal */
    LGQuit,			/* quit */
    LGRead,			/* read user sysmbol */
    LGSize,                     /* character size */
    LGSave,			/* save current set in file */
    LGShowPoints,		/* display reference points in set */
    LGText,			/* input text */
    LGUndo,			/* undo last command */
    LGVAdjust,                  /* vertical adjust */
    LGWrite,			/* write file */
    LGPoint};			/* create point from cursor */
  

int
LGLookup(str, table, next)
char str[];			/* Pointer to a string to be looked up */
char *(table[]);		/* Pointer to an array of string pointers
				 * which are the valid commands.  The strings
				 * must be ordered monotonically (i.e. all
				 * strings whose first characters are identical
				 * must be adjacent in the table).
				 */
int *next;

/*---------------------------------------------------------
 *	LGLookup searches a table of strings to find one that matches a
 *	given string.
 *
 *	Results:
 *	If str is an unambiguous abbreviation for one of the entries
 *	in table, then the index of the matching entry is returned.
 *	If str is an abbreviation for more than one entry in table,
 *	then -1 is returned.  If str doesn't match any entry, then
 *	-2 is returned.
 *
 *	Side Effects:	None.
 *
 * (Modified from software written by John Ousterhout for the caesar
 *  program)
 *---------------------------------------------------------
 */

{
    /* The search is carried out by using two pointers, one which moves
     * forward through table from its start, and one which moves backward
     * through table from its end.  The two pointers mark the range of
     * strings that match the portion of str that we have scanned.  When
     * all of the characters of str have been scanned, then the two
     * pointers better be identical.
     */
    char **bot, **top;
    int match, index;

    bang = FALSE;
    match = 0;
    bot = table;
    for (top = table; *top != NULL; top++);
    if (top == bot) return(-2);
    top--;

    for (index=0; ; index++)
    {
        *next = index;

	/* Check for the end of string */

	if ( (str[index] == '\0')  ||
             (str[index] == ',')   ||
             (str[index] == ' ')       )
	{
	    if (bot == top) return(match);
	    else return(-1);
	}
	if (str[index] == '!') bang = TRUE;
	else
	{

	/* Move bot up until the string it points to matches str in the
	 * index'th position.  Make match refer to the index of bot in table.
	 */
	    while ((*bot)[index] != str[index])
	    {
	        if (bot == top) return(-2);
	        bot++;
	        match++;
	    }
    
	/* Move top down until it matches */
	    while ((*top)[index] != str[index])
	    {
	        if (bot == top) return(-2);
	        top--;
	    }
        }
    }
}


LGCommand(command)
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
    int index, next;

    if (*command == '\0') 
    {
        Consume = FALSE;
        return; 
    }
    index = LGLookup(command, lcmds, &next);
    if (index >= 0)
    {

	(*(lrtns[index]))(command + next);
    }
    else
    {
	if (index == -1) error("command is ambiguous.");
	if (index == -2) error("not a command.");
    }
}

static char badarg[10] = "bad args";
static char noset[15] = "no current set";

#define BADNUM -1
#define NONUM -2
#define Delimiter(c) ((c == '\0') || (c == ' ') || (c == ','))

GetNumParm(line, index)
char *line;
int *index;
/*
 *      This routine trys to interpret the string starting at
 * line+index as a positive integeral numeric parameter.  The function
 * returns the numeric equivalent or a negative number it there is some
 * error in interpreting the string.
 */

{
	char num[20];
	int i, result;

	for (i=0; !Delimiter(*(line + *index)); ++i)
	{
		num[i] = *(line + *index);
		if ( !isdigit(num[i]) ) return (BADNUM);
		++(*index);
	}  /* end for */
	if ( i == 0 ) return(NONUM);
	num[i] = '\0';
	(void) sscanf(num,"%d",&result);
	return(result);
}  /* end GetNumParm */


LGOpoint(line)
char *line;
/*
 *      This routine accepts coordinates from the text terminal
 * and creates and displays a point from them by passing them
 * along to LGPoint.
 *
 * NOTE: coordinates from the terminal must be non-negative
 * integers.
 */

{
	int index, xcoord, ycoord;

	index = 1;
	xcoord = GetNumParm(line, &index);
	if (xcoord < 0)
	{
		error(badarg);
		return;
	}
	++index;
	ycoord = GetNumParm(line, &index);
	if (ycoord < 0)
	{
		error(badarg);
		return;
	}
	PX = xcoord;
	PY = ycoord;
	LGPoint(line);
}  /* end LGOpoint */


LGPoint(line)
char *line;
/*
 *      This routine accepts cursor coordinates and creates and
 * displays points according to the current adjustment and alignment
 * modes.  Note that alignment and gravity are mutually exclusive
 * and adjustment takes precedence over either.
 */

{
	ELT *temp;
	POINT *p1;

	temp = DBInit();
	if (GravityOn) DBGravitate (PX, PY, &PX, &PY, &p1, &temp, PICTURE);
	if (DBNullelt(temp))     /* no gravity in effect */
	{
		/* Round to nearest alignment boundary */

		PX = (float) (((int) (PX / Alignment + 0.5)) * Alignment);
		PY = (float) (((int) (PY / Alignment + 0.5)) * Alignment);
	}


	if (SEQ > 0)      /* this isn't the first point */
	{
		if (Adjustment == HORZ) PY = Lasty;
		if (Adjustment == VERT) PX = Lastx;
		if (Adjustment == MAN)
			if (fabs(PX - Lastx) > fabs(PY - Lasty)) PY = Lasty;
			else   PX = Lastx;
	}  /* end if SEQ */;
	GRDisplayPoint((int) PX, (int) PY, SEQ, pointstyle);
	(void) PTMakePoint(PX, PY, &POINTLIST);
	Lastx = PX;
	Lasty = PY;
	Consume = FALSE;
	++SEQ;
} /* end Point */

CP()
/*
 *      This routine deletes all points from the POINTLIST and 
 * clears them from the display also.
 */

{
	POINT *temp;

	while ( !Nullpoint(BACKPOINT) )
	{
		temp = PTNextPoint(BACKPOINT);
		free ((char *) BACKPOINT);
		BACKPOINT = temp;
	}
       	BACKPOINT = POINTLIST;
        POINTLIST = PTInit();
	SEQ = 0;
	GRBlankPoints();
}  /* end CP */


LGClearPoints(line)
char *line;
/*
 *      This routine is a no-op since all points are cleared by default
 * each time through the interpreter loop unless Consume is false.
 */
{
}

LGDeletePoint(line)
char *line;
/*
 *      This routine removes the last point from the POINTLIST
 * and erases it from the screen.
 */

{
	POINT *pt1, *pt2, *pt3; 
	float savex, savey;
	int i;

	if (SEQ == 0)
	{
		error("no point");
		return;
	}
	pt2 = pt3 = POINTLIST;
	while ( !Nullpoint(pt3) )     /* find last point and pointer to it */
	{
		pt1 = pt2;
		pt2 = pt3;
		pt3 = PTNextPoint(pt3);
	};
	SEQ--;
	savex = pt2->x;
	savey = pt2->y;
	GRErasePoint( (int) pt2->x, (int) pt2->y, SEQ);
	PTDeletePoint(pt2);
	if (SEQ > 0)  /* any points left after deleting */
	{             /* then pt1 points to last of them */
		Lastx = pt1->x;
		Lasty = pt1->y;
		pt3 = POINTLIST;   /* redisplay any nearby points */
		for (i=0; i<SEQ; ++i)   /* which may have been clobbered by */
		{                      /* the erase */
			if (fabs(pt3->x - savex) < (2*halfpoint))
				if (fabs(pt3->y - savey) < (2*halfpoint))
					GRDisplayPoint((int) pt3->x,
					               (int) pt3->y,
					               i, pointstyle);
			pt3 = PTNextPoint(pt3);
		}  /* end for */
	}
	Consume = FALSE;
}  /* end DeletePoint */


LGShowPoints(line)
char *line;
/*
 *      This routine causes the text elements in the current set
 * to be redrawn using the new font.
 */

{
	ELT *elist;
	POINT *p1;
	int pno;

	if (DBNullelt(cset))
	{
		error(noset);
		return;
	}
	elist = cset;
	while ( !DBNullelt(elist) )
	{
		p1 = elist->ptlist;
		pno = 0;
		while (!Nullpoint(p1))
		{
			GRDisplayPoint((int)p1->x, (int)p1->y, pno, pointstyle);
			p1 = PTNextPoint(p1);
			pno++;
		}
		elist = DBNextofSet(elist);
	}  /* end while */
	Consume = FALSE;
} /* end ShowPoints */


LGText(line)
char *line;
/*      This routine implements the text commands.  It first looks
 * for a justification specification (if not specified, default is
 * centering), and then the text string from the input line.  Text
 * justification requires a point upon which the text is positioned.
 */

{
	ELT *e1;
	POINT pos, ppnt, *p1;
	int i, j;
	char *text;

	if (SEQ == 0)
	{
		error("not enough points");
		return;
	}
	if (*line == '\0') return;   /* no text */
	for (; *line == ' '; ++line) /* delete leading blanks */
	i = strlen(line);
	text = malloc((unsigned) i + 1);
	(void) strcpy(text, line);
	DBClearSet();         /* clear old current set so this can form */
	DISClearSetDisplay();  /* the new one  */
	GRsetwmask(textmask | setmask);
	ppnt.x = POINTLIST->x;
	ppnt.y = POINTLIST->y;
	if (SEQ > 1)
	{
		p1 = PTNextPoint(POINTLIST);
		ppnt.x = (POINTLIST->x + p1->x)/2;
		ppnt.y = (POINTLIST->y + p1->y)/2;
	}

	GRPutText(CJUST, &ppnt, CFONT, CSIZE, text, &pos);
	p1 = PTInit();
	(void) PTMakePoint(ppnt.x, ppnt.y, &p1);
                  /* end extra positioning points */
	(void) PTMakePoint(pos.x, pos.y, &p1);   
	(void) PTMakePoint(pos.x + i * charxsize / 2, pos.y, &p1);
	(void) PTMakePoint(pos.x + i * charxsize, pos.y, &p1);
	e1 = DBCreateElt(CJUST, p1, CFONT, CSIZE, text, &PICTURE);
	DBAddSet(e1);
	CHANGED = TRUE;
}  /* end LGText */



LGBrush(line)
char *line;
/*
 *      This routine sets the current brush to that specified in the
 * parameter.
 */

{
	int newbrush, index;
	char string[2];

	index = 0;
	if (isalpha(*(++line))) 
        {
              newbrush = LGLookup(line, lines, &index);
              if (newbrush >= 0) newbrush = lnum[newbrush];
              else  newbrush = BADNUM;
        }
	else newbrush = GetNumParm(line, &index);
	if ( (newbrush == BADNUM) || (newbrush > NBRUSHES) )
	{
		error(badarg);
		return;
	}
	MNUnHighLt(HiBrush[CBRUSH-1]);
	MNHighLt(HiBrush[newbrush-1], hicolor);
	CBRUSH = newbrush;
	(void) sprintf(string,"%1d",newbrush);
	TxPutString(&TBrush,string);
	Consume = FALSE;
}  /* end LGBrush */



LGMBrush(line)
char *line;
/*
 *      This routine causes the elements in the current set
 * to be redrawn using the new brush.
 */

{
	ELT *elist;
	int new, index;

	index = 0;
	if (isalpha(*(++line))) 
        {
              new = LGLookup(line, lines, &index);
              if (new >= 0) new = lnum[new];
              else  new = BADNUM;
        }
	else new = GetNumParm(line, &index);
	if ( (new < 0) || (new > NBRUSHES) )
	{
		error(badarg);
		return;
	}
	if (DBNullelt(cset))
	{
		error(noset);
		return;
	}
	elist = cset;
	while ( !DBNullelt(elist) )
	{
		if (!TEXT(elist->type))
		{
			DISScreenErase(elist, (linemask | setmask));
			DBChangeBrush(elist, new, &PICTURE);
			DISScreenAdd(elist, (linemask | setmask));
		}  /* end if */
		elist = DBNextofSet(elist);
	}  /* end while */
	CHANGED = TRUE;
} /* end MBrush */


LGMFont(line)
char *line;
/*
 *      This routine causes the text elements in the current set
 * to be redrawn using the new font.
 */

{
	ELT *elist;
	int new, index;

	index = 0;
	if (isalpha(*(++line))) 
        {
              new = LGLookup(line, fonts, &index);
              if (new >= 0) new = fnum[new];
              else  new = BADNUM;
        }
	else new = GetNumParm(line, &index);
	if ( (new < 0) || (new > NFONTS) )
	{	
		error(badarg);
		return;
	}
	if (DBNullelt(cset))
	{
		error(noset);
		return;
	}
	elist = cset;
	while ( !DBNullelt(elist) )
	{
		if (TEXT(elist->type))
		{
			DISScreenErase(elist, (linemask | setmask));
			TxMsgOK();
			DBChangeFont(elist, new, elist->size, &PICTURE);
			DISScreenAdd(elist, (linemask | setmask));
		}  /* end if */
		elist = DBNextofSet(elist);
	}  /* end while */
	CHANGED = TRUE;
} /* end MFont */


LGMSize(line)
char *line;
/*
 *      This routine causes the text elements in the current set
 * to be redrawn using the new size.
 *
 */

{
	POINT *p1, *p2, pos;
	ELT *elist;
	int new, index, i, j;

	index = 1;
	new = GetNumParm(line, &index);
	if ( (new < 0) || (new > NSIZES) )
	{	
		error(badarg);
		return;
	}
	if (DBNullelt(cset))
	{
		error(noset);
		return;
	}
	elist = cset;
	while ( !DBNullelt(elist) )
	{
		if (TEXT(elist->type))
		{
			DISScreenErase(elist, (linemask | setmask));
			TxMsgOK();
			UNRembMod(elist,&PICTURE);
			elist->size = new;
			GRsetwmask(textmask | setmask);
			p1 = elist->ptlist;
			GRPutText(elist->type, p1, elist->brushf,
				  elist->size,elist->textpt, &pos);
			i= strlen(elist->textpt);
			p2 = PTInit();
			(void) PTMakePoint(p1->x, p1->y, &p2);
                   	   /* end extra positioning points */
			(void) PTMakePoint(pos.x, pos.y, &p2);   
			(void) PTMakePoint(pos.x + i * charxsize / 2, 
					   pos.y, &p2);
			(void) PTMakePoint(pos.x + i * charxsize, pos.y, &p2);
			elist->ptlist = p2;
		}  /* end if */
		elist = DBNextofSet(elist);
	}  /* end while */
	CHANGED = TRUE;
} /* end MSize */


LGMText(line)
char *line;
/*      This routine allows modification of text by replacing 
 * an existing string with a new one, appropriately repositioned
 */

{
    ELT *e1;
    POINT pos, ppnt, *p1;
    int i, j;
    char *text;

    i = strlen(++line);
    text = malloc((unsigned) i + 1);
    (void) strcpy(text, line);
    GRsetwmask(textmask | setmask);
    e1 = cset;
    while ( !DBNullelt(e1) )
    {
        if (TEXT(e1->type))
        {
            DISScreenErase(e1, (textmask | setmask));
            TxMsgOK();
            UNRembMod(e1, &PICTURE);
            ppnt.x = e1->ptlist->x;
            ppnt.y = e1->ptlist->y;
        
            GRPutText(e1->type, &ppnt, e1->brushf, e1->size, text, &pos);
            p1 = PTInit();
            (void) PTMakePoint(ppnt.x, ppnt.y, &p1);
                       /* end extra positioning points */
            (void) PTMakePoint(pos.x, pos.y, &p1);   
            (void) PTMakePoint(pos.x + i * charxsize / 2, pos.y, &p1);
            (void) PTMakePoint(pos.x + i * charxsize, pos.y, &p1);
            e1->textpt = text;
        }  /* end if text */
        e1 = DBNextofSet(e1);
    }  /* end while */
    CHANGED = TRUE;
}  /* end LGMText */

LGMPoint(line)
char *line;
/*
 *      This routine modifies the element which contains the point
 * closest to the first of two specified points so that that point
 * coincides with the second of the points (if specified).
 *
 *       Note: it implies knowlege of the database representation by modifying
 *           the element directly.
 */

{
	ELT *e1;
	POINT *p1, *p2, *p3, *p4;
	float x1, y1;

	if (SEQ < 1)     /* no points */
	{
		error("no point specified");
		return;
	}
                      /* find point */
	DBSetGravitate(POINTLIST->x, POINTLIST->y, &x1, &y1, &p1, &e1, cset);
	if ( !DBNullelt(e1) )
	{
		DBClearSet();
		DISClearSetDisplay();
		DISScreenErase(e1, (linemask | setmask));
		UNRembMod(e1,&PICTURE);
		if (SEQ > 1)     /* move a point, not delete */
		{
			p2 = PTNextPoint(POINTLIST);
			p1->x = p2->x;
			p1->y = p2->y;
			p2 = PTNextPoint(p2);
			if (!Nullpoint(p2))
			{
				p3 = PTInit();
				while (!Nullpoint(p2))
				{
					p4 = PTMakePoint(p2->x, p2->y, &p3);
					p2 = PTNextPoint(p2);
				}
				p4->nextpt = p1->nextpt;
				p1->nextpt = p3;
			}
		}
		else
		{
			PTDeletePoint(p1);
		}
		DISScreenAdd(e1, (linemask | setmask));
		DBAddSet(e1);
	}  /* end if !DBNullelt */
}  /* end MPOINT */


LGGripe(line)
char *line;
/*
 *      This routine allows users to leave gripe messages or report
 * bugs to the maintainer.  Mail is invoked via the defined constant GRIPE.
 */

{
	TxClose();     /* Restore text terminal to 'normal' state */
	(void) system(GMailCommand);  /* mail complaint */
	SHRedis();  /* reclaim terminal */
}  /* end Gripe */

LGLittlePoint(line)
char *line;
/*
 *      This routine controls the size of the point that is displayed
 * The sizes available are artmode in which a small (3 x 3) point is displayed
 * with no number and regular (non- artmode).
 */

{

	if (artmode) artmode = FALSE;
	else artmode = TRUE;
	Consume = FALSE;
}  /* end Little Point */


SetOrient(orient)
int orient;
/*
 *      This routine sets up the orientation of the drawing area.
 */

{
    int x1, x2, y1, y2;      /* used to set grid parameters */

    if (orient == 0)    /* initialize grid */
    {
        x1 = y1 = 0;
        x2 = 511;
        y2 = 395;
    }
    else
    {
        x1 = 116;
        y1 = 0;
        x2 = 511;
        y2 = 483;
    };
    MNInitMenu(orient);
    GRSetGrid(x1, y1, x2, y2, 16);
}

LGOrient(line)
char *line;
/*
 *      This routine toggles the orientation of the drawing area.
 */

{

    if (Orientation == 1)
    {
        Orientation = 0;
    }
    else
    {
        Orientation = 1;
    };
    SetOrient(Orientation);
    SHUpdate();
}  /* end Orient */


LGSave(line)
char *line;
/*
 *      This routine writes the current set into the specified filename
 * or to the current Editfile
 */

{
    FILE *fp, *fopen();
    char tname[50], filename[100], string[100], *tn, *fn, *wfile;
    ELT *elist;
    POINT *plist, pos;
    int i, space, stat;

    space = 100;
    ++line;
    tn = tname;  fn = filename;
    (void) sscanf(line, "%s", tname);
    i = strlen(tname);
    if (i == 0)       /* no filename */
    {
        error("write to where?");
        return;
    }
    stat = PConvertTilde(&tn, &fn, &space);
    *fn = '\0';
    if (stat == FALSE)
    {
        sprintf(string, "unknown path %s", tname);
        error(string);
        return;
    }
    if ( !bang )  /* user doesn't insist */
    {
        fp = fopen(filename, "r");
        if ( fp != NULL )
        {
             error("file already exists");
             return;
        }
    }
    fp = fopen(filename, "w");
    wfile = filename;
    if (fp == NULL)      /* file error */
    {
        (void) sprintf(string,"can't open %s", wfile);
        error(string);
        return;
    };
    TxPutMsg("writing file...");
    CHANGED = FALSE;
    if (SEQ > 0)       /* specified a positioning point */
    {
        pos.x = POINTLIST->x;
        pos.y = POINTLIST->y;
    }
    else
    {
        if ( !DBNullelt(PICTURE) )
        {
            pos.x = PICTURE->ptlist->x;
            pos.y = PICTURE->ptlist->y;
        }
        else
        {
            pos.x = pos.y = 0;
        };
    }
    fprintf(fp,"gremlinfile\n");    /* write header */
    fprintf(fp, "%d %1.2f %1.2f\n", Orientation, pos.x, pos.y);
    elist = cset;
    while ( !DBNullelt(elist) )    /* write each element */
    {
        fprintf(fp, "%d\n", elist->type);
        plist = elist->ptlist;
        while ( !Nullpoint(plist) )  /* write each point */
        {
            fprintf(fp, "%1.2f %1.2f\n",plist->x, plist->y);
            plist = PTNextPoint(plist);
        }  /* end while plist */
        fprintf(fp, "%1.2f %1.2f\n", -1.0, -1.0);  /* end pointlist */
        fprintf(fp, "%d %d\n",elist->brushf, elist->size);
        fprintf(fp,"%d %s\n ", strlen(elist->textpt), elist->textpt);
        elist = DBNextofSet(elist);
    }  /* end while */
    fprintf(fp,"%d\n",-1);   /* end of element list */
    TxMsgOK();
    (void) fclose(fp);
}  /* end LGSave */;
