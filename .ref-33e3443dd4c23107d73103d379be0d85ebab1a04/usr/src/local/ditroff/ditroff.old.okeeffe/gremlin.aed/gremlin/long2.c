/* @(#)long2.c	1.4	%G%
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

/* imports from graphics files */

extern GRVector(), GRArc(), GRPutText(), GRClose();
extern GRDisplayPoint(), GRDeletePoint(), GRBlankPoints();
extern charxsize, charysize, GrXMax, GrYMax;

/* import from path.c */

extern int PSetPath(), PConvertTilde();
extern char *PGetPath();

/* imports from display.c */

extern DISScreenAdd(), DISScreenErase();
extern DISDisplaySet(), DISEraseSet(), DISClearSetDisplay();

/* imports from database files */

extern ELT *DBInit(), *DBCreateElt(), *DBRead();
extern DBDelete(), DBGravitate(), DBClearElt();
extern ELT *DBCopy();
extern DBXform(), DBChangeBrush();
extern DBAddSet(), DBClearSet();
extern POINT *PTInit(), *PTMakePoint();
extern PTDeletePoint();

/* imports from undodb.c */

extern UNELT *unlist, *unback;
extern UNForget();

/* imports from short.c */

extern SHUpdate();
extern int adj[];

/* imports from textio.c */

extern TxPutString(), TxPutMsg(), TxMsgOK(), TxClose();
extern TXFIELD TAlign, TAdjust, TBrush, TFont, TGravity, TCSize;
extern TEdit, TJustmode;

/* imports from menu.c  */

extern MNHighLt(), MNUnHighLt();
extern HiMen[], HiFont[], HiBrush[], HiMode[];

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
extern SEARCH;                      /* flag for path search          */
extern Alignment;                   /* point alignment indicator     */
extern float PX, PY;                /* cursor coordinates            */
extern float Lastx, Lasty;          /* previous cursor coordinates   */
extern SEQ;                         /* point sequence number         */
extern POINT *POINTLIST, *BACKPOINT;/* accumulated point list        */
extern Gridsize;                    /* grid spacing                  */
extern Adjustment;                  /* point adjustment mode         */
extern GravityOn;                   /* gravity mode flag             */
extern Consume;                     /* point clear flag              */
extern CHANGED;                     /* PICTURE changed flag          */
extern ELT *MEN[];                  /* pointers for user symbols     */
extern POINT MENPOINT[];            /* pointers used fo user symbols */
extern cmdbuf[];                    /* line buffer for commands      */
extern char *textpos[], *dispmode[];/* text positioning modes        */
extern int textmode[];              /* text positioning              */
extern char *lines[], *fonts[];     /* line and character styles     */
extern int jmodes, lnum[], fnum[];

/*  imports from long1.c         */
extern bang;
extern GetNumParm();
extern LGLookup();
extern SetOrient();

char *Editfile;

#define BADNUM -1
#define NONUM -2
#define Delimiter(c) ((c == '\0') || (c == ' ') || (c == ','))

static char badarg[10] = "bad args";


LGFont(line)
char *line;
/*
 *      This routine looks at the command line for parameters to set
 * the current Font.
 */

{
    int new, index;
    char string[2];

    Consume = FALSE;
    index = 0;
    if (isalpha(*(++line))) 
    {
        new = LGLookup(line, fonts, &index);
        if ( new >= 0) new = fnum[new];
        else new = BADNUM;
    }
    else new = GetNumParm(line, &index);
    if ( (new == BADNUM) || (new > NFONTS) )
    {
        error(badarg);
        return;
    }
    if (new != NONUM)
    {
        MNUnHighLt(HiFont[CFONT-1]);
        MNHighLt(HiFont[new-1], hicolor);
        CFONT = new;
        (void) sprintf(string, "%1d",new);
        TxPutString(&TFont,string);
    }
}  /* end LGFont */


LGJust(line)
char *line;
/*
 *      This routine looks at the command line for parameters to set
 * the current text justification mode.
 */

{
    int new, index;

    Consume = FALSE;
    index = 0;
    if (isalpha(*(++line))) 
    {
      /* make sure mode is in lower case, and look up in table */
        if (isupper(*line))
        {
            *line = tolower(*line);
            *(line+1) = tolower(*(line+1));
        }
        for (new = 0; (strcmp(line, textpos[new]) != 0); ++new)
            if (new > jmodes)
            {
               error("no such mode");
               return;
            }
        if ( new < 0) new = BADNUM;
    }
    else new = GetNumParm(line, &index) - 1;
    if ( (new <= BADNUM) || (new > jmodes) )
    {
        error(badarg);
        return;
    }
    if (new != NONUM)
    {
        new = textmode[new];
        CJUST = new;
        TxPutString(&TJustmode,dispmode[new]);
    }
}  /* end LGJust */


LGSize(line)
char *line;
/*
 *      This routine changes the current character size.
 */

{
    int new, index;
    char string[2];

    index = 1;
    new = GetNumParm(line, &index);
    if ( (new == BADNUM) || (new > NSIZES) )
    {
        error(badarg);
        return;
    }    
    if (new != NONUM)
    {
        CSIZE = new;
        (void) sprintf(string, "%1d",new);
        TxPutString(&TCSize,string);
    }
    Consume = FALSE;
}  /* end LGSize */

LGAlign(line)
char *line;
/*
 *      This routine sets the point alignment indicator
 */

{
    int newalign, index;
    char string[4];

    index = 1;
    newalign = GetNumParm(line, &index);
    if (newalign == NONUM)
        if (Alignment == 1) Alignment = Gridsize;
        else Alignment = 1;
    else
    {
        if ((newalign < 1) || (newalign > GrYMax/2) ) 
        {
            error(badarg);
            return;
        }
        Alignment = newalign;
    }
    (void) sprintf(string, "%3d",Alignment);
    TxPutString(&TAlign,string);
    Consume = FALSE;
}  /* end LGAlign */


LGIncludeSet(line)
char *line;
/*
 *      This routine adds all elements selected by points in POINTLIST
 * to the current set.  It does not remove previously selected elements.
 *
 */

{
    POINT *p1, *p2;
    ELT *e1;
    float n1, n2;

    if (DBNullelt(PICTURE)) return;
    if (SEQ == 0)    /* no points: entire picture becomes */
    {                /* current set                       */
        e1 = PICTURE;
        while ( !DBNullelt(e1) )
        {
            DBAddSet(e1);
            DISDisplaySet(e1);
            e1 = DBNextElt(e1);
        }
    }  /* end if */
    else
    {
        p1 = POINTLIST;
        while ( !Nullpoint(p1) )
        {
            DBGravitate(p1->x, p1->y, &n1, &n2, &p2, &e1, PICTURE);
            if ( !DBNullelt(e1) ) 
            {
                DBAddSet(e1);
                DISDisplaySet(e1);
            }
            p1 = PTNextPoint(p1);
        }  /* end while */;
    }  /* end else */
} /* end LGIncludeSet */



LGMenu(line)
char *line;
/*
 *      This routine implements the menu command.  The contents of 
 * the specified user menu item is copied into the PICTURE transformed
 * to the positioning point.
 */

{

    ELT *elist, *e1;
    POINT *plist;
    int symbol, index;
    float xmat[3][2];

    if (SEQ < 1) 
    {
        error("no positioning point");
        return;
    }
    index = 1;
    symbol = GetNumParm(line, &index);
    if ( (symbol <= 0) || (symbol > NUSER) )
    {
        error(badarg);
        return;
    }
    symbol--;     /* users inputs number between 1 and N, actual
                     symbol number is between 0 and N-1          */
    xmat[0][0] = xmat[1][1] = 1;    /* create transformation matrix */
    xmat[0][1] = xmat[1][0] = 0;    /* for copy into PICTURE        */
    plist = POINTLIST;
    while ( !Nullpoint(plist) )
    {
        DISClearSetDisplay();   /* Clear old current set */
        DBClearSet();
        xmat[2][0] = plist->x - (MENPOINT[symbol]).x;
        xmat[2][1] = plist->y - (MENPOINT[symbol]).y;
        elist = MEN[symbol];
        while ( !DBNullelt(elist) )  /* copy buffer to picture */
        {
            e1 = DBCopy(elist, xmat, &PICTURE);
            DBAddSet(e1);
            DISScreenAdd(e1, (linemask | setmask));
            elist = DBNextElt(elist);
        }  /* end while */
        plist = PTNextPoint(plist);
    }  /* end while */
    CHANGED = TRUE;
}  /* end LGMenu */


LGRead(line)
char *line;
/*
 *      This routine reads in the specified filename (command line) to the
 * selected user symbol or current set if no user symbol is selected.  If
 * no filename is specified, the current set is copied to the user symbol;
 */

{
    POINT pos, ppos;
    ELT *elist, *e1;
    char tname[50], filename[100];
    float xmat[3][2];
    int i, orient;

    if ( *line == '\0' )     /* no arguments */
    {
        error(badarg);
        return;
    }
    ++line;
    (void) sscanf(line, "%s", tname);
    elist = DBRead(tname, &orient, &pos); /* read file */
    UNForget();     /* forget changes registered by DBRead */
    if (SEQ < 1)    /* no positioning point */
    {
        ppos.x = pos.x;
        ppos.y = pos.y;
    }
    else
    {
        ppos.x = POINTLIST->x;
        ppos.y = POINTLIST->y;
    }
    xmat[0][0] = xmat[1][1] = 1;   /* set up matrix to copy to */
    xmat[0][1] = xmat[1][0] = 0;   /* appropriate place in     */
    xmat[2][0] = ppos.x - pos.x;   /* picture as current set   */
    xmat[2][1] = ppos.y - pos.y;
    DBClearSet();
    DISClearSetDisplay();
    while ( !DBNullelt(elist) )
    {
        e1 = DBCopy(elist, xmat, &PICTURE);
        DISScreenAdd(e1, (linemask | setmask));
        DBAddSet(e1);
        e1 = DBNextElt(elist);
        DBClearElt(elist);
        elist = e1;
    }
    CHANGED = TRUE;
}  /* end LGRead */


LGEdit(line)
char *line;
/*
 * This routine reads in a new PICTURE for editing
 */

{
    FILE *fp, *POpen();
    POINT pos;
    ELT *e1;
    char *tn, tname[50]; 
    int i;

    if (!bang)     /* no ! */
    {
        if (CHANGED)
        {
            error("no write");
            return;
        }
    }  /* end if !bang */;
    DBClearSet();
    while ( !DBNullelt(PICTURE) )   /* clear current PICTURE */
    {
        e1 = DBNextElt(PICTURE);
        DBClearElt(PICTURE);
        PICTURE = e1;
    };
    ++line;
    tn = tname;  
    (void) sscanf(line, "%s", tname);

    POINTLIST = PTInit();   /* Initialize globals */
    SEQ = 0;
    CHANGED = FALSE;

    i = strlen(tname);
    if (i > 0)        /* filename present */
    {
        fp = POpen(tname, (char **) NULL, SEARCH);
        TxPutString(&TEdit, tname);
        if (fp == NULL)
        {
            PICTURE = DBInit();
            error(" (creating new file)");
        }
        else     
        {
	    fclose(fp);		/* bug fix 10/10/84 mro */
            PICTURE = DBRead(tname, &Orientation, &pos);
            SetOrient(Orientation);    /* Set appropriate picture area
                                        * orientation                    */
        }
        (void) strcpy (Editfile, tname);
    }
    else
    {
        TxPutString(&TEdit, "");
        (void) strcpy(Editfile, "");
    }
    unlist = unback = nullun;
    CP();
    SHUpdate();      /* display new picture */
}  /* end LGEdit */

static restorepoints()

/* This routine (re) displays the points in the back-up pointlist
 */
{

    int i;
    POINT *plist, *pl1, *pl2;

    GRBlankPoints();
    plist = BACKPOINT;
    for (i=0; !Nullpoint(plist); ++i)
    {
        Lastx = plist->x;
        Lasty = plist->y;
        GRDisplayPoint( (int) plist->x, (int) plist->y, i, pointstyle );
        plist = PTNextPoint(plist);
    }
    pl1 = POINTLIST;
    POINTLIST = BACKPOINT;
    SEQ = i;
    BACKPOINT = pl1;
}  /* end restorepoints */


LGUndo(line)
char *line;
/* 
 *      This routine uses the information in the undo database to reconstruct
 * the PICTURE as it was before the last command.  The undo database is set
 * so that the next undo would nullify this one.
 * An undo of an Add is to delete the new element.
 * Add the old element back to undo a delete.
 * Modified elements are undone by copying the old element into the database
 * in place of the modified element.
 */

{
    UNELT *fix, *temp;
    ELT *(*e1);

    fix = unlist;       /* initialize unlist so that undo-ing can   */
    unlist = nullun;    /* add items to properly undo the undo      */
    if (fix == nullun)
    {
        fix = unback;
        unback = nullun;
    }
    DBClearSet();
    DISClearSetDisplay();
    GRBlankPoints();
    while (fix != nullun)
    {
        switch (fix->action)
        {
            case ADD: DISScreenErase(fix->newelt, linemask);
                      TxMsgOK();
                      restorepoints();
                      DBDelete(fix->newelt, fix->dbase);
                      temp = fix->nextun;
                      free((char *) fix);
                      fix = temp;
                      break;

         case DELETE: fix->action = ADD;   /* create undo unelt */
                      fix->newelt = fix->oldelt;
                      fix->oldelt = NULL;
                      fix->newelt->nextelt = PICTURE;
                      restorepoints();
                      DBAddSet(fix->newelt);
                      DISScreenAdd(fix->newelt,(linemask|setmask));
                      PICTURE = fix->newelt;    /* put in database */
                      temp = fix->nextun;
                      fix->nextun = unlist;     /* link into unlist */
                      unlist = fix;
                      fix = temp;
                      break;

            case MOD: DISScreenErase(fix->newelt, linemask);
                      TxMsgOK();
                      restorepoints();
                      DISScreenAdd(fix->oldelt, (setmask | linemask));
                      DBAddSet(fix->oldelt);
                      e1 = fix->dbase;
                      while ( *e1 != fix->newelt )
                      {                     /* find elt to replace */
                          e1 = &(DBNextElt((*e1)));
                      }
                      fix->oldelt->nextelt = DBNextElt((*e1));
                      *e1 = fix->oldelt;
                      fix->oldelt = fix->newelt;
                      fix->newelt = *e1;     /* create undo unelt */
                      temp = fix->nextun;
                      fix->nextun = unlist;
                      unlist = fix;     /* link into unlist */
                      fix = temp;
                      break;

        }  /* end switch */;
    }  /* end while */
    Consume = FALSE;
}  /* LGUndo */


LGWrite(line)
char *line;
/*
 *      This routine writes the current PICTURE into the specified filename
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
        if ( *Editfile == '\0' )
        {
            error("write to where?");
            return;
        }
        fp = fopen(Editfile, "w");
        wfile = Editfile;
    }
    else
    {
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
    };
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
    elist = PICTURE;
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
        elist = DBNextElt(elist);
    }  /* end while */
    fprintf(fp,"%d\n",-1);   /* end of element list */
    TxMsgOK();
    (void) fclose(fp);
}  /* end LGWrite */;


LGQuit(line)
char *line;
/*
 *      This routine terminates the editor.  The terminal states for the text
 * terminal and the graphics display are restored and an EXIT is performed.
 */

{
    if (!bang)
    {
        if (CHANGED)
        {
            error("no write");
            return;
        }
    }  /* end if */;
    GRClose();
    TxClose();
    exit(0);
}  /* end LGQuit */

LGHAdjust()
/*
 * Horizontal adjust -
 *      This routine toggles the adjustment mode.
 */

{
    if (Adjustment == HORZ) 
    {
        MNUnHighLt(HiMode[adj[HORZ]]);
        Adjustment = NOADJ;
        TxPutString(&TAdjust, "NO ADJUSTMENT");
    }
    else    
    {
        MNUnHighLt(HiMode[adj[Adjustment]]);
        MNHighLt(HiMode[adj[HORZ]], hicolor);
        Adjustment = HORZ;
        TxPutString(&TAdjust, " HORIZONTAL  ");
    }
    Consume = FALSE;
}


LGVAdjust()
/*
 * Vertical adjust -
 *      This routine toggles the adjustment mode.
 */

{
    if (Adjustment == VERT) 
    {
        MNUnHighLt(HiMode[adj[VERT]]);
        Adjustment = NOADJ;
        TxPutString(&TAdjust, "NO ADJUSTMENT");
    }
    else    
    {
        MNUnHighLt(HiMode[adj[Adjustment]]);
        MNHighLt(HiMode[adj[VERT]], hicolor);
        Adjustment = VERT;
        TxPutString(&TAdjust, "  VERTICAL   ");
    }
    Consume = FALSE;
}



static sign(x)
float x;
/*
 *      This local routine returns 1 if x >= 0
 * otherwise returns 0;
 */

{
    if (x >= 0) return(1);
    else  return(0);
}

LGMirror(line)
char *line;
/*
 *      This routine mirrors the elements in the current set as defined
 * by points.  The mirroring is accomplished by defining a transformation
 * matrix and calling DBXform.
 */

{
    ELT *e1;
    POINT pt, pos, *p1, *p2;
    float xmat[3][2], scalex, scaley;
    int i, j;

    if (SEQ < 3)        /* not enough points */
    {
        error("not enough points");
        return;
    }
    if (DBNullelt(cset))
    {
        error("no current set");
        return;
    }
    p1 = PTNextPoint(POINTLIST);
    p2 = PTNextPoint(p1);
    scalex = scaley = 1;
    if (sign(p1->x - POINTLIST->x) != sign(p2->x - POINTLIST->x))
        scalex = -scalex;
    if (sign(p1->y - POINTLIST->y) != sign(p2->y - POINTLIST->y))
        scaley = -scaley;

    /* create transformation matrix to translate set to origin, 
       performing the mirroring and translating back               */

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
            GRPutText(e1->type, p1, e1->brushf, e1->size,e1->textpt, &pos);
            i= strlen(e1->textpt);
            p2 = PTInit();
            (void) PTMakePoint(p1->x, p1->y, &p2);
                       /* add extra positioning points */
            (void) PTMakePoint(pos.x, pos.y, &p2);   
            (void) PTMakePoint(pos.x + i * charxsize / 2, pos.y, &p2);
            (void) PTMakePoint(pos.x + i * charxsize, pos.y, &p2);
            e1->ptlist = p2;
        }  /* end if TEXT */
        else
        {
            if (e1->type == ARC)   /* arcs require special handling */
                if (e1->size > 0)   /* circles are OK */
                    if (scalex * scaley < 0)  /* both directions OK */
                    {          /* swap starting and ending points of arc */
                        p1 = PTNextPoint(e1->ptlist);
                        p2 = PTNextPoint(p1);
                        pt.x = p1->x;
                        pt.y = p1->y;
                        p1->x = p2->x;
                        p1->y = p2->y;
                        p2->x = pt.x;
                        p2->y = pt.y;
                    }
            DISScreenAdd(e1, (linemask | setmask));
        }  /* end else */
        e1 = DBNextofSet(e1);
    }  /* end while */
    CHANGED = TRUE;
}  /* end LGMirror */


LGPath(line)
char *line;
/*
 *      This routine looks at the command line for parameters to set
 * the current search path.
 */

{
    char path[100];

    if ( *line == '\0' )  TxPutMsg(PGetPath());     /* no arguments */
    else
    {
        SEARCH = TRUE;
        (void) sscanf(line, "%s", path);
        PSetPath(path);
    }
    Consume = FALSE;
}  /* end LGFont */
