/*
 * @(#)long2.c	1.2	%G%
 *
 * More routines to implement "long" commands for the SUN Gremlin
 * picture editor.
 *
 * Mark Opperman (opcode@monet.BERKELEY)
 *
 */

#include <suntool/tool_hs.h>
#include <suntool/menu.h>
#include <sys/file.h>
#include "gremlin.h"
#include <ctype.h>

/* imports from graphics files */

extern GRBlankPoints();
extern GRDisplayPoint();
extern GRSetTextPos();

/* imports from path.c */

extern PSetPath();
extern PConvertTilde();
extern char *PGetPath();

/* imports from display.c */

extern DISClearSetDisplay();
extern DISScreenAdd();
extern DISScreenErase();

/* imports from database files */

extern ELT *DBCopy();
extern ELT *DBRead();
extern ELT *DBCreateElt();

extern POINT *PTMakeTextPoints();
extern POINT *PTMakePoint();

extern DBAddSet();
extern DBChangeBrush();
extern DBChangeType();
extern DBChangeTypeStipple();
extern DBClearElt();
extern DBClearSet();
extern DBDelete();
extern DBGravitate();
extern DBXform();

/* imports from undodb.c */

extern UNELT *unlist;
extern UNELT *unback;
extern UNForget();

/* imports from short.c */

extern SHUpdate();
extern int adj[];

/* imports from text.c */

extern TxKillLine();
extern TxMsgOK();
extern TxPutMsg();

/* imports from menu.c  */

extern MNHighLt();
extern MNUnHighLt();
extern HiMode[];

/* imports from C */

extern char *strcpy();
extern char *sprintf();
extern char *malloc();
extern FILE *fopen();

/* imports from sun.c */

extern flush_window_input();
extern prompt_ok();

/* imports from main.c */

extern char namestripe[];
extern char version[];
extern struct tool *tool;
extern struct pixfont *text_pf;

extern tool_fd;
extern menu_fd;

extern ELT *PICTURE;                /* current PICTURE database      */
extern ELT *cset;                   /* current set database          */
extern Orientation;                 /* orientation of workspace      */
extern SEARCH;                      /* flag for path search          */
extern Alignment;                   /* point alignment indicator     */
extern CBRUSH;			    /* current brush                 */
extern CSTIPPLE;		    /* current stipple               */
extern float PX, PY;                /* cursor coordinates            */
extern float Lastx, Lasty;          /* previous cursor coordinates   */
extern SEQ;                         /* point sequence number         */
extern POINT *POINTLIST, *BACKPOINT;/* accumulated point list        */
extern Gridsize;                    /* grid spacing                  */
extern Adjustment;                  /* point adjustment mode         */
extern CHANGED;                     /* PICTURE changed flag          */
extern ELT *MEN[];                  /* pointers for user symbols     */
extern POINT MENPOINT[];            /* pointers used fo user symbols */
extern newfileformat;		    /* TRUE if using SUN file format */

/*  imports from long1.c         */

extern CSP();
extern CP();

char *Editfile;
char *eltnames[] = {
    "BOTLEFT", "BOTRIGHT", "CENTCENT", "VECTOR", "ARC", "CURVE", "POLYGON",
    "", "", "", 
    "TOPLEFT", "TOPCENT", "TOPRIGHT", "CENTLEFT", "CENTRIGHT", "BOTCENT"
};

char nowrite_msg[] = "NO WRITE SINCE LAST CHANGE!       Press left button to \
edit new file, middle or right button to cancel.";
char filexists_msg[] = "FILE EXISTS!  Press left button to overwrite, middle \
or right button to cancel.";
static char quit_msg[] = "NO WRITE SINCE LAST CHANGE!       Press left button \
to confirm quit, middle or right button to cancel.";
static char quit2_msg[] = "Press left button to confirm quit, middle or right \
button to cancel.";

#define BADNUM -1
#define NONUM -2

static char badarg[] = "bad args";


/*
 * This routine creates and displays a POLYGON element from the
 * points previously specified.
 */
static
LGDrawPolygon(bordered)
int bordered;
{
    register POINT *p1, *p2;
    POINT *p0, *plist;
    ELT *e1;
    char *txt;

    if (SEQ < 3) {      /* not enough points */
	error("need at least 3 points");
	return;
    }

    UNForget();

    DISClearSetDisplay();
    DBClearSet();

    plist = PTInit();
    p0 = p1 = POINTLIST;
    (void) PTMakePoint(p1->x, p1->y, &plist);
    p2 = PTNextPoint(p1);

    while (!Nullpoint(p2)) {
	(void) PTMakePoint(p2->x, p2->y, &plist);
	p1 = p2;
	p2 = PTNextPoint(p1);
    }

    txt = malloc(1);
    *txt = '\0';
    e1 = DBCreateElt(POLYGON, plist, bordered ? CBRUSH : 0, CSTIPPLE, 
								txt, &PICTURE);
    DISScreenAdd(e1, pixmask | csetmask);
    DBAddSet(e1);

    CP();
    CHANGED = TRUE;
}  /* end LGDrawPolygon */


LGBPolygon()
{
    LGDrawPolygon(TRUE);
}


LGPolygon()
{
    LGDrawPolygon(FALSE);
}


/*
 * Modify elements in current set to POLYGON type with the indicated
 * brush.  POLYGONs, CURVEs and VECTORs can be modified to either
 * bordered or unbordered POLYGONs.
 */
LGModifyPolygon(brush)
int brush;		/* zero for unbordered */
{
    register ELT *elt;

    if (DBNullelt(cset)) {
	error("no current set");
	return;
    }

    UNForget();
    CSP();

    elt = cset;
    while (!DBNullelt(elt)) {
	if (elt->type == POLYGON) {
	    DISScreenErase(elt, pixmask | csetmask);
	    DBChangeBrush(elt, brush, &PICTURE);
	    DISScreenAdd(elt, pixmask | csetmask);
	}
	else if ((elt->type == VECTOR) || (elt->type == CURVE)) {
	    DISScreenErase(elt, pixmask | csetmask);
	    if (brush != 0)			/* bordered polygon */
		DBChangeTypeStipple(elt, POLYGON, CSTIPPLE, &PICTURE);
	    else				/* unbordered polygon */
		DBChangeTypeBrushStipple(elt, POLYGON, 0, CSTIPPLE, &PICTURE);
	    DISScreenAdd(elt, pixmask | csetmask);
	}

	elt = DBNextofSet(elt);
    }

    CP();
    CHANGED = TRUE;
}  /* end LGModifyPolygon */


/*
 * Modify curves, vectors and polygons in the current set
 * to bordered polygons.
 */
LGMBPolygon()
{
    LGModifyPolygon(CBRUSH);
}


/*
 * Modify curves, vectors and polygons in the current set
 * to unbordered polygons.
 */
LGMPolygon()
{
    LGModifyPolygon(0);
}


/*
 * Modify curves and polygons in the current set to vectors.
 */
LGMVector()
{
    register ELT *elt;

    if (DBNullelt(cset)) {
	error("no current set");
	return;
    }

    UNForget();
    CSP();

    elt = cset;
    while (!DBNullelt(elt)) {
	if (elt->type == POLYGON) {
	    DISScreenErase(elt, pixmask | csetmask);
	    if (elt->brushf != 0)
		DBChangeTypeStipple(elt, VECTOR, 0, &PICTURE);
	    else
		DBChangeTypeBrushStipple(elt, VECTOR, CBRUSH, 0, &PICTURE);
	    DISScreenAdd(elt, pixmask | csetmask);
	}
	else if (elt->type == CURVE) {
	    DISScreenErase(elt, pixmask | csetmask);
	    DBChangeType(elt, VECTOR, &PICTURE);
	    DISScreenAdd(elt, pixmask | csetmask);
	}
	elt = DBNextofSet(elt);
    }

    CP();
    CHANGED = TRUE;
}


/*
 * Modify vectors and polygons in the current set to curves.
 */
LGMCurve()
{
    register ELT *elt;

    if (DBNullelt(cset)) {
	error("no current set");
	return;
    }

    UNForget();
    CSP();

    elt = cset;
    while (!DBNullelt(elt)) {
	if (elt->type == VECTOR) {
	    DISScreenErase(elt, pixmask | csetmask);
	    DBChangeType(elt, CURVE, &PICTURE);
	    DISScreenAdd(elt, pixmask | csetmask);
	}
	else if (elt->type == POLYGON) {
	    DISScreenErase(elt, pixmask | csetmask);
	    if (elt->brushf != 0)		/* bordered polygon */
		DBChangeTypeStipple(elt, CURVE, 0, &PICTURE);
	    else				/* unbordered polygon */
		DBChangeTypeBrushStipple(elt, CURVE, CBRUSH, 0, &PICTURE);
	    DISScreenAdd(elt, pixmask | csetmask);
	}
	elt = DBNextofSet(elt);
    }

    CP();
    CHANGED = TRUE;
}


LGIncludeSet()
/*
 * This routine adds all elements selected by points in POINTLIST
 * to the current set.  It does not remove previously selected elements.
 */
{
    POINT *p1, *p2;
    ELT *e1;
    float n1, n2;

    if (DBNullelt(PICTURE)) 
	return;

    if (SEQ == 0) {	/* no points: entire picture becomes current set */
        e1 = PICTURE;
        while (!DBNullelt(e1)) {
	    if (!DBInCset(e1)) {		/* not now in current set */
		DBAddSet(e1);			/* add it to current set */
		DISScreenAdd(e1, csetmask);	/* and display it */
	    }
            e1 = DBNextElt(e1);
        }
    }
    else {
        p1 = POINTLIST;

	/* for each user point */
        while (!Nullpoint(p1)) {

	    /* find closest element */
            DBGravitate(p1->x, p1->y, &n1, &n2, &p2, &e1, PICTURE, FALSE);

	    /* if something's close and its not already in the current set */
            if (!DBNullelt(e1) && !DBInCset(e1)) {
                DBAddSet(e1);				/* add it */
                DISScreenAdd(e1, csetmask);		/* and display it */
            }
            p1 = PTNextPoint(p1);
        }
    }

    CP();
} /* end LGIncludeSet */


/*
 * This routine implements the menu command.  The contents of 
 * the specified user menu item is copied into the PICTURE transformed
 * to the positioning point.
 */
LGGet(buffer)
int buffer;
{

    ELT *elist, *e1;
    POINT *plist;
    int symbol, index;
    float xmat[3][2];

    if (SEQ < 1) {
        error("no positioning point");
        return;
    }

    UNForget();
    buffer--;     /* users inputs number between 1 and N, actual
                     buffer number is between 0 and N-1 */

    xmat[0][0] = xmat[1][1] = 1;    /* create transformation matrix */
    xmat[0][1] = xmat[1][0] = 0;    /* for copy into PICTURE        */
    plist = POINTLIST;

    while (!Nullpoint(plist)) {
	DISClearSetDisplay();
        DBClearSet();
        xmat[2][0] = plist->x - (MENPOINT[buffer]).x;
        xmat[2][1] = plist->y - (MENPOINT[buffer]).y;
        elist = MEN[buffer];

        while (!DBNullelt(elist)) { /* copy buffer to picture */
            e1 = DBCopy(elist, xmat, &PICTURE);
            DISScreenAdd(e1, pixmask | csetmask);
            DBAddSet(e1);
            elist = DBNextElt(elist);
        }

        plist = PTNextPoint(plist);
    }

    CP();
    CHANGED = TRUE;
}  /* end LGGet */


LGGet1()
{
    LGGet(1);
}


LGGet2()
{
    LGGet(2);
}


LGGet3()
{
    LGGet(3);
}


LGGet4()
{
    LGGet(4);
}


/*
 * This routine reads in the specified filename (command line) to the
 * selected user symbol or current set if no user symbol is selected.  If
 * no filename is specified, the current set is copied to the user symbol;
 */
LGRead()
{
    POINT pos, ppos;
    ELT *elist, *e1;
    char tname[TEXT_BUFMAX];
    float xmat[3][2];
    int orient;

    text_getvalue(&tname[0]);
    if (*tname == '\0') {
        error("read from where?");
        return;
    }

    elist = DBRead(tname, &orient, &pos);	/* read file */
    if (elist == (ELT *) NULL)
	return;

    UNForget();					/* forget changes registered */
						/* by DBRead */
    if (SEQ < 1) {				/* no positioning point */
        ppos.x = pos.x;
        ppos.y = pos.y;
    }
    else {
        ppos.x = POINTLIST->x;
        ppos.y = POINTLIST->y;
    }

    xmat[0][0] = xmat[1][1] = 1;		/* set up matrix to copy to */
    xmat[0][1] = xmat[1][0] = 0;		/* appropriate place in */
    xmat[2][0] = ppos.x - pos.x;		/* picture as current set */
    xmat[2][1] = ppos.y - pos.y;
    DISClearSetDisplay();
    DBClearSet();

    while (!DBNullelt(elist)) {
        e1 = DBCopy(elist, xmat, &PICTURE);
        DISScreenAdd(e1, pixmask | csetmask);
        DBAddSet(e1);
        e1 = DBNextElt(elist);
        DBClearElt(elist);
        elist = e1;
    }

    CHANGED = TRUE;
    TxKillLine();
    CP();
}  /* end LGRead */


/*
 * This routine reads in a new PICTURE for editing
 */
LGEdit()
{
    FILE *fp, *POpen();
    POINT pos;
    ELT *e1;
    char *prealname, *tn, tname[TEXT_BUFMAX]; 
    int fd;

    text_getvalue(&tname[0]);

    if (CHANGED) {
	if (!prompt_ok(menu_fd, nowrite_msg)) {
	    return;
	}
    }

    DISClearSetDisplay();
    DBClearSet();

    while (!DBNullelt(PICTURE)) {	/* clear current PICTURE */
        e1 = DBNextElt(PICTURE);
        DBClearElt(PICTURE);
        PICTURE = e1;
    }

    tn = tname;  

    POINTLIST = PTInit();		/* initialize globals */
    SEQ = 0;
    CHANGED = FALSE;
    (void) strcpy(namestripe, version);

    if (*tname != '\0') {		/* filename present */
        fp = POpen(tname, &prealname, SEARCH);

        if (fp == NULL) {
            PICTURE = DBInit();
	    strcpy(Editfile, tname);
	    strcat(namestripe, tname);
	    error("creating new file");
        }
        else {
	    fclose(fp);
	    strcpy(Editfile, prealname);
	    strcat(namestripe, prealname);
            PICTURE = DBRead(tname, &Orientation, &pos);
	    if ((fd = open(prealname, O_WRONLY | O_APPEND)) < 0)
		strcat(namestripe, " (read only)");
	    else
		close(fd);
        }
    }
    else {				/* create new file */
	PICTURE = DBInit();
	(void) strcat(namestripe, "new file");
	(void) strcpy(Editfile, "");
    }

    tool_display(tool);

    unlist = unback = NULL;
    CP();
    SHUpdate();      /* display new picture */
    TxKillLine();
}  /* end LGEdit */


/* 
 * This routine (re) displays the points in the back-up pointlist
 */
static 
restorepoints()
{

    register POINT *plist, *pl1;
    register i;

    GRBlankPoints(POINTLIST);
    plist = BACKPOINT;

    for (i=0; !Nullpoint(plist); ++i) {
        Lastx = plist->x;
        Lasty = plist->y;
        GRDisplayPoint(plist->x, plist->y, i);
        plist = PTNextPoint(plist);
    }

    pl1 = POINTLIST;
    POINTLIST = BACKPOINT;
    SEQ = i;
    BACKPOINT = pl1;
}  /* end restorepoints */


/* 
 * This routine uses the information in the undo database to reconstruct
 * the PICTURE as it was before the last command.  The undo database is set
 * so that the next undo would nullify this one.
 * An undo of an Add is to delete the new element.
 * Add the old element back to undo a delete.
 * Modified elements are undone by copying the old element into the database
 * in place of the modified element.
 */
LGUndo()
{
    UNELT *fix, *temp;
    ELT *(*e1);

    fix = unlist;	/* initialize unlist so that undo-ing can */
    unlist = NULL;	/* add items to properly undo the undo */

    if (fix == NULL) {
        fix = unback;
        unback = NULL;
    }

    DISClearSetDisplay();
    DBClearSet();

    while (fix != NULL) {
        switch (fix->action) {
	    case ADD: 
		DISScreenErase(fix->newelt, pixmask);
                TxMsgOK();
                restorepoints();
                DBDelete(fix->newelt, fix->dbase);
                temp = fix->nextun;
                free((char *) fix);
                fix = temp;
                break;
	    case DELETE: 
		fix->action = ADD;   /* create undo unelt */
                fix->newelt = fix->oldelt;
                fix->oldelt = NULL;
                fix->newelt->nextelt = PICTURE;
                restorepoints();
                DISScreenAdd(fix->newelt, pixmask | csetmask);
                DBAddSet(fix->newelt);
                PICTURE = fix->newelt;    /* put in database */
                temp = fix->nextun;
                fix->nextun = unlist;     /* link into unlist */
                unlist = fix;
                fix = temp;
                break;
	    case MOD: 
		DISScreenErase(fix->newelt, pixmask);
                TxMsgOK();
                restorepoints();
                DISScreenAdd(fix->oldelt, pixmask | csetmask);
                DBAddSet(fix->oldelt);
                e1 = fix->dbase;

                while (*e1 != fix->newelt) { /* find elt to replace */
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
        }
    }
}  /* end LGUndo */


/* 
 * Write elements from elist to filename.
 * If setonly is true, elements are taken from the "setnext"
 * pointer; otherwise, elements are taken from "nextelt".
 * Ie., the current set is written with setonly = TRUE and
 * the complete picture is written with setonly = FALSE.
 */
static
LGWriteSet(elist, filename, setonly)
ELT *elist;
char *filename;
int setonly;
{
    FILE *fp;
    POINT *plist, pos;
    char string[256];

    fp = fopen(filename, "w");
    if (fp == NULL) {
        (void) sprintf(string, "can't open %s", filename);
        error(string);
        return;
    }

    TxPutMsg("writing file...");
    UNForget();
    CHANGED = FALSE;

    if (SEQ > 0) {			/* specified a positioning point */
        pos.x = POINTLIST->x;
        pos.y = POINTLIST->y;
    }
    else {
        if (!DBNullelt(elist)) {
            pos.x = elist->ptlist->x;
            pos.y = elist->ptlist->y;
        }
        else {
            pos.x = pos.y = 0.0;
        }
    }

    if (newfileformat)
	fprintf(fp, "sungremlinfile\n");		/* write header */
    else
	fprintf(fp, "gremlinfile\n");			/* write header */
    fprintf(fp, "%d %1.2f %1.2f\n", Orientation, pos.x, pos.y);

    while (!DBNullelt(elist)) {			/* write each element */
	if (newfileformat)
	    fprintf(fp, "%s\n", eltnames[elist->type]);
	else
	    fprintf(fp, "%d\n", elist->type);

        plist = elist->ptlist;

        while (!Nullpoint(plist)) {		/* write each point */
            fprintf(fp, "%1.2f %1.2f\n", plist->x, plist->y);
            plist = PTNextPoint(plist);
        }

	if (newfileformat)
	    fprintf(fp, "*\n");			/* end pointlist */
	else
	    fprintf(fp, "-1.00 -1.00\n");	/* end pointlist */

        fprintf(fp, "%d %d\n", elist->brushf, elist->size);
        fprintf(fp,"%d %s\n", strlen(elist->textpt), elist->textpt);
        elist = setonly ? DBNextofSet(elist) : DBNextElt(elist);
    }
    fprintf(fp, "-1\n");

    (void) fclose(fp);
    TxMsgOK();
    TxKillLine();
    CP();
}  /* end LGWriteSet */


/*
 * This routine writes the current set into the specified filename
 */
LGSave()
{
    FILE *fp;
    char tname[TEXT_BUFMAX], filename[TEXT_BUFMAX], *tn, *fn;
    int space, stat;

    space = TEXT_BUFMAX;
    text_getvalue(&tname[0]);
    tn = tname;  
    fn = filename;

    if (*tname == '\0') {
        error("write to where?");
        return;
    }

    stat = PConvertTilde(&tn, &fn, &space);
    *fn = '\0';

    if (stat == FALSE) {
        sprintf(filename, "unknown path %s", tname);
        error(filename);
        return;
    }

    fp = fopen(filename, "r");
    if (fp != NULL) {
	 if (!prompt_ok(menu_fd, filexists_msg)) {
	    fclose(fp);
	    return;
	}
	else
	    fclose(fp);
    }

    LGWriteSet(cset, filename, TRUE);
}  /* end LGSave */;


/*
 * This routine writes the current PICTURE into the specified filename
 * or to the current Editfile
 */
LGWrite()
{
    FILE *fp;
    char tname[TEXT_BUFMAX], filename[TEXT_BUFMAX], *tn, *fn;
    int space, stat;

    space = TEXT_BUFMAX;
    text_getvalue(&tname[0]);
    tn = tname;
    fn = filename;

    if (tname[0] == '\0') {
        if (Editfile[0] == '\0') {
            error("write to where?");
            return;
        }
	strcpy(filename, Editfile);
    }
    else {
	stat = PConvertTilde(&tn, &fn, &space);
	*fn = '\0';
	if (stat == FALSE) {
	    sprintf(filename, "unknown path %s", tname);
	    error(filename);
	    return;
	}
	fp = fopen(filename, "r");
	if (fp != NULL) {
	    if (!prompt_ok(menu_fd, filexists_msg)) {
		fclose(fp);
		return;
	    }
	    else
		fclose(fp);
	}
    }

    LGWriteSet(PICTURE, filename, FALSE);
}  /* end LGWrite */;


/*
 * This routine terminates the editor.  The terminal states for the text
 * terminal and the graphics display are restored and an EXIT is performed.
 */
LGQuit()
{
    if (prompt_ok(tool_fd, CHANGED ? quit_msg : quit2_msg))
	exit(0);
}  /* end LGQuit */


/*
 * Horizontal Adjust -
 * This routine toggles the adjustment mode.
 */
LGHAdjust()
{
    if (Adjustment == HORZ) {
        MNUnHighLt(HiMode[adj[HORZ]]);
        Adjustment = NOADJ;
    }
    else {
	if (Adjustment != NOADJ)
	    MNUnHighLt(HiMode[adj[Adjustment]]);
        MNHighLt(HiMode[adj[HORZ]]);
        Adjustment = HORZ;
    }
}  /* end LGHAdjust */


/*
 * Vertical Adjust -
 * This routine toggles the adjustment mode.
 */
LGVAdjust()
{
    if (Adjustment == VERT) {
        MNUnHighLt(HiMode[adj[VERT]]);
        Adjustment = NOADJ;
    }
    else {
	if (Adjustment != NOADJ)
	    MNUnHighLt(HiMode[adj[Adjustment]]);
        MNHighLt(HiMode[adj[VERT]]);
        Adjustment = VERT;
    }
}  /* end LGVAdjust */


/*
 * This local routine returns 1 if x >= 0
 * otherwise returns -1
 */
static
sign(x)
float x;
{
    return((x >= 0) ? 1 : -1);
}


/*
 * This routine is called by all mirroring routines to effect the
 * transformation specified by xmat.
 */
static
mirror(xmat)
float xmat[3][2];
{
    register ELT *elt;
    POINT pt, pos, *p1, *p2;
    int i, j;

    UNForget();
    elt = cset;
    CSP();

    while (!DBNullelt(elt)) {
        DISScreenErase(elt, pixmask | csetmask);
        TxMsgOK();
        DBXform(elt, xmat, &PICTURE);
        if (TEXT(elt->type)) {
	    GRSetTextPos(elt->textpt, elt->type, elt->brushf, elt->size, 
						    elt->ptlist, &pos);
            elt->ptlist = PTMakeTextPoints(elt->textpt, elt->brushf, elt->size, 
						    elt->ptlist, &pos);
	    DISScreenAdd(elt, pixmask | csetmask);
        }
        else {
            if ((elt->type == ARC) && (elt->size > 0) &&
					(xmat[0][0] * xmat[1][1] < 0)) {
		/* arcs require special handling */
                /* but, circles OK and mirror in both directions OK */
	        /* otherwise, swap starting and ending points of arc */
                p1 = PTNextPoint(elt->ptlist);
                p2 = PTNextPoint(p1);
                pt.x = p1->x;
                pt.y = p1->y;
                p1->x = p2->x;
                p1->y = p2->y;
                p2->x = pt.x;
                p2->y = pt.y;
	    }
            DISScreenAdd(elt, pixmask | csetmask);
        }
        elt = DBNextofSet(elt);
    }
    CP();
}  /* end mirror */


/*
 * This routine mirrors the elements in the current set VERTICALLY
 * The mirroring is accomplished by defining a transformation
 * matrix and calling DBXform.
 */
LGVMirror()
{
    float xmat[3][2];

    if (SEQ < 1) {      /* not enough points */
        error("not enough points");
        return;
    }

    if (DBNullelt(cset)) {
        error("no current set");
        return;
    }

    /* create transformation matrix to translate set to origin, 
       perform the mirroring and translate back */

    xmat[0][0] = -1.0;
    xmat[1][1] = 1.0;
    xmat[1][0] = xmat[0][1] = xmat[2][1] = 0.0;
    xmat[2][0] = 2.0 * POINTLIST->x;

    mirror(xmat);
    CHANGED = TRUE;
}  /* end LGVMirror */


/*
 * This routine mirrors the elements in the current set HORIZONTALLY
 * The mirroring is accomplished by defining a transformation
 * matrix and calling DBXform.
 */
LGHMirror()
{
    float xmat[3][2];

    if (SEQ < 1) {      /* not enough points */
        error("not enough points");
        return;
    }

    if (DBNullelt(cset)) {
        error("no current set");
        return;
    }

    /* create transformation matrix to translate set to origin, 
       perform the mirroring and translate back */

    xmat[0][0] = 1.0;
    xmat[1][1] = -1.0;
    xmat[1][0] = xmat[0][1] = xmat[2][0] = 0.0;
    xmat[2][1] = 2.0 * POINTLIST->y;

    mirror(xmat);
    CHANGED = TRUE;
}  /* end LGHMirror */


/*
 * This routine looks at the command line for parameters to set
 * the current search path.
 */
LGPath()
{
    char buf[TEXT_BUFMAX];
    char buf2[TEXT_BUFMAX];
    register i, i2;

    i = i2 = -1;
    text_getvalue(&buf[0]);
    while (buf[++i]) {
	if (buf[i] != ' ')
	    buf2[++i2] = buf[i];
    }
    buf2[++i2] = '\0';

    if (*buf2 == '\0')
	TxPutMsg(PGetPath());     /* no arguments */
    else {
        SEARCH = TRUE;
        PSetPath(buf2);
    }

    TxKillLine();
}  /* end LGPath */


/*
 * Sometimes it's important to do nothing.
 */
nop()
{
}
