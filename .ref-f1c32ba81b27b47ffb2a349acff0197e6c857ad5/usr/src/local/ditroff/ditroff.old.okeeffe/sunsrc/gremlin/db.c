/*
 * @(#)db.c	1.2	%G%
 *
 * This file contains routines for database manipulation for the
 * SUN Gremlin picture editor.
 *
 * Mark Opperman (opcode@monet.BERKELEY)
 *
 */

#include "gremlin.h"
#include <ctype.h>

/* imports from undodb */

extern UNRembAdd();
extern UNRembDelete();
extern UNRembMod();

/* imports from C */

extern char *malloc();

/* imports from point.c */

extern PTModifyTextPoints();
extern POINT *PTMakePoint();

/* imports from text.c  */

extern TxPutMsg();
extern TxMsgOK();

/* imports from main */

extern SEARCH;			/* Search the path for filename */
extern TOOLINSTALLED;

/* cset is a pointer to the current set available to the outside world. */

ELT *cset;

/*
 * This routine creates a new element with the specified attributes and
 * links it into database db.
 */
ELT *
DBCreateElt(type, pointlist, brush, size, text, db)
int type, brush, size;
POINT *pointlist;
char *text;
ELT *(*db);
{
    register ELT *temp;

    temp = (ELT *) malloc(sizeof(ELT));
    temp->nextelt = *db;
    temp->type = type;
    temp->ptlist = pointlist;
    temp->brushf = brush;
    temp->size = size;
    temp->textpt = text;
    *db = temp;
    UNRembAdd(temp, db);
    return(temp);
} /* end DBCreateElt */


/*
 * This routine deletes the specified element by searching the database
 * for its predecessor and deleting the pointer to the element.
 * Flag indicates whether or not the element was in the current set
 * and is passed along for use by the undo routines.
 */
DBDelete(elt, db)
register ELT *elt, *(*db);
{
    register ELT *(*temp);

    temp = db;

    while (*temp != elt) {
        if (DBNullelt(*temp)) {
            error("no such element");
            return;
        }
        temp = &(DBNextElt((*temp)));
    }

    UNRembDelete(*temp, db);
    *temp = DBNextElt(elt);
}  /* end DBDelete */


#define highval 100000			/* arbitrary value greater than any 
					 * expected distance */


/*
 * This routine searches the database for the point closest to
 * (Euclidean distance) point1.  This point is returned as point2
 * and the element which contained the point is also returned.
 * The point must be closer than some predefined maximum distance
 * in order to be gravitated.
 * If setonly == TRUE the element's "setnext" pointer is used to
 * find the elements in the list, otherwise "nextelt" is used.
 */
DBGravitate(x1, y1, x2, y2, point, elt, db, setonly)
float x1, y1, *x2, *y2;
POINT *(*point);
ELT *(*elt), *db;
int setonly;
{
    register POINT *holdpt;
    register ELT *temp;
    register t, t1, t2;
    register distance = highval;

    temp = db;
    *elt = DBInit();
    *x2 = x1;
    *y2 = y1;
    while (!DBNullelt(temp)) {
        holdpt = temp->ptlist;
        while (!Nullpoint(holdpt)) {
	    /* Calculate the distance between the point in the data
	       base and the specified point.  Use Euclidean distance
	       except that, since we only need relative distance and
	       not an absolute number, it is not necessary to take the
	       square root.  The equation for the distance was broken up
	       as below in order to allow integer arithmetic wherever
	       possible to increase efficiency when it was discovered
	       that this routine was too slow. */
            t1 = holdpt->x - x1;
            t1 *= t1;           
            t2 = holdpt->y - y1; 
            t2 *= t2;
            t = t1 + t2;

            if ((t < distance) && (t < MAXGDIST)) {
                distance = t;
                *x2 = holdpt->x;
                *y2 = holdpt->y;
    		*point = holdpt;
                *elt = temp;
            }

            holdpt = holdpt->nextpt;
        }
        temp = setonly ? DBNextofSet(temp) : DBNextElt(temp);
    }
}  /* end DBGravitate */


/*
 * This routine returns all storage associated with the element to
 * free storage.
 */
DBClearElt(elt)
register ELT *elt;
{
    register POINT *pt, *pt2;

    pt = elt->ptlist;

    while (!Nullpoint(pt)) {
        pt2 = PTNextPoint(pt);
        free ((char *) pt);
        pt = pt2;
    }

    free(elt->textpt);
    free((char *) elt);
}  /* end DBClearElt */


/*
 * This routine reads the specified file into a database and 
 * returns a pointer to that database.  Orient and pos are also set
 * from the file.
 *
 * The format of a file written by gremlin is:
 * the string: "gremlinfile" followed by a carriage return.
 * the orientation (integer) and the x and y coordinates of a positioning
 * point (float) followed by another carriage return.
 * The output of 0 or more elements (see below).
 * a -1 (integer) indicating end of data.
 *
 * The format of each element is:
 * The element type (integer) followed by a carriage return.
 * a list of 0 or more pairs of point coordinates (float) each on separate
 * lines and terminated by the coordinates -1.0 -1.0.
 * the brush (font) and size (integer) the element was defined with then <cr>
 * the length (integer) of the string followed by the string terminated with
 * a carriage return.
 *
 * All numbers are printed using standard C output conversion (ascii).
 *
 * +++ NEW FORMAT FOR SUN +++
 *
 * "sungremlinfile" is keyword in place of "gremlinfile"
 *
 * Point lists are terminated by a line containing a single asterik ('*')
 * to allow the legal point (-1.00 -1.00) in the point list.  All negative
 * coordinates are now legal.  Element types are indicated by ascii text,
 * eg, POLYGON, VECTOR, ARC, BOTLEFT, TOPCENT, etc.
 */
ELT *
DBRead(filename, orient, pos)
char *filename;
int *orient;
POINT *pos;
{
    FILE *fp, *POpen();
    ELT *elt, *elist;
    POINT *plist;
    char string[128], *txt, *prealname;
    float x, y;
    int len, type, i, brush, size, done, lastpoint, sunfile;

    sunfile = FALSE;
    elist = DBInit();
    fp = POpen(filename, &prealname, SEARCH);

    if (fp == NULL) {
        (void) sprintf(string, "can't open %s",filename);
        error(string);
        return(elist);
    }

    if (TOOLINSTALLED)		/* no message if reading startup edit file */
	TxPutMsg("reading file...");
    (void) fscanf(fp, "%s\n", string);

    if (strcmp(string, "gremlinfile")) {
	if (strcmp(string, "sungremlinfile")) {
	    error("not gremlin file");
	    return(elist);
	}
	sunfile = TRUE;
    }

    (void) fscanf(fp, "%d%f%f\n", orient, &x, &y);
    pos->x = x;
    pos->y = y;

    done = FALSE;
    while (!done) {
        if (fscanf(fp,"%s\n", string) == EOF) {		/* element type */
            error("error in file format");
	    fclose(fp);
            return(elist);
        }

        if ((type = DBGetType(string)) < 0) {		/* no more data */
            done = TRUE;
        }
        else {
            plist = PTInit();
            (void) fscanf(fp, "%f%f\n", &x, &y);	/* read first point */

	    /* Files created on the SUN have point lists terminated
	     * by a line containing only an asterik ('*').  Files
	     * created on the AED have point lists terminated by the
	     * coordinate pair (-1.00 -1.00).
	     */
	    lastpoint = FALSE;
	    do {
		(void) PTMakePoint(x, y, &plist);
		fgets(string, 127, fp);
		if (string[0] == '*') {     /* SUN gremlin file */
		    lastpoint = TRUE;
		}
		else {
		    (void) sscanf(string, "%f%f", &x, &y);
		    if ((x == -1.00 && y == -1.00) && (!sunfile))
			lastpoint = TRUE;
		}
	    } while (!lastpoint);
#ifdef oldway
            while ((x != -1) && (y != -1)) { /* plist terminated by -1, -1 */
                (void) PTMakePoint(x, y, &plist);
                (void) fscanf(fp, "%f%f\n", &x, &y);
            }
#endif

	    (void) fscanf(fp, "%d%d\n", &brush, &size);
	    (void) fscanf(fp, "%d", &len);   
	    (void) getc(fp);	/* eat blank */
            txt = malloc((unsigned) len + 1);
            for (i=0; i<len; ++i)
                txt[i] = getc(fp);
            txt[len] = '\0';
            elt = DBCreateElt(type, plist, brush, size, txt, &elist);
	    if (TEXT(elt->type))	/* recompute text reference points */
		PTModifyTextPoints(elt);
        }
    }

    TxMsgOK();
    fclose(fp);
    return(elist);
} /* end DBRead */


/*
 * Interpret element type in string s.
 * Old file format consisted of integer element types.
 * New file format has literal names for element types.
 */
DBGetType(s)
register char *s;
{
    if (isdigit(s[0]) || (s[0] == '-'))		/* old element format or EOF */
	return(atoi(s));

    switch (s[0]) {
	case 'P':
	    return(POLYGON);
	case 'V':
	    return(VECTOR);
	case 'A':
	    return(ARC);
	case 'C':
	    if (s[1] == 'U')
		return(CURVE);
	    switch (s[4]) {
		case 'L':
		    return(CENTLEFT);
		case 'C':
		    return(CENTCENT);
		case 'R':
		    return(CENTRIGHT);
		default:
		    error("unknown element type");
		    return(-1);
	    }
	case 'B':
	    switch (s[3]) {
		case 'L':
		    return(BOTLEFT);
		case 'C':
		    return(BOTCENT);
		case 'R':
		    return(BOTRIGHT);
		default:
		    error("unknown element type");
		    return(-1);
	    }
	case 'T':
	    switch (s[3]) {
		case 'L':
		    return(TOPLEFT);
		case 'C':
		    return(TOPCENT);
		case 'R':
		    return(TOPRIGHT);
		default:
		    error("unknown element type");
		    return(-1);
	    }
	default:
	    error("unknown element type");
	    return(-1);
    }
}  /* end DBGetType */


/*
 * This routine returns true if all points in elt are bounded by
 * the rectangle who diagonal is formed by (x1, y1) and (x2, y2).
 */
DBBounded(elt, x1, y1, x2, y2)
register ELT *elt;
register float x1, y1, x2, y2;
{
    register POINT *p1;
    register float lox, loy, hix, hiy;	/* OK to compare register floats */

    lox = (x1 < x2) ? x1 : x2;
    loy = (y1 < y2) ? y1 : y2;
    hix = (x1 > x2) ? x1 : x2;
    hiy = (y1 > y2) ? y1 : y2;
    p1 = elt->ptlist;

    while (!Nullpoint(p1)) {
	if ((p1->x < lox) || (p1->x > hix) || (p1->y < loy) || (p1->y > hiy))
	    return(FALSE);
	p1 = PTNextPoint(p1);
    }

    return(TRUE);
}  /* end DBBounded */


/*
 * This routine creates a copy of the the element transformed by
 * the transformation matrix and adds the new copy to the database.
 */
ELT *
DBCopy(elt, transform, db)
register ELT *elt;
ELT *(*db);
float transform[3][2];
{
    register POINT *pt;
    POINT *newlist;
    char *newtext;

    newlist = PTInit();
    pt = elt->ptlist;

    while (!Nullpoint(pt)) { /* matrix multiply */
	(void) PTMakePoint((((pt->x) * transform[0][0]) + 
			    ((pt->y) * transform[1][0]) + 
				       transform[2][0]),
			   (((pt->x) * transform[0][1]) + 
			    ((pt->y) * transform[1][1]) + 
				       transform[2][1]), &newlist);
	pt = pt->nextpt;
    }

    newtext = malloc((unsigned) strlen(elt->textpt) + 1);
    (void) strcpy(newtext, elt->textpt);
    return( DBCreateElt(elt->type, newlist, elt->brushf,
			elt->size, newtext, db) );
}  /* end DBCopy */


/*
 * This routine transforms the element by multiplying the
 * coordinates of each of the points in the element by the 
 * transformation matrix.
 */
DBXform(elt, transform, db)
register ELT *elt;
float transform[3][2];
ELT *(*db);
{
    register POINT *pt;
    float px, py;

    UNRembMod(elt, db);
    pt = elt->ptlist;

    while (!Nullpoint(pt)) {
	px =  ((pt->x) * transform[0][0]) + 
	      ((pt->y) * transform[1][0]) + transform[2][0];
	py =  ((pt->x) * transform[0][1]) + 
	      ((pt->y) * transform[1][1]) + transform[2][1];
	pt->x = px;
	pt->y = py;
	pt = pt->nextpt;
    }
}  /* end DBXform */


/*
 * This routine changes the brush attribute of the element.
 */
DBChangeBrush(elt, brush, db)
ELT *elt, *(*db);
int brush;
{
    UNRembMod(elt, db);
    elt->brushf = brush;
}  /* end DBChangeBrush */


/*
 * This routine changes the justify attribute of the element.
 */
DBChangeJustify(elt, justmode, db)
ELT *elt, *(*db);
int justmode;
{
    register length;
    register POINT *pos, *point;

    UNRembMod(elt, db);
    elt->type = justmode;
    PTModifyTextPoints(elt);
}  /* end DBChangeJustify */


/*
 * This routine changes the font attribute of the given element.
 */
DBChangeFont(elt, font, db)
ELT *elt, *(*db);
int font;
{
    UNRembMod(elt, db);
    elt->brushf = font;
    PTModifyTextPoints(elt);
}  /* end DBChangeFont */


/*
 * This routine changes the size attribute of the given element.
 */
DBChangeSize(elt, size, db)
ELT *elt, *(*db);
int size;
{
    UNRembMod(elt, db);
    elt->size = size;
    PTModifyTextPoints(elt);
}  /* end DBChangeSize */


/*
 * This routine changes the stipple attribute of the given element.
 */
DBChangeStipple(elt, stipple, db)
ELT *elt, *(*db);
int stipple;
{
    UNRembMod(elt, db);
    elt->size = stipple;
}  /* end DBChangeStipple */


/*
 * This routine changes the text attribute of the given element.
 */
DBChangeText(elt, text, db)
ELT *elt, *(*db);
char *text;
{
    char *new;

    UNRembMod(elt, db);
    free(elt->textpt);
    new = malloc((unsigned) strlen(text) + 1);
    (void) strcpy(new, text);
    elt->textpt = new;
    PTModifyTextPoints(elt);
}  /* end DBChangeText */


/*
 * This routine changes the type attribute of the given element.
 */
DBChangeType(elt, newtype, db)
ELT *elt, *(*db);
int newtype;
{
    UNRembMod(elt, db);
    elt->type = newtype;
}  /* end DBChangeType */


/*
 * This routine changes the type and stipple attributes of the given element.
 */
DBChangeTypeStipple(elt, newtype, newstipple, db)
ELT *elt, *(*db);
int newtype, newstipple;
{
    UNRembMod(elt, db);
    elt->type = newtype;
    elt->size = newstipple;
}  /* end DBChangeType */


/*
 * This routine changes the type, brush and stipple attributes
 * of the given element.
 */
DBChangeTypeBrushStipple(elt, newtype, newbrush, newstipple, db)
ELT *elt, *(*db);
int newtype, newbrush, newstipple;
{
    UNRembMod(elt, db);
    elt->type = newtype;
    elt->brushf = newbrush;
    elt->size = newstipple;
}  /* end DBChangeType */


/*
 * This routine adds the element to the current set database.
 */
DBAddSet(elt)
register ELT *elt;
{
    register ELT *elist;

    elist = cset;

    while (!DBNullelt(elist)) { /* makes sure element not already in list */
	if (elist == elt) 
	    return;
	elist = DBNextofSet(elist);
    }

    elt->setnext = cset;
    cset = elt;
}  /* end DBAddSet */


/*
 *  Return TRUE if element in current set, else FALSE.
 */
DBInCset(elt)
register ELT *elt;
{
    register ELT *elist;

    elist = cset;

    while (!DBNullelt(elist)) { /* makes sure element not already in list */
	if (elist == elt) 
	    return(TRUE);
	elist = DBNextofSet(elist);
    }
    return(FALSE);
}  /* end DBInCset */


/*
 * This routine clears the current set by setting the pointer
 * to a null element.
 */
DBClearSet()
{
    while (!DBNullelt(cset))
	cset = DBNextofSet(cset);
}  /* end DBClearSet */
