/* @(#)db1.c	1.2	%G%
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 *      This file contains routines for database manipulation for the
 * gremlin picture editor.
 */

#include "gremlin.h"
#include "grem2.h"

/* imports from undodb */

extern UNRembAdd(), UNRembDelete();

/* imports from c */

extern char *malloc();
extern char *strcpy();
extern char *sprintf();

/* imports from point.c */

extern POINT *PTInit();
extern POINT *PTMakePoint();

/* imports from textio.c  */

extern TxPutMsg(), TxMsgOK();

/* imports from main */

extern int SEARCH;        /* Search the path for filename ?? */

ELT *DBInit()
/*
 *      This routine returns a pointer to an initialized database element
 * which would be the only element in an empty list.
 */

{
    return(NULL);
}  /* end DBInit */

ELT *DBCreateElt(type, pointlist, brush, size, text, db)
int type, brush, size;
POINT *pointlist;
char *text;
ELT *(*db) ;
/*
 *      This routine creates a new element with the specified attributes and
 * links it into database.
 */

{
    ELT *temp;

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
} /* end CreateElt */


DBDelete(element, db)
ELT *element, *(*db);
/*
 *      This routine deletes the specified element by searching the database
 * for its predecessor and deleting the pointer to the element.
 * Flag indicates whether or not the element was in the current set
 * and is passed along for use by the undo routines.
 */

{
    ELT *(*temp);

    temp = db;
    while (*temp != element)
    {
        if (DBNullelt(*temp))
        {
            error("no such element");
            return;
        };
        temp = &(DBNextElt((*temp)));
    };
    UNRembDelete(*temp, db);
    *temp = DBNextElt(element);
}  /* end DBDelete */


#define highval 10000               /* arbitrary value greater than any 
                                     * expected distance                */

DBGravitate(x1, y1, x2, y2, point, element, db)
float x1, y1, *x2, *y2;
POINT *(*point);
ELT *(*element), *db;
/*
 *      This routine searches the database for the point closest to
 * (Euclidean distance) point1.  This point is returned as point2
 * and the element which contained the point is also returned.
 * The point must be closer than some predefined maximum distance
 * in order to be gravitated.
 */

{
    POINT *holdpt;
    ELT *temp;
    long int t, t1, t2, distance = highval;

    temp = db;
    *element = DBInit();
    *x2 = x1;
    *y2 = y1;
    while ( !DBNullelt(temp) )
    {
        holdpt = temp->ptlist;
        while ( !Nullpoint(holdpt) )
        {

                /*  Calculate the distance between the point in the data
                 * base and the specified point.  Use Euclidean distance
                 * except that, since we only need relative distance and
                 * not an absolute number, it is not necessary to take the
                 * square root.  The equation for the distance was broken up
                 * as below in order to allow integer arithmetic wherever
                 * possible to increase efficiency when it was discovered
                 * that this routine was too slow.
                 */
            t1 = holdpt->x - x1;
            t1 *= t1;           
            t2 = holdpt->y - y1; 
            t2 *= t2;
            t = t1 + t2;
            if ((t < distance) && (t < MAXGDIST))
            {
                distance = t;
                *x2 = holdpt->x;
                *y2 = holdpt->y;
    		*point = holdpt;
                *element = temp;
            }  /* end if */;
            holdpt = holdpt->nextpt;
        }  /* end while holdpt */;
        temp = temp->nextelt;
    }  /* end while temp */;
}  /* end Gravitate */


DBSetGravitate(x1, y1, x2, y2, point, element, db)
float x1, y1, *x2, *y2;
POINT *(*point);
ELT *(*element), *db;
/*
 *      This routine searches the database for the point closest to
 * (Euclidean distance) point1.  This point is returned as point2
 * and the element which contained the point is also returned.
 * The point must be closer than some predefined maximum distance
 * in order to be gravitated.
 */

{
    POINT *holdpt;
    ELT *temp;
    long int t, t1, t2, distance = highval;

    temp = db;
    *element = DBInit();
    *x2 = x1;
    *y2 = y1;
    while ( !DBNullelt(temp) )
    {
        holdpt = temp->ptlist;
        while ( !Nullpoint(holdpt) )
        {

                /*  Calculate the distance between the point in the data
                 * base and the specified point.  Use Euclidean distance
                 * except that, since we only need relative distance and
                 * not an absolute number, it is not necessary to take the
                 * square root.  The equation for the distance was broken up
                 * as below in order to allow integer arithmetic wherever
                 * possible to increase efficiency when it was discovered
                 * that this routine was too slow.
                 */
            t1 = holdpt->x - x1;
            t1 *= t1;           
            t2 = holdpt->y - y1; 
            t2 *= t2;
            t = t1 + t2;
            if ((t < distance) && (t < MAXGDIST))
            {
                distance = t;
                *x2 = holdpt->x;
                *y2 = holdpt->y;
    		*point = holdpt;
                *element = temp;
            }  /* end if */;
            holdpt = holdpt->nextpt;
        }  /* end while holdpt */;
        temp = temp->setnext;
    }  /* end while temp */;
}  /* end Gravitate */



DBClearElt(elt)
ELT *elt;
/*
 *      This routine returns all storage associated with the element to
 * free storage
 */

{
    POINT *pt, *pt2;

    pt = elt->ptlist;
    while ( !Nullpoint(pt) )
    {
        pt2 = PTNextPoint(pt);
        free ((char *) pt);
        pt = pt2;
    }  /* end while */;
    free(elt->textpt);
    free((char *) elt);
}  /* end DBClearElt */


ELT *DBRead(filename, orient, pos)
char *filename;
int *orient;
POINT *pos;
/*
 *      This routine reads the specified file into a database and 
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
 * All numbers are printed using standard c output conversion (ascii).
 */

{
    FILE *fp, *POpen();
    ELT *elist;
    POINT *plist;
    char string[100], *txt;
    float x, y;
    int len, type, i, brush, size, done;

    elist = DBInit();
    fp = POpen(filename,(char **) NULL,SEARCH);
    if (fp == NULL)
    {
        (void) sprintf(string, "can't open %s",filename);
        error(string);
        return(elist);
    }
    TxPutMsg("reading file...");
    (void) fscanf(fp,"%s",string);
    if ( strcmp(string, "gremlinfile") )
    {
        error("not gremlin file");
        return(elist);
    }
    (void) fscanf(fp, "%d%f%f", orient, &x, &y);
    pos->x = x;
    pos->y = y;

    done = FALSE;
    while (!done)
    {
        if ( fscanf(fp,"%d", &type) == EOF )
        {
            error("error in file format");
            return(elist);
        }
        if (type < 0)         /* no more data */
        {
            done = TRUE;
            (void) fclose(fp);
        }
        else
        {
            (void) fscanf(fp, "%f%f", &x, &y);
            plist = PTInit();
            while ((x != -1) && (y != -1)) /* pointlist terminated by -1, -1 */
            {
                (void) PTMakePoint(x, y, &plist);
                (void) fscanf(fp, "%f%f", &x, &y);
            }
            (void) fscanf(fp, "%d%d", &brush, &size);
            (void) fscanf(fp, "%d", &len);   
            txt = malloc((unsigned) len + 1);
            (void) getc(fp);        /* throw away space character */
            for (i=0; i<len; ++i)
                txt[i] = getc(fp);
            txt[len] = '\0';
            (void) DBCreateElt(type, plist, brush, size, txt, &elist);
        }  /* end else */
    }  /* end while not done */;
    TxMsgOK();
    return(elist);
} /* end DBRead */


DBBounded(elt, x1, y1, x2, y2)
ELT *elt;
float x1, y1, x2, y2;
/*
 *      This routine returns true if all points in elt are bounded by
 * the rectangle who diagonal is formed by (x1, y1) and (x2, y2).
 */

{
	POINT *p1;
	float lox, loy, hix, hiy;

	lox = (x1 < x2) ? x1 : x2;
	loy = (y1 < y2) ? y1 : y2;
	hix = (x1 > x2) ? x1 : x2;
	hiy = (y1 > y2) ? y1 : y2;
	p1 = elt->ptlist;
	while ( !Nullpoint(p1) )
	{
		if (p1->x < lox) return(FALSE);
		if (p1->x > hix) return(FALSE);
		if (p1->y < loy) return(FALSE);
		if (p1->y > hiy) return(FALSE);
		p1 = PTNextPoint(p1);
	}  /* end while */;
	return(TRUE);
}  /* end DBBounded */
