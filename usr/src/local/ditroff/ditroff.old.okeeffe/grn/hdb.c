/*	hdb.c	1.4	(Berkeley) 83/07/25
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 *      This file contains database routines for the hard copy programs of the
 * gremlin picture editor.
 */

#include "gprint.h"


/* imports from c */

extern char *malloc();
extern char *strcpy();
extern char *sprintf();

/* imports from point.c */

extern POINT *PTInit();
extern POINT *PTMakePoint();

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
    register ELT *temp;

    temp = (ELT *) malloc(sizeof(ELT));
    temp->nextelt = *db;
    temp->type = type;
    temp->ptlist = pointlist;
    temp->brushf = brush;
    temp->size = size;
    temp->textpt = text;
    *db = temp;
    return(temp);
} /* end CreateElt */

ELT *DBRead(file)
register FILE *file;
/*
 *      This routine reads the specified file into a database and 
 * returns a pointer to that database.
 */

{
    register int i;
    register int done;		/* flag for input exhausted */
    register int type;		/* element type */
    register float nx;		/* x holder so x is not set before orienting */
    ELT *elist;			/* pointer to the file's elements */
    POINT *plist;		/* pointer for reading in points */
    char  string[100], *txt;
    float x, y;			/* x and y are read in point coords */
    int len, brush, size;


    elist = DBInit();
    (void) fscanf(file,"%s",string);
    if (strcmp(string, "gremlinfile")) {
        fprintf(stderr, "not gremlin file\n");
        return(elist);
    }
    (void) fscanf(file, "%d%f%f", &size, &x, &y);
			/* ignore orientation and file positioning point */
    done = FALSE;
    while (!done) {
        if (fscanf(file,"%d", &size) == EOF) {
            fprintf(stderr, "error in file format\n");
            return(elist);
        }
        if ((type = size) < 0) {	/* no more data */
            done = TRUE;
            (void) fclose(file);
        } else {
            (void) fscanf(file, "%f%f", &x, &y);
            plist = PTInit();		/* pointlist terminated by -1,-1 */
	    if (TEXT(type)) {		/* only one point for text elements */
		nx = xorn(x,y);
                (void) PTMakePoint(nx, y = yorn(x,y), &plist);
		savebounds(nx, y);
                do
		    (void) fscanf(file, "%f%f", &x, &y);
		while ((x >= 0.0) && (y >= 0.0));
	    } else {
		while ((x >= 0.0) && (y >= 0.0)) {
		    nx = xorn(x,y);
		    (void) PTMakePoint(nx, y = yorn(x,y), &plist);
		    savebounds(nx, y);
		    (void) fscanf(file, "%f%f", &x, &y);
		}
	    }
            (void) fscanf(file, "%d%d", &brush, &size);
            (void) fscanf(file, "%d", &len);   
            txt = malloc((unsigned) len + 1);
            (void) getc(file);        /* throw away space character */
            for (i=0; i<len; ++i) {
                txt[i] = getc(file);
	    }
            txt[len] = '\0';
            (void) DBCreateElt(type, plist, brush, size, txt, &elist);
        }  /* end else */
    }  /* end while not done */;
    return(elist);
} /* end DBRead */

