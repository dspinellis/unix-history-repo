/*	hdb.c	1.1	(Berkeley) 83/07/09
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

ELT *DBRead(file, orient, pos)
register FILE *file;
int *orient;
POINT *pos;
/*
 *      This routine reads the specified file into a database and 
 * returns a pointer to that database.  Orient and pos are also set
 * from the file.
 */

{
    register int i;
    register int done;
    ELT *elist;
    POINT *plist;
    char  string[100], *txt;
    float x, y;
    int len, type, brush, size;

    elist = DBInit();
    (void) fscanf(file,"%s",string);
    if (strcmp(string, "gremlinfile")) {
        fprintf(stderr, "not gremlin file\n");
        return(elist);
    }
    (void) fscanf(file, "%d%f%f", orient, &x, &y);
    pos->x = x;
    pos->y = y;

    done = FALSE;
    while (!done) {
        if (fscanf(file,"%d", &type) == EOF) {
            fprintf(stderr, "error in file format\n");
            return(elist);
        }
        if (type < 0) {		/* no more data */
            done = TRUE;
            (void) fclose(file);
        } else {
            (void) fscanf(file, "%f%f", &x, &y);
            plist = PTInit();
            while ((x != -1) && (y != -1)) { /* pointlist terminated by -1,-1 */
                (void) PTMakePoint(x, y, &plist);
                (void) fscanf(file, "%f%f", &x, &y);
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

