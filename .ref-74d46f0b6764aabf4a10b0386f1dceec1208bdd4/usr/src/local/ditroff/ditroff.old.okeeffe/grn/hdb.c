/*	hdb.c	1.9	(Berkeley) 86/04/14
 *
 * Copyright -C- 1982 Barry S. Roitblat
 *
 *      This file contains database routines for the hard copy programs of the
 * gremlin picture editor.
 */

#include "gprint.h"
#include <ctype.h>

#define MAXSTRING 128

/* imports from main.c */

extern int linenum;		/* current line number in input file */
extern char gremlinfile[];	/* name of file currently reading */
extern int SUNFILE;		/* TRUE if SUN gremlin file */

/* imports from c */

extern char *malloc();
extern char *strcpy();
extern char *sprintf();

/* imports from point.c */

extern POINT *PTInit();
extern POINT *PTMakePoint();

/*
 * This routine returns a pointer to an initialized database element
 * which would be the only element in an empty list.
 */
ELT *
DBInit()
{
    return((ELT *) NULL);
}  /* end DBInit */


/*
 * This routine creates a new element with the specified attributes and
 * links it into database.
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
    return(temp);
} /* end CreateElt */


/*
 * This routine reads the specified file into a database and 
 * returns a pointer to that database.
 */
ELT *
DBRead(file)
register FILE *file;
{
    register int i;
    register int done;		/* flag for input exhausted */
    register float nx;		/* x holder so x is not set before orienting */
    int type;			/* element type */
    ELT *elist;			/* pointer to the file's elements */
    POINT *plist;		/* pointer for reading in points */
    char  string[MAXSTRING], *txt;
    float x, y;			/* x and y are read in point coords */
    int len, brush, size;
    int lastpoint;


    SUNFILE = FALSE;
    elist = DBInit();
    (void) fscanf(file,"%s\n",string);
    if (strcmp(string, "gremlinfile")) {
	if (strcmp(string, "sungremlinfile")) {
	    error("%s is not a gremlin file", gremlinfile);
	    return(elist);
	}
	SUNFILE = TRUE;
    }
    (void) fscanf(file, "%d%f%f\n", &size, &x, &y);
			/* ignore orientation and file positioning point */
    done = FALSE;
    while (!done) {
        if (fscanf(file,"%s\n", string) == EOF) {
            error("%s, error in file format", gremlinfile);
            return(elist);
        }
	type = DBGetType(string);		/* interpret element type */
        if (type < 0) {					/* no more data */
            done = TRUE;
            (void) fclose(file);
        } else {
            (void) fscanf(file, "%f%f\n", &x, &y);	/* always one point */
            plist = PTInit();				/* NULL point list */

	    /* Files created on the SUN have point lists terminated 
	     * by a line containing only an asterik ('*').  Files 
	     * created on the AED have point lists terminated by the
	     * coordinate pair (-1.00 -1.00).
	     */
	    if (TEXT(type)) {	/* read only first point for TEXT elements */
		nx = xorn(x, y);
		y = yorn(x, y);
                (void) PTMakePoint(nx, y, &plist);
		savebounds(nx, y);

		lastpoint = FALSE;
                do {
		    fgets(string, MAXSTRING, file);
		    if (string[0] == '*') {	/* SUN gremlin file */
			lastpoint = TRUE;
		    }
		    else {
			(void) sscanf(string, "%f%f", &x, &y);
			if ((x == -1.00 && y == -1.00) && (!SUNFILE)) {
			    lastpoint = TRUE;
			} else {
			    savebounds(xorn(x, y), yorn(x, y));
			}
		    }
		} while (!lastpoint);
	    } 
	    else {		/* not TEXT element */
		lastpoint = FALSE;
		while (!lastpoint) {
		    nx = xorn(x, y);
		    y = yorn(x, y);
		    (void) PTMakePoint(nx, y, &plist);
		    savebounds(nx, y);

		    fgets(string, MAXSTRING, file);
		    if (string[0] == '*') {	/* SUN gremlin file */
			lastpoint = TRUE;
		    }
		    else {
			(void) sscanf(string, "%f%f", &x, &y);
			if ((x == -1.00 && y == -1.00) && (!SUNFILE))
			    lastpoint = TRUE;
		    }
		}
	    }
            (void) fscanf(file, "%d%d\n", &brush, &size);
            (void) fscanf(file, "%d", &len);	/* text length */
	    (void) getc(file);			/* eat blank */
            txt = malloc((unsigned) len + 1);
            for (i=0; i<len; ++i) {		/* read text */
                txt[i] = getc(file);
	    }
            txt[len] = '\0';
            (void) DBCreateElt(type, plist, brush, size, txt, &elist);
        }  /* end else */
    }  /* end while not done */;
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
		    exit(1);
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
		    exit(1);
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
		    exit(1);
	    }
	default:
	    error("unknown element type");
	    exit(1);
    }
}
