static char *sccsid = "%W%";

#include "parms.h"
#include "structs.h"
/*
 *	preptxt(fid, nfname, author, unique, date, atext)
 *	put the standard headers and text into the specified file
 *
 *	Original coding:	Ray Essick	December 1981
 */

preptxt (io, zfile, nfname, author, unique, date, where)
struct io_f *io;
FILE * zfile;
char   *nfname;
struct auth_f  *author;
struct id_f *unique;
struct when_f  *date;
struct daddr_f *where;
{
    char    buf[DATELEN + 6];			/* hold formatted date */
    char    line[SYSSZ + NNLEN + SYSSZ + NAMESZ + 44];
							/* scratch pad */
    register char  *p;
    char   *q;
    int    linecount;

    p = line;
    p += strmove("/***** ", p);
    p += strmove(SYSTEM, p);			/* copy in system name */
    p += strmove(":", p);

    q = nfname;					/* copy in notefile name */
    while (*q != ' ' && *q != '\0') {
	*p++ = *q++;
    }

    p += strmove(" / ", p);			/* more separators */
    if (strcmp(SYSTEM, unique->sys) != 0) {
	p += strmove(unique->sys, p);
	p += strmove("!", p);
    }
    q = author->aname;			/* copy over the author name */
    while (*q != ' ' && *q != '\0') {
	*p++ = *q++;
    }

    p += strmove(" / ", p);
    sprdate (date, buf);			/* format the date written */
    p += strmove(buf, p);

    p += strmove("*/\n", p);
    fprintf(zfile, "%s", line);			/* dump the header */
    linecount = 1;				/* how many lines saved */

    linecount += pageout(io, where, zfile);	/* write to the file */
    fprintf(zfile, "/* ---------- */\n");	/* a trailer */
    linecount++;				/* and a line for the trailer */
    return(linecount);
}
