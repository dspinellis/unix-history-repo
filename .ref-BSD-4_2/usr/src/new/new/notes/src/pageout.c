static char *sccsid = "@(#)pageout.c	1.1\t1/23/83";

#include "parms.h"
#include "structs.h"
/*
 *	pageout(where,zfile)
 *	FILE *zfile; struct daddr_f *where;
 *
 *	Dumps the page to the file. Returns the characters dumped.
 *
 *	Original Coding:	Ray Essick	Long agao
 *		moved out of gtext.c and mult.c and others   4/5/82
 *	Converted for infinite size notes	RBE 5/8/82
 *
 */

pageout (io, where, zfile)
struct io_f *io;
struct daddr_f *where;
FILE * zfile;
{
    int     i,
            j,
            lines,
            count;
    struct txtbuf_f buf;				/* hold text */
    struct txthead_f    txthead;

    lines = 0;
    gethrec(io, where, &txthead);			/* get header */

    for (j = i = count = 0; j < txthead.textlen; j++) {
	if (i == count) {
	    x ((count = read(io->fidtxt, buf.txtbuf, BUFSIZE)) < 0, "pageout: read");
	    i = 0;
	}
	putc(buf.txtbuf[i], zfile);
	if (buf.txtbuf[i++] == '\n')
	    lines++;					/* count lines */
    }
    return(lines);
}
