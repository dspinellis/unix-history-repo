#include "parms.h"
#include "structs.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: pageout.c,v 1.7 85/01/18 15:33:54 notes Rel $";
#endif	RCSIDENT

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
            lines;
    long    count;
    struct txtbuf_f buf;				/* hold text */

    lines = 0;

    if (where -> addr == 0)				/* no text */
	where -> textlen = 0;				/* fix this */
    x (lseek (io -> fidtxt, where -> addr, 0) != where -> addr, "pageout: seek");
    for (j = i = count = 0; count < where -> textlen; count++)
    {
	if (i == j)					/* another buffer */
	{
	    x ((j = read (io -> fidtxt, buf.txtbuf, BUFSIZE)) < 0, "pageout: read");
	    i = 0;
	}
	putc (buf.txtbuf[i], zfile);
	if (buf.txtbuf[i++] == '\n')
	    lines++;					/* count lines */
    }
    return lines;
}
