static char *sccsid = "@(#)pagein.c	1.2 2/2/83";

#include "parms.h"
#include "structs.h"
/*
 *	pagein(zfile, where)
 *	FILE *zfile; struct daddr_f *where;
 *
 *	reads in a single 'page' as defined by the notesfile system and
 *	returns the count of characters moved.
 *
 *	Original Coding:	Ray Essick	Long agao
 *		moved out of gtext.c and mult.c and others   4/5/82
 *	Converted for infinite size notes	RBE 5/8/82
 *
 */

char longtitle[WDLEN];

pagein (io, zfile, where)
struct io_f *io;
FILE * zfile;
struct daddr_f *where;
{

    int     c,
            i,
            nchars;
    struct txthead_f    txthead;			/* a text header */
    struct daddr_f  nwhere;
    struct txtbuf_f buf;		/* hold bunches of text */

    lock(io, 't');
    x (lseek(io->fidtxt, 0L, 0) < 0, "pagein: bad seek 0");
    x (read(io->fidtxt, where, sizeof(*where)) < 0, "pagein: read 0");
    x (lseek(io->fidtxt, where->addr + sizeof(txthead), 0) < 0, "pagein:badseek");

    nchars = 0;
    i = 0;
    if (strlen(longtitle) > TITLEN) {
	strcpy(&buf.txtbuf[0],"Subject: ");
	strcpy(&buf.txtbuf[9],longtitle);
	i = 9 + strlen(longtitle);
	nchars = i;
    }
    while ((c = getc(zfile)) != EOF) {			/* grab input */
	if (i == BUFSIZE) {		/* buffer is full, flush it */
	    x (write(io->fidtxt, buf.txtbuf, BUFSIZE) != BUFSIZE, "pagein: bad text");
	    i = 0;					/* reset buffer */
	}
	buf.txtbuf[i++] = c;
	if (++nchars == MAXMSG) {		/* count characters */
	    break;				/* filled our counter */
	}
    }
    x (write(io->fidtxt, buf.txtbuf, i) != i, "pagein: bad text");
    x (lseek(io->fidtxt, where->addr, 0) < 0, "pagein: bad reseek");
    txthead.note_no = 0;
    txthead.resp_no = 0;
    txthead.textlen = nchars;				/* fill header */
    x (write(io->fidtxt, &txthead, sizeof(txthead)) != sizeof(txthead),
	    "pagein: bad header");
    x (lseek(io->fidtxt, 0l, 0) < 0, "pagein:bad reseek");
    nwhere.addr = where->addr + nchars + sizeof(txthead);
    if (nwhere.addr & 1) {				/* odd?? */
	nwhere.addr++;				/* round to word boundary */
    }
    x (write(io->fidtxt, &nwhere, sizeof(nwhere)) != sizeof(nwhere), "pagein: badupdate");

    unlock(io, 't');
    return(nchars);
}
