static char *sccsid = "%W%";

#include "parms.h"
#include "structs.h"
#ifdef BSD4.1c
#include <sys/types.h>
#include <sys/dir.h>
#include <sys/file.h>
#else
#define L_SET 0
#define L_INCR 1
#define L_XTND 2
#endif BSD4.1c

/*
 *  init(io,p), finish(io)  struct io_f *io, char *p
 *    initopens the three i/o files and initializes session stats
 *
 *    finish(io) closes all those files.
 *
 *  getnrec, putnrec, getrrec, putrrec
 *  getdscr, putdscr, gettrec, puttrec
 *  each gets or puts physical records inside its appropriate file.
 *
*/

long lseek();					/* for sake of lint */

init(io, p)
struct io_f *io;
char   *p;
{
    char   *q,
           *r;					/* temp char pointers */
    int     i;					/* counter */
    char    fn[WDLEN];				/* build scratch names */
    struct auth_f   me;				/* for identifying self */
#ifdef BSD4.1c
    DIR *dir;
#endif BSD4.1c

    if (chkpath(p) != 0)
	return(QUITNEX);
    q = p;
    r = io->nf;
    i = NNLEN;
    while ((*r++ = *q++) && --i);		/* transfer notefile name */
    sprintf(fn, "%s/%s", MSTDIR, p);		/* open the directory */
#ifdef BSD4.1c
    if ((dir = opendir(fn)) == NULL) {
#else
    if ((i = open(fn, 0)) < 0) {
#endif BSD4.1c
	return(QUITNEX);
    }
#ifdef BSD4.1c
    closedir(dir);
#else
    close(i);					/* close the directory */
#endif BSD4.1c

    if (opennf(io, p) < 0)
	return(QUITBAD);			/* bad luck opening */

    getdscr(io, &io->descr);
    getname(&me, 0);			/* grab identity for permissions */
    getperms(io, 0, me.aname);		/* go establish access rights */

    io->nrspwrit = io->nnotwrit = 0;	/* set up stats */
    io->nrspread = io->nnotread = 0;
    io->nnotxmit = io->nrspxmit = 0;
    io->nnotrcvd = io->nrsprcvd = 0;
    io->nnotdrop = io->nrspdrop = 0;
    io->norphans = 0;
    io->xstring[0] = io->xaname[0] = io->xasys[0] = '\0';
					/* clear search strings */

    time(&io->entered);			/* get entry time */

    return(io->access);			/* return the access bits */
}

opennf (io, p)
struct io_f *io;
char   *p;
{
    char fn[WDLEN];

    sprintf (fn, "%s/%s/%s", MSTDIR, p, TEXT);
#ifdef BSD4.1c
    if ((io->fidtxt = open(fn, O_RDWR, 0)) < 0) {
#else
    if ((io->fidtxt = open(fn, 2)) < 0) {
#endif BSD4.1c
	return(-1);					/* bad nf */
    }

    sprintf (fn, "%s/%s/%s", MSTDIR, p, INDEXN);
#ifdef BSD4.1c
    if ((io->fidndx = open(fn, O_RDWR, 0)) < 0) {
#else
    if ((io->fidndx = open(fn, 2)) < 0) {
#endif BSD4.1c
	close(io->fidtxt);
	return(-1);
    }

    sprintf (fn, "%s/%s/%s", MSTDIR, p, INDEXR);
#ifdef BSD4.1c
    if ((io->fidrdx = open(fn, O_RDWR, 0)) < 0) {
#else
    if ((io->fidrdx = open(fn, 2)) < 0) {
#endif BSD4.1c
	close(io->fidtxt);
	close(io->fidndx);
	return(-1);					/* bad nf */
    }
    return(0);						/* all's well */
}


finish (io)
struct io_f *io;
{
    long left;
    struct when_f lvtime;				/* for days used */

    lock(io, 'n');					/* update statistics */
    getdscr(io, &io->descr);
    io->descr.d_notwrit += io->nnotwrit;
    io->descr.d_rspwrit += io->nrspwrit;
    io->descr.d_notread += io->nnotread;
    io->descr.d_rspread += io->nrspread;
    io->descr.d_notxmit += io->nnotxmit;
    io->descr.d_rspxmit += io->nrspxmit;
    io->descr.d_notrcvd += io->nnotrcvd;
    io->descr.d_rsprcvd += io->nrsprcvd;
    io->descr.d_notdrop += io->nnotdrop;
    io->descr.d_rspdrop += io->nrspdrop;
    io->descr.d_orphans += io->norphans;
    io->descr.entries++;			/* count of entries */
    time(&left);
    io->descr.walltime += left - io->entered;   /* time spent in nf */
    gettime(&lvtime);
    if ((lvtime.w_day != io->descr.d_lastuse.w_day) ||
	    (lvtime.w_month != io->descr.d_lastuse.w_month) ||
	    (lvtime.w_year != io->descr.d_lastuse.w_year))
    {
	io->descr.d_daysused++;
	copydate(&lvtime, &io->descr.d_lastuse);
    }
    putdscr(io, &io->descr);			/* update the block */
    unlock(io, 'n');

    closenf(io);
}

closenf(io)
struct io_f *io;
{

    x (close(io->fidtxt) < 0, "finish: text fail");
    x (close(io->fidndx) < 0, "finish: nindx fail");
    x (close(io->fidrdx) < 0, "finish: rindx fail");
}



/* n is the number of the note to get.  0 is policy note */
getnrec (io, n, note)
struct io_f *io;
int n;
struct note_f *note;
{
    long where;				/* going to seek here eventually */
    struct descr_f *descr;		/* for sizeof below */

    x (n < 0, "getnrec: negative recnum");
    where = sizeof(*descr) + n * sizeof(*note);
    x (lseek(io->fidndx, where, L_SET) < 0, "getnrec: seek");
    x (read(io->fidndx, note, sizeof(*note)) < sizeof(*note), "getnrec: read");
}

/* n is the number of the note to put.  0 is policy note */
putnrec (io, n, note)
struct io_f *io;
int n;
struct note_f *note;
{
    long where;				/* going to seek here eventually */
    struct descr_f *descr;		/* for sizeof below */

    x (n < 0, "putnrec: negative recnum");
    where = sizeof(*descr) + n * sizeof(*note);
    x (lseek(io->fidndx, where, L_SET) < 0, "putnrec: seek");
    x (write(io->fidndx, note, sizeof(*note)) < sizeof(*note), "putnrec: write ");
}

getdscr (io, descr)
struct io_f *io;
struct descr_f *descr;
{

    x (lseek(io->fidndx, 0L, L_SET) < 0, "getdscr: seek");
    x (read(io->fidndx, descr, sizeof(*descr)) < sizeof(*descr), "getdscr: read");
}

putdscr (io, descr)
struct io_f *io;
struct descr_f *descr;
{

    x (lseek(io->fidndx, 0L, L_SET) < 0, "putdscr: seek");
    x (write(io->fidndx, descr, sizeof(*descr)) < sizeof(*descr), "putdscr: write");
}

getrrec (io, n, resp)
struct io_f *io;
int n;
struct resp_f *resp;	/* n is the number of the resp to get */
{
    long where;				/* going to seek here eventually */
    int  a;				/* size of free link */
    x (n < 0, "getrrec: negative recnum");

    where = sizeof a + n * sizeof(*resp);
    x (lseek(io->fidrdx, where, L_SET) < 0, "getrrec: seek");
    x (read(io->fidrdx, resp, sizeof(*resp)) < sizeof(*resp), "getrrec: read");
}

putrrec (io, n, resp)
struct io_f *io;
int n;
struct resp_f *resp;	/* n is the number of the resp to put */
{
    long where;				/* going to seek here eventually */
    int  a;					/* size of free link */
    x (n < 0, "putrrec: negative recnum");

    where = sizeof a + n * sizeof(*resp);
    x (lseek(io->fidrdx, where, L_SET) < 0, "putrrec: seek");
    x (write(io->fidrdx, resp, sizeof(*resp)) < sizeof(*resp), "putrrec: write");
}


/*
 *	gethrec(&io_f, &daddr_f, &txthead_f)
 *
 *	seeks and reads the text record header. Leaves the file pointing
 *	at the text itself.
 *
 *	Ray Essick	May 8, 1982
 */
gethrec (io, where, head)
struct io_f *io;
struct daddr_f *where;
struct txthead_f   *head;
{
    x (lseek(io->fidtxt, where->addr, L_SET) < 0, "gethrec: seek");
    x (read(io->fidtxt, head, sizeof(*head)) != sizeof(*head), "gethrec: read");
}

/*
 *	puttrec(i&io_f, &FILE, &daddr_f, int)
 *
 *	reads cound characters from the input stream specified ad
 *	puts them into the text file. The address is returned...
 *
 *	Ray Essick	May 8, 1982
 */
puttrec (io, zfile, where, count)
struct io_f *io;
FILE * zfile;
struct daddr_f *where;
int count;
{

    int     i,
            nchars;
    struct txthead_f    txthead;		/* a text header */
    struct daddr_f  nwhere;
    struct txtbuf_f buf;			/* hold bunches of text */

    lock(io, 't');				/* grab access to the file */
    x (lseek(io->fidtxt, 0L, L_SET) < 0, "pagein: bad seek 0");
    x (read(io->fidtxt, where, sizeof nwhere) < 0, "pagein: read 0");
    x (lseek(io->fidtxt, where->addr + sizeof(txthead), 0) < 0, "pagein:badseek");

    nchars = 0;
    i = 0;
    while (nchars != count) {		/* grab input */
	if (i == BUFSIZE) {		/* buffer is full, flush it */
	    x (write (io->fidtxt, buf.txtbuf, BUFSIZE) != BUFSIZE, "pagein: bad text");
	    i = 0;					/* reset buffer */
	}
	buf.txtbuf[i++] = getc(zfile);
	nchars++;				/* and count characters */
    }
    if (i) {				/* write out partial last page */
	x (write(io->fidtxt, buf.txtbuf, i) != i, "pagein: bad text");
    }
    x (lseek(io->fidtxt, where->addr, L_SET) < 0, "pagein: bad reseek");
    txthead.note_no = 0;
    txthead.resp_no = 0;
    txthead.textlen = nchars;				/* fill header */
    x (write(io->fidtxt, &txthead, sizeof(txthead)) != sizeof(txthead),
	    "pagein: bad header");
    x (lseek(io->fidtxt, 0L, L_SET) < 0, "pagein:bad reseek");
    nwhere.addr = where->addr + nchars + sizeof(txthead);
    if (nwhere.addr & 1) {		/* odd ? */
	nwhere.addr++;			/* round to word boundary */
    }
    x (write(io->fidtxt, &nwhere, sizeof(nwhere)) != sizeof(nwhere), "pagein: badupdate");

    unlock(io, 't');
    return(nchars);
}
