/* muinc.c - mmdf to uucp inc */
#ifndef	lint
static char Id[] = "@(#)$Id: muinc.c,v 1.2 1993/08/25 17:30:36 jromine Exp $";
#endif

#include "mf.h"
#include <stdio.h>
#include "../mts/mts.h"
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>


static int  mmdf = NOTOK;
static int  uucp = NOTOK;
static char mmdfbox[LINESIZ];
static char uucpbox[LINESIZ];

/*  */

main (argc, argv)
int     argc;
char   *argv[];
{
    int     fd,
            tmp;
    struct stat st1,
                st2;

    mts_init (*argv);
    sprintf (mmdfbox, "%s/%s", MAILDIR, MAILFIL);
    if (stat (mmdfbox, &st1) == NOTOK || st1.st_size == 0L)
	exit (0);
    if ((mmdf = lkopen (mmdfbox, 0)) == NOTOK)
	die ("unable to lock and open %s", mmdfbox);
    tmp = tmp_open (&fd);

    switch (fd = mmdf2uucp (mmdf, fd, FALSE)) {
	case MFOK: 
	    break;

	case MFPRM: 
	    die ("internal error while filtering MMDF mail");

	case MFSIO: 
	    die ("no free file pointers -- you lose");

	case MFERR: 
	    die ("i/o error while filtering MMDF mail");

	case MFROM: 
	case MFHDR: 
	case MFTXT: 
	    fprintf (stderr, "MMDF mailbox in bad format, patched...\n");
	    break;
    }

    sprintf (uucpbox, "%s/%s", UUCPDIR, UUCPFIL);
    uucp = mbx_open (uucpbox);
    mbx_copy (tmp, uucp);
    close (tmp);
    lkclose (uucp, uucpbox), uucp = NOTOK;

    if (stat (mmdfbox, &st2) != NOTOK && st1.st_mtime != st2.st_mtime)
	fprintf (stderr, "MMDF mailbox has been updated... (%s)\n",
		"so it won't be zero'd");
    else
	if ((fd = creat (mmdfbox, st1.st_mode & ~S_IFMT)) != NOTOK)
	    close (fd);
	else
	    fprintf (stderr, "unable to zero MMDF mailbox\n");
    lkclose (mmdf, mmdfbox), mmdf = NOTOK;

    exit (0);
}

/*  */

static int  mbx_open (file)
char   *file;
{
    int     count,
            fd;
    extern int  errno;

    for (count = 2; count > 0; count--)
	if ((fd = lkopen (file, 1)) == NOTOK)
	    switch (errno) {
		case ENOENT: 
		    mbx_create (file);
		    break;
		case ETXTBSY: 
		    sleep (5);
		    break;
		default: 
		    goto openerr;
	    }

    if (fd == NOTOK) {
openerr: 
	if (errno == ETXTBSY)
	    die ("your UUCP mailbox '%s' is busy", file);
	else
	    die ("unable to open UUCP mailbox '%s'", file);
    }

    lseek (fd, (off_t)0, 2);

    return fd;
}

/*  */

static  mbx_create (file)
char   *file;
{
    int     fd;

    if ((fd = creat (file, MBXMODE)) == NOTOK)
	die ("unable to create UUCP mailbox '%s'", file);

    close (fd);
}

/*  */

static  mbx_copy (in, out)
int     in,
        out;
{
    int     i;
    char    buffer[BUFSIZ];

    lseek (in, (off_t)0, 0);

    while ((i = read (in, buffer, sizeof buffer)) > 0)
	if (write (out, buffer, i) != i)
	    die ("error writing UUCP mailbox");
    if (i < 0)
	die ("error reading temporary file");
}

/*  */

static int  tmp_open (mbx_fd)
int    *mbx_fd;
{
    int     fd;
    char    tmpfil[LINESIZ];

    strcpy (tmpfil, "/tmp/muincXXXXXX");
    unlink (mktemp (tmpfil));
    if ((fd = creat (tmpfil, TMPMODE)) == NOTOK)
	die ("unable to create temporary file '%s'", tmpfil);
    close (fd);

    if ((fd = open (tmpfil, 2)) == NOTOK)
	die ("unable to create temporary file '%s'", tmpfil);
    unlink (tmpfil);

    if ((*mbx_fd = dup (fd)) == NOTOK)
	die ("unable to duplicate fd for temporary file '%s'", tmpfil);

    return fd;
}

/*  */

static  die (fmt, a, b, c, d)
char   *fmt,
       *a,
       *b,
       *c,
       *d;
{
    lkclose (mmdf, mmdfbox), mmdf = NOTOK;
    lkclose (uucp, uucpbox), uucp = NOTOK;

    fflush (stdout);
    fprintf (stderr, fmt, a, b, c, d);
    putc ('\n', stderr);

    exit (1);
}
