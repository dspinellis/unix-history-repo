/* uminc.c - uucp to mmdf inc */
#ifndef	lint
static char Id[] = "@(#)$Id: uminc.c,v 1.2 1993/08/25 17:31:30 jromine Exp $";
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


off_t    lseek ();

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
    sprintf (uucpbox, "%s/%s", UUCPDIR, UUCPFIL);
    if (stat (uucpbox, &st1) == NOTOK || st1.st_size == 0L)
	exit (0);
    if ((uucp = lkopen (uucpbox, 0)) == NOTOK)
	die ("unable to lock and open %s", uucpbox);
    tmp = tmp_open (&fd);

    switch (fd = uucp2mmdf (uucp, fd, FALSE)) {
	case MFOK: 
	    break;

	case MFPRM: 
	    die ("internal error while filtering UUCP mail");

	case MFSIO: 
	    die ("no free file pointers -- you lose");

	case MFERR: 
	    die ("i/o error while filtering UUCP mail");

	case MFROM: 
	case MFHDR: 
	case MFTXT: 
	    fprintf (stderr, "UUCP mailbox in bad format, patched...\n");
	    break;
    }

    sprintf (mmdfbox, "%s/%s", MAILDIR, MAILFIL);
    mmdf = mbx_open (mmdfbox);
    mbx_copy (tmp, mmdf);
    close (tmp);
    lkclose (mmdf, mmdfbox), mmdf = NOTOK;

    if (stat (uucpbox, &st2) != NOTOK && st1.st_mtime != st2.st_mtime)
	fprintf (stderr, "UUCP mailbox has been updated... (%s)\n",
		"so it won't be removed");
    else
	if (unlink (uucpbox) == NOTOK)
	    if ((fd = creat (uucpbox, st1.st_mode & ~S_IFMT)) != NOTOK)
		close (fd);
	    else
		fprintf (stderr, "unable to remove or zero UUCP mailbox\n");
    lkclose (uucp, uucpbox), uucp = NOTOK;

    exit (0);
}

/*  */

static int  mbx_open (file)
char   *file;
{
    int     clear,
            count,
            fd;
    extern int  errno;
    struct stat stbuf;

    for (clear = FALSE, count = 2; count > 0; count--)
	if ((fd = lkopen (file, 6)) == NOTOK)
	    switch (errno) {
		case ENOENT: 
		    mbx_create (file);
		    clear++;
		    break;

		case ETXTBSY: 
		    sleep (5);
		    break;

		default: 
		    goto openerr;
	    }
	else {
	    if (fstat (fd, &stbuf) == NOTOK)
		die ("unable to stat MMDF mailbox '%s'", file);
	    clear = stbuf.st_size == 0L;
	    break;
	}

    if (fd == NOTOK) {
openerr: 
	if (errno == ETXTBSY)
	    die ("your MMDF mailbox '%s' is busy", file);
	else
	    die ("unable to open MMDF mailbox '%s'", file);
    }
    if (!clear)
	mbx_chk (fd, file);

    return fd;
}

/*  */

static  mbx_create (file)
char   *file;
{
    int     fd;

    if ((fd = creat (file, MBXMODE)) == NOTOK)
	die ("unable to create MMDF mailbox '%s'", file);

    close (fd);
}


static  mbx_chk (fd, file)
int     fd;
char   *file;
{
    int     count;
    char    ldelim[20];

    count = strlen (mmdlm2);

    if (lseek (fd, (off_t) - count, 2) == (off_t) NOTOK
	    || read (fd, ldelim, count) != count)
	die ("error reading MMDF mailbox '%s'", file);
    ldelim[count] = NULL;

    if (strcmp (ldelim, mmdlm2)) {
	fprintf (stderr,
		"MMDF mailbox '%s' has bad delimiter, patching...\n",
		file);
	if (write (fd, mmdlm2, count) != count)
	    die ("error writing MMDF mailbox '%s'", file);
    }
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
	    die ("error writing MMDF mailbox");
    if (i < 0)
	die ("error reading temporary file");

    close (in);
    close (out);
}

/*  */

static int  tmp_open (mbx_fd)
int    *mbx_fd;
{
    int     fd;
    char    tmpfil[LINESIZ];

    strcpy (tmpfil, "/tmp/umincXXXXXX");
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
