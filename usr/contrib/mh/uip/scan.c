/* scan.c - display a one-line "scan" listing */

#include "../h/mh.h"
#include "../h/formatsbr.h"
#include "../h/scansbr.h"
#include "../zotnet/tws.h"
#include <errno.h>
#include <stdio.h>

/*  */

static struct swit switches[] = {
#define	CLRSW	0
    "clear", 0,
#define	NCLRSW	1
    "noclear", 0,

#define	FORMSW	2
    "form formatfile", 0,
#define	FMTSW	3
    "format string", 5,

#define	HEADSW	4
    "header", 0,
#define	NHEADSW	5
    "noheader", 0,

#define	WIDSW	6
    "width columns", 0,

#ifdef	BERK
#define	REVSW	7
    "reverse", 0,
#define	NREVSW	8
    "noreverse", 0,

#define	HELPSW	9
#else	not BERK
#define	HELPSW	7
#endif	not BERK
    "help", 4,

    NULL, NULL
};

/*  */

extern int errno;


void	clear_screen ();

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char   *argv[];
{
     int    clearflag = 0,
	    hdrflag = 0,
	    width = 0,
            msgp = 0,
	    ontty,
	    state,
            msgnum;
#ifdef  BERK
    register int    revflag = 0,
		    firstlim,
		    lastlim,
		    incr;
#endif  BERK
    char   *cp,
           *maildir,
           *folder = NULL,
	   *form = NULL,
	   *format = NULL,
            buf[100],
          **ap,
          **argp,
           *nfs,
           *arguments[MAXARGS],
           *msgs[MAXARGS];
    struct msgs *mp;
    FILE * in;

    invo_name = r1bindex (argv[0], '/');
    if ((cp = m_find (invo_name)) != NULL) {
	ap = brkstring (cp = getcpy (cp), " ", "\n");
	ap = copyip (ap, arguments);
    }
    else
	ap = arguments;
    (void) copyip (argv + 1, ap);
    argp = arguments;

/*  */

    while (cp = *argp++) {
	if (*cp == '-')
	    switch (smatch (++cp, switches)) {
		case AMBIGSW: 
		    ambigsw (cp, switches);
		    done (1);
		case UNKWNSW: 
		    adios (NULLCP, "-%s unknown", cp);
		case HELPSW: 
		    (void) sprintf (buf, "%s [+folder] [msgs] [switches]",
			    invo_name);
		    help (buf, switches);
		    done (1);

		case CLRSW: 
		    clearflag++;
		    continue;
		case NCLRSW: 
		    clearflag = 0;
		    continue;

		case FORMSW: 
		    if (!(form = *argp++) || *form == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    format = NULL;
		    continue;
		case FMTSW: 
		    if (!(format = *argp++) || *format == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    form = NULL;
		    continue;

		case HEADSW: 
		    hdrflag++;
		    continue;
		case NHEADSW: 
		    hdrflag = 0;
		    continue;

		case WIDSW: 
		    if (!(cp = *argp++) || *cp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    width = atoi (cp);
		    continue;
#ifdef  BERK
		case REVSW:
		    revflag++;
		    continue;
		case NREVSW:
		    revflag = 0;
		    continue;
#endif  BERK
	    }
	if (*cp == '+' || *cp == '@') {
	    if (folder)
		adios (NULLCP, "only one folder at a time!");
	    else
		folder = path (cp + 1, *cp == '+' ? TFOLDER : TSUBCWF);
	}
	else
	    msgs[msgp++] = cp;
    }

/*  */

    if (!m_find ("path"))
	free (path ("./", TFOLDER));
    if (!msgp)
	msgs[msgp++] = "all";
    if (!folder)
	folder = m_getfolder ();
    maildir = m_maildir (folder);

    if (chdir (maildir) == NOTOK)
	adios (maildir, "unable to change directory to");
    if (!(mp = m_gmsg (folder)))
	adios (NULLCP, "unable to read folder %s", folder);
    if (mp -> hghmsg == 0)
	adios (NULLCP, "no messages in %s", folder);

    for (msgnum = 0; msgnum < msgp; msgnum++)
	if (!m_convert (mp, msgs[msgnum]))
	    done(1);
    m_setseq (mp);

    m_replace (pfolder, folder);
    m_sync (mp);
    m_update ();

    nfs = new_fs (form, format, FORMAT);

    ontty = isatty (fileno (stdout));

/*  */

#ifdef  BERK
    if (revflag) {
	firstlim = mp -> hghsel;
	lastlim = mp -> lowsel;
	incr = -1;
    }
    else {
	firstlim = mp -> lowsel;
	lastlim = mp -> hghsel;
	incr = 1;
    }

    for (msgnum = firstlim;
	    (revflag ? msgnum >= lastlim : msgnum <= lastlim);
	    msgnum += incr)
#else	not BERK
    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
#endif  not BERK
	if (mp -> msgstats[msgnum] & SELECTED) {
	    if ((in = fopen (cp = m_name (msgnum), "r")) == NULL) {
		if (errno != EACCES)
		    admonish (cp, "unable to open message");
		else
		    printf ("%*d  unreadable\n", DMAXFOLDER, msgnum);
		free (cp);
		continue;
	    }

	    if (hdrflag)
		printf ("Folder %-32s%s\n\n", folder, dtimenow ());
	    switch (state = scan (in, msgnum, 0, nfs, width,
			msgnum == mp -> curmsg,
			hdrflag, 0L, 1)) {
		case SCNMSG: 
		case SCNERR: 
		    break;

		default: 
		    adios (NULLCP, "scan() botch (%d)", state);

		case SCNEOF: 
		    printf ("%*d  empty\n", DMAXFOLDER, msgnum);
		    break;
	    }
	    hdrflag = 0;
	    (void) fclose (in);
	    if (ontty)
		(void) fflush (stdout);
	}

/*  */

    if (clearflag)
	clear_screen ();

    done (0);
}
