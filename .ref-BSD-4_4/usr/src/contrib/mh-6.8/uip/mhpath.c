/* mhpath.c - print full pathnames */
#ifndef	lint
static char ident[] = "@(#)$Id: mhpath.c,v 1.4 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <stdio.h>
#ifdef LOCALE
#include	<locale.h>
#endif

/*  */

static struct swit switches[] = {
#define	HELPSW	0
    "help", 4,

    NULL, 0
};

/*  */

/* ARGSUSED */

main(argc, argv)
	int argc;
	char *argv[];
{
    int     msgp = 0,
            msgnum;
    char   *cp,
           *maildir,
           *folder = NULL,
            buf[100],
          **ap,
          **argp,
           *arguments[MAXARGS],
           *msgs[MAXARGS];
    struct msgs *mp;

#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
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

    if (!folder)
	folder = m_getfolder ();
    maildir = m_maildir (folder);
    if (!msgp) {
	printf ("%s\n", maildir);
	done (0);
    }

    if (chdir (maildir) == NOTOK)
	adios (maildir, "unable to change directory to");
    if (!(mp = m_gmsg (folder)))
	adios (NULLCP, "unable to read folder %s", folder);

    if ((mp = m_remsg (mp, 0, MAXFOLDER)) == NULL)
	adios (NULLCP, "unable to allocate folder storage");
    mp -> msgflags |= MHPATH;

    for (msgnum = 0; msgnum < msgp; msgnum++)
	if (!m_convert (mp, msgs[msgnum]))
	    done (1);
    m_setseq (mp);

/*  */

    if (mp -> numsel > MAXARGS - 2)
	adios (NULLCP, "more than %d messages", MAXARGS - 2);

    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	if (mp -> msgstats[msgnum] & SELECTED)
	    printf ("%s/%s\n", mp -> foldpath, m_name (msgnum));

    m_sync (mp);
    m_update ();

    done (0);
}
