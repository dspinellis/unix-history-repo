/* rmm.c - remove a message */
#ifndef	lint
static char ident[] = "@(#)$Id: rmm.c,v 1.5 1992/12/15 00:20:22 jromine Exp $";
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

main (argc, argv)
int     argc;
char  **argv;
{
    int     msgp = 0,
            msgnum,
            vecp;
    char   *cp,
           *dp,
           *maildir,
           *folder = NULL,
            buf[100],
          **ap,
          **argp,
          **vec,
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
		    adios (NULLCP, "-%s unknown\n", cp);
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
    if (!msgp)
	msgs[msgp++] = "cur";
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
	    done (1);
    m_setseq (mp);

    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	if (mp -> msgstats[msgnum] & SELECTED) {
#ifdef	notdef
	    mp -> msgstats[msgnum] |= DELETED;
#endif	/* notdef */
	    mp -> msgstats[msgnum] &= ~EXISTS;
	}
    mp -> msgflags |= SEQMOD;

    m_replace (pfolder, folder);
    m_sync (mp);
    m_update ();

    if (rmmproc) {
	if (mp -> numsel > MAXARGS - 2)
	    adios (NULLCP, "more than %d messages for %s exec", MAXARGS - 2,
		    rmmproc);
	vec = (char **) calloc ((unsigned) (mp -> numsel + 2), sizeof *vec);
	if (vec == NULL)
	    adios (NULLCP, "unable to allocate exec vector");
	vecp = 1;
	for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	    if (mp -> msgstats[msgnum] & SELECTED)
		vec[vecp++] = getcpy (m_name (msgnum));
	vec[vecp] = NULL;

	(void) fflush (stdout);
	vec[0] = r1bindex (rmmproc, '/');
	execvp (rmmproc, vec);
	adios (rmmproc, "unable to exec");
    }

    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	if (mp -> msgstats[msgnum] & SELECTED) {
	    (void) strcpy (buf, m_backup (dp = m_name (msgnum)));
	    if (rename (dp, buf) == NOTOK)
		admonish (buf, "unable to rename %s to", dp);
	}

    done (0);
}
