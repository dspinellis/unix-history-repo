/* anno.c - annotate messages */

#include "../h/mh.h"
#include <ctype.h>
#include <stdio.h>

/*  */

static struct swit switches[] = {
#define	COMPSW	0
    "component field", 0,

#define	INPLSW	1
    "inplace", 0,
#define	NINPLSW	2
    "noinplace", 0,

#define	TEXTSW	3
    "text body", 0,

#define	HELPSW	4
    "help", 4,

    NULL, NULL
};

/*  */

/* ARGSUSED */

main (argc, argv)
int     argc;
char  **argv;
{
    int     inplace = 0,
            msgp = 0,
            msgnum;
    char   *cp,
           *maildir,
           *comp = NULL,
           *text = NULL,
           *folder = NULL,
            buf[100],
          **ap,
          **argp,
           *arguments[MAXARGS],
           *msgs[MAXARGS];
    struct msgs *mp;

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

		case COMPSW: 
		    if (comp)
			adios (NULLCP, "only one component at a time!");
		    if (!(comp = *argp++) || *comp == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;

		case INPLSW: 
		    inplace++;
		    continue;
		case NINPLSW: 
		    inplace = 0;
		    continue;

		case TEXTSW: 
		    if (text)
			adios (NULLCP, "only one body at a time!");
		    if (!(text = *argp++) || *text == '-')
			adios (NULLCP, "missing argument to %s", argp[-2]);
		    continue;
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

    make_comp (&comp);

    for (msgnum = mp -> lowsel; msgnum <= mp -> hghsel; msgnum++)
	if (mp -> msgstats[msgnum] & SELECTED)
	    (void) annotate (m_name (msgnum), comp, text, inplace);

    m_replace (pfolder, folder);
    if (mp -> lowsel != mp -> curmsg)
	m_setcur (mp, mp -> lowsel);
    m_sync (mp);
    m_update ();

    done (0);
}

/*  */

static make_comp (ap)
register char **ap;
{
    register char  *cp;
    char    buffer[BUFSIZ];

    if (*ap == NULL) {
	printf ("Enter component name: ");
	(void) fflush (stdout);

	if (fgets (buffer, sizeof buffer, stdin) == NULL)
	    done (1);
	*ap = trimcpy (buffer);
    }

    if ((cp = *ap + strlen (*ap) - 1) > *ap && *cp == ':')
	*cp = NULL;
    if (strlen (*ap) == 0)
	adios (NULLCP, "null component name");
    if (**ap == '-')
	adios (NULLCP, "invalid component name %s", *ap);
    if (strlen (*ap) >= NAMESZ)
	adios (NULLCP, "too large component name %s", *ap);

    for (cp = *ap; *cp; cp++)
	if (!isalnum (*cp) && *cp != '-')
	    adios (NULLCP, "invalid component name %s", *ap);
}
