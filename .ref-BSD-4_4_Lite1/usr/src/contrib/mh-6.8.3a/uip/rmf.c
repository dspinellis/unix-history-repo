/* rmf.c - remove a folder */
#ifndef	lint
static char ident[] = "@(#)$Id: rmf.c,v 2.7 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include "../h/local.h"
#include <stdio.h>
#ifdef LOCALE
#include	<locale.h>
#endif

/*  */

static struct swit switches[] = {
#define	INTRSW	0
    "interactive", 0,
#define	NINTRSW	1
    "nointeractive", 0,

#define	HELPSW	2
    "help", 4,

    NULL, 0
};

static	int	rmf();
static 		rma();
/*  */

/* ARGSUSED */

main (argc, argv)
int argc;
char *argv[];
{
    int     defolder = 0,
            interactive = -1;
    char   *cp,
           *folder = NULL,
            newfolder[BUFSIZ],
            buf[100],
          **ap,
          **argp,
           *arguments[MAXARGS];

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
		    (void) sprintf (buf, "%s [+folder] [switches]", invo_name);
		    help (buf, switches);
		    done (1);

		case INTRSW: 
		    interactive = 1;
		    continue;
		case NINTRSW: 
		    interactive = 0;
		    continue;
	    }
	if (*cp == '+' || *cp == '@') {
	    if (folder)
		adios (NULLCP, "only one folder at a time!");
	    else
		folder = path (cp + 1, *cp == '+' ? TFOLDER : TSUBCWF);
	}
	else
	    adios (NULLCP, "usage: %s [+folder] [switches]", invo_name);
    }

/*  */

    if (!m_find ("path"))
	free (path ("./", TFOLDER));
    if (!folder) {
	folder = m_getfolder ();
	defolder++;
    }
    if (strcmp (m_mailpath (folder), pwd ()) == 0)
	adios (NULLCP, "sorry, you can't remove the current working directory");

    if (interactive == -1)
	interactive = defolder;

    if (index (folder, '/') && (*folder != '/') && (*folder != '.')) {
	for (cp = copy (folder, newfolder); cp > newfolder && *cp != '/'; cp--)
	    continue;
	if (cp > newfolder)
	    *cp = '\0';
	else
	    (void) strcpy (newfolder, (cp = m_find (inbox)) ? cp : defalt);
    }
    else
	(void) strcpy (newfolder, (cp = m_find (inbox)) ? cp : defalt);

    if (interactive) {
	cp = concat ("Remove folder \"", folder, "\"? ", NULLCP);
	if (!getanswer (cp))
	    done (0);
	free (cp);
    }

    if (rmf (folder) == OK && strcmp (m_find (pfolder), newfolder)) {
	printf ("[+%s now current]\n", newfolder);
	m_replace (pfolder, newfolder);
    }
    m_update ();

    done (0);
}

/*  */

static int  rmf (folder)
register char *folder;
{
    int     i,
            j,
            others;
    register char  *maildir;
    char    cur[BUFSIZ];
#ifdef SYS5DIR
    register struct dirent *dp;
#else	/*  SYS5DIR */
    register struct direct *dp;
#endif	/* SYS5DIR */
    register    DIR * dd;

#ifdef	COMPAT
    (void) m_delete (concat (current, "-", m_mailpath (folder), NULLCP));
#endif	/* COMPAT */
    switch (i = chdir (maildir = m_maildir (folder))) {
	case OK: 
	    if (access (".", 2) != NOTOK && access ("..", 2) != NOTOK)
		break;		/* fall otherwise */

	case NOTOK: 
	    (void) sprintf (cur, "atr-%s-%s", current, m_mailpath (folder));
	    if (!m_delete (cur)) {
		printf ("[+%s de-referenced]\n", folder);
		return OK;
	    }
	    advise (NULLCP, "you have no profile entry for the %s folder +%s",
		    i == NOTOK ? "unreadable" : "read-only", folder);
	    return NOTOK;
    }

    if ((dd = opendir (".")) == NULL)
	adios (NULLCP, "unable to read folder +%s", folder);
    others = 0;

    j = strlen (SBACKUP);
    while (dp = readdir (dd)) {
	switch (dp -> d_name[0]) {
	    case '.': 
		if (strcmp (dp -> d_name, ".") == 0
			|| strcmp (dp -> d_name, "..") == 0)
		    continue;	/* else fall */

	    case ',': 
#ifdef	MHE
	    case '+': 
#endif	/* MHE */
#ifdef	UCI
	    case '_': 
	    case '#': 
#endif	/* UCI */
		break;

	    default: 
		if (m_atoi (dp -> d_name))
		    break;
#ifdef	COMPAT
		if (strcmp (dp -> d_name, current) == 0)
		    break;
#endif	/* COMPAT */
		if (strcmp (dp -> d_name, LINK) == 0
			|| strncmp (dp -> d_name, SBACKUP, j) == 0)
		    break;

		admonish (NULLCP, "file \"%s/%s\" not deleted",
			folder, dp -> d_name);
		others++;
		continue;
	}
	if (unlink (dp -> d_name) == NOTOK) {
	    admonish (dp -> d_name, "unable to unlink %s:", folder);
	    others++;
	}
    }

    closedir (dd);

    rma (folder);

    (void) chdir ("..");
    if (others == 0 && remdir (maildir))
	return OK;

    advise (NULLCP, "folder +%s not removed", folder);
    return NOTOK;
}

/*  */

static  rma (folder)
register char   *folder;
{
    register int    alen,
                    j,
                    plen;
    register char  *cp;
    register struct node   *np,
                           *pp;

    alen = strlen ("atr-");
    plen = strlen (cp = m_mailpath (folder)) + 1;

    m_getdefs ();
    for (np = m_defs, pp = NULL; np; np = np -> n_next)
	if (ssequal ("atr-", np -> n_name)
		&& (j = strlen (np -> n_name) - plen) > alen
		&& *(np -> n_name + j) == '-'
		&& strcmp (cp, np -> n_name + j + 1) == 0) {
	    if (!np -> n_context)
		admonish (NULLCP, "bug: m_delete(key=\"%s\")", np -> n_name);
	    if (pp) {
		pp -> n_next = np -> n_next;
		np = pp;
	    }
	    else
		m_defs = np -> n_next;
	    ctxflags |= CTXMOD;
	}
	else
	    pp = np;
}
