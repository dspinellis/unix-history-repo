/* m_maildir.c - get the path for the mail directory */
#ifndef	lint
static char ident[] = "@(#)$Id: m_maildir.c,v 1.6 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <stdio.h>

#define	CWD	"./"
#define	NCWD	(sizeof CWD - 1)
#define	DOT	"."
#define	DOTDOT	".."
#define	PWD	"../"
#define	NPWD	(sizeof PWD - 1)


static char mailfold[BUFSIZ];

static char   *exmaildir ();


char   *m_maildir (folder)
register char   *folder;
{
    register char  *cp,
                   *ep;

    if ((cp = exmaildir (folder))
	    && (ep = cp + strlen (cp) - 1) > cp
	    && *ep == '/')
	*ep = 0;

    return cp;
}

/*  */

char   *m_mailpath (folder)
register char   *folder;
{
    register char  *cp;
    char    maildir[BUFSIZ];

    if (*folder == '/'
	    || strncmp (folder, CWD, NCWD) == 0
	    || strcmp (folder, DOT) == 0
	    || strcmp (folder, DOTDOT) == 0
	    || strncmp (folder, PWD, NPWD) == 0)
	cp = path (folder, TFOLDER);
    else {
	(void) strcpy (maildir, mailfold);/* preserve... */
	cp = getcpy (m_maildir (folder));
	(void) strcpy (mailfold, maildir);
    }

    return cp;
}

/*  */

static char *exmaildir (folder)
register char   *folder;
{
    register char  *cp,
                   *pp;

    if (folder == NULL)
	folder = m_getfolder ();
    if (*folder == '/'
	    || strncmp (folder, CWD, NCWD) == 0
	    || strcmp (folder, DOT) == 0
	    || strcmp (folder, DOTDOT) == 0
	    || strncmp (folder, PWD, NPWD) == 0) {
	(void) strcpy (mailfold, folder);
	return mailfold;
    }

    cp = mailfold;
    if ((pp = m_find ("path")) != NULL && *pp) {
	if (*pp != '/') {
	    (void) sprintf (cp, "%s/", mypath);
	    cp += strlen (cp);
	}
	cp = copy (pp, cp);
    }
    else
	cp = copy (path ("./", TFOLDER), cp);
    if (cp[-1] != '/')
	*cp++ = '/';
    (void) strcpy (cp, folder);

    return mailfold;
}
