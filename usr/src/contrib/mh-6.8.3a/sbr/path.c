/* path.c - return a pathname */
#ifndef	lint
static char ident[] = "@(#)$Id: path.c,v 1.6 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#include <stdio.h>

#define	CWD	"./"
#define	NCWD	(sizeof CWD - 1)
#define	DOT	"."
#define	DOTDOT	".."
#define	PWD	"../"
#define	NPWD	(sizeof PWD - 1)


static char *pwds;

static char   *expath ();
static	compath();


char   *path (name, flag)
register char   *name;
register int     flag;
{
    register char  *cp,
                   *ep;

    if ((cp = expath (name, flag))
	    && (ep = cp + strlen (cp) - 1) > cp
	    && *ep == '/')
	*ep = 0;

    return cp;
}

/*  */

static  char   *expath (name, flag)
register char   *name;
register int     flag;
{
    register char  *cp,
                   *ep;
    char    buffer[BUFSIZ];

    if (flag == TSUBCWF) {
	(void) sprintf (buffer, "%s/%s", m_getfolder (), name);
	name = m_mailpath (buffer);
	compath (name);
	(void) sprintf (buffer, "%s/", m_maildir (""));
	if (ssequal (buffer, name)) {
	    cp = name;
	    name = getcpy (name + strlen (buffer));
	    free (cp);
	}
	flag = TFOLDER;
    }

    if (*name == '/'
	    || (flag == TFOLDER
		&& (strncmp (name, CWD, NCWD)
		    && strcmp (name, DOT)
		    && strcmp (name, DOTDOT)
		    && strncmp (name, PWD, NPWD))))
	return getcpy (name);

    if (pwds == NULL)
	pwds = pwd ();

    if (strcmp (name, DOT) == 0 || strcmp (name, CWD) == 0)
	return getcpy (pwds);

    ep = pwds + strlen (pwds);
    if ((cp = rindex (pwds, '/')) == NULL)
	cp = ep;
    else
	if (cp == pwds)
	    cp++;

    if (strncmp (name, CWD, NCWD) == 0)
	name += NCWD;

    if (strcmp (name, DOTDOT) == 0 || strcmp (name, PWD) == 0) {
	(void) sprintf (buffer, "%.*s", cp - pwds, pwds);
	return getcpy (buffer);
    }

    if (strncmp (name, PWD, NPWD) == 0)
	name += NPWD;
    else
	cp = ep;

    (void) sprintf (buffer, "%.*s/%s", cp - pwds, pwds, name);
    return getcpy (buffer);
}

/*  */

static  compath (f)
register char  *f;
{
    register char  *cp,
                   *dp;

    if (*f != '/')
	return;

    for (cp = f; *cp;)
	if (*cp == '/') {
	    switch (*++cp) {
		case 0: 
		    if (--cp > f)
			*cp = 0;
		    break;

		case '/': 
		    for (dp = cp; *dp == '/'; dp++)
			continue;
		    (void) strcpy (cp--, dp);
		    continue;

		case '.': 
		    if (strcmp (cp, DOT) == 0) {
			if (cp > f + 1)
			    cp--;
			*cp = 0;
			break;
		    }
		    if (strcmp (cp, DOTDOT) == 0) {
			for (cp -= 2; cp > f; cp--)
			    if (*cp == '/')
				break;
			if (cp <= f)
			    cp = f + 1;
			*cp = 0;
			break;
		    }
		    if (strncmp (cp, PWD, NPWD) == 0) {
			for (dp = cp - 2; dp > f; dp--)
			    if (*dp == '/')
				break;
			if (dp <= f)
			    dp = f;
			(void) strcpy (dp, cp + NPWD - 1);
			cp = dp;
			continue;
		    }
		    if (strncmp (cp, CWD, NCWD) == 0) {
			(void) strcpy (cp - 1, cp + NCWD - 1);
			cp--;
			continue;
		    }
		    continue;

		default: 
		    cp++;
		    continue;
	    }
	    break;
	}
	else
	    cp++;
}
