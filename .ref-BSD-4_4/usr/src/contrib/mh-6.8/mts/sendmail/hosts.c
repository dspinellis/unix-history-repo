/* hosts.c - find out the official name of a host */
#ifndef	lint
static char ident[] = "@(#)$Id: hosts.c,v 1.7 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

/* LINTLIBRARY */

/* In the SendMail world, we really don't know what the valid hosts are.
   We could poke around in the sendmail.cf file, but that still isn't a
   guarantee.  As a result, we'll say that everything is a valid host, and
   let SendMail worry about it. */


#include "../h/strings.h"
#include <stdio.h>
#include "../zotnet/mts.h"
#include <ctype.h>
#if	defined(BSD42) || defined(SOCKETS)
#include <netdb.h>
#endif	/* BSD42 or SOCKETS */


#define	NOTOK	(-1)


static struct host {
    char   *h_name;
    char  **h_aliases;
    struct host   *h_next;
}                   hosts;

char   *getcpy ();

static int	init_hs();

/*  */

char   *OfficialName (name)
register char   *name;
{
    register char  *p;
    char   *q,
            site[BUFSIZ];
#if	defined(BSD42) || defined(SOCKETS)
    register struct hostent *hp;
#endif	/* BSD42 or SOCKETS */
    static char buffer[BUFSIZ];
    register char **r;
    register struct host   *h;

    for (p = name, q = site; *p; p++, q++)
	*q = isupper (*p) ? tolower (*p) : *p;
    *q = 0;
    q = site;

    if (uleq (LocalName (), site))
	return LocalName ();

#ifdef	BSD41A
    if (rhost (&q) != NOTOK) {
	(void) strcpy (buffer, q);
	free (q);
	return buffer;
    }
#endif	/* BSD41A */
#if	defined(BSD42) || defined(SOCKETS)
#ifndef	BIND
    sethostent (1);
#endif
    if (hp = gethostbyname (q)) {
	(void) strcpy (buffer, hp -> h_name);
	return buffer;
    }
#endif	/* BSD42 or SOCKETS */

    if (hosts.h_name || init_hs ())
	for (h = hosts.h_next; h; h = h -> h_next)
	    if (uleq (h -> h_name, q))
		return h -> h_name;
	    else
		for (r = h -> h_aliases; *r; r++)
		    if (uleq (*r, q))
			return h -> h_name;

    (void) strcpy (buffer, site);
    return buffer;
}

/*  */

/* Use hostable as an exception file for those hosts that aren't on the
   Internet (listed in /etc/hosts).  These are usually PhoneNet and UUCP
   sites. */


#define	NALIASES	50

static int  init_hs () {
    register char  *cp,
                   *dp,
                  **q,
                  **r;
    char    buffer[BUFSIZ],
           *aliases[NALIASES];
    register struct host   *h;
    register FILE  *fp;

    if ((fp = fopen (hostable, "r")) == NULL)
	return 0;

    h = &hosts;
    while (fgets (buffer, sizeof buffer, fp) != NULL) {
	if (cp = index (buffer, '#'))
	    *cp = 0;
	if (cp = index (buffer, '\n'))
	    *cp = 0;
	for (cp = buffer; *cp; cp++)
	    if (isspace (*cp))
		*cp = ' ';
	for (cp = buffer; isspace (*cp); cp++)
	    continue;
	if (*cp == 0)
	    continue;

	q = aliases;
	if (cp = index (dp = cp, ' ')) {
	    *cp = 0;
	    for (cp++; *cp; cp++) {
		while (isspace (*cp))
		    cp++;
		if (*cp == 0)
		    break;
		if (cp = index (*q++ = cp, ' '))
		    *cp = 0;
		else
		    break;
		if (q >= aliases + NALIASES)
		    break;
	    }
	}

	*q = 0;

	h -> h_next = (struct host *) calloc (1, sizeof *h);
	h = h -> h_next;
	h -> h_name = getcpy (dp);
	r = h -> h_aliases =
		(char **) calloc ((unsigned) (q - aliases + 1), sizeof *q);
	for (q = aliases; *q; q++)
	    *r++ = getcpy (*q);
	*r = 0;
    }

    (void) fclose (fp);
    return 1;
}
