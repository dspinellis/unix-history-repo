/* hosts.c - find out the official name of a host */
#ifndef	lint
static char ident[] = "@(#)$Id: hosts.c,v 1.6 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

/* LINTLIBRARY */

#undef	NETWORK
#if	defined(BSD41A) || defined(BSD42) || defined(SOCKETS)
#define	NETWORK
#endif	/* not (defined(BSD41A) || defined(BSD42) || defined(SOCKETS)) */

#include "../h/strings.h"
#include <stdio.h>
#include "../zotnet/mts.h"
#include <ctype.h>
#if	defined(BSD42) || defined(SOCKETS)
#include <netdb.h>
#endif	/* BSD42 or SOCKETS */


#define	NOTOK	(-1)

/*  */

char   *OfficialName (name)
char   *name;
{
    register char  *p;
    char   *q,
	    site[BUFSIZ];
#ifdef	NETWORK
    static char buffer[BUFSIZ];
#if	defined(BSD42) || defined(SOCKETS)
    register struct hostent *hp;
#endif	/* BSD42 or SOCKETS */
#endif	/* NETWORK */

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
    if (hp = gethostbyname (q)) {
	(void) strcpy (buffer, hp -> h_name);
	return buffer;
    }
#endif	/* BSD42 or SOCKETS */

    return NULL;
}
