/* hosts.c - find out the official name of a host */

/* LINTLIBRARY */

#undef	NETWORK
#if	defined(BSD41A) || defined(BSD42)
#define	NETWORK
#endif	not (defined(BSD41A) || defined(BSD42))

#include "../h/strings.h"
#include <stdio.h>
#include "../zotnet/mts.h"
#include <ctype.h>
#ifdef	BSD42
#include <netdb.h>
#endif	BSD42


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
#ifdef	BSD42
    register struct hostent *hp;
#endif	BSD42
#endif	NETWORK

    for (p = name, q = site; *p; p++, q++)
	*q = isupper (*p) ? tolower (*p) : *p;
    *q = NULL;
    q = site;

    if (uleq (LocalName (), site))
	return LocalName ();

#ifdef	BSD41A
    if (rhost (&q) != NOTOK) {
	(void) strcpy (buffer, q);
	free (q);
	return buffer;
    }
#endif	BSD41A
#ifdef	BSD42
    if (hp = gethostbyname (q)) {
	(void) strcpy (buffer, hp -> h_name);
	return buffer;
    }
#endif	BSD42

    return NULL;
}
