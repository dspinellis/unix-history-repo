/*
 * %G% hack (aps) %W%
 *
 * Defines for hack.
 *	aps.
 */

#include  <stdio.h>
#include  <utmp.h>
#include  <pwd.h>
#include  <X/Xlib.h>

#define	NOSTR		((char *) 0)	/* Null string pointer */
#define	LINESIZE	BUFSIZ		/* max from stdio.h */
#define	YES		1		/* status return */
#define	NO		0		/* status return */
#define	UTMPNAMSIZ	8	/* utmp, who, login, ls, ... should be fixed */
#define	NAMLEN		16	/* utmp unwisely uses 8 as length of name */

#define	DELAY(n)	{ register int N = (n); while (--N > 0); }
#define	EVER	;;
#define	equal(a, b)	(strcmp(a,b)==0)/* A nice function to string compare */

