static char sccsid[] = "@(#)uucpname.c	4.1	(Berkeley)	9/11/82";

#include "uucp.h"

#ifdef UNAME
#include <sys/utsname.h>
#endif
/*******
 *	uucpname(name)		get the uucp name
 *
 *	return code - none
 */

uucpname(name)
register char *name;
{
#ifdef	GETHOST
	static char hostname[32];
#endif
	register char *s, *d;

#ifdef UNAME
	struct utsname utsname;

	uname(&utsname);		/* UNIX 3.0 and later BTL Versions */
	s = &utsname.nodename;
#else
#ifdef	GETHOST
	gethostname(hostname, sizeof (hostname)); /* UCB 4.1A ans later */
	s = hostname;
#else
	s = MYNAME; /* V7, V32 and earlier */
#endif
#endif

	d = name;
	while ((*d = *s++) && d < name + 7)
		d++;
	*(name + 7) = '\0';
	return;
}
