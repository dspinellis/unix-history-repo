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
char *name;
{
	char *s, *d;
	int i;

#ifdef UNAME
	struct utsname utsname;

	uname(&utsname);
	s = &utsname.nodename;
#endif

#ifndef UNAME
	s = MYNAME;
#endif

	d = name;
	while ((*d = *s++) && d < name + 7)
		d++;
	*(name + 7) = '\0';
	return;
}
