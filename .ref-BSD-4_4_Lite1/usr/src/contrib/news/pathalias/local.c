/* pathalias -- by steve bellovin, as told to peter honeyman */
#ifndef lint
static char	*sccsid = "@(#)local.c	9.3 91/06/11";
#endif /* lint */

#include "config.h"

#include STDIO_H

#ifdef	UNAME
#include <sys/utsname.h>

char	*
local()
{
	static struct utsname utsname;
	extern int uname();

	(void) uname(&utsname);
	return(utsname.nodename);
}

#else /* !UNAME */

char	*
local()
{
	static char lname[64];
	extern int gethostname();

	(void) gethostname(lname, (int) sizeof(lname));
	lname[sizeof(lname)] = 0;
	return(lname);
}

#ifndef GETHOSTNAME

STATIC int
gethostname(name, len)
	char *name;
	int len;
{	FILE *whoami;
	char *ptr;
	extern int pclose();
	extern FILE *fopen(), *popen();

	*name = '\0';

	/* try /etc/whoami */
	if ((whoami = fopen("/etc/whoami", "r")) != 0) {
		(void) fgets(name, len, whoami);
		(void) fclose(whoami);
		if ((ptr = index(name, '\n')) != 0)
			*ptr = '\0';
	}
	if (*name)
		return 0;

	/* try /usr/include/whoami.h */
	if ((whoami = fopen("/usr/include/whoami.h", "r")) != 0) {
		while (!feof(whoami)) {
			char	buf[100];

			if (fgets(buf, 100, whoami) == 0)
				break;
			if (sscanf(buf, "#define sysname \"%[^\"]\"", name))
				break;
		}
		(void) fclose(whoami);
		if (*name)
			return 0;
	}

	/* ask uucp */
	if ((whoami = popen("uuname -l", "r")) != 0) {
		(void) fgets(name, len, whoami);
		(void) pclose(whoami);
		if ((ptr = index(name, '\n')) != 0)
			*ptr = '\0';
	}
	if (*name)
		return 0;
	
	/* aw hell, i give up!  is this really unix? */
	return -1;
}
#endif /* GETHOSTNAME */
#endif /* UNAME */
