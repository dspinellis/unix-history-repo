/* pathalias -- by steve bellovin, as told to peter honeyman */
#ifndef lint
static char	*sccsid = "@(#)local.c	8.1 (down!honey) 86/01/19";
#endif lint

#include <stdio.h>
#include "config.h"

#ifdef	UNAME
#include <sys/utsname.h>

char	*
local()
{
	static struct utsname utsname;

	uname(&utsname);
	return(utsname.nodename);
}

#else !UNAME

char	*
local()
{
	static char lname[64];
	void	gethostname();

	gethostname(lname, sizeof(lname));
	return(lname);
}

#ifndef GETHOSTNAME

static void
gethostname(name, len)
char	*name;
{
	FILE	*whoami, *fopen(), *popen();
	char	*ptr, *index();

	*name = '\0';

	/* try /etc/whoami */
	if ((whoami = fopen("/etc/whoami", "r")) != 0) {
		(void) fgets(name, len, whoami);
		(void) fclose(whoami);
		if ((ptr = index(name, '\n')) != 0)
			*ptr = '\0';
	}
	if (*name)
		return;

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
			return;
	}

	/* ask uucp */
	if ((whoami = popen("uuname -l", "r")) != 0) {
		(void) fgets(name, len, whoami);
		(void) pclose(whoami);
		if ((ptr = index(name, '\n')) != 0)
			*ptr = '\0';
	}
	if (*name)
		return;
	
	/* aw hell, i give up!  is this a real unix? */
	return;
}
#endif GETHOSTNAME
#endif UNAME
