#include "../h/config.h"
#include "../h/rt.h"
#include <stdio.h>

#ifdef UNAME
#include <sys/utsname.h>
#endif UNAME

iconhost(hostname)
char *hostname;
{
#ifdef WHOHOST
	whohost(hostname);
#endif WHOHOST

#ifdef UNAME
	{
	struct utsname *uptr;
	uname(utsname);
	strcpy(hostname,uptr->nodename);
	}
#endif UNAME

#ifdef GETHOST
	gethostname(hostname, MAXSTRING);
#endif GETHOST

#ifdef HOSTSTR
	strcpy(hostname,HOSTSTR);
#endif HOSTSTR
}

#ifdef WHOHOST
#define	HDRFILE "/usr/include/whoami.h"

whohost(hostname)
char *hostname;
{
	char buf[BUFSIZ];
	FILE *fd;

	fd = fopen(HDRFILE, "r");
	if (fd == NULL) {
		sprintf(buf, "Cannot open %s, no value for &host\n", HDRFILE);
		syserr(buf);
	}
	setbuf(fd,NULL);

	for (;;) {	/* each line in the file */
		if (fgets(buf, sizeof buf, fd) == NULL) {
			sprintf(buf, "No #define for sysname in %s, no value for &host\n", HDRFILE);
			syserr(buf);
		}
		if (sscanf(buf,"#define sysname \"%[^\"]\"", hostname) == 1) {
			fclose(fd);
			return;
		}
	}
}
#endif WHOHOST
