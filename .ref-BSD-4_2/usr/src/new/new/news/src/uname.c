/*
 * This routine is compatible with the Unix T/S system call uname,
 * which figures out the name of the local system.
 * However, we do it by reading the file /usr/include/whoami.h.
 * This avoids having to recompile uucp for each site and hence
 * avoids having to distribute the source to uucp to people who
 * have only binary licenses.
 */

static char *SccsId = "@(#)uname.c	2.4	5/3/83";

#include "params.h"


#ifdef UNAME
# define DONE
#endif

#ifdef GHNAME
uname(uptr)
struct utsname *uptr;
{
	gethostname(uptr->nodename, sizeof (uptr->nodename));
}
# define DONE
#endif

#ifndef DONE
#define	HDRFILE "/usr/include/whoami.h"

uname(uptr)
struct utsname *uptr;
{
	char buf[BUFSIZ];
	FILE *fd;
	
	fd = fopen(HDRFILE, "r");
	if (fd == NULL) {
		fprintf(stderr, "Cannot open %s\n", HDRFILE);
		exit(1);
	}
	
	for (;;) {	/* each line in the file */
		if (fgets(buf, sizeof buf, fd) == NULL) {
			fprintf(stderr, "no sysname in %s\n", HDRFILE);
			fclose(fd);
			exit(2);
		}
		if (sscanf(buf, "#define sysname \"%[^\"]\"", uptr->nodename) == 1) {
			fclose(fd);
			return;
		}
	}
}
#endif
