/*
 * This software is Copyright (c) 1986 by Rick Adams.
 *
 * Permission is hereby granted to copy, reproduce, redistribute or
 * otherwise use this software as long as: there is no monetary
 * profit gained specifically from the use or reproduction or this
 * software, it is not sold, rented, traded or otherwise marketed, and
 * this copyright notice is included prominently in any copy
 * made.
 *
 * The author make no claims as to the fitness or correctness of
 * this software for any use whatsoever, and it is provided as is. 
 * Any use of this software is at the user's own risk.
 *
 * This routine is compatible with the Unix T/S system call uname,
 * which figures out the name of the local system.
 * However, we do it by reading the file /usr/include/whoami.h.
 * This avoids having to recompile uucp for each site and hence
 * avoids having to distribute the source to uucp to people who
 * have only binary licenses.
 */

#ifdef SCCSID
static char	*SccsId = "@(#)uname.c	2.9	1/20/86";
#endif /* SCCSID */

#include <stdio.h>
#include "../common/conf.h"

#ifdef UNAME
# define DONE
#endif /* UNAME */

#ifdef GHNAME
uname(uptr)
char	*uptr;
{
	gethostname(uptr, 256);
}
# define DONE
#endif

#ifdef	UUNAME
uname(uptr)
char *uptr;
{
	FILE *uucpf;
	register char *p;
	/* uucp name is stored in /etc/uucpname or /local/uucpname */

	if (((uucpf = fopen("/etc/uucpname", "r")) == NULL &&
	     (uucpf = fopen("/local/uucpname", "r")) == NULL) ||
		fgets(uptr, 256, uucpf) == NULL) {
			fprintf(stderr, "no sysname in %s\n", "/etc/uucpname");
			return;
	}
	p = index(uptr, '\n');
	if (p)
		*p = '\0';
	if (uucpf != NULL)
		fclose(uucpf);
}
#define DONE
#endif /* UUNAME */

#ifndef DONE
#define	HDRFILE "/usr/include/whoami.h"

uname(uptr)
char *uptr;
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
		if (sscanf(buf, "#define sysname \"%[^\"]\"", uptr) == 1) {
			fclose(fd);
			return;
		}
	}
}
#endif
