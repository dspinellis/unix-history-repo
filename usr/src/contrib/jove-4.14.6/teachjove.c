/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>

#ifndef	TEACHJOVE
#    define TEACHJOVE	"/usr/lib/jove/teach-jove"
#endif

#ifndef	W_OK
#   define W_OK	2
#   define F_OK	0
#endif

#ifdef	__STDC__
#define	proto(x)	x
#else
#define	proto(x)	()
#endif

extern char	*getenv proto((const char *));
extern int	access proto((const char *, int));
extern int	system proto((const char *));
extern int	execlp proto((const char *, const char *, ...));

int
main()
{
	char	cmd[256],
		fname[256],
		*home;

	if ((home = getenv("HOME")) == NULL) {
		printf("teachjove: cannot find your home!\n");
		exit(-1);
	}
	(void) sprintf(fname, "%s/teach-jove", home);
	if (access(fname, F_OK) != 0) {
		(void) sprintf(cmd, "cp %s %s; chmod 644 %s", TEACHJOVE, fname, fname);
		system(cmd);
	}
	(void) execlp("jove", "teachjove", fname, (char *) NULL);
	printf("teachjove: cannot execl jove!\n");
	return 1;
}

