/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>

#ifndef TEACHJOVE
#    define TEACHJOVE	"/usr/lib/jove/teach-jove"
#endif

#ifndef W_OK
#   define W_OK	2
#   define F_OK	0
#endif

extern char	*getenv();

int
main()
{
	char	cmd[256],
		fname[256],
		*home;

	if ((home = getenv("HOME")) == 0) {
		printf("teachjove: cannot find your home!\n");
		exit(-1);
	}
	(void) sprintf(fname, "%s/teach-jove", home);
	if (access(fname, F_OK) != 0) {
		(void) sprintf(cmd, "cp %s %s; chmod 644 %s", TEACHJOVE, fname, fname);
		system(cmd);
	}
	(void) execlp("jove", "teachjove", fname, (char *) 0);
	printf("teachjove: cannot execl jove!\n");
	return 1;
}

