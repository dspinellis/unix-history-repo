/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

#include <sys/types.h>
#include <sys/file.h>

#ifndef TEACHJOVE
#    define TEACHJOVE	"/usr/lib/jove/teach-jove"
#endif

#ifndef W_OK
#    define W_OK 2
#endif

extern char	*getenv();

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
		(void) sprintf(cmd, "cp %s %s", TEACHJOVE, fname);
		system(cmd);
	}
	(void) execlp("jove", "teachjove", fname, (char *) 0);
	printf("teachjove: cannot execl jove!\n");
}

