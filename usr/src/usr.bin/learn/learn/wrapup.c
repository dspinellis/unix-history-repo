#ifndef lint
static char sccsid[] = "@(#)wrapup.c	4.4	(Berkeley)	%G%";
#endif not lint

#include <sys/signal.h>
#include <stdio.h>
#include "lrnref.h"
#include "pathnames.h"

extern char learnrc[];

wrapup(n)
int n;
{
	FILE *fp;
/* this routine does not use 'system' because it wants interrupts turned off */

	signal(SIGINT, SIG_IGN);
	chdir("..");
	if (fork() == 0) {
		signal(SIGHUP, SIG_IGN);
#if BSD4_2
		if (fork() == 0) {
			close(1);
			open(_PATH_TTY, 1);
			execl(_PATH_STTY, "stty", "new", 0);
		}
#endif
		execl(_PATH_RM, "rm", "-rf", dir, 0);
		perror(_PATH_RM);
		fprintf(stderr, "Wrapup:  can't find 'rm' command.\n");
		exit(0);
	}
	if (n == -1)
		unlink(learnrc);
	else if (!n && todo) {
		if ((fp=fopen(learnrc, "w")) == NULL)
			exit(0);
		fprintf(fp, "%s %s %d\n", sname, todo, speed);
		fclose(fp);
	}
	printf("Bye.\n");	/* not only does this reassure user but it
				stalls for time while deleting directory */
	fflush(stdout);
	wait(0);
	exit(n);
}
