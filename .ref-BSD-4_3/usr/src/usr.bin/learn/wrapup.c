#ifndef lint
static char sccsid[] = "@(#)wrapup.c	4.3	(Berkeley)	5/15/86";
#endif not lint

#include "signal.h"
#include "stdio.h"
#include "lrnref.h"

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
			open("/dev/tty", 1);
			execl("/bin/stty", "stty", "new", 0);
		}
#endif
		execl("/bin/rm", "rm", "-rf", dir, 0);
		execl("/usr/bin/rm", "rm", "-rf", dir, 0);
		perror("bin/rm");
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
