#include "signal.h"
#include "stdio.h"
#include "lrnref"

wrapup(n)
int n;
{
	/* this routine does not use 'system' because it wants
	 interrupts turned off */
	int retval, pid, pidw;

	signal(SIGINT, SIG_IGN);
	chdir("..");
	if ( (pid=fork()) ==0) {
		signal(SIGHUP, SIG_IGN);
		execl("/bin/rm", "rm", "-r", dir, 0);
		execl("/usr/bin/rm", "rm", "-r", dir, 0);
		fprintf(stderr, "Can't find 'rm' command.\n");
		exit(0);
	}
	printf("Bye.\n"); /* not only does this reassure user but 
			it stalls for time while deleting directory */
	fflush(stdout);
	/* printf("Wantd %d got %d val %d\n",pid, pidw, retval); */
	exit(n);
}
