static char *sccsid = "@(#)whoami.c	4.2 (Berkeley) %G%";
#include <pwd.h>
/*
 * whoami
 */
struct	passwd *getpwuid();

main()
{
	register struct passwd *pp;

	pp = getpwuid(geteuid());
	if (pp == 0) {
		printf("Intruder alert.\n");
		exit(1);
	}
	printf("%s\n", pp->pw_name);
	exit(0);
}
