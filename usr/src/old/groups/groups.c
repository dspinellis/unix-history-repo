/*	groups.c	4.4	83/02/10	*/

/*
 * groups
 */

#include <sys/param.h>
#include <grp.h>
#include <pwd.h>

short	groups[NGROUPS];
struct	group *gr, *getgrgid();

main(argc, argv)
	int argc;
	char *argv[];
{
	int ngroups;
	char *sep = "";
	int i;

	ngroups = getgroups(NGROUPS, groups);
	for (i = 0; i < ngroups; i++) {
		gr = getgrgid(groups[i]);
		if (gr == NULL)
			printf("%s%d", sep, groups[i]);
		else
			printf("%s%s", sep, gr->gr_name);
		sep = " ";
	}
	printf("\n");
	exit(0);
}
