/*	groups.c	4.2	82/09/11	*/

/*
 * groups
 */

#include <sys/param.h>
#include <grp.h>
#include <pwd.h>

int	grps[NGRPS/(sizeof(int)*8)];
struct	group *gr, *getgrgid();

main(argc, argv)
	int argc;
	char *argv[];
{
	char *sep = "";
	int i;

	getgrp(grps);
	for (i = 0; i < NGRPS; i++)
		if (grps[i/(sizeof(int)*8)] & (1<<(i%(sizeof(int)*8)))) {
			gr = getgrgid(i);
			if (gr == NULL)
				printf("%s%d", sep, i);
			else
				printf("%s%s", sep, gr->gr_name);
			sep = " ";
		}
	printf("\n");
	exit(0);
}
