/*	groups.c	4.6	83/06/29	*/

/*
 * groups
 */

#include <sys/param.h>
#include <grp.h>
#include <pwd.h>

int	groups[NGROUPS];

main(argc, argv)
	int argc;
	char *argv[];
{
	int ngroups, i;
	char *sep = "";
	struct group *gr;

	if (argc > 1)
		showgroups(argv[1]);
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

showgroups(user)
	register char *user;
{
	register struct group *gr;
	register char **cp;
	char *sep = "";

	while (gr = getgrent())
		for (cp = gr->gr_mem; cp && *cp; cp++)
			if (strcmp(*cp, user) == 0) {
				printf("%s%s", sep, gr->gr_name);
				sep = " ";
				break;
			}
	printf("\n");
	exit(0);
}
