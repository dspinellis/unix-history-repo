/*	initgroups.c	4.2	83/02/15	*/

/*
 * initgroups
 */
#include <stdio.h>
#include <sys/param.h>
#include <grp.h>

struct group *getgrent();

initgroups(uname, agroup)
	char *uname;
	int agroup;
{
	int groups[NGROUPS], ngroups = 0;
	register struct group *grp;
	register int i;

	if (agroup >= 0)
		groups[ngroups++] = agroup;
	setgrent();
	while (grp = getgrent())
		for (i = 0; grp->gr_mem[i]; i++)
			if (!strcmp(grp->gr_mem[i], uname)) {
				groups[ngroups++] = grp->gr_gid;
				if (ngroups == NGROUPS) {
fprintf(stderr, "inigrp: %s is in too many groups\n", uname);
					goto toomany;
				}
			}
toomany:
	if (setgroups(ngroups, groups) < 0) {
		perror("setgrp");
		return (1);
	}
	return (0);
}
