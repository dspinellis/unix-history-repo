#ifndef lint
static char sccsid[] = "@(#)quotaon.c	4.2 (Berkeley, Melbourne) %G%";
#endif
/*
 * Turn quota on/off for a filesystem.
 *
 * NEED TO REFLECT CURRENT STATUS IN MTAB.
 */
#include <sys/param.h>
#include <stdio.h>
#include <fstab.h>

int	vflag;		/* verbose */
int	aflag;		/* all file systems */

char *qfname = "quotas";
char quotafile[MAXPATHLEN + 1];

main(argc, argv)
	int argc;
	char **argv;
{
	register struct fstab *fs;
	char *whoami, *rindex();
	int offmode = 0, errs = 0;

	whoami = rindex(*argv, '/') + 1;
	if (whoami == (char *)1)
		whoami = *argv;
	if (strcmp(whoami, "quotaoff") == 0)
		offmode++;
	else if (strcmp(whoami, "quotaon") != 0) {
		fprintf(stderr, "Name must be quotaon or quotaoff not %s\n",
			whoami);
		exit(1);
	}
again:
	argc--, argv++;
	if (argc > 0 && strcmp(*argv, "-v") == 0) {
		vflag++;
		goto again;
	}
	if (argc > 0 && strcmp(*argv, "-a") == 0) {
		aflag++;
		goto again;
	}
	if (argc <= 0 && !aflag) {
		fprintf(stderr, "Usage:\n\t%s [-v] -a\n\t%s [-v] filesys ...\n",
			whoami, whoami);
		exit(1);
	}
	setfsent();
	while ((fs = getfsent()) != NULL) {
		if (fs->fs_type == 0)
			continue;
		if (aflag && strcmp(fs->fs_type, "rq") != 0)
			continue;
		if (!aflag && !oneof(fs->fs_file, argv, argc))
			continue;
		if (offmode) {
			if (setquota(fs->fs_spec, NULL) < 0) {
				fprintf(stderr, "setquota: ");
				perror(fs->fs_spec);
				errs++;
				continue;
			}
			if (vflag)
				printf("%s: quotas turned off\n", fs->fs_file);
			continue;
		}
		(void) sprintf(quotafile, "%s/%s", fs->fs_file, qfname);
		if (setquota(fs->fs_spec, quotafile) < 0) {
			fprintf(stderr, "setquota: ");
			perror(fs->fs_spec);
			errs++;
			continue;
		}
		if (vflag)
			printf("%s: quotas turned on\n", fs->fs_file);
	}
	endfsent();
	exit(errs);
}

oneof(target, list, n)
	char *target, *list[];
	register int n;
{
	register int i;

	for (i = 0; i < n; i++)
		if (strcmp(target, list[i]) == 0)
			return (1);
	return (0);
}
