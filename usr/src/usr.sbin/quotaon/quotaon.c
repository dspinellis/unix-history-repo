#ifndef lint
static char sccsid[] = "@(#)quotaon.c	4.1 (Melbourne) %G%";
#endif
/*
 * Turn quota on/off for a filesystem
 */
#include <sys/param.h>

#include <stdio.h>
#include <fstab.h>

int	vflag;		/* verbose */
int	aflag;		/* all file systems */

main(argc, argv)
	char **argv;
{
	register i, on;
	register struct fstab *fs;

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
	if (argc <= 0 || strcmp(*argv, "on") && strcmp(*argv, "off")) {
		fprintf(stderr,
			"usage: setquota [-v] [-a] on | off [filesys...]\n");
		exit(1);
	}
	on = strcmp(*argv, "on") == 0;
	argc--, argv++;
	setfsent();
	while ((fs = getfsent()) != NULL) {
		if (fs->fs_quotafile == 0 || *fs->fs_quotafile == '\0')
			continue;
		if (fs->fs_type == 0 || strcmp(fs->fs_type, "rw"))
			continue;
		if (!aflag && !oneof(fs->fs_file, argv, argc))
			continue;
		if (vflag)
			printf("%s: quotas turned %s\n", fs->fs_file,
				on ? "on" : "off");
		if (on) {
			char quotafile[MAXPATHLEN + 1];

			(void) sprintf(quotafile, "%s/%s", fs->fs_file,
			   fs->fs_quotafile);
			i = setquota(fs->fs_spec, quotafile);
		} else
			i = setquota(fs->fs_spec, NULL);
		if (i < 0) {
			fprintf(stderr, "setquota: ");
			perror(fs->fs_spec);
		}
	}
	endfsent();
	exit(i < 0);
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
