static char *sccsid = "@(#)swapon.c	4.4 (Berkeley) 10/16/80";
#include <stdio.h>
#include <fstab.h>

#define	VSWAPON	85

main(argc, argv)
	int argc;
	char *argv[];
{
	int stat = 0;

	--argc, argv++;
	if (argc == 0) {
		fprintf(stderr, "usage: swapon name...\n");
		exit(1);
	}
	if (argc == 1 && !strcmp(*argv, "-a")) {
		struct	fstab	*fsp;
		if (setfsent() == 0)
			perror(FSTAB), exit(1);
		while ( (fsp = getfsent()) != 0){
			if (strcmp(fsp->fs_type, FSTAB_SW) != 0)
				continue;
			printf("Adding %s as swap device\n",
			    fsp->fs_spec);
			if (syscall(VSWAPON, fsp->fs_spec) == -1) {
				extern errno;
				extern char *sys_errlist[];
				printf("%s: %s\n",
				    sys_errlist[errno]);
				stat = 1;
			}
		}
		endfsent();
		exit(stat);
	}
	do {
		if (syscall(VSWAPON, *argv++) == -1) {
			stat = 1;
			perror(argv[-1]);
		}
		argc--;
	} while (argc > 0);
	exit(stat);
}
