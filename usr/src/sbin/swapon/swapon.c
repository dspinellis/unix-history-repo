/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)swapon.c	5.1 (Berkeley) %G%";
#endif not lint

#include <stdio.h>
#include <fstab.h>
#include <errno.h>

#define	VSWAPON	85

extern int errno;

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
			if (syscall(VSWAPON, fsp->fs_spec) == -1) {
				switch(errno) {
				case EINVAL:
					fprintf(stderr,
						"%s: Device not configured\n",
						fsp->fs_spec);
					stat = 1;
					break;

				case EBUSY:	/* ignore already in use */
					break;

				default:
					perror(fsp->fs_spec);
					stat = 1;
					break;
				}
			} else
				printf("Adding %s as swap device\n",
				    fsp->fs_spec);
		}
		endfsent();
		exit(stat);
	}
	do {
		if (syscall(VSWAPON, *argv++) == -1) {
			stat = 1;
			switch (errno) {
			case EINVAL:
				fprintf(stderr, "%s: Device not configured\n",
						argv[-1]);
				break;

			case EBUSY:
				fprintf(stderr, "%s: Device already in use\n",
						argv[-1]);
				break;

			default:
				perror(argv[-1]);
				break;
			}
		}
		argc--;
	} while (argc > 0);
	exit(stat);
}
