static char *sccsid = "@(#)swapon.c	4.2 (Berkeley) %G%";
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
		FILE	*fs_file;
		struct	fstab	fs;
		if ((fs_file = fopen(FSTAB, "r")) == NULL){
			perror(FSTAB);
			exit(1);
		}
		while (!feof(fs_file)){
			fscanf(fs_file, FSTABFMT, FSTABARG(&fs));
			if (strcmp(fs.fs_type, "sw"))
				continue;
			fprintf(stderr, "Adding %s as swap device\n",
			    fs.fs_spec);
			if (syscall(VSWAPON, fs.fs_spec) == -1) {
				perror(fs.fs_spec);
				stat = 1;
			}
		}
		fclose(fs_file);
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
