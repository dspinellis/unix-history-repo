static	char *sccsid = "@(#)badsect.c	4.2 (Berkeley) 81/05/11";

/*
 * badsect
 *
 * Badsect takes a list of file-system relative sector numbers
 * and makes files containing the blocks of which these sectors are a part.
 * It can be used to contain sectors which have problems if these sectors
 * are not part of the bad file for the pack (see bad144).  For instance,
 * this program can be used if the driver for the file system in question
 * does not support bad block forwarding.
 */
#include <sys/param.h>

main(argc, argv)
	int argc;
	char **argv;
{
	char nambuf[32];
	int errs = 0;

	--argc, argv++;
	while (argc > 0) {
		if (mknod(*argv, 0, atoi(*argv) / CLSIZE))
			perror("mknod"), errs++;
		argc--, argv++;
	}
	exit(errs);
}
