/*	@(#)fstat.c	1.2 90/01/03 NFS Rev 2 Testsuite
 *	1.3 Lachman ONC Test Suite source
 *
 * test statfs for file count
 */
#include <sys/param.h>
#ifdef SVR3
#include <sys/statfs.h>
#else
#include <sys/vfs.h>
#endif
#include <stdio.h>

main(argc, argv)
	int argc;
	char *argv[];
{
	struct statfs fs;
	char *name = ".";

	if (argc > 2) {
		fprintf(stderr, "usage: %s [path]\n", argv[0]);
		exit(1);
	}
	if (argc == 2) {
		name = argv[1];
	}
	fs.f_files = 0;
	fs.f_ffree = 0;
#ifdef SVR3
	if (statfs(name, &fs, sizeof(fs), 0) < 0) {
#else
	if (statfs(name, &fs) < 0) {
#endif
		perror(argv[1]);
		exit(1);
	}
	printf("inodes %d free %d\n", fs.f_files, fs.f_ffree);
	exit(0);
}
