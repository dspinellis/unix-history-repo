/*
 *      @(#)domount.c	1.1 90/01/02 NFS Rev 2 testsuite
 *	combined with 1.1 Lachman ONC Test Suite source
 *
 * domount [-u] [args]
 *
 * NOTE: This program should be suid root to work properly.
 */
main(argc, argv)
	int argc;
	char **argv;
{
	*argv = "/etc/mount";
	if (argc > 1 && strcmp(argv[1], "-u") == 0)
		*++argv = "/etc/umount";
	(void)execv(*argv, argv);
	exit(1);
	/* NOTREACHED */
}
