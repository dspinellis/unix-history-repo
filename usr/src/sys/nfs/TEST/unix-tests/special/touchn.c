/*	@(#)touchn.c	1.1 88/10/12 NFS Rev 2 Testsuite	*/
/*
 *  touch n files
 */
main(argc,argv)
char **argv;
{
	int n;
	char buf[1024];

	if (argc != 2) {
		printf("usage: %s count\n", argv[0]);
		exit(1);
	}
	n = atoi(argv[1]);
	for (; n; n--) {
		sprintf(buf, "name%d", n);
		close(creat(buf, 0666));
	}
	exit(0);
}
