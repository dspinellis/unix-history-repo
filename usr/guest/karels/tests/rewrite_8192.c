#define	BUFSIZ 8192

main(argc, argv)
	int argc;
	char *argv[];
{
	char buf[BUFSIZ];
	int i, j;

	if (argc < 2) {
		printf("Usage: rewrite_8192 file\n");
		exit(1);
	}
	j = open(argv[1], 2);
	if (j < 0) {
		perror(argv[1]);
		exit(2);
	}
	for (i = 0; i < 1024; i++)
		write(j, buf, BUFSIZ);
}
