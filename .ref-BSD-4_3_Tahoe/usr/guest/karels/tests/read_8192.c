#define	BUFSIZ 8192

main(argc, argv)
	int argc;
	char *argv[];
{
	char buf[BUFSIZ];
	int i, j;

	if (argc < 2) {
		printf("Usage: read_8192 file\n");
		exit(1);
	}
	j = open(argv[1], 0);
	if (j < 0) {
		perror(argv[1]);
		exit(2);
	}
	for (i = 0; i < 1024; i++)
		read(j, buf, BUFSIZ);
}
