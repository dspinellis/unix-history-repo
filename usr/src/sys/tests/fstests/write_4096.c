#define	BUFSIZ 4096

main(argc, argv)
	int argc;
	char *argv[];
{
	char buf[BUFSIZ];
	int i, j;

	if (argc < 2) {
		printf("Usage: write_4096 file\n");
		exit(1);
	}
	j = creat(argv[1], 0666);
	if (j < 0) {
		perror(argv[1]);
		exit(2);
	}
	for (i = 0; i < 2048; i++)
		write(j, buf, BUFSIZ);
}
