#define BUFSIZ 4096

main(argc, argv)
	int argc;
	char *argv[];
{
	int buf[BUFSIZ / sizeof(int)];
	int i, j, k;

	if (argc != 2) {
		printf("Usage: readchk file\n");
		exit(1);
	}
	j = creat(argv[1], 0666);
	if (j < 0) {
		perror(argv[1]);
		exit(2);
	}
	for (i = 0; i < 2048; i++) {
		for (k = 0; k < BUFSIZ / sizeof(int); k++)
			buf[k] = i + k;
		if (write(j, buf, BUFSIZ) != BUFSIZ) {
			perror("write");
			exit(3);
		}
	}
	close(j);
	j = open(argv[1], 0);
	if (j < 0) {
		perror(argv[1]);
		exit(4);
	}
	for (i = 0; i < 2048; i++) {
		if (read(j, buf, BUFSIZ) != BUFSIZ) {
			perror("read");
			exit(5);
		}
		for (k = 0; k < BUFSIZ / sizeof(int); k++)
			if (buf[k] != i + k)
				printf("bad data at %d\n",
					tell(j) - BUFSIZ + 4 * k);
	}
}
