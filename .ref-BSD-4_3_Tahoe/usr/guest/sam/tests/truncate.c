main(argc, argv)
	char *argv[];
{
	int size = 0;

	if (argc < 2 || argc > 4) {
		printf("usage: truncate file [size]\n");
		exit(1);
	}
	if (argc == 3)
		size = atoi(argv[2]);
	if (size < 0) {
		printf("truncate: size %d?\n", size);
		exit(1);
	}
	if (truncate(argv[1], size) < 0)
		perror(argv[1]);
}
