main(argc, argv)
	char *argv[];
{
	if (argc < 3) {
		printf("rename from to\n");
		exit(1);
	}
	if (rename(argv[1], argv[2]) < 0)
		perror("rename");
}
