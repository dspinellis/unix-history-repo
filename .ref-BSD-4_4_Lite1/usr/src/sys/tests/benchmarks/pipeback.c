/*
 * IPC benchmark,
 * read and reply using pipes.
 *
 * Process forks and exchanges messages
 * over a pipe in a request-response fashion.
 */

main(argc, argv)
	char *argv[];
{
	char buf[512];
	int fd[2], fd2[2], msgsize;
	register int i, iter;

	if (argc < 3) {
		printf("usage: %s iterations message-size\n", argv[0]);
		exit(1);
	}
	argc--, argv++;
	iter = atoi(*argv);
	argc--, argv++;
	msgsize = atoi(*argv);
	if (msgsize > sizeof (buf) || msgsize <= 0) {
		printf("%s: Bad message size.\n", *argv);
		exit(2);
	}
	if (pipe(fd) < 0) {
		perror("pipe");
		exit(3);
	}
	if (pipe(fd2) < 0) {
		perror("pipe");
		exit(3);
	}
	if (fork() == 0)
		for (i = 0; i < iter; i++) {
			read(fd[0], buf, msgsize);
			write(fd2[1], buf, msgsize);
		}
	else
		for (i = 0; i < iter; i++) {
			write(fd[1], buf, msgsize);
			read(fd2[0], buf, msgsize);
		}
}
