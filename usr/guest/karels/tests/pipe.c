main()
{
	int pipefd[2];
	int ret;
	char data[64*1024];
	pipe(pipefd);
	printf("write\n");
	write(pipefd[1], data, 64*1024);
	/* Shouldn't get here, right? WRONG! */
	/* (it should hang because the pipe's not that big, */
	/* and no one is reading it) */
	printf("close write\n");
	close(pipefd[1]);
	printf("read\n");
	while (1) {
		ret = read(pipefd[0], data, 1);
		printf("read %d\n",ret);
		if (ret <= 0)
			break;
	}
	close(pipefd[0]);
	/* SURPRISE! your system just crashed. */
}
