# include	<stdio.h>
# include	<sys/types.h>
# include	<sys/socket.h>
# include	<sys/un.h>

char	*sock_name = "choke";

main()
{
	int n, p, dr, pl;

	(void) chdir("/tmp");		/* if this fails, doomed anyway */
	if ((dr = fork()) == 0)
		driver();
	sleep(1);
	if ((pl = fork()) == 0)
		player();
	n = 2;
	while (n) {
		p = wait(0);
		if (p == -1) {
			perror("wait");
			break;
		}
		if (p == dr || p == pl)
			n--;
	}
	/* sleep(5*60); */
	player();
	exit(0);
}

driver()
{
	int	s, len, t;
	struct	sockaddr_un	addr;

	s = socket(AF_UNIX, SOCK_STREAM, 0);
	if (s < 0) {
		perror("socket");
		exit(1);
	}
	addr.sun_family = AF_UNIX;
	(void) strncpy(addr.sun_path, sock_name, sizeof(addr.sun_path));
	fprintf(stderr, "driver: socket name '%.*s'\n",
		sizeof(addr.sun_path) - 1, addr.sun_path);
	if (bind(s, &addr, sizeof(addr) - 1) != 0) {
		fputs("bind failed\n", stderr);
		exit(1);
	}
/*	fputs("bind succeeded\n", stderr); */

	if (listen(s, 5) != 0) {
		perror("driver: listen");
		exit(1);
	}
/*	fputs("listen succeeded\n", stderr); */

	len = sizeof(addr) - 1;
	if ((t = accept(s, &addr, &len)) < 0) {
		perror("driver: accept");
		exit(1);
	}
/*	fputs("accept succeeded\n", stderr); */

	exit(0);
}

player()
{
	struct	sockaddr_un	addr;
	int	s;

	s = socket(AF_UNIX, SOCK_STREAM, 0);
	if (s < 0) {
		perror("player: socket");
		exit(1);
	}
	addr.sun_family = AF_UNIX;
	(void) strncpy(addr.sun_path, sock_name, sizeof addr.sun_path);
	if (connect(s, &addr, sizeof addr - 1) != 0) {
		perror("player: connect");
		exit(1);
	}
/*	fputs("connect succeeded\n", stderr); */
	exit(0);
}
