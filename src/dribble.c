/* Copyright (c) 1979 Regents of the University of California */
#include <signal.h>
#include <errno.h>

#define	BUFSIZ	512

int	inpipe[2], outpipe[2];

int	dribfil;
int	inpid, outpid, cmdpid;
int	badpipe();
char	*nargv[3] = { "/bin/csh", "-i", 0 };

main(argc, argv)
	char *argv[];
{
	int status;

	argc--, argv++;
	if (argc < 0)
		error("Usage: dribble [ dribble.out [ command ] ] ...");
	if (pipe(outpipe) < 0) {
piperr:
		perror("pipe");
		exit(1);
	}
	dribfil = creat(argc == 0 ? "dribble.out" : argv[0], 0644);
	if (dribfil < 0) {
		perror(argc == 0 ? "dribble.out" : argv[0]);
		exit(1);
	}
	if (argc <= 1) {
		argv = nargv - 1;
		argc = 3;
		if (getenv("SHELL"))
			argv[1] = getenv("SHELL");
		else
			argv[1] = "/bin/sh";
	}
	inpid = getpid();
	outpid = fork();
	if (outpid < 0)
		error("No more processes");
	if (outpid == 0) {
		signal(SIGINT, SIG_IGN);
		close(0);
		close(2);
		close(outpipe[1]);
		outloop();
		/* no return */
	}
	signal(SIGTRAP, SIG_IGN);
	if (pipe(inpipe) < 0)
		goto piperr;
	cmdpid = fork();
	if (cmdpid < 0) {
		kill(outpid, SIGKILL);
		error("No more processes");
	}
	if (cmdpid == 0) {
		close(0);
		dup(inpipe[0]);
		close(1);
		dup(outpipe[1]);
		close(inpipe[0]);
		close(inpipe[1]);
		close(outpipe[0]);
		close(outpipe[1]);
		close(2);
		dup(1);
		argv[argc] = 0;
		argv++;
		doexec(argv[0], &argv[0]);
		/* no return */
	}
	signal(SIGPIPE, badpipe);
	close(inpipe[0]);
	close(outpipe[0]);
	input();
}

outloop()
{
	int cnt;
	char linebuf[BUFSIZ];
	int bufcnt;
	extern int errno;

	bufcnt = 0;
	for (;;) {
		errno = 0;
		bufcnt = read(outpipe[0], linebuf, BUFSIZ);
		if (bufcnt == 0)
			break;
		if (bufcnt > 0) {
			linebuf[bufcnt] = 0;
			write(dribfil, linebuf, bufcnt);
			write(1, linebuf, bufcnt);
		}
	}
	kill(inpid, SIGPIPE);
	exit(0);
}

input()
{
	register int i;
	char linebuf[BUFSIZ];
	int bufcnt;

	signal(SIGINT, SIG_IGN);
	for (;;) {
		do {
			bufcnt = read(0, linebuf, BUFSIZ);
			if (bufcnt == 0) {
				linebuf[0] = 4;
				linebuf[1] = '\n';
				linebuf[2] = 0;
				write(inpipe[1], linebuf, 3);
				continue;
			}
			if (bufcnt == 3 && linebuf[0] == 'E' &&
			    linebuf[1] == 'O' && linebuf[2] == 'F') {
				bufcnt = 4;
				linebuf[3] = '\n';
				write(1, "\n", 1);
				write(dribfil, linebuf, bufcnt);
				bufcnt = 0;
				break;
			}
			if (bufcnt > 0) {
				write(dribfil, linebuf, bufcnt);
				write(inpipe[1], linebuf, bufcnt);
			}
		} while (bufcnt > 0);
		if (bufcnt == 0) {
			int status;

			close(inpipe[1]);
			do
				i = wait(&status);
			while (i > 0 && i != cmdpid);
			exit(1);
		}
	}
}

error(cp, a1, a2, a3, a4)
	char *cp;
{

	close(1);
	open("/dev/tty", 1);
	printf(cp, a1, a2, a3, a4);
	putchar('\n');
	exit(1);
}

badpipe()
{

	kill(cmdpid, SIGKILL);
	kill(outpid, SIGKILL);
	exit(1);
}

doexec(cmd, args)
	char *cmd;
	char *args[];
{
	char cmdbuf[120];

	strcpy(cmdbuf, "");
	strcat(cmdbuf, cmd);
	execv(cmdbuf, args);
	strcpy(cmdbuf, "/usr/new/");
	strcat(cmdbuf, cmd);
	execv(cmdbuf, args);
	strcpy(cmdbuf, "/bin/");
	strcat(cmdbuf, cmd);
	execv(cmdbuf, args);
	strcpy(cmdbuf, "/usr/bin/");
	strcat(cmdbuf, cmd);
	execv(cmdbuf, args);
	error("%s: Cannot find", cmd);
}
