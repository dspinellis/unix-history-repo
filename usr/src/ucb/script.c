#ifndef lint
static char *sccsid = "@(#)script.c	4.3 82/03/26";
#endif
/*
 * script
 */
#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sgtty.h>
#include <time.h>

char	*getenv();
char	*ctime();
char	*shell;
FILE	*fscript;
int	master;
int	slave;
int	child;
char	*fname = "typescript";
int	finish();

struct	sgttyb b;
struct	tchars tc;
struct	ltchars lc;
int	lb;
int	l;
char	*line = "/dev/ptyXX";
int	aflg;

main(argc, argv)
	int argc;
	char *argv[];
{
	int f;

	shell = getenv("SHELL");
	if (shell == 0)
		shell = "/bin/sh";
	argc--, argv++;
	while (argc > 0 && argv[0][0] == '-') {
		switch (argv[0][1]) {

		case 'a':
			aflg++;
			break;

		default:
			fprintf(stderr,
			    "usage: script [ -a ] [ typescript ]\n");
			exit(1);
		}
		argc--, argv++;
	}
	if (argc > 0)
		fname = argv[0];
	if ((fscript = fopen(fname, aflg ? "a" : "w")) == NULL) {
		perror(fname);
		fail();
	}
	getmaster();
	printf("Script started, file is %s\n", fname);
	fixtty();

	(void) signal(SIGCHLD, finish);
	child = fork();
	if (child < 0) {
		perror("fork");
		fail();
	}
	if (child == 0) {
		f = fork();
		if (f < 0) {
			perror("fork");
			fail();
		}
		if (f)
			dooutput();
		else
			doshell();
	}
	doinput();
}

doinput()
{
	char ibuf[BUFSIZ];
	int cc;

	(void) fclose(fscript);
	while ((cc = read(0, ibuf, BUFSIZ)) > 0)
		(void) write(master, ibuf, cc);
	done();
}

#include <wait.h>

finish()
{
	union wait status;

	if (wait3(&status, WNOHANG, 0) != child)
		return;
	done();
}

dooutput()
{
	time_t tvec;
	char obuf[BUFSIZ];
	int cc;

	(void) close(0);
	tvec = time((time_t *)0);
	fprintf(fscript, "Script started on %s", ctime(&tvec));
	for (;;) {
		cc = read(master, obuf, sizeof (obuf));
		if (cc <= 0)
			break;
		(void) write(1, obuf, cc);
		(void) fwrite(obuf, 1, cc, fscript);
	}
	tvec = time((time_t *)0);
	fprintf(fscript,"\nscript done on %s", ctime(&tvec));
	(void) fclose(fscript);
	(void) close(master);
	exit(0);
}

doshell()
{
	int t;

	t = open("/dev/tty", 2);
	if (t >= 0) {
		ioctl(t, TIOCNOTTY, (char *)0);
		(void) close(t);
	}
	getslave();
	(void) close(master);
	(void) fclose(fscript);
	dup2(slave, 0);
	dup2(slave, 1);
	dup2(slave, 2);
	(void) close(slave);
	execl(shell, "sh", "-i", 0);
	perror(shell);
	fail();
}

fixtty()
{
	struct sgttyb sbuf;

	sbuf = b;
	sbuf.sg_flags |= RAW;
	sbuf.sg_flags &= ~ECHO;
	ioctl(0, TIOCSETP, (char *)&sbuf);
}

fail()
{

	(void) kill(0, SIGTERM);
	done();
}

done()
{

	ioctl(0, TIOCSETP, (char *)&b);
	printf("Script done, file is %s\n", fname);
	exit(0);
}

getmaster()
{
	char c;
	struct stat stb;
	int i;

	for (c = 'p'; c <= 's'; c++) {
		line[strlen("/dev/pty")] = c;
		line[strlen("/dev/ptyp")] = '0';
		if (stat(line, &stb) < 0)
			break;
		for (i = 0; i < 16; i++) {
			line[strlen("/dev/ptyp")] = "0123456789abcdef"[i];
			master = open(line, 2);
			if (master >= 0) {
				ioctl(0, TIOCGETP, (char *)&b);
				ioctl(0, TIOCGETC, (char *)&tc);
				ioctl(0, TIOCGETD, (char *)&l);
				ioctl(0, TIOCGLTC, (char *)&lc);
				ioctl(0, TIOCLGET, (char *)&lb);
				return;
			}
		}
	}
	fprintf(stderr, "Out of pty's\n");
	fail();
}

getslave()
{

	line[strlen("/dev/")] = 't';
	slave = open(line, 2);
	if (slave < 0) {
		perror(line);
		fail();
	}
	ioctl(slave, TIOCSETP, (char *)&b);
	ioctl(slave, TIOCSETC, (char *)&tc);
	ioctl(slave, TIOCSLTC, (char *)&lc);
	ioctl(slave, TIOCLSET, (char *)&lb);
	ioctl(slave, TIOCSETD, (char *)&l);
}
