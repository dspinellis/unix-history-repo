/*	cmds.c	4.6	81/11/20	*/
#include "tip.h"
/*
 * tip
 *
 * miscellaneous commands
 */

int	quant[] = { 60, 60, 24 };

char	null = '\0';
char	*sep[] = { "second", "minute", "hour" };
static char *argv[10];		/* argument vector for take and put */

int	timeout();		/* timeout function called on alarm */
int	stopsnd();		/* SIGINT handler during file transfers */
int	intprompt();		/* used in handling SIG_INT during prompt */
int	intcopy();		/* interrupt routine for file transfers */

/*
 * FTP - remote ==> local
 *  get a file from the remote host
 */
getfl(c)
	char c;
{
	char buf[256];
	
	putchar(c);
	/*
	 * get the UNIX receiving file's name
	 */
	if (prompt("Local file name? ", copyname))
		return;
	if ((sfd = creat(copyname, 0666)) < 0) {
		printf("\r\n%s: cannot creat\r\n", copyname);
		return;
	}
	
	/*
	 * collect parameters
	 */
	if (prompt("List command for remote system? ", buf)) {
		unlink(copyname);
		return;
	}
	transfer(buf, sfd, value(EOFREAD));
}

/*
 * Cu-like take command
 */
cu_take(cc)
	char cc;
{
	int fd, argc;
	char line[BUFSIZ];

	if (prompt("[take] ", copyname))
		return;
	if ((argc = args(copyname, argv)) < 1 || argc > 2) {
		printf("usage: <take> from [to]\r\n");
		return;
	}
	if (argc == 1)
		argv[1] = argv[0];
	if ((fd = creat(argv[1], 0666)) < 0) {
		printf("\r\n%s: cannot create\r\n", argv[1]);
		return;
	}
	sprintf(line, "cat '%s';echo \01", argv[0]);
	transfer(line, fd, "\01");
}

/*
 * Bulk transfer routine --
 *  used by getfl(), cu_take(), and pipefile()
 */
transfer(buf, fd, eofchars)
	char *buf, *eofchars;
{
	register int ct;
	char c, buffer[BUFSIZ];
	register char *p = buffer;
	register int cnt, eof;
	time_t start;

	write(FD, buf, size(buf));
	quit = 0;
	signal(SIGINT, intcopy);
	kill(pid, SIGIOT);
	read(repdes[0], (char *)&ccc, 1);  /* Wait until read process stops */
	
	/*
	 * finish command
	 */
	write(FD, "\r", 1);
	do
		read(FD, &c, 1); 
	while ((c&0177) != '\n');
	ioctl(0, TIOCSETC, &defchars);
	
	start = time(0);
	for (ct = 0; !quit;) {
		eof = read(FD, &c, 1) <= 0;
		c &= 0177;
		if (quit)
			continue;
		if (eof || any(c, eofchars))
			break;
		if (c == 0)
			continue;	/* ignore nulls */
		if (c == '\r')
			continue;
		*p++ = c;

		if (c == '\n' && boolean(value(VERBOSE)))
			printf("\r%d", ++ct);
		if ((cnt = (p-buffer)) == number(value(FRAMESIZE))) {
			if (write(fd, buffer, cnt) != cnt) {
				printf("\r\nwrite error\r\n");
				quit = 1;
			}
			p = buffer;
		}
	}
	if (cnt = (p-buffer))
		if (write(fd, buffer, cnt) != cnt)
			printf("\r\nwrite error\r\n");

	if (boolean(value(VERBOSE)))
		prtime(" lines transferred in ", time(0)-start);
	ioctl(0, TIOCSETC, &tchars);
	write(fildes[1], (char *)&ccc, 1);
	signal(SIGINT, SIG_DFL);
	close(fd);
}

/*
 * FTP - remote ==> local process
 *   send remote input to local process via pipe
 */
pipefile()
{
	int cpid, pdes[2];
	char buf[256];
	int status, p;
	extern int errno;

	if (prompt("Local command? ", buf))
		return;

	if (pipe(pdes)) {
		printf("can't establish pipe\r\n");
		return;
	}

	if ((cpid = fork()) < 0) {
		printf("can't fork!\r\n");
		return;
	} else if (cpid) {
		if (prompt("List command for remote system? ", buf)) {
			close(pdes[0]), close(pdes[1]);
			kill (cpid, SIGKILL);
		} else {
			close(pdes[0]);
			signal(SIGPIPE, intcopy);
			transfer(buf, pdes[1], value(EOFREAD));
			signal(SIGPIPE, SIG_DFL);
			while ((p = wait(&status)) > 0 && p != cpid)
				;
		}
	} else {
		register int f;

		dup2(pdes[0], 0);
		close(pdes[0]);
		for (f = 3; f < 20; f++)
			close(f);
		execute(buf);
		printf("can't execl!\r\n");
		exit(0);
	}
}

/*
 * Interrupt service routine for FTP
 */
stopsnd()
{
	stop = 1;
	signal(SIGINT, SIG_IGN);
}

/*
 * FTP - local ==> remote
 *  send local file to remote host
 *  terminate transmission with pseudo EOF sequence
 */
sendfile(cc)
	char cc;
{
	FILE *fd;

	putchar(cc);
	/*
	 * get file name
	 */
	if (prompt("Local file name? ", fname))
		return;

	/*
	 * look up file
	 */
	if ((fd = fopen(fname, "r")) == NULL) {
		printf("%s: cannot open\r\n", fname);
		return;
	}
	transmit(fd, value(EOFWRITE), NULL);
	if (!boolean(value(ECHOCHECK))) {
		struct sgttyb buf;

		ioctl(FD, TIOCGETP, &buf);	/* this does a */
		ioctl(FD, TIOCSETP, &buf);	/*   wflushtty */
	}
}

/*
 * Bulk transfer routine to remote host --
 *   used by sendfile() and cu_put()
 */
transmit(fd, eofchars, command)
	FILE *fd;
	char *eofchars, *command;
{
	char *pc, lastc;
	int c, ccount, lcount;
	time_t start_t, stop_t;

	kill(pid, SIGIOT);	/* put TIPOUT into a wait state */
	signal(SIGINT, stopsnd);
	stop = 0;
	ioctl(0, TIOCSETC, &defchars);
	read(repdes[0], (char *)&ccc, 1);
	if (command != NULL) {
		for (pc = command; *pc; pc++)
			send(*pc);
		if (boolean(value(ECHOCHECK)))
			read(FD, (char *)&c, 1);	/* trailing \n */
		else {
			struct sgttyb buf;

			ioctl(FD, TIOCGETP, &buf);	/* this does a */
			ioctl(FD, TIOCSETP, &buf);	/*   wflushtty */
			sleep(5); /* wait for remote stty to take effect */
		}
	}
	lcount = 0;
	lastc = '\0';
	start_t = time(0);
	while(1) {
		ccount = 0;
		do {
			c = getc(fd);
			if (stop)
				goto out;
			if (c == EOF)
				goto out;
			if (c == 0177)
				continue;
			lastc = c;
			if (c < 040) {
				if (c == '\n')
					c = '\r';
				else if (c == '\t') {
					if (boolean(value(TABEXPAND))) {
						send(' ');
						while((++ccount % 8) != 0)
							send(' ');
						continue;
					}
				} else
					continue;
			}
			send(c);
		} while (c != '\r');
		if (boolean(value(VERBOSE)))
			printf("\r%d", ++lcount);
		if (boolean(value(ECHOCHECK))) {
			alarm(10);
			timedout = 0;
			do {	/* wait for prompt */
				read(FD, (char *)&c, 1);
				if (timedout || stop) {
					if (timedout)
						printf("\r\ntimed out at eol\r\n");
					alarm(0);
					goto out;
				}
			} while ((c&0177) != character(value(PROMPT)));
			alarm(0);
		}
	}
out:
	if (lastc != '\n')
		send('\r');
	for (pc = eofchars; *pc; pc++)
		send(*pc);
	stop_t = time(0);
	fclose(fd);
	signal(SIGINT, SIG_DFL);
	if (boolean(value(VERBOSE)))
		prtime(" lines transferred in ", stop_t-start_t);
	write(fildes[1], (char *)&ccc, 1);
	ioctl(0, TIOCSETC, &tchars);
}

/*
 * Cu-like put command
 */
cu_put(cc)
	char cc;
{
	FILE *fd;
	char line[BUFSIZ];
	int argc;

	if (prompt("[put] ", copyname))
		return;
	if ((argc = args(copyname, argv)) < 1 || argc > 2) {
		printf("usage: <put> from [to]\r\n");
		return;
	}
	if (argc == 1)
		argv[1] = argv[0];
	if ((fd = fopen(argv[0], "r")) == NULL) {
		printf("%s: cannot open\r\n", argv[0]);
		return;
	}
	if (boolean(value(ECHOCHECK)))
		sprintf(line, "cat>'%s'\r", argv[1]);
	else
		sprintf(line, "stty -echo;cat>'%s';stty echo\r", argv[1]);
	transmit(fd, "\04", line);
}

/*
 * FTP - send single character
 *  wait for echo & handle timeout
 */
send(c)
	char c;
{
	int cc;
	int retry = 0;

	cc = c;
	write(FD, (char *)&cc, 1);
	if (!boolean(value(ECHOCHECK)))
		return;
tryagain:
	timedout = 0;
	alarm(10);
	read(FD, (char *)&cc, 1);
	alarm(0);
	if (timedout) {
		printf("\r\ntimeout error (%s)\r\n", ctrl(c));
		if (retry++ > 3)
			return;
		write(FD, &null, 1); /* poke it */
		goto tryagain;
	}
}

timeout()
{
	signal(SIGALRM, timeout);
	timedout = 1;
}

#ifdef CONNECT
/*
 * Fork a program with:
 *  0 <-> local tty in
 *  1 <-> local tty out
 *  2 <-> local tty out
 *  3 <-> remote tty in
 *  4 <-> remote tty out
 */
consh(c)
{
	char buf[256];
	int cpid, status, p;
	time_t start;

	putchar(c);
	if (prompt("Local command? ", buf))
		return;
	kill(pid, SIGIOT);	/* put TIPOUT into a wait state */
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	ioctl(0, TIOCSETC, &defchars);
	read(repdes[0], (char *)&ccc, 1);
	/*
	 * Set up file descriptors in the child and
	 *  let it go...
	 */
	if ((cpid = fork()) < 0)
		printf("can't fork!\r\n");
	else if (cpid) {
		start = time(0);
		while ((p = wait(&status)) > 0 && p != cpid)
			;
	} else {
		register int i;

		dup2(FD, 3);
		dup2(3, 4);
		for (i = 5; i < 20; i++)
			close(i);
		signal(SIGINT, SIG_DFL);
		signal(SIGQUIT, SIG_DFL);
		execute(buf);
		printf("can't find `%s'\r\n", buf);
		exit(0);
	}
	if (boolean(value(VERBOSE)))
		prtime("away for ", time(0)-start);
	write(fildes[1], (char *)&ccc, 1);
	ioctl(0, TIOCSETC, &tchars);
	signal(SIGINT, SIG_DFL);
	signal(SIGQUIT, SIG_DFL);
}
#endif

/*
 * Escape to local shell
 */
shell()
{
	int shpid, status;
	extern char **environ;
	char *cp;

	printf("[sh]\r\n");
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	unraw();
	if (shpid = fork()) {
		while (shpid != wait(&status));
		raw();
		printf("\r\n!\r\n");
		signal(SIGINT, SIG_DFL);
		signal(SIGQUIT, SIG_DFL);
		return;
	} else {
		signal(SIGQUIT, SIG_DFL);
		signal(SIGINT, SIG_DFL);
		if ((cp = rindex(value(SHELL), '/')) == NULL)
			cp = value(SHELL);
		else
			cp++;
		execl(value(SHELL), cp, 0);
		printf("\r\ncan't execl!\r\n");
		exit(1);
	}
}

/*
 * TIPIN portion of scripting
 *   initiate the conversation with TIPOUT
 */
setscript()
{
	char c;
	/*
	 * enable TIPOUT side for dialogue
	 */
	kill(pid, SIGEMT);
	if (boolean(value(SCRIPT)))
		write(fildes[1], value(RECORD), size(value(RECORD)));
	write(fildes[1], "\n", 1);
	/*
	 * wait for TIPOUT to finish
	 */
	read(repdes[0], &c, 1);
	if (c == 'n')
		printf("can't create %s\r\n", value(RECORD));
}

/*
 * Change current working directory of
 *   local portion of tip
 */
chdirectory()
{
	char	dirname[80];
	register char *cp = dirname;

	if (prompt("[cd] ", dirname))
		if (stoprompt)
			return;
		else
			cp = value(HOME);
	if (chdir(cp) < 0)
		printf("%s: bad directory\r\n", cp);
	printf("!\r\n");
}

finish()
{
	kill(pid, SIGTERM);
	disconnect();
	printf("\r\n[EOT]\r\n");
	delock(uucplock);
	unraw();
	exit(0);
}

intcopy()
{
	raw();
	quit = 1;
}

execute(s)
	char *s;
{
	register char *cp;

	if ((cp = rindex(value(SHELL), '/')) == NULL)
		cp = value(SHELL);
	else
		cp++;
	execl(value(SHELL), cp, "-c", s, 0);
}

args(buf, a)
	char *buf, *a[];
{
	register char *p = buf, *start;
	register char **parg = a;
	register int n = 0;

	do {
		while (*p && (*p == ' ' || *p == '\t'))
			p++;
		start = p;
		if (*p)
			*parg = p;
		while (*p && (*p != ' ' && *p != '\t'))
			p++;
		if (p != start)
			parg++, n++;
		if (*p)
			*p++ = '\0';
	} while (*p);

	return(n);
}

prtime(s, a)
	char *s;
	time_t a;
{
	register i;
	int nums[3];

	for (i = 0; i < 3; i++) {
		nums[i] = (int)(a % quant[i]);
		a /= quant[i];
	}
	printf("%s", s);
	while (--i >= 0)
		if (nums[i])
			printf("%d %s%c ", nums[i], sep[i],
				nums[i] == 1 ? '\0' : 's');
	printf("\r\n!\r\n");
}

variable()
{
	char	buf[256];

	if (prompt("[set] ", buf))
		return;
	vlex(buf);
	if (vtable[BEAUTIFY].v_access&CHANGED) {
		vtable[BEAUTIFY].v_access &= ~CHANGED;
		signal(pid, SIGSYS);
	}
	if (vtable[SCRIPT].v_access&CHANGED) {
		vtable[SCRIPT].v_access &= ~CHANGED;
		setscript();
		/*
		 * So that "set record=blah script" doesn't
		 *  cause two transactions to occur.
		 */
		if (vtable[RECORD].v_access&CHANGED)
			vtable[RECORD].v_access &= ~CHANGED;
	}
	if (vtable[RECORD].v_access&CHANGED) {
		vtable[RECORD].v_access &= ~CHANGED;
		if (boolean(value(SCRIPT)))
			setscript();
	}
}

/*
 * Send a break.
 * If we can't do it directly (as on VMUNIX), then simulate it.
 */
genbrk()
{
#ifdef VMUNIX
	ioctl(FD, TIOCSBRK, NULL);
	sleep(1);
	ioctl(FD, TIOCCBRK, NULL);
#else
	struct sgttyb ttbuf;
	int sospeed;

	ioctl(FD, TIOCGETP, &ttbuf);
	sospeed = ttbuf.sg_ospeed;
	ttbuf.sg_ospeed = B150;
	ioctl(FD, TIOCSETP, &ttbuf);
	write(FD, "\0\0\0\0\0\0\0\0\0\0", 10);
	ttbuf.sg_ospeed = sospeed;
	ioctl(FD, TIOCSETP, &ttbuf);
	write(FD, "@", 1);
#endif
}
