/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* This is a server for jove sub processes.  It runs the command and
   signals jove when there is some output ready to send to jove. By the
   time we get here, out standard output goes to jove's process input. */

#include "tune.h"

#ifdef PIPEPROCS	/* the whole file! */

#include "jove.h"

#include <signal.h>
#include <sys/ioctl.h>
#ifdef BSD4_2
#   include <sys/wait.h>
#else
#   include <wait.h>
#endif

struct header {
	int	pid;
	int	nbytes;
	char	buf[512];
} header;

#define HEADSIZE	((sizeof header.pid) + sizeof (header.nbytes))

error(str)
char	*str;
{
	header.pid = getpid();
	header.nbytes = strlen(str);
	strcpy(header.buf, str);
	proc_write(&header, header.nbytes + HEADSIZE);
	exit(-2);
}

int	ppid,
	InputFD,
	JovesInput;

p_inform()
{
	long	nbytes;

	ioctl(JovesInput, FIONREAD, (char *) &nbytes);
	if (nbytes > 0)
		kill(ppid, INPUT_SIG);
}

proc_write(ptr, n)
char	*ptr;
{
	long	nbytes;

	ioctl(1, FIONREAD, (char *) &nbytes);
	
	if (nbytes == 0)
		kill(ppid, INPUT_SIG);

	(void) write(1, ptr, n);
	alarm(1);
}

read_pipe()
{
	register int	n;
	
	(void) signal(SIGALRM, p_inform);

	while ((header.nbytes = read(InputFD, header.buf, sizeof header.buf)) > 0) {
		n = HEADSIZE + header.nbytes;
		proc_write(&header, n);
	}
}

/* ARGSUSED */
main(argc, argv)
char	*argv[];
{
	int	p[2];
	int	pid;
	int	tty_fd,
		i;

/*	tty_fd = open("/dev/tty", 1); */

	if (pipe(p) == -1)
		error("Cannot pipe jove portsrv.\n");

/*	for (i = 0; i < argc; i++) {
		write(tty_fd, "*argv++ = ", 10);
		write(tty_fd, argv[i], strlen(argv[i]));
		write(tty_fd, "\n", 1);
	} */

	ppid = getppid();
	switch (pid = fork()) {
	case -1:
		error("portsrv: cannot fork.\n");

	case 0:
		/* We'll intercept childs output in p[0] */
		(void) dup2(p[1], 1);
		(void) dup2(p[1], 2);
		(void) close(p[0]);
		(void) close(p[1]);
			
		(void) setpgrp(getpid(), getpid());
		execv(argv[2], &argv[3]);
		_exit(-4);

	default:
		(void) close(0);

		 /* don't want this guy to read anything jove sends to
		    our soon to be created child */

		JovesInput = atoi(argv[1]);
		(void) signal(SIGINT, SIG_IGN);
		(void) signal(SIGQUIT, SIG_IGN);
		(void) close(p[1]);

		/* tell jove the pid of the real child as opposed to us */
		header.pid = getpid();
		header.nbytes = sizeof (int);
		*(int *) header.buf = pid;
		(void) write(1, (char *) &header, sizeof pid + HEADSIZE);
		p_inform();	/* Inform jove */

		/* read proc's output and send it to jove */
		InputFD = p[0];
		read_pipe();
		(void) close(p[0]);
		header.pid = getpid();
		header.nbytes = EOF;	/* tell jove we are finished */
		(void) write(1, (char *) &header, HEADSIZE);
		p_inform();
		/* try to exit like our child did ... */
		{
			union wait	w;

#ifndef BSD4_2
			while (wait2(&w.w_status, 0) != pid)
#else
			while (wait3(&w.w_status, 0, 0) != pid)
#endif
				;
			if (WIFEXITED(w))
				exit(w.w_retcode);
			else if (WIFSIGNALED(w))
				kill(getpid(), w.w_termsig);
		}
	}
}

#else /* PIPEPROCS */
main()
{
}
#endif
