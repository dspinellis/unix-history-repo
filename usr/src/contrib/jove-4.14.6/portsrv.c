/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* This is a server for jove sub processes.  By the time we get here, our
   standard output goes to jove's process input. */

#include "jove.h"

#ifdef	PIPEPROCS	/* the whole file! */

#include <signal.h>
#include <sys/ioctl.h>
#include "wait.h"

private struct header {
	int	pid;
	int	nbytes;
	char	buf[512];
} header;

private int	tty_fd;

#define HEADSIZE	((sizeof header.pid) + sizeof (header.nbytes))

private void
proc_write(ptr, n)
UnivConstPtr	ptr;
size_t	n;
{
	(void) write(1, ptr, n);
}

private void
read_pipe(fd)
int	fd;
{
	register size_t	n;

	while ((header.nbytes = read(fd, (UnivPtr) header.buf, sizeof header.buf)) > 0) {
		n = HEADSIZE + header.nbytes;
		proc_write((UnivConstPtr) &header, n);
	}
}

private void
proc_error(str)
char	*str;
{
	header.pid = getpid();
	header.nbytes = strlen(str);
	strcpy(header.buf, str);
	proc_write((UnivConstPtr) &header, header.nbytes + HEADSIZE);
	write(tty_fd, str, strlen(str));
	exit(-2);
}

/* ARGSUSED */
int
main(argc, argv)
int	argc;
char	*argv[];
{
	int	p[2];
	int	pid;

	if (pipe(p) == -1)
		proc_error("Cannot pipe jove portsrv.\n");

	switch (pid = fork()) {
	case -1:
		proc_error("portsrv: cannot fork.\n");
		/*NOTREACHED*/

	case 0:
		/* We'll intercept childs output in p[0] */
		(void) dup2(p[1], 1);
		(void) dup2(p[1], 2);
		(void) close(p[0]);
		(void) close(p[1]);

		(void) setpgrp(getpid(), getpid());
		execv(argv[1], (const char **) &argv[2]);
		_exit(-4);
		/*NOTREACHED*/

	default:
		(void) close(0);
		tty_fd = open("/dev/tty", 1);

		(void) signal(SIGINT, SIG_IGN);
		(void) signal(SIGQUIT, SIG_IGN);
		(void) close(p[1]);

		/* tell jove the pid of the real child as opposed to us */
		header.pid = getpid();
		header.nbytes = sizeof (int);
		*(int *) header.buf = pid;
		(void) write(1, (char *) &header, sizeof pid + HEADSIZE);

		/* read proc's output and send it to jove */
		read_pipe(p[0]);

		/* received EOF - wait for child to die and then exit
		   ourself in the same way so that JOVE knows how the
		   child died.  This is sort of a kludge, but the alternative
		   is to write the childs status to JOVE, which seems sorta
		   yucky, too.

		   Actually, 4 or 5 years later I like that idea much better,
		   so remind me to implement it that way when I get a chance.

		   7-23-89  Gee thanks, whoever implemented this for me! */

		(void) close(p[0]);
		header.pid = getpid();
		header.nbytes = EOF;	/* tell jove we are finished */
		/* try to exit like our child did ... */
		{
			union wait	status;

			do ; while (wait(&status) != pid);
			*(int *) header.buf = status.w_status;
		}
		(void) write(1, (UnivConstPtr) &header, HEADSIZE + sizeof (int));
	}
	return 0;
}

#else	/* !PIPEPROCS */
int
main()
{
	return 0;
}
#endif	/* !PIPEPROCS */
