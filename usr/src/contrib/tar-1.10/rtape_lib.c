/* Remote tape emulator subroutines.
   Copyright (C) 1988 Free Software Foundation

This file is part of GNU Tar.

GNU Tar is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Tar is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Tar; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* JF: modified to make all rmtXXX calls into macros for speed */

#ifndef lint
static char *RCSid = "$Header: /usr/src/local/usr.lib/librmt/RCS/rmtlib.c,v 1.7 89/03/23 14:09:51 root Exp Locker: root $";
#endif

/*
 * $Log:	rmtlib.c,v $
 * Revision 1.7  89/03/23  14:09:51  root
 * Fix from haynes@ucscc.ucsc.edu for use w/compat. ADR.
 * 
 * Revision 1.6  88/10/25  17:04:29  root
 * rexec code and a bug fix from srs!dan, miscellanious cleanup. ADR.
 * 
 * Revision 1.5  88/10/25  16:30:17  root
 * Fix from jeff@gatech.edu for getting user@host:dev right. ADR.
 * 
 * Revision 1.4  87/10/30  10:36:12  root
 * Made 4.2 syntax a compile time option. ADR.
 * 
 * Revision 1.3  87/04/22  11:16:48  root
 * Two fixes from parmelee@wayback.cs.cornell.edu to correctly
 * do fd biasing and rmt protocol on 'S' command. ADR.
 * 
 * Revision 1.2  86/10/09  16:38:53  root
 * Changed to reflect 4.3BSD rcp syntax. ADR.
 * 
 * Revision 1.1  86/10/09  16:17:35  root
 * Initial revision
 * 
 */

/*
 *	rmt --- remote tape emulator subroutines
 *
 *	Originally written by Jeff Lee, modified some by Arnold Robbins
 *
 *	WARNING:  The man page rmt(8) for /etc/rmt documents the remote mag
 *	tape protocol which rdump and rrestore use.  Unfortunately, the man
 *	page is *WRONG*.  The author of the routines I'm including originally
 *	wrote his code just based on the man page, and it didn't work, so he
 *	went to the rdump source to figure out why.  The only thing he had to
 *	change was to check for the 'F' return code in addition to the 'E',
 *	and to separate the various arguments with \n instead of a space.  I
 *	personally don't think that this is much of a problem, but I wanted to
 *	point it out.
 *	-- Arnold Robbins
 *
 *	Redone as a library that can replace open, read, write, etc, by
 *	Fred Fish, with some additional work by Arnold Robbins.
 */
 
/* Use -DUSE_REXEC for rexec code, courtesy of Dan Kegel, srs!dan */

#if defined(USG) && !defined(HAVE_MTIO)
#define NO_RMTIOCTL
#endif

#include <stdio.h>
#include <signal.h>
#include <sys/types.h>

#ifndef NO_RMTIOCTL
#include <sys/ioctl.h>
#include <sys/mtio.h>
#endif

#ifdef USE_REXEC
#include <netdb.h>
#endif

#include <errno.h>
#include <setjmp.h>
#include <sys/stat.h>

#define BUFMAGIC	64	/* a magic number for buffer sizes */

/*
 *	MAXUNIT --- Maximum number of remote tape file units
 */
#define MAXUNIT	4

/*
 *	READ --- Return the number of the read side file descriptor
 *	WRITE --- Return the number of the write side file descriptor
 */
#define READ(fd)	(Ctp[fd][0])
#define WRITE(fd)	(Ptc[fd][1])

static int Ctp[MAXUNIT][2] = { -1, -1, -1, -1, -1, -1, -1, -1 };
static int Ptc[MAXUNIT][2] = { -1, -1, -1, -1, -1, -1, -1, -1 };

extern int errno;

char *__rmt_path;

/*
 *	_rmt_panic --- close off a remote tape connection
 */

static void _rmt_panic(fildes)
int fildes;
{
	close(READ(fildes));
	close(WRITE(fildes));
	READ(fildes) = -1;
	WRITE(fildes) = -1;
}



/*
 *	command --- attempt to perform a remote tape command
 */

static int command(fildes, buf)
int fildes;
char *buf;
{
	register int blen;
#ifdef SIGNAL_VOID
	void (*pstat)();
#else
	int (*pstat)();
#endif

/*
 *	save current pipe status and try to make the request
 */

	blen = strlen(buf);
	pstat = signal(SIGPIPE, SIG_IGN);
	if (write(WRITE(fildes), buf, blen) == blen)
	{
		signal(SIGPIPE, pstat);
		return(0);
	}

/*
 *	something went wrong. close down and go home
 */

	signal(SIGPIPE, pstat);
	_rmt_panic(fildes);

	errno = EIO;
	return(-1);
}



/*
 *	status --- retrieve the status from the pipe
 */

static int status(fildes)
int fildes;
{
	int i;
	char c, *cp;
	char buffer[BUFMAGIC];

/*
 *	read the reply command line
 */

	for (i = 0, cp = buffer; i < BUFMAGIC; i++, cp++)
	{
		if (read(READ(fildes), cp, 1) != 1)
		{
			_rmt_panic(fildes);
			errno = EIO;
			return(-1);
		}
		if (*cp == '\n')
		{
			*cp = 0;
			break;
		}
	}

	if (i == BUFMAGIC)
	{
		_rmt_panic(fildes);
		errno = EIO;
		return(-1);
	}

/*
 *	check the return status
 */

	for (cp = buffer; *cp; cp++)
		if (*cp != ' ')
			break;

	if (*cp == 'E' || *cp == 'F')
	{
		errno = atoi(cp + 1);
		while (read(READ(fildes), &c, 1) == 1)
			if (c == '\n')
				break;

		if (*cp == 'F')
			_rmt_panic(fildes);

		return(-1);
	}

/*
 *	check for mis-synced pipes
 */

	if (*cp != 'A')
	{
		_rmt_panic(fildes);
		errno = EIO;
		return(-1);
	}

	return(atoi(cp + 1));
}

#ifdef USE_REXEC

/*
 * _rmt_rexec
 *
 * execute /etc/rmt on a remote system using rexec().
 * Return file descriptor of bidirectional socket for stdin and stdout
 * If username is NULL, or an empty string, uses current username.
 *
 * ADR: By default, this code is not used, since it requires that
 * the user have a .netrc file in his/her home directory, or that the
 * application designer be willing to have rexec prompt for login and
 * password info. This may be unacceptable, and .rhosts files for use
 * with rsh are much more common on BSD systems.
 */

static int
_rmt_rexec(host, user)
char *host;
char *user;		/* may be NULL */
{
	struct servent *rexecserv;
	int save_stdin = dup(fileno(stdin));
	int save_stdout = dup(fileno(stdout));
	int tape_fd;		/* Return value. */

	/*
	 * When using cpio -o < filename, stdin is no longer the tty.
	 * But the rexec subroutine reads the login and the passwd on stdin, 
	 * to allow remote execution of the command.
	 * So, reopen stdin and stdout on /dev/tty before the rexec and
	 * give them back their original value after.
	 */
	if (freopen("/dev/tty", "r", stdin) == NULL)
		freopen("/dev/null", "r", stdin);
	if (freopen("/dev/tty", "w", stdout) == NULL)
		freopen("/dev/null", "w", stdout);

	rexecserv = getservbyname("exec", "tcp");
	if (NULL == rexecserv) {
		fprintf (stderr, "? exec/tcp: service not available.");
		exit (-1);
	}
	if ((user != NULL) && *user == '\0')
		user = (char *) NULL;
	tape_fd = rexec (&host, rexecserv->s_port, user, NULL,
			 "/etc/rmt", (int *)NULL);
	fclose(stdin);
	fdopen(save_stdin, "r");
	fclose(stdout);
	fdopen(save_stdout, "w");

	return tape_fd;
}
#endif /* USE_REXEC */

/*
 *	_rmt_open --- open a magtape device on system specified, as given user
 *
 *	file name has the form [user@]system:/dev/????
#ifdef COMPAT
 *	file name has the form system[.user]:/dev/????
#endif
 */

#define MAXHOSTLEN	257	/* BSD allows very long host names... */

int __rmt_open (path, oflag, mode, bias)
char *path;
int oflag;
int mode;
int bias;
{
	int i, rc;
	char buffer[BUFMAGIC];
	char system[MAXHOSTLEN];
	char device[BUFMAGIC];
	char login[BUFMAGIC];
	char *sys, *dev, *user;

	sys = system;
	dev = device;
	user = login;

/*
 *	first, find an open pair of file descriptors
 */

	for (i = 0; i < MAXUNIT; i++)
		if (READ(i) == -1 && WRITE(i) == -1)
			break;

	if (i == MAXUNIT)
	{
		errno = EMFILE;
		return(-1);
	}

/*
 *	pull apart system and device, and optional user
 *	don't munge original string
 *	if COMPAT is defined, also handle old (4.2) style person.site notation.
 */

	while (*path != '@'
#ifdef COMPAT
			&& *path != '.'
#endif
			&& *path != ':') {
		*sys++ = *path++;
	}
	*sys = '\0';
	path++;

	if (*(path - 1) == '@')
	{
		(void) strcpy (user, system);	/* saw user part of user@host */
		sys = system;			/* start over */
		while (*path != ':') {
			*sys++ = *path++;
		}
		*sys = '\0';
		path++;
	}
#ifdef COMPAT
	else if (*(path - 1) == '.')
	{
		while (*path != ':') {
			*user++ = *path++;
		}
		*user = '\0';
		path++;
	}
#endif
	else
		*user = '\0';

	while (*path) {
		*dev++ = *path++;
	}
	*dev = '\0';

#ifdef USE_REXEC
/* 
 *	Execute the remote command using rexec 
 */
	READ(i) = WRITE(i) = _rmt_rexec(system, login);
	if (READ(i) < 0)
		return -1;
#else
/*
 *	setup the pipes for the 'rsh' command and fork
 */

	if (pipe(Ptc[i]) == -1 || pipe(Ctp[i]) == -1)
		return(-1);

	if ((rc = fork()) == -1)
		return(-1);

	if (rc == 0)
	{
		close(0);
		dup(Ptc[i][0]);
		close(Ptc[i][0]); close(Ptc[i][1]);
		close(1);
		dup(Ctp[i][1]);
		close(Ctp[i][0]); close(Ctp[i][1]);
		(void) setuid (getuid ());
		(void) setgid (getgid ());
		if (*login)
		{
			execl("/usr/ucb/rsh", "rsh", system, "-l", login,
				"/etc/rmt", (char *) 0);
			execl("/usr/bin/remsh", "remsh", system, "-l", login,
				"/etc/rmt", (char *) 0);
			execl("/usr/bin/rsh", "rsh", system, "-l", login,
				"/etc/rmt", (char *) 0);
			execl("/usr/bsd/rsh", "rsh", system, "-l", login,
				"/etc/rmt", (char *)0);
			execl("/usr/bin/nsh", "nsh", system, "-l", login,
			        "/etc/rmt", (char *)0);
		}
		else
		{
			execl("/usr/ucb/rsh", "rsh", system,
				"/etc/rmt", (char *) 0);
			execl("/usr/bin/remsh", "remsh", system,
				"/etc/rmt", (char *) 0);
			execl("/usr/bin/rsh", "rsh", system,
				"/etc/rmt", (char *) 0);
			execl("/usr/bsd/rsh", "rsh", system,
				"/etc/rmt", (char *) 0);
			execl("/usr/bin/nsh", "nsh", system,
			        "/etc/rmt", (char *)0);
		}

/*
 *	bad problems if we get here
 */

		perror("remote shell exec");
		exit(1);
	}

	close(Ptc[i][0]); close(Ctp[i][1]);
#endif

/*
 *	now attempt to open the tape device
 */

	sprintf(buffer, "O%s\n%d\n", device, oflag);
	if (command(i, buffer) == -1 || status(i) == -1)
		return(-1);

	return(i+bias);
}



/*
 *	_rmt_close --- close a remote magtape unit and shut down
 */

 int __rmt_close(fildes)
int fildes;
{
	int rc;

	if (command(fildes, "C\n") != -1)
	{
		rc = status(fildes);

		_rmt_panic(fildes);
		return(rc);
	}

	return(-1);
}



/*
 *	_rmt_read --- read a buffer from a remote tape
 */

int __rmt_read(fildes, buf, nbyte)
int fildes;
char *buf;
unsigned int nbyte;
{
	int rc, i;
	char buffer[BUFMAGIC];

	sprintf(buffer, "R%d\n", nbyte);
	if (command(fildes, buffer) == -1 || (rc = status(fildes)) == -1)
		return(-1);

	for (i = 0; i < rc; i += nbyte, buf += nbyte)
	{
		nbyte = read(READ(fildes), buf, rc);
		if (nbyte <= 0)
		{
			_rmt_panic(fildes);
			errno = EIO;
			return(-1);
		}
	}

	return(rc);
}



/*
 *	_rmt_write --- write a buffer to the remote tape
 */

int __rmt_write(fildes, buf, nbyte)
int fildes;
char *buf;
unsigned int nbyte;
{
	char buffer[BUFMAGIC];
#ifdef SIGNAL_VOID
	void (*pstat)();
#else
	int (*pstat)();
#endif

	sprintf(buffer, "W%d\n", nbyte);
	if (command(fildes, buffer) == -1)
		return(-1);

	pstat = signal(SIGPIPE, SIG_IGN);
	if (write(WRITE(fildes), buf, nbyte) == nbyte)
	{
		signal (SIGPIPE, pstat);
		return(status(fildes));
	}

	signal (SIGPIPE, pstat);
	_rmt_panic(fildes);
	errno = EIO;
	return(-1);
}



/*
 *	_rmt_lseek --- perform an imitation lseek operation remotely
 */

long __rmt_lseek(fildes, offset, whence)
int fildes;
long offset;
int whence;
{
	char buffer[BUFMAGIC];

	sprintf(buffer, "L%d\n%d\n", offset, whence);
	if (command(fildes, buffer) == -1)
		return(-1);

	return(status(fildes));
}


/*
 *	_rmt_ioctl --- perform raw tape operations remotely
 */

#ifndef NO_RMTIOCTL
__rmt_ioctl(fildes, op, arg)
int fildes, op;
char *arg;
{
	char c;
	int rc, cnt;
	char buffer[BUFMAGIC];

/*
 *	MTIOCOP is the easy one. nothing is transfered in binary
 */

	if (op == MTIOCTOP)
	{
		sprintf(buffer, "I%d\n%d\n", ((struct mtop *) arg)->mt_op,
			((struct mtop *) arg)->mt_count);
		if (command(fildes, buffer) == -1)
			return(-1);
		return(status(fildes));
	}

/*
 *	we can only handle 2 ops, if not the other one, punt
 */

	if (op != MTIOCGET)
	{
		errno = EINVAL;
		return(-1);
	}

/*
 *	grab the status and read it directly into the structure
 *	this assumes that the status buffer is (hopefully) not
 *	padded and that 2 shorts fit in a long without any word
 *	alignment problems, ie - the whole struct is contiguous
 *	NOTE - this is probably NOT a good assumption.
 */

	if (command(fildes, "S") == -1 || (rc = status(fildes)) == -1)
		return(-1);

	for (; rc > 0; rc -= cnt, arg += cnt)
	{
		cnt = read(READ(fildes), arg, rc);
		if (cnt <= 0)
		{
			_rmt_panic(fildes);
			errno = EIO;
			return(-1);
		}
	}

/*
 *	now we check for byte position. mt_type is a small integer field
 *	(normally) so we will check its magnitude. if it is larger than
 *	256, we will assume that the bytes are swapped and go through
 *	and reverse all the bytes
 */

	if (((struct mtget *) arg)->mt_type < 256)
		return(0);

	for (cnt = 0; cnt < rc; cnt += 2)
	{
		c = arg[cnt];
		arg[cnt] = arg[cnt+1];
		arg[cnt+1] = c;
	}

	return(0);
  }
#endif /* NO_RMTIOCTL */
