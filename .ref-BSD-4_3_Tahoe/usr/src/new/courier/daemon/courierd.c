#ifndef lint
static char sccsid[] = "@(#)courierd.c	4.4 (Berkeley) 9/17/85";
#endif

/*
 * Courier program instantiation server
 *
 * When a connection is made, it reads the program name as a Courier string.
 * The directory /usr/new/lib/courier is used as a database of program names;
 * the executable program must either reside there or have a link there.
 * The program is spawned with user and group ID equal to that of the
 * executable file, and must write a single zero byte back to the caller to 
 * indicate successful instantiation.
 * If instantiation was unsuccessful, the daemon will write back a null-
 * terminated error message.
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include <netinet/in.h>

#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <netdb.h>
#include <setjmp.h>
#include <syslog.h>

#include "../lib/courier.h"

#define LIBCOURIER "/usr/new/lib/courier"

jmp_buf	alarmbuf;
int	catch();

main(argc, argv)
	int argc;
	char **argv;
{
	struct sockaddr_in sin;
	Cardinal n, nbytes;
	struct stat statbuf;
	char name[200];
	int len;

	len = sizeof (sin);
	if (getpeername(0, &sin, &len) < 0) {
		fprintf(stderr, "%s: ", argv[0]), perror("getpeername");
		_exit(1);
	}
	openlog("courier", LOG_ODELAY, LOG_DAEMON);
	if (chdir(LIBCOURIER)) {
		syslog(LOG_ERR, "chdir: %s: %m", LIBCOURIER);
		_exit(2);
	}
	setpgrp(0, getpid());
	signal(SIGHUP, SIG_DFL);
	signal(SIGINT, SIG_DFL);
	signal(SIGQUIT, SIG_DFL);
	signal(SIGALRM, catch);

	dup2(0, 1);

	alarm(60);
	if (setjmp(alarmbuf) == 0)
		read(0, &n, sizeof(Cardinal));
	alarm(0);

	UnpackCardinal(&nbytes, &n);
	/*
	 * Courier strings are always word-aligned, so if the byte count is odd,
	 * we must also read the padding byte.
	 */
	read(0, name, nbytes + (nbytes % 2));
	name[nbytes] = '\0';

	if (name[0] == '/' || name[0] == '.' || stat(name, &statbuf) != 0)
		goto bad;
	setgroups(0, 0);
	setregid(statbuf.st_gid, statbuf.st_gid);
	setreuid(statbuf.st_uid, statbuf.st_uid);
	execl(name, name, 0);
bad:
	error("Unknown Courier program.\n");
	syslog(LOG_ERR, "Unknown Courier program");
	exit(1);
}

/*
 * Catch alarm clock so read's will timeout.
 */
catch()
{

	longjmp(alarmbuf, 1);
}

/*
 * Write back a null-terminated error message.
 */
error(s)
	char *s;
{
	write(1, s, strlen(s)+1);
}
