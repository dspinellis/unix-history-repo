#ifndef lint
static char sccsid[] = "@(#)courierd.c	4.2 (Berkeley) 7/7/83";
#endif

/*
 * Courier program instantiation server
 *
 * Listens on the well-known socket for Courier, as defined in /etc/services.
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
#include "../lib/courier.h"

#define LIBCOURIER "/usr/new/lib/courier"

main(argc, argv)
	int argc;
	char **argv;
{
	int f, s, pid, len;
	struct sockaddr_in sin, from;
	struct servent *srvp;
	extern int errno;

	if (argc != 1)
		fprintf(stderr, "Courier daemon: arguments ignored\n");
	srvp = getservbyname("courier", "tcp");
	if (srvp == 0) {
		fprintf(stderr, "tcp/courier: unknown service\n");
		exit(1);
	}
	if (chdir(LIBCOURIER)) {
		perror(LIBCOURIER);
		exit(1);
	}
	sin.sin_port = srvp->s_port;
	f = socket(AF_INET, SOCK_STREAM, 0);
	if (f < 0) {
		perror("Courier daemon: socket");
		exit(1);
	}
	sin.sin_family = AF_INET;
	if (bind(f, &sin, sizeof(struct sockaddr_in)) < 0) {
		perror("Courier daemon: bind");
		exit(1);
	}
	listen(f, 10);
	for (;;) {
		len = sizeof(from);
		s = accept(f, &from, &len);
		if (s < 0) {
			if (errno != EINTR) {
				perror("Courier daemon: accept");
				sleep(1);
			}
			continue;
		}
		if ((pid = fork()) == 0) {
			close(f);
			instantiate(s);
		}
		close(s);
		if (pid == -1)
			error("Try again.\n");
		while(wait3(0, WNOHANG, 0) > 0)
			continue;
	}
}

#include <setjmp.h>

jmp_buf	alarmbuf;
int	catch();

instantiate(f)
	int f;
{
	Cardinal n, nbytes;
	struct stat statbuf;
	char name[200];

	setpgrp(0, getpid());
	signal(SIGHUP, SIG_DFL);
	signal(SIGINT, SIG_DFL);
	signal(SIGQUIT, SIG_DFL);
	signal(SIGALRM, catch);

	dup2(f, 0);
	dup2(f, 1);
	close(f);

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
