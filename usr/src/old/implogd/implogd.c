/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)implogd.c	5.1 (Berkeley) %G%";
#endif not lint

#include <sgtty.h>

#include <sys/time.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/file.h>

#include <netinet/in.h>
#include <netimp/if_imp.h>

#define	LOGFILE	"/usr/adm/implog"

u_char	request[1024];
int	marktime();
int	options;
extern	int errno;
int	log;

/*
 * Socket address, internet style, with
 * unused space taken by timestamp and packet
 * size.
 */
struct sockstamp {
	short	sin_family;
	u_short	sin_port;
	struct	in_addr sin_addr;
	time_t	sin_time;
	int	sin_len;
};

main(argc, argv)
	char *argv[];
{
	int s;
	time_t t;
	struct sockstamp from;

	argc--, argv++;
	if (argc > 0 && !strcmp(argv[0], "-d"))
		options |= SO_DEBUG;
#ifndef DEBUG
	if (fork())
		exit(0);
	for (s = 0; s < 10; s++)
		(void) close(t);
	(void) open("/", 0);
	(void) dup2(0, 1);
	(void) dup2(0, 2);
	{ int tt = open("/dev/tty", 2);
	  if (tt > 0) {
		ioctl(tt, TIOCNOTTY, 0);
		close(tt);
	  }
	}
#endif
	log = open(LOGFILE, O_CREAT|O_WRONLY|O_APPEND, 0644);
	if (log < 0) {
		perror("implogd: open");
		exit(1);
	}
	from.sin_time = time(0);
	from.sin_len = sizeof (time_t);
	write(log, (char *)&from, sizeof (from));
	while ((s = socket(AF_IMPLINK, SOCK_RAW, 0, 0)) < 0) {
		perror("implogd: socket");
		sleep(5);
	}
	for (;;) {
		int fromlen = sizeof (from), len;

		len = recvfrom(s, request, sizeof (request), 0,
			&from, &fromlen);
		if (len < 0) {
			perror("implogd: recvfrom");
			continue;
		}
		if (len == 0 || len > IMPMTU)	/* sanity */
			continue;
		from.sin_len = len;
		from.sin_time = time(0);
		write(log, (char *)&from, sizeof (from));
		write(log, request, len);
	}
	/*NOTREACHED*/
}
