/*	implogd.c	4.2	82/10/10	*/

#include <time.h>
#include <sgtty.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <net/in.h>

#define	LOGFILE	"/usr/adm/implog"
#define	IMPMTU	((8159 / 8) & ~01)

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
	int	sin_cc;
};

struct	sockproto improto = { PF_IMPLINK, 0 };
struct	sockstamp from;

main(argc, argv)
	char *argv[];
{
	int s, cc;
	time_t t;

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
	log = open(LOGFILE, 1);
	if (log < 0)
		exit(1);
	lseek(log, 0L, 2);
	from.sin_time = time(0);
	from.sin_cc = sizeof(time_t);
	write(log, (char *)&from, sizeof(from));
again:
	errno = 0;
	if ((s = socket(SOCK_RAW, &improto, 0, options)) < 0) {
		perror("socket");
		sleep(5);
		goto again;
	}
	for (;;) {
		cc = receive(s, &from, request, sizeof(request));
		if (cc <= 0)
			continue;
		if (cc > IMPMTU)	/* sanity */
			continue;
		from.sin_cc = cc;
		from.sin_time = time(0);
		write(log, (char *)&from, sizeof(from));
		write(log, request, cc);
	}
	/*NOTREACHED*/
}
