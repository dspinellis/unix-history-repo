/*	implogd.c	4.1	82/04/04	*/

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
	s = open("/dev/tty", 2);
	if (s >= 0) {
		ioctl(s, TIOCNOTTY, 0);
		close(s);
	}
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
