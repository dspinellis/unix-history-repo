/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)startslip.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/signal.h>
#include <sys/socket.h>
#include <sys/syslog.h>
#include <netinet/in.h>
#include <net/if.h>
#include <net/if_slvar.h>
#include <sgtty.h>
#include <netdb.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>

#define DEFAULT_BAUD    B9600
int     speed = DEFAULT_BAUD;
int     slipdisc = SLIPDISC;
#ifdef DEBUG
int	debug = 1;
#undef LOG_ERR
#undef LOG_INFO
#define syslog fprintf
#define LOG_ERR stderr
#define LOG_INFO stderr
#else
int	debug = 0;
#endif
#define	printd	if (debug) printf

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	int ch;
	int fd = -1, sighup();
	FILE *wfd;
	char *dialerstring = 0, buf[BUFSIZ];
	struct sgttyb sgtty;
	int first = 0;

	while ((ch = getopt(argc, argv, "ds:")) != EOF)
		switch(ch) {
		case 'd':
			debug = 1;
			break;
		case 's':
			dialerstring = optarg;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 3)
		usage();

	openlog("startslip", LOG_PID, LOG_DAEMON);

#if BSD <= 43
	if (debug == 0 && (fd = open("/dev/tty", 0))) {
		ioctl(fd, TIOCNOTTY, 0);
		close(fd);
	}
#endif

	signal(SIGHUP, sighup);
restart:
	printd("restart\n");
	if (fd >= 0)
		close(fd);
	if (first)
		sleep(2);
	if ((fd = open(argv[0], O_RDWR)) < 0) {
		perror(argv[0]);
		syslog(LOG_ERR, "startslip: open %s: %m\n", argv[0]);
		if (first)
			exit(1);
		else {
			sleep(5*60);
			goto restart;
		}
	}
	printd("open %d\n", fd);
	if (dialerstring) {
		(void) write(fd, dialerstring, strlen(dialerstring));
		(void) write(fd, "\n", 1);
	}
	if (ioctl(fd, TIOCGETP, &sgtty) < 0) {
	        perror("ioctl (TIOCGETP)");
	        exit(2);
	}
	sgtty.sg_flags = RAW | ANYP;
	sgtty.sg_erase = sgtty.sg_kill = 0377;
	sgtty.sg_ispeed = sgtty.sg_ospeed = speed;
	if (ioctl(fd, TIOCSETP, &sgtty) < 0) {
	        perror("ioctl (TIOCSETP)");
		syslog(LOG_ERR, "startslip: %s: ioctl (TIOCSETP): %m\n",
		    argv[0]);
	        exit(2);
	}
	printd("ioctl\n");
	/*
	 * Log in
	 */
	wfd = fdopen(fd, "w");
	printd("fdopen\n");
	if (wfd == NULL) {
		syslog(LOG_ERR, "startslip: can't fdopen slip line\n");
		exit(10);
	}
	putc('\n', wfd);
	while (fflush(wfd), getline(buf, BUFSIZ, fd) != NULL) {
	        if (bcmp(&buf[1], "ogin:", 5) == 0) {
	                fprintf(wfd, "%s\r", argv[1]);
	                continue;
	        }
	        if (bcmp(&buf[1], "assword:", 8) == 0) {
	                fprintf(wfd, "%s\r", argv[2]);
	                fflush(wfd);
	                break;
	        }
	}
	printd("login\n");
	/*
	 * Attach
	 */
	if (ioctl(fd, TIOCSETD, &slipdisc) < 0) {
	        perror("ioctl(TIOCSETD)");
		syslog(LOG_ERR, "startslip: %s: ioctl (TIOCSETD): %m\n",
		    argv[0]);
	        exit(1);
	}
	slipdisc = SC_COMPRESS;
	if (ioctl(fd, SLIOCSFLAGS, (caddr_t)&slipdisc) < 0) {
	        perror("ioctl(SLIOCFLAGS)");
		syslog(LOG_ERR, "ioctl (SLIOCSFLAGS): %m");
		exit(1);
	}
	printd("setd\n");
	if (!first++) {
		if (fork() > 0)
			exit(0);
#ifdef TIOCSCTTY
		if (setsid() == -1)
			perror("setsid");
		if (ioctl(fd, TIOCSCTTY, 0) < 0)
			perror("ioctl (TIOCSCTTY)");
#endif
		if (debug == 0) {
			close(0);
			close(1);
			close(2);
		}
	}
	(void) system("ifconfig sl0 up");
	sigpause(0L);
	printd("fclose\n");
	fclose(wfd);
	goto restart;
}

sighup()
{

	syslog(LOG_INFO, "startslip: hangup signal\n");
}

getline(buf, size, fd)
	char *buf;
	int size, fd;
{
	register int i;

	size--;
	for (i = 0; i < size; i++) {
	        if (read(fd, &buf[i], 1) == 1) {
	                buf[i] &= 0177;
	                if (buf[i] == '\r')
	                        buf[i] = '\n';
	                if (buf[i] != '\n' && buf[i] != ':')
	                        continue;
	                buf[i + 1] = '\0';
			if (debug)
				fprintf(stderr, "Got %d: \"%s\"\n", i, buf);
	                return (i+1);
	        }
	        perror("read");
	        i--;
	}
	return(NULL);
}

usage()
{
	fprintf(stderr, "usage: startslip [-d] [-s string] dev user passwd\n");
	exit(1);
}
