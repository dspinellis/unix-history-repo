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
static char sccsid[] = "@(#)startslip.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sgtty.h>
#include <sys/socket.h>
#include <sys/syslog.h>
#include <netinet/in.h>
#include <net/if.h>
#include <net/if_slvar.h>
#include <netdb.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <signal.h>

#define DEFAULT_BAUD    B9600
int     speed = DEFAULT_BAUD;
int	hup;
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
	int ch, disc;
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
	if (debug == 0 && (fd = open("/dev/tty", 0)) >= 0) {
		ioctl(fd, TIOCNOTTY, 0);
		close(fd);
	}
#endif

	signal(SIGHUP, sighup);
restart:
	hup = 0;
	printd("restart\n");
	if (fd >= 0)
		close(fd);
	if (!first) {
		if (fork() > 0)
			exit(0);
		printd("(pid %d)\n", getpid());
#ifdef TIOCSCTTY
		if (setsid() == -1)
			perror("setsid");
#endif
	} else
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
#ifdef TIOCSCTTY
	if (ioctl(fd, TIOCSCTTY, 0) < 0)
		perror("ioctl (TIOCSCTTY)");
#endif
	if (debug) {
		if (ioctl(fd, TIOCGETD, &disc) < 0)
			perror("ioctl(TIOCSETD)");
		printf("disc was %d\n", disc);
	}
	disc = TTYDISC;
	if (ioctl(fd, TIOCSETD, &disc) < 0) {
	        perror("ioctl(TIOCSETD)");
		syslog(LOG_ERR, "startslip: %s: ioctl (TIOCSETD 0): %m\n",
		    argv[0]);
	}
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
	wfd = fdopen(fd, "w+");
	printd("fdopen\n");
	if (wfd == NULL) {
		syslog(LOG_ERR, "startslip: can't fdopen slip line\n");
		exit(10);
	}
	putc('\n', wfd);
	while (fflush(wfd), getline(buf, BUFSIZ, fd) != NULL) {
		if (hup != 0)
			goto cleanup;
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
	disc = SLIPDISC;
	if (ioctl(fd, TIOCSETD, &disc) < 0) {
	        perror("ioctl(TIOCSETD)");
		syslog(LOG_ERR, "startslip: %s: ioctl (TIOCSETD SLIP): %m\n",
		    argv[0]);
	        exit(1);
	}
	printd("setd\n");
	disc = SC_COMPRESS;
	if (ioctl(fd, SLIOCSFLAGS, (caddr_t)&disc) < 0) {
	        perror("ioctl(SLIOCFLAGS)");
		syslog(LOG_ERR, "ioctl (SLIOCSFLAGS): %m");
		exit(1);
	}
	if (!first++ && debug == 0) {
		close(0);
		close(1);
		close(2);
		(void) open("/dev/null", O_RDWR);
		(void) dup2(0, 1);
		(void) dup2(0, 2);
	}
	(void) system("ifconfig sl0 up");
	while (hup == 0) {
		sigpause(0L);
		printd("sigpause return ");
	}
cleanup:
	printd("close\n");
	fclose(wfd);
	close(fd);
#ifdef TIOCSCTTY
	if (fork() > 0)
		exit(0);
	printd("(pid %d)\n", getpid());
	if (setsid() == -1)
		perror("setsid");
	sleep(5);
#endif
	goto restart;
}

sighup()
{

	printd("hup\n");
	if (hup == 0)
		syslog(LOG_INFO, "startslip: hangup signal\n");
	hup = 1;
}

getline(buf, size, fd)
	char *buf;
	int size, fd;
{
	register int i;
	int ret;

	size--;
	for (i = 0; i < size; i++) {
		if (hup)
			return (0);
	        if ((ret = read(fd, &buf[i], 1)) == 1) {
	                buf[i] &= 0177;
	                if (buf[i] == '\r')
	                        buf[i] = '\n';
	                if (buf[i] != '\n' && buf[i] != ':')
	                        continue;
	                buf[i + 1] = '\0';
			if (debug)
				fprintf(stderr, "Got %d: \"%s\"\n", i + 1, buf);
	                return (i+1);
	        }
		if (ret < 0) {
			perror("getline: read (sleeping)");
			buf[i] = '\0';
			sleep(60);
			printd("returning 0 after %d: \"%s\"\n", i, buf);
			return (0);
		}
	}
	return (0);
}

usage()
{
	fprintf(stderr, "usage: startslip [-d] [-s string] dev user passwd\n");
	exit(1);
}
