#ifndef lint
static char *sccsid = "@(#)sliplogin.c	1.3	MS/ACF	89/04/18";
#endif

/*
 * sliplogin.c
 *
 * This program initializes its own tty port to be an async TCP/IP interface.
 * It merely sets up the SLIP module all by its lonesome on the STREAMS stack,
 * initializes the network interface, and pauses forever waiting for hangup.
 *
 * It is a remote descendant of several similar programs with incestuous ties:
 * - Kirk Smith's slipconf, modified by Richard Johnsson @ DEC WRL.
 * - slattach, probably by Rick Adams but touched by countless hordes.
 * - the original sliplogin for 4.2bsd, Doug Kingston the mover behind it.
 * - a simple slattach-like program used to test the STREAMS SLIP code.
 *
 * There are three basic forms of usage:
 *
 * "sliplogin"
 * Invoked simply as "sliplogin" and a realuid != 0, the program looks up
 * the uid in /etc/passwd, and then the username in the file /etc/hosts.slip.
 * If and entry is found, the line on fd0 is configured for SLIP operation
 * as specified in the file.
 *
 * "sliplogin IPhost1 </dev/ttyb"
 * Invoked by root with a username, the name is looked up in the
 * /etc/hosts.slip file and if found fd0 is configured as in case 1.
 *
 * "sliplogin 192.100.1.1 192.100.1.2 255.255.255.0 < /dev/ttyb"
 * Finally, if invoked with a remote addr, local addr, and optionally
 * a net mask, the line on fd0 is setup as specified if the user is root.
 *
 * Doug Kingston 8810??		- logging + first pass at adding I_STR ioctl's
 * Rayan Zachariassen 881011	- version for SunOS STREAMS SLIP
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stropts.h>
#include <sys/termios.h>
#include <sys/ttold.h>
#include <sys/sockio.h>
#include <sys/file.h>
#include <sys/syslog.h>

#include <sys/slip.h>

#include <netinet/in.h>
#include <net/if.h>

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <netdb.h>

#include <signal.h>
#include <strings.h>
#include <pwd.h>
#ifdef BSD >= 43
#include <ttyent.h>
#endif

#define	DCD_CHECK_INTERVAL 0	/* if > 0, time between automatic DCD checks */
#define	DCD_SETTLING_TIME 1	/* time between DCD change and status check */

int gotalarm = 0;
int timeleft = DCD_CHECK_INTERVAL;

void
alarm_handler()
{
	if (timeleft > DCD_SETTLING_TIME)
		(void) alarm(timeleft-DCD_SETTLING_TIME);
	else
		(void) alarm(DCD_CHECK_INTERVAL);
	gotalarm = 1;
	timeleft = 0;
}

#if	defined(SIGDCD) && SIGDCD > 0
void
dcd_handler()
{
#if	DCD_SETTLING_TIME > 0
	timeleft = alarm(DCD_SETTLING_TIME);
#else
	gotalarm = 1;
#endif	/* DCD_SETTLING_TIME */
}
#endif

/* Use TIOCMGET to test if DCD is low on the port of the passed descriptor */

int
lowdcd(fd)
	int fd;
{
	int mbits;

	if (ioctl(fd, TIOCMGET, (caddr_t)&mbits) < 0)
		return 1;	/* port is dead, we die */
	return !(mbits & TIOCM_CAR);
}

char	*Accessfile = "/etc/hosts.slip";

extern char *malloc(), *ttyname();
extern struct passwd *getpwuid();

char	*dstaddr, *localaddr, *netmask;

main(argc, argv)
	int argc;
	char *argv[];
{
	int	fd, s, unit;
	struct	termios tios;
	struct	ifreq ifr;

	s = getdtablesize();
	for (fd = 3 ; fd < s ; fd++)
		close(fd);

	openlog("sliplogin", LOG_PID, LOG_DAEMON);

	if (getuid() == 0) {
		if (argc <= 1) {
			fprintf(stderr, "Usage: %s loginname\n", argv[0]);
			fprintf(stderr, "   or: %s dstaddr localaddr [mask]\n",
					argv[0]);
			exit(1);
		} else if (argc == 2) {
			findid(argv[1]);
			fprintf(stderr, "local %s remote %s mask %s\n",
				localaddr, dstaddr, netmask);
		} if (argc > 2) {
			if (argc < 3 || argc > 4) {
				fprintf(stderr,
					"Usage: %s dstaddr localaddr [mask]\n",
					argv[0]);
				exit(1);
			}
			dstaddr = argv[1];
			localaddr = argv[2];
			if (argc == 4)
				netmask = argv[3];
			else
				netmask = "default";
		}
	} else
		findid((char *)0);

	/* ensure that the slip line is our controlling terminal */
	if ((fd = open("/dev/tty", O_RDONLY, 0)) >= 0) {
		(void) ioctl(fd, TIOCNOTTY, 0);
		(void) close(fd);
		fd = open(ttyname(0), O_RDWR, 0);
		if (fd >= 0)
			(void) close(fd);
		(void) setpgrp(0, getpid());
	}

	fchmod(0, 0600);

	/* pop all streams modules */
	while (ioctl(0, I_POP, 0) == 0)
		continue;

	/* set up the line parameters */
	if (ioctl(0, TCGETS, (caddr_t)&tios) < 0) {
		syslog(LOG_ERR, "ioctl (TCGETS): %m");
		exit(1);
	}
	tios.c_cflag &= 0xf;	/* only save the speed */
	tios.c_cflag |= CS8|CREAD|HUPCL;
	tios.c_iflag = IGNBRK;
	if (ioctl(0, TCSETS, (caddr_t)&tios) < 0) {
		syslog(LOG_ERR, "ioctl (TCSETS): %m");
		exit(1);
	}

	/* push the SLIP module */
	if (ioctl(0, I_PUSH, "slip") < 0) {
		syslog(LOG_ERR, "ioctl (I_PUSH): %m");
		exit(1);
	}

	/* find out what unit number we were assigned */
	if (ioctl(0, SLIOGUNIT, (caddr_t)&unit) < 0) {
		syslog(LOG_ERR, "ioctl (SLIOGUNIT): %m");
		exit(1);
	}

	syslog(LOG_NOTICE, "attaching slip%d: local %s remote %s mask %s\n",
		unit, localaddr, dstaddr, netmask);

	/* set the local and remote interface addresses */
	s = socket(AF_INET, SOCK_DGRAM, 0);

	if (getuid() != 0 || argc == 4) {
		(void) sprintf(ifr.ifr_name, "%s%d", SLIPIFNAME, unit);
		in_getaddr(netmask, &ifr.ifr_addr);
		if (ioctl(s, SIOCSIFNETMASK, (caddr_t)&ifr) < 0) {
			syslog(LOG_ERR, "ioctl (SIOCSIFNETMASK): %m");
			exit(1);
		}
	}

	(void) sprintf(ifr.ifr_name, "%s%d", SLIPIFNAME, unit);
	in_getaddr(dstaddr, &ifr.ifr_addr);
	if (ioctl(s, SIOCSIFDSTADDR, (caddr_t)&ifr) < 0) {
		syslog(LOG_ERR, "ioctl (SIOCSIFDSTADDR): %m");
		exit(1);
	}

	(void) sprintf(ifr.ifr_name, "%s%d", SLIPIFNAME, unit);
	in_getaddr(localaddr, &ifr.ifr_addr);
	/* this has the side-effect of marking the interface up */
	if (ioctl(s, SIOCSIFADDR, (caddr_t)&ifr) < 0) {
		syslog(LOG_ERR, "ioctl (SIOCSIFADDR): %m");
		exit(1);
	}

	/* set up signal handlers */
#if	defined(SIGDCD) && SIGDCD > 0
	(void) signal(SIGDCD, dcd_handler);
#endif
	(void) sigblock(sigmask(SIGALRM));
	(void) signal(SIGALRM, alarm_handler);
	/* a SIGHUP will kill us */

	/* timeleft = 60 * 60 * 24 * 365 ; (void) alarm(timeleft); */

	/* twiddle thumbs until we get a signal */
	while (1) {
		sigpause(0);
		(void) sigblock(sigmask(SIGALRM));
		if (gotalarm && lowdcd(0))
			break;
		gotalarm = 0;
	}

	/* closing the descriptor should pop the slip module */
	exit(0);
}

findid(name)
	char *name;
{
	char buf[BUFSIZ];
	static char mode[16];
	static char laddr[16];
	static char raddr[16];
	static char mask[16];
	char user[16];
	FILE *fp;
	struct passwd *pw;
	int n;

	if (name == NULL && (pw = getpwuid(getuid())) == NULL) {
		fprintf(stderr, "Your UID (%d) is unknown\n", getuid());
		syslog(LOG_ERR, "UID (%d) is unknown\n", getuid());
		exit(1);
	} else if (name == NULL)
		name = pw->pw_name;
	if ((fp = fopen(Accessfile, "r")) == NULL) {
		perror(Accessfile);
		syslog(LOG_ERR, "%s: %m\n", Accessfile);
		exit(3);
	}
	while (fgets(buf, sizeof(buf) - 1, fp)) {
		if (ferror(fp))
			break;
		n = sscanf(buf, "%15s%*[ \t]%15s%*[ \t]%15s%*[ \t]%15s%*[ \t]%15s\n",
			user, mode, laddr, raddr, mask);
		if (user[0] == '#' || n != 5)
			continue;
		if (strcmp(user, name) == 0) {
			/* eventually deal with "mode" */
			localaddr = laddr;
			dstaddr = raddr;
			netmask = mask;
			fclose(fp);
			return 0;
		}
		if (feof(fp))
			break;
	}
	fputs("SLIP access denied\n", stderr);
	syslog(LOG_ERR, "SLIP access denied for %s\n", name);
	exit(4);
}

in_getaddr(s, saddr)
	char *s;
	struct sockaddr *saddr;
{
	register struct sockaddr_in *sin = (struct sockaddr_in *)saddr;
	struct hostent *hp;
	struct netent *np;
	int val;
	extern struct in_addr inet_makeaddr();
 
	bzero((caddr_t)saddr, sizeof *saddr);
	sin->sin_family = AF_INET;
	val = inet_addr(s);
	if (val != -1) {
		sin->sin_addr.s_addr = val;
		return;
	}
	hp = gethostbyname(s);
	if (hp) {
		sin->sin_family = hp->h_addrtype;
		bcopy(hp->h_addr, (char *)&sin->sin_addr, hp->h_length);
		return;
	}
	np = getnetbyname(s);
	if (np) {
		sin->sin_family = np->n_addrtype;
		sin->sin_addr = inet_makeaddr(np->n_net, INADDR_ANY);
		return;
	}
	fprintf(stderr, "sliplogin: %s: bad value\n", s);
	syslog(LOG_ERR, "%s: bad value\n", s);
	exit(1);
}

