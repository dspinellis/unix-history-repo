#ifndef lint
static char sccsid[] = "@(#)rlogind.c	4.1 82/04/02";
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/socket.h>
#include <net/in.h>
#include <errno.h>
#include <pwd.h>
#include <wait.h>
#include <signal.h>
#include <sgtty.h>
#include <stdio.h>

extern	errno;
struct	passwd *getpwnam();
char	*crypt(), *rindex(), *index(), *malloc(), *raddr();
int	options = SO_ACCEPTCONN|SO_KEEPALIVE;
struct	sockaddr_in sin = { AF_INET, IPPORT_LOGINSERVER };
/*
 * remote login server:
 *	remuser\0
 *	locuser\0
 *	terminal type\0
 *	data
 */
main(argc, argv)
	int argc;
	char **argv;
{
	union wait status;
	int f, debug = 0;
	struct sockaddr_in from;

#ifndef DEBUG
	if (fork())
		exit(0);
	for (f = 0; f < 10; f++)
		(void) close(f);
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
#if vax
	sin.sin_port = htons(sin.sin_port);
#endif
	argc--, argv++;
	if (argc > 0 && !strcmp(argv[0], "-d"))
		options |= SO_DEBUG;
	for (;;) {
		f = socket(SOCK_STREAM, 0, &sin, options);
		if (f < 0) {
			perror("socket");
			sleep(5);
			continue;
		}
		if (accept(f, &from) < 0) {
			perror("accept");
			close(f);
			sleep(1);
			continue;
		}
		if (fork() == 0)
			doit(f, &from);
		close(f);
		while (wait3(status, WNOHANG, 0) > 0)
			continue;
	}
}

char	locuser[32], remuser[32];
char	buf[BUFSIZ];
int	child;
int	cleanup();
int	netf;
extern	errno;
char	*line;

doit(f, fromp)
	int f;
	struct sockaddr_in *fromp;
{
	char c, *rhost;
	int i, p, cc, t;
	int stop = TIOCPKT_DOSTOP;

	alarm(60);
	read(f, &c, 1);
	if (c != 0)
		exit(1);
	alarm(0);
#if vax
	fromp->sin_port = htons(fromp->sin_port);
#endif
	rhost = raddr(fromp->sin_addr.s_addr);
	if (fromp->sin_family != AF_INET ||
	    fromp->sin_port >= IPPORT_RESERVED ||
	    rhost == 0) {
		write(f, "\01Permission denied.\n", 20);
		exit(1);
	}
	write(f, "", 1);
	for (c = 'p'; c <= 's'; c++) {
		struct stat stb;
		line = "/dev/ptyXX";
		line[strlen("/dev/pty")] = c;
		line[strlen("/dev/ptyp")] = '0';
		if (stat(line, &stb) < 0)
			break;
		for (i = 0; i < 16; i++) {
			line[strlen("/dev/ptyp")] = "0123456789abcdef"[i];
			p = open(line, 2);
			if (p > 0)
				goto gotpty;
		}
	}
	dup2(f, 1);
	printf("All network ports in use.\r\n");
	exit(1);
gotpty:
	dup2(f, 0);
	line[strlen("/dev/")] = 't';
#ifdef DEBUG
	{ int tt = open("/dev/tty", 2);
	  if (tt > 0) {
		ioctl(tt, TIOCNOTTY, 0);
		close(tt);
	  }
	}
#endif
	t = open(line, 2);
	if (t < 0) {
		dup2(f, 2);
		perror(line);
		exit(1);
	}
	{ struct sgttyb b;
	  gtty(t, &b); b.sg_flags = RAW|ANYP; stty(t, &b);
	}
	if (fork()) {
		char pibuf[1024], fibuf[1024], *pbp, *fbp;
		int pcc = 0, fcc = 0, on = 1;
/* FILE *console = fopen("/dev/console", "w");  */
/* setbuf(console, 0); */

/* fprintf(console, "f %d p %d\r\n", f, p); */
		ioctl(f, FIONBIO, &on);
		ioctl(p, FIONBIO, &on);
		ioctl(p, TIOCPKT, &on);
		signal(SIGTSTP, SIG_IGN);
		sigset(SIGCHLD, cleanup);
		for (;;) {
			int ibits = 0, obits = 0;
			if (fcc) obits |= (1<<p); else ibits |= (1<<f);
			if (pcc >= 0)
			if (pcc) obits |= (1<<f); else ibits |= (1<<p);
			if (fcc < 0 && pcc < 0) break;
/* fprintf(console, "ibits from %d obits from %d\r\n", ibits, obits); */
			select(32, &ibits, &obits, 10000000);
/* fprintf(console, "ibits %d obits %d\r\n", ibits, obits); */
			if (ibits == 0 && obits == 0) {
				sleep(5);
				continue;
			}
			if (ibits & (1<<f)) {
				fcc = read(f, fibuf, sizeof (fibuf));
/* fprintf(console, "%d from f\r\n", fcc); */
				if (fcc < 0 && errno == EWOULDBLOCK)
					fcc = 0;
				else {
					if (fcc <= 0)
						break;
					fbp = fibuf;
				}
			}
			if (ibits & (1<<p)) {
				pcc = read(p, pibuf, sizeof (pibuf));
/* fprintf(console, "%d from p, buf[0] %x, errno %d\r\n", pcc, buf[0], errno); */
				pbp = pibuf;
				if (pcc < 0 && errno == EWOULDBLOCK)
					pcc = 0;
				else if (pcc <= 0)
					pcc = -1;
				else if (pibuf[0] == 0)
					pbp++, pcc--;
				else {
					if (pibuf[0]&(TIOCPKT_FLUSHWRITE|
						      TIOCPKT_NOSTOP|
						      TIOCPKT_DOSTOP)) {
						int nstop = pibuf[0] &
						    (TIOCPKT_NOSTOP|
						     TIOCPKT_DOSTOP);
						if (nstop)
							stop = nstop;
						pibuf[0] |= nstop;
						ioctl(f,SIOCSENDOOB,&pibuf[0]);
					}
					pcc = 0;
				}
			}
			if ((obits & (1<<f)) && pcc > 0) {
				cc = write(f, pbp, pcc);
/* fprintf(console, "%d of %d to f\r\n", cc, pcc); */
				if (cc > 0) {
					pcc -= cc;
					pbp += cc;
				}
			}
			if ((obits & (1<<p)) && fcc > 0) {
				cc = write(p, fbp, fcc);
/* fprintf(console, "%d of %d to p\r\n", cc, fcc); */
				if (cc > 0) {
					fcc -= cc;
					fbp += cc;
				}
			}
		}
		cleanup();
	}
	close(f);
	close(p);
	dup2(t, 0);
	dup2(t, 1);
	dup2(t, 2);
	close(t);
	execl("/bin/login", "login", "-r", rhost, 0);
	perror("/bin/login");
	exit(1);
}

cleanup()
{
	int how = 2;

	rmut();
	ioctl(netf, SIOCDONE, &how);
	kill(0, SIGKILL);
	exit(1);
}

#include <utmp.h>

struct	utmp wtmp;
char	wtmpf[]	= "/usr/adm/wtmp";
char	utmp[] = "/etc/utmp";
#define SCPYN(a, b)	strncpy(a, b, sizeof(a))
#define SCMPN(a, b)	strncmp(a, b, sizeof(a))

rmut()
{
	register f;
	int found = 0;

	f = open(utmp, 2);
	if (f >= 0) {
		while(read(f, (char *)&wtmp, sizeof(wtmp)) == sizeof(wtmp)) {
			if (SCMPN(wtmp.ut_line, line+5) || wtmp.ut_name[0]==0)
				continue;
			lseek(f, -(long)sizeof(wtmp), 1);
			SCPYN(wtmp.ut_name, "");
			time(&wtmp.ut_time);
			write(f, (char *)&wtmp, sizeof(wtmp));
			found++;
		}
		close(f);
	}
	if (found) {
		f = open(wtmpf, 1);
		if (f >= 0) {
			SCPYN(wtmp.ut_line, line+5);
			SCPYN(wtmp.ut_name, "");
			time(&wtmp.ut_time);
			lseek(f, (long)0, 2);
			write(f, (char *)&wtmp, sizeof(wtmp));
			close(f);
		}
	}
	chmod(line, 0666);
	chown(line, 0, 0);
	line[strlen("/dev/")] = 'p';
	chmod(line, 0666);
	chown(line, 0, 0);
}
