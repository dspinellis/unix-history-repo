#ifndef lint
static char sccsid[] = "@(#)ns_main.c	4.3 (Berkeley) 5/30/86";
#endif

/*
 * Copyright (c) 1986 Regents of the University of California
 *	All Rights Reserved
 */

/*
 * Internet Name server (see rfc883 & others).
 */

#include <sys/param.h>
#include <sys/file.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/resource.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <syslog.h>
#include <errno.h>
#include <signal.h>
#include <arpa/nameser.h>
#include <arpa/inet.h>
#include "ns.h"
#include "db.h"

#ifdef BOOTFILE 			/* default boot file */
char	*bootfile = BOOTFILE;
#else
char	*bootfile = "/etc/named.boot";
#endif

#ifdef DEBUGFILE 			/* default debug output file */
char	*debugfile = DEBUGFILE;
#else
char	*debugfile = "/usr/tmp/named.run";
#endif

#ifdef PIDFILE 				/* file to store current named PID */
char	*PidFile = PIDFILE;
#else
char	*PidFile = "/etc/named.pid";	
#endif

#ifndef FD_SET
#define	NFDBITS		32
#define	FD_SETSIZE	32
#define	FD_SET(n, p)	((p)->fds_bits[(n)/NFDBITS] |= (1 << ((n) % NFDBITS)))
#define	FD_CLR(n, p)	((p)->fds_bits[(n)/NFDBITS] &= ~(1 << ((n) % NFDBITS)))
#define	FD_ISSET(n, p)	((p)->fds_bits[(n)/NFDBITS] & (1 << ((n) % NFDBITS)))
#define FD_ZERO(p)	bzero((char *)(p), sizeof(*(p)))
#endif

FILE	*fp;  				/* file descriptor for pid file */

#ifdef DEBUG
FILE	*ddt;
#endif

int	debug = 0;			/* debugging flag */
int	ds;				/* datagram socket */
int	read_interrupted = 0;		/* flag for read timer */
int	needreload = 0;			/* received SIGHUP, need to reload db */
int	needmaint = 0;			/* need to call ns_maint()*/
int	rbufsize = 8 * 1024;		/* UDP recive buffer size */

struct	qstream *streamq = QSTREAM_NULL; /* list of open streams */
struct	sockaddr_in nsaddr;
struct	timeval tt;
short	ns_port;

char		**Argv = NULL;		/* pointer to argument vector */
char		*LastArg = NULL;	/* end of argv */

extern char *malloc(), *realloc(), *calloc();

extern int errno;



main(argc, argv, envp)
	int argc;
	char *argv[], *envp[];
{
	register int n, udpcnt;
	register char *arg;
	register struct qstream *sp;
	int vs, len;
	int nfds;
	int on = 1;
	int rfd, size;
	u_long lasttime, maxctime;
	char buf[BUFSIZ];

	fd_set mask, tmpmask;

	struct timeval t, *tp;
	struct sockaddr_in from;
	struct qstream *candidate = QSTREAM_NULL;
	extern int onintr(), maint_alarm(), reapchild(), doadump(), onhup();
	extern int sigsetdebug(), signodebug(), sigprof();
	extern struct qstream *sqadd();
	extern char Version[];
	struct	sigvec sv;

	ns_port = htons(NAMESERVER_PORT);

	/*
	**  Save start and extent of argv for setproctitle.
	*/

	Argv = argv;
	if (envp == 0 || *envp == 0)
		envp = argv;
	while (*envp)
		envp++;
	LastArg = envp[-1] + strlen(envp[-1]);

	while (--argc > 0) {
		arg = *++argv;
		if (*arg == '-') {
			while (*++arg)
				switch (*arg) {
				case 'b':
					if (--argc <= 0)
						usage();
					bootfile = *++argv;
					break;

  				case 'd':
 					++argv;

 					if (*argv != 0) {
 					    if (**argv == '-') {
 						argv--;
 						break;
 					    }
 					    debug = atoi(*argv);
 					    --argc;
 					}
					if (debug <= 0)
						debug = 1;
					setdebug(1);
					break;

				case 'p':
					if (--argc <= 0)
						usage();
					ns_port = htons((u_short)atoi(*++argv));
					break;

				default:
					usage();
				}
		} else
			bootfile = *argv;
	}

	if (!debug) {
		if (fork())
			exit(0);
		for (n = getdtablesize() - 1; n >= 0; n--)
			(void) close(n);
		(void) open("/dev/null", O_RDONLY);
		(void) dup2(0, 1);
		(void) dup2(0, 2);
		n = open("/dev/tty", O_RDWR);
		if (n > 0) {
			(void) ioctl(n, TIOCNOTTY, (char *)NULL);
			(void) close(n);
		}
	}
#ifdef DEBUG
	else {
		fprintf(ddt,"Debug turned ON, Level %d\n",debug);
		fprintf(ddt,"Version = %s\t",Version);
		fprintf(ddt,"bootfile = %s\n",bootfile);
	}		
#endif

#ifdef BSD4_3
	openlog("named", LOG_PID|LOG_CONS|LOG_NDELAY, LOG_DAEMON);
#else
	openlog("named", LOG_PID);
#endif

	nsaddr.sin_family = AF_INET;
	nsaddr.sin_addr.s_addr = INADDR_ANY;
	nsaddr.sin_port = ns_port;
	/*
	** Initialize and load database.
	*/
	ns_init(bootfile);

	/* Block signals during maintenance */
	sv.sv_handler = maint_alarm;
	sv.sv_onstack = 0;
	sv.sv_mask = ~0;

	(void) sigvec(SIGALRM, &sv, (struct sigvec *)0);

	(void) signal(SIGHUP, onhup);
	(void) signal(SIGCHLD, reapchild);
	(void) signal(SIGPIPE, SIG_IGN);
	(void) signal(SIGSYS, sigprof);

#if	BSD >= 43
	/* flames to mckusick@monet.Berkeley.EDU - I lost the battle -KJD */
	(void) signal(SIGINT, doadump);
	(void) signal(SIGUSR1, sigsetdebug);
	(void) signal(SIGUSR2, signodebug);
#else	BSD
	(void) signal(SIGQUIT, doadump);
	(void) signal(SIGEMT, sigsetdebug);
	(void) signal(SIGFPE, signodebug);
#endif BSD

#ifdef DEBUG
	if (debug) {
		fprintf(ddt,"database initialized\n");
	}
#endif
	/*
	** Open stream port.
	*/
	if ((vs = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		syslog(LOG_ERR, "socket(SOCK_STREAM): %m");
		exit(1);
	}	
	(void)setsockopt(vs, SOL_SOCKET, SO_REUSEADDR, (char *)&on, sizeof(on));
	if (bind(vs, &nsaddr, sizeof(nsaddr))) {
		syslog(LOG_ERR, "bind(vs): %m");
		exit(1);
	}
	(void) listen(vs, 5);
	/*
	** Open datagram port.
	*/
	if ((ds = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
		syslog(LOG_ERR, "socket(SOCK_DGRAM): %m");
		exit(1);
	}	
	(void)setsockopt(ds, SOL_SOCKET, SO_REUSEADDR, (char *)&on, sizeof(on));
#ifdef BSD4_3
	(void)setsockopt(ds, SOL_SOCKET, SO_RCVBUF, (char *)&rbufsize,
	    sizeof(rbufsize));
#endif
	(void) fcntl(ds, F_SETFL, FNDELAY);
	if (bind(ds, &nsaddr, sizeof(nsaddr))) {
		syslog(LOG_ERR, "bind(ds): %m");
		exit(1);
	}
	/* tuck my process id away */
	fp = fopen(PidFile, "w");
	if (fp != NULL) {
		fprintf(fp, "%d\n", getpid());
		(void) fclose(fp);
	}

	t.tv_usec = 0;

#ifdef DEBUG
	if (debug)
		fprintf(ddt,"Ready to answer queries.\n");
#endif
	nfds = getdtablesize();       /* get the number of file descriptors */
	if (nfds > FD_SETSIZE) {
		syslog(LOG_ERR, "Return from getdtablesize() > FD_SETSIZE");
#ifdef DEBUG
		if (debug)
		      fprintf(ddt,"Return from getdtablesize() > FD_SETSIZE\n");
#endif
	}
	FD_ZERO(&mask);
	FD_SET(vs, &mask);
	FD_SET(ds, &mask);
	for (;;) {
		/*
		** Wait until a query arrives; can be interrupted by maintenance
		*/
		if (retryqp != NULL) {
			if (gettimeofday(&tt, (struct timezone *)0) < 0)
				syslog(LOG_ERR, "gettimeofday failed: %m");
			t.tv_sec = (long) retryqp->q_time - tt.tv_sec;
			if (t.tv_sec <= 0) {
				retry(retryqp);
				continue;
			}
			tp = &t;
		} else
			tp = NULL;
		if(needreload) {
			needreload = 0;
			db_reload();
		}
		if(needmaint) {
			needmaint = 0;
			ns_maint();
		}
		tmpmask = mask;
		n = select(nfds, &tmpmask, (fd_set *)NULL, (fd_set *)NULL, tp);
		if (n < 0) {
			if (errno == EINTR)
				continue;
			syslog(LOG_ERR, "select: %m");
			break;
		}
		if (n == 0) {
			retry(retryqp);
			continue;
		}
		if (gettimeofday(&tt, (struct timezone *)0) < 0) {
			syslog(LOG_ERR, "gettimeofday failed: %m");
			break;
		}
		/*
		** Process datagram
		*/
		if (FD_ISSET(ds, &tmpmask))
		    for(udpcnt = 0; udpcnt < 25; udpcnt++) {
			len = sizeof(from);
			if ((n = recvfrom(ds, buf, sizeof(buf), 0,
				&from, &len)) < 0)
			{
				if ((n == -1) && (errno == EWOULDBLOCK))
					break;
				syslog(LOG_WARNING, "recvfrom: %m");
				break;
			}
#ifdef DEBUG
			if (debug)
				fprintf(ddt,"datagram from %s, %d (%d)\n",
					inet_ntoa(from.sin_addr),
					ntohs(from.sin_port), n);
			if (debug >= 10)
				fp_query(buf, ddt);
#endif
			/*
			 * Consult database to get the answer.
			 */
			if (gettimeofday(&tt, (struct timezone *)0) < 0) {
				syslog(LOG_ERR, "gettimeofday failed: %m");
				break;
			}
			ns_req(buf, n, PACKETSZ, QSTREAM_NULL, &from);
		    }
		/*
		** Process stream connection
		*/
		if (FD_ISSET(vs, &tmpmask)) {
			len = sizeof(from);
			rfd = accept(vs, &from, &len);
			if (gettimeofday(&tt, (struct timezone *)0) < 0) {
				syslog(LOG_ERR, "gettimeofday failed: %m");
				break;
			}
			if (rfd < 0) {
			    if (errno == EMFILE) {
				if (streamq != NULL) {
				    maxctime = 0;
				    candidate = QSTREAM_NULL;
				    for (sp = streamq; sp != QSTREAM_NULL;
				       sp = sp->s_next)
				    {
					if (sp->s_refcnt != 0)
					    continue;
					lasttime = tt.tv_sec - sp->s_time;
					if (lasttime >= 900)
					    sqrm(sp, &tmpmask);
					else if (lasttime > maxctime) {
					    candidate = sp;
					    maxctime = lasttime;
					}
				    }
				    rfd = accept(vs, &from, &len);
				    if ((rfd < 0) && (errno == EMFILE))
					if (candidate != QSTREAM_NULL) {
					    sqrm(candidate, &tmpmask);
				    	    rfd = accept(vs, &from, &len);
				   	    if (rfd < 0)
					        syslog(LOG_WARNING,
						    "accept: %m");
						continue;
					}
				} else {
					syslog(LOG_WARNING, "accept: %m");
					continue;
				}
			    } else {
				syslog(LOG_WARNING, "accept: %m");
				continue;
			    }
			}
			(void) fcntl(rfd, F_SETFL, FNDELAY);
			(void) setsockopt(rfd, SOL_SOCKET, SO_KEEPALIVE,
				(char *)&on, sizeof(on));
			if ((sp = sqadd()) == QSTREAM_NULL)
				(void) close(rfd);
			sp->s_rfd = rfd;	/* stream file descriptor */
			sp->s_size = -1;	/* amount of data to recive */
			if (gettimeofday(&tt, (struct timezone *)0) < 0) {
				syslog(LOG_ERR, "gettimeofday failed: %m");
				break;
			}
			sp->s_time = tt.tv_sec;	/* last transaction time */
			sp->s_from = from;	/* address to respond to */
			sp->s_bufsize = 0;
			sp->s_bufp = (char *)&sp->s_tempsize;
			sp->s_refcnt = 0;
			FD_SET(rfd, &mask);
			FD_SET(rfd, &tmpmask);
#ifdef DEBUG
			if (debug)
			{
				fprintf(ddt,"stream from %s, %d (%d)\n",
					inet_ntoa(sp->s_from.sin_addr),
					ntohs(sp->s_from.sin_port), n);
			}
#endif
		}
#ifdef DEBUG
		if (debug > 2)
			fprintf(ddt,"streamq  = x%x\n",streamq);
#endif
		if (streamq != NULL) {
			for (sp = streamq; sp != QSTREAM_NULL; sp = sp->s_next)
			    if (FD_ISSET(sp->s_rfd, &tmpmask)) {
#ifdef DEBUG
				if (debug > 5) {
				    fprintf(ddt,
					"sp x%x rfd %d size %d time %d ",
					sp, sp->s_rfd, sp->s_size,
					sp->s_time );
				    fprintf(ddt," next x%x \n", sp->s_next );
				    fprintf(ddt,"\tbufsize %d",sp->s_bufsize);
				    fprintf(ddt," buf x%x%d ",sp->s_buf);
				    fprintf(ddt," bufp x%x%d\n",sp->s_bufp);
				}
#endif DEBUG
			    if (sp->s_size < 0) {
			        size = sizeof(u_short) -
				(sp->s_bufp - (char *)&sp->s_tempsize);
			        while (size > 0 &&
			           (n = read(sp->s_rfd, sp->s_bufp, size)) > 0){
					    sp->s_bufp += n;
					    size -= n;
			        }
			        if ((n == -1) && (errno == EWOULDBLOCK))
					    continue;
			        if (n <= 0) {
					    sp->s_refcnt = 0;
					    sqrm(sp, &mask);
					    continue;
			        }
			        if ((sp->s_bufp - (char *)&sp->s_tempsize) ==
					sizeof(u_short)) {
					sp->s_size = htons(sp->s_tempsize);
					if (sp->s_bufsize == 0) {
					    if ( (sp->s_buf = malloc(BUFSIZ))
						== NULL) {
						    sp->s_buf = buf;
						    sp->s_size  = sizeof(buf);
					    } else {
						    sp->s_bufsize = BUFSIZ;
					    }
					}
					if (sp->s_size > sp->s_bufsize &&
					  sp->s_bufsize != 0) {
					    if ((sp->s_buf = realloc(
						(char *)sp->s_buf,
						(unsigned)sp->s_size)) == NULL){
						    sp->s_buf = buf;
						    sp->s_bufsize = 0;
						    sp->s_size  = sizeof(buf);
					   } else {
						    sp->s_bufsize = sp->s_size;
					   }
					}
					sp->s_bufp = sp->s_buf;	
				    }
			    }
			    if (gettimeofday(&tt, (struct timezone *)0) < 0) {
				    syslog(LOG_ERR, "gettimeofday failed: %m");
				    break;
			    }
			    sp->s_time = tt.tv_sec;
			    while (sp->s_size > 0 &&
			      (n = read(sp->s_rfd, sp->s_buf, sp->s_size)) > 0)
			    {
				    sp->s_bufp += n;
				    sp->s_size -= n;
			    }
			    /*
			     * we don't have enough memory for the query.
			     * if we have a query id, then we will send an
			     * error back to the user.
			     */
			    if (sp->s_bufsize == 0 &&
				(sp->s_bufp - sp->s_buf > sizeof(u_short))) {
				    HEADER *hp;

				    hp = (HEADER *)sp->s_buf;
				    hp->qr = 1;
				    hp->ra = 1;
				    hp->ancount = 0;
				    hp->qdcount = 0;
				    hp->nscount = 0;
				    hp->arcount = 0;
				    hp->rcode = SERVFAIL;
				    (void) writemsg(sp->s_rfd, sp->s_buf,
					sizeof(HEADER));
				    continue;
			    }
			    if ((n == -1) && (errno == EWOULDBLOCK))
				    continue;
			    if (n <= 0) {
				    sp->s_refcnt = 0;
				    sqrm(sp, &mask);
				    continue;
			    }
			    /*
			     * Consult database to get the answer.
			     */
			    if (sp->s_size == 0) {
				    sp->s_refcnt++;
				    ns_req(sp->s_buf,
					sp->s_bufp - sp->s_buf,
					sp->s_bufsize, sp,
					&sp->s_from);
				    sp->s_bufp = (char *)&sp->s_tempsize;
				    sp->s_size = -1;
				    continue;
			    }
		    }
		}
	}
	(void) close(vs);
	(void) close(ds);
	return (0);
}

usage()
{
	fprintf(stderr, "Usage: named [-d #] [-p port] [{-b} bootfile]\n");
	exit(1);
}

/*
** Set flag saying to reload database upon receiving SIGHUP.
** Must make sure that someone isn't walking through a data
** structure at the time.
*/

onhup()
{
	needreload = 1;
}

/*
** Set flag saying to call ns_maint()
** Must make sure that someone isn't walking through a data
** structure at the time.
*/

maint_alarm()
{
	needmaint = 1;
}

/*
** Set flag saying to read was interrupted
** used for a read timer
*/

read_alarm()
{
	extern int read_interrupted;
	read_interrupted = 1;
}

reapchild()
{
	union wait status;

	while (wait3(&status, WNOHANG, (struct rusage *)NULL) > 0)
		;
}

/*
** Turn on or off debuging by open or closeing the debug file
*/

setdebug(code)
int code;
{
#if defined(lint) && !defined(DEBUG)
	code = code;
#endif
#ifdef DEBUG

	if (code) {
		ddt = freopen(debugfile, "w+", stderr);
		if ( ddt == NULL)
			syslog(LOG_WARNING, "can't open debug file: %m");
		else
			setlinebuf(ddt);
	}
	else {
		fprintf(ddt,"Debug turned OFF, Level %d\n",debug);
		(void) fclose(ddt);
		debug = 0;
	}
#endif
}

/*
** Catch a special signal  SIGEMT and set debug level
**
**  SIGEMT - if debuging is off then turn on debuging else incremnt the level
**
** Handy for looking in on long running name servers.
*/

sigsetdebug()
{

#ifdef DEBUG
	if (debug == 0) {
		debug++;
		setdebug(1);
	}
	else {
		debug++;
	}
	fprintf(ddt,"Debug turned ON, Level %d\n",debug);
#endif
}

/*
** Catch a special signal's SIGFPE and turn off debugging
**
**  SIGFPE - turn off debugging
*/

signodebug()
{
	setdebug(0);
}


/*
** Catch a special signal SIGSYS
**
**  this is setup to fork and exit to drop to /usr/tmp/gmon.out
**   and keep the server running
*/

sigprof()
{
#ifdef DEBUG
	if (debug)
		fprintf(ddt,"sigprof()\n");
#endif
	if ( fork() == 0)
	{
		(void) chdir("/usr/tmp");
		exit(1);
	}
}

/*
** Routines for managing stream queue
*/

struct qstream *
sqadd()
{
	register struct qstream *sqp;

	if ((sqp = (struct qstream *)calloc(1, sizeof(struct qstream)))
	    == NULL ) {
#ifdef DEBUG
		if (debug >= 5)
			fprintf(ddt,"sqadd: malloc error\n");
#endif
		syslog(LOG_ERR, "sqadd: Out Of Memory");
		return(QSTREAM_NULL);
	}
#ifdef DEBUG
	if (debug > 3)
		fprintf(ddt,"sqadd(x%x)\n", sqp);
#endif

	sqp->s_next = streamq;
	streamq = sqp;
	return(sqp);
}

sqrm(qp, mask)
	register struct qstream *qp;
	fd_set *mask;
{
	register struct qstream *qsp;

#ifdef DEBUG
	if (debug > 1) {
		fprintf(ddt,"sqrm(%#x, %d ) rfcnt=%d\n",
		    qp, qp->s_rfd, qp->s_refcnt);
	}
#endif
	if (qp->s_refcnt != 0)
		return;

	if (qp->s_bufsize != 0)
		(void) free(qp->s_buf);
	FD_CLR(qp->s_rfd, mask);
	(void) close(qp->s_rfd);
	if (qp == streamq) {
		streamq = qp->s_next;
	} else {
		for (qsp = streamq; qsp->s_next != qp; qsp = qsp->s_next)
			;
		qsp->s_next = qp->s_next;
	}
	(void)free((char *)qp);
}

setproctitle(a, s)
	char *a;
	int s;
{
	int size;
	register char *cp;
	struct sockaddr_in sin;
	char buf[80];

	cp = Argv[0];
	size = sizeof(sin);
	if (getpeername(s, &sin, &size) == 0)
		(void) sprintf(buf, "-%s [%s]", a, inet_ntoa(sin.sin_addr));
	else
		(void) sprintf(buf, "-%s", a);
	(void) strncpy(cp, buf, LastArg - cp);
	cp += strlen(cp);
	while (cp < LastArg)
		*cp++ = ' ';
}
