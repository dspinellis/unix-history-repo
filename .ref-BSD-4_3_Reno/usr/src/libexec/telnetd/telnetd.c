/*
 * Copyright (c) 1989 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)telnetd.c	5.46 (Berkeley) 6/28/90";
#endif /* not lint */

#include "telnetd.h"

/*
 * I/O data buffers,
 * pointers, and counters.
 */
char	ptyibuf[BUFSIZ], *ptyip = ptyibuf;
char	ptyibuf2[BUFSIZ];

#ifdef	CRAY
int	hostinfo = 1;			/* do we print login banner? */
#endif

#ifdef	CRAY
extern int      newmap; /* nonzero if \n maps to ^M^J */
int	lowpty = 0, highpty;	/* low, high pty numbers */
#endif /* CRAY */

int debug = 0;
char *progname;

#if	defined(NEED_GETTOS)
struct tosent {
	char	*t_name;	/* name */
	char	**t_aliases;	/* alias list */
	char	*t_proto;	/* protocol */
	int	t_tos;		/* Type Of Service bits */
};

struct tosent *
gettosbyname(name, proto)
char *name, *proto;
{
	static struct tosent te;
	static char *aliasp = 0;

	te.t_name = name;
	te.t_aliases = &aliasp;
	te.t_proto = proto;
	te.t_tos = 020;	/* Low Delay bit */
	return(&te);
}
#endif

main(argc, argv)
	char *argv[];
{
	struct sockaddr_in from;
	int on = 1, fromlen;
#if	defined(HAS_IP_TOS) || defined(NEED_GETTOS)
	struct tosent *tp;
#endif /* defined(HAS_IP_TOS) || defined(NEED_GETTOS) */

	pfrontp = pbackp = ptyobuf;
	netip = netibuf;
	nfrontp = nbackp = netobuf;

	progname = *argv;

#ifdef CRAY
	/*
	 * Get number of pty's before trying to process options,
	 * which may include changing pty range.
	 */
	highpty = getnpty();
#endif /* CRAY */

top:
	argc--, argv++;

	if (argc > 0 && strcmp(*argv, "-debug") == 0) {
		debug++;
		goto top;
	}

#ifdef	LINEMODE
	if (argc > 0 && !strcmp(*argv, "-l")) {
		alwayslinemode = 1;
		goto top;
	}
#endif	/* LINEMODE */

#ifdef CRAY
	if (argc > 0 && !strcmp(*argv, "-h")) {
		hostinfo = 0;
		goto top;
	}

	if (argc > 0 && !strncmp(*argv, "-r", 2)) {
		char *strchr();
		char *c;

		/*
		 * Allow the specification of alterations to the pty search
		 * range.  It is legal to specify only one, and not change the
		 * other from its default.
		 */
		*argv += 2;
		if (**argv == '\0' && argc)
			argv++, argc--;
		c = strchr(*argv, '-');
		if (c) {
			*c++ = '\0';
			highpty = atoi(c);
		}
		if (**argv != '\0')
			lowpty = atoi(*argv);
		if ((lowpty > highpty) || (lowpty < 0) || (highpty > 32767)) {
			usage();
			/* NOT REACHED */
		}
		goto top;
	}
# ifdef	NEWINIT
	if (argc > 0 && !strncmp(*argv, "-I", 2)) {
		extern char *gen_id;

		*argv += 2;
		if (**argv == '\0') {
			if (argc < 2) {
				usage();
				/* NOT REACHED */
			}
			argv++, argc--;
			if (**argv == '\0') {
				usage();
				/* NOT REACHED */
			}
		}
		gen_id = *argv;
		goto top;
	}
# endif	/* NEWINIT */
#endif	/* CRAY */

#ifdef DIAGNOSTICS
	/*
	 * Check for desired diagnostics capabilities.
	 */
	if (argc > 0 && !strncmp(*argv, "-D", 2)) {
		*argv += 2;
		if (**argv == '\0') {
			if (argc < 2) {
				usage();
				/* NOT REACHED */
			}
			argv++, argc--;
			if (**argv == '\0') {
				usage();
				/* NOT REACHED */
			}
		}
		if (!strcmp(*argv, "report")) {
			diagnostic |= TD_REPORT|TD_OPTIONS;
		} else if (!strcmp(*argv, "exercise")) {
			diagnostic |= TD_EXERCISE;
		} else if (!strcmp(*argv, "netdata")) {
			diagnostic |= TD_NETDATA;
		} else if (!strcmp(*argv, "ptydata")) {
			diagnostic |= TD_PTYDATA;
		} else if (!strcmp(*argv, "options")) {
			diagnostic |= TD_OPTIONS;
		} else {
			usage();
			/* NOT REACHED */
		}
		goto top;
	}
#endif /* DIAGNOSTICS */

#ifdef BFTPDAEMON
	/*
	 * Check for bftp daemon
	 */
	if (argc > 0 && !strncmp(*argv, "-B", 2)) {
		bftpd++;
		goto top;
	}
#endif /* BFTPDAEMON */

	if (argc > 0 && **argv == '-') {
		fprintf(stderr, "telnetd: %s: unknown option\n", *argv+1);
		usage();
		/* NOT REACHED */
	}

	if (debug) {
	    int s, ns, foo;
	    struct servent *sp;
	    static struct sockaddr_in sin = { AF_INET };

	    if (argc > 1) {
		usage();
		/* NOT REACHED */
	    } else if (argc == 1) {
		    if (sp = getservbyname(*argv, "tcp")) {
			sin.sin_port = sp->s_port;
		    } else {
			sin.sin_port = atoi(*argv);
			if ((int)sin.sin_port <= 0) {
			    fprintf(stderr, "telnetd: %s: bad port #\n", *argv);
			    usage();
			    /* NOT REACHED */
			}
			sin.sin_port = htons((u_short)sin.sin_port);
		   }
	    } else {
		sp = getservbyname("telnet", "tcp");
		if (sp == 0) {
		    fprintf(stderr, "telnetd: tcp/telnet: unknown service\n");
		    exit(1);
		}
		sin.sin_port = sp->s_port;
	    }

	    s = socket(AF_INET, SOCK_STREAM, 0);
	    if (s < 0) {
		    perror("telnetd: socket");;
		    exit(1);
	    }
	    (void) setsockopt(s, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));
	    if (bind(s, (struct sockaddr *)&sin, sizeof sin) < 0) {
		perror("bind");
		exit(1);
	    }
	    if (listen(s, 1) < 0) {
		perror("listen");
		exit(1);
	    }
	    foo = sizeof sin;
	    ns = accept(s, (struct sockaddr *)&sin, &foo);
	    if (ns < 0) {
		perror("accept");
		exit(1);
	    }
	    (void) dup2(ns, 0);
	    (void) close(ns);
	    (void) close(s);
	} else if (argc > 0) {
		usage();
		/* NOT REACHED */
	}

	openlog("telnetd", LOG_PID | LOG_ODELAY, LOG_DAEMON);
	fromlen = sizeof (from);
	if (getpeername(0, (struct sockaddr *)&from, &fromlen) < 0) {
		fprintf(stderr, "%s: ", progname);
		perror("getpeername");
		_exit(1);
	}
	if (setsockopt(0, SOL_SOCKET, SO_KEEPALIVE, &on, sizeof (on)) < 0) {
		syslog(LOG_WARNING, "setsockopt (SO_KEEPALIVE): %m");
	}

#if	defined(HAS_IP_TOS) || defined(NEED_GETTOS)
	if ((tp = gettosbyname("telnet", "tcp")) &&
	    (setsockopt(0, IPPROTO_IP, IP_TOS, &tp->t_tos, sizeof(int)) < 0))
		syslog(LOG_WARNING, "setsockopt (IP_TOS): %m");
#endif	/* defined(HAS_IP_TOS) || defined(NEED_GETTOS) */
	net = 0;
	doit(&from);
	/* NOTREACHED */
}  /* end of main */

usage()
{
	fprintf(stderr, "Usage: telnetd [-debug] [-h]");
#ifdef	NEWINIT
	fprintf(stderr, " [-Iinitid]");
#endif	/* NEWINIT */
#ifdef DIAGNOSTICS
	fprintf(stderr, " [-D (options|report|exercise|netdata|ptydata)]");
#endif /* DIAGNOSTICS */
#ifdef LINEMODE
	fprintf(stderr, " [-l]");
#endif
#ifdef	CRAY
	fprintf(stderr, " [-r[lowpty]-[highpty]]");
#endif
#ifdef BFTPDAEMON
	fprintf(stderr, " [-B]");
#endif /* BFTPDAEMON */
	fprintf(stderr, " [port]\n");
	exit(1);
}

void	cleanup();

/*
 * getterminaltype
 *
 *	Ask the other end to send along its terminal type and speed.
 * Output is the variable terminaltype filled in.
 */
static char ttytype_sbbuf[] = { IAC, SB, TELOPT_TTYPE, TELQUAL_SEND, IAC, SE };
void
getterminaltype()
{
    void ttloop();

    settimer(baseline);
    send_do(TELOPT_TTYPE, 1);
    send_do(TELOPT_TSPEED, 1);
    send_do(TELOPT_XDISPLOC, 1);
    send_do(TELOPT_ENVIRON, 1);
    while (his_will_wont_is_changing(TELOPT_TTYPE) ||
	   his_will_wont_is_changing(TELOPT_TSPEED) ||
	   his_will_wont_is_changing(TELOPT_XDISPLOC) ||
	   his_will_wont_is_changing(TELOPT_ENVIRON)) {
	ttloop();
    }
    if (his_state_is_will(TELOPT_TSPEED)) {
	static char sbbuf[] = { IAC, SB, TELOPT_TSPEED, TELQUAL_SEND, IAC, SE };

	bcopy(sbbuf, nfrontp, sizeof sbbuf);
	nfrontp += sizeof sbbuf;
    }
    if (his_state_is_will(TELOPT_XDISPLOC)) {
	static char sbbuf[] = { IAC, SB, TELOPT_XDISPLOC, TELQUAL_SEND, IAC, SE };

	bcopy(sbbuf, nfrontp, sizeof sbbuf);
	nfrontp += sizeof sbbuf;
    }
    if (his_state_is_will(TELOPT_ENVIRON)) {
	static char sbbuf[] = { IAC, SB, TELOPT_ENVIRON, TELQUAL_SEND, IAC, SE };

	bcopy(sbbuf, nfrontp, sizeof sbbuf);
	nfrontp += sizeof sbbuf;
    }
    if (his_state_is_will(TELOPT_TTYPE)) {

	bcopy(ttytype_sbbuf, nfrontp, sizeof ttytype_sbbuf);
	nfrontp += sizeof ttytype_sbbuf;
    }
    if (his_state_is_will(TELOPT_TSPEED)) {
	while (sequenceIs(tspeedsubopt, baseline))
	    ttloop();
    }
    if (his_state_is_will(TELOPT_XDISPLOC)) {
	while (sequenceIs(xdisplocsubopt, baseline))
	    ttloop();
    }
    if (his_state_is_will(TELOPT_ENVIRON)) {
	while (sequenceIs(environsubopt, baseline))
	    ttloop();
    }
    if (his_state_is_will(TELOPT_TTYPE)) {
	char first[256], last[256];

	while (sequenceIs(ttypesubopt, baseline))
	    ttloop();

	/*
	 * If the other side has already disabled the option, then
	 * we have to just go with what we (might) have already gotten.
	 */
	if (his_state_is_will(TELOPT_TTYPE) && !terminaltypeok(terminaltype)) {
	    (void) strncpy(first, terminaltype, sizeof(first));
	    for(;;) {
		/*
		 * Save the unknown name, and request the next name.
		 */
		(void) strncpy(last, terminaltype, sizeof(last));
		_gettermname();
		if (terminaltypeok(terminaltype))
		    break;
		if ((strncmp(last, terminaltype, sizeof(last)) == 0) ||
		    his_state_is_wont(TELOPT_TTYPE)) {
		    /*
		     * We've hit the end.  If this is the same as
		     * the first name, just go with it.
		     */
		    if (strncmp(first, terminaltype, sizeof(first) == 0))
			break;
		    /*
		     * Get the terminal name one more time, so that
		     * RFC1091 compliant telnets will cycle back to
		     * the start of the list.
		     */
		     _gettermname();
		    if (strncmp(first, terminaltype, sizeof(first) != 0))
			(void) strncpy(terminaltype, first, sizeof(first));
		    break;
		}
	    }
	}
    }
}  /* end of getterminaltype */

_gettermname()
{
    /*
     * If the client turned off the option,
     * we can't send another request, so we
     * just return.
     */
    if (his_state_is_wont(TELOPT_TTYPE))
	return;
    settimer(baseline);
    bcopy(ttytype_sbbuf, nfrontp, sizeof ttytype_sbbuf);
    nfrontp += sizeof ttytype_sbbuf;
    while (sequenceIs(ttypesubopt, baseline))
	ttloop();
}

terminaltypeok(s)
char *s;
{
    char buf[1024];

    if (terminaltype == NULL)
	return(1);

    /*
     * tgetent() will return 1 if the type is known, and
     * 0 if it is not known.  If it returns -1, it couldn't
     * open the database.  But if we can't open the database,
     * it won't help to say we failed, because we won't be
     * able to verify anything else.  So, we treat -1 like 1.
     */
    if (tgetent(buf, s) == 0)
	return(0);
    return(1);
}

/*
 * Get a pty, scan input lines.
 */
doit(who)
	struct sockaddr_in *who;
{
	char *host, *inet_ntoa();
	int t;
	struct hostent *hp;
#if BSD > 43
	extern char *line;

	if (openpty(&pty, &t, line, NULL, NULL) == -1)
		fatal(net, "All network ports in use");
	init_termbuf();
#else
	
	/*
	 * Find an available pty to use.
	 */
	pty = getpty();
	if (pty < 0)
		fatal(net, "All network ports in use");

	t = getptyslave();
#endif

	/* get name of connected client */
	hp = gethostbyaddr((char *)&who->sin_addr, sizeof (struct in_addr),
		who->sin_family);
	if (hp)
		host = hp->h_name;
	else
		host = inet_ntoa(who->sin_addr);

	init_env();
	/*
	 * get terminal type.
	 */
	getterminaltype();
	setenv("TERM", terminaltype ? terminaltype : "network", 1);

	/*
	 * Start up the login process on the slave side of the terminal
	 */
	startslave(t, host);

	telnet(net, pty);  /* begin server processing */
	/*NOTREACHED*/
}  /* end of doit */

#ifndef	MAXHOSTNAMELEN
#define	MAXHOSTNAMELEN 64
#endif	MAXHOSTNAMELEN
/*
 * Main loop.  Select from pty and network, and
 * hand data to telnet receiver finite state machine.
 */
telnet(f, p)
int f, p;
{
	int on = 1;
	char hostname[MAXHOSTNAMELEN];
#if	defined(CRAY2) && defined(UNICOS5)
	int termstat();
	int interrupt(), sendbrk();
#endif
#define	TABBUFSIZ	512
	char	defent[TABBUFSIZ];
	char	defstrs[TABBUFSIZ];
#undef	TABBUFSIZ
	char *HE;
	char *HN;
	char *IM;
	void netflush();
	
	/*
	 * Initialize the slc mapping table.
	 */
	get_slc_defaults();

	/*
	 * Do some tests where it is desireable to wait for a response.
	 * Rather than doing them slowly, one at a time, do them all
	 * at once.
	 */
	if (my_state_is_wont(TELOPT_SGA))
		send_will(TELOPT_SGA, 1);
	/*
	 * Is the client side a 4.2 (NOT 4.3) system?  We need to know this
	 * because 4.2 clients are unable to deal with TCP urgent data.
	 *
	 * To find out, we send out a "DO ECHO".  If the remote system
	 * answers "WILL ECHO" it is probably a 4.2 client, and we note
	 * that fact ("WILL ECHO" ==> that the client will echo what
	 * WE, the server, sends it; it does NOT mean that the client will
	 * echo the terminal input).
	 */
	send_do(TELOPT_ECHO, 1);

#ifdef	LINEMODE
	if (his_state_is_wont(TELOPT_LINEMODE)) {
		/* Query the peer for linemode support by trying to negotiate
		 * the linemode option.
		 */
		linemode = 0;
		editmode = 0;
		send_do(TELOPT_LINEMODE, 1);  /* send do linemode */
	}
#endif	/* LINEMODE */

	/*
	 * Send along a couple of other options that we wish to negotiate.
	 */
	send_do(TELOPT_NAWS, 1);
	send_will(TELOPT_STATUS, 1);
	flowmode = 1;  /* default flow control state */
	send_do(TELOPT_LFLOW, 1);

	/*
	 * Spin, waiting for a response from the DO ECHO.  However,
	 * some REALLY DUMB telnets out there might not respond
	 * to the DO ECHO.  So, we spin looking for NAWS, (most dumb
	 * telnets so far seem to respond with WONT for a DO that
	 * they don't understand...) because by the time we get the
	 * response, it will already have processed the DO ECHO.
	 * Kludge upon kludge.
	 */
	while (his_will_wont_is_changing(TELOPT_NAWS))
		ttloop();

	/*
	 * But...
	 * The client might have sent a WILL NAWS as part of its
	 * startup code; if so, we'll be here before we get the
	 * response to the DO ECHO.  We'll make the assumption
	 * that any implementation that understands about NAWS
	 * is a modern enough implementation that it will respond
	 * to our DO ECHO request; hence we'll do another spin
	 * waiting for the ECHO option to settle down, which is
	 * what we wanted to do in the first place...
	 */
	if (his_want_state_is_will(TELOPT_ECHO) &&
	    his_state_is_will(TELOPT_NAWS)) {
		while (his_will_wont_is_changing(TELOPT_ECHO))
			ttloop();
	}
	/*
	 * On the off chance that the telnet client is broken and does not
	 * respond to the DO ECHO we sent, (after all, we did send the
	 * DO NAWS negotiation after the DO ECHO, and we won't get here
	 * until a response to the DO NAWS comes back) simulate the
	 * receipt of a will echo.  This will also send a WONT ECHO
	 * to the client, since we assume that the client failed to
	 * respond because it believes that it is already in DO ECHO
	 * mode, which we do not want.
	 */
	if (his_want_state_is_will(TELOPT_ECHO)) {
#ifdef DIAGNOSTICS
		if (diagnostic & TD_OPTIONS) {
			sprintf(nfrontp, "td: simulating recv\r\n");
			nfrontp += strlen(nfrontp);
		}
#endif /* DIAGNOSTICS */
		willoption(TELOPT_ECHO);
	}

	/*
	 * Finally, to clean things up, we turn on our echo.  This
	 * will break stupid 4.2 telnets out of local terminal echo.
	 */

	if (my_state_is_wont(TELOPT_ECHO))
		send_will(TELOPT_ECHO, 1);

	/*
	 * Turn on packet mode, and default to line at at time mode.
	 */
	(void) ioctl(p, TIOCPKT, (char *)&on);
#ifdef	LINEMODE
	tty_setlinemode(1);

# ifdef	KLUDGELINEMODE
	/*
	 * Continuing line mode support.  If client does not support
	 * real linemode, attempt to negotiate kludge linemode by sending
	 * the do timing mark sequence.
	 */
	if (lmodetype < REAL_LINEMODE)
		send_do(TELOPT_TM, 1);
# endif	/* KLUDGELINEMODE */
#endif	/* LINEMODE */

	/*
	 * Call telrcv() once to pick up anything received during
	 * terminal type negotiation, 4.2/4.3 determination, and
	 * linemode negotiation.
	 */
	telrcv();

	(void) ioctl(f, FIONBIO, (char *)&on);
	(void) ioctl(p, FIONBIO, (char *)&on);
#if	defined(CRAY2) && defined(UNICOS5)
	init_termdriver(f, p, interrupt, sendbrk);
#endif

#if	defined(SO_OOBINLINE)
	(void) setsockopt(net, SOL_SOCKET, SO_OOBINLINE, &on, sizeof on);
#endif	/* defined(SO_OOBINLINE) */

#ifdef	SIGTSTP
	(void) signal(SIGTSTP, SIG_IGN);
#endif
#ifdef	SIGTTOU
	/*
	 * Ignoring SIGTTOU keeps the kernel from blocking us
	 * in ttioct() in /sys/tty.c.
	 */
	(void) signal(SIGTTOU, SIG_IGN);
#endif

	(void) signal(SIGCHLD, cleanup);

#if	defined(CRAY2) && defined(UNICOS5)
	/*
	 * Cray-2 will send a signal when pty modes are changed by slave
	 * side.  Set up signal handler now.
	 */
	if ((int)signal(SIGUSR1, termstat) < 0)
		perror("signal");
	else if (ioctl(p, TCSIGME, (char *)SIGUSR1) < 0)
		perror("ioctl:TCSIGME");
	/*
	 * Make processing loop check terminal characteristics early on.
	 */
	termstat();
#endif

#ifdef	NO_SETSID
	(void) setpgrp(0, 0);
#else
	(void) setsid();
#endif
#if	defined(TIOCSCTTY) && defined(CRAY)
	ioctl(p, TIOCSCTTY, 0);
#endif

	/*
	 * Show banner that getty never gave.
	 *
	 * We put the banner in the pty input buffer.  This way, it
	 * gets carriage return null processing, etc., just like all
	 * other pty --> client data.
	 */

	(void) gethostname(hostname, sizeof (hostname));

	if (getent(defent, "default") == 1) {
		char *getstr();
		char *cp=defstrs;

		HE = getstr("he", &cp);
		HN = getstr("hn", &cp);
		IM = getstr("im", &cp);
		if (HN && *HN)
			(void) strcpy(hostname, HN);
		if (IM == 0)
			IM = "";
	} else {
#ifdef	CRAY
		if (hostinfo == 0)
			IM = 0;
		else
#endif
			IM = DEFAULT_IM;
		HE = 0;
	}
	edithost(HE, hostname);
	if (IM && *IM)
		putf(IM, ptyibuf2);

	if (pcc)
		(void) strncat(ptyibuf2, ptyip, pcc+1);
	ptyip = ptyibuf2;
	pcc = strlen(ptyip);
#ifdef	LINEMODE
	/*
	 * Last check to make sure all our states are correct.
	 */
	init_termbuf();
	localstat();
#endif	/* LINEMODE */

#ifdef DIAGNOSTICS
	if (diagnostic & TD_REPORT) {
		sprintf(nfrontp, "td: Entering processing loop\r\n");
		nfrontp += strlen(nfrontp);
	}
#endif /* DIAGNOSTICS */

	for (;;) {
		fd_set ibits, obits, xbits;
		register int c;

		if (ncc < 0 && pcc < 0)
			break;

#if	defined(CRAY2) && defined(UNICOS5)
		if (needtermstat)
			_termstat();
#endif	/* defined(CRAY2) && defined(UNICOS5) */
		FD_ZERO(&ibits);
		FD_ZERO(&obits);
		FD_ZERO(&xbits);
		/*
		 * Never look for input if there's still
		 * stuff in the corresponding output buffer
		 */
		if (nfrontp - nbackp || pcc > 0) {
			FD_SET(f, &obits);
		} else {
			FD_SET(p, &ibits);
		}
		if (pfrontp - pbackp || ncc > 0) {
			FD_SET(p, &obits);
		} else {
			FD_SET(f, &ibits);
		}
		if (!SYNCHing) {
			FD_SET(f, &xbits);
		}
		if ((c = select(16, &ibits, &obits, &xbits,
						(struct timeval *)0)) < 1) {
			if (c == -1) {
				if (errno == EINTR) {
					continue;
				}
			}
			sleep(5);
			continue;
		}

		/*
		 * Any urgent data?
		 */
		if (FD_ISSET(net, &xbits)) {
		    SYNCHing = 1;
		}

		/*
		 * Something to read from the network...
		 */
		if (FD_ISSET(net, &ibits)) {
#if	!defined(SO_OOBINLINE)
			/*
			 * In 4.2 (and 4.3 beta) systems, the
			 * OOB indication and data handling in the kernel
			 * is such that if two separate TCP Urgent requests
			 * come in, one byte of TCP data will be overlaid.
			 * This is fatal for Telnet, but we try to live
			 * with it.
			 *
			 * In addition, in 4.2 (and...), a special protocol
			 * is needed to pick up the TCP Urgent data in
			 * the correct sequence.
			 *
			 * What we do is:  if we think we are in urgent
			 * mode, we look to see if we are "at the mark".
			 * If we are, we do an OOB receive.  If we run
			 * this twice, we will do the OOB receive twice,
			 * but the second will fail, since the second
			 * time we were "at the mark", but there wasn't
			 * any data there (the kernel doesn't reset
			 * "at the mark" until we do a normal read).
			 * Once we've read the OOB data, we go ahead
			 * and do normal reads.
			 *
			 * There is also another problem, which is that
			 * since the OOB byte we read doesn't put us
			 * out of OOB state, and since that byte is most
			 * likely the TELNET DM (data mark), we would
			 * stay in the TELNET SYNCH (SYNCHing) state.
			 * So, clocks to the rescue.  If we've "just"
			 * received a DM, then we test for the
			 * presence of OOB data when the receive OOB
			 * fails (and AFTER we did the normal mode read
			 * to clear "at the mark").
			 */
		    if (SYNCHing) {
			int atmark;

			(void) ioctl(net, SIOCATMARK, (char *)&atmark);
			if (atmark) {
			    ncc = recv(net, netibuf, sizeof (netibuf), MSG_OOB);
			    if ((ncc == -1) && (errno == EINVAL)) {
				ncc = read(net, netibuf, sizeof (netibuf));
				if (sequenceIs(didnetreceive, gotDM)) {
				    SYNCHing = stilloob(net);
				}
			    }
			} else {
			    ncc = read(net, netibuf, sizeof (netibuf));
			}
		    } else {
			ncc = read(net, netibuf, sizeof (netibuf));
		    }
		    settimer(didnetreceive);
#else	/* !defined(SO_OOBINLINE)) */
		    ncc = read(net, netibuf, sizeof (netibuf));
#endif	/* !defined(SO_OOBINLINE)) */
		    if (ncc < 0 && errno == EWOULDBLOCK)
			ncc = 0;
		    else {
			if (ncc <= 0) {
			    break;
			}
			netip = netibuf;
		    }
#ifdef DIAGNOSTICS
		    if (diagnostic & (TD_REPORT | TD_NETDATA)) {
			    sprintf(nfrontp, "td: netread %d chars\r\n", ncc);
			    nfrontp += strlen(nfrontp);
		    }
		    if (diagnostic & TD_NETDATA) {
			    printdata("nd", netip, ncc);
		    }
#endif /* DIAGNOSTICS */
		}

		/*
		 * Something to read from the pty...
		 */
		if (FD_ISSET(p, &ibits)) {
			pcc = read(p, ptyibuf, BUFSIZ);
			if (pcc < 0 && errno == EWOULDBLOCK)
				pcc = 0;
			else {
				if (pcc <= 0)
					break;
#if	!defined(CRAY2) || !defined(UNICOS5)
#ifdef	LINEMODE
				/*
				 * If ioctl from pty, pass it through net
				 */
				if (ptyibuf[0] & TIOCPKT_IOCTL) {
					copy_termbuf(ptyibuf+1, pcc-1);
					localstat();
					pcc = 1;
				}
#endif	LINEMODE
				if (ptyibuf[0] & TIOCPKT_FLUSHWRITE) {
					netclear();	/* clear buffer back */
#ifdef	notdef
					/*
					 * We really should have this in, but
					 * there are client telnets on some
					 * operating systems get screwed up
					 * royally if we send them urgent
					 * mode data.  So, for now, we'll not
					 * do this...
					 */
					*nfrontp++ = IAC;
					*nfrontp++ = DM;
					neturg = nfrontp-1; /* off by one XXX */
#endif
				}
				if (his_state_is_will(TELOPT_LFLOW) &&
				    (ptyibuf[0] &
				     (TIOCPKT_NOSTOP|TIOCPKT_DOSTOP))) {
					(void) sprintf(nfrontp, "%c%c%c%c%c%c",
					    IAC, SB, TELOPT_LFLOW,
					    ptyibuf[0] & TIOCPKT_DOSTOP ? 1 : 0,
					    IAC, SE);
					nfrontp += 6;
				}
				pcc--;
				ptyip = ptyibuf+1;
#else	/* defined(CRAY2) && defined(UNICOS5) */
				if (!uselinemode) {
					unpcc = pcc;
					unptyip = ptyibuf;
					pcc = term_output(&unptyip, ptyibuf2,
								&unpcc, BUFSIZ);
					ptyip = ptyibuf2;
				} else
					ptyip = ptyibuf;
#endif	/* defined(CRAY2) && defined(UNICOS5) */
			}
		}

		while (pcc > 0) {
			if ((&netobuf[BUFSIZ] - nfrontp) < 2)
				break;
			c = *ptyip++ & 0377, pcc--;
			if (c == IAC)
				*nfrontp++ = c;
#if	defined(CRAY2) && defined(UNICOS5)
			else if (c == '\n' &&
				     my_state_is_wont(TELOPT_BINARY) && newmap)
				*nfrontp++ = '\r';
#endif	/* defined(CRAY2) && defined(UNICOS5) */
			*nfrontp++ = c;
			if ((c == '\r') && (my_state_is_wont(TELOPT_BINARY))) {
				if (pcc > 0 && ((*ptyip & 0377) == '\n')) {
					*nfrontp++ = *ptyip++ & 0377;
					pcc--;
				} else
					*nfrontp++ = '\0';
			}
		}
#if	defined(CRAY2) && defined(UNICOS5)
		/*
		 * If chars were left over from the terminal driver,
		 * note their existence.
		 */
		 if (!uselinemode && unpcc) {
			pcc = unpcc;
			unpcc = 0;
			ptyip = unptyip;
		}
#endif	/* defined(CRAY2) && defined(UNICOS5) */

		if (FD_ISSET(f, &obits) && (nfrontp - nbackp) > 0)
			netflush();
		if (ncc > 0)
			telrcv();
		if (FD_ISSET(p, &obits) && (pfrontp - pbackp) > 0)
			ptyflush();
	}
	cleanup();
}  /* end of telnet */
	
#ifndef	TCSIG
# ifdef	TIOCSIG
#  define TCSIG TIOCSIG
# endif
#endif

/*
 * Send interrupt to process on other side of pty.
 * If it is in raw mode, just write NULL;
 * otherwise, write intr char.
 */
interrupt()
{
	ptyflush();	/* half-hearted */

#ifdef	TCSIG
	(void) ioctl(pty, TCSIG, (char *)SIGINT);
#else	/* TCSIG */
	init_termbuf();
	*pfrontp++ = slctab[SLC_IP].sptr ?
			(unsigned char)*slctab[SLC_IP].sptr : '\177';
#endif	/* TCSIG */
}

/*
 * Send quit to process on other side of pty.
 * If it is in raw mode, just write NULL;
 * otherwise, write quit char.
 */
sendbrk()
{
	ptyflush();	/* half-hearted */
#ifdef	TCSIG
	(void) ioctl(pty, TCSIG, (char *)SIGQUIT);
#else	/* TCSIG */
	init_termbuf();
	*pfrontp++ = slctab[SLC_ABORT].sptr ?
			(unsigned char)*slctab[SLC_ABORT].sptr : '\034';
#endif	/* TCSIG */
}

sendsusp()
{
#ifdef	SIGTSTP
	ptyflush();	/* half-hearted */
# ifdef	TCSIG
	(void) ioctl(pty, TCSIG, (char *)SIGTSTP);
# else	/* TCSIG */
	*pfrontp++ = slctab[SLC_SUSP].sptr ?
			(unsigned char)*slctab[SLC_SUSP].sptr : '\032';
# endif	/* TCSIG */
#endif	/* SIGTSTP */
}

doeof()
{
#if	defined(USE_TERMIO) && defined(SYSV_TERMIO)
	extern char oldeofc;
#endif
	init_termbuf();

#if	defined(USE_TERMIO) && defined(SYSV_TERMIO)
	if (!tty_isediting()) {
		*pfrontp++ = oldeofc;
		return;
	}
#endif
	*pfrontp++ = slctab[SLC_EOF].sptr ?
			(unsigned char)*slctab[SLC_EOF].sptr : '\004';
}
