#ifndef lint
static char RCSid[] = "$Header: gap2d.c,v 2.0 85/11/21 07:23:00 jqj Exp $";
#endif
/*
 * server for GAP-style (TransportObject=server,teletype) telnet connections
 * Note that we support only GAP version 2, although a server for version 3
 * exists.  The version 2 server has not been tested as thoroughly as has the
 * version 3; it does NOT support RESERVE functionality.
 */

/* $Log:	gap2d.c,v $
 * Revision 2.0  85/11/21  07:23:00  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.2  85/05/23  06:22:18  jqj
 * *** empty log message ***
 * 
 * Revision 1.2  85/05/23  06:22:18  jqj
 * *** empty log message ***
 * 
 * Revision 1.1  85/05/22  09:46:52  jqj
 * Initial revision
 */
#include <stdio.h>
#include <signal.h>
#include <sgtty.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/uio.h>
#include <sys/socket.h>
#include <netns/ns.h>
#include <netns/idp.h>
#include <netns/sp.h>
#include <sys/wait.h>
#include <xnscourier/realcourierconnection.h>
#include "GAP2.h"
#include "gapcontrols.h"
#include <xnscourier/except.h>
#include <errno.h>

#define	BELL	'\07'
#define BANNER	"\r\n\r\n4.3 BSD UNIX (%s)\r\n\r\r\n\r%s"

int pty, net;
extern CourierConnection *_serverConnection;
char buf[sizeof(struct sphdr)+SPPMAXDATA];
struct sphdr our_sphdr;
struct iovec our_iovec[2] = {{((caddr_t)&our_sphdr), sizeof(our_sphdr)}};
/*
 * I/O data buffers, pointers, and counters.
 */
char	ptyibuf[512], *ptyip = ptyibuf;
char	ptyobuf[BUFSIZ], *pfrontp = ptyobuf, *pbackp = ptyobuf;
char	*netip = buf;
char	netobuf[512], *nfrontp = netobuf, *nbackp = netobuf;
int	pcc, ncc;
char	line[12];
extern	char **environ;
extern	int errno;

char *envinit[3];
char wsenv[50];

/*
 * session parameters
 */
Cardinal frametimeout;		/* 0 or time in seconds to wait */


/*
 * This modified version of the server is necessary since GAP specifies
 * that the telnet data-transfer session occurs after the RPC to create
 * it has returned!
 */
Server(skipcount,skippedwords)
	int skipcount;
	Unspecified skippedwords[];
{
	Cardinal _procedure;
	register Unspecified *_buf;
	LongCardinal programnum;
	Cardinal versionnum;
	Cardinal _n;

#ifdef DEBUG
	BUGOUT("Server: %d %d",skipcount,skippedwords);
#endif
	for (;;) {
		_buf = ReceiveCallMessage(&_procedure, skipcount, skippedwords);
		DURING switch (_procedure) {
		case 3:
			server_GAP2_Delete(_buf);
			break;
		case 2:
			server_GAP2_Create(_buf);
			net = _serverConnection->fd;
			gaptelnet(); /* returns on connection close */
			break;
		case 0:
			server_GAP2_Reset(_buf);
			break;
		default:
			NoSuchProcedureValue("GAP", _procedure);
			break;
		} HANDLER {
		    Deallocate(_buf);
		    switch (Exception.Code) {
		    case GAP2_terminalAddressInvalid:
		    case GAP2_terminalAddressInUse:
		    case GAP2_controllerDoesNotExist:
		    case GAP2_controllerAlreadyExists:
		    case GAP2_gapCommunicationError:
		    case GAP2_gapNotExported:
		    case GAP2_bugInGAPCode:
		    case GAP2_tooManyGateStreams:
		    case GAP2_inconsistentParams:
		    case GAP2_transmissionMediumUnavailable:
		    case GAP2_dialingHardwareProblem:
		    case GAP2_noDialingHardware:
		    case GAP2_badAddressFormat:
		    case GAP2_mediumConnectFailed:
		    case GAP2_illegalTransport:
		    case GAP2_noCommunicationHardware:
		    case GAP2_unimplemented:
			_buf = Allocate(0);
			SendAbortMessage(Exception.Code-ERROR_OFFSET, 0, _buf);
			break;
		    default:
			_buf = Allocate(0);
			SendRejectMessage(unspecifiedError, 0, _buf);
			break;
		    }
		} END_HANDLER;
		Deallocate(_buf);
		for (;;) {
			skipcount = LookAheadCallMsg(&programnum, &versionnum,
					skippedwords);
			if (skipcount < 0) return(0);	/* timed out */
			if (programnum != 3 || versionnum != 2)
				ExecCourierProgram(programnum, versionnum,
						skipcount, skippedwords);
		}  /* loop if can't exec that program */
	}
}

void
GAP2_Delete(session)
	GAP2_SessionHandle session; 
{
}

void
GAP2_Reset()
{
}

GAP2_CreateResults
GAP2_Create(conn, BDTproc, sessionparams, transports,
	    createTimeout)
	CourierConnection *conn;
	int BDTproc;
	GAP2_SessionParameterObject sessionparams;
	struct {Cardinal length;
		GAP2_TransportObject *sequence;
	} transports;
	GAP2_WaitTime createTimeout;
{
	GAP2_CreateResults result;
	char *c1, *c2, *host;
	int t, pid;
	struct sgttyb b;
	char *xntoa(), *wsname();
	struct sockaddr_ns who;
	int whosize = sizeof(who);
	LongCardinal servicetype;
	GAP2_CommParamObject *cp;
	GAP2_Duplexity duplexity;	/* fullDuplex, halfDuplex */

#ifdef DEBUG
	BUGOUT("CREATE");
#endif
	switch (sessionparams.designator) {
	case ttyHost:
		frametimeout = sessionparams.ttyHost_case.frameTimeout/1000;
		/* could set other parameters here */
		break;
	default:
		raise(GAP2_unimplemented, 0);
		/*NOTREACHED*/
	}
	if (transports.length != 2) {
		raise(GAP2_illegalTransport);
		/*NOTREACHED*/
	}
	switch (transports.sequence[0].designator) {
	case rs232c:	/* maybe some day */
		cp = &transports.sequence[0].rs232c_case.commParams;
		if (cp->accessDetail.designator == directConn) {
			duplexity = cp->duplex;
			servicetype = 0; /* fake it */
			break;
		}
		raise(GAP2_noCommunicationHardware, 0);
		/*NOTREACHED*/
	default:
		raise(GAP2_illegalTransport, 0);
		/*NOTREACHED*/
	}
	if (transports.sequence[1].designator != teletype)
	  raise(GAP2_illegalTransport, 0);
	/* ignore createTimeout */
	/* ignore credentials and verifier */
	
	for (c1 = "pq"; *c1 != 0; c1++)
	  for (c2 = "0123456789abcdef"; *c2 != 0; c2++) {
		  sprintf(line, "/dev/pty%c%c", *c1, *c2);
		  pty = open(line, 2);
		  if (pty < 0) continue;
		  line[strlen("/dev/")] = 't';
		  t = open(line, 2);
		  if (t > 0) goto gotpty;
		  close(pty);
	  }
	raise(GAP2_tooManyGateStreams, 0);
	/*NOTREACHED*/
 gotpty:
	getpeername(_serverConnection->fd, &who, &whosize);
	host = wsname(who.sns_addr);
#ifdef DEBUG
	BUGOUT("gotpty <%s> %d <%s>",line, pty, host);
#endif
	ioctl(t, TIOCGETP, &b);
	b.sg_flags = CRMOD|XTABS|ANYP;
	ioctl(t, TIOCSETP, &b);
	ioctl(pty, TIOCGETP, &b);
	if (duplexity == fullduplex)
	  b.sg_flags |= ECHO;
	else
	  b.sg_flags &= ~ECHO;
	ioctl(pty, TIOCSETP, &b);
	/* we do the fork now so we can return failures as REPORTS */
	pid = fork();
	if (pid < 0) {
		close(pty); close(t);
		raise(GAP2_tooManyGateStreams, 0);
		/*NOTREACHED*/
	}
	else if (pid == 0) {	/* in the execed fork */
		sleep(1);	/* let parent get ready for us */
		close(_serverConnection->fd); /* close net */
		close(pty);
		dup2(t, 0);
		dup2(t, 1);
		dup2(t, 2);
		if (t > 2) close(t);
		envinit[0] = "TERM=network";
		(void)sprintf(wsenv, "WORKSTATION=%s", xntoa(who.sns_addr));
		envinit[1] = wsenv;
		envinit[2] = (char*) 0;
#ifdef DEBUG
		BUGOUT("about to exec /bin/login");
#endif
		execl("/bin/login","login", "-h", host, 0);
#ifdef DEBUG
		BUGOUT("exec of /bin/login failed");
#endif
		perror("/bin/login");
		exit(1);
		/*NOTREACHED*/
	}
	close(t);
#ifdef DEBUG
	BUGOUT("fork successful");
#endif
	result.session[0] = pid;
	return(result);
}

jmp_buf childdiedbuf;

/*
 * Main loop.  Select from pty and network, and
 * hand data to telnet receiver finite state machine.
 * Returns 0 on orderly shutdown, 1 on abnormal shutdown.
 */
gaptelnet()
{
	int on = 1;
	char hostname[32];
	int childdied();
	int ibits = 0, obits = 0;
	register int c;
	struct sphdr *si = (struct sphdr *)buf;
	static struct timeval timeout = {600,0};
	int keepalives = 0;
	int i;

#ifdef DEBUG
	BUGOUT("gaptelnet net=%d,pty=%d",net,pty);
#endif
	if (setjmp(childdiedbuf) != 0)
	  return(0);		/* child died */
	signal(SIGCHLD, childdied);
	signal(SIGTSTP, SIG_IGN);
	ioctl(net, FIONBIO, &on);
	ioctl(pty, FIONBIO, &on);


	/*
	 * Show banner that getty never gave.
	 */
	gethostname(hostname, sizeof (hostname));
	sprintf(nfrontp, BANNER, hostname, "");
	nfrontp += strlen(nfrontp);
	/*
	 * Send status message indicating we're ready to go
	 */
	changeSPPopts(net, GAPCTLnone, 1);
	sendoobdata(GAPCTLmediumUp);
	for (;;) {
#ifdef DEBUG
		BUGOUT("looping in gaptelnet");
#endif
		ibits = obits = 0;
		/*
		 * Never look for input if there's still
		 * stuff in the corresponding output buffer
		 */
		if (nfrontp - nbackp || pcc > 0)
			obits |= (1 << net);
		else
			ibits |= (1 << pty);
		if (pfrontp - pbackp || ncc > 0)
			obits |= (1 << pty);
		else
			ibits |= (1 << net);
		if (ncc < 0 && pcc < 0)
			break;
		timeout.tv_sec = 600;
		timeout.tv_usec = 0;
		select(16, &ibits, &obits, 0, &timeout);
		if (ibits == 0 && obits == 0) {
			/* timeout means no activity for a long time */
#ifdef DEBUG
			BUGOUT("timeout from select");
#endif
			if (keepalives++ < 2) {
			  /* first 2 times through send warning */
				if (nfrontp == nbackp && pcc == 0) {
					/* but only if not blocked on output */
#define WARNING "\r\nYou've been idle much too long.  Respond or log off.\r\n"
					strcpy(nfrontp, WARNING);
					nfrontp += sizeof(WARNING);
				}
				sleep(5);
				continue;
			}
#ifdef DEBUG
			BUGOUT("keepalive expired -- calling cleanup");
#endif
			/* keepalive count has expired */
			cleanup();
			return(1);
		}

		/*
		 * Something to read from the network...
		 */
		if (ibits & (1 << net)) {
			ncc = read(net, buf, sizeof(buf));
#ifdef DEBUG
			BUGOUT("read from net %d",ncc);
#endif
			if (ncc < 0 && errno == EWOULDBLOCK)
				ncc = 0;
			else if (ncc < sizeof(struct sphdr)) {
#ifdef DEBUG
				BUGOUT("short read, %d.  calling cleanup",ncc);
#endif
				cleanup(); /* will probably fail or block */
				return(1);
			}
			else if (si->sp_cc & SP_OB) {
				/* a status or OOB control */
				switch (buf[sizeof(struct sphdr)]) {
				case GAPCTLinterrupt:
					/* shove interrupt char in buffer */
					interrupt();
					break; /* from switch */
				case GAPCTLareYouThere:
					sendoobdata(GAPCTLiAmHere);
					break; /* from switch */
				default:
					/* Ignore other controls instead of:
					 * sendoobdata(
					 *     GAPCTLunexpectedRemoteBehavior);
					 */
					break; /* from switch */
				}
				ncc = 0; /* no chars here */
			}
			else if (si->sp_dt==GAPCTLnone) {
				/* the normal case */
				ncc -= sizeof(struct sphdr);
				netip = buf + sizeof(struct sphdr);
				keepalives = 0;
			}
			else if(si->sp_dt==GAPCTLcleanup) {
#ifdef DEBUG
				BUGOUT("got CLEANUP packet.  Done");
#endif
				cleanup(); /* normal termination */
				return(0);
			}
			else if (si->sp_dt==SPPSST_END) {
				/* got premature termination */
				quitquit(net, pty);
				return(1);
			}
		}

		/*
		 * Something to read from the pty...
		 */
		if (ibits & (1 << pty)) {
			if (frametimeout > 0) sleep(frametimeout);
			pcc = read(pty, ptyibuf, sizeof(ptyibuf));
#ifdef DEBUG
			BUGOUT("read from pty %d",pcc);
#endif
			if (pcc < 0 && errno == EWOULDBLOCK)
				pcc = 0;
			else if (pcc <= 0) {
#ifdef DEBUG
				BUGOUT("short read from pty. Calling cleanup");
#endif
				cleanup();
				return(1); /* ?? abnormal termination */
			}
			ptyip = ptyibuf;
		}

		while (pcc > 0) {
			if ((&netobuf[sizeof(netobuf)] - nfrontp) < 2)
				break;
			*nfrontp++ = *ptyip++ & 0377; pcc--;
		}
		if ((obits & (1 << net)) && (nfrontp - nbackp) > 0)
			netflush();
		while (ncc > 0) {
			if ((&ptyobuf[sizeof(ptyobuf)] - pfrontp) < 2) break;
			*pfrontp++ = *netip++ & 0377;
			ncc--;
		}
		if ((obits & (1 << pty)) && (pfrontp - pbackp) > 0)
			ptyflush();
	}
	/* we should never get to here */
#ifdef DEBUG
	BUGOUT("broke out of for(;;) somehow.  calling cleanup");
#endif
	cleanup();
	return(0);
}

/*
 * Send out of band data to other end of network
 */
sendoobdata(value)
	u_char value;
{
	struct {
		struct sphdr hdr;
		char val;
	} oob;
	oob.hdr = our_sphdr;
	oob.val = value;
#ifdef DEBUG
	BUGOUT("sendoobdata 0%o",value);
#endif
	send(net, &oob, sizeof(oob), MSG_OOB);
}

/*
 * Send interrupt to process on other side of pty.
 * If it is in raw mode, just write NULL;
 * otherwise, write intr char.
 */
interrupt()
{
	struct sgttyb b;
	struct tchars tchars;

	ptyflush();	/* half-hearted */
	ioctl(pty, TIOCGETP, &b);
	if (b.sg_flags & RAW) {
		*pfrontp++ = '\0';
		return;
	}
	*pfrontp++ = ioctl(pty, TIOCGETC, &tchars) < 0 ?
		'\177' : tchars.t_intrc;
}

ptyflush()
{
	register int n;

	if ((n = pfrontp - pbackp) > 0)
		n = write(pty, pbackp, n);
#ifdef DEBUG
	BUGOUT("ptyflush wrote %d",n);
#endif
	if (n < 0)
		return;
	pbackp += n;
	if (pbackp >= pfrontp)	/* actually, > is an error */
		pbackp = pfrontp = ptyobuf;
}

netflush()
{
	register int n;

	if ((n = nfrontp - nbackp) > 0) {
		our_iovec[1].iov_len = ((n > SPPMAXDATA) ? SPPMAXDATA : n);
		our_iovec[1].iov_base = nbackp;
		n = writev(net, our_iovec, 2) - sizeof(struct sphdr);
	}
#ifdef DEBUG
	BUGOUT("netflush wrote %d",n);
	if (our_iovec[0].iov_base != (char*)&our_sphdr)
		BUGOUT("Oops:  our_iovec clobbered");
	BUGOUT("header: %d %d, %d %d %d %d %d %d",
		our_sphdr.sp_cc, our_sphdr.sp_dt, 
		our_sphdr.sp_sid, our_sphdr.sp_did, our_sphdr.sp_seq, 
		our_sphdr.sp_ack, our_sphdr.sp_alo);
#endif
	if (n < 0) {
		if (errno == EWOULDBLOCK)
			return;
		/* should blow this guy away... */
		return;
	}
	nbackp += n;
	if (nbackp >= nfrontp)	/* actually , > is an error */
		nbackp = nfrontp = netobuf;
}

/*
 * handle receipt of an SPPSST_END packet
 * This is currently an error, since client didn't send "cleanup" first
 */
quitquit()
{
	changeSPPopts(net, SPPSST_ENDREPLY, 1);
	write(net, &our_sphdr, sizeof(our_sphdr));
	sleep(3);

	rmut();
	vhangup();	/* XXX */
	shutdown(net, 1);
	close(net);
}

/*
 * shut down the data connection for one reason or another
 */
cleanup()
{
	int fdmask;
	struct timeval timeout;
	struct sphdr *si = (struct sphdr *)buf;
	int off = 0;

	signal(SIGCHLD, SIG_IGN);
	sendoobdata(GAPCTLcleanup);
	changeSPPopts(net, SPPSST_END, 1);
	if (write(net, &our_sphdr, sizeof(our_sphdr)) < 0) {
		fdmask = 1<<net;
		timeout.tv_sec = 10;
		while (select(net+1,&fdmask,(int*)0, (int*)0, &timeout) > 0 &&
		       read(net,buf,sizeof(buf)) >= sizeof(struct sphdr)) {
#ifdef DEBUG
			BUGOUT("cleanup -- got packet");
#endif
			if ((si->sp_cc & SP_OB)
			    && si->sp_dt == SPPSST_ENDREPLY) {
				changeSPPopts(net, SPPSST_ENDREPLY, 1);
				write(net, &our_sphdr, sizeof(our_sphdr));
#ifdef DEBUG
				BUGOUT("cleanup -- wrote ENDREPLY");
#endif
				sleep(1);
				changeSPPopts(net,0,0);
				ioctl(net, FIONBIO, &off);
				rmut();
				vhangup();	/* XXX */
				return;
			}
			/* loop: ignore everything except ENDREPLY */
			fdmask = 1<<net;
			timeout.tv_sec = 10;
		}
		/* timed out or read failed */
		changeSPPopts(net, SPPSST_ENDREPLY, 1);
		write(net, &our_sphdr, sizeof(our_sphdr));
		sleep(1);
	}
	shutdown(net, 1);
	close(net);
	rmut();
	vhangup();	/* XXX */
}

/*
 * SIGCHLD interrupt handler
 */
childdied()
{
#ifdef DEBUG
	BUGOUT("child died");
#endif
	cleanup();
	longjmp(childdiedbuf, -1);
}

changeSPPopts(s, stream, eom)
	int s;			/* SPP socket */
	u_char stream;		/* datastream type */
	char eom;		/* Boolean EOM */
{
	our_sphdr.sp_dt = stream;
	our_sphdr.sp_cc = (eom ? SP_EM : 0);
}


#include <utmp.h>

struct	utmp wtmp;
char	wtmpf[]	= "/usr/adm/wtmp";
char	utmp[] = "/etc/utmp";
#define SCPYN(a, b)	strncpy(a, b, sizeof (a))
#define SCMPN(a, b)	strncmp(a, b, sizeof (a))

rmut()
{
	register f;
	int found = 0;

	f = open(utmp, 2);
	if (f >= 0) {
		while(read(f, (char *)&wtmp, sizeof (wtmp)) == sizeof (wtmp)) {
			if (SCMPN(wtmp.ut_line, line+5) || wtmp.ut_name[0]==0)
				continue;
			lseek(f, -(long)sizeof (wtmp), 1);
			SCPYN(wtmp.ut_name, "");
			SCPYN(wtmp.ut_host, "");
			time(&wtmp.ut_time);
			write(f, (char *)&wtmp, sizeof (wtmp));
			found++;
		}
		close(f);
	}
	if (found) {
		f = open(wtmpf, 1);
		if (f >= 0) {
			SCPYN(wtmp.ut_line, line+5);
			SCPYN(wtmp.ut_name, "");
			SCPYN(wtmp.ut_host, "");
			time(&wtmp.ut_time);
			lseek(f, (long)0, 2);
			write(f, (char *)&wtmp, sizeof (wtmp));
			close(f);
		}
	}
	chmod(line, 0666);
	chown(line, 0, 0);
	line[strlen("/dev/")] = 'p';
	chmod(line, 0666);
	chown(line, 0, 0);
}

/*
 * Convert network-format xns address
 * to ascii
 * --Replace this with a clearinghouse name lookup someday.
 */
char *
wsname(addr)
	struct ns_addr addr;
{
	static char b[50];
	char temp[10];
	int i;

	/* net */
	sprintf(b, "%D.", ntohl(ns_netof(addr)));
	/* skip leading zeros */
	for(i=0; (addr.x_host.c_host[i] == (char) 0); i++) ;
	/* print the rest */
	for(; i < 6; i++) {
		sprintf(temp,"%x", addr.x_host.c_host[i]);
		strcat(b, temp);
		if(i != 5) strcat(b, ":");
	}
	return (b);
}

/*
  * generate an xns address that "DE" can parse.
  * This goes in the environment.  Should be the same as above
  */
char *
xntoa(addr)
	struct ns_addr addr;
{
	static char b[50];
	char temp[10];
	int i;

	/* net */
	sprintf(b, "%X#", ntohl(ns_netof(addr)));
	/* print the rest */
	for(i=0; i < 6; i++) {
		sprintf(temp,"%x", addr.x_host.c_host[i]);
		strcat(b, temp);
		if(i != 5) strcat(b, ".");
	}
	return (b);
}

#ifdef DEBUG
BUGOUT(str,a,b,c,d,e,f,g,h)
	char *str;
{
	FILE *fd;
	fd = fopen("/tmp/GAP2d.log","a");
	fprintf(fd,str,a,b,c,d,e,f,g,h);
	putc('\n',fd);
	fclose(fd);
}
#endif
