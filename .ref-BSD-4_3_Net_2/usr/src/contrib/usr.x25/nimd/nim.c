/*
 * Network Interface Machine Server
 *
 * Frank Pronk
 * Copyright (c) 1984
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>

#include <signal.h>
#include <errno.h>
#include <setjmp.h>
#include <sgtty.h>
#include <pwd.h>

#include "../h/x29.h"

#include "buf.h"
#include "nim.h"

extern	char chartab[128];

jmp_buf	JmpBuf;			/* non-local goto buffer for interval timer */
short	PtyFd = -1, NetFd = -1;
short	LogFd = -1;
char	*LogDev;
short	TimerOn;		/* interval timer armed */
char	Banner[] = "NIM daemon\r";
extern	int errno;
char	*TraceFile;		/* trace file name */
char	*PtyName;
struct	bufhd ptyoqueue, netoqueue;
struct	buf *packet;
char	user_name[50];

struct	netinfo NetInfo = { CCITT1980, NX29_1980_PARMS, 128 };

struct	PtyPacket {
	char	p_type;
	char	p_data[1];	/* usually more than one byte */
};

char	*x25err();

main(argc, argv)
register char **argv;
{
	register int s;
	register char *p;
	int on = 1, uid;
	struct passwd *pw;

#ifdef waterloo
	/*
	 * If this host doesn't support X.25, give up.
	 */
	s = socket(AF_CCITT, SOCK_STREAM, 0);
	if (s < 0 && errno == EPROTONOSUPPORT) {
		fprint(2, "nimd: X.25 is not supported on this machine\n");
		exit(1);
	}
	close(s);
#endif

	for (argv++; argc > 1; argc--, argv++)
		if (**argv == '-')
			for (p = *argv+1; *p; p++)
			switch (*p) {
			case 'c':
				if (argc > 1) {
					argc--; argv++;
					if (strcmp (*argv, "1978") == 0) {
						NetInfo.n_nparms = NX29_1978_PARMS;
						break;
					} else if (strcmp (*argv, "1980") == 0) {
						NetInfo.n_nparms = NX29_1980_PARMS;
						break;
					}
				}
				fatal ("1978 or 1980 expected after -c");
				break;

			case 't':
				if (argc > 1) {
					argc--; argv++;
					TraceFile = *argv;
				} else
					fatal ("trace file name expected");
				break;

			default:
				fatal("usage: nimd [-c ccitt-date] [-h size] [-n size] [-t trace_file] pty_name");
			}
		else
			PtyName = *argv;

	if (PtyName == 0)
		fatal ("No pseudo-tty specified");

	if (fork())
		exit(0);
	for (s = 0; s < 10; s++)
		(void) close(s);
	(void) open("/", 0);
	(void) dup2(0, 1);
	(void) dup2(0, 2);
	{ int tt = open("/dev/tty", 2);
	  if (tt > 0) {
		ioctl(tt, TIOCNOTTY, (char *)0);
		close(tt);
	  }
	}

	OpenLog ();
	packet = getbuf (MAXPSIZ);
	ResetBufs ();
	signal (SIGPIPE, SIG_IGN);

	for (;;) {
		int bits;

		if ((PtyFd = open (PtyName, 2)) < 0)
			fatal (x25err(PtyName));
		ioctl (PtyFd, FIONBIO, (char *)&on);
		ioctl (PtyFd, TIOCPKT, (char *)&on);
		ioctl (PtyFd, TIOCREMOTE, (char *)&on);
		bits = (1 << PtyFd);
		select (16, (int *)0, &bits, (int *)0, (struct timeval *)0);
		sleep (1);	/* wait for pty to settle down */
#ifdef TIOCGSUID
		/*
		 * Get the slave's uid.
		 */
		uid = -1;
		if (ioctl(PtyFd, TIOCGSUID, &uid) < 0) {
			perror("nimd: ioctl TIOCGSUID");
			close(PtyFd);
			continue;
		}
		if (uid < 0 || (pw = getpwuid(uid)) == 0) {
			message("nimd: uid %d: Who are you?\n", uid);
			close(PtyFd);
			continue;
		}
		strcpy(user_name, pw->pw_name);
		/*
		 * Set effective uid to the slave
		 * so he/she will be charged for X.25 usage.
		 */
		setreuid(0, pw->pw_uid);
#endif
		nim ();
		sleep (1);	/* wait for slave to disconnect */
		setreuid(0, 0);
		CloseLog(); OpenLog();	/* allow log rollover */
	}
	/*NOTREACHED*/
}

fatal (msg)
char *msg;
{
	OpenLog ();
	log ("%s", msg);
	print ("nimd: %s\n", msg);
	exit (1);
}

/*VARARGS*/
message(fmt, a1, a2, a3, a4, a5)
char *fmt;
{
	char buf[128];

	sprint (buf, fmt, a1, a2, a3, a4, a5);
	ToPty (buf, strlen (buf), FROMNIM);
}

error()
{
	register char *errp;

	errp = x25err ((char *)0);
	log (errp);
	message ("nimd: %s\r", errp);
}


/*
 * Main loop.  Select from pty and network, and
 * hand data to nim receiver.
 */

nim()
{
	register int len;
	char buf[MAXPSIZ];
	int on = 1, ibits, obits, first = 1;

	InitProfile (DEFAULT_PROFILE);
	message (Banner);
	State = ST_COMMAND;
	OpenLog ();
	if (user_name[0])
		log("slave connect: %s", user_name);
	else
		log ("slave connect");

	for (;;) {
		(void) setjmp (JmpBuf);
		if (first) {
			obits = (1 << PtyFd);
			ibits = 0;
			first = 0;
			goto do_io;
		}
		ibits = obits = 0;

		/*
		 * Never look for input if there's still
		 * stuff in the corresponding output buffer
		 */

		if (PtyFd >= 0)
			if (!QEMPTY(&ptyoqueue))
				obits |= (1 << PtyFd);
			else
				if (NetFd >= 0 && State == ST_DATA)
					ibits |= (1 << NetFd);

		if (!QEMPTY(&netoqueue) && NetFd >= 0) {
			if (!OutputBlocked)
				obits |= (1 << NetFd);
		} else
			if (PtyFd >= 0)
				ibits |= (1 << PtyFd);

		if (ibits == 0 && obits == 0)	/* nothing to do; go home */
			break;

		if (State & ST_COMMAND) {
			struct timeval TimeOut;

			TimeOut.tv_sec = 60;
			TimeOut.tv_usec = 0;
			if (select (16, &ibits, &obits, (int *)0, &TimeOut) <= 0) {
				log ("slave inactivity timeout");
				break;
			}
		} else {
			if (TimerOn)
				sigsetmask (0);
			(void) select (16, &ibits, &obits, (int *)0, (struct timeval *)0);
			if (TimerOn)
				sigsetmask (1 << (SIGALRM -1 ));
		}

	do_io:
		/*
		 * Something to read from the network...
		 */
		if (ibits & (1 << NetFd))
			if ((len = ReadAndTrace(NetFd, buf, MAXPSIZ, "net rx")) == 0) {
				if (errno != EWOULDBLOCK)
					NetworkError ();
			} else
				FromNet (buf, len);

		/*
		 * Something to read from the pty...
		 */
		if (ibits & (1 << PtyFd))
			if ((len = ReadAndTrace (PtyFd, buf, NetInfo.n_psize+1, "pty rx")) == 0) {
				if (errno != EWOULDBLOCK) {
					close (PtyFd);
					PtyFd = -1;
				}
			} else
				FromPty ((struct PtyPacket *)buf, len);

		if (obits & (1<<NetFd)) {
			if (FlushQueue (NetFd, &netoqueue, "net tx") < 0 && 
				errno != EWOULDBLOCK)
				NetworkError ();
		}

		if (obits & (1 << PtyFd))
			if (FlushQueue (PtyFd, &ptyoqueue, "pty tx") < 0 &&
				errno != EWOULDBLOCK) {
				close (PtyFd);
				PtyFd = -1;
			}
	}
	cleanup ();
}

ReadAndTrace (fd, buf, len, who)
char *buf, *who;
{
	register int bytes;

	bytes = read (fd, buf, len);
	if (bytes <= 0)
		return (0);
	NimTrace (who, buf, bytes);
	return (bytes);
}

NetworkError ()
{

	error ();
	State = ST_COMMAND;
	close (NetFd);
	NetFd = -1;
	message (Banner);
}

cleanup ()
{
	log ("slave disconnect");
	close (NetFd);
	close (PtyFd);
	NetFd = PtyFd = -1;
	if (TimerOn)
		ClearTimer ();
	ResetBufs ();
}

ResetBufs ()
{
	InitQueue (&ptyoqueue);
	InitQueue (&netoqueue);
	RESET (packet);
}

ToPty (str, len, from)
register char *str;
{
	register char *end = str + len, c, c1;
	register struct buf *bp = 0;
	register int lfcode;

	while (str < end) {
		c = *str++;
		c1 = c & 0177;
		if (CurrentX29Parms[X29_AUX_DEV_CONTROL_CODE]) {
			if (c1 == 023) {
				OutputBlocked++;
				continue;
			}
			if (c1 == 021) {
				OutputBlocked = 0;
				continue;
			}
		}
		if (ptyoqueue.b_count > 256)
			continue;
		if (bp == 0)
			bp = getbuf (MAXPSIZ);
		PUTCHAR (c, bp);
		if (SIZE (bp) >= MAXPSIZ-1) {
			enqueue (bp, &ptyoqueue);
			bp = 0;
		}
		if (c1 != '\r' || (lfcode = CurrentX29Parms[X29_LF_AFTER_CR]) <= 0)
			continue;
		if ((lfcode & 01) && from == FROMNET ||
		    (lfcode & 04) && from == FROMPTY ||
		     from == FROMNIM)
			PUTCHAR ('\n', bp);
	}
	if (bp)
		enqueue (bp, &ptyoqueue);
}

FromPty(pp, len)
register struct PtyPacket *pp;
{
	register int echo;
	register struct buf *tp = packet;
	register char *cp, c;
	char c1;

	if (pp->p_type != TIOCPKT_DATA) {	/* fetch control byte */
		PtyControl (pp);
		return;
	}
	if (State & ST_UGLY_50_BAUD_BREAK_IN_PROGRESS)
		return;
	cp = pp->p_data;
	echo = CurrentX29Parms[X29_ECHO_CODE] > 0 && ptyoqueue.b_count < 256;
	while (cp < ((char *)pp)+len) {
		c = (c1 = *cp++) & 0177;
		if (State & ST_ESCSEEN && C_TYPE(c) != C_ESCAPE)
			EnterCommandState ();
		switch (C_TYPE(c)) {
		case C_ERASE:
			if (!ISEMPTY(tp)) {
				tp->b_top--;
				if (echo)
					if (c == '\b')
						ToPty ("\b \b", 3, FROMPTY);
					else
						ToPty (&c1, 1, FROMPTY);
			}
			continue;

		case C_KILL:
			RESET (tp);
			if (echo)
				ToPty ("*poof*\r", 7, FROMPTY);
			continue;

		case C_DISPLAY:
			ToPty (tp->b_bot, SIZE (tp), FROMPTY);
			continue;

		case C_ESCAPE:
			if ((State & (ST_COMMAND|ST_ESCSEEN)) == 0) {
				State |= ST_ESCSEEN;
				continue;
			}
			State &= ~ST_ESCSEEN;
			/* fall through */

		default:
			if (State & ST_COMMAND) {
				if (c == '\r') {
					if (echo)
						ToPty (&c1, 1, FROMPTY);
					PUTCHAR ('\0', tp);
					FlushQueue (PtyFd, &ptyoqueue, "pty tx");
					NimCommand (tp->b_bot);
					RESET(tp);
					continue;
				}
				if (SIZE (tp) < MAXPSIZ-1) {
					PUTCHAR (c, tp);
					if (echo)
						ToPty (&c1, 1, FROMPTY);
				} else
/*					ToPty ("\007", 1, FROMPTY)*/;
			} else {
				PUTCHAR (c1, tp);
				if (echo)
					ToPty (&c1, 1, FROMPTY);
				if (ISFORWARD(c) || SIZE (tp) >= NetInfo.n_psize) {
					ForwardPacket ();
					continue;
				}
			}
		}
	}
	if (!ISEMPTY (tp) && (State & ST_COMMAND) == 0)
		SetTimer ();
}

PtyControl(pp)
register struct PtyPacket *pp;
{
#ifdef notdef
	if ((pp->p_type & (TIOCPKT_FLUSHWRITE|TIOCPKT_FLUSHREAD)) ==
	   (TIOCPKT_FLUSHWRITE|TIOCPKT_FLUSHREAD)) {	/* break indication from pty */
		if (State & ST_COMMAND)
			RESET (packet);
		else
			Break (CurrentX29Parms[X29_BREAK_PROCEDURE_CODE]);
		return;
	   }
#endif
#ifdef TIOCPKT_IOCTL
	if (pp->p_type & TIOCPKT_IOCTL) {	/* some kind of set tty done by slave */
		static short UnixToX29Speed[] = {
			0, 10, 5, 0, 1, 6, 8, 2, 4, 3,	/* B0 thru B1200 */
			12, 13, 14, 15, 16		/* B2400 thru EXTB */
		};
		struct sgttyb sg;

		ioctl(PtyFd, TIOCGETP, (char *)&sg);
		if (sg.sg_ospeed == B50) {
			State |= ST_UGLY_50_BAUD_BREAK_IN_PROGRESS;
			return;
		}
		CurrentX29Parms[X29_TRANSMISSION_SPEED_CODE]
			= UnixToX29Speed[sg.sg_ospeed];
		if (State & ST_UGLY_50_BAUD_BREAK_IN_PROGRESS && sg.sg_ospeed != B50) {
			State &= ~ST_UGLY_50_BAUD_BREAK_IN_PROGRESS;
			if (State & ST_COMMAND)
				RESET (packet);
			else
				Break (CurrentX29Parms[X29_BREAK_PROCEDURE_CODE]);
		}
	}
#endif
}

EnterCommandState ()
{
	State &= ~ST_ESCSEEN;
	State |= ST_COMMAND | ST_ESCCOMM;
	ForwardPacket ();
}

ExitDataState (cause)
char *cause;
{
	ResetBufs ();
	close (NetFd);
	NetFd = -1;
	State = ST_COMMAND;
	OutputBlocked = 0;
	CurrentX29Parms[X29_DISCARD_OUTPUT_CODE] = 0;
	message ("nimd: Call cleared - %s\r", cause);
	log ("Call cleared - %s", cause);
}

ForwardPacket ()
{
	register struct buf *bp, *tp = packet;

	if (!ISEMPTY(tp) && (State & ST_COMMAND) == 0) {
		AddParity (tp->b_bot, SIZE (tp));
		bp = getbuf (SIZE (tp) + 1);
		PUTCHAR(0, bp);
		BufCopy(tp, bp);
		enqueue (bp, &netoqueue);
	}
	RESET (tp);
	if (TimerOn)
		ClearTimer ();
}

ToNet (pp, len)
struct packet *pp;
{
	register struct buf *bp;

	/*
	 * round buffer size up to a multiple of 64 bytes
	 * to reduce accumulation of small and usually
	 * useless buffers in the free list.  This speeds
	 * up malloc().
	 */
	bp = getbuf ((len + 63) & ~63);
	bcopy ((char *)pp, bp->b_bot, len);
	bp->b_top = bp->b_bot + len;
	enqueue (bp, &netoqueue);
}

timeout()
{
	TimerOn = 0;
	ForwardPacket ();
	longjmp (JmpBuf, 0);
}

SetTimer ()
{
	register int t;
	struct itimerval itv;

	if (TimerOn || (t = CurrentX29Parms[X29_IDLE_TIMER_CODE]) <= 0)
		return;
	itv.it_interval.tv_sec = 0;
	itv.it_interval.tv_usec = 0;
	itv.it_value.tv_sec = t / 20;
	itv.it_value.tv_usec = t % 20 * 1000000 / 20;
	signal (SIGALRM, SIG_IGN);	/* cancel possible pending signal */
	signal (SIGALRM, timeout);
	sigsetmask (1 << (SIGALRM - 1));
	TimerOn++;
	setitimer (ITIMER_REAL, &itv, (struct itimerval *)0);
}

ClearTimer ()
{
	struct itimerval itv;

	signal (SIGALRM, SIG_IGN);
	bzero ((char *)&itv, sizeof (itv));
	setitimer (ITIMER_REAL, &itv, (struct itimerval *)0);
	TimerOn = 0;
}

FromNet (bp, len)
char *bp;
{
	if ((*bp & Q_BIT) == 0) {
		register struct x25packet *xp = (struct x25packet *)bp;

		if (CurrentX29Parms[X29_DISCARD_OUTPUT_CODE] == 0) {
			AddParity (xp->p_x25data, len - 1);
			ToPty (xp->p_x25data, len - 1, FROMNET);
		}
		return;
	}
	X29ControlMessage ((struct x29packet *)bp, len);
}

SendX25Interrupt()
{
	char c = 0x77;

	send (NetFd, &c, 1, MSG_OOB);
}

#ifdef fastidious	/* we need stdio */
/*
 * Sorry about this...
 * Defining this dummy procedure prevents the stdio package
 * (about 17K bytes worth) from being loaded.  This program
 * does not require any support from the 4.2bsd stdio library.
 */

#ifdef vax
_cleanup()
{
}
#endif
#endif

NimTrace(who, bp, n)
char *who, *bp;
{
	static int fd;

	if (TraceFile == 0)
		return;
	if(fd <= 0 && (fd = creat(TraceFile, 0640)) < 0)
		return;
	DisplayPacketContents (fd, who, bp, n);
}

OpenLog ()
{

	if (LogFd >= 0)
		return;
	if (LogDev = rindex (PtyName, '/'))
		LogDev++;
	else
		LogDev = PtyName;
	if ((LogFd = open (LOGFILE, 1)) >= 0)
		return;
	LogFd = creat (LOGFILE, 0640);
}

CloseLog()
{
	if (LogFd >= 0) {
		close(LogFd);
		LogFd = -1;
	}
}

/*VARARGS*/
log(fmt, a1, a2, a3, a4, a5)
char *fmt;
{
	register char *DateTime;
	char format[128], *ctime ();
	time_t t;

	if (LogFd < 0)
		return;
	(void) time (&t);
	DateTime = ctime (&t);
	DateTime[19] = '\0';
	sprint (format, "%s %s %s\n", DateTime+4, LogDev, fmt);
	lseek (LogFd, (long)0, 2);
	fprint (LogFd, format, a1, a2, a3, a4, a5);
}

LogPacket (bp, len)
char *bp;
{
	if (LogFd < 0)
		return;
	DisplayPacketContents (LogFd, "net rx", bp, len);
}

DisplayPacketContents (fd, from, pp, len)
char *from;
register char *pp;
register int len;
{
	register int first = 1;
	char buf[128];

	lseek (fd, (long)0, 2);
	do {
		ConvertToOctal (pp, len, buf);
		if (first) {
			fprint (fd, "%s[%d]\t%s\n", from, len, buf);
			first = 0;
		} else
			fprint (fd, "\t\t%s\n", buf);
		ConvertToAscii (pp, len, buf);
		fprint (fd, "\t\t%s\n", buf);
		pp += 16;
		len -= 16;
	} while (len > 0);
}

ConvertToOctal (start, len, bp)
register char *start, *bp;
{
	register char *cp;

	if (len > 16)
		len = 16;
	for (cp = start; cp - start < len; cp++) {
		*bp++ = ((*cp & 0300) >> 6) + '0';
		*bp++ = ((*cp & 070) >> 3) + '0';
		*bp++ = (*cp & 07) + '0';
		*bp++ = ' ';
	}
	bp[-1] = '\0';
}
			
ConvertToAscii (start, len, bp)
register char *start, *bp;
{
	register char *cp;

	if (len > 16)
		len = 16;
	for (cp = start; cp - start < len; cp++) {
		*bp++ = ' ';
		switch (*cp) {
		case '\b':
			*bp++ = '\\';
			*bp++ = 'b';
			break;

		case '\t':
			*bp++ = '\\';
			*bp++ = 't';
			break;

		case '\n':
			*bp++ = '\\';
			*bp++ = 'n';
			break;

		case '\f':
			*bp++ = '\\';
			*bp++ = 'f';
			break;

		case '\r':
			*bp++ = '\\';
			*bp++ = 'r';
			break;

		default:
			*bp++ = ' ';
			if ((*cp&0177) > ' ' && (*cp&0177) < 0177)
				*bp++ = *cp;
			else
				*bp++ = ' ';
		}
		*bp++ = ' ';
	}
	bp[-1] = '\0';
}
