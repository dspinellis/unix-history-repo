/* vtd.c - VT responder */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/vt/RCS/vtd.c,v 7.3 91/02/22 09:48:28 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/vt/RCS/vtd.c,v 7.3 91/02/22 09:48:28 mrose Interim $
 *
 *
 * $Log:	vtd.c,v $
 * Revision 7.3  91/02/22  09:48:28  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/07/09  14:52:06  mrose
 * sync
 * 
 * Revision 7.1  89/11/30  23:51:42  mrose
 * pa2str
 * 
 * Revision 7.0  89/11/23  22:31:55  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#undef MAP_BACKSPACE	/*Map backspace character to VT ERASE CHAR*/

#include <signal.h>
#include "vtpm.h"
#include "sector1.h"
#include "tailor.h"
#include <sys/ioctl.h>
#ifdef BSD44
#include <sys/termios.h>
#include <sys/ttydefaults.h>
#endif

#ifndef _PATH_LOGIN
#ifndef BSD44
#define _PATH_LOGIN "/bin/login"
#else
#define _PATH_LOGIN "/usr/bin/login"
#endif
#endif

#include <arpa/telnet.h>

#include <ctype.h>
#include <setjmp.h>
#include <pwd.h>
#include <varargs.h>

#define	BELL	'\07'
#ifndef	SUNOS4
#define BANNER	"\r\n\r\n4.2 BSD UNIX (%s)\r\n\r\n\r%s"
#else
#define BANNER	"\r\n\r\nSunOS UNIX (%s)\r\n\r\n\r%s"
#endif

int	connected = FALSE;
char	command[256];


/*
 * I/O data buffers, pointers, and counters.
 */
char	ptyobuf[BUFSIZ], *pfrontp = ptyobuf, *pbackp = ptyobuf;
char	netobuf[BUFSIZ], *nfrontp = netobuf, *nbackp = netobuf;
int pcc;

VT_PROFILE vtp_profile;
int rflag = 0;
char erase_char;
char erase_line;
char intr_char;
char *crp = "\r";
char *my_displayobj = "D";	/*Acceptor's Display Object Name*/
char *my_echo_obj = "NA";	/*Acceptor's Negotiation Control Object Name*/
char *my_signal_obj = "DI";	/*Acceptor's Signal Control Object*/
char *his_echo_obj = "NI";	/*Initiator's Negotiation Control Object*/
char *his_signal_obj = "KB";	/*Initiator/s Signal control Object*/
int my_right = ACCEPTOR;
char kb_image;			/*KB Control Object image*/
char di_image;			/*DI Control Object Image*/
char ni_image;			/*NI Control Object image*/
char na_image;			/*NA Control Object image*/
char nego_state;		/*Current state of NA affected options*/
char sync_image;		/*SY Control Object image*/
char ga_image;
int transparent = 0;		/*Flag for Transparent repertoire*/
int telnet_profile =1;
int do_break = 1;		/*If VT-BREAK agreed to*/
int showoptions = 0;
int debug = 0;

#ifdef BSD44
struct termios oterm;
#else
struct tchars otc;
struct ltchars oltc;
struct sgttyb ottyb;
#endif
char *myhostname;
char peerhost[BUFSIZ];
struct PSAPaddr ts_bound;
struct passwd *pwd;

int	pty, net;
int	inter;
extern	char **environ;
extern	int errno;
char	line[] = "/dev/ptyp0";
char	*envinit[] = { "TERM=network", 0 };
SFD	cleanup();
static int do_cleaning = 0;

char   *myname;
LLog    _vt_log = {
    "vt.log", NULLCP, NULLCP,
    LLOG_FATAL | LLOG_EXCEPTIONS | LLOG_NOTICE, LLOG_FATAL, -1,
    LLOGCLS | LLOGCRT | LLOGZER, NOTOK
 
};
LLog   *vt_log = &_vt_log;

main(argc, argv)
	int	argc;
	char *argv[];
{
	int f = 0;
	char *cp = line;
	char *logname = NULLCP;
	int i, p, t;
	char   j;
#ifndef BSD44
	struct sgttyb b;
#endif

    if (myname = rindex (*argv, '/'))
	myname++;
    if (myname == NULL || *myname == NULL)
	myname = *argv;

    isodetailor (myname, 0);
    if (debug = isatty (fileno (stderr)))
	ll_dbinit (vt_log, myname);
    else
	ll_hdinit (vt_log, myname);

	for(i=1; i<(argc - 2); i++)
	{
		if (!strcmp(argv[i], "-d"))
		{
			if (!isdigit(argv[++i][0])
			        || sscanf(argv[i], "%d", &debug) != 1)
			    adios (NULLCP, "usage: %s -d 0-7", myname);
						advise(LLOG_DEBUG,NULLCP, "setting debug level to %d", debug);
			if (debug)
			    ll_dbinit (vt_log, myname);
		}
		else if(!strcmp(argv[i], "-F"))
		{
			if ((logname = argv[++i]) == NULL || *logname == '-')
			    adios (NULLCP, "usage: %s -F logfile", myname);
			vt_log -> ll_file = logname;
			(void) ll_close (vt_log);
			advise(LLOG_DEBUG,NULLCP, "logging to %s",logname);
		}
		else
		    adios(NULLCP, "usage: %s [-F logfile] [-d N]",
			  myname);
	}

    advise (LLOG_NOTICE,NULLCP,  "starting");

	acc = &accs;
	acr = &acrs;
	aci = &acis;
	acs = &acss;

	if (ass_ind(argc,argv) == OK) {
		connected = TRUE;
		if( !strcmp(vtp_profile.profile_name,"default") )
			telnet_profile = 0;
	}
	else
		exit(1);
#ifdef BSD44
	na_image = 0;
	nego_state = 0;			/*Start off in Local echo*/
	i = forkpty(&p, line, NULL, NULL);
	if (i == -1) {
		perror("vtd -- forkpty");
		vrelreq();
		/*NOTREACHED*/
	}
	if (i) {
		 vtd(sd, p);
		 /*NOTREACHED*/
	}
#else
/*
 * Get a pty, scan input lines.
 */
        for (j = 'p' ; j <= 't'; j++) {
	    cp[strlen ("/dev/pty")] = j;
	    for (i = 0; i < 16; i++) {
		cp[strlen("/dev/ptyp")] = "0123456789abcdef"[i];
		p = open(cp, 2);
		if (p >= 0)
		    goto gotpty;
	    }
	}
	perror("All network ports in use");
	vrelreq();
	/*NOTREACHED*/
gotpty:

	cp[strlen("/dev/")] = 't';
	t = open("/dev/tty", 2);
	if (t >= 0) {
		if (ioctl(t, TIOCNOTTY, 0) == -1) {
			perror("ioctl (TIOCNOTTY)");
		}
		(void)close(t);
	}
	t = open(cp, 2);
	if (t < 0)
		fatalperror(f, cp, errno);
	if (ioctl(t, TIOCGETP, (char*)&b) == -1) {
		perror("ioctl (TIOCGETP)");
		adios(NULLCP, "ioctl failed (TIOCGETP)");
	}
	b.sg_flags = CRMOD|XTABS|ANYP;
	if (ioctl(t, TIOCSETP, (char*)&b) == -1) {
		perror("ioctl (TIOCSETP)");
		adios(NULLCP, "ioctl failed (TIOCSETP)");
	}
	if (ioctl(p, TIOCGETP, (char*)&b) == -1) {	/* XXX why is this done on the controller */
		perror("ioctl (TIOCGETP)");
		adios(NULLCP, "ioctl failed (TIOCGETP)");
	}
	if (telnet_profile)
		b.sg_flags &= ~ECHO;
	else
		b.sg_flags |= ECHO;	/*Remote echo for Default*/
	if (ioctl(p, TIOCSETP, (char*)&b) == -1) {
		perror("ioctl (TIOCSETP)");
		adios(NULLCP, "ioctl failed (TIOCSETP)");
	}
	na_image = 0;
	nego_state = 0;			/*Start off in Local echo*/

	if ((i = fork()) < 0)
		fatalperror(f, "fork", errno);
	if (i)
		 vtd(sd, p);
	(void)close(sd);
	(void)close(p);
	if (dup2(t, 0) == -1) {
		perror("dup2");
		adios(NULLCP, "dup2 failed");
	}
	if (dup2(t, 1) == -1) {
		perror("dup2");
		adios(NULLCP, "dup2 failed");
	}
	if (dup2(t, 2) == -1) {
		perror("dup2");
		adios(NULLCP, "dup2 failed");
	}
	(void)close(t);
#endif
	environ = envinit;

	execl(_PATH_LOGIN,"login","-h",peerhost,NULLCP);
	fatalperror(f, _PATH_LOGIN, errno);
	/*NOTREACHED*/
}

fatal(f, msg)
	int f;
	char *msg;
{
	char buf[BUFSIZ];

	(void) sprintf(buf, "%s: %s.\n", myname, msg);
	(void) write(f, buf, strlen(buf));
	adios (NULLCP, msg);
}

fatalperror(f, msg, errnum)
	int f;
	char *msg;
	int errnum;
{
	char buf[BUFSIZ];
	extern char *sys_errlist[];

	(void) sprintf(buf, "%s: %s", msg, sys_errlist[errnum]);
	fatal(f, buf);
}

/*
 * Main loop.  Select from pty and network.
 */

vtd(f, p)
{
	int on = 1;
	int	nfds, result;

	do_cleaning = 1;
	net = f, pty = p;
	nfds = (f > p ? f : p) + 1;

	if (ioctl(p, FIONBIO, (char*)&on) == -1) {
		perror("ioctl");
		adios(NULLCP, "ioctl failed (FIONBIO)");
	}
#ifdef	SIGTSTP
	(void) signal(SIGTSTP, SIG_IGN);
#endif
#ifdef	SIGCHLD
	(void) signal(SIGCHLD, cleanup);
#endif
	/*
	 * Show banner that getty never gave.
	 */
	myhostname = PLocalHostName ();
	(void) sprintf(nfrontp, BANNER, myhostname, "");
	nfrontp += strlen(nfrontp);

#ifdef BSD44
	if (tcgetattr(pty, &oterm) == -1) {
		perror("tcgetattr");
		adios(NULLCP, "tcgetattr failed");
	}
	if (telnet_profile) {
		oterm.c_lflag &= ~ECHO;
		if (tcsetattr(pty, TCSADRAIN, &oterm) == -1) {
			perror("tcgetattr");
			adios(NULLCP, "tcgetattr failed");
		}
	}
	erase_char = oterm.c_cc[VERASE];
	erase_line = oterm.c_cc[VKILL];
	intr_char = oterm.c_cc[VINTR];
#else
	if (ioctl(pty,TIOCGETP,(char*)&ottyb) == -1) {
		perror("ioctl");
		adios(NULLCP, "ioctl failed (TIOCGETP)");
	}
	if (ioctl(pty,TIOCGETC,(char*)&otc) == -1) {
		perror("ioctl");
		adios(NULLCP, "ioctl failed (TIOCGETC)");
	}
	if (ioctl(pty,TIOCGLTC,(char*)&oltc) == -1) {
		perror("ioctl");
		adios(NULLCP, "ioctl failed (TIOCGLTC)");
	}
	erase_char = ottyb.sg_erase;
	erase_line = ottyb.sg_kill;
	intr_char = otc.t_intrc;
#endif


	for (;;) {
	    	fd_set    ibits, obits;
		register int c;

		FD_ZERO (&ibits);
		FD_ZERO (&obits);
		/*
		 * Never look for input if there's still
		 * stuff in the corresponding output buffer
		 */
		if (nfrontp - nbackp) {
		    FD_SET (f, &obits);
		}
		else {
		    FD_SET (p, &ibits);
		}
		if (pfrontp - pbackp) {
		    FD_SET (p, &obits);
		}
		else {
		    FD_SET (f, &ibits);
		}
		if (FD_ISSET (f, &ibits) && data_pending()) {
		        FD_CLR (f, &ibits);

			result = xselect(nfds, &ibits, &obits,
					 (fd_set *)NULL, OK);

			if (result < 0)
				adios("failed", "xselect");
			FD_SET (f, &ibits);
		}
		else {
			if (xselect(nfds, &ibits, &obits, (fd_set *)NULL,
				    NOTOK) == -1)
			    adios("failed", "xselect");
		}
		if (!FD_ISSET (f, &ibits)
		        && !FD_ISSET (p, &ibits)
		        && !FD_ISSET (f, &obits)
		        && !FD_ISSET (p, &obits)) {
			sleep(5);
			continue;
		}

		/*
		 * Something to read from the network...
		 */
		if (FD_ISSET (f, &ibits)) {
			while ((c = getch()) > 0) 
				*pfrontp++ = c;
		}
		if (c == E_EOF) {
			break;
		}

		/*
		 * Something to read from the pty...
		 */
		if (FD_ISSET (p, &ibits)) {
			pcc = read(p, nfrontp, (&netobuf[BUFSIZ] - nfrontp));

			if (pcc < 0 && errno == EWOULDBLOCK)
				pcc = 0;
			else {
				if (pcc <= 0) {
					if (debug)
					    advise(LLOG_EXCEPTIONS,NULLCP,
						   "problem reading from pty");
					break;
				}
			}
			nfrontp += pcc;
		}

		if (FD_ISSET (f, &obits) && (nfrontp - nbackp) > 0)
			netflush();

		if (FD_ISSET (p, &obits) && (pfrontp - pbackp) > 0)
			ptyflush();
	}
	if (debug)
		advise(LLOG_DEBUG,NULLCP,  "finished loop in vtp");
	cleanup();
}

/*
 * Send interrupt to process on other side of pty.
 * If it is in raw mode, just write NULL;
 * otherwise, write intr char.
 */
interrupt()
{
#ifdef BSD44
	struct termios term;

	ptyflush();
	if (tcgetattr(pty, &term) == -1) {
		perror("tcgetattr");
		return;
	}
	if ((term.c_lflag&ISIG) && term.c_cc[VINTR] != _POSIX_VDISABLE)
		*pfrontp++ = term.c_cc[VINTR];
	else
		*pfrontp++ = '\0';
#else
	struct sgttyb b;
	struct tchars tchars;

	ptyflush();	/* half-hearted */
	if (ioctl(pty, TIOCGETP, (char*)&b) == -1) {
		perror("ioctl");
		adios(NULLCP, "ioctl failed");
	}
	if (b.sg_flags & RAW) {
		*pfrontp++ = '\0';
		return;
	}
	*pfrontp++ = ioctl(pty, TIOCGETC, (char*)&tchars) < 0 ?
		'\177' : tchars.t_intrc;
#endif
}

netflush()
{
	register char *cp;
	int n;
	int i, j;
	int nl_flag;	/*Records if Newline is included in current PDU to
			  decide if Deliver Request should follow it.  Should
			  not be required but some implementations may wait
			  for it before delivering NDQ to application*/

	nl_flag = 0;
	if ((n = nfrontp - nbackp) > 0) {

		if (debug) {
			(void) ll_log (vt_log, LLOG_DEBUG, NULLCP,
				("writing to the net"));
			(void) ll_printf (vt_log, "<<");
			for(i=0; i<(nfrontp-nbackp); i++)
			    (void)ll_printf (vt_log, "%02x ",*(nbackp+i));
			(void)ll_printf (vt_log,  ">>\n");
			(void)ll_sync (vt_log);
		}
		if(transparent)
		{
			(void)vt_text(nbackp,n);
			vtsend();
			cp = nbackp;
			for(i=0; i<n; i++)
			{
				if((*cp == '\r') ||
				   (*cp == '\n'))
				{
					vdelreq(FALSE);
					break;
				}
				++cp;
			}
			nbackp += n;
		}
		else
		{
		    cp = nbackp;
		    for(i=0,j=0; i<n; i++)
		    {
			if(*cp == '\r')
			{
			    if(rflag) (void)vt_text(crp,1);
				/*Previous char was CR so put one in NDQ*/
			    if(j) 
					(void)vt_text(nbackp,j);
			    nbackp += (j+1); /*Skip over current CR*/
			    cp = nbackp;
			    j = 0;
			    if(i == (n-1) ) (void)vt_text(crp,1);
				/*If CR is last char in buffer, send it*/
			    else rflag = 1;
				/*If not last char in buffer, read next one*/
			    continue;
			}
			else if(rflag) /*If previous character was CR*/
			{
			    if(*cp == '\n') /*Got CR-LF -- map to Next X-Array*/
			    {
				nbackp += (j+1);
				cp = nbackp;
				rflag = 0;
				vt_newline();
				++nl_flag;
				continue;
			    }
			    else /*Preceeding char was CR but not followed by
				   LF.  Put CR in buffer*/
			        (void) vt_text(crp,1);
			    rflag = 0;
			} 
			if(telnet_profile)
			{
			    rflag = 0;
#ifdef MAP_BACKSPACE
			    if(*cp == 0x08) /*If believed to be erase*/
			    {
				    if(j) 
						(void)vt_text(nbackp,j);
				    nbackp += (j+1);
				    cp = nbackp;
				    j = 0;
			 	    vt_char_erase();
				    continue;
			    }
#endif
			    if(!vtp_profile.arg_val.tel_arg_list.full_ascii)
					/*If ASCII GO, dump ctrl chars*/
			    {
				if((*cp < 0x20) || (*cp > 0x7e))
				{
				    if(j) 
						(void)vt_text(nbackp,j);
				    nbackp += (j+1);
				    cp = nbackp;
				    j = 0;
				}
				else
				{
				    ++j; ++cp;
				}
			    }
			    else 
			    {
				++j;
				++cp;
			    }
			}
			else		/*Else Default Profile*/
			{
			    if((*cp < 0x20) || (*cp > 0x7e))
			    {
				if(j) 
					(void)vt_text(nbackp,j);
				nbackp += (j+1);
				cp = nbackp;
				j = 0;
			    }
			    else 
			    {
				++j;
				++cp;
			    }
			}
		    }		/*End for loop*/
		    if(j) 
				(void)vt_text(nbackp,j); /*Load anything left if CR or LF
						wasn't last char in buffer*/
		    nbackp += j;
		    vtsend();	/*Send the whole NDQ*/
		    if(nl_flag && telnet_profile) vdelreq(FALSE);
		}
	}
	if (n < 0) {
		if (errno != ENOBUFS && errno != EWOULDBLOCK) {
			adios("closed", "association");
			/*NOTREACHED*/
		}
		n = 0;
	}
	if (nbackp == nfrontp)
		nbackp = nfrontp = netobuf;
}

SFD	cleanup()
{
	sleep(1);
	while(getch() > 0);	/*Clean out unread VT-DATA PDU's still held
				  in network.  Kludge to overcome deficiency
				  in Session Release. */
	rmut();
#ifndef BSD44
	vhangup();
#endif
	vrelreq();
	(void)kill(0, SIGKILL);
	exit(1);
}

#include <utmp.h>

struct	utmp wtmp;
char	wtmpf[]	= "/usr/adm/wtmp";
char	utmp[] = "/etc/utmp";
#define SCPYN(a, b)	strncpy(a, b, sizeof (a))
#define SCMPN(a, b)	strncmp(a, b, sizeof (a))

long	lseek (), time ();

rmut()
{
	register f;
	int found = 0;

	f = open(utmp, 2);
	if (f >= 0) {
		while(read(f, (char *)&wtmp, sizeof (wtmp)) == sizeof (wtmp)) {
			if (SCMPN(wtmp.ut_line, line+5) || wtmp.ut_name[0]==0)
				continue;
			(void)lseek(f, -(long)sizeof (wtmp), 1);
			(void)SCPYN(wtmp.ut_name, "");
#if	!defined(SYS5) && !defined(bsd43_ut_host)
			(void)SCPYN(wtmp.ut_host, "");
#endif
			(void)time(&wtmp.ut_time);
			(void) write(f, (char *)&wtmp, sizeof (wtmp));
			found++;
		}
		(void)close(f);
	}
	if (found) {
		f = open(wtmpf, 1);
		if (f >= 0) {
			(void)SCPYN(wtmp.ut_line, line+5);
			(void)SCPYN(wtmp.ut_name, "");
#if	!defined(SYS5) && !defined(bsd43_ut_host)
			(void)SCPYN(wtmp.ut_host, "");
#endif
			(void)time(&wtmp.ut_time);
			(void)lseek(f, (long)0, 2);
			(void) write(f, (char *)&wtmp, sizeof (wtmp));
			(void)close(f);
		}
	}
	(void)chmod(line, 0666);
	(void)chown(line, 0, 0);
	line[strlen("/dev/")] = 'p';
	(void)chmod(line, 0666);
	(void)chown(line, 0, 0);
}

bye()
{
    if(do_cleaning) {
	rmut();
	(void)kill(0, SIGKILL);
    }
    exit(0);
}
	
flushbufs()
{
	pcc = 0;
	pfrontp = pbackp = ptyobuf;
	nfrontp = nbackp = netobuf;
	while (getch() > 0)
	    continue;
}

/*    ERRORS */

void	finalbye ()
{
    bye ();
}


#ifndef	lint
void	adios (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    (void) _ll_log (vt_log, LLOG_FATAL, ap);

    va_end (ap);

    bye ();

    _exit (1);
}
#else
/* VARARGS2 */

void	adios (what, fmt)
char   *what,
       *fmt;
{
    adios (what, fmt);
}
#endif


#ifndef	lint
void	advise (va_alist)
va_dcl
{
    int	    code;
    va_list ap;

    va_start (ap);

    code = va_arg (ap, int);

    (void) _ll_log (vt_log, code, ap);

    va_end (ap);
}
#else
/* VARARGS3 */

void	advise (code, what, fmt)
int	code;
char   *what,
       *fmt;
{
    advise (code, what, fmt);
}
#endif

ptyflush()
{
	int n;

	if ((n = pfrontp - pbackp) > 0)
	{
		n = write(pty, pbackp, n);
	}
	if (n < 0)
		return;
	pbackp += n;
	if (pbackp == pfrontp)
		pbackp = pfrontp = ptyobuf;
}

#ifdef BSD44
ptyecho(on)
{
	struct termios term;

	ptyflush();
	if (tcgetattr(pty, &term) == -1) {
		perror("tcgetattr");
		return;
	}
	if (on)
		term.c_lflag |= ECHO;
	else
		term.c_lflag &= ECHO;
	if (tcsetattr(pty, TCSAFLUSH, &term) == -1) {
		perror("tcsetattr");
		return;
	}
}
#else
setmode(on, off)
	int on, off;
{
	struct sgttyb b;

	ptyflush();
	if(ioctl(pty, TIOCGETP, (char*)&b) < 0) {
		perror("ioctl");
		adios(NULLCP, "ioctl failed");
	}
	b.sg_flags |= on;
	b.sg_flags &= ~off;
	if (ioctl(pty, TIOCSETP, (char*)&b) == -1) {
		perror("ioctl");
		adios(NULLCP, "ioctl failed");
	}
}
#endif
