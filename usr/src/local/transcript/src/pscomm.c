#ifndef lint
static char Notice[] = "Copyright (c) 1985 Adobe Systems Incorporated";
static char *ORCSID="$Header: pscomm.bsd,v 2.1 85/11/24 11:50:16 shore Rel $";
static char *RCSID="$Header: pscomm.c,v 1.4 87/10/31 20:42:02 cuong Exp $";
#endif
/* pscomm.c
 *
 * Copyright (C) 1985 Adobe Systems Incorporated
 *
 * 4.2BSD lpr/lpd communications filter for PostScript printers
 * (formerly "psif" in TranScript release 1.0)
 *
 * pscomm is the general communications filter for
 * sending files to a PostScript printer (e.g., an Apple LaserWriter,
 * QMS PostScript printer, or Linotype PostScript typesetter)
 * via RS232 lines.  It does page accounting, error handling/reporting,
 * job logging, banner page printing, etc.
 * It observes (parts of) the PostScript file structuring conventions.
 * In particular, it distinguishes between PostScript files (beginning
 * with the "%!" magic number) -- which are shipped to the printer --
 * and text files (no magic number) which are formatted and listed
 * on the printer.  Files which begin with "%!PS-Adobe-" may be
 * page-reversed if the target printer has that option specified.
 *
 * depending on the values of BANNERFIRST and BANNERLAST, 
 * pscomm looks for a file named ".banner", (created by the "of" filter)
 * in the current working directory and ships it to the printer also.
 *
 * pscomm gets called with:
 *	stdin	== the file to print (may be a pipe!)
 *	stdout	== the printer
 *	stderr	== the printer log file
 *	cwd	== the spool directory
 *	argv	== set up by interface shell script:
 *	  filtername	-P printer
 *			-p filtername
 *			[-r]		(don't ever reverse)
 *			-n login
 *			-h host
 *			[accntfile]
 *
 *	environ	== various environment variable effect behavior
 *		VERBOSELOG	- do verbose log file output
 *		BANNERFIRST	- print .banner before job
 *		BANNERLAST	- print .banner after job
 *		REVERSE		- page reversal filter program
 *				  (no reversal if null or missing)
 *		PSLIBDIR	- transcript library directory
 *		PSTEXT		- simple text formatting filter
 *		JOBOUTPUT	- file for actual printer stream
 *				  output (if defined)
 *
 * pscomm depends on certain additional features of the 4.2BSD spooling
 * architecture.  In particular it assumes that the printer status file
 * has the default name (./status) and it uses this file to communicate
 * printer error status information to the user -- the contents of the
 * status file gets incorporated in "lpq" and "lpc status" messages.
 *
 * Edit History:
 * Andrew Shore: Sat Nov 16 11:59:58 1985
 * End Edit History.
 *
 * RCSLOG:
 * $Log:	pscomm.c,v $
 * Revision 1.4  87/10/31  20:42:02  cuong
 * Two changes:
 *     1. Make sender wait for listener's signal when requesting initial
 *        pagecount.  The problem is with short jobs & fast machines;
 *        the sender will merrily finish the job and send an asynchronous
 *        status request the response to which will clobber the listener's
 *        input stream.
 *     2. Make sender sleep(1) just after sending the job-finish EOF to give
 *        the LaserWriter a chance to update its status.
 * 
 * Revision 1.3  87/10/03  16:42:47  cuong
 * Takes care of improper handling of abnormal exits when
 * accounting is turned on.  Pagecount information was again
 * waited on abortively.  Now, it's fixed, no?
 * 
 * Revision 1.2  87/09/20  19:03:25  cuong
 * Fixed bug:
 * Symptom: pagecount accounting turned on, then pscomms will hang.
 * Reason: old pscomm listener assumed the final pagecount information
 *         will come after the ctrl-d; this is not true.  Hence it
 * 	hangs waiting after the ctrl-d is received.
 * Fix:    while waiting for ctrl-d, the pscomm listener must also
 *         scan for the pattern %%[ pagecount: %d ]%%, and save
 *         this in the pbuf[] array if found.  
 * Cuong
 * 
 * Revision 1.1  87/06/13  19:26:31  cuong
 * Initial revision
 * 
 * Revision 2.1  85/11/24  11:50:16  shore
 * Product Release 2.0
 * 
 * Revision 1.1  85/11/20  00:35:21  shore
 * Initial revision
 * 
 * Revision 1.2  85/05/14  11:25:29  shore
 * better support for BANNERLAST, still buggy though
 * 
 *
 */

#include <ctype.h>
#include <setjmp.h>
#include <sgtty.h>
#include <signal.h>
#include <stdio.h>
#include <strings.h>

#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/wait.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "transcript.h"
#include "psspool.h"

#ifdef BDEBUG
#define debugp(x) \
{ \
   fprintf(stderr, "(pid %d) ", getpid()); \
   fprintf x; \
   (void) fflush(stderr); \
}
#else
#define debugp(x)
#endif BDEBUG

/*
 * the following string is sent to the printer when we want it to
 * report its current pagecount (for accounting)
 */

private char *getpages = "\n(%%%%[ pagecount: )print \
statusdict/pagecount get exec(                )cvs print( ]%%%%)= flush\n%s";

private jmp_buf waitonreverse, startstatus, dwait, sendint;

private char	*prog;			/* invoking program name */
private char	*name;			/* user login name */
private char	*host;			/* host name */
private char	*pname;			/* printer name */
private char	*accountingfile;	/* file for printer accounting */
private int	doactng;		/* true if we can do accounting */
private int	progress, oldprogress;	/* finite progress counts */
private int	getstatus = FALSE;
private int	revdone = FALSE;	/* reverse done, send new */
private int	goahead = FALSE;	/* got initial status back */
private int	gotemt = FALSE;		/* got ^D ack from listener */
private int	sendend = TRUE;		/* send an ^D */

private char *bannerfirst;
private char *bannerlast;
private char *verboselog;
private char *reverse;
private int BannerFirst;
private int BannerLast;
private int VerboseLog;

private int	fpid = 0;	/* formatter pid */
private int	cpid = 0;	/* listener pid */

private int	intrup = FALSE;	/* interrupt flag */

private char abortbuf[] = "\003";	/* ^C abort */
private char statusbuf[] = "\024";	/* ^T status */
private char eofbuf[] = "\004";		/* ^D end of file */

private char EOFerr[] = "%s: unexpected EOF from printer (%s)!\n";

/* global file descriptors (avoid stdio buffering!) */
private int fdsend;		/* to printer (from stdout) */
private int fdlisten;		/* from printer (same tty line) */
private int fdinput;		/* file to print (from stdin) */

private FILE *jobout;		/* special printer output log */

private int flg = FREAD|FWRITE;	 /* ioctl FLUSH arg */


extern char *getenv();

private VOID	intinit();
private VOID	intsend();
private VOID	intwait();
private VOID	salarm();
private VOID	walarm();
private VOID	falarm();
private VOID	reverseready();
private VOID	readynow();
private VOID	emtdead();
private VOID	emtdone();
private char 	*FindPattern();

#define SENDALARM 90
#define WAITALARM 30

main(argc,argv)
	int argc;
	char *argv[];
{
    register char  *cp;
    register int cnt, wc;
    register char *mbp;

    char  **av;
    long clock;		/* for log timestamp */
    char magic[11];	/* first few bytes of stdin ?magic number and type */
    int  noReverse = 0; /* flag if we should never page reverse */
    int  canReverse = 0;/* flag if we can page-reverse the ps file */
    int  reversing = 0;
    FILE *streamin;

    char mybuf[BUFSIZ];
    int wpid;
    union wait status;
    int fdpipe[2];
    int format = 0;
    int i;

    VOIDC signal(SIGINT, intinit);
    VOIDC signal(SIGHUP, intinit);
    VOIDC signal(SIGQUIT, intinit);
    VOIDC signal(SIGTERM, intinit);

    /* parse command-line arguments */
    /* the argv (see header comments) comes from the spooler daemon */
    /* itself, so it should be canonical, but at least one 4.2-based */
    /* system uses -nlogin -hhost (insead of -n login -h host) so I */
    /* check for both */

    av = argv;
    prog = *av;

    while (--argc) {
	if (*(cp = *++av) == '-') {
	    switch (*(cp + 1)) {
		case 'P':	/* printer name */
		    argc--;
		    pname = *(++av);
		    break;

		case 'n': 	/* user name */
		    argc--;
		    name = *(++av);
		    break;

		case 'h': 	/* host */
		    argc--;
		    host = *(++av);
		    break;

		case 'p':	/* prog */
		    argc--;
		    prog = *(++av);
		    break;

		case 'r':	/* never reverse */
		    argc--;
		    noReverse = 1;
		    break;

		default:	/* unknown */
		    fprintf(stderr,"%s: unknown option: %s\n",prog,cp);
		    break;
	    }
	}
	else
	    accountingfile = cp;
    }

    debugp((stderr,"args: %s %s %s %s\n",prog,host,name,accountingfile));

    /* do printer-specific options processing */

    VerboseLog = 1;
    BannerFirst = BannerLast = 0;
    reverse = NULL;
    if (bannerfirst=envget("BANNERFIRST")) {
	BannerFirst=atoi(bannerfirst);
    }
    if (bannerlast=envget("BANNERLAST")) {
	BannerLast=atoi(bannerlast);
    }
    if (verboselog=envget("VERBOSELOG")) {
	VerboseLog=atoi(verboselog);
    }
    if (!noReverse) {
	reverse=envget("REVERSE");	/* name of the filter itself */
    }

    if (VerboseLog) {
	fprintf(stderr, "%s: %s:%s %s start - %s", prog, host, name, pname,
            (VOIDC time(&clock), ctime(&clock)));
	VOIDC fflush(stderr);
    }
    debugp((stderr,"%s: pid %d ppid %d\n",prog,getpid(),getppid()));
    debugp((stderr,"%s: options BF %d BL %d VL %d R %s\n",prog,BannerFirst,
    	BannerLast, VerboseLog, ((reverse == NULL) ? "norev": reverse)));

    /* IMPORTANT: in the case of cascaded filters, */ 
    /* stdin may be a pipe! (and hence we cannot seek!) */

    if ((cnt = read(fileno(stdin),magic,11)) != 11) goto badfile;
    debugp((stderr,"%s: magic number is %11.11s\n",prog,magic));
    streamin = stdin;

    if (strncmp(magic,"%!PS-Adobe-",11) == 0) {
	canReverse = TRUE;
	goto go_ahead;
    }
    else if (strncmp(magic,"%!",2) == 0) {
	canReverse = FALSE;
	goto go_ahead;
    }

    /* here is where you might test for other file type
     * e.g., PRESS, imPRESS, DVI, Mac-generated, etc.
     */

    /* final sanity check on the text file, to guard
     * against arbitrary binary data
     */

    for (i = 0; i < 11; i++) {
	if (!isascii(magic[i]) || (!isprint(magic[i]) && !isspace(magic[i]))){
	    fprintf(stderr,"%s: spooled binary file rejected\n",prog);
	    VOIDC fflush(stderr);
	    sprintf(mybuf,"%s/bogusmsg.ps",envget("PSLIBDIR"));
	    if ((streamin = freopen(mybuf,"r",stdin)) == NULL) {
		exit(THROW_AWAY);
	    }
	    format = 1;
	    goto lastchance;
	}
    }

    goto format_text;

    badfile:
        fprintf(stderr,"%s: bad magic number, EOF\n", prog);
	VOIDC fflush(stderr);
	exit(THROW_AWAY);

    format_text:
        /* exec dumb formatter to make a listing */
	    debugp((stderr,"formatting\n"));
	    format = 1;
	    VOIDC lseek(0,0L,0);
	    rewind(stdin);
	    if (pipe (fdpipe)) pexit2(prog, "format pipe",THROW_AWAY);
	    if ((fpid = fork()) < 0) pexit2(prog, "format fork",THROW_AWAY);
	    if (fpid == 0) { /* child */
		/* set up child stdout to feed parent stdin */
		if (close(1) || (dup(fdpipe[1]) != 1)
		|| close(fdpipe[1]) || close(fdpipe[0])) {
		    pexit2(prog, "format child",THROW_AWAY);
		}
		execl(envget("PSTEXT"), "pstext", pname, 0);
	   	pexit2(prog,"format exec",THROW_AWAY);
	    }
	/* parent continues */
	/* set up stdin to be pipe */
	if (close(0) || (dup(fdpipe[0]) != 0)
	|| close(fdpipe[0]) || close(fdpipe[1])) {
	    pexit2(prog, "format parent",THROW_AWAY);
	}

	/* fall through to spooler with new stdin */
	/* can't seek here but we should be at the right place */
	streamin = fdopen(0,"r");
	canReverse = TRUE; /* we know we can reverse pstext output */

    go_ahead:

    /* do page reversal if specified */
    if (reversing = ((reverse != NULL) && canReverse)) {
	debugp((stderr,"reversing\n"));
	VOIDC setjmp(waitonreverse);
	if (!revdone) {
	    VOIDC signal(SIGEMT, reverseready);
	    if (pipe (fdpipe)) pexit2(prog, "reverse pipe", THROW_AWAY);
	    if ((fpid = fork()) < 0) pexit2(prog, "reverse fork", THROW_AWAY);
	    if (fpid == 0) { /* child */
		/* set up child stdout to feed parent stdin */
		if (close(1) || (dup(fdpipe[1]) != 1)
		|| close(fdpipe[1]) || close(fdpipe[0])) {
		    pexit2(prog, "reverse child", THROW_AWAY);
		}
		execl(reverse, "psrv", pname, 0);
		pexit2(prog,"reverse exec",THROW_AWAY);
	    }
	    /* parent continues */
	    if (close(0) || (dup(fdpipe[0]) != 0)
	    || close(fdpipe[0]) || close(fdpipe[1])) {
		pexit2(prog, "reverse parent", THROW_AWAY);
	    }
	    /* fall through to spooler with new stdin */
	    /* VOIDC lseek(0,0L,0); */
	    streamin = fdopen(0,"r");

	    while (TRUE) {
		if (revdone) break;
		pause();
	    }
	}
	VOIDC signal(SIGEMT, SIG_IGN);
	debugp((stderr,"%s: reverse feeding\n",prog));
    }

    lastchance:;

    /* establish an input stream from the printer --
     * the printcap entry specifies "rw" and we get
     * invoked with stdout == the device, so we
     * dup stdout, and reopen it for reading;
     * this seems to work fine...
     */

    fdinput = fileno(streamin); /* the file to print */
    fdsend = fileno(stdout);	/* the printer (write) */

    if ((fdlisten = dup(fdsend)) < 0) /* the printer (read) */
       pexit(prog, THROW_AWAY);

    doactng = name && accountingfile && (access(accountingfile, W_OK) == 0);

    /* get control of the "status" message file.
     * we copy the current one to ".status" so we can restore it
     * on exit (to be clean).
     * Our ability to use this is publicized nowhere in the
     * 4.2 lpr documentation, so things might go bad for us.
     * We will use it to report that printer errors condition
     * has been detected, and the printer should be checked.
     * Unfortunately, this notice may persist through
     * the end of the print job, but this is no big deal.
     */
    BackupStatus(".status","status");

    if ((cpid = fork()) < 0) pexit(prog, THROW_AWAY);
    else if (cpid) {/* parent - sender */
	VOIDC setjmp(sendint);

	if (intrup) {
	    /* we only get here if there was an interrupt */

	    fprintf(stderr,"%s: abort (sending)\n",prog);
	    VOIDC fflush(stderr);

	    /* flush and restart output to printer,
	     * send an abort (^C) request and wait for the job to end
	     */
	    if (ioctl(fdsend, TIOCFLUSH,&flg) || ioctl(fdsend, TIOCSTART,&flg)
	    || (write(fdsend, abortbuf, 1) != 1)) {
		RestoreStatus();
		pexit(prog,THROW_AWAY);
	    }
	    debugp((stderr,"%s: sent interrupt - waiting\n",prog));
	    intrup = 0;
	    goto donefile; /* sorry ewd! */
	}

        VOIDC signal(SIGINT, intsend);
        VOIDC signal(SIGHUP, intsend);
        VOIDC signal(SIGQUIT, intsend);
        VOIDC signal(SIGTERM, intsend);
	VOIDC signal(SIGEMT, readynow);

	progress = oldprogress = 0; /* finite progress on sender */
	getstatus = FALSE; /* prime the pump for fun FALSE; */

	VOIDC signal(SIGALRM, salarm); /* sending phase alarm */
	VOIDC alarm(SENDALARM); /* schedule an alarm/timeout */

	/* loop, trying to send a ^T to get printer status
	 * We will hang here (and post a message) if the printer
	 * is unreachable.  Eventually, we will succeed, the listener
	 * will see the status report, signal us, and we will proceed
	 */

	cnt = 1;
	VOIDC setjmp(startstatus);

	while (TRUE) {
	    if (goahead) break;
	    debugp((stderr,"%s: get start status\n",prog));
	    VOIDC write(fdsend, statusbuf, 1);
	    pause();
	    if (goahead) break; 
	    /* if we get here, we got an alarm */
	    ioctl(fdsend, TIOCFLUSH, &flg);
	    ioctl(fdsend, TIOCSTART, &flg);
	    sprintf(mybuf, "Not Responding for %d minutes",
	    	(cnt * SENDALARM+30)/60);
	    Status(mybuf);
	    alarm(SENDALARM);
	    cnt++;
	}

	VOIDC signal(SIGEMT, emtdead); /* now EMTs mean printer died */

	RestoreStatus();
	debugp((stderr,"%s: printer responding\n",prog));

	/* initial page accounting (BEFORE break page) */
	if (doactng) {
	    sprintf(mybuf, getpages, "\004");
	    VOIDC write(fdsend, mybuf, strlen(mybuf));
	    debugp((stderr, "%s: sent pagecount request\n", prog));
	    progress++;

            /* Sat Oct 31 17:51:45 PST 1987
             * loop, waiting for the listener to signal initial pagecount is
             * received.  The problem is with fast machines and short jobs;
             * if we don't loop here, we may finish the job and send another
             * CTRL-T before the initial pagecount ever came back.  The way
             * the laserwriter behaves, this may result in a mix of pagecount
             * data and status information like this:
             * %%[ pagecount: %%[ status: busy; source: serial 25 ]%% 24418 ]%%
             *
             * That is really silly - Cuong
             */

	    VOIDC signal(SIGINT, intsend);
	    VOIDC signal(SIGHUP, intsend);
	    VOIDC signal(SIGQUIT, intsend);
	    VOIDC signal(SIGTERM, intsend);
	    VOIDC signal(SIGEMT, readynow);

	    progress = oldprogress = 0; /* finite progress on sender */
	    getstatus = FALSE; /* prime the pump for fun FALSE; */

	    VOIDC signal(SIGALRM, salarm); /* sending phase alarm */
	    VOIDC alarm(SENDALARM); /* schedule an alarm/timeout */

	    cnt = 1; goahead = FALSE;
	    VOIDC setjmp(startstatus);

	    while (TRUE) {
		if (goahead) break;
		pause();
		if (goahead) break; 
		/* if we get here, we got an alarm */
		ioctl(fdsend, TIOCFLUSH, &flg);
		ioctl(fdsend, TIOCSTART, &flg);
		sprintf(mybuf, "Not Responding for %d minutes",
		    (cnt * SENDALARM+30)/60);
		Status(mybuf);
		alarm(SENDALARM);
		cnt++;
	    }

	    VOIDC signal(SIGEMT, emtdead); /* now EMTs mean printer died */

	    RestoreStatus();
	    debugp((stderr,"%s: sender received EMT (goahead) from listener\n",prog));
        } /* if (doactng) */

	/* initial break page ? */
	if (BannerFirst) {
	    SendBanner();
	    progress++;
	}

	/* ship the magic number! */
	if ((!format) && (!reversing)) {
	   VOIDC write(fdsend,magic,11);
	   progress++;
	}

	/* now ship the rest of the file */

	VOIDC alarm(SENDALARM); /* schedule an alarm */

	while ((cnt = read(fdinput, mybuf, sizeof mybuf)) > 0) {
	    /* VOIDC alarm(SENDALARM);	/* we made progress, reset alarm */
	    if (intrup == TRUE) break;

	    /* get status every other time */
	    if (getstatus) {
		debugp((stderr,"%s: get periodic status\n",prog));
		VOIDC write(fdsend, statusbuf, 1);
		getstatus = FALSE;
		progress++;
	    }
	    mbp = mybuf;
	    while ((cnt > 0) && ((wc = write(fdsend, mbp, cnt)) != cnt)) {
		/* this seems necessary but not sure why */
		if (wc < 0) {
		    fprintf(stderr,"%s: error writing to printer:\n",prog);
		    perror(prog);
		    RestoreStatus();
		    sleep(10);
		    exit(TRY_AGAIN);
		}
		mbp += wc;
		cnt -= wc;
		progress++;
	    }
	    progress++;
	}
	if (cnt < 0) {
	    fprintf(stderr,"%s: error reading from stdin: \n", prog);
	    perror(prog);
	    RestoreStatus();
	    sleep(10);
	    exit(TRY_AGAIN);	/* kill the listener? */
	}

	/* final break page ? */
	if (BannerLast) {
	    SendBanner();
	    progress++;
	}

	donefile:;

	sendend = 1;

	VOIDC setjmp(dwait);

	if (sendend && !gotemt) {

	    VOIDC signal(SIGEMT, emtdone);

	    debugp((stderr,"%s: done sending\n",prog));

	    /* now send the PostScript EOF character */
	    debugp((stderr,"%s: sending PostScript EOF\n",prog));
	    VOIDC write(fdsend, eofbuf, 1);
	    sendend = 0;
	    progress++;

	    VOIDC signal(SIGINT, intwait);
	    VOIDC signal(SIGHUP, intwait);
	    VOIDC signal(SIGQUIT, intwait);
	    VOIDC signal(SIGTERM, intwait);

	    VOIDC signal(SIGALRM, walarm);
	    VOIDC alarm(WAITALARM);
	    getstatus = TRUE;
	}

	/* for very short jobs and very fast machines,
	 * we've experienced that the whole job is sent
	 * before the LaserWriter has a chance to update
	 * its status.  Hence we may get a false idle
	 * status if we immediately send the statusbuf.
	 *
	 * Keep in mind that the LaserWriter status response
	 * is asynchronous to the datastream.
	 */
	sleep(1);

	/* wait to sync with listener EMT signal
	 * to indicate it got an EOF from the printer
	 */
	while (TRUE) {
	    if (gotemt) break;
	    if (getstatus) {
		debugp((stderr,"%s: get final status\n",prog));
		VOIDC write(fdsend, statusbuf, 1);
		getstatus = FALSE;
	    }
	    debugp((stderr,"waiting e%d i%d %d %d\n",
	    	gotemt,intrup,wpid,status));
	    wpid = wait(&status);
	    if (wpid == -1) break;
	}

	/* final page accounting */
	if (doactng) {
	    sprintf(mybuf, getpages, "\004");
	    VOIDC write(fdsend, mybuf, strlen(mybuf));
	    debugp((stderr, "%s: sent pagecount request\n", prog));
	    progress++;
        }

	/* wait for listener to die */
	VOIDC setjmp(dwait);
        while ((wpid = wait(&status)) > 0);
	VOIDC alarm(0);
	VOIDC signal(SIGINT, SIG_IGN);
	VOIDC signal(SIGHUP, SIG_IGN);
	VOIDC signal(SIGQUIT, SIG_IGN);
	VOIDC signal(SIGTERM, SIG_IGN);
	VOIDC signal(SIGEMT, SIG_IGN);
	debugp((stderr,"w2: s%lo p%d = p%d\n", status, wpid, cpid));

	if (VerboseLog) {
	    fprintf(stderr,"%s: end - %s",prog,
	    	(VOIDC time(&clock),ctime(&clock)));
	    VOIDC fflush(stderr);
	}
    RestoreStatus();
    exit(0);
    }
    else {/* child - listener */
      register FILE *psin;
      register int r;

      char pbuf[BUFSIZ]; /* buffer for pagecount info */
      char *pb;		/* pointer for above */
      int pc1, pc2; 	/* page counts before and after job */
      int sc;		/* pattern match count for sscanf */
      char *outname;	/* file name for job output */
      int havejobout = FALSE; /* flag if jobout != stderr */
      int ppid;		/* parent process id */

      VOIDC signal(SIGINT, SIG_IGN);
      VOIDC signal(SIGHUP, SIG_IGN);
      VOIDC signal(SIGQUIT, SIG_IGN);
      VOIDC signal(SIGTERM, SIG_IGN);
      VOIDC signal(SIGALRM, SIG_IGN);

      ppid = getppid();

      /* get jobout from environment if there, otherwise use stderr */
      if (((outname = envget("JOBOUTPUT")) == NULL)
      || ((jobout = fopen(outname,"w")) == NULL)) {
	  jobout = stderr;
      }
      else havejobout = TRUE;

      pc1 = pc2 = -1; /* bogus initial values */
      if ((psin = fdopen(fdlisten, "r")) == NULL) {
	  RestoreStatus();
	  pexit(prog, THROW_AWAY);
      }

      /* listen for first status (idle?) */
      pb = pbuf;
      *pb = '\0';
      while (TRUE) {
	  r = getc(psin);
	  if (r == EOF) {
	      fprintf(stderr, EOFerr, prog, "startup");
	      VOIDC fflush(stderr);
	      sleep(20); /* printer may be coming up */
	      /* RestoreStatus(); */
	      /* exit(TRY_AGAIN); */
	  }
	  if ((r & 0377) == '\n') break; /* newline */
	  *pb++ = r;
      }
      *pb = 0;
      debugp((stderr,"%s: initial status - %s\n",prog,pbuf));
      if (strcmp(pbuf, "%%[ status: idle ]%%\r") != 0) {
	  fprintf(stderr,"%s: initial status - %s\n",prog,pbuf);
	  VOIDC fflush(stderr);
      }

      /* flush input state and signal sender that we heard something */
      ioctl(fdlisten, TIOCFLUSH, &flg);

      VOIDC kill(ppid,SIGEMT);

      /* listen for first pagecount */
      if (doactng) {
        pb = pbuf;
	*pb = '\0';
	while (TRUE) {
	  r = getc(psin);
	  if (r == EOF) {
	      fprintf(stderr, EOFerr, prog, "accounting1");
	      VOIDC fflush(stderr);
	      RestoreStatus();
	      sleep(10);	/* give interface a chance */
	      exit(TRY_AGAIN);
	  }
	  if ((r&0377) == 004) break; /* PS_EOF */
	  *pb++ = r;
	}
	*pb = '\0';

	if (pb = FindPattern(pb, pbuf, "%%[ pagecount: ")) {
	    sc = sscanf(pb, "%%%%[ pagecount: %d ]%%%%\r", &pc1);
	}
	if ((pb == NULL) || (sc != 1)) {
	    fprintf(stderr, "%s: accounting error 1 (%s)\n", prog,pbuf);
	    VOIDC fflush(stderr);
	}
	debugp((stderr,"%s: accounting 1 (%s)\n",prog,pbuf));

        /* flush input state and signal sender that we heard something */
        ioctl(fdlisten, TIOCFLUSH, &flg);

        VOIDC kill(ppid,SIGEMT);

	/*
	    Sun Sep 20 18:32:28 PDT 1987
	    The previous bug was that it was assumed the ctrl-d comes
	    before the final pagecount.  This doesn't happen, and the
	    listener waits forever after a ctrl-d for a pagecount.
	    The fix is to clear out the pbuf[] buffer, then check for it
	    when we get to looking for the final pagecount.  If it is
	    non-empty, we know we *already* read the final pagecount
	    *before* the ctrl-d, and use it, without waiting for
	    anything to come back from the printer.
	*/
	pbuf[0] = '\0';
      }

      /* listen for the user job */
      while (TRUE) {
	r = getc(psin);
	debugp((stderr, "%s: listener got character \\%o '%c'\n", prog, r, r));
	if ((r&0377) == 004) break; /* PS_EOF */
	else if (r == EOF) {
	    VOIDC fclose(psin);
	    fprintf(stderr, EOFerr, prog, "job");
	    VOIDC fflush(stderr);
	    RestoreStatus();
	    VOIDC kill(ppid,SIGEMT);
	    exit(THROW_AWAY); 
	}
/*
    Sun Sep 20 18:37:01 PDT 1987
    GotChar() takes an addition argument: the pointer to the
    pbuf[] buffer, and fills it with the final pagecount
    information if that is received from the printer.
*/
	GotChar(r, pbuf);
      }

      /* let sender know we saw the end of the job */
      /* sync - wait for sender to restart us */

      debugp((stderr,"%s: listener saw eof, signaling\n",prog));

      VOIDC kill(ppid,SIGEMT);

      /* now get final page count */
      if (doactng) {
/*
    Sun Sep 20 18:48:35 PDT 1987
    We attempt to wait for the final pagecount only if it has *not*
    been sent by the printer.  It is the case that the final pagecount
    is sent before the ctrl-d above, hence if we wait, it'll be forever.
    Final pagecount information 'prematurely' received has already
    been stored in pbuf[] iff pbuf[0] is non-null.
*/

	if (pbuf[0] == '\0') {
	    debugp((stderr, "%s: waiting for pagecount\n", prog));
	    pb = pbuf;
	    *pb = '\0';	/* ignore the previous pagecount */
	    while (TRUE) {
	      r = getc(psin);
	      if (r == EOF) {
		  fprintf(stderr, EOFerr, prog, "accounting2");
		  VOIDC fflush(stderr);
		  RestoreStatus();
		  sleep(10);
		  exit(THROW_AWAY); /* what else to do? */
	      }
	      if ((r&0377) == 004) break; /* PS_EOF */
	      *pb++ = r;
	    }
	    *pb = '\0';
	} else {
	    pb = pbuf + strlen(pbuf) - 1;
	}
	debugp((stderr,"%s: accounting 2 (%s)\n",prog,pbuf));
	if (pb = FindPattern(pb, pbuf, "%%[ pagecount: ")) {
	    sc = sscanf(pb, "%%%%[ pagecount: %d ]%%%%\r", &pc2);
	}
	if ((pb == NULL) || (sc != 1)) {
	    fprintf(stderr, "%s: accounting error 2 (%s)\n", prog,pbuf);
	    VOIDC fflush(stderr);
	}
        else if ((pc2 < pc1) || (pc1 < 0) || (pc2 < 0)) {
	    fprintf(stderr,"%s: accounting error 3 %d %d\n", prog,pc1,pc2);
	    VOIDC fflush(stderr);
	}
	else if (freopen(accountingfile, "a", stdout) != NULL) {
	  printf("%7.2f\t%s:%s\n", (float)(pc2 - pc1), host, name);
	  VOIDC fclose(stdout);
/*
    Sun Sep 20 18:55:32 PDT 1987
    File append failure report added for future use.
*/
	} else {
	  debugp((stderr, "%s: can't append accounting file\n", prog));
	  perror(accountingfile);
	}
      }

      /* all done -- let sender know */
      if (havejobout) VOIDC fclose(jobout);
      VOIDC fclose(psin);
      exit(0); /* to parent */
    }
}

/* send the file ".banner" */
private SendBanner()
{
    register int banner;
    int cnt;
    char buf[BUFSIZ];

    if ((banner = open(".banner",O_RDONLY|O_NDELAY,0)) < 0) return;
    while ((cnt = read(banner,buf,sizeof buf)) > 0) {
	VOIDC write(fdsend,buf,cnt);
    }
    VOIDC close(banner);
    VOIDC unlink(".banner");
}

/* search backwards from p in start for patt */
private char *FindPattern(p, start, patt)
char *p;
char *start;
char *patt;
{
    int patlen;
    patlen = strlen(patt);
    
    p -= patlen;
    for (; p >= start; p--) {
	if (strncmp(p, patt, patlen) == 0) return(p);
    }
    return ((char *)NULL);
}

private GotChar(c, pbuf)
register int c;
char	*pbuf;
{
    static char linebuf[BUFSIZ];
    static char *cp = linebuf;
    static enum State {normal, onep, twop, inmessage,
    			close1, close2, close3, close4} st = normal;
    char *match, *last;

    switch (st) {
	case normal:
	    if (c == '%') {
		st = onep;
		cp = linebuf;
		*cp++ = c;
		break;
	    }
	    putc(c,jobout);
	    VOIDC fflush(jobout);
	    break;
	case onep:
	    if (c == '%') {
		st = twop;
		*cp++ = c;
		break;
	    }
	    putc('%',jobout);
	    putc(c,jobout);
	    VOIDC fflush(jobout);
	    st = normal;
	    break;
	case twop:
	    if (c == '\[') {
		st = inmessage;
		*cp++ = c;
		break;
	    }
	    if (c == '\%') {
		putc('%',jobout);
		VOIDC fflush(jobout);
		/* don't do anything to cp */
		break;
	    }
	    putc('%',jobout);
	    putc('%',jobout);
	    VOIDC fflush(jobout);
	    st = normal;
	    break;
	case inmessage:
	    *cp++ = c;
	    if (c == '\]') st = close1;
	    break;
	case close1:
	    *cp++ = c;
	    switch (c) {
		case '%': st = close2; break;
		case '\]': st = close1; break;
		default: st = inmessage; break;
	    }
	    break;
	case close2:
	    *cp++ = c;
	    switch (c) {
		case '%': st = close3; break;
		case '\]': st = close1; break;
		default: st = inmessage; break;
	    }
	    break;
	case close3:
	    *cp++ = c;
	    switch (c) {
		case '\r': st = close4; break;
		case '\]': st = close1; break;
		default: st = inmessage; break;
	    }
	    break;
	case close4:
	    *cp++ = c;
	    switch(c) {
		case '\n': st = normal; break;
		case '\]': st = close1; break;
		default: st = inmessage; break;
	    }
	    if (st == normal) {
		/* parse complete message */
		last = cp;
		*cp = 0;
		debugp((stderr,">>%s",linebuf));
		if (match = FindPattern(cp, linebuf, " PrinterError: ")) {
		    if (*(match-1) != ':') {
			fprintf(stderr,"%s",linebuf);
			VOIDC fflush(stderr);
			*(last-6) = 0;
			Status(match+15);
		    }
		    else {
			last = index(match,';');
			*last = 0;
			Status(match+15);
		    }
		}
		else if (match = FindPattern(cp, linebuf, " status: ")) {
		    match += 9;
		    if (strncmp(match,"idle",4) == 0) {
			/* we are hopelessly lost, get everyone to quit */
			fprintf(stderr,"%s: ERROR: printer is idle, giving up!\n",prog);
			VOIDC fflush(stderr);
			VOIDC kill(getppid(),SIGKILL); /* will this work */
			exit(THROW_AWAY);
		    }
		    else {
			/* one of: busy, waiting, printing, initializing */
			/* clear status message */
			RestoreStatus();
		    }
		}
/*
    Sun Sep 20 18:39:40 PDT 1987
    Additional else necessary: if we get the final pagecount
    information here from the printer, store it in the given
    array pbuf[].
*/
		else if (match = FindPattern(cp, linebuf, "%%[ pagecount: ")) {
		    /* fill pbuf */
		    strcpy(pbuf, linebuf);
		    debugp((stderr, "%s: 'premature' final pagecount read = '%s'\n", prog, pbuf));
		}
		else {
		    /* message not for us */
		    fprintf(jobout,"%s",linebuf);
		    VOIDC fflush(jobout);
		    st = normal;
		    break;
		}
	    }
	    break;
	default:
	    fprintf(stderr,"bad case;\n");
    }
    return;
}

/* backup "status" message file in ".status",
 * in case there is a PrinterError
 */

private BackupStatus(file1, file2)
char *file1, *file2;
{
    register int fd1, fd2;
    char buf[BUFSIZ];
    int cnt;

    VOIDC umask(0);
    fd1 = open(file1, O_WRONLY|O_CREAT, 0664);
    if ((fd1 < 0) || (flock(fd1,LOCK_EX) < 0)) {
	VOIDC unlink(file1);
	VOIDC flock(fd1,LOCK_UN);
	VOIDC close(fd1);
	fd1 = open(file1, O_WRONLY|O_CREAT, 0664);
    }
    if ((fd1 < 0) || (flock(fd1,LOCK_EX) <0)) {
	fprintf(stderr, "%s: writing %s:\n",prog,file1);
	perror(prog);
	VOIDC close(fd1);
	return;
    }
    VOIDC ftruncate(fd1,0);
    if ((fd2 = open(file2, O_RDONLY,0)) < 0) {
	fprintf(stderr, "%s: error reading %s:\n", prog, file2);
	perror(prog);
	VOIDC close(fd1);
	return;
    }
    cnt = read(fd2,buf,BUFSIZ);
    VOIDC write(fd1,buf,cnt);
    VOIDC flock(fd1,LOCK_UN);
    VOIDC close(fd1);
    VOIDC close(fd2);
}

/* restore the "status" message from the backed-up ".status" copy */
private RestoreStatus() {
    BackupStatus("status",".status");
}

/* report PrinterError via "status" message file */
private Status(msg)
register char *msg;
{
    register int fd;
    char msgbuf[100];

    if ((fd = open("status",O_WRONLY|O_CREAT,0664)) < 0) return;
    VOIDC ftruncate(fd,0);
    sprintf(msgbuf,"Printer Error: may need attention! (%s)\n\0",msg);
    VOIDC write(fd,msgbuf,strlen(msgbuf));
    VOIDC close(fd);
}

/* sending phase alarm handler for sender */

private VOID salarm() {

    debugp((stderr,"%s: AS %d %d %d\n",prog,oldprogress,progress,getstatus));

    /* if progress != oldprogress, we made some progress (sent something)
     * else, we had two alarms without sending anything...
     * It may be that a PrinterError has us stopped, or we are computing
     * for a long time (forever?) -- printer jobtimeout may help here
     * in any case, all we do is set the flag to get status...
     * this will help us clear printererror notification
     */

    oldprogress = progress;
    getstatus = TRUE;

    /* reset the alarm and return */
    VOIDC alarm(SENDALARM);
    return;
}

/* waiting phase alarm handler for sender */

private VOID walarm() {
    static int acount = 0;

    debugp((stderr,"%s: WA %d %d %d %d\n",
    	prog,acount,oldprogress,progress,getstatus));

    if ((oldprogress != progress) || (acount == 4)) {
	getstatus = TRUE;
	acount = 0;
	oldprogress = progress;
    }
    else acount++;

    /* reset alarm */
    VOIDC alarm(WAITALARM);

    /* return to wait loop */
    longjmp(dwait, 0);
}

/* final phase alarm handler for sender */

private VOID falarm() {

    debugp((stderr,"%s: FA %d %d %d\n",prog,oldprogress,progress,getstatus));

    /* no reason to count progress, just get status */
    if (!intrup) {
	VOIDC write(fdsend, statusbuf, 1);
    }
    getstatus = FALSE;

    /* reset alarm */
    VOIDC alarm(WAITALARM);
    return;
}

/* initial interrupt handler - before communications begin, so
 * nothing to be sent to printer
 */
private VOID intinit() {
    long clock;

    /* get rid of banner file */
    VOIDC unlink(".banner");

    fprintf(stderr,"%s: abort (during setup)\n",prog);
    VOIDC fflush(stderr);

    /* these next two may be too cautious */
    VOIDC kill(0,SIGINT);
    while (wait((union wait *) 0) > 0);

    if (VerboseLog) {
	fprintf (stderr, "%s: end - %s", prog, (time(&clock), ctime(&clock)));
	VOIDC fflush(stderr);
    }

    exit(THROW_AWAY);
}

/* interrupt during sending phase to sender process */

private VOID intsend() {
    /* set flag */
    intrup = TRUE;
    longjmp(sendint, 0);
}

/* interrupt during waiting phase to sender process */

private VOID intwait() {

    intrup = TRUE;

    fprintf(stderr,"%s: abort (waiting)\n",prog);
    VOIDC fflush(stderr);
    if (ioctl(fdsend, TIOCFLUSH, &flg) || ioctl(fdsend, TIOCSTART, &flg)
    || (write(fdsend, abortbuf, 1) != 1)) {
	fprintf(stderr, "%s: error in ioctl(fdsend):\n", prog);
	perror(prog);
    }

    /* VOIDC alarm(2); /* force an alarm soon to get us out of wait! ? */
    longjmp(dwait, 0);
}

/* EMT for reverse filter, avoid printer timeout at the expense
 * of performance (sigh)
 */

private VOID reverseready() {
    revdone = TRUE;
    longjmp(waitonreverse, 0);
}

/* EMT on startup to sender -- signalled by listener after first status
 * message received
 */

private VOID readynow() {
    goahead = TRUE;
    longjmp(startstatus, 0);
}

/* EMT on sending phase, hard EOF printer died! */
private VOID emtdead() {
    VOIDC alarm(0);
    exit(THROW_AWAY);
}

/* EMT during waiting phase -- listener saw an EOF (^D) from printer */

private VOID emtdone() {
    VOIDC alarm(0);
    gotemt = TRUE;
    longjmp(dwait, 0);
}
