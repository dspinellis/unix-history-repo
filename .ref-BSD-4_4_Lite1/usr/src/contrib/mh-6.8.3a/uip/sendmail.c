/* sendmail.c - */
#ifndef	lint
static char Id[] = "$Id: sendmail.c,v 1.9 1992/11/24 18:37:01 jromine Exp $";
#endif
/*
 **  A Sendmail fake.
 *
 * Contributed by Scott Erickson <erickson@ics.uci.edu>
 */
/* Include files glommed from post.c */

#include "../h/mh.h"
#include "../h/addrsbr.h"
#include "../h/aliasbr.h"
#include "../h/dropsbr.h"
#include "../zotnet/tws.h"
#ifndef MMDFMTS
#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <stdio.h>
#include <sys/types.h>
#else   MMDFMTS
#include "../mts/mmdf/util.h"
#include "../mts/mmdf/mmdf.h"
#endif  MMDFMTS
#include "../zotnet/mts.h"
#ifdef  MHMTS
#ifndef V7
#include <sys/ioctl.h>
#endif  not V7
#include <sys/stat.h>
#endif  MHMTS
#ifdef  SENDMTS
#include "../mts/sendmail/smail.h"
#undef  MF
#endif  SENDMTS
#include <signal.h>
#ifdef LOCALE
#include	<locale.h>
#endif

char	*SMTPSRVR = "smtpsrvr";

char	msgfname[50];	/* name of message file */
char	*FullName;	/* sender's full name */
char	*from;		/* sender's mail address */
int	verbose;
int	verify;
int	extract;
int	dodist;
int	rewritefrom;
int	status;		/* return value from procedures */
static int childid;	/* id from smtp child process */
TYPESIG	die();
long	lclock = 0L;	/* the time we started (more or less) */


FILE *fp;		/* file pointer for message file */
extern FILE *tmpfile();

static struct swit switches[] = {
#define ARPASW    0
    "ba", -2,
#define DAEMONSW  1
    "bd", -2,
#define INITALSW  2
    "bi", -2,
#define DELIVSW   3
    "bm", -2,
#define QSUMSW    4
    "bp", -2,
#define SMTPSW    5
    "bs", -2,
#define ADRTSTSW  6
    "bt", -2,
#define ADRVRFSW  7
    "bv", -2,
#define CFGFRZSW  8
    "bz", -2,
#define ALTCFGSW  9
    "C", -1,
#define DBGVALSW 10
    "d", -1,
#define FULLSW   11
    "F", -1,
#define FROMSW   12
    "f", -1,
#define HOPCNTSW 13
    "h", -1,
#define MSGIDSW  14
    "M", -1,
#define NOALISW  15
    "n", -1,
#define QTIMESW  16
    "q", -1,
#define OBSFRMSW 17
    "r", -1,
#define EXTHDRSW 18
    "t", -1,
#define VERBSW	 19
    "v", -1,
#define ALTALISW 20
    "oA", -2,
#define NOCONSW   21
    "oc", -2,
#define DLVMODSW 22
    "od", -2,
#define NEWALISW 23
    "oD", -2,
#define ERRMODSW 24
    "oe", -2,
#define TMPMODSW 25
    "oF", -2,
#define UFROMSW  26
    "of", -2,
#define GIDSW    27
    "og", -2,
#define HLPFILSW 28
    "oH", -2,
#define NODOTSW  29
    "oi", -2,
#define LOGLEVSW 30
    "oL", -2,
#define MEOKSW   31
    "om", -2,
#define OLDHDRSW 32
    "oo", -2,
#define QDIRSW   33
    "oQ", -2,
#define RTMOUTSW 34
    "or", -2,
#define SFILESW  35
    "oS", -2,
#define QMSGSW   36
    "os", -2,
#define MTMOUTSW 37
    "oT", -2,
#define TZSW     38
    "ot", -2,
#define UIDSW    39
    "ou", -2,
    
    NULL, 0
    };

#if !defined(POSIX) && !defined(_POSIX_SOURCE)
extern char *mktemp();
#endif

static void removemsg();
static int  isheader(), sendfile();

/*ARGSUSED*/
main (argc, argv)
int    argc;
char **argv;
{
    register char *cp;
    char **argp = argv + 1;
    
#ifdef LOCALE
	setlocale(LC_ALL, "");
#endif
    invo_name = r1bindex (argv[0], '/');
    mts_init(argv[0]);
    
    if (signal(SIGINT, SIG_IGN) != SIG_IGN)
	(void) signal(SIGINT, die);
    if (signal(SIGHUP, SIG_IGN) != SIG_IGN)
	(void) signal(SIGHUP, die);
    (void) signal(SIGTERM, die);
    (void) signal(SIGPIPE, die);
    
    FullName = getfullname();
    from = adrsprintf(NULLCP,NULLCP);
    (void) time (&lclock);
    
    while ( (cp = *argp) &&  *cp == '-' ) {
	argp++;
	switch (smatch ( ++cp, switches )) {
	case ARPASW:	/* smtp on stdin */
	case SMTPSW:	/* smtp on stdin */
	    smtp();
	    exit(98);	/* should never happen */
	    
	case DELIVSW:	/* just send mail */
	    continue;
	    
	case ADRVRFSW:	/* verify mode */
	    verify = 1;
	    continue;
	    
	case FROMSW:	/* from address */
	case OBSFRMSW:	/* obsolete -f flag */
	    if (*(++cp) == '\0' &&
		(!(cp = *argp++) || *cp == '-'))
		adios (NULLCP, "missing argument to %s", argp[-2]);
	    /* At this point, cp points to the from name */
	    if (rewritefrom) {
		adios (NULLCP, "More than one \"from\" person");
		continue;
	    }
	    from = cp;
	    rewritefrom = 1;
	    continue;
	    
	case EXTHDRSW:	/* read recipients from message */
	    extract = 1;
	    continue;
	    
	case VERBSW:	/* give blow-by-blow description */
	    verbose = 1;
	    continue;
	    
	    /* These switches have no args. */
	case QMSGSW:	/* always queue the message */
	case DAEMONSW:	/* run as a daemon & wait for SMTP */
	case INITALSW:	/* initialize the alias database */
	case QSUMSW:	/* print summary of mail queue */
	case ADRTSTSW:	/* test the addresses to debug config file */
	case CFGFRZSW:	/* create the configuration freeze file */
	case NOALISW:	/* do not do aliasing */
	case NOCONSW:	/* do not initiate immediate host connection */
	case NEWALISW:	/* run newaliases to rebuild db */
	case UFROMSW:	/* save UNIX-style From lines at front of msg*/
	case NODOTSW:	/* dots on line are not msg terminators */
	case MEOKSW:	/* ok to send to me if I'm in an alias */
	case OLDHDRSW:	/* msg may have old-style headers */
	    continue;
	    
	    /* These switches have string args. */
	case ALTALISW:	/* use alternate alias file */
	case ALTCFGSW:	/* use alternate configuration file */
	case DBGVALSW:	/* set the debug value */
	case FULLSW:	/* set full name */
	case MSGIDSW:	/* try to deliver queued msg with msg-id */
	case QTIMESW:	/* interval between queue passes */
	case DLVMODSW:	/* set the delivery mode */
	case ERRMODSW:	/* set the error mode */
	case TMPMODSW:	/* the mode to use when creating tmp files */
	case HLPFILSW:	/* the SMTP help file */
	case QDIRSW:	/* directory into which to queue messages */
	case RTMOUTSW:	/* timeout on reads */
	case SFILESW:	/* save statistics in this file */
	case MTMOUTSW:	/* timeout on messages in the queue */
	case TZSW:	/* set the name of the timezone */
	    if (*(++cp) == '\0' &&
		(!(cp = *argp++) || *cp == '-'))
		adios (NULLCP, "missing argument to %s", argp[-2]);
	    /* At this point, cp points to the argument */
	    continue;	/* Ignore */
	    
	    /* These switches have numeric args. */
	case HOPCNTSW:	/* hop count */
	case GIDSW:	/* gid when calling mailers */
	case LOGLEVSW:	/* the log level */
	case UIDSW:	/* uid when calling mailers */
	    if (*(++cp) == '\0' &&
		(!(cp = *argp++) || *cp == '-'))
		adios (NULLCP, "missing argument to %s", argp[-2]);
	    /* At this point, cp points to the numeric arg */
	    if (!isdigit(*cp))
		adios (NULLCP, "non-numeric argument to %s", argp[-2]);
	    continue;	/* Ignore */
	}
    }
    
    (void) setuid(getuid());
    
    if (verify && extract)
	adios (NULLCP, "mode not supported on header components");
    
    if (*argp == NULL && !extract)
	adios (NULLCP, "usage: /usr/lib/sendmail [flags] addr...");
    
    strcpy (msgfname, "/tmp/sendmhXXXXXX");
    if ( mktemp(msgfname) == NULL )
	adios (msgfname, "can't create msg file ");
    
    if ( (fp = fopen(msgfname,"w") ) == NULL ) {
	adios (msgfname, "error opening ");
    }
    
    doheader(argp);
    if ( verify ) {
	(void) fclose(fp);
	status = doverify();
	removemsg();
	exit ( status ) ;
    }
    dobody();
    status = sendfile();
    removemsg();
    exit ( status );
}

static void removemsg()
{
  if ( unlink(msgfname) != 0 )
      perror("unlink");
}

doheader(argp)
char **argp;
{
    char	line[BUFSIZ];
    int		gotdate, gotfrom, gotsender, gotto;
    
    /* if we're not extracting the headers from the message, then we
     * need to check to see if we need to do a "send" or a "dist".
     */
    
    if ( !extract ) {
	/* If we're doing a verify, just create a "To:" header. */
	if ( ! verify ) {
	    gotdate = gotfrom = gotto = gotsender = dodist = 0;
	    while (fgets (line, BUFSIZ, stdin) != NULL) {
		if (line[0] == '\n')		/* end of header */
		    break;
		if ( !isheader(line) )
		    break;

		/* if any of the following headers are present, then we
		 * want to do a dist.
		 */
		if ( !gotdate && uprf(line, "date") )
		    gotdate = dodist = 1;

		else if ( !gotto && (uprf(line, "to") || uprf(line, "cc")) )
		    gotto = dodist = 1;

		else if ( uprf(line, "message-id") )
		    dodist = 1;

		else if ( !gotsender && uprf(line, "sender") )
		    gotsender = dodist = 1;
		
		else if ( uprf ( line, "resent-" ) ) {
		    dodist = 1;
		    (void) fputs("Prev-", fp);
		}
		
		/* See if we are re-writing the from line */
		if ( uprf(line, "from") ) {
		    gotfrom = 1;
		    if ( rewritefrom )
			dofrom();
		    else
			(void) fputs(line,fp);
		}
		else
		    (void) fputs(line,fp);
	    }
	}
	/* Now, generate a "to" line.  The first line is easy.
	 * Write the rest of the lines with a newline/tab so that we
	 * don't accidentally write a line that's too long to be parsed
	 * by post.
	 */
	(void) fprintf (fp, "%sTo: %s", (dodist ? "Resent-" : "" ), *argp++);
	while ( *argp )
	    (void) fprintf ( fp, ",\n\t%s", *argp++ );
	(void) fputs("\n",fp);

	/* If we're doing a dist, we must have a "Date:" and "From:" field.
	 */
	if ( dodist ) {
	    if ( !gotdate )
		(void) fprintf (fp, "Date: %s\n", dtime (&lclock));
	    if ( !gotfrom )
		dofrom();
	}
#ifdef	MMDFI			/* sigh */
	if ( !gotsender )
	    (void) fprintf (fp, "Sender: %s\n", from);
#endif	MMDFI
    } else {	/* we're verifying, so just pass everything through */
	while (fgets (line, BUFSIZ, stdin) != NULL) {
	    if (line[0] == '\n')		/* end of header */
		break;
	    
	    if ( rewritefrom && uprf(line, "from"))
		dofrom();
	    else
		(void) fputs(line,fp);
	}
    }
    /* At this point, line is either a newline (end of header) or the
     * first line of the body (poorly formatted message).  If line
     * contains a line of body from a poorly formatted message, then
     * print a newline to separate the header correctly, then print
     * the body line.
     */
    if ( line[0] != '\n' )	/* i.e. a "body" line */
	(void) fputc('\n', fp);
    (void) fputs(line, fp);
}

static int isheader(s)
char *s;
{
    register char *cp;

    /* If the first character is a space, assume a continuation of a header */
    if ( isspace(*s) )
	return 1;

    /* If there's no ':', it's not a header */
    if ( (cp = index(s,':')) == NULL )
	return 0;

    /* If there's a space between BOL and ':', it's not a header */
    while ( s < cp ) {
	if ( isspace(*s) )
	    return 0;
	s++;
    }
    return 1;
}

/* This procedure does the verify and returns the status */
doverify() {
    char *command, buf[BUFSIZ], *bp;
    FILE *verfp, *popen();
    
    /* set up the command line for post */
    if ( (command = (char *)malloc((strlen(postproc) +
				    strlen(" -whom -check -verbose ") +
				    strlen(msgfname) + 1 )*sizeof(char)))
	== NULL ) {
	perror("malloc");
	return NOTOK;
    }
    
    (void) strcpy(command,postproc);
    (void) strcat(command," -whom -check ");
    if ( verbose )
	(void) strcat(command, "-verbose " );
    (void) strcat(command, msgfname);
    
    /* open up the pipe */
    if ( (verfp = popen(command,"r")) == NULL )
	return NOTOK;
    
    while ( fgets(buf, BUFSIZ, verfp) != NULL )
	/* sendmail returns:
	 *   address:  result
	 * so we need to strip the extra post headers.
	 */
	if ( verbose ) {
	    bp = buf;
	    while (isspace(*bp))
		bp++;
	    if ( *bp != '-' )
		(void) fputs(bp,stdout);
	}
    
    /* return the error status of post */
    return( pclose(verfp) >> 8 );
}

static int sendfile()
{
    char *command, buf[BUFSIZ];
    FILE *verfp, *popen();
    
    /* set up the command line for post */
    if ( (command = (char *)malloc((strlen(postproc) +
				    strlen(" -dist -verbose ") +
				    strlen(msgfname) + 1 )*sizeof(char)))
	== NULL ) {
	perror("malloc");
	return NOTOK;
    }
    
    (void) strcpy(command,postproc);
    (void) strcat(command," ");
    if ( verbose )
	(void) strcat(command, "-verbose " );
    if ( dodist )
	(void) strcat(command, "-dist " );
    (void) strcat(command, msgfname);
    
    /* open up the pipe */
    if ( (verfp = popen(command,"r")) == NULL )
	return NOTOK;
    
    while ( fgets(buf, BUFSIZ, verfp) != NULL )
	(void) fputs(buf,stdout);

    /* return the error status of post */
    return( pclose(verfp) >> 8 );
}

dofrom() {
    char	line[128];
    
    if (FullName)
	(void) sprintf(line, "From: %s <%s>\n", FullName, from);
    else
	(void) sprintf(line, "From: %s\n", from);
    (void) fputs(line, fp);
}

dobody() {
    register int    i;
    char    buffer[BUFSIZ];
    
    while (!feof (stdin) && !ferror (stdin) &&
	   (i = fread (buffer, sizeof (char), sizeof (buffer), stdin)) > 0)
	if (fwrite (buffer, sizeof (char), i , fp) != i )
	    adios (NULLCP, "Problem writing body");
    
    if (ferror (stdin))
	adios (NULLCP, "Problem reading body");
    
    if ( fclose(fp) != 0 )
	adios (NULLCP, "problem ending submission");
}

TYPESIG silentdie();

smtp()
{
    int sd,len;
    char buf[BUFSIZ], response[BUFSIZ];

    if ((sd = client(NULLCP, "tcp", "smtp", 0, response)) == NOTOK)
	adios (NULLCP, "cannot open smtp client process");

    (void) signal(SIGCHLD, silentdie);

    switch ((childid = fork())) {
	case NOTOK:
	    adios (NULLCP, "unable to fork smtp process");
	
	case OK:	/* i.e. child */
	    (void) dup2(sd,0);
	    break;

	default:	/* i.e. parent */
	    (void) dup2(sd,1);
	    break;
    }
    while ( (len = read(0, buf, BUFSIZ)) > 0)
	(void) write (1, buf, len);

    if (childid)
	(void) kill(childid, SIGHUP);

    exit(9);
}

/* ARGSUSED */
TYPESIG	die(sig)
int sig;
{
    if (fp) {
	(void) fclose(fp);
	(void) unlink(msgfname);
    }
    if (sig != SIGHUP)
	(void) fprintf(stderr, "sendmail: dying from signal %d\n", sig);
    exit(99);
}

/* ARGSUSED */

TYPESIG	silentdie(sig)
int sig;
{
    pidwait (childid, OK);
    exit(0);
}
