/* dsa.c - Main routine for QUIPU DSA process */

#ifndef lint
static char *rcsid = "$Header: /f/osi/quipu/RCS/dsa.c,v 7.9 91/03/09 11:56:48 mrose Exp $";
#endif

/*
 * $Header: /f/osi/quipu/RCS/dsa.c,v 7.9 91/03/09 11:56:48 mrose Exp $
 *
 *
 * $Log:	dsa.c,v $
 * Revision 7.9  91/03/09  11:56:48  mrose
 * update
 * 
 * Revision 7.8  91/02/22  09:39:04  mrose
 * Interim 6.8
 * 
 * Revision 7.7  90/11/20  15:28:47  mrose
 * cjr
 * 
 * Revision 7.6  90/10/17  11:54:00  mrose
 * sync
 * 
 * Revision 7.5  90/07/09  14:45:56  mrose
 * sync
 * 
 * Revision 7.4  90/03/15  11:18:57  mrose
 * quipu-sync
 * 
 * Revision 7.3  90/01/11  23:55:57  mrose
 * lint
 * 
 * Revision 7.2  90/01/11  18:37:21  mrose
 * real-sync
 * 
 * Revision 7.1  89/12/19  16:53:08  mrose
 * dgram
 * 
 * Revision 7.0  89/11/23  22:17:19  mrose
 * Release 6.0
 * 
 */

/*
 *                                NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


#include <signal.h>
#include <stdio.h>
#include <varargs.h>
#include "rosap.h"
#include "tsap.h"
#include "logger.h"
#include "tailor.h"
#include "quipu/util.h"
#include "quipu/connection.h"
#include <sys/ioctl.h>
#include <sys/stat.h>
#ifdef  BSD42
#include <sys/file.h>
#endif
#ifdef  SYS5
#include <fcntl.h>
#endif

#include "dgram.h"
#ifdef	TCP
#include "internet.h"
#endif

PS      opt;
static  int   debug = 1;
static  int   nbits = FD_SETSIZE;

extern LLog * log_dsap;

static  char *myname;

void    adios (), advise ();
static  envinit (), setdsauid();
SFD attempt_restart();
extern int print_parse_errors;
extern int parse_line;
struct task_act	* task_select();
extern time_t	time();

extern SFP abort_vector;

#ifndef NO_STATS
extern LLog    *log_stat;
#endif

/*
* Basic data structure of the DSA server.
*/
char			* mydsaname = "undefined";
struct PSAPaddr		* mydsaaddr = NULLPA;
struct PSAPaddr		* dsaladdr = NULLPA;
struct connection	* connlist;
int			  conns_used;
struct connection	* connwaitlist;
struct di_block		* deferred_dis = NULL_DI_BLOCK;
struct oper_act		* get_edb_ops;

char ** sargv;

main(argc, argv)
int    argc;
char    **argv;
{
#ifdef DEBUG
    unsigned proc_size = 0;
    unsigned new_size;
    extern caddr_t sbrk();
#endif
    extern char	* mydsaname;
    extern char	  startup_update;
    extern time_t	timenow;
    struct task_act	* task;
    int		  secs;
    char start_buf [LINESIZE];
    /*
    * Function to stop DSA server.
    */
    SFD          stop_dsa();
#ifdef	SIGUSR1
    SFD		 list_status ();
#endif

    sargv = argv;

    if (myname = rindex (argv[0], '/'))
	myname++;
    if (myname == NULL || *myname == NULL)
	myname = argv[0];

    isodetailor (myname,0);

    envinit();  /* detach */

    quipu_syntaxes ();
    dsa_sys_init(&argc, &argv);
    setdsauid();

    print_parse_errors = FALSE;

#ifndef NO_STATS
    ll_hdinit (log_stat,myname);
#endif

    if ((opt = ps_alloc (std_open)) == NULLPS)
	     fatal (-12,"ps_alloc failed");
    if (std_setup (opt,stdout) == NOTOK)
	     fatal (-13,"std_setup failed");

    DLOG (log_dsap,LLOG_DEBUG,( "About to dsa_init()"));

    if(dsa_init() == NOTOK)
    {
	fatal(-14,"Couldn't initialise the DSA!!");
    }


    if(net_init() == NOTOK)
    {
	fatal(-15,"Couldn't start the DSA!!");
    }


    if (startup_update) 
    {
	/* Will generate a list of EDB operations! */
	(void) time (&timenow);
  	slave_update();
    }

    {
	extern char *treedir;
	char    filebuf[BUFSIZ];
	FILE   *fp;

	(void) sprintf (filebuf, "%s/PID", treedir);
	if (fp = fopen (filebuf, "w")) {
	    (void) fprintf (fp, "%d\n", getpid ());
	    (void) fclose (fp);
	}
	else
	    LLOG (log_dsap,LLOG_EXCEPTIONS,("Can't open PID file %s",filebuf));
    }

    /*
    * Do stop_dsa() on receiving a Ctrl-C
    */

    (void) signal (SIGINT, stop_dsa);
    (void) signal (SIGTERM,stop_dsa);
    (void) signal (SIGHUP, stop_dsa);

    /* now started don't stop on core dumps !!! */
    (void) signal (SIGQUIT, attempt_restart);
    (void) signal (SIGILL,  attempt_restart);
    (void) signal (SIGBUS,  attempt_restart);
    (void) signal (SIGSEGV, attempt_restart);
    (void) signal (SIGSYS,  attempt_restart);
    (void) signal (SIGPIPE,  attempt_restart);
#ifdef	SIGUSR1
    (void) signal (SIGUSR1, list_status);
#endif

    abort_vector = attempt_restart;
    parse_line = 0;

    (void) sprintf (start_buf,"DSA %s has started on %s",mydsaname,
			    paddr2str(dsaladdr,NULLNA));

    LLOG (log_dsap,LLOG_NOTICE,(start_buf));
#ifndef NO_STATS
    LLOG (log_stat,LLOG_NOTICE,(start_buf));
#endif

     if (debug)
	(void) fprintf (stderr,"%s\n",start_buf);

    start_malloc_trace (NULLCP);

#ifdef DEBUG
    proc_size = (unsigned) sbrk(0);
#endif

    for(;;)
    {
	if((task = task_select(&secs)) == NULLTASK)
	{
#ifdef DEBUG
	   if ( secs != 0 ) {
		/* Only if we are idle ! */
		new_size = (unsigned) sbrk(0);
		if ( new_size > proc_size) {
			LLOG (log_dsap, LLOG_NOTICE, ("Process grown by %d bytes", new_size - proc_size));
			proc_size = new_size;
		}
	    }
#endif
	    dsa_wait(secs);	/* Check network with timeout of secs */
	}
	else
	{
	    dsa_work(task);	/* Process the DSA task selected */
	    dsa_wait(0);
	}
    } /* forever */
} /* main */

dsa_abort(isfatal)
int	isfatal;
{
    struct connection		* cn;
    struct DSAPindication	  di_s;
    struct DSAPindication	* di = &di_s;

    for(cn=connlist; cn!=NULLCONN; cn=cn->cn_next)
	if (cn -> cn_ad != NOTOK) {
	    if (isfatal || (! cn -> cn_initiator))
		(void) close (cn -> cn_ad);
	    else {
		(void) DUAbortRequest(cn->cn_ad, di);
	    }
	}

    watch_dog ("stop_listeners");
    stop_listeners();
    watch_dog_reset();
}

SFD stop_dsa (sig)
int sig;
{
	(void) signal (sig, SIG_DFL); /* to stop recursion */
	LLOG (log_dsap,LLOG_FATAL,("*** Stopping on signal %d ***",sig));
	if (debug)
		(void) fprintf (stderr,"DSA %s has Stopped\n",mydsaname);
	dsa_abort(0);
	exit (0);
}

#ifdef	SIGUSR1
/* ARGSUSED */

SFD	list_status (sig)
int	sig;
{
    int	    fd;
    struct stat st;
    register struct connection *cn;
    time_t  now;
    
#ifndef	BSD42
    (void) signal (SIGUSR1, list_status);
#endif

    for (fd = getdtablesize () - 1; fd >= 0; fd--)
	if (fstat (fd, &st) != NOTOK)
	    LLOG (log_dsap, LLOG_EXCEPTIONS,
		  ("fd %d: fmt=0%o", fd, st.st_mode & S_IFMT));
#ifndef	NO_STATS
    LLOG (log_dsap, LLOG_EXCEPTIONS, ("logs dsap=%d stat=%d",
				  log_dsap -> ll_fd, log_stat -> ll_fd));
#else
    LLOG (log_dsap, LLOG_EXCEPTIONS, ("logs dsap=%d", log_dsap -> ll_fd));
#endif
    (void) time (&now);
    for (cn = connlist; cn; cn = cn -> cn_next)
	if (cn -> cn_ad != NOTOK)
	    LLOG (log_dsap, LLOG_EXCEPTIONS,
		  ("cn %d: init=%d used=%ld release=%ld",
		   cn -> cn_ad, cn -> cn_initiator,
		   (long) now - cn -> cn_last_used,
		   (long) now - cn -> cn_last_release));
}
#endif

static  envinit () {
    int     i,
	    sd;

    nbits = getdtablesize ();

    if (!(debug = isatty (2))) {
	for (i = 0; i < 5; i++) {
	    switch (fork ()) {
		case NOTOK:
		    sleep (5);
		    continue;

		case OK:
		    goto fork_ok;

		default:
		    _exit (0);
	    }
	    break;
	}

fork_ok:;
	(void) chdir ("/");

	if ((sd = open ("/dev/null", O_RDWR)) == NOTOK)
	    adios ("/dev/null", "unable to read");
	if (sd != 0)
	    (void) dup2 (sd, 0), (void) close (sd);
	(void) dup2 (0, 1);
	(void) dup2 (0, 2);

#ifdef	SETSID
	if (setsid () == NOTOK)
	    advise (LLOG_EXCEPTIONS, "failed", "setsid");
#endif
#ifdef  TIOCNOTTY
	if ((sd = open ("/dev/tty", O_RDWR)) != NOTOK) {
	    (void) ioctl (sd, TIOCNOTTY, NULLCP);
	    (void) close (sd);
	}
#else
#ifdef  SYS5
	(void) setpgrp ();
	(void) signal (SIGINT, SIG_IGN);
	(void) signal (SIGQUIT, SIG_IGN);
#endif
#endif
    }
#ifndef	DEBUG
    /* "Normal" ISODE behavior of full logging only without DEBUG */
    else
	ll_dbinit (log_dsap, myname);
#endif

#ifndef	sun		/* damn YP... */
    for (sd = 3; sd < nbits; sd++) {
	if (log_dsap -> ll_fd == sd)
	    continue;
#ifdef	NO_STATS
	if (log_stats -> ll_fd == sd)
	    continue;
#endif
	(void) close (sd);
    }
#endif

    (void) signal (SIGPIPE, SIG_IGN);

    ll_hdinit (log_dsap, myname);
#ifdef	DEBUG
    advise (LLOG_TRACE, NULLCP, "starting");
#endif
}


/* 	ERRORS */

#ifndef	lint
void    adios (va_alist)
va_dcl
{
    va_list ap;

    va_start (ap);

    _ll_log (log_dsap, LLOG_FATAL, ap);

    va_end (ap);

    if (debug)
	(void) fprintf (stderr,"adios exit - see dsap.log\n");
    dsa_abort(1);
    _exit (-18);
}
#else
/* VARARGS */

void    adios (what, fmt)
char	*what,
	*fmt;
{
    adios (what, fmt);
}
#endif

#ifndef	lint
void    advise (va_alist)
va_dcl
{
    int     code;
    va_list ap;

    va_start (ap);

    code = va_arg (ap, int);

    (void) _ll_log (log_dsap, code, ap);

    va_end (ap);
}
#else
/* VARARGS */

void	advise (code, what, fmt)
char	*what,
	*fmt;
int	 code;	
{
    advise (code, what, fmt);
}
#endif



static setdsauid ()
{
struct stat buf;
extern char * treedir;

	(void) stat (treedir,&buf);

	if (setgid (buf.st_gid) == -1)
		LLOG (log_dsap,LLOG_EXCEPTIONS,("Can't set gid %d (database directory \"%s\")",buf.st_uid,treedir));

	if (setuid (buf.st_uid) == -1)
		LLOG (log_dsap,LLOG_EXCEPTIONS,("Can't set uid %d (database directory \"%s\")",buf.st_uid,treedir));
}



#define	RESTART_TIME	30		/* for connections to clear... */
#define	CLEAR_TIME	300		/*   .. */

SFD attempt_restart (sig)
int     sig;
{
int fpid, sd;
unsigned int secs;
extern char * mydsaname;

	if (sig > 0)
		(void) signal (sig, SIG_DFL); /* to stop recursion */

	if (sig >= 0 && debug)
		(void) fprintf (stderr,"DSA %s has a problem\n",mydsaname);

	dsa_abort(sig != NOTOK);
	secs = sig != NOTOK ? CLEAR_TIME : RESTART_TIME;

	for (sd = 3; sd < nbits; sd++) {
	    if (log_dsap -> ll_fd == sd)
		continue;
#ifdef	NO_STATS
	    if (log_stats -> ll_fd == sd)
		continue;
#endif
	    (void) close (sd);
	}

	if ( sig == -2 || (fpid = fork()) == 0) {
		if (sig == -2) {		    /* restart due to congestion... */
			LLOG (log_dsap,LLOG_FATAL, ("*** in-situ restart attempted ***"));
#ifndef NO_STATS
			LLOG (log_stat,LLOG_NOTICE,("RESTARTING (%s)",mydsaname));
#endif
	    	}

		sleep (secs);	/* give connections time to clear */
		(void) execv (isodefile(sargv[0], 1),sargv);
		exit (-19);
	}

	log_dsap -> ll_syslog = LLOG_FATAL;

	if (fpid != -1) {
		LLOG (log_dsap,LLOG_FATAL,("Quipu restart attempted in %d seconds (sig %d)", secs,sig));
#ifndef NO_STATS
		LLOG (log_stat,LLOG_NOTICE,("RESTARTING with pid %d (%s)",fpid,mydsaname));
#endif
	} else {
#ifndef NO_STATS
		LLOG (log_stat,LLOG_NOTICE,("PANIC (%s)",mydsaname));
#endif
		LLOG (log_dsap,LLOG_FATAL,("Quipu aborting - sig (%d)",sig));
	}

	(void) signal (SIGIOT, SIG_DFL);
	abort ();
	exit (-20);  /* abort should not return */
}

