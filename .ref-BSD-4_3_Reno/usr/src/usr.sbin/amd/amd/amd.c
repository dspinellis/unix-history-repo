/*
 * $Id: amd.c,v 5.2 90/06/23 22:19:18 jsp Rel $
 *
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
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
 *
 *	@(#)amd.c	5.1 (Berkeley) 6/29/90
 */

/*
 * Automounter
 */

#include "am.h"
#include <sys/signal.h>
#include <netdb.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <setjmp.h>

char pid_fsname[16 + MAXHOSTNAMELEN];	/* "kiska.southseas.nz:(pid%d)" */
char *progname;				/* "amd" */
#ifdef HAS_HOST
char *host_helper;
#endif /* HAS_HOST */
char *auto_dir = "/a";
char *hostdomain = "unknown.domain";
char hostname[MAXHOSTNAMELEN];		/* Hostname */
char hostd[2*MAXHOSTNAMELEN];		/* Host+domain */
char *op_sys = OS_REP;			/* Name of current op_sys */
char *arch = ARCH_REP;			/* Name of current architecture */
char *endian = ARCH_ENDIAN;		/* Big or Little endian */
int foreground = 1;			/* This is the top-level server */
int mypid;				/* Current process id */
int immediate_abort;			/* Should close-down unmounts be retried */
struct in_addr myipaddr;		/* (An) IP address of this host */
serv_state amd_state = Start;
struct amd_stats amd_stats;		/* Server statistics */
time_t do_mapc_reload = 0;		/* mapc_reload() call required? */
jmp_buf select_intr;
int select_intr_valid;
int orig_umask;

/*
 * Signal handler:
 * SIGINT - tells amd to do a full shutdown, including unmounting all filesystem.
 * SIGTERM - tells amd to shutdown now.  Just unmounts the automount nodes.
 */
static void sigterm(sig)
int sig;
{
#ifdef SYS5_SIGNALS
	signal(sig, sigterm);
#endif * SYS5_SIGNALS */

	switch (sig) {
	case SIGINT:
		immediate_abort = 15;
		break;

	case SIGTERM:
		immediate_abort = -1;
		/* fall through... */

	default:
		plog(XLOG_WARNING, "WARNING: automounter going down on signal %d", sig);
		break;
	}
	if (select_intr_valid)
		longjmp(select_intr, sig);
}

/*
 * Hook for cache reload.
 * When a SIGHUP arrives it schedules a call to mapc_reload
 */
static void sighup(sig)
int sig;
{
#ifdef SYS5_SIGNALS
	signal(sig, sighup);
#endif /* SUS5_SIGNALS */

#ifdef DEBUG
	if (sig != SIGHUP)
		dlog("spurious call to sighup");
#endif /* DEBUG */
	/*
	 * Force a reload by zero'ing the timer
	 */
	if (amd_state == Run)
		do_mapc_reload = 0;
}

static void parent_exit(sig)
int sig;
{
	exit(0);
}

static int daemon_mode(P_void)
{
	int bgpid = background();

	if (bgpid != 0) {
		if (print_pid) {
			printf("%d\n", bgpid);
			fflush(stdout);
		}
		/*
		 * Now wait for the automount points to
		 * complete.
		 */
		signal(SIGQUIT, parent_exit);
		for (;;)
			pause();
	}

	/*
	 * Pretend we are in the foreground again
	 */
	foreground = 1;
#ifdef TIOCNOTTY
	{
		int t = open("/dev/tty", O_RDWR);
		if (t < 0) {
			if (errno != ENXIO)	/* not an error if already no controlling tty */
				plog(XLOG_WARNING, "Could not open controlling tty: %m");
		} else if (ioctl(t, TIOCNOTTY, 0) < 0) {
			plog(XLOG_WARNING, "Could not disassociate tty (TIOCNOTTY): %m");
		}
	}
#else
	(void) setpgrp();
#endif /* TIOCNOTTY */

	return getppid();
}

main(argc, argv)
int argc;
char *argv[];
{
	struct hostent *hp, *gethostbyname();
	char *domdot;
	int ppid = 0;
	int error;

	/*
	 * Make sure some built-in assumptions are true before we start
	 */
	assert(sizeof(nfscookie) >= sizeof (unsigned int));
	assert(sizeof(int) >= 4);

	/*
	 * Set processing status.
	 */
	amd_state = Start;

	/*
	 * Get local machine name
	 */
	if (gethostname(hostname, sizeof(hostname)) < 0) {
		plog(XLOG_FATAL, "gethostname: %m");
		going_down(1);
	}
	/*
	 * Check it makes sense
	 */
	if (!*hostname) {
		plog(XLOG_FATAL, "host name is not set");
		going_down(1);
	}
	/*
	 * Partially initialise hostd[].  This
	 * is completed in get_args().
	 */
	if (domdot = strchr(hostname, '.')) {
		/*
		 * Hostname already contains domainname.
		 * Split out hostname and domainname
		 * components
		 */
		*domdot++ = '\0';
		hostdomain = domdot;
	}
	strcpy(hostd, hostname);

	/*
	 * Trap interrupts for shutdowns.
	 */
	(void) signal(SIGINT, sigterm);

	/*
	 * Hangups tell us to reload the cache
	 */
	(void) signal(SIGHUP, sighup);

	/*
	 * Trap Terminate so that we can shutdown gracefully (some chance)
	 */
	(void) signal(SIGTERM, sigterm);
	/*
	 * Trap Death-of-a-child.  These allow us to
	 * pick up the exit status of backgrounded mounts.
	 * See "sched.c".
	 */
	(void) signal(SIGCHLD, sigchld);

	/*
	 * Initialise process id.  This is kept
	 * cached since it is used for generating
	 * and using file handles.
	 */
	mypid = getpid();

#ifdef notdef
/*
 * XXX - Doing this plugs most of a memory leak in
 * gethostbyname on SunOS 4.  I see no good reason
 * why the host database needs to grab 1.5K of
 * private data space...  However, for the moment,
 * I will take its word that it is a _good thing_
 * (jsp)
 */
	(void) sethostent(0);
#endif /* notdef */

	/*
	 * Fix-up any umask problems.  Most systems default
	 * to 002 which is not too convenient for our purposes
	 */
	orig_umask = umask(0);

	/*
	 * Determine command-line arguments
	 */
	get_args(argc, argv);

	/*
	 * Get our own IP address so that we
	 * can mount the automounter.  There
	 * is probably a better way of doing
	 * this, but messing about with SIOCGIFCONF
	 * seems to be heading towards the non-portable
	 * arena.
	 */
	hp = gethostbyname(hostname);
	if (!hp || hp->h_addrtype != AF_INET) {
		plog(XLOG_FATAL, "Can't determine IP address of this host (%s)", hostname);
		going_down(1);
	}
	myipaddr = *(struct in_addr *) hp->h_addr;

	/*
	 * Now check we are root.
	 */
	if (geteuid() != 0) {
		plog(XLOG_FATAL, "Must be root to mount filesystems (euid = %d)", geteuid());
		going_down(1);
	}

#ifdef HAS_NIS_MAPS
	/*
	 * If the domain was specified then bind it here
	 * to circumvent any default bindings that may
	 * be done in the C library.
	 */
	if (domain && yp_bind(domain)) {
		plog(XLOG_FATAL, "Can't bind to domain \"%s\"", domain);
		going_down(1);
	}
#endif /* HAS_NIS_MAPS */

#ifdef DEBUG
	Debug(D_DAEMON)
#endif /* DEBUG */
	ppid = daemon_mode();

	sprintf(pid_fsname, "%s:(pid%d)", hostname, mypid);

	do_mapc_reload = clocktime() + ONE_HOUR;

	/*
	 * Register automounter with system
	 */
	error = mount_automounter(ppid);
	if (error && ppid)
		kill(SIGALRM, ppid);
	going_down(error);

	abort();
}
