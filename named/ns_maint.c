/*
 * Copyright (c) 1986, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)ns_maint.c	4.29 (Berkeley) 2/7/89";
#endif /* not lint */

#include <sys/param.h>
#include <sys/socket.h>
#include <sys/time.h>
#if defined(SYSV)
#include <unistd.h>
#endif SYSV
#include <netinet/in.h>
#include <stdio.h>
#include <syslog.h>
#include <signal.h>
#include <errno.h>
#include <arpa/nameser.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include "ns.h"
#include "db.h"

extern int errno;
extern int maint_interval;
extern int needzoneload;

int xfers_running;	       /* number of xfers running */
int xfers_deferred;	       /* number of needed xfers not run yet */


/*
 * Invoked at regular intervals by signal interrupt; refresh all secondary
 * zones from primary name server and remove old cache entries.  Also,
 * ifdef'd ALLOW_UPDATES, dump database if it has changed since last
 * dump/bootup.
 */
ns_maint()
{
	register struct zoneinfo *zp;
	struct itimerval ival;
	time_t next_refresh = 0;
	int zonenum;

#ifdef DEBUG
	if (debug)
		fprintf(ddt,"\nns_maint()\n");
#endif

	gettime(&tt);
	xfers_deferred = 0;
	for (zp = zones, zonenum = 0; zp < &zones[nzones]; zp++, zonenum++) {
#ifdef DEBUG
		if (debug >= 2)
			printzoneinfo(zonenum);
#endif
		if (tt.tv_sec >= zp->z_time && zp->z_refresh > 0) {
			/*
			 * Set default time for next action first,
			 * so that it can be changed later if necessary.
			 */
			zp->z_time = tt.tv_sec + zp->z_refresh;

			switch (zp->z_type) {

			case Z_CACHE:
				doachkpt();
				break;

			case Z_SECONDARY:
				if ((zp->z_state & Z_NEED_RELOAD) == 0) {
				    if (zp->z_state & Z_XFER_RUNNING)
					abortxfer(zp);
				    else if (xfers_running < MAX_XFERS_RUNNING)
					startzoneref(zp);
				    else {
					zp->z_state |= Z_NEED_XFER;
					++xfers_deferred;
#ifdef DEBUG
					if (debug > 1)
					    fprintf(ddt,
						"xfer deferred for %s\n",
						zp->z_origin);
#endif
				    }
				}
				break;
#ifdef ALLOW_UPDATES
			case Z_PRIMARY:
				/*
				 * Checkpoint the zone if it has changed
				 * since we last checkpointed
				 */
				if (zp->hasChanged)
					zonedump(zp);
				break;
#endif ALLOW_UPDATES
			}
			gettime(&tt);
		}

		/*
		 * Find when the next refresh needs to be and set
		 * interrupt time accordingly.
		 */
		if (next_refresh == 0 ||
		    (zp->z_time != 0 && next_refresh > zp->z_time))
			next_refresh = zp->z_time;
	}

        /*
	 *  Schedule the next call to this function.
	 *  Don't visit any sooner than maint_interval.
	 */
	bzero((char *)&ival, sizeof (ival));
	ival.it_value.tv_sec = next_refresh - tt.tv_sec;
	if (ival.it_value.tv_sec < maint_interval)
		ival.it_value.tv_sec = maint_interval;
	(void) setitimer(ITIMER_REAL, &ival, (struct itimerval *)NULL);
#ifdef DEBUG
	if (debug)
		fprintf(ddt,"exit ns_maint() Next interrupt in %d sec\n",
			ival.it_value.tv_sec);
#endif
}

/*
 * Start an asynchronous zone transfer for a zone.
 * Depends on current time being in tt.
 */
startzoneref(zp)
	struct zoneinfo *zp;
{
	static char *argv[NSMAX + 15], argv_ns[NSMAX][MAXDNAME];
	int cnt, argc = 0, argc_ns = 0, pid, omask;
	char debug_str[10];
	char serial_str[10];

#ifdef DEBUG
	if (debug)
		fprintf(ddt,"startzoneref() %s\n", zp->z_origin);
#endif

	argv[argc++] = "named-xfer";
	argv[argc++] = "-z";
	argv[argc++] = zp->z_origin;
	argv[argc++] = "-f";
	argv[argc++] = zp->z_source;
	argv[argc++] = "-s";
	sprintf(serial_str, "%d", zp->z_serial);
	argv[argc++] = serial_str;
	if (zp->z_state & Z_SYSLOGGED)
		argv[argc++] = "-q";
#ifdef DEBUG
	if (debug) {
		argv[argc++] = "-d";
		sprintf(debug_str, "%d", debug);
		argv[argc++] = debug_str;
		argv[argc++] = "-l";
		argv[argc++] = "/usr/tmp/xfer.ddt";
		if (debug > 5) {
			argv[argc++] = "-t";
			argv[argc++] = "/usr/tmp/xfer.trace";
		}
	}
#endif
	
	/*
	 * Copy the server ip addresses into argv, after converting
	 * to ascii and saving the static inet_ntoa result
	 */
	for (cnt = 0; cnt < zp->z_addrcnt; cnt++)
		argv[argc++] = strcpy(argv_ns[argc_ns++],
		    inet_ntoa(zp->z_addr[cnt]));

	argv[argc] = 0;

#ifdef DEBUG
#ifdef ECHOARGS
	if (debug) {
		int i;
		for (i = 0; i < argc; i++) 
			fprintf(ddt, "Arg %d=%s\n", i, argv[i]);
        }
#endif /* ECHOARGS */
#endif /* DEBUG */

#ifdef SYSV
#define vfork fork
#else
	omask = sigblock(sigmask(SIGCHLD));
#endif
	if ((pid = vfork()) == -1) {
#ifdef DEBUG
		if (debug)
			fprintf(ddt, "xfer [v]fork: %d\n", errno);
#endif
		syslog(LOG_ERR, "xfer [v]fork: %m");
#ifndef SYSV
		(void) sigsetmask(omask);
#endif
		zp->z_time = tt.tv_sec + 10;
		return;
	}

	if (pid) {
#ifdef DEBUG
		if (debug)
			fprintf(ddt, "started xfer child %d\n", pid);
#endif
		zp->z_state &= ~Z_NEED_XFER;
		zp->z_state |= Z_XFER_RUNNING;
		zp->z_xferpid = pid;
		xfers_running++;
		zp->z_time = tt.tv_sec + MAX_XFER_TIME;
#ifndef SYSV
		(void) sigsetmask(omask);
#endif
	} else {
		execve(XFER_BINARY, argv, NULL);
		syslog(LOG_ERR, "can't exec %s: %m", XFER_BINARY);
		_exit(XFER_FAIL);	/* avoid duplicate buffer flushes */
	}
}

#ifdef DEBUG
printzoneinfo(zonenum)
int zonenum;
{
	struct timeval  tt;
	struct zoneinfo *zp = &zones[zonenum];
	char *ZoneType;

	if (!debug)
		return; /* Else fprintf to ddt will bomb */
	fprintf(ddt, "printzoneinfo(%d):\n", zonenum);

	gettime(&tt);
	switch (zp->z_type) {
		case Z_PRIMARY: ZoneType = "Primary"; break;
		case Z_SECONDARY: ZoneType = "Secondary"; break;
		case Z_CACHE: ZoneType = "Cache"; break;
		default: ZoneType = "Unknown";
	}
	if (zp->z_origin[0] == '\0')
		fprintf(ddt,"origin ='.'");
	else
		fprintf(ddt,"origin ='%s'", zp->z_origin);
	fprintf(ddt,", type = %s", ZoneType);
	fprintf(ddt,", source = %s\n", zp->z_source);
	fprintf(ddt,"z_refresh = %ld", zp->z_refresh);
	fprintf(ddt,", retry = %ld", zp->z_retry);
	fprintf(ddt,", expire = %ld", zp->z_expire);
	fprintf(ddt,", minimum = %ld", zp->z_minimum);
	fprintf(ddt,", serial = %ld\n", zp->z_serial);
	fprintf(ddt,"z_time = %d", zp->z_time);
	fprintf(ddt,", now time : %d sec", tt.tv_sec);
	fprintf(ddt,", time left: %d sec; state %x\n", zp->z_time - tt.tv_sec,
	    zp->z_state);
}
#endif DEBUG

#ifdef notdef
/*
 * New code added by Rich Wales (UCLA), October 1986:
 *
 * The following routines manipulate the d_mark field.  When a zone
 * is being refreshed, the old RR's are marked.  This allows old RR's to
 * be cleaned up after the new copy of the zone has been completely read
 * -- or new RR's to be cleaned up if an error prevents transfer of a
 * new zone copy.
 *
 */

/*
 *     Set the "d_mark" field to on each RR in the zone "zone".
 *     Initially called with "htp" equal to "hashtab", this routine
 *     calls itself recursively in order to traverse all subdomains.
 */
mark_zone (htp, zone, flag)
    struct hashbuf *htp;
    register int zone;
    register int flag;
{
    register struct databuf *dp;
    register struct namebuf *np;
    register struct namebuf **npp, **nppend;

    nppend = htp->h_tab + htp->h_size;
    for (npp = htp->h_tab; npp < nppend; npp++) {
        for (np = *npp; np != NULL; np = np->n_next) {
	    for (dp = np->n_data; dp != NULL; dp = dp->d_next)
		if (dp->d_zone == zone)
		    dp->d_mark = flag;
	    if (np->n_hash != NULL)	/* mark subdomains */
		mark_zone (np->n_hash, zone, flag);
        }
    }
}

/*
 * clean_zone (htp, zone, flag) --
 *     Delete all RR's in the zone "zone" whose "d_mark" values are
 *     equal to "flag".  Originally called with "htp" equal to
 *     "hashtab", this routine calls itself recursively in order to
 *     traverse all subdomains.
 */
clean_zone (htp, zone, flag)
    register struct hashbuf *htp;
    register int zone;
    register int flag;
{
    register struct databuf *dp, *pdp;
    register struct namebuf *np;
    struct namebuf **npp, **nppend;

    nppend = htp->h_tab + htp->h_size;
    for (npp = htp->h_tab; npp < nppend; npp++) {
    	for (np = *npp; np != NULL; np = np->n_next) {
    	    for (pdp = NULL, dp = np->n_data; dp != NULL; ) {
		if (dp->d_zone == zone && dp->d_mark == flag)
		    dp = rm_datum(dp, np, pdp);
    	    	else {
    	    	    pdp = dp;
		    dp = dp->d_next;
    	    	}
    	    }
	    /* Call recursively to clean up subdomains. */
	    if (np->n_hash != NULL)
		clean_zone (np->n_hash, zone, flag);
	}
    }
}
#endif /* notdef */

/*
 * remove_zone (htp, zone) --
 *     Delete all RR's in the zone "zone" under specified hash table.
 */
remove_zone(htp, zone)
	register struct hashbuf *htp;
	register int zone;
{
	register struct databuf *dp, *pdp;
	register struct namebuf *np;
	struct namebuf **npp, **nppend;

	nppend = htp->h_tab + htp->h_size;
	for (npp = htp->h_tab; npp < nppend; npp++)
	    for (np = *npp; np != NULL; np = np->n_next) {
		for (pdp = NULL, dp = np->n_data; dp != NULL; ) {
			if (dp->d_zone == zone)
				dp = rm_datum(dp, np, pdp);
			else {
				pdp = dp;
				dp = dp->d_next;
			}
		}
		/* Call recursively to remove subdomains. */
		if (np->n_hash)
			remove_zone(np->n_hash, zone);
	    }
}
   
/*
 * Abort an xfer that has taken too long.
 */
abortxfer(zp)
	register struct zoneinfo *zp;
{

	kill(zp->z_xferpid, SIGKILL); /* don't trust it at all */
#ifdef DEBUG
	if (debug)
	  fprintf(ddt, "Killed child %d (zone %s) due to timeout\n",
	     zp->z_xferpid, zp->z_origin);
#endif /* DEBUG */
	zp->z_time = tt.tv_sec + zp->z_retry;
}

#ifdef SYSV
union wait {
	unsigned short	w_termsig:7;	/* termination signal */
	unsigned short	w_coredump:1;	/* core dump indicator */
	unsigned short	w_retcode:8;	/* exit code if w_termsig==0 */
};
#endif

/*
 * SIGCHLD signal handler: process exit of xfer's.
 */
endxfer()
{
    	register struct zoneinfo *zp;   
	int pid;
	union wait status;

	gettime(&tt);
#if defined(SYSV)
	{ int stat;
	pid = wait(&stat);
	status.w_termsig = stat & 0x7f;
	status.w_retcode = stat >> 8;
	}
	(void)signal(SIGCLD, endxfer);
#ifdef DEBUG
	if (debug > 1) 
	    fprintf(ddt, "child %d returned status=%d\n", pid, status);
#endif
#else /* SYSV */
	while ((pid = wait3(&status, WNOHANG, (struct rusage *)NULL)) > 0) {
#ifdef DEBUG
		if (debug > 1) 
		    fprintf(ddt, "child %d returned status=%d termsig=%d\n", 
		       pid, status.w_retcode, status.w_termsig);
#endif
#endif /* SYSV */
		for (zp = zones; zp < &zones[nzones]; zp++)
		    if (zp->z_xferpid == pid) {
			xfers_running--;
			zp->z_xferpid = 0;
			zp->z_state &= ~Z_XFER_RUNNING;
			if (status.w_termsig != 0) {
				if (status.w_termsig != SIGKILL) {
					syslog(LOG_ERR,
					   "named-xfer exited with signal %d\n",
					   status.w_termsig);
#ifdef DEBUG
					if (debug)
					    fprintf(ddt,
					 "\tchild termination with signal %d\n",
						status.w_termsig);
#endif
				}
			} else switch (status.w_retcode) {
				case XFER_UPTODATE:
					zp->z_state &= ~Z_SYSLOGGED;
					zp->z_lastupdate = tt.tv_sec;
					zp->z_time = tt.tv_sec + zp->z_refresh;
					if (zp->z_source) {
#if defined(SYSV)
						struct utimbuf t;

						t.actime = tt.tv_sec;
						t.modtime = tt.tv_sec;
						(void) utime(zp->z_source, &t);
#else
						struct timeval t[2];

						t[0] = tt;
						t[1] = tt;
						(void) utimes(zp->z_source, t);
#endif /* SYSV */
					}
					break;

				case XFER_SUCCESS:
					zp->z_state |= Z_NEED_RELOAD;
					zp->z_state &= ~Z_SYSLOGGED;
					needzoneload++;
					break;

				case XFER_TIMEOUT:
#ifdef DEBUG
					if (debug) fprintf(ddt,
		    "zoneref: Masters for secondary zone %s unreachable\n",
					    zp->z_origin);
#endif
					if ((zp->z_state & Z_SYSLOGGED) == 0) {
						zp->z_state |= Z_SYSLOGGED;
						syslog(LOG_WARNING,
		      "zoneref: Masters for secondary zone %s unreachable",
						    zp->z_origin);
					}
					zp->z_time = tt.tv_sec + zp->z_retry;
					break;

				default:
					if ((zp->z_state & Z_SYSLOGGED) == 0) {
						zp->z_state |= Z_SYSLOGGED;
						syslog(LOG_ERR,
						    "named-xfer exit code %d",
						    status.w_retcode);
					}
					/* FALLTHROUGH */
				case XFER_FAIL:
					zp->z_state |= Z_SYSLOGGED;
					zp->z_time = tt.tv_sec + zp->z_retry;
					break;
			}
			break;
		}
#ifndef SYSV
	}
#endif /* SYSV */
	for (zp = zones;
	    xfers_deferred != 0 && xfers_running < MAX_XFERS_RUNNING &&
	    zp < &zones[nzones]; zp++)
		if (zp->z_state & Z_NEED_XFER) {
			xfers_deferred--;
			startzoneref(zp);
		}
}

/*
 * Reload zones whose transfers have completed.
 */
loadxfer()
{
    	register struct zoneinfo *zp;   

	gettime(&tt);
	for (zp = zones; zp < &zones[nzones]; zp++)
	    if (zp->z_state & Z_NEED_RELOAD) {
		zp->z_state &= ~Z_NEED_RELOAD;
		zp->z_auth = 0;
		remove_zone(hashtab, zp - zones);
		if (db_load(zp->z_source, zp->z_origin, zp) == 0)
			zp->z_auth = 1;

		/*
		 * need to find SOA and set refresh, etc.;
		 * could do in db_load/doupdate/etc.
		 */
		zp->z_lastupdate = tt.tv_sec;
		zp->z_time = tt.tv_sec + zp->z_refresh;
	}
}
