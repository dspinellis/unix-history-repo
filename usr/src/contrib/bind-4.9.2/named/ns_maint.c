#if !defined(lint) && !defined(SABER)
static char sccsid[] = "@(#)ns_maint.c	4.39 (Berkeley) 3/2/91";
static char rcsid[] = "$Id: ns_maint.c,v 4.9.1.9 1993/12/06 00:43:02 vixie Exp $";
#endif /* not lint */

/*
 * ++Copyright++ 1986, 1988
 * -
 * Copyright (c) 1986, 1988
 *    The Regents of the University of California.  All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 * 	This product includes software developed by the University of
 * 	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * -
 * Portions Copyright (c) 1993 by Digital Equipment Corporation.
 * 
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies, and that
 * the name of Digital Equipment Corporation not be used in advertising or
 * publicity pertaining to distribution of the document or software without
 * specific, written prior permission.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS ALL
 * WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL EQUIPMENT
 * CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 * -
 * --Copyright--
 */

#include <sys/param.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <arpa/nameser.h>
#include <sys/wait.h>
#include <stdio.h>
#include <syslog.h>
#include <signal.h>
#include <errno.h>

#include "named.h"

#ifdef USE_UTIME
# include <utime.h>
#endif

static int		xfers_running,	/* # of xfers running */
			xfers_deferred,	/* # of needed xfers not run yet */
			alarm_pending;

static void		startxfer __P((struct zoneinfo *)),
			abortxfer __P((struct zoneinfo *)),
			qserial_query __P((struct zoneinfo *)),
			tryxfer __P((void));

/*
 * Invoked at regular intervals by signal interrupt; refresh all secondary
 * zones from primary name server and remove old cache entries.  Also,
 * ifdef'd ALLOW_UPDATES, dump database if it has changed since last
 * dump/bootup.
 */
void
ns_maint()
{
	register struct zoneinfo *zp;
	int zonenum;

	gettime(&tt);

	dprintf(1, (ddt, "\nns_maint(); now %s", ctime(&tt.tv_sec)));

	xfers_deferred = 0;
	alarm_pending = 0;
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
#ifdef STUBS
			case Z_STUB:
#endif
				if ((zp->z_state & Z_NEED_RELOAD) == 0) {
				    if (zp->z_state & Z_XFER_RUNNING)
					abortxfer(zp);
				    else if (!(zp->z_state & Z_QSERIAL))
					qserial_query(zp);
				}
				break;
#ifdef ALLOW_UPDATES
			case Z_PRIMARY:
				/*
				 * Checkpoint the zone if it has changed
				 * since we last checkpointed
				 */
				if (zp->z_state & Z_CHANGED)
					zonedump(zp);
				break;
#endif /* ALLOW_UPDATES */
			}
			gettime(&tt);
		}
	}
	sched_maint();
	dprintf(1, (ddt, "exit ns_maint()\n"));
}

/*
 * Find when the next refresh needs to be and set
 * interrupt time accordingly.
 */
void
sched_maint()
{
	register struct zoneinfo *zp;
	struct itimerval ival;
	time_t next_refresh = 0;
	static time_t next_alarm;

	for (zp = zones; zp < &zones[nzones]; zp++)
		if (zp->z_time != 0 &&
		    (next_refresh == 0 || next_refresh > zp->z_time))
			next_refresh = zp->z_time;
        /*
	 *  Schedule the next call to ns_maint.
	 *  Don't visit any sooner than maint_interval.
	 */
	bzero((char *)&ival, sizeof (ival));
	if (next_refresh != 0) {
		if (next_refresh == next_alarm && alarm_pending) {
			dprintf(1, (ddt, "sched_maint: no schedule change\n"));
			return;
		}
		/*
		 *  tv_sec can be an unsigned long, so we can't let
		 *  it go negative.
		 */
		if (next_refresh < tt.tv_sec)
			next_refresh = tt.tv_sec;
		ival.it_value.tv_sec = next_refresh - tt.tv_sec;
		if ((long) ival.it_value.tv_sec < maint_interval)
			ival.it_value.tv_sec = maint_interval;
		next_alarm = next_refresh;
		alarm_pending = 1;
	}
	(void) setitimer(ITIMER_REAL, &ival, (struct itimerval *)NULL);
	dprintf(1, (ddt, "sched_maint: Next interrupt in %d sec\n",
		    ival.it_value.tv_sec));
}

/*
 * Mark a zone "up to date" after named-xfer tells us this or we
 * discover it through the qserial_*() logic.
 */
static void
markUpToDate(zp)
	struct zoneinfo *zp;
{
	zp->z_state &= ~Z_SYSLOGGED;
	zp->z_lastupdate = tt.tv_sec;
	zp->z_time = tt.tv_sec + zp->z_refresh;
	/*
	 * Restore Z_AUTH in case expired,
	 * but only if there were no errors
	 * in the zone file.
	 */
	if ((zp->z_state & Z_DB_BAD) == 0)
		zp->z_state |= Z_AUTH;
	if (zp->z_source) {
#if defined(USE_UTIME)
		struct utimbuf t;

		t.actime = tt.tv_sec;
		t.modtime = tt.tv_sec;
		(void) utime(zp->z_source, &t);
#else
		struct timeval t[2];

		t[0] = tt;
		t[1] = tt;
		(void) utimes(zp->z_source, t);
#endif /* USE_UTIME */
	}
}

/*
 * Query for the serial number of a zone, so that
 * we can check to see if we need to transfer it.
 */
static void
qserial_query(zp)
	struct zoneinfo *zp;
{
	struct qinfo *qp;

	dprintf(1, (ddt, "qserial_query(%s)\n", zp->z_origin));
	qp = sysquery(zp->z_origin, zp->z_class, T_SOA,
		      zp->z_addr, zp->z_addrcnt);
	if (!qp) {
		dprintf(1, (ddt, "qserial_query(%s) FAILED\n", zp->z_origin));
		return;		/* XXX - this is bad, we should do something */
	}
	qp->q_flags |= Q_ZSERIAL;
	qp->q_zquery = zp;
	zp->z_state |= Z_QSERIAL;
	dprintf(1, (ddt, "qserial_query(%s) QUEUED\n", zp->z_origin));
}

void
qserial_answer(qp, serial)
	struct qinfo *qp;
	u_int32_t serial;
{
	struct zoneinfo *zp = qp->q_zquery;

	dprintf(1, (ddt, "qserial_answer(%s, %lu)\n", zp->z_origin, serial));
	zp->z_state &= ~Z_QSERIAL;
	if (serial == 0) {
		/* an error occurred, or the query timed out, or this is
		 * the qremove() being called from ns_resp after calling us.
		 * we have already turned off the Z_QSERIAL bit, so just 
		 * return.  this is _not_ an error.
		 */
		return;
	}
	if (SEQ_GT(serial, zp->z_serial)) {
		dprintf(1, (ddt, "qserial_answer: zone is out of date\n"));
		zp->z_state |= Z_NEED_XFER;
		zp->z_xaddr = from_addr.sin_addr; /* don't use qp->q_from */
		xfers_deferred++;
		tryxfer();
	} else if (SEQ_GT(zp->z_serial, serial)) {
		dprintf(1, (ddt, "qserial_answer: serial# went backward\n"));
		if (!haveComplained((char*)zp, "went backward")) {
			syslog(LOG_CRIT,
	"Zone \"%s\" SOA serial# (%lu) rcvd from [%s] lower than ours (%lu)\n",
			       zp->z_origin, serial,
			       inet_ntoa(from_addr.sin_addr),
			       zp->z_serial);
		}
	} else {
		dprintf(1, (ddt, "qserial_answer: zone serial is still OK\n"));
		markUpToDate(zp);
	}
}

/*
 * Start an asynchronous zone transfer for a zone.
 * Depends on current time being in tt.
 * The caller must call sched_maint after startxfer.
 */
static void
startxfer(zp)
	struct zoneinfo *zp;
{
	static char *argv[NSMAX + 20], argv_ns[NSMAX][MAXDNAME];
	int cnt, argc = 0, argc_ns = 0, pid, omask;
	char debug_str[10];
	char serial_str[10];
	char port_str[10];
#ifdef GEN_AXFR
	char class_str[10];
#endif

	dprintf(1, (ddt, "startxfer() %s\n", zp->z_origin));

	argv[argc++] = "named-xfer";
	argv[argc++] = "-z";
	argv[argc++] = zp->z_origin;
	argv[argc++] = "-f";
	argv[argc++] = zp->z_source;
	argv[argc++] = "-s";
	sprintf(serial_str, "%lu", zp->z_serial);
	argv[argc++] = serial_str;
#ifdef GEN_AXFR
	argv[argc++] = "-C";
	sprintf(class_str, "%d", zp->z_class);
	argv[argc++] = class_str;
#endif
 	if (zp->z_state & Z_SYSLOGGED)
		argv[argc++] = "-q";
	argv[argc++] = "-P";
	sprintf(port_str, "%d", ns_port);
	argv[argc++] = port_str;
#ifdef STUBS
	if (zp->z_type == Z_STUB)
		argv[argc++] = "-S";
#endif
#ifdef DEBUG
	if (debug) {
		argv[argc++] = "-d";
		sprintf(debug_str, "%d", debug);
		argv[argc++] = debug_str;
		argv[argc++] = "-l";
		argv[argc++] = _PATH_XFERDDT;
		if (debug > 5) {
			argv[argc++] = "-t";
			argv[argc++] = _PATH_XFERTRACE;
		}
	}
#endif
	
	if (zp->z_xaddr.s_addr != 0x00000000) {
		/* address was specified by the qserial logic, use it */
		argv[argc++] = strcpy(argv_ns[argc_ns++],
				      inet_ntoa(zp->z_xaddr));
	} else {
		/*
		 * Copy the server ip addresses into argv, after converting
		 * to ascii and saving the static inet_ntoa result
		 */
		for (cnt = 0;  cnt < zp->z_addrcnt;  cnt++) {
			struct in_addr a;

			a = zp->z_addr[cnt];
			if (aIsUs(a)
			    && !haveComplained(zp->z_origin,
					       (char*)startxfer)) {
				syslog(LOG_ERR,
				  "attempted to fetch zone %s from self (%s)",
				       zp->z_origin, inet_ntoa(a));
				continue;
			}
			argv[argc++] = strcpy(argv_ns[argc_ns++],
					      inet_ntoa(a));
		}
        }

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
	gettime(&tt);
	omask = sigblock(sigmask(SIGCHLD));
#endif
	if ((pid = vfork()) == -1) {
		dprintf(1, (ddt, "xfer [v]fork: %d\n", errno));
		syslog(LOG_ERR, "xfer [v]fork: %m");
#ifndef SYSV
		(void) sigsetmask(omask);
#endif
		zp->z_time = tt.tv_sec + 10;
		return;
	}

	if (pid == 0) {
		/* child */
		execv(_PATH_XFER, argv);
		syslog(LOG_ERR, "can't exec %s: %m", _PATH_XFER);
		_exit(XFER_FAIL);	/* avoid duplicate buffer flushes */
	}
	/* parent */
	dprintf(1, (ddt, "started xfer child %d\n", pid));
	zp->z_state &= ~Z_NEED_XFER;
	zp->z_state |= Z_XFER_RUNNING;
	zp->z_xferpid = pid;
	xfers_running++;
	zp->z_time = tt.tv_sec + MAX_XFER_TIME;
#ifndef SYSV
	(void) sigsetmask(omask);
#endif
}

#ifdef DEBUG
void
printzoneinfo(zonenum)
int zonenum;
{
	struct timeval  tt;
	struct zoneinfo *zp = &zones[zonenum];
	char *ZoneType;

	if (!debug)
		return;

	fprintf(ddt, "printzoneinfo(%d):\n", zonenum);

	gettime(&tt);
	switch (zp->z_type) {
		case Z_PRIMARY: ZoneType = "Primary"; break;
		case Z_SECONDARY: ZoneType = "Secondary"; break;
#ifdef STUBS
		case Z_STUB: ZoneType = "Stub"; break;
#endif
		case Z_CACHE: ZoneType = "Cache"; break;
		default: ZoneType = "Unknown";
	}
	if (zp->z_origin != NULL && (zp->z_origin[0] == '\0'))
		fprintf(ddt,"origin ='.'");
	else
		fprintf(ddt,"origin ='%s'", zp->z_origin);
#ifdef GEN_AXFR
	fprintf(ddt,", class = %d", zp->z_class);
#endif
 	fprintf(ddt,", type = %s", ZoneType);
	if (zp->z_source)
		fprintf(ddt,", source = %s\n", zp->z_source);
	fprintf(ddt,"z_refresh = %ld", zp->z_refresh);
	fprintf(ddt,", retry = %ld", zp->z_retry);
	fprintf(ddt,", expire = %ld", zp->z_expire);
	fprintf(ddt,", minimum = %ld", zp->z_minimum);
	fprintf(ddt,", serial = %lu\n", zp->z_serial);
	fprintf(ddt,"z_time = %d", zp->z_time);
	if (zp->z_time) {
		fprintf(ddt,", now time : %d sec", tt.tv_sec);
		fprintf(ddt,", time left: %d sec", zp->z_time - tt.tv_sec);
	}
	fprintf(ddt,"; state %x\n", zp->z_state);
}
#endif /* DEBUG */

/*
 * remove_zone (htp, zone) --
 *     Delete all RR's in the zone "zone" under specified hash table.
 */
void
remove_zone(htp, zone)
	register struct hashbuf *htp;
	register int zone;
{
	register struct databuf *dp, *pdp;
	register struct namebuf *np, *pnp, *npn;
	struct namebuf **npp, **nppend;

	nppend = htp->h_tab + htp->h_size;
	for (npp = htp->h_tab; npp < nppend; npp++)
	    for (pnp = NULL, np = *npp; np != NULL; np = npn) {
		for (pdp = NULL, dp = np->n_data; dp != NULL; ) {
			if (dp->d_zone == zone)
				dp = rm_datum(dp, np, pdp);
			else {
				pdp = dp;
				dp = dp->d_next;
			}
		}

		if (np->n_hash) {
			/* call recursively to remove subdomains. */
			remove_zone(np->n_hash, zone);

			/* if now empty, free it */
			if (np->n_hash->h_cnt == 0) {
				free((char*)np->n_hash);
				np->n_hash = NULL;
			}
		}

		if ((np->n_hash == NULL) && (np->n_data == NULL)) {
			npn = rm_name(np, npp, pnp);
			htp->h_cnt--;
		} else {
			npn = np->n_next;
			pnp = np;
		}
	    }
}
   
/*
 * Abort an xfer that has taken too long.
 */
static void
abortxfer(zp)
	register struct zoneinfo *zp;
{

	kill(zp->z_xferpid, SIGKILL); /* don't trust it at all */
	dprintf(1, (ddt, "Killed child %d (zone %s) due to timeout\n",
		    zp->z_xferpid, zp->z_origin));
	zp->z_time = tt.tv_sec + zp->z_retry;
}

/*
 * SIGCHLD signal handler: process exit of xfer's.
 * (Note: also called when outgoing transfer completes.)
 */
SIG_FN
endxfer()
{
    	register struct zoneinfo *zp;   
	int exitstatus, pid, xfers = 0;
	int status;

	gettime(&tt);
#if defined(USE_WAITPID)
	while ((pid = waitpid(-1, &status, WNOHANG)) > 0) {
#else /* USE_WAITPID */
	{
		pid = wait(&status);
#endif /* USE_WAITPID */
		exitstatus = WIFEXITED(status) ?WEXITSTATUS(status) :0;

		for (zp = zones; zp < &zones[nzones]; zp++)
		    if (zp->z_xferpid == pid) {
			xfers++;
			xfers_running--;
			zp->z_xferpid = 0;
			zp->z_state &= ~Z_XFER_RUNNING;
			dprintf(1, (ddt,
		"\nendxfer: child %d zone %s returned status=%d termsig=%d\n", 
				    pid, zp->z_origin, exitstatus,
				    WIFSIGNALED(status) ?WTERMSIG(status) :-1
				    )
				);
			if (WIFSIGNALED(status)) {
				if (WTERMSIG(status) != SIGKILL) {
					syslog(LOG_ERR,
					  "named-xfer exited with signal %d\n",
					  WTERMSIG(status));
					dprintf(1, (ddt,
					"\tchild termination with signal %d\n",
						    WTERMSIG(status)));
				}
				zp->z_time = tt.tv_sec + zp->z_retry;
			} else switch (exitstatus) {
				case XFER_UPTODATE:
					markUpToDate(zp);
					break;

				case XFER_SUCCESS:
					zp->z_state |= Z_NEED_RELOAD;
					zp->z_state &= ~Z_SYSLOGGED;
					needzoneload++;
					break;

				case XFER_TIMEOUT:
					dprintf(1, (ddt,
		    "zoneref: Masters for secondary zone %s unreachable\n",
						    zp->z_origin));
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
						 "named-xfer for %s exited %d",
						    zp->z_origin, exitstatus);
					}
					/* FALLTHROUGH */
				case XFER_FAIL:
					zp->z_state |= Z_SYSLOGGED;
					zp->z_time = tt.tv_sec + zp->z_retry;
					break;
			}
			break;
		}
	}
	tryxfer();
#if defined(SYSV)
	(void)signal(SIGCLD, endxfer);
#endif
}

/*
 * Try to start some xfers
 */
static void
tryxfer()
{
	struct zoneinfo *zp;

	for (zp = zones;
	     xfers_deferred != 0
		&& xfers_running < MAX_XFERS_RUNNING
		&& zp < &zones[nzones];
	     zp++) {
		if (zp->z_state & Z_NEED_XFER) {
			xfers_deferred--;
			startxfer(zp);
		}
	}
	sched_maint();
}

/*
 * Reload zones whose transfers have completed.
 */
void
loadxfer()
{
    	register struct zoneinfo *zp;   

	gettime(&tt);
	for (zp = zones; zp < &zones[nzones]; zp++) {
		if (zp->z_state & Z_NEED_RELOAD) {
			dprintf(1, (ddt, "loadxfer() '%s'\n",
				    zp->z_origin[0] ? zp->z_origin : "."));
			zp->z_state &= ~(Z_NEED_RELOAD|Z_AUTH);
			remove_zone(hashtab, zp - zones);
			if (db_load(zp->z_source, zp->z_origin, zp, 0) == 0)
				zp->z_state |= Z_AUTH;
			if (zp->z_state & Z_TMP_FILE)
				(void) unlink(zp->z_source);
		}
	}
	sched_maint();
}
