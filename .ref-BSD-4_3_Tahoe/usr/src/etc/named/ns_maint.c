/*
 * Copyright (c) 1986 Regents of the University of California.
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
static char sccsid[] = "@(#)ns_maint.c	4.24 (Berkeley) 6/18/88";
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
#include "ns.h"
#include "db.h"

extern int errno;
extern int maint_interval;


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
		fprintf(ddt,"ns_maint()\n");
#endif

	for (zp = zones, zonenum = 0; zp < &zones[nzones]; zp++, zonenum++) {
		switch(zp->z_type) {
#ifdef ALLOW_UPDATES
		case Z_PRIMARY:
#endif ALLOW_UPDATES
		case Z_SECONDARY:
		case Z_CACHE:
			break;

		default:
			continue;
		}
		gettime(&tt);
#ifdef DEBUG
		if (debug >= 2)
			printzoneinfo(zonenum);
#endif
		if (tt.tv_sec >= zp->z_time && zp->z_refresh > 0) {
			if (zp->z_type == Z_CACHE)
				doachkpt();
			if (zp->z_type == Z_SECONDARY)
				zoneref(zp);
#ifdef ALLOW_UPDATES
			/*
			 * Checkpoint the zone if it has changed
			 * since we last checkpointed
			 */
			if (zp->z_type == Z_PRIMARY && zp->hasChanged)
				zonedump(zp);
#endif ALLOW_UPDATES
			zp->z_time = tt.tv_sec + zp->z_refresh;
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

zoneref(zp)
	struct zoneinfo *zp;
{
	HEADER *hp;
	u_short len;
	u_long serial;
	int s, n, l, tries;
	int cnt, soacnt, error = 0;
	int zone = zp - zones;
	u_char *cp, *nmp, *eom;
	u_char *tmp;
	u_char buf[PACKETSZ];
	char name[MAXDNAME], name2[MAXDNAME];
	struct sockaddr_in sin;
	struct zoneinfo zp_start, zp_finish;
	struct itimerval ival;
	struct itimerval zeroival;
	extern struct sockaddr_in nsaddr;
	extern int errno;
	extern int read_interrupted;
	extern int read_alarm();
	struct sigvec sv, osv;

#ifdef DEBUG
	if (debug)
		fprintf(ddt,"zoneref() %s\n", zp->z_origin);
#endif
	bzero((char *)&zeroival, sizeof(zeroival));
	ival = zeroival;
	ival.it_value.tv_sec = 30;
	sv.sv_handler = read_alarm;
	sv.sv_onstack = 0;
	sv.sv_mask = ~0;
	(void) sigvec(SIGALRM, &sv, &osv);

	for( cnt = 0; cnt < zp->z_addrcnt; cnt++) {
		error = 0;
		bzero((char *)&sin, sizeof(sin));
		sin.sin_family = AF_INET;
		sin.sin_port = nsaddr.sin_port;
		sin.sin_addr = zp->z_addr[cnt];
		if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
			syslog(LOG_ERR, "zoneref: socket: %m");
			error++;
			break;
		}	
#ifdef DEBUG
		if (debug >= 2) {
			fprintf(ddt,"connecting to server #%d %s, %d\n",
			   cnt+1, inet_ntoa(sin.sin_addr), ntohs(sin.sin_port));
		}
#endif
		if (connect(s, (struct sockaddr *)&sin, sizeof(sin)) < 0) {
			(void) close(s);
			error++;
#ifdef DEBUG
			if (debug >= 2)
				fprintf(ddt,"connect failed, errno %d\n", errno);
#endif
			continue;
		}	
		if ((n = res_mkquery(QUERY, zp->z_origin, C_IN,
		    T_SOA, (char *)NULL, 0, NULL, buf, sizeof(buf))) < 0) {
		        syslog(LOG_ERR, "zoneref: res_mkquery failed");
			(void) close(s);
			(void) sigvec(SIGALRM, &osv, (struct sigvec *)0);
		        return;
		}
		/*
	 	 * Send length & message for zone transfer
	 	 */
		if (writemsg(s, buf, n) < 0) {
			(void) close(s);
			error++;
#ifdef DEBUG
			if (debug >= 2)
				fprintf(ddt,"writemsg failed\n");
#endif
			continue;	
		}
		/*
		 * Get out your butterfly net and catch the SOA
		 */
		cp = buf;
		l = sizeof(u_short);
		read_interrupted = 0;
		while (l > 0) {
			(void) setitimer(ITIMER_REAL, &ival,
			    (struct itimerval *)NULL);
			if ((n = recv(s, cp, l, 0)) > 0) {
				cp += n;
				l -= n;
			} else {
				if (errno == EINTR && !read_interrupted)
					continue;
				error++;
				break;
			}
		}
		(void) setitimer(ITIMER_REAL, &zeroival,
		    (struct itimerval *)NULL);
		if (error) {
			(void) close(s);
			continue;
		}
		if ((len = htons(*(u_short *)buf)) == 0) {
			(void) close(s);
			if (zp->z_sysloged == 0)
			    syslog(LOG_ERR,
				"no SOA from server %s, zone %s (len 0)\n",
				inet_ntoa(sin.sin_addr), zp->z_origin);
			continue;
		}
		l = len;
		cp = buf;
		while (l > 0) {
			(void) setitimer(ITIMER_REAL, &ival,
			    (struct itimerval *)NULL);
			if ((n = recv(s, cp, l, 0)) > 0) {
				cp += n;
				l -= n;
			} else {
				if (errno == EINTR && !read_interrupted)
					continue;
				error++;
				break;
			}
		}
		(void) setitimer(ITIMER_REAL, &zeroival,
		    (struct itimerval *)NULL);
		if (error) {
			(void) close(s);
			continue;
		}
#ifdef DEBUG
		if (debug >= 3) {
			fprintf(ddt,"len = %d\n", len);
			fp_query(buf, ddt);
		}
#endif DEBUG
		zp_start = *zp;
		tmp = buf + sizeof(HEADER);
		eom = buf + len;
		/* NEED TO CHECK MESSAGE LENGTH, ANCOUNT, AA */
		tmp += dn_skipname(tmp, eom) + QFIXEDSZ;
		tmp += dn_skipname(tmp, eom);
		soa_zinfo(&zp_start, tmp, eom);
		if (zp->z_serial >= zp_start.z_serial && zp->z_auth)  {
#ifdef DEBUG
			if (debug)
				fprintf(ddt,"zoneref: up to date (%d >= %d)\n",
					zp->z_serial, zp_start.z_serial);
#endif DEBUG
			zp->z_lastupdate = tt.tv_sec;
			zp->z_refresh = zp_start.z_refresh;
			(void) close(s);
			(void) sigvec(SIGALRM, &osv, (struct sigvec *)0);
			if (zp->z_source) {
#if defined(SYSV)
				struct utimbuf t;

				t.actime = t.modtime = tt.tv_sec;
				(void) utime(zp->z_source, &t);
#else
				struct timeval t[2];

				t[0] = tt;
				t[1] = tt;
				(void) utimes(zp->z_source, t);
#endif /* SYSV */
			}
			return;
		}
#ifdef DEBUG
		if (debug)
			fprintf(ddt,"zoneref: need xfer (%d < %d)\n",
				zp->z_serial, zp_start.z_serial);
#endif DEBUG
		hp = (HEADER *) buf;
		soacnt = 0;
		/* mark all existing RR's for zone as "old" */
		mark_zone (hashtab, zone, 1);
		for (tries = 0; ; tries++) {
			if (soacnt == 0) {
			    /* delete unmarked (new) RR's for zone */
			    if (tries)
			        clean_zone (hashtab, zone, 0);
			    if ((n = res_mkquery(QUERY, zp->z_origin, C_IN,
			      T_AXFR, (char *)NULL, 0, NULL,
			      buf, sizeof(buf))) < 0) {
				syslog(LOG_ERR, "zoneref: res_mkquery failed");
				(void) close(s);
				(void) sigvec(SIGALRM, &osv,
				    (struct sigvec *)0);
				return;
			    }
			    /*
	 	 	     * Send length & message for zone transfer
	 	 	     */
			    if (writemsg(s, buf, n) < 0) {
				    (void) close(s);
				    error++;
#ifdef DEBUG
				    if (debug >= 2)
					    fprintf(ddt,"writemsg failed\n");
#endif
				    break;	
			    }
			}
			/*
			 * Receive length & response
			 */
			cp = buf;
			l = sizeof(u_short);
			while (l > 0) {
				(void) setitimer(ITIMER_REAL, &ival,
				    (struct itimerval *)NULL);
				if ((n = recv(s, cp, l, 0)) > 0) {
					cp += n;
					l -= n;
				} else {
					if (errno == EINTR && !read_interrupted)
						continue;
					error++;
					break;
				}
			}
			(void) setitimer(ITIMER_REAL, &zeroival,
			    (struct itimerval *)NULL);
			if (error)
				break;
			if ((len = htons(*(u_short *)buf)) == 0)
				break;
			l = len;
			cp = buf;
			eom = buf + len;
			while (l > 0) {
				(void) setitimer(ITIMER_REAL, &ival,
				    (struct itimerval *)NULL);
				if ((n = recv(s, cp, l, 0)) > 0) {
					cp += n;
					l -= n;
				} else {
					if (errno == EINTR && !read_interrupted)
						continue;
					error++;
					break;
				}
			}
			(void) setitimer(ITIMER_REAL, &zeroival,
			    (struct itimerval *)NULL);
			if (error)
				break;
#ifdef DEBUG
			if (debug >= 3) {
				fprintf(ddt,"len = %d\n", len);
				fp_query(buf, ddt);
			}
#endif
			cp = buf + sizeof(HEADER);
			if (hp->qdcount)
				cp += dn_skipname(cp, eom) + QFIXEDSZ;
			nmp = cp;
			tmp = cp + dn_skipname(cp, eom);
			n = doupdate(buf, sizeof(buf), cp, zone,
					(struct databuf **)0, DB_NODATA);
			if (cp + n != eom) {
#ifdef DEBUG
			   if (debug)
	                     fprintf(ddt,"zoneref: doupdate failed (%d, %d)\n",
					cp - buf, n);
#endif
				error++;
				break;
			}
			GETSHORT(n, tmp);
			if (n == T_SOA) {
				if (soacnt == 0) {
					soacnt++;
					dn_expand(buf, buf + 512, nmp, name,
						sizeof(name));
					tmp += 2 * sizeof(u_short)
						+ sizeof(u_long);
					tmp += dn_skipname(tmp, eom);
					tmp += dn_skipname(tmp, eom);
					GETLONG(serial, tmp);
#ifdef DEBUG
					if (debug)
					    fprintf(ddt,
					        "first SOA for %s, serial %d\n",
					        name, serial);
#endif DEBUG
					continue;
				}
				dn_expand(buf, buf + 512, nmp, name2, 
					sizeof(name2));
				if (strcasecmp(name, name2) !=0) {
#ifdef DEBUG
					if (debug)
					    fprintf(ddt,
					      "extraneous SOA for %s\n",
					      name2);
#endif DEBUG
					continue;
				}
				tmp -= sizeof(u_short);
				soa_zinfo(&zp_finish, tmp, eom);
#ifdef DEBUG
				if (debug)
				    fprintf(ddt,
				      "SOA, serial %d\n", zp_finish.z_serial);
#endif DEBUG
				if (serial != zp_finish.z_serial) {
					 soacnt = 0;
#ifdef DEBUG
					if (debug)
					    fprintf(ddt,
						"serial changed, restart\n");
#endif DEBUG
				} else
					break;
			}
		}
		(void) close(s);
		if ( error == 0) {
			zp->z_refresh = zp_finish.z_refresh;
			zp->z_retry   = zp_finish.z_retry;
			zp->z_expire  = zp_finish.z_expire;
			zp->z_minimum = zp_finish.z_minimum;
			zp->z_serial  = zp_finish.z_serial;
			zp->z_lastupdate = tt.tv_sec;
			zp->z_sysloged = 0;
			zp->z_auth = 1;
			/* delete previously marked RR's here, then dump */
			clean_zone (hashtab, zone, 1);
			zonedump(zp);
			(void) sigvec(SIGALRM, &osv, (struct sigvec *)0);
			return;
		}
		/* error: delete unmarked RR's here; remove old marks */
		clean_zone (hashtab, zone, 0);
		mark_zone (hashtab, zone, 0);
#ifdef DEBUG
		if (debug >= 2)
			fprintf(ddt,"error receiving zone transfer\n");
#endif
	}
	(void) sigvec(SIGALRM, &osv, (struct sigvec *)0);
	/*
	 *     Freedom at last!!
	 *
	 *  The land where all repressed slaves dream of.
	 *
	 *  Can't find a master to talk to.
	 *  syslog it and hope we can find a master during next maintenance.
	 */
	if (error && (!zp->z_sysloged)) {
	    syslog(LOG_WARNING,
		"zoneref: Masters for secondary zone %s unreachable",
		zp->z_origin);
	    zp->z_sysloged++;
	}
	zp->z_refresh = zp->z_retry;
	if (tt.tv_sec - zp->z_lastupdate > zp->z_expire)
		zp->z_auth = 0;
}

#ifdef unused
/*
 * Recursively delete all domains (except for root SOA records),
 * starting from head of list pointed to by np.
 */
static DelDom(fnp, isroot)
	struct namebuf *fnp;
	int isroot;
{
	register struct databuf *dp, *pdp = NULL;
	register struct namebuf *np = fnp;
	struct namebuf **npp, **nppend;

#ifdef DEBUG
	if (debug >= 3)
		fprintf(ddt, "DelDom('%s', %d)\n", fnp->n_dname, isroot);
#endif DEBUG

	/* first do data records */
	for (dp = np->n_data; dp != NULL; ) {
		/* skip the root SOA record (marks end of data) */
		if (isroot && dp->d_type == T_SOA) {
			pdp = dp;
			dp = dp->d_next;
			continue;
                }
		dp = rm_datum(dp, np, pdp);
	}

	/* next do subdomains */
        if (np->n_hash == NULL)
                return;
	npp = np->n_hash->h_tab;
	nppend = npp + np->n_hash->h_size;
	while (npp < nppend) {
		for (np = *npp++; np != NULL; np = np->n_next) {
			DelDom(np, 0);
		}
	}
}
#endif unused

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
	fprintf(ddt,", time left: %d sec\n", zp->z_time - tt.tv_sec);
}
#endif DEBUG

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

