#ifndef lint
static char sccsid[] = "@(#)ns_maint.c	4.3 (Berkeley) 6/4/86";
#endif

/*
 * Copyright (c) 1986 Regents of the University of California
 *	All Rights Reserved
 */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <stdio.h>
#include <syslog.h>
#include <signal.h>
#include <errno.h>
#include <arpa/nameser.h>
#include "ns.h"
#include "db.h"

/*
 * Invoked at regular intervals by signal interrupt; refresh all secondary
 * zones from primary name server and remove old cache entries.
 */
ns_maint()
{
	register struct zoneinfo *zp;
	struct itimerval ival;
	time_t now, next_refresh;
	int first;
	extern errno;	
#ifdef DEBUG
	if (debug)
		fprintf(ddt,"ns_maint()\n");
#endif


	first = 1;
	for (zp = zones; zp < &zones[nzones]; zp++) {
		if (zp->z_type != Z_SECONDARY)
			continue;
		if (gettimeofday(&tt, (struct timezone *)0) < 0)
			syslog(LOG_ERR, "gettimeofday failed: %m");
		now = tt.tv_sec;
#ifdef DEBUG
		if (debug >=2) {
			if (zp->z_origin[0] == '\0')
				fprintf(ddt,"origin ='.'");
			else
				fprintf(ddt,"origin ='%s'", zp->z_origin);
			fprintf(ddt,", source = %s\n", zp->z_source);
			fprintf(ddt,"z_refresh = %ld", zp->z_refresh);
			fprintf(ddt,", retry = %ld", zp->z_retry);
			fprintf(ddt,", expire = %ld", zp->z_expire);
			fprintf(ddt,", minimum = %ld", zp->z_minimum);
			fprintf(ddt,", serial = %ld\n", zp->z_serial);
			fprintf(ddt,"z_time = %d", zp->z_time);
			fprintf(ddt,", now time : %d sec", now);
			fprintf(ddt,", time left: %d sec\n", zp->z_time - now);
		}
#endif
		if (now >= zp->z_time) {
			zoneref(zp);
			zp->z_time = tt.tv_sec + zp->z_refresh;
		}
		/*
		 * Find when the next refresh needs to be and set
		 * interupt time accordingly.
		 * Why have needless intruptions.
		 * 	I just hate it when the cleaning crew come early.
		 */
		if (first) {
			next_refresh = zp->z_time;
			first = 0;
		}
		else if (next_refresh > zp->z_time)
			next_refresh = zp->z_time;
	}
	/*
	 * If first is still true, no secondary zones were found
	 *   therefore refreshes aren't needed and interupts are turned off
	 * This needs to be changed when we have refreshes for co-masters
	 */
	if (!first) {
		bzero((char *)&ival, sizeof (ival));
		ival.it_value.tv_sec = next_refresh - now;
		if (ival.it_value.tv_sec < 0)
			ival.it_value.tv_sec = 60;
		(void) setitimer(ITIMER_REAL, &ival, (struct itimerval *)NULL);
#ifdef DEBUG
		if (debug)
			fprintf(ddt,"exit ns_maint() Next interupt in %d sec\n",
				ival.it_value.tv_sec);
#endif
	}
}

zoneref(zp)
	struct zoneinfo *zp;
{
	register struct databuf *dp;
	HEADER *hp;
	u_short len;
	u_long serial;
	int s, n, l;
	int cnt, soacnt, error = 0;
	int zone = zp - zones;
	char *cp;
	char *tmp;
	char *fname;
	char buf[PACKETSZ];
	struct sockaddr_in sin;
	struct zoneinfo zp_start, zp_finish;
	struct databuf *pdp, *tdp;
	struct namebuf *np;
	struct hashbuf *htp;
	struct itimerval ival;
	struct itimerval zeroival;
	extern struct sockaddr_in nsaddr;
	extern int errno;
	extern int read_interrupted;
	extern int read_alarm();
	struct sigvec sv, osv;

#ifdef DEBUG
	if (debug)
		fprintf(ddt,"zoneref()\n");
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
			exit(1);
		}	
#ifdef DEBUG
		if (debug >= 2) {
			fprintf(ddt,"connecting to server #%d %s, %d\n",
			   cnt+1, inet_ntoa(sin.sin_addr), ntohs(sin.sin_port));
		}
#endif
		if (connect(s, &sin, sizeof(sin)) < 0) {
			(void) close(s);
			error++;
#ifdef DEBUG
			if (debug >= 2)
				fprintf(ddt,"connect failed\n");
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
		tmp += dn_skip(tmp) + QFIXEDSZ;
		tmp += dn_skip(tmp);
		soa_zinfo(&zp_start, tmp);
		if (zp->z_serial >= zp_start.z_serial)  {
			(void) close(s);
			(void) sigvec(SIGALRM, &osv, (struct sigvec *)0);
			return;
		}
		zp_finish = *zp;
		hp = (HEADER *) buf;
		soacnt = 0;
		for(;;) {
			if (soacnt == 0) {
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
				cp += dn_skip(cp) + QFIXEDSZ;
			tmp = cp + dn_skip(cp);
			n = doupdate(buf, sizeof(buf), cp, zone, 0);
			if ((cp - buf) + n != len) {
#ifdef DEBUG
			   if (debug)
	                     fprintf(ddt,"zoneref: doupdate failed (%d, %d)\n",
					cp - buf, n);
#endif
				error++;
				break;
			}
			if ((getshort(tmp)) == T_SOA) {
				if (soacnt == 0) {
					soacnt++;
					tmp += 3 * sizeof(u_short)
						+ sizeof(u_long);
					tmp += dn_skip(tmp);
					tmp += dn_skip(tmp);
					serial = getlong(tmp);
					continue;
				}
				soa_zinfo(zp, tmp);
				if (serial != zp->z_serial)
					 soacnt = 0;
				else {
					break;
				}
			}
		}
		(void) close(s);
		if ( error == 0) {
			htp = hashtab;
			np = nlookup(zp->z_origin, &htp, &fname, 0);
			if (np == NULL || fname != zp->z_origin) {
			    (void) sigvec(SIGALRM, &osv, (struct sigvec *)0);
			    return;
			}
			pdp = NULL;
			dp = np->n_data;
			while (dp != NULL) {
				if (!match(dp, C_ANY, T_SOA)) {
					pdp = dp;
					dp = dp->d_next;
					continue;
				}
			/* find serial number */
			cp = dp->d_data;
			cp += strlen(cp) + 1; /* origin */
			cp += strlen(cp) + 1; /* address */
			serial = getlong(cp);
#ifdef DEBUG
			if (debug >= 2)
				fprintf(ddt,"Found serial = %d\n",serial);
#endif

			/* remove data if not = current serial number */
				if (serial != zp->z_serial) {
#ifdef DEBUG
				    if (debug >= 2)
					fprintf(ddt,"deleting SOA serial #%d\n",
					    serial);
#endif
				    tdp = dp->d_next;
				    free((char *)dp);
				    dp = tdp;
				    if (pdp == NULL)
					    np->n_data = dp;
				    else
					    pdp->d_next = dp;
				    continue;
				}
				pdp = dp;
				dp = dp->d_next;
			}
			(void) sigvec(SIGALRM, &osv, (struct sigvec *)0);
			return;
		}
#ifdef DEBUG
		if (debug >= 2)
			fprintf(ddt,"error reciving zone transfer\n");
#endif
	}
	(void) sigvec(SIGALRM, &osv, (struct sigvec *)0);
	/*
	 *     Freedom at last!!
	 *
	 *  The land where all repressed slaves dream of.
	 *
	 *  Can't find a master to talk to.
	 *  syslog it and hope we can find a master during maintenance
	 */
	if (error)
	    syslog(LOG_ERR, "zoneref: Can't find Master for secondary zone %s",
		zp->z_origin);
	zp->z_refresh = zp->z_retry;
	if (gettimeofday(&tt, (struct timezone *)0) < 0)
			syslog(LOG_ERR, "gettimeofday failed: %m");
	zp->z_time = tt.tv_sec + zp->z_retry;
	return;
}
