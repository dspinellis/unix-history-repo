#ifndef lint
static char sccsid[] = "@(#)ns_init.c	4.3 (Berkeley) 6/4/86";
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
#include <errno.h>
#include <signal.h>
#include <syslog.h>
#include <ctype.h>
#include <arpa/nameser.h>
#include "ns.h"
#include "db.h"

struct	zoneinfo zones[MAXZONES];	/* zone information */
int	nzones;				/* number of zones in use */

/*
 * Read boot file for configuration info.
 */
ns_init(bootfile)
	char *bootfile;
{
	register struct zoneinfo *zp;
	char buf[BUFSIZ];
	char *tm;
	FILE *fp;
	int type, first = 1;
	time_t next_refresh;
	struct itimerval ival;

#ifdef DEBUG
	if (debug >= 3)
		fprintf(ddt,"ns_init(%s)\n", bootfile);
#endif

	if ((fp = fopen(bootfile, "r")) == NULL) {
		syslog(LOG_ERR, "%s: %m", bootfile);
		exit(1);
	}

	/* allocate root hash table */
	hashtab = savehash((struct hashbuf *)NULL);

	/* init zone data */
	nzones = 1;		/* zone zero is cache data */
	while (getword(buf, sizeof(buf), fp)) {
		/* read 'primary', 'secondary', 'cache' or 'domain' */
	top:	
		if (cistrcmp(buf, "cache") == 0) {
			type = Z_CACHE;
			zp = zones;
		} else {
			if (cistrcmp(buf, "primary") == 0)
				type = Z_PRIMARY;
			else if (cistrcmp(buf, "secondary") == 0)
				type = Z_SECONDARY;
			else if (cistrcmp(buf, "domain") == 0)
				type = Z_DOMAIN;
			else {
				syslog(LOG_ERR, "%s: unknown type", buf);
				exit(1);
			}
			if (nzones >= MAXZONES) {
				syslog(LOG_ERR, "too many zones");
				exit(1);
			}
			zp = &zones[nzones++];
		}
		zp->z_type = type;
		zp->z_addrcnt = 0;
		/*
		 * read zone origin
		 */
		(void) getword(buf, sizeof(buf), fp);
		if (buf[0] == '.')
			buf[0] = '\0';
		zp->z_origin = savestr(buf);
		/*
		 * read source file or host address
		 */
		if (type != Z_DOMAIN) {
			(void) getword(buf, sizeof(buf), fp);
			zp->z_source = savestr(buf);
#ifdef DEBUG
			if (debug) {
				fprintf(ddt,"zone found (%d): ", zp->z_type);
				if (zp->z_origin[0] == '\0')
					fprintf(ddt,"'.'");
				else
					fprintf(ddt,"'%s'", zp->z_origin);
				fprintf(ddt,", source = %s", zp->z_source);
			}
#endif
		}
#ifdef DEBUG
		else if (debug)
			fprintf(ddt,"zone found (%d): domain name = '%s'",
				zp->z_type, zp->z_origin);
#endif
		switch (zp->z_type) {
		case Z_PRIMARY:
		case Z_CACHE:
#ifdef DEBUG
			if (debug)
				fprintf(ddt,"\n");
#endif
			(void) db_load(zp->z_source, zp->z_origin, zp - zones);
			break;

		case Z_SECONDARY:
			zp->z_addr[zp->z_addrcnt].s_addr =
				inet_addr(zp->z_source);
			if (zp->z_addr[zp->z_addrcnt].s_addr != (unsigned)-1)
					zp->z_addrcnt++;
			while (getword(buf, sizeof(buf), fp)) {
				tm = savestr(buf);
				zp->z_addr[zp->z_addrcnt].s_addr =
					inet_addr(tm);
				if (zp->z_addr[zp->z_addrcnt].s_addr ==
						(unsigned)-1) {
#ifdef DEBUG
					if (debug)
						fprintf(ddt," (addrcnt) = %d\n",
							zp->z_addrcnt);
#endif
					zoneinit(zp);
					if (first) {
					    next_refresh = zp->z_time;
					    first = 0;
					} else
					    if (next_refresh > zp->z_time)
						next_refresh = zp->z_time;
					goto top;
				}
#ifdef DEBUG
				if (debug)
					fprintf(ddt,", %s",buf);
#endif
				if (++zp->z_addrcnt >= MAXNS) {
					zp->z_addrcnt = MAXNS;
#ifdef DEBUG
					if (debug)
					    fprintf(ddt,
						"\nns.h MAXNS reached\n");
#endif
					break;
				}
			}
#ifdef DEBUG
			if (debug)
				fprintf(ddt," addrcnt = %d\n", zp->z_addrcnt);
#endif
			zoneinit(zp);
			if (first) {
				next_refresh = zp->z_time;
				first = 0;
			} else
				if (next_refresh > zp->z_time)
					next_refresh = zp->z_time;
			break;

		case Z_DOMAIN:
			while (getword(buf, sizeof(buf), fp)) {
				tm = savestr(buf);
				zp->z_addr[zp->z_addrcnt].s_addr =
					inet_addr(tm);
				if (zp->z_addr[zp->z_addrcnt].s_addr ==
						(unsigned)-1) {
#ifdef DEBUG
					if (debug)
						fprintf(ddt," addrcnt = %d\n",
							zp->z_addrcnt);
#endif
					goto top;
				}
#ifdef DEBUG
				if (debug)
					fprintf(ddt,", %s",buf);
#endif
				if (++zp->z_addrcnt >= MAXNS) {
				    zp->z_addrcnt = MAXNS;
#ifdef DEBUG
				    if (debug)
				       fprintf(ddt,"\nns.h MAXNS reached\n");
#endif
				    break;
				}
			}
#ifdef DEBUG
			if (debug)
				fprintf(ddt," addrcnt = %d\n",
					zp->z_addrcnt);
#endif
			break;

		}
	}
	(void) fclose(fp);
	if (!first) {
		if (gettimeofday(&tt, (struct timezone *)0) < 0) {
			syslog(LOG_ERR, "gettimeofday failed: %m");
			ival.it_value.tv_sec = 5 * 60;
		} else {
			bzero((char *)&ival, sizeof(ival));
			ival.it_value.tv_sec = next_refresh - tt.tv_sec;
			if (ival.it_value.tv_sec < 0)
				ival.it_value.tv_sec = 60;
		}
	} else
		ival.it_value.tv_sec = 0;
	(void) setitimer(ITIMER_REAL, &ival, (struct itimerval *)NULL);
#if DEBUG
	if (debug)
		fprintf(ddt,"exit ns_init() Next interupt in %d sec\n",
			ival.it_value.tv_sec);	
#endif
}

zoneinit(zp)
	struct zoneinfo *zp;
{
	struct sockaddr_in sin;
	HEADER *hp;
	char buf[PACKETSZ];
	u_short len;
	char *cp;
	char *tmp;
	int s, n, l;
	int cnt, soacnt, error = 0;
	int zone = zp - zones;
	u_long serial;
	struct itimerval ival;
	struct itimerval zeroival;
	extern struct sockaddr_in nsaddr;
	extern int errno;
	extern int read_alarm();
	struct sigvec sv, osv;
	extern int read_interrupted;
	
#ifdef DEBUG
	if (debug)
		fprintf(ddt,"zoneinit()\n");
#endif

	bzero((char *)&zeroival, sizeof(zeroival));
	ival = zeroival;
	ival.it_value.tv_sec = 2 * 60;
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
		if ((n = res_mkquery(QUERY, zp->z_origin, C_IN, T_AXFR,
	    	(char *)NULL, 0, NULL, buf, sizeof(buf))) < 0) {
			syslog(LOG_ERR, "zoneinit: res_mkquery failed");
			(void) sigvec(SIGALRM, &osv, (struct sigvec *)0);
			return;
		}
		if ((s = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
			syslog(LOG_ERR, "zoneref: socket: %m");
			exit(1);
		}	
#ifdef DEBUG
		if (debug >= 2) {
			fprintf(ddt,"connecting to server #%d %s, %d (%d)\n",
				cnt+1, inet_ntoa(sin.sin_addr),
				ntohs(sin.sin_port), n);
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
	
		hp = (HEADER *) buf;
		soacnt = 0;

		for (;;) {
			/*
			 * Receive length & response
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
	                     fprintf(ddt,"zoneinit: doupdate failed (%d, %d)\n",
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
			(void) sigvec(SIGALRM, &osv, (struct sigvec *)0);
			return;
		}
#ifdef DEBUG
		if (debug >= 2)
			fprintf(ddt,"error reciving zone transfer\n");
#endif
	}
	/*
	 *     Freedom at last!!
	 *
	 *  The land where all repressed slaves dream of.
	 *
	 *  Can't find a master to talk to.
	 *  syslog it and hope we can find a master during maintenance
	 */
	if (error)
	    syslog(LOG_ERR, "zoneinit: Can't find Master for secondary zone %s",
		zp->z_origin);
	/*
	 * Set zone to be refreshed in 5 min.
	 *	maybe by then we can refresh it.
	 */
	zp->z_refresh = 300; /* 300 seconds = 5 Min. */
	zp->z_retry = 300;
	if (gettimeofday(&tt, (struct timezone *)0) < 0)
		syslog(LOG_ERR, "gettimeofday failed: %m");
	zp->z_time = tt.tv_sec + 300;
	(void) sigvec(SIGALRM, &osv, (struct sigvec *)0);
	return;
}
#ifdef notdef
/*
 * Look for an authoritative zone that matches the RHS of dname
 * and return is zone # or zero if not found.
 */
findzone(dname, class)
	char *dname;
	int class;
{
	register struct zoneinfo *zp;
	register char *d1, *d2;
	register int c;
	char *end;

	end = dname + strlen(dname);
	for (zp = &zones[1]; zp < &zones[nzones]; zp++) {
		d1 = end;
		d2 = zp->z_origin + strlen(zp->z_origin);
		while (d2 > zp->z_origin) {
			if ((c*--
		}
		return (zp - zones);
	}
	return (0);
}
#endif

soa_zinfo(zp, cp)
	struct zoneinfo *zp;
	char *cp;
{
	cp += 3 * sizeof(u_short);
	cp += sizeof(u_long);
	cp += dn_skip(cp);
	cp += dn_skip(cp);
	zp->z_serial = getlong(cp);
	cp += sizeof(u_long);
	zp->z_refresh = getlong(cp);
	if (gettimeofday(&tt, (struct timezone *)0) < 0)
		syslog(LOG_ERR, "gettimeofday failed: %m");
	zp->z_time = tt.tv_sec + zp->z_refresh;
	cp += sizeof(u_long);
	zp->z_retry = getlong(cp);
	cp += sizeof(u_long);
	zp->z_expire = getlong(cp);
	cp += sizeof(u_long);
	zp->z_minimum = getlong(cp);
	cp += sizeof(u_long);
}
