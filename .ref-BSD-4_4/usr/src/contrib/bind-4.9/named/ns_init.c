#if !defined(lint) && !defined(SABER)
static char sccsid[] = "@(#)ns_init.c	4.38 (Berkeley) 3/21/91";
static char rcsid[] = "$Id:";
#endif /* not lint */

/*
 * ++Copyright++ 1986, 1990
 * -
 * Copyright (c) 1986, 1990 Regents of the University of California.
 * All rights reserved.
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
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <netinet/in.h>
#include <arpa/nameser.h>
#include <arpa/inet.h>
#include <syslog.h>
#include <signal.h>
#include <resolv.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include "../conf/portability.h"
#include "../conf/options.h"
#include "pathnames.h"
#include "ns.h"
#include "db.h"

#undef nsaddr

#ifdef BOGUSNS
extern struct netinfo *boglist;
#endif
#ifdef XFRNETS
extern struct netinfo *xfrnets;
#endif
extern struct netinfo **enettab;
struct	zoneinfo *zones;		/* zone information */
int	nzones;				/* number of zones in use */
int	forward_only = 0;		/* run only as a slave */
char	*cache_file;
#ifdef LOCALDOM
char	*localdomain;			/* "default" for non-dotted names */
#endif
int	maint_interval = 15*60;		/* minimum ns_maint() interval */

extern	int lineno;

extern void get_netlist(), endline(), remove_zone(), gettime();
extern int db_load(), getword();

void zoneinit(), get_forwarders(), boot_read(), free_forwarders();

#ifdef DEBUG
void content_zone();
#endif

/*
 * Read boot file for configuration info.
 */
void
ns_init(bootfile)
	char *bootfile;
{
	register struct zoneinfo *zp;
	extern int needmaint;
	static int loads = 0;			/* number of times loaded */

#ifdef DEBUG
	if (debug)
		fprintf(ddt,"\nns_init(%s)\n", bootfile);
#endif
	gettime(&tt);

        if (loads == 0) {
		if ((zones =
			(struct zoneinfo *)calloc(64, sizeof(struct zoneinfo)))
					    == NULL) {
		    syslog(LOG_ERR,
			"Not enough memory to allocate initial zones array");
		    exit(1);
		}
		nzones = 1;		/* zone zero is cache data */
		/* allocate cache hash table, formerly the root hash table. */
		hashtab = savehash((struct hashbuf *)NULL);

		/* allocate root-hints/file-cache hash table */
		fcachetab = savehash((struct hashbuf *)NULL);
		/* init zone data */
		zones[0].z_type = Z_CACHE;
        } else {
		/* Mark previous zones as not yet found in boot file. */
		for (zp = &zones[1]; zp < &zones[nzones]; zp++)
			zp->z_state &= ~Z_FOUND;
#ifdef LOCALDOM
		if (localdomain) {
			free(localdomain);
			localdomain = NULL;
		}
#endif
		free_forwarders();
		free_netlist(enettab);
#ifdef XFRNETS
		free_netlist(&xfrnets);
#endif
#ifdef BOGUSNS
		free_netlist(&boglist);
#endif
		forward_only = 0;
	}

#ifdef DEBUG
        if (debug >= 3) {
        	fprintf(ddt,"\n content of zones before loading \n");
        	content_zone(nzones - 1);
        }
#endif
	boot_read(bootfile);

        /* erase all old zones that were not found */
        for (zp = &zones[1]; zp < &zones[nzones]; zp++)
            if (zp->z_type && (zp->z_state & Z_FOUND) == 0) {
		remove_zone(hashtab, zp - zones);
		bzero((char *) zp, sizeof(*zp));
#ifdef DEBUG
		if(debug >=2)
			fprintf(ddt,"\n zone no %d was removed \n", zp - zones);
#endif
        }
#ifdef DEBUG
        if(debug >= 2) {
        	fprintf(ddt,"\n content of zones after loading \n");
        	content_zone(nzones-1);
        }
#endif

	/*
	 * Schedule calls to ns_maint().
	 */
	if (needmaint == 0)
		sched_maint();
#ifdef DEBUG
	if (debug)
		fprintf(ddt,"exit ns_init()%s\n", needmaint ?
		    ", need maintenance immediately" : "");
#endif
	loads++;
}

/*
 * Read the actual boot file.
 * Set up to recurse.
 */
void
boot_read(bootfile)
	char * bootfile;
{
	register struct zoneinfo *zp;
	struct zoneinfo *find_zone();
	char buf[BUFSIZ], obuf[BUFSIZ], *source;
	FILE *fp;
	int type;
	extern int needmaint;
	struct stat f_time;
	static int tmpnum = 0;		/* unique number for tmp zone files */
#ifdef ALLOW_UPDATES
	char *cp, *flag;
#endif
	int slineno;			/* Saved global line number. */

	if ((fp = fopen(bootfile, "r")) == NULL) {
		syslog(LOG_ERR, "%s: %m", bootfile);
		exit(1);
	}

	slineno = lineno;
	lineno = 0;

	while (!feof(fp) && !ferror(fp)) {
		if (!getword(buf, sizeof(buf), fp))
			continue;
		/* read named.boot keyword and process args */
		if (strcasecmp(buf, "directory") == 0) {
			(void) getword(buf, sizeof(buf), fp);
			if (chdir(buf) < 0) {
				syslog(LOG_CRIT, "directory %s: %m\n",
					buf);
				exit(1);
			}
			continue;
		} else if (strcasecmp(buf, "sortlist") == 0) {
			get_netlist(fp, enettab, ALLOW_NETS, buf);
			continue;
		} else if (strcasecmp(buf, "forwarders") == 0) {
			get_forwarders(fp);
			continue;
		} else if (strcasecmp(buf, "slave") == 0) {
			forward_only++;
			endline(fp);
			continue;
#ifdef BOGUSNS
		} else if (strcasecmp(buf, "bogusns") == 0) {
			get_netlist(fp, &boglist, ALLOW_ALL, buf);
			continue;
#endif
#ifdef XFRNETS
		} else if ((strcasecmp(buf, "tcplist") == 0) ||
			   (strcasecmp(buf, "xfrnets") == 0)) {
			get_netlist(fp, &xfrnets, ALLOW_ALL, buf);
			continue;
#endif
#ifdef LOCALDOM
		} else if (strcasecmp(buf, "domain") == 0) {
			if (getword(buf, sizeof(buf), fp))
				localdomain = savestr(buf);
			endline(fp);
			continue;
#endif
		} else if (strcasecmp(buf, "include") == 0) {	/* asp@uunet */
			if (getword(buf, sizeof(buf), fp))
				boot_read(buf);
			endline(fp);
			continue;
		} else if (strcasecmp(buf, "cache") == 0) {
			type = Z_CACHE;
		} else if (strcasecmp(buf, "primary") == 0) {
			type = Z_PRIMARY;
		} else if (strcasecmp(buf, "secondary") == 0) {
			type = Z_SECONDARY;
		} else {
			syslog(LOG_ERR, "%s: line %d: unknown field '%s'\n",
				bootfile, lineno+1, buf);
			endline(fp);
			continue;
		}

		/*
		 * read zone origin
		 */
		if (!getword(obuf, sizeof(obuf), fp)) {
			syslog(LOG_ERR, "%s: line %d: missing origin\n",
			    bootfile, lineno);
			continue;
		}
#ifdef DEBUG
		if (debug)
			fprintf(ddt, "zone origin %s", obuf);
#endif
		if (obuf[0] == '.' && obuf[1] == '\0')
			obuf[0] = '\0';
		/*
		 * read source file or host address
		 */
		if (!getword(buf, sizeof(buf), fp)) {
			syslog(LOG_ERR, "%s: line %d: missing %s\n",
				bootfile, lineno, 
			   (type == Z_SECONDARY)
			       ?"host address"
			       :"source file");
			continue;
		}

		/* check for previous instance of this zone (reload) */
		if ((zp = find_zone(obuf, type)) == (struct zoneinfo *)0) {
			if (type == Z_CACHE) {
				zp = &zones[0];
				zp->z_origin = "";
				goto gotzone;
			}
			for (zp = &zones[1]; zp < &zones[nzones]; zp++)
				if (zp->z_type == Z_NIL)
					goto gotzone;
			/*
			 * this code assumes that nzones never decreases
			 */
			if (nzones % 64 == 0) {
#ifdef DEBUG
			    if (debug > 1)
				fprintf(ddt, "Reallocating zones structure\n");
#endif
			    /*
			     * Realloc() not used since it might damage zones
			     * if an error occurs
			     */
			    zp = (struct zoneinfo *)
				malloc((64 + nzones)
				       * sizeof(struct zoneinfo));
			    if (zp == (struct zoneinfo *)0) {
				    syslog(LOG_ERR,
					   "no memory for more zones");
#ifdef DEBUG
				    if (debug)
					fprintf(ddt,
					    "Out of memory for new zones\n");
#endif
				    endline(fp);
				    continue;
			    }
			    bcopy((char *)zones, (char *)zp,
				  nzones * sizeof(struct zoneinfo));
			    bzero((char *)&zp[nzones],
				  64 * sizeof(struct zoneinfo));
			    free(zones);
			    zones = zp;
			}
			zp = &zones[nzones++];
	gotzone:
			zp->z_origin = savestr(obuf);
			zp->z_type = type;
		}
		zp->z_addrcnt = 0;

		switch (type) {
		case Z_CACHE:
			source = savestr(buf);
#ifdef DEBUG
			if (debug)
				fprintf(ddt,", source = %s\n", source);
#endif
			zp->z_refresh = 0;	/* by default, no dumping */
			if (getword(buf, sizeof(buf), fp)) {
#ifdef notyet
				zp->z_refresh = atoi(buf);
				if (zp->z_refresh <= 0) {
					syslog(LOG_ERR,
				"%s: line %d: bad refresh time '%s', ignored\n",
						bootfile, lineno, buf);
					zp->z_refresh = 0;
				} else if (cache_file == NULL)
					cache_file = source;
#else
				syslog(LOG_WARNING,
				"%s: line %d: cache refresh ignored\n",
					bootfile, lineno);
#endif
				endline(fp);
			}
			/*
			 * If we've loaded this file, and the file has
			 * not been modified and contains no $include,
			 * then there's no need to reload.
			 */
			if (zp->z_source && (!strcmp(source, zp->z_source)) &&
			    (zp->z_state & Z_INCLUDE) == 0 && 
			    stat(zp->z_source, &f_time) == 0 &&
			    zp->z_ftime == f_time.st_mtime) {
#ifdef DEBUG
				if (debug)
					fprintf(ddt, "cache is up to date\n");
#endif
				if (source != cache_file)
					free(source);
				break; /* zone is already up to date */
			}

			/* file has changed, or hasn't been loaded yet */
			if (zp->z_source) {
				free(zp->z_source);
				remove_zone(fcachetab, 0);
			}
			zp->z_source = source;
#ifdef DEBUG
			if (debug)
				fprintf(ddt, "reloading zone\n");
#endif
			(void) db_load(zp->z_source, zp->z_origin, zp, 0);
			break;

		case Z_PRIMARY:
			source = savestr(buf);
#ifdef ALLOW_UPDATES
			if (getword(buf, sizeof(buf), fp)) {
				endline(fp);
				flag = buf;
				while (flag) {
				    cp = strchr(flag, ',');
				    if (cp)
					*cp++ = 0;
				    if (strcasecmp(flag, "dynamic") == 0)
					zp->z_state |= Z_DYNAMIC;
				    else if (strcasecmp(flag, "addonly") == 0)
					zp->z_state |= Z_DYNADDONLY;
				    else {
					syslog(LOG_ERR,
					       "%s: line %d: bad flag '%s'\n",
					       bootfile, lineno, flag);
				    }
				    flag = cp;
				}
			}
#else /*ALLOW_UPDATES*/
    		        endline(fp);
#endif

#ifdef DEBUG
			if (debug)
				fprintf(ddt,", source = %s\n", source);
#endif
			/*
			 * If we've loaded this file, and the file has
			 * not been modified and contains no $include,
			 * then there's no need to reload.
			 */
			if (zp->z_source && strcmp(source, zp->z_source) == 0 &&
			    (zp->z_state & Z_INCLUDE) == 0 && 
			    stat(zp->z_source, &f_time) == 0 &&
			    zp->z_ftime == f_time.st_mtime) {
#ifdef DEBUG
				if (debug)
					fprintf(ddt, "zone is up to date\n");
#endif
				free(source);
				break; /* zone is already up to date */
			}
			if (zp->z_source) {
				free(zp->z_source);
				remove_zone(hashtab, zp - zones);
			}
                        zp->z_source = source;
			zp->z_state &= ~Z_AUTH;
#ifdef DEBUG
			if (debug)
				fprintf(ddt, "reloading zone\n");
#endif
			if (db_load(zp->z_source, zp->z_origin, zp, 0) == 0)
				zp->z_state |= Z_AUTH;
#ifdef ALLOW_UPDATES
			/* Guarantee calls to ns_maint() */
			zp->z_refresh = maint_interval;
#else
			zp->z_refresh = 0;	/* no maintenance needed */
			zp->z_time = 0;
#endif
			break;

		case Z_SECONDARY:
			source = NULL;
#ifdef DEBUG
			if (debug)
				fprintf(ddt,"\n\taddrs: ");
#endif
			do {
				if (!inet_aton(buf,
					      &zp->z_addr[zp->z_addrcnt])
				    ) {
					source = savestr(buf);
    		                        endline(fp);
					break;
				}
#ifdef DEBUG
				if (debug)
					fprintf(ddt,"%s, ",buf);
#endif
				if (++zp->z_addrcnt > NSMAX - 1) {
					zp->z_addrcnt = NSMAX - 1;
#ifdef DEBUG
					if (debug)
					    fprintf(ddt,
						"\nns.h NSMAX reached\n");
#endif
				}
			} while (getword(buf, sizeof(buf), fp));
#ifdef DEBUG
			if (debug)
				fprintf(ddt,"addrcnt = %d\n", zp->z_addrcnt);
#endif
			if (!source) {
				/*
				 * We will always transfer this zone again
				 * after a reload.
				 */
				sprintf(buf, "/%s/NsTmp%d.%d", _PATH_TMPDIR,
					getpid(), tmpnum++);
				source = savestr(buf);
				zp->z_state |= Z_TMP_FILE;
			} else
				zp->z_state &= ~Z_TMP_FILE;
			/*
			 * If we had a backup file name, and it was changed,
			 * free old zone and start over.  If we don't have
			 * current zone contents, try again now in case
			 * we have a new server on the list.
			 */
			if (zp->z_source && strcmp(source, zp->z_source)) {
#ifdef DEBUG
				if (debug)
					fprintf(ddt,"backup file changed\n");
#endif
				free(zp->z_source);
				zp->z_source = 0;
				zp->z_state &= ~Z_AUTH;
				zp->z_serial = 0;	/* force xfer */
                        	remove_zone(hashtab, zp - zones);
			}
			if (zp->z_source)
				free(source);
			else
				zp->z_source = source;
			if (!(zp->z_state & Z_AUTH))
				zoneinit(zp);
#ifdef FORCED_RELOAD
			else {
				extern int reloading;

				/* 
				** Force secondary to try transfer right away 
				** after SIGHUP.
				*/
				if(reloading == 1)
					zp->z_time = tt.tv_sec;
			}
#endif /* FORCED_RELOAD */
			break;

		}
                zp->z_state |= Z_FOUND;
#ifdef DEBUG
		if (debug)
			fprintf(ddt,"zone[%d] type %d: '%s'",
			        zp-zones, type,
				*(zp->z_origin) == '\0' ? "." : zp->z_origin);
#endif
		if (zp->z_refresh && zp->z_time == 0)
			zp->z_time = zp->z_refresh + tt.tv_sec;
		if (zp->z_time <= tt.tv_sec)
			needmaint = 1;
#ifdef DEBUG
		if (debug)
			fprintf(ddt, " z_time %d, z_refresh %d\n",
			    zp->z_time, zp->z_refresh);
#endif
	}
	(void) my_fclose(fp);
	lineno = slineno;
}

void
zoneinit(zp)
	register struct zoneinfo *zp;
{
	extern int needmaint;
	struct stat sb;

	/*
	 * Try to load zone from backup file,
	 * if one was specified and it exists.
	 * If not, or if the data are out of date,
	 * we will refresh the zone from a primary
	 * immediately.
	 */
	if (zp->z_source == NULL)
		return;
	if (stat(zp->z_source, &sb) == -1 ||
	    db_load(zp->z_source, zp->z_origin, zp, 0) != 0) {
		/*
		 * Set zone to be refreshed immediately.
		 */
		zp->z_refresh = INIT_REFRESH;
		zp->z_retry = INIT_REFRESH;
		zp->z_time = tt.tv_sec;
		needmaint = 1;
	} else
		zp->z_state |= Z_AUTH;
}

#ifdef ALLOW_UPDATES
/*
 * Look for the authoritative zone with the longest matching RHS of dname
 * and return its zone # or zero if not found.
 */
int
findzone(dname, class)
	char *dname;
	int class;
{
	char *dZoneName, *zoneName;
	int dZoneNameLen, zoneNameLen;
	int maxMatchLen = 0;
	int maxMatchZoneNum = 0;
	int zoneNum;

#ifdef DEBUG
	if (debug >= 4)
		fprintf(ddt, "findzone(dname=%s, class=%d)\n", dname, class);
	if (debug >= 5) {
		fprintf(ddt, "zone dump:\n");
		for (zoneNum = 1; zoneNum < nzones; zoneNum++)
			printzoneinfo(zoneNum);
	}
#endif

	dZoneName = strchr(dname, '.');
	if (dZoneName == NULL)
		dZoneName = "";	/* root */
	else
		dZoneName++;	/* There is a '.' in dname, so use remainder of
				   string as the zone name */
	dZoneNameLen = strlen(dZoneName);
	for (zoneNum = 1; zoneNum < nzones; zoneNum++) {
		zoneName = (zones[zoneNum]).z_origin;
		zoneNameLen = strlen(zoneName);
		/* The zone name may or may not end with a '.' */
		if (zoneName[zoneNameLen - 1] == '.')
			zoneNameLen--;
		if (dZoneNameLen != zoneNameLen)
			continue;
#ifdef DEBUG
		if (debug >= 5)
			fprintf(ddt, "about to strncasecmp('%s', '%s', %d)\n",
			        dZoneName, zoneName, dZoneNameLen);
#endif
		if (strncasecmp(dZoneName, zoneName, dZoneNameLen) == 0) {
#ifdef DEBUG
			if (debug >= 5)
				fprintf(ddt, "match\n");
#endif
			/*
			 * See if this is as long a match as any so far.
			 * Check if "<=" instead of just "<" so that if
			 * root domain (whose name length is 0) matches,
			 * we use it's zone number instead of just 0
			 */
			if (maxMatchLen <= zoneNameLen) {
				maxMatchZoneNum = zoneNum;
				maxMatchLen = zoneNameLen;
			}
		}
#ifdef DEBUG
		else
			if (debug >= 5)
				fprintf(ddt, "no match\n");
#endif
	}
#ifdef DEBUG
	if (debug >= 4)
		fprintf(ddt, "findzone: returning %d\n", maxMatchZoneNum);
#endif
	return (maxMatchZoneNum);
}
#endif /* ALLOW_UPDATES */

#ifdef SLAVE_FORWARD
int slave_retry = 4;  /* default it to 4 seconds */
#endif /* SLAVE_FORWARD */

void
get_forwarders(fp)
	FILE *fp;
{
	char buf[BUFSIZ];
	register struct fwdinfo *fip = NULL, *ftp = NULL;
	extern u_short ns_port;		/* port to which we send queries */
	extern struct fwdinfo *fwdtab;

#ifdef SLAVE_FORWARD
	int forward_count = 0;
#endif

#ifdef DEBUG
	if (debug)
		fprintf(ddt,"forwarders ");
#endif

	/* on mulitple forwarder lines, move to end of the list */
#ifdef SLAVE_FORWARD
	if (fwdtab != NULL){
		forward_count++;
		for (fip = fwdtab; fip->next != NULL; fip = fip->next)
			forward_count++;
	}
#else
	if (fwdtab != NULL)
		for (fip = fwdtab; fip->next != NULL; fip = fip->next)
			;
#endif /* SLAVE_FORWARD */

	while (getword(buf, sizeof(buf), fp)) {
		if (strlen(buf) == 0)
			break;
#ifdef DEBUG
		if (debug)
			fprintf(ddt," %s",buf);
#endif
		if (ftp == NULL)
			ftp = (struct fwdinfo *)malloc(sizeof(struct fwdinfo));
		if (inet_aton(buf, &ftp->fwdaddr.sin_addr)) {
			ftp->fwdaddr.sin_port = ns_port;
			ftp->fwdaddr.sin_family = AF_INET;
		} else {
			syslog(LOG_ERR, "'%s' (ignored, NOT dotted quad)",
			       buf);
#ifdef DEBUG
			if (debug)
				fprintf(ddt," (ignored, NOT dotted quad)");
#endif
			continue;	
		}
#ifdef FWD_LOOP
		if (aIsUs(ftp->fwdaddr.sin_addr)) {
			syslog(LOG_ERR,
			       "Forwarder '%s' ignored, my address",
			       buf);
#ifdef DEBUG
			if (debug)
				fprintf(ddt," (ignored, my address)");
#endif
			continue;
		}
#endif /* FWD_LOOP */
		ftp->next = NULL;
		if (fwdtab == NULL)
			fwdtab = ftp;	/* First time only */
		else
			fip->next = ftp;
		fip = ftp;
		ftp = NULL;
#ifdef SLAVE_FORWARD
		forward_count++;
#endif /* SLAVE_FORWARD */
	}
	if (ftp)
		free((char *)ftp);
	
#ifdef SLAVE_FORWARD
	/*
	** Set the slave retry time to 60 seconds total divided
	** between each forwarder
	*/
	if(forward_count != 0){
		slave_retry = (int) (60 / forward_count);
		if(slave_retry <= 0)
			slave_retry = 1;
	}
#endif

#ifdef DEBUG
	if (debug) 
		fprintf(ddt,"\n");
	if (debug > 2)
		for (ftp = fwdtab; ftp != NULL; ftp = ftp->next)
			fprintf(ddt,"ftp x%x %s next x%x\n", ftp,
				inet_ntoa(ftp->fwdaddr.sin_addr), ftp->next);
#endif
}

void
free_forwarders()
{
	extern struct fwdinfo *fwdtab;
	register struct fwdinfo *ftp, *fnext;

	for (ftp = fwdtab; ftp != NULL; ftp = fnext) {
		fnext = ftp->next;
		free((char *)ftp);
	}
	fwdtab = NULL;
}

struct zoneinfo *
find_zone(name, type) 
	char *name;
	int type;
{
	register struct zoneinfo *zp;

        for (zp = &zones[1]; zp < &zones[nzones]; zp++)
		if (zp->z_type == type && strcasecmp(name, zp->z_origin) == 0) {
#ifdef DEBUG
			if (debug > 1)
				fprintf(ddt, ", old zone (%d)", zp - zones);
#endif
			return (zp);
		}
#ifdef DEBUG
	if(debug > 1)
		fprintf(ddt, ", new zone");
#endif
	return ((struct zoneinfo *)NULL);
}

#ifdef DEBUG
/* prints out the content of zones */
void
content_zone(end) 
	int  end;
{
	int i;
	for (i = 1; i <= end; i++)
		printzoneinfo(i);
}
#endif
