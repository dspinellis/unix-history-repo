/*
 * Copyright (c) 1983 The Regents of the University of California.
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
char copyright[] =
"@(#) Copyright (c) 1983 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)ruptime.c	5.5 (Berkeley) 8/25/88";
#endif /* not lint */

#include <sys/param.h>
#include <sys/dir.h>
#include <sys/file.h>
#include <protocols/rwhod.h>
#include <stdio.h>

#define	NHOSTS	100
int	nhosts;
struct hs {
	struct	whod *hs_wd;
	int	hs_nusers;
} hs[NHOSTS];
struct	whod awhod;

#define	WHDRSIZE	(sizeof (awhod) - sizeof (awhod.wd_we))
#define	RWHODIR		"/usr/spool/rwho"
#define	down(h)		(now - (h)->hs_wd->wd_recvtime > 11 * 60)

time_t	now;
int	rflg = 1;
int	hscmp(), ucmp(), lcmp(), tcmp();

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	register struct hs *hsp = hs;
	register struct whod *wd;
	register struct whoent *we;
	register DIR *dirp;
	struct direct *dp;
	int aflg, cc, ch, f, i, maxloadav;
	char buf[sizeof(struct whod)];
	int (*cmp)() = hscmp;
	time_t time();
	char *interval(), *malloc();

	aflg = 0;
	maxloadav = -1;
	while ((ch = getopt(argc, argv, "alrut")) != EOF)
		switch((char)ch) {
		case 'a':
			aflg = 1;
			break;
		case 'l':
			cmp = lcmp;
			break;
		case 'r':
			rflg = -1;
			break;
		case 't':
			cmp = tcmp;
			break;
		case 'u':
			cmp = ucmp;
			break;
		default: 
			fprintf(stderr, "usage: ruptime [-alrut]\n");
			exit(1);
		}

	if (chdir(RWHODIR) || (dirp = opendir(".")) == NULL) {
		perror(RWHODIR);
		exit(1);
	}
	while (dp = readdir(dirp)) {
		if (dp->d_ino == 0 || strncmp(dp->d_name, "whod.", 5))
			continue;
		if (nhosts == NHOSTS) {
			fprintf(stderr, "ruptime: too many hosts\n");
			exit(1);
		}
		f = open(dp->d_name, O_RDONLY, 0);
		if (f > 0) {
			cc = read(f, buf, sizeof(struct whod));
			if (cc >= WHDRSIZE) {
				/* NOSTRICT */
				hsp->hs_wd = (struct whod *)malloc(WHDRSIZE);
				wd = (struct whod *)buf;
				bcopy(wd, hsp->hs_wd, WHDRSIZE);
				hsp->hs_nusers = 0;
				for (i = 0; i < 2; i++)
					if (wd->wd_loadav[i] > maxloadav)
						maxloadav = wd->wd_loadav[i];
				we = (struct whoent *)(buf+cc);
				while (--we >= wd->wd_we)
					if (aflg || we->we_idle < 3600)
						hsp->hs_nusers++;
				nhosts++; hsp++;
			}
			(void)close(f);
		}
	}
	if (!nhosts) {
		printf("ruptime: no hosts!?!\n");
		exit(1);
	}
	qsort((char *)hs, nhosts, sizeof (hs[0]), cmp);
	(void)time(&now);
	for (i = 0; i < nhosts; i++) {
		hsp = &hs[i];
		if (down(hsp)) {
			printf("%-12.12s%s\n", hsp->hs_wd->wd_hostname,
			    interval(now - hsp->hs_wd->wd_recvtime, "down"));
			continue;
		}
		printf("%-12.12s%s,  %4d user%s  load %*.2f, %*.2f, %*.2f\n",
		    hsp->hs_wd->wd_hostname,
		    interval(hsp->hs_wd->wd_sendtime -
			hsp->hs_wd->wd_boottime, "  up"),
		    hsp->hs_nusers,
		    hsp->hs_nusers == 1 ? ", " : "s,",
		    maxloadav >= 1000 ? 5 : 4,
			hsp->hs_wd->wd_loadav[0] / 100.0,
		    maxloadav >= 1000 ? 5 : 4,
		        hsp->hs_wd->wd_loadav[1] / 100.0,
		    maxloadav >= 1000 ? 5 : 4,
		        hsp->hs_wd->wd_loadav[2] / 100.0);
		cfree(hsp->hs_wd);
	}
	exit(0);
}

char *
interval(tval, updown)
	time_t tval;
	char *updown;
{
	static char resbuf[32];
	int days, hours, minutes;

	if (tval < 0 || tval > 365*24*60*60) {
		(void)sprintf(resbuf, "   %s ??:??", updown);
		return(resbuf);
	}
	minutes = (tval + 59) / 60;		/* round to minutes */
	hours = minutes / 60; minutes %= 60;
	days = hours / 24; hours %= 24;
	if (days)
		(void)sprintf(resbuf, "%s %2d+%02d:%02d",
		    updown, days, hours, minutes);
	else
		(void)sprintf(resbuf, "%s    %2d:%02d",
		    updown, hours, minutes);
	return(resbuf);
}

hscmp(h1, h2)
	struct hs *h1, *h2;
{
	return(rflg * strcmp(h1->hs_wd->wd_hostname, h2->hs_wd->wd_hostname));
}

/*
 * Compare according to load average.
 */
lcmp(h1, h2)
	struct hs *h1, *h2;
{
	if (down(h1))
		if (down(h2))
			return(tcmp(h1, h2));
		else
			return(rflg);
	else if (down(h2))
		return(-rflg);
	else
		return(rflg *
			(h2->hs_wd->wd_loadav[0] - h1->hs_wd->wd_loadav[0]));
}

/*
 * Compare according to number of users.
 */
ucmp(h1, h2)
	struct hs *h1, *h2;
{
	if (down(h1))
		if (down(h2))
			return(tcmp(h1, h2));
		else
			return(rflg);
	else if (down(h2))
		return(-rflg);
	else
		return(rflg * (h2->hs_nusers - h1->hs_nusers));
}

/*
 * Compare according to uptime.
 */
tcmp(h1, h2)
	struct hs *h1, *h2;
{
	return(rflg * (
		(down(h2) ? h2->hs_wd->wd_recvtime - now
			  : h2->hs_wd->wd_sendtime - h2->hs_wd->wd_boottime)
		-
		(down(h1) ? h1->hs_wd->wd_recvtime - now
			  : h1->hs_wd->wd_sendtime - h1->hs_wd->wd_boottime)
	));
}
