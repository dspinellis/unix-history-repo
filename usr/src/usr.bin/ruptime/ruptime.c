#ifndef lint
static char sccsid[] = "@(#)ruptime.c	4.6 82/11/14";
#endif

#include <sys/param.h>
#include <stdio.h>
#include <dir.h>
#include <utmp.h>
#include "rwhod.h"

DIR	*etc;

#define	NHOSTS	100
int	nhosts;
struct	hs {
	struct	whod *hs_wd;
	int	hs_nusers;
} hs[NHOSTS];
struct	whod awhod;
int	hscmp(), ucmp(), lcmp(), tcmp();

#define	WHDRSIZE	(sizeof (awhod) - sizeof (awhod.wd_we))

char	*interval();
int	now;
char	*malloc(), *sprintf();
int	aflg;

#define down(h)		(now - (h)->hs_wd->wd_recvtime > 5 * 60)

main(argc, argv)
	int argc;
	char **argv;
{
	struct direct *dp;
	int f, i, t;
	char buf[BUFSIZ]; int cc;
	register struct hs *hsp = hs;
	register struct whod *wd;
	register struct whoent *we;
	int maxloadav = 0;
	int (*cmp)() = hscmp;

	time(&t);
	argc--, argv++;
again:
	if (!strcmp(*argv, "-a")) {
		aflg++;
		argc--, argv++;
		goto again;
	}
	if (!strcmp(*argv, "-l")) {
		cmp = lcmp;
		argc--, argv++;
		goto again;
	}
	if (!strcmp(*argv, "-u")) {
		cmp = ucmp;
		argc--, argv++;
		goto again;
	}
	if (!strcmp(*argv, "-t")) {
		cmp = tcmp;
		argc--, argv++;
		goto again;
	}
	if (chdir("/etc") < 0) {
		perror("/etc");
		exit(1);
	}
	etc = opendir(".");
	if (etc == NULL) {
		perror("/etc");
		exit(1);
	}
	while (dp = readdir(etc)) {
		if (dp->d_ino == 0)
			continue;
		if (strncmp(dp->d_name, "whod.", 5))
			continue;
		if (nhosts == NHOSTS) {
			fprintf(stderr, "too many hosts\n");
			exit(1);
		}
		f = open(dp->d_name, 0);
		if (f > 0) {
			cc = read(f, buf, BUFSIZ);
			if (cc >= WHDRSIZE) {
				hsp->hs_wd = (struct whod *)malloc(WHDRSIZE);
				wd = (struct whod *)buf;
				bcopy(buf, hsp->hs_wd, WHDRSIZE);
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
		}
		(void) close(f);
	}
	(void) time(&now);
	qsort((char *)hs, nhosts, sizeof (hs[0]), cmp);
	if (nhosts == 0) {
		printf("no hosts!?!\n");
		exit(1);
	}
	for (i = 0; i < nhosts; i++) {
		hsp = &hs[i];
		if (down(hsp)) {
			printf("%-8.8s%s\n", hsp->hs_wd->wd_hostname,
			    interval(now - hsp->hs_wd->wd_recvtime, "down"));
			continue;
		}
		printf("%-8.8s%s,  %4d user%s  load %*.2f, %*.2f, %*.2f\n",
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
interval(time, updown)
	int time;
	char *updown;
{
	static char resbuf[32];
	int days, hours, minutes;

	if (time < 0 || time > 3*30*24*60*60) {
		(void) sprintf(resbuf, "   %s ??:??", updown);
		return (resbuf);
	}
	minutes = (time + 59) / 60;		/* round to minutes */
	hours = minutes / 60; minutes %= 60;
	days = hours / 24; hours %= 24;
	if (days)
		(void) sprintf(resbuf, "%s %2d+%02d:%02d",
		    updown, days, hours, minutes);
	else
		(void) sprintf(resbuf, "   %s %2d:%02d",
		    updown, hours, minutes);
	return (resbuf);
}

hscmp(h1, h2)
	struct hs *h1, *h2;
{

	return (strcmp(h1->hs_wd->wd_hostname, h2->hs_wd->wd_hostname));
}

/*
 * Compare according to load average.
 */
lcmp(h1, h2)
	struct hs *h1, *h2;
{

	if (down(h1))
		if (down(h2))
			return (tcmp(h1, h2));
		else
			return (1);
	else if (down(h2))
		return (-1);
	else
		return (h2->hs_wd->wd_loadav[0] - h1->hs_wd->wd_loadav[0]);
}

/*
 * Compare according to number of users.
 */
ucmp(h1, h2)
	struct hs *h1, *h2;
{

	if (down(h1))
		if (down(h2))
			return (tcmp(h1, h2));
		else
			return (1);
	else if (down(h2))
		return (-1);
	else
		return (h2->hs_nusers - h1->hs_nusers);
}

/*
 * Compare according to uptime.
 */
tcmp(h1, h2)
	struct hs *h1, *h2;
{
	long t1, t2;

	return (
		(down(h2) ? h2->hs_wd->wd_recvtime - now
			  : h2->hs_wd->wd_sendtime - h2->hs_wd->wd_boottime)
		-
		(down(h1) ? h1->hs_wd->wd_recvtime - now
			  : h1->hs_wd->wd_sendtime - h1->hs_wd->wd_boottime)
	);
}
