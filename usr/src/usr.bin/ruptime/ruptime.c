#ifndef lint
static char sccsid[] = "@(#)ruptime.c	4.4 82/05/09";
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
int	hscmp();

#define	WHDRSIZE	(sizeof (awhod) - sizeof (awhod.wd_we))

char	*interval();
int	now;
char	*malloc(), *sprintf();
int	aflg;

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

	time(&t);
	argc--, argv++;
again:
	if (!strcmp(*argv, "-a")) {
		aflg++;
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
	qsort((char *)hs, nhosts, sizeof (hs[0]), hscmp);
	(void) time(&now);
	if (nhosts == 0) {
		printf("no hosts!?!\n");
		exit(1);
	}
	for (i = 0; i < nhosts; i++) {
		hsp = &hs[i];
		if (now - hsp->hs_wd->wd_recvtime > 5 * 60) {
			printf("%-8.8s%s\n", hsp->hs_wd->wd_hostname,
			    interval(now - hsp->hs_wd->wd_recvtime, "down"));
			continue;
		}
		printf("%-8.8s%s,  %4d user%s  load %*.2f, %*.2f, %*.2f\n",
		    hsp->hs_wd->wd_hostname,
		    interval(hsp->hs_wd->wd_sendtime -
			hsp->hs_wd->wd_bootime, "  up"),
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
