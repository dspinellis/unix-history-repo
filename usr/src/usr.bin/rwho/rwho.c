#ifndef lint
static char sccsid[] = "@(#)rwho.c	4.2 82/05/09";
#endif

#include <sys/param.h>
#include <stdio.h>
#include <dir.h>
#include <utmp.h>
#include "rwhod.h"

DIR	*etc;

struct	whod wd;
int	utmpcmp();
#define	NUSERS	1000
struct	myutmp {
	char	myhost[32];
	int	myidle;
	struct	utmp myutmp;
} myutmp[NUSERS];
int	nusers;

#define	WHDRSIZE	(sizeof (wd) - sizeof (wd.wd_we))

char	*ctime(), *strcpy();
int	now;
int	aflg;

main(argc, argv)
	int argc;
	char **argv;
{
	struct direct *dp;
	int cc, width;
	register struct whod *w = &wd;
	register struct whoent *we;
	register struct myutmp *mp;
	int f, n, i;

	argc--, argv++;
again:
	if (argc > 0 && !strcmp(argv[0], "-a")) {
		argc--, argv++;
		aflg++;
		goto again;
	}
	(void) time(&now);
	if (chdir("/etc") < 0) {
		perror("/etc");
		exit(1);
	}
	etc = opendir(".");
	if (etc == NULL) {
		perror("/etc");
		exit(1);
	}
	mp = myutmp;
	while (dp = readdir(etc)) {
		if (dp->d_ino == 0)
			continue;
		if (strncmp(dp->d_name, "whod.", 5))
			continue;
		f = open(dp->d_name, 0);
		if (f < 0)
			continue;
		cc = read(f, (char *)&wd, sizeof (struct whod));
		if (cc < WHDRSIZE) {
			(void) close(f);
			continue;
		}
		if (now - w->wd_recvtime > 5 * 60) {
			(void) close(f);
			continue;
		}
		cc -= WHDRSIZE;
		we = w->wd_we;
		for (n = cc / sizeof (struct whoent); n > 0; n--) {
			if (aflg == 0 && we->we_idle >= 60*60) {
				we++;
				continue;
			}
			if (nusers >= NUSERS) {
				printf("too many users\n");
				exit(1);
			}
			mp->myutmp = we->we_utmp; mp->myidle = we->we_idle;
			(void) strcpy(mp->myhost, w->wd_hostname);
			nusers++; we++; mp++;
		}
		(void) close(f);
	}
	qsort((char *)myutmp, nusers, sizeof (struct myutmp), utmpcmp);
	mp = myutmp;
	width = 0;
	for (i = 0; i < nusers; i++) {
		int j = strlen(mp->myhost) + 1 + strlen(mp->myutmp.ut_line);
		if (j > width)
			width = j;
		mp++;
	}
	mp = myutmp;
	for (i = 0; i < nusers; i++) {
		char buf[22];
		sprintf(buf, "%s:%s", mp->myhost, mp->myutmp.ut_line);
		printf("%-8.8s %-*s %.12s",
		   mp->myutmp.ut_name,
		   width,
		   buf,
		   ctime((time_t *)&mp->myutmp.ut_time)+4);
		mp->myidle /= 60;
		if (mp->myidle) {
			if (aflg) {
				if (mp->myidle >= 100*60)
					mp->myidle = 100*60 - 1;
				if (mp->myidle >= 60)
					printf(" %2d", mp->myidle / 60);
				else
					printf("   ");
			} else
				printf(" ");
			printf(":%02d", mp->myidle % 60);
		}
		printf("\n");
		mp++;
	}
	exit(0);
}

utmpcmp(u1, u2)
	struct myutmp *u1, *u2;
{
	int rc;

	rc = strncmp(u1->myutmp.ut_name, u2->myutmp.ut_name, 8);
	if (rc)
		return (rc);
	rc = strncmp(u1->myhost, u2->myhost, 8);
	if (rc)
		return (rc);
	return (strncmp(u1->myutmp.ut_line, u2->myutmp.ut_line, 8));
}
