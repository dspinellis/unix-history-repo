#ifndef lint
static char sccsid[] = "@(#)repquota.c	4.1 (Melbourne) %G%";
#endif

/*
 * Quota report
 */
#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include <sys/param.h>
#define	QUOTA
#include <sys/quota.h>

#define	NUID	3000

struct dqblk dq[NUID];

char	*dn[NUID];
int	nuids;

main(argc, argv)
	char **argv;
{
	register struct passwd *lp;
	register n;
	register i;
	register FILE *qf;
	char *copy();
	struct passwd *getpwent();

	argc--;
	if ((qf = fopen(*++argv, "r")) == NULL) {
		fprintf(stderr, "Can't open %s\n", *argv);
		exit(1);
	}
	argc--, argv++;
	nuids = fread(dq, sizeof(struct dqblk), NUID, qf);
	fclose(qf);
	while ((lp = getpwent()) != (struct passwd *)0) {
		n = lp->pw_uid;
		if (n >= NUID)
			continue;
		if (dn[n])
			continue;
		dn[n] = copy(lp->pw_name);
	}

	for (n = 0; n < nuids; n++) {
		if (argc > 0) {
			for (i = 0; i < argc; i++) {
				register char *p;

				for (p = argv[i]; *p && isdigit(*p); p++)
					;
				if (*p)
					continue;
				if (n == atoi(argv[i]))
					goto rep;
			}
			if (!dn[n])
				continue;
			for (i = 0; i < argc; i++)
				if (strcmp(argv[i], dn[n]) == 0)
					break;
			if (i >= argc)
				continue;
		} else if (dq[n].dqb_curinodes == 0 && dq[n].dqb_curblocks == 0)
			continue;
	rep:
		if (dn[n])
			printf("%-10s", dn[n]);
		else
			printf("#%-9d", n);

		printf("%c%c %5d %5d %5d %5d %5d %5d %5d %5d\n"
			, dq[n].dqb_bsoftlimit && dq[n].dqb_curblocks >= dq[n].dqb_bsoftlimit
				? '+' : '-'
			, dq[n].dqb_isoftlimit && dq[n].dqb_curinodes >= dq[n].dqb_isoftlimit
				? '+' : '-'
			, dq[n].dqb_curblocks
			, dq[n].dqb_bsoftlimit
			, dq[n].dqb_bhardlimit
			, dq[n].dqb_bwarn
			, dq[n].dqb_curinodes
			, dq[n].dqb_isoftlimit
			, dq[n].dqb_ihardlimit
			, dq[n].dqb_iwarn
		);
	}
	exit(0);
}

char *
copy(s)
	char *s;
{
	register char *p;
	register n;
	char *malloc();

	for(n=0; s[n]; n++)
		;
	p = malloc((unsigned)n+1);
	for(n=0; p[n] = s[n]; n++)
		;
	return(p);
}
