/* Copyright (c) 1979 Regents of the University of California */
#include <retrofit.h>
#include <stdio.h>
#include <sgtty.h>
#include <sys/types.h>
#include <sys/stat.h>
/*
 * man - intelligent man command
 *
 * Author: Bill Joy UCB August 25, 1977
 *
 * Man is an intelligent man command which obviates the need to know
 * section numbers in the manual.  Also if the standard output is a teletype and
 * the option - is not given we pipe through "ssp and cr3" to eliminate piled
 * up blank lines.
 */
int	nocr3;
char	*strcpy();
char	*strcat();
int	section;
int	subsec;
int	troffit;

#define	eq(a,b)	(strcmp(a,b) == 0)

main(argc, argv)
	int argc;
	char *argv[];
{

	if (argc <= 1) {
		fprintf(stderr, "Usage: man [ section ] name ...\n");
		exit(1);
	}
	if (chdir("/usr/man") < 0) {
		fprintf(stderr, "Can't chdir to /usr/man.\n");
		exit(1);
	}
	argc--, argv++;
	while (argc > 0 && argv[0][0] == '-') {
		switch(argv[0][1]) {

		case 0:
			nocr3++;
			break;

		case 't':
			troffit++;
			break;
		}
		argc--, argv++;
	}
	if (troffit == 0 && nocr3 == 0 && !isatty(1))
		nocr3++;
	section = 0;
	do {
		if (eq(argv[0], "new") || eq(argv[0], "ucb")) {
			section = 'u';
			goto sectin;
		} else if (eq(argv[0], "public")) {
			section = 'p';
			goto sectin;
		} else if (eq(argv[0], "sccs")) {
			section = 's';
			goto sectin;
		} else if (argv[0][0] >= '0' && argv[0][0] <= '9' && (argv[0][1] == 0 || argv[0][2] == 0)) {
			section = argv[0][0];
			subsec = argv[0][1];
sectin:
			argc--, argv++;
			if (argc == 0) {
				fprintf(stderr, "But what do you want from section %s?\n", argv[-1]);
				exit(1);
			}
			continue;
		}
		manual(section, argv[0]);
		argc--, argv++;
	} while (argc > 0);
	exit(0);
}

manual(sec, name)
	char sec;
	char *name;
{
	char section = sec;
	char work[100];
	int ss;
	struct stat stbuf;
	int last;
	char *sp = "u16823457ps";

	strcpy(work, "manx/");
	strcat(work, name);
	strcat(work, ".x");
	last = strlen(work) - 1;
	if (section == '1') {
		sp = "u16p";
		section = 0;
	}
	if (section == 0) {
		ss = 0;
		for (section = *sp++; section; section = *sp++) {
			work[3] = section;
			work[last] = section;
			work[last+1] = 0;
			if (stat(work, &stbuf) >= 0)
				break;
			if (work[last] == '1' || work[last] == '3') {
				char *cp;
search:
				cp = work[last] == '1' ? "mcg" : "xmsf";
				while (*cp) {
					work[last+1] = *cp++;
					if (stat(work, &stbuf) >= 0) {
						ss = work[last+1];
						goto found;
					}
				}
				if (ss = 0)
					work[last+1] = 0;
			}
		}
		if (section == 0) {
			if (sec == 0)
				printf("No manual entry for %s.\n", name);
			else
				printf("No entry for %s in section %c of the manual.\n", name, sec);
			return;
		}
	} else {
		work[3] = section;
		work[last] = section;
		work[last+1] = subsec;
		if (stat(work, &stbuf) < 0) {
			if ((section == '1' || section == '3') && subsec == 0) {
				sp = "\0";
				goto search;
			}
			printf("No entry for %s in section %c", name, section);
			if (subsec)
				putchar(subsec);
			printf(" of the manual.\n");
			return;
		}
	}
found:
	if (troffit)
		troff(work);
	else
		nroff(work);
}

nroff(cp)
	char *cp;
{
	char cmdbuf[BUFSIZ];

	if (nocr3)
		sprintf(cmdbuf, "nroff -h -man %s", cp);
	else
		sprintf(cmdbuf, "nroff -h -man %s | /usr/ucb/ssp | /usr/ucb/cr3", cp);
	system(cmdbuf);
}

troff(cp)
	char *cp;
{
	char cmdbuf[BUFSIZ];

	sprintf(cmdbuf, "/usr/ucb/troff -t -man /usr/lib/tmac/tmac.vcat %s | /usr/ucb/vsort | /usr/ucb/vpr -t", cp);
	system(cmdbuf);
}

any(c, sp)
	register int c;
	register char *sp;
{
	register int d;

	while (d = *sp++)
		if (c == d)
			return (1);
	return (0);
}
