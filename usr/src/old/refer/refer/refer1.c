/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)refer1.c	4.6 (Berkeley) %G%";
#endif /* not lint */

#include <signal.h>
#include "refer..c"
#include "pathnames.h"

main(argc,argv)		/* process command-line arguments */
char *argv[];
{
	char line[BUFSIZ], *s;
	int nodeflt = 0;

	signals();
	while (argv[1][0] == '-') {
		switch(argv[1][1]) {
		case 'e':
			endpush++; 
			break;
		case 's':
			sort++;
			endpush = 1;
			if (argv[1][2])
				keystr = argv[1]+2;
			break;
		case 'l': 
			labels++;
			s = argv[1]+2;
			nmlen = atoi(s);
			while (*s)
				if (*s++ == ',')
					break;
			dtlen = atoi(s);
			break;
		case 'k':
			keywant = (argv[1][2] ? argv[1][2] : 'L');
			labels++;
			break;
		case 'n':
			nodeflt = 1;
			break;
		case 'p':
			argc--; 
			argv++;
			*search++ = argv[1];
			if (search-rdata > NSERCH)
				err("too many -p options (%d)", NSERCH);
			break;
		case 'a':
			authrev = atoi(argv[1]+2);
			if (authrev<=0)
				authrev = 1000;
			break;
		case 'b':
			bare = (argv[1][2] == '1') ? 1 : 2;
			break;
		case 'c':
			smallcaps = argv[1]+2;
			break;
		case 'f':
			refnum = atoi(argv[1]+2) - 1;
			break;
		case 'B':
			biblio++;
			bare = 2;
			if (argv[1][2])
				convert = argv[1]+2;
			break;
		case 'S':
			science++;
			labels = 1;
			break;
		case 'P':
			postpunct++;
			break;
		}
		argc--; 
		argv++;
	}
	if (getenv("REFER") != NULL)
		*search++ = getenv("REFER");
	else if (nodeflt == 0)
		*search++ = _PATH_IND;
	if (!labels) {
		sprintf(ofile, "%s/rj%db", _PATH_TMP, getpid());
		ftemp = fopen(ofile, "w");
		if (ftemp == NULL) {
			fprintf(stderr, "Can't open scratch file\n");
			exit(1);
		}
	}
	if (endpush) {
		sprintf(tfile, "%s/rj%da", _PATH_TMP, getpid());
		fo = fopen(tfile, "w");
		if (fo == NULL) {
			fo = ftemp;
			fprintf(stderr, "Can't open scratch file");
		}
		sep = 002; /* separate records without confusing sort..*/
	} else 
		fo = ftemp;
	do {
		if (argc > 1) {
			fclose(in);
			Iline = 0;
			in = fopen(Ifile = argv[1], "r");
			argc--; 
			argv++;
			if (in == NULL) {
				err("Can't read %s", Ifile);
				continue;
			}
		}
		while (input(line)) {
			Iline++;
			if (biblio && *line == '\n')
				doref(line);
			else if (biblio && Iline == 1 && *line == '%')
				doref(line);
			else if (!prefix(".[", line))
				output(line);
			else
				doref(line);
		}
	} while (argc > 1);

	if (endpush && fo != NULL)
		dumpold();
	output("");
	if (!labels)
		recopy(ofile);
	clfgrep();
	cleanup();
	exit(0);
}

signals()
{
	void intr();

	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, intr);
	signal(SIGHUP, intr);
	signal(SIGPIPE, intr);
	signal(SIGTERM, intr);
}

void
intr()
{
	signal(SIGINT, SIG_IGN);
	cleanup();
	exit(1);
}

cleanup()
{
	if (tfile[0])
		unlink(tfile);
	if (gfile[0])
		unlink(gfile);
	if (ofile[0])
		unlink(ofile);
	if (hidenam[0])
		unlink(hidenam);
}
