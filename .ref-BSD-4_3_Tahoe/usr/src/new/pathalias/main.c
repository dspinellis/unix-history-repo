/* pathalias -- by steve bellovin, as told to peter honeyman */
#ifndef lint
static char	*sccsid = "@(#)main.c	8.1 (down!honey) 86/01/19";
#endif

#define MAIN	/* for sccsid in header files */

#include "def.h"

#define USAGE "usage: %s [-vci] [-l localname] [-d deadlink] [-t tracelink] [-g file] [-s file]\n"

main(argc, argv) 
int	argc; 
char	*argv[];
{
	char	*locname = 0;
	int	c, errflg = 0;

	ProgName = argv[0];
	(void) allocation();	/* initialize data space monitoring */
	Cfile = "[deadlinks]";	/* for tracing dead links */
	while ((c = getopt(argc, argv, "cd:g:il:s:t:v")) != EOF)
		switch(c) {
		case 'c':	/* print cost info */
			Cflag++;
			break;
		case 'd':	/* dead host or link */
			deadlink(optarg);
			break;
		case 'g':	/* graph output file */
			Graphout = optarg;
			break;
		case 'i':	/* ignore case */
			Iflag++;
			break;
		case 'l':	/* local name */
			locname = optarg;
			break;
		case 's':	/* show shortest path tree */
			Linkout = optarg;
			break;
		case 't':	/* trace this link */
			if (tracelink(optarg) < 0) {
				fprintf(stderr, "%s: can trace only %d links\n", ProgName, NTRACE);
				exit(1);
			}
			Tflag = 1;
			break;
		case 'v':	/* verbose stderr, mixed blessing */
			Vflag++;
			break;
		default:
			errflg++;
		}

	if (errflg) {
		fprintf(stderr, USAGE, ProgName);
		exit(1);
	}
	argv += optind;		/* kludge for yywrap() */

	if (*argv) {
		Ifiles = argv;
		freopen("/dev/null", "r", stdin);
	}

	if (!locname) 
		locname = local();
	if (*locname == 0) {
		locname = "lostinspace";
		fprintf(stderr, "%s: using \"%s\" for local name\n",
				ProgName, locname);
	}

	Home = addnode(locname);	/* add home node */
	Home->n_cost = 0;		/* doesn't cost to get here */

	yyparse();			/* read in link info */

	if (Vflag > 1)
		hashanalyze();
	vprintf(stderr, "%d vertices, %d edges\n", Ncount, Lcount);
	vprintf(stderr, "allocation is %ldk after parsing\n", allocation());

	Cfile = "[backlinks]";	/* for tracing back links */
	Lineno = 0;

	mapit();			/* compute shortest path tree */
	vprintf(stderr, "allocation is %ldk after mapping\n", allocation());

	printit();			/* traverse tree and print paths */
	vprintf(stderr, "allocation is %ldk after printing\n", allocation());

	exit(0);
}

badmagic(n)
{
	if (n) {
		fprintf(stderr, "%s: cannot recover!\n", ProgName);
#ifdef DEBUG
		abort();
#else
		exit(n);
#endif
	}
}
