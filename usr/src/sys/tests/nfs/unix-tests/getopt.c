#ifndef lint
static char NFSTestID[] = "@(#)getopt.c	1.1 Lachman ONC Test Suite source";
#endif /* lint */
/*
 * Here's something you've all been waiting for:  the AT&T public domain
 * source for getopt(3).  It is the code which was given out at the 1985
 * UNIFORUM conference in Dallas.  I obtained it by electronic mail
 * directly from AT&T.  The people there assure me that it is indeed
 * in the public domain.
 * 
 * There is no manual page.  That is because the one they gave out at
 * UNIFORUM was slightly different from the current System V Release 2
 * manual page.  The difference apparently involved a note about the
 * famous rules 5 and 6, recommending using white space between an option
 * and its first argument, and not grouping options that have arguments.
 * Getopt itself is currently lenient about both of these things White
 * space is allowed, but not mandatory, and the last option in a group can
 * have an argument.  That particular version of the man page evidently
 * has no official existence, and my source at AT&T did not send a copy.
 * The current SVR2 man page reflects the actual behavor of this getopt.
 * However, I am not about to post a copy of anything licensed by AT&T.
 */


/*LINTLIBRARY*/
#define NULL	0
#define EOF	(-1)
#define ERR(s, c)	if(opterr){\
	extern int write();\
	char errbuf[2];\
	errbuf[0] = c; errbuf[1] = '\n';\
	(void) write(2, argv[0], (unsigned)strlen(argv[0]));\
	(void) write(2, s, (unsigned)strlen(s));\
	(void) write(2, errbuf, 2);}

#ifdef SVR3
extern char *strchr();
#else
extern char *index();
#endif

int	opterr = 1;
int	optind = 1;
int	optopt;
char	*optarg;

int
getopt(argc, argv, opts)
int	argc;
char	**argv, *opts;
{
	static int sp = 1;
	register int c;
	register char *cp;

	if(sp == 1)
		if(optind >= argc ||
		   argv[optind][0] != '-' || argv[optind][1] == '\0')
			return(EOF);
		else if(strcmp(argv[optind], "--") == NULL) {
			optind++;
			return(EOF);
		}
	optopt = c = argv[optind][sp];
#ifdef SVR3
	if(c == ':' || (cp=strchr(opts, c)) == NULL) {
#else
	if(c == ':' || (cp=index(opts, c)) == NULL) {
#endif
		ERR(": illegal option -- ", c);
		if(argv[optind][++sp] == '\0') {
			optind++;
			sp = 1;
		}
		return('?');
	}
	if(*++cp == ':') {
		if(argv[optind][sp+1] != '\0')
			optarg = &argv[optind++][sp+1];
		else if(++optind >= argc) {
			ERR(": option requires an argument -- ", c);
			sp = 1;
			return('?');
		} else
			optarg = argv[optind++];
		sp = 1;
	} else {
		if(argv[optind][++sp] == '\0') {
			sp = 1;
			optind++;
		}
		optarg = NULL;
	}
	return(c);
}

#include	<stdio.h>

main(ac, av)
char	**av;
{
	register int	i;
	int	first = 1;
	int	error = 0;
	char	buf[BUFSIZ];
	char	line[BUFSIZ];
	extern char	*optarg;
	extern int	optind;

	if (ac == 1) {
		fprintf(stderr, "usage:  getopt legal-args $*\n");
		exit(2);
	}

	line[0] = '\0';
	while ((i = getopt(ac - 1, &av[1], av[1])) != EOF) {
		if (i == '?')
			exit(2);

		if (first) {
			first = 0;
			sprintf(buf, "-%c", i & 0xff);
		}
		else
			sprintf(buf, " -%c", i & 0xff);
		strcat(line, buf);
		if (optarg) {
			sprintf(buf, " %s", optarg);
			strcat(line, buf);
		}
	}

	if (first)
		strcat(line, "--");
	else
		strcat(line, " --");

	optind++;
	for (; optind < ac; optind++) {
		sprintf(buf, " %s", av[optind]);
		strcat(line, buf);
	}
	printf("%s\n", line);
	exit(0);
}
