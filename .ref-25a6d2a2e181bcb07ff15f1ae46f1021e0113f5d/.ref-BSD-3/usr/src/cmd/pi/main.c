/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.2 November 1978
 */

#include "whoami"
#include "0.h"
#include "yy.h"

/*
 * This version of pi has been in use at Berkeley since May 1977
 * and is very stable, except for the syntactic error recovery which
 * has just been written.  Please report any problems with the error
 * recovery to the second author at the address given in the file
 * READ_ME.  The second author takes full responsibility for any bugs
 * in the syntactic error recovery.
 */

char	piusage[]	= "pi [ -blnpstuw ] [ -i file ... ] name.p";
char	pixusage[]	= "pix [ -blnpstuw ] [ -i file ... ] name.p [ arg ... ]";

char	*usageis	= piusage;
char	*obj		= "obj";

#ifdef PPC
    char	*ppcname = "ppc.p1";
#   ifdef DEBUG
	char	*ppcdname = "ppcd.p1";
#   endif
#endif
#ifdef PTREE
    char	*pTreeName = "pi.pTree";
#endif

/*
 * Be careful changing errfile and howfile.
 * There are the "magic" constants 9 and 15 immediately below.
 */
char	*errfile	= "/usr/lib/pi1.2strings";
char	*howfile	= "/usr/lib/how_pi\0";

int	onintr();

extern	char *lastname;

FILE	*ibuf;

/*
 * these are made real variables
 * so they can be changed
 * if you are compiling on a smaller machine
 */
double	MAXINT	=  2147483647.;
double	MININT	= -2147483648.;

/*
 * Main program for pi.
 * Process options, then call yymain
 * to do all the real work.
 */
main(argc, argv)
	int argc;
	char *argv[];
{
	register char *cp;
	register c;
	int i;

	if (argv[0][0] == 'a')
		errfile += 9, howfile += 9;
	if (argv[0][0] == '-' && argv[0][1] == 'o') {
		obj = &argv[0][2];
		usageis = pixusage;
		howfile[15] = 'x';
		ofil = 3;
	} else {
		ofil = creat(obj, 0755);
		if (ofil < 0) {
			perror(obj);
			pexit(NOSTART);
		}
	}
	argv++, argc--;
	if (argc == 0) {
		i = fork();
		if (i == -1)
			goto usage;
		if (i == 0) {
			execl("/bin/cat", "cat", howfile, 0);
			goto usage;
		}
		while (wait(&i) != -1)
			continue;
		pexit(NOSTART);
	}
	opt('p') = opt('t') = opt('b') = 1;
	while (argc > 0) {
		cp = argv[0];
		if (*cp++ != '-')
			break;
		while (c = *cp++) switch (c) {
#ifdef DEBUG
			case 'c':
			case 'r':
			case 'y':
				togopt(c);
				continue;
			case 'C':
				yycosts();
				pexit(NOSTART);
			case 'A':
				testtrace++;
			case 'F':
				fulltrace++;
			case 'E':
				errtrace++;
				opt('r')++;
				continue;
			case 'U':
				yyunique = 0;
				continue;
#			ifdef PPC
			    case 'P':
				ppcdebug++;
				continue;
#			endif
#endif
			case 'b':
				opt('b') = 2;
				continue;
			case 'i':
				pflist = argv + 1;
				pflstc = 0;
				while (argc > 1) {
					if (dotted(argv[1], 'p'))
						break;
					pflstc++, argc--, argv++;
				}
				if (pflstc == 0)
					goto usage;
				continue;
			case 'l':
			case 'n':
			case 'p':
			case 's':
			case 't':
			case 'u':
			case 'w':
				togopt(c);
				continue;
			case 'z':
				monflg++;
				continue;
			default:
usage:
				Perror( "Usage", usageis);
				pexit(NOSTART);
		}
		argc--, argv++;
	}
	if (argc != 1)
		goto usage;
	efil = open ( errfile, 0 );
	if ( efil < 0 )
		perror(errfile), pexit(NOSTART);
	filename = argv[0];
	if (!dotted(filename, 'p')) {
		Perror(filename, "Name must end in '.p'");
		pexit(NOSTART);
	}
	close(0);
	if ( ( ibuf = fopen ( filename , "r" ) ) == NULL )
		perror(filename), pexit(NOSTART);
	ibp = ibuf;
#	ifdef PPC
	    if ( ( ppcstream = fopen( ppcname , "w" ) ) == NULL ) {
		perror( ppcname );
		pexit( NOSTART );
	    }
#	    ifdef DEBUG
		if ( ppcdebug ) {
		    if ( ( ppcdstream = fopen( ppcdname , "w" ) ) == NULL ) {
			perror( ppcdname );
			pexit( NOSTART );
		    }
		}
#	    endif
	    putprintf( "#		compilation of %s" , filename );
#	endif
#	ifdef PTREE
#	    define	MAXpPAGES	16
	    if ( ! pCreate( pTreeName , MAXpPAGES ) ) {
		perror( pTreeName );
		pexit( NOSTART );
	    }
#	endif
	if ((signal(2, 1) & 01) == 0)
		signal(2, onintr);
	if (opt('l')) {
		opt('n')++;
		yysetfile(filename);
		opt('n')--;
	} else
		lastname = filename;
	yymain();
	/* No return */
}

pchr(c)
	char c;
{

	putc ( c , stdout );
}

char	ugh[]	= "Fatal error in pi\n";
/*
 * Exit from the Pascal system.
 * We throw in an ungraceful termination
 * message if c > 1 indicating a severe
 * error such as running out of memory
 * or an internal inconsistency.
 */
pexit(c)
	int c;
{

	if (opt('l') && c != DIED && c != NOSTART)
		while (getline() != -1)
			continue;
	yyflush();
	switch (c) {
		case DIED:
			write(2, ugh, sizeof ugh);
		case NOSTART:
		case ERRS:
			if (ofil > 0)
				unlink(obj);
			break;
		case AOK:
			pflush();
			break;
	}
	/*
	 *	this to gather statistics on programs being compiled
	 *	taken 20 june 79 	... peter
	 *
	 *  if (fork() == 0) {
	 *  	char *cp = "-0";
	 *  	cp[1] += c;
	 *  	execl("/usr/lib/gather", "gather", cp, filename, 0);
	 *  	exit(1);
	 *  }
	 */
#	ifdef PTREE
	    pFinish();
#	endif
	exit(c);
}

onintr()
{

	signal(2, 1);
	pexit(NOSTART);
}

/*
 * Get an error message from the error message file
 */
geterr(seekpt, buf)
	int seekpt;
	char *buf;
{

	lseek(efil, (long) seekpt, 0);
	if (read(efil, buf, 256) <= 0)
		perror(errfile), pexit(DIED);
}

header()
{
	extern char version[];
	static char anyheaders;

	gettime( filename );
	if (anyheaders && opt('n'))
		putc( '\f' , stdout );
	anyheaders++;
	printf("Berkeley Pascal PI -- Version 1.2 (%s)\n\n%s  %s\n\n",
		version, myctime(&tvec), filename);
}
