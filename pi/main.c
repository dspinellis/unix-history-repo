#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.0 August 1977
 */

#include "whoami"
#include "0.h"

/*
 * This program is described in detail in the "PI 1.0 Implementation Notes"
 *
 * This version of pi has been in use at Berkeley since May 1977
 * and is very stable, except for the syntactic error recovery which
 * has just been written.  Please report any problems with the error
 * recovery to the second author at the address given in the file
 * READ_ME.  The second author takes full responsibility for any bugs
 * in the syntactic error recovery.
 */

char	piusage[]	"pi [ -blnpstuw ] [ -i file ... ] name.p";
char	pixusage[]	"pix [ -blnpstuw ] [ -i file ... ] name.p [ arg ... ]";

char	*usageis	piusage;
char	*obj		"obj";
/*
 * Be careful changing errfile and howfile.
 * There are the "magic" constants 9 and 15 immediately below.
 */
char	*errfile	"/usr/lib/pi_strings";
char	*howfile	"/usr/lib/how_pi\0";

int	onintr();

extern	int ibuf[259];

extern	char *lastname;

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
		errfile =+ 9, howfile =+ 9;
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
	efil = open(errfile, 0);
	if (efil < 0)
		perror(errfile), pexit(NOSTART);
	filename = argv[0];
	if (!dotted(filename, 'p')) {
		Perror(filename, "Name must end in '.p'");
		pexit(NOSTART);
	}
	close(0);
	if (fopen(filename, ibuf) < 0)
		perror(filename), pexit(NOSTART);
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

/*
 * Buffer for putchar
 */
char	pcbuf[128];
char	*pcbp pcbuf;

/*
 * Line buffered putchar for pi.
 */
putchar(c)
	char c;
{

	*pcbp++ = c;
	if (c == '\n' || pcbp == &pcbuf[sizeof pcbuf-1]) {
		write(1, &pcbuf, pcbp-pcbuf);
		pcbp = pcbuf;
	}
}

char	ugh[]	"Fatal error in pi\n";
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

	if (seek(efil, seekpt, 0) || read(efil, buf, 256) <= 0)
		perror(errfile), pexit(DIED);
}

header()
{
	extern char version[];
	static char anyheaders;

	gettime();
	if (anyheaders && opt('n'))
		putchar('\f');
	anyheaders++;
	printf("UNIX Pascal PI -- Version 1.0 (%s)\n\n%s  %s\n\n", version, myctime(tvec), filename);
}
