#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#define	fstat	_fstat

/*
 * Gather -code file
 *
 * Code indicates the exit status.
 * File is the primary file name.
 * We don't deal with #include here.
 */

char	*pxerrs[] {
	"NORMAL",
	"CHR",
	"DIVCHK",
	"FDIVCHK",
	"HALT",
	"NILPTR",
	"PASTEOF",
	"SQRT",
	"STKNEMP",
	"SUBSCR",
	"REFINAF",
	"WRITE",
	"CREATE",
	"LN",
	"BADOP",
	"BADINUM",
	"GOTO",
	"CASE",
	"SEEK",
	"ALLOC",
	"OUTOFMEM",
	"CTTOT",
	"TOODIGITS",
	"MODCHK",
	"BADFNUM",
	"REMOVE",
	"CLOSE",
	"OPEN",
	"ARGV",
	"PACK",
	"UNPACK",
	"RANGE",
	"ASRT",
	"READIT",
	"WRITEIT",
	"BIGGIE",
	"STLIM",
	"STKOVFLO",
	"INTR",
	"FPOVFLO"
};

char	*pierrs[] {
	"AOK",
	"ERRS",
	"NOSTART",
	"DIED"
};

char	gatherdir[] =	"/d/gather";
#define	GATHERID	7

main(argc, argv)
	int argc;
	char *argv[];
{
	FILE *control;
	long curtime;
	register int c;
	char namebuf[10];
	struct stat stbuf;
	register char *cp;

	argc--, argv++;
	if (argc != 2 || argv[0][0] != '-' || !digit(argv[0][1]))
		exit(1);
	if (strcmp(argv[1], "px") != 0 && freopen(argv[1], "r", stdin) == NULL)
		exit(1);
	if (chdir(gatherdir) < 0)
		exit(1);
	if (getname(getuid(), namebuf) < 0)
		exit(1);
	if (chdir(namebuf) < 0)
		exit(1);
	control = fopen("control", "a");
	if (control == NULL)
		exit(1);
	time(&curtime);
	fstat(fileno(control), &stbuf);
	fprintf(control, "%07D.p\t%s\t%s\t%s", stbuf.st_size,
	    decomp(argv[0], argv[1]), argv[1], ctime(&curtime));
	fflush(control);
	fclose(control);
	sprintf(namebuf, "%07D.p", stbuf.st_size);
	if (strcmp(argv[0], "-0") == 0 || strcmp(argv[1], "px") == 0 ||
		strcmp(argv[0], "-2") == 0)
		exit(1);
	setuid(GATHERID);
	if (freopen(namebuf, "w", stdout) == NULL)
		exit(1);
	for (;;) {
		c = getc(stdin);
		if (c < 0)
			exit(0);
		putchar(c);
		if (ferror(stdout)) {
			unlink(namebuf);
			exit(1);
		}
	}
}

any(c, cp)
	int c;
	char *cp;
{

	while (*cp)
		if (c == *cp++)
			return (1);
	return (0);
}

digit(c)
	char c;
{

	return (c >= '0' && c <= '9');
}

decomp(cp, dp)
	register char *cp;
	char *dp;
{
	register int i;

	if (*cp++ != '-')
		return (--cp);
	i = 0;
	while (*cp)
		i = i * 10 + *cp++ - '0';
	if (strcmp(dp, "px") == 0) {
		if (i < 0 || i >= ((sizeof pxerrs) / (sizeof pxerrs[0])))
			return ("?????");
		return (pxerrs[i]);
	} else {
		if (i < 0 || i >= ((sizeof pierrs) / (sizeof pierrs[0])))
			return ("?????");
		return (pierrs[i]);
	}
}
