#include <ctype.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

/*
 * Vfontedpr.
 *
 * Bill Joy, Apr. 1979.
 * (made somewhat table driven - Dave Presotto 11/17/80)
 *
 * To add a new language:
 *	1) add a new keyword table
 *	2) add a new entry to "struct langdef ld[]" to
 *	   describe comments, strings, etc.
 *	3) add two routines to recognize start and end of 
 *	   procedures
 *
 */
#define NOLANG -1		/* indicates no language chosen */
#define C 0
#define PASCAL 1
#define MODEL 2
#define NLANG 4			/* total number of languages */

#define STRLEN 10		/* length of strings introducing things */
#define PNAMELEN 40		/* length of a function/procedure name */

/* routines used by the different languages */

int	cprbegin();
int	cprend();
int	iprbegin();
int	iprend();
int	pprbegin();
int	pprend();
int	mprbegin();
int	mprend();

/* keywords for the model language */

char	*mkw[] = {
	"abs",
	"and",
	"array",
	"beginproc",
	"boolean",
	"by",
	"case",
	"cdnl",
	"char",
	"copied",
	"dispose",
	"div",
	"do",
	"dynamic",
	"else",
	"elsif",
	"end",
	"endproc",
	"entry",
	"external",
	"f",
	"FALSE",
	"false",
	"fi",
	"file",
	"for",
	"formal",
	"fortran",
	"global",
	"if",
	"in",
	"include",
	"inline",
	"is",
	"lbnd",
	"max",
	"min",
	"mod",
	"new",
	"NIL",
	"nil",
	"noresult",
	"not",
	"notin",
	"od",
	"of",
	"or",
	"procedure",
	"public",
	"read",
	"readln",
	"readonly",
	"record",
	"recursive",
	"rem",
	"rep",
	"repeat",
	"result",
	"return",
	"set",
	"space",
	"string",
	"subscript",
	"such",
	"then",
	"TRUE",
	"true",
	"type",
	"ubnd",
	"union",
	"until",
	"varies",
	"while",
	"width",
	"write",
	"writeln",
	0,
};

/* keywords for the pascal language */

char	*pkw[] = {
	"and",
	"array",
	"assert",
	"begin",
	"case",
	"const",
	"div",
	"do",
	"downto",
	"else",
	"end",
	"file",
	"for",
	"forward",
	"function",
	"goto",
	"if",
	"in",
	"label",
	"mod",
	"nil",
	"not",
	"of",
	"or",
	"packed",
	"procedure",
	"program",
	"record",
	"repeat",
	"set",
	"then",
	"to",
	"type",
	"until",
	"var",
	"while",
	"with",
	"oct",
	"hex",
	"external",
	0,
};

/* keywords for the C language */

char	*ckw[] = {
	"asm",
	"auto",
	"break",
	"case",
	"char",
	"continue",
	"default",
	"do",
	"double",
	"else",
	"enum",
	"extern",
	"float",
	"for",
	"fortran",
	"goto",
	"if",
	"int",
	"long",
	"register",
	"return",
	"short",
	"sizeof",
	"static",
	"struct",
	"switch",
	"typedef",
	"union",
	"unsigned",
	"while",
	"#define",
	"#else",
	"#endif",
	"#if",
	"#ifdef",
	"#ifndef",
	"#include",
	"#undef",
	"#",
	"define",
	"else",
	"endif",
	"if",
	"ifdef",
	"ifndef",
	"include",
	"undef",
	0,
};

/* keywords for the ISP language */

char	*ikw[] = {
	"and",
	"begin",
	"decode",
	"define",
	"end",
	"eql",
	"eqv",
	"geq",
	"gtr",
	"if",
	"leave",
	"leq",
	"lss",
	"mod",
	"neq",
	"next",
	"not",
	"or",
	"otherwise",
	"repeat",
	"restart",
	"resume",
	"sr0",
	"sr1",
	"srd",
	"srr",
	"sl0",
	"sl1",
	"sld",
	"slr",
	"tst",
	"xor",
	0,
};

/*
 * the following structure defines a language
 */

struct langdef {
	char	*option;	/* its option switch */
	char	**kwd;		/* address of its keyword table */
	int	(*isproc)();	/* test for procedure begin */
	int	(*ispend)();	/* test for procedure end */
	char	*combeg;	/* string introducing a comment */
	char	*comend;	/* string ending a comment */
	char	*comout;	/* string output in place of combeg string */
	char	*acmbeg;	/* alternate comment start */
	char	*acmend;	/* alternate comment end */
	char	*acmout;	/* alternate comment start preface */
	char    strdel;		/* delimiter for string constant */
	char	chrdel;		/* delimiter for character constant */
	int	onelncom;	/* comments do not continue on next line */
	int	onelnstr;	/* string constants do not continue on next */
	int	onecase;	/* upper and lower case are equivalent */
};

struct langdef ld[] = {
	/* the C language */
	"-c",
	ckw,		/* kwd */
	cprbegin,	/* isproc */
	cprend,		/* ispend */
	"/*",		/* combeg */
	"*/",		/* comend */
	"\\*(/*",	/* comout */
	0,		/* acmbeg */
	0,		/* acmend */
	0,		/* acmout */
	'"',		/* strdel */
	'\'',		/* chrdel */
	0,		/* onelncom */
	0,		/* onelnstr */
	0,		/* onecase */

	/* the ISP language */
	"-i",
	ikw,		/* kwd */
	iprbegin,	/* isproc */
	iprend,		/* ispend */
	"!",		/* combeg */
	0,		/* comend */
	"!",		/* comout */
	0,		/* acmbeg */
	0,		/* acmend */
	0,		/* acmout */
	0,		/* strdel */
	0,		/* chrdel */
	1,		/* onelncom */
	1,		/* onelnstr */
	1,		/* onecase */

	/* the pascal language */
	"-p",
	pkw,		/* kwd */
	pprbegin,	/* isproc */
	pprend,		/* ispend */
	"{",		/* combeg */
	"}",		/* comend */
	"{",		/* comout */
	"(*",		/* acmbeg */
	"*)",		/* acmend */
	"(*",		/* acmout */
	'\'',		/* strdel */
	0,		/* chrdel */
	0,		/* onelncom */
	0,		/* onelnstr */
	0,		/* onecase */

	/* the model language */
	"-m",
	mkw,		/* kwd */
	mprbegin,	/* isproc */
	mprend,		/* ispend */
	"$",		/* combeg */
	"$",		/* comend */
	"$",		/* comout */
	0,		/* acmbeg */
	0,		/* acmend */
	0,		/* acmout */
	'"',		/* strdel */
	'\'',		/* chrdel */
	1,		/* onelncom */
	0,		/* onelnstr */
	0,		/* onecase */
};

char	*ctime();
int	incomm;			/* in a comment of the primary type */
int	inacomm;		/* in the alternate type of comment */
int	instr;			/* in a string constant */
int	nokeyw;
int	index;
int	margin;

char	*comcol;		/* character position comment starts in */
int	language = NOLANG;	/* the language indicator */
char	**keywds;		/* keyword table address */
int	(*isprbeg)();		/* test for beginning of procedure */
int	(*isprend)();		/* test for end of procedure */
char	*cstart;		/* start of comment string */
char	*cstop;			/* end of comment string */
char	*cout;			/* string to substitute for cstart */
int	lcstart;		/* length of comment string starter */
int	lcstop;			/* length of comment string terminator */
char	*acstart;		/* start of comment string */
char	*acstop;		/* end of comment string */
char	*acout;			/* string to substitute for cstart */
int	lacstart;		/* length of comment string starter */
int	lacstop;		/* length of comment string terminator */
char	sdelim;			/* string constant delimiter */
char	cdelim;			/* character constant delimiter */
int	com1line;		/* one line comments */
int	str1line;		/* one line strings */
int	upeqlow;		/* upper and lower case equivalent */
char	pname[PNAMELEN];

#define	ps(x)	printf("%s", x)

main(argc, argv)
	int argc;
	char *argv[];
{
	int lineno;
	char *fname = "";
	struct stat stbuf;
	char buf[BUFSIZ];
	int needbp = 0;

	argc--, argv++;
	do {
		char *cp;
		int i;

		if (argc > 0) {
			if (!strcmp(argv[0], "-h")) {
				if (argc == 1) {
					printf("'ds =H\n");
					argc = 0;
					goto rest;
				}
				printf("'ds =H %s\n", argv[1]);
				argc -= 2;
				argv += 2;
				if (argc > 0)
					continue;
				goto rest;
			}
			if (!strcmp(argv[0], "-x")) {
				index++;
				argv[0] = "-n";
			}
			if (!strcmp(argv[0], "-n")) {
				nokeyw++;
				argc--, argv++;
				continue;
			}
			if (!strncmp(argv[0], "-s", 2)) {
				i = 0;
				cp = argv[0] + 2;
				while (*cp)
					i = i * 10 + (*cp++ - '0');
				printf("'ps %d\n'vs %d\n", i, i+1);
				argc--, argv++;
				continue;
			}
			for (i = 0; i < NLANG; i++)
				if (!strcmp(argv[0], ld[i].option)) {
					language = i;
					break;
				}
			if (i != NLANG) {
				argc--, argv++;
				continue;
			}
			if (freopen(argv[0], "r", stdin) == NULL) {
				perror(argv[0]);
				exit(1);
			}
			if (index)
				printf("'ta 4i 4.25i 5.5iR\n'in .5i\n");
			fname = argv[0];
			argc--, argv++;
		}
rest:
		if (language == NOLANG)
			language = C;		/* C is the default */

		/* initialize for the appropriate language */
		/* This is done because subscripting is too */
		/* damned slow to be done every reference */

		keywds = ld[language].kwd;
		isprbeg = ld[language].isproc;
		isprend = ld[language].ispend;

		cstart = ld[language].combeg;
		cstop = ld[language].comend;
		cout = ld[language].comout;
		lcstart = strlen (cstart);
		lcstop = strlen (cstop);

		acstart = ld[language].acmbeg;
		acstop = ld[language].acmend;
		acout = ld[language].acmout;
		lacstart = strlen (acstart);
		lacstop = strlen (acstop);

		sdelim = ld[language].strdel;
		cdelim = ld[language].chrdel;
		com1line = ld[language].onelncom;
		str1line = ld[language].onelnstr;
		upeqlow = ld[language].onecase;
		pname[0] = 0;

		/* initialize the program */

		incomm = 0;
		inacomm = 0;
		instr = 0;
		ps("'-F\n");
		printf(".ds =F %s\n", fname);
		fstat(fileno(stdin), &stbuf);
		cp = ctime(&stbuf.st_mtime);
		cp[16] = '\0';
		cp[24] = '\0';
		printf(".ds =M %s %s\n", cp+4, cp+20);
		if (needbp) {
			needbp = 0;
			printf(".()\n");
			printf(".bp\n");
		}
		while (fgets(buf, sizeof buf, stdin) != NULL) {
			if (buf[0] == '\f' && buf[1] == '\n') {
				printf(".bp\n");
				continue;
			}
			if (com1line && (incomm || inacomm)) {
				incomm = 0;
				inacomm = 0;
				ps("\\c\n'-C\n");
			}
			if (str1line)
				instr = 0;
			comcol = NULL;
			putScp(buf);
			if (buf[strlen(buf) - 2] != '\\')
				instr = 0;
			margin = 0;
		}
		needbp = 1;
	} while (argc > 0);
	exit(0);
}

#define isidchr(c) (isalnum(c) || (c) == '_')

putScp(os)
	char *os;
{
	register char *s = os;
	register int i;
	int xfld = 0;

	if (nokeyw || incomm || inacomm || instr)
		goto skip;
	if ((*isprbeg)(s)) {
		ps("'FN ");
		ps(pname);
		ps("\n");
	} else if ((*isprend)(s))
		ps("'-F\n");
skip:
	while (*s) {
		if (*s == '\f') {
			ps("\n.bp\n");
			s++;
			continue;
		}
		if (index) {
			if (*s == ' ' || *s == '\t') {
				if (xfld == 0)	
					printf("");
				printf("\t");
				xfld = 1;
				while (*s == ' ' || *s == '\t')
					s++;
				continue;
			}
		}

		/* 
		 *  for the following "really" hacked code, I
		 *  apologize. Creeping featurism got me. DLP
		 */

		/* check for the start of a string */
		if (!nokeyw && !(incomm || inacomm) && *s == sdelim) {
			if (instr) {
				if (s[-1] != '\\')
					instr = 0;
			} else
				if (s[-1] != cdelim)
					instr = 1;
		}

		/* check for the end of a comment */
		if (incomm && lcstop != 0 && comcol != s-1 && s - os >= lcstop 
		    && !strncmp(cstop, s - lcstop, lcstop)) {
			incomm = 0;
			ps("\\c\n'-C\n");

		/* check for the end of a comment of the alternate type */
		} else if (inacomm && lacstop != 0 && comcol != s-1 
		    && s - os >= lacstop 
		    && !strncmp(acstop, s - lacstop, lacstop)) {
			inacomm = 0;
			ps("\\c\n'-C\n");

		/* check for the start of a comment */
		} else if (!instr && !nokeyw && !(incomm || inacomm)
		    && lcstart != 0
		    && !strncmp(cstart, s, lcstart)) {
			comcol = s;
			incomm = 1;
			if (s != os)
				ps("\\c");
			ps("\\c\n'+C\n");
			margin = width(os, s);
			ps(cout);
			s += lcstart;
			continue;

		/* check for the start of a comment of the alternate type */
		} else if (!instr && !nokeyw && !(incomm || inacomm)
		    && lacstart != 0
		    && !strncmp(acstart, s, lacstart)) {
			comcol = s;
			inacomm = 1;
			if (s != os)
				ps("\\c");
			ps("\\c\n'+C\n");
			margin = width(os, s);
			ps(acout);
			s += lacstart;
			continue;
		}

		/* take care of nice tab stops */
		if (*s == '\t') {
			while (*s == '\t')
				s++;
			i = tabs(os, s) - margin / 8;
			printf("\\h'|%dn'", i * 10 + 1 - margin % 8);
			continue;
		}
/*
		if (*s == '-' && s[1] == '>') {
			s += 2;
			ps("\\(->");
			continue;
		}
*/
		if (!incomm && !nokeyw && !instr && (*s == '#' || isalpha(*s)) 
		    && (s == os || !isidchr(s[-1]))) {
			i = iskw(s);
			if (i > 0) {
				ps("\\*(+K");
				do 
					putcp(*s++);
				while (--i > 0);
				ps("\\*(-K");
				continue;
			}
		}
		putcp(*s++);
	}
}

tabs(s, os)
	char *s, *os;
{

	return (width(s, os) / 8);
}

width(s, os)
	register char *s, *os;
{
	register int i = 0;

	while (s < os) {
		if (*s == '\t') {
			i = (i + 8) &~ 7;
			s++;
			continue;
		}
		if (*s < ' ')
			i += 2;
		else
			i++;
		s++;
	}
	return (i);
}

putcp(c)
	register int c;
{

	switch(c) {

	case '{':
		ps("\\*(+K{\\*(-K");
		break;

	case '}':
		ps("\\*(+K}\\*(-K");
		break;

	case '\\':
		ps("\\e");
		break;

	case '_':
		ps("\\*_");
		break;

	case '-':
		ps("\\*-");
		break;

	case '`':
		ps("\\`");
		break;

	case '\'':
		ps("\\'");
		break;

	case '.':
		ps("\\&.");
		break;

	default:
		if (c < 040)
			putchar('^'), c |= '@';
	case '\t':
	case '\n':
		putchar(c);
	}
}


/*  STRNCMP -	like strncmp except that we convert the
 *	 	first string to lower case before comparing.
 */
#define makelower(c) (isupper((c)) ? tolower((c)) : (c))

STRNCMP(s1, s2, len)
	register char *s1,*s2;
	register int len;
{
	do
	    if (*s2 - makelower(*s1))
		    return (*s2 - makelower(*s1));
	    else {
		    s2++;
		    s1++;
	    }
	while (--len);
	return(0);
}

/*  iskw -	check to see if the next word is a keyword
 */

iskw(s)
	register char *s;
{
	register char **ss = keywds;
	register int i = 1;
	register char *cp = s;

	while (++cp, isidchr(*cp))
		i++;
	while (cp = *ss++)
		if (!(upeqlow?STRNCMP(s,cp,i):strncmp(s,cp,i)) 
		    && !isidchr(cp[i]))
			return (i);
	return (0);
}

cprbegin(s)
	register char *s;
{
	register char *p;

	p = pname;

	/*
	 * some people like to start the names of routines that return
	 * a pointer with a '*'.
	 */
	if (*s == '*')
		s++;

	if ((*s == '_' || isalpha(*s)) && s[strlen(s) - 2] == ')') {
		while (isidchr(*s))
			*p++ = *s++;
		*p = 0;
		return (1);
	}
	return (0);
}

cprend(s)
	register char *s;
{
	if (!strcmp(s, "}\n"))
		return (1);
	else
		return (0);
}

iprbegin(s)
	register char *s;
{
	return(0);
}

iprend(s)
	register char *s;
{
	return(0);
}

pprbegin(s)
	register char *s;
{
	register char *p;

	p = pname;
	while ((*s == ' ') || (*s == '\t'))
		s++;
	if (strncmp(s, "procedure", 9) == 0)
		s += 9;
	else if (strncmp(s, "function", 8) ==0)
		s += 8;
	else
		return (0);
	while ((*s == ' ') || (*s == '\t'))
		s++;
	while ((*s != ' ') && (*s != '\t') && (*s != '(') && (*s != ';'))
		*p++ = *s++;
	*p = 0;
	return (1);
}

pprend(s)
	register char *s;
{
	if (strncmp (s, "end", 3) == 0)
		return (1);
	else
		return (0);
}

mprbegin(s)
	register char *s;
{
	register char *p;

	p = pname;
	if (strcmp(&s[strlen(s) - 10], "beginproc\n") == 0) {

		while ((*s == ' ') || (*s == '\t'))
			s++;

		while (*s != ' ')
			*p++ = *s++;
		*p = 0;
		return (1);
	} else
		return (0);
}

mprend(s)
	register char *s;
{
	if (!strcmp(&s[strlen(s) - 9], "endproc;\n"))
		return (1);
	else
		return (0);
}
