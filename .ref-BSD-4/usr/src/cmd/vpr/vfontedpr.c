#include <ctype.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

/*
 * Vfontedpr.
 *
 * Bill Joy, Apr. 1979.
 */
char	*ctime();
int	incomm;
int	instr;
int	nokeyw;
int	index;
int	margin;

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
		incomm = 0;
		instr = 0;
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
			if (buf[0] == '\f') {
				printf(".bp\n");
				continue;
			}
			putScp(buf);
			if (buf[strlen(buf) - 2] != '\\')
				instr = 0;
			margin = 0;
		}
		needbp = 1;
	} while (argc > 0);
	exit(0);
}

#define	ps(x)	printf("%s", x)
#define isidchr(c) (isalnum(c) || (c) == '_')

putScp(os)
	char *os;
{
	register char *s = os;
	register int i;
	int xfld = 0;

	if (nokeyw)
		goto skip;
	if ((*s == '_' || isalpha(*s)) && s[strlen(s) - 2] == ')') {
		register char *t = s + 1;

		while (isidchr(*t))
			t++;
		ps("'FN ");
		while (s < t)
			putchar(*s++);
		ps("\n");
	} else if (!strcmp(s, "}\n"))
		ps("'-F\n");
skip:
	while (*s) {
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
		if (!nokeyw && !incomm && *s == '"') {
			if (instr) {
				if (s[-1] != '\\')
					instr = 0;
			} else
				if (s[-1] != '\'')
					instr = 1;
		}
		if (incomm && s - os >= 2 && !strncmp("*/", s - 2, 2)) {
			incomm = 0;
			ps("\\c\n'-C\n");
		} else if (!nokeyw && !incomm && !strncmp("/*", s, 2)) {
			incomm = 1;
			if (s != os)
				ps("\\c");
			ps("\\c\n'+C\n");
			margin = width(os, s);
			ps("\\*(/*");
			s += 2;
			continue;
		}
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
		if (!nokeyw && !instr && (*s == '#' || isalpha(*s)) && (s == os || !isidchr(s[-1]))) {
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

iskw(s)
	register char *s;
{
	register char **ss = ckw;
	register int i = 1;
	register char *cp = s;

	while (++cp, isidchr(*cp))
		i++;
	while (cp = *ss++)
		if (*s == *cp && !strncmp(s, cp, i) && !isidchr(cp[i]))
			return (i);
	return (0);
}
