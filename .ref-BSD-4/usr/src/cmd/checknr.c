static char *sccsid = "@(#)checknr.c	4.2 (Berkeley) 10/12/80";
/*
 * checknr: check an nroff/troff input file for matching macro calls.
 * we also attempt to match size and font changes, but only the embedded
 * kind.  These must end in \s0 and \fP resp.  Maybe more sophistication
 * later but for now think of these restrictions as contributions to
 * structured typesetting.
 */
#include <stdio.h>
#include <ctype.h>

#define MAXSTK	100	/* Stack size */
#define MAXBR	100	/* Max number of bracket pairs known */
#define MAXCMDS	500	/* Max number of commands known */

/*
 * The stack on which we remember what we've seen so far.
 */
struct stkstr {
	int opno;	/* number of opening bracket */
	int pl;		/* '+', '-', ' ' for \s, 1 for \f, 0 for .ft */
	int parm;	/* parm to size, font, etc */
	int lno;	/* line number the thing came in in */
} stk[MAXSTK];
int stktop;

/*
 * The kinds of opening and closing brackets.
 */
struct brstr {
	char *opbr;
	char *clbr;
} br[MAXBR] = {
	/* A few bare bones troff commands */
#define SZ	0
	"sz",	"sz",	/* also \s */
#define FT	1
	"ft",	"ft",	/* also \f */
	/* the -ms package */
	"AB",	"AE",
	"RS",	"RE",
	"LG",	"NL",
	"SM",	"NL",
	"FS",	"FE",
	"DS",	"DE",
	"CD",	"DE",
	"LD",	"DE",
	"ID",	"DE",
	"KS",	"KE",
	"KF",	"KE",
	"QS",	"QE",
	/* Things needed by preprocessors */
	"TS",	"TE",
	"EQ",	"EN",
	/* The -me package */
	"(l",	")l",
	"(q",	")q",
	"(b",	")b",
	"(z",	")z",
	"(c",	")c",
	"(d",	")d",
	"(f",	")f",
	"(x",	")x",
	0,	0
};

/*
 * All commands known to nroff, plus ms and me.
 * Used so we can complain about unrecognized commands.
 */
char *knowncmds[MAXCMDS] = {
"$c", "$f", "$h", "$p", "$s", "(b", "(c", "(d", "(f", "(l",
"(q", "(t", "(x", "(z", ")b", ")c", ")d", ")f", ")l", ")q",
")t", ")x", ")z", "++", "+c", "1C", "1c", "2C", "2c", "@(",
"@)", "@C", "@D", "@F", "@I", "@M", "@c", "@e", "@f", "@h",
"@m", "@n", "@o", "@p", "@r", "@t", "@z", "AB", "AB", "AE",
"AE", "AI", "AI", "AT", "AU", "AU", "AX", "B",  "B1", "B2",
"BD", "BG", "BT", "BX", "C1", "C2", "CD", "CM", "CT", "D", 
"DA", "DE", "DF", "DS", "EG", "EM", "EN", "EQ", "EQ", "FA",
"FE", "FJ", "FK", "FL", "FN", "FO", "FQ", "FS", "FV", "FX",
"HO", "I",  "ID", "IE", "IH", "IM", "IP", "IZ", "KD", "KE",
"KF", "KQ", "KS", "LB", "LD", "LG", "LP", "MC", "ME", "MF",
"MH", "MR", "ND", "NH", "NL", "NP", "OK", "PP", "PT", "PY",
"QE", "QP", "QS", "R",  "RA", "RC", "RE", "RP", "RQ", "RS",
"RT", "S0", "S2", "S3", "SG", "SH", "SM", "SY", "TA", "TC",
"TD", "TE", "TH", "TL", "TL", "TM", "TQ", "TR", "TS", "TS",
"TX", "UL", "US", "UX", "WH", "XD", "XF", "XK", "XP", "[-",
"[0", "[1", "[2", "[3", "[4", "[5", "[<", "[>", "[]", "]-",
"]<", "]>", "][", "ab", "ac", "ad", "af", "am", "ar", "as",
"b",  "ba", "bc", "bd", "bi", "bl", "bp", "bp", "br", "bx",
"c.", "c2", "cc", "ce", "cf", "ch", "cs", "ct", "cu", "da",
"de", "di", "dl", "dn", "ds", "dt", "dw", "dy", "ec", "ef",
"eh", "el", "em", "eo", "ep", "ev", "ex", "fc", "fi", "fl",
"fo", "fp", "ft", "fz", "hc", "he", "hl", "hp", "ht", "hw",
"hx", "hy", "i",  "ie", "if", "ig", "in", "ip", "it", "ix",
"lc", "lg", "li", "ll", "ll", "ln", "lo", "lp", "ls", "lt",
"m1", "m2", "m3", "m4", "mc", "mk", "mo", "n1", "n2", "na",
"ne", "nf", "nh", "nl", "nm", "nn", "np", "nr", "ns", "nx",
"of", "oh", "os", "pa", "pc", "pi", "pl", "pm", "pn", "po",
"po", "pp", "ps", "q",  "r",  "rb", "rd", "re", "re", "rm",
"rn", "ro", "rr", "rs", "rt", "sb", "sc", "sh", "sk", "so",
"sp", "ss", "st", "sv", "sz", "ta", "tc", "th", "ti", "tl",
"tm", "tp", "tr", "u",  "uf", "uh", "ul", "vs", "wh", "yr",
0
};

int	lineno;		/* current line number in input file */
char	line[256];	/* the current line */
char	*cfilename;	/* name of current file */
int	nfiles;		/* number of files to process */
int	fflag;		/* -f: ignore \f */
int	sflag;		/* -s: ignore \s */
int	ncmds;		/* size of knowncmds */
int	slot;		/* slot in knowncmds found by binsrch */

char	*malloc();

main(argc, argv)
int argc;
char **argv;
{
	FILE *f;
	int i;
	char *cp;
	char b1[4];

	if (argc <= 1)
		usage();
	/* Figure out how many known commands there are */
	while (knowncmds[ncmds])
		ncmds++;
	while (argc > 1 && argv[1][0] == '-') {
		switch(argv[1][1]) {

		/* -a: add pairs of macros */
		case 'a':
			i = strlen(argv[1]) - 2;
			if (i % 6 != 0)
				usage();
			/* look for empty macro slots */
			for (i=0; br[i].opbr; i++)
				;
			for (cp=argv[1]+3; cp[-1]; cp += 6) {
				br[i].opbr = malloc(3);
				strncpy(br[i].opbr, cp, 2);
				br[i].clbr = malloc(3);
				strncpy(br[i].clbr, cp+3, 2);
				addmac(br[i].opbr);	/* knows pairs are also known cmds */
				addmac(br[i].clbr);
				i++;
			}
			break;

		/* -c: add known commands */
		case 'c':
			i = strlen(argv[1]) - 2;
			if (i % 3 != 0)
				usage();
			for (cp=argv[1]+3; cp[-1]; cp += 3) {
				if (cp[2] && cp[2] != '.')
					usage();
				strncpy(b1, cp, 2);
				addmac(b1);
			}
			break;

		/* -f: ignore font changes */
		case 'f':
			fflag = 1;
			break;

		/* -s: ignore size changes */
		case 's':
			sflag = 1;
			break;
		default:
			usage();
		}
		argc--; argv++;
	}

	nfiles = argc - 1;

	if (nfiles > 0) {
		for (i=1; i<argc; i++) {
			cfilename = argv[i];
			f = fopen(cfilename, "r");
			if (f == NULL)
				perror(cfilename);
			else
				process(f);
		}
	} else {
		cfilename = "stdin";
		process(stdin);
	}
	exit(0);
}

usage()
{
	printf("Usage: checknr -s -f -a.xx.yy.xx.yy... -c.xx.xx.xx...\n");
	exit(1);
}

process(f)
FILE *f;
{
	register int i, n;
	char mac[5];	/* The current macro or nroff command */
	int pl;

	stktop = -1;
	for (lineno = 1; fgets(line, sizeof line, f); lineno++) {
		if (line[0] == '.') {
			/*
			 * find and isolate the macro/command name.
			 */
			strncpy(mac, line+1, 4);
			if (isspace(mac[0])) {
				pe(lineno);
				printf("Empty command\n");
			} else if (isspace(mac[1])) {
				mac[1] = 0;
			} else if (isspace(mac[2])) {
				mac[2] = 0;
			} else if (mac[2] != '\\' || mac[3] != '\"') {
				pe(lineno);
				printf("Command too long\n");
			}

			/*
			 * Is it a known command?
			 */
			checkknown(mac);

			/*
			 * Should we add it?
			 */
			if (eq(mac, "de"))
				addcmd(line);

			chkcmd(line, mac);
		}

		/*
		 * At this point we process the line looking
		 * for \s and \f.
		 */
		for (i=0; line[i]; i++)
			if (line[i]=='\\' && (i==0 || line[i-1]!='\\')) {
				if (!sflag && line[++i]=='s') {
					pl = line[++i];
					if (isdigit(pl)) {
						n = pl - '0';
						pl = ' ';
					} else
						n = 0;
					while (isdigit(line[++i]))
						n = 10 * n + line[i] - '0';
					i--;
					if (n == 0) {
						if (stk[stktop].opno == SZ) {
							stktop--;
						} else {
							pe(lineno);
							printf("unmatched \\s0\n");
						}
					} else {
						stk[++stktop].opno = SZ;
						stk[stktop].pl = pl;
						stk[stktop].parm = n;
						stk[stktop].lno = lineno;
					}
				} else if (!fflag && line[i]=='f') {
					n = line[++i];
					if (n == 'P') {
						if (stk[stktop].opno == FT) {
							stktop--;
						} else {
							pe(lineno);
							printf("unmatched \\fP\n");
						}
					} else {
						stk[++stktop].opno = FT;
						stk[stktop].pl = 1;
						stk[stktop].parm = n;
						stk[stktop].lno = lineno;
					}
				}
			}
	}
	/*
	 * We've hit the end and look at all this stuff that hasn't been
	 * matched yet!  Complain, complain.
	 */
	for (i=stktop; i>=0; i--) {
		complain(i);
	}
}

complain(i)
{
	pe(stk[i].lno);
	printf("Unmatched ");
	prop(i);
	printf("\n");
}

prop(i)
{
	if (stk[i].pl == 0)
		printf(".%s", br[stk[i].opno].opbr);
	else switch(stk[i].opno) {
	case SZ:
		printf("\\s%c%d", stk[i].pl, stk[i].parm);
		break;
	case FT:
		printf("\\f%c", stk[i].parm);
		break;
	default:
		printf("Bug: stk[%d].opno = %d = .%s, .%s",
			i, stk[i].opno, br[stk[i].opno].opbr, br[stk[i].opno].clbr);
	}
}

chkcmd(line, mac)
char *line;
char *mac;
{
	register int i, n;

	/*
	 * Check to see if it matches top of stack.
	 */
	if (stktop >= 0 && eq(mac, br[stk[stktop].opno].clbr))
		stktop--;	/* OK. Pop & forget */
	else {
		/* No. Maybe it's an opener */
		for (i=0; br[i].opbr; i++) {
			if (eq(mac, br[i].opbr)) {
				/* Found. Push it. */
				stktop++;
				stk[stktop].opno = i;
				stk[stktop].pl = 0;
				stk[stktop].parm = 0;
				stk[stktop].lno = lineno;
				break;
			}
			/*
			 * Maybe it's an unmatched closer.
			 * NOTE: this depends on the fact
			 * that none of the closers can be
			 * openers too.
			 */
			if (eq(mac, br[i].clbr)) {
				nomatch(mac);
				break;
			}
		}
	}
}

nomatch(mac)
char *mac;
{
	register int i, j;

	/*
	 * Look for a match further down on stack
	 * If we find one, it suggests that the stuff in
	 * between is supposed to match itself.
	 */
	for (j=stktop; j>=0; j--)
		if (eq(mac,br[stk[j].opno].clbr)) {
			/* Found.  Make a good diagnostic. */
			if (j == stktop-2) {
				/*
				 * Check for special case \fx..\fR and don't
				 * complain.
				 */
				if (stk[j+1].opno==FT && stk[j+1].parm!='R'
				 && stk[j+2].opno==FT && stk[j+2].parm=='R') {
					stktop = j -1;
					return;
				}
				/*
				 * We have two unmatched frobs.  Chances are
				 * they were intended to match, so we mention
				 * them together.
				 */
				pe(stk[j+1].lno);
				prop(j+1);
				printf(" does not match %d: ", stk[j+2].lno);
				prop(j+2);
				printf("\n");
			} else for (i=j+1; i <= stktop; i++) {
				complain(i);
			}
			stktop = j-1;
			return;
		}
	/* Didn't find one.  Throw this away. */
	pe(lineno);
	printf("Unmatched .%s\n", mac);
}

/* eq: are two strings equal? */
eq(s1, s2)
char *s1, *s2;
{
	return (strcmp(s1, s2) == 0);
}

/* print the first part of an error message, given the line number */
pe(lineno)
int lineno;
{
	if (nfiles > 1)
		printf("%s: ", cfilename);
	printf("%d: ", lineno);
}

checkknown(mac)
char *mac;
{

	if (eq(mac, "."))
		return;
	if (binsrch(mac) >= 0)
		return;

	pe(lineno);
	printf("Unknown command: .%s\n", mac);
}

/*
 * We have a .de xx line in "line".  Add xx to the list of known commands.
 */
addcmd(line)
char *line;
{
	char *mac;

	/* grab the macro being defined */
	mac = line+4;
	while (isspace(*mac))
		mac++;
	if (*mac == 0) {
		pe(lineno);
		printf("illegal define: %s\n", line);
		return;
	}
	mac[2] = 0;
	if (isspace(mac[1]) || mac[1] == '\\')
		mac[1] = 0;
	if (ncmds >= MAXCMDS) {
		printf("Only %d known commands allowed\n", MAXCMDS);
		exit(1);
	}
	addmac(mac);
}

/*
 * Add mac to the list.  We should really have some kind of tree
 * structure here but this is a quick-and-dirty job and I just don't
 * have time to mess with it.  (I wonder if this will come back to haunt
 * me someday?)  Anyway, I claim that .de is fairly rare in user
 * nroff programs, and the register loop below is pretty fast.
 */
addmac(mac)
char *mac;
{
	register char **src, **dest, **loc;

	binsrch(mac);	/* it's OK to redefine something */
	/* binsrch sets slot as a side effect */
#ifdef DEBUG
printf("binsrch(%s) -> %d\n", mac, slot);
#endif
	loc = &knowncmds[slot];
	src = &knowncmds[ncmds-1];
	dest = src+1;
	while (dest > loc)
		*dest-- = *src--;
	*loc = malloc(3);
	strcpy(*loc, mac);
	ncmds++;
#ifdef DEBUG
printf("after: %s %s %s %s %s, %d cmds\n", knowncmds[slot-2], knowncmds[slot-1], knowncmds[slot], knowncmds[slot+1], knowncmds[slot+2], ncmds);
#endif
}

/*
 * Do a binary search in knowncmds for mac.
 * If found, return the index.  If not, return -1.
 */
binsrch(mac)
char *mac;
{
	register char *p;	/* pointer to current cmd in list */
	register int d;		/* difference if any */
	register int mid;	/* mid point in binary search */
	register int top, bot;	/* boundaries of bin search, inclusive */

	top = ncmds-1;
	bot = 0;
	while (top >= bot) {
		mid = (top+bot)/2;
		p = knowncmds[mid];
		d = p[0] - mac[0];
		if (d == 0)
			d = p[1] - mac[1];
		if (d == 0)
			return mid;
		if (d < 0)
			bot = mid + 1;
		else
			top = mid - 1;
	}
	slot = bot;	/* place it would have gone */
	return -1;
}
