/* main.c */

/* Author:
 *	Steve Kirkendall
 *	14407 SW Teal Blvd. #C
 *	Beaverton, OR 97005
 *	kirkenda@cs.pdx.edu
 */


/* This file contains the main() function of vi */

#include "config.h"
#include <signal.h>
#include <setjmp.h>
#include "vi.h"

extern		trapint(); /* defined below */
extern char	*getenv();
jmp_buf		jmpenv;

#ifndef NO_DIGRAPH
static init_digraphs();
#endif

/*---------------------------------------------------------------------*/

void main(argc, argv)
	int	argc;
	char	*argv[];
{
	int	i;
	char	*cmd = (char *)0;
	char	*tag = (char *)0;
	char	*err = (char *)0;
	char	*str;
#if MSDOS || TOS
	char firstarg[256];
#else
	char *firstarg;
#endif

	/* set mode to MODE_VI or MODE_EX depending on program name */
	switch (argv[0][strlen(argv[0]) - 1])
	{
	  case 'x':			/* "ex" */
		mode = MODE_EX;
		break;

	  case 'w':			/* "view" */
		mode = MODE_VI;
		*o_readonly = TRUE;
		break;
#ifndef NO_EXTENSIONS
	  case 't':			/* "edit" or "input" */
		mode = MODE_VI;
		*o_inputmode = TRUE;
		break;
#endif
	  default:			/* "vi" or "elvis" */
		mode = MODE_VI;
	}

#ifndef DEBUG
# ifdef	SIGQUIT
	/* normally, we ignore SIGQUIT.  SIGINT is trapped later */
	signal(SIGQUIT, SIG_IGN);
# endif
#endif

	/* temporarily ignore SIGINT */
	signal(SIGINT, SIG_IGN);

	/* start curses */
	initscr();
	cbreak();
	noecho();
	scrollok(stdscr, TRUE);

	/* initialize the options */
	initopts();

	/* map the arrow keys.  The KU,KD,KL,and KR variables correspond to
	 * the :ku=: (etc.) termcap capabilities.  The variables are defined
	 * as part of the curses package.
	 */
	if (has_KU) mapkey(has_KU, "k",    WHEN_VICMD|WHEN_INMV, "<Up>");
	if (has_KD) mapkey(has_KD, "j",    WHEN_VICMD|WHEN_INMV, "<Down>");
	if (has_KL) mapkey(has_KL, "h",    WHEN_VICMD|WHEN_INMV, "<Left>");
	if (has_KR) mapkey(has_KR, "l",    WHEN_VICMD|WHEN_INMV, "<Right>");
	if (has_HM) mapkey(has_HM, "^",    WHEN_VICMD|WHEN_INMV, "<Home>");
	if (has_EN) mapkey(has_EN, "$",    WHEN_VICMD|WHEN_INMV, "<End>");
	if (has_PU) mapkey(has_PU, "\002", WHEN_VICMD|WHEN_INMV, "<PgUp>");
	if (has_PD) mapkey(has_PD, "\006", WHEN_VICMD|WHEN_INMV, "<PgDn>");
#if MSDOS
	if (*o_pcbios)
	{
		mapkey("#R", "i", WHEN_VICMD|WHEN_INMV,	"<Insrt>");
		mapkey("#S", "x", WHEN_VICMD|WHEN_INMV,	"<Del>");
		mapkey("#s", "B", WHEN_VICMD|WHEN_INMV,	"^<left>");
		mapkey("#t", "W", WHEN_VICMD|WHEN_INMV,	"^<right>");
	}
#else
	if (ERASEKEY != '\177')
	{
		mapkey("\177", "x", WHEN_VICMD|WHEN_INMV, "<Del>");
	}
#endif

#ifndef NO_DIGRAPH
	init_digraphs();
#endif /* NO_DIGRAPH */

	/* process any flags */
	for (i = 1; i < argc && *argv[i] == '-'; i++)
	{
		switch (argv[i][1])
		{
		  case 'R':	/* readonly */
			*o_readonly = TRUE;
			break;

		  case 'r':	/* recover */
			msg("Use the `virec` program to recover lost files");
			endmsgs();
			refresh();
			endwin();
			exit(0);
			break;

		  case 't':	/* tag */
			if (argv[i][2])
			{
				tag = argv[i] + 2;
			}
			else
			{
				i++;
				tag = argv[i];
			}
			break;

		  case 'v':	/* vi mode */
			mode = MODE_VI;
			break;

		  case 'e':	/* ex mode */
			mode = MODE_EX;
			break;
#ifndef NO_EXTENSIONS
		  case 'i':	/* input mode */
			*o_inputmode = TRUE;
			break;
#endif
#ifndef NO_ERRLIST
		  case 'm':	/* use "errlist" as the errlist */
			if (argv[i][2])
			{
				err = argv[i] + 2;
			}
			else if (i + 1 < argc)
			{
				i++;
				err = argv[i];
			}
			else
			{
				err = "";
			}
			break;
#endif
		  default:
			msg("Ignoring unknown flag \"%s\"", argv[i]);
		}
	}

	/* if we were given an initial ex command, save it... */
	if (i < argc && *argv[i] == '+')
	{
		if (argv[i][1])
		{
			cmd = argv[i++] + 1;
		}
		else
		{
			cmd = "$"; /* "vi + file" means start at EOF */
			i++;
		}
	}

	/* the remaining args are file names. */
	nargs = argc - i;
	if (nargs > 0)
	{
#if ! ( MSDOS || TOS )
		firstarg = argv[i];
#endif
		strcpy(args, argv[i]);
		while (++i < argc && strlen(args) + 1 + strlen(argv[i]) < sizeof args)
		{
			strcat(args, " ");
			strcat(args, argv[i]);
		}
	}
#if ! ( MSDOS || TOS )
	else
	{
		firstarg = "";
	}
#endif
	argno = 0;

#if MSDOS || TOS
	if (nargs > 0)
	{
		strcpy(args, wildcard(args));
		nargs = 1;
		for (i = 0; args[i]; i++)
		{
			if (args[i] == ' ')
			{
				nargs++;
			}
		}
		for (i = 0; args[i] && args[i] != ' '; i++)
		{
			firstarg[i] = args[i];
		}
		firstarg[i] = '\0';
	}
	else
	{
		firstarg[0] = '\0';
	}
#endif

	/* perform the .exrc files and EXINIT environment variable */
#ifdef SYSEXRC
	doexrc(SYSEXRC);
#endif
#ifdef HMEXRC
	str = getenv("HOME");
	if (str)
	{
		sprintf(tmpblk.c, "%s%c%s", str, SLASH, HMEXRC);
		doexrc(tmpblk.c);
	}
#endif
	doexrc(EXRC);
#ifdef EXINIT
	str = getenv(EXINIT);
	if (str)
	{
		exstring(str, strlen(str));
	}
#endif

	/* search for a tag (or an error) now, if desired */
	blkinit();
	if (tag)
	{
		cmd_tag(MARK_FIRST, MARK_FIRST, CMD_TAG, 0, tag);
	}
#ifndef NO_ERRLIST
	else if (err)
	{
		cmd_errlist(MARK_FIRST, MARK_FIRST, CMD_ERRLIST, 0, err);
	}
#endif

	/* if no tag/err, or tag failed, then start with first arg */
	if (tmpfd < 0 && tmpstart(firstarg) == 0 && *origname)
	{
		ChangeText
		{
		}
		clrflag(file, MODIFIED);
	}

	/* now we do the immediate ex command that we noticed before */
	if (cmd)
	{
		doexcmd(cmd);
	}

	/* repeatedly call ex() or vi() (depending on the mode) until the
	 * mode is set to MODE_QUIT
	 */
	while (mode != MODE_QUIT)
	{
		if (setjmp(jmpenv))
		{
			/* Maybe we just aborted a change? */
			abortdo();
		}
#if TURBOC
		signal(SIGINT, (void(*)()) trapint);
#else
		signal(SIGINT, trapint);
#endif

		switch (mode)
		{
		  case MODE_VI:
			vi();
			break;

		  case MODE_EX:
			ex();
			break;
#ifdef DEBUG
		  default:
			msg("mode = %d?", mode);
			mode = MODE_QUIT;
#endif
		}
	}

	/* free up the cut buffers */
	cutend();

	/* end curses */
#ifndef	NO_CURSORSHAPE
	if (has_CQ)
		do_CQ();
#endif
	endmsgs();
	move(LINES - 1, 0);
	clrtoeol();
	refresh();
	endwin();

	exit(0);
	/*NOTREACHED*/
}


/*ARGSUSED*/
int trapint(signo)
	int	signo;
{
	resume_curses(FALSE);
	abortdo();
#if OSK
	sigmask(-1);
#endif
#if TURBO_C
	signal(signo, (void (*)())trapint);
#else
	signal(signo, trapint);
#endif
	longjmp(jmpenv, 1);

	return 0;
}


#ifndef NO_DIGRAPH

/* This stuff us used to build the default digraphs table. */
static char	digtable[][4] =
{
# if CS_IBMPC
	"C,\200",	"u\"\1",	"e'\2",		"a^\3",
	"a\"\4",	"a`\5",		"a@\6",		"c,\7",
	"e^\10",	"e\"\211",	"e`\12",	"i\"\13",
	"i^\14",	"i`\15",	"A\"\16",	"A@\17",
	"E'\20",	"ae\21",	"AE\22",	"o^\23",
	"o\"\24",	"o`\25",	"u^\26",	"u`\27",
	"y\"\30",	"O\"\31",	"U\"\32",	"a'\240",
	"i'!",		"o'\"",		"u'#",		"n~$",
	"N~%",		"a-&",		"o-'",		"~?(",
	"~!-",		"\"<.",		"\">/",
#  if CS_SPECIAL
	"2/+",		"4/,",		"^+;",		"^q<",
	"^c=",		"^r>",		"^t?",		"pp]",
	"^^^",		"oo_",		"*a`",		"*ba",
	"*pc",		"*Sd",		"*se",		"*uf",
	"*tg",		"*Ph",		"*Ti",		"*Oj",
	"*dk",		"*Hl",		"*hm",		"*En",
	"*No",		"eqp",		"pmq",		"ger",
	"les",		"*It",		"*iu",		"*/v",
	"*=w",		"sq{",		"^n|",		"^2}",
	"^3~",		"^_\377",
#  endif /* CS_SPECIAL */
# endif /* CS_IBMPC */
# if CS_LATIN1
	"~!!",		"a-*",		"\">+",		"o-:",
	"\"<>",		"~??",

	"A`@",		"A'A",		"A^B",		"A~C",
	"A\"D",		"A@E",		"AEF",		"C,G",
	"E`H",		"E'I",		"E^J",		"E\"K",
	"I`L",		"I'M",		"I^N",		"I\"O",
	"-DP",		"N~Q",		"O`R",		"O'S",
	"O^T",		"O~U",		"O\"V",		"O/X",
	"U`Y",		"U'Z",		"U^[",		"U\"\\",
	"Y'_",

	"a``",		"a'a",		"a^b",		"a~c",
	"a\"d",		"a@e",		"aef",		"c,g",
	"e`h",		"e'i",		"e^j",		"e\"k",
	"i`l",		"i'm",		"i^n",		"i\"o",
	"-dp",		"n~q",		"o`r",		"o's",
	"o^t",		"o~u",		"o\"v",		"o/x",
	"u`y",		"u'z",		"u^{",		"u\"|",
	"y'~",
# endif /* CS_LATIN1 */
	""
};

static init_digraphs()
{
	int	i;

	for (i = 0; *digtable[i]; i++)
	{
		do_digraph(FALSE, digtable[i]);
	}
	do_digraph(FALSE, (char *)0);
}
#endif /* NO_DIGRAPH */
