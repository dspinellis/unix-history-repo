/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)tset.c	5.13 (Berkeley) 6/1/90";
#endif /* not lint */

/*
**  TSET -- set terminal modes
**
**	This program does sophisticated terminal initialization.
**	I recommend that you include it in your .profile or .login
**	file to initialize whatever terminal you are on.
**
**	There are several features:
**
**	A special file or sequence (as controlled by the termcap file)
**	is sent to the terminal.
**
**	Mode bits are set on a per-terminal_type basis.
**	This allows special delays, automatic tabs, etc.
**
**	Erase and Kill characters can be set to whatever you want.
**	Default is to change erase to control-H on a terminal which
**	can overstrike, and leave it alone on anything else.  Kill
**	is always left alone unless specifically requested.  These
**	characters can be represented as "^X" meaning control-X;
**	X is any character.
**
**	Terminals which are dialups or plugboard types can be aliased
**	to whatever type you may have in your home or office.  Thus,
**	if you know that when you dial up you will always be on a
**	TI 733, you can specify that fact to tset.  You can represent
**	a type as "?type".  This will ask you what type you want it
**	to be -- if you reply with just a newline, it will default
**	to the type given.
**
**	The current terminal type can be queried.
**
**	Usage:
**		tset [-] [-hnrsIQS] [-eC] [-iC] [-kC] [-EC]
**		[-m [port-type][test baudrate]:terminal-type] [terminal-type]
**
**		In systems with environments, use:
**			eval `tset -s ...`
**		Actually, this doesn't work in old csh's.
**		Instead, use:
**			tset -s ... > tset.tmp
**			source tset.tmp
**			rm tset.tmp
**		or:
**			set noglob
**			set term=(`tset -S ....`)
**			setenv TERM $term[1]
**			setenv TERMCAP "$term[2]"
**			unset term
**			unset noglob
**
**	Positional Parameters:
**		type -- the terminal type to force.  If this is
**			specified, initialization is for this
**			terminal type.
**
**	Flags:
**		- -- report terminal type.  Whatever type is
**			decided on is reported.  If no other flags
**			are stated, the only affect is to write
**			the terminal type on the standard output.
**		-r -- report to user in addition to other flags.
**		-EC -- set the erase character to C on all terminals
**			except those which cannot backspace (e.g.,
**			a TTY 33).
**		-eC -- set the erase character to C on all terminals.
**			If not specified,
**			the erase character is untouched; however, if
**			not specified and the erase character is NULL
**			(zero byte), the erase character  is set to delete.
**		-kC -- set the kill character to C on all terminals.
**			If not specified,
**			the kill character is untouched; however, if
**			not specified and the kill character is NULL
**			(zero byte), the kill character is set to control-U.
**		-iC -- set the interrupt character to C on all terminals.
**			If not specified, the
**			interrupt character is untouched; however, if
**			not specified and the interrupt character is NULL
**			(zero byte), the interrupt character is set to
**			control-C.
**		-qC -- reserved for settable quit character.
**		-m -- map the system identified type to some user
**			specified type. The mapping can be baud rate
**			dependent. This replaces the old -d, -p flags.
**			(-d type  ->  -m dialup:type)
**			(-p type  ->  -m plug:type)
**			Syntax:	-m identifier [test baudrate] :type
**			where: ``identifier'' is terminal type found in
**			/etc/ttys for this port, (absence of an identifier
**			matches any identifier); ``test'' may be any
**			combination of  >  =  <  !  @; ``baudrate'' is as with
**			stty (1); ``type'' is the actual terminal type to use
**			if the mapping condition is met. Multiple maps are
**			scanned in order and the first match prevails.
**		-h -- don't read terminal type from environment.
**		-s -- output setenv commands for TERM.  This can be
**			used with
**				`tset -s ...`
**			and is to be prefered to:
**				setenv TERM `tset - ...`
**			because -s sets the TERMCAP variable also.
**		-S -- Similar to -s but outputs 2 strings suitable for
**			use in csh .login files as follows:
**				set noglob
**				set term=(`tset -S .....`)
**				setenv TERM $term[1]
**				setenv TERMCAP "$term[2]"
**				unset term
**				unset noglob
**		-Q -- be quiet.  don't output 'Erase set to' etc.
**		-I -- don't do terminal initialization (is & if
**			strings).
**
**	Files:
**		/etc/ttys
**			contains a terminal id -> terminal type
**			mapping; used when any user mapping is specified,
**			or the environment doesn't have TERM set.
**		/etc/termcap
**			a terminal_type -> terminal_capabilities
**			mapping.
**
**	Exit Codes:
**		2 -- couldn't open termcap.
**		1 -- invalid terminal type, or standard error not tty.
**		0 -- ok.
**
**	Author:
**		Eric Allman
**		Electronics Research Labs
**		U.C. Berkeley
**
**	History:
**		1/81 -- Added alias checking for mapping identifiers.
**		9/80 -- Added UCB_NTTY mods to setup the new tty driver.
**			Added the 'reset ...' invocation.
**		7/80 -- '-S' added. '-m' mapping added. TERMCAP string
**			cleaned up.
**		3/80 -- Changed to use tputs.  Prc & flush added.
**		10/79 -- '-s' option extended to handle TERMCAP
**			variable, set noglob, quote the entry,
**			and know about the Bourne shell.  Terminal
**			initialization moved to before any information
**			output so screen clears would not screw you.
**			'-Q' option added.
**		8/79 -- '-' option alone changed to only output
**			type.  '-s' option added.  'VERSION7'
**			changed to 'V6' for compatibility.
**		12/78 -- modified for eventual migration to VAX/UNIX,
**			so the '-' option is changed to output only
**			the terminal type to STDOUT instead of
**			FILEDES.
**		9/78 -- '-' and '-p' options added (now fully
**			compatible with ttytype!), and spaces are
**			permitted between the -d and the type.
**		8/78 -- The sense of -h and -u were reversed, and the
**			-f flag is dropped -- same effect is available
**			by just stating the terminal type.
**		10/77 -- Written.
*/

#include <sys/types.h>
#include <stdio.h>
#include <ttyent.h>
#include <termios.h>
#include <unistd.h>
#include <stdlib.h>
#include <signal.h>
#include <ctype.h>
#include <string.h>
#include <memory.h>
#include <fcntl.h>

/* Default values for control characters. */
#ifndef CEOF
#define	_CTRL(c) ((c) & 0x1f)
#define	CEOF	_CTRL('d')
#define	CERASE	0177		/* DEL, ^? */
#define	CINTR	_CTRL('c')
#define CFLUSH  _CTRL('o')
#define	CKILL	_CTRL('u')
#define	CLNEXT	_CTRL('v')
#define	CQUIT	034		/* FS, ^\ */
#define	CRPRNT	_CTRL('r')
#define	CSTART	_CTRL('q')
#define	CSTOP	_CTRL('s')
#define	CSUSP	_CTRL('z')
#define	CWERASE	_CTRL('w')
#endif

/* This should be in <termcap.h> instead. */
int tgetent();
int tgetnum();
int tgetflag();
char *tgetstr();
int tputs();
char *tgoto();

extern char PC;
extern short ospeed;

/* This should be in <getopt.h> instead. */
int getopt();

extern int optind;
extern char *optarg;

char *mapped();
char *putbuf();
char *stypeof();
int baudrate();
int cancelled();
int isalias();
int prc();
int settabs();
void add_mapping();
void cat();
void flush();
void set_control_chars();
void get_termcap_entry();
void get_tty_type();
void makealias();
void output_initializations();
void prs();
void reportek();
void reset_mode();
void set_conversions();
void setdelay();
void set_mode();
void usage();
void wrtermcap();

#define curerase mode.c_cc[VERASE]
#define curkill mode.c_cc[VKILL]
#define curintr mode.c_cc[VINTR]
#define olderase oldmode.c_cc[VERASE]
#define oldkill oldmode.c_cc[VKILL]
#define oldintr oldmode.c_cc[VINTR]

#define	YES		1
#define	NO		0
#define	CNTL(c)		((c)&037)
#define	BACKSPACE	(CNTL('H'))
#define	CHK(val, dft)	(val<=0 ? dft : val)

#define	FILEDES		2	/* change attrs on this descriptor */
#define	STDOUT		1	/* output -s/-S to this descriptor */

/* Last resort default term type. */
#define	DEFTYPE		"unknown"

/* Baud rate conditionals for mapping. */
#define	ANY		0
#define	GT		1
#define	EQ		2
#define	LT		4
#define	GE		(GT|EQ)
#define	LE		(LT|EQ)
#define	NE		(GT|LT)
#define	ALL		(GT|EQ|LT)

/* Maximum number of mappings allowed. */
#define	NMAP		10

struct	map {
	char *Ident;		/* Port type, or "" for any. */
	char Test;		/* Baud rate conditionals bitmask. */
	char Speed;		/* Baud rate to compare against. */
	char *Type;		/* Terminal type to select. */
};

struct map map[NMAP];

/* Next available element of `map'. */
struct map *Map = map;

struct
{
	char	*string;	/* ASCII representation. */
	int	speed;		/* Internal form. */
	int	baudrate;	/* Numeric value. */
} speeds[] = {
	"0",	B0,	0,
	"50",	B50,	50,
	"75",	B75,	75,
	"110",	B110,	110,
	"134",	B134,	134,
	"134.5",B134,	134,
	"150",	B150,	150,
	"200",	B200,	200,
	"300",	B300,	300,
	"600",	B600,	600,
	"1200",	B1200,	1200,
	"1800",	B1800,	1800,
	"2400",	B2400,	2400,
	"4800",	B4800,	4800,
	"9600",	B9600,	9600,
	"19200",B19200,	19200,
	"38400",B38400,	38400,
	"exta",	B19200,	19200,
	"extb",	B38400,	38400,
	NULL
};

char	Erase_char;		/* new erase character */
char	Kill_char;		/* new kill character */
char	Intr_char;		/* new interrupt character */
char	Specialerase;		/* set => Erase_char only on terminals with backspace (-E) */

char	*TtyPath = NULL;	/* terminal device */
char	*TtyType = NULL;	/* type of terminal */
char	*DefType = NULL;	/* default type if none other computed */
int	Mapped;			/* mapping has been specified */
int	NoTermFromEnv;		/* don't get terminal type from environ (-h) */
int	DoSetenv;		/* output TERMCAP strings (-s, -S) */
int	CmndLine;		/* output setenv command lines (-s) */
int	BeQuiet;		/* Don't say ctrl key settings (-Q, reset) */
int	NoInit;			/* don't output initialization string (-I) */
int	IsReset;		/* invoked as reset */
int	Report;			/* report term type on stdout (-) */
int	Ureport;		/* report term type to user on stderr (-r) */
int	RepOnly;		/* only report term type (- & no other args) */
int	Ask;			/* ask user for termtype */
int	PadBaud;		/* Min rate of padding needed, -1 if none */
int	lines, columns;

#define CAPBUFSIZ	1024
char	Capbuf[CAPBUFSIZ];	/* line from /etc/termcap for this TtyType */
char	*Ttycap;		/* termcap line from termcap or environ */

char	Aliasbuf[128];
/* Null-terminated array of alternative names for the terminal.
   The first element is the terminal's two-letter V6 short name.
   The elements are pointers into 'Aliasbuf'. */
char	*Alias[16];

struct termios mode;
struct termios oldmode;

/* The name this program was run with. */
char *program_name;

void
main(argc, argv)
int	argc;
char	*argv[];
{
	char		*command;
	register int	i;
	int		optc;
	int		csh = NO;
#ifdef TIOCGWINSZ
	struct winsize	win;
#endif

	program_name = argv[0];

	if (tcgetattr(FILEDES, &mode) < 0)
	{
		fprintf(stderr, "%s: standard error must be a terminal\n",
			program_name);
		exit(1);
	}
	oldmode = mode;

	ospeed = cfgetospeed(&mode);

	(void) signal(SIGINT, set_mode);
	(void) signal(SIGQUIT, set_mode);
	(void) signal(SIGTERM, set_mode);

	command = strrchr(argv[0], '/');
	if (command)
		command++;
	else
		command = argv[0];
	if (!strcmp(command, "reset") )
	{
		reset_mode();
		BeQuiet = YES;
		IsReset = YES;
	}

	while ((optc = getopt(argc, argv, "e:hi:k:m:nrsE:IQS")) != EOF)
	{
			switch (optc)
			{
			  case 'n':	/* obsolete -- ignore */
				break;

			  case 'r':	/* report to user */
				Ureport = YES;
				break;

			  case 'E':	/* special erase: operate on all but TTY33 */
				Specialerase = YES;
				/* explicit fall-through to -e case */

			  case 'e':	/* erase character */
				if (optarg[0] == '^' && optarg[1] != '\0')
				{
					if (optarg[1] == '?')
						Erase_char = '\177';
					else
						Erase_char = CNTL(optarg[1]);
				}
				else
					Erase_char = optarg[0];
				break;

			  case 'i':	/* interrupt character */
				if (optarg[0] == '^' && optarg[1] != '\0')
				{
					if (optarg[1] == '?')
						Intr_char = '\177';
					else
						Intr_char = CNTL(optarg[1]);
				}
				else
					Intr_char = optarg[0];
				break;

			  case 'k':	/* kill character */
				if (optarg[0] == '^' && optarg[1] != '\0')
				{
					if (optarg[1] == '?')
						Kill_char = '\177';
					else
						Kill_char = CNTL(optarg[1]);
				}
				else
					Kill_char = optarg[0];
				break;

			  case 'm':	/* map identifier to type */
				add_mapping(optarg);
				break;

			  case 'h':	/* don't get type from env */
				NoTermFromEnv = YES;
				break;

			  case 's':	/* output setenv commands */
				DoSetenv = YES;
				CmndLine = YES;
				break;

			  case 'S':	/* output setenv strings */
				DoSetenv = YES;
				CmndLine = NO;
				break;

			  case 'Q':	/* be quiet */
				BeQuiet = YES;
				break;

			  case 'I':	/* no initialization */
				NoInit = YES;
				break;

			  default:
				usage();
			}
	}

	if (optind < argc && !strcmp(argv[optind], "-"))
	{
		Report = YES;
		if (argc == 2)
			RepOnly = YES;
		++optind;
	}

	if (optind < argc)
	{
		/* Terminal type. */
		DefType = argv[optind];
		++optind;
	}

	if (optind < argc)
	{
		fprintf(stderr, "%s: extra arguments\n", program_name);
		usage();
	}

	get_tty_type();

	get_termcap_entry();

	if (!RepOnly)
	{
		set_control_chars();

		set_conversions(command);

		columns = tgetnum("co");
		lines = tgetnum("li");

#ifdef TIOCGWINSZ
		/* Set window size */
		(void) ioctl(FILEDES, TIOCGWINSZ, (char *)&win);
		if (win.ws_row == 0 && win.ws_col == 0 &&
		    lines > 0 && columns > 0) {
			win.ws_row = lines;
			win.ws_col = columns;
			(void) ioctl(FILEDES, TIOCSWINSZ, (char *)&win);
		}
#endif

		if (!NoInit)
			output_initializations();

		set_mode(0);	/* set new modes, if they've changed */

		/* set up environment for the shell we are using
		   (this code is rather heuristic, checking for $SHELL
		   ending in the 3 characters "csh") */
		if (DoSetenv)
		{
			char *sh;

			sh = getenv("SHELL");
			if (sh != NULL)
			{
				i = strlen(sh);
				if (i >= 3)
				{
					csh = !strcmp(&sh[i - 3], "csh");
					if (csh && CmndLine)
					  puts("set noglob;");
				}
			}
			if (!csh)
			    /* running Bourne shell */
			    puts("export TERMCAP TERM;");
		}
	}

	/* report type if appropriate */
	if (DoSetenv || Report || Ureport)
	{
		/* If 'TtyType' is the short name, use the first alias
		   (if any) instead. */
		makealias(Ttycap);
		if (!strcmp(TtyType, Alias[0]) && Alias[1])
			TtyType = Alias[1];

		if (DoSetenv)
		{
			if (csh)
			{
				if (CmndLine)
				    fputs("setenv TERM ", stdout);
				fputs(TtyType, stdout);
				putchar(' ');
				if (CmndLine)
				    puts(";");
			}
			else
			{
				fputs("TERM=", stdout);
				fputs(TtyType, stdout);
				puts(";");
			}
		}
		else if (Report)
		{
			puts(TtyType);
		}
		if (Ureport)
		{
			prs("Terminal type is ");
			prs(TtyType);
			prs("\n");
			flush();
		}

		if (DoSetenv)
		{
			if (csh)
			{
			    if (CmndLine)
				fputs("setenv TERMCAP '", stdout);
			}
			else
			    fputs("TERMCAP='", stdout);
			fflush(stdout);
			wrtermcap(Ttycap);
			if (csh)
			{
				if (CmndLine)
				{
				    puts("';\nunset noglob;");
				}
			}
			else
				puts("';");
		}
	}

	if(!RepOnly && !BeQuiet) {
	/* tell about changing erase, kill and interrupt characters */
	reportek("Erase", curerase, olderase, CERASE);
	reportek("Kill", curkill, oldkill, CKILL);
	reportek("Interrupt", curintr, oldintr, CINTR);
	}

	exit(0);
}

/* Syntax for P: [port-type][test baudrate]:terminal-type
   The baud rate tests are: > < @/= ! 
   The baudrate number can be preceded by a 'B', which is ignored.
   The baudrate number can also be `exta' or `extb'.
   This code is very loose. Almost no syntax checking is done!
   However, invalid syntax will only produce weird results. */

void
add_mapping(p)
char *p;
{
	int Break = NO;
	int Not = NO;

	if (isalnum (*p))
	{
		Map->Ident = p;	/* Port-type identifier. */
		while (isalnum (*p))
		  p++;
	}
	else
		Map->Ident = "";

	/* Scan for optional test and baudrate. */
	while (Break == NO)
	{
		switch (*p)
		{
		      case '\0':
			Break = YES;
			continue;
	  
		      case ':':	/* Mapped type. */
			*p++ = '\0';
			Break = YES;
			continue;
	  
		      case '>':	/* Conditional. */
			Map->Test |= GT;
			*p++ = '\0';
			continue;
	  
		      case '<':	/* Conditional. */
			Map->Test |= LT;
			*p++ = '\0';
			continue;
	  
		      case '=':	/* Conditional. */
		      case '@':
			Map->Test |= EQ;
			*p++ = '\0';
			continue;
	  
		      case '!':	/* Invert conditions. */
			Not = ~Not;
			*p++ = '\0';
			continue;
	  
		      case 'B':	/* Baud rate */
			p++;
			/* Intentional fallthru. */

		      default:
			if (isdigit(*p) || *p == 'e')
			{
				Map->Speed = baudrate (p);
				while (isalnum(*p) || *p == '.')
				  p++;
			}
			else
				Break = YES;
			continue;
		}
	}
	if (Not)		/* Invert sense of test. */
		Map->Test = (~(Map->Test)) & ALL;
	Map->Type = p;
	Map++;
	Mapped = YES;
}

/* Reset the terminal mode bits to a sensible state.
   Very useful after crapping out in raw mode. */

void
reset_mode()
{
	tcgetattr(FILEDES, &mode);
	curintr = CHK(curintr, CINTR);
	mode.c_cc[VQUIT] = CHK(mode.c_cc[VQUIT], CQUIT);
	curerase = CHK(curerase, CERASE);
	curkill = CHK(curkill, CKILL);
	mode.c_cc[VEOF] = CHK(mode.c_cc[VEOF], CEOF);
	mode.c_cc[VSTART] = CHK(mode.c_cc[VSTART], CSTART);
	mode.c_cc[VSTOP] = CHK(mode.c_cc[VSTOP], CSTOP);
	mode.c_cc[VSUSP] = CHK(mode.c_cc[VSUSP], CSUSP);
#if defined(VREPRINT) && defined(CRPRNT)
	mode.c_cc[VREPRINT] = CHK(mode.c_cc[VREPRINT], CRPRNT);
#endif
#if defined(VWERASE) && defined(CWERASE)
	mode.c_cc[VWERASE] = CHK(mode.c_cc[VWERASE], CWERASE);
#endif
#if defined(VLNEXT) && defined(CLNEXT)
	mode.c_cc[VLNEXT] = CHK(mode.c_cc[VLNEXT], CLNEXT);
#endif
#if defined(VFLUSH) && defined(CFLUSH)
	mode.c_cc[VFLUSH] = CHK(mode.c_cc[VFLUSH], CFLUSH);
#endif
#if defined(VDISCARD) && defined(CDISCARD)
	mode.c_cc[VDISCARD] = CHK(mode.c_cc[VDISCARD], CDISCARD);
#endif
	mode.c_iflag &= ~(IGNBRK | PARMRK | INPCK | ISTRIP | INLCR | IGNCR
#ifdef IUCLC
			  | IUCLC
#endif
#ifdef IXANY
			  | IXANY
#endif
			  | IXOFF);
	mode.c_iflag |= (BRKINT | IGNPAR | ICRNL | IXON
#ifdef IMAXBEL
			 | IMAXBEL
#endif
			 );
	mode.c_oflag &= ~(0
#ifdef OLCUC
			  | OLCUC
#endif
#ifdef OCRNL
			  | OCRNL
#endif
#ifdef ONOCR
			  | ONOCR
#endif
#ifdef ONLRET
			  | ONLRET
#endif
#ifdef OFILL
			  | OFILL
#endif
#ifdef OFDEL
			  | OFDEL
#endif
#ifdef NLDLY
			  | NLDLY | CRDLY | TABDLY | BSDLY | VTDLY | FFDLY
#endif
			  );
	mode.c_oflag |= (OPOST
#ifdef ONLCR
			 | ONLCR
#endif
			 );
	mode.c_cflag &= ~(CSIZE | CSTOPB | PARENB | PARODD | CLOCAL);
	mode.c_cflag |= (CS8 | CREAD);
	mode.c_lflag &= ~(ECHONL | NOFLSH | TOSTOP
#ifdef ECHOPTR
			  | ECHOPRT
#endif
#ifdef XCASE
			  | XCASE
#endif
			  );
	mode.c_lflag |= (ISIG | ICANON | ECHO | ECHOE | ECHOK
#ifdef ECHOCTL
			 | ECHOCTL
#endif
#ifdef ECHOKE
			 | ECHOKE
#endif
 			 );
	tcsetattr(FILEDES, TCSADRAIN, &mode);
}

/* Determine the terminal type and place it in 'TtyType'. */

void
get_tty_type()
{
	char *bufp;

	if (DefType)
	{
		if (Mapped)
		{
			Map->Ident = "";	/* means "map any type" */
			Map->Test = ALL;	/* at all baud rates */
			Map->Type = DefType;	/* to the default type */
		}
		else
			TtyType = DefType;
	}

	/* Get rid of $TERMCAP, if it's there, so we get a real
	   entry from /etc/termcap.  This prevents us from being
	   fooled by out of date stuff in the environment. */
	bufp = getenv("TERMCAP");
	if (bufp != NULL && *bufp != '/')
		strcpy(bufp - 8, "NOTHING"); /* Overwrite only "TERMCAP". */

	/* Get current idea of terminal type from environment. */
	if (!NoTermFromEnv && TtyType == NULL)
		TtyType = getenv("TERM");

	/* Determine terminal id if needed. */
	if (!RepOnly && TtyPath == NULL && (TtyType == NULL || !NoTermFromEnv))
		TtyPath = ttyname(FILEDES);

	/* If still undefined, look at /etc/ttytype. */
	if (TtyType == NULL)
		TtyType = stypeof(TtyPath);

	/* If still undefined, use DEFTYPE. */
	if (TtyType == NULL)
		TtyType = DEFTYPE;

	/* Check for dialup or other mapping. */
	if (Mapped)
	{
		if (Alias[0] == NULL || !isalias(TtyType))
		{
			if (tgetent(Capbuf, TtyType) > 0)
			  makealias(Capbuf);
		}
		TtyType = mapped(TtyType);
	}

	/* TtyType now contains a pointer to the type of the terminal.
	   If the first character is '?', ask the user. */
	if (TtyType[0] == '?')
	{
		Ask = YES;
		TtyType++;
		if (TtyType[0] == '\0')
		  TtyType = DEFTYPE;
	}
}

/* Query the user for TtyType if needed, then
   read the termcap entry for TtyType into Capbuf and TTycap. */

void
get_termcap_entry()
{
	static char termbuf[32];
	int i;

	if (Ask)
	{
	      ask:
		prs("TERM = (");
		prs(TtyType);
		prs(") ");
		flush();

		/* Read the terminal.  If not empty, set type. */
		i = read(2, termbuf, sizeof termbuf - 1);
		if (i > 0)
		{
			if (termbuf[i - 1] == '\n')
				i--;
			termbuf[i] = '\0';
			if (termbuf[0] != '\0')
				TtyType = termbuf;
		}
	}

	/* Get terminal capabilities. */
	if (Alias[0] == NULL || !isalias(TtyType))
	{
		switch (tgetent(Capbuf, TtyType))
		{
		      case -1:
			prs(program_name);
			prs(": cannot find termcap\n");
			flush();
			exit(2);

		      case 0:
			prs(program_name);
			prs(": type ");
			prs(TtyType);
			prs(" is unknown\n");
			flush();
			if (!DoSetenv)
				exit(1);
			TtyType = DEFTYPE;
			Alias[0] = NULL;
			goto ask;
		}
	}
	Ttycap = Capbuf;
}

/* Determine the erase, interrupt, and kill characters
   from the termcap entry and command line
   and update their values in 'mode'. */

void
set_control_chars()
{
	char buf[CAPBUFSIZ];
	char *bufp;
	char bs_char;
	char *p;

	if (Specialerase && !tgetflag("bs"))
		Erase_char = 0;
	bufp = buf;
	p = tgetstr("kb", &bufp);
	if (p == NULL || p[1] != '\0')
		p = tgetstr("bc", &bufp);
	if (p != NULL && p[1] == '\0')
		bs_char = p[0];
	else if (tgetflag("bs"))
		bs_char = BACKSPACE;
	else
		bs_char = 0;
	if (Erase_char == 0 && !tgetflag("os") && curerase == CERASE)
	{
		if (tgetflag("bs") || bs_char != 0)
			Erase_char = -1;
	}
	if (Erase_char < 0)
		Erase_char = (bs_char != 0) ? bs_char : BACKSPACE;
  
	if (curerase == 0)
		curerase = CERASE;
	if (Erase_char != 0)
		curerase = Erase_char;
  
	if (curintr == 0)
		curintr = CINTR;
	if (Intr_char != 0)
		curintr = Intr_char;
  
	if (curkill == 0)
		curkill = CKILL;
	if (Kill_char != 0)
		curkill = Kill_char;
}

/* Set up various conversions in 'mode', including
   parity, tabs, returns, echo, and case, according to
   the termcap entry.
   if 'command' starts with uppercase, map external uppercase
   to internal lowercase. */

void
set_conversions(command)
char *command;
{
	if (tgetflag("UC") || (command[0] & 0140) == 0100)
	{
#ifdef IUCLC
		mode.c_iflag |= IUCLC;
		mode.c_oflag |= OLCUC;
#endif
	}
	else if (tgetflag("LC"))
	{
#ifdef IUCLC
		mode.c_iflag &= ~IUCLC;
		mode.c_oflag &= ~OLCUC;
#endif
	}
	mode.c_iflag &= ~(PARMRK | INPCK);
	mode.c_lflag |= ICANON;
	if (tgetflag("EP"))
	{
		mode.c_cflag |= PARENB;
		mode.c_cflag &= ~PARODD;
	}
	if (tgetflag("OP"))
	{
		mode.c_cflag |= PARENB;
		mode.c_cflag |= PARODD;
	}
  
#ifdef ONLCR
	mode.c_oflag |= ONLCR;
#endif
	mode.c_iflag |= ICRNL;
	mode.c_lflag |= ECHO;
	mode.c_oflag |= OXTABS;
	if (tgetflag("NL"))
	{
		/* Newline, not linefeed. */
#ifdef ONLCR
		mode.c_oflag &= ~ONLCR;
#endif
		mode.c_iflag &= ~ICRNL;
	}
	if (tgetflag("HD"))
		/* Half duplex. */
		mode.c_lflag &= ~ECHO;
	if (tgetflag("pt"))
		/* Print tabs. */
		mode.c_oflag &= ~OXTABS;
	mode.c_lflag |= (ECHOE | ECHOK);
}

/*
 * Set the hardware tabs on the terminal, using the ct (clear all tabs),
 * st (set one tab) and ch (horizontal cursor addressing) capabilities.
 * This is done before if and is, so they can patch in case we blow this.
 * Return nonzero if we set any tab stops, zero if not.
 */
int
settabs()
{
	char caps[100];
	char *capsp = caps;
	char *clear_tabs, *set_tab, *set_column, *set_pos = NULL;
	char *tg_out;
	int c;

	clear_tabs = tgetstr("ct", &capsp);
	set_tab = tgetstr("st", &capsp);
	set_column = tgetstr("ch", &capsp);
	if (set_column == NULL)
		set_pos = tgetstr("cm", &capsp);

	if (clear_tabs && set_tab) {
		prc('\r');	/* force to be at left margin */
		tputs(clear_tabs, 0, prc);
	}
	if (set_tab) {
		for (c=8; c<columns; c += 8) {
			/* get to that column. */
			tg_out = "OOPS";	/* also returned by tgoto */
			if (set_column)
				tg_out = tgoto(set_column, 0, c);
			if (*tg_out == 'O' && set_pos)
				tg_out = tgoto(set_pos, c, lines-1);
			if (*tg_out != 'O')
				tputs(tg_out, 1, prc);
			else {
				prs("        ");
			}
			/* set the tab */
			tputs(set_tab, 0, prc);
		}
		prc('\r');
		return 1;
	}
	return 0;
}

/* Output startup string. */

void
output_initializations()
{
	char buf[CAPBUFSIZ];
	char *bufp;
	int settle = NO;

	/* Get pad character. */
	bufp = buf;
	if (tgetstr("pc", &bufp) != 0)
		PC = buf[0];

#ifdef TAB3
	if (oldmode.c_oflag & (TAB3 | ONLCR | OCRNL | ONLRET))
	{
		oldmode.c_oflag &= (TAB3 | ONLCR | OCRNL | ONLRET);
		set_mode(-1);
	}
#endif
	if (settabs())
	{
		settle = YES;
		flush ();
	}
	bufp = buf;
	if (IsReset && tgetstr("rs", &bufp) != 0 || tgetstr("is", &bufp) != 0)
	{
		tputs(buf, 0, prc);
		settle = YES;
		flush();
	}
	bufp = buf;
	if (IsReset && tgetstr("rf", &bufp) != 0 || tgetstr("if", &bufp) != 0)
	{
		cat(buf);
		settle = YES;
	}
  
	if (settle)
	{
		prc('\r');
		flush();
		sleep(1);	/* Let the terminal settle down. */
	}
}

/* Set the terminal processing according to 'mode' or 'oldmode'.
   If called as the result of a signal, flag is > 0.
   If called from terminal init, flag == -1 means reset 'oldmode'.
   If called at end of normal mode processing, flag == 0. */

void
set_mode(flag)
int	flag;
{
	if (flag < 0)
		tcsetattr(FILEDES, TCSADRAIN, &oldmode);
	else if (memcmp((char *) &mode, (char *) &oldmode, sizeof mode))
		tcsetattr(FILEDES, TCSADRAIN, &mode);

	if (flag > 0)	/* trapped signal */
		exit(1);
}

/* Tell the user the value that a control key is set to,
   if it has been changed from the default value.
   'new', 'old', and 'def' are the current, previous, and default
   ASCII values for the control key called 'name' in English. */

void
reportek(name, new, old, def)
char	*name;
unsigned char	new;
unsigned char	old;
unsigned char	def;
{
	register unsigned char	o;
	register unsigned char	n;
	char		buf[CAPBUFSIZ];
	char		*bufp;

	o = old;
	n = new;

	if (o == n && n == def)
		return;
	prs(name);
	if (o == n)
		prs(" is ");
	else
		prs(" set to ");
	bufp = buf;
	if (tgetstr("kb", &bufp) && n == buf[0] && buf[1] == '\0')
		prs("Backspace\n");
	else if (n == 0177)
		prs("Delete\n");
	else
	{
		if (n < 040)
		{
			prs("Ctrl-");
			n ^= 0100;
		}
		prc(n);
		prc('\n');
	}
	flush();
}

/* Print string 's' to stderr, buffered. */

void
prs(s)
char	*s;
{
	while (*s != '\0')
		prc(*s++);
}

char	OutBuf[512];
int	OutPtr;

/* Print character 'c' to stderr, buffered. */

int
prc(c)
char	c;
{
	OutBuf[OutPtr++] = c & 0x7f;
	if (OutPtr >= sizeof OutBuf)
		flush();
	return c;
}

/* Write out and empty the buffer used by prs and prc. */

void
flush()
{
	if (OutPtr > 0)
		(void) write(2, OutBuf, OutPtr);
	OutPtr = 0;
}

/* Print file 'file' to STDERR. */

void
cat(file)
char	*file;
{
	register int	fd;
	register int	i;
	char		buf[BUFSIZ];

	fd = open(file, 0);
	if (fd < 0)
	{
		prs(program_name);
		prs(": cannot open ");
		prs(file);
		prs("\n");
		flush();
		return;
	}

	while ((i = read(fd, buf, BUFSIZ)) > 0)
		(void) write(2, buf, i);

	(void) close(fd);
}

/* Put each of the alternative names for the terminal
   described by the termcap entry 'buf' into an element of 'Alias'.
   Make the final element a NULL. */

void
makealias(buf)
char	*buf;
{
	register int i;		/* Index into 'Alias'. */
	register char *a;	/* Pointer into 'Aliasbuf'. */
	register char *b;	/* Pointer into 'buf'. */

	Alias[0] = a = Aliasbuf;
	b = buf;
	i = 1;
	while (*b && *b != ':') {
		if (*b == '|') {
			*a++ = '\0';
			Alias[i++] = a;
			b++;
		}
		else
			*a++ = *b++;
	}
	*a = '\0';
	Alias[i] = NULL;
}

/* Return YES if 'ident' is the same as one of the aliases,
   NO if not. */

int
isalias(ident)
char	*ident;
{
	char **a;

	for (a = Alias; *a; ++a)
		if (!strcmp(ident, *a))
			return YES;
	return NO;
}

/* Return the default type of terminal for line 'ttyid'. */

char *
stypeof(ttyid)
char	*ttyid;
{
	register char	*PortType;
	register char	*TtyId;
	struct ttyent *t;

	if (ttyid == NULL)
		return (DEFTYPE);

	/* Set TtyId to the basename of ttyid. */
	TtyId = ttyid;
	while (*ttyid)
		if (*ttyid++ == '/')
			TtyId = ttyid;

	t = getttynam(TtyId);
	if (t == NULL)
		return DEFTYPE;

	PortType = t->ty_type;
	/* get aliases from termcap entry */
	if (Mapped && tgetent(Capbuf, PortType) > 0) {
		makealias(Capbuf);
		if (!strcmp(Alias[0], PortType) && Alias[1])
			PortType = Alias[1];
	}
	return (PortType);
}

#define	WHITE(c)	(c == ' ' || c == '\t')

/* Output termcap entry 'bp' to stdout, quoting characters that
   would give the shell problems and omitting empty fields. */

void
wrtermcap(bp)
char *bp;
{
	char buf[CAPBUFSIZ];
	char *p = buf;
	char *tp;
	int space, empty;

	/* Copy the terminal names in 'bp' into 'buf',
	   discarding (verbose) names that contain blanks. */
	while (*bp && *bp != ':') {
		if (*bp == '|') {
			tp = bp+1;
			space = NO;
			while (*tp && *tp != '|' && *tp != ':') {
				space = (space || WHITE(*tp) );
				tp++;
			}
			if (space) {
				bp = tp;
				continue;
			}
		}
		*p++ = *bp++;
	}

	while (*bp) {
		switch (*bp) {
		case ':':	/* discard empty, cancelled  or dupl fields */
			tp = bp+1;
			empty = YES;
			while (*tp && *tp != ':') {
				empty = (empty && WHITE(*tp) );
				tp++;
			}
			if (empty || cancelled(bp+1)) {
				bp = tp;
				continue;
			}
			break;

		case ' ':	/* no spaces in output */
			p = putbuf(p, "\\040");
			bp++;
			continue;

		case '!':	/* the shell thinks this is history */
			p = putbuf(p, "\\041");
			bp++;
			continue;

		case ',':	/* the shell thinks this is history */
			p = putbuf(p, "\\054");
			bp++;
			continue;

		case '"':	/* no quotes in output */
			p = putbuf(p, "\\042");
			bp++;
			continue;

		case '\'':	/* no quotes in output */
			p = putbuf(p, "\\047");
			bp++;
			continue;

		case '`':	/* no back quotes in output */
			p = putbuf(p, "\\140");
			bp++;
			continue;

		case '\\':
		case '^':	/* anything following is OK */
			*p++ = *bp++;
		}
		*p++ = *bp++;
	}
	*p++ = ':';	/* we skipped the last : with the : lookahead hack */
	(void) write (STDOUT, buf, p-buf);
}

/* Array of termcap entries processed so far by 'cancelled',
   for omitting duplicates from the output. */
char delcap[128][2];

/* Number of valid entries in 'delcap'. */
int ncap = 0;

/* Return YES if 'cap' is a commented-out or duplicate capability,
   NO if not. */

int
cancelled(cap)
char	*cap;
{
	register int i;

	for (i = 0; i < ncap; i++)
	{
		if (cap[0] == delcap[i][0] && cap[1] == delcap[i][1])
			return (YES);
	}
	/* delete a second occurrance of the same capability */
	delcap[ncap][0] = cap[0];
	delcap[ncap][1] = cap[1];
	ncap++;
	return (cap[2] == '@');
}

/* Append a printable representation of 'str' to 'ptr' and
   return a pointer to the next available character after 'ptr'. */

char *
putbuf(ptr, str)
char	*ptr;
char	*str;
{
	char buf[20];

	while (*str) {
		switch (*str) {
		case '\033':
			ptr = putbuf(ptr, "\\E");
			str++;
			break;
		default:
			if (*str <= ' ') {
				(void) sprintf(buf, "\\%03o", *str);
				ptr = putbuf(ptr, buf);
				str++;
			} else
				*ptr++ = *str++;
		}
	}
	return (ptr);
}

/* Return the internal form of ASCII baud rate 'p',
   or -1 if 'p' does not represent a valid baud rate. */

int
baudrate(p)
char	*p;
{
	char buf[8];
	int i = 0;

	while (i < 7 && (isalnum(*p) || *p == '.'))
		buf[i++] = *p++;
	buf[i] = NULL;
	for (i=0; speeds[i].string; i++)
		if (!strcmp(speeds[i].string, buf))
			return (speeds[i].speed);
	return (-1);
}

/* Return the type of terminal to use for a port of type 'type',
   as specified by the first applicable mapping in 'map'.
   If no mappings apply, return 'type'. */

char *
mapped(type)
char	*type;
{
	int	match;

	Map = map;
	while (Map->Ident)
	{
		if (*(Map->Ident) == '\0'
		    || !strcmp(Map->Ident, type) || isalias(Map->Ident))
		{
			match = NO;
			switch (Map->Test)
			{
				case ANY:	/* no test specified */
				case ALL:
					match = YES;
					break;
				
				case GT:
					match = (ospeed > Map->Speed);
					break;

				case GE:
					match = (ospeed >= Map->Speed);
					break;

				case EQ:
					match = (ospeed == Map->Speed);
					break;

				case LE:
					match = (ospeed <= Map->Speed);
					break;

				case LT:
					match = (ospeed < Map->Speed);
					break;

				case NE:
					match = (ospeed != Map->Speed);
					break;
			}
			if (match)
				return (Map->Type);
		}
		Map++;
	}
	/* no match found; return given type */
	return (type);
}

void
usage()
{
	fprintf(stderr, "\
Usage: %s [-] [-hnrsIQS] [-eC] [-iC] [-kC] [-EC]\n\
       [-m [port-type][test baudrate]:terminal-type] [terminal-type]\n",
		program_name);
	exit(1);
}
