#
/*
**  TSET -- set terminal modes
**
**	This program does sophisticated terminal initialization.
**	I recommend that you include it in your .start_up or .login
**	file to initialize whatever terminal you are on.
**
**	There are several features:
**
**	A special file or sequence (as controlled by the ttycap file)
**	is sent to the terminal.
**
**	Mode bits are set on a per-terminal_type basis (much better
**	than UNIX itself).  This allows special delays, automatic
**	tabs, etc.
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
**	The htmp file, used by ex, etc., can be updated.
**
**	The current terminal type can be queried.
**
**	Usage:
**		tset [-] [-EC] [-eC] [-kC] [-s] [-h] [-u] 
**			[-d type] [-p type] [-b type] [-a type]
**			[-Q] [-I] [type]
**
**		In systems with environments, use:
**			`tset -s ...`
**		Actually, this doesn't work because of a shell bug.
**		Instead, use:
**			tset -s ... > tset.tmp
**			source tset.tmp
**			rm tset.tmp
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
**		-EC -- set the erase character to C on all terminals
**			except those which cannot backspace (e.g.,
**			a TTY 33).  C defaults to control-H.
**		-eC -- set the erase character to C on all terminals.
**			C defaults to control-H.  If neither -E or -e
**			are specified, the erase character is set to
**			control-H if the terminal can both backspace
**			and not overstrike (e.g., a CRT).  If the erase
**			character is NULL (zero byte), it will be reset
**			to '#' if nothing else is specified.
**		-kC -- set the kill character to C on all terminals.
**			Default for C is control-X.  If not specified,
**			the kill character is untouched; however, if
**			not specified and the kill character is NULL
**			(zero byte), the kill character is set to '@'.
**		-iC -- reserved for setable interrupt character.
**		-qC -- reserved for setable quit character.
**		-d type -- set the dialup type to be type.  If the
**			terminal type seems to be dialup, make it
**			'type' instead.  There need not be a space
**			between 'd' and 'type'.
**		-p type -- ditto for a plugboard.
**		-b type -- ditto for a bussiplexor.
**		-a type -- ditto for an arpanet link.
**		-h -- don't read htmp file.  Normally the terminal type
**			is determined by reading the htmp file (unless
**			-d or -p are specified).  This forces a read
**			of the ttytype file -- useful when htmp is
**			somehow wrong.
**		-u -- don't update htmp.  It seemed like this should
**			be put in.  Note that htmp is never actually
**			written if there are no changes, so don't bother
**			bother using this for efficiency reasons alone.
**		-s -- output setenv commands for TERM.  This can be
**			used with
**				`tset -s ...`
**			and is to be prefered to:
**				setenv TERM `tset - ...`
**			because -s sets the TERMCAP variable also.
**		-Q -- be quiet.  don't output 'Erase set to' etc.
**		-I -- don't do terminal initialization (is & if
**			strings).
**
**	Files:
**		/etc/ttytype
**			contains a terminal id -> terminal type
**			mapping; used when -h, -d, or -p is used.
**		/etc/termcap
**			a terminal_type -> terminal_capabilities
**			mapping.
**
**	Return Codes:
**		-1 -- couldn't open ttycap.
**		1 -- bad terminal type, or standard output not tty.
**		0 -- ok.
**
**	Defined Constants:
**		DIALUP -- the type code for a dialup port
**		PLUGBOARD -- the code for a plugboard port.
**		BUSSIPLEXER -- the code for a bussiplexer port.
**		ARPANET -- the code for an arpanet port.
**		BACKSPACE -- control-H, the default for -e.
**		CONTROLX -- control-X, the default for -k.
**		OLDERASE -- the system default erase character.
**		OLDKILL -- the system default kill character.
**		FILEDES -- the file descriptor to do the operation
**			on, nominally 1 or 2.
**		STDOUT -- the standard output file descriptor.
**		UIDMASK -- the bit pattern to mask with the getuid()
**			call to get just the user id.
**
**	Requires:
**		Routines to handle htmp, ttytype, and ttycap.
**
**	Compilation Flags:
**		PLUGBOARD -- if defined, accept the -p flag.
**		BUSSIPLEXER -- if defined, accept the -b flag.
**		FULLLOGIN -- if defined, login sets the ttytype from
**			/etc/ttytype file.
**		V6 -- if clear, use environments, not htmp.
**			also use TIOCSETN rather than stty to avoid flushing
**		GTTYN -- if set, uses generalized tty names.
**
**	Trace Flags:
**		none
**
**	Diagnostics:
**		Bad flag
**			An incorrect option was specified.
**		Cannot open ...
**			The specified file could not be openned.
**		Type ... unknown
**			An unknown terminal type was specified.
**		Cannot update htmp
**			Cannot update htmp file when the standard
**			output is not a terminal.
**		Erase set to ...
**			Telling that the erase character has been
**			set to the specified character.
**		Kill set to ...
**			Ditto for kill
**		Erase is ...    Kill is ...
**			Tells that the erase/kill characters were
**			wierd before, but they are being left as-is.
**		Not a terminal
**			Set if FILEDES is not a terminal.
**
**	Compilation Instructions:
**		cc -n -O tset.c -lX
**		mv a.out tset
**		chown bin tset
**		chmod 4755 tset
**
**		where 'bin' should be whoever owns the 'htmp' file.
**		If 'htmp' is 666, then tset need not be setuid.
**
**	Author:
**		Eric Allman
**		Electronics Research Labs
**		U.C. Berkeley
**
**	History:
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
**			FILEDES.  FULLLOGIN flag added.
**		9/78 -- '-' and '-p' options added (now fully
**			compatible with ttytype!), and spaces are
**			permitted between the -d and the type.
**		8/78 -- The sense of -h and -u were reversed, and the
**			-f flag is dropped -- same effect is available
**			by just stating the terminal type.
**		10/77 -- Written.
*/

/*
# define	FULLLOGIN	1
*/
# ifndef V6
# define	GTTYN		1
# endif

# include	<sgtty.h>
# include	<stdio.h>

# define	BACKSPACE	('H' & 037)
# define	CONTROLX	('X' & 037)
# define	OLDERASE	'#'
# define	OLDKILL		'@'

# define	FILEDES		2
# define	STDOUT		1

# ifdef V6
# define	UIDMASK		0377
# else
# define	UIDMASK		-1
# endif

# define	DIALUP		"sd"
# define	PLUGBOARD	"sp"
# define	BUSSIPLEXER	"sb"
# define	ARPANET		"sa"

# ifdef GTTYN
typedef char	*ttyid_t;
# define	NOTTY		0
# else
typedef char	ttyid_t;
# define	NOTTY		'x'
# endif






char	Erase_char;		/* new erase character */
char	Kill_char;		/* new kill character */
char	Specialerase;		/* set => Erase_char only on terminals with backspace */

ttyid_t	Ttyid = NOTTY;		/* terminal identifier */
char	*TtyType;		/* type of terminal */
char	*DefType;		/* default type if none other computed */
char	*DialType;		/* override type if dialup port */
char	*PlugType;		/* override type if plugboard port */
char	*BxType;		/* override type if bussiplexer port */
char	*AnType;		/* override type if arpanet port */
int	Dash_u;			/* don't update htmp */
int	Dash_h;			/* don't read htmp */
int	DoSetenv;		/* output setenv commands */
int	BeQuiet;		/* be quiet */
int	NoInit;			/* don't output initialization string */
int	Report;			/* report current type */
int	Ureport;		/* report to user */
int	RepOnly;		/* report only */

char	Usage[] = "usage: tset [-] [-r] [-s] [-eC] [-kC] [-d T] [-p T] [-b T] [-a T]\n\t[-Q] [-I] [-h] [-u] [type]\n";

char	Capbuf[1024];		/* line from /etc/termcap for this TtyType */
char	*Ttycap;		/* termcap line from termcap or environ */

struct delay
{
	int	d_delay;
	int	d_bits;
};

# include	"tset.delays.h"



main(argc, argv)
int	argc;
char	*argv[];
{
	struct sgttyb	mode;
	struct sgttyb	oldmode;
	char		buf[256];
	auto char	*bufp;
	register char	*p;
	char		*command;
	register int	i;
	register int	error;
	extern char	*stypeof();
# ifdef V6
	extern char	*hsgettype();
# else
	extern char	*getenv();
# endif
# ifdef GTTYN
	extern char	*ttyname();
	extern char	*tgetstr();
# endif
	char		bs_char;
	int		csh;

	/* scan argument list and collect flags */
	error = 0;
	command = argv[0];
	if (argc == 2 && argv[1][0] == '-' && argv[1][1] == '\0')
		RepOnly++;
	argc--;
	while (--argc >= 0)
	{
		p = *++argv;
		if (p[0] == '-')
		{
			switch (p[1])
			{

			  case 0:	/* report current terminal type */
				Report++;
				continue;

			  case 'r':	/* report to user */
				Ureport++;
				continue;

			  case 'E':	/* special erase: operate on all but TTY33 */
				Specialerase++;
				/* explicit fall-through to -e case */

			  case 'e':	/* erase character */
				if (p[2] == 0)
					Erase_char = -1;
				else if (p[2] == '^' && p[3] != '\0')
					Erase_char = p[3] & 037;
				else
					Erase_char = p[2];
				continue;

			  case 'k':	/* kill character */
				if (p[2] == 0)
					Kill_char = CONTROLX;
				else if (p[2] == '^' && p[3] != '\0')
					Kill_char = p[3] & 037;
				else
					Kill_char = p[2];
				continue;

			  case 'd':	/* dialup type */
				if (p[2] != 0)
					DialType = &p[2];
				else if (--argc < 0 || argv[1][0] == '-')
					error++;
				else
					DialType = *++argv;
				continue;

# ifdef PLUGBOARD
			  case 'p':	/* plugboard type */
				if (p[2] != 0)
					PlugType = &p[2];
				else if (--argc < 0 || argv[1][0] == '-')
					error++;
				else
					PlugType = *++argv;
				continue;
# endif

# ifdef BUSSIPLEXER
			  case 'b':	/* bussiplexer type */
				if (p[2] != 0)
					BxType = &p[2];
				else if (--argc < 0 || argv[1][0] == '-')
					error++;
				else
					BxType = *++argv;
# endif

# ifdef ARPANET
			  case 'a':	/* arpanet type */
				if (p[2] != 0)
					AnType = &p[2];
				else if (--argc < 0 || argv[1][0] == '-')
					error++;
				else
					AnType = *++argv;
# endif

			  case 'h':	/* don't get type from htmp or env */
				Dash_h++;
				continue;

			  case 'u':	/* don't update htmp */
				Dash_u++;
				continue;
			  
			  case 's':	/* output setenv commands */
				DoSetenv++;
				continue;
			  
			  case 'Q':	/* be quiet */
				BeQuiet++;
				continue;
			  
			  case 'I':	/* no initialization */
				NoInit++;
				continue;

			  default:
				prs("Bad flag ");
				prs(p);
				prs("\n");
				error++;

			}
		}
		else
		{
			/* terminal type */
			DefType = p;
		}
	}

	if (error)
	{
		prs(Usage);
		exit(1);
	}

	/* if dialup is specified, check ttytype not htmp */
	if (DialType == 0 && PlugType == 0 && BxType == 0 && AnType == 0)
		TtyType = DefType;
# ifndef FULLLOGIN
	else
		Dash_h++;
# endif
	
	/* if report only, say that we won't update htmp */
	if (RepOnly)
		Dash_u++;

# ifndef V6
	/* get current idea of terminal type from environment */
	if (!Dash_h && TtyType == 0)
		TtyType = getenv("TERM");
# endif

	/* determine terminal id if needed */
# ifdef V6
	if (Ttyid == NOTTY && (TtyType == 0 || !Dash_h || !Dash_u))
		Ttyid = ttyn(FILEDES);
# else
	if (!RepOnly && Ttyid == NOTTY && (TtyType == 0 || !Dash_h))
		Ttyid = ttyname(FILEDES);
# endif

# ifdef V6
	/* get htmp if ever used */
	if (!Dash_u || (TtyType == 0 && !Dash_h))
	{
		/* get htmp entry -- if error or wrong user use ttytype */
		if (Ttyid == NOTTY || hget(Ttyid) < 0 ||
		    hgettype() == 0 || hgetuid() != (getuid() & UIDMASK))
			Dash_h++;
	}
# endif

	/* find terminal type (if not already known) */
	if (TtyType == 0)
	{
		/* get type from /etc/ttytype or /etc/htmp */
		if (!Dash_h)
		{
# ifdef V6
			TtyType = hsgettype();
# else
			TtyType = getenv("TERM");
# endif
		}
		if (TtyType == 0)
		{
			TtyType = stypeof(Ttyid);
		}

		/* check for dialup or plugboard override */
		if (DialType != 0 && bequal(TtyType, DIALUP, 2))
			TtyType = DialType;
# ifdef PLUGBOARD
		else if (PlugType != 0 && bequal(TtyType, PLUGBOARD, 2))
			TtyType = PlugType;
# endif
# ifdef BUSSIPLEXER
		else if (BxType != 0 && bequal(TtyType, BUSSIPLEXER, 2))
			TtyType = BxType;
# endif
# ifdef ARPANET
		else if (AnType != 0 && bequal(TtyType, ARPANET, 2))
			TtyType = AnType;
# endif
		else if (DefType != 0)
			TtyType = DefType;
	}

	/* TtyType now contains a pointer to the type of the terminal */
	/* If the first character is '?', ask the user */
	if (TtyType[0] == '?')
	{
		if (*++TtyType == '\0')
			TtyType = "dumb";
		prs("TERM = (");
		prs(TtyType);
		prs(") ");

		/* read the terminal.  If not empty, set type */
		i = read(2, buf, sizeof buf - 1);
		if (i >= 0)
		{
			if (buf[i - 1] == '\n')
				i--;
			buf[i] = '\0';
			if (buf[0] != '\0')
				TtyType = buf;
		}
	}

	if (Ttycap == 0)
	{
		/* get terminal capabilities */
		switch (tgetent(Capbuf, TtyType))
		{
		  case -1:
			prs("Cannot open termcap file\n");
			exit(-1);

		  case 0:
			prs("Type ");
			prs(TtyType);
			prs(" unknown\n");
			exit(1);
		}
		Ttycap = Capbuf;
	}

	/* output startup string */
	if (!RepOnly && !NoInit)
	{
		bufp = buf;
		if (tgetstr("is", &bufp) != 0)
			prs(buf);
		bufp = buf;
		if (tgetstr("if", &bufp) != 0)
			cat(buf);
		sleep(1);	/* let terminal settle down */
	}

	/* set up environment for the shell we are using */
	/* (this code is rather heuristic) */
	csh = 0;
	if (DoSetenv)
	{
# ifndef V6
		if (bequal(getenv("SHELL"), "/bin/csh", 9))
		{
# endif
			/* running csh */
			csh++;
			write(STDOUT, "set noglob;\n", 12);
# ifndef V6
		}
		else
		{
			/* running system shell */
			write(STDOUT, "export TERMCAP TERM;\n", 21);
		}
# endif
	}

	/* report type if appropriate */
	if (DoSetenv || Report || Ureport)
	{
		/* find first alias (if any) */
		for (p = Ttycap; *p != 0 && *p != '|' && *p != ':'; p++)
			continue;
		if (*p == 0 || *p == ':')
			p = Ttycap;
		else
			p++;
		bufp = p;
		while (*p != '|' && *p != ':' && *p != 0)
			p++;
		i = *p;
		if (DoSetenv)
		{
			if (csh)
				write(STDOUT, "setenv TERM ", 12);
			else
				write(STDOUT, "TERM=", 5);
		}
		if (Report || DoSetenv)
		{
			write(STDOUT, bufp, p - bufp);
			if (DoSetenv)
				write(STDOUT, ";", 1);
			write(STDOUT, "\n", 1);
		}
		if (Ureport)
		{
			*p = '\0';
			prs("Terminal type is ");
			prs(bufp);
			prs("\n");
		}
		*p = i;
		if (DoSetenv)
		{
			for (p = Ttycap; *p != '\0'; p++)
				continue;
			if (csh)
				write(STDOUT, "setenv TERMCAP '", 16);
			else
				write(STDOUT, "TERMCAP='", 9);
			write(STDOUT, Ttycap, p - Ttycap);
			write(STDOUT, "';\n", 3);

			/* reset noglob */
			if (csh)
				write(STDOUT, "unset noglob;\n", 14);
		}
	}

	/* exit if report only mode */
	if (RepOnly)
		exit(0);

	if (gtty(FILEDES, &mode) < 0)
	{
		prs("Not a terminal\n");
		exit(1);
	}
	bmove(&mode, &oldmode, sizeof mode);

	/* determine erase and kill characters */
	if (Specialerase && !tgetflag("bs"))
		Erase_char = 0;
	bufp = buf;
	p = tgetstr("bc", &bufp);
	if (p != NULL)
		bs_char = p[0];
	else if (tgetflag("bs"))
		bs_char = BACKSPACE;
	else
		bs_char = 0;
	if (Erase_char == 0 && !tgetflag("os"))
	{
		if (tgetflag("bs") || bs_char != 0)
			Erase_char = -1;
	}
	if (Erase_char < 0)
		Erase_char = (bs_char != 0) ? bs_char : BACKSPACE;

	if (mode.sg_erase == 0)
		mode.sg_erase = OLDERASE;
	if (Erase_char != 0)
		mode.sg_erase = Erase_char;

	if (mode.sg_kill == 0)
		mode.sg_kill = OLDKILL;
	if (Kill_char != 0)
		mode.sg_kill = Kill_char;

	/* set modes */
	setdelay("dC", CRdelay, CRbits, &mode.sg_flags);
	setdelay("dN", NLdelay, NLbits, &mode.sg_flags);
	setdelay("dB", BSdelay, BSbits, &mode.sg_flags);
	setdelay("dF", FFdelay, FFbits, &mode.sg_flags);
	setdelay("dT", TBdelay, TBbits, &mode.sg_flags);
	if (tgetflag("UC") || command[0] == 'T')
		mode.sg_flags |= LCASE;
	else if (tgetflag("LC"))
		mode.sg_flags &= ~LCASE;
	mode.sg_flags &= ~(EVENP | ODDP | RAW);
# ifndef V6
	mode.sg_flags &= ~CBREAK;
# endif
	if (tgetflag("EP"))
		mode.sg_flags |= EVENP;
	if (tgetflag("OP"))
		mode.sg_flags |= ODDP;
	if ((mode.sg_flags & (EVENP | ODDP)) == 0)
		mode.sg_flags |= EVENP | ODDP;
	mode.sg_flags |= CRMOD | ECHO | XTABS;
	if (tgetflag("NL"))	/* new line, not line feed */
		mode.sg_flags &= ~CRMOD;
	if (tgetflag("HD"))	/* half duplex */
		mode.sg_flags &= ~ECHO;
	if (tgetflag("pt"))	/* print tabs */
		mode.sg_flags &= ~XTABS;
	if (!bequal(&mode, &oldmode, sizeof mode))
# ifndef V6
		ioctl(FILEDES, TIOCSETN, &mode);
# else
		stty(FILEDES, &mode);
# endif

	/* tell about changing erase and kill characters */
	reportek("Erase", mode.sg_erase, oldmode.sg_erase, OLDERASE);
	reportek("Kill", mode.sg_kill, oldmode.sg_kill, OLDKILL);

# ifdef V6
	/* update htmp */
	if (!Dash_u)
	{
		if (Ttyid == 0)
			Ttyid = ttyn(FILEDES);
		if (Ttyid == 'x')
			prs("Cannot update htmp\n");
		else
		{
			/* update htmp file only if changed */
			if (!bequal(Capbuf, hsgettype(), 2))
			{
				hsettype(Capbuf[0] | (Capbuf[1] << 8));
				hput(Ttyid);
			}
		}
	}
# endif

	exit(0);
}


reportek(name, new, old, def)
char	*name;
char	old;
char	new;
char	def;
{
	register char	o;
	register char	n;
	register char	*p;

	if (BeQuiet)
		return;
	o = old;
	n = new;

	if (o == n && n == def)
		return;
	prs(name);
	if (o == n)
		prs(" is ");
	else
		prs(" set to ");
	if (n < 040)
	{
		prs("control-");
		n = (n & 037) | 0100;
	}
	p = "x\n";
	p[0] = n;
	prs(p);
}




setdelay(cap, dtab, bits, flags)
char		*cap;
struct delay	dtab[];
int		bits;
int		*flags;
{
	register int	i;
	register struct delay	*p;

	/* see if this capability exists at all */
	i = tgetnum(cap);
	if (i < 0)
		i = 0;

	/* clear out the bits, replace with new ones */
	*flags &= ~bits;

	/* scan dtab for first entry with adequate delay */
	for (p = dtab; p->d_delay >= 0; p++)
	{
		if (p->d_delay >= i)
		{
			p++;
			break;
		}
	}

	/* use last entry if none will do */
	*flags |= (--p)->d_bits;
}


prs(s)
char	*s;
{
	register char	*p;
	register char	*q;
	register int	i;

	p = q = s;
	i = 0;
	while (*q++ != 0)
		i++;

	if (i > 0)
		write(FILEDES, p, i);
}


cat(file)
char	*file;
{
	register int	fd;
	register int	i;
	char		buf[BUFSIZ];

	fd = open(file, 0);
	if (fd < 0)
	{
		prs("Cannot open ");
		prs(file);
		prs("\n");
		exit(-1);
	}

	while ((i = read(fd, buf, BUFSIZ)) > 0)
		write(FILEDES, buf, i);

	close(fd);
}



bmove(from, to, length)
char	*from;
char	*to;
int	length;
{
	register char	*p, *q;
	register int	i;

	i = length;
	p = from;
	q = to;

	while (i-- > 0)
		*q++ = *p++;
}



bequal(a, b, len)
char	*a;
char	*b;
int	len;
{
	register char	*p, *q;
	register int	i;

	i = len;
	p = a;
	q = b;

	while (i-- > 0)
		if (*p++ != *q++)
			return (0);
	return (1);
}

# ifdef GTTYN
char *
stypeof(ttyid)
char	*ttyid;
{
	static char	typebuf[3];
	char		buf[50];
	register char	*p;
	register FILE	*f;
	register char	*t;
	register char	*q;

	if (ttyid == NOTTY)
		return ("un");
	f = fopen("/etc/ttytype", "r");
	if (f == NULL)
		return ("un");

	/* split off end of name */
	for (p = q = ttyid; *p != 0; p++)
		if (*p == '/')
			q = p + 1;

	/* scan the file */
	while (fgets(buf, sizeof buf, f) != NULL)
	{
		for (p = q, t = &buf[3]; *p != '\0'; p++, t++)
			if (*p != *t)
				break;
		if (*p == '\0' && (*t == '\n' || *t == '\t'))
		{
			typebuf[0] = buf[0];
			typebuf[1] = buf[1];
			typebuf[2] = '\0';
			fclose(f);
			return (typebuf);
		}
	}
	fclose (f);
	return ("un");
}
# endif
