# include	<sgtty.h>
# include	<stdio.h>

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
**	is always left alone unless specifically requested.
**
**	Terminals which are dialups or plugboard types can be aliased
**	to whatever type you may have in your home or office.  Thus,
**	if you know that when you dial up you will always be on a
**	TI 733, you can specify that fact to tset.
**
**	The htmp file, used by ex, etc., can be updated.
**
**	The current terminal type can be queried.
**
**	Usage:
**		tset [-] [-r] [-EC] [-eC] [-d type] [-p type]
**			[-b type] [-h] [-u] [type]
**
**		In systems with environments, use:
**			setenv TERM `tset - ...`
**
**	Positional Parameters:
**		type -- the terminal type to force.  If this is
**			specified, initialization is for this
**			terminal type.
**
**	Flags:
**		- -- report terminal type.  Whatever type is
**			decided on is reported.
**		-r -- report to user, on diagnostic output instead
**			of standard output.
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
**		-d type -- set the dialup type to be type.  If the
**			terminal type seems to be dialup, make it
**			'type' instead.  There need not be a space
**			between 'd' and 'type'.
**		-p type -- ditto for a plugboard.
**		-b type -- ditto for a bussiplexer.
**		-h -- don't read htmp file.  Normally the terminal type
**			is determined by reading the htmp file (unless
**			-d or -p are specified).  This forces a read
**			of the ttytype file -- useful when htmp is
**			somehow wrong.  On a version seven system, this
**			flag means don't look at the TERM entry in
**			the environment.
**		-u -- don't update htmp.  It seemed like this should
**			be put in.  Note that htmp is never actually
**			written if there are no changes, so don't bother
**			bother using this for efficiency reasons alone.
**			On version seven systems this flag is ignored.
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
**		VERSION7 -- if set, use environments, not htmp.
**			Also, use 'ioctl' not 'stty' -- to get type-
**			ahead.
**		GTTYN -- if set, uses generalized tty names.
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
**		3/79 -- Use ioctl in version7.
**		12/78 -- modified for eventual migration to VAX/UNIX,
**			so the '-' option is changed to output only
**			the terminal type to STDOUT instead of
**			FILEDES.  FULLLOGIN flag added.  BUSSIPLEXER
**			and -r added.
**		9/78 -- '-' and '-p' options added (now fully
**			compatible with ttytype!), and spaces are
**			permitted between the -d and the type.
**		8/78 -- The sense of -h and -u were reversed, and the
**			-f flag is dropped -- same effect is available
**			by just stating the terminal type.
**		10/77 -- This version, in much it's previous state,
**			written by Eric Allman.
*/

# define	BACKSPACE	('H' & 037)
# define	CONTROLX	('X' & 037)
# define	OLDERASE	'#'
# define	OLDKILL		'@'

# define	FILEDES		2
# define	STDOUT		1

# define	DIALUP		"du"
# define	PLUGBOARD	"pb"
# define	BUSSIPLEXER	"bx"
/* # define	FULLLOGIN	FULLLOGIN	login does everything */
/* # define	VERSION7	VERSION7	   version seven flag */
/* # define	GTTYN		GTTYN		   general tty names */

# ifdef VERSION7
# define	UIDMASK		0177777
# else
# define	UIDMASK		0377
# endif

# ifdef GTTYN
typedef char	*ttyid_t;
# else
typedef char	ttyid_t;
# endif

# define	NOTTY		0





char	Erase_char;		/* new erase character */
char	Kill_char;		/* new kill character */
char	Specialerase;		/* set => Erase_char only on terminals with backspace */

ttyid_t	Ttyid = NOTTY;		/* terminal identifier */
char	*Ttytype;		/* type of terminal */
char	*Dialtype;		/* override type if dialup terminal */
char	*Plugtype;		/* override type if plugboard port */
char	*Bxtype;		/* override type if bussiplexer port */
int	Dash_u;			/* don't-update-htmp flag */
int	Dash_h;			/* don't-read-htmp flag */
int	Report;			/* report current type */
int	Ureport;		/* report to user */

char	Usage[] = "usage: tset [-] [-r] [-eC] [-kC] [-d T] [-p T] [-b T] [-h] [-u] [type]\n";

char	Capbuf[256];		/* line from /etc/ttycap for this Ttytype */

struct delay
{
	int	d_delay;
	int	d_bits;
};

# include	"tset.del.h"



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
	int		mdvect[2];
	extern char	*stypeof();
# ifndef VERSION7
	extern char	*hsgettype();
# else
	extern char	*getenv();
# endif
# ifdef GTTYN
	extern char	*ttyname();
# endif

	/* scan argument list and collect flags */
	error = 0;
	command = argv[0];
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
				/* exlicit fall-through to -e case */

			  case 'e':	/* erase character */
				if (p[2] == 0)
					Erase_char = BACKSPACE;
				else
					Erase_char = p[2];
				continue;

			  case 'k':	/* kill character */
				if (p[2] == 0)
					Kill_char = CONTROLX;
				else
					Kill_char = p[2];
				continue;

			  case 'd':	/* dialup type */
				if (p[2] != 0)
					Dialtype = &p[2];
				else if (--argc < 0 || argv[1][0] == '-')
					error++;
				else
					Dialtype = *++argv;
				continue;

# ifdef PLUGBOARD
			  case 'p':	/* plugboard type */
				if (p[2] != 0)
					Plugtype = &p[2];
				else if (--argc < 0 || argv[1][0] == '-')
					error++;
				else
					Plugtype = *++argv;
				continue;
# endif

# ifdef BUSSIPLEXER
			  case 'b':	/* bussiplexer type */
				if (p[2] != 0)
					Bxtype = &p[2];
				else if (--argc < 0 || argv[1][0] == '-')
					error++;
				else
					Bxtype = *++argv;
# endif

			  case 'h':	/* don't get type from htmp */
				Dash_h++;
				continue;

# ifndef VERSION7
			  case 'u':	/* don't update htmp */
				Dash_u++;
				continue;
# endif

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
			Ttytype = p;
		}
	}

	if (error)
	{
		prs(Usage);
		exit(1);
	}

# ifndef FULLLOGIN
	/* if dialup is specified, check ttytype not htmp */
	if (Dialtype != 0 || Plugtype != 0 || Bxtype != 0)
		Dash_h++;
# endif

	/* determine terminal id if needed */
	if (Ttyid == NOTTY && (Ttytype == 0 || !Dash_h || !Dash_u))
# ifndef VERSION7
		Ttyid = ttyn(FILEDES);
# else
		Ttyid = ttyname(FILEDES);
# endif

# ifndef VERSION7
	/* get htmp if ever used */
	if (!Dash_u || (Ttytype == 0 && !Dash_h))
	{
		/* get htmp entry */
		hget(Ttyid);
	
		/* if not for this user, look at ttytype file */
		if (hgettype() == 0 || hgetuid() != (getuid() & UIDMASK))
			Dash_h++;
	}
# endif

	/* find terminal type (if not already known) */
	if (Ttytype == 0)
	{
		/* get type from /etc/ttytype or /etc/htmp */
		if (!Dash_h)
		{
# ifndef VERSION7
			Ttytype = hsgettype();
# else
			Ttytype = getenv("TERM");
# endif
		}
		if (Ttytype == 0)
		{
			Ttytype = stypeof(Ttyid);
		}

		/* check for dialup or plugboard override */
		if (Dialtype != 0 && bequal(Ttytype, DIALUP, 2))
			Ttytype = Dialtype;
# ifdef PLUGBOARD
		else if (Plugtype != 0 && bequal(Ttytype, PLUGBOARD, 2))
			Ttytype = Plugtype;
# endif
# ifdef BUSSIPLEXER
		else if (Bxtype != 0 && bequal(Ttytype, BUSSIPLEXER, 2))
			Ttytype = Bxtype;
# endif
	}

	/* Ttytype now contains a pointer to the type of the terminal */

	if (gtty(FILEDES, &mode) < 0)
	{
		prs("Not a terminal\n");
		exit(1);
	}
	bmove(&mode, &oldmode, sizeof mode);

	/* get terminal capabilities */
	switch (tgetent(Capbuf, Ttytype))
	{

	  case -1:
		prs("Cannot open ttycap file\n");
		exit(-1);

	  case 0:
		prs("Type ");
		prs(Ttytype);
		prs(" unknown\n");
		exit(1);
	}

	/* report type if appropriate */
	if (Report || Ureport)
	{
		/* find first alias (if any) */
		for (p = Capbuf; *p != 0 && *p != '|' && *p != ':'; p++)
			continue;
		if (*p == 0 || *p == ':')
			p = Capbuf;
		else
			p++;
		bufp = p;
		while (*p != '|' && *p != ':' && *p != 0)
			p++;
		i = *p;
		if (Report)
		{
			*p = '\n';
			write(STDOUT, bufp, p + 1 - bufp);
		}
		if (Ureport)
		{
			*p = '\0';
			prs("Terminal type is ");
			prs(bufp);
			prs("\n");
		}
		*p = i;
	}

	/* determine erase and kill characters */
	if (Specialerase && !tgetflag("bs"))
		Erase_char = 0;
	if (Erase_char == 0)
	{
		if (mode.sg_erase == 0)
			mode.sg_erase = OLDERASE;
		if (tgetflag("bs") && !tgetflag("os"))
			mode.sg_erase = BACKSPACE;
	}
	else
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
# ifdef VERSION7
	if (!bequal(&mode, &oldmode, sizeof mode))
		ioctl(FILEDES, TIOCSETN, &mode);
# else
	if (!bequal(&mode, &oldmode, sizeof mode))
		stty(FILEDES, &mode);
# endif

	/* output startup string */
	bufp = buf;
	if (tgetstr("is", &bufp) != 0)
		prs(buf);
	bufp = buf;
	if (tgetstr("if", &bufp) != 0)
		cat(buf);

	/* tell about changing erase and kill characters */
	reportek("Erase", mode.sg_erase, oldmode.sg_erase, OLDERASE);
	reportek("Kill", mode.sg_kill, oldmode.sg_kill, OLDKILL);

# ifndef VERSION7
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
	char		buf[512];

	fd = open(file, 0);
	if (fd < 0)
	{
		prs("Cannot open ");
		prs(file);
		prs("\n");
		exit(-1);
	}

	while ((i = read(fd, buf, 512)) > 0)
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
