# include "sendmail.h"

SCCSID(@(#)readcf.c	3.37		%G%);

/*
**  READCF -- read control file.
**
**	This routine reads the control file and builds the internal
**	form.
**
**	The file is formatted as a sequence of lines, each taken
**	atomically.  The first character of each line describes how
**	the line is to be interpreted.  The lines are:
**		Dxval		Define macro x to have value val.
**		Cxword		Put word into class x.
**		Fxfile [fmt]	Read file for lines to put into
**				class x.  Use scanf string 'fmt'
**				or "%s" if not present.  Fmt should
**				only produce one string-valued result.
**		Hname: value	Define header with field-name 'name'
**				and value as specified; this will be
**				macro expanded immediately before
**				use.
**		Sn		Use rewriting set n.
**		Rlhs rhs	Rewrite addresses that match lhs to
**				be rhs.
**		Mn p f s r a	Define mailer.  n - internal name,
**				p - pathname, f - flags, s - rewriting
**				ruleset for sender, s - rewriting ruleset
**				for recipients, a - argument vector.
**		Oxvalue		Set option x to value.
**		Pname=value	Set precedence name to value.
**
**	Parameters:
**		cfname -- control file name.
**		safe -- set if this is a system configuration file.
**			Non-system configuration files can not do
**			certain things (e.g., leave the SUID bit on
**			when executing mailers).
**
**	Returns:
**		none.
**
**	Side Effects:
**		Builds several internal tables.
*/

readcf(cfname, safe)
	char *cfname;
	bool safe;
{
	FILE *cf;
	char buf[MAXLINE];
	register char *p;
	struct rewrite *rwp = NULL;
	extern char **prescan();
	extern char **copyplist();
	int class;
	int ruleset = 0;
	char exbuf[MAXLINE];
	char *q;

	cf = fopen(cfname, "r");
	if (cf == NULL)
	{
		syserr("cannot open %s", cfname);
		exit(EX_OSFILE);
	}

	LineNumber = 0;
	while (fgetfolded(buf, sizeof buf, cf) != NULL)
	{
		switch (buf[0])
		{
		  case '\0':
		  case '#':		/* comment */
			break;

		  case 'R':		/* rewriting rule */
			for (p = &buf[1]; *p != '\0' && *p != '\t'; p++)
				continue;

			if (*p == '\0')
			{
				syserr("line %d: invalid rewrite line \"%s\"",
					LineNumber, buf);
				break;
			}

			/* allocate space for the rule header */
			if (rwp == NULL)
			{
				RewriteRules[ruleset] = rwp =
					(struct rewrite *) xalloc(sizeof *rwp);
			}
			else
			{
				rwp->r_next = (struct rewrite *) xalloc(sizeof *rwp);
				rwp = rwp->r_next;
			}
			rwp->r_next = NULL;

			/* expand and save the LHS */
			*p = '\0';
			expand(&buf[1], exbuf, &exbuf[sizeof exbuf], CurEnv);
			rwp->r_lhs = prescan(exbuf, '\t');
			if (rwp->r_lhs != NULL)
				rwp->r_lhs = copyplist(rwp->r_lhs, TRUE);

			/* expand and save the RHS */
			while (*++p == '\t')
				continue;
			q = p;
			while (*p != '\0' && *p != '\t')
				p++;
			*p = '\0';
			expand(q, exbuf, &exbuf[sizeof exbuf], CurEnv);
			rwp->r_rhs = prescan(exbuf, '\t');
			if (rwp->r_rhs != NULL)
				rwp->r_rhs = copyplist(rwp->r_rhs, TRUE);
			break;

		  case 'S':		/* select rewriting set */
			ruleset = atoi(&buf[1]);
			if (ruleset >= MAXRWSETS || ruleset < 0)
			{
				syserr("readcf: line %d: bad ruleset %d (%d max)",
					LineNumber, ruleset, MAXRWSETS);
				ruleset = 0;
			}
			rwp = NULL;
			break;

		  case 'D':		/* macro definition */
			define(buf[1], newstr(&buf[2]));
			break;

		  case 'H':		/* required header line */
			(void) chompheader(&buf[1], TRUE);
			break;

		  case 'C':		/* word class */
		  case 'F':		/* word class from file */
			class = buf[1];
			if (!isalpha(class))
				goto badline;
			if (isupper(class))
				class -= 'A';
			else
				class -= 'a';
			
			/* read list of words from argument or file */
			if (buf[0] == 'F')
			{
				/* read from file */
				for (p = &buf[2]; *p != '\0' && !isspace(*p); p++)
					continue;
				if (*p == '\0')
					p = "%s";
				else
				{
					*p = '\0';
					while (isspace(*++p))
						continue;
				}
				fileclass(class, &buf[2], p);
				break;
			}

			/* scan the list of words and set class for all */
			for (p = &buf[2]; *p != '\0'; )
			{
				register char *wd;
				char delim;
				register STAB *s;

				while (*p != '\0' && isspace(*p))
					p++;
				wd = p;
				while (*p != '\0' && !isspace(*p))
					p++;
				delim = *p;
				*p = '\0';
				if (wd[0] != '\0')
				{
					s = stab(wd, ST_CLASS, ST_ENTER);
					s->s_class |= 1L << class;
				}
				*p = delim;
			}
			break;

		  case 'M':		/* define mailer */
			makemailer(&buf[1], safe);
			break;

		  case 'O':		/* set option */
			setoption(buf[1], &buf[2], safe, FALSE);
			break;

		  case 'P':		/* set precedence */
			if (NumPriorities >= MAXPRIORITIES)
			{
				syserr("readcf: line %d: too many P lines, %d max",
					LineNumber, MAXPRIORITIES);
				break;
			}
			for (p = &buf[1]; *p != '\0' && *p != '='; p++)
				continue;
			if (*p == '\0')
				goto badline;
			*p = '\0';
			Priorities[NumPriorities].pri_name = newstr(&buf[1]);
			Priorities[NumPriorities].pri_val = atoi(++p);
			NumPriorities++;
			break;

		  default:
		  badline:
			syserr("readcf: line %d: unknown control line \"%s\"",
				LineNumber, buf);
		}
	}
}
/*
**  FILECLASS -- read members of a class from a file
**
**	Parameters:
**		class -- class to define.
**		filename -- name of file to read.
**		fmt -- scanf string to use for match.
**
**	Returns:
**		none
**
**	Side Effects:
**
**		puts all lines in filename that match a scanf into
**			the named class.
*/

fileclass(class, filename, fmt)
	int class;
	char *filename;
	char *fmt;
{
	register FILE *f;
	char buf[MAXLINE];

	f = fopen(filename, "r");
	if (f == NULL)
	{
		syserr("cannot open %s", filename);
		return;
	}

	while (fgets(buf, sizeof buf, f) != NULL)
	{
		register STAB *s;
		char wordbuf[MAXNAME+1];

		if (sscanf(buf, fmt, wordbuf) != 1)
			continue;
		s = stab(wordbuf, ST_CLASS, ST_ENTER);
		s->s_class |= 1L << class;
	}

	(void) fclose(f);
}
/*
**  MAKEMAILER -- define a new mailer.
**
**	Parameters:
**		line -- description of mailer.  This is in tokens
**			separated by white space.  The fields are:
**			* the name of the mailer, as refered to
**			  in the rewriting rules.
**			* the pathname of the program to fork to
**			  execute it.
**			* the options needed by this program.
**			* the macro string needed to translate
**			  a local "from" name to one that can be
**			  returned to this machine.
**			* the argument vector (a series of parameters).
**		safe -- set if this is a safe configuration file.
**
**	Returns:
**		none.
**
**	Side Effects:
**		enters the mailer into the mailer table.
*/

# define SETWORD \
		{ \
			while (*p != '\0' && isspace(*p)) \
				p++; \
			q = p; \
			while (*p != '\0' && !isspace(*p)) \
				p++; \
			if (*p != '\0') \
				*p++ = '\0'; \
		}

makemailer(line, safe)
	char *line;
	bool safe;
{
	register char *p;
	register char *q;
	register struct mailer *m;
	register STAB *s;
	int i;
	char *mname;
	char *mpath;
	u_long mopts;
	short mrset, msset;
	char *margv[MAXPV + 1];
	extern u_long mfencode();
	extern int NextMailer;

	if (NextMailer >= MAXMAILERS)
	{
		syserr("readcf: line %d: too many mailers defined (%d max)",
			LineNumber, MAXMAILERS);
		return;
	}

	/* collect initial information */
	p = line;
	SETWORD;
	mname = q;
	SETWORD;
	mpath = q;
	SETWORD;
	mopts = mfencode(q);
	if (!safe)
		mopts &= ~M_RESTR;
	SETWORD;
	msset = atoi(q);
	SETWORD;
	mrset = atoi(q);

	if (*p == '\0')
	{
		syserr("readcf: line %d: invalid M line in configuration file",
			LineNumber);
		return;
	}
	if (msset >= MAXRWSETS || mrset >= MAXRWSETS)
	{
		syserr("readcf: line %d: invalid rewrite set, %d max",
			LineNumber, MAXRWSETS);
		return;
	}

	/* allocate a mailer */
	m = (struct mailer *) xalloc(sizeof *m);
	m->m_name = newstr(mname);
	m->m_mailer = newstr(mpath);
	m->m_flags = mopts;
	m->m_r_rwset = mrset;
	m->m_s_rwset = msset;
	m->m_badstat = EX_UNAVAILABLE;
	m->m_mno = NextMailer;
	Mailer[NextMailer++] = m;

	/* collect the argument vector */
	for (i = 0; i < MAXPV - 1 && *p != '\0'; i++)
	{
		SETWORD;
		margv[i] = newstr(q);
	}
	margv[i++] = NULL;

	/* save the argv */
	m->m_argv = (char **) xalloc(sizeof margv[0] * i);
	bmove((char *) margv, (char *) m->m_argv, sizeof margv[0] * i);
	s = stab(m->m_name, ST_MAILER, ST_ENTER);
	s->s_mailer = m;
}
/*
**  PRINTRULES -- print rewrite rules (for debugging)
**
**	Parameters:
**		none.
**
**	Returns:
**		none.
**
**	Side Effects:
**		prints rewrite rules.
*/

# ifdef DEBUG

printrules()
{
	register struct rewrite *rwp;
	register int ruleset;

	for (ruleset = 0; ruleset < 10; ruleset++)
	{
		if (RewriteRules[ruleset] == NULL)
			continue;
		printf("\n----Rule Set %d:", ruleset);

		for (rwp = RewriteRules[ruleset]; rwp != NULL; rwp = rwp->r_next)
		{
			printf("\nLHS:");
			printav(rwp->r_lhs);
			printf("RHS:");
			printav(rwp->r_rhs);
		}
	}
}

# endif DEBUG
/*
**  MFENCODE -- crack mailer options
**
**	These options modify the functioning of the mailer
**	from the configuration table.
**
**	Parameters:
**		p -- pointer to vector of options.
**
**	Returns:
**		option list in binary.
**
**	Side Effects:
**		none.
*/

struct optlist
{
	char	opt_name;	/* external name of option */
	u_long	opt_value;	/* internal name of option */
};
struct optlist	OptList[] =
{
	'f',	M_FOPT,
	'r',	M_ROPT,
	'q',	M_QUIET,
	'S',	M_RESTR,
	'n',	M_NHDR,
	'l',	M_LOCAL,
	's',	M_STRIPQ,
	'm',	M_MUSER,
	'F',	M_NEEDFROM,
	'D',	M_NEEDDATE,
	'M',	M_MSGID,
	'u',	M_USR_UPPER,
	'h',	M_HST_UPPER,
	'x',	M_FULLNAME,
	'A',	M_ARPAFMT,
	'U',	M_UGLYUUCP,
	'e',	M_EXPENSIVE,
	'X',	M_FULLSMTP,
	'C',	M_CANONICAL,
	'\0',	0
};

u_long
mfencode(p)
	register char *p;
{
	register struct optlist *o;
	register u_long opts = 0;

	while (*p != '\0')
	{
		for (o = OptList; o->opt_name != '\0' && o->opt_name != *p; o++)
			continue;
		if (o->opt_name == '\0')
			syserr("bad mailer option %c", *p);
		opts |= o->opt_value;
		p++;
	}
	return (opts);
}
/*
**  MFDECODE -- decode mailer flags into external form.
**
**	Parameters:
**		flags -- value of flags to decode.
**		f -- file to write them onto.
**
**	Returns:
**		none.
**
**	Side Effects:
**		none.
*/

mfdecode(flags, f)
	u_long flags;
	FILE *f;
{
	register struct optlist *o;

	putc('?', f);
	for (o = OptList; o->opt_name != '\0'; o++)
	{
		if ((o->opt_value & flags) == o->opt_value)
		{
			flags &= ~o->opt_value;
			putc(o->opt_name, f);
		}
	}
	putc('?', f);
}
/*
**  SETOPTION -- set global processing option
**
**	Parameters:
**		opt -- option name.
**		val -- option value (as a text string).
**		safe -- if set, this came from a system configuration file.
**		sticky -- if set, don't let other setoptions override
**			this value.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Sets options as implied by the arguments.
*/

static int	StickyOpt[128 / sizeof (int)];	/* set if option is stuck */
extern char	*WizWord;			/* the stored wizard password */

setoption(opt, val, safe, sticky)
	char opt;
	char *val;
	bool safe;
	bool sticky;
{
	time_t tval;
	int ival;
	bool bval;
	int smask;
	int sindex;
	extern bool atobool();

# ifdef DEBUG
	if (tTd(37, 1))
		printf("setoption %c=%s\n", opt, val);
# endif DEBUG

	/*
	**  See if this option is preset for us.
	*/

	sindex = opt;
	smask = 1 << (sindex % sizeof (int));
	sindex /= sizeof (int);
	if (bitset(smask, StickyOpt[sindex]))
	{
# ifdef DEBUG
		if (tTd(37, 2))
			printf("(ignored)\n");
# endif DEBUG
		return;
	}
	if (sticky)
		StickyOpt[sindex] |= smask;

	if (getruid() == 0)
		safe = TRUE;

	/*
	**  Encode this option as appropriate.
	*/

	if (index("rT", opt) != NULL)
		tval = convtime(val);
	else if (index("gLu", opt) != NULL)
		ival = atoi(val);
	else if (index("cfimosv", opt) != NULL)
		bval = atobool(val);
	else if (index("be", opt) != NULL)
		/* do nothing */ ;
	else if (val[0] == '\0')
		val = "";
	else
		val = newstr(val);

	/*
	**  Now do the actual assignment.
	*/

	switch (opt)
	{
	  case 'A':		/* set default alias file */
		AliasFile = val;
		if (AliasFile[0] == '\0')
			AliasFile = "aliases";
		break;

	  case 'b':		/* operations mode */
		Mode = *val;
		switch (Mode)
		{
		  case MD_DAEMON:	/* run as a daemon */
#ifdef DAEMON
			ArpaMode = Smtp = TRUE;
#else DAEMON
			syserr("Daemon mode not implemented");
#endif DAEMON
			break;

		  case '\0':	/* default: do full delivery */
			Mode = MD_DEFAULT;
			/* fall through....... */

		  case MD_DELIVER:	/* do everything (default) */
		  case MD_FORK:		/* fork after verification */
		  case MD_QUEUE:	/* queue only */
		  case MD_VERIFY:	/* verify only */
		  case MD_TEST:		/* test addresses */
			break;

		  default:
			syserr("Unknown operation mode -b%c", Mode);
			exit(EX_USAGE);
		}
		break;

	  case 'c':		/* don't connect to "expensive" mailers */
		NoConnect = bval;
		break;

	  case 'e':		/* set error processing mode */
		switch (*val)
		{
		  case 'p':	/* print errors normally */
			break;	/* (default) */

		  case 'q':	/* be silent about it */
			(void) freopen("/dev/null", "w", stdout);
			break;

		  case 'm':	/* mail back */
			MailBack = TRUE;
			HoldErrs = TRUE;
			break;

		  case 'e':	/* do berknet error processing */
			BerkNet = TRUE;
			HoldErrs = TRUE;
			break;

		  case 'w':	/* write back (or mail) */
			WriteBack = TRUE;
			HoldErrs = TRUE;
			break;
		}
		break;

	  case 'f':		/* save Unix-style From lines on front */
		SaveFrom = bval;
		break;

	  case 'g':		/* default gid */
		if (!safe)
			goto syntax;
		DefGid = ival;
		break;

	  case 'H':		/* help file */
		HelpFile = val;
		if (HelpFile[0] == '\0')
			HelpFile = "sendmail.hf";
		break;

	  case 'i':		/* ignore dot lines in message */
		IgnrDot = bval;
		break;

	  case 'L':		/* log level */
		LogLevel = ival;
		break;

	  case 'M':		/* define macro */
		define(val[0], &val[1]);
		break;

	  case 'm':		/* send to me too */
		MeToo = bval;
		break;

	  case 'o':		/* assume old style headers */
		CurEnv->e_oldstyle = bval;
		break;

	  case 'Q':		/* queue directory */
		QueueDir = val;
		if (QueueDir[0] == '\0')
			QueueDir = "mqueue";
		break;

	  case 'r':		/* read timeout */
		ReadTimeout = tval;
		break;

	  case 'S':		/* status file */
		StatFile = val;
		if (StatFile[0] == '\0')
			StatFile = "sendmail.st";
		break;

	  case 's':		/* be super safe, even if expensive */
		SuperSafe = bval;
		break;

	  case 'T':		/* queue timeout */
		TimeOut = tval;
		break;

	  case 't':		/* time zone name */
# ifdef V6
		StdTimezone = val;
		DstTimezone = index(val, ',');
		if (DstTimezone == NULL)
			goto syntax;
		*DstTimezone++ = '\0';
# endif V6
		break;

	  case 'u':		/* set default uid */
		if (!safe)
			goto syntax;
		DefUid = ival;
		break;

	  case 'v':		/* run in verbose mode */
		Verbose = bval;
		if (Verbose)
			NoConnect = FALSE;
		break;

# ifdef DEBUG
	  case 'W':		/* set the wizards password */
		if (!safe)
			goto syntax;
		WizWord = val;
		break;
# endif DEBUG

	  default:
	  syntax:
		syserr("setoption: line %d: syntax error on \"%c%s\"",
		       LineNumber, opt, val);
		break;
	}
}
