/*
**  Sendmail
**  Copyright (c) 1983  Eric P. Allman
**  Berkeley, California
**
**  Copyright (c) 1983 Regents of the University of California.
**  All rights reserved.  The Berkeley software License Agreement
**  specifies the terms and conditions for redistribution.
*/

#ifndef lint
static char	SccsId[] = "@(#)readcf.c	5.3 (Berkeley) %G%";
#endif not lint

# include "sendmail.h"

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
**
**	Returns:
**		none.
**
**	Side Effects:
**		Builds several internal tables.
*/

readcf(cfname)
	char *cfname;
{
	FILE *cf;
	int ruleset = 0;
	char *q;
	char **pv;
	struct rewrite *rwp = NULL;
	char buf[MAXLINE];
	register char *p;
	extern char **prescan();
	extern char **copyplist();
	char exbuf[MAXLINE];
	char pvpbuf[PSBUFSIZE];
	extern char *fgetfolded();
	extern char *munchstring();

	cf = fopen(cfname, "r");
	if (cf == NULL)
	{
		syserr("cannot open %s", cfname);
		exit(EX_OSFILE);
	}

	FileName = cfname;
	LineNumber = 0;
	while (fgetfolded(buf, sizeof buf, cf) != NULL)
	{
		/* map $ into \001 (ASCII SOH) for macro expansion */
		for (p = buf; *p != '\0'; p++)
		{
			if (*p != '$')
				continue;

			if (p[1] == '$')
			{
				/* actual dollar sign.... */
				(void) strcpy(p, p + 1);
				continue;
			}

			/* convert to macro expansion character */
			*p = '\001';
		}

		/* interpret this line */
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
				syserr("invalid rewrite line \"%s\"", buf);
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
			rwp->r_lhs = prescan(exbuf, '\t', pvpbuf);
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
			rwp->r_rhs = prescan(exbuf, '\t', pvpbuf);
			if (rwp->r_rhs != NULL)
				rwp->r_rhs = copyplist(rwp->r_rhs, TRUE);
			break;

		  case 'S':		/* select rewriting set */
			ruleset = atoi(&buf[1]);
			if (ruleset >= MAXRWSETS || ruleset < 0)
			{
				syserr("bad ruleset %d (%d max)", ruleset, MAXRWSETS);
				ruleset = 0;
			}
			rwp = NULL;
			break;

		  case 'D':		/* macro definition */
			define(buf[1], newstr(munchstring(&buf[2])), CurEnv);
			break;

		  case 'H':		/* required header line */
			(void) chompheader(&buf[1], TRUE);
			break;

		  case 'C':		/* word class */
		  case 'F':		/* word class from file */
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
				fileclass(buf[1], &buf[2], p);
				break;
			}

			/* scan the list of words and set class for all */
			for (p = &buf[2]; *p != '\0'; )
			{
				register char *wd;
				char delim;

				while (*p != '\0' && isspace(*p))
					p++;
				wd = p;
				while (*p != '\0' && !isspace(*p))
					p++;
				delim = *p;
				*p = '\0';
				if (wd[0] != '\0')
					setclass(buf[1], wd);
				*p = delim;
			}
			break;

		  case 'M':		/* define mailer */
			makemailer(&buf[1]);
			break;

		  case 'O':		/* set option */
			setoption(buf[1], &buf[2], FALSE);
			break;

		  case 'P':		/* set precedence */
			if (NumPriorities >= MAXPRIORITIES)
			{
				toomany('P', MAXPRIORITIES);
				break;
			}
			for (p = &buf[1]; *p != '\0' && *p != '=' && *p != '\t'; p++)
				continue;
			if (*p == '\0')
				goto badline;
			*p = '\0';
			Priorities[NumPriorities].pri_name = newstr(&buf[1]);
			Priorities[NumPriorities].pri_val = atoi(++p);
			NumPriorities++;
			break;

		  case 'T':		/* trusted user(s) */
			p = &buf[1];
			while (*p != '\0')
			{
				while (isspace(*p))
					p++;
				q = p;
				while (*p != '\0' && !isspace(*p))
					p++;
				if (*p != '\0')
					*p++ = '\0';
				if (*q == '\0')
					continue;
				for (pv = TrustedUsers; *pv != NULL; pv++)
					continue;
				if (pv >= &TrustedUsers[MAXTRUST])
				{
					toomany('T', MAXTRUST);
					break;
				}
				*pv = newstr(q);
			}
			break;

		  default:
		  badline:
			syserr("unknown control line \"%s\"", buf);
		}
	}
	FileName = NULL;
}
/*
**  TOOMANY -- signal too many of some option
**
**	Parameters:
**		id -- the id of the error line
**		maxcnt -- the maximum possible values
**
**	Returns:
**		none.
**
**	Side Effects:
**		gives a syserr.
*/

toomany(id, maxcnt)
	char id;
	int maxcnt;
{
	syserr("too many %c lines, %d max", id, maxcnt);
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
		setbitn(class, s->s_class);
	}

	(void) fclose(f);
}
/*
**  MAKEMAILER -- define a new mailer.
**
**	Parameters:
**		line -- description of mailer.  This is in labeled
**			fields.  The fields are:
**			   P -- the path to the mailer
**			   F -- the flags associated with the mailer
**			   A -- the argv for this mailer
**			   S -- the sender rewriting set
**			   R -- the recipient rewriting set
**			   E -- the eol string
**			The first word is the canonical name of the mailer.
**
**	Returns:
**		none.
**
**	Side Effects:
**		enters the mailer into the mailer table.
*/

makemailer(line)
	char *line;
{
	register char *p;
	register struct mailer *m;
	register STAB *s;
	int i;
	char fcode;
	extern int NextMailer;
	extern char **makeargv();
	extern char *munchstring();
	extern char *DelimChar;
	extern long atol();

	/* allocate a mailer and set up defaults */
	m = (struct mailer *) xalloc(sizeof *m);
	bzero((char *) m, sizeof *m);
	m->m_mno = NextMailer;
	m->m_eol = "\n";

	/* collect the mailer name */
	for (p = line; *p != '\0' && *p != ',' && !isspace(*p); p++)
		continue;
	if (*p != '\0')
		*p++ = '\0';
	m->m_name = newstr(line);

	/* now scan through and assign info from the fields */
	while (*p != '\0')
	{
		while (*p != '\0' && (*p == ',' || isspace(*p)))
			p++;

		/* p now points to field code */
		fcode = *p;
		while (*p != '\0' && *p != '=' && *p != ',')
			p++;
		if (*p++ != '=')
		{
			syserr("`=' expected");
			return;
		}
		while (isspace(*p))
			p++;

		/* p now points to the field body */
		p = munchstring(p);

		/* install the field into the mailer struct */
		switch (fcode)
		{
		  case 'P':		/* pathname */
			m->m_mailer = newstr(p);
			break;

		  case 'F':		/* flags */
			for (; *p != '\0'; p++)
				setbitn(*p, m->m_flags);
			break;

		  case 'S':		/* sender rewriting ruleset */
		  case 'R':		/* recipient rewriting ruleset */
			i = atoi(p);
			if (i < 0 || i >= MAXRWSETS)
			{
				syserr("invalid rewrite set, %d max", MAXRWSETS);
				return;
			}
			if (fcode == 'S')
				m->m_s_rwset = i;
			else
				m->m_r_rwset = i;
			break;

		  case 'E':		/* end of line string */
			m->m_eol = newstr(p);
			break;

		  case 'A':		/* argument vector */
			m->m_argv = makeargv(p);
			break;

		  case 'M':		/* maximum message size */
			m->m_maxsize = atol(p);
			break;
		}

		p = DelimChar;
	}

	/* now store the mailer away */
	if (NextMailer >= MAXMAILERS)
	{
		syserr("too many mailers defined (%d max)", MAXMAILERS);
		return;
	}
	Mailer[NextMailer++] = m;
	s = stab(m->m_name, ST_MAILER, ST_ENTER);
	s->s_mailer = m;
}
/*
**  MUNCHSTRING -- translate a string into internal form.
**
**	Parameters:
**		p -- the string to munch.
**
**	Returns:
**		the munched string.
**
**	Side Effects:
**		Sets "DelimChar" to point to the string that caused us
**		to stop.
*/

char *
munchstring(p)
	register char *p;
{
	register char *q;
	bool backslash = FALSE;
	bool quotemode = FALSE;
	static char buf[MAXLINE];
	extern char *DelimChar;

	for (q = buf; *p != '\0'; p++)
	{
		if (backslash)
		{
			/* everything is roughly literal */
			backslash = FALSE;
			switch (*p)
			{
			  case 'r':		/* carriage return */
				*q++ = '\r';
				continue;

			  case 'n':		/* newline */
				*q++ = '\n';
				continue;

			  case 'f':		/* form feed */
				*q++ = '\f';
				continue;

			  case 'b':		/* backspace */
				*q++ = '\b';
				continue;
			}
			*q++ = *p;
		}
		else
		{
			if (*p == '\\')
				backslash = TRUE;
			else if (*p == '"')
				quotemode = !quotemode;
			else if (quotemode || *p != ',')
				*q++ = *p;
			else
				break;
		}
	}

	DelimChar = p;
	*q++ = '\0';
	return (buf);
}
/*
**  MAKEARGV -- break up a string into words
**
**	Parameters:
**		p -- the string to break up.
**
**	Returns:
**		a char **argv (dynamically allocated)
**
**	Side Effects:
**		munges p.
*/

char **
makeargv(p)
	register char *p;
{
	char *q;
	int i;
	char **avp;
	char *argv[MAXPV + 1];

	/* take apart the words */
	i = 0;
	while (*p != '\0' && i < MAXPV)
	{
		q = p;
		while (*p != '\0' && !isspace(*p))
			p++;
		while (isspace(*p))
			*p++ = '\0';
		argv[i++] = newstr(q);
	}
	argv[i++] = NULL;

	/* now make a copy of the argv */
	avp = (char **) xalloc(sizeof *avp * i);
	bcopy((char *) argv, (char *) avp, sizeof *avp * i);

	return (avp);
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
**  SETOPTION -- set global processing option
**
**	Parameters:
**		opt -- option name.
**		val -- option value (as a text string).
**		sticky -- if set, don't let other setoptions override
**			this value.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Sets options as implied by the arguments.
*/

static BITMAP	StickyOpt;		/* set if option is stuck */
extern char	*WizWord;		/* the stored wizard password */
extern char	*NetName;		/* name of home (local) network */

setoption(opt, val, sticky)
	char opt;
	char *val;
	bool sticky;
{
	extern bool atobool();
	extern time_t convtime();
	extern int QueueLA;
	extern int RefuseLA;
	extern bool trusteduser();
	extern char *username();

# ifdef DEBUG
	if (tTd(37, 1))
		printf("setoption %c=%s", opt, val);
# endif DEBUG

	/*
	**  See if this option is preset for us.
	*/

	if (bitnset(opt, StickyOpt))
	{
# ifdef DEBUG
		if (tTd(37, 1))
			printf(" (ignored)\n");
# endif DEBUG
		return;
	}

#ifdef DEBUG
	if (tTd(37, 1))
		printf("\n");
#endif DEBUG

	switch (opt)
	{
	  case 'A':		/* set default alias file */
		if (val[0] == '\0')
			AliasFile = "aliases";
		else
			AliasFile = newstr(val);
		break;

	  case 'a':		/* look N minutes for "@:@" in alias file */
		if (val[0] == '\0')
			SafeAlias = 5;
		else
			SafeAlias = atoi(val);
		break;

	  case 'B':		/* substitution for blank character */
		SpaceSub = val[0];
		if (SpaceSub == '\0')
			SpaceSub = ' ';
		break;

	  case 'c':		/* don't connect to "expensive" mailers */
		NoConnect = atobool(val);
		break;

	  case 'd':		/* delivery mode */
		switch (*val)
		{
		  case '\0':
			SendMode = SM_DELIVER;
			break;

		  case SM_QUEUE:	/* queue only */
#ifndef QUEUE
			syserr("need QUEUE to set -odqueue");
#endif QUEUE
			/* fall through..... */

		  case SM_DELIVER:	/* do everything */
		  case SM_FORK:		/* fork after verification */
			SendMode = *val;
			break;

		  default:
			syserr("Unknown delivery mode %c", *val);
			exit(EX_USAGE);
		}
		break;

	  case 'D':		/* rebuild alias database as needed */
		AutoRebuild = atobool(val);
		break;

	  case 'e':		/* set error processing mode */
		switch (*val)
		{
		  case EM_QUIET:	/* be silent about it */
		  case EM_MAIL:		/* mail back */
		  case EM_BERKNET:	/* do berknet error processing */
		  case EM_WRITE:	/* write back (or mail) */
			HoldErrs = TRUE;
			/* fall through... */

		  case EM_PRINT:	/* print errors normally (default) */
			ErrorMode = *val;
			break;
		}
		break;

	  case 'F':		/* file mode */
		FileMode = atooct(val) & 0777;
		break;

	  case 'f':		/* save Unix-style From lines on front */
		SaveFrom = atobool(val);
		break;

	  case 'g':		/* default gid */
		DefGid = atoi(val);
		break;

	  case 'H':		/* help file */
		if (val[0] == '\0')
			HelpFile = "sendmail.hf";
		else
			HelpFile = newstr(val);
		break;

	  case 'i':		/* ignore dot lines in message */
		IgnrDot = atobool(val);
		break;

	  case 'L':		/* log level */
		LogLevel = atoi(val);
		break;

	  case 'M':		/* define macro */
		define(val[0], newstr(&val[1]), CurEnv);
		sticky = FALSE;
		break;

	  case 'm':		/* send to me too */
		MeToo = atobool(val);
		break;

# ifdef DAEMON
	  case 'N':		/* home (local?) network name */
		NetName = newstr(val);
		break;
# endif DAEMON

	  case 'o':		/* assume old style headers */
		if (atobool(val))
			CurEnv->e_flags |= EF_OLDSTYLE;
		else
			CurEnv->e_flags &= ~EF_OLDSTYLE;
		break;

	  case 'Q':		/* queue directory */
		if (val[0] == '\0')
			QueueDir = "mqueue";
		else
			QueueDir = newstr(val);
		break;

	  case 'r':		/* read timeout */
		ReadTimeout = convtime(val);
		break;

	  case 'S':		/* status file */
		if (val[0] == '\0')
			StatFile = "sendmail.st";
		else
			StatFile = newstr(val);
		break;

	  case 's':		/* be super safe, even if expensive */
		SuperSafe = atobool(val);
		break;

	  case 'T':		/* queue timeout */
		TimeOut = convtime(val);
		break;

	  case 't':		/* time zone name */
# ifdef V6
		StdTimezone = newstr(val);
		DstTimezone = index(StdTimeZone, ',');
		if (DstTimezone == NULL)
			syserr("bad time zone spec");
		else
			*DstTimezone++ = '\0';
# endif V6
		break;

	  case 'u':		/* set default uid */
		DefUid = atoi(val);
		break;

	  case 'v':		/* run in verbose mode */
		Verbose = atobool(val);
		break;

# ifdef DEBUG
	  case 'W':		/* set the wizards password */
		WizWord = newstr(val);
		break;
# endif DEBUG

	  case 'x':		/* load avg at which to auto-queue msgs */
		QueueLA = atoi(val);
		break;

	  case 'X':		/* load avg at which to auto-reject connections */
		RefuseLA = atoi(val);
		break;

	  default:
		break;
	}
	if (sticky)
		setbitn(opt, StickyOpt);
	return;
}
/*
**  SETCLASS -- set a word into a class
**
**	Parameters:
**		class -- the class to put the word in.
**		word -- the word to enter
**
**	Returns:
**		none.
**
**	Side Effects:
**		puts the word into the symbol table.
*/

setclass(class, word)
	int class;
	char *word;
{
	register STAB *s;

	s = stab(word, ST_CLASS, ST_ENTER);
	setbitn(class, s->s_class);
}
