/*
 * Copyright (c) 1983 Eric P. Allman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)readcf.c	6.1 (Berkeley) %G%";
#endif /* not lint */

# include "sendmail.h"
# include <sys/stat.h>
# include <unistd.h>
#ifdef NAMED_BIND
# include <arpa/nameser.h>
# include <resolv.h>
#endif

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
**		Mn arg=val...	Define mailer.  n is the internal name.
**				Args specify mailer parameters.
**		Oxvalue		Set option x to value.
**		Pname=value	Set precedence name to value.
**		Vversioncode	Version level of configuration syntax.
**		Kmapname mapclass arguments....
**				Define keyed lookup of a given class.
**				Arguments are class dependent.
**
**	Parameters:
**		cfname -- control file name.
**		safe -- TRUE if this is the system config file;
**			FALSE otherwise.
**		e -- the main envelope.
**
**	Returns:
**		none.
**
**	Side Effects:
**		Builds several internal tables.
*/

readcf(cfname)
	char *cfname;
	bool safe;
	register ENVELOPE *e;
{
	FILE *cf;
	int ruleset = 0;
	char *q;
	char **pv;
	struct rewrite *rwp = NULL;
	char *bp;
	char buf[MAXLINE];
	register char *p;
	extern char **prescan();
	extern char **copyplist();
	struct stat statb;
	char exbuf[MAXLINE];
	char pvpbuf[PSBUFSIZE];
	extern char *fgetfolded();
	extern char *munchstring();
	extern void makemapentry();

	FileName = cfname;
	LineNumber = 0;

	cf = fopen(cfname, "r");
	if (cf == NULL)
	{
		syserr("cannot open");
		exit(EX_OSFILE);
	}

	if (fstat(fileno(cf), &statb) < 0)
	{
		syserr("cannot fstat");
		exit(EX_OSFILE);
	}

	if (!S_ISREG(statb.st_mode))
	{
		syserr("not a plain file");
		exit(EX_OSFILE);
	}

	if (OpMode != MD_TEST && bitset(S_IWGRP|S_IWOTH, statb.st_mode))
	{
		if (OpMode == MD_DAEMON || OpMode == MD_FREEZE)
			fprintf(stderr, "%s: WARNING: dangerous write permissions\n",
				FileName);
#ifdef LOG
		if (LogLevel > 0)
			syslog(LOG_CRIT, "%s: WARNING: dangerous write permissions",
				FileName);
#endif
	}

	while ((bp = fgetfolded(buf, sizeof buf, cf)) != NULL)
	{
		if (bp[0] == '#')
		{
			if (bp != buf)
				free(bp);
			continue;
		}

		/* map $ into \001 (ASCII SOH) for macro expansion */
		for (p = bp; *p != '\0'; p++)
		{
			if (*p == '#' && p > bp && ConfigLevel >= 3)
			{
				/* this is an on-line comment */
				register char *e;

				switch (*--p)
				{
				  case '\001':
					/* it's from $# -- let it go through */
					p++;
					break;

				  case '\\':
					/* it's backslash escaped */
					(void) strcpy(p, p + 1);
					break;

				  default:
					/* delete preceeding white space */
					while (isspace(*p) && p > bp)
						p--;
					if ((e = strchr(++p, '\n')) != NULL)
						(void) strcpy(p, e);
					else
						p[0] = p[1] = '\0';
					break;
				}
				continue;
			}

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
		switch (bp[0])
		{
		  case '\0':
		  case '#':		/* comment */
			break;

		  case 'R':		/* rewriting rule */
			for (p = &bp[1]; *p != '\0' && *p != '\t'; p++)
				continue;

			if (*p == '\0')
			{
				syserr("invalid rewrite line \"%s\"", bp);
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
			expand(&bp[1], exbuf, &exbuf[sizeof exbuf], e);
			rwp->r_lhs = prescan(exbuf, '\t', pvpbuf);
			if (rwp->r_lhs != NULL)
				rwp->r_lhs = copyplist(rwp->r_lhs, TRUE);
			else
				syserr("R line: null LHS");

			/* expand and save the RHS */
			while (*++p == '\t')
				continue;
			q = p;
			while (*p != '\0' && *p != '\t')
				p++;
			*p = '\0';
			expand(q, exbuf, &exbuf[sizeof exbuf], e);
			rwp->r_rhs = prescan(exbuf, '\t', pvpbuf);
			if (rwp->r_rhs != NULL)
				rwp->r_rhs = copyplist(rwp->r_rhs, TRUE);
			else
				syserr("R line: null RHS");
			break;

		  case 'S':		/* select rewriting set */
			ruleset = atoi(&bp[1]);
			if (ruleset >= MAXRWSETS || ruleset < 0)
			{
				syserr("bad ruleset %d (%d max)", ruleset, MAXRWSETS);
				ruleset = 0;
			}
			rwp = NULL;
			break;

		  case 'D':		/* macro definition */
			define(bp[1], newstr(munchstring(&bp[2])), e);
			break;

		  case 'H':		/* required header line */
			(void) chompheader(&bp[1], TRUE, e);
			break;

		  case 'C':		/* word class */
		  case 'F':		/* word class from file */
			/* read list of words from argument or file */
			if (bp[0] == 'F')
			{
				/* read from file */
				for (p = &bp[2]; *p != '\0' && !isspace(*p); p++)
					continue;
				if (*p == '\0')
					p = "%s";
				else
				{
					*p = '\0';
					while (isspace(*++p))
						continue;
				}
				fileclass(bp[1], &bp[2], p, safe);
				break;
			}

			/* scan the list of words and set class for all */
			for (p = &bp[2]; *p != '\0'; )
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
					setclass(bp[1], wd);
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
			for (p = &bp[1]; *p != '\0' && *p != '=' && *p != '\t'; p++)
				continue;
			if (*p == '\0')
				goto badline;
			*p = '\0';
			Priorities[NumPriorities].pri_name = newstr(&bp[1]);
			Priorities[NumPriorities].pri_val = atoi(++p);
			NumPriorities++;
			break;

		  case 'T':		/* trusted user(s) */
			p = &bp[1];
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

		  case 'V':		/* configuration syntax version */
			ConfigLevel = atoi(&bp[1]);
			break;

		  case 'K':
			makemapentry(&bp[1]);
			break;

		  default:
		  badline:
			syserr("unknown control line \"%s\"", bp);
		}
		if (bp != buf)
			free(bp);
	}
	if (ferror(cf))
	{
		syserr("I/O read error", cfname);
		exit(EX_OSFILE);
	}
	fclose(cf);
	FileName = NULL;

	if (stab("host", ST_MAP, ST_FIND) == NULL)
	{
		/* user didn't initialize: set up host map */
		strcpy(buf, "host host");
		if (ConfigLevel >= 2)
			strcat(buf, " -a.");
		makemapentry(buf);
	}
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

fileclass(class, filename, fmt, safe)
	int class;
	char *filename;
	char *fmt;
	bool safe;
{
	FILE *f;
	struct stat stbuf;
	char buf[MAXLINE];

	if (stat(filename, &stbuf) < 0)
	{
		syserr("fileclass: cannot stat %s", filename);
		return;
	}
	if (!S_ISREG(stbuf.st_mode))
	{
		syserr("fileclass: %s not a regular file", filename);
		return;
	}
	if (!safe && access(filename, R_OK) < 0)
	{
		syserr("fileclass: access denied on %s", filename);
		return;
	}
	f = fopen(filename, "r");
	if (f == NULL)
	{
		syserr("fileclass: cannot open %s", filename);
		return;
	}

	while (fgets(buf, sizeof buf, f) != NULL)
	{
		register STAB *s;
		register char *p;
# ifdef SCANF
		char wordbuf[MAXNAME+1];

		if (sscanf(buf, fmt, wordbuf) != 1)
			continue;
		p = wordbuf;
# else /* SCANF */
		p = buf;
# endif /* SCANF */

		/*
		**  Break up the match into words.
		*/

		while (*p != '\0')
		{
			register char *q;

			/* strip leading spaces */
			while (isspace(*p))
				p++;
			if (*p == '\0')
				break;

			/* find the end of the word */
			q = p;
			while (*p != '\0' && !isspace(*p))
				p++;
			if (*p != '\0')
				*p++ = '\0';

			/* enter the word in the symbol table */
			s = stab(q, ST_CLASS, ST_ENTER);
			setbitn(class, s->s_class);
		}
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
			syserr("mailer %s: `=' expected", m->m_name);
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
				if (!isspace(*p))
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

		  case 'L':		/* maximum line length */
			m->m_linelimit = atoi(p);
			break;
		}

		p = DelimChar;
	}

	/* do some heuristic cleanup for back compatibility */
	if (bitnset(M_LIMITS, m->m_flags))
	{
		if (m->m_linelimit == 0)
			m->m_linelimit = SMTPLINELIM;
		if (ConfigLevel < 2)
			setbitn(M_7BITS, m->m_flags);
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


#ifdef NAMED_BIND

struct resolverflags
{
	char	*rf_name;	/* name of the flag */
	long	rf_bits;	/* bits to set/clear */
} ResolverFlags[] =
{
	"debug",	RES_DEBUG,
	"aaonly",	RES_AAONLY,
	"usevc",	RES_USEVC,
	"primary",	RES_PRIMARY,
	"igntc",	RES_IGNTC,
	"recurse",	RES_RECURSE,
	"defnames",	RES_DEFNAMES,
	"stayopen",	RES_STAYOPEN,
	"dnsrch",	RES_DNSRCH,
	NULL,		0
};

#endif

setoption(opt, val, sticky)
	char opt;
	char *val;
	bool sticky;
{
	register char *p;
	extern bool atobool();
	extern time_t convtime();
	extern int QueueLA;
	extern int RefuseLA;
	extern bool trusteduser();
	extern char *username();

	if (tTd(37, 1))
		printf("setoption %c=%s", opt, val);

	/*
	**  See if this option is preset for us.
	*/

	if (bitnset(opt, StickyOpt))
	{
		if (tTd(37, 1))
			printf(" (ignored)\n");
		return;
	}

	if (tTd(37, 1))
		printf("\n");

	switch (opt)
	{
	  case '8':		/* allow eight-bit input */
		EightBit = atobool(val);
		break;

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

	  case 'C':		/* checkpoint every N addresses */
		CheckpointInterval = atoi(val);
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
#endif /* QUEUE */
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

	  case 'E':		/* error message header/header file */
		if (*val != '\0')
			ErrMsgFile = newstr(val);
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

	  case 'G':		/* match recipients against GECOS field */
		MatchGecos = atobool(val);
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

	  case 'h':		/* maximum hop count */
		MaxHopCount = atoi(val);
		break;

	  case 'I':		/* use internet domain name server */
#ifdef NAMED_BIND
		UseNameServer = TRUE;
		for (p = val; *p != 0; )
		{
			bool clearmode;
			char *q;
			struct resolverflags *rfp;

			while (*p == ' ')
				p++;
			if (*p == '\0')
				break;
			clearmode = FALSE;
			if (*p == '-')
				clearmode = TRUE;
			else if (*p != '+')
				p--;
			p++;
			q = p;
			while (*p != '\0' && !isspace(*p))
				p++;
			if (*p != '\0')
				*p++ = '\0';
			for (rfp = ResolverFlags; rfp->rf_name != NULL; rfp++)
			{
				if (strcasecmp(q, rfp->rf_name) == 0)
					break;
			}
			if (clearmode)
				_res.options &= ~rfp->rf_bits;
			else
				_res.options |= rfp->rf_bits;
		}
		if (tTd(8, 2))
			printf("_res.options = %x\n", _res.options);
#else
		usrerr("name server (I option) specified but BIND not compiled in");
#endif
		break;

	  case 'i':		/* ignore dot lines in message */
		IgnrDot = atobool(val);
		break;

	  case 'J':		/* .forward search path */
		ForwardPath = newstr(val);
		break;

	  case 'k':		/* connection cache size */
		MaxMciCache = atoi(val);
		if (MaxMciCache < 0)
			MaxMciCache = 0;
		break;

	  case 'K':		/* connection cache timeout */
		MciCacheTimeout = convtime(val);
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

	  case 'n':		/* validate RHS in newaliases */
		CheckAliases = atobool(val);
		break;

	  case 'o':		/* assume old style headers */
		if (atobool(val))
			CurEnv->e_flags |= EF_OLDSTYLE;
		else
			CurEnv->e_flags &= ~EF_OLDSTYLE;
		break;

	  case 'P':		/* postmaster copy address for returned mail */
		PostMasterCopy = newstr(val);
		break;

	  case 'q':		/* slope of queue only function */
		QueueFactor = atoi(val);
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
		TimeZoneSpec = newstr(val);
		break;

	  case 'U':		/* location of user database */
		UdbSpec = newstr(val);
		break;

	  case 'u':		/* set default uid */
		DefUid = atoi(val);
		setdefuser();
		break;

	  case 'v':		/* run in verbose mode */
		Verbose = atobool(val);
		break;

	  case 'w':		/* we don't have wildcard MX records */
		NoWildcardMX = atobool(val);
		break;

	  case 'x':		/* load avg at which to auto-queue msgs */
		QueueLA = atoi(val);
		break;

	  case 'X':		/* load avg at which to auto-reject connections */
		RefuseLA = atoi(val);
		break;

	  case 'y':		/* work recipient factor */
		WkRecipFact = atoi(val);
		break;

	  case 'Y':		/* fork jobs during queue runs */
		ForkQueueRuns = atobool(val);
		break;

	  case 'z':		/* work message class factor */
		WkClassFact = atoi(val);
		break;

	  case 'Z':		/* work time factor */
		WkTimeFact = atoi(val);
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
/*
**  MAKEMAPENTRY -- create a map entry
**
**	Parameters:
**		line -- the config file line
**
**	Returns:
**		TRUE if it successfully entered the map entry.
**		FALSE otherwise (usually syntax error).
**
**	Side Effects:
**		Enters the map into the dictionary.
*/

void
makemapentry(line)
	char *line;
{
	register char *p;
	char *mapname;
	char *classname;
	register STAB *map;
	STAB *class;

	for (p = line; isspace(*p); p++)
		continue;
	if (!isalnum(*p))
	{
		syserr("readcf: config K line: no map name");
		return;
	}

	mapname = p;
	while (isalnum(*++p))
		continue;
	if (*p != '\0')
		*p++ = '\0';
	while (isspace(*p))
		p++;
	if (!isalnum(*p))
	{
		syserr("readcf: config K line, map %s: no map class", mapname);
		return;
	}
	classname = p;
	while (isalnum(*++p))
		continue;
	if (*p != '\0')
		*p++ = '\0';
	while (isspace(*p))
		p++;

	/* look up the class */
	class = stab(classname, ST_MAPCLASS, ST_FIND);
	if (class == NULL)
	{
		syserr("readcf: map %s: class %s not available", mapname, classname);
		return;
	}

	/* enter the map */
	map = stab(mapname, ST_MAP, ST_ENTER);
	map->s_map.map_class = &class->s_mapclass;

	if ((*class->s_mapclass.map_init)(&map->s_map, mapname, p))
		map->s_map.map_flags |= MF_VALID;
}
