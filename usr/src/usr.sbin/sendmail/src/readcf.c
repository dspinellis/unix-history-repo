# include "sendmail.h"

SCCSID(@(#)readcf.c	3.23		%G%);

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
**		Mn p f r a	Define mailer.  n - internal name,
**				p - pathname, f - flags, r - rewriting
**				rule for sender, a - argument vector.
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

struct rewrite	*RewriteRules[10];


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

	cf = fopen(cfname, "r");
	if (cf == NULL)
	{
		syserr("cannot open %s", cfname);
		exit(EX_OSFILE);
	}

	while (fgets(buf, sizeof buf, cf) != NULL)
	{
		p = rindex(buf, '\n');
		if (p != NULL)
			*p = '\0';

		switch (buf[0])
		{
		  case '\n':
		  case '\0':
		  case ' ':
		  case '\t':
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
			(void) expand(&buf[1], exbuf, &exbuf[sizeof exbuf]);
			rwp->r_lhs = prescan(exbuf, '\t');
			if (rwp->r_lhs != NULL)
				rwp->r_lhs = copyplist(rwp->r_lhs, TRUE);

			/* expand and save the RHS */
			while (*++p == '\t')
				continue;
			(void) expand(p, exbuf, &exbuf[sizeof exbuf]);
			rwp->r_rhs = prescan(exbuf, '\t');
			if (rwp->r_rhs != NULL)
				rwp->r_rhs = copyplist(rwp->r_rhs, TRUE);
			break;

		  case 'S':		/* select rewriting set */
			ruleset = atoi(&buf[1]);
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

		  default:
		  badline:
			syserr("unknown control line \"%s\"", buf);
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
	char *mname;
	char *mpath;
	u_long mopts;
	extern u_long mfencode();
	char *mfrom;
	register struct mailer *m;
	char *margv[MAXPV + 1];
	register int i;
	extern int NextMailer;
	STAB *s;

	if (NextMailer >= MAXMAILERS)
	{
		syserr("Too many mailers defined");
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
	mfrom = q;

	if (*p == '\0')
	{
		syserr("invalid M line in configuration file");
		return;
	}

	/* allocate a mailer */
	m = (struct mailer *) xalloc(sizeof *m);
	m->m_name = newstr(mname);
	m->m_mailer = newstr(mpath);
	m->m_flags = mopts;
	m->m_from = newstr(mfrom);
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
	m->m_argv = (char **) xalloc((unsigned) (sizeof margv[0] * i));
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
		printf("\n----Rule Set %d:\n", ruleset);

		for (rwp = RewriteRules[ruleset]; rwp != NULL; rwp = rwp->r_next)
		{
			register char **av;

			printf("\n");
			for (av = rwp->r_lhs; *av != NULL; av++)
			{
				xputs(*av);
				putchar('_');
			}
			printf("\n\t");
			for (av = rwp->r_rhs; *av != NULL; av++)
			{
				xputs(*av);
				putchar('_');
			}
			printf("\n");
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
	'R',	M_RELRCPT,
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
