# include "sendmail.h"

static char SccsId[] = "@(#)readcf.c	3.13	%G%";

/*
**  READCF -- read control file.
**
**	This routine reads the control file and builds the internal
**	form.
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
				syserr("invalid rewrite line \"%s\"", buf);
			else
			{
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

				rwp->r_lhs = prescan(&buf[1], '\t');
				if (rwp->r_lhs != NULL)
					rwp->r_lhs = copyplist(rwp->r_lhs, TRUE);
				while (*p == '\t')
					p++;
				rwp->r_rhs = prescan(p, '\t');
				if (rwp->r_rhs != NULL)
					rwp->r_rhs = copyplist(rwp->r_rhs, TRUE);
			}
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
			class = buf[1];
			if (!isalpha(class))
				goto badline;
			if (isupper(class))
				class -= 'A';
			else
				class -= 'a';

			/* scan the list of words and set class 'i' for all */
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
					s->s_class |= 1 << class;
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
	int mopts;
	char *mfrom;
	register struct mailer *m;
	char *margv[MAXPV + 1];
	register int i;
	extern int NextMailer;

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
	mopts = crackopts(q);
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
	m->m_sendq = NULL;
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
**  CRACKOPTS -- crack mailer options
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
	int	opt_value;	/* internal name of option */
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
	'\0',	0
};

crackopts(p)
	register char *p;
{
	register struct optlist *o;
	register int opts = 0;

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
