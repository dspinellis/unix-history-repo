# include <stdio.h>
# include "sendmail.h"
# include <ctype.h>

static char SccsId[] = "@(#)readcf.c	3.2	%G%";

/*
**  READCF -- read control file.
**
**	This routine reads the control file and builds the internal
**	form.
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

struct rewrite	*RewriteRules;


readcf(cfname)
	char *cfname;
{
	FILE *cf;
	char buf[MAXLINE];
	register char *p;
	struct rewrite *rwp = NULL;
	extern char *xalloc();
	extern char **prescan();
	extern char **copyplist();
	extern char *rindex();
	extern char *newstr();

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
					RewriteRules = rwp = (struct rewrite *) xalloc(sizeof *rwp);
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

		  case 'D':		/* macro definition */
			define(buf[1], newstr(&buf[2]));
			break;

		  case 'H':		/* required header line */
			chompheader(&buf[1], H_DEFAULT);
			break;

		  default:
			syserr("unknown control line \"%s\"", buf);
		}
	}

/*
	printrules();
*/
}
/*
**  RWCRACK -- crack rewrite line.
**
**	Parameters:
**		l -- line to crack.
**
**	Returns:
**		local copy of cracked line.
**
**	Side Effects:
**		none.
*/

char **
rwcrack(l)
	register char *l;
{
	char *av[MAXATOM];
	int ac = 0;
	register char **avp;
	char buf[MAXNAME];
	register char *b;
	bool wasdelim = FALSE;
	char *delims = ":@!^.";
	extern char *index();
	bool tchange;
	extern char *newstr(), *xalloc();

	for (avp = av; *l != '\0' && *l != '\n'; avp++)
	{
		b = buf;
		tchange = FALSE;
		while (!tchange)
		{
			if (*l != '$')
			{
				if (wasdelim || index(delims, *l) != NULL)
					tchange = TRUE;
				wasdelim = (index(delims, *l) != NULL);
				if (wasdelim)
					tchange = TRUE;
				*b++ = *l++;
				continue;
			}

			tchange = TRUE;
			switch (*++l)
			{
			  case '$':		/* literal $ */
				*b++ = *l;
				break;

			  case '+':		/* match anything */
				*b++ = MATCHANY;
				*b++ = *++l;
				break;

			  case '-':		/* match one token */
				*b++ = MATCHONE;
				*b++ = *++l;
				break;

			  case '#':		/* canonical net name */
				*b++ = CANONNET;
				break;

			  case '@':		/* canonical host name */
				*b++ = CANONHOST;
				break;

			  case ':':		/* canonical user name */
				*b++ = CANONUSER;
				break;

			  default:
				*b++ = '$';
				l--;
				break;
			}
			l++;
		}

		/* save the argument we have collected */
		*b = '\0';
		*avp = newstr(buf);
		ac++;
	}

	/* allocate new space for vector */
	ac++;
	*avp = NULL;
	avp = (char **) xalloc(ac * sizeof *av);
	bmove(av, avp, ac * sizeof *av);

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

	for (rwp = RewriteRules; rwp != NULL; rwp = rwp->r_next)
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
