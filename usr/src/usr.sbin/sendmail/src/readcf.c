# include <stdio.h>
# include "sendmail.h"
# include <ctype.h>

static char SccsId[] = "@(#)readcf.c	3.6	%G%";

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

struct rewrite	*RewriteRules[10];


readcf(cfname)
	char *cfname;
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
					s = stab(wd, ST_ENTER);
					s->s_class |= 1 << class;
				}
				*p = delim;
			}
			break;

		  default:
		  badline:
			syserr("unknown control line \"%s\"", buf);
		}
	}

# ifdef DEBUG
	if (Debug > 6)
		printrules();
# endif DEBUG
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
