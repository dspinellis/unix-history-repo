# include <stdio.h>
# include <ctype.h>
# include <pwd.h>
# include "sendmail.h"

static char SccsId[] = "@(#)alias.c	3.9	%G%";

/*
**  ALIAS -- Compute aliases.
**
**	Scans the file AliasFile for a set of aliases.
**	If found, it arranges to deliver to them.  Uses libdbm
**	database if -DDBM.
**
**	Parameters:
**		a -- address to alias.
**
**	Returns:
**		none
**
**	Side Effects:
**		Aliases found are expanded.
**
**	Defined Constants:
**		MAXRCRSN -- the maximum recursion depth.
**
**	Files:
**		AliasFile -- the mail aliases.  The format is
**			a series of lines of the form:
**				alias:name1,name2,name3,...
**			where 'alias' expands to all of
**			'name[i]'.  Continuations begin with
**			space or tab.
**		AliasFile.pag, AliasFile.dir: libdbm version
**			of alias file.  Keys are aliases, datums
**			(data?) are name1,name2, ...
**
**	Notes:
**		If NoAlias (the "-n" flag) is set, no aliasing is
**			done.
**
**	Deficiencies:
**		It should complain about names that are aliased to
**			nothing.
**		It is unsophisticated about line overflows.
*/


# define MAXRCRSN	10

#ifdef DBM
typedef struct
{
	char	*dptr;
	int dsize;
} datum;
datum lhs, rhs;
extern datum fetch();
#endif DBM

alias(a)
	register ADDRESS *a;
{
	register ADDRESS *q;
	register char *p;
	extern char *AliasFile;
# ifndef DBM
	FILE *af;
	char line[MAXLINE+1];
	bool didalias;
	bool gotmatch;
	auto ADDRESS al;
# endif DBM

	if (NoAlias)
		return;
# ifdef DEBUG
	if (Debug)
		printf("--- alias ---\n");
# endif

# ifdef DBM
	dbminit(AliasFile);
# endif DBM

	/*
	**  Scan send queue for local mailer.
	**	We only have to do this once, since anything we alias
	**	to is being put at the end of the queue we are
	**	scanning or another queue.  This is because we only
	**	scan the local mailer queue.
	*/

	for (q = Mailer[M_LOCAL]->m_sendq; q != NULL; q = q->q_next)
	{
		To = q->q_paddr;

		/* don't realias already aliased names */
		if (bitset(QDONTSEND, q->q_flags))
			continue;

# ifdef DBM
		/* create a key for fetch */
		lhs.dptr = q->q_user;
		lhs.dsize = strlen(q->q_user) + 1;
		rhs = fetch(lhs);

		/* find this alias? */
		p = rhs.dptr;
		if (p == NULL)
			continue;
# else DBM
		s = stab(q->q_user, ST_ALIAS, ST_FIND);
		if (s == NULL)
			continue;
		p = s->s_alias;
# endif DBM

		/*
		**  Match on Alias.
		**	Deliver to the target list.
		**	Remove the alias from the send queue
		**	  and put it on the Alias queue.
		*/

# ifdef DEBUG
		if (Debug)
			printf("%s (%s, %s) aliased to %s\n",
			    q->q_paddr, q->q_host, q->q_user, p);
# endif
		if (Verbose)
			message("050", "aliased to %s", p);
		q->q_flags |= QDONTSEND;
		sendto(p, 1);
	}
}
/*
**  FORWARD -- Try to forward mail
**
**	This is similar but not identical to aliasing.
**
**	Currently it is undefined, until the protocol for userinfo
**	databases is finalized.
**
**	Parameters:
**		user -- the name of the user who's mail we
**			would like to forward to.
**
**	Returns:
**		TRUE -- we have forwarded it somewhere.
**		FALSE -- not forwarded; go ahead & deliver.
**
**	Side Effects:
**		New names are added to send queues.
*/

bool
forward(user)
	ADDRESS *user;
{
	char buf[60];
	register FILE *fp;
	register char *p;

	if (user->q_mailer != M_LOCAL || bitset(QBADADDR, user->q_flags))
		return (FALSE);

	/* good address -- look for .forward file in home */
	(void) expand("$z/.forward", buf, &buf[sizeof buf - 1]);
	fp = fopen(buf, "r");
	if (fp == NULL)
		return (FALSE);

	/* we do have an address to forward to -- do it */
	(void) fgets(buf, sizeof buf, fp);
	if ((p = index(buf, '\n')) != NULL)
		*p = '\0';
	(void) fclose(fp);
	if (buf[0] == '\0')
		return (FALSE);
	if (Verbose)
		message("050", "forwarded to %s", buf);
	sendto(buf, 1);
	return (TRUE);
}
