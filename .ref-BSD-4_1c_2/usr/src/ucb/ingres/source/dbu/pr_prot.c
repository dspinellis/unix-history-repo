# include	<ingres.h>
# include	<tree.h>
# include	<aux.h>
# include	<catalog.h>
# include	<symbol.h>
# include	<sccs.h>

SCCSID(@(#)pr_prot.c	7.1	2/5/81)

/*
**  PR_PROT.C -- Print out a protection query
**
**	Trace Flags:
**		51
*/






extern QTREE	*gettree();	/* gets a tree from "tree" catalog [readtree.c] */

char	*Days [] =
{
	"sunday",
	"monday",
	"tuesday",
	"wednesday",
	"thursday",
	"friday",
	"saturday",
};

struct rngtab
{
	char	relid [MAXNAME];
	char	rowner [2];
	char	rused;
}; 




/*
**  PR_PROT -- print out protection info on a relation
**
**	Prints out a "define permit" statement for 
**	each permission on a relation.
**	First calls pr_spec_permit() to print permissions
**	indicated in the relation.relstat bits. Lower level pr_??
**	routines look for these bits, so in the calls to pr_permit
**	for tuples actually gotten from the "protect" catalog,
**	pr_prot sets the relstat bits, thereby suppressing their special
**	meaning (they are inverse bits: 0 means on).
**
**	Parameters:
**		relid -- non-unique relation name used by user in DBU call
**		r -- ptr to relation tuple
**
**	Returns:
**		0 -- some permissions on rel
**		1 -- no permissions on rel
**
**	Side Effects:
**		reads trees from protection catalog
**
**	Trace Flags:
**		39, 0
*/

pr_prot(relid, r)
char				*relid;
register struct relation	*r;
{
	extern DESC	Prodes;
	TID		hitid, lotid;
	struct protect	key, tuple;
	register int	i;
	int		flag;	/* indicates whether a special case occurred */

#	ifdef xZTR1
	if (tTf(51, 0))
		printf("pr_prot: relation \"%s\" owner \"%s\"relstat 0%o\n",
		r->relid, r->relowner, r->relstat);
#	endif

	flag = 0;
	if (r->relstat & S_PROTUPS || !(r->relstat & S_PROTALL)
	   || !(r->relstat & S_PROTRET))
		printf("Permissions on %s are:\n\n", relid);
	/* print out special permissions, if any */
	flag += pr_spec_permit(r, S_PROTALL);
	flag += pr_spec_permit(r, S_PROTRET);

	if (!(r->relstat & S_PROTUPS))
		if (flag)
			return (0);
		else
			return (1);
	opencatalog("protect", 0);
	
	/* get protect catalog tuples for "r", "owner" */
	clearkeys(&Prodes);
	setkey(&Prodes, &key, r->relid, PRORELID);
	setkey(&Prodes, &key, r->relowner, PRORELOWN);
	if (i = find(&Prodes, EXACTKEY, &lotid, &hitid, &key))
		syserr("pr_prot: find %d", i);
	/* ready for pr_user call to getuser() */
	getuser(-1);
	for ( ; ; )
	{
		if (i = get(&Prodes, &lotid, &hitid, &tuple, TRUE))
			break;
		/* print out protection info */
		if (kcompare(&Prodes, &tuple, &key) == 0)
			/* permission from real protect tuple, concoct
			 * neutral relstat
			 */
			pr_permit(&tuple, r->relstat | S_PROTALL | S_PROTRET);
	}
	if (i != 1)
		syserr("pr_prot: get %d", i);

	/* close user file opened by pr_user call to getuser */
	getuser(0);
}
/*
**  PR_SPEC_PERMIT -- Print out special permissions
**	Prints out permissios indicated by the relation.relstat field bits.
**	Concocts a protection tuple for the permission and assigns a 
**	propermid-like number to it for printing. Passes to pr_permit()
**	the concocted tuple, together with a relstat where the appropriate
**	bit is 0, so that the special printing at the lower level pr_??? 
**	routines takes place.
**
**	Parameters:
**		r -- relation relation tuple for the permitted relation
**		relst_bit -- if this bit is 0 inthe relstat, prints the query
**				{S_PROTALL, S_PROTRET}
**
**	Returns:
**		1 -- if prints
**		0 -- otherwise
*/


pr_spec_permit(r, relst_bit)
register struct relation	*r;
int				relst_bit;
{
	register struct protect	*p;
	struct protect		prot;

	if (r->relstat & relst_bit)
		return (0);
	p = &prot;
	clrmem(p, sizeof *p);
	p->protree = -1;
	if (relst_bit == S_PROTALL)
		p->propermid = 0;
	else if (relst_bit == S_PROTRET)
		p->propermid = 1;
	else
		syserr("pr_spec_permit(relst_bit == 0%o)", relst_bit);

	bmove(r->relid, p->prorelid, MAXNAME);
	bmove("  ", p->prouser, 2);
	pmove("", p->proterm, sizeof p->proterm, ' ');
	pr_permit(p, (r->relstat | S_PROTRET | S_PROTALL) & ~relst_bit);
	return (1);
}
/*
**  PR_PERMIT -- print out a DEFINE PERMIT query for a protection tuple
**
**	Parameters:
**		p -- ptr to protection tuple
**		relstat -- relstat from relation
**
**	Returns:
**		none
**
**	Side Effects:
**		reads in a tree from the "tree" catalog
**		prints out a query
*/


pr_permit(p, relstat)
register struct protect	*p;
int			relstat;
{
	register QTREE	*t;
	extern DESC	Prodes;
	DESC		pdesc;

	/* 
	 * if there is a qualification then
	 * clear range table, then read in protect tree, 
	 * the print out range statements
	 * else create single entry range table.
	 */
	clrrange();
	if (p->protree >= 0)
	{
		t = gettree(p->prorelid, p->prorelown, mdPROT, p->protree, TRUE);
	}
	else
	{
		t = 0;
		bmove(p->prorelid, pdesc.reldum.relid, MAXNAME);
		bmove(p->prorelown, pdesc.reldum.relowner, 2);
		declare(0, &pdesc);
		p->proresvar = 0;
	}
	printf("Permission %d -\n\n", p->propermid);
	pr_range();

#	ifdef xZTR1
	if (tTf(51, 1))
	{
		printf("pr_permit: prot=");
		printup(&Prodes, p);
		printf(", Qt.qt_resvar=%d\n", Qt.qt_resvar);
	}
#	endif

	/* print out query */
	printf("define permit ");
	pr_ops(p->proopset, relstat);
	printf("on ");
	pr_rv(Qt.qt_resvar = p->proresvar);
	putchar(' ');
	pr_doms(p->prodomset, relstat);
	printf("\n\t");
	pr_user(p->prouser);
	pr_term(p->proterm);
	if ((relstat & S_PROTRET) && (relstat & S_PROTALL))
	{
		/* not special case */
		pr_time(p->protodbgn, p->protodend);
		pr_day(p->prodowbgn, p->prodowend);
	}
	if (t && t->right->sym.type != QLEND)
	{
		printf("\nwhere ");
		pr_qual(t->right);
	}
	printf("\n\n\n");

	/* clear up the old range table */
	clrrange();
}
/*
**  PR_OPS -- Prints the the operation list defined by a protection opset
**
**	Eliminates the appropriate bits from a copy of the opset while printing
**	out the appropriate operation list.
**
**	Parameters:
**		opset -- protection.opset for the relation
**		relstat
**
**	Returns:
**		none
**
**	Side Effects:
**		printing of permitted op list
*/

pr_ops(opset, relstat)
int	opset;
int	relstat;
{
	register int	op, j;

#	ifdef xZTR1
	if (tTf(51, 2))
		printf("pr_ops(0%o)\n", opset);
#	endif

	if (!(relstat & S_PROTALL) || opset == -1)
	{
		printf("all ");
		return;
	}
	if (!(relstat & S_PROTRET))
	{
		printf("retrieve ");
		return;
	}

	op = (opset & ~PRO_AGGR & ~PRO_TEST) & 077;
	for ( ; ; )
	{
		if (op & (j = PRO_RETR))
			printf("retrieve");
		else if (op & (j = PRO_REPL))
			printf("replace");
		else if (op & (j = PRO_DEL))
			printf("delete");
		else if (op & (j = PRO_APP))
			printf("append");
		op ^= j;
		if (op)
			printf(", ");
		else
			break;
	}
	putchar(' ');
}
/*
**  PR_DOMS -- Print domains in permit target list
**
**	Parameters:
**		doms -- an 8 byte integer array; a bit map of the domains
**			if all 8 integers are -1, then "all" is printed fo
**			for the target list
**		relstat
**
**	Returns:
**		none
**
**	Side Effects:
**		prints out target list
*/

pr_doms(doms, relstat)
short 	doms[8];
int	relstat;
{
	register short	*d;
	register int	flag, shift;
	int		word;
	char		*rel;

	word = shift = 0;
	d = doms;
	rel = Qt.qt_rangev[Qt.qt_resvar].rngvdesc->reldum.relid;

#	ifdef xZTR1
	if (tTf(51, 3))
	{
		printf("pr_doms: rel=\"%s\" ", rel);
		for (word = 0; word < 8; )
			printf("0%o ", d [word++]);
		word = 0;
		putchar('\n');
	}
#	endif
	if (!(relstat & S_PROTALL) || !(relstat & S_PROTRET))
		return;
	flag = 1;
	for (word = 0; word < 8; word++)
		if (*d++ != -1)
		{
			flag = 0;
			break;
		}

	if (!flag)
	{
		putchar('(');
		for (d = doms, word = 0; word < 8; word++, d++)
		{
			for (shift = 0; shift < 16; shift++, *d >>= 1)
			{
				if (*d & 01)
				{
					if (flag++)
						printf(", ");
					pr_attname(rel, word * 16 + shift);
				}
			}
		}
		putchar(')');
	}
}
/*
**  PR_USER -- prints out permitted user's name
**
**	Parameters:
**		user -- 2 char array, user's usercode as in
**			users file
**
**	Returns:
**		none
**
**	Side Effects:
**		prints users name or "all" if user was "  "
*/

pr_user(user)
char	user[2];
{
	register	i;
	char		buf[MAXLINE];
	register char	*c, *u;
	
#	ifdef xZTR1
	if (tTf(51, 4))
		printf("pr_user(\"%c%c\")\n", user[0], user[1]);
#	endif

	c = buf;
	u = user;
	printf("to ");
	if (bequal(u, "  ", 2))
		printf("all ");
	else
	{
		if (getuser(u, c))
		{
			printf("%c%c ", u[0], u[1]);
			return;
		}
		while (*c != ':' && *c != '\n')
			putchar(*c++);
		putchar(' ');
	}
}
/*
**  PR_TIME -- Prints out clock time range access is allowed
**
**	Parameters:
**		bgn, end -- begin end times in seconds (if all day, returns)
**
**	Returns:
**		none
**
**	Side Effects:
**		prints out time
*/

pr_time(bgn, end)
int	bgn, end;
{
	char		time [3];
	register char	*t;
	register int	b, e;

	t = time;
	b = bgn;
	e = end;
#	ifdef xZTR1
	if (tTf(51, 5))
		printf("pr_time(bgn=%d, end=%d)\n", b, e);
#	endif
	if (b == 0 && e == 24 * 60)
		return;
	printf("from %d:", b / 60);
	itoa(b % 60, t);
	if (!t [1])
		putchar('0');
	printf("%s to %d:", t, e / 60);
	itoa(e % 60, t);
	if (!t [1])
		putchar('0');
	printf("%s ", t);
}
/*
**  PR_DAY -- Prints day range permitted
**
**	Parameters:
**		bgn, end -- bgn end days [0..6] (if all week returns)
**
**	Returns:
**		none
**
**	Side Effects:
**		prints days or nothing
*/

pr_day(bgn, end)
int	bgn, end;
{
#	ifdef xZTR1
	if (tTf(51, 6))
		printf("pr_day(bgn=%d, end=%d)\n", bgn, end);
#	endif
	if (bgn == 0 && end >= 6)
		return;
	printf("on %s to %s ", Days [bgn], Days [end]);
}
/*
**  PR_TERM -- Print terminal from which access permitted
**
**	Parameters:
**		term -- 1 char terminal id as in /etc/tty* (if ' ' the returns)
**
**	Returns:
**		none
**
**	Side Effects:
**		prints terminal or nothing
*/

pr_term(term)
char	term[8];
{
#	ifdef xZTR1
	if (tTf(51, 7))
		printf("pr_term(term='%.8s')\n", term);
#	endif

	if (term[0] != ' ')
		printf("at %8.8s ", term);
}
