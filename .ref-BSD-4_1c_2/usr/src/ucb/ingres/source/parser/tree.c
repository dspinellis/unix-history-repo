# include	<ingres.h>
# include	<aux.h>
# include	<tree.h>
# include	<pv.h>
# include	"parser.h"
# include	<symbol.h>
# include	<sccs.h>

SCCSID(@(#)tree.c	7.1	2/5/81)


/*
** TREE
**  	FUNCTION TO ADD NODE TO QUERY TREE
**	RETURN VALUE IS POINTER TO NODE JUST CREATED		
*/
QTREE *
tree(lptr, rptr, typ, len, valu, attnum)
QTREE			*lptr;
QTREE			*rptr;
char			typ;
int			len;
register int		valu;
register struct atstash	*attnum;
{
	register QTREE	*tptr;
	extern char	Trfrmt;
	extern char	Trfrml;
	extern char	*need();
	extern QTREE	*norm();
	extern int	Err_current;

# ifdef	xPTR3
	tTfp(55, 0, "tree type(%d), len(%d), value(%d).\n", typ, len, valu);
# endif

	if (Err_current)
		return (NULL);

	/* Following is a hack.  Sorry about that John.  */
	if (typ == AND)
		len = sizeof (struct rootnode) - sizeof (short);
	
	tptr = (QTREE *) need(Qbuf, QT_HDR_SIZ + len);
	tptr->left = lptr;
	tptr->right = rptr;
	tptr->sym.type = typ;
	tptr->sym.len = len;

	switch (typ)
	{
	  case VAR:
		tptr->sym.value.sym_var.varno = valu & I1MASK;
		tptr->sym.value.sym_var.attno = attnum->atbid;
		tptr->sym.value.sym_var.varfrmt = attnum->atbfrmt;
		tptr->sym.value.sym_var.varfrml = attnum->atbfrml;
		tptr->sym.value.sym_var.valptr = NULL;
		break;

	  case ROOT:
	  case AGHEAD:
		tptr->sym.value.sym_root.rootuser = valu;
		break;

	  case TREE:
	  case BYHEAD:
	  case AND:
	  case OR:
	  case QLEND:
		break;

	  case UOP:
	  case BOP:
		tptr->sym.value.sym_op.opno = valu;
		format(tptr);
		break;

	  case COP:
		if ((tptr->sym.value.sym_op.opno = getcop(valu)) == BADCOP)
		{
			/* bad const operator */
			par_error(BADCONSTOP, WARN, valu, 0);
			return(NULL);
		}
		break;

	  case AOP:
		format(tptr->right);
		tptr->sym.value.sym_op.agfrmt = Trfrmt;
		tptr->sym.value.sym_op.agfrml = Trfrml;

	  case RESDOM:
		tptr->sym.value.sym_resdom.resno = valu;
		format(tptr);
		tptr->sym.value.sym_resdom.resfrmt = Trfrmt;
		tptr->sym.value.sym_resdom.resfrml = Trfrml;
		break;

	  default:
		/* INT, FLOAT, CHAR */
		bmove(valu, &tptr->sym.value, len & I1MASK);
		break;
	}
	return (tptr);
}

/*
** WINDUP
**	assign resno's to resdoms of an agg fcn
*/
windup(ptr)
QTREE	*ptr;
{
	register int			tot;
	register int			kk;
	register QTREE			*t;

	/* COUNT THE RESDOM'S OF THIS TARGET LIST */
	kk = 1;
	for (t = ptr; t; t = t->left)
		kk++;
	tot = 1;
	for (t=ptr; t;t = t->left)
		t->sym.value.sym_resdom.resno = kk - tot++;
}

/*
** ADDRESDOM - makes a new entry for the target list
**
**	Trname must contain the name of the resdom to
**	use for the header, create and Rsdmno for append, replace
**
**	the parameters are pointers to the subtrees to be
**	suspended from the node
*/
QTREE *
addresdom(lptr, rptr)
QTREE	*lptr, *rptr;
{
	register QTREE	*rtval;
	register struct atstash		*aptr;
	char				buf[10];	/* buffer type and length in ascii for dbu */

	extern int			Opflag;
	extern int			Rsdmno;
	extern int			Equel;
	extern int			Resrng;
	extern char			Trfrmt;
	extern char			Trfrml;
	extern char			*Trname;
	extern PARRNG			Parrng[];

	extern QTREE			*tree();
	extern struct atstash		*attlookup();

	int				temp;

	switch (Opflag)
	{
	  case mdRETR:
	  case mdRET_UNI:
	  case mdVIEW:
		Rsdmno++;
		if (Rsdmno >= MAXDOM)
			/* too many resdoms */
			par_error(RESXTRA, FATAL, 0);
		rtval = tree(lptr, rptr, RESDOM, sizeof (struct resdomnode), Rsdmno);
		if (!Equel || Resrng)
		{
			/* buffer info for header or CREATE */
			setp(PV_STR, Trname);

			buf[0] = Trfrmt & I1MASK;
			smove(iocv(Trfrml & I1MASK), &buf[1]);

			setp(PV_STR, buf);
		}
		break;

	  default:
		/*
		** for append and replace, the result domain
		** number is determined by the location of
		** the attribute in the result relation
		*/
		if (sequal(Trname, "tid"))
			/* attrib not found */
			par_error(NOATTRIN, WARN, Trname,
			    trim_relname(Parrng[Resrng].vardesc.reldum.relid), 0);
#		ifdef	DISTRIB
		if (sequal(Trname, "sid"))
			/* attrib not found */
			par_error(NOATTRIN, WARN, Trname,
			    trim_relname(Parrng[Resrng].vardesc.reldum.relid), 0);
#		endif
		aptr = attlookup(Resrng, Trname);
		Rsdmno = aptr->atbid;
		rtval = tree(lptr, rptr, RESDOM, sizeof (struct resdomnode), Rsdmno);
		if (Opflag != mdPROT)	/* INTEGRITY not possible here */
			attcheck(aptr);
		break;
	}
	return (rtval);
}
/*
** GETCOP
**	routine to lookup 'string' in constant operators table
**	constant table is declared in tables.y
**	structure is defined in ../parser.h
*/
getcop(string)
char	*string;
{
	register struct constop	*cpt;
	register char		*sptr;
	extern struct constop	Coptab[];

	sptr = string;
	for (cpt = Coptab; cpt->copname; cpt++)
		if (sequal(sptr, cpt->copname))
			return (cpt->copnum);
	return (BADCOP);
}
