# include	<ingres.h>
# include	<aux.h>
# include	<opsys.h>
# include	<access.h>
# include	<tree.h>
# include	<symbol.h>
# include	"globs.h"
# include	<sccs.h>

SCCSID(@(#)call_ovqp.c	7.1	2/5/81)


/*
** CALL_OVQP -- Routines which interface to the One Variable Query Processor.
**
**	This file contains the routines associated with sending queries
**	and receiving results from OVQP. The interface to these routines is
**	still messy. Call_ovqp is given the query, mode, and result relation
**	as parameters and gets the source relation, and two flags
**	(De.de_newq, De.de_newr) as globals. The routines include:
**
**	Call_ovqp -- Sends a One-var query to ovqp and flushes the pipe.
**
**	Readresult -- Reads the result from a one-var query.
**
**	Endovqp    -- Informs ovqp that the query is over. Helps to synchronize
**			the batch file (if any).
**
**	Trace Flags:
**		61
*/
/*
** Call_ovqp -- send query down pipe to ovqp and flush pipe.
**	Inputs are:
**		mode		retrieve, append, etc.
**		resultnum	result relation id
**		tree		the query
**		De.de_sourcevar	(global) if >= 0 then source var
**		De.de_newq		send NEWQ symbol
**		De.de_newr		send NEWR symbol
*/

call_ovqp(tree, mode, resultnum)
register QTREE 	*tree;
int		mode;
int		resultnum;
{
	register int	i;
	char		*rangename();
	extern int	derror();
	extern bool	Batchupd;
	extern DESC	Inddes;
	char		ovqpbuf[LBUFSIZE];
	DESC		*readopen();
	extern DESC	*specopen();
	extern char	*rnum_convert();

#	ifdef xOTM
	if (tTf(90, 1))
		timtrace(7, 0);
#	endif

#	ifdef xDTR1
	if (tTf(61, -1))
	{
		if (tTf(61, 0))
			printf("CALL_OVQP-\n");
		if (tTf(61, 1))
		{
			if (De.de_newq)
			{
				printf("new query to ovqp\n");
				treepr(tree);
			}
			else
				printf("query same as previous\n");
		}
		if (tTf(61, 2))
		{
			printf("De.de_sourcevar=%d\t", De.de_sourcevar);
			if (De.de_sourcevar >= 0)
				printf("relid=%s\t", rangename(De.de_sourcevar));
			if (resultnum >= 0)
				printf("De.ov_resultname=%s", rnum_convert(resultnum));
			if (tree->sym.value.sym_root.rootuser)
				printf(", userqry");
			printf("\n");
		}
	}
#	endif



	/* assign mode of this query */
	De.de_qmode = mode;

	if (De.de_newr)
	{
		De.de_newr = FALSE;
	}

	if (resultnum >= 0)
	{
		De.ov_result = specopen(resultnum);
	}
	else
		De.ov_result = NULL;

	if (De.de_sourcevar >= 0)
		De.ov_source = readopen(De.de_sourcevar);
	else
		De.ov_source = NULL;

	/* assume this will be direct update */
	De.ov_userqry = De.de_buflag = FALSE;

	if (tree->sym.value.sym_root.rootuser)
	{
		De.ov_userqry = TRUE;
		/* handle batch file */
		if (De.ov_result && De.de_qmode != mdRETR)
		{
			if (Batchupd || De.ov_result->reldum.relindxd > 0)
			{
				if (De.ov_bopen == 0)
				{
					if (De.ov_result->reldum.relindxd > 0)
						opencatalog("indexes", 0);
					if (i = openbatch(De.ov_result, &Inddes, De.de_qmode))
						syserr("call_ovqp:opn batch %d", i);
					De.ov_bopen = TRUE;
				}
				De.de_buflag = TRUE;
			}
		}
	}

	/*  now write the query list itself  */
	if (De.de_newq)
	{
		De.ov_ovqpbuf = ovqpbuf;
		initbuf(De.ov_ovqpbuf, LBUFSIZE, LISTFULL, derror);
		De.de_qvptr = 0;
		De.ov_alist = De.ov_bylist = De.ov_qlist = De.ov_tlist = NULL;
		De.ov_targvc = tree->sym.value.sym_root.lvarc;
		De.ov_qualvc = bitcnt(tree->sym.value.sym_root.rvarm);
		De.ov_agcount = 0;

		if (tree->sym.type == AGHEAD)
		{
			De.ov_alist = &De.de_qvect[0];
			if (tree->left->sym.type == BYHEAD)
			{
				mklist(tree->left->right);
				ovqpnod(tree->left);	/* BYHEAD node */
				De.ov_bylist = &De.de_qvect[De.de_qvptr];
				mklist(tree->left->left);
			}
			else
				mklist(tree->left);
		}
		else
		{
			if (tree->left->sym.type != TREE)
			{
				De.ov_tlist = &De.de_qvect[0];
				mklist(tree->left);
			}
		}

		/* now for the qualification */
		ovqpnod(tree);	/* ROOT node */

		if (tree->right->sym.type != QLEND)
		{
			De.ov_qlist = &De.de_qvect[De.de_qvptr];
			mklist(tree->right);
		}
		ovqpnod(De.de_qle);	/* QLEND node */
	}

	/* Now call ovqp */
	if (strategy())
	{

#		ifdef xOTM
		if (tTf(90, 2))
			timtrace(9, 0);
#		endif

		i = scan();	/* scan the relation */

#		ifdef xOTM
		if (tTf(90, 2))
			timtrace(10, 0);
#		endif

	}
	else
		i = EMPTY;

#	ifdef xOTM
	if (tTf(90, 1))
		timtrace(8, 0);
#	endif


	/* return result of query */
	return (i == NONEMPTY);	/* TRUE if tuple satisfied */
}
/*
** Endovqp -- Inform ovqp that processing is complete. "Ack" indicates
**	whether to wait for an acknowledgement from ovqp. The overall
**	mode of the query is sent followed by an EXIT command.
**
**	Ovqp decides whether to use batch update or not. If ack == ACK
**	then endovqp will read a RETVAL symbol from ovqp and return
**	a token which specifies whether to call the update processor or not.
*/

endovqp(ack)
int	ack;
{
	register int	i;

	if (ack != RUBACK)
	{
		if (Equel && De.de_qry_mode == mdRETTERM)
			equeleol(EXIT);	/* signal end of retrieve to equel process */
	}

	i = NOUPDATE;

	if (ack == ACK)
	{
		if (De.ov_bopen)
		{
			closebatch();
			De.ov_bopen = FALSE;
			i = UPDATE;
		}
	}
	else
	{
		if (De.ov_bopen)
		{
			rmbatch();
			De.ov_bopen = FALSE;
		}
	}

	closecatalog(FALSE);

	return (i);
}
/*
**	Add node q to ovqp's list
*/

ovqpnod(q)
register QTREE	*q;
{
	register SYMBOL	*s;
	extern QTREE	*ckvar();
	extern char	*need();
	register int	i;

	s = &q->sym;

	/* VAR nodes must be specially processed */
	if (s->type == VAR)
	{
		/* locate currently active VAR */
		q = ckvar(q);

		/* Allocate an ovqp var node for the VAR */
		s = (SYMBOL *) need(De.ov_ovqpbuf, SYM_HDR_SIZ + sizeof s->value.sym_var);
		s->len = sizeof s->value.sym_var;
		s->value.sym_var.attno = q->sym.value.sym_var.attno;
		s->value.sym_var.varfrmt = q->sym.value.sym_var.varfrmt;
		s->value.sym_var.varfrml = q->sym.value.sym_var.varfrml;

		/* If VAR has been substituted for, get value */
		if (q->sym.value.sym_var.valptr)
		{
			/* This is a substituted variable */
			if (q->sym.value.sym_var.varno == De.de_sourcevar)
				syserr("ovqpnod:bd sub %d,%d", q->sym.value.sym_var.varno, De.de_sourcevar);

			s->type = S_VAR;
			s->value.sym_var.valptr = q->sym.value.sym_var.valptr;
		}
		else
		{
			/* Var for one variable query */
			if (q->sym.value.sym_var.varno != De.de_sourcevar)
				syserr("ovqpnod:src var %d,%d", q->sym.value.sym_var.varno, De.de_sourcevar);
			s->type = VAR;
			i = q->sym.value.sym_var.attno;
			if (i != 0)
				s->value.sym_var.valptr = (ANYTYPE *) (De.ov_intup + De.ov_source->reloff[i]);
			else
				s->value.sym_var.valptr = (ANYTYPE *) &De.ov_intid;
		}
	}
	if (s->type == AOP)
		De.ov_agcount++;

	/* add symbol to list */
	if (De.de_qvptr > MAXNODES - 1)
		ov_err(NODOVFLOW);
	De.de_qvect[De.de_qvptr++] = s;
}
/*
**  READAGG_RESULT
*/

readagg_result(result)
QTREE	*result[];
{
	register QTREE	**r, *aop;
	register int	i;

	De.ov_tend = De.ov_outtup;
	r = result;

	while (aop = *r++)
	{
		i = aop->sym.len & I1MASK;

		if (aop->sym.type == CHAR)
			pad(De.ov_tend, i);

		bmove(De.ov_tend, (char *)&aop->sym.value, i);

		De.ov_tend += i;
#		ifdef xDTR1
		if (tTf(61, 3))
			nodepr(aop);
#		endif
	}
}


ov_err(code)
int	code;
{
	derror(code);
}


DESC *
openindex(name)
char	*name;
{
	register DESC	*d;
	register int	varno;
	DESC		*readopen();

	varno = SECINDVAR;
	De.de_rangev[varno].relnum = rnum_assign(name);
	d = readopen(varno);
	return (d);
}
/*
**	Use "closer()" for closing relations. See
**	desc_close in openrs.c for details.
*/
extern int	closer();
int		(*Des_closefunc)()	= closer;

init_decomp()
{
	static struct accbuf	xtrabufs[12];

	set_so_buf();
	acc_addbuf(xtrabufs, 12);
}


startdecomp()
{
	/* called at the start of each user query */
	initrange();
	rnum_init();
	startovqp();
}
