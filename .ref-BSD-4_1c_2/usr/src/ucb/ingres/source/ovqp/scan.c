# include	<ingres.h>
# include	<symbol.h>
# include	<tree.h>
# include	"../decomp/globs.h"
# include	<sccs.h>

SCCSID(@(#)scan.c	7.1	2/5/81)


/*
**	SCAN
**
**	performs tuple by tuple scan of source reln or index reln
**	within limits found by strategy routine. 
**	When the source reln tuple is obtained the interpreter is invoked
**	to continue further processing
**
*/


scan()
{
	register	j, mode, domno;
	struct symbol	**rlist;	/* "result" list of query */
	long		count;
	long		tid, temptid;
	char		agtup[MAXTUP], outtup1[MAXTUP];
	int		qualfound, ok;
	struct symbol	*interpret();

#	ifdef xOTR1
	if (tTf(71, -1))
	{
		printf("SCAN\tScanr=%.12s\n", De.ov_scanr ? De.ov_scanr->reldum.relid : "(none)");
		printf("\tDe.ov_result %x\n", De.ov_result);
		if (tTf(71, 4))
			printf(" De.ov_alist=%x, De.ov_bylist=%x, De.ov_tlist=%x, De.ov_qlist=%x\n", De.ov_alist, De.ov_bylist, De.ov_tlist, De.ov_qlist);
		if (De.ov_result)
			printdesc(De.ov_result);
	}
#	endif

	if (De.ov_result || De.ov_alist)
	{
		if (De.ov_result)
		{
			clr_tuple(De.ov_result, De.ov_outtup);
		}
		else
		{
			j = MAXTUP;
			while (j--)
				De.ov_outtup[j] = 0;
		}
	}

	count = 0;
	qualfound = EMPTY;
	mode = De.de_qmode;

	/*
	** Check for identical source and result relations.
	** For modes mdREPL and mdDEL, De.ov_origtup must point
	** to the original (unmodified result tuple).
	**
	** If there is no De.ov_source or De.ov_result relations then
	** the code has no effect.
	*/

	if (De.ov_source == NULL ||
	    !bequal(De.ov_source->reldum.relid, De.ov_result->reldum.relid, MAXNAME))
	{
		De.ov_diffrel = TRUE;
		De.ov_origtup = outtup1;
	}
	else
	{
		De.ov_diffrel = FALSE;
		De.ov_origtup = De.ov_intup;
	}

	/*  determine type of result list */
	/* the only valid combinations are:
	**
	** De.ov_tlist=no	De.ov_alist=no	De.ov_bylist=no
	** De.ov_tlist=yes	De.ov_alist=no	De.ov_bylist=no
	** De.ov_tlist=no	De.ov_alist=yes	De.ov_bylist=no
	** De.ov_tlist=no	De.ov_alist=yes	De.ov_bylist=yes
	*/
	rlist = (De.ov_tlist? De.ov_tlist: De.ov_alist);
	if (De.ov_bylist)
		rlist = 0;

	De.ov_counter= &count;
	if (De.ov_bylist)
	{
		/*
		** For aggregate functions the result relation
		** is in the format:
		** domain 1 = I4 (used as a counter)
		** domain 2 through relatts - De.ov_agcount (by-domains)
		** remaining domains (the actual aggregate values)
		*/

		/* set up keys for the getequal */
		/* domno must end with the domain number of the first aggregate */

		for (domno = 2; domno <= De.ov_result->reldum.relatts - De.ov_agcount; domno++)
			De.ov_result->relgiven[domno] = 1;


		De.ov_counter = (long *)De.ov_outtup;	/* first four bytes of De.ov_outtup is counter for De.ov_bylist */
	}


	/*
	** check for constant qualification.
	** If the constant qual is true then remove
	** the qual to save reprocessing it.
	** If it is false then block further processing.
	*/

	ok = TRUE;
	if (De.ov_qlist && De.ov_qualvc == 0)
		if (interpret(De.ov_qlist)->value.sym_data.i2type)
			De.ov_qlist = 0;	/* qual always true */
		else
			ok = FALSE;	/* qual always false */



	/* if no source relation, interpret target list */
	if (!De.ov_scanr && ok)
	{
		/* there is no source relation and the qual is true */
		qualfound = NONEMPTY;
		De.ov_tend = De.ov_outtup;
		/* if there is a rlist then process it. (There should always be one) */
		if (rlist)
		{
			(*De.ov_counter)++;
			interpret(rlist);
		}
		if (De.ov_tlist)
			dispose(mode);
		else
			if (De.ov_userqry)
				De.ov_tupsfound++;
	}


	if (De.ov_scanr && ok)
	{
		/* There is a source relation. Iterate through each tuple */
		while (!(j = get(De.ov_scanr, &De.ov_lotid, &De.ov_hitid, De.ov_intup, NXTTUP)))
		{
#			ifdef xOTR1
			if (tTf(71, 5))
			{
				if (De.ov_scanr != De.ov_source)
					printf("Sec Index:");
				else
					printf("De.ov_intup:");
				printup(De.ov_scanr, De.ov_intup);
			}
#			endif
			De.ov_intid = De.ov_lotid;
			if (De.ov_scanr != De.ov_source)
			{
				/* make sure index tuple is part of the solution */
				if (!indexcheck())
					/* index keys don't match what we want */
					continue;
				bmove(De.ov_intup + De.ov_scanr->reldum.relwid - TIDLEN, (char *)&tid, TIDLEN);
				if (j = get(De.ov_source, &tid, &temptid, De.ov_intup, CURTUP))
					syserr("scan:indx get %d %.12s", j, De.ov_scanr->reldum.relid);
#				ifdef xOTR1
				if (tTf(71, 6))
				{
					printf("De.ov_intup:");
					printup(De.ov_source, De.ov_intup);
				}
#				endif
				De.ov_intid = tid;
			}


			if (!De.ov_qlist || interpret(De.ov_qlist)->value.sym_data.i2type)
			{
				qualfound = NONEMPTY;
				De.ov_tend = De.ov_outtup;
				if (rlist)
				{
					(*De.ov_counter)++;
					interpret(rlist);
				}

				if (De.ov_tlist)
					dispose(mode);
				else
					if (De.ov_userqry)
						De.ov_tupsfound++;

				if (!De.ov_targvc)	/* constant Target list */
					break;


				/* process De.ov_bylist if any */
				if (De.ov_bylist)
				{
					interpret(De.ov_bylist);
					if ((j = getequal(De.ov_result, De.ov_outtup, agtup, &De.ov_uptid)) < 0)
						syserr("scan:getequal %d,%.12s", j, De.ov_result->reldum.relid);
	
					if (!j)
					{
						/* match on bylist */
						bmove(agtup, De.ov_outtup, De.ov_result->reldum.relwid);
						mode = mdREPL;
						(*De.ov_counter)++;
					}
					else
					{
						/* first of this bylist */
						mode = mdAPP;
						*De.ov_counter = 1;
					}
	
					De.ov_tend = De.ov_outtup + De.ov_result->reloff[domno];
					interpret(De.ov_alist);
					dispose(mode);
				}
			}
		}


		if (j < 0)
			syserr("scan:get prim %d %.12s", j, De.ov_source->reldum.relid);
	}
	if (De.ov_result)
	{
		if (j = noclose(De.ov_result))
			syserr("scan:noclose %d %.12s", j, De.ov_result->reldum.relid);
	}
	return (qualfound);
}
/*
**  DISPOSE
*/

dispose(mode)
{
	register int	i;

	i = 0;

	if (!De.ov_result)
	{
		if (Equel)
			equeleol(EOTUP);
		else
			printeol();
	}
	else
	{
#		ifdef xOTR1
		if (tTf(71, -1))
		{
			if (tTf(71, 1))
				printf("mode=%d,",mode);
			if (tTf(71, 2) && (mode == mdREPL || mode == mdDEL))
				printf("De.ov_uptid:%ld, ",De.ov_uptid);
			if (tTf(71, 3))
				if (mode == mdDEL)
					printup(De.ov_source, De.ov_intup);
				else
					printup(De.ov_result, De.ov_outtup);
		}
#		endif


		/* SPOOL UPDATES OF EXISTING USER RELNS TO BATCH PROCESSOR */
		if (De.de_buflag)
		{
			addbatch(&De.ov_uptid, De.ov_outtup, De.ov_origtup);
			return;
		}

		/* PERFORM ALL OTHER OPERATIONS DIRECTLY */
		switch (mode)
		{
		  case mdRETR:
		  case mdAPP:
			if ((i = insert(De.ov_result, &De.ov_uptid, De.ov_outtup, NODUPS)) < 0)
				syserr("dispose:insert %d %.12s", i, De.ov_result->reldum.relid);
			break;

		  case mdREPL:
			if ((i = replace(De.ov_result, &De.ov_uptid, De.ov_outtup, NODUPS)) < 0)
				syserr("dispose:replace %d %.12s", i, De.ov_result->reldum.relid);
			break;

		  case mdDEL:
			if ((i = delete(De.ov_result, &De.ov_uptid)) < 0)
				syserr("dispose:delete %d %.12s", i, De.ov_result->reldum.relid);
			break;

		  default:
			syserr("dispose:bad mode %d", mode);
		}
	}

	if (De.ov_userqry && i == 0)
		De.ov_tupsfound++;
}
