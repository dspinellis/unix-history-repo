# include	<ingres.h>
# include	<aux.h>
# include	<catalog.h>
# include	<symbol.h>
# include	<tree.h>
# include	"../decomp/globs.h"
# include	"strategy.h"
# include	<sccs.h>

SCCSID(@(#)strategy.c	7.1	2/5/81)

/*
** STRATEGY
**
**	Attempts to limit access scan to less than the entire De.ov_source
**	relation by finding a key which can be used for associative
**	access to the De.ov_source reln or an index thereon.  The key is
**	constructed from domain-value specifications found in the
**	clauses of the qualification list using sub-routine findsimp
**	in findsimp.c and other subroutines in file key.c
*/



strategy()
{
	register int		i, allexact;
	struct accessparam	sourceparm, indexparm;
	struct index		itup, rtup;
	struct key		lowikey[MAXKEYS+1], highikey[MAXKEYS+1];
	register DESC		*d;
	DESC			*openindex();
	extern DESC		Inddes;

#	ifdef xOTR1
	if (tTf(70, 0))
		printf("STRATEGY\tSource=%.12s\tNewq = %d\n",
		       De.ov_source ? De.ov_source->reldum.relid : "(none)",
		       De.de_newq);
#	endif

	while (De.de_newq)	/* if De.de_newq=TRUE then compute a new strategy */
			/* NOTE: This while loop is executed only once */
	{
		De.ov_scanr = De.ov_source;
	
		if (!De.ov_scanr)
			return (1);	/* return immediately if there is no source relation */
	
		De.ov_fmode = NOKEY;	/* assume a find mode with no key */
	
		if (!De.ov_qlist)
			break;	/* if no qualification then you must scan entire rel */
	
		/* copy structure of source relation into sourceparm */
		paramd(De.ov_source, &sourceparm);
	
		/* if source is unkeyed and has no sec index then give up */
		if (sourceparm.mode == NOKEY && De.ov_source->reldum.relindxd <= 0)
			break;

		/* find all simple clauses if any */
		if (!findsimps())
			break;	/* break if there are no simple clauses */
	
		/* Four steps are now performed to try and find a key.
		** First if the relation is hashed then an exact key is search for
		**
		** Second if there are secondary indexes, then a search is made
		** for an exact key. If that fails then a  check is made for
		** a range key. The result of the rangekey check is saved.
		**
		** Third if the relation is an ISAM a check is  made for
		** an exact key or a range key.
		**
		** Fourth if there is a secondary index, then if step two
		** found a key, that key is used.
		**
		**  Lastly, give up and scan the  entire relation
		*/
	
		/* step one. Try to find exact key on primary */
		if (exactkey(&sourceparm, De.ov_lkey_struct))
		{
			De.ov_fmode = EXACTKEY;
			break;
		}
	
		/* step two. If there is an index, try to find an exactkey on one of them */
		if (De.ov_source->reldum.relindxd)
		{
	
			opencatalog("indexes", 0);
			setkey(&Inddes, &itup, De.ov_source->reldum.relid, IRELIDP);
			setkey(&Inddes, &itup, De.ov_source->reldum.relowner, IOWNERP);
			if (i = find(&Inddes, EXACTKEY, &De.ov_lotid, &De.ov_hitid, (char *)&itup))
				syserr("strategy:find indexes %d", i);
	
			while (!(i = get(&Inddes, &De.ov_lotid, &De.ov_hitid, (char *)&itup, NXTTUP)))
			{
#				ifdef xOTR1
				if (tTf(70, 3))
					printup(&Inddes, (char *)&itup);
#				endif
				if (!bequal(itup.irelidp, De.ov_source->reldum.relid, MAXNAME) ||
				    !bequal(itup.iownerp, De.ov_source->reldum.relowner, 2))
					continue;
				parami(&itup, &indexparm);
				if (exactkey(&indexparm, De.ov_lkey_struct))
				{
					De.ov_fmode = EXACTKEY;
					d = openindex(itup.irelidi);
					/* temp check for 6.0 index */
					if ((int) d->reldum.relindxd == -1)
						ov_err(BADSECINDX);
					De.ov_scanr = d;
					break;
				}
				if (De.ov_fmode == LRANGEKEY)
					continue;	/* a range key on a s.i. has already been found */
				if (allexact = rangekey(&indexparm, lowikey, highikey))
				{
					bmove((char *)&itup, (char *)&rtup, sizeof itup);	/* save tuple */
					De.ov_fmode = LRANGEKEY;
				}
			}
			if (i < 0)
				syserr("stragery:bad get from index-rel %d", i);
			/* If an exactkey on a secondary index was found, look no more. */
			if (De.ov_fmode == EXACTKEY)
				break;
		}
	


		/* step three. Look for a range key on primary */
		if (i = rangekey(&sourceparm, De.ov_lkey_struct, De.ov_hkey_struct))
		{
			if (i < 0)
				De.ov_fmode = EXACTKEY;
			else
				De.ov_fmode = LRANGEKEY;
			break;
		}
	
		/* last step. If a secondary index range key was found, use it */
		if (De.ov_fmode == LRANGEKEY)
		{
			if (allexact < 0)
				De.ov_fmode = EXACTKEY;
			d = openindex(rtup.irelidi);
			/* temp check for 6.0 index */
			if ((int) d->reldum.relindxd == -1)
				ov_err(BADSECINDX);
			De.ov_scanr = d;
			bmove((char *)lowikey, (char *)De.ov_lkey_struct, sizeof lowikey);
			bmove((char *)highikey, (char *)De.ov_hkey_struct, sizeof highikey);
			break;
		}

		/* nothing will work. give up! */
		break;
	
	}

	/* check for De.de_newq = FALSE and no source relation */
	if (!De.ov_scanr)
		return (1);
	/*
	** At this point the strategy is determined.
	**
	** If De.ov_fmode is EXACTKEY then De.ov_lkey_struct contains
	** the pointers to the keys.
	**
	** If De.ov_fmode is LRANGEKEY then De.ov_lkey_struct contains
	** the pointers to the low keys and De.ov_hkey_struct
	** contains pointers to the high keys.
	**
	** If De.ov_fmode is NOKEY, then a full scan will be performed
	*/
#	ifdef xOTR1
	if (tTf(70, -1))
		printf("De.ov_fmode= %d\n",De.ov_fmode);
#	endif

	/* set up the key tuples */
	if (De.ov_fmode != NOKEY)
	{
		if (setallkey(De.ov_lkey_struct, De.ov_keyl))
			return (0);	/* query false. There is a simple
					** clause which can never be satisfied.
					** These simple clauses can be choosey!
					*/
	}

	if (i = find(De.ov_scanr, De.ov_fmode, &De.ov_lotid, &De.ov_hitid, De.ov_keyl))
		syserr("strategy:find1 %.12s, %d", De.ov_scanr->reldum.relid, i);

	if (De.ov_fmode == LRANGEKEY)
	{
		setallkey(De.ov_hkey_struct, De.ov_keyh);
		if (i = find(De.ov_scanr, HRANGEKEY, &De.ov_lotid, &De.ov_hitid, De.ov_keyh))
			syserr("strategy:find2 %.12s, %d", De.ov_scanr->reldum.relid, i);
	}

#	ifdef xOTR1
	if (tTf(70, 1))
	{
		printf("Lo");
		dumptid(&De.ov_lotid);
		printf("Hi");
		dumptid(&De.ov_hitid);
	}
#	endif

	return (1);
}
