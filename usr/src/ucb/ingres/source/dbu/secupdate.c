# include	<ingres.h>
# include	<aux.h>
# include	<catalog.h>
# include	<symbol.h>
# include	<access.h>
# include	<batch.h>
# include	<sccs.h>

SCCSID(@(#)secupdate.c	7.1	2/5/81)

secupdate(r)
register DESC	*r;
{
	register char	*p;
	register int	i;
	int		j, domcnt, mode, dom;
	long		tupcnt;
	long		oldtid, newtid;
	long		lotid, hitid, uptid;
	char		oldtup[MAXTUP], newtup[MAXTUP];
	char		oldkey[MAXTUP], newkey[MAXTUP];
	char		dumtup[MAXTUP];
	struct index	itup;
	DESC		si_desc;
	extern DESC	Inddes;
	struct key_pt
	{
		char	*pt_old;
		char	*pt_new;
	};
	struct key_pt	keys[MAXKEYS+1];

	mode = Batchhd.mode_up;
	Batch_dirty = FALSE;
#	ifdef xZTR1
	if (tTf(49, -1))
		printf("SECUPDATE\n");
#	endif
	opencatalog("indexes", 0);
	setkey(&Inddes, &itup, r->reldum.relid, IRELIDP);
	setkey(&Inddes, &itup, r->reldum.relowner, IOWNERP);
	if (i = find(&Inddes, EXACTKEY, &lotid, &hitid, &itup))
		syserr("secupdate:find indexes %d", i);

	/* update each secondary index */
	while(!(i = get(&Inddes, &lotid, &hitid, &itup, TRUE)))
	{
		/* check if the index is on the right relation */
#		ifdef xZTR1
		if (tTf(49, 7))
			printup(&Inddes, &itup);
#		endif
		if (!bequal(itup.irelidp, r->reldum.relid, MAXNAME) ||
			!bequal(itup.iownerp, r->reldum.relowner, 2))
			continue;

		if (i = openr(&si_desc, 1, itup.irelidi))
			syserr("secupdate:can't openr %.12s %d", itup.irelidi, i);
		/* reposition batch file to the beginning. */
		if ((i = lseek(Batch_fp, 0L, 0)) < 0)
			syserr("secupdate:seek %d %d", i, Batch_fp);
		Batch_cnt = BATCHSIZE;
		getbatch(&Batchhd, sizeof Batchhd);	/* reread header */

		/* set up the key structure */
		p = itup.idom;
		for (domcnt = 0; domcnt < MAXKEYS; domcnt++)
		{
			if ((dom = *p++) == 0)
				break;	/* no more key domains */
#			ifdef xZTR1
			if (tTf(49, 15))
				
				printf("dom %d,tupo_off %d\n", dom, Batchhd.si[dom].tupo_off);
#			endif
			keys[domcnt].pt_old = &oldtup[Batchhd.si[dom].tupo_off];
			keys[domcnt].pt_new = &newtup[r->reloff[dom]];
		}

		/* the last domain is the "tidp" field */
		keys[domcnt].pt_old = (char *) &oldtid;
		keys[domcnt].pt_new = (char *) &newtid;

		/*
		** Start reading the batch file and updating
		** the secondary indexes.
		*/
		tupcnt = Batchhd.num_updts;
		while (tupcnt--)
		{
			getbatch(&oldtid, Batchhd.tido_size);
			getbatch(oldtup, Batchhd.tupo_size);
			getbatch(newtup, Batchhd.tupn_size);
			getbatch(&newtid, Batchhd.tidn_size);

			/* if this is a replace or append form the new key */
			if (mode != mdDEL)
			{
				for (j = 0; j <= domcnt; j++)
					setkey(&si_desc, newkey, keys[j].pt_new, j+1);
#				ifdef xZTR1
				if (tTf(49, 7))
					printup(&si_desc, newkey);
#				endif
			}

			/* if this is delete or replace form the old key */
			if (mode != mdAPP)
			{
				for (j = 0; j <= domcnt; j++)
					setkey(&si_desc, oldkey, keys[j].pt_old, j+1);
#				ifdef xZTR1
				if (tTf(49, 8))
					printup(&si_desc, oldkey);
#				endif
			}

			switch (mode)
			{

			  case mdDEL:
				if (i = getequal(&si_desc, oldkey, dumtup, &uptid))
				{
					if (i > 0)
						break;
					syserr("secupdate:getequal %d", i);
				}
				if ((i = delete(&si_desc, &uptid)) < 0)
					syserr("secupdate:delete %d", i);
				break;

			  case mdREPL:
				/* if the newtup = oldtup then do nothing */
				if (bequal(oldkey, newkey, si_desc.reldum.relwid))
					break;
				if (i = getequal(&si_desc, oldkey, dumtup, &uptid))
				{
					if (Batch_recovery && i > 0)
						goto secinsert;
					syserr("secupdate:getequal-repl %d", i);
				}
				if (i = replace(&si_desc, &uptid, newkey, TRUE))
				{
					/* if newtuple is dup of old, ok */
					if (i == 1)
						break;
					/* if this is recovery and old tid not there, try an insert */
					if (Batch_recovery && i == 2)
						goto secinsert;
					syserr("secupdate:replace %d", i);
				}
				break;

			  case mdAPP:
			  secinsert:
				if ((i = insert(&si_desc, &uptid, newkey, TRUE)) < 0)
					syserr("secupdate:insert %d", i);
			}
		}
		if (i = closer(&si_desc))
			syserr("secupdate:closer %.12s %d", si_desc.reldum.relid, i);
	}
	if (i < 0)
		syserr("secupdate:bad get from indexes %d", i);
}
