# include	<pv.h>
# include	<ingres.h>
# include	<aux.h>
# include	<catalog.h>
# include	<access.h>
# include	<batch.h>
# include	<opsys.h>
# include	<sccs.h>

SCCSID(@(#)modupdate.c	7.1	2/5/81)

/*
** MODUPDATE
**	This routine is used to exicute the updates 
**	for modifies so they are recoverable.
**	It is also used by restore to complete an aborted modify.
**	During a restore the Batch_recover flag should be set to 1;
*/


modupdate()
{
	char			batchname[MAXNAME + 3];
	char			temprel[MAXNAME+ 3];
	char			relfile[MAXNAME + 3];
	register int		i;
	register int		j;
	struct stat		sbuf;
	char			aflag;
	struct tup_id		tid;
	DESC			desx;
	struct attribute	atttup;
	struct relation		oldreltup;
	struct index		ikey, itup;
	PARM			newpv[2];
	extern DESC		Inddes, Attdes, Reldes;
	register DESC		*desp;
	extern char		*trim_relname();
	extern int		errno;


	desp =  &desx;
	concat(MODBATCH,Fileset,batchname);
	concat(MODTEMP, Fileset, temprel);

#	ifdef xZTR1
	if (tTf(34, 8))
		printf("Modupdate: %s, %s\n",batchname, temprel);
#	endif
	if ((Batch_fp = open(batchname, 0)) < 0)
		syserr("MODUPDATE:Can't open %s", batchname);
	Batch_cnt = BATCHSIZE;
	Batch_dirty = FALSE;
	getbatch(desp, sizeof *desp);
	ingresname(desp->reldum.relid, desp->reldum.relowner, relfile);

	/* don't loose old file before verifying new file */
	if (stat(temprel, &sbuf) >= 0)
	{
		unlink(relfile);	/* Ok if failure */
		errno = 0;
		if (link(temprel, relfile) == -1)
			syserr("MODUPDATE:Can't link: %s, %s", temprel, relfile);
		if (unlink(temprel) < 0)
			syserr("modupdate:unlink(%s)", temprel);
	}

	else
		if(stat(relfile, &sbuf) < 0 || !Batch_recovery)
			syserr("MODUPDATE:Relation and/or temporary files for %s are missing",
				relfile);


	/* Update admin if this is relation or atribute relations */
	/* Should only happen in Sysmod				  */
	if ((aflag = bequal(desp->reldum.relid, "attribute   ", 12)) ||
		bequal(desp->reldum.relid, "relation    ", 12))
	{
		ingresname(desp->reldum.relid, desp->reldum.relowner, temprel);
		if ((i = open("admin", 2)) < 0)
			syserr("MODUPDATE:Can't open admin file");
		if (lseek(i, (long) sizeof Admin.adhdr, 0) < 0 ||
			(aflag && lseek(i, (long) sizeof *desp, 1) < 0))
			syserr("MODUPDATE:Seek error");
		if (write(i, desp, sizeof *desp) != sizeof *desp)
			syserr("MODUPDATE:Write error on admin");
		close(i);

		if (aflag)
		{
			closer(&Attdes);
			cleanrel(&Admin.adattd);
			close(Admin.adattd.relfp);
			bmove(desp, &Admin.adattd, sizeof *desp);
			ingresname(Admin.adattd.reldum.relid, Admin.adattd.reldum.relowner, temprel);
			if ((Admin.adattd.relfp = open(temprel, 2)) < 0)
				syserr("MODUPDATE: open wr Admin.adattd %d", Admin.adattd.relfp);
			Admin.adattd.relopn = (Admin.adattd.relfp + 1) * -5;
		}
		else
		{
			closer(&Reldes);
			cleanrel(&Admin.adreld);
			close(Admin.adreld.relfp);
			bmove(desp, &Admin.adreld, sizeof *desp);
			if ((Admin.adreld.relfp = open(temprel, 2)) < 0)
				syserr("MODUPDATE: open Admin.adreld %d",
					Admin.adreld.relfp);
			Admin.adreld.relopn = (Admin.adreld.relfp + 1) * -5;
		}
	}

	if (i = get(&Admin.adreld, &desp->reltid, &desp->reltid, &oldreltup, FALSE))
		syserr("MODUPDATE: get oldrel=%d",i);
	/* update relation relation */

	if ((i = replace(&Admin.adreld, &desp->reltid, desp, FALSE)))
		if (i < 0 || i == 2)
			syserr("MODUPDATE:Replace error(rel): %d", i);
	if (i = cleanrel(&Admin.adreld))
		syserr("MODUPDATE:clean rel %d", i);

	/* update attribute relation */
	Admin.adattd.relopn = (Admin.adattd.relfp + 1) * -5;
	for (i = desp->reldum.relatts; i > 0; i--)
	{
		getbatch(&tid, sizeof tid);
		getbatch(&atttup, sizeof atttup);
		if (j = replace(&Admin.adattd, &tid, &atttup, FALSE))
			if (j < 0 || j == 2)
				syserr("MODUPDATE:Replace error(att): %d", j);
	}

	if (i = cleanrel(&Admin.adattd))
		syserr("MODUPDATE:clean att %d", i);

	/* make the admin readonly */
	Admin.adattd.relopn = (Admin.adattd.relfp + 1) * 5;

	close(Batch_fp);

	/* if this is an index, change the relspec in the index catalog */
	if (oldreltup.relindxd < 0)
	{
		opencatalog("indexes", 2);
		setkey(&Inddes, &ikey, desp->reldum.relid, IRELIDI);
		setkey(&Inddes, &ikey, desp->reldum.relowner, IOWNERP);
		if ((i = getequal(&Inddes, &ikey, &itup, &tid)) == 0)
		{
			itup.irelspeci = desp->reldum.relspec;
			if ((i = replace(&Inddes, &tid, &itup, 0)) != 0)
				if (i < 0 || i == 2)
					syserr("MODUPDATE: rep(ix) %d", i);
		}
	}

	else if(desp->reldum.relindxd > 0)
	{
		/* destroy any secondary indexes on this primary */
		opencatalog("indexes", 2);
		setkey(&Inddes, &ikey, desp->reldum.relid, IRELIDP);
		setkey(&Inddes, &ikey, desp->reldum.relowner, IOWNERP);
		while ((i = getequal(&Inddes, &ikey, &itup, &tid)) == 0)
		{
			newpv[0].pv_val.pv_str = itup.irelidi;
			newpv[1].pv_type = PV_EOF;
			printf("destroying secondary index %s\n", trim_relname(itup.irelidi));
			if (destroy(1, newpv))
				syserr("MODUPDATE:Can't destroy %s", itup.irelidi);
		}
	}
	if (i < 0)
		syserr("MODUPDATE: geteq(ix)b %d", i);

	/* clean things up and exit */
	unlink(batchname);
#	ifdef xZTR1
	if (tTf(34, 8))
		printf("Leaving modupdate\n");
#	endif
	return (0);
}
