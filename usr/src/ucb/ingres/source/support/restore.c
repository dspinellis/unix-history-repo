# include	<stdio.h>
# include	<ingres.h>
# include	<aux.h>
# include	<catalog.h>
# include	<access.h>
# include	<batch.h>
# include	<opsys.h>
# include	<lock.h>
# include	<symbol.h>
# include	<resp.h>
# include	<dir.h>
# include	<sccs.h>

SCCSID(@(#)restore.c	7.2	11/5/82)

/*
** INGRES crash recovery processor
**	to recover a database you must be the dba or the ingres superuser
**	RESTORE attempts to complete updates from batch files left in a
**	database.  After finishing all the batch files it calls PURGE.
*/

# ifndef PURGE
# ifdef xV7_UNIX
# define	PURGE		"purge"
# else xV7_UNIX
# define	PURGE		"/usr/bin/purge"
# endif xV7_UNIX
# endif PURGE

/* first file to close on error */
# define	CLOSEFILES 	3

extern int	Status;
extern char	*Usercode;
char		Utemp[2];
char		*Fileset;
char		Berror;		/* batch error */
char		Error;
extern char	Ask;
extern char	Superuser;
extern char	All;
extern char	Qrymod;
int		Direc		= CLOSEFILES - 1;
extern int	Wait_action;
short		tTvect[100];
short		tTdbu[100];
struct resp	Resp;


struct direc
{
	int	inumber;
	char	fname[15];
};



main(argc, argv)
int	argc;
char	*argv[];
{
	register int	fd;
	register int	i;
	register char	*dbname;
	extern char	*Proc_name;
	auto int	stat;
	extern		(*ExitFn)();
	extern          rubproc(), exit();
	char		*nargv[20];
	char		**avp;
	char		**fvp;
	extern char	*Flagvect[];
	extern char	*getnxtdb();
	char		*lookucode();

	Proc_name = "RESTORE";

	/* check param list */
	argv[argc] = NULL;
# 	ifdef xTTR1
	tTrace(argv, 'T', tTvect, 100);
	tTrace(argv, 'Z', tTdbu, 100);
#	endif

	initialize(argc, argv);

	/* do it to it */
	ExitFn = rubproc;
	signal(3, exit);
	while (dbname = getnxtdb())
	{
		Berror = Error = 0;

		/* first restart point for this database */
		setexit();
		if (Error)	/* if set, will cause skip to next database */
			continue;
		printf("\nRestoring database: %s\t", dbname);

		acc_init();
		printf("owner: %s\n", lookucode(Admin.adhdr.adowner));

		/* set exclusive lock on data base */
		db_lock(M_EXCL);

		restore();	/* recover batch update and modify files */
		printf("\tRecovery of batch files complete.\n");

		/*
		** second restart point for this database
		**	the batch files are completed and now the system
		**	relations need checking
		*/
		setexit();
		if (Error)		/* again, may cause skipping to next database */
			continue;
		printf("\tChecking system relations\n");


		/*
		** check the relation relation
		**	this will mean checking for file existence,
		**	and whether the relstat bits are supported by
		**	the information in the other catalogs.
		*/
		checkrel();

		/*
		** check the attribute relation
		**	for each tuple in the attribute relation, there must
		**	be a tuple in the relation relation.
		**	the indexes relation doesn't need to be reverse checked
		**	into the relation relation since the order things are
		**	handled else where in the system is in the correct
		**	order.  All the other catalogs need to be reverse checked.
		*/
		checkatts();

		/* only check the qrymod catalogs if qrymod is turned on */
		if (Qrymod)
		{
			/* check the protect relation */
			checkprotect();

			/* check the integrities relation */
			checkinteg();

			/*
			** check the tree relation
			** must be done last since it depends upon
			** a state of the system relations provided
			** by the other check... routines.
			*/
			checktree();
		}

		/* finished, close up the database and go on to the next */
		closecatalog(TRUE);
		unldb();
		acc_close();

		/* call PURGE if no errors */
		if (!Berror && !Error)
		{
			printf("\tCalling purge: ");
			fflush(stdout);
			if ((i = fork()) == -1)
				printf("Can't fork\n");
			else if (!i)
			{
				avp = nargv;
				*avp++ = "Purge";
				for (fvp = Flagvect; *fvp != NULL; )
					*avp++ = *fvp++;
				*avp++ = dbname;
				*avp++ = 0;
#				ifdef	xTTR2
				if (tTf(0, 1))
					for (avp = nargv, i = 0; *avp != NULL; avp++, i++)
						printf("%d %s\n", i, *avp);
#				endif
				for (i=3; i <= NOFILE; i++)
					close(i);
				execv(ztack(Pathname, "/bin/purge"), nargv);
# ifdef xV7_UNIX
				execvp(PURGE, nargv);
# else xV7_UNIX
				execv(PURGE, nargv);
# endif xV7_UNIX
				printf("Cannot exec %s\n", PURGE);
				exit(-1);
			}
			else
				wait(&stat);
		}
	}
}
/*
** RESTORE -- find the batch files and process them
*/
restore()
{
	DESC			descr;
# ifdef	DIRBLKSIZ
	register	DIR	*dirp;
	register struct	direct  *dp;
# else	DIRBLKSIZ
	struct direc		dir;
	register struct direc	*d;
	register int		dfd;
# endif	DIRBLKSIZ
	register int		i;
	extern char		*Fileset;
	extern 			uperr(), (*ExitFn)();
	int			(*tmpfn)();
	char			*lookucode();

# ifdef	DIRBLKSIZ
	if ( (dirp = opendir(".")) == NULL )
		syserr("Can't open data base directory");
# else	DIRBLKSIZ
	d = &dir;
	if ((dfd = open(".", 0)) < 0)
		syserr("cannot open database directory");
	d->fname[14] = 0;
# endif	DIRBLKSIZ
	bmove(Usercode, Utemp, 2);
	Batch_recovery = 1;
	tmpfn = ExitFn;
	ExitFn = uperr;

	/* restart point */
	setexit();
# ifdef	DIRBLKSIZ
	for ( dp = readdir(dirp) ; dp != NULL ; dp = readdir(dirp) )
# else	DIRBLKSIZ
	while (read(dfd, d, 16) == 16)
# endif	DIRBLKSIZ
	{
# ifdef	DIRBLKSIZ
		if ( !strcmp(".",dp->d_name) || !strcmp("..",dp->d_name) )
# else	DIRBLKSIZ
		if (d->inumber == 0)
# endif	DIRBLKSIZ
			continue;
# ifdef	DIRBLKSIZ
		if (bequal("_SYSbatch", dp->d_name, 9))
# else	DIRBLKSIZ
		if (bequal("_SYSbatch", d->fname, 9))
# endif	DIRBLKSIZ
		{
# ifdef	DIRBLKSIZ
			Fileset = &dp->d_name[9];
# else	DIRBLKSIZ
			Fileset = &d->fname[9];
# endif	DIRBLKSIZ
			Batch_fp = open(batchname(), 0);
			Batch_cnt = BATCHSIZE;
			getbatch(&Batchhd, sizeof(Batchhd));
# ifdef	DIRBLKSIZ
			printf("\tFound batch file:  %s\n", dp->d_name);
# else	DIRBLKSIZ
			printf("\tFound batch file:  %s\n", d->fname);
# endif	DIRBLKSIZ
			printf("\tRelation: %s\tUser: %s\n", Batchhd.rel_name,
				lookucode(Batchhd.userid));
			close(Batch_fp);
			bmove(Batchhd.userid, Usercode, 2);
			if(ask("\tUpdate? "))
				update();
		}
# ifdef	DIRBLKSIZ
		if (bequal(MODBATCH, dp->d_name, sizeof(MODBATCH) - 1))
		{
			Fileset = &dp->d_name[sizeof(MODBATCH) - 1];
			if ((Batch_fp = open(dp->d_name, 0)) < 0)
				syserr("Can't open %s", dp->d_name);
# else	DIRBLKSIZ
		if (bequal(MODBATCH, d->fname, sizeof(MODBATCH) - 1))
		{
			Fileset = &d->fname[sizeof(MODBATCH) - 1];
			if ((Batch_fp = open(d->fname, 0)) < 0)
				syserr("Can't open %s", d->fname);
# endif	DIRBLKSIZ
			Batch_cnt = 0;
			if((i = getbatch(&descr, sizeof(descr))) != sizeof(descr))
				syserr(" cant read %d",i);
			printf("\tFound incomplete modify of %.12s, user = %s\n",
				descr.reldum.relid, lookucode(descr.reldum.relowner));

			bmove(descr.reldum.relowner, Usercode, sizeof(descr.reldum.relowner));
			close(Batch_fp);
			if (ask("\tComplete? "))
				modupdate();
		}
	}
	bmove(Utemp, Usercode, 2);
	ExitFn = tmpfn;
# ifdef	DIRBLKSIZ
	closedir(dirp);
# else	DIRBLKSIZ
	close(dfd);
# endif	DIRBLKSIZ
}
/*
** handles syserr's in the update processor
*/
uperr()
{

	if (Batch_fp)
		close(Batch_fp);
	Berror++;
	reset();
}



/*
** Catch errors in other places
*/
rubproc()
{
	register int		i;
	register struct desxx	*p;
	extern struct desxx	Desxx[];
	extern int		Acc_init;

	Error++;
	printf("Unable to restore!\n");

	/* restore user code */
	bmove(Utemp, Usercode, sizeof Utemp);

	/* close all possible files */
	if (Acc_init)
	{
		closecatalog(TRUE);
		unldb();
		acc_close();
	}

	/* close users file */
	getuser(0);

	/* get everything else */
	for (i = Direc + 1; i <= NOFILE; i++)
		close(i);
}
/*
** looks up user by usercode in users file
*/
char *
lookucode(ucode)
char	ucode[2];
{
	static char	buf[MAXLINE + 1];
	register char	*p;

	if (getuser(ucode, buf))
		syserr("cannot identify user %.2s", ucode);
	for (p = buf; *p != ':'; p++);
	*p = 0;
	return (buf);
}
/*
** CHECKATTS
**	Checks that all attributes are in a relation
*/
checkatts()
{
	extern DESC		Reldes, Attdes;
	register int		i;
	register int		once;
	TID			tid, limtid, reltid;
	char			key[MAXTUP];
	struct attribute	atttup;
	struct relation		reltup;
	char			lastrel[MAXNAME + 2];

	once = 0;
	opencatalog("relation", 2);
	opencatalog("attribute", 2);
	clearkeys(&Attdes);
	lastrel[0] = '\0';
	if (find(&Attdes, NOKEY, &tid, &limtid))
		syserr("CHECKATT: find");

	while (!(i = get(&Attdes, &tid, &limtid, &atttup, TRUE)))
	{
		if (bequal(atttup.attrelid, lastrel, MAXNAME + 2))
			continue;

		clearkeys(&Reldes);
		setkey(&Reldes, key, atttup.attrelid, ATTRELID);
		setkey(&Reldes, key, atttup.attowner, ATTOWNER);

		if (i = getequal(&Reldes, key, &reltup, &reltid))
		{
			if (i < 0)
				syserr("ATTCHECK: getequal");
			if (!once++)
				printf("\tNo relation for attribute(s):\n");
			printf("\t");
			printup(&Attdes, &atttup);
			if (ask("\tDelete?"))
				if (i = delete(&Attdes, &tid))
					syserr("ATTCHECK: delete=%d", i);
		}
		else
			bmove(atttup.attrelid, lastrel, MAXNAME + 2);
	}

	if (i < 0)
		syserr("ATTCHECK: get=%d", i);
}
/*
** CHECKREL -- check relation relation against every thing else
**
**	Each tuple in the relation relation is read and each verifiable
**	characteristic is checked for accuracy.  Including the existence
**	of the physical file (if not a view), the qrymod definition if
**	appropriate and the secondary indexing.
*/

checkrel()
{
	extern DESC	Reldes;
	register int	i, j;
	struct relation	rel;
	TID		rtid, limtid;
	char		fname[MAXNAME + 3];

	/* setup for search of entire relation */
	opencatalog("relation", 2);
	clearkeys(&Reldes);
	if (find(&Reldes, NOKEY, &rtid, &limtid))
		syserr("CHECKREL: find");

	/* loop until all tuples checked */
	for (;;)
	{
		/* for each tuple in the rel-rel */
		i = get(&Reldes, &rtid, &limtid, &rel, TRUE);
		if (i > 0)
			break;	/* have finished */
		if (i < 0)
			syserr("CHECKREL: get=%d", i);

		/* if not a view, check for the file */
		if ((rel.relstat & S_VIEW) != S_VIEW)
		{
			ingresname(rel.relid, rel.relowner, fname);
			if ((j = open(fname, 2)) == -1)
			{
				printf("\tNo file for:\n\t");
				printup(&Reldes, &rel);
				if (ask("\tDelete tuple? "))
				{
					if(j = delete(&Reldes, &rtid))
						syserr("CHECKREL: delete=%d", j);
					continue;
				}
				else
					/* don't call purge the file might still be there */
					Error++;
			}
			else
				close(j);
		}

		/* does it think that it has a secondary index */
		if (rel.relindxd > 0)
		{
			/* does it really have an index? */
			if (!hasndx(rel.relid, rel.relowner))
			{
				/* no, should it be fixed */
				printf("\tNo indexes entry for primary relation:\n\t");
				printup(&Reldes, &rel);
				if (ask("\tAdjust? "))
				{
					/* fix up relation relation entry */
					rel.relindxd = 0;
					if (i = replace(&Reldes, &rtid, &rel, FALSE))
						syserr("CHECKREL: replace=%d", i);
				}
			}
		}

		/* does it think that it is a secondary index */
		if (rel.relindxd < 0)
		{
			/* check to make sure */
			if (!isndx(rel.relid, rel.relowner))
			{
				/* none, what should be done? */
				printf("\tNo indexes entry for index:\n\t");
				printup(&Reldes, &rel);
				if(ask("\tDelete? "))
				{
					/*
					** get rid of rel-rel tuple for
					** secondary index,
					** purge will do rest of
					** removal if necessary
					*/
					if (i = delete(&Reldes, &rtid))
						syserr("CHECKREL: delete=%d", i);
					continue;	/* go on to next tuple */
				}
			}
		}

		/* if qrymod on in the database, check those catalogs too */
		if (Qrymod)
		{
			/*
			** cannot deal with S_VBASE since there is no way to
			** find the tree catalog entries without decoding the
			** 'treetree' fields.
			**
			** check to see if this is a view
			*/
			if ((rel.relstat & S_VIEW) && !havetree(rel.relid, rel.relowner, mdVIEW))
			{
				/* no entry, should it be fixed? */
				printf("\tNo tree entry for this view:\n\t");
				printup(&Reldes, &rel);
				if (ask("\tDelete tuple? "))
				{
					/* delete relation entry */
					if (i = delete(&Reldes, &rtid))
						syserr("CHECKREL: delete=%d", i);
					continue;	/* skip to next entry in rel-rel */
				}
			}

			/* check to see if has 'protect' entry */
			if ((rel.relstat & S_PROTUPS) && !isprot(rel.relid, rel.relowner, -1))
			{
				/* no entry, should the bit be reset */
				printf("\tNo protect entry for:\n\t");
				printup(&Reldes, &rel);
				if (ask("\tAdjust? "))
				{
					/* fix the bit */
					rel.relstat &= ~S_PROTUPS;
					if (i = replace(&Reldes, &rtid, &rel, FALSE))
						syserr("CHECKREL: replace=%d", i);
				}
			}

			/* check to see if has 'integrities entry */
			if ((rel.relstat & S_INTEG) && !isinteg(rel.relid, rel.relowner, -1))
			{
				/* no entry, should bit be reset */
				printf("\tNo integrities entry for:\n\t");
				printup(&Reldes, &rel);
				if (ask("\tAdjust? "))
				{
					/* fix up the bit */
					rel.relstat &= ~S_INTEG;
					if (i = replace(&Reldes, &rtid, &rel, FALSE))
						syserr("CHECKREL: replace=%d", i);
				}
			}
		}
	}
}
/*
** HASNDX -- the relation indicated an index, check it out
**
**	will search the index relation for all secondary indexes
**	and check to see that each secondary index named has an
**	entry in the relation relation.
*/
hasndx(id, own)
char	id[MAXNAME];
char	own[2];
{
	register int	hasindexes;
	register int	i, j;
	extern DESC	Reldes, Inddes;
	TID		rtid;
	struct relation	rkey, rel;
	TID		itid, ihitid;
	struct index	ikey, ind;

	/* presume that answer is negative */
	hasindexes = FALSE;

	/* set search for all tuples with 'id' and 'own' in indexes */
	opencatalog("indexes", 2);
	clearkeys(&Inddes);
	setkey(&Inddes, &ikey, id, IRELIDP);
	setkey(&Inddes, &ikey, own, IOWNERP);
	if (find(&Inddes, EXACTKEY, &itid, &ihitid, &ikey))
		syserr("HASNDX: find");

	/* for each possible tuple in the indexes relation */
	for (;;)
	{
		i = get(&Inddes, &itid, &ihitid, &ind, TRUE);

		/* check return values */
		if (i < 0)
			syserr("HASNDX: get=%d\n", i);
		if (i > 0)
			break;	/* finished */

		/* if key doesn't match, skip to next tuple */
		if(kcompare(&Inddes, &ikey, &ind))
			continue;
		hasindexes = TRUE;

		/* verify that primary entry for sec index exists */
		opencatalog("relation", 2);
		clearkeys(&Reldes);
		setkey(&Reldes, &rkey, ind.irelidi, RELID);
		setkey(&Reldes, &rkey, ind.iownerp, RELOWNER);
		if (j = getequal(&Reldes, &rkey, &rel, &rtid, FALSE))
		{
			/* one doesn't exist, should we ignore it */
			if (j < 0)
				syserr("HASNDX: getequal=%d", j);
			printf("\tNo secondary index for indexes entry:\n\t");
			printup(&Inddes, &ind);
			if (ask("\tDelete? "))
			{
				/* get rid of bad entry in indexes relation */
				if (j = delete(&Inddes, &itid))
					syserr("HASNDX: delete=%d", j);
				hasindexes = FALSE;
			}
		}
	}
	return (hasindexes);
}
/*
** ISNDX -- so you think that you're a secondary index, I'll check it out.
**
**	searches the indexes relation for the name of the primary relation
**	and check to see if the primary is real.  Will also update the
**	'relindxd' field of the primary if it isn't correct.
*/
isndx(id, own)
char	id[MAXNAME];
char	own[2];
{
	register int	isindex;
	register int	i;
	extern DESC	Inddes;
	TID		itid;
	struct index	ind, ikey;
	extern DESC	Reldes;
	TID		rtid;
	struct relation	rel, rkey;

	/* search for tuple in index relation, should only be one */
	opencatalog("indexes", 2);
	clearkeys(&Inddes);
	setkey(&Inddes, &ikey, id, IRELIDI);
	setkey(&Inddes, &ikey, own, IOWNERP);
	if (i = getequal(&Inddes, &ikey, &ind, &itid))
	{
		/* there isn't a tuple in the indexes relation */
		if (i < 0)
			syserr("ISNDX: getequal=%d", i);
		isindex = FALSE;
	}
	else
	{
		isindex = TRUE;

		/* there is a tuple in the indexes relation */
		opencatalog("relation", 2);
		clearkeys(&Reldes);
		setkey(&Reldes, &rkey, ind.irelidp, RELID);
		setkey(&Reldes, &rkey, ind.iownerp, RELOWNER);

		/* see if the primary relation exists */
		if (i = getequal(&Reldes, &rkey, &rel, &rtid))
		{
			/* no it doesn't */
			if (i < 0)
				syserr("ISNDX: getequal=%d", i);

			/* what should be done about it */
			printf("\tNo primary relation for index:\n\t");
			printup(&Inddes, &ind);
			if (ask("\tDelete?"))
			{
				/*
				** get rid of indexes tuple,
				** a FALSE return will also get rid
				** of the relation tuple
				*/
				if (i = delete(&Inddes, &itid))
					syserr("ISNDX: delete=%d", i);
				isindex = FALSE;
			}
		}
		else if (!(rel.relindxd > 0) || (rel.relstat & S_INDEX) == S_INDEX)
		{
			/*
			** the primary tuple exists but isn't marked correctly
			*/
			printf("\t%.12s is index for:\n\t", rel.relid);
			printup(&Reldes, &rel);
			if (ask("\tMark as indexed? "))
			{
				rel.relstat |= S_INDEX;
				rel.relindxd = SECBASE;
				if (i = replace(&Reldes, &rtid, &rel, FALSE))
					syserr("ISNDX: replace=%d", i);
			}
		}
	}
	return (isindex);
}
/*
** HAVETREE -- check tree catalog for an entry with right name and owner
**
**	The 'id' and 'own' parameters are used to look in the tree catalog
**	for at least on tuple that also has a 'treetype' of 'mdvalue'.
**
**	If any tuples are found, havetree returns TRUE, else FALSE
*/

havetree(id, own, mdvalue)
char	id[MAXNAME];
char	own[2];
int	mdvalue;
{
	extern DESC	Treedes;
	register int	i;
	struct tree	tkey, trent;
	TID		ttid, thitid;

	/* search tree relation for tuple that matches */
	opencatalog("tree", 2);
	clearkeys(&Treedes);
	setkey(&Treedes, &tkey, id, TREERELID);
	setkey(&Treedes, &tkey, own, TREEOWNER);
	setkey(&Treedes, &tkey, &mdvalue, TREETYPE);

	/* set search limit tids from the key */
	if (i = find(&Treedes, EXACTKEY, &ttid, &thitid, &tkey))
		syserr("HAVETREE: find=%d", i);

	for (;;)
	{
		i = get(&Treedes, &ttid, &thitid, &trent, TRUE);

		if (i < 0)
			syserr("HAVETREE: get=%d", i);
		if (i > 0)
			break;	/* finished, didn't find one */
		
		if (kcompare(&Treedes, &tkey, &trent) == 0)
			return (TRUE);
	}
	return (FALSE);
}
/*
** ISPROT -- check in the 'protect' catalog for a tuple with right name, owner
**
**	search the 'protect' catalog for at least on tuple with matches the
**	values in the parameters. If 'treeid' is >= 0 then it is not used as
**	a key.
**
**	if one is found, returns TRUE, otherwise, returns FALSE
*/

isprot(id, own, treeid)
char	id[MAXNAME];
char	own[2];
int	treeid;
{
	extern DESC	Prodes;
	register int	i;
	struct protect	pkey, pent;
	TID		ptid, phitid;

	/* search the protect relation for at least on matching tuple */
	opencatalog("protect", 2);
	clearkeys(&Prodes);
	setkey(&Prodes, &pkey, id, PRORELID);
	setkey(&Prodes, &pkey, own, PRORELOWN);
	if (treeid >= 0)
		setkey(&Prodes, &pkey, &treeid, PROTREE);

	/* set search limit tids from the keys */
	if (i = find(&Prodes, EXACTKEY, &ptid, &phitid, &pkey))
		syserr("ISPROT: find=%d", i);

	for (;;)
	{
		i = get(&Prodes, &ptid, &phitid, &pent, TRUE);

		if (i < 0)
			syserr("ISPROT: get=%d", i);
		if (i > 0)
			break;	/* finished, didn't find one */
		
		if (kcompare(&Prodes, &pkey, &pent) == 0)
			return (TRUE);
	}
	return (FALSE);
}
/*
** ISINTEG -- check for a tuple in 'integrities'
**
**	searches the integrities relation for 'id' and 'own'.
**
**	returns TRUE if one is found, else FALSE
*/

isinteg(id, own, treeid)
char	id[MAXNAME];
char	own[2];
int	treeid;
{
	extern DESC		Intdes;
	register int		i;
	struct integrity	inkey, integ;
	TID			intid, inhitid;

	/* search the entire relation for a tuple that matches */
	opencatalog("integrities", 2);
	clearkeys(&Intdes);
	setkey(&Intdes, &inkey, id, INTRELID);
	setkey(&Intdes, &inkey, own, INTRELOWNER);
	if (treeid >= 0)
		setkey(&Intdes, &inkey, &treeid, INTTREE);

	/* set the search limit tids from the key */
	if (i = find(&Intdes, EXACTKEY, &intid, &inhitid, &inkey))
		syserr("ISINTEG: find=%d", i);
	
	for (;;)
	{
		i = get(&Intdes, &intid, &inhitid, &integ, TRUE);

		if (i < 0)
			syserr("ISINTEG: get=%d", i);
		if (i > 0)
			break;	/* finished, didn't find one */
		
		if (kcompare(&Intdes, &inkey, &integ) == 0)
			return (TRUE);
	}
	return (FALSE);
}
/*
** CHECKTREE -- check the tree catalog against the others
*/

checktree()
{ 
	extern DESC	Treedes, Reldes;
	register int	i;
	struct tree	tkey, trent;
	TID		ttid, thitid;
	struct relation	rkey, rel;
	TID		rtid;

	/* search the entire tree catalog */
	opencatalog("tree", 2);
	clearkeys(&Treedes);
	if (i = find(&Treedes, NOKEY, &ttid, &thitid))
		syserr("CHECKTREE: find=%d", i);
	
	/* for each tuple in 'tree' */
	for (;;)
	{
		i = get(&Treedes, &ttid, &thitid, &trent, TRUE);
		if (i > 0)
			break;	/* finished */
		if (i < 0)
			syserr("CHECKTREE: get=%d", i);
		
		/* verify that a tuple exists in the relation relation */
		opencatalog("relation", 2);
		clearkeys(&Reldes);
		setkey(&Reldes, &rkey, trent.treerelid, RELID);
		setkey(&Reldes, &rkey, trent.treeowner, RELOWNER);

		/* fetch the tuple */
		if (i = getequal(&Reldes, &rkey, &rel, &rtid))
		{
			/*
			** Oops, a tuple doesn't exist in the relation
			** relation.
			**
			** maybe it's just a fatal error
			*/
			if (i < 0)
				syserr("CHECKTREE: getequal=%d", i);

			/* not a fatal error, what to do about it? */
			printf("\tNo relation tuple for:\n\t");
			printup(&Treedes, &trent);
			if (ask("\tDelete? "))
			{
				if (i = delete(&Treedes, &ttid))
					syserr("CHECKTREE: delete=%d", i);
				continue;	/* go on to next tuple */
			}
		}
		else
		{
			/*
			** Ah. A tuple does exist.
			**
			** If the relstat bits are correct then we can stop
			** here since elsewhere the 'protect' and 'integrity'
			** entries were verified.
			*/
			switch (trent.treetype)
			{
			  case mdVIEW:
				/* mere existence is sufficient */
				break;

			  case mdPROT:
				if ((rel.relstat & S_PROTUPS) != S_PROTUPS)
				{
					printf("\tNo 'protect' entry for:\n\t");
				deltup:
					printup(&Treedes, &trent);
					if (ask("\tDelete? "))
					{
						if (i = delete(&Treedes, &ttid))
							syserr("CHECKTREE: delete=%d", i);
						continue;
					}
				}
				break;

			  case mdINTEG:
				if ((rel.relstat & S_INTEG) != S_INTEG)
				{
					printf("\tNo 'integrities' entry for:\n\t");
					goto	deltup;
				}
				break;

			  default:
				syserr("Unknown treetype: %d\n", trent.treetype);
			}
		}
	}
}
/*
**  CHECKPROTECT
*/

checkprotect()
{
	register int	i;
	extern DESC	Reldes, Prodes;
	struct protect	pkey, pent;
	TID		ptid, phitid;
	struct relation	rkey, rel;
	TID		rtid;

	/* for each entry in the 'protect' relation */
	opencatalog("protect", 2);
	clearkeys(&Prodes);
	if (i = find(&Prodes, NOKEY, &ptid, &phitid))
		syserr("CHECKPROTECT: find=%d", i);
	
	for (;;)
	{
		i = get(&Prodes, &ptid, &phitid, &pent, TRUE);
		if (i > 0)
			break;	/* finished */
		if (i < 0)
			syserr("CHECKPROTECT: get=%d", i);

		/* verify that a tuple exists in 'relation' */
		opencatalog("relation", 2);
		clearkeys(&Reldes);
		setkey(&Reldes, &rkey, pent.prorelid, RELID);
		setkey(&Reldes, &rkey, pent.prorelown, RELOWNER);

		/* fetch the tuple if possible */
		if (i = getequal(&Reldes, &rkey, &rel, &rtid))
		{
			/*
			** Oops.  A tuple doesn't exits in 'relation'
			**
			** Maybe it's just a fatal error.
			*/
			if (i < 0)
				syserr("CHECKPROTECT: getequal=%d", i);
			
			/* not a fatal error, what to do? */
			printf("\tNo relation for 'protect' entry:\n\t");
			printup(&Prodes, &pent);
			if (ask("\tRemove 'protect' entry? "))
			{
				if (i = delete(&Prodes, &ptid))
					syserr("CHECKPROTECT: delete=%d", i);
				continue;	/* go on to next tuple */
			}
		}
		else
		{
			/* 'relation' entry exists, check for the tree entry */
			if (pent.protree >= 0)
			{
				if (!havetree(pent.prorelid, pent.prorelown, mdPROT))
				{
					/* no tuples in 'tree' */
					printf("\tNo tree for:\n\t");
					printup(&Prodes, &pent);
					if (ask("\tDelete entry and fix relation status bits? "))
					{
						if (i = delete(&Prodes, &pent))
							syserr("CHECKPROTECT: delete=%d", i);
						rel.relstat &= ~S_PROTUPS;
						if (i = replace(&Reldes, &rtid, &rel, FALSE))
							syserr("CHECKPROTECT: replace=%d", i);
						continue;	/* go on to next tuple */
					}
				}
			}
			if ((rel.relstat & S_PROTUPS) != S_PROTUPS)
			{
				/* bits not set correctly */
				printf("\tIncorrect relation status bits for:\n\t");
				printup(&Reldes, &rel);
				if (ask("\tAdjust? "))
				{
					rel.relstat |= S_PROTUPS;
					if (i = replace(&Reldes, &rtid, &rel, FALSE))
						syserr("CHECKPROTECT: replace=%d", i);
					continue;	/* go on to next tuple */
				}
			}
		}
	}
}
/*
**  CHECKINTEG
*/

checkinteg()
{
	register int		i;
	extern DESC		Reldes, Intdes;
	struct integrity	inkey, inent;
	TID			intid, inhitid;
	struct relation		rkey, rel;
	TID			rtid;

	/* for each entry in 'integrities' */
	opencatalog("integrities", 2);
	clearkeys(&Intdes);
	if (i = find(&Intdes, NOKEY, &intid, &inhitid))
		syserr("CHECKINTEG: find=%d", i);
	
	for (;;)
	{
		i = get(&Intdes, &intid, &inhitid, &inent, TRUE);
		if (i > 0)
			break;	/* finished */
		if (i < 0)
			syserr("CHECKINTEG: get=%d", i);

		/* verify that a tuple exists in 'relation' */
		opencatalog("relation", 2);
		clearkeys(&Reldes);
		setkey(&Reldes, &rkey, inent.intrelid, RELID);
		setkey(&Reldes, &rkey, inent.intrelowner, RELOWNER);

		/* fetch the tuple if possible */
		if (i = getequal(&Reldes, &rkey, &rel, &rtid))
		{
			/*
			** Oops.  A tuple doesn't exits in 'relation'
			**
			** Maybe it's just a fatal error.
			*/
			if (i < 0)
				syserr("CHECKINTEG: getequal=%d", i);
			
			/* not a fatal error, what to do? */
			printf("\tNo relation for 'integrities' entry:\n\t");
			printup(&Intdes, &inent);
			if (ask("\tRemove 'integrities' entry? "))
			{
				if (i = delete(&Intdes, &intid))
					syserr("CHECKINTEG: delete=%d", i);
				continue;	/* go on to next tuple */
			}
		}
		else
		{
			/* 'relation' entry exists, check for the tree entry */
			if (inent.inttree >= 0)
			{
				if (!havetree(inent.intrelid, inent.intrelowner, mdINTEG))
				{
					/* no tuples in 'tree' */
					printf("\tNo tree for:\n\t");
					printup(&Intdes, &inent);
					if (ask("\tDelete entry and fix relation status bits? "))
					{
						if (i = delete(&Intdes, &inent))
							syserr("CHECKINTEG: delete=%d", i);
						rel.relstat &= ~S_INTEG;
						if (i = replace(&Reldes, &rtid, &rel, FALSE))
							syserr("CHECKINTEG: replace=%d", i);
						continue;	/* go on to next tuple */
					}
				}
			}
			if ((rel.relstat & S_INTEG) != S_INTEG)
			{
				/* bits not set correctly */
				printf("\tIncorrect relation status bits for:\n\t");
				printup(&Reldes, &rel);
				if (ask("\tAdjust? "))
				{
					rel.relstat |= S_INTEG;
					if (i = replace(&Reldes, &rtid, &rel, FALSE))
						syserr("CHECKINTEG: replace=%d", i);
					continue;	/* go on to next tuple */
				}
			}
		}
	}
}
