# include	<stdio.h>
# include	<ingres.h>
# include	<pv.h>
# include	<aux.h>
# include	<access.h>
# include	<batch.h>
# include	<lock.h>
# include	<opsys.h>
# include 	<func.h>
# include	<version.h>
# include	<sccs.h>

SCCSID(@(#)modify.c	7.1	2/5/81)

extern	short	tTdbu[];
extern	int	modify();
extern 	int	null_fn();

struct fn_def ModifyFn =
{
	"MODIFY",
	modify,
	null_fn,
	null_fn,
	NULL,
	0,
	tTdbu,
	100,
	'Z',
	0
};

/*
**  MODIFY -- converts any relation to the specified
**		storage structure
**
**	arguments:
**	0 - relation name
**	1 - storage structure ("heap", "cheap", "hash", "chash",
**		"isam", "cisam")
**	2 - "name" for attribute names, or "num" for numbers
**	3 - key1
**	4 - key2
**	    .
**	    .
**	i - null
**	i+1 - option name (e.g., "fillfactor")
**	i+2 - option value
**	    .
**	    .
**
**	If all the options default, parameter i -> pc are omitted.
**	If no keys are provided, parameter 2 is omitted.
*/

int		F_fac, Mn_pages, Mx_pages;

struct modtab
{
	char	*type;
	char	newrelspec;
	char	yeskeys;
	char	sortit;
	char	yes_seq;
	int	f_fac;
	int	mn_pages;
	int	mx_pages;
};


struct modtab	Modtab[] =
{
	/* type		spec	keys	sort	seq	ffac	min	max */

	"heap",		M_HEAP,	FALSE,	FALSE,	FALSE,	0,	0,	0,
	"cheap",	-M_HEAP,FALSE,	FALSE,	FALSE,	0,	0,	0,
	"hash",		M_HASH,	TRUE,	TRUE,	FALSE,	50,	10,	-1,
	"chash",	-M_HASH,TRUE,	TRUE,	FALSE,	75,	1,	-1,
	"isam",		M_ISAM,	TRUE,	TRUE,	FALSE,	80,	0,	0,
	"cisam",	-M_ISAM,TRUE,	TRUE,	FALSE,	100,	0,	0,
	"heapsort",	M_HEAP,	TRUE,	TRUE,	TRUE,	0,	0,	0,
	"cheapsort",	-M_HEAP,TRUE,	TRUE,	TRUE,	0,	0,	0,
	"truncated",	M_TRUNC,FALSE,	FALSE,	FALSE,	0,	0,	0,
	0
};

struct mod_info
{
	char	outfile[MAXNAME + 4];	/* result file filled by ksort */
	char	formfile[MAXNAME + 4];	/* file with descriptor for ksort */
	char	infile[MAXNAME + 4];	/* input file for ksort (relation itself */
	char	reltemp[MAXNAME + 4];	/* file holding new relation */
	char	spfile[MAXNAME + 4], spflag;	/* isam spool file for overflow */
};

struct mod_info	Mod_info;





modify(pc, pv)
int	pc;
PARM	*pv;
{
	register int		i;
	register char		*rname;
	register struct modtab	*mp;
	int			sorted;
	DESC			dold, dnew;
	long			temptid;
	extern int		Noupdt;

	
#	ifdef xZTR1
	if (tTf(34, -1))
	{
		printf("enter modify\n");
		prvect(pc, pv);
	}
#	endif

	pv[pc].pv_val.pv_str = NULL;

	/* check for nice parameters */
	if (pc < 2)
		syserr("MODIFY: pc %d", pc);

	/* save relation name for error messages */
	rname = (pv++)->pv_val.pv_str;	/* *pv now pointes to storage spec */

	/* check for good relation */
	i = openr(&dold, 0, rname);
	if (i == AMOPNVIEW_ERR)
		return (error(5519, rname, 0));
	if (i > 0)
		/* reln does not exist */
		return (error(5500, rname, 0));	
	else if (i < 0)
		syserr("MODIFY: openr (%.14s) %d", rname, i);
	/* can only modify a relation you own and isn't a sys rel */
	
	if (!bequal(Usercode, dold.reldum.relowner, 2))
	{
		i = 5501;
	}
	if ((dold.reldum.relstat & S_CATALOG) && Noupdt)
	{
		i = 5504;
	}
	if (i)
	{
		closer(&dold);
		return (error(i, rname, 0));
	}

	/*
	** Form descriptor for new relation. Here we need to
	** separate the pages from the old and new relations.
	** Since pages are identified by the TID of the relation
	** relation tuple, both old and new have the same identifiers.
	** To avoid this problem, a special TID is hand crafted for
	** the new relation.
	*/
	bmove(&dold, &dnew, sizeof dnew);
	dnew.reltid.s_tupid.line_id = (char) -2;	/* choose impossible reltid */

	/* In case of an interrupt from a previous modify,
	** there might be pages around. Get rid of them.
	*/
	cleanrel(&dnew);

	ingresname(dold.reldum.relid, dold.reldum.relowner, Mod_info.infile);

	/* scan for entry in relspec table */
	for (mp = Modtab; mp->type; mp++)
		if (sequal(mp->type, pv->pv_val.pv_str))
			break;

	/* if not found, error */
	if (!mp->type)
	{
		closer(&dold);
		return (error(5510, rname, pv->pv_val.pv_str, 0));	/* bad relspec */
	}
	dnew.reldum.relspec = mp->newrelspec;
	if (dnew.reldum.relspec == M_TRUNC)
		dnew.reldum.relspec = M_HEAP;

	pv++;	/* now points to first parameter */

	F_fac = mp->f_fac;
	Mn_pages = mp->mn_pages;
	Mx_pages = mp->mx_pages;

	/* get the key domains information */
	if (i = getkeys(&pv, rname, &dnew, mp))
	{
		closer(&dold);
		return (i);	/* user error */
	}

	/* get fillfactor and other options if any */
	if (i = getfill(pv, rname, mp))
	{
		closer(&dold);
		return (i);	/* user error */
	}


	/* lock the relation relation */
	if (Lockrel)
	{
		get_p_tid(&dold, &temptid);
		setrll(A_SLP, temptid, M_EXCL);
	}

	/* compute new relation parameters & build descriptor */
	make_newrel(&dnew);

	if (sorted = (mp->sortit && (dold.reldum.reltups != 0)))
		sortrel(&dold, &dnew);

	/* physically create the new relation */
	if (formatpg(&dnew, dnew.reldum.relprim) != 0)
		syserr("modify: formatpg");

	/* clear relgiven field; if heap remove any keys */
	clearkeys(&dnew);
	if (abs(dnew.reldum.relspec) == M_HEAP)
		for (i = 1; i <= dnew.reldum.relatts; i++)
			dnew.relxtra[i] = 0;

	if (mp->newrelspec != M_TRUNC)
		fill_rel(&dold, &dnew, sorted);

	closer(&dold);	/* error return is impossible */
	if (abs(dnew.reldum.relspec) == M_ISAM)
	{
		if (i = bldindex(&dnew))
			syserr("bldindex: %.14s %d", dnew.reldum.relid, i);
		unspool(&dnew);
	}

	/*
	** New relation is now complete. The system relations need to
	** be updated. First destroy all buffers with pages from the
	** new relation.
	*/
	if (i = cleanrel(&dnew))
		syserr("modify:clean new %d,%.14s", i, dnew.reldum.relid);

	fill_batch(&dold, &dnew);

	/*
	** Close the file for the new relation. This must be
	** done after the fill_batch in case we are modifing
	** the attribute relation.
	*/
	close(dnew.relfp);
	dnew.relopn = 0;
	ruboff("modify");
	modupdate();
	rubon();

	if (Lockrel)
		unlrl(temptid);

#	ifdef xZTM
	if (tTf(35, 1))
		timtrace(16, 0);
#	endif
	return (0);
}


getkeys(ppv, relname, d, mp)
PARM		**ppv;
char		*relname;
register DESC	*d;
struct modtab	*mp;
{
	register PARM		*pv;
	register char		*cp;
	int			namemode, sort_only, as_ds;
	int			i, keyno, keywid;
	struct attribute	attkey, atttup;
	TID			tid;
	extern DESC		Attdes;

	pv = *ppv;	/* copy list of params */

	/* zero key info */
	for (i = 0; i <= d->reldum.relatts; i++)
		d->relxtra[i] = d->relgiven[i] = 0;

	/* determine whether there are any keys at all */
	keyno = 0;
	keywid = 0;
	sort_only = FALSE;
	cp = pv->pv_val.pv_str;

	if (cp == NULL || *cp == NULL)
	{
		/* no key information. default as needed */
		if (mp->yeskeys)
		{
			cp = "\1";	/* default to first key */
			namemode = FALSE;
		}
		else
			pv++;	/* point one to far */
	}
	else
	{
		/* check for name mode */
		if (namemode = sequal(cp, "name"))
		{

			/* check attribute names, and convert them to numbers */
			opencatalog("attribute", 0);
			setkey(&Attdes, &attkey, Mod_info.infile, ATTRELID);
			setkey(&Attdes, &attkey, Usercode, ATTOWNER);
		}
		pv++;	/* inc to next key */
		cp = (pv++)->pv_val.pv_str;
	}


	/* scan for attribute names */
	for (; cp != NULL; cp = (pv++)->pv_val.pv_str)
	{
		/* check for separator between keys & options */
		if (*cp == NULL)
		{
			pv++;	/* point two past NULL */
			break;
		}

		if (namemode)
		{
			/* check for "sort only" attribute */
			if (*cp == '#')
			{
				cp++;	/* inc to start of name */
				sort_only = TRUE;
			}

			/* check for ascending/descending modifier */
			if ((as_ds = modseqkey(cp, relname, mp->yes_seq)) > 0)
				return (as_ds);	/* error */

			setkey(&Attdes, &attkey, cp, ATTNAME);
			i = getequal(&Attdes, &attkey, &atttup, &tid);
			if (i < 0)
				syserr("MODIFY: geteq(att) %d", i);
			if (i > 0)
			{
				return (error(5511, relname, cp, 0));	/* bad att name */
			}
			i = atttup.attid;
		}
		else
		{
			i = *cp;
			as_ds = 0;
		}

		/* add new key to descriptor */
		keyno++;
		if (!sort_only)
		{
			d->relxtra[i] = keyno;
			keywid += (d->relfrml[i] & I1MASK);
		}
		if (d->relgiven[i])
			return (error(5507, relname, cp, 0));	/* duplicate attribute */
		d->relgiven[i] = as_ds == 0 ? keyno : -keyno;
	}
	pv--;	/* back up one to point to "-1" terminator */


	if (abs(d->reldum.relspec) == M_ISAM && keywid > (MAXTUP / 2 - 4))
	{
		return (error(5508, relname, iocv(keywid), 0));
	}

	/* if a heap, there can be no keys */
	if (!mp->yeskeys && keyno != 0)
	{
		return (error(5502, relname, mp->type, 0));	/* no keys allowed on heap */
	}

	/* fill out default sort on remainder of keys */
	if (mp->yeskeys)
		for (i = 1; i <= d->reldum.relatts; i++)
			if (d->relgiven[i] == 0)
				d->relgiven[i] = ++keyno;
	*ppv = pv;
	return (0);
}


modseqkey(domain, relname, seq_ok)
char	*domain;
char	*relname;
int	seq_ok;
{
	register char	*cp, c;
	register int	ret;

	ret = 0;

	for (cp = domain; c = *cp++; )
		if (c == ':')
			break;

	if (c != '\0')
	{
		/* replace ":" with null */
		*(cp - 1) = '\0';

		/* verify sequence is valid */
		if (!seq_ok)
			ret = error(5520, relname, cp, domain, 0);
		else if (sequal("descending", cp) || sequal("d", cp))
			ret = -1;
		else if (!(sequal("ascending", cp) || sequal("a", cp)))
			ret = error(5518, relname, cp, domain, 0);
	}

	return (ret);
}
/*
**	GETFILL -- Get fill factor and minimum pages parameters
**		from argument list, convert them from ascii to integer
**		and store them in global variables.  If the global
**		variable for the corresponding parameter is zero,
**		it means that that parameter is not allowed and an
**		error is generated.
*/

/*ARGSUSED*/
getfill(pv, rel, mp)
register PARM	*pv;
char		*rel;
struct modtab	*mp;
{
	register char	*p1;
	register int	err;
	char		*p2;
	int		fill_flag, min_flag, max_flag;

	err = 0;
	fill_flag = min_flag = max_flag = FALSE;

	while ((p1 = (pv++)->pv_val.pv_str) != NULL)
	{
		p2 = (pv++)->pv_val.pv_str;
		if (sequal(p1, "fillfactor"))
		{
			if (F_fac == 0 || fill_flag)
			{
				err = 5512;
				break;
			}
			p1 = p2;
			atoi(p1, &F_fac);
			if (F_fac > 100 || F_fac < 1)
			{
				err = 5513;
				break;
			}
			fill_flag = TRUE;
			continue;
		}
		if (sequal(p1, "minpages"))
		{
			if (Mn_pages == 0 || min_flag)
			{
				err = 5512;
				break;
			}
			p1 = p2;
			atoi(p1, &Mn_pages);
			if (Mn_pages < 1)
			{
				err = 5514;
				break;
			}
			if (max_flag && (Mn_pages > Mx_pages))
			{
				err = 5517;
				break;
			}
			min_flag = TRUE;
			continue;
		}
		if (sequal(p1, "maxpages"))
		{
			if (Mx_pages == 0 || max_flag)
			{
				err = 5512;
				break;
			}
			p1 = p2;
			atoi(p1, &Mx_pages);
			if (Mx_pages < 1)
			{
				err = 5516;
				break;
			}
			if (min_flag && (Mn_pages > Mx_pages))
			{
				err = 5517;
				break;
			}
			max_flag = TRUE;
			continue;
		}
		err = 5515;
		break;
	}
	if (err)
		return (error(err, rel, p1, 0));
	return (0);
}
/*
**  MAKE_NEWREL -- Create a file for the modified relation
**	and build one or more primary pages for the
**	relation based on its storage structure and the
**	number of tuples it must hold.
*/

make_newrel(desc)
register DESC	*desc;
{
	register int	tups_p_page;

	concat(MODTEMP, Fileset, Mod_info.reltemp);
	close(creat(Mod_info.reltemp, FILEMODE));
	if ((desc->relfp = open(Mod_info.reltemp, 2)) < 0)
		syserr("MAKE_NEWREL: open %.14s %d", Mod_info.reltemp, desc->relfp);
	desc->relopn = (desc->relfp + 1) * -5;
	desc->reldum.relprim = 1;
	if (abs(desc->reldum.relspec) == M_HASH && F_fac > 0 && Mn_pages > 0)
	{
		/*
		** Determine the number of primary pages. The following
		** first determines the number of tuples/page which the
		** relation should have in order to get the requested
		** fillfactor. Then that number is divided into the
		** number of tuples to get the number of primary pages.
		** To avoid round off, it must guaranteed that the
		** number of tuples per page must be at least 1.
		**
		** primary_pages = #tuples / (#tuples/page * fillfactor)
		*/
		tups_p_page = (((MAXTUP+2) / (desc->reldum.relwid+2)) * F_fac) / 100;
		if (tups_p_page == 0)
			tups_p_page = 1;
		 /* we add one to simulate a ceiling function */
		desc->reldum.relprim = desc->reldum.reltups / tups_p_page + 1;
		if (desc->reldum.relprim < Mn_pages)
			desc->reldum.relprim = Mn_pages;
		if (Mx_pages > 0 && desc->reldum.relprim > Mx_pages)
			desc->reldum.relprim = Mx_pages;
#		ifdef xZTR1
		if (tTf(36, 0))
			printf("using %ld prim pages\n", desc->reldum.relprim);
#		endif
	}
	desc->reldum.reltups = 0;
	return (0);
}
/*
**	SORTREL - Call KSORT to sort the given relation.  SORTREL
**		sets up the descriptor struct specifying the sort
**		keys and tells KSORT whether or not the hash key should
**		be included as a sort key.
*/

sortrel(odesc, desc)
DESC		*odesc;
register DESC	*desc;
{
	extern char	*Pathname;
	register int	fp, i;
	char		savespec;
	char		buf[50];

	concat(ISAM_SORTED, Fileset, Mod_info.outfile);
	if (close(creat(Mod_info.outfile, FILEMODE)))
		syserr("SORTREL: creat %.14s", Mod_info.outfile);
	concat(ISAM_DESC, Fileset, Mod_info.formfile);
	if ((fp = creat(Mod_info.formfile, FILEMODE)) < 0)
		syserr("SORTREL: creat %.14s %d", Mod_info.formfile, fp);
	if (abs(desc->reldum.relspec) == M_HASH)
	{
		/* sort on hash bucket first */
		desc->relgiven[0] = 1;
		for (i = 1; i <= desc->reldum.relatts; i++)
			desc->relgiven[i]++;
	}
	savespec = desc->reldum.relspec;
	desc->reldum.relspec = odesc->reldum.relspec;

# ifdef xZTR2
	if (tTf(36, 4))
	{
		printf("sortrel: ");
		printdesc(desc);
	}
# endif

	if (write(fp, desc, sizeof *desc) != sizeof *desc)
		syserr("SORTREL: desc write err");
	close(fp);
	desc->reldum.relspec = savespec;

	i = fork();
	if (i == -1)
		syserr("SORTREL: fork");
	if (i == 0)
	{
		for (i = 3; i < NOFILE; i++)
			close(i);
# ifdef KSORTPATH
		smove(KSORTPATH, buf);
# else
		concat(Pathname, ztack("/bin/ksort", VERSION), buf);
# endif KSORTPATH
# ifdef xZTR2
		if (tTf(36, 0))
		{
			printf("Calling ksort, args:\n");
			printf("\tbuf = `%s'\n", buf);
			printf("\tFileset = `%s'\n", Fileset);
			printf("\ttTf = `%s'\n", iocv(tTf(37, -1)));
			printf("\tformfile = `%s'\n", Mod_info.formfile);
			printf("\tinfile = `%s'\n", Mod_info.infile);
			printf("\toutfile = `%s'\n", Mod_info.outfile);
		}
# endif xZTR2
		execl(buf, buf, Fileset, iocv(tTf(37, -1)),
			Mod_info.formfile, Mod_info.infile,
			Mod_info.outfile, 0);
		syserr("SORTREL: exec %s", buf);
	}

# ifdef	xZTR1
	tTfp(36, 9, "SORTREL: after execl; pid = %d\n", i);
# endif

	if (fp = fullwait(i, "modify"))	/* wait for ksort to complete */
		syserr("modify:ksort failed %d", fp);

# ifdef	xZTR1
	tTfp(36, 10, "SORTREL: after fullwait\n");
# endif

	unlink(Mod_info.formfile);
	return (0);
}
/*
**	FILL_REL -- Fill the new relation with tuples from either
**		the old relation or the output file of KSORT.
*/

fill_rel(sdesc, desc, sortit)
register DESC	*sdesc, *desc;
char			sortit;
{
	register int	i;
	char		tup_buf[MAXTUP], last_tup[MAXTUP];
	char		junk[4], newreltype, anytups, chkdups;
	int		need, j;
	long		lnum;
	TID		tid, stid, stidlim;
	FILE		*fp, *spfp;

	newreltype = abs(desc->reldum.relspec);
	if (sortit)
	{
		if ((fp = fopen(Mod_info.outfile, "r")) == NULL)
			syserr("FILL_REL: fopen %.14s", Mod_info.outfile);
	}
	else
	{
		cleanrel(sdesc);	/* make sure each page is read fresh */
		find(sdesc, NOKEY, &stid, &stidlim);
	}
	if (newreltype == M_ISAM)
	{
		lnum = 0;
		stuff_page(&tid, &lnum);
		tid.line_id = 0;
		get_page(desc, &tid);
		concat(ISAM_SPOOL, Fileset, Mod_info.spfile);
		/* assume that spool file is not needed */
		spfp = NULL;
		Mod_info.spflag = FALSE;
		if (F_fac == 0)
			F_fac = 100;
		/* setup relgiven field for kcompare later on */
		for (i = 1; i <= desc->reldum.relatts; i++)
			desc->relgiven[i] = desc->relxtra[i];
	}
	desc->reladds = 0;
	anytups = FALSE;
	chkdups = !sortit;
# ifdef xZTR2
	if (tTf(36, 3))
	{
		printf("  FILLREL: ");
		printdesc(desc);
	}
# endif
	for (;;)
	{
		if (sortit)
		{
			i = fread(tup_buf, 1, desc->reldum.relwid, fp);
			if (i == 0)
				break;
			if (i != desc->reldum.relwid)
				syserr("FILL_REL: fread A %d", i);
			if (newreltype == M_HASH)
				if (fread(junk, 1, 4, fp) != 4)
					syserr("FILL_REL: fread B");
		}
		else
		{
#			ifdef xZTR2
			if (tTf(36, 1))
			{
				printf("FILL_REL: stid ");
				dumptid(&stid);
				printf("FILL_REL: stidlim ");
				dumptid(&stidlim);
			}
#			endif
			i = get(sdesc, &stid, &stidlim, tup_buf, TRUE);
#			ifdef xZTR2
			if (tTf(36, 1))
			{
				printf("FILLREL: get %d ", i);
				printup(sdesc, tup_buf);
			}
#			endif
			if (i < 0)
				syserr("FILL_REL: get %d", i);
			if (i == 1)
				break;
		}
		if (newreltype != M_ISAM)
		{
			if ((i = insert(desc, &tid, tup_buf, chkdups)) < 0)
				syserr("FILL_REL: insert %d", i);
#			ifdef xZTR2
			if (tTf(36, 2))
			{
				printf("FILL_REL: insert ");
				printup(desc, tup_buf);
				printf("FILL_REL: insert ret %d at", i);
				dumptid(&tid);
			}
#			endif
			continue;
		}
		if (anytups)
			i = kcompare(desc, tup_buf, last_tup);
		else
		{
			anytups = TRUE;
			i = 1;
		}
		bmove(tup_buf, last_tup, desc->reldum.relwid);
		need = canonical(desc, tup_buf);
		if (i == 0 && need > space_left(Acc_head))
		{
			/* spool out this tuple. will go on overflow page later */
			if (spfp == NULL)
			{
				if ((spfp = fopen(Mod_info.spfile, "w")) == NULL)
					syserr("FILL_REL: fopen %.14s", Mod_info.spfile);
				Mod_info.spflag = TRUE;
			}
			if (fwrite(tup_buf, 1, desc->reldum.relwid, spfp) != desc->reldum.relwid)
				syserr("FILL_REL: putb spool");
			continue;
		}
		j = (100 - F_fac) * MAXTUP / 100;
		if (j < need)
			j = need;
		if (i != 0 && j > space_left(Acc_head))
		{
			if (i = add_prim(desc, &tid))
				syserr("FILL_REL: force ovflo %d", i);
		}
		tid.line_id = newlino(need);
		put_tuple(&tid, Acctuple, need);
		desc->reladds++;
	}
	if (sortit)
	{
		fclose(fp);
		unlink(Mod_info.outfile);
	}
	if (newreltype == M_ISAM)
	{
		if (i = pageflush(Acc_head))
			syserr("fill_rel:pg clean %d", i);
		if (spfp != NULL)
			fclose(spfp);
	}
	desc->reldum.reltups = desc->reladds;
	desc->reladds = 0;
	return (0);
}


bldindex(d)
register DESC	*d;
{
	register TID	*tid;
	register int	tmp;
	TID		tidx;
	struct accbuf	dirbuf;
	int		keywid, level, savespec, keyx[MAXDOM];
	int		offset, len;
	char		tuple[MAXTUP], temptup[MAXTUP], *key;
	long		pageid, start, stop, newstart, newstop;

	tid = &tidx;
	keywid = 0;
	for (tmp = 0; tmp < MAXDOM; tmp++)
		keyx[tmp] = 0;
	for (tmp = 1; tmp <= d->reldum.relatts; tmp++)
		if (d->relxtra[tmp] > 0)
		{
			keyx[d->relxtra[tmp] - 1] = tmp;
			keywid += d->relfrml[tmp] & I1MASK;
		}

	/* Determine the last page of the relation. This will
	** only work if all pages have been written out. Fill_rel
	** must guarantee that all pages have been written
	*/
	level = 0;
	last_page(d, tid, 0);
	pluck_page(tid, &stop);
	start = 0;
	dirbuf.filedesc = d->relfp;
	dirbuf.rel_tupid = d->reltid.ltid;
	savespec = d->reldum.relspec;
	for (;;)
	{
#		ifdef xZTR2
		if (tTf(38, 7))
			printf("isam: level %d\n", level);
#		endif
		dirbuf.ovflopg = start;
		dirbuf.mainpg = level;
		dirbuf.thispage = stop + 1;
		dirbuf.linetab[0] = (short) (dirbuf.firstup - (char *) &dirbuf);
		offset = dirbuf.linetab[0];
		dirbuf.bufstatus = BUF_DIRTY | BUF_DIRECT;

		dirbuf.nxtlino = 0;
		newstart = stop + 1;
		newstop = newstart;
		for (pageid = start; pageid <= stop; pageid++)
		{
#			ifdef xZTR2
			if (tTf(38, 8))
				printf("isam:get key from %ld\n", pageid);
#			endif
			stuff_page(tid, &pageid);
			tid->line_id = 0;
			if (tmp = get(d, tid, tid, tuple, FALSE))
			{
				/*
				** If the relation is empty, then page 0 will
				** return AMINVL_ERR on a get(). Form a blank tuple
				** and use it to create a one tuple directory
				*/
				if (pageid == 0 && tmp == AMINVL_ERR)
				{
					clr_tuple(d, tuple);
				}
				else
				{
					return (-2);
				}
			}

			/*
			** If this is the first level then form the tuple
			** from the mainpage of the relation. Otherwise
			** the tuple is the first tuple of a directory page
			** and it is already correctly formed.
			*/
			if (level == 0)
			{
				key = temptup;
				for (tmp = 0; keyx[tmp] != 0; tmp++)
				{
					len = d->relfrml[keyx[tmp]] & I1MASK;
					bmove(&tuple[d->reloff[keyx[tmp]]], key, len);
					key += len;
				}
				key = temptup;
			}
			else
				key = tuple;

			if (keywid > space_left(&dirbuf))
			{
				if (pageflush(&dirbuf))
					return (-3);
				dirbuf.thispage++;
				newstop = dirbuf.thispage;
				dirbuf.ovflopg = pageid;
				dirbuf.linetab[0] = (short) (dirbuf.firstup - (char *) &dirbuf);
				offset = dirbuf.linetab[0];
				dirbuf.bufstatus = BUF_DIRTY;
				dirbuf.nxtlino = 0;
			}
			/* copy key to directory page */
			bmove(key, (char *) &dirbuf + offset, keywid);

			/* update next line number */
			offset += keywid;
			dirbuf.nxtlino++;
			dirbuf.linetab[-dirbuf.nxtlino] = offset;
		}
		if (pageflush(&dirbuf))
			return (-4);
		if (newstart == newstop)
			break;
		d->reldum.relspec = abs(d->reldum.relspec);
		level++;
		start = newstart;
		stop = newstop;
	}
	d->reldum.relspec = savespec;
	d->reldum.relprim = newstart;
	return (0);
}
/*
**	UNSPOOL -- Take tuples saved in spool file and insert them
**		in new relation.  This is only for ISAM relations.
*/

unspool(desc)
register DESC	*desc;
{
	register int	i;
	TID		tid;
	char		tup_buf[MAXTUP];
	FILE		*spfp;

	if (Mod_info.spflag)
	{
		if ((spfp = fopen(Mod_info.spfile, "r")) == NULL)
			syserr("UNSPOOL: fopen spool");
		while ((i = fread(tup_buf, 1, desc->reldum.relwid, spfp)) == desc->reldum.relwid)
			if ((i = insert(desc, &tid, tup_buf, FALSE)) < 0)
				syserr("UNSPOOL: insert %.14s %d", desc->reldum.relid, i);
		if (i != 0)
			syserr("UNSPOOL: read %d", i);
		fclose(spfp);
		unlink(Mod_info.spfile);
	}
	desc->reldum.reltups += desc->reladds;
	desc->reladds = 0;
	return (0);
}
/*
**	FILL_BATCH -- Create and fill a batch file containing the
**		updates for the system catalog so that MODIFY will
**		be recoverable if the system crashes.
*/

fill_batch(odesc, desc)
DESC		*odesc;
register DESC	*desc;
{
	register DESC		*dessys;
	register int		i;
	struct relation		reltup, rkey;
	TID			tid, lotid, hitid;
	struct attribute	atttup, akey;
	int			j;
	char			prebatch[MAXNAME + 4], modbatch[MAXNAME + 4];

	if (bequal(desc->reldum.relid, "relation    ", 12))
	{
		clearkeys(desc);
		setkey(desc, &rkey, desc->reldum.relid, RELID);
		setkey(desc, &rkey, desc->reldum.relowner, RELOWNER);
		if (i = getequal(desc, &rkey, &reltup, &tid))
			syserr("FILL_BATCH: geteq rel rel %d", i);
		bmove(&tid, &desc->reltid, sizeof desc->reltid);
	}
	else
		bmove(&odesc->reltid, &desc->reltid, sizeof desc->reltid);
	resetacc(Acc_head);
	concat(MOD_PREBATCH, Fileset, prebatch);
	close(creat(prebatch, FILEMODE));
	if ((Batch_fp = open(prebatch, 2)) < 0)
		syserr("FILL_BATCH: open %.14s %d", prebatch, Batch_fp);
	smove(Fileset, Batchbuf.file_id);
	Batch_cnt = 0;
	wrbatch(desc, sizeof *desc);
	if (bequal(desc->reldum.relid, "attribute   ", 12))
		dessys = desc;
	else
		dessys = &Admin.adattd;
	clearkeys(dessys);
	setkey(dessys, &akey, desc->reldum.relid, ATTRELID);
	setkey(dessys, &akey, desc->reldum.relowner, ATTOWNER);
	if (i = find(dessys, EXACTKEY, &lotid, &hitid, &akey))
		syserr("FILL_BATCH: find %d", i);
	j = desc->reldum.relatts;
	while(!(i = get(dessys, &lotid, &hitid, &atttup, TRUE)) && j > 0)
		if (!kcompare(dessys, &akey, &atttup))
		{
			j--;
			atttup.attxtra = desc->relxtra[atttup.attid];
			wrbatch(&lotid, sizeof lotid);
			wrbatch(&atttup, sizeof atttup);
		}
	if (i < 0 || j > 0)
		syserr("FILL_BATCH: get att %d count %d", i, j);
	/* get rid of attribute pages */
	cleanrel(dessys);
	flushbatch();
	close(Batch_fp);
	concat(MODBATCH, Fileset, modbatch);
	if (link(prebatch, modbatch) == -1)
		syserr("FILL_BATCH: can't link %.14s %.14s",
			prebatch, modbatch);
	unlink(prebatch);
	return (0);

}
