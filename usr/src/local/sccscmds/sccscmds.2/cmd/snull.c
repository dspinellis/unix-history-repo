# include	"../hdr/defines.h"
# include	"../hdr/had.h"

SCCSID(@(#)snull.c	4.1);
USXALLOC();

int	Debug	0;
struct packet gpkt;
struct sid sid;
int	num_files;
char	had[26];
FILE	*Xiop;
int	Xcreate;
struct deltalist {
	struct deltalist *ds_olderdel;
	struct deltalist *ds_youngerdel;
	struct sid ds_sid;
	int ds_origser;
	int ds_ser;
	int ds_pred;
	long ds_datetime;
	char ds_pgmr[8];
	char ds_type;
	struct stats ds_stats;
	int ds_insnull;
};
struct deltalist *Dhead;
struct deltalist *Dtail;
char line[512];
int *New_ser_ptr;
int Max_old_ser;

main(argc,argv)
int argc;
register char *argv[];
{
	register int i;
	register char *p;
	char c;
	extern snull();
	extern int Fcnt;

	/*
	Flags for 'fatal'.
	*/
	Fflags = FTLEXIT | FTLMSG | FTLCLN;

	/*
	Process arguments.
	*/
	for (i = 1; i < argc; i++)
		if (argv[i][0] == '-' && (c = argv[i][1])) {
			p = &argv[i][2];
			switch (c) {
			default:
				fatal("unknown key letter (cm1)");
			}
			if (had[c - 'a']++)
				fatal("key letter twice (cm2)");
			argv[i] = 0;
		}
		else num_files++;

	if(num_files == 0)
		fatal("missing file arg (cm3)");

	setsig();
	/*
	Reset flags for 'fatal' so that it will return to 'main'
	rather than exiting.
	*/
	Fflags =& ~FTLEXIT;
	Fflags =| FTLJMP;

	/*
	Invoke 'snull' for each file argument. 
	*/
	for (i = 1; i < argc; i++)
		if (p = argv[i])
			do_file(p,snull);

	exit(Fcnt ? 1 : 0);
}


snull(file)
{
	register char *p;
	register int ser;
	extern char had_dir, had_standinp;
	extern char *Sflags[];
	struct stats stats;
	int newser;

	/*
	Set up to return to caller ('main') from 'fatal'.
	*/
	if (setjmp(Fjmp))
		return;

	sinit(&gpkt,file,1);	/* init packet and open file */

	if (exists(auxf(gpkt.p_file,'p')))
		fatal("p-file exists (sn3)");

	if (lockit(auxf(gpkt.p_file,'z'),2,getpid()))
		fatal("cannot create lock file (cm4)");

	/*
	Indicate that file is to be re-opened (from beginning)
	after it reaches EOF.
	The file is read once to get the delta table
	(without copying to x-file) and then again to make
	required modifications to it (using x-file).
	*/
	gpkt.p_reopen = 1;

	dodeltbl(&gpkt);	/* get delta table */
	flushto(&gpkt,EUSERNAM,1);
	doflags(&gpkt);		/* get flags */

	/*
	Indicate to 'getline' that EOF is allowable.
	*/
	gpkt.p_chkeof = 1;

	/*
	Flush through rest of file.
	This checks for corruptions.
	*/
	while (getline(&gpkt))
		;

	if (num_files > 1 || had_dir || had_standinp)
		printf("\n%s:\n",gpkt.p_file);

	/*
	Here, file has already been re-opened (by 'getline').
	Indicate that x-file is to be used.
	*/
	gpkt.p_upd = 1;

	gpkt.p_wrttn = 1;
	getline(&gpkt);		/* skip over old */
	gpkt.p_wrttn = 1;	/* header record */

	/*
	Write new header.
	*/
	putline(&gpkt,sprintf(line,"%c%c00000\n",CTLCHAR,HEAD));
	mkdelt();		/* insert 'null' deltas */
	wrtdeltbl(&gpkt);	/* write out new delta table */

	flushto(&gpkt,EUSERNAM,0);
	/*
	If file does not have the 'n' flag, put one in.
	*/
	if (!Sflags[NULLFLAG - 'a'])
		putline(&gpkt,sprintf(line,"%c%c %c\n",CTLCHAR,
						FLAG,NULLFLAG));

	flushto(&gpkt,EUSERTXT,0);

	/*
	Process body, changing control-line serial numbers
	appropriately.
	*/
	fixbody(&gpkt);

	flushline(&gpkt,0);	/* flush buffer, fix header, and close */
	rename(auxf(gpkt.p_file,'x'),gpkt.p_file);
	clean_up(0);
}


dodeltbl(pkt)
register struct packet *pkt;
{
	struct deltab dt;
	struct stats stats;
	struct deltalist *newp;
	int n;

	Dhead = Dtail = newp = 0;

	/*
	Read entire delta table.
	*/
	while (getstats(pkt,&stats)) {
		if (getadel(pkt,&dt) != BDELTAB)
			fmterr(pkt);
		newp = alloc(n = sizeof(*Dhead));
		zero(newp,n);
		if (!Dhead) {
			Dhead = newp;
			New_ser_ptr = alloc(n = 2 * (dt.d_serial + 1));
			zero(New_ser_ptr,n);
			Max_old_ser = dt.d_serial;
		}
		else {
			Dtail->ds_olderdel = newp;
			newp->ds_youngerdel = Dtail;
		}
		newp->ds_sid.s_rel = dt.d_sid.s_rel;
		newp->ds_sid.s_lev = dt.d_sid.s_lev;
		newp->ds_sid.s_br = dt.d_sid.s_br;
		newp->ds_sid.s_seq = dt.d_sid.s_seq;
		newp->ds_origser = dt.d_serial;
		newp->ds_ser = dt.d_serial;
		newp->ds_pred = dt.d_pred;
		newp->ds_datetime = dt.d_datetime;
		move(&dt.d_pgmr,newp->ds_pgmr,sizeof(dt.d_pgmr));
		newp->ds_type = dt.d_type;
		newp->ds_stats.s_ins = stats.s_ins;
		newp->ds_stats.s_del = stats.s_del;
		newp->ds_stats.s_unc = stats.s_unc;
		Dtail = newp;

		/*
		Skip over rest of delta entry. 
		*/
		while ((n = getline(pkt)) != NULL)
			if (pkt->p_line[0] != CTLCHAR)
				break;
			else {
				switch (pkt->p_line[1]) {
				case EDELTAB:
					break;
				case INCLUDE:
				case EXCLUDE:
				case IGNORE:
				case MRNUM:
				case COMMENTS:
					continue;
				default:
					fmterr(pkt);
				}
				break;
			}
		if (n == NULL || pkt->p_line[0] != CTLCHAR)
			fmterr(pkt);
	}
}


getadel(pkt,dt)
register struct packet *pkt;
register struct deltab *dt;
{
	if (getline(pkt) == NULL)
		fmterr(pkt);
	return(del_ab(pkt->p_line,dt,pkt));
}


getstats(pkt,statp)
register struct packet *pkt;
register struct stats *statp;
{
	register char *p;

	p = pkt->p_line;
	if (getline(pkt) == NULL || *p++ != CTLCHAR || *p++ != STATS)
		return(0);
	NONBLANK(p);
	p = satoi(p,&statp->s_ins);
	p = satoi(++p,&statp->s_del);
	satoi(++p,&statp->s_unc);
	return(1);
}


mkdelt()
{
	struct deltalist *ptr;
	struct deltalist *nulldel;
	struct deltalist *oldp;
	struct deltalist *ptrtemp;
	int n;
	int currel;
	int reldiff, numnull;
	int serhold;

	/*
	Set current release to that of oldest (first) delta.
	*/
	currel = Dtail->ds_sid.s_rel;

	/*
	The following loop processes each delta, starting with the
	oldest one in the file (the last one read).
	*/
	ptr = Dtail;
	while (ptr) {
		reldiff = ptr->ds_sid.s_rel - currel;

		/*
		Skip removed deltas, branch deltas, or any delta whose
		release number is the same as the current release number.
		*/
		if (ptr->ds_type == 'R' || ptr->ds_sid.s_br ||
				 ptr->ds_sid.s_seq || reldiff == 0) {
			ptr = ptr->ds_youngerdel;
			continue;
		}

		/*
		Check if delta is the next trunk delta in sequence, and if so
		bump up current release number and continue.
		*/
		if (reldiff == 1) {
			currel++;
			ptr = ptr->ds_youngerdel;
			continue;
		}

		/*
		Here, a trunk delta has been found, and its release
		number is greater (by at least 2) than the current
		release number.
		This requires insertion of 'null' deltas.
		First, check that this trunk delta's release
		number is greater than currel.
		(This catches deltas whose SIDs have been changed
		by the user to make them look like trunk deltas.)
		*/
		if (reldiff < 0)
			fatal("file has invalid trunk delta (sn1)");

		currel =+ reldiff;	/* update currel */

		/*
		Find pointer to ancestor delta.
		*/
		oldp = ser_to_ptr(ptr->ds_pred);

		/*
		Retain serial number for later use in fixing
		other deltas' serial numbers.
		*/
		serhold = ptr->ds_ser;

		ptrtemp = ptr;
		numnull = reldiff;	/* number of null deltas needed */
		while (--numnull) {	/* insert null deltas */
			nulldel = alloc(n = sizeof(*Dhead));
			zero(nulldel,n);
			nulldel->ds_youngerdel = ptrtemp;
			nulldel->ds_olderdel = ptrtemp->ds_olderdel;
			ptrtemp->ds_olderdel = nulldel;
			(nulldel->ds_olderdel)->ds_youngerdel = nulldel;
			nulldel->ds_sid.s_rel = ptrtemp->ds_sid.s_rel - 1;
			nulldel->ds_sid.s_lev = 1;
			nulldel->ds_sid.s_br = 0;
			nulldel->ds_sid.s_seq = 0;
			nulldel->ds_ser = serhold + numnull - 1;
			if (numnull != 1)
				nulldel->ds_pred = nulldel->ds_ser - 1;
			else
				nulldel->ds_pred = oldp->ds_ser;
			nulldel->ds_datetime = ptr->ds_datetime;
			substr(logname(),nulldel->ds_pgmr,0,7);
			nulldel->ds_type = 'D';
			nulldel->ds_stats.s_ins = 0;
			nulldel->ds_stats.s_del = 0;
			nulldel->ds_stats.s_unc = oldp->ds_stats.s_unc +
						oldp->ds_stats.s_ins;
			nulldel->ds_insnull = 1;     /* null delta indicator */
			ptrtemp = nulldel;
		}

		/*
		Fix up sequence and predecessor numbers of those deltas
		which are younger than the ones just processed.
		*/
		ptrtemp = ptr;
		reldiff--;
		while (ptrtemp) {
			if (ptrtemp->ds_ser >= serhold)
				ptrtemp->ds_ser =+ reldiff;
			if (ptrtemp->ds_pred >= serhold)
				ptrtemp->ds_pred =+ reldiff;

			ptrtemp = ptrtemp->ds_youngerdel;
		}

		/*
		Fix predecessor of current delta.
		*/
		ptr->ds_pred = serhold + reldiff - 1;

		/*
		Point to next (non-null) delta.
		*/
		ptr = ptr->ds_youngerdel;
	}

	/*
	Create array of original values of serial numbers of
	the original deltas.
	*/
	ptr = Dtail;
	while (ptr) {
		if (ptr->ds_insnull != 1)
			New_ser_ptr[ptr->ds_origser] = ptr->ds_ser;
		ptr = ptr->ds_youngerdel;
	}
}


ser_to_ptr(ser)
int ser;
{
	struct deltalist *ptr;

	ptr = Dtail;
	while (ptr) {
		if (ptr->ds_ser == ser)
			return(ptr);
		ptr = ptr->ds_youngerdel;
	}
	fatal("internal error -- ser_to_ptr (sn2)");
}


wrtdeltbl(pkt)
register struct packet *pkt;
{
	struct deltalist *ptr;
	char *p;
	int ser;

	/*
	The following loop writes out the new delta table.
	*/
	ptr = Dhead;
	while (ptr) {
		if (ptr->ds_insnull) {		/* 'null' delta */
			/*
			Write out statistics line.
			*/
			putline(pkt,sprintf(line,"%c%c %05u/%05u/%05u\n",
				CTLCHAR,STATS,ptr->ds_stats.s_ins,
						ptr->ds_stats.s_del,
						ptr->ds_stats.s_unc));

			/*
			Write 'delta' line, taken from
			in-core list.
			*/
			putdel(pkt,ptr);

			putline(pkt,sprintf(line,"%c%c %s\n",CTLCHAR,COMMENTS,
					"INSERTED BY SNULL"));
			putline(pkt,sprintf(line,CTLSTR,CTLCHAR,EDELTAB));
		}
		else {
			getline(pkt);		/* statistics line */
			getline(pkt);		/* 'delta' line */

			/*
			Indicate not to output previously read line.
			*/
			pkt->p_wrttn = 1;

			/*
			Write 'delta' line from in-core list.
			*/
			putdel(pkt,ptr);

			/*
			Process rest of entry, changeing serial
			numbers of deltas included, excluded,
			or ignored.
			*/
			while (getline(pkt))
				if (pkt->p_line[0] != CTLCHAR)
					break;
				else {
					switch (*(p = &pkt->p_line[1])) {
					case EDELTAB:
						putline(pkt,0);
						break;
					case INCLUDE:
					case EXCLUDE:
					case IGNORE:
						pkt->p_wrttn = 1;
						putline(pkt,sprintf(line,
							"%c%c",CTLCHAR,*p++));
						NONBLANK(p);
						while (numeric(*p)) {
							p = satoi(p,&ser);

							if (!(ser > 0 &&
							ser <= Max_old_ser))
								fmterr(pkt);

							putline(pkt,sprintf(
							line," %u",
							New_ser_ptr[ser]));

							NONBLANK(p);
						}
						putline(pkt,"\n");
						continue;
					default:
						putline(pkt,0);
						continue;
					}
					break;
				}
		}

		/*
		Point to next delta to be output.
		*/
		ptr = ptr->ds_olderdel;
	}
}


putdel(pkt,ptr)
struct packet *pkt;
struct deltalist *ptr;
{
	struct deltab dt;

	move(&ptr->ds_sid,&dt.d_sid,sizeof(dt.d_sid));
	dt.d_serial = ptr->ds_ser;
	dt.d_pred = ptr->ds_pred;
	dt.d_datetime = ptr->ds_datetime;
	move(ptr->ds_pgmr,&dt.d_pgmr,sizeof(dt.d_pgmr));
	dt.d_type = ptr->ds_type;

	del_ba(&dt,line);
	putline(pkt,line);
}


fixbody(pkt)
register struct packet *pkt;
{
	int ser;
	char *p, type;

	while (getline(pkt)) {
		p = pkt->p_line;

		if (*p++ == CTLCHAR) {
			if (!((type = *p++) == INS || type == DEL ||
							type == END))
				fmterr(pkt);
			NONBLANK(p);
			satoi(p,&ser);
			if (!(ser > 0 && ser <= Max_old_ser))
				fmterr(pkt);

			/*
			Indicate not to output line just read.
			*/
			pkt->p_wrttn = 1;

			/*
			Output new value of sequence number.
			*/
			putline(pkt,sprintf(line,"%c%c %u\n",CTLCHAR,type,
						New_ser_ptr[ser]));
		}
	}
}


clean_up(n)
{
	if (gpkt.p_file[0])
		unlockit(auxf(gpkt.p_file,'z'),getpid());
	if (gpkt.p_iop)
		fclose(gpkt.p_iop);
	xrm(&gpkt);
	xfreeall();
}
