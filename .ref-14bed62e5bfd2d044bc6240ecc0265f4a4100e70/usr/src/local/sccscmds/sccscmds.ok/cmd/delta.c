# include	"../hdr/defines.h"
# include	"../hdr/had.h"

SCCSID(@(#)delta.c	4.2);
USXALLOC();

char	Diffpgm[]	"/usr/local/bdiff";
FILE	*Diffin;
int	Debug	0;
struct packet gpkt;
struct sid sid;
int	num_files;
char	had[26];
char	*ilist, *elist, *glist;
char	*Comments, *Mrs;
int	Domrs;
int verbosity;
int	Did_id;
long	Szqfile;
char	Pfilename[FILESIZE];
FILE	*Xiop;
int	Xcreate;

main(argc,argv)
int argc;
register char *argv[];
{
	register int i;
	register char *p;
	char c;
	int testmore;
	extern delta();
	extern int Fcnt;

	Fflags = FTLEXIT | FTLMSG | FTLCLN;
	for(i=1; i<argc; i++)
		if(argv[i][0] == '-' && (c=argv[i][1])) {
			p = &argv[i][2];
			testmore = 0;
			switch (c) {

			case 'r':
				if (!p[0]) {
					argv[i] = 0;
					continue;
				}
				chksid(sid_ab(p,&sid),&sid);
				break;
			case 'g':
				glist = p;
				break;
			case 'y':
				Comments = p;
				break;
			case 'm':
				Mrs = p;
				break;
			case 'p':
			case 'n':
			case 's':
				testmore++;
				break;
			default:
				fatal("unknown key letter (cm1)");
			}

			if (testmore) {
				testmore = 0;
				if (*p)
					fatal(sprintf(Error,
					  "value after %c arg (cm7)",c));
			}
			if (had[c - 'a']++)
				fatal("key letter twice (cm2)");
			argv[i] = 0;
		}
		else num_files++;

	if(num_files == 0)
		fatal("missing file arg (cm3)");
	if (!HADS)
		verbosity = -1;
	setsig();
	Fflags =& ~FTLEXIT;
	Fflags =| FTLJMP;
	for (i=1; i<argc; i++)
		if (p=argv[i])
			do_file(p,delta);
	exit(Fcnt ? 1 : 0);
}


delta(file)
{
	static int first 1;
	register char *p;
	int n, linenum;
	char type;
	register int ser;
	extern char had_dir, had_standinp;
	extern char *Sflags[];
	char dfilename[FILESIZE];
	char gfilename[FILESIZE];
	char line[512];
	FILE *gin;
	struct stats stats;
	struct pfile *pp;
	int inserted, deleted, orig;
	int newser;
	int status;
	int diffloop;
	int difflim;

	if (setjmp(Fjmp))
		return;
	if (first) {
		first = 0;
		dohist(file);
	}
	sinit(&gpkt,file,1);
	if (lockit(auxf(gpkt.p_file,'z'),2,getpid()))
		fatal("cannot create lock file (cm4)");
	gpkt.p_reopen = 1;
	gpkt.p_stdout = stdout;
	copy(auxf(gpkt.p_file,'g'),gfilename);
	gin = xfopen(gfilename,0);
	pp = rdpfile(&gpkt,&sid);
	gpkt.p_cutoff = pp->pf_date;
	ilist = pp->pf_ilist;
	elist = pp->pf_elist;

	if (dodelt(&gpkt,&stats,0,0) == 0)
		fmterr(&gpkt);
	if ((ser = sidtoser(&pp->pf_gsid,&gpkt)) == 0 ||
		sidtoser(&pp->pf_nsid,&gpkt))
			fatal("invalid sid in p-file (de3)");
	doie(&gpkt,ilist,elist,glist);
	setup(&gpkt,ser);
	finduser(&gpkt);
	doflags(&gpkt);
	move(&pp->pf_nsid,&gpkt.p_reqsid,sizeof(gpkt.p_reqsid));
	permiss(&gpkt);
	flushto(&gpkt,EUSERTXT,1);
	gpkt.p_chkeof = 1;
	copy(auxf(gpkt.p_file,'d'),dfilename);
	gpkt.p_gout = xfcreat(dfilename,0444);
	while(readmod(&gpkt)) {
		chkid(gpkt.p_line);
		fputs(gpkt.p_line,gpkt.p_gout);
	}
	fclose(gpkt.p_gout);
	orig = gpkt.p_glnno;
	gpkt.p_glnno = 0;
	gpkt.p_verbose = verbosity;
	Did_id = 0;
	while (fgets(line,sizeof(line),gin) != NULL && !chkid(line))
		;
	fclose(gin);
	if (gpkt.p_verbose && (num_files > 1 || had_dir || had_standinp))
		fprintf(gpkt.p_stdout,"\n%s:\n",gpkt.p_file);
	if (!Did_id)
		if (Sflags[IDFLAG - 'a'])
			fatal("no id keywords (cm6)");
		else if (gpkt.p_verbose)
			fprintf(stderr,"No id keywords (cm7)\n");

	/*
	The following while loop executes 'bdiff' on g-file and
	d-file. If 'bdiff' fails (usually because segmentation
	limit it is using is too large for 'diff'), it is
	invoked again, with a lower segmentation limit.
	*/
	difflim = 3500;
	diffloop = 0;
	while (1) {
		inserted = deleted = 0;
		gpkt.p_glnno = 0;
		gpkt.p_upd = 1;
		gpkt.p_wrttn = 1;
		getline(&gpkt);
		gpkt.p_wrttn = 1;
		newser = mkdelt(&gpkt,&pp->pf_nsid,&pp->pf_gsid,
						diffloop,orig);
		diffloop = 1;
		flushto(&gpkt,EUSERTXT,0);
		Diffin = dodiff(auxf(gpkt.p_file,'g'),dfilename,difflim);
		while (n = getdiff(&type,&linenum)) {
			if (type == INS) {
				inserted =+ n;
				insert(&gpkt,linenum,n,newser);
			}
			else {
				deleted =+ n;
				delete(&gpkt,linenum,n,newser);
			}
		}
		fclose(Diffin);
		if (gpkt.p_iop)
			while (readmod(&gpkt))
				;
		wait(&status);
		if (status) {		/* diff failed */
			/*
			Check top byte (exit code of child).
			*/
			if (((status >> 8) & 0377) == 32) /* 'execl' failed */
				fatal(sprintf(Error,
						"cannot execute '%s' (de12)",
						Diffpgm));
			/*
			Re-try.
			*/
			if (difflim =- 500) {	/* reduce segmentation */
				fprintf(stderr,
			"'%s' failed, re-trying, segmentation = %d (de13)\n",
					Diffpgm,difflim);
				fclose(Xiop);	/* set up */
				Xiop = 0;	/* for new x-file */
				Xcreate = 0;
				/*
				Re-open s-file.
				*/
				gpkt.p_iop = xfopen(gpkt.p_file,0);
				setbuf(gpkt.p_iop,gpkt.p_buf);
				/*
				Reset counters.
				*/
				gpkt.p_slnno = 0;
				gpkt.p_ihash = 0;
				gpkt.p_chash = 0;
				gpkt.p_nhash = 0;
				gpkt.p_keep = 0;
			}
			else
				/* tried up to 500 lines, can't go on */
				fatal("diff failed (de4)");
		}
		else {		/* no need to try again, worked */
			break;			/* exit while loop */
		}
	}
	unlink(dfilename);
	stats.s_ins = inserted;
	stats.s_del = deleted;
	stats.s_unc = orig - deleted;
	if (gpkt.p_verbose) {
		fprintf(gpkt.p_stdout,"%u inserted\n",stats.s_ins);
		fprintf(gpkt.p_stdout,"%u deleted\n",stats.s_del);
		fprintf(gpkt.p_stdout,"%u unchanged\n",stats.s_unc);
	}
	flushline(&gpkt,&stats);
	rename(auxf(gpkt.p_file,'x'),gpkt.p_file);
	if (Szqfile)
		rename(auxf(&gpkt.p_file,'q'),Pfilename);
	else {
		xunlink(Pfilename);
		xunlink(auxf(&gpkt.p_file,'q'));
	}
	clean_up(0);
	if (!HADN) {
		setuid(getuid());
		unlink(gfilename);
	}
}


mkdelt(pkt,sp,osp,diffloop,orig_nlines)
struct packet *pkt;
struct sid *sp, *osp;
int diffloop;
int orig_nlines;
{
	extern long Timenow;
	struct deltab dt;
	char str[128];
	int newser;
	extern char *Sflags[];
	register char *p;
	int ser_inc, opred, nulldel;

	if (!diffloop && pkt->p_verbose) {
		sid_ba(sp,str);
		fprintf(pkt->p_stdout,"%s\n",str);
	}
	putline(pkt,sprintf(str,"%c%c00000\n",CTLCHAR,HEAD));
	newstats(pkt,str,"0");
	move(sp,&dt.d_sid,sizeof(dt.d_sid));

	/*
	Check if 'null' deltas should be inserted
	(only if 'null' flag is in file and
	releases are being skipped) and set
	'nulldel' indicator appropriately.
	*/
	if (Sflags[NULLFLAG - 'a'] && (sp->s_rel > osp->s_rel + 1) &&
			!sp->s_br && !sp->s_seq &&
			!osp->s_br && !osp->s_seq)
		nulldel = 1;
	else
		nulldel = 0;
	/*
	Calculate how many serial numbers are needed.
	*/
	if (nulldel)
		ser_inc = sp->s_rel - osp->s_rel;
	else
		ser_inc = 1;
	/*
	Find serial number of the new delta.
	*/
	newser = dt.d_serial = maxser(pkt) + ser_inc;
	/*
	Find old predecessor's serial number.
	*/
	opred = sidtoser(osp,pkt);
	if (nulldel)
		dt.d_pred = newser - 1;	/* set predecessor to 'null' delta */
	else
		dt.d_pred = opred;
	dt.d_datetime = Timenow;
	substr(logname(),dt.d_pgmr,0,7);
	dt.d_type = 'D';
	del_ba(&dt,str);
	putline(pkt,str);
	if (ilist)
		mkixg(pkt,INCLUSER,INCLUDE);
	if (elist)
		mkixg(pkt,EXCLUSER,EXCLUDE);
	if (glist)
		mkixg(pkt,IGNRUSER,IGNORE);
	if (Mrs) {
		if (!(p = Sflags[VALFLAG - 'a']))
			fatal("MRs not allowed (de8)");
		if (*p && !diffloop && valmrs(pkt,p))
			fatal("invalid MRs (de9)");
		putmrs(pkt);
	}
	else if (Sflags[VALFLAG - 'a'])
		fatal("MRs required (de10)");
	putline(pkt,sprintf(str,"%c%c ",CTLCHAR,COMMENTS));
	putline(pkt,Comments);
	putline(pkt,"\n");
	putline(pkt,sprintf(str,CTLSTR,CTLCHAR,EDELTAB));
	if (nulldel)			/* insert 'null' deltas */
		while (--ser_inc) {
			putline(pkt,sprintf(str,"%c%c %s/%s/%05u\n",
				CTLCHAR, STATS,
				"00000", "00000", orig_nlines));
			dt.d_sid.s_rel =- 1;
			dt.d_serial =- 1;
			if (ser_inc != 1)
				dt.d_pred =- 1;
			else
				dt.d_pred = opred;	/* point to old pred */
			del_ba(&dt,str);
			putline(pkt,str);
			putline(pkt,sprintf(str,"%c%c ",CTLCHAR,COMMENTS));
			putline(pkt,"AUTO NULL DELTA\n");
			putline(pkt,sprintf(str,CTLSTR,CTLCHAR,EDELTAB));
		}
	return(newser);
}


mkixg(pkt,reason,ch)
struct packet *pkt;
int reason;
char ch;
{
	int n;
	char str[512];

	putline(pkt,sprintf(str,"%c%c",CTLCHAR,ch));
	for (n = maxser(pkt); n; n--) {
		if (pkt->p_apply[n].a_reason == reason)
			putline(pkt,sprintf(str," %u",n));
	}
	putline(pkt,"\n");
}


putmrs(pkt)
struct packet *pkt;
{
	register char **argv;
	char str[64];
	extern char *Varg[];

	for (argv = &Varg[VSTART]; *argv; argv++)
		putline(pkt,sprintf(str,"%c%c %s\n",CTLCHAR,MRNUM,*argv));
}


rdpfile(pkt,sp)
register struct packet *pkt;
struct sid *sp;
{
	char *user;
	struct pfile pf;
	static struct pfile goodpf;
	char line[512];
	int cnt;
	FILE *in, *out;

	cnt = -1;
	user = logname();
	zero(&goodpf,sizeof(goodpf));
	in = xfopen(auxf(pkt->p_file,'p'),0);
	out = xfcreat(auxf(pkt->p_file,'q'),0644);
	while (fgets(line,sizeof(line),in) != NULL) {
		pf_ab(line,&pf,1);
		if (equal(pf.pf_user,user)) {
			if (sp->s_rel == 0) {
				if (++cnt) {
					fclose(out);
					fclose(in);
					fatal("missing -r argument (de1)");
				}
				move(&pf,&goodpf,sizeof(pf));
				continue;
			}
			else if (sp->s_rel == pf.pf_gsid.s_rel &&
				sp->s_lev == pf.pf_gsid.s_lev &&
				sp->s_br == pf.pf_gsid.s_br &&
				sp->s_seq == pf.pf_gsid.s_seq) {
					move(&pf,&goodpf,sizeof(pf));
					continue;
			}
		}
		fputs(line,out);
	}
	fflush(out);
	fstat(fileno(out),&Statbuf);
	Szqfile = Statbuf.st_size;
	copy(auxf(pkt->p_file,'p'),Pfilename);
	fclose(out);
	fclose(in);
	if (!goodpf.pf_user[0])
		fatal("not in p-file (de2)");
	return(&goodpf);
}


dodiff(newf,oldf,difflim)
char *newf, *oldf;
int difflim;
{
	register int i;
	int pfd[2];
	FILE *iop;
	extern char Diffpgm[];
	char num[10];

	xpipe(pfd);
	if ((i = fork()) < 0) {
		close(pfd[0]);
		close(pfd[1]);
		fatal("cannot fork, try again (de11)");
	}
	else if (i == 0) {
		close(pfd[0]);
		close(1);
		dup(pfd[1]);
		close(pfd[1]);
		for (i = 5; i < 15; i++)
			close(i);
		sprintf(num,"%d",difflim);
		execl(Diffpgm,Diffpgm,oldf,newf,num,"-s",0);
		close(1);
		exit(32);	/* tell parent that 'execl' failed */
	}
	else {
		close(pfd[1]);
		iop = fdfopen(pfd[0],0);
		return(iop);
	}
}


getdiff(type,plinenum)
register char *type;
register int *plinenum;
{
	char line[512];
	register char *p;
	int num_lines;
	static int chg_num, chg_ln;
	int lowline, highline;

	if ((p = rddiff(line,512)) == NULL)
		return(0);

	if (*p == '-') {
		*type = INS;
		*plinenum = chg_ln;
		num_lines = chg_num;
	}
	else {
		p = linerange(p,&lowline,&highline);
		*plinenum = lowline;

		switch(*p++) {
		case 'd':
			num_lines = highline - lowline + 1;
			*type = DEL;
			skiplines(line,num_lines);
			break;

		case 'a':
			linerange(p,&lowline,&highline);
			num_lines = highline - lowline + 1;
			*type = INS;
			break;

		case 'c':
			chg_ln = lowline;
			num_lines = highline - lowline + 1;
			linerange(p,&lowline,&highline);
			chg_num = highline - lowline + 1;
			*type = DEL;
			skiplines(line,num_lines);
			break;
		}
	}

	return(num_lines);
}


insert(pkt,linenum,n,ser)
register struct packet *pkt;
register int linenum;
register int n;
int ser;
{
	char str[512];

	after(pkt,linenum);
	putline(pkt,sprintf(str,"%c%c %u\n",CTLCHAR,INS,ser));
	for (++n; --n; ) {
		rddiff(str,sizeof(str));
		putline(pkt,&str[2]);
	}
	putline(pkt,sprintf(str,"%c%c %u\n",CTLCHAR,END,ser));
}


delete(pkt,linenum,n,ser)
register struct packet *pkt;
register int linenum;
int n;
register int ser;
{
	char str[512];

	before(pkt,linenum);
	putline(pkt,sprintf(str,"%c%c %u\n",CTLCHAR,DEL,ser));
	after(pkt,linenum + n - 1);
	putline(pkt,sprintf(str,"%c%c %u\n",CTLCHAR,END,ser));
}


after(pkt,n)
register struct packet *pkt;
register int n;
{
	before(pkt,n);
	if (pkt->p_glnno == n)
		putline(pkt,0);
}


before(pkt,n)
register struct packet *pkt;
register int n;
{
	while (pkt->p_glnno < n) {
		if (!readmod(pkt))
			break;
	}
}


linerange(cp,low,high)
register char *cp;
register int *low, *high;
{
	cp = satoi(cp,low);
	if (*cp == ',')
		cp = satoi(++cp,high);
	else
		*high = *low;

	return(cp);
}


skiplines(lp,num)
register char *lp;
register int num;
{
	for (++num;--num;)
		rddiff(lp,512);
}


rddiff(s,n)
register char *s;
register int n;
{
	register int r;

	if ((r = fgets(s,n,Diffin)) != NULL && HADP)
		fputs(s,gpkt.p_stdout);
	return(r);
}


enter(pkt,ch,n,sidp)
struct packet *pkt;
char ch;
int n;
struct sid *sidp;
{
	char str[32];
	register struct apply *ap;

	sid_ba(sidp,str);
	ap = &pkt->p_apply[n];
	if (pkt->p_cutoff > pkt->p_idel[n].i_datetime)
		switch(ap->a_code) {
	
		case EMPTY:
			switch (ch) {
			case INCLUDE:
				condset(ap,APPLY,INCLUSER);
				break;
			case EXCLUDE:
				condset(ap,NOAPPLY,EXCLUSER);
				break;
			case IGNORE:
				condset(ap,EMPTY,IGNRUSER);
				break;
			}
			break;
		case APPLY:
			fatal("internal error in delta/enter() (de5)");
			break;
		case NOAPPLY:
			fatal("internal error in delta/enter() (de6)");
			break;
		default:
			fatal("internal error in delta/enter() (de7)");
			break;
		}
}


escdodelt()	/* dummy routine for dodelt() */
{
}


clean_up(n)
{
	if (gpkt.p_file[0])
		unlockit(auxf(gpkt.p_file,'z'),getpid());
	xrm(&gpkt);
	xfreeall();
}
