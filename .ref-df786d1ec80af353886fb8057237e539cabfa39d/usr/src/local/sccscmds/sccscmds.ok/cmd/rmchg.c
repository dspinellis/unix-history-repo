# include "../hdr/defines.h"
# include "../hdr/had.h"

SCCSID(@(#)rmchg.c	4.1);

/*
	Program to remove a specified delta from an SCCS file,
	when invoked as 'rmdel',
	or to change the MRs and/or comments of a specified delta,
	when invoked as 'chghist'.
	(The program has two links to it, one called 'rmdel', the
	other 'chghist'.)

	The delta to be removed (or whose MRs and/or comments
	are to be changed) is specified via the
	r argument, in the form of an SID.

	If the delta is to be removed, it must be the most recent one
	in its branch in the delta tree (a so-called 'leaf' delta).
	For either function, the delta being processed must not
	have any 'delivered' MRs, and the user must have basically
	the same permissions as are required to make deltas.

	If a directory is given as an argument, each SCCS file
	within the directory will be processed as if it had been
	specifically named. If a name of '-' is given, the standard
	input will be read for a list of names of SCCS files to be
	processed. Non SCCS files are ignored.
*/

# define COPY 0
# define NOCOPY 1

struct sid sid;
int num_files;
char had[26];
char D_type;
int D_serial;

main(argc,argv)
int argc;
char *argv[];
{
	register int i;
	register char *p;
	char c;
	extern rmchg();
	extern int Fcnt;

	/*
	Set flags for 'fatal' to issue message, call clean-up
	routine, and terminate processing.
	*/
	Fflags = FTLMSG | FTLCLN | FTLEXIT;

	for(i=1; i<argc; i++)
		if(argv[i][0] == '-' && (c = argv[i][1])) {
			p = &argv[i][2];
			switch (c) {

			case 'r':
				if (!(*p))
					fatal("r has no sid (rc11)");
				chksid(sid_ab(p,&sid),&sid);
				break;
			default:
				fatal("unknown key letter (cm1)");
			}

			if (had[c - 'a']++)
				fatal("key letter twice (cm2)");
			argv[i] = 0;
		}
		else
			num_files++;

	if(num_files == 0)
		fatal("missing file arg (cm3)");

	if (*(p = sname(argv[0])) == 'n')
		p++;
	if (equal(p,"rmdel"))
		D_type = 'R';		/* invoked as 'rmdel' */
	else if (equal(p,"chghist"))
		D_type = 'D';		/* invoked as 'chghist' */
	else
		fatal("bad invocation (rc10)");

	setsig();

	/*
	Change flags for 'fatal' so that it will return to this
	routine (main) instead of terminating processing.
	*/
	Fflags =& ~FTLEXIT;
	Fflags =| FTLJMP;

	/*
	Call 'rmchg' routine for each file argument.
	*/
	for (i=1; i<argc; i++)
		if (p = argv[i])
			do_file(p,rmchg);

	exit(Fcnt ? 1 : 0);
}


/*
	Routine that actually causes processing of the delta.
	Processing on the file takes place on a
	temporary copy of the SCCS file (the x-file).
	The name of the x-file is the same as that of the
	s-file (SCCS file) with the 's.' replaced by 'x.'.
	At end of processing, the s-file is removed
	and the x-file is renamed with the name of the old s-file.

	This routine makes use of the z-file to lock out simultaneous
	updates to the SCCS file by more than one user.
*/

struct packet gpkt;	/* see file s.h */
char line[BUFSIZ];
char *Mrs;
char *Comments;
int Domrs;

USXALLOC();		/* defines alloc() and free() */

rmchg(file)
char *file;
{
	static int first_time 1;
	struct deltab dt;	/* see file s.defines.h */
	struct stats stats;	/* see file s.defines.h */
	extern char *Sflags[];
	int n;
	char *p, *cp;
	int keep;
	extern char Pgmr[8];
	int fowner, downer, user;

	if (setjmp(Fjmp))	/* set up to return here from 'fatal' */
		return;		/* and return to caller of rmchg */

	if (!HADR)
		fatal("missing r (rc1)");

	if (D_type == 'D' && first_time) {
		first_time = 0;
		dohist(file);
	}

	if (!exists(file))
		fatal(sprintf(Error,"file %s does not exist (rc2)",file));

	/*
	Lock out any other user who may be trying to process
	the same file.
	*/
	if (lockit(auxf(file,'z'),2,getpid()))
		fatal("cannot create lock file (cm4)");

	sinit(&gpkt,file,1);	/* initialize packet and open s-file */

	/*
	Flag for 'putline' routine to tell it to open x-file
	and allow writing on it.
	*/
	gpkt.p_upd = 1;

	/*
	Save requested SID for later checking of
	permissions (by 'permiss').
	*/
	move(&sid,&gpkt.p_reqsid,sizeof(gpkt.p_reqsid));

	/*
	Now read-in delta table. The 'dodelt' routine
	will read the table and change the delta entry of the
	requested SID to be of type 'R' if this is
	being executed as 'rmdel'; otherwise, for 'chghist', only
	the MR and comments sections will be changed 
	(by 'escdodelt', called by 'dodelt').
	*/
	if (dodelt(&gpkt,&stats,&sid,D_type) == 0)
		fmterr(&gpkt);

	/*
	Get serial number of requested SID from
	delta table just processed.
	*/
	D_serial = sidtoser(&gpkt.p_reqsid,&gpkt);

	/*
	If SID has not been zeroed (by 'dodelt'),
	SID was not found in file.
	*/
	if (sid.s_rel != 0)
		fatal("nonexistent sid (rc3)");
	/*
	Replace 'sid' with original 'sid'
	requested.
	*/
	move(&gpkt.p_reqsid,&sid,sizeof(gpkt.p_reqsid));

	/*
	Now check permissions.
	*/
	finduser(&gpkt);
	doflags(&gpkt);
	permiss(&gpkt);

	/*
	Check that user is either owner of file or
	directory, or is one who made the delta.
	*/
	fstat(fileno(gpkt.p_iop),&Statbuf);
	fowner = Statbuf.st_uid & 0377;
	copy(gpkt.p_file,line);		/* temporary for dname() */
	if (stat(dname(line),&Statbuf))
		downer = -1;
	else
		downer = Statbuf.st_uid & 0377;
	user = getuid() & 0377;
	if (user != fowner || user != downer)
		if (!equal(Pgmr,logname()))
			fatal(sprintf(Error,
				"you are neither owner nor '%s' (rc4)",Pgmr));

	/*
	For 'rmdel', check that delta being removed is a
	'leaf' delta, and if ok,
	process the body.
	*/
	if (D_type == 'R') {
		for (n = maxser(&gpkt); n > D_serial; n--) {
			p = &gpkt.p_idel[n];
			if (p->i_pred == D_serial)
				fatal("not a 'leaf' delta (rc5)");
		}

		/*
		   For 'rmdel' check that the sid requested is
		   not contained in p-file, should a p-file
		   exist.
		*/

		if (exists(auxf(gpkt.p_file,'p')))
			rdpfile(&gpkt,&sid);

		flushto(&gpkt,EUSERTXT,COPY);

		keep = YES;
		gpkt.p_chkeof = 1;		/* set EOF is ok */
		while ((p = getline(&gpkt)) != NULL) {
			if (*p++ == CTLCHAR) {
				cp = p++;
				NONBLANK(p);
				/*
				Convert serial number to binary.
				*/
				if (*(p = satoi(p,&n)) != '\n')
					fmterr(&gpkt);
				if (n == D_serial) {
					gpkt.p_wrttn = 1;
					if (*cp == INS)
						keep = NO;
					else
						keep = YES;
				}
			}
			else
				if (keep == NO)
					gpkt.p_wrttn = 1;
		}
	}
	else {
		/*
		This is for invocation as 'chghist'.
		Check MRs.
		*/
		if (Mrs) {
			if (!(p = Sflags[VALFLAG - 'a']))
				fatal("MRs not allowed (rc6)");
			if (*p && valmrs(&gpkt,p))
				fatal("inavlid MRs (rc7)");
		}
		else
			if (Sflags[VALFLAG - 'a'])
				fatal("MRs required (rc8)");

		/*
		Indicate that EOF at this point is ok, and
		flush rest of s-file to x-file.
		*/
		gpkt.p_chkeof = 1;
		while (getline(&gpkt))
			;
	}

	flushline(&gpkt,0);

	/*
	Delete old s-file, change x-file name to s-file.
	*/
	rename(auxf(&gpkt,'x'),&gpkt);

	clean_up();
}


escdodelt(pkt)
struct packet *pkt;
{
	extern int First_esc;
	char *p;
	extern long Timenow;

	if (D_type == 'D' && First_esc) {	/* chghist, first time */
		First_esc = 0;
		if (Mrs)
			putmrs(pkt);

		putline(pkt,sprintf(line,"%c%c ",CTLCHAR,COMMENTS));
		putline(pkt,Comments);
		putline(pkt,"\n");
		putline(pkt,sprintf(line,"%c%c ",CTLCHAR,COMMENTS));
		putline(pkt,"*** CHANGED *** ");
		date_ba(&Timenow,line);		/* get date and time */
		putline(pkt,line);
		putline(pkt,sprintf(line," %s\n",logname()));
	}

	if (pkt->p_line[1] == MRNUM) {
		p = &pkt->p_line;
		while (*p)
			p++;
		if (*(p - 2) == DELIVER)
			fatal("delta specified has delivered MR (rc9)");

		if (D_type == 'D')		/* turn MRs into comments */
			pkt->p_line[1] = COMMENTS;
	}
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


clean_up()
{
	xrm(&gpkt);
	if (gpkt.p_file[0])
		unlockit(auxf(gpkt.p_file,'z'),getpid());
	if (exists(auxf(gpkt.p_file,'x')))
		xunlink(auxf(gpkt.p_file,'x'));
	xfreeall();
}


rdpfile(pkt,sp)
register struct packet *pkt;
struct sid *sp;
{
	struct pfile pf;
	char line[BUFSIZ];
	FILE *in;

	in = xfopen(auxf(pkt->p_file,'p'),0);
	while (fgets(line,sizeof(line),in) != NULL) {
		pf_ab(line,&pf,1);
		if (sp->s_rel == pf.pf_gsid.s_rel &&
			sp->s_lev == pf.pf_gsid.s_lev &&
			sp->s_br == pf.pf_gsid.s_br &&
			sp->s_seq == pf.pf_gsid.s_seq) {
				fclose(in);
				fatal("being edited -- sid is in p-file (rc12)");
		}
	}
	fclose(in);
	return;
}
