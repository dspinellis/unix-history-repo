# include	"../hdr/defines.h"
# include	"../hdr/had.h"

static char Sccsid[] = "@(#)cmt.c	4.5	%G%";

struct packet gpkt;
int	num_files, had_ffile;
int	F_Opened, Opened, Domrs, First;
char	*Comments, *Mrs, *ffile;
char	Cstr[BUFSIZ], Mstr[BUFSIZ], Line[BUFSIZ], had[26];
FILE	*iop, *Xiop;
static	char ifde[] = "initial file does not exists";

main(argc,argv)
int argc;
register char *argv[];
{
	register int i;
	register char *p;
	char c;
	extern cmt();
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
			case 'f':
				if (*p) {
					ffile = p;
					++had_ffile;
					if (!exists(ffile))
						fatal(ifde);
				}
				break;
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
	Fflags &= ~FTLEXIT;
	Fflags |= FTLJMP;

	/*
	Invoke 'cmt' for each file argument. 
	*/
	for (i = 1; i < argc; i++)
		if (p = argv[i])
			do_file(p,cmt);

	exit(Fcnt ? 1 : 0);
}


static char s_warn[] = "WARNING: MR flag is set; `%s' should contain both MR line and comment line\n";

static char ns_warn[] = "WARNING: MR flag is not set; `%s' should only contain comment line\n";

cmt(file)
register char *file;
{
	extern char had_dir, had_standinp;
	extern	char	*Sflags[];
	extern	char	Pgmr[SZLNAM];
	char	line[BUFSIZ];
	int	fowner, downer, user;

	/*
	Set up to return to caller ('main') from 'fatal'.
	*/
	if (setjmp(Fjmp))
		return;

	sinit(&gpkt,file,1);	/* init packet and open file */

	if (lockit(auxf(gpkt.p_file,'z'),2,getpid()))
		fatal("cannot create lock file (cm4)");

	if (num_files > 1 || had_dir || had_standinp)
		printf("\n%s:\n",gpkt.p_file);

	First = 1;
	gpkt.p_reopen = 1;
	do_delt(&gpkt);		/* read delta table for First time */
	finduser(&gpkt);
	doflags(&gpkt);		/* get flags (see if v flag is set) */
	permiss(&gpkt);

	/*
	Check that user is either owner of file or
	directory, or is one who made the initial delta
	*/

	fstat(fileno(gpkt.p_iop),&Statbuf);
	fowner = Statbuf.st_uid & 0377;
	copy(gpkt.p_file,line);		/* temporary for dname() */
	if (stat(dname(line),&Statbuf))
		downer = -1;
	else downer = Statbuf.st_uid & 0377;
	user = getuid() & 0377;
	if (user != fowner || user != downer)
		if (!equal(Pgmr,logname())) {
			sprintf(Error, "you are neither owner nor '%s' (rc4)",Pgmr);
			fatal(Error);
		}

	if ((HADF && had_ffile)) {
		if (Sflags[VALFLAG - 'a'])
			fprintf(stderr,s_warn,ffile);
		else fprintf(stderr,ns_warn,ffile);
		sleep(5);
	}
	flushto(&gpkt,EUSERTXT,1);
	gpkt.p_chkeof = 1;	/* indicate that EOF is okay */
	while (getline(&gpkt))	/* this will read body checking for cor */
		;

	gpkt.p_upd = 1;		/* x-file is to be used */
	gpkt.p_wrttn = 1;	/* prevent printing of header line */
	getline(&gpkt);		/* skip over old header record */
	gpkt.p_wrttn = 1;

	/*
	Write new header.
	*/
	sprintf(Line,"%c%c00000\n",CTLCHAR,HEAD);
	putline(&gpkt,Line);
	do_delt(&gpkt);		/* read delta table second time */

	flushto(&gpkt,EUSERNAM,0);
	flushto(&gpkt,EUSERTXT,0);
	while(getline(&gpkt))
		;

	flushline(&gpkt,0);	/* flush buffer, fix header, and close */
	rename(auxf(gpkt.p_file,'x'),gpkt.p_file);
	xrm(&gpkt);
	unlockit(auxf(gpkt.p_file,'z'),getpid());
	return;
}


static	char cle[] = "comment line for initial delta already exists";

do_delt(pkt)
register struct packet *pkt;
{
	int	n;
	int	did_zero = 0;
	struct deltab dt;
	struct stats stats;

	while(getstats(pkt,&stats)) {
		if(getadel(pkt,&dt) != BDELTAB)
			fmterr(pkt);
		if(dt.d_type == 'D' && dt.d_pred == 0) {
			copy(dt.d_pgmr,Pgmr);
			if (First)
				did_zero++;
			else {
				putline(pkt,0);
				fixintdel();
			}
		}
		while((n = getline(pkt)) != NULL)
			if (pkt->p_line[0] != CTLCHAR)
				break;
			else {
				switch(pkt->p_line[1]) {
				case EDELTAB:
					break;
				case INCLUDE:
				case EXCLUDE:
				case IGNORE:
				case MRNUM:
					continue;
				case COMMENTS:
					if (First)
						if(did_zero)
							fatal(cle);
					continue;
				default:
					fmterr(pkt);
				}
				break;
			}
		if (n ==NULL || pkt->p_line[0] != CTLCHAR)
			fmterr(pkt);
	}
	First = 0;
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
	extern	char	*satoi();

	p = pkt->p_line;
	if (getline(pkt) == NULL || *p++ != CTLCHAR || *p++ != STATS)
		return(0);
	NONBLANK(p);
	p = satoi(p,&statp->s_ins);
	p = satoi(++p,&statp->s_del);
	satoi(++p,&statp->s_unc);
	return(1);
}

clean_up(n)
{
	if (gpkt.p_file[0])
		unlockit(auxf(gpkt.p_file,'z'),getpid());
	if (gpkt.p_iop)
		fclose(gpkt.p_iop);

	xrm(&gpkt);
	if (exists(auxf(gpkt.p_file,'x')))
		remove(auxf(gpkt.p_file,'x'));	/* remove x-file */
	Xiop = 0;
	if (F_Opened)
		fclose(iop);
	iop = F_Opened = Opened = 0;
	xfreeall();
}


fixintdel()
{

	register char	*p;
	register int	doprmt;
	int	tty[3];
	char	str[128];

	doprmt = 0;
	if (gtty(0,tty) >= 0)
		doprmt++;

	if (!HADF && !had_ffile) {
		Opened++;
		iop = stdin;
	}
	else if (HADF && had_ffile) {
		iop = xfopen(ffile,0);
		doprmt = 0;
		Opened++;
		F_Opened++;
	}
	else if (HADF && !had_ffile)
		doprmt = 0;

	if ((p = Sflags[VALFLAG - 'a'])) {
		if (doprmt)
			printf("MRs? ");
		if (Opened) {
			Mrs = getinput(" ",Mstr);
			mrfixup();
			if (*p && valmrs(&gpkt,p))
				fatal("invalid MRs (de9)");
			putmrs(&gpkt);
		}
		else {
			sprintf(Line,CTLSTR,CTLCHAR,MRNUM);
			putline(&gpkt,Line);
		}
	}
	if (doprmt)
		printf("comments? ");
	if (Opened) {
		sprintf(Line,"\n%c%c ",CTLCHAR,COMMENTS);
		Comments = getinput(Line,Cstr);
		sprintf(str,"%c%c ",CTLCHAR,COMMENTS);
		putline(&gpkt,str);
		putline(&gpkt,Comments);
		putline(&gpkt,"\n");
	}
	else {
		sprintf(Line,CTLSTR,CTLCHAR,COMMENTS);
		putline(&gpkt,Line);
	}

	if (F_Opened)
		fclose(iop);
	F_Opened = Opened = 0;
}


getinput(repstr,result)
char *repstr;
char *result;
{
	char line[BUFSIZ];
	register int done, sz;
	register char *p;

	result[0] = 0;
	done = 0;
	setbuf(iop,NULL);
	sz = sizeof(line) - size(repstr);
	while (!done && fgets(line,sz,iop) != NULL) {
		p = index(line, '\0');
		if (*--p == '\n') {
			if (*--p == '\\') {
				copy(repstr,p);
			}
			else {
				*++p = 0;
				++done;
			}
		}
		else
			fatal("line too long (co18)");
		if ((size(line) + size(result)) > RESPSIZE)
			fatal("response too long (co19)");
		strcat(result,line);
	}
	return(result);
}


putmrs(pkt)
struct packet *pkt;
{
	register char **argv;
	char str[64];
	extern char *Varg[];

	for (argv = &Varg[VSTART]; *argv; argv++) {
		sprintf(str,"%c%c %s\n",CTLCHAR,MRNUM,*argv);
		putline(pkt,str);
	}
}
