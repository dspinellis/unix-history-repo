/*************************************************************************/
/*									 */
/*	prs [-d<dataspec>] [-r<sid>] [-c<cutoff>] [-a]			 */
/*	    [-y<reverse-cutoff>] file ...				 */
/*									 */
/*************************************************************************/

/*
	Program to print parts or all of an SCCS file
	in user supplied format.
	Arguments to the program may appear in any order
	and consist of keyletters, which begin with '-',
	and named files.

	If a direcory is given as an argument, each
	SCCS file within the directory is processed as if
	it had been specifically named. If a name of '-'
	is given, the standard input is read for a list
	of names of SCCS files to be processed.
	Non-SCCS files are ignored.
*/

# include "../hdr/defines.h"
# include "../hdr/had.h"

SCCSID(@(#)prs.c	1.3);

char	had[26];
char	Getpgm[]	"/usr/local/get";
char	Sid[32];
char	Mod[16];
char	*Type;
char	Deltadate[18];
char	*Deltatime;
char	tempskel[]	"/tmp/prXXXXXX";	/* used to generate temp
						   file names
						*/
char	untmp[32], uttmp[32], cmtmp[32];
char	mrtmp[32], bdtmp[32];
FILE	*UNiop;
FILE	*UTiop;
FILE	*CMiop;
FILE	*MRiop;
FILE	*BDiop;
char	line[BUFSIZ];
int	num_files;
long	cutoff;
long	revcut;
char	*dataspec;
char	iline[BUFSIZ], xline[BUFSIZ], gline[BUFSIZ];
char	*maket();
struct	packet	gpkt;
struct	sid	sid;
struct	time	*Dtime;

main(argc,argv)
int argc;
char *argv[];
{
	register int j;
	register char *p;
	char c;
	extern prs();
	extern int Fcnt;

	/*
	Set flags for 'fatal' to issue message, call clean-up
	routine, and terminate processing.
	*/
	Fflags = FTLMSG | FTLCLN | FTLEXIT;


	/*
	The following loop processes keyletters and arguments.
	Note that these are processed only once for each
	invocation of 'main'.
	*/
	for (j = 1; j < argc; j++)
		if (argv[j][0] == '-' && (c = argv[j][1])) {
			p = &argv[j][2];
			switch (c) {

			case 'r':	/* delta cutoff */
				if (*p) {
					if (invalid(p))
						fatal("invalid sid (co8)");
					sid_ab(p,&sid);
				}
				break;

			case 'c':	/* time cutoff */
				if (*p && date_ab(p,&cutoff))
					fatal("bad date/time (cm5)");
				break;

			case 'y':	/* reverse time cutoff */
				if (*p && date_ab(p,&revcut))
					fatal ("bad date/time (cm5)");
				break;

			case 'a':
				if (*p)
					fatal("value after a arg (cm7)");
				break;
			case 'd':	/* dataspec line */
				dataspec = p;
				break;
			default:
				fatal("unknown key letter (cm1)");
			}

			if (had[c - 'a']++)
				fatal("key letter twice (cm2)");
			argv[j] = 0;
		}
		else
			num_files++;

	if (num_files == 0)
		fatal("missing file arg (cm3)");

	if (!HADD)
		exit(0);
	if (HADC && HADY)
		fatal("both 'c' and 'y' keyletters specified (prs2)");

	setsig();

	/*
	Change flags for 'fatal' so that it will return to this
	routine (main) instead of terminating processing.
	*/
	Fflags =& ~FTLEXIT;
	Fflags =| FTLJMP;

	/*
	Call 'prs' routine for each file argument.
	*/
	for (j = 1; j < argc; j++)
		if (p = argv[j])
			do_file(p,prs);

	exit(Fcnt ? 1 : 0);
}


prs(file)
register	char	*file;
{
	int	n;
	extern	char	had_dir, had_standinp;

	if (setjmp(Fjmp))
		return;
	sinit(&gpkt,file,1);	/* init packet and open SCCS file */

	gpkt.p_reqsid.s_rel = sid.s_rel;
	gpkt.p_reqsid.s_lev = sid.s_lev;
	gpkt.p_reqsid.s_br = sid.s_br;
	gpkt.p_reqsid.s_seq = sid.s_seq;
	gpkt.p_cutoff = cutoff;
	gpkt.p_reopen = 1;

	/*
	read delta table entries checking only for format error
	*/
	deltblchk(&gpkt);

	/*
	create auxiliary file for User Name Section
	*/

	aux_create(UNiop,untmp,EUSERNAM);

	doflags(&gpkt);

	/*
	create auxiliary file for the User Text section
	*/

	aux_create(UTiop,uttmp,EUSERTXT);

	/*
	indicate to 'getline' that EOF is okay
	*/
	gpkt.p_chkeof = 1;

	/*
	read body of SCCS file and create temp file for it
	*/
	while(read_mod(&gpkt))
		;

	if (num_files > 1 || had_dir || had_standinp)
		printf("\n%s:\n",gpkt.p_file);
	/*
	Here, file has already been re-opened (by 'getline')
	*/
	getline(&gpkt);		/* skip over header line */

	/*
	call dodeltbl to read delta table entries
	*/

	dodeltbl(&gpkt);

	clean_up();

	return;
}


dodeltbl(pkt)
register struct packet *pkt;
{
	int	n;
	struct	deltab	dt;
	struct	stats	stats;

	/*
	Read entire delta table.
	*/
	while (getstats(pkt,&stats)) {
		if (getadel(pkt,&dt) != BDELTAB)
			fmterr(pkt);

		/*
		Read rest of delta entry. 
		*/
		while ((n = getline(pkt)) != NULL)
			if (pkt->p_line[0] != CTLCHAR)
				break;
			else {
				switch (pkt->p_line[1]) {
				case EDELTAB:
					scanspec(dataspec,&dt,&stats);
					break;
				case INCLUDE:
					getit(iline,n);
					continue;
				case EXCLUDE:
					getit(xline,n);
					continue;
				case IGNORE:
					getit(gline,n);
					continue;
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


/*
 * The scanspec procedure scans the dataspec searching for ID keywords.
 * When a keyword is found the value is replaced and printed on the
 * standard output. Any character that is not an ID keyword is printed
 * immediately.
*/

static	char	Zkeywd[5]	"@(#)";
scanspec(spec,dtp,statp)
char spec[];
struct	deltab	*dtp;
struct	stats	*statp;
{

	extern	char	*Sflags[];
	register char *lp;
	register char	*k;
	union {
		char	str[2];
		int	istr;
	} u;
	register	char	c;

	idsetup(&dtp->d_sid,&gpkt,&dtp->d_datetime);
	for(lp = spec; *lp != 0; lp++) {
		if(lp[0] == ':' && lp[1] != 0 && lp[2] == ':') {
			c = *++lp;
			switch (c) {
			case 'I':	/* SID */
				printf("%s",Sid);
				break;
			case 'R':	/* Release number */
				printf("%u",dtp->d_sid.s_rel);
				break;
			case 'L':	/* Level number */
				printf("%u",dtp->d_sid.s_lev);
				break;
			case 'B':	/* Branch number */
				if (dtp->d_sid.s_br != 0)
					printf("%u",dtp->d_sid.s_br);
				break;
			case 'S':	/* Sequence number */
				if (dtp->d_sid.s_seq != 0)
					printf("%u",dtp->d_sid.s_seq);
				break;
			case 'D':	/* Date delta created */
				printf("%s",Deltadate);
				break;
			case 'T':	/* Time delta created */
				printf("%s",Deltatime);
				break;
			case 'P':	/* Programmer who created delta */
				printf("%s",dtp->d_pgmr);
				break;
			case 'C':	/* Comments */
				break;
			case 'Y':	/* Type flag */
				printf("%s",Type);
				break;
			case 'M':	/* Module name */
				printf("%s",Mod);
				break;
			case 'W':	/* Form of what string */
				printf("%s",Zkeywd);
				printf("%s",Mod);
				putchar('\t');
				printf("%s",Sid);
				break;
			case 'A':	/* Form of what string */
				printf("%s",Zkeywd);
				printf("%s ",Type);
				printf("%s ",Mod);
				printf("%s",Sid);
				printf("%s",Zkeywd);
				break;
			case 'Z':	/* what string constructor */
				printf("%s",Zkeywd);
				break;
			case 'F':	/* File name */
				printf("%s",sname(gpkt.p_file));
				break;
			default:
				putchar(':');
				putchar(c);
				putchar(':');
				break;
			}
			lp++;
		}
		else if(lp[0] == ':' && lp[1] != 0 && lp[2] !=0 && lp[3] == ':') {
			if (lp[1] == ':') {
				putchar(':');
				*lp =+ 2;
				continue;
			}
			u.str[0] = *++lp;
			u.str[1] = *++lp;
			switch (u.istr) {
			case 'Dl':	/* Delta line statistics */
				printf("%05d",statp->s_ins);
				putchar('/');
				printf("%05d",statp->s_del);
				putchar('/');
				printf("%05d",statp->s_unc);
				break;
			case 'Li':	/* Lines inserted by delta */
				printf("%05d",statp->s_ins);
				break;
			case 'Ld':	/* Lines deleted by delta */
				printf("%05d",statp->s_del);
				break;
			case 'Lu':	/* Lines unchanged by delta */
				printf("%05d",statp->s_unc);
				break;
			case 'DT':	/* Delta type */
				printf("%c",dtp->d_type);
				break;
			case 'Dy':	/* Year delta created */
				printf("%02d",Dtime->t_year);
				break;
			case 'Dm':	/* Month delta created */
				printf("%02d",(Dtime->t_month + 1));
				break;
			case 'Dd':	/* Day delta created */
				printf("%02d",Dtime->t_day_month);
				break;
			case 'Th':	/* Hour delta created */
				printf("%02d",Dtime->t_hours);
				break;
			case 'Tm':	/* Minutes delta created */
				printf("%02d",Dtime->t_minutes);
				break;
			case 'Ts':	/* Seconds delta created */
				printf("%02d",Dtime->t_seconds);
				break;
			case 'DS':	/* Delta sequence number */
				printf("%d",dtp->d_serial);
				break;
			case 'DP':	/* Predecessor delta sequence number */
				printf("%d",dtp->d_pred);
				break;
			case 'DI':	/* Deltas included,excluded,ignored */
				printf("%s",iline);
				putchar('/');
				printf("%s",xline);
				putchar('/');
				printf("%s",gline);
				break;
			case 'Di':	/* Deltas included */
				printf("%s",iline);
				break;
			case 'Dx':	/* Deltas excluded */
				printf("%s",xline);
				break;
			case 'Dg':	/* Deltas ignored */
				printf("%s",gline);
				break;
			case 'MR':	/* MR numbers */
				break;
			case 'UN':	/* User names */
				printfile(untmp);
				break;
			case 'MF':	/* MR validation flag */
				if (Sflags[VALFLAG - 'a'])
					printf("yes");
				else printf("no");
				break;
			case 'MP':	/* MR validation program */
				if (!(k = Sflags[VALFLAG - 'a']))
					printf("none");
				else printf("%s",k);
				break;
			case 'KF':	/* Keyword err/warn flag */
				if (Sflags[IDFLAG - 'a'])
					printf("yes");
				else printf("no");
				break;
			case 'BF':	/* Branch flag */
				if (Sflags[BRCHFLAG - 'a'])
					printf("yes");
				else printf("no");
				break;
			case 'FB':	/* Floor Boundry */
				if (k = Sflags[FLORFLAG - 'a'])
					printf("%s",k);
				else printf("none");
				break;
			case 'CB':	/* Ceiling Boundry */
				if (k = Sflags[CEILFLAG - 'a'])
					printf("%s",k);
				else printf("none");
				break;
			case 'Ds':	/* Default SID */
				if (k = Sflags[DEFTFLAG - 'a'])
					printf("%s",k);
				else printf("none");
				break;
			case 'ND':	/* Null delta */
				if (Sflags[NULLFLAG - 'a'])
					printf("yes");
				else printf("no");
				break;
			case 'FD':	/* File descriptive text */
				printfile(uttmp);
				break;
			case 'BD':	/* Entire file body */
				printfile(bdtmp);
				break;
			case 'GB':	/* Gotten body from 'get' */
				getbody(&dtp->d_sid,&gpkt);
				break;
			default:
				putchar(':');
				printf("%c",u.istr);
				putchar(':');
				break;
			}
			lp++;
		}
		else {
			c = *lp;
			if (c == '\\') {
				switch(*++lp) {
				case 'n':	/* for newline */
					putchar('\n');
					break;
				case ':':	/* for wanted colon */
					putchar(':');
					break;
				case 't':	/* for tab */
					putchar('\t');
					break;
				case 'b':	/* for backspace */
					putchar('\b');
					break;
				case 'r':	/* for carriage return */
					putchar('\r');
					break;
				case 'f':	/* for form feed */
					putchar('\f');
					break;
				case '\\':	/* for backslash */
					putchar('\\');
					break;
				case '\'':	/* for single quote */
					putchar('\'');
					break;
				default:	/* unknown case */
					putchar('\\');
					putchar(*lp);
					break;
				}
			}
			else putchar(*lp);
		}
	}
	/*
	zero out first char of global string lines in case
	a value is not gotten in next delta table entry
	*/
	iline[0] = xline[0] = gline[0] = 0;
	putchar('\n');
	return;
}


clean_up()
{
	unlink(untmp);
	unlink(uttmp);
	unlink(bdtmp);
}


/* This function takes as it's argument the SID inputed and determines
 * whether or not it is valid (e. g. not ambiguous or illegal).
*/
invalid(i_sid)
register char	*i_sid;
{
	register int count;
	register int digits;
	count = digits = 0;
	if (*i_sid == '0' || *i_sid == '.')
		return (1);
	i_sid++;
	digits++;
	while (*i_sid != '\0') {
		if (*i_sid++ == '.') {
			digits = 0;
			count++;
			if (*i_sid == '0' || *i_sid == '.')
				return (1);
		}
		digits++;
		if (digits > 5)
			return (1);
	}
	if (*(--i_sid) == '.' )
		return (1);
	if (count == 1 || count == 3)
		return (0);
	return (1);
}


deltblchk(pkt)
register struct packet *pkt;
{
	int	n;
	struct	deltab	dt;
	struct	stats	stats;

	/*
	Read entire delta table.
	*/
	while (getstats(pkt,&stats)) {
		if (getadel(pkt,&dt) != BDELTAB)
			fmterr(pkt);

		/*
		Read rest of delta entry. 
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
	if (pkt->p_line[1] != BUSERNAM)
		fmterr(pkt);
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


getadel(pkt,dt)
register struct packet *pkt;
register struct deltab *dt;
{
	if (getline(pkt) == NULL)
		fmterr(pkt);
	return(del_ab(pkt->p_line,dt,pkt));
}



char	*maket(file)
char	*file;
{
	FILE *iop;

	copy(tempskel,file);
	iop = xfcreat(mktemp(file),0644);

	return(iop);
}


printfile(file)
register	char	*file;
{
	register	char	*p;
	FILE	*iop;

	iop = xfopen(file,0);
	while ((p = fgets(line,sizeof(line),iop)) != NULL)
		printf("%s",p);
	fclose(iop);
}


read_mod(pkt)
register struct packet *pkt;
{
	register char *p;
	int ser;
	int iord;
	register struct apply *ap;

	BDiop = maket(bdtmp);
	while (getline(pkt) != NULL) {
		p = pkt->p_line;
		fputs(p,BDiop);
		if (*p++ != CTLCHAR)
			continue;
		else {
			if (!((iord = *p++) == INS || iord == DEL || iord == END))
				fmterr(pkt);
			NONBLANK(p);
			satoi(p,&ser);
			if (iord == END)
				remq(pkt,ser);
			else if ((ap = &pkt->p_apply[ser])->a_code == APPLY)
				addq(pkt,ser,iord == INS ? YES : NO,iord,ap->a_reason & USER);
			else
				addq(pkt,ser,iord == INS ? NO : NULL,iord,ap->a_reason & USER);
		}
	}
	fclose(BDiop);
	if (pkt->p_q)
		fatal("premature eof (co5)");
	return(0);
}


getbody(gsid,pkt)
struct	sid	*gsid;
struct packet *pkt;
{
	int	i;
	int	status;
	extern	char	Getpgm[];
	char	str[128];
	char	rarg[20];
	char	filearg[80];

	sid_ba(gsid,str);
	sprintf(rarg,"%s",str);
	sprintf(filearg,"%s",pkt->p_file);
	/*
	fork here so 'getbody' can execute 'get' to
	print out gotten body :GB:
	*/
	if ((i = fork()) < 0)
		fatal("cannot fork, try again");
	if (i = 0) {
		/*
		perform 'get' and redirect output
		to standard output
		*/
		execl(Getpgm,Getpgm,"-s","-p","-r",rarg,filearg,0);
		fatal(sprintf(Error,"cannot execute '%s'",Getpgm));
	}
	else {
		wait(&status);
		return;
	}
}


getit(str,cp)
register	char	*str, *cp;
{
	cp =+ 2;
	NONBLANK(cp);
	cp[length(cp) - 1] = '\0';
	sprintf(str,"%s",cp);
}


aux_create(iop,file,delchar)
FILE	*iop;
char	*file;
char	delchar;
{

	int	n;
	int	text;
	/*
	create auxiliary file for the named section
	*/

	text = 0;
	iop = maket(file);
	while ((n = getline(&gpkt)) != NULL && gpkt.p_line[0] != CTLCHAR) {
		text = 1;
		fputs(n,iop);
	}
	/*
	check to see that delimiter found is correct
	*/
	if (n == NULL || gpkt.p_line[0] != CTLCHAR || gpkt.p_line[1] != delchar)
		fmterr(&gpkt);
	if (!text)
		fprintf(iop,"No entries\n");
	fclose(iop);
}


idsetup(gsid,pkt,bdate)
struct	sid	*gsid;
struct	packet	*pkt;
long	*bdate;
{

	register	char	*p;
	extern	struct	time	*localtime();

	date_ba(bdate,Deltadate);

	Deltatime = &Deltadate[9];
	Deltadate[8] = 0;

	sid_ba(gsid,Sid);

	Dtime = localtime(bdate);

	if (p = Sflags[MODFLAG - 'a'])
		copy(p,Mod);
	else sprintf(Mod,"%s",sname(pkt->p_file));

	if (!(Type = Sflags[TYPEFLAG - 'a']))
		Type = "none";
}
