# include	"../hdr/defines.h"
# include	"../hdr/had.h"

SCCSID(@(#)get.c	4.4);
USXALLOC();

int	Debug	0;
struct packet gpkt;
struct sid sid;
unsigned	Ser;
int	num_files;
char	had[26];
char	*ilist, *elist, *lfile;
long	cutoff	0X7FFFFFFFL;	/* max positive long */
int verbosity;
char	Gfile[16];
char	*Type;
int	Did_id;

main(argc,argv)
int argc;
register char *argv[];
{
	register int i;
	register char *p;
	char c;
	int testmore;
	extern int Fcnt;
	extern get();

	Fflags = FTLEXIT | FTLMSG | FTLCLN;
	for(i=1; i<argc; i++)
		if(argv[i][0] == '-' && (c=argv[i][1])) {
			p = &argv[i][2];
			testmore = 0;
			switch (c) {

			case 'a':
				if (!p[0]) {
					argv[i] = 0;
					continue;
				}
				Ser = patoi(p);
				break;
			case 'r':
				if (!p[0]) {
					argv[i] = 0;
					continue;
				}
				chksid(sid_ab(p,&sid),&sid);
				break;
			case 'c':
				if (!p[0]) {
					argv[i] = 0;
					continue;
				}
				if (date_ab(p,&cutoff))
					fatal("bad date/time (cm5)");
				break;
			case 'l':
				lfile = p;
				break;
			case 'i':
				if (!p[0]) {
					argv[i] = 0;
					continue;
				}
				ilist = p;
				break;
			case 'x':
				if (!p[0]) {
					argv[i] = 0;
					continue;
				}
				elist = p;
				break;
			case 'b':
			case 'g':
			case 'e':
			case 'p':
			case 'k':
			case 'm':
			case 'n':
			case 's':
			case 't':
				testmore++;
				break;
			default:
				fatal("unknown key letter (cm1)");
			}

			if (testmore) {
				testmore = 0;
				if (*p)
					fatal(sprintf(Error,
					  "value after %c arg (cm8)",c));
			}
			if (had[c - 'a']++)
				fatal("key letter twice (cm2)");
			argv[i] = 0;
		}
		else num_files++;

	if(num_files == 0)
		fatal("missing file arg (cm3)");
	if (HADE && HADM)
		fatal("e not allowed with m (ge3)");
	if (HADE || HADI || HADX)
		HADK = 1;
	if (!HADS)
		verbosity = -1;
	setsig();
	Fflags =& ~FTLEXIT;
	Fflags =| FTLJMP;
	for (i=1; i<argc; i++)
		if (p=argv[i])
			do_file(p,get);
	exit(Fcnt ? 1 : 0);
}


get(file)
{
	register char *p;
	register unsigned ser;
	extern char had_dir, had_standinp;
	extern char *Sflags[];
	struct stats stats;
	char	str[32];

	if (setjmp(Fjmp))
		return;
	sinit(&gpkt,file,1);
	gpkt.p_ixuser = (HADI | HADX);
	gpkt.p_reqsid.s_rel = sid.s_rel;
	gpkt.p_reqsid.s_lev = sid.s_lev;
	gpkt.p_reqsid.s_br = sid.s_br;
	gpkt.p_reqsid.s_seq = sid.s_seq;
	gpkt.p_verbose = verbosity;
	gpkt.p_stdout = (HADP ? stderr : stdout);
	gpkt.p_cutoff = cutoff;
	gpkt.p_lfile = lfile;
	copy(auxf(gpkt.p_file,'g'),Gfile);

	if (gpkt.p_verbose && (num_files > 1 || had_dir || had_standinp))
		fprintf(gpkt.p_stdout,"\n%s:\n",gpkt.p_file);
	if (dodelt(&gpkt,&stats,0,0) == 0)
		fmterr(&gpkt);
	finduser(&gpkt);
	doflags(&gpkt);
	if (!HADA)
		ser = getser(&gpkt);
	else {
		if ((ser = Ser) > maxser(&gpkt))
			fatal("serial number too large (ge19)");
		move(&gpkt.p_idel[ser].i_sid, &gpkt.p_gotsid, sizeof(sid));
		if (HADR && sid.s_rel != gpkt.p_gotsid.s_rel) {
			zero(&gpkt.p_reqsid, sizeof(gpkt.p_reqsid));
			gpkt.p_reqsid.s_rel = sid.s_rel;
		}
		else
			move(&gpkt.p_gotsid, &gpkt.p_reqsid, sizeof(sid));
	}
	doie(&gpkt,ilist,elist,0);
	setup(&gpkt,ser);
	if (!(Type = Sflags[TYPEFLAG - 'a']))
		Type = Null;
	if (!(HADP || HADG) && writable(Gfile))
		fatal(sprintf(Error,"writable `%s' exists (ge4)",Gfile));
	if (gpkt.p_verbose) {
		sid_ba(&gpkt.p_gotsid,str);
		fprintf(gpkt.p_stdout,"%s\n",str);
	}
	if (HADE) {
		if (!HADR)
			move(&gpkt.p_gotsid,&gpkt.p_reqsid,
			     sizeof(gpkt.p_reqsid));
		newsid(&gpkt,Sflags[BRCHFLAG - 'a'] && HADB);
		permiss(&gpkt);
		wrtpfile(&gpkt,ilist,elist);
	}
	setuid(getuid());
	if (HADL)
		gen_lfile(&gpkt);
	if (HADG) {
		fclose(gpkt.p_iop);
		xfreeall();
		return;
	}
	flushto(&gpkt,EUSERTXT,1);
	idsetup(&gpkt);
	gpkt.p_chkeof = 1;
	Did_id = 0;
	while(readmod(&gpkt)) {
		if (gpkt.p_gout == 0) {
			if (HADP)
				gpkt.p_gout = stdout;
			else
				gpkt.p_gout = xfcreat(Gfile,HADK ? 0666 : 0444);
		}
		prfx(&gpkt);
		p = idsubst(&gpkt,gpkt.p_line);
		fputs(p,gpkt.p_gout);
	}
	fflush(gpkt.p_gout);
	if (gpkt.p_gout && gpkt.p_gout != stdout)
		fclose(gpkt.p_gout);
	if (gpkt.p_verbose)
		fprintf(gpkt.p_stdout,"%u lines\n",gpkt.p_glnno);
	if (!Did_id && !HADK)
		if (Sflags[IDFLAG - 'a'])
			fatal("no id keywords (cm6)");
		else if (gpkt.p_verbose)
			fprintf(stderr,"No id keywords (cm7)\n");
	xfreeall();
}

writable(fn)
char *fn;
{
	struct stat s;

	return (stat(fn, &s) >= 0 && (s.st_mode & 0222) != 0);
}


newsid(pkt,branch)
register struct packet *pkt;
int branch;
{
	int chkbr;

	chkbr = 0;
	if (pkt->p_reqsid.s_br == 0) {
		pkt->p_reqsid.s_lev =+ 1;
		if (sidtoser(&pkt->p_reqsid,pkt) ||
			pkt->p_maxr > pkt->p_reqsid.s_rel || branch) {
				pkt->p_reqsid.s_rel = pkt->p_gotsid.s_rel;
				pkt->p_reqsid.s_lev = pkt->p_gotsid.s_lev;
				pkt->p_reqsid.s_br = pkt->p_gotsid.s_br + 1;
				pkt->p_reqsid.s_seq = 1;
				chkbr++;
		}
	}
	else if (pkt->p_reqsid.s_seq == 0 && !branch)
		pkt->p_reqsid.s_seq = pkt->p_gotsid.s_seq + 1;
	else {
		pkt->p_reqsid.s_seq =+ 1;
		if (branch || sidtoser(&pkt->p_reqsid,pkt)) {
			pkt->p_reqsid.s_br =+ 1;
			pkt->p_reqsid.s_seq = 1;
			chkbr++;
		}
	}
	if (chkbr)
		while (sidtoser(&pkt->p_reqsid,pkt))
			pkt->p_reqsid.s_br =+ 1;
	if (sidtoser(&pkt->p_reqsid,pkt))
		fatal("internal error in newsid()");
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
	if (pkt->p_verbose)
		fprintf(pkt->p_stdout,"%s\n",str);
	ap = &pkt->p_apply[n];
	switch(ap->a_code) {

	case EMPTY:
		if (ch == INCLUDE)
			condset(ap,APPLY,INCLUSER);
		else
			condset(ap,NOAPPLY,EXCLUSER);
		break;
	case APPLY:
		sid_ba(sidp,str);
		fatal(sprintf(Error,"%s already included (ge9)",str));
		break;
	case NOAPPLY:
		sid_ba(sidp,str);
		fatal(sprintf(Error,"%s already excluded (ge10)",str));
		break;
	default:
		fatal("internal error in get/enter() (ge11)");
		break;
	}
}


gen_lfile(pkt)
register struct packet *pkt;
{
	int n;
	int reason;
	char str[32];
	char line[BUFSIZ];
	struct deltab dt;
	FILE *in;
	FILE *out;

	in = xfopen(pkt->p_file,0);
	if (*pkt->p_lfile)
		out = stdout;
	else
		out = xfcreat(auxf(pkt->p_file,'l'),0444);
	fgets(line,sizeof(line),in);
	while (fgets(line,sizeof(line),in) != NULL && line[0] == CTLCHAR && line[1] == STATS) {
		fgets(line,sizeof(line),in);
		del_ab(line,&dt);
		if (dt.d_type == 'D') {
			reason = pkt->p_apply[dt.d_serial].a_reason;
			if (pkt->p_apply[dt.d_serial].a_code == APPLY) {
				putc(' ',out);
				putc(' ',out);
			}
			else {
				putc('*',out);
				if (reason & IGNR)
					putc(' ',out);
				else
					putc('*',out);
			}
			switch (reason & (INCL | EXCL | CUTOFF)) {
	
			case INCL:
				putc('I',out);
				break;
			case EXCL:
				putc('X',out);
				break;
			case CUTOFF:
				putc('C',out);
				break;
			default:
				putc(' ',out);
				break;
			}
			putc(' ',out);
			sid_ba(&dt.d_sid,str);
			fprintf(out,"%s\t",str);
			date_ba(&dt.d_datetime,str);
			fprintf(out,"%s %s\n",str,dt.d_pgmr);
		}
		while ((n = fgets(line,sizeof(line),in)) != NULL)
			if (line[0] != CTLCHAR)
				break;
			else {
				switch (line[1]) {

				case EDELTAB:
					break;
				default:
					continue;
				case MRNUM:
				case COMMENTS:
					if (dt.d_type == 'D')
						fprintf(out,"\t%s",&line[3]);
					continue;
				}
				break;
			}
		if (n == NULL || line[0] != CTLCHAR)
			break;
		putc('\n',out);
	}
	fclose(in);
	if (out != stdout)
		fclose(out);
}


char	Curdate[18];
char	*Curtime;
char	Gdate[9];
char	Chgdate[18];
char	*Chgtime;
char	Gchgdate[9];
char	Sid[32];
char	Mod[16];
char	Olddir[BUFSIZ];
char	Pname[BUFSIZ];
char	Dir[BUFSIZ];

idsetup(pkt)
register struct packet *pkt;
{
	extern long Timenow;
	register int n;
	register char *p;

	date_ba(&Timenow,Curdate);
	Curtime = &Curdate[9];
	Curdate[8] = 0;
	copy(pkt->p_file,Dir);
	dname(Dir);
	if(curdir(Olddir) != 0)
		fatal("curdir failed (ge20)");
	if(chdir(Dir) != 0)
		fatal("cannot change directory (ge22)");
	if(curdir(Pname) != 0)
		fatal("curdir failed (ge21)");
	if(chdir(Olddir) != 0)
		fatal("cannot change directory (ge23)");
	makgdate(Curdate,Gdate);
	for (n = maxser(pkt); n; n--)
		if (pkt->p_apply[n].a_code == APPLY)
			break;
	if (n)
		date_ba(&pkt->p_idel[n].i_datetime,Chgdate);
	Chgtime = &Chgdate[9];
	Chgdate[8] = 0;
	makgdate(Chgdate,Gchgdate);
	sid_ba(&pkt->p_gotsid,Sid);
	if (p = Sflags[MODFLAG - 'a'])
		copy(p,Mod);
	else
		copy(Gfile,Mod);
}


makgdate(old,new)
register char *old, *new;
{
	if ((*new = old[3]) != '0')
		new++;
	*new++ = old[4];
	*new++ = '/';
	if ((*new = old[6]) != '0')
		new++;
	*new++ = old[7];
	*new++ = '/';
	*new++ = old[0];
	*new++ = old[1];
	*new = 0;
}


static char Zkeywd[5]	"@(#)";

idsubst(pkt,line)
register struct packet *pkt;
char line[];
{
	static char tline[BUFSIZ];
	static char str[32];
	register char *lp, *tp;
	extern char *Type;
	extern char *Sflags[];

	if (HADK || !any('%',line))
		return(line);

	tp = tline;
	for(lp=line; *lp != 0; lp++) {
		if(lp[0] == '%' && lp[1] != 0 && lp[2] == '%') {
			switch(*++lp) {

			case 'M':
				tp = trans(tp,Mod);
				break;
			case 'R':
				sprintf(str,"%u",pkt->p_gotsid.s_rel);
				tp = trans(tp,str);
				break;
			case 'L':
				sprintf(str,"%u",pkt->p_gotsid.s_lev);
				tp = trans(tp,str);
				break;
			case 'B':
				sprintf(str,"%u",pkt->p_gotsid.s_br);
				tp = trans(tp,str);
				break;
			case 'S':
				sprintf(str,"%u",pkt->p_gotsid.s_seq);
				tp = trans(tp,str);
				break;
			case 'D':
				tp = trans(tp,Curdate);
				break;
			case 'H':
				tp = trans(tp,Gdate);
				break;
			case 'T':
				tp = trans(tp,Curtime);
				break;
			case 'E':
				tp = trans(tp,Chgdate);
				break;
			case 'G':
				tp = trans(tp,Gchgdate);
				break;
			case 'U':
				tp = trans(tp,Chgtime);
				break;
			case 'Z':
				tp = trans(tp,Zkeywd);
				break;
			case 'Y':
				tp = trans(tp,Type);
				break;
			case 'W':
				tp = trans(tp,Zkeywd);
				tp = trans(tp,Mod);
				*tp++ = '\t';
			case 'I':
				tp = trans(tp,Sid);
				break;
			case 'P':
				tp = trans(tp,Pname);
				*tp++ = '/';
				tp = trans(tp,(sname(pkt->p_file)));
				break;
			case 'F':
				tp = trans(tp,pkt->p_file);
				break;
			case 'C':
				sprintf(str,"%u",pkt->p_glnno);
				tp = trans(tp,str);
				break;
			case 'A':
				tp = trans(tp,Zkeywd);
				tp = trans(tp,Type);
				*tp++ = ' ';
				tp = trans(tp,Mod);
				*tp++ = ' ';
				tp = trans(tp,Sid);
				tp = trans(tp,Zkeywd);
				break;
			default:
				*tp++ = '%';
				*tp++ = *lp;
				continue;
			}
			lp++;
		}
		else
			*tp++ = *lp;
	}

	*tp = 0;
	return(tline);
}


trans(tp,str)
register char *tp, *str;
{
	Did_id = 1;
	while(*tp++ = *str++)
		;
	return(tp-1);
}


prfx(pkt)
register struct packet *pkt;
{
	char str[32];

	if (HADN)
		fprintf(pkt->p_gout,"%s\t",Mod);
	if (HADM) {
		sid_ba(&pkt->p_inssid,str);
		fprintf(pkt->p_gout,"%s\t",str);
	}
}


clean_up(n)
{
	if (gpkt.p_file[0])
		unlockit(auxf(gpkt.p_file,'z'),getpid());
	if (gpkt.p_iop)
		fclose(gpkt.p_iop);
	xfreeall();
}


wrtpfile(pkt,inc,exc)
register struct packet *pkt;
char *inc, *exc;
{
	char line[64], str1[32], str2[32];
	char *user;
	FILE *in, *out;
	struct pfile pf;
	register char *p;
	int fd;
	int i;
	extern long Timenow;

	user = logname();
	if (lockit(auxf(pkt->p_file,'z'),2,getpid()))
		fatal("cannot create lock file (cm4)");
	if (exists(p = auxf(pkt->p_file,'p'))) {
		fd = xopen(p,2);
		in = fdfopen(fd,0);
		while (fgets(line,sizeof(line),in) != NULL) {
			p = line;
			p[length(p) - 1] = 0;
			pf_ab(p,&pf,0);
			if ((pf.pf_gsid.s_rel == pkt->p_gotsid.s_rel &&
				pf.pf_gsid.s_lev == pkt->p_gotsid.s_lev &&
				pf.pf_gsid.s_br == pkt->p_gotsid.s_br &&
				pf.pf_gsid.s_seq == pkt->p_gotsid.s_seq) ||
				(pf.pf_nsid.s_rel == pkt->p_reqsid.s_rel &&
				pf.pf_nsid.s_lev == pkt->p_reqsid.s_lev &&
				pf.pf_nsid.s_br == pkt->p_reqsid.s_br &&
				pf.pf_nsid.s_seq == pkt->p_reqsid.s_seq)) {
				fclose(in);
				fatal(sprintf(Error,"being edited: `%s' (ge17)",
					line));
				}
			if (!equal(pf.pf_user,user))
				fprintf(stderr,"WARNING: being edited: `%s' (ge18)\n",line);
		}
		out = fdfopen(dup(fd),1);
		fclose(in);
	}
	else
		out = xfcreat(p,0666);
	fseek(out,0L,2);
	sid_ba(&pkt->p_gotsid,str1);
	sid_ba(&pkt->p_reqsid,str2);
	date_ba(&Timenow,line);
	fprintf(out,"%s %s %s %s",str1,str2,user,line);
	if (inc)
		fprintf(out," -i%s",inc);
	if (exc)
		fprintf(out," -x%s",exc);
	fprintf(out,"\n");
	fclose(out);
	if (pkt->p_verbose)
		fprintf(pkt->p_stdout,"new delta %s\n",str2);
	unlockit(auxf(pkt->p_file,'z'),getpid());
}


getser(pkt)
register struct packet *pkt;
{
	register struct idel *rdp;
	int n, ser, def;
	char *p;
	extern char *Sflags[];

	def = 0;
	if (pkt->p_reqsid.s_rel == 0) {
		if (p = Sflags[DEFTFLAG - 'a'])
			chksid(sid_ab(p, &pkt->p_reqsid), &pkt->p_reqsid);
		else {
			pkt->p_reqsid.s_rel = MAX;
			def = 1;
		}
	}
	ser = 0;
	if (pkt->p_reqsid.s_lev == 0) {
		for (n = maxser(pkt); n; n--) {
			rdp = &pkt->p_idel[n];
			if ((rdp->i_sid.s_br == 0 || HADT) &&
				pkt->p_reqsid.s_rel >= rdp->i_sid.s_rel &&
				rdp->i_sid.s_rel > pkt->p_gotsid.s_rel) {
					ser = n;
					pkt->p_gotsid.s_rel = rdp->i_sid.s_rel;
			}
		}
	}
	else if (pkt->p_reqsid.s_br && pkt->p_reqsid.s_seq == 0) {
		for (n = maxser(pkt); n; n--) {
			rdp = &pkt->p_idel[n];
			if (rdp->i_sid.s_rel == pkt->p_reqsid.s_rel &&
				rdp->i_sid.s_lev == pkt->p_reqsid.s_lev &&
				rdp->i_sid.s_br == pkt->p_reqsid.s_br)
					break;
		}
		ser = n;
	}
	else {
		ser = sidtoser(&pkt->p_reqsid,pkt);
	}
	if (ser == 0)
		fatal("nonexistent sid (ge5)");
	rdp = &pkt->p_idel[ser];
	move(&rdp->i_sid, &pkt->p_gotsid, sizeof(pkt->p_gotsid));
	if (def || (pkt->p_reqsid.s_lev == 0 && pkt->p_reqsid.s_rel == pkt->p_gotsid.s_rel))
		move(&pkt->p_gotsid, &pkt->p_reqsid, sizeof(pkt->p_gotsid));
	return(ser);
}


/* Null routine to satisfy external reference from dodelt() */

escdodelt()
{
}
