#
/*
	Program to convert release 3 (or release 2 or even 1) SCCS files
	to release 4 SCCS files.
	Usage:
		scv arg ...
	arg is any argument acceptable as an SCCS file
	name argument to the get command. E.g.:
		scv mysccsdir
	will convert every release 3 (or 2 or 1 but NOT 4) SCCS file in the
	directory "mysccsdir".
*/
# include	"../hdr/defines.h"
# include	"dir.h"

SCCSID(@(#)scv.c	4.4);


/*
	Release 3 SCCS File Structures  (2.1  78/06/05 17:31:17)
	See osccsfile(V).
*/

struct Header {
	short  Hmagicno;
	char Htype[10];
	char Hpers[14];
	char Hdesc[100];
	short  Hfloor;
	short  Hceil;
	short  Hsw[5];
	short  Hrdef;
	char Hulist[32];
	char Hexpand[50];
	short  Hash;
};
#define MAGICNO (7)
#define HASHADDR (226)


struct Reltab {
	short  Rrel;
	short  Rlevs;
};


struct Deltab {
	short  Drel;
	short  Dlev;
	char Dtype;	/*'D': delta,'P','U': non-prop,'I': incl,'E': excl */
	char Dfill;	/* Used to be option letter */
/* compiler once forced unfortunate alignment here.
/* also, fp-11c high/low long goof strikes again.
/*	long  Ddatetime;
*/
	short Ddthi,Ddtlo;
	char Dpgmr[SZLNAM];
	char Dhist[200];
};


struct Control {
	short  Crel;
	short  Clev;
	char Cctl;	/* -11: ins, -12: del, -13: end */
};
#define SIZEOFCONTROL (5)
#define OINS (-11)
#define ODEL (-12)
#define OEND (-13)


struct Line {
	char Lline [256];
};


/*
	Structure for use with buffered I/O routines opnl, opnr, 
	getl and getr.
*/
struct Ibufr {
	short  Ifildes;
	char *Irecptr;
	char *Iend;
	char Ibuff1[256];
	char Ibuff2[512];
	char Ibuff3[2];
	short  Ilen;
	short  Ihflag;
	short  Ihcnt;
	short  Ihtot;
};


/*
	Structure for use with buffered I/O routines crtr, crtl, putl,
	putr, flshr and buflsh.
*/
struct Obufr {
	short  Ofildes;
	char *Orecptr;
	char *Oend;
	char Obuff1[512];
	short  Ohflag;
	short  Ohcnt;
};


/*
 * structure to access an
 * shorteger in bytes
 */
struct
{
	char	lobyte;
	char	hibyte;
};


/*
 * structure to access an shorteger
 */
struct
{
	short	shorteg;
};


/*
 * structure to access a long as shortegers
 */
struct {
	short	hiword;
	short	loword;
};


/*
	Structure for referencing pieces of localtime().
*/
struct Time {
	short	Tseconds;
	short	Tminutes;
	short	Thours;
	short	Tday_month;
	short	Tmonth;
	short	Tyear;
	short	Tday_week;
	short	Tday_year;
	short	Tflag;
};
/*
	SCCS Internal Structures (used by get and delta).     (2.1)
*/

struct Apply {
	short    Adt;		/* pseudo date-time */
	short    Acode;		/* APPLY, NOAPPLY or EMPTY */
};
#define APPLY	  (1)
#define NOAPPLY  (-1)
#define EMPTY	  (0)


struct Queue {
	struct Queue *Qnext;
	short    Qrel;		/* release */
	short    Qlev;		/* level */
	short    Qdt;		/* pseudo date-time */
	short    Qkeep;		/* keep switch setting */
};
#define YES	 (1)
#define NO	(-1)

#define SIZEOFPfile (50)


struct Packet {
	char	Pfile[SIZEOFPfile];	/* file name containing module */
/*
			Note: the order of the next two words
				can not___ be changed!
			This is because the release and level together
			are treated as a long.
*/
	short	Prel;		/* specified release (-1 = not spec.) */
	short	Plev;		/* specified level (-1 = not spec.)*/
	char	Pverbose;	/* verbose flags (see #define's below) */
	char	Pupd;		/* update flag (!0 = update mode) */
	long    Pcutoff;	/* specified cutoff date-time */
	struct	Header	Phdr;	/* header from module */
	short	Plnno;		/* line number of current line */
	short	Precno;		/* record number of current rec */
	char	Pwrttn;		/* written flag (!0 = written) */
	char	Pkeep;		/* keep switch for readmod() */
	struct	Apply **Papply;	/* ptr to apply array */
	struct	Queue *Pq;	/* ptr to control queue */
	struct	Ibufr Pibuf;	/* input buffer */
	long	Pcdt;		/* date/time of newest applied delta */
	char	*Plfile;	/* 0 = no l-file; else ptr to l arg */
	char	Punack;		/* !0 if unacknowledged non-prop deltas */
	char	Pnoprop;	/* !0 if new delta is to be non-prop */
	short	Pirel;		/* rel which inserted current rec */
	short	Pilev;		/* lev which inserted current rec */
};
/*
	Masks for Pverbose
*/

# define RLACCESS	(1)
# define NLINES		(2)
# define DOLIST		(4)
# define UNACK		(8)
# define NEWRL		(16)
# define WARNING	(32)

/*
	size of login name
*/


USXALLOC();

main(argc,argv)
char **argv;
{
	register short i;
	register char *p;
	extern conv();
	extern short Fcnt;

	setsig();
	Fflags = FTLMSG | FTLCLN | FTLJMP;
	for (i = 1; i < argc; i++)
		if (p = argv[i])
			odo_file(p,conv);
	exit(Fcnt ? 1 : 0);
}


struct packet npkt;

conv(ofile)
char *ofile;
{
	struct Packet opkt;
	struct deltab *dt;
	char **hists;
	short **rlp;
	char statstr[32];
	short ndels;
	char *line;
	short n;
	char *p;

	if (setjmp(Fjmp))
		return;
	printf("%s:\n",ofile);
	ckpfile(auxf(ofile,'p'));
	zero(&opkt,sizeof(opkt));
	opnr(&opkt.Pibuf,ofile);
	dohead(&opkt);
	rlp = 0;
	ndels = doreltab(&opkt,&rlp);
	hists = alloc((ndels + 1) * sizeof(*hists));
	dt = alloc((ndels + 1) * sizeof(*dt));
	dodelt(&opkt,dt,hists,ndels);
	fixup(dt,ndels,rlp);
	sinit(&npkt,ofile,0);
	npkt.p_upd = 1;
	line = npkt.p_line;
	putline(&npkt,sprintf(line,"%c%c00000\n",CTLCHAR,HEAD),0);
	statstr[0] = 0;
	for (n = ndels; n; n--) {
		if (!statstr[0])
			newstats(&npkt,statstr,"?");
		else
			putline(&npkt,statstr);
		putline(&npkt,del_ba(&dt[n],line));
		putline(&npkt,sprintf(line,"%c%c %s\n",CTLCHAR,COMMENTS,
			hists[n]));
		putline(&npkt,sprintf(line,CTLSTR,CTLCHAR,EDELTAB));
	}
	putline(&npkt,sprintf(line,CTLSTR,CTLCHAR,BUSERNAM));
	dousers(opkt.Phdr.Hulist,&npkt);
	putline(&npkt,sprintf(line,CTLSTR,CTLCHAR,EUSERNAM));
	if (*(p = opkt.Phdr.Htype))
		putline(&npkt,sprintf(line,"%c%c %c %s\n",CTLCHAR,FLAG,
			TYPEFLAG,p));
	if (n = opkt.Phdr.Hfloor)
		putline(&npkt,sprintf(line,"%c%c %c %d\n",CTLCHAR,FLAG,
			FLORFLAG,n));
	if (n = opkt.Phdr.Hceil)
		putline(&npkt,sprintf(line,"%c%c %c %d\n",CTLCHAR,FLAG,
			CEILFLAG,n));
	if (n = opkt.Phdr.Hrdef)
		putline(&npkt,sprintf(line,"%c%c %c %d\n",CTLCHAR,FLAG,
			DEFTFLAG,n));
	putline(&npkt,sprintf(line,CTLSTR,CTLCHAR,BUSERTXT));
	if (*(p = opkt.Phdr.Hpers))
		putline(&npkt,sprintf(line,"%s\n",p));
	if (*(p = opkt.Phdr.Hdesc))
		putline(&npkt,sprintf(line,"%s\n",p));
	putline(&npkt,sprintf(line,CTLSTR,CTLCHAR,EUSERTXT));
	dobod(&opkt,&npkt,rlp,line);
	convflush(&npkt);
	close(opkt.Pibuf.Ifildes);
	for (n = ndels; n; n--)
		free(hists[n]);
	free(hists);
	free(dt);
/* [compiler bug, ignore this for now ]
	if (rlp) {
		for (n = (short) (*rlp); n; n--)
			if (rlp[n])
				free(rlp[n]);
		free(rlp);
	}
*/
	rename(auxf(npkt.p_file,'x'),npkt.p_file);
	xrm(&npkt);
}


getline()
{
}


clean_up()
{
	xrm(&npkt);
}



fixup(dt,ndels,rlp)
struct deltab *dt;
short ndels;
short **rlp;
{
	short m, n;
	short maxr;
	short seqcnt;
	short pred;
	register struct deltab *p1, *p2;
	register short *brp;

	for (m = ndels; m; m--) {
		p1 = &dt[m];
		if (p1->d_sid.s_lev > 1) {
			for (n = m - 1; n; n--) {
				if (p1->d_sid.s_rel == dt[n].d_sid.s_rel)
					break;
			}
			pred = n;
		}
		else {
			maxr = pred = 0;
			for (n = m - 1; n; n--) {
				p2 = &dt[n];
				if (p1->d_sid.s_rel > p2->d_sid.s_rel &&
					p2->d_type == 'D' &&
					p2->d_sid.s_rel > maxr) {
						maxr = p2->d_sid.s_rel;
						pred = n;
				}
			}
		}
		p1->d_pred = pred;
		rlp[p1->d_sid.s_rel][p1->d_sid.s_lev] = m;
	}
	brp = alloca(n = (ndels + 1) * sizeof(*brp));
	zero(brp,n);
	for (m = 1; m <= ndels; m++) {
		p1 = &dt[m];
		if (p1->d_type != 'D') {
			seqcnt = 0;
			p2 = &dt[p1->d_pred];
			p1->d_type = 'D';
			p1->d_sid.s_rel = p2->d_sid.s_rel;
			p1->d_sid.s_lev = p2->d_sid.s_lev;
			p1->d_sid.s_br = ++brp[p1->d_pred];
			p1->d_sid.s_seq = ++seqcnt;
			pred = m;
			for (n = m + 1; n <= ndels; n++) {
				if (dt[n].d_pred == pred) {
					p2 = &dt[n];
					p2->d_type = 'D';
					p2->d_sid.s_rel = p1->d_sid.s_rel;
					p2->d_sid.s_lev = p1->d_sid.s_lev;
					p2->d_sid.s_br = p1->d_sid.s_br;
					p2->d_sid.s_seq = ++seqcnt;
					pred = n;
				}
			}
		}
	}
}



struct	names {
	struct	names	*n_next;
	char	n_name[SZLNAM];
	short	n_uid;
};

struct names *names;

dousers(up,pkt)
register char *up;
struct packet *pkt;
{
	short i, j;
	register char mask, c;
	char *p;
	char str[16];

	for (i = 0; i < 32; i++)
		if (c = *up++) {
			j = 0;
			for (mask = 1; mask; mask =<< 1) {
				if ((c & mask) && (p = getlnam(i * SZLNAM + j)))
					putline(pkt,sprintf(str,"%s\n",p));
				j++;
			}
		}
}


getlnam(uid)
short uid;
{
	char str[128];
	register struct names *cur, *prev;
	register char *p;

	for (cur = &names; cur = (prev = cur)->n_next; )
		if (cur->n_uid == uid)
			return(cur->n_name);
	if (getpw(uid,str))
		return(0);
	prev->n_next = cur = alloc(sizeof(*cur));
	cur->n_next = 0;
	cur->n_uid = uid;
	for (p = str; *p++ != ':'; )
		;
	*--p = 0;
	str[SZLNAM] = 0;
	copy(str,cur->n_name);
	return(cur->n_name);
}



/*
	Routine to process the module header. All that's necessary is
	to slide it shorto the packet.
*/

dohead(pkt)
register struct Packet *pkt;
{
	register struct Header *hdr;

	if(rdrec(pkt) == 1) fatal("premature eof (58)");
	hdr = pkt->Pibuf.Irecptr;
	if(hdr->Hmagicno != MAGICNO) fatal("not an SCCS file (53)");
	move(hdr,&pkt->Phdr,sizeof(*hdr));
}


doreltab(pkt,rlp)
register struct Packet *pkt;
register short ***rlp;
{
	short n;
	short sz;
	register struct Reltab *rt;

	n = 0;
	while (rdrec(pkt) != 1 && (rt = pkt->Pibuf.Irecptr)->Rrel) {
		if (n == 0) {
			*rlp = alloc(sz = (rt->Rrel + 1) * sizeof(**rlp));
			zero(*rlp,sz);
			**rlp = rt->Rrel;
		}
		(*rlp)[rt->Rrel] = alloc((rt->Rlevs + 1) * sizeof(***rlp));
		(*rlp)[rt->Rrel][0] = rt->Rlevs;
		n =+ rt->Rlevs;
	}
	return(n);
}


dodelt(pkt,dt,hists,ndels)
struct Packet *pkt;
register struct deltab *dt;
char **hists;
short ndels;
{
	short n;
	register struct deltab *ndt;
	register struct Deltab *odt;

	for (; rdrec(pkt) != 1 && (odt = pkt->Pibuf.Irecptr)->Drel; --ndels) {
		if (!(odt->Dtype == 'D' || odt->Dtype == 'P' || odt->Dtype == 'U')) {
			++ndels;
			continue;
		}
		if (!ndels)
			return(fatal("internal error in dodeltab"));
		ndt = &dt[ndels];
		ndt->d_type = odt->Dtype;
		move(odt->Dpgmr,ndt->d_pgmr,sizeof(ndt->d_pgmr));
		ndt->d_datetime = (odt->Ddthi<<16)+(unsigned)odt->Ddtlo;
		ndt->d_sid.s_rel = odt->Drel;
		ndt->d_sid.s_lev = odt->Dlev;
		ndt->d_sid.s_br = 0;
		ndt->d_sid.s_seq = 0;
		ndt->d_serial = ndels;
		ndt->d_pred = 0;
		n = size(odt->Dhist);
		n++;
		n =& ~1;
		if (odt->Dtype == 'P' || odt->Dtype == 'U') {
			hists[ndels] = alloc(n + 16);
			sprintf(hists[ndels],"[was %d.%d] ",odt->Drel,odt->Dlev);
		}
		else {
			hists[ndels] = alloc(n);
			hists[ndels][0] = 0;
		}
		move(odt->Dhist,strend(hists[ndels]),n);
	}
	if (ndels) {
		fatal("in dodelt");
	}
}


dobod(opkt,npkt,rlp,line)
struct Packet *opkt;
struct packet *npkt;
short **rlp;
char *line;
{
	register struct Control *octl;
	register char *p, c;

	while (rdrec(opkt) != 1 && (octl = opkt->Pibuf.Irecptr)->Crel) {
		if (octlrec(octl,opkt->Pibuf.Ilen))
			putline(npkt,sprintf(line,"%c%c %u\n",CTLCHAR,
				"EDI"[octl->Cctl-OEND],
				rlp[octl->Crel][octl->Clev]));
		else {
			c = (p = octl)[opkt->Pibuf.Ilen];
			p[opkt->Pibuf.Ilen] = 0;
			putline(npkt,sprintf(line,"%s\n",p));
			p[opkt->Pibuf.Ilen] = c;
		}
	}
}


octlrec(ctl,len)
register struct Control *ctl;
short len;
{
	register short ch;

	if (len==SIZEOFCONTROL &&
		((ch=ctl->Cctl)==OINS || ch==ODEL || ch==OEND))
			return(1);
	return(0);
}


rdrec(pkt)
register struct Packet *pkt;
{
	register n;

	if ((n = getr(&pkt->Pibuf)) != 1)
		pkt->Precno++;
	return(n);
}


xwrite(a,b,c)
{
	return(write(a,b,c));
}




# define CALL(p,func,cnt)	Ffile=p; (*func)(p); cnt++;
short	nfiles;
char	had_dir;
char	had_standinp;


odo_file(p,func)
register char *p;
short (*func)();
{
	extern char *Ffile;
	char str[FILESIZE];
	char ibuf[FILESIZE];
	FILE *iop;
	struct dir dir[2];
	register char *s;
	short fd;

	if (p[0] == '-') {
		had_standinp = 1;
		while (gets(ibuf) != NULL) {
			if (osccsfile(ibuf)) {
				CALL(ibuf,func,nfiles);
			}
		}
	}
	else if (exists(p) && (Statbuf.st_mode & S_IFMT) == S_IFDIR) {
		had_dir = 1;
		Ffile = p;
		if((iop = fopen(p,"r")) == NULL)
			return;
		dir[1].d_ino = 0;
		fread(dir,sizeof(dir[0]),1,iop);   /* skip "."  */
		fread(dir,sizeof(dir[0]),1,iop);   /* skip ".."  */
		while(fread(dir,sizeof(dir[0]),1,iop) == 1) {
			if(dir[0].d_ino == 0) continue;
			sprintf(str,"%s/%s",p,dir[0].d_name);
			if(osccsfile(str)) {
				CALL(str,func,nfiles);
			}
		}
		fclose(iop);
	}
	else {
		CALL(p,func,nfiles);
	}
}


osccsfile(file)
register char *file;
{
	register short ff, result;
	short magic[2];

	result = (ff=open(file,0)) > 0
	  && read(ff,magic,4) == 4
	  && magic[1] == MAGICNO;
	close(ff);
	return(result);
}



/*
	Routine to write out either the current line in the packet
	(if newline is zero) or the line specified by newline.
	A line is actually written (and the x-file is only
	opened) if pkt->p_upd is non-zero.  When the current line from 
	the packet is written, pkt->p_wrttn is set non-zero, and
	further attempts to write it are ignored.  When a line is
	read shorto the packet, pkt->p_wrttn must be turned off.
*/

short	Xcreate;
FILE	*Xiop;


putline(pkt,newline)
register struct packet *pkt;
char *newline;
{
	static char obf[BUFSIZ];
	char *xf;
	register char *p;

	if(pkt->p_upd == 0) return;

	if(!Xcreate) {
		stat(pkt->p_file,&Statbuf);
		xf = auxf(pkt->p_file,'x');
		Xiop = xfcreat(xf,Statbuf.st_mode);
		setbuf(Xiop,obf);
		chown(xf,(Statbuf.st_gid<<8)|Statbuf.st_uid);
	}
	if (newline)
		p = newline;
	else {
		if(!pkt->p_wrttn++)
			p = pkt->p_line;
		else
			p = 0;
	}
	if (p) {
		fputs(p,Xiop);
		if (Xcreate)
			while (*p)
				pkt->p_nhash =+ *p++;
	}
	Xcreate = 1;
}


convflush(pkt)
register struct packet *pkt;
{
	register char *p;
	char hash[6];

	if (pkt->p_upd == 0)
		return;
	putline(pkt,0);
	rewind(Xiop);
	sprintf(hash,"%5u",pkt->p_nhash&0xFFFF);
	zeropad(hash);
	fprintf(Xiop,"%c%c%s\n",CTLCHAR,HEAD,hash);
	fclose(Xiop);
}


xrm(pkt)
struct packet *pkt;
{
	if (Xiop)
		fclose(Xiop);
	if(Xcreate)
		unlink(auxf(pkt,'x'));
	Xiop = Xcreate = 0;
}


char bpf[] "bad p-file (216)";

rdpfile(f,rp,un)
char f[], un[];
short *rp;
{
	register short fd, i;
	register char *p;
	char s[65], *name;

	fd = xopen(f,0);
	if ((i=read(fd,s,64))<=0)
		fatal(bpf);
	close(fd);
	p = s;
	p[i] = 0;
	for (; *p != ' '; p++)
		if (*p == 0)
			fatal(bpf);
	*p = 0;
	if ((*rp=patoi(s)) == -1)
		fatal(bpf);
	++p;
	while (*p++ == ' ') ;
	name = --p;
	for (; *p != '\n'; p++)
		if (*p == 0)
			fatal(bpf);
	*p = 0;
	if ((p-name)>SZLNAM)
		fatal(bpf);
	copy(name,un);
}


ckpfile(file)
register char *file;
{
	short r;
	char un[SZLNAM];

	if(exists(file)) {
		rdpfile(file,&r,un);
		fatal(sprintf(Error,"being edited at release %d by `%s' (scv1)",
		  r,un));
	}
}


/*
	Bottom level read routines for release 3 SCCS files.

	Usage:
		struct Ibufr ib;
		...
		opnr(&ib,"filename");
		...
		if(getr(&ib) == 1) [end-of-file];
		[ib.Irecptr is addr of record (always on word boundary)]
		[ib.Ilen is length]

	Address HASHADDR of the file must contain a 1-word stored hash count.
	If this count is non-zero, then on end-of-file a computed hash count
	is compared with it and a fatal error is issued if they aren't equal.
*/

opnr(buf,file)
register struct Ibufr *buf;
char file[];
{
	buf->Ifildes = xopen(file,0);
	buf->Irecptr = buf->Ibuff2 + 2;
	buf->Iend = buf->Irecptr + 510;
	buf->Ilen = 510;
	buf->Ibuff3[1] = -128;
	buf->Ihcnt = buf->Ihtot = buf->Ihflag = 0;
}


getr(buf)
register struct Ibufr *buf;
{
	register char *p, *q;
	short *w;
	short i, n;

	buf->Irecptr =+ buf->Ilen + !(buf->Ilen & 1);

	i = 0;
	while(1) {
		buf->Ilen = 0;
		buf->Ilen = *buf->Irecptr + 128;

		if(buf->Irecptr <= buf->Iend - (buf->Ilen+!(buf->Ilen&1)))
			return(++buf->Irecptr);

		if(i++ == 1) return(1);

		q = buf->Irecptr;
		p = buf->Irecptr =- 512;

		while(q <= buf->Iend) *p++ = *q++;

		if((n = read(buf->Ifildes,buf->Ibuff2,512)) <= 0)
			return(1);

		buf->Iend = buf->Ibuff2 + n - 1;
		*(buf->Iend + 1) = -128;

		w = buf->Ibuff2;
		if(buf->Ihflag == 0) {
			buf->Ihflag = 1;
			buf->Ihtot = w[HASHADDR>>1];
			w[HASHADDR>>1] = 0;
		}
		if(n < 512) buf->Ibuff2[n] = 0;

		buf->Ihcnt =+ sumr(w,&w[(n&1?n-1:n-2)>>1]);

		if(n<512 && buf->Ihtot && buf->Ihcnt != buf->Ihtot)
			fatal("corrupted file (201)");
	}
}


sumr(from,to)
register short *from, *to;
{
	register short sum;

	for (sum=0; from<=to; )
		sum =+ *from++;
	return(sum);
}
