#
/*
 *  link editor
 */

#define	SIGINT	2
#define	ARCMAGIC 0177555
#define	FMAGIC	0407
#define	NMAGIC	0410
#define	IMAGIC	0411

#define	EXTERN	040
#define	UNDEF	00
#define	ABS	01
#define	TEXT	02
#define	DATA	03
#define	BSS	04
#define	COMM	05	/* internal use only */

#define	RABS	00
#define	RTEXT	02
#define	RDATA	04
#define	RBSS	06
#define	REXT	010

#define	RELFLG	01
#define	NROUT	256
#define	NSYM	501
#define	NSYMPR	500

#define	RONLY	0400

char	premeof[] "Premature EOF on %s";

struct	page {
	int	nuser;
	int	bno;
	int	nibuf;
	int	buff[256];
} page[2];

struct	{
	int	nuser;
	int	bno;
} fpage;

struct	stream {
	int	*ptr;
	int	bno;
	int	nibuf;
	int	size;
	struct	page *pno;
};

struct	stream text;
struct	stream reloc;

struct	archdr {
	char	aname[8];
	int	atime[2];
	char	auid, amode;
	int	asize;
} archdr;

struct	filhdr {
	int	fmagic;
	int	tsize;
	int	dsize;
	int	bsize;
	int	ssize;
	int	entry;
	int	pad;
	int	relflg;
} filhdr;

struct	liblist {
	int	off;
	int	bno;
};

struct	liblist	liblist[NROUT];
struct	liblist	*libp { &liblist[0] };

struct	symbol {
	char	sname[8];
	char	stype;
	char	spad;
	int	svalue;
};

struct	symbol	cursym;
struct	symbol	symtab[NSYM];
struct	symbol	*hshtab[NSYM+2];
struct	symbol	*symp { symtab };
struct	symbol	**local[NSYMPR];
struct	symbol	*p_etext;
struct	symbol	*p_edata;
struct	symbol	*p_end;

int	xflag;		/* discard local symbols */
int	Xflag;		/* discard locals starting with 'L' */
int	rflag;		/* preserve relocation bits, don't define common */
int	arflag;		/* original copy of rflag */
int	sflag;		/* discard all symbols */
int	nflag;		/* pure procedure */
int	dflag;		/* define common even with rflag */
int	iflag;		/* I/D space separated */

int	infil;
char	*filname;

int	tsize;
int	dsize;
int	bsize;
int	ssize;
int	nsym;

int	torigin;
int	dorigin;
int	borigin;

int	ctrel;
int	cdrel;
int	cbrel;

int	errlev;
int	delarg	4;
char	tfname[]	"/tmp/lxyyyyy";
int	toutb[259];
int	doutb[259];
int	troutb[259];
int	droutb[259];
int	soutb[259];

struct	symbol	**lookup();
struct	symbol	**slookup();

main(argc, argv)
char **argv;
{
	extern int delexit();
	register c;
	register char *ap, **p;
	struct symbol **hp;

	if ((signal(SIGINT, 1) & 01) == 0)
		signal(SIGINT, delexit);
	if (argc == 1)
		exit(4);
	p = argv + 1;
	for (c = 1; c<argc; c++) {
		filname = 0;
		ap = *p++;
		if (*ap == '-') switch (ap[1]) {

		case 'u':
			if (++c >= argc)
				error(1, "Bad 'use'");
			if (*(hp = slookup(*p++)) == 0) {
				*hp = symp;
				enter();
			}
			continue;

		case 'l':
			break;

		case 'x':
			xflag++;
			continue;

		case 'X':
			Xflag++;
			continue;

		case 'r':
			rflag++;
			arflag++;
			continue;

		case 's':
			sflag++;
			xflag++;
			continue;

		case 'n':
			nflag++;
			continue;

		case 'd':
			dflag++;
			continue;

		case 'i':
			iflag++;
			continue;
		}
		load1arg(ap);
		close(infil);
	}
	middle();
	setupout();
	p = argv+1;
	libp = liblist;
	for (c=1; c<argc; c++) {
		ap = *p++;
		if (*ap == '-') switch (ap[1]) {

		case 'u':
			++c;
			++p;
		default:
			continue;

		case 'l':
			break;
		}
		load2arg(ap);
		close(infil);
	}
	finishout();
}

load1arg(acp)
char *acp;
{
	register char *cp;
	register noff, nbno;

	cp = acp;
	if (getfile(cp)==0) {
		load1(0, 0, 0);
		return;
	}
	nbno = 0;
	noff = 1;
	for (;;) {
		dseek(&text, nbno, noff, sizeof archdr);
		if (text.size <= 0) {
			libp->bno = -1;
			libp++;
			return;
		}
		mget(&archdr, sizeof archdr);
		if (load1(1, nbno, noff + (sizeof archdr) / 2)) {
			libp->bno = nbno;
			libp->off = noff;
			libp++;
		}
		noff =+ (archdr.asize + sizeof archdr)>>1;
		nbno =+ (noff >> 8) & 0377;
		noff =& 0377;
	}
}

load1(libflg, bno, off)
{
	register struct symbol *sp, **hp, ***cp;
	struct symbol *ssymp;
	int ndef, nloc;

	readhdr(bno, off);
	ctrel = tsize;
	cdrel =+ dsize;
	cbrel =+ bsize;
	ndef = 0;
	nloc = sizeof cursym;
	cp = local;
	ssymp = symp;
	if ((filhdr.relflg&RELFLG)==1) {
		error(0, "No relocation bits");
		return(0);
	}
	off =+ (sizeof filhdr)/2 + filhdr.tsize + filhdr.dsize;
	dseek(&text, bno, off, filhdr.ssize);
	while (text.size > 0) {
		mget(&cursym, sizeof cursym);
		if ((cursym.stype&EXTERN)==0) {
			if (Xflag==0 || cursym.sname[0]!='L')
				nloc =+ sizeof cursym;
			continue;
		}
		symreloc();
		hp = lookup();
		if ((sp = *hp) == 0) {
			*hp = enter();
			*cp++ = hp;
			continue;
		}
		if (sp->stype != EXTERN+UNDEF)
			continue;
		if (cursym.stype == EXTERN+UNDEF) {
			if (cursym.svalue > sp->svalue)
				sp->svalue = cursym.svalue;
			continue;
		}
		if (sp->svalue != 0 && cursym.stype == EXTERN+TEXT)
			continue;
		ndef++;
		sp->stype = cursym.stype;
		sp->svalue = cursym.svalue;
	}
	if (libflg==0 || ndef) {
		tsize =+ filhdr.tsize;
		dsize =+ filhdr.dsize;
		bsize =+ filhdr.bsize;
		ssize =+ nloc;
		return(1);
	}
/*
 * No symbols defined by this library member.
 * Rip out the hash table entries and reset the symbol table.
 */
	symp = ssymp;
	while (cp > local)
		**--cp = 0;
	return(0);
}

middle()
{
	register struct symbol *sp;
	register t, csize;
	int nund, corigin;

	p_etext = *slookup("_etext");
	p_edata = *slookup("_edata");
	p_end = *slookup("_end");
/*
 * If there are any undefined symbols, save the relocation bits.
 */
	if (rflag==0) for (sp=symtab; sp<symp; sp++)
		if (sp->stype==EXTERN+UNDEF && sp->svalue==0
		 && sp!=p_end && sp!=p_edata && sp!=p_etext) {
			rflag++;
			dflag = 0;
			nflag = 0;
			iflag = 0;
			sflag = 0;
			break;
		}
/*
 * Assign common locations.
 */
	csize = 0;
	if (dflag || rflag==0) {
		for (sp=symtab; sp<symp; sp++)
			if (sp->stype==EXTERN+UNDEF && (t=sp->svalue)!=0) {
				t = (t+1) & ~01;
				sp->svalue = csize;
				sp->stype = EXTERN+COMM;
				csize =+ t;
			}
		if (p_etext && p_etext->stype==EXTERN+UNDEF) {
			p_etext->stype = EXTERN+TEXT;
			p_etext->svalue = tsize;
		}
		if (p_edata && p_edata->stype==EXTERN+UNDEF) {
			p_edata->stype = EXTERN+DATA;
			p_edata->svalue = dsize;
		}
		if (p_end && p_end->stype==EXTERN+UNDEF) {
			p_end->stype = EXTERN+BSS;
			p_end->svalue = bsize;
		}
	}
/*
 * Now set symbols to their final value
 */
	if (nflag || iflag)
		tsize = (tsize + 077) & ~077;
	dorigin = tsize;
	if (nflag)
		dorigin = (tsize+017777) & ~017777;
	if (iflag)
		dorigin = 0;
	corigin = dorigin + dsize;
	borigin = corigin + csize;
	nund = 0;
	for (sp=symtab; sp<symp; sp++) switch (sp->stype) {
	case EXTERN+UNDEF:
		errlev =| 01;
		if (arflag==0 && sp->svalue==0) {
			if (nund==0)
				printf("Undefined:\n");
			nund++;
			printf("%.8s\n", sp->sname);
		}
		continue;

	case EXTERN+ABS:
	default:
		continue;

	case EXTERN+TEXT:
		sp->svalue =+ torigin;
		continue;

	case EXTERN+DATA:
		sp->svalue =+ dorigin;
		continue;

	case EXTERN+BSS:
		sp->svalue =+ borigin;
		continue;

	case EXTERN+COMM:
		sp->stype = EXTERN+BSS;
		sp->svalue =+ corigin;
		continue;
	}
	if (sflag || xflag)
		ssize = 0;
	bsize =+ csize;
	nsym = ssize / (sizeof cursym);
}

setupout()
{
	register char *p;
	register pid;

	if ((toutb[0] = creat("l.out", 0666)) < 0)
		error(1, "Can't create l.out");
	pid = getpid();
	for (p = &tfname[12]; p > &tfname[7];) {
		*--p = (pid&07) + '0';
		pid =>> 3;
	}
	tcreat(doutb, 'a');
	if (sflag==0 || xflag==0)
		tcreat(soutb, 'b');
	if (rflag) {
		tcreat(troutb, 'c');
		tcreat(droutb, 'd');
	}
	filhdr.fmagic = FMAGIC;
	if (nflag)
		filhdr.fmagic = NMAGIC;
	if (iflag)
		filhdr.fmagic = IMAGIC;
	filhdr.tsize = tsize;
	filhdr.dsize = dsize;
	filhdr.bsize = bsize;
	filhdr.ssize = sflag? 0: (ssize + (sizeof cursym)*(symp-symtab));
	filhdr.entry = 0;
	filhdr.pad = 0;
	filhdr.relflg = (rflag==0);
	mput(toutb, &filhdr, sizeof filhdr);
	return;
}

tcreat(buf, letter)
int *buf;
{
	tfname[6] = letter;
	if ((buf[0] = creat(tfname, RONLY)) < 0)
		error(1, "Can't create temp");
}

load2arg(acp)
char *acp;
{
	register char *cp;
	register struct liblist *lp;

	cp = acp;
	if (getfile(cp) == 0) {
		while (*cp)
			cp++;
		while (cp >= acp && *--cp != '/');
		mkfsym(++cp);
		load2(0, 0);
		return;
	}
	for (lp = libp; lp->bno != -1; lp++) {
		dseek(&text, lp->bno, lp->off, sizeof archdr);
		mget(&archdr, sizeof archdr);
		mkfsym(archdr.aname);
		load2(lp->bno, lp->off + (sizeof archdr) / 2);
	}
	libp = ++lp;
}

load2(bno, off)
{
	register struct symbol *sp;
	register int *lp, symno;

	readhdr(bno, off);
	ctrel = torigin;
	cdrel =+ dorigin;
	cbrel =+ borigin;
/*
 * Reread the symbol table, recording the numbering
 * of symbols for fixing external references.
 */
	lp = local;
	symno = -1;
	off =+ (sizeof filhdr)/2;
	dseek(&text, bno, off+filhdr.tsize+filhdr.dsize, filhdr.ssize);
	while (text.size > 0) {
		symno++;
		mget(&cursym, sizeof cursym);
		symreloc();
		if ((cursym.stype&EXTERN) == 0) {
			if (!sflag&&!xflag&&(!Xflag||cursym.sname[0]!='L'))
				mput(soutb, &cursym, sizeof cursym);
			continue;
		}
		if ((sp = *lookup()) == 0)
			error(1, "internal error: symbol not found");
		if (cursym.stype == EXTERN+UNDEF) {
			if (lp >= &local[NSYMPR])
				error(1, "Local symbol overflow");
			*lp++ = symno;
			*lp++ = sp;
			continue;
		}
		if (cursym.stype!=sp->stype || cursym.svalue!=sp->svalue) {
			printf("%.8s: ", cursym.sname);
			error(0, "Multiply defined");
		}
	}
	dseek(&text, bno, off, filhdr.tsize);
	dseek(&reloc, bno, off+(filhdr.tsize+filhdr.dsize)/2, filhdr.tsize);
	load2td(lp, ctrel, toutb, troutb);
	dseek(&text, bno, off+(filhdr.tsize/2), filhdr.dsize);
	dseek(&reloc, bno, off+filhdr.tsize+(filhdr.dsize/2), filhdr.dsize);
	load2td(lp, cdrel, doutb, droutb);
	torigin =+ filhdr.tsize;
	dorigin =+ filhdr.dsize;
	borigin =+ filhdr.bsize;
}

load2td(lp, creloc, b1, b2)
int *lp;
{
	register r, t;
	register struct symbol *sp;

	for (;;) {
	/*
	 * The pickup code is copied from "get" for speed.
	 */
		if (--text.size <= 0) {
			if (text.size < 0)
				break;
			text.size++;
			t = get(&text);
		} else if (--text.nibuf < 0) {
			text.nibuf++;
			text.size++;
			t = get(&text);
		} else
			t = *text.ptr++;
		if (--reloc.size <= 0) {
			if (reloc.size < 0)
				error(1, "Relocation error");
			reloc.size++;
			r = get(&reloc);
		} else if (--reloc.nibuf < 0) {
			reloc.nibuf++;
			reloc.size++;
			r = get(&reloc);
		} else
			r = *reloc.ptr++;
		switch (r&016) {

		case RTEXT:
			t =+ ctrel;
			break;

		case RDATA:
			t =+ cdrel;
			break;

		case RBSS:
			t =+ cbrel;
			break;

		case REXT:
			sp = lookloc(lp, r);
			if (sp->stype==EXTERN+UNDEF) {
				r = (r&01) + ((nsym+(sp-symtab))<<4) + REXT;
				break;
			}
			t =+ sp->svalue;
			r = (r&01) + ((sp->stype-(EXTERN+ABS))<<1);
			break;
		}
		if (r&01)
			t =- creloc;
		putw(t, b1);
		if (rflag)
			putw(r, b2);
	}
}

finishout()
{
	register n, *p;

	if (nflag||iflag) {
		n = torigin;
		while (n&077) {
			n =+ 2;
			putw(0, toutb);
			if (rflag)
				putw(0, troutb);
		}
	}
	copy(doutb, 'a');
	if (rflag) {
		copy(troutb, 'c');
		copy(droutb, 'd');
	}
	if (sflag==0) {
		if (xflag==0)
			copy(soutb, 'b');
		for (p=symtab; p < symp;)
			putw(*p++, toutb);
	}
	fflush(toutb);
	close(toutb[0]);
	unlink("a.out");
	link("l.out", "a.out");
	delarg = errlev;
	delexit();
}

delexit()
{
	register c;

	unlink("l.out");
	for (c = 'a'; c <= 'd'; c++) {
		tfname[6] = c;
		unlink(tfname);
	}
	if (delarg==0)
		chmod("a.out", 0777);
	exit(delarg);
}

copy(buf, c)
int *buf;
{
	register f, *p, n;

	fflush(buf);
	close(buf[0]);
	tfname[6] = c;
	f = open(tfname, 0);
	while ((n = read(f, doutb, 512)) > 1) {
		n =>> 1;
		p = doutb;
		do
			putw(*p++, toutb);
		while (--n);
	}
	close(f);
}

mkfsym(s)
char *s;
{

	if (sflag || xflag)
		return;
	cp8c(s, cursym.sname);
	cursym.stype = 037;
	cursym.svalue = torigin;
	mput(soutb, &cursym, sizeof cursym);
}

mget(aloc, an)
int *aloc;
{
	register *loc, n;
	register *p;

	n = an;
	n =>> 1;
	loc = aloc;
	if ((text.nibuf =- n) >= 0) {
		if ((text.size =- n) > 0) {
			p = text.ptr;
			do
				*loc++ = *p++;
			while (--n);
			text.ptr = p;
			return;
		} else
			text.size =+ n;
	}
	text.nibuf =+ n;
	do {
		*loc++ = get(&text);
	} while (--n);
}

mput(buf, aloc, an)
int *aloc;
{
	register *loc;
	register n;

	loc = aloc;
	n = an>>1;
	do {
		putw(*loc++, buf);
	} while (--n);
}

dseek(asp, ab, o, s)
{
	register struct stream *sp;
	register struct page *p;
	register b;
	int n;

	sp = asp;
	b = ab + ((o>>8) & 0377);
	o =& 0377;
	--sp->pno->nuser;
	if ((p = &page[0])->bno!=b && (p = &page[1])->bno!=b)
		if (p->nuser==0 || (p = &page[0])->nuser==0) {
			if (page[0].nuser==0 && page[1].nuser==0)
				if (page[0].bno < page[1].bno)
					p = &page[0];
			p->bno = b;
			seek(infil, b, 3);
			if ((n = read(infil, p->buff, 512)>>1) < 0)
				n = 0;
			p->nibuf = n;
		} else
			error(1, "No pages");
	++p->nuser;
	sp->bno = b;
	sp->pno = p;
	sp->ptr = p->buff + o;
	if (s != -1)
		sp->size = (s>>1) & 077777;
	if ((sp->nibuf = p->nibuf-o) <= 0)
		sp->size = 0;
}

get(asp)
struct stream *asp;
{
	register struct stream *sp;

	sp = asp;
	if (--sp->nibuf < 0) {
		dseek(sp, sp->bno+1, 0, -1);
		--sp->nibuf;
	}
	if (--sp->size <= 0) {
		if (sp->size < 0)
			error(1, premeof);
		++fpage.nuser;
		--sp->pno->nuser;
		sp->pno = &fpage;
	}
	return(*sp->ptr++);
}

getfile(acp)
char *acp;
{
	register char *cp;
	register c;

	cp = acp;
	archdr.aname[0] = '\0';
	if (cp[0]=='-' && cp[1]=='l') {
		if ((c = cp[2]) == '\0')
			c = 'a';
		cp = "/lib/lib?.a";
		cp[8] = c;
	}
	filname = cp;
	if ((infil = open(cp, 0)) < 0)
		error(1, "cannot open");
	page[0].bno = page[1].bno = -1;
	page[0].nuser = page[1].nuser = 0;
	text.pno = reloc.pno = &fpage;
	fpage.nuser = 2;
	dseek(&text, 0, 0, 2);
	if (text.size <= 0)
		error(1, premeof);
	return(get(&text) == ARCMAGIC);
}

struct symbol **lookup()
{
	int i;
	register struct symbol **hp;
	register char *cp, *cp1;

	i = 0;
	for (cp=cursym.sname; cp < &cursym.sname[8];)
		i = (i<<1) + *cp++;
	for (hp = &hshtab[(i&077777)%NSYM+2]; *hp!=0;) {
		cp1 = (*hp)->sname;
		for (cp=cursym.sname; cp < &cursym.sname[8];)
			if (*cp++ != *cp1++)
				goto no;
		break;
	    no:
		if (++hp >= &hshtab[NSYM+2])
			hp = hshtab;
	}
	return(hp);
}

struct symbol **slookup(s)
char *s;
{
	cp8c(s, cursym.sname);
	cursym.stype = EXTERN+UNDEF;
	cursym.svalue = 0;
	return(lookup());
}

enter()
{
	register struct symbol *sp;
	
	if ((sp=symp) >= &symtab[NSYM])
		error(1, "Symbol table overflow");
	cp8c(cursym.sname, sp->sname);
	sp->stype = cursym.stype;
	sp->svalue = cursym.svalue;
	symp++;
	return(sp);
}

symreloc()
{
	switch (cursym.stype) {

	case TEXT:
	case EXTERN+TEXT:
		cursym.svalue =+ ctrel;
		return;

	case DATA:
	case EXTERN+DATA:
		cursym.svalue =+ cdrel;
		return;

	case BSS:
	case EXTERN+BSS:
		cursym.svalue =+ cbrel;
		return;

	case EXTERN+UNDEF:
		return;
	}
	if (cursym.stype&EXTERN)
		cursym.stype = EXTERN+ABS;
}

error(n, s)
char *s;
{
	if (filname) {
		printf("%s", filname);
		if (archdr.aname[0])
			printf("(%.8s)", archdr.aname);
		printf(": ");
	}
	printf("%s\n", s);
	if (n)
		delexit();
	errlev = 2;
}

lookloc(alp, r)
{
	register int *clp, *lp;
	register sn;

	lp = alp;
	sn = (r>>4) & 07777;
	for (clp=local; clp<lp; clp =+ 2)
		if (clp[0] == sn)
			return(clp[1]);
	error(1, "Local symbol botch");
}

readhdr(bno, off)
{
	register st, sd;

	dseek(&text, bno, off, sizeof filhdr);
	mget(&filhdr, sizeof filhdr);
	if (filhdr.fmagic != FMAGIC)
		error(1, "Bad format");
	st = (filhdr.tsize+01) & ~01;
	filhdr.tsize = st;
	cdrel = -st;
	sd = (filhdr.dsize+01) & ~01;
	cbrel = - (st+sd);
	filhdr.bsize = (filhdr.bsize+01) & ~01;
}

cp8c(from, to)
char *from, *to;
{
	register char *f, *t, *te;

	f = from;
	t = to;
	te = t+8;
	while ((*t++ = *f++) && t<te);
	while (t<te)
		*t++ = 0;
}
