#
/*
 *  link editor for VAX
 *
 *  Written by J. Reiser, modified by K. sklower to have an incremental
 *  loading facility
 */

/*	layout of a.out file:
 *
 *	header of 8 words	magic number 0410:
					data starts at 1st 0777
					boundary above text
				magic number 0407:
					data starts immediately after
					text
 *				text size	)
 *				data size	) in bytes
 *				bss size	)
 *				symbol table size
 *				entry point
 *				size of text relocation info
 *				size of data relocation info
 *
 *  'segment'   origin   comments
 *	header:		0
 *	text:		32       0 padded to multiple of 4 bytes
 *	data:		32+textsize     0 padded to multiple of 4 bytes
 *	text relocation:	32+textsize+datasize
 *	data relocation:	32+textsize+datasize+textrelocationsize
 *	symbol table:	32+textsize+datasize+textrelocationsize+datarelocationsize
 *
 */
#include <signal.h>
#include <stdio.h>
#include <ar.h>
#include <a.out.h>

typedef struct {short hiword; short loword;} *bless; /* stupid fp-11 */
fixl(p) register long *p;{
	register short t;
	t=((bless) p)->hiword; ((bless) p)->hiword=((bless) p)->loword; ((bless) p)->loword=t;
}

writel(p,n,f) long *p; FILE *f; {
#ifdef vax
	fwrite(p,sizeof(*p),n,f);
#else
	while (n--) {
		fwrite(&(*p).loword,2,1,f);
		fwrite(&(*p).hiword,2,1,f);
		p++;
	}
#endif
}

long htoi(p) register char *p; {/* hex to integer conversion */
register long n = 0;
while (*p) {
	n <<= 4;
	if 		(*p<='9' && *p>='0') n += *p - '0';
	else if (*p<='f' && *p>='a') n += *p -'a' +10;
	else if (*p<='F' && *p>='A') n += *p -'A' +10;
	p++;
}
return(n);
}

typedef	char *STRING;
typedef	int BOOL;
#define TRUE	1
#define FALSE	0

#define	OMAGIC	0407
#define	NMAGIC	0410

/*
 * Symbol types
 */
#define	UNDEF	0x0
#define	ABS	0x2
#define	TEXT	0x4
#define	DATA	0x6
#define	BSS	0x8
#define	DATAO	0xA
#define	BSSO	0xC
#define	TEXTO	0xE
#define	ABSO	0x10

#define	COMM	0x12	/* for internal use only */

#define	EXTERN	0x1
#define	TYPE	0x1E
#define STABTYPS 0xE0
/*
 * address reference types
 */
#define PCREL	1
#define LEN1	0
#define LEN2	2
#define LEN4	4

#define	HW	01
#define	FW	03
#define	DW	07

#define	PAGRND	0777

#define	TYPMASK	0x1E
#define	TYMASK	(0x1E)
#define TMASK	0x1F

#define	RABS	(ABS)
#define	RTEXT	TEXT
#define	RDATA	DATA
#define	RBSS	BSS
#define	RDATAO	DATAO
#define	RBSSO	BSSO
#define	RTEXTO	TEXTO
#define	RABSO	ABSO
#define	REXT	(01<<3)
#define	ROFF	(02<<3)
#define	REFMASK	0x7

#define NOVLY	1
#define	RELFLG	01
#define	NROUT	256
#define	NSYM	1103
#define	NSYMPR	500

char	premeof[] = "Premature EOF";

typedef struct {
	long	loc;
} LIBLIST;

/* overlay management */
int	vindex;
typedef struct {
	int	argsav;
	int	symsav;
	LIBLIST	*libsav;
	STRING	vname;
	long	ctsav, cdsav, cbsav;
	long	offt, offd, offb, offtr, offdr, offs;
} OVERLAY;
OVERLAY	vnodes[NOVLY];

/* input management */
typedef struct {
	short	*fakeptr;
	int	bno;
	int	nibuf;
	int	nuser;
	char	buff[512];
} PAGE;

PAGE	page[2];

struct {
	short	*fakeptr;
	int	bno;
	int	nibuf;
	int	nuser;
} fpage;

typedef struct {
	char	*ptr;
	int	bno;
	int	nibuf;
	long	size;
	long	pos;
	PAGE	*pno;
} STREAM;

STREAM text;
STREAM reloc;

struct	ar_hdr archdr;

struct	exec filhdr;

/* one entry for each archive member referenced;
 * set in first pass; needs restoring for overlays
 */

LIBLIST	liblist[NROUT];
LIBLIST	*libp = liblist;


/* symbol management */
typedef struct {
	char	sname[8];
	char	stype;
	char	spare;
	short	symhash;	/* index of hash table entry pointing to this symbol */
	long	svalue;
} SYMBOL;

typedef struct {
	int locindex;		/* index to symbol in file */
	SYMBOL *locsymbol;	/* ptr to symbol table */
} LOCAL;

SYMBOL	cursym;			/* current symbol */
SYMBOL	*symtab;		/* actual symbols */
SYMBOL	*lastsym;		/* last symbol entered */
SYMBOL	*nextsym;		/* next available symbol table entry */
SYMBOL	*addsym;		/* first symbol defined during incremental
				   load */
int nsym;			/* number of symbols allocated in symtab */
SYMBOL	*hshtab[NSYM+2];	/* hash table for symbols */
LOCAL	*local;

/* internal symbols */
SYMBOL	*p_data;
SYMBOL	*p_etext;
SYMBOL	*p_edata;
SYMBOL	*p_end;
SYMBOL	*entrypt;

int	trace;
/* flags */
int	xflag;		/* discard local symbols */
int	Xflag;		/* discard locals starting with 'L' */
int	Sflag;		/* discard all except locals and globals*/
int	rflag;		/* preserve relocation bits, don't define common */
int	arflag;		/* original copy of rflag */
int	sflag;		/* discard all symbols */
int	nflag = 1;	/* pure procedure */
int	dflag;		/* define common even with rflag */
int	iflag;		/* I/D space separated */
BOOL	vflag;		/* overlays used */
int	Aflag;		/* doing incremental load */
int	RFflag;		/* used to escape while reading fundament file*/

int	ofilfnd;
char	*ofilename	= "l.out";
int	infil;
char	*filname;

long	textbase;
/* cumulative sizes set in pass 1 */
long	tsize;
long	dsize;
long	bsize;
long	trsize;
long	drsize;
long	ssize;

/* symbol relocation; both passes */
long	ctrel;
long	cdrel;
long	cbrel;
long	ctorel;
long	cdorel;
long	cborel;

int	errlev;
int	delarg	= 4;


FILE	*tout;
FILE	*dout;
char	*doutn	= "";
FILE	*trout;
char	*troutn	= "";
FILE	*drout;
char	*droutn	= "";
FILE	*sout;
char	*soutn	= "";

char	*mktemp();
char 	get();
char	getb();
short	gets();
long	get3();
long	getl();
SYMBOL	**lookup();
FILE	*tcreat();
long	round();
SYMBOL	**slookup();
SYMBOL	*lookloc();

symwrite(sp,n,f) SYMBOL *sp; FILE *f; {
#ifdef vax
	fwrite(sp,sizeof(*symtab),n,f);
#else
	while (n--) {
		fwrite(sp,sizeof(*symtab)-sizeof(sp->svalue),1,f);
		writel(&(sp->svalue),1,f); sp++;
	}
#endif
}

delexit()
{
	unlink("l.out");
	unlink(doutn);
	unlink(troutn);
	unlink(droutn);
	unlink(soutn);
	if (delarg==0)
		chmod(ofilename, 0777);
	exit(delarg);
}

main(argc, argv)
char **argv;
{
	register int c, i; 
	int num;
	register char *ap, **p;
	BOOL found; 
	int vscan; 
	char save;

	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, delexit);
	if (argc == 1)
		exit(4);
	p = argv+1;

	nextsym=symtab=(SYMBOL *)sbrk(0); nsym=0;
	/* scan files once to find symdefs */
	for (c=1; c<argc; c++) {
		if (trace) printf("%s:\n", *p);
		filname = 0;
		ap = *p++;

		if (*ap == '-') {
			for (i=1; ap[i]; i++) {
			switch (ap[i]) {
			case 'o':
				if (++c >= argc)
					error(1, "Bad output file");
				ofilename = *p++;
				ofilfnd++;
				continue;

			case 'u':
			case 'e':
				if (++c >= argc)
					error(1, "Bad 'use' or 'entry'");
				enter(slookup(*p++));
				if (ap[i]=='e')
					entrypt = lastsym;
				continue;

			case 'A':
				if (++c >= argc)
					error(1, "-A: arg missing");
				if (Aflag) 
					error(2, "-A: Only one fundament file allowed");
				Aflag = TRUE;
				nflag = FALSE;
				fundament(*p++);
				continue;

			case 'v':
				if (++c >= argc)
					error(1, "-v: arg missing");
				vflag=TRUE;
				vscan = vindex; 
				found=FALSE;
				while (--vscan>=0 && found==FALSE)
					found = eq(vnodes[vscan].vname, *p);
				if (found) {
					endload(c, argv);
					restore(vscan);
				} else
					record(c, *p);
				p++;
				continue;

			case 'D':
				if (++c >= argc)
					error(1, "-D: arg missing");
				num = htoi(*p++);
				if (dsize>num)
					error(1, "-D: too small");
				dsize = num;
				continue;

			case 'T':
				if (++c >= argc)
					error(1, "-T: arg missing");
				if (tsize!=0 && !Aflag)
					error(1, "-T: too late, some text already loaded");
				textbase = htoi(*p++);
				continue;

			case 'l':
				save = ap[--i]; 
				ap[i]='-';
				load1arg(&ap[i]); 
				ap[i]=save;
				break;

			case 'x':
				xflag++;
				continue;

			case 'X':
				Xflag++;
				continue;

			case 'S':
				Sflag++; 
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

			case 'N':
				nflag = 0;
				continue;

			case 'd':
				dflag++;
				continue;

			case 'i':
				iflag++;
				continue;

			case 't':
				trace++;
				continue;

			default:
				error(1, "bad flag");
			} /*endsw*/
			break;
			} /*endfor*/
		} else
			load1arg(ap);
	}
	endload(argc, argv);
	exit(0);
}

/* used after pass 1 */
long	torigin;
long	dorigin;
long	borigin;
long	database;

endload(argc, argv)
int argc; 
char **argv;
{
	register int c, i; 
	long dnum;
	register char *ap, **p;

	brk(nextsym);
	filname = 0;
	middle();
	setupout();
	if ((LOCAL *)-1==(local=(LOCAL *)sbrk(NSYMPR*sizeof(*local)))) error(1,"Memory overflow");
	p = argv+1;
	libp = liblist;
	for (c=1; c<argc; c++) {
		ap = *p++;
		if (trace) printf("%s:\n", ap);
		if (*ap == '-') {
			for (i=1; ap[i]; i++) {
			switch (ap[i]) {
			case 'D':
				for (dnum = htoi(*p); dorigin<dnum; dorigin++) putc(0, dout);
			case 'T':
			case 'u':
			case 'e':
			case 'o':
			case 'v':
				++c; 
				++p;

			default:
				continue;

			case 'A':
				fund2(*p++);
				c++;
				continue;

			case 'l':
				ap[--i]='-'; 
				load2arg(&ap[i]);
				break;
			} /*endsw*/
			break;
			} /*endfor*/
		} else
			load2arg(ap);
	}
	finishout();
}

record(c, nam)
int c; 
STRING nam;
{
	register OVERLAY *v;

	v = &vnodes[vindex++];
	v->argsav = c;
	v->symsav = nextsym-symtab;
	v->libsav = libp;
	v->vname = nam;
	v->offt = tsize; 
	v->offd = dsize; 
	v->offb = bsize; 
	v->offtr = trsize;
	v->offdr = drsize;
	v->offs = ssize;
	v->ctsav = ctrel; 
	v->cdsav = cdrel; 
	v->cbsav = cbrel;
}

restore(vscan)
int vscan;
{
	register OVERLAY *v;
	register SYMBOL *saved,*sp;

	v = &vnodes[vscan];
	vindex = vscan+1;
	libp = v->libsav;
	ctrel = v->ctsav; 
	cdrel = v->cdsav; 
	cbrel = v->cbsav;
	tsize = v->offt; 
	dsize = v->offd; 
	bsize = v->offb; 
	trsize = v->offtr;
	drsize = v->offdr;
	ssize = v->offs;
	saved = symtab + v->symsav;
	sp = nextsym;
	while (sp>saved)
		hshtab[(--sp)->symhash]=0;
	nextsym = saved;
}

/* scan file to find defined symbols */
load1arg(cp)
register char *cp;
{
	long loc;

	if (getfile(cp)==0)
		load1(0, 0L);
	else {
		loc = sizeof(int);
		for (;;) {
			dseek(&text, loc, (long)sizeof(archdr));
			if (text.size <= 0) {
				libp->loc = -1;
				if( ++libp >= liblist + NROUT)
					error(1,"liblist overflow");
					/* thanks to Dennis Wasley */
				return;
			}
			mget((short *)&archdr, sizeof archdr, &text);
			if (load1(1, loc+sizeof(archdr))) {
				libp->loc = loc;
				libp++;
			}
#ifndef vax
			if (archdr.ar_size.loword==0) fixl(&archdr.ar_size);
#endif
			loc += round(archdr.ar_size, 1) + sizeof(archdr);
		}
	}
	close(infil);
}

/* single file or archive member */
load1(libflg, loc)
long loc;
{
	register SYMBOL *sp;
	SYMBOL *savnext;
	int ndef, nlocal, type;

	readhdr(loc);
	ctrel = tsize;
	cdrel += dsize;
	cbrel += bsize;
	ndef = 0;
	nlocal = sizeof(cursym);
	savnext = nextsym;
/*	if (filhdr.a_trsize+filhdr.a_drsize==0) {
/*		error(0, "No relocation bits");
/*		return(0);
/*	}
*/
	loc += filhdr.a_text + filhdr.a_data +
			filhdr.a_trsize + filhdr.a_drsize + sizeof(filhdr);
	dseek(&text, loc, filhdr.a_syms);
	while (text.size > 0) {
		symget(&cursym, &text);
		type = cursym.stype;
		if ((type&EXTERN)==0) {
			if (Xflag==0 || cursym.sname[0]!='L' || type & STABTYPS)
				nlocal += sizeof cursym;
			continue;
		}
		symreloc();
		if (enter(lookup()))
			continue;
		if ((sp = lastsym)->stype != EXTERN+UNDEF)
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
		tsize += filhdr.a_text;
		dsize += round(filhdr.a_data, FW);
		bsize += round(filhdr.a_bss, FW);
		ssize += nlocal;
		trsize += filhdr.a_trsize;
		drsize += filhdr.a_drsize;
		if (RFflag) textbase = (*slookup("_end"))->svalue;
		return(1);
	}
	/*
	 * No symbols defined by this library member.
	 * Rip out the hash table entries and reset the symbol table.
	 */
	while (nextsym>savnext)
		hshtab[(--nextsym)->symhash]=0;
	return(0);
}

middle()
{
	register SYMBOL *sp, *symp;
	long csize, t, corigin, ocsize;
	int nund, rnd;
	char s;

	torigin = 0; 
	dorigin = 0; 
	borigin = 0;

	p_data = *slookup("_data");
	p_etext = *slookup("_etext");
	p_edata = *slookup("_edata");
	p_end = *slookup("_end");
	/*
	 * If there are any undefined symbols, save the relocation bits.
	 */
	symp = nextsym;
	if (rflag==0) {
		for (sp = symtab; sp<symp; sp++)
			if (sp->stype==EXTERN+UNDEF && sp->svalue==0
				&& sp!=p_end && sp!=p_edata && sp!=p_etext
				&& sp!=p_data) {
				rflag++;
				dflag = 0;
				break;
			}
	}
	if (rflag) 
		sflag = iflag = 0;
	/*
	 * Assign common locations.
	 */
	csize = 0;
	if(!Aflag) addsym = symtab;
	database = round(tsize+textbase, (nflag? PAGRND:FW));
	if (dflag || rflag==0) {
		ldrsym(p_data, (long)0 , EXTERN+DATA);
		ldrsym(p_etext, tsize, EXTERN+TEXT);
		ldrsym(p_edata, dsize, EXTERN+DATA);
		ldrsym(p_end, bsize, EXTERN+BSS);
		for (sp = addsym; sp<symp; sp++) {
			if ((s=sp->stype)==EXTERN+UNDEF && (t = sp->svalue)!=0) {
				if (t>DW)
					rnd = DW;
				else if (t>FW)
					rnd = FW;
				else
					rnd = HW;
				csize = round(csize, rnd);
				sp->svalue = csize;
				sp->stype = EXTERN+COMM;
				ocsize = csize;	
				csize += t;
			}
			if (((s&TMASK) == EXTERN+UNDEF) && (s & STABTYPS)) {
				sp->svalue = ocsize;
				sp->stype = (s & STABTYPS) | (EXTERN+COMM);
			}
		}
	}
	/*
	 * Now set symbols to their final value
	 */
	csize = round(csize, FW);
	torigin = textbase;
	dorigin = database;
	corigin = dorigin + dsize;
	borigin = corigin + csize;
	cdorel = 0;
	cborel = dsize+csize;
	nund = 0;
	for (sp = addsym; sp<symp; sp++) switch (sp->stype & TMASK) {
	case EXTERN+UNDEF:
		errlev |= 01;
		if ((arflag==0 || dflag) && sp->svalue==0) {
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
		sp->svalue += torigin;
		continue;

	case EXTERN+DATA:
		sp->svalue += dorigin;
		continue;

	case EXTERN+BSS:
		sp->svalue += borigin;
		continue;

	case EXTERN+COMM:
		sp->stype = (sp->stype & STABTYPS) | (EXTERN+BSS);
		sp->svalue += corigin;
		continue;
	}
	if (sflag || xflag)
		ssize = 0;
	bsize += csize;
	nsym = ssize / (sizeof cursym);
	if (Aflag) {
		fixspec(p_data,dorigin);
		fixspec(p_etext,torigin);
		fixspec(p_edata,dorigin);
		fixspec(p_end,borigin);
	}
}
fixspec(sym,offset)
SYMBOL *sym; long offset;
{
	if(sym < addsym && sym!=0)
		sym->svalue += offset;
}
	

ldrsym(asp, val, type)
long val;
SYMBOL *asp;
{
	register SYMBOL *sp;

	if ((sp = asp) == 0)
		return;
	if ((sp->stype != EXTERN+UNDEF || sp->svalue) && !Aflag) {
		printf("%.8s: ", sp->sname);
		error(0, "Multiply defined (internal)");
		return;
	}
	sp->stype = type;
	sp->svalue = val;
}

extern char _sibuf[BUFSIZ]; /* the space is forced upon us; might as well use it */

setupout()
{
	tout = fopen(ofilename, "w");
	if (tout==NULL)
		error(1, "cannot create output");
	setbuf(tout,_sibuf);
	dout = tcreat(&doutn, "/tmp/ldaaXXXXX");
	if (sflag==0 || xflag==0)
		sout = tcreat(&soutn, "/tmp/ldbaXXXXX");
	if (rflag) {
		trout = tcreat(&troutn, "/tmp/ldcaXXXXX");
		drout = tcreat(&droutn, "/tmp/lddaXXXXX");
	}
	filhdr.a_magic = nflag? NMAGIC:OMAGIC;
	filhdr.a_text = nflag? tsize:round(tsize, FW);
	filhdr.a_data = dsize;
	filhdr.a_bss = bsize;
	filhdr.a_trsize = trsize;
	filhdr.a_drsize = drsize;
	filhdr.a_syms = sflag? 0: (ssize + (sizeof cursym)*(nextsym-symtab));
	if (entrypt) {
		if (entrypt->stype!=EXTERN+TEXT)
			error(0, "Entry point not in text");
		else
			filhdr.a_entry = entrypt->svalue;
	} else
		filhdr.a_entry=0;
	filhdr.a_trsize = (rflag ? trsize:0);
	filhdr.a_drsize = (rflag ? drsize:0);
	writel(&filhdr,8,tout);
}

FILE *
tcreat(namep, name)
char **namep, *name;
{
	register FILE *fp;
	register char *tnm;

	tnm = mktemp(name);
	if ((fp = fopen(tnm, "w")) == NULL)
		error(1, "Cannot create temp file");
	chmod(tnm, 0600);
	*namep = tnm;
	return(fp);
}

load2arg(acp)
char *acp;
{
	register char *cp;
	register LIBLIST *lp;

	cp = acp;
	if (getfile(cp) == 0) {
		while (*cp)
			cp++;
		while (cp >= acp && *--cp != '/');
		mkfsym(++cp);
		load2(0L);
	} else {	/* scan archive members referenced */
		for (lp = libp; lp->loc != -1; lp++) {
			dseek(&text, lp->loc, (long)sizeof(archdr));
			mget((short *)&archdr, sizeof(archdr), &text);
			mkfsym(archdr.ar_name);
			load2(lp->loc + (long)sizeof(archdr));
		}
		libp = ++lp;
	}
	close(infil);
}

load2(loc)
long loc;
{
	register SYMBOL *sp;
	register LOCAL *lp;
	register int symno;
	int type;

	readhdr(loc);
	if(!RFflag) {
		ctrel = torigin;
		cdrel += dorigin;
		cbrel += borigin;
	}
	/*
	 * Reread the symbol table, recording the numbering
	 * of symbols for fixing external references.
	 */
	lp = local;
	symno = -1;
	loc += sizeof(filhdr);
	dseek(&text, loc+filhdr.a_text+filhdr.a_data+
		filhdr.a_trsize+filhdr.a_drsize, filhdr.a_syms);
	while (text.size > 0) {
		symno++;
		symget(&cursym, &text);
		symreloc();
		type = cursym.stype;
		if ((type&EXTERN) == 0) {
			if (!sflag&&!xflag&&
				(!Xflag||cursym.sname[0]!='L'||type&STABTYPS))
				symwrite(&cursym, 1, sout);
			continue;
		}
		if (RFflag) continue;
		if ((sp = *lookup()) == 0)
			error(1, "internal error: symbol not found");
		if (cursym.stype == EXTERN+UNDEF) {
			if (lp >= local+NSYMPR)
				error(1, "Local symbol overflow");
			lp->locindex = symno;
			lp++->locsymbol = sp;
			continue;
		}
		if(cursym.stype & STABTYPS) continue;
		if (cursym.stype!=sp->stype || cursym.svalue!=sp->svalue) {
			printf("%.8s: ", cursym.sname);
			error(0, "Multiply defined");
		}
	}
	if(RFflag) return;
	dseek(&text, loc, filhdr.a_text);
	dseek(&reloc, loc+filhdr.a_text+filhdr.a_data, filhdr.a_trsize);
	load2td(lp, ctrel, tout, trout);
	dseek(&text, loc+filhdr.a_text, filhdr.a_data);
	dseek(&reloc, loc+filhdr.a_text+filhdr.a_data+filhdr.a_trsize, filhdr.a_drsize);
	load2td(lp, cdrel, dout, drout);
	while (filhdr.a_data&FW) {
		putc(0, dout); filhdr.a_data++;
	}
	torigin += filhdr.a_text;
	dorigin += filhdr.a_data;
	borigin += filhdr.a_bss;
	cdorel += filhdr.a_data;
	cborel += filhdr.a_bss;
}

load2td(lp, creloc, b1, b2)
LOCAL *lp;
long creloc;
FILE *b1, *b2;
{
	register r1;
	register char r2; 
	register long t;
	register SYMBOL *sp;
	long tw,u,l;

	for (;;) {
	if (reloc.size==0) {while (text.size) putc(get(&text),b1); break;}
	t=getl(&reloc); /* position of relocatable stuff */
	if (rflag) putl(t+creloc,b2); /* remember for subsequent link editing */
	while (text.pos<t) putc(get(&text),b1); /* advance to proper position */
	r1=get3(&reloc); /* kind of relocation */
	r2 = getb(&reloc);
	switch (r2&06) {/* read raw datum according to its length */
		case LEN1: tw=get(&text); break;
		case LEN2: tw=gets(&text); break;
		case LEN4: tw=getl(&text); break;
	}
	if (r2&REXT) {
		sp=lookloc(lp,r1); /* find the symbol */
		if (sp->stype==EXTERN+UNDEF) { /* still undefined */
			r2=(r2&(REFMASK+REXT+ROFF));
			r1 = nsym+(sp-symtab); /* new reloc */
		}
		else {
			if (sp->stype==EXTERN+DATA && r2&ROFF) {
				r1=RDATAO;
				r2&=REFMASK;
			}
			else if (sp->stype==EXTERN+BSS && r2&ROFF) {
				r1=RBSSO;
				r2&=REFMASK;
			}
			else if (sp->stype==EXTERN+ABS && r2&ROFF) {
				r1=RABSO;
				r2&=REFMASK;
			}
			else if (sp->stype==EXTERN+TEXT && r2&ROFF) {
				r1=RTEXTO;
				r2&=REFMASK;
			}
			else {if (r2&ROFF) {if (rflag) {error(0,"!-r; see JFR"); rflag=0;}}
				 else tw += database;
				 r1=sp->stype&TYPE;
				 r2&=REFMASK;
			}
			tw += sp->svalue - database;
		}
	} else switch (r1&TYMASK) {
		case RTEXT:	tw += ctrel; break;
		case RTEXTO:tw += round(filhdr.a_text,PAGRND)+ctrel-database; break;
		case RDATA: tw += cdrel; break;
		case RDATAO:tw += cdorel; break;
		case RBSS:	tw += cbrel; break;
		case RBSSO: tw += cborel-filhdr.a_data; break;
		case RABSO: tw += round(filhdr.a_text,PAGRND)-database; break;
	}
	if (rflag) { /* remember for subsequent link editing */
		put3(r1,b2);
		putb(r2,b2);
	}
	if (r2&PCREL) tw -= creloc; /* assembler already subtracted text.pos */
	switch (r2&06) {/* output relocated datum according to its length */
		case LEN1: l= -128; u=127; putc((char)tw,b1); break;
		case LEN2: l= -32768; u=32767; puts((short)tw,b1); break;
		case LEN4: l=0x80000000; u=0x7FFFFFFF; putl(tw,b1); break;
	}
	if (tw<l || u<tw) error(0,"Displacement overflow");
	}
}

finishout()
{

	if (!nflag)
		while (tsize&FW) {
			putc(0, tout); tsize++;
		}
	fclose(dout);
	copy(doutn);
	if (rflag) {
		fclose(trout);
		copy(troutn);
		fclose(drout);
		copy(droutn);
	}
	if (sflag==0) {
		if (xflag==0) {
			fclose(sout);
			copy(soutn);
		}
		symwrite(symtab, nextsym-symtab, tout);
	}
	fclose(tout);
	if (!ofilfnd) {
		unlink("a.out");
		link("l.out", "a.out");
		ofilename = "a.out";
	}
	delarg = errlev;
	delexit();
}

copy(np)
char *np;
{
	register c;
	register FILE *fp;

	if ((fp = fopen(np, "r")) == NULL)
		error(1, "cannot recopy output");
	while ((c = getc(fp)) != EOF)
		putc(c, tout);
	fclose(fp);
}

mkfsym(s)
char *s;
{

	if (sflag || xflag)
		return;
	cp8c(s, cursym.sname);
	cursym.stype = TEXT;
	cursym.svalue = torigin;
	symwrite(&cursym, 1, sout);
}

mget(loc, n, sp)
register STREAM *sp;
register char *loc;
{
	register char *p;

	if ((sp->nibuf -= n) >= 0) {
		if ((sp->size -= n) > 0) {
			p = sp->ptr;
			sp->pos += n;
			do
				*loc++ = *p++;
			while (--n);
			sp->ptr = p;
			return;
		} else
			sp->size += n;
	}
	sp->nibuf += n;
	do {
		*loc++ = get(sp);
	} while (--n);
}

short
gets(sp) STREAM *sp; {
short t; mget(&t,2,sp); return(t);
}

char
getb(sp) STREAM *sp; {
char t; mget(&t,1,sp); return(t);
}

long
get3(sp) STREAM *sp; {
long t; t=0; mget(&t,3,sp); return(t);
}

long
getl(sp) STREAM *sp; {
	long t; mget(&t,4,sp);
#ifndef vax
	fixl(&t);
#endif
	return(t);
}

symget(sp,f) SYMBOL *sp; STREAM *f; {
	mget(sp,sizeof(*sp),f);
#ifndef vax
	fixl(&sp->svalue);
#endif
}

dseek(sp, loc, s)
register STREAM *sp;
long loc, s;
{
	register PAGE *p;
	register b, o;
	int n;

	b = loc>>9;
	o = loc&0777;
	if (o&01)
		error(1, "loader error; odd offset");
	--sp->pno->nuser;
	if ((p = &page[0])->bno!=b && (p = &page[1])->bno!=b)
		if (p->nuser==0 || (p = &page[0])->nuser==0) {
			if (page[0].nuser==0 && page[1].nuser==0)
				if (page[0].bno < page[1].bno)
					p = &page[0];
			p->bno = b;
			lseek(infil, loc & ~0777L, 0);
			if ((n = read(infil, p->buff, sizeof(p->buff))) < 0)
				n = 0;
			p->nibuf = n;
	} else
		error(1, "No pages");
	++p->nuser;
	sp->bno = b;
	sp->pno = p;
	if (s != -1) {sp->size = s; sp->pos = 0;}
	sp->ptr = (char *)(p->buff + o);
	if ((sp->nibuf = p->nibuf-o) <= 0)
		sp->size = 0;
}

char
get(asp)
STREAM *asp;
{
	register STREAM *sp;

	sp = asp;
	if ((sp->nibuf -= sizeof(char)) < 0) {
		dseek(sp, ((long)(sp->bno+1)<<9), (long)-1);
		sp->nibuf -= sizeof(char);
	}
	if ((sp->size -= sizeof(char)) <= 0) {
		if (sp->size < 0)
			error(1, premeof);
		++fpage.nuser;
		--sp->pno->nuser;
		sp->pno = (PAGE *) &fpage;
	}
	sp->pos += sizeof(char);
	return(*sp->ptr++);
}

getfile(acp)
STRING acp;
{
	register STRING cp;
	register int c;
	int arcmag;

	cp = acp; 
	infil = -1;
	archdr.ar_name[0] = '\0';
	filname = cp;
	if (cp[0]=='-' && cp[1]=='l') {
		if(cp[2] == '\0')
			cp = "-la";
		filname = "/usr/lib/libxxxxxxxxxxxxxxx";
		for(c=0; cp[c+2]; c++)
			filname[c+12] = cp[c+2];
		filname[c+12] = '.';
		filname[c+13] = 'a';
		filname[c+14] = '\0';
		if ((infil = open(filname+4, 0)) >= 0) {
			filname += 4;
		}
	}
	if (infil == -1 && (infil = open(filname, 0)) < 0)
		error(1, "cannot open");
	page[0].bno = page[1].bno = -1;
	page[0].nuser = page[1].nuser = 0;
	text.pno = reloc.pno = (PAGE *) &fpage;
	fpage.nuser = 2;
	dseek(&text, 0L, (long)sizeof(int));
	if (text.size <= 0)
		error(1, premeof);
	mget(&arcmag, sizeof(arcmag), &text);
	return(arcmag==ARMAG);
}

SYMBOL **lookup()
{
	int i; 
	BOOL clash;
	register SYMBOL **hp;
	register char *cp, *cp1;

	i = 0;
	for (cp = cursym.sname; cp < &cursym.sname[8];)
		i = (i<<1) + *cp++;
	for (hp = &hshtab[(i&077777)%NSYM+2]; *hp!=0;) {
		cp1 = (*hp)->sname; 
		clash=FALSE;
		for (cp = cursym.sname; cp < &cursym.sname[8];)
			if (*cp++ != *cp1++) {
				clash=TRUE; 
				break;
			}
		if (clash) {
			if (++hp >= &hshtab[NSYM+2])
				hp = hshtab;
		} else
			break;
	}
	return(hp);
}

SYMBOL **slookup(s)
char *s;
{
	cp8c(s, cursym.sname);
	cursym.stype = EXTERN+UNDEF;
	cursym.svalue = 0;
	return(lookup());
}

enter(hp)
register SYMBOL **hp;
{
	register SYMBOL *sp;

	if (*hp==0) {
		if ((nextsym-symtab)>=NSYM)
			error(1, "Symbol table overflow");
		if ((nextsym-symtab)>=nsym) {
			if (-1==sbrk(NSYM/5 * sizeof(*symtab))) error(1,"Memory overflow");
			nsym += NSYM/5;
		}
		*hp = lastsym = sp = nextsym++;
		cp8c(cursym.sname, sp->sname);
		sp->stype = cursym.stype;
		sp->symhash = hp-hshtab;
		sp->svalue = cursym.svalue;
		return(1);
	} else {
		lastsym = *hp;
		return(0);
	}
}

symreloc()
{
	if(RFflag) return;
	switch (cursym.stype & 017) {

	case TEXT:
	case EXTERN+TEXT:
		cursym.svalue += ctrel;
		return;

	case DATA:
	case EXTERN+DATA:
		cursym.svalue += cdrel;
		return;

	case BSS:
	case EXTERN+BSS:
		cursym.svalue += cbrel;
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
	if (errlev==0)
		printf("ld:");
	if (filname) {
		printf("%s", filname);
		if (archdr.ar_name[0])
			printf("(%.14s)", archdr.ar_name);
		printf(": ");
	}
	printf("%s\n", s);
	if (n)
		delexit();
	errlev = 2;
}

SYMBOL *
lookloc(lp, r)
register LOCAL *lp;
{
	register LOCAL *clp;
	register sn;

	sn = r;
	for (clp = local; clp<lp; clp++)
		if (clp->locindex == sn)
			return(clp->locsymbol);
	error(1, "Local symbol botch");
}

readhdr(loc)
long loc;
{
	long *p; int i;
	dseek(&text, loc, (long)sizeof(filhdr));
	mget((short *)&filhdr, sizeof(filhdr), &text);
#ifndef vax
	for (p= &filhdr,i=8;--i>=0;) fixl(p++);
#endif
	if (filhdr.a_magic!=A_MAGIC1 && filhdr.a_magic!=A_MAGIC2 &&
		filhdr.a_magic!=A_MAGIC3 && filhdr.a_magic!=A_MAGIC4)
			error(1,"Bad magic number");
	if (filhdr.a_text&01 || filhdr.a_data&01) {
		printf("tsize=%X  dsize=%X\n",filhdr.a_text,filhdr.a_data);
		error(1, "Text/data size odd");
	}
	filhdr.a_bss = round(filhdr.a_bss, FW);
	if (filhdr.a_magic == NMAGIC) {
		cdrel = -round(filhdr.a_text, PAGRND);
		cbrel = cdrel - filhdr.a_data;
	} else if (filhdr.a_magic == OMAGIC) {
		cdrel = -filhdr.a_text;
		cbrel = cdrel - filhdr.a_data;
	} else
		error(1, "Bad format");
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

eq(s1, s2)
STRING s1; 
STRING s2;
{
	while (*s1==*s2++)
		if ((*s1++)==0)
			return(TRUE);
	return(FALSE);
}

long
round(v, r)
long v;
unsigned r;
{
	v += r;
	v &= ~(long)r;
	return(v);
}

puts(w, f)
FILE *f; short w; {
fwrite(&w,sizeof(short),1,f);
}

putb(w, f)
FILE *f; char w; {
fwrite(&w,sizeof(char),1,f);
}

put3(w, f)
FILE *f; long w; {
fwrite(&w,3,1,f);
}

putl(w, f)
FILE *f; long w; {
#ifndef vax
	fixl(&w);
#endif
	fwrite(&w,sizeof(long),1,f);
}
static fundament(name)
STRING name;
{
	RFflag = TRUE;
	load1arg(name);
	trsize = drsize = tsize = dsize = bsize = ctrel = cdrel = cbrel = 0;
	RFflag = FALSE;
	addsym = nextsym;
}
fund2(name)
STRING name;
{
	RFflag = TRUE;
	load2arg(name);
	RFflag = FALSE;
}
