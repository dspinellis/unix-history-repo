#ifndef lint
static char sccsid[] = "@(#)cpp.c	1.21 7/24/90";
#endif lint

#ifdef FLEXNAMES
#define	NCPS	128
#else
#define	NCPS	8
#endif

#include <sys/param.h>
#include <stdio.h>
#include <ctype.h>
#include "pathnames.h"

/* C command
/* written by John F. Reiser
/* July/August 1978
*/

#define STATIC

#define FIRSTOPEN -2
#define STDIN 0
#define READ 0
#define WRITE 1
#define SALT '#'
#if !defined BUFSIZ || BUFSIZ < 8192
#undef BUFSIZ
#define BUFSIZ 8192
#endif

char *pbeg,*pbuf,*pend;
char *outp,*inp;
char *newp;
char cinit;

/* some code depends on whether characters are sign or zero extended */
/*	#if '\377' < 0		not used here, old cpp doesn't understand */
#if pdp11 | vax | mc68000 | tahoe
#define COFF 128
#else
#define COFF 0
#endif

# if gcos
#define ALFSIZ 512	/* alphabet size */
# else
#define ALFSIZ 256	/* alphabet size */
# endif
char macbit[ALFSIZ+11];
char toktyp[ALFSIZ];
#define BLANK 1
#define IDENT 2
#define NUMBR 3

/* a superimposed code is used to reduce the number of calls to the
/* symbol table lookup routine.  (if the kth character of an identifier
/* is 'a' and there are no macro names whose kth character is 'a'
/* then the identifier cannot be a macro name, hence there is no need
/* to look in the symbol table.)  'scw1' enables the test based on
/* single characters and their position in the identifier.  'scw2'
/* enables the test based on adjacent pairs of characters and their
/* position in the identifier.  scw1 typically costs 1 indexed fetch,
/* an AND, and a jump per character of identifier, until the identifier
/* is known as a non-macro name or until the end of the identifier.
/* scw1 is inexpensive.  scw2 typically costs 4 indexed fetches,
/* an add, an AND, and a jump per character of identifier, but it is also
/* slightly more effective at reducing symbol table searches.
/* scw2 usually costs too much because the symbol table search is
/* usually short; but if symbol table search should become expensive,
/* the code is here.
/* using both scw1 and scw2 is of dubious value.
*/
#define scw1 1
#define scw2 0

#if scw2
char t21[ALFSIZ],t22[ALFSIZ],t23[ALFSIZ+NCPS];
#endif

#if scw1
#define b0 1
#define b1 2
#define b2 4
#define b3 8
#define b4 16
#define b5 32
#define b6 64
#define b7 128
#endif

#define IB 1
#define SB 2
#define NB 4
#define CB 8
#define QB 16
#define WB 32
char fastab[ALFSIZ];
char slotab[ALFSIZ];
char *ptrtab;
#define isslo (ptrtab==(slotab+COFF))
#define isid(a)  ((fastab+COFF)[a]&IB)
#define isspc(a) (ptrtab[a]&SB)
#define isnum(a) ((fastab+COFF)[a]&NB)
#define iscom(a) ((fastab+COFF)[a]&CB)
#define isquo(a) ((fastab+COFF)[a]&QB)
#define iswarn(a) ((fastab+COFF)[a]&WB)

#define eob(a) ((a)>=pend)
#define bob(a) (pbeg>=(a))

# define cputc(a,b)	if(!flslvl) putc(a,b)

char buffer[NCPS+BUFSIZ+BUFSIZ+NCPS];

char	*lastcopy;

char *malloc(), *realloc();

# define DROP 0xFE	/* special character not legal ASCII or EBCDIC */
# define WARN DROP
# define SAME 0
# define MAXINC 15
# define MAXFRE 14	/* max buffers of macro pushback */
# define MAXFRM 31	/* max number of formals/actuals to a macro */

static char warnc = WARN;

int mactop,fretop;
char *instack[MAXFRE],*bufstack[MAXFRE],*endbuf[MAXFRE];

int plvl;	/* parenthesis level during scan for macro actuals */
int maclin;	/* line number of macro call requiring actuals */
char *macfil;	/* file name of macro call requiring actuals */
char *macnam;	/* name of macro requiring actuals */
int maclvl;	/* # calls since last decrease in nesting level */
char *macforw;	/* pointer which must be exceeded to decrease nesting level */
int macdam;	/* offset to macforw due to buffer shifting */

#if tgp
int tgpscan;	/* flag for dump(); */
#endif

STATIC	int	inctop[MAXINC];
STATIC	char	*fnames[MAXINC];
STATIC	char	*dirnams[MAXINC];	/* actual directory of #include files */
STATIC	int	fins[MAXINC];
STATIC	int	lineno[MAXINC];

STATIC	char	*dirs[10];	/* -I and <> directories */
char *strdex(), *copy(), *subst(), *trmdir();
struct symtab *stsym();
STATIC	int	fin	= FIRSTOPEN;
STATIC	FILE	*fout	= stdout;
STATIC	int	nd	= 1;
STATIC	int	pflag;	/* don't put out lines "# 12 foo.c" */
int	passcom;	/* don't delete comments */
int	incomment;	/* True if parsing a comment */
STATIC	int rflag;	/* allow macro recursion */
STATIC	int mflag;	/* generate makefile dependencies */
STATIC	char *infile;	/* name of .o file to build dependencies from */
STATIC 	FILE *mout;	/* file to place dependencies on */
#define START 1
#define CONT  2
#define BACK  3
STATIC	int	ifno;
# define NPREDEF 30
STATIC	char *prespc[NPREDEF];
STATIC	char **predef = prespc;
STATIC	char *punspc[NPREDEF];
STATIC	char **prund = punspc;
STATIC	int	exfail;
struct symtab {
	char	*name;
	char	*value;
} *lastsym, *lookup(), *slookup();

# if gcos
#include <setjmp.h>
static jmp_buf env;
# define main	mainpp
# undef exit
# define exit(S)	longjmp(env, 1)
# define open(S,D)	fileno(fopen(S, "r"))
# define close(F)	fclose(_f[F])
extern FILE *_f[];
# define symsiz 500
# else
# define symsiz 5000		/* std = 500, wnj aug 1979 */
# endif
STATIC	struct symtab stab[symsiz];

STATIC	struct symtab *defloc;
STATIC	struct symtab *udfloc;
STATIC	struct symtab *incloc;
STATIC	struct symtab *ifloc;
STATIC	struct symtab *elsloc;
STATIC	struct symtab *eifloc;
STATIC	struct symtab *ifdloc;
STATIC	struct symtab *ifnloc;
STATIC	struct symtab *ysysloc;
STATIC	struct symtab *varloc;
STATIC	struct symtab *lneloc;
STATIC	struct symtab *ulnloc;
STATIC	struct symtab *uflloc;
STATIC	struct symtab *identloc;	/* Sys 5r3 compatibility */
STATIC	int	trulvl;
STATIC	int	flslvl;

sayline(where)
	int where;
{
	if (mflag && where==START) fprintf(mout, "%s: %s\n", infile, fnames[ifno]);
	if (pflag==0) fprintf(fout,"# %d \"%s\"\n", lineno[ifno], fnames[ifno]);
}

/* data structure guide
/*
/* most of the scanning takes place in the buffer:
/*
/*  (low address)                                             (high address)
/*  pbeg                           pbuf                                 pend
/*  |      <-- BUFSIZ chars -->      |         <-- BUFSIZ chars -->        |
/*  _______________________________________________________________________
/* |_______________________________________________________________________|
/*          |               |               |
/*          |<-- waiting -->|               |<-- waiting -->
/*          |    to be      |<-- current -->|    to be
/*          |    written    |    token      |    scanned
/*          |               |               |
/*          outp            inp             p
/*
/*  *outp   first char not yet written to output file
/*  *inp    first char of current token
/*  *p      first char not yet scanned
/*
/* macro expansion: write from *outp to *inp (chars waiting to be written),
/* ignore from *inp to *p (chars of the macro call), place generated
/* characters in front of *p (in reverse order), update pointers,
/* resume scanning.
/*
/* symbol table pointers point to just beyond the end of macro definitions;
/* the first preceding character is the number of formal parameters.
/* the appearance of a formal in the body of a definition is marked by
/* 2 chars: the char WARN, and a char containing the parameter number.
/* the first char of a definition is preceded by a zero character.
/*
/* when macro expansion attempts to back up over the beginning of the
/* buffer, some characters preceding *pend are saved in a side buffer,
/* the address of the side buffer is put on 'instack', and the rest
/* of the main buffer is moved to the right.  the end of the saved buffer
/* is kept in 'endbuf' since there may be nulls in the saved buffer.
/*
/* similar action is taken when an 'include' statement is processed,
/* except that the main buffer must be completely emptied.  the array
/* element 'inctop[ifno]' records the last side buffer saved when
/* file 'ifno' was included.  these buffers remain dormant while
/* the file is being read, and are reactivated at end-of-file.
/*
/* instack[0 : mactop] holds the addresses of all pending side buffers.
/* instack[inctop[ifno]+1 : mactop-1] holds the addresses of the side
/* buffers which are "live"; the side buffers instack[0 : inctop[ifno]]
/* are dormant, waiting for end-of-file on the current file.
/*
/* space for side buffers is obtained from 'malloc' and is never returned.
/* bufstack[0:fretop-1] holds addresses of side buffers which
/* are available for use.
*/

dump() {
/* write part of buffer which lies between  outp  and  inp .
/* this should be a direct call to 'write', but the system slows to a crawl
/* if it has to do an unaligned copy.  thus we buffer.  this silly loop
/* is 15% of the total time, thus even the 'putc' macro is too slow.
*/
	register char *p1,*p2; register FILE *f;
	if ((p1=outp)==inp || flslvl!=0) return;
#if tgp
#define MAXOUT 80
	if (!tgpscan) {/* scan again to insure <= MAXOUT chars between linefeeds */
		register char c,*pblank; char savc,stopc,brk;
		tgpscan=1; brk=stopc=pblank=0; p2=inp; savc= *p2; *p2='\0';
		while (c= *p1++) {
			if (c=='\\') c= *p1++;
			if (stopc==c) stopc=0;
			else if (c=='"' || c=='\'') stopc=c;
			if (p1-outp>MAXOUT && pblank!=0) {
				*pblank++='\n'; inp=pblank; dump(); brk=1; pblank=0;
			}
			if (c==' ' && stopc==0) pblank=p1-1;
		}
		if (brk) sayline(CONT);
		*p2=savc; inp=p2; p1=outp; tgpscan=0;
	}
#endif
	f=fout;
# if gcos
/* filter out "$ program c" card if first line of input */
/* gmatch is a simple pattern matcher in the GCOS Standard Library */
{	static int gmfirst = 0;
	if (!gmfirst) {
		++gmfirst;
		if (gmatch(p1, "^$*program[ \t]*c*"))
			p1 = strdex(p1, '\n');
	}
}
# endif
	while (p1<inp) putc(*p1++,f);
	outp=p1;
}

char *
refill(p) register char *p; {
/* dump buffer.  save chars from inp to p.  read into buffer at pbuf,
/* contiguous with p.  update pointers, return new p.
*/
	register char *np,*op; register int ninbuf;
	dump(); np=pbuf-(p-inp); op=inp;
	if (bob(np+1)) {pperror("token too long"); np=pbeg; p=inp+BUFSIZ;}
	macdam += np-inp; outp=inp=np;
	while (op<p) *np++= *op++;
	p=np;
	for (;;) {
		if (mactop>inctop[ifno]) {/* retrieve hunk of pushed-back macro text */
			op=instack[--mactop]; np=pbuf;
			do {while (*np++= *op++);} while (op<endbuf[mactop]); pend=np-1;
			/* make buffer space avail for 'include' processing */
			if (fretop<MAXFRE) bufstack[fretop++]=instack[mactop];
			return(p);
		} else {/* get more text from file(s) */
			maclvl=0;
			if (0<(ninbuf=read(fin,pbuf,BUFSIZ))) {
				pend=pbuf+ninbuf; *pend='\0';
				return(p);
			}
			/* end of #include file */
			if (ifno==0) {/* end of input */
				if (plvl!=0) {
					int n=plvl,tlin=lineno[ifno]; char *tfil=fnames[ifno];
					lineno[ifno]=maclin; fnames[ifno]=macfil;
					pperror("%s: unterminated macro call",macnam);
					lineno[ifno]=tlin; fnames[ifno]=tfil;
					np=p; *np++='\n';	/* shut off unterminated quoted string */
					while (--n>=0) *np++=')';	/* supply missing parens */
					pend=np; *np='\0'; if (plvl<0) plvl=0;
					return(p);
				}
				if (trulvl || flslvl)
					if (incomment)
						pperror("unterminated comment");
					else
						pperror("missing endif");
				inp=p; dump(); exit(exfail);
			}
			close(fin); fin=fins[--ifno]; dirs[0]=dirnams[ifno]; sayline(BACK);
		}
	}
}

#define BEG 0
#define LF 1

char *
cotoken(p) register char *p; {
	register int c,i; char quoc;
	static int state = BEG;

	if (state!=BEG) goto prevlf;
for (;;) {
again:
	while (!isspc(*p++));
	switch (*(inp=p-1)) {
	case 0: {
		if (eob(--p)) {p=refill(p); goto again;}
		else ++p; /* ignore null byte */
	} break;
	case '|': case '&': for (;;) {/* sloscan only */
		if (*p++== *inp) break;
		if (eob(--p)) p=refill(p);
		else break;
	} break;
	case '=': case '!': for (;;) {/* sloscan only */
		if (*p++=='=') break;
		if (eob(--p)) p=refill(p);
		else break;
	} break;
	case '<': case '>': for (;;) {/* sloscan only */
		if (*p++=='=' || p[-2]==p[-1]) break;
		if (eob(--p)) p=refill(p);
		else break;
	} break;
	case '\\': for (;;) {
		if (*p++=='\n') {++lineno[ifno]; break;}
		if (eob(--p)) p=refill(p);
		else {++p; break;}
	} break;
	case '/': for (;;) {
		if (*p++=='*') {/* comment */
			incomment++;
			if (!passcom) {inp=p-2; dump(); ++flslvl;}
			for (;;) {
				while (!iscom(*p++));
				if (p[-1]=='*') for (;;) {
					if (*p++=='/') goto endcom;
					if (eob(--p)) {
						if (!passcom) {inp=p; p=refill(p);}
						else if ((p-inp)>=BUFSIZ) {/* split long comment */
							inp=p; p=refill(p);	/* last char written is '*' */
							cputc('/',fout);	/* terminate first part */
							/* and fake start of 2nd */
							outp=inp=p-=3; *p++='/'; *p++='*'; *p++='*';
						} else p=refill(p);
					} else break;
				} else if (p[-1]=='\n') {
					++lineno[ifno]; if (!passcom) putc('\n',fout);
				} else if (eob(--p)) {
					if (!passcom) {inp=p; p=refill(p);}
					else if ((p-inp)>=BUFSIZ) {/* split long comment */
						inp=p; p=refill(p);
						cputc('*',fout); cputc('/',fout);
						outp=inp=p-=2; *p++='/'; *p++='*';
					} else p=refill(p);
				} else ++p; /* ignore null byte */
			}
		endcom:
			incomment--;
			if (!passcom) {outp=inp=p; --flslvl; goto again;}
			break;
		}
		if (eob(--p)) p=refill(p);
		else break;
	} break;
# if gcos
	case '`':
# endif
	case '"': case '\'': {
		quoc=p[-1];
		for (;;) {
			while (!isquo(*p++));
			if (p[-1]==quoc) break;
			if (p[-1]=='\n') {--p; break;} /* bare \n terminates quotation */
			if (p[-1]=='\\') for (;;) {
				if (*p++=='\n') {++lineno[ifno]; break;} /* escaped \n ignored */
				if (eob(--p)) p=refill(p);
				else {++p; break;}
			} else if (eob(--p)) p=refill(p);
			else ++p;	/* it was a different quote character */
		}
	} break;
	case '\n': {
		++lineno[ifno]; if (isslo) {state=LF; return(p);}
prevlf:
		state=BEG;
		for (;;) {
			if (*p++=='#') return(p);
			if (eob(inp= --p)) p=refill(p);
			else goto again;
		}
	} break;
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	for (;;) {
		while (isnum(*p++));
		if (eob(--p)) p=refill(p);
		else break;
	} break;
	case 'A': case 'B': case 'C': case 'D': case 'E':
	case 'F': case 'G': case 'H': case 'I': case 'J':
	case 'K': case 'L': case 'M': case 'N': case 'O':
	case 'P': case 'Q': case 'R': case 'S': case 'T':
	case 'U': case 'V': case 'W': case 'X': case 'Y':
	case 'Z': case '_':
	case 'a': case 'b': case 'c': case 'd': case 'e':
	case 'f': case 'g': case 'h': case 'i': case 'j':
	case 'k': case 'l': case 'm': case 'n': case 'o':
	case 'p': case 'q': case 'r': case 's': case 't':
	case 'u': case 'v': case 'w': case 'x': case 'y':
	case 'z':
#if scw1
#define tmac1(c,bit) if (!xmac1(c,bit,&)) goto nomac
#define xmac1(c,bit,op) ((macbit+COFF)[c] op (bit))
#else
#define tmac1(c,bit)
#define xmac1(c,bit,op)
#endif

#if scw2
#define tmac2(c0,c1,cpos) if (!xmac2(c0,c1,cpos,&)) goto nomac
#define xmac2(c0,c1,cpos,op)\
	((macbit+COFF)[(t21+COFF)[c0]+(t22+COFF)[c1]] op (t23+COFF+cpos)[c0])
#else
#define tmac2(c0,c1,cpos)
#define xmac2(c0,c1,cpos,op)
#endif

	if (flslvl) goto nomac;
	for (;;) {
		c= p[-1];                          tmac1(c,b0);
		i= *p++; if (!isid(i)) goto endid; tmac1(i,b1); tmac2(c,i,0);
		c= *p++; if (!isid(c)) goto endid; tmac1(c,b2); tmac2(i,c,1);
		i= *p++; if (!isid(i)) goto endid; tmac1(i,b3); tmac2(c,i,2);
		c= *p++; if (!isid(c)) goto endid; tmac1(c,b4); tmac2(i,c,3);
		i= *p++; if (!isid(i)) goto endid; tmac1(i,b5); tmac2(c,i,4);
		c= *p++; if (!isid(c)) goto endid; tmac1(c,b6); tmac2(i,c,5);
		i= *p++; if (!isid(i)) goto endid; tmac1(i,b7); tmac2(c,i,6);
		                                                tmac2(i,0,7);
		while (isid(*p++));
		if (eob(--p)) {refill(p); p=inp+1; continue;}
		goto lokid;
	endid:
		if (eob(--p)) {refill(p); p=inp+1; continue;}
		tmac2(p[-1],0,-1+(p-inp));
	lokid:
		slookup(inp,p,0); if (newp) {p=newp; goto again;}
		else break;
	nomac:
		while (isid(*p++));
		if (eob(--p)) {p=refill(p); goto nomac;}
		else break;
	} break;
	} /* end of switch */
	
	if (isslo) return(p);
} /* end of infinite loop */
}

char *
skipbl(p) register char *p; {/* get next non-blank token */
	do {outp=inp=p; p=cotoken(p);} while ((toktyp+COFF)[*inp]==BLANK);
	return(p);
}

char *
unfill(p) register char *p; {
/* take <= BUFSIZ chars from right end of buffer and put them on instack .
/* slide rest of buffer to the right, update pointers, return new p.
*/
	register char *np,*op; register int d;
	if (mactop>=MAXFRE) {
		pperror("%s: too much pushback",macnam);
		p=inp=pend; dump();	/* begin flushing pushback */
		while (mactop>inctop[ifno]) {p=refill(p); p=inp=pend; dump();}
	}
	if (fretop>0) np=bufstack[--fretop];
	else {
		np=malloc(BUFSIZ+1);
		if (np==NULL) {pperror("no space"); exit(exfail);}
		np[BUFSIZ]='\0';
	}
	instack[mactop]=np; op=pend-BUFSIZ; if (op<p) op=p;
	for (;;) {while (*np++= *op++); if (eob(op)) break;} /* out with old */
	endbuf[mactop++]=np;	/* mark end of saved text */
	np=pbuf+BUFSIZ; op=pend-BUFSIZ; pend=np; if (op<p) op=p;
	while (outp<op) *--np= *--op; /* slide over new */
	if (bob(np)) pperror("token too long");
	d=np-outp; outp+=d; inp+=d; macdam+=d; return(p+d);
}

char *
doincl(p) register char *p; {
	int filok,inctype;
	register char *cp; char **dirp,*nfil; char filname[BUFSIZ];

	p=skipbl(p); cp=filname;
	if (*inp++=='<') {/* special <> syntax */
		inctype=1;
		++flslvl;	/* prevent macro expansion */
		for (;;) {
			outp=inp=p; p=cotoken(p);
			if (*inp=='\n') {--p; *cp='\0'; break;}
			if (*inp=='>') {      *cp='\0'; break;}
# ifdef gimpel
			if (*inp=='.' && !intss()) *inp='#';
# endif
			while (inp<p) *cp++= *inp++;
		}
		--flslvl;	/* reenable macro expansion */
	} else if (inp[-1]=='"') {/* regular "" syntax */
		inctype=0;
# ifdef gimpel
		while (inp<p) {if (*inp=='.' && !intss()) *inp='#'; *cp++= *inp++;}
# else
		while (inp<p) *cp++= *inp++;
# endif
		if (*--cp=='"') *cp='\0';
	} else {pperror("bad include syntax",0); inctype=2;}
	/* flush current file to \n , then write \n */
	++flslvl; do {outp=inp=p; p=cotoken(p);} while (*inp!='\n'); --flslvl;
	inp=p; dump(); if (inctype==2) return(p);
	/* look for included file */
	if (ifno+1 >=MAXINC) {
		pperror("Unreasonable include nesting",0); return(p);
	}
	if((nfil=malloc(BUFSIZ))==NULL) {pperror("no space"); exit(exfail);}
	filok=0;
	for (dirp=dirs+inctype; *dirp; ++dirp) {
		if (
# if gcos
			strdex(filname, '/')
# else
			filname[0]=='/' 
# endif
				|| **dirp=='\0') strcpy(nfil,filname);
		else {
			strcpy(nfil,*dirp);
# if unix || gcos
			strcat(nfil,"/");
# endif
#ifdef ibm
#ifndef gimpel
			strcat(nfil,".");
#endif
#endif
			strcat(nfil,filname);
		}
		if (0<(fins[ifno+1]=open(nfil,READ))) {
			filok=1; fin=fins[++ifno]; break;
		}
	}
	if(filok==0){pperror("Can't find include file %s",filname);free(nfil);}
	else {
		nfil=realloc(nfil,strlen(nfil)+1);
		lineno[ifno]=1; fnames[ifno]=nfil;
		dirnams[ifno]=dirs[0]=trmdir(copy(nfil));
		sayline(START);
		/* save current contents of buffer */
		while (!eob(p)) p=unfill(p);
		inctop[ifno]=mactop;
	}
	return(p);
}

equfrm(a,p1,p2) register char *a,*p1,*p2; {
	register char c; int flag;
	c= *p2; *p2='\0';
	flag=strcmp(a,p1); *p2=c; return(flag==SAME);
}

char *
dodef(p) char *p; {/* process '#define' */
	register char *pin,*psav,*cf;
	char **pf,**qf; int b,c,params; struct symtab *np;
	char *oldval;
	char *space, *newspace;
	char *formal[MAXFRM]; /* formal[n] is name of nth formal */
	char formtxt[BUFSIZ]; /* space for formal names */
	int opt_passcom=passcom;

	passcom=0;	/* don't put comments in macro expansions */

	++flslvl; /* prevent macro expansion during 'define' */
	p=skipbl(p); pin=inp;
	if ((toktyp+COFF)[*pin]!=IDENT) {
		ppwarn("illegal macro name"); while (*inp!='\n') p=skipbl(p);
		passcom=opt_passcom; return(p);
	}
	np=slookup(pin,p,1);
	if (oldval=np->value) free(lastcopy);	/* was previously defined */
	b=1; cf=pin;
	while (cf<p) {/* update macbit */
		c= *cf++; xmac1(c,b,|=); b=(b+b)&0xFF;
		if (cf!=p) xmac2(c,*cf,-1+(cf-pin),|=);
		else xmac2(c,0,-1+(cf-pin),|=);
	}
	params=0; outp=inp=p; p=cotoken(p); pin=inp;
	if (*pin=='(') {/* with parameters; identify the formals */
		cf=formtxt; pf=formal;
		for (;;) {
			p=skipbl(p); pin=inp;
			if (*pin=='\n') {
				--lineno[ifno]; --p; pperror("%s: missing )",np->name); break;
			}
			if (*pin==')') break;
			if (*pin==',') continue;
			if ((toktyp+COFF)[*pin]!=IDENT) {
				c= *p; *p='\0'; pperror("bad formal: %s",pin); *p=c;
			} else if (pf>= &formal[MAXFRM]) {
				c= *p; *p='\0'; pperror("too many formals: %s",pin); *p=c;
			} else {
				*pf++=cf; while (pin<p) *cf++= *pin++; *cf++='\0'; ++params;
			}
		}
		if (params==0) --params; /* #define foo() ... */
	} else if (*pin=='\n') {--lineno[ifno]; --p;}
	/* remember beginning of macro body, so that we can
	/* warn if a redefinition is different from old value.
	*/
	space=psav=malloc(BUFSIZ);
	if (space==NULL) {
		pperror("too much defining");
		passcom=opt_passcom;
		return(p);
	}
	*psav++ = '\0';
	for (;;) {/* accumulate definition until linefeed */
		outp=inp=p; p=cotoken(p); pin=inp;
		if (*pin=='\\' && pin[1]=='\n') {putc('\n',fout); continue;}	/* ignore escaped lf */
		if (*pin=='\n') break;
		if (params) {/* mark the appearance of formals in the definiton */
			if ((toktyp+COFF)[*pin]==IDENT) {
				for (qf=pf; --qf>=formal; ) {
					if (equfrm(*qf,pin,p)) {
						*psav++=qf-formal+1; *psav++=WARN; pin=p; break;
					}
				}
			} else if (*pin=='"' || *pin=='\''
# if gcos
					|| *pin=='`'
# endif
						) {/* inside quotation marks, too */
				char quoc= *pin;
				for (*psav++= *pin++; pin<p && *pin!=quoc; ) {
					while (pin<p && !isid(*pin)) *psav++= *pin++;
					cf=pin; while (cf<p && isid(*cf)) ++cf;
					for (qf=pf; --qf>=formal; ) {
						if (equfrm(*qf,pin,cf)) {
							*psav++=qf-formal+1; *psav++=WARN; pin=cf; break;
						}
					}
					while (pin<cf) *psav++= *pin++;
				}
			}
		}
		while (pin<p) *psav++= *pin++;
	}
	*psav++=params; *psav++='\0';
	if ((cf=oldval)!=NULL) {/* redefinition */
		--cf;	/* skip no. of params, which may be zero */
		while (*--cf);	/* go back to the beginning */
		if (0!=strcmp(++cf,space+1)) {/* redefinition different from old */
			--lineno[ifno]; ppwarn("%s redefined",np->name); ++lineno[ifno];
			np->value=psav-1;
		} else free(space); /* identical redef.; reclaim space */
	} else np->value=psav-1;
	--flslvl; inp=pin;
	if (np->value == psav-1) {
		newspace = realloc(space, psav-space);
		if (newspace==NULL) {pperror("no space"); exit(exfail);}
		/*
		 * Adjust pointer in case this moved.
		 */
		np->value += newspace-space;
	}
	passcom=opt_passcom;
	return(p);
}

#define fasscan() ptrtab=fastab+COFF
#define sloscan() ptrtab=slotab+COFF

char *
control(p) register char *p; {/* find and handle preprocessor control lines */
	register struct symtab *np;
for (;;) {
	fasscan(); p=cotoken(p); if (*inp=='\n') ++inp; dump();
	sloscan(); p=skipbl(p);
	*--inp=SALT; outp=inp; ++flslvl; np=slookup(inp,p,0); --flslvl;
	if (np==defloc) {/* define */
		if (flslvl==0) {p=dodef(p); continue;}
	} else if (np==incloc) {/* include */
		if (flslvl==0) {p=doincl(p); continue;}
	} else if (np==ifnloc) {/* ifndef */
		++flslvl; p=skipbl(p); np=slookup(inp,p,0); --flslvl;
		if (flslvl==0 && np->value==0) ++trulvl;
		else ++flslvl;
	} else if (np==ifdloc) {/* ifdef */
		++flslvl; p=skipbl(p); np=slookup(inp,p,0); --flslvl;
		if (flslvl==0 && np->value!=0) ++trulvl;
		else ++flslvl;
	} else if (np==eifloc) {/* endif */
		if (flslvl) {if (--flslvl==0) sayline(CONT);}
		else if (trulvl) --trulvl;
		else pperror("If-less endif",0);
	} else if (np==elsloc) {/* else */
		if (flslvl) {
			if (--flslvl!=0) ++flslvl;
			else {++trulvl; sayline(CONT);}
		}
		else if (trulvl) {++flslvl; --trulvl;}
		else pperror("If-less else",0);
	} else if (np==udfloc) {/* undefine */
		if (flslvl==0) {
			++flslvl; p=skipbl(p); slookup(inp,p,DROP); --flslvl;
		}
	} else if (np==ifloc) {/* if */
#if tgp
		pperror(" IF not implemented, true assumed", 0);
		if (flslvl==0) ++trulvl; else ++flslvl;
#else
		newp=p;
		if (flslvl==0 && yyparse()) ++trulvl; else ++flslvl;
		p=newp;
#endif
	} else if (np==lneloc) {/* line */
		if (flslvl==0 && pflag==0) {
			char *savestring();
			char filename[BUFSIZ], *cp = filename;
			outp=inp=p; *--outp='#';
			/* Find the line number.. */
			do {
				p = cotoken(p);
			} while (!isnum(*inp) && *inp != '\n');
			if (isnum(*inp))
				lineno[ifno] = atoi(inp)-1;
			/* Skip over the blank token */
			inp = p;
			if (*inp != '\n') {
				p = cotoken(p); inp = p;
			}
			/* Add a quote if missing..  */
			if (*inp != '\n') {
				p = cotoken(p);
				/* Add a quote if missing..  */
				if (*inp == '"')
					inp++;
				else {
					dump();
					*--outp = '"';
				}
				while (*inp != '\n') {
					while (inp < p && *inp != '"' &&
						cp < filename+sizeof(filename))
						*cp++ = *inp++;
					if (*inp == '"')
						break;
					inp = p; p = cotoken(p);
				}
				fnames[ifno] = savestring(filename, cp);
				/* Add a quote if missing..  */
				if (*inp != '"') {
					dump();
					*--outp = '"';
				}
			}
			while (*inp != '\n')
				p = cotoken(p);
			continue;
		}
	} else if (np==identloc) {/* ident (for Sys 5r3 compat) */
		while(*inp!='\n') p=cotoken(p);
	} else if (*++inp=='\n') outp=inp;	/* allows blank line after # */
	else pperror("undefined control",0);
	/* flush to lf */
	++flslvl; while (*inp!='\n') {outp=inp=p; p=cotoken(p);} --flslvl;
}
}

char *
savestring(start, finish)
	register char *start, *finish;
{
	char *retbuf;
	register char *cp;

	retbuf = (char *) calloc(finish - start + 1, sizeof (char));
	cp = retbuf;
	while (start < finish)
		*cp++ = *start++;
	*cp = 0;
	return(retbuf);
}

struct symtab *
stsym(s) register char *s; {
	char buf[BUFSIZ]; register char *p;

	/* make definition look exactly like end of #define line */
	/* copy to avoid running off end of world when param list is at end */
	p=buf; while (*p++= *s++);
	p=buf; while (isid(*p++)); /* skip first identifier */
	if (*--p=='=') {*p++=' '; while (*p++);}
	else {s=" 1"; while (*p++= *s++);}
	pend=p; *--p='\n';
	sloscan(); dodef(buf); return(lastsym);
}

struct symtab *
ppsym(s) char *s; {/* kluge */
	register struct symtab *sp;
	register char *name;

	cinit=SALT; sp=stsym(s); name = malloc(strlen(sp->name)+1+1);
	name[0] = '#'; strcpy(name+1, sp->name); sp->name = name;
	cinit=0; return(sp);
}

/* VARARGS1 */
pperror(s,x,y) char *s; {
	if (fnames[ifno][0]) fprintf(stderr,
# if gcos
			"*%c*   \"%s\", line ", exfail >= 0 ? 'F' : 'W',
# else
			"%s: ",
# endif
				 fnames[ifno]);
	fprintf(stderr, "%d: ",lineno[ifno]);
	fprintf(stderr, s, x, y);
	fprintf(stderr,"\n");
	++exfail;
}

yyerror(s,a,b) char *s; {
	pperror(s,a,b);
}

ppwarn(s,x) char *s; {
	int fail = exfail;
	exfail = -1;
	pperror(s,x);
	exfail = fail;
}

struct symtab *
lookup(namep, enterf)
char *namep;
{
	register char *np, *snp;
	register int c, i; int around;
	register struct symtab *sp;

	/* namep had better not be too long (currently, <=NCPS chars) */
	np=namep; around=0; i=cinit;
	while (c= *np++) i += i+c; c=i;	/* c=i for register usage on pdp11 */
	c %= symsiz; if (c<0) c += symsiz;
	sp = &stab[c];
	while (snp=sp->name) {
		np = namep;
		while (*snp++ == *np) if (*np++ == '\0') {
				if (enterf==DROP) {sp->name[0]= DROP; sp->value=0;}
				return(lastsym=sp);
			}
		if (--sp < &stab[0])
			if (around) {pperror("too many defines", 0); exit(exfail);}
			else {++around; sp = &stab[symsiz-1];}
	}
	if (enterf==1) sp->name=namep;
	return(lastsym=sp);
}

struct symtab *
slookup(p1,p2,enterf) register char *p1,*p2; int enterf;{
	register char *p3; char c2,c3; struct symtab *np;
	         c2= *p2; *p2='\0';	/* mark end of token */
	if ((p2-p1)>NCPS) p3=p1+NCPS; else p3=p2;
			 c3= *p3; *p3='\0';	/* truncate to NCPS chars or less */
	if (enterf==1) p1=copy(p1);
	np=lookup(p1,enterf); *p3=c3; *p2=c2;
	if (np->value!=0 && flslvl==0) newp=subst(p2,np);
	else newp=0;
	return(np);
}

char *
subst(p,sp) register char *p; struct symtab *sp; {
	static char match[]="%s: argument mismatch";
	register char *ca,*vp; int params;
	char *actual[MAXFRM]; /* actual[n] is text of nth actual   */
	char actused[MAXFRM]; /* for newline processing in actuals */
	char acttxt[BUFSIZ];  /* space for actuals */
	int  nlines = 0;

	if (0==(vp=sp->value)) return(p);
	if ((p-macforw)<=macdam) {
		if (++maclvl>symsiz && !rflag) {
			pperror("%s: macro recursion",sp->name); return(p);
		}
	} else maclvl=0;	/* level decreased */
	macforw=p; macdam=0;	/* new target for decrease in level */
	macnam=sp->name;
	dump();
	if (sp==ulnloc) {
		vp=acttxt; *vp++='\0';
		(void)sprintf(vp,"%d",lineno[ifno]); while (*vp++);
	} else if (sp==uflloc) {
		vp=acttxt; *vp++='\0';
		(void)sprintf(vp,"\"%s\"",fnames[ifno]); while (*vp++);
	}
	if (0!=(params= *--vp&0xFF)) {/* definition calls for params */
		register char **pa;
		ca=acttxt; pa=actual;
		if (params==0xFF) params=1;	/* #define foo() ... */
		sloscan(); ++flslvl; /* no expansion during search for actuals */
		plvl= -1;
		do p=skipbl(p); while (*inp=='\n');	/* skip \n too */
		if (*inp=='(') {
			maclin=lineno[ifno]; macfil=fnames[ifno];
			for (plvl=1; plvl!=0; ) {
				*ca++='\0';
				for (;;) {
					outp=inp=p; p=cotoken(p);
					if (*inp=='(') ++plvl;
					if (*inp==')' && --plvl==0) {--params; break;}
					if (plvl==1 && *inp==',') {--params; break;}
					while (inp<p) *ca++= *inp++;
					if (ca> &acttxt[BUFSIZ])
						pperror("%s: actuals too long",sp->name);
				}
				if (pa>= &actual[MAXFRM]) ppwarn(match,sp->name);
				else { actused[pa-actual]=0; *pa++=ca; }
			}
			nlines = lineno[ifno] - maclin;
			lineno[ifno] = maclin; /* don't count newlines here */
		}
		if (params!=0) ppwarn(match,sp->name);
		while (--params>=0) *pa++=""+1;	/* null string for missing actuals */
		--flslvl; fasscan();
	}
	for (;;) {/* push definition onto front of input stack */
		while (!iswarn(*--vp)) {
			if (bob(p)) {outp=inp=p; p=unfill(p);}
			*--p= *vp;
		}
		if (*vp==warnc) {/* insert actual param */
			ca=actual[*--vp-1];
			while (*--ca) {
				if (bob(p)) {outp=inp=p; p=unfill(p);}
				/* Actuals with newlines confuse line numbering */
				if (*ca == '\n' && actused[*vp-1])
					if (*(ca-1) == '\\') ca--;
					else *--p = ' ';
				else { *--p= *ca; if (*ca == '\n') nlines--; }
			}
			actused[*vp-1] = 1;
		} else {
			if (nlines > 0 )
				while (nlines-- > 0)
					*--p = '\n';
			break;
		}
	}
	outp=inp=p;
	return(p);
}




char *
trmdir(s) register char *s; {
	register char *p = s;
	while (*p++); --p; while (p>s && *--p!='/');
# if unix
	if (p==s) *p++='.';
# endif
	*p='\0';
	return(s);
}

STATIC char *
copy(s) register char *s; {
	register char *old;

	old = malloc(strlen(s)+1);
	if (old==NULL) {pperror("no space"); exit(exfail);}
	strcpy(old, s);
	return(lastcopy=old);
}

char *
strdex(s,c) char *s,c; {
	while (*s) if (*s++==c) return(--s);
	return(0);
}

yywrap(){ return(1); }

main(argc,argv)
	char *argv[];
{
	register int i,c;
	register char *p;
	char *tf,**cp2;

# if gcos
	if (setjmp(env)) return (exfail);
# endif
	p="_$ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
		i=0;
		while (c= *p++) {
			(fastab+COFF)[c] |= IB|NB|SB; (toktyp+COFF)[c]=IDENT;
#if scw2
			/* 53 == 63-10; digits rarely appear in identifiers,
			/* and can never be the first char of an identifier.
			/* 11 == 53*53/sizeof(macbit) .
			*/
			++i; (t21+COFF)[c]=(53*i)/11; (t22+COFF)[c]=i%11;
#endif
		}
	p="0123456789.";
		while (c= *p++) {(fastab+COFF)[c] |= NB|SB; (toktyp+COFF)[c]=NUMBR;}
# if gcos
	p="\n\"'`/\\";
# else
	p="\n\"'/\\";
# endif
		while (c= *p++) (fastab+COFF)[c] |= SB;
# if gcos
	p="\n\"'`\\";
# else
	p="\n\"'\\";
# endif
		while (c= *p++) (fastab+COFF)[c] |= QB;
	p="*\n"; while (c= *p++) (fastab+COFF)[c] |= CB;
	(fastab+COFF)[warnc] |= WB;
	(fastab+COFF)['\0'] |= CB|QB|SB|WB;
	for (i=ALFSIZ; --i>=0; ) slotab[i]=fastab[i]|SB;
	p=" \t\013\f\r";	/* note no \n;	\v not legal for vertical tab? */
		while (c= *p++) (toktyp+COFF)[c]=BLANK;
#if scw2
	for ((t23+COFF)[i=ALFSIZ+7-COFF]=1; --i>=-COFF; )
		if (((t23+COFF)[i]=(t23+COFF+1)[i]<<1)==0) (t23+COFF)[i]=1;
#endif

# if unix
	fnames[ifno=0] = ""; dirnams[0]=dirs[0]=".";
# endif
# if ibm
	fnames[ifno=0] = "";
# endif
# if gcos
	if (inquire(stdin, _TTY)) freopen("*src", "rt", stdin);
# endif
# if gimpel || gcos
	fnames[ifno=0] = (char *)inquire(stdin, _FILENAME);
	dirnams[0] = dirs[0] = trmdir(copy(fnames[0]));
# endif
	for(i=1; i<argc; i++)
		{
		switch(argv[i][0])
			{
			case '-':
# if gcos
			switch(toupper(argv[i][1])) { /* case-independent on GCOS */
# else
			switch(argv[i][1]) {
# endif
				case 'M': mflag++;
				case 'P': pflag++;
				case 'E': continue;
				case 'R': ++rflag; continue;
				case 'C': passcom++; continue;
				case 'D':
					if (predef>prespc+NPREDEF) {
						pperror("too many -D options, ignoring %s",argv[i]);
						continue;
					}
					/* ignore plain "-D" (no argument) */
					if (*(argv[i]+2)) *predef++ = argv[i]+2;
					continue;
				case 'U':
					if (prund>punspc+NPREDEF) {
						pperror("too many -U options, ignoring %s",argv[i]);
						continue;
					}
					*prund++ = argv[i]+2;
					continue;
				case 'I':
					if (nd>8) pperror("excessive -I file (%s) ignored",argv[i]);
					else dirs[nd++] = argv[i]+2;
					continue;
				case '\0': continue;
				default: 
					pperror("unknown flag %s", argv[i]);
					continue;
				}
			default:
				if (fin==FIRSTOPEN) {
					if (0>(fin=open(argv[i], READ))) {
						pperror("No source file %s",argv[i]); exit(8);
					}
					fnames[ifno]=copy(argv[i]);
					infile=copy(argv[i]);
					dirs[0]=dirnams[ifno]=trmdir(argv[i]);
# ifndef gcos
/* too dangerous to have file name in same syntactic position
   be input or output file depending on file redirections,
   so force output to stdout, willy-nilly
	[i don't see what the problem is.  jfr]
*/
				} else if (fout==stdout) {
					if (NULL==(fout=fopen(argv[i], "w"))) {
						pperror("Can't create %s", argv[i]); exit(8);
					} else fclose(stdout);
# endif
				} else pperror("extraneous name %s", argv[i]);
			}
		}
	if (fin == FIRSTOPEN)
		fin = STDIN;

	if (mflag) {
		if (infile==(char *)0) {
			fprintf(stderr,
				"no input file specified with -M flag\n");
			exit(8);
		}
		tf=(char *)rindex(infile, '.');
		if (tf==0) {
			fprintf(stderr, "missing component name on %s\n",
				infile);
			exit(8);
		}
		tf[1]='o';
		tf=(char *)rindex(infile, '/');
		if (tf!=(char *)0)
			infile = tf + 1;
		mout=fout;
		if (NULL==(fout=fopen(_PATH_DEVNULL, "w"))) {
			fprintf(stderr, "cpp: can't open %s\n", _PATH_DEVNULL);
			exit(8);
		}
	}
	fins[ifno]=fin;
	exfail = 0;
		/* after user -I files here are the standard include libraries */
# if unix
	dirs[nd++] = _PATH_INCLUDES;
# endif
# if gcos
	dirs[nd++] = "cc/include";
# endif
# if ibm
# ifndef gimpel
	dirs[nd++] = "BTL$CLIB";
# endif
# endif
# ifdef gimpel
	dirs[nd++] = intss() ?  "SYS3.C." : "" ;
# endif
	/* dirs[nd++] = "/compool"; */
	dirs[nd++] = 0;
	defloc=ppsym("define");
	udfloc=ppsym("undef");
	incloc=ppsym("include");
	elsloc=ppsym("else");
	eifloc=ppsym("endif");
	ifdloc=ppsym("ifdef");
	ifnloc=ppsym("ifndef");
	ifloc=ppsym("if");
	lneloc=ppsym("line");
	identloc=ppsym("ident");	/* Sys 5r3 compatibility */
	for (i=sizeof(macbit)/sizeof(macbit[0]); --i>=0; ) macbit[i]=0;
# if unix
	ysysloc=stsym("unix");
# endif
	ysysloc=stsym(MACHINE);
	ulnloc=stsym ("__LINE__");
	uflloc=stsym ("__FILE__");

	tf=fnames[ifno]; fnames[ifno]="command line"; lineno[ifno]=1;
	cp2=prespc;
	while (cp2<predef) stsym(*cp2++);
	cp2=punspc;
	while (cp2<prund) {
		if (p=strdex(*cp2, '=')) *p++='\0';
		lookup(*cp2++, DROP);
	}
	fnames[ifno]=tf;
	pbeg=buffer+NCPS; pbuf=pbeg+BUFSIZ; pend=pbuf+BUFSIZ;

	trulvl = 0; flslvl = 0;
	lineno[0] = 1; sayline(START);
	outp=inp=pend;
	control(pend);
	return (exfail);
}
