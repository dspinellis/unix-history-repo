static	char *sccsid = "@(#)sort.c	4.13 (Berkeley) 11/12/87";
#include <sys/param.h>
#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <sys/stat.h>

#define	L	2048
#define	N	7
#define	C	20
#ifndef pdp11
#define	MEM	(128*2048)
#else
#define	MEM	(16*2048)
#endif
#define NF	10

#define rline(mp)	(fgets((mp)->l, L, (mp)->b) == NULL)

FILE	*is, *os;
char	*dirtry[] = {"/usr/tmp", "/tmp", NULL};
char	**dirs;
char	file1[MAXPATHLEN];
char	*file = file1;
char	*filep;
int	nfiles;
unsigned	nlines;
unsigned	ntext;
int	*lspace;
char	*tspace;
int	cmp(), cmpa();
int	(*compare)() = cmpa;
char	*eol();
int	term();
int 	mflg;
int	cflg;
int	uflg;
char	*outfil;
int unsafeout;	/*kludge to assure -m -o works*/
char	tabchar;
int 	eargc;
char	**eargv;

char zero[256];

char	fold[256] = {
	0200,0201,0202,0203,0204,0205,0206,0207,
	0210,0211,0212,0213,0214,0215,0216,0217,
	0220,0221,0222,0223,0224,0225,0226,0227,
	0230,0231,0232,0233,0234,0235,0236,0237,
	0240,0241,0242,0243,0244,0245,0246,0247,
	0250,0251,0252,0253,0254,0255,0256,0257,
	0260,0261,0262,0263,0264,0265,0266,0267,
	0270,0271,0272,0273,0274,0275,0276,0277,
	0300,0301,0302,0303,0304,0305,0306,0307,
	0310,0311,0312,0313,0314,0315,0316,0317,
	0320,0321,0322,0323,0324,0325,0326,0327,
	0330,0331,0332,0333,0334,0335,0336,0337,
	0340,0341,0342,0343,0344,0345,0346,0347,
	0350,0351,0352,0353,0354,0355,0356,0357,
	0360,0361,0362,0363,0364,0365,0366,0367,
	0370,0371,0372,0373,0374,0375,0376,0377,
	0000,0001,0002,0003,0004,0005,0006,0007,
	0010,0011,0012,0013,0014,0015,0016,0017,
	0020,0021,0022,0023,0024,0025,0026,0027,
	0030,0031,0032,0033,0034,0035,0036,0037,
	0040,0041,0042,0043,0044,0045,0046,0047,
	0050,0051,0052,0053,0054,0055,0056,0057,
	0060,0061,0062,0063,0064,0065,0066,0067,
	0070,0071,0072,0073,0074,0075,0076,0077,
	0100,0101,0102,0103,0104,0105,0106,0107,
	0110,0111,0112,0113,0114,0115,0116,0117,
	0120,0121,0122,0123,0124,0125,0126,0127,
	0130,0131,0132,0133,0134,0135,0136,0137,
	0140,0101,0102,0103,0104,0105,0106,0107,
	0110,0111,0112,0113,0114,0115,0116,0117,
	0120,0121,0122,0123,0124,0125,0126,0127,
	0130,0131,0132,0173,0174,0175,0176,0177
};
char nofold[256] = {
	0200,0201,0202,0203,0204,0205,0206,0207,
	0210,0211,0212,0213,0214,0215,0216,0217,
	0220,0221,0222,0223,0224,0225,0226,0227,
	0230,0231,0232,0233,0234,0235,0236,0237,
	0240,0241,0242,0243,0244,0245,0246,0247,
	0250,0251,0252,0253,0254,0255,0256,0257,
	0260,0261,0262,0263,0264,0265,0266,0267,
	0270,0271,0272,0273,0274,0275,0276,0277,
	0300,0301,0302,0303,0304,0305,0306,0307,
	0310,0311,0312,0313,0314,0315,0316,0317,
	0320,0321,0322,0323,0324,0325,0326,0327,
	0330,0331,0332,0333,0334,0335,0336,0337,
	0340,0341,0342,0343,0344,0345,0346,0347,
	0350,0351,0352,0353,0354,0355,0356,0357,
	0360,0361,0362,0363,0364,0365,0366,0367,
	0370,0371,0372,0373,0374,0375,0376,0377,
	0000,0001,0002,0003,0004,0005,0006,0007,
	0010,0011,0012,0013,0014,0015,0016,0017,
	0020,0021,0022,0023,0024,0025,0026,0027,
	0030,0031,0032,0033,0034,0035,0036,0037,
	0040,0041,0042,0043,0044,0045,0046,0047,
	0050,0051,0052,0053,0054,0055,0056,0057,
	0060,0061,0062,0063,0064,0065,0066,0067,
	0070,0071,0072,0073,0074,0075,0076,0077,
	0100,0101,0102,0103,0104,0105,0106,0107,
	0110,0111,0112,0113,0114,0115,0116,0117,
	0120,0121,0122,0123,0124,0125,0126,0127,
	0130,0131,0132,0133,0134,0135,0136,0137,
	0140,0141,0142,0143,0144,0145,0146,0147,
	0150,0151,0152,0153,0154,0155,0156,0157,
	0160,0161,0162,0163,0164,0165,0166,0167,
	0170,0171,0172,0173,0174,0175,0176,0177
};

char	nonprint[256] = {
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1
};

char	dict[256] = {
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,
	1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,
	1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1
};

struct	field {
	char *code;
	char *ignore;
	int nflg;
	int rflg;
	int bflg[2];
	int m[2];
	int n[2];
}	fields[NF];
struct field proto = {
	nofold+128,
	zero+128,
	0,
	1,
	0,0,
	0,-1,
	0,0
};
int	nfields;
int 	error = 1;
char	*setfil();
char	*sbrk();
char	*brk();

#define	blank(c)	((c) == ' ' || (c) == '\t')

main(argc, argv)
char **argv;
{
	register a;
	extern char end[1];
	char *ep;
	char *arg;
	struct field *p, *q;
	int i;

	copyproto();
	eargv = argv;
	while (--argc > 0) {
		if(**++argv == '-') for(arg = *argv;;) {
			switch(*++arg) {
			case '\0':
				if(arg[-1] == '-')
					eargv[eargc++] = "-";
				break;

			case 'o':
				if(--argc > 0)
					outfil = *++argv;
				continue;

			case 'T':
				if (--argc > 0)
					dirtry[0] = *++argv;
				continue;

			default:
				field(++*argv,nfields>0);
				break;
			}
			break;
		} else if (**argv == '+') {
			if(++nfields>=NF) {
				diag("too many keys","");
				exit(1);
			}
			copyproto();
			field(++*argv,0);
		} else
			eargv[eargc++] = *argv;
	}
	q = &fields[0];
	for(a=1; a<=nfields; a++) {
		p = &fields[a];
		if(p->code != proto.code) continue;
		if(p->ignore != proto.ignore) continue;
		if(p->nflg != proto.nflg) continue;
		if(p->rflg != proto.rflg) continue;
		if(p->bflg[0] != proto.bflg[0]) continue;
		if(p->bflg[1] != proto.bflg[1]) continue;
		p->code = q->code;
		p->ignore = q->ignore;
		p->nflg = q->nflg;
		p->rflg = q->rflg;
		p->bflg[0] = p->bflg[1] = q->bflg[0];
	}
	if(eargc == 0)
		eargv[eargc++] = "-";
	if(cflg && eargc>1) {
		diag("can check only 1 file","");
		exit(1);
	}
	safeoutfil();

	ep = end + MEM;
	lspace = (int *)sbrk(0);
	while((int)brk(ep) == -1)
		ep -= 512;
#ifndef	vax
	brk(ep -= 512);	/* for recursion */
#endif
	a = ep - (char*)lspace;
	nlines = (a-L);
	nlines /= (5*(sizeof(char *)/sizeof(char)));
	ntext = nlines * 4 * (sizeof(char *)/sizeof(char));
	tspace = (char *)(lspace + nlines);
	a = -1;
	for(dirs=dirtry; *dirs; dirs++) {
		sprintf(filep=file1, "%s/stm%05uaa", *dirs, getpid());
		while (*filep)
			filep++;
		filep -= 2;
		if ( (a=creat(file, 0600)) >=0)
			break;
	}
	if(a < 0) {
		diag("can't locate temp","");
		exit(1);
	}
	close(a);
	unlink(file);
	if (signal(SIGHUP, SIG_IGN) != SIG_IGN)
		signal(SIGHUP, term);
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, term);
	signal(SIGPIPE,term);
	if (signal(SIGTERM, SIG_IGN) != SIG_IGN)
		signal(SIGTERM,term);
	nfiles = eargc;
	if(!mflg && !cflg) {
		sort();
		fclose(stdin);
	}
	for(a = mflg|cflg?0:eargc; a+N<nfiles || unsafeout&&a<eargc; a=i) {
		i = a+N;
		if(i>=nfiles)
			i = nfiles;
		newfile();
		merge(a, i);
	}
	if(a != nfiles) {
		oldfile();
		merge(a, nfiles);
	}
	error = 0;
	term();
}

sort()
{
	register char *cp;
	register char **lp;
	register lines, text, len;
	int done = 0;
	int i = 0;
	char *f;
	char c;

	if((f = setfil(i++)) == NULL)
		is = stdin;
	else if((is = fopen(f, "r")) == NULL)
		cant(f);

	do {
		cp = tspace;
		lp = (char **)lspace;
		lines = nlines;
		text = ntext;
		while(lines > 0 && text > 0) {
			if(fgets(cp, L, is) == NULL) {
				if(i >= eargc) {
					++done;
					break;
				}
				fclose(is);
				if((f = setfil(i++)) == NULL)
					is = stdin;
				else if((is = fopen(f, "r")) == NULL)
					cant(f);
				continue;
			}
			*lp++ = cp;
			len = strlen(cp) + 1; /* null terminate */
			if(cp[len - 2] != '\n')
				if (len == L) {
					diag("line too long (skipped): ", cp);
					while((c=getc(is)) != EOF && c != '\n')
						/* throw it away */;
					--lp;
					continue;
				} else {
					diag("missing newline before EOF in ",
						f ? f : "standard input");
					/* be friendly, append a newline */
					++len;
					cp[len - 2] = '\n';
					cp[len - 1] = '\0';
				}
			cp += len;
			--lines;
			text -= len;
		}
		qsort((char **)lspace, lp);
		if(done == 0 || nfiles != eargc)
			newfile();
		else
			oldfile();
		clearerr(os);
		while(lp > (char **)lspace) {
			cp = *--lp;
			if(*cp)
				fputs(cp, os);
			if (ferror(os)) {
				error = 1;
				term();
			}
		}
		fclose(os);
	} while(done == 0);
}

struct merg
{
	char	l[L];
	FILE	*b;
} *ibuf[256];

merge(a,b)
{
	struct	merg	*p;
	register char	*cp, *dp;
	register	i;
	struct merg **ip, *jp;
	char	*f;
	int	j;
	int	k, l;
	int	muflg;

	p = (struct merg *)lspace;
	j = 0;
	for(i=a; i < b; i++) {
		f = setfil(i);
		if(f == 0)
			p->b = stdin;
		else if((p->b = fopen(f, "r")) == NULL)
			cant(f);
		ibuf[j] = p;
		if(!rline(p))	j++;
		p++;
	}

	do {
		i = j;
		qsort((char **)ibuf, (char **)(ibuf+i));
		l = 0;
		while(i--) {
			cp = ibuf[i]->l;
			if(*cp == '\0') {
				l = 1;
				if(rline(ibuf[i])) {
					k = i;
					while(++k < j)
						ibuf[k-1] = ibuf[k];
					j--;
				}
			}
		}
	} while(l);

	clearerr(os);
	muflg = mflg & uflg | cflg;
	i = j;
	while(i > 0) {
		cp = ibuf[i-1]->l;
		if (!cflg && (uflg == 0 || muflg || i == 1 ||
			(*compare)(ibuf[i-1]->l,ibuf[i-2]->l))) {
			fputs(cp, os);
			if (ferror(os)) {
				error = 1;
				term();
			}
		}
		if(muflg){
			cp = ibuf[i-1]->l;
			dp = p->l;
			do {
			} while((*dp++ = *cp++) != '\n');
		}
		for(;;) {
			if(rline(ibuf[i-1])) {
				i--;
				if(i == 0)
					break;
				if(i == 1)
					muflg = uflg;
			}
			ip = &ibuf[i];
			while(--ip>ibuf&&(*compare)(ip[0]->l,ip[-1]->l)<0){
				jp = *ip;
				*ip = *(ip-1);
				*(ip-1) = jp;
			}
			if(!muflg)
				break;
			j = (*compare)(ibuf[i-1]->l,p->l);
			if(cflg) {
				if(j > 0)
					disorder("disorder:",ibuf[i-1]->l);
				else if(uflg && j==0)
					disorder("nonunique:",ibuf[i-1]->l);
			} else if(j == 0)
				continue;
			break;
		}
	}
	p = (struct merg *)lspace;
	for(i=a; i<b; i++) {
		fclose(p->b);
		p++;
		if(i >= eargc)
			unlink(setfil(i));
	}
	fclose(os);
}

disorder(s,t)
char *s, *t;
{
	register char *u;
	for(u=t; *u!='\n';u++) ;
	*u = 0;
	diag(s,t);
	term();
}

newfile()
{
	register char *f;

	f = setfil(nfiles);
	if((os=fopen(f, "w")) == NULL) {
		diag("can't create ",f);
		term();
	}
	nfiles++;
}

char *
setfil(i)
{

	if(i < eargc)
		if(eargv[i][0] == '-' && eargv[i][1] == '\0')
			return(0);
		else
			return(eargv[i]);
	i -= eargc;
	filep[0] = i/26 + 'a';
	filep[1] = i%26 + 'a';
	return(file);
}

oldfile()
{

	if(outfil) {
		if((os=fopen(outfil, "w")) == NULL) {
			diag("can't create ",outfil);
			term();
		}
	} else
		os = stdout;
}

safeoutfil()
{
	register int i;
	struct stat obuf,ibuf;

	if(!mflg||outfil==0)
		return;
	if(stat(outfil,&obuf)==-1)
		return;
	for(i=eargc-N;i<eargc;i++) {	/*-N is suff., not nec.*/
		if(stat(eargv[i],&ibuf)==-1)
			continue;
		if(obuf.st_dev==ibuf.st_dev&&
		   obuf.st_ino==ibuf.st_ino)
			unsafeout++;
	}
}

cant(f)
char *f;
{

	perror(f);
	term();
}

diag(s,t)
char *s, *t;
{
	fputs("sort: ",stderr);
	fputs(s,stderr);
	fputs(t,stderr);
	fputs("\n",stderr);
}

term()
{
	register i;

	signal(SIGINT, SIG_IGN);
	signal(SIGHUP, SIG_IGN);
	signal(SIGTERM, SIG_IGN);
	if(nfiles == eargc)
		nfiles++;
	for(i=eargc; i<=nfiles; i++) {	/*<= in case of interrupt*/
		unlink(setfil(i));	/*with nfiles not updated*/
	}
	_exit(error);
}

cmp(i, j)
char *i, *j;
{
	register char *pa, *pb;
	char *skip();
	char *code, *ignore;
	int a, b;
	int k;
	char *la, *lb;
	register int sa;
	int sb;
	char *ipa, *ipb, *jpa, *jpb;
	struct field *fp;

	for(k = nfields>0; k<=nfields; k++) {
		fp = &fields[k];
		pa = i;
		pb = j;
		if(k) {
			la = skip(pa, fp, 1);
			pa = skip(pa, fp, 0);
			lb = skip(pb, fp, 1);
			pb = skip(pb, fp, 0);
		} else {
			la = eol(pa);
			lb = eol(pb);
		}
		if(fp->nflg) {
			if(tabchar) {
				if(pa<la&&*pa==tabchar)
					pa++;
				if(pb<lb&&*pb==tabchar)
					pb++;
			}
			while(blank(*pa))
				pa++;
			while(blank(*pb))
				pb++;
			sa = sb = fp->rflg;
			if(*pa == '-') {
				pa++;
				sa = -sa;
			}
			if(*pb == '-') {
				pb++;
				sb = -sb;
			}
			for(ipa = pa; ipa<la&&isdigit(*ipa); ipa++) ;
			for(ipb = pb; ipb<lb&&isdigit(*ipb); ipb++) ;
			jpa = ipa;
			jpb = ipb;
			a = 0;
			if(sa==sb)
				while(ipa > pa && ipb > pb)
					if(b = *--ipb - *--ipa)
						a = b;
			while(ipa > pa)
				if(*--ipa != '0')
					return(-sa);
			while(ipb > pb)
				if(*--ipb != '0')
					return(sb);
			if(a) return(a*sa);
			if(*(pa=jpa) == '.')
				pa++;
			if(*(pb=jpb) == '.')
				pb++;
			if(sa==sb)
				while(pa<la && isdigit(*pa)
				   && pb<lb && isdigit(*pb))
					if(a = *pb++ - *pa++)
						return(a*sa);
			while(pa<la && isdigit(*pa))
				if(*pa++ != '0')
					return(-sa);
			while(pb<lb && isdigit(*pb))
				if(*pb++ != '0')
					return(sb);
			continue;
		}
		code = fp->code;
		ignore = fp->ignore;
loop: 
		while(ignore[*pa])
			pa++;
		while(ignore[*pb])
			pb++;
		if(pa>=la || *pa=='\n')
			if(pb<lb && *pb!='\n')
				return(fp->rflg);
			else continue;
		if(pb>=lb || *pb=='\n')
			return(-fp->rflg);
		if((sa = code[*pb++]-code[*pa++]) == 0)
			goto loop;
		return(sa*fp->rflg);
	}
	if(uflg)
		return(0);
	return(cmpa(i, j));
}

cmpa(pa, pb)
register char *pa, *pb;
{
	while(*pa == *pb) {
		if(*pa++ == '\n')
			return(0);
		pb++;
	}
	return(
		*pa == '\n' ? fields[0].rflg:
		*pb == '\n' ?-fields[0].rflg:
		*pb > *pa   ? fields[0].rflg:
		-fields[0].rflg
	);
}

char *
skip(pp, fp, j)
struct field *fp;
char *pp;
{
	register i;
	register char *p;

	p = pp;
	if( (i=fp->m[j]) < 0)
		return(eol(p));
	while(i-- > 0) {
		if(tabchar != 0) {
			while(*p != tabchar)
				if(*p != '\n')
					p++;
				else goto ret;
			if(i>0||j==0)
				p++;
		} else {
			while(blank(*p))
				p++;
			while(!blank(*p))
				if(*p != '\n')
					p++;
				else goto ret;
		}
	}
	if(tabchar==0||fp->bflg[j])
		while(blank(*p))
			p++;
	i = fp->n[j];
	while(i-- > 0) {
		if(*p != '\n')
			p++;
		else goto ret;
	} 
ret:
	return(p);
}

char *
eol(p)
register char *p;
{
	while(*p != '\n') p++;
	return(p);
}

copyproto()
{
	register i;
	register int *p, *q;

	p = (int *)&proto;
	q = (int *)&fields[nfields];
	for(i=0; i<sizeof(proto)/sizeof(*p); i++)
		*q++ = *p++;
}

field(s,k)
char *s;
{
	register struct field *p;
	register d;
	p = &fields[nfields];
	d = 0;
	for(; *s!=0; s++) {
		switch(*s) {
		case '\0':
			return;

		case 'b':
			p->bflg[k]++;
			break;

		case 'd':
			p->ignore = dict+128;
			break;

		case 'f':
			p->code = fold+128;
			break;
		case 'i':
			p->ignore = nonprint+128;
			break;

		case 'c':
			cflg = 1;
			continue;

		case 'm':
			mflg = 1;
			continue;

		case 'n':
			p->nflg++;
			break;
		case 't':
			tabchar = *++s;
			if(tabchar == 0) s--;
			continue;

		case 'r':
			p->rflg = -1;
			continue;
		case 'u':
			uflg = 1;
			break;

		case '.':
			if(p->m[k] == -1)	/* -m.n with m missing */
				p->m[k] = 0;
			d = &fields[0].n[0]-&fields[0].m[0];

		default:
			p->m[k+d] = number(&s);
		}
		compare = cmp;
	}
}

number(ppa)
char **ppa;
{
	int n;
	register char *pa;
	pa = *ppa;
	n = 0;
	while(isdigit(*pa)) {
		n = n*10 + *pa - '0';
		*ppa = pa++;
	}
	return(n);
}

#define qsexc(p,q) t= *p;*p= *q;*q=t
#define qstexc(p,q,r) t= *p;*p= *r;*r= *q;*q=t

qsort(a,l)
char **a, **l;
{
	register char **i, **j;
	char **k;
	char **lp, **hp;
	int c;
	char *t;
	unsigned n;



start:
	if((n=l-a) <= 1)
		return;


	n /= 2;
	hp = lp = a+n;
	i = a;
	j = l-1;


	for(;;) {
		if(i < lp) {
			if((c = (*compare)(*i, *lp)) == 0) {
				--lp;
				qsexc(i, lp);
				continue;
			}
			if(c < 0) {
				++i;
				continue;
			}
		}

loop:
		if(j > hp) {
			if((c = (*compare)(*hp, *j)) == 0) {
				++hp;
				qsexc(hp, j);
				goto loop;
			}
			if(c > 0) {
				if(i == lp) {
					++hp;
					qstexc(i, hp, j);
					i = ++lp;
					goto loop;
				}
				qsexc(i, j);
				--j;
				++i;
				continue;
			}
			--j;
			goto loop;
		}


		if(i == lp) {
			if(uflg)
				for(k=lp+1; k<=hp;) **k++ = '\0';
			if(lp-a >= l-hp) {
				qsort(hp+1, l);
				l = lp;
			} else {
				qsort(a, lp);
				a = hp+1;
			}
			goto start;
		}


		--lp;
		qstexc(j, lp, i);
		j = --hp;
	}
}

