/* C compiler

Copyright 1972 Bell Telephone Laboratories, Inc. 

*/

init(s, t)
char s[]; {
	extern symbuf, namsiz;
	char symbuf[], sp[];
	int np[], i;

	i = namsiz;
	sp = symbuf;
	while(i--)
		if ((*sp++ = *s++)=='\0') --s;
	np = lookup();
	*np++ = 1;
	*np = t;
}

main(argc, argv)
int argv[]; {
	extern extdef, eof;
	extern fout, fin, nerror, tmpfil, xdflg;

	if(argc<4) {
		error("Arg count");
		exit(1);
	}
	if((fin=open(argv[1],0))<0) {
		error("Can't find %s", argv[1]);
		exit(1);
	}
	if((fout=creat(argv[2], 017))<0) {
		error("Can't create %s", argv[2]);
		exit(1);
	}
	tmpfil = argv[3];
	xdflg++;
	init("int", 0);
	init("char", 1);
	init("float", 2);
	init("double", 3);
	init("struct", 4);
	init("auto", 5);
	init("extern", 6);
	init("static", 7);
	init("goto", 10);
	init("return", 11);
	init("if", 12);
	init("while", 13);
	init("else", 14);
	init("switch", 15);
	init("case", 16);
	init("break", 17);
	init("continue", 18);
	init("do", 19);
	init("default", 20);
	xdflg = 0;
	while(!eof) {
		extdef();
		blkend();
	}
	flush();
	flshw();
	exit(nerror!=0);
}

lookup() {
	extern hshtab, hshsiz, pssiz, symbuf, xdflg;
	int hshtab[], symbuf[];
	extern hshlen, hshused, nwps;
	auto i, j, np[], sp[], rp[];

	i = 0;
	sp = symbuf;
	j = nwps;
	while(j--)
		i =+ *sp++ & 077577;
	if (i<0) i = -i;
	i =% hshsiz;
	i =* pssiz;
	while(*(np = &hshtab[i+4])) {
		sp = symbuf;
		j = nwps;
		while(j--)
			if ((*np++&077577) != *sp++) goto no;
		return(&hshtab[i]);
no:		if ((i =+ pssiz) >= hshlen) i = 0;
	}
	if(++hshused > hshsiz) {
		error("Symbol table overflow");
		exit(1);
	}
	rp = np = &hshtab[i];
	sp = symbuf;
	j = 4;
	while(j--)
		*np++ = 0;
	j = nwps;
	while(j--)
		*np++ = *sp++;
	*np = 0;
	if (xdflg)
		rp[4] =| 0200;		/* mark non-deletable */
	return(rp);
}

symbol() {
	extern peeksym, peekc, eof, line;
	extern csym, symbuf, namsiz, lookup, ctab, cval;
	int csym[];
	extern isn, mosflg, xdflg;
	auto b, c;
	char symbuf[], sp[], ctab[];

	if (peeksym>=0) {
		c = peeksym;
		peeksym = -1;
		if (c==20)
			mosflg = 0;
		return(c);
	}
	if (peekc) {
		c = peekc;
		peekc = 0;
	} else
		if (eof)
			return(0); else
			c = getchar();
loop:
	switch(ctab[c]) {

	case 125:	/* newline */
		line++;

	case 126:	/* white space */
		c = getchar();
		goto loop;

	case 0:		/* EOF */
		eof++;
		return(0);

	case 40:	/* + */
		return(subseq(c,40,30));

	case 41:	/* - */
		return(subseq(c,subseq('>',41,50),31));

	case 80:	/* = */
		if (subseq(' ',0,1)) return(80);
		c = symbol();
		if (c>=40 & c<=49)
			return(c+30);
		if (c==80)
			return(60);
		peeksym = c;
		return(80);

	case 63:	/* < */
		if (subseq(c,0,1)) return(46);
		return(subseq('=',63,62));

	case 65:	/* > */
		if (subseq(c,0,1)) return(45);
		return(subseq('=',65,64));

	case 34:	/* ! */
		return(subseq('=',34,61));

	case 43:	/* / */
		if (subseq('*',1,0))
			return(43);
com:
		c = getchar();
com1:
		if (c=='\0') {
			eof++;
			error("Nonterminated comment");
			return(0);
		}
		if (c=='\n')
			line++;
		if (c!='*')
			goto com;
		c = getchar();
		if (c!='/')
			goto com1;
		c = getchar();
		goto loop;

	case 120:	/* . */
	case 124:	/* number */
		peekc = c;
		switch(c=getnum(c=='0'? 8:10)) {
			case 25:		/* float 0 */
				c = 23;
				break;

			case 23:		/* float non 0 */
				cval = isn++;
		}
		return(c);

	case 122:	/* " */
		return(getstr());

	case 121:	/* ' */
		return(getcc());

	case 123:	/* letter */
		sp = symbuf;
		if (mosflg) {
			*sp++ = '.';
			mosflg = 0;
		}
		while(ctab[c]==123 | ctab[c]==124) {
			if (sp<symbuf+namsiz) *sp++ = c;
			c = getchar();
		}
		while(sp<symbuf+namsiz)
			*sp++ = '\0';
		peekc = c;
		csym = lookup();
		if (csym[0]==1) {	/* keyword */
			cval = csym[1];
			return(19);
		}
		return(20);

	case 127:	/* unknown */
		error("Unknown character");
		c = getchar();
		goto loop;

	}
	return(ctab[c]);
}

subseq(c,a,b) {
	extern peekc;

	if (!peekc)
		peekc = getchar();
	if (peekc != c)
		return(a);
	peekc = 0;
	return(b);
}
getstr() {
	extern isn, cval, strflg;
	auto c;
	char t[], d[];

	t = ".text";
	d = ".data";
	printf("%s;L%d:.byte ", (strflg?t:d), cval=isn++);
	while((c=mapch('"')) >= 0)
		printf("%o,", c);
	printf("0;.even;%s\n", (strflg?d:t));
	return(22);
}

getcc()
{
	extern cval, ncpw;
	auto c, cc;
	char cp[];

	cval = 0;
	cp = &cval;
	cc = 0;
	while((c=mapch('\'')) >= 0)
		if(cc++ < ncpw)
			*cp++ = c;
	if(cc>ncpw)
		error("Long character constant");
	return(21);
}

mapch(c)
{
	extern peekc, line;
	auto a;

	if((a=getchar())==c)
		return(-1);
	switch(a) {

	case '\n':
	case 0:
		error("Nonterminated string");
		peekc = a;
		return(-1);

	case '\\':
		switch (a=getchar()) {

		case 't':
			return('\t');

		case 'n':
			return('\n');

		case '0':
			return('\0');

		case 'r':
			return('\r');

		case '\n':
			line++;
			return('\n');
		}

	}
	return(a);
}

tree() {
	extern csym, ctyp, isn, fcval, peeksym, opdope, cp, cmst;
	int csym[], opdope[], cp[], cmst[];
	extern space, cval, ossiz, cmsiz, mosflg, osleft;
	double fcval;
	int space[];

	int op[], opst[20], pp[], prst[20], andflg, o,
		p, ps, os;

	osleft = ossiz;
	space = 0;
	*space++ = 0;
	op = opst;
	pp = prst;
	cp = cmst;
	*op = 200;		/* stack EOF */
	*pp = 06;
	andflg = 0;

advanc:
	switch (o=symbol()) {

	/* name */
	case 20:
		if (*csym==0)
			if((peeksym=symbol())==6) {	/* ( */
				*csym = 6;		/* extern */
				csym[1] = 020;		/* int() */
			} else {
				csym[1] = 030;		/* array */
				if (csym[2]==0)
					csym[2] = isn++;
			}
		*cp++ = block(2,20,csym[1],csym[3],*csym,0);
		if (*csym==6) {			/* external */
			o = 3;
			while(++o<8) {
				pblock(csym[o]);
				if ((csym[o]&077400) == 0)
					break;
			}
		} else
			pblock(csym[2]);
		goto tand;

	/* short constant */
	case 21:
	case21:
		*cp++ = block(1,21,ctyp,0,cval);
		goto tand;

	/* floating constant */
	case 23:
		*cp++ = block(1,23,3,0,cval);
		if (cval)		/* non-0 */
			printf(".data;L%d:%o;%o;%o;%o;.text\n",cval,fcval);
		goto tand;

	/* string constant: fake a static char array */
	case 22:
		*cp++ = block(3, 20, 031, 1, 7, 0, cval);

tand:
		if(cp>=cmst+cmsiz) {
			error("Expression overflow");
			exit(1);
		}
		if (andflg)
			goto syntax;
		andflg = 1;
		goto advanc;

	/* ++, -- */
	case 30:
	case 31:
		if (andflg)
			o =+ 2;
		goto oponst;

	/* ! */
	case 34:
	/* ~ */
	case 38:
		if (andflg)
			goto syntax;
		goto oponst;

	/* - */
	case 41:
		if (!andflg) {
			peeksym = symbol();
			if (peeksym==21) {
				peeksym = -1;
				cval = -cval;
				goto case21;
			}
			o = 37;
		}
		andflg = 0;
		goto oponst;

	/* & */
	/* * */
	case 47:
	case 42:
		if (andflg)
			andflg = 0; else
			if(o==47)
				o = 35;
			else
				o = 36;
		goto oponst;

	/* ( */
	case 6:
		if (andflg) {
			o = symbol();
			if (o==7)
				o = 101; else {
				peeksym = o;
				o = 100;
				andflg = 0;
			}
		}
	goto oponst;

	/* ) */
	/* ] */
	case 5:
	case 7:
		if (!andflg)
			goto syntax;
		goto oponst;

	case 39:	/* . */
		mosflg++;
		break;

	}
	/* binaries */
	if (!andflg)
		goto syntax;
	andflg = 0;

oponst:
	p = (opdope[o]>>9) & 077;
opon1:
	ps = *pp;
	if (p>ps | p==ps & (opdope[o]&0200)!=0) { /* right-assoc */
putin:
		switch (o) {

		case 6: /* ( */
		case 4: /* [ */
		case 100: /* call */
			p = 04;
		}
		if(op>=opst+20) {		/* opstack size */
			error("expression overflow");
			exit(1);
		}
		*++op = o;
		*++pp = p;
		goto advanc;
	}
	--pp;
	switch (os = *op--) {

	/* EOF */
	case 200:
		peeksym = o;
		return(*--cp);

	/* call */
	case 100:
		if (o!=7)
			goto syntax;
		build(os);
		goto advanc;

	/* mcall */
	case 101:
		*cp++ = block(0,0,0,0);	/* 0 arg call */
		os = 100;
		goto fbuild;

	/* ( */
	case 6:
		if (o!=7)
			goto syntax;
		goto advanc;

	/* [ */
	case 4:
		if (o!=5)
			goto syntax;
		build(4);
		goto advanc;
	}
fbuild:
	build(os);
	goto opon1;

syntax:
	error("Expression syntax");
	errflush(o);
	return(0);
}

scdeclare(kw)
{
	extern csym, paraml, parame, peeksym;
	int csym[], paraml[], parame[];
	int o;

	while((o=symbol())==20) {		/* name */
		if(*csym>0 & *csym!=kw)
			redec();
		*csym = kw;
		if(kw==8)  {		/* parameter */
			*csym = -1;
			if (paraml==0)
				paraml = csym;
			else
				*parame = csym;
			parame = csym;
		}
		if ((o=symbol())!=9)	/* , */
			break;
	}
	if(o==1 & kw!=8 | o==7 & kw==8)
		return;
syntax:
	decsyn(o);
}

tdeclare(kw, offset, mos)
{
	int o, elsize, ds[];
	extern xdflg, peeksym, mosflg, defsym, csym;
	int csym[], ssym[];

	if (kw == 4) {				/* struct */
		ssym = 0;
		ds = defsym;
		mosflg = mos;
		if ((o=symbol())==20) {		/* name */
			ssym = csym;
			o = symbol();
		}
		mosflg = mos;
		if (o != 6) {			/* ( */
			if (ssym==0)
				goto syntax;
			if (*ssym!=8)		/* class structname */
				error("Bad structure name");
			if (ssym[3]==0) {	/* no size yet */
				kw = 5;		/* deferred MOS */
				elsize = ssym;
			} else
				elsize = ssym[3];
			peeksym = o;
		} else {
			if (ssym) {
				if (*ssym)
					redec();
				*ssym = 8;
				ssym[3] = 0;
			}
			elsize = declist(4);
			if ((elsize&01) != 0)
				elsize++;
			defsym = ds;
			if ((o = symbol()) != 7)	/* ) */
				goto syntax;
			if (ssym)
				ssym[3] = elsize;
		}
	}
	mosflg = mos;
	if ((peeksym=symbol()) == 1) {		/* ; */
		peeksym = -1;
		mosflg = 0;
		return(offset);
	}
	do {
		offset =+ t1dec(kw, offset, mos, elsize);
		if (xdflg & !mos)
			return;
	} while ((o=symbol()) == 9);		/* , */
	if (o==1)
		return(offset);
syntax:
	decsyn(o);
}

t1dec(kw, offset, mos, elsize)
{
	int type, nel, defsym[], t1;
	extern defsym, mosflg;

	nel = 0;
	mosflg = mos;
	if ((t1=getype(&nel)) < 0)
		goto syntax;
	type = 0;
	do
		type = type<<2 | (t1 & 03);
	while(t1 =>> 2);
	t1 = type<<3 | kw;
	if (defsym[1] & defsym[1]!=t1)
		redec();
	defsym[1] = t1;
	defsym[3] = elsize;
	elsize = length(defsym);
	if (mos) {
		if (*defsym)
			redec();
		else
			*defsym = 4;
		if ((offset&1)!=0 & elsize!=1)
			offset++;
		defsym[2] = offset;
	} else
		if (*defsym == 0)
			*defsym = -2;		/* default auto */
	if (nel==0)
		nel = 1;
	defsym[8] = nel;
syntax:
	return(nel*elsize);
}

getype(pnel)
int pnel[];
{
	int o, type;
	extern cval, peeksym, xdflg, defsym, csym, pssiz;
	int defsym[], csym[];

	switch(o=symbol()) {

	case 42:					/* * */
		return(getype(pnel)<<2 | 01);

	case 6:						/* ( */
		type = getype(pnel);
		if ((o=symbol()) != 7)			/* ) */
			goto syntax;
		goto getf;

	case 20:					/* name */
		defsym = csym;
		type = 0;
	getf:
		switch(o=symbol()) {

		case 6:					/* ( */
			if (xdflg) {
				xdflg = 0;
				o = defsym;
				scdeclare(8);
				defsym = o;
				xdflg++;
			} else
				if ((o=symbol()) != 7)	/* ) */
					goto syntax;
			type = type<<2 | 02;
			goto getf;

		case 4:					/* [ */
			if ((o=symbol()) != 5) {	/* ] */
				if (o!=21)		/* const */
					goto syntax;
				*pnel = cval;
				if ((o=symbol())!=5)
					goto syntax;
			}
			type = type<<2 | 03;
			goto getf;
		}
		peeksym = o;
		return(type);
	}
syntax:
	decsyn(o);
	return(-1);
}

decsyn(o)
{
	error("Declaration syntax");
	errflush(o);
}

redec()
{
	extern csym;
	int csym[];

	error("%p redeclared", &csym[4]);
}

/* storage */

regtab 0;
efftab 1;
cctab 2;
sptab 3;
symbuf[4];
pssiz 9;
namsiz 8;
nwps 4;
hshused;
hshsiz 100;
hshlen 900;	/* 9*hshsiz */
hshtab[900];
space;
cp;
cmsiz 40;
cmst[40];
ctyp;
isn 1;
swsiz 120;
swtab[120];
swp;
contlab;
brklab;
deflab;
nreg 4;
nauto;
stack;
peeksym 0177777;
peekc;
eof;
line 1;
defsym;
xdflg;
csym;
cval;
fcval 0;	/* a double number */
fc1 0;
fc2 0;
fc3 0;
ncpw 2;
nerror;
paraml;
parame;
tmpfil;
strflg;
ossiz 250;
osleft;
mosflg;
debug 0;

