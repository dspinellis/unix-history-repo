/*

	    	C compiler, part 2

	Copyright 1972 Bell Telephone Laboratories, Inc.

*/

waste()		/* waste space */
{
	waste(waste(waste),waste(waste),waste(waste));
	waste(waste(waste),waste(waste),waste(waste));
	waste(waste(waste),waste(waste),waste(waste));
	waste(waste(waste),waste(waste),waste(waste));
	waste(waste(waste),waste(waste),waste(waste));
	waste(waste(waste),waste(waste),waste(waste));
	waste(waste(waste),waste(waste),waste(waste));
	waste(waste(waste),waste(waste),waste(waste));
}
main(argc, argv)
char argv[][];
{
	extern fout, fin, nerror, line;
	extern getwrd, rcexpr, tmpfil;
	extern cctab[], regtab[], efftab[], sptab[];
	int sp[], c, table[], tabtab[3][], tree;

	if (argc<4) {
		error("Arg count");
		exit(1);
	}
	if((fin=open(argv[1],0))<0) {
		error("Cant't find %s", argv[1]);
		exit(1);
	}
	if((fout=creat(argv[3],017))<0) {
		error("Can't create %s", argv[3]);
		exit(1);
	}
	tmpfil = argv[2];

	tabtab[0] = regtab;
	tabtab[1] = efftab;
	tabtab[2] = cctab;
	tabtab[3] = sptab;
	while(c=getchar()) {
		if(c=='#') {
			sp = 0;
			c = getwrd();
			tree = getwrd();
			table = tabtab[getwrd()];
			line = getwrd();
			while(c--)
				*sp++ = getwrd();
			rcexpr(tree, table, 0);
		} else
			putchar(c);
	}
	flush();
	exit(nerror!=0);
}

match(tree, table, nreg)
int tree[], table[]; {
	extern opdope[], cctab, maprel;
	int op, d1, d2, t1, t2, p1[], p2[], dope, cctab[];
	int maprel[];
	char mp[];

	if (tree==0)
		return(0);
	op = *tree;
	if (op>=29)			/* if not leaf */
		p1 = tree[3];
	else
		p1 = tree;
	t1 = p1[1];
	d1 = dcalc(p1, nreg);
	if (((dope=opdope[op])&01)!=0) {	/* binary? */
		p2 = tree[4];
		t2 = p2[1];
		d2 = dcalc(p2, nreg);
		if (d2>d1 & (dope&0100)!=0)	/* commute? */
		    if (table!=cctab | (op!=47&op!=48)) { /* commute? */
			if ((dope&04)!=0)	/* relation? */
				*tree = op = maprel[op-60];
			dope = t1;
			t1 = t2;
			t2 = dope;
			dope = p1;
			tree[3] = p1 = p2;
			tree[4] = p2 = dope;
			dope = d1;
			d1 = d2;
			d2 = dope;
			dope = t1;
			t1 = t2;
			t2 = dope;
		}
	}
	while(*table) {
		if (*table++ == op) goto foundop;
		table++;
	}
	return(0);
foundop:
	table = *table;
nxtry:
	mp = table;
	if (*mp == 0)
		return(0);
	if (d1 > (*mp&077) | (*mp>=0100)&(*p1!=36))
		goto notyet;
	if (notcompat(t1, mp[1]))
		goto notyet;
	if ((opdope[op]&01)!=0 & p2!=0) {
		if (d2 > (mp[2]&077) | (mp[2]>=0100)&(*p2!=36))
			goto notyet;
		if (notcompat(t2,mp[3]))
			goto notyet;
	}
now:
	return(table[2]);
notyet:
	table = table+3;
	goto nxtry;
}

rcexpr(tree, table, reg)
int tree[]; {
	extern cexpr, regtab, cctab, sptab, printf, error;
	extern jumpc, cbranch;
	int r, modf;

	if(tree==0)
		return(0);
	if(*tree == 103 | *tree==104) {
		(*tree==103?jumpc:cbranch)(tree[1],tree[2],tree[3],0);
		return(0);
	}
	modf = isfloat(tree)? 'f':0;
	if (*tree == 110) {			/* force r0 */
		if((r=rcexpr(tree[3], table, reg)) != 0)
			printf("mov%c	r%d,r0\n", modf, r);
		return(0);
	}
	if ((r=cexpr(tree, table, reg))>=0)
		return(r);
	if (table!=regtab) 
		if((r=cexpr(tree, regtab, reg))>=0) {
			if (table==sptab)
				printf("mov%c	r%d,-(sp)\n", modf, r);
			if (table==cctab) {
				if (modf=='f')
					printf("cfcc\n");
				printf("tst%c	r%d\n", modf, r);
			}
			return(0);
		}
	error("No match for op %d", *tree);
}

cexpr(tree, table, reg)
int tree[][], table[]; {
	extern match, nreg, printf, pname, putchar, regtab;
	extern sptab, cctab, rcexpr, prins, rlength, popstk;
	extern collcon, isn, label, branch, cbranch;
	extern maprel[];
	int p1[], p2[], c, r, p[], otable[], ctable[], regtab[], cctab[];
	int sptab[];
	char string[], match[];
	int reg1, rreg;

	if ((c = *tree)==100) {		/* call */
		p1 = tree[3];
		p2 = tree[4];
		r = 0;
		if(*p2) {
			while (*p2==9) { /* comma */
				rcexpr(p2[4], sptab, 0);
				r =+ arlength((p=p2[4])[1]);
				p2 = p2[3];
			}
			rcexpr(p2, sptab, 0);
			r =+ arlength(p2[1]);
		}
		*tree = 101;
		tree[2] = r;		/* save arg length */
	}
	if(c==90) {		/* ? */
		cbranch(tree[3], c=isn++, 0, reg);
		rcexpr(tree[4][3], table, reg);
		branch(r=isn++, 0);
		label(c);
		reg = rcexpr(tree[4][4], table, reg);
		label(r);
		goto retrn;
	}
	reg = oddreg(tree, reg);
	reg1 = reg+1;
	if ((string=match(tree, table, nreg-reg))==0) 
		return(-1);
	p1 = tree[3];
	p2 = tree[4];
loop:
	switch(c = *string++) {

	case '\0':
		p = tree;
		if (*p==101) {
			if (p[2]>0)
				popstk(p[2]);
			reg = 0;
		}
retrn:
		c = isfloat(tree);
		if (table==cctab & c)
			printf("cfcc\n");
		if (!c)
			if ((c = *tree)==43 | c==73)
				reg--;
		return(reg);

	/* A1 */
	case 'A':
		p = tree[3];
		goto adr;

	/* A2 */
	case 'B':
		p = tree[4];
		goto adr;

	/* A */
	case 'O':
		p = tree;
	adr:
		pname(p);
		goto loop;

	/* I */
	case 'M':
		if ((c = *string)=='\'')
			string++; else
			c = 0;
		prins(*tree, c);
		goto loop;

	/* B1 */
	case 'C':
		if ((c = *tree)<28)
			p = tree;
		else
			p = tree[3];
		goto pbyte;

	/* BF */
	case 'P':
		p = tree;
		goto pb1;

	/* B2 */
	case 'D':
		p = tree[4];
	pbyte:
		if (p[1]==1)	/* char type? */
			putchar('b');
	pb1:
		if (isfloat(p))
			putchar('f');
		goto loop;

	/* BE */
	case 'L':
		if (tree[3][1]==1 | tree[4][1]==1)
			putchar('b');
		p = tree;
		goto pb1;

	/* C1 */
	case 'E':
		p = p1[3];
		goto const;

	/* C2 */
	case 'F':
		p = p2[3];
	const:
		printf("%o", p);
		goto loop;

	/* F */
	case 'G':
		p = p1;
		goto subtre;

	/* S */
	case 'K':
		p = p2;
		goto subtre;

	/* H */
	case 'H':
		p = tree;

	subtre:
		ctable = regtab;
		r = reg;
		c = *string++ - 'A';
		if ((c&02)!=0)
			ctable = sptab;
		if ((c&04)!=0)
			ctable = cctab;
		if((c&010)!=0)
			r = reg1;
		if((c&01)!=0)
			if(*p==36) {
				p = p[3];
				if(collcon(p) & ctable!=sptab)
					p = p[3];
			}
		rreg = rcexpr(p, ctable, r);
		if (rreg==r | ctable!=regtab)
			goto loop;
		if (string[-2]=='G')	/* left operand */
			if (oddreg(tree, 0)==1) {
				printf("mov	r%d,r%d\n", rreg, r);
				goto loop;
			}
		if (r==reg) {
			reg = rreg;
			reg1 = rreg+1;
		} else
			reg1 = rreg;
		goto loop;

	/* R */
	case 'I':
		r = reg;
		if (*string=='-') {
			string++;
			r--;
		}
		goto preg;

	/* R1 */
	case 'J':
		r = reg1;
	preg:
		if (r>=5)
			error("Register overflow: simplify expression");
		printf("r%d", r);
		goto loop;

	case '#':
		p = p1[3];
		goto nmbr;

	case '"':
		p = p2[3];
		goto nmbr;

	case '~':
		p = tree[3];

	nmbr:
		if(collcon(p)) {
			if (*p==41)			/* - */
				putchar('-');
			switch (*(p = p[4])) {

			case 21:		/* number */
				if (p[3])
					printf("%d.", p[3]);
				break;

			case 35:		/* & name */
				pname(p[3]);
				break;

			}
	}
		goto loop;

	/* V */
	case 'V':
		tree[0] = maprel[(c=tree[0])-60];
		goto loop;

	/* Z */
	case 'Z':
		printf("$%o", p1[5]+p1[4]);
		goto loop;

	case '^':		/* for ++ -- */
		printf("%o", tree[4]);
		goto loop;
	}
	putchar(c);
	goto loop;
}

pname(p)
int p[][]; {
	char np[];
	int i;

loop:
	switch(*p) {

	case 21:		/* const */
		printf("$%o", p[3]);
		return;

	case 23:		/* float const */
		printf("L%d", p[3]);
		return;

	casename:
	case 20:		/* name */
		if (i=p[4])
			printf("%d.+", i);
		switch(p[3]) {

		case 5:		/* auto, param */
			printf("%d.(r5)", p[5]);
			return;

		/* extern */
		case 6:
			printf("%p", &p[5]);
			return;

		case 4:
			error("Illegal structure reference");
			printf("$0");
			return;

		}
		printf("L%d", p[5]);
		return;

	case 35:		/* & */
		putchar('$');
		p = p[3];
		goto loop;

	case 36:		/* * */
		putchar('*');
		p = p[3];
		goto loop;

	}
	error("pname called illegally");
}

dcalc(p, nreg)
int p[]; {
	int op, t, p1[], p2[];

	if (p==0)
		return(0);
	op = *p;
	switch (op) {

	case 20:		/* name */
	case 35:		/* & (non-automatic) */
		return(12);

	case 21:		/* short constant */
		return(p[3]==0? 4:8);

	case 23:		/* float const */
		return(p[3]==0? 4:12);

	case 36:		/* * */
		p1 = p[3];
		if (*p1==20)		/* name or offset name */
			return(12);
	}

def:
	return(p[2]<=nreg? 20: 24);
}

notcompat(at, st) {

	if (st==0)		/* word, byte */
		return(at>1 & at<=07);
	if (st==1)		/* word */
		return(at>0 & at<=07);
	st =- 2;
	if ((at&077740) != 0)
		at = 020;		/* *int for **stuff */
	if ((at&077770) != 0)
		at = at&07 | 020;
	if (st==2 & at==3)
		at = 2;
	return(st != at);
}

prins(op, c) {
	extern instab[], printf;
	int insp[];

	insp = instab;
	while(*insp) {
		if (*insp++ == op) {
			if ((c = insp[c!=0])==0)
				goto err;
			printf("%s", c);
			return;
		} else
			insp = insp + 2;
	}
err:
	error("No match' for op %d", op);
}

collcon(p)
int p[]; {
	int p1[], t[];

	if(*p==40 | *p==41) {
		if(*(p1=p[4])==21) {	/* number */
			return(1);
		}
		if (*p1==35)
			return(1);
		if (*(p1=p[3])==35) {
			p1 = p[3];
			p[3] = p[4];
			p[4] = p1;
			return(1);
		}
	}
	return(0);
}

isfloat(t)
int t[];
{
	extern opdope[];
	int rt;

	if ((opdope[t[0]]&04)!=0)	/* relational */
		t = t[3];
	if ((rt=t[1])>=2 & rt<=3)
		return(rt);
	return(0);
}

nreg 3;
isn 10000;
namsiz 8;
line;
tmpfil;
nerror;

oddreg(t, reg)
int t[];
{
	if (!isfloat(t))
		switch(*t) {
		case 43:	/* / */
		case 44:	/* % */
		case 73:	/* =/ */
		case 74:	/* =% */
			reg++;

		case 42:	/* * */
		case 72:	/* =* */
			return(reg|1);
		}
	return(reg);
}

arlength(t)
{
	int arl;

	if ((arl=rlength(t)) == 4)
		return(8);
	return(arl);
}

maprel[] 60, 61, 64, 65, 62, 63, 68, 69, 66, 67;

