/*

	    	C compiler, part 2

	Copyright 1972 Bell Telephone Laboratories, Inc.

*/

ospace() {}	/* fake */

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
	extern getwrd, rcexpr, ospace, tmpfil;
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
			sp = ospace;
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
	extern opdope[], dcalc, notcompat;
	int op, d1, d2, t1, t2, p1[], p2[];
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
	if ((opdope[op]&01)!=0) {	/* binary? */
		p2 = tree[4];
		t2 = p2[1];
		d2 = dcalc(p2, nreg);
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

	if(tree==0)
		return;
	if(*tree >= 103) {
		(*tree==103?jumpc:cbranch)(tree[1],tree[2],tree[3],0);
		return;
	}
	if (cexpr(tree, table, reg))
		return;
	if (table!=regtab) 
		if(cexpr(tree, regtab, reg)) {
			if (table==sptab)
				printf("mov	r%d,-(sp)\n", reg);
			if (table==cctab)
				printf("tst	r%d\n", reg);
			return;
		}
	error("No match for op %d", *tree);
}

cexpr(tree, table, reg)
int tree[][], table[]; {
	extern match, nreg, printf, pname, putchar, regtab;
	extern sptab, cctab, rcexpr, prins, rlength, popstk;
	extern collcon, isn, label, branch, cbranch, fltmod;
	int p1[], p2[], c, r, p[], otable[], ctable[], regtab[], cctab[];
	char string[], match[];

	if ((c = *tree)==100) {		/* call */
		p1 = tree[3];
		p2 = tree[4];
		r = 0;
		if(p2) {
			while (*p2==9) { /* comma */
				rcexpr(p2[4], sptab, 0);
				r =+ rlength((p=p2[4])[1]);
				p2 = p2[3];
			}
			rcexpr(p2, sptab, 0);
			r =+ rlength(p2[1]);
		}
		*tree = 101;
		tree[2] = r;		/* save arg length */
	}
	if(c==90) {		/* ? */
		cbranch(tree[3], c=isn++, 0, reg);
		rcexpr(tree[4][3], table, reg);
		branch(r=isn++, 0);
		label(c);
		rcexpr(tree[4][4], table, reg);
		label(r);
		return(1);
	}
	if ((string=match(tree, table, nreg-reg))==0) 
		return(0);
	p1 = tree[3];
	p2 = tree[4];
loop:
	switch(c = *string++) {

	case '\0':
		p = tree;
		if (*p==101 & p[2]>0) {
			popstk(p[2]);
		}
		return(1);

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
		goto loop;

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
			r = reg+1;
		if((c&01)!=0)
			if(*p==36) {
				p = p[3];
				if(collcon(p) & ctable!=sptab)
					p = p[3];
			}
		rcexpr(p, ctable, r);
		goto loop;

	/* R */
	case 'I':
		r = reg;
		goto preg;

	/* R1 */
	case 'J':
		r = reg+1;
	preg:
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
			c = *p;
			if(r = (p=p[4])[3])
				printf("%o", c==40?r:-r);
		}
		goto loop;

	/* M */
	case 'N':
		if ((c=isfloat(tree, &string))==fltmod)
			goto loop;
		printf((fltmod=c)==2?"setf\n":"setd\n");
		goto loop;

	/* Z */
	case 'Z':
		printf("$%o", p1[4]);
		goto loop;
	}
	putchar(c);
	goto loop;
}

pname(p)
int p[][][]; {
	extern putchar, printf, error;
	char np[];
	int i;

loop:
	switch(*p) {

	case 21:		/* const */
		printf("$%o", p[3]);
		return;

	case 22:		/* string */
		printf("$l%d", p[3]);
		return;

	case 20:		/* name */
		switch(p[3]) {

		case 5:		/* auto, param */
			printf("%o(r5)", p[4]);
			return;

		/* extern */
		case 6:
			printf("%p", &p[4]);
			return;

		}
		printf("L%d", p[4]);
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
	int op, t;

	if (p==0)
		return(0);
	op = *p;
	switch (op) {

	case 20:		/* name */
	case 22:		/* string */
	case 23:		/* float */
	case 24:		/* double */
		return(12);

	case 21:		/* short constant */
		return(p[3]==0? 4:8);

	case 35:		/* & */
		return(12);

	case 36:		/* * */
		if ((op=dcalc(p[3], nreg))<16)
			return(16);
	}

def:
	return(p[2]<=nreg? 20: 24);
}

notcompat(at, st) {

	if (st==0)		/* word, byte */
		return(at>1 & at<16);
	if (st==1)		/* word */
		return(at>0 & at<16);
	st =- 2;
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
	int p1[];

	if(*p==40 | *p==41)
		if(*(p1=p[4])==21)
			return(1);
	return(0);
}

isfloat(t, s)
int t[];
char s[][];
{
	extern opdope[];
	int rt;

	rt = **s - '0';
	if (rt==2 | rt==4) {
		(*s)++;
		return(rt>2?3:2);
	}
	if ((opdope[t[0]]&010)!=0)	/* relational */
		t = t[3];
	if ((rt=t[1])>=2 & rt<=3)
		return(rt);
	return(0);
}

nreg 4;
isn 10000;
namsiz 8;
line;
tmpfil;
nerror;
fltmod;

