extdef() {
	extern eof, cval, defsym;
	extern csym, strflg, xdflg, peeksym, fcval;
	int o, c, cs[], type, csym[], width, nel, ninit, defsym[];
	char s[];
	float sf;
	double fcval;

	if(((o=symbol())==0) | o==1)	/* EOF */
		return;
	type = 0;
	if (o==19) {			/* keyword */
		if ((type=cval)>4)
			goto syntax;	/* not type */
	} else {
		if (o==20)
			csym[4] =| 0200;	/* remember name */
		peeksym = o;
	}
	defsym = 0;
	xdflg++;
	tdeclare(type, 0, 0);
	if (defsym==0)
		return;
	*defsym = 6;
	cs = &defsym[4];
	printf(".globl	%p\n", cs);
	strflg = 1;
	xdflg = 0;
	type = defsym[1];
	if ((type&030)==020) {		/* a function */
		printf(".text\n%p:\nmov r5,-(sp); mov sp,r5\n", cs);
		declist(0);
		strflg = 0;
		c = 0;
		if ((peeksym=symbol())!=2) {	/* { */
			blkhed();
			c++;
		}
		statement(1);
		retseq();
		if (c)
			blkend();
		return;
	}
	width = length(defsym);
	if ((type&030)==030)	/* array */
		width = plength(defsym);
	nel = defsym[8];
	ninit = 0;
	if ((peeksym=symbol()) == 1) {	/* ; */
		printf(".comm	%p,%o\n", &defsym[4], nel*width);
		peeksym = -1;
		return;
	}
	printf(".data\n%p:", &defsym[4]);
loop:	{
		ninit++;
		switch(o=symbol()) {
	
		case 22:			/* string */
			if (width!=2)
				bxdec();
			printf("L%d\n", cval);
			break;
	
		case 41:			/* - const */
			if ((o=symbol())==23) {	/* float */
				fcval = -fcval;
				goto case23;
			}
			if (o!=21)
				goto syntax;
			cval = -cval;
	
		case 21:			/* const */
			if (width==1)
				printf(".byte ");
			if (width>2) {
				fcval = cval;
				goto case23;
			}
			printf("%o\n", cval);
			break;
	
		case 20:			/* name */
			if (width!=2)
				bxdec();
			printf("%p\n", &csym[4]);
			break;
	
		case 23:
		case23:
			if (width==4) {
				sf = fcval;
				printf("%o;%o\n", sf);
				break;
			}
			if (width==8) {
				printf("%o;%o;%o;%o\n", fcval);
				break;
			}
			bxdec();
			break;
	
		default:
			goto syntax;
	
		}
	} if ((o=symbol())==9) goto loop;	/* , */
	if (o==1) {			/* ; */
	done:
		if (ninit<nel)
			printf(".=.+%d.\n", (nel-ninit)*width);
		return;
	}
syntax:
	error("External definition syntax");
	errflush(o);
	statement(0);
}

bxdec()
{
	error("Inconsistent external initialization");
}

statement(d) {
	extern symbol, error, blkhed, eof, peeksym;
	extern blkend, csym[], rcexpr, block[], tree[], regtab[];
	extern retseq, jumpc, jump, label, contlab, brklab, cval;
	extern swp[], isn, pswitch, peekc;
	extern efftab[], deflab, errflush, swtab[], swsiz, branch;

	int o, o1, o2, o3, np[];

stmt:
	switch(o=symbol()) {

	/* EOF */
	case 0:
		error("Unexpected EOF");
	/* ; */
	case 1:
	/* } */
	case 3:
		return;

	/* { */
	case 2: {
		if(d)
			blkhed();
		while (!eof) {
			if ((o=symbol())==3)	/* } */
				goto bend;
			peeksym = o;
			statement(0);
		}
		error("Missing '}'");
	bend:
		return;
	}

	/* keyword */
	case 19:
		switch(cval) {

		/* goto */
		case 10:
			np = tree();
			if ((np[1]&030)!=030)	/* not array */
				np = block(1, 36, 1, np[2]+1, np);
			rcexpr(block(1,102,0,0,np), regtab);
			goto semi;

		/* return */
		case 11:
			if((peeksym=symbol())==6)	/* ( */
				rcexpr(block(1,110,0,0,pexpr()), regtab);
			retseq();
			goto semi;

		/* if */
		case 12:
			jumpc(pexpr(), o1=isn++, 0);
			statement(0);
			if ((o=symbol())==19 & cval==14) {  /* else */
				o2 = isn++;
				(easystmt()?branch:jump)(o2);
				label(o1);
				statement(0);
				label(o2);
				return;
			}
			peeksym = o;
			label(o1);
			return;

		/* while */
		case 13:
			o1 = contlab;
			o2 = brklab;
			label(contlab = isn++);
			jumpc(pexpr(), brklab=isn++, 0);
			o3 = easystmt();
			statement(0);
			(o3?branch:jump)(contlab);
			label(brklab);
			contlab = o1;
			brklab = o2;
			return;

		/* break */
		case 17:
			if(brklab==0)
				error("Nothing to break from");
			jump(brklab);
			goto semi;

		/* continue */
		case 18:
			if(contlab==0)
				error("Nothing to continue");
			jump(contlab);
			goto semi;

		/* do */
		case 19:
			o1 = contlab;
			o2 = brklab;
			contlab = isn++;
			brklab = isn++;
			label(o3 = isn++);
			statement(0);
			label(contlab);
			contlab = o1;
			if ((o=symbol())==19 & cval==13) { /* while */
				jumpc(tree(), o3, 1);
				label(brklab);
				brklab = o2;
				goto semi;
			}
			goto syntax;

		/* case */
		case 16:
			if ((o=symbol())!=21) {	/* constant */
				if (o!=41)	/* - */
					goto syntax;
				if ((o=symbol())!=21)
					goto syntax;
				cval = - cval;
			}
			if ((o=symbol())!=8)	/* : */
				goto syntax;
			if (swp==0) {
				error("Case not in switch");
				goto stmt;
			}
			if(swp>=swtab+swsiz) {
				error("Switch table overflow");
			} else {
				*swp++ = isn;
				*swp++ = cval;
				label(isn++);
			}
			goto stmt;

		/* switch */
		case 15:
			o1 = brklab;
			brklab = isn++;
			np = pexpr();
			if (np[1]>1 & np[1]<07)
				error("Integer required");
			rcexpr(block(1,110,0,0,np), regtab);
			pswitch();
			brklab = o1;
			return;

		/* default */
		case 20:
			if (swp==0)
				error("Default not in switch");
			if ((o=symbol())!=8)	/* : */
				goto syntax;
			deflab = isn++;
			label(deflab);
			goto stmt;
		}

		error("Unknown keyword");
		goto syntax;

	/* name */
	case 20:
		if (peekc==':') {
			peekc = 0;
			if (csym[0]>0) {
				error("Redefinition");
				goto stmt;
			}
			csym[0] = 7;
			csym[1] = 030;	/* int[] */
			if (csym[2]==0)
				csym[2] = isn++;
			label(csym[2]);
			goto stmt;
		}
	}

	peeksym = o;
	rcexpr(tree(), efftab);
	goto semi;

semi:
	if ((o=symbol())!=1)		/* ; */
		goto syntax;
	return;

syntax:
	error("Statement syntax");
	errflush(o);
	goto stmt;
}

pexpr()
{
	auto o, t;

	if ((o=symbol())!=6)	/* ( */
		goto syntax;
	t = tree();
	if ((o=symbol())!=7)	/* ) */
		goto syntax;
	return(t);
syntax:
	error("Statement syntax");
	errflush(o);
	return(0);
}

pswitch() {
	extern swp[], isn, swtab[], printf, deflab, statement, brklab;
	extern label;
	int sswp[], dl, cv, swlab;

	sswp = swp;
	if (swp==0)
		swp = swtab;
	swlab = isn++;
	printf("jsr	pc,bswitch; l%d\n", swlab);
	dl = deflab;
	deflab = 0;
	statement(0);
	if (!deflab) {
		deflab = isn++;
		label(deflab);
	}
	printf("L%d:.data;L%d:", brklab, swlab);
	while(swp>sswp & swp>swtab) {
		cv = *--swp;
		printf("%o; l%d\n", cv, *--swp);
	}
	printf("L%d; 0\n.text\n", deflab);
	deflab = dl;
	swp = sswp;
}

blkhed()
{
	extern symbol, cval, peeksym, paraml[], parame[];
	extern error, rlength, setstk, defvec, isn, defstat;
	extern stack, hshtab[], hshsiz, pssiz;
	int al, pl, cs[], hl, t[];

	declist(0);
	stack = al = 0;
	pl = 4;
	while(paraml) {
		*parame = 0;
		paraml = *(cs = paraml);
		if (cs[1]==2)		/* float args -> double */
			cs[1] = 3;
		cs[2] = pl;
		*cs = 10;
		if ((cs[1]&030) == 030)		/* array */
			cs[1] =- 020;		/* set ref */
		pl =+ rlength(cs);
	}
	cs = hshtab;
	hl = hshsiz;
	while(hl--) {
	    if (cs[4]) {
		if (cs[0]>1 & (cs[1]&07)==05) {  /* referred structure */
			t = cs[3];
			cs[3] = t[3];
			cs[1] = cs[1]&077770 | 04;
		}
		switch(cs[0]) {

		/* sort unmentioned */
		case -2:
			cs[0] = 5;		/* auto */

		/* auto */
		case 5:
			al =- trlength(cs);
			cs[2] = al;
			break;

		/* parameter */
		case 10:
			cs[0] = 5;
			break;

		/* static */
		case 7:
			cs[2] = isn;
			printf(".bss; L%d: .=.+%o; .text\n",
				isn++, trlength(cs));
			break;

		}
	    }
	    cs = cs+pssiz;
	}
	setstk(al);
}

blkend() {
	extern hshtab[], hshsiz, pssiz, hshused, debug;
	auto i, hl;

	i = 0;
	hl = hshsiz;
	while(hl--) {
		if(hshtab[i+4]) {
if (debug)
if (hshtab[i]!=1)
error("%p	%o	%o	%o	%o	%o",
	&hshtab[i+4],
	hshtab[i],
	hshtab[i+1],
	hshtab[i+2],
	hshtab[i+3],
	hshtab[i+8]);
			if (hshtab[i]==0)
				error("%p undefined", &hshtab[i+4]);
			if((hshtab[i+4]&0200)==0) {	/* not top-level */
				hshtab[i+4] = 0;
				hshused--;
			}
		}
		i =+ pssiz;
	}
}

errflush(o) {
	extern symbol, peeksym, eof;

	while(o>3)	/* ; { } */
		o = symbol();
	peeksym  = o;
}

declist(mosflg)
{
	extern peeksym, csym[], cval;
	auto o, offset;

	offset = 0;
	while((o=symbol())==19 & cval<10)
		if (cval<=4)
			offset = tdeclare(cval, offset, mosflg);
		else
			scdeclare(cval);
	peeksym = o;
	return(offset);
}

easystmt()
{
	extern peeksym, peekc, cval;

	if((peeksym=symbol())==20)	/* name */
		return(peekc!=':');	 /* not label */
	if (peeksym==19) {		/* keyword */
		switch(cval)

		case 10:	/* goto */
		case 11:	/* return */
		case 17:	/* break */
		case 18:	/* continue */
			return(1);
		return(0);
	}
	return(peeksym!=2);		/* { */
}

branch(lab)
{
	printf("br	L%d\n", lab);
}

