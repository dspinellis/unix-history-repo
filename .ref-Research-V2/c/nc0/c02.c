function() {
	extern declare, blkhed, blkend;
	extern printf, statement, peeksym, cval, symbol, retseq;
	extern paraml;
	auto o;

	printf(".text; 1:mov r5,-(sp); mov sp,r5\n");
	declare(8);
	declist();
	statement(1);
	retseq();
}

extdef() {
	extern eof, function, cval;
	extern symbol, block, printf, pname, errflush, csym[];
	extern error;
	auto o, c, cs[];
	char s[];

	if(((o=symbol())==0) | o==1)	/* EOF */
		return;
	if(o!=20)
		goto syntax;
	csym[0] = 6;
	cs = &csym[4];
	printf(".globl	%p\n", cs);
	s = ".data; %p:1f\n";
	switch(o=symbol()) {

	case 6:				/* ( */
		printf(s, cs);
		function();
		return;

	case 21:			/* const */
		printf(".data; %p: %o\n", cs, cval);
		if((o=symbol())!=1)	/* ; */
			goto syntax;
		return;

	case 1:				/* ; */
		printf(".bss; %p: .=.+2\n", cs);
		return;

	case 4:				/* [ */
		c = 0;
		if((o=symbol())==21) {	/* const */
			c = cval<<1;
			o = symbol();
		}
		if(o!=5)		/* ] */
			goto syntax;
		printf(s, cs);
		if((o=symbol())==1) {	/* ; */
			printf(".bss; 1:.=.+%o\n", c);
			return;
		}
		printf("1:");
		while(o==21) {		/* const */
			printf("%o\n", cval);
			c =- 2;
			if((o=symbol())==1)	/* ; */
				goto done;
			if(o!=9)		/* , */
				goto syntax;
			else
				o = symbol();
		}
		goto syntax;
	done:
		if(c>0)
			printf(".=.+%o\n", c);
		return;

	case 0:				/* EOF */
		return;
	}

syntax:
	error("External definition syntax");
	errflush(o);
	statement(0);
}

statement(d) {
	extern symbol, error, blkhed, eof, peeksym;
	extern blkend, csym[], rcexpr, block[], tree[], regtab[];
	extern retseq, jumpc, jump, label, contlab, brklab, cval;
	extern swp[], isn, pswitch, peekc, slabel;
	extern efftab[], declare, deflab, errflush, swtab[], swsiz, branch;

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
			o1 = block(1,102,0,0,tree());
			rcexpr(o1, regtab);
			goto semi;

		/* return */
		case 11:
			if((peeksym=symbol())==6)	/* ( */
				rcexpr(pexpr(), regtab);
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
			if ((o=symbol())!=21)	/* constant */
				goto syntax;
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
			if (np[1]>1 & np[1]<16)
				error("Integer required");
			rcexpr(np, regtab);
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
			csym[0] = 2;
			csym[1] = 020;	/* int[] */
			if (csym[2]==0)
				csym[2] = isn++;
			slabel();
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
	extern symbol, cval, declare, peeksym, paraml[], parame[];
	extern error, length, rlength, setstk, defvec, isn, defstat;
	extern stack, hshtab[], hshsiz, pssiz;
	int o, al, pl, cs[], hl;

	declist();
	stack = al = -2;
	pl = 4;
	while(paraml) {
		*parame = 0;
		paraml = *(cs = paraml);
		cs[2] = pl;
		*cs = 10;
		pl =+ rlength(cs[1]);
	}
	cs = hshtab;
	hl = hshsiz;
	while(hl--) {
	    if (cs[4])
		switch(cs[0]) {

		/* sort unmentioned */
		case 0177776:	/* -2 */
			cs[0] = 5;		/* auto */

		/* auto */
		case 5:
			if (cs[3]) {	/* vector */
				al =- (cs[3]*length(cs[1]-020)+1) & 077776;
				setstk(al);
				defvec(al);
			}
			cs[2] = al;
			al =- rlength(cs[1]);
			goto loop;

		/* parameter */
		case 10:
			cs[0] = 5;
			goto loop;

		/* static */
		case 7:
			cs[2] = isn++;
			defstat(cs);
			goto loop;

		loop:;
		}
		cs = cs+pssiz;
	}
	setstk(al);
}

blkend() {
	extern hshtab[], hshsiz, pssiz, hshused;
	auto i, hl;

	i = 0;
	hl = hshsiz;
	while(hl--) {
		if(hshtab[i+4])
			if (hshtab[i]==0)
				error("%p undefined", &hshtab[i+4]);
			if(hshtab[i]!=1) {	/* not keyword */
				hshused--;
				hshtab[i+4] = 0;
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

declist()
{
	extern peeksym, peekc, csym[], cval;
	auto o;

	while((o=symbol())==19 & cval<10)
		declare(cval);
	peeksym = o;
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

