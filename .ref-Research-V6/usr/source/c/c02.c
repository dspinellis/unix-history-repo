#
/* C compiler
 *
 *
 */

#include "c0h.c"

/*
 * Process a single external definition
 */
extdef()
{
	register o, elsize;
	int type, sclass;
	register struct hshtab *ds;

	if(((o=symbol())==EOF) || o==SEMI)
		return;
	peeksym = o;
	type = INT;
	sclass = EXTERN;
	xdflg = FNDEL;
	if ((elsize = getkeywords(&sclass, &type)) == -1 && peeksym!=NAME)
		goto syntax;
	if (type==STRUCT)
		blkhed();
	do {
		defsym = 0;
		decl1(EXTERN, type, 0, elsize);
		if ((ds=defsym)==0)
			return;
		funcsym = ds;
		ds->hflag =| FNDEL;
		outcode("BS", SYMDEF, ds->name);
		xdflg = 0;
		if ((ds->type&XTYPE)==FUNC) {
			if ((peeksym=symbol())==LBRACE || peeksym==KEYW) {
				funcblk.type = decref(ds->type);
				cfunc(ds->name);
				return;
			}
		} else 
			cinit(ds);
	} while ((o=symbol())==COMMA);
	if (o==SEMI)
		return;
syntax:
	if (o==RBRACE) {
		error("Too many }'s");
		peeksym = 0;
		return;
	}
	error("External definition syntax");
	errflush(o);
	statement(0);
}

/*
 * Process a function definition.
 */
cfunc(cs)
char *cs;
{
	register savdimp;

	savdimp = dimp;
	outcode("BBS", PROG, RLABEL, cs);
	declist(ARG);
	regvar = 5;
	retlab = isn++;
	if ((peeksym = symbol()) != LBRACE)
		error("Compound statement required");
	statement(1);
	outcode("BNB", LABEL, retlab, RETRN);
	dimp = savdimp;
}

/*
 * Process the initializers for an external definition.
 */
cinit(ds)
struct hshtab *ds;
{
	register basetype, nel, ninit;
	int o, width, realwidth;

	nel = 1;
	basetype = ds->type;
	/*
	 * If it's an array, find the number of elements.
	 * "basetype" is the type of thing it's an array of.
	 */
	while ((basetype&XTYPE)==ARRAY) {
		if ((nel = dimtab[ds->ssp&0377])==0)
			nel = 1;
		basetype = decref(basetype);
	}
	realwidth = width = length(ds) / nel;
	/*
	 * Pretend a structure is kind of an array of integers.
	 * This is a kludge.
	 */
	if (basetype==STRUCT) {
		nel =* realwidth/2;
		width = 2;
	}
	if ((peeksym=symbol())==COMMA || peeksym==SEMI) {
		outcode("BSN",CSPACE,ds->name,(nel*width+ALIGN)&~ALIGN);
		return;
	}
	ninit = 0;
	outcode("BBS", DATA, NLABEL, ds->name);
	if ((o=symbol())==LBRACE) {
		do
			ninit = cinit1(ds, basetype, width, ninit, nel);
		while ((o=symbol())==COMMA);
		if (o!=RBRACE)
			peeksym = o;
	} else {
		peeksym = o;
		ninit = cinit1(ds, basetype, width, 0, nel);
	}
	/*
	 * Above we pretended that a structure was a bunch of integers.
	 * Readjust in accordance with reality.
	 * First round up partial initializations.
	 */
	if (basetype==STRUCT) {
		if (o = 2*ninit % realwidth)
			outcode("BN", SSPACE, realwidth-o);
		ninit = (2*ninit+realwidth-2) / realwidth;
		nel =/ realwidth/2;
	}
	/*
	 * If there are too few initializers, allocate
	 * more storage.
	 * If there are too many initializers, extend
	 * the declared size for benefit of "sizeof"
	 */
	if (ninit<nel)
		outcode("BN", SSPACE, (nel-ninit)*realwidth);
	else if (ninit>nel) {
		if ((ds->type&XTYPE)==ARRAY)
			dimtab[ds->ssp&0377] = ninit;
		nel = ninit;
	}
	/*
	 * If it's not an array, only one initializer is allowed.
	 */
	if (ninit>1 && (ds->type&XTYPE)!=ARRAY)
		error("Too many initializers");
	if (((nel&width)&ALIGN))
		outcode("B", EVEN);
}

/*
 * Process a single expression in a sequence of initializers
 * for an external. Mainly, it's for checking
 * type compatibility.
 */
cinit1(ds, type, awidth, aninit, nel)
struct hshtab *ds;
{
	float sf;
	register struct tnode *s;
	register width, ninit;

	width = awidth;
	ninit = aninit;
	if ((peeksym=symbol())==STRING && type==CHAR) {
		peeksym = -1;
		if (ninit)
			bxdec();
		putstr(0);
		if (nel>nchstr) {
			strflg++;
			outcode("BN", SSPACE, nel-nchstr);
			strflg = 0;
			nchstr = nel;
		}
		return(nchstr);
	}
	if (peeksym==RBRACE)
		return(ninit);
	initflg++;
	s = tree();
	initflg = 0;
	switch(width) {

	case 1:
		if (s->op != CON)
			goto bad;
		outcode("B1N0", BDATA, s->value);
		break;

	case 2:
		if (s->op==CON) {
			outcode("B1N0", WDATA, s->value);
			break;
		}
		if (s->op==FCON || s->op==SFCON) {
			if (type==STRUCT) {
				ninit =+ 3;
				goto prflt;
			}
			goto bad;
		}
		rcexpr(block(1,INIT,0,0,s));
		break;

	case 4:
		sf = fcval;
		outcode("B1N1N0", WDATA, sf);
		goto flt;

	case 8:
	prflt:
		outcode("B1N1N1N1N0", WDATA, fcval);
	flt:
		if (s->op==FCON || s->op==SFCON)
			break;

	default:
	bad:
		bxdec();

	}
	return(++ninit);
}

bxdec()
{
	error("Inconsistent external initialization");
}

/*
 * Process one statement in a function.
 */
statement(d)
{
	register o, o1, o2;
	int o3, o4;
	struct tnode *np;

stmt:
	switch(o=symbol()) {

	case EOF:
		error("Unexpected EOF");
	case SEMI:
		return;

	case LBRACE:
		if (d) {
			if (proflg)
				outcode("BN", PROFIL, isn++);
			outcode("BN", SAVE, blkhed());
		}
		while (!eof) {
			if ((o=symbol())==RBRACE)
				return;
			peeksym = o;
			statement(0);
		}
		error("Missing '}'");
		return;

	case KEYW:
		switch(cval) {

		case GOTO:
			if (o1 = simplegoto())
				branch(o1);
			else 
				dogoto();
			goto semi;

		case RETURN:
			doret();
			goto semi;

		case IF:
			np = pexpr();
			o2 = 0;
			if ((o1=symbol())==KEYW) switch (cval) {
			case GOTO:
				if (o2=simplegoto())
					goto simpif;
				cbranch(np, o2=isn++, 0);
				dogoto();
				label(o2);
				goto hardif;

			case RETURN:
				if (nextchar()==';') {
					o2 = retlab;
					goto simpif;
				}
				cbranch(np, o1=isn++, 0);
				doret();
				label(o1);
				o2++;
				goto hardif;

			case BREAK:
				o2 = brklab;
				goto simpif;

			case CONTIN:
				o2 = contlab;
			simpif:
				chconbrk(o2);
				cbranch(np, o2, 1);
			hardif:
				if ((o=symbol())!=SEMI)
					goto syntax;
				if ((o1=symbol())==KEYW && cval==ELSE) 
					goto stmt;
				peeksym = o1;
				return;
			}
			peeksym = o1;
			cbranch(np, o1=isn++, 0);
			statement(0);
			if ((o=symbol())==KEYW && cval==ELSE) {
				o2 = isn++;
				branch(o2);
				label(o1);
				statement(0);
				label(o2);
				return;
			}
			peeksym = o;
			label(o1);
			return;

		case WHILE:
			o1 = contlab;
			o2 = brklab;
			label(contlab = isn++);
			cbranch(pexpr(), brklab=isn++, 0);
			statement(0);
			branch(contlab);
			label(brklab);
			contlab = o1;
			brklab = o2;
			return;

		case BREAK:
			chconbrk(brklab);
			branch(brklab);
			goto semi;

		case CONTIN:
			chconbrk(contlab);
			branch(contlab);
			goto semi;

		case DO:
			o1 = contlab;
			o2 = brklab;
			contlab = isn++;
			brklab = isn++;
			label(o3 = isn++);
			statement(0);
			label(contlab);
			contlab = o1;
			if ((o=symbol())==KEYW && cval==WHILE) {
				cbranch(tree(), o3, 1);
				label(brklab);
				brklab = o2;
				goto semi;
			}
			goto syntax;

		case CASE:
			o1 = conexp();
			if ((o=symbol())!=COLON)
				goto syntax;
			if (swp==0) {
				error("Case not in switch");
				goto stmt;
			}
			if(swp>=swtab+swsiz) {
				error("Switch table overflow");
			} else {
				swp->swlab = isn;
				(swp++)->swval = o1;
				label(isn++);
			}
			goto stmt;

		case SWITCH:
			o1 = brklab;
			brklab = isn++;
			np = pexpr();
			chkw(np, -1);
			rcexpr(block(1,RFORCE,0,0,np));
			pswitch();
			brklab = o1;
			return;

		case DEFAULT:
			if (swp==0)
				error("Default not in switch");
			if ((o=symbol())!=COLON)
				goto syntax;
			label(deflab = isn++);
			goto stmt;

		case FOR:
			o1 = contlab;
			o2 = brklab;
			contlab = isn++;
			brklab = isn++;
			if (o=forstmt())
				goto syntax;
			label(brklab);
			contlab = o1;
			brklab = o2;
			return;
		}

		error("Unknown keyword");
		goto syntax;

	case NAME:
		if (nextchar()==':') {
			peekc = 0;
			o1 = csym;
			if (o1->hclass>0) {
				error("Redefinition");
				goto stmt;
			}
			o1->hclass = STATIC;
			o1->htype = ARRAY;
			if (o1->hoffset==0)
				o1->hoffset = isn++;
			label(o1->hoffset);
			goto stmt;
		}
	}
	peeksym = o;
	rcexpr(tree());

semi:
	if ((o=symbol())==SEMI)
		return;
syntax:
	error("Statement syntax");
	errflush(o);
}

/*
 * Process a for statement.
 */
forstmt()
{
	register int l, o, sline;
	int sline1, *ss;
	struct tnode *st;

	if ((o=symbol()) != LPARN)
		return(o);
	if ((o=symbol()) != SEMI) {		/* init part */
		peeksym = o;
		rcexpr(tree());
		if ((o=symbol()) != SEMI)
			return(o);
	}
	label(contlab);
	if ((o=symbol()) != SEMI) {		/* test part */
		peeksym = o;
		rcexpr(block(1,CBRANCH,tree(),brklab,0));
		if ((o=symbol()) != SEMI)
			return(o);
	}
	if ((peeksym=symbol()) == RPARN) {	/* incr part */
		peeksym = -1;
		statement(0);
		branch(contlab);
		return(0);
	}
	l = contlab;
	contlab = isn++;
	st = tree();
	sline = line;
	if ((o=symbol()) != RPARN)
		return(o);
	ss = treespace;
	treespace = space;
	statement(0);
	sline1 = line;
	line = sline;
	label(contlab);
	rcexpr(st);
	line = sline1;
	treespace = ss;
	branch(l);
	return(0);
}

/*
 * A parenthesized expression,
 * as after "if".
 */
pexpr()
{
	register o, t;

	if ((o=symbol())!=LPARN)
		goto syntax;
	t = tree();
	if ((o=symbol())!=RPARN)
		goto syntax;
	return(t);
syntax:
	error("Statement syntax");
	errflush(o);
	return(0);
}

/*
 * The switch stateent, which involves collecting the
 * constants and labels for the cases.
 */
pswitch()
{
	register struct swtab *cswp, *sswp;
	int dl, swlab;

	cswp = sswp = swp;
	if (swp==0)
		cswp = swp = swtab;
	branch(swlab=isn++);
	dl = deflab;
	deflab = 0;
	statement(0);
	branch(brklab);
	label(swlab);
	if (deflab==0)
		deflab = brklab;
	outcode("BNN", SWIT, deflab, line);
	for (; cswp < swp; cswp++)
		outcode("NN", cswp->swlab, cswp->swval);
	outcode("0");
	label(brklab);
	deflab = dl;
	swp = sswp;
}

/*
 * blkhed is called at the start of each function.
 * It reads the declarations at the start;
 * then assigns storage locations for the
 * parameters (which have been linked into a list,
 * in order of appearance).
 * This list is necessary because in
 * f(a, b) float b; int a; ...
 * the names are seen before the types.
 * Also, the routine adjusts structures involved
 * in some kind of forward-referencing.
 */
blkhed()
{
	register pl;
	register struct hshtab *cs;

	autolen = 6;
	declist(0);
	pl = 4;
	while(paraml) {
		parame->hoffset = 0;
		cs = paraml;
		paraml = paraml->hoffset;
		if (cs->htype==FLOAT)
			cs->htype = DOUBLE;
		cs->hoffset = pl;
		cs->hclass = AUTO;
		if ((cs->htype&XTYPE) == ARRAY) {
			cs->htype =- (ARRAY-PTR);	/* set ptr */
			cs->ssp++;		/* pop dims */
		}
		pl =+ rlength(cs);
	}
	for (cs=hshtab; cs<hshtab+hshsiz; cs++) {
		if (cs->name[0] == '\0')
			continue;
		/* check tagged structure */
		if (cs->hclass>KEYWC && (cs->htype&TYPE)==RSTRUCT) {
			cs->lenp = dimtab[cs->lenp&0377]->lenp;
			cs->htype = cs->htype&~TYPE | STRUCT;
		}
		if (cs->hclass == STRTAG && dimtab[cs->lenp&0377]==0)
			error("Undefined structure: %.8s", cs->name);
		if (cs->hclass == ARG)
			error("Not an argument: %.8s", cs->name);
		if (stflg)
			prste(cs);
	}
	space = treespace;
	outcode("BN", SETREG, regvar);
	return(autolen);
}

/*
 * After a function definition, delete local
 * symbols.
 * Also complain about undefineds.
 */
blkend() {
	register struct hshtab *cs;

	for (cs=hshtab; cs<hshtab+hshsiz; cs++) {
		if (cs->name[0]) {
			if (cs->hclass==0 && (cs->hflag&FNUND)==0) {
				error("%.8s undefined", cs->name);
				cs->hflag =| FNUND;
			}
			if((cs->hflag&FNDEL)==0) {
				cs->name[0] = '\0';
				hshused--;
				cs->hflag =& ~(FNUND|FFIELD);
			}
		}
	}
}

/*
 * write out special definitions of local symbols for
 * benefit of the debugger.  None of these are used
 * by the assembler except to save them.
 */
prste(acs)
{
	register struct hshtab *cs;
	register nkind;

	cs = acs;
	switch (cs->hclass) {
	case REG:
		nkind = RNAME;
		break;

	case AUTO:
		nkind = ANAME;
		break;

	case STATIC:
		nkind = SNAME;
		break;

	default:
		return;

	}
	outcode("BSN", nkind, cs->name, cs->hoffset);
}

/*
 * In case of error, skip to the next
 * statement delimiter.
 */
errflush(ao)
{
	register o;

	o = ao;
	while(o>RBRACE)	/* ; { } */
		o = symbol();
	peeksym  = o;
}
