#
/*
 * C compiler, phase 1
 *
 *
 * Handles processing of declarations,
 * except for top-level processing of
 * externals.
 */

#include "c0h.c"

/*
 * Process a sequence of declaration statements
 */
declist(sclass)
{
	register sc, elsize, offset;
	int type;

	offset = 0;
	sc = sclass;
	while ((elsize = getkeywords(&sclass, &type)) != -1) {
		offset = declare(sclass, type, offset, elsize);
		sclass = sc;
	}
	return(offset+align(INT, offset, 0));
}

/*
 * Read the keywords introducing a declaration statement
 */
getkeywords(scptr, tptr)
int *scptr, *tptr;
{
	register skw, tkw, longf;
	int o, elsize, isadecl, ismos;

	isadecl = 0;
	longf = 0;
	tkw = -1;
	skw = *scptr;
	elsize = 0;
	ismos = skw==MOS;
	for (;;) {
		mosflg = ismos;
		switch ((o=symbol())==KEYW? cval: -1) {

		case AUTO:
		case STATIC:
		case EXTERN:
		case REG:
			if (skw && skw!=cval)
				error("Conflict in storage class");
			skw = cval;
			break;
	
		case LONG:
			longf++;
			break;

		case STRUCT:
			o = STRUCT;
			elsize = strdec(&o, ismos);
			cval = o;
		case INT:
		case CHAR:
		case FLOAT:
		case DOUBLE:
			if (tkw>=0)
				error("Type clash");
			tkw = cval;
			break;
	
		default:
			peeksym = o;
			if (isadecl==0)
				return(-1);
			if (tkw<0)
				tkw = INT;
			if (skw==0)
				skw = AUTO;
			if (longf) {
				if (tkw==FLOAT)
					tkw = DOUBLE;
				else if (tkw==INT)
					tkw = LONG;
				else
					error("Misplaced 'long'");
			}
			*scptr = skw;
			*tptr = tkw;
			return(elsize);
		}
		isadecl++;
	}
}

/*
 * Process a structure declaration; a subroutine
 * of getkeywords.
 */
strdec(tkwp, mosf)
int *tkwp;
{
	register elsize, o;
	register struct hshtab *ssym;
	int savebits;
	struct hshtab *ds;

	mosflg = 1;
	ssym = 0;
	if ((o=symbol())==NAME) {
		ssym = csym;
		if (ssym->hclass==0) {
			ssym->hclass = STRTAG;
			ssym->lenp = dimp;
			chkdim();
			dimtab[dimp++] = 0;
		}
		if (ssym->hclass != STRTAG)
			redec();
		mosflg = mosf;
		o = symbol();
	}
	mosflg = 0;
	if (o != LBRACE) {
		if (ssym==0) {
		syntax:
			decsyn(o);
			return(0);
		}
		if (ssym->hclass!=STRTAG)
			error("Bad structure name");
		if ((elsize = dimtab[ssym->lenp&0377])==0) {
			*tkwp = RSTRUCT;
			elsize = ssym;
		}
		peeksym = o;
	} else {
		ds = defsym;
		mosflg = 0;
		savebits = bitoffs;
		bitoffs = 0;
		elsize = declist(MOS);
		bitoffs = savebits;
		defsym = ds;
		if ((o = symbol()) != RBRACE)
			goto syntax;
		if (ssym) {
			if (dimtab[ssym->lenp&0377])
				error("%.8s redeclared", ssym->name);
			dimtab[ssym->lenp&0377] = elsize;
		}
	}
	return(elsize);
}

/*
 * Check that the dimension table has not overflowed
 */
chkdim()
{
	if (dimp >= dimsiz) {
		error("Dimension/struct table overflow");
		exit(1);
	}
}

/*
 * Process a comma-separated list of declarators
 */
declare(askw, tkw, offset, elsize)
{
	register int o;
	register int skw;

	skw = askw;
	do {
		offset =+ decl1(skw, tkw, offset, elsize);
	} while ((o=symbol()) == COMMA);
	if (o==SEMI || o==RPARN && skw==ARG1)
		return(offset);
	decsyn(o);
}

/*
 * Process a single declarator
 */
decl1(askw, tkw, offset, elsize)
{
	int t1, chkoff, a;
	register int type, skw;
	register struct hshtab *dsym;

	skw = askw;
	chkoff = 0;
	mosflg = skw==MOS;
	if ((peeksym=symbol())==SEMI || peeksym==RPARN)
		return(0);
	/*
	 * Filler field
	 */
	if (peeksym==COLON && skw==MOS) {
		peeksym = -1;
		t1 = conexp();
		elsize = align(tkw, offset, t1);
		bitoffs =+ t1;
		return(elsize);
	}
	if ((t1=getype()) < 0)
		goto syntax;
	type = 0;
	do
		type = type<<TYLEN | (t1 & XTYPE);
	while (((t1=>>TYLEN) & XTYPE)!=0);
	type =| tkw;
	dsym = defsym;
	if (!(dsym->hclass==0
	   || (skw==ARG && dsym->hclass==ARG1)
	   || (skw==EXTERN && dsym->hclass==EXTERN && dsym->htype==type)))
		if (skw==MOS && dsym->hclass==MOS && dsym->htype==type)
			chkoff = 1;
		else {
			redec();
			goto syntax;
		}
	dsym->htype = type;
	if (skw)
		dsym->hclass = skw;
	if (skw==ARG1) {
		if (paraml==0)
			paraml = dsym;
		else
			parame->hoffset = dsym;
		parame = dsym;
	}
	if (elsize && ((type&TYPE)==RSTRUCT || (type&TYPE)==STRUCT)) {
		dsym->lenp = dimp;
		chkdim();
		dimtab[dimp++] = elsize;
	}
	elsize = 0;
	if (skw==MOS) {
		elsize = length(dsym);
		t1 = 0;
		if ((peeksym = symbol())==COLON) {
			elsize = 0;
			peeksym = -1;
			t1 = conexp();
			dsym->hflag =| FFIELD;
		}
		a = align(type, offset, t1);
		elsize =+ a;
		offset =+ a;
		if (t1) {
			if (chkoff && (dsym->bitoffs!=bitoffs
		 	 || dsym->flen!=t1))
				redec();
			dsym->bitoffs = bitoffs;
			dsym->flen = t1;
			bitoffs =+ t1;
		}
		if (chkoff && dsym->hoffset != offset)
			redec();
		dsym->hoffset = offset;
	}
	if ((dsym->htype&XTYPE)==FUNC) {
		if (dsym->hclass!=EXTERN && dsym->hclass!=AUTO)
			error("Bad function");
		dsym->hclass = EXTERN;
	}
	if (dsym->hclass==AUTO) {
		autolen =+ rlength(dsym);
		dsym->hoffset = -autolen;
	} else if (dsym->hclass==STATIC) {
		dsym->hoffset = isn;
		outcode("BBNBNB", BSS, LABEL, isn++,
		    SSPACE, rlength(dsym), PROG);
	} else if (dsym->hclass==REG) {
		if ((type&TYPE)>CHAR && (type&XTYPE)==0
		 || (type&XTYPE)>PTR || regvar<3)
			error("Bad register %o", type);
		dsym->hoffset = --regvar;
	}
syntax:
	return(elsize);
}

/*
 * Read a declarator and get the implied type
 */
getype()
{
	register int o, type;
	register struct hshtab *ds;

	switch(o=symbol()) {

	case TIMES:
		return(getype()<<TYLEN | PTR);

	case LPARN:
		type = getype();
		if ((o=symbol()) != RPARN)
			goto syntax;
		goto getf;

	case NAME:
		defsym = ds = csym;
		type = 0;
		ds->ssp = dimp;
	getf:
		switch(o=symbol()) {

		case LPARN:
			if (xdflg) {
				xdflg = 0;
				ds = defsym;
				declare(ARG1, 0, 0, 0);
				defsym = ds;
				xdflg++;
			} else
				if ((o=symbol()) != RPARN)
					goto syntax;
			type = type<<TYLEN | FUNC;
			goto getf;

		case LBRACK:
			chkdim();
			if ((o=symbol()) != RBRACK) {
				peeksym = o;
				cval = conexp();
				for (o=ds->ssp&0377; o<dimp; o++)
					dimtab[o] =* cval;
				dimtab[dimp++] = cval;
				if ((o=symbol())!=RBRACK)
					goto syntax;
			} else
				dimtab[dimp++] = 1;
			type = type<<TYLEN | ARRAY;
			goto getf;
		}
		peeksym = o;
		return(type);
	}
syntax:
	decsyn(o);
	return(-1);
}

/*
 * Enforce alignment restrictions in structures,
 * including bit-field considerations.
 */
align(type, offset, aflen)
{
	register a, t, flen;
	char *ftl;

	flen = aflen;
	a = offset;
	t = type;
	ftl = "Field too long";
	if (flen==0 && bitoffs) {
		a =+ (bitoffs-1) / NBPC;
		bitoffs = 0;
	}
	while ((t&XTYPE)==ARRAY)
		t = decref(t);
	if (t!=CHAR) {
		a = (a+ALIGN) & ~ALIGN;
		if (a>offset)
			bitoffs = 0;
	}
	if (flen) {
		if (type==INT) {
			if (flen > NBPW)
				error(ftl);
			if (flen+bitoffs > NBPW) {
				bitoffs = 0;
				a =+ NCPW;
			}
		} else if (type==CHAR) {
			if (flen > NBPC)
				error(ftl);
			if (flen+bitoffs > NCPW) {
				bitoffs = 0;
				a =+ 1;
			}
		} else
			error("Bad type for field");
	}
	return(a-offset);
}

/*
 * Complain about syntax error in declaration
 */
decsyn(o)
{
	error("Declaration syntax");
	errflush(o);
}

/*
 * Complain about a redeclaration
 */
redec()
{
	error("%.8s redeclared", defsym->name);
}
