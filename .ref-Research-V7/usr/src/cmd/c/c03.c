#
/*
 * C compiler, phase 1
 *
 *
 * Handles processing of declarations,
 * except for top-level processing of
 * externals.
 */

#include "c0.h"

/*
 * Process a sequence of declaration statements
 */
declist(sclass)
{
	register sc, offset;
	struct hshtab typer;

	offset = 0;
	sc = sclass;
	while (getkeywords(&sclass, &typer)) {
		offset = declare(sclass, &typer, offset);
		sclass = sc;
	}
	return(offset+align(INT, offset, 0));
}

/*
 * Read the keywords introducing a declaration statement
 * Store back the storage class, and fill in the type
 * entry, which looks like a hash table entry.
 */
getkeywords(scptr, tptr)
int *scptr;
struct hshtab *tptr;
{
	register skw, tkw, longf;
	int o, isadecl, ismos, unsignf;

	isadecl = 0;
	longf = 0;
	unsignf = 0;
	tptr->htype = INT;
	tptr->hstrp = NULL;
	tptr->hsubsp = NULL;
	tkw = -1;
	skw = *scptr;
	ismos = skw==MOS||skw==MOU;
	for (;;) {
		mosflg = ismos && isadecl;
		o = symbol();
		if (o==NAME && csym->hclass==TYPEDEF && tkw<0) {
			tkw = csym->htype;
			tptr->hsubsp = csym->hsubsp;
			tptr->hstrp = csym->hstrp;
			isadecl++;
			continue;
		}
		switch (o==KEYW? cval: -1) {
		case AUTO:
		case STATIC:
		case EXTERN:
		case REG:
		case TYPEDEF:
			if (skw && skw!=cval) {
				if (skw==ARG && cval==REG)
					cval = AREG;
				else
					error("Conflict in storage class");
			}
			skw = cval;
			break;
	
		case UNSIGN:
			unsignf++;
			break;

		case LONG:
			longf++;
			break;

		case ENUM:
			strdec(ismos, cval);
			cval = INT;
			goto types;

		case UNION:
		case STRUCT:
			tptr->hstrp = strdec(ismos, cval);
			cval = STRUCT;
		case INT:
		case CHAR:
		case FLOAT:
		case DOUBLE:
		types:
			if (tkw>=0)
				error("Type clash");
			tkw = cval;
			break;
	
		default:
			peeksym = o;
			if (isadecl==0)
				return(0);
			if (tkw<0)
				tkw = INT;
			if (skw==0)
				skw = blklev==0? DEFXTRN: AUTO;
			if (unsignf) {
				if (tkw==INT)
					tkw = UNSIGN;
				else
					error("Misplaced 'unsigned'");
			}
			if (longf) {
				if (tkw==FLOAT)
					tkw = DOUBLE;
				else if (tkw==INT)
					tkw = LONG;
				else
					error("Misplaced 'long'");
			}
			*scptr = skw;
			tptr->htype = tkw;
			return(1);
		}
		isadecl++;
	}
}

/*
 * Process a structure, union, or enum declaration; a subroutine
 * of getkeywords.
 */
struct str *
strdec(mosf, kind)
{
	register elsize, o;
	register struct hshtab *ssym;
	int savebits;
	struct hshtab **savememlist;
	int savenmems;
	struct str *strp;
	struct hshtab *ds;
	struct hshtab *mems[NMEMS];
	struct hshtab typer;
	int tagkind;

	if (kind!=ENUM) {
		tagkind = STRTAG;
		mosflg = 1;
	} else
		tagkind = ENUMTAG;
	ssym = 0;
	if ((o=symbol())==NAME) {
		ssym = csym;
		mosflg = mosf;
		o = symbol();
		if (o==LBRACE && ssym->hblklev<blklev)
			pushdecl(ssym);
		if (ssym->hclass==0) {
			ssym->hclass = tagkind;
			ssym->strp = gblock(sizeof(*strp));
			funcbase = curbase;
			ssym->strp->ssize = 0;
			ssym->strp->memlist = NULL;
		}
		if (ssym->hclass != tagkind)
			redec();
		strp = ssym->strp;
	} else {
		strp = gblock(sizeof(*strp));
		funcbase = curbase;
		strp->ssize = 0;
		strp->memlist = NULL;
	}
	mosflg = 0;
	if (o != LBRACE) {
		if (ssym==0)
			goto syntax;
		if (ssym->hclass!=tagkind)
			error("Bad structure/union/enum name");
		peeksym = o;
	} else {
		ds = defsym;
		mosflg = 0;
		savebits = bitoffs;
		savememlist = memlist;
		savenmems = nmems;
		memlist = mems;
		nmems = 2;
		bitoffs = 0;
		if (kind==ENUM) {
			typer.htype = INT;
			typer.hstrp = strp;
			declare(ENUM, &typer, 0);
		} else
			elsize = declist(kind==UNION?MOU:MOS);
		bitoffs = savebits;
		defsym = ds;
		if (strp->ssize)
			error("%.8s redeclared", ssym->name);
		strp->ssize = elsize;
		*memlist++ = NULL;
		strp->memlist = gblock((memlist-mems)*sizeof(*memlist));
		funcbase = curbase;
		for (o=0; &mems[o] != memlist; o++)
			strp->memlist[o] = mems[o];
		memlist = savememlist;
		nmems = savenmems;
		if ((o = symbol()) != RBRACE)
			goto syntax;
	}
	return(strp);
   syntax:
	decsyn(o);
	return(0);
}

/*
 * Process a comma-separated list of declarators
 */
declare(askw, tptr, offset)
struct hshtab *tptr;
{
	register int o;
	register int skw, isunion;

	skw = askw;
	isunion = 0;
	if (skw==MOU) {
		skw = MOS;
		isunion++;
		mosflg = 1;
		if ((peeksym=symbol()) == SEMI) {
			o = length(tptr);
			if (o>offset)
				offset = o;
		}
	}
	do {
		if (skw==ENUM && (peeksym=symbol())==RBRACE) {
			o = peeksym;
			peeksym = -1;
			break;
		}
		o = decl1(skw, tptr, isunion?0:offset, NULL);
		if (isunion) {
			o =+ align(CHAR, o, 0);
			if (o>offset)
				offset = o;
		} else
			offset =+ o;
	} while ((o=symbol()) == COMMA);
	if (o==RBRACE) {
		peeksym = o;
		o = SEMI;
	}
	if (o!=SEMI && (o!=RPARN || skw!=ARG1))
		decsyn(o);
	return(offset);
}

/*
 * Process a single declarator
 */
decl1(askw, atptr, offset, absname)
struct hshtab *atptr, *absname;
{
	int t1, chkoff, a, elsize;
	register int skw;
	int type;
	register struct hshtab *dsym;
	register struct hshtab *tptr;
	struct tdim dim;
	struct field *fldp;
	int *dp;
	int isinit;

	skw = askw;
	tptr = atptr;
	chkoff = 0;
	mosflg = skw==MOS;
	dim.rank = 0;
	if (((peeksym=symbol())==SEMI || peeksym==RPARN) && absname==NULL)
		return(0);
	/*
	 * Filler field
	 */
	if (peeksym==COLON && skw==MOS) {
		peeksym = -1;
		t1 = conexp();
		elsize = align(tptr->htype, offset, t1);
		bitoffs =+ t1;
		return(elsize);
	}
	t1 = getype(&dim, absname);
	if (t1 == -1)
		return(0);
	if (tptr->hsubsp) {
		type = tptr->htype;
		for (a=0; type&XTYPE;) {
			if ((type&XTYPE)==ARRAY)
				dim.dimens[dim.rank++] = tptr->hsubsp[a++];
			type =>> TYLEN;
		}
	}
	type = tptr->htype & ~TYPE;
	while (t1&XTYPE) {
		if (type&BIGTYPE) {
			typov();
			type = t1 = 0;
		}
		type = type<<TYLEN | (t1 & XTYPE);
		t1 =>> TYLEN;
	}
	type =| tptr->htype&TYPE;
	if (absname)
		defsym = absname;
	dsym = defsym;
	if (dsym->hblklev < blklev)
		pushdecl(dsym);
	if (dim.rank == 0)
		dsym->subsp = NULL;
	else {
		dp = gblock(dim.rank*sizeof(dim.rank));
		funcbase = curbase;
		if (skw==EXTERN)
			maxdecl = curbase;
		for (a=0; a<dim.rank; a++) {
			if ((t1 = dp[a] = dim.dimens[a])
			 && (dsym->htype&XTYPE) == ARRAY
			 && dsym->subsp[a] && t1!=dsym->subsp[a])
				redec();
		}
		dsym->subsp = dp;
	}
	if ((type&XTYPE) == FUNC) {
		if (skw==AUTO)
			skw = EXTERN;
		if ((skw!=EXTERN && skw!=TYPEDEF) && absname==NULL)
			error("Bad func. storage class");
	}
	if (!(dsym->hclass==0
	   || ((skw==ARG||skw==AREG) && dsym->hclass==ARG1)
	   || (skw==EXTERN && dsym->hclass==EXTERN && dsym->htype==type)))
		if (skw==MOS && dsym->hclass==MOS && dsym->htype==type)
			chkoff = 1;
		else {
			redec();
			goto syntax;
		}
	if (dsym->hclass && (dsym->htype&TYPE)==STRUCT && (type&TYPE)==STRUCT)
		if (dsym->hstrp != tptr->hstrp) {
			error("Warning: structure redeclaration");
			nerror--;
		}
	dsym->htype = type;
	if (tptr->hstrp)
		dsym->hstrp = tptr->hstrp;
	if (skw==TYPEDEF) {
		dsym->hclass = TYPEDEF;
		return(0);
	}
	if (absname)
		return(0);
	if (skw==ARG1) {
		if (paraml==0)
			paraml = dsym;
		else
			parame->hoffset = dsym;
		parame = dsym;
		dsym->hclass = skw;
		return(0);
	}
	elsize = 0;
	if (skw==MOS) {
		elsize = length(dsym);
		if ((peeksym = symbol())==COLON) {
			elsize = 0;
			peeksym = -1;
			t1 = conexp();
			a = align(type, offset, t1);
			if (dsym->hflag&FFIELD) {
				if (dsym->hstrp->bitoffs!=bitoffs
			 	 || dsym->hstrp->flen!=t1)
					redec();
			} else {
				dsym->hstrp = gblock(sizeof(*fldp));
				funcbase = curbase;
			}
			dsym->hflag =| FFIELD;
			dsym->hstrp->bitoffs = bitoffs;
			dsym->hstrp->flen = t1;
			bitoffs =+ t1;
		} else
			a = align(type, offset, 0);
		elsize =+ a;
		offset =+ a;
		if (++nmems >= NMEMS) {
			error("Too many structure members");
			nmems =- NMEMS/2;
			memlist =- NMEMS/2;
		}
		if (a)
			*memlist++ = &structhole;
		if (chkoff && dsym->hoffset != offset)
			redec();
		dsym->hoffset = offset;
		*memlist++ = dsym;
	}
	if (skw==REG)
		if ((dsym->hoffset = goodreg(dsym)) < 0)
			skw = AUTO;
	dsym->hclass = skw;
	isinit = 0;
	if ((a=symbol())!=COMMA && a!=SEMI && a!=RBRACE)
		isinit++;
	if (a!=ASSIGN)
		peeksym = a;
	if (skw==AUTO) {
	/*	if (STAUTO < 0) {	*/
			autolen =- rlength(dsym);
			dsym->hoffset = autolen;
			if (autolen < maxauto)
				maxauto = autolen;
	/*	} else { 			*/
	/*		dsym->hoffset = autolen;	*/
	/*		autolen =+ rlength(dsym);	*/
	/*		if (autolen > maxauto)		*/
	/*			maxauto = autolen;	*/
	/*	}			*/
		if (isinit)
			cinit(dsym, 0, AUTO);
	} else if (skw==STATIC) {
		dsym->hoffset = isn;
		if (isinit) {
			outcode("BBN", DATA, LABEL, isn++);
			if (cinit(dsym, 1, STATIC) & ALIGN)
				outcode("B", EVEN);
		} else
			outcode("BBNBN", BSS, LABEL, isn++, SSPACE, rlength(dsym));
		outcode("B", PROG);
	} else if (skw==REG && isinit)
		cinit(dsym, 0, REG);
	else if (skw==ENUM) {
		if (type!=INT)
			error("Illegal enumeration %.8s", dsym->name);
		dsym->hclass = ENUMCON;
		dsym->hoffset = offset;
		if (isinit)
			cinit(dsym, 0, ENUMCON);
		elsize = dsym->hoffset-offset+1;
	}
	prste(dsym);
syntax:
	return(elsize);
}

/*
 * Push down an outer-block declaration
 * after redeclaration in an inner block.
 */
pushdecl(asp)
struct phshtab *asp;
{
	register struct phshtab *sp, *nsp;

	sp = asp;
	nsp = gblock(sizeof(*nsp));
	maxdecl = funcbase = curbase;
	cpysymb(nsp, sp);
	sp->hclass = 0;
	sp->hflag =& (FKEYW|FMOS);
	sp->htype = 0;
	sp->hoffset = 0;
	sp->hblklev = blklev;
	sp->hpdown = nsp;
}

/*
 * Copy the non-name part of a symbol
 */
cpysymb(s1, s2)
struct phshtab *s1, *s2;
{
	register struct phshtab *rs1, *rs2;

	rs1 = s1;
	rs2 = s2;
	rs1->hclass = rs2->hclass;
	rs1->hflag = rs2->hflag;
	rs1->htype = rs2->htype;
	rs1->hoffset = rs2->hoffset;
	rs1->hsubsp = rs2->hsubsp;
	rs1->hstrp = rs2->hstrp;
	rs1->hblklev = rs2->hblklev;
	rs1->hpdown = rs2->hpdown;
}


/*
 * Read a declarator and get the implied type
 */
getype(adimp, absname)
struct tdim *adimp;
struct hshtab *absname;
{
	static struct hshtab argtype;
	int type;
	register int o;
	register struct hshtab *ds;
	register struct tdim *dimp;

	ds = defsym;
	dimp = adimp;
	type = 0;
	switch(o=symbol()) {

	case TIMES:
		type = getype(dimp, absname);
		if (type==-1)
			return(type);
		if (type&BIGTYPE) {
			typov();
			type = 0;
		}
		return(type<<TYLEN | PTR);

	case LPARN:
		if (absname==NULL || nextchar()!=')') {
			type = getype(dimp, absname);
			if (type==-1)
				return(type);
			ds = defsym;
			if ((o=symbol()) != RPARN)
				goto syntax;
			goto getf;
		}

	default:
		peeksym = o;
		if (absname) {
			defsym = ds = absname;
			absname = NULL;
			goto getf;
		}
		break;

	case NAME:
		defsym = ds = csym;
	getf:
		switch(o=symbol()) {

		case LPARN:
			if (blklev==0) {
				blklev++;
				ds = defsym;
				declare(ARG1, &argtype, 0);
				defsym = ds;
				blklev--;
			} else
				if ((o=symbol()) != RPARN)
					goto syntax;
			if (type&BIGTYPE) {
				typov();
				type = 0;
			}
			type = type<<TYLEN | FUNC;
			goto getf;

		case LBRACK:
			if (dimp->rank>=5) {
				error("Rank too large");
				dimp->rank = 4;
			}
			if ((o=symbol()) != RBRACK) {
				peeksym = o;
				cval = conexp();
				defsym = ds;
				if ((o=symbol())!=RBRACK)
					goto syntax;
			} else {
				if (dimp->rank!=0)
					error("Null dimension");
				cval = 0;
			}
			dimp->dimens[dimp->rank++] = cval;
			if (type&BIGTYPE) {
				typov();
				type = 0;
			}
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
 * More bits required for type than allowed.
 */
typov()
{
	error("Type is too complicated");
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
	if (flen==0) {
		a =+ (NBPC+bitoffs-1) / NBPC;
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
		if (type==INT || type==UNSIGN) {
			if (flen > NBPW)
				error(ftl);
			if (flen+bitoffs > NBPW) {
				bitoffs = 0;
				a =+ NCPW;
			}
		} else if (type==CHAR) {
			if (flen > NBPC)
				error(ftl);
			if (flen+bitoffs > NBPC) {
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

/*
 * Determine if a variable is suitable for storage in
 * a register; if so return the register number
 */
goodreg(hp)
struct hshtab *hp;
{
	int type;

	type = hp->htype;
	/*
	 * Special dispensation for unions
	 */
	if (type==STRUCT && length(hp)<=SZINT)
		type = INT;
	if ((type!=INT && type!=CHAR && type!=UNSIGN && (type&XTYPE)==0)
	 || (type&XTYPE)>PTR || regvar<3)
		return(-1);
	return(--regvar);
}
