/*
 * INTERMEDIATE CODE GENERATION PROCEDURES COMMON TO BOTH
 * JOHNSON (PORTABLE) AND RITCHIE FAMILIES OF SECOND PASSES
*/

#include "defs"

#if FAMILY == PCC
#	include "pccdefs"
#else
#	include "dmrdefs"
#endif

/*
char *ops [ ] =
	{
	"??", "+", "-", "*", "/", "**", "-",
	"OR", "AND", "EQV", "NEQV", "NOT",
	"CONCAT",
	"<", "==", ">", "<=", "!=", ">=",
	" of ", " ofC ", " = ", " += ", " *= ", " CONV ", " << ", " % ",
	" , ", " ? ", " : "
	" abs ", " min ", " max ", " addr ", " indirect ",
	" bitor ", " bitand ", " bitxor ", " bitnot ", " >> ",
	};
*/

int ops2 [ ] =
	{
	P2BAD, P2PLUS, P2MINUS, P2STAR, P2SLASH, P2BAD, P2NEG,
	P2OROR, P2ANDAND, P2EQ, P2NE, P2NOT,
	P2BAD,
	P2LT, P2EQ, P2GT, P2LE, P2NE, P2GE,
	P2CALL, P2CALL, P2ASSIGN, P2PLUSEQ, P2STAREQ, P2CONV, P2LSHIFT, P2MOD,
	P2COMOP, P2QUEST, P2COLON,
	P2BAD, P2BAD, P2BAD, P2BAD, P2BAD,
	P2BITOR, P2BITAND, P2BITXOR, P2BITNOT, P2RSHIFT
	};


int types2 [ ] =
	{
	P2BAD, P2INT|P2PTR, P2SHORT, P2LONG, P2REAL, P2DREAL,
#if TARGET == INTERDATA
	P2BAD, P2BAD, P2LONG, P2CHAR, P2INT, P2BAD
#else
	P2REAL, P2DREAL, P2LONG, P2CHAR, P2INT, P2BAD
#endif
	};


setlog()
{
types2[TYLOGICAL] = types2[tylogical];
typesize[TYLOGICAL] = typesize[tylogical];
typealign[TYLOGICAL] = typealign[tylogical];
}


putex1(p)
expptr p;
{
putx( fixtype(p) );
templist = hookup(templist, holdtemps);
holdtemps = NULL;
}





putassign(lp, rp)
expptr lp, rp;
{
putx( fixexpr( mkexpr(OPASSIGN, lp, rp) ));
}




puteq(lp, rp)
expptr lp, rp;
{
putexpr( mkexpr(OPASSIGN, lp, rp) );
}




/* put code for  a *= b */

putsteq(a, b)
expptr a, b;
{
putx( fixexpr( mkexpr(OPSTAREQ, cpexpr(a), cpexpr(b)) ));
}





Addrp realpart(p)
register Addrp p;
{
register Addrp q;

q = (Addrp) cpexpr(p);
if( ISCOMPLEX(p->vtype) )
	q->vtype += (TYREAL-TYCOMPLEX);
return(q);
}




expptr imagpart(p)
register Addrp p;
{
register Addrp q;
expptr mkrealcon();

if( ISCOMPLEX(p->vtype) )
	{
	q = (Addrp) cpexpr(p);
	q->vtype += (TYREAL-TYCOMPLEX);
	q->memoffset = mkexpr(OPPLUS, q->memoffset, ICON(typesize[q->vtype]));
	return( (expptr) q );
	}
else
	return( mkrealcon( ISINT(p->vtype) ? TYDREAL : p->vtype , 0.0));
}





ncat(p)
register expptr p;
{
if(p->tag==TEXPR && p->exprblock.opcode==OPCONCAT)
	return( ncat(p->exprblock.leftp) + ncat(p->exprblock.rightp) );
else	return(1);
}




ftnint lencat(p)
register expptr p;
{
if(p->tag==TEXPR && p->exprblock.opcode==OPCONCAT)
	return( lencat(p->exprblock.leftp) + lencat(p->exprblock.rightp) );
else if( p->headblock.vleng!=NULL && ISICON(p->headblock.vleng) )
	return(p->headblock.vleng->constblock.const.ci);
else if(p->tag==TADDR && p->addrblock.varleng!=0)
	return(p->addrblock.varleng);
else
	{
	err("impossible element in concatenation");
	return(0);
	}
}

Addrp putconst(p)
register Constp p;
{
register Addrp q;
struct Literal *litp, *lastlit;
int i, k, type;
int litflavor;

if( p->tag != TCONST )
	badtag("putconst", p->tag);

q = ALLOC(Addrblock);
q->tag = TADDR;
type = p->vtype;
q->vtype = ( type==TYADDR ? TYINT : type );
q->vleng = (expptr) cpexpr(p->vleng);
q->vstg = STGCONST;
q->memno = newlabel();
q->memoffset = ICON(0);

/* check for value in literal pool, and update pool if necessary */

switch(type = p->vtype)
	{
	case TYCHAR:
		if(p->vleng->constblock.const.ci > XL)
			break;	/* too long for literal table */
		litflavor = 1;
		goto loop;

	case TYREAL:
	case TYDREAL:
		litflavor = 2;
		goto loop;

	case TYLOGICAL:
		type = tylogical;
	case TYSHORT:
	case TYLONG:
		litflavor = 3;

	loop:
		lastlit = litpool + nliterals;
		for(litp = litpool ; litp<lastlit ; ++litp)
			if(type == litp->littype) switch(litflavor)
				{
			case 1:
				if(p->vleng->constblock.const.ci != litp->litval.litcval.litclen)
					break;
				if(! eqn( (int) p->vleng->constblock.const.ci, p->const.ccp,
					litp->litval.litcval.litcstr) )
						break;

			ret:
				q->memno = litp->litnum;
				frexpr(p);
				return(q);

			case 2:
				if(p->const.cd[0] == litp->litval.litdval)
					goto ret;
				break;

			case 3:
				if(p->const.ci == litp->litval.litival)
					goto ret;
				break;
				}
		if(nliterals < MAXLITERALS)
			{
			++nliterals;
			litp->littype = type;
			litp->litnum = q->memno;
			switch(litflavor)
				{
				case 1:
					litp->litval.litcval.litclen =
						p->vleng->constblock.const.ci;
					cpn( (int) litp->litval.litcval.litclen,
						p->const.ccp,
						litp->litval.litcval.litcstr);
					break;

				case 2:
					litp->litval.litdval = p->const.cd[0];
					break;

				case 3:
					litp->litval.litival = p->const.ci;
					break;
				}
			}
	default:
		break;
	}

preven(typealign[ type==TYCHAR ? TYLONG : type ]);
prlabel(asmfile, q->memno);

k = 1;
switch(type)
	{
	case TYLOGICAL:
	case TYSHORT:
	case TYLONG:
		prconi(asmfile, type, p->const.ci);
		break;

	case TYCOMPLEX:
		k = 2;
	case TYREAL:
		type = TYREAL;
		goto flpt;

	case TYDCOMPLEX:
		k = 2;
	case TYDREAL:
		type = TYDREAL;

	flpt:
		for(i = 0 ; i < k ; ++i)
			prconr(asmfile, type, p->const.cd[i]);
		break;

	case TYCHAR:
		putstr(asmfile, p->const.ccp,
			(int) (p->vleng->constblock.const.ci) );
		break;

	case TYADDR:
		prcona(asmfile, p->const.ci);
		break;

	default:
		badtype("putconst", p->vtype);
	}

frexpr(p);
return( q );
}

/*
 * put out a character string constant.  begin every one on
 * a long integer boundary, and pad with nulls
 */
putstr(fp, s, n)
FILEP fp;
char *s;
int n;
{
int b[SZSHORT];
int i;

i = 0;
while(--n >= 0)
	{
	b[i++] = *s++;
	if(i == SZSHORT)
		{
		prchars(fp, b);
		i = 0;
		}
	}

while(i < SZSHORT)
	b[i++] = '\0';
prchars(fp, b);
}
