/*
 * INTERMEDIATE CODE GENERATION PROCEDURES COMMON TO BOTH
 * JOHNSON AND RITCHIE FAMILIES OF SECOND PASSES
*/

#include "defs"

#if FAMILY == SCJ
#	include "scjdefs"
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





struct addrblock *realpart(p)
register struct addrblock *p;
{
register struct addrblock *q;

q = cpexpr(p);
if( ISCOMPLEX(p->vtype) )
	q->vtype += (TYREAL-TYCOMPLEX);
return(q);
}




struct addrblock *imagpart(p)
register struct addrblock *p;
{
register struct addrblock *q;
struct constblock *mkrealcon();

if( ISCOMPLEX(p->vtype) )
	{
	q = cpexpr(p);
	q->vtype += (TYREAL-TYCOMPLEX);
	q->memoffset = mkexpr(OPPLUS, q->memoffset, ICON(typesize[q->vtype]));
	}
else
	q = mkrealcon( ISINT(p->vtype) ? TYDREAL : p->vtype , 0.0);
return(q);
}

struct addrblock *putconst(p)
register struct constblock *p;
{
register struct addrblock *q;
struct literal *litp, *lastlit;
int i, k, type;
int litflavor;

if( ! ISCONST(p) )
	fatal1("putconst: bad tag %d", p->tag);

q = ALLOC(addrblock);
q->tag = TADDR;
type = p->vtype;
q->vtype = ( type==TYADDR ? TYINT : type );
q->vleng = cpexpr(p->vleng);
q->vstg = STGCONST;
q->memno = newlabel();
q->memoffset = ICON(0);

/* check for value in literal pool, and update pool if necessary */

switch(type = p->vtype)
	{
	case TYCHAR:
		if(p->vleng->const.ci > XL)
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
				if(p->vleng->const.ci != litp->litval.litcval.litclen)
					break;
				if(! eqn( (int) p->vleng->const.ci, p->const.ccp,
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
					litp->litval.litcval.litclen = p->vleng->const.ci;
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
		putstr(asmfile, p->const.ccp, p->vleng->const.ci);
		break;

	case TYADDR:
		prcona(asmfile, p->const.ci);
		break;

	default:
		fatal1("putconst: bad type %d", p->vtype);
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
ftnint n;
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
