#include "defs"

char *ops[ ] = 	{ "", "+", "-", "*", "/", "**",
	".not. ", " .and. ", ".andand.", ".oror.", " .or. ",
	" .eq. ", " .lt. ", " .gt. ", " .le. ", " .ge. ", " .ne. ",
	"(", ")", " = ", ", " };

int opprecs[ ]	= { 0, 7, 7, 8, 8, 9, 5, 4, 4, 3, 3,
		6, 6, 6, 6, 6, 6, 10, 10, 1, 0 };

char *qualops[ ]	= { "", "->", ".", " of ", " sub " };


char *classes[ ]	= { "", "arg ", "valarg ", "static ", "auto ",
			"common ", "mos ", "external ", "statement function " };

char *precs[ ]	= { "", "long " };

char *types[ ]	= { "", "integer ", "real ", "double precision ", "logical ",
			"complex ", "char ", "type " };

char *ftntypes[]	= { "integer ", "real ", "logical ", "complex ",
			"double precision ", 0, 0 };


char *langs[]	= { "pfort", "ratfor", "efl"};


propts()
{
fprintf(diagfile, "Options: ");
fprintf(diagfile, "%s ", langs[langopt]);
fprintf(diagfile, "%s ", (dbgopt ? "debug" : "ndebug") );
fprintf(diagfile, "%s ", (dotsopt? "dotson" : "dotsoff") );
fprintf(diagfile, "\n");
}




prexpr(e)
ptr e;
{
if(e)  prexp1(e, 0,0,0);
}





prexp1(e, prec, subt, leftside)
register ptr e;
int prec, subt, leftside;
{
ptr p, q;
int prec1, needpar;

needpar = 0;

switch(e->tag)
	{
case TERROR:
	break;

case TCONST:
	TEST fprintf(diagfile, "%s", e->leftp);
	if(e->rightp)
		putzcon(e);
	else
		putconst(e->vtype, e->leftp);
	break;

case TFTNBLOCK:
	putname(e);
	break;

case TNAME:
	if(e->sthead == 0) fatal("name without entry");
	TEST fprintf(diagfile, "%s", e->sthead->namep);
	putname(e);
	if(e->vsubs)
		prexp1(e->vsubs, 0,0,0);
	break;

case TTEMP:
	TEST fprintf(diagfile, "(fakename %o)", e);
	putname(e);
	break;

case TLIST:
	if(e->leftp == 0) break;
	TEST fprintf(diagfile, "( ");
	putic(ICOP, OPLPAR);
	for(p=e->leftp ; p!=0 ; p = p->nextp)
		{
		prexp1(p->datap, 0,0,0);
		if(p->nextp)
			{
			TEST fprintf(diagfile, " , ");
			putic(ICOP, OPCOMMA);
			}
		}
	TEST fprintf(diagfile, " )");
	putic(ICOP, OPRPAR);
	break;

case TSTFUNCT:
	fprintf(diagfile, "statement function ");
	prexp1(e->leftp, 0,0,0);
	TEST fprintf(diagfile, " = ");
	putic(ICOP, OPEQUALS);
	prexp1(e->rightp, 0,0,0);
	break;

case TAROP:
	if(e->subtype==OPSTAR && e->leftp->tag!=TCONST && e->rightp->tag==TCONST)
		{
		q = e->leftp;
		e->leftp = e->rightp;
		e->rightp = q;
		}
case TLOGOP:
	prec1 = opprecs[e->subtype];
	goto print;
case TNOTOP:
	prec1 = 5;
	if(prec > 1)	/* force parens */
		needpar = 1;
	goto print;
case TNEGOP:
	if(prec > 1)	/* force parens */
		needpar = 1;
	prec1 = 8;
	goto print;
case TASGNOP:
	prec1 = 1;
	goto print;
case TRELOP:
	prec1 = 6;
	goto print;
case TCALL:
	prec1 = 10;
	goto print;
case TREPOP:
	prec1 = 2;
	goto print;

print:
	if(prec1 < prec )
		needpar = 1;
	else if(prec1 == prec)
		if(e->needpar)
			needpar = 1;
		else if(subt == e->subtype)
			needpar |= ! (e->tag==TLOGOP || leftside || subt==0
					|| subt==OPPLUS || subt==OPSTAR);
		else	needpar |=  ! (leftside || subt==OPPLUS || subt==OPSTAR);

	if(needpar)
		{
		putic(ICOP,OPLPAR);
		TEST fprintf(diagfile, "(");
		}

	if(e->rightp != 0)
		{
		prexp1(e->leftp, prec1, e->subtype, 1);
		switch(e->tag) {
		case TASGNOP:
			TEST fprintf(diagfile, "=");
			putic(ICOP, OPEQUALS);
			if(e->subtype != 0)
				prexp1(e->leftp, prec1, 0, 1);
	
		case TAROP:
		case TNEGOP:
		case TLOGOP:
		case TNOTOP:
		case TRELOP:
			if(e->subtype)
				{
				TEST fprintf(diagfile, " %s ", ops[e->subtype]);
				putic(ICOP, e->subtype);
				}
			break;
	
		case TCALL:
			TEST fprintf(diagfile, " %s ", qualops[e->subtype]);
			break;
	
		case TREPOP:
			TEST fprintf(diagfile, "$");
			break;
			}

		prexp1(e->rightp, prec1,e->subtype, 0);
		}
	else	{ /* e->rightp == 0 */
		TEST fprintf(diagfile, " %s  ", ops[e->subtype]);
		putic(ICOP, e->subtype);
		prexp1(e->leftp, prec1,e->subtype, 0);
		}
	if(needpar)
		{
		putic(ICOP, OPRPAR);
		TEST fprintf(diagfile, ")");
		}
	break;

default:
	badtag("prexp1", e->tag);
	break;
	}
}
