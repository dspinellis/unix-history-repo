# include	<ingres.h>
# include	<aux.h>
# include	<tree.h>
# include	<symbol.h>
# include	"parser.h"
# include	<sccs.h>

SCCSID(@(#)format.c	7.1	2/5/81)

/*
**  FORMAT
**	routine to compute the format of the result relation attributes
**	it is called after ATTLOOKUP so the tuple defining the current
**	attribute is already available.
**	if the element is a function of more than one attribute, the result
**	domain format is computed by the following rules:
**		- no fcns allowed on character attributes
**		- fcn of integer attribs is an integer fcn with
**		  length = MAX(length of all attributes)
**		- fcn of floating point attribs is a floating point
**		  fcn with length = MIN(length of all attribs)
**		- fcn of integer and floating attributes is a
**		  floating fcn with length = MIN(length of all floating
**		  attributes)
**
**	Trace Flags:
**		Format ~~ 52.0, 52.1
*/

format(result1)
QTREE	*result1;
{
	register char		rfrmt;
	register char		rfrml;
	register QTREE		*result;
	struct constop		*cpt;

	extern struct out_arg	Out_arg;
	extern struct constop	Coptab[];
	extern char		Trfrml;
	extern char		Trfrmt;
	extern int		Qlflag;

# ifdef	xPTR2
	tTfp(52, 0, "format:.\n");
# endif

	result = result1;
	switch (result->sym.type)
	{
	  case VAR:
		rfrmt = result->sym.value.sym_var.varfrmt;
		rfrml = result->sym.value.sym_var.varfrml;
      		break;

	  case AOP:
		switch (result->sym.value.sym_op.opno)
		{
		  case opAVG:
		  case opAVGU:
			rfrmt = FLOAT;
			rfrml = 8;
			if (result->sym.value.sym_op.agfrmt == CHAR)
				/* character domain not allowed in these aggs */
				par_error(AVGTYPE, WARN, 0);
			break;

		  case opCOUNT:
		  case opCOUNTU:
			rfrmt = INT;
			rfrml = 4;
			break;

		  case opANY:
			rfrmt = INT;
			rfrml = 2;
			break;

		  case opSUM:
		  case opSUMU:
			rfrmt = result->sym.value.sym_op.agfrmt;
			rfrml = result->sym.value.sym_op.agfrml;
			if (rfrmt == CHAR)
				/* no char domains for these aggs */
				par_error(SUMTYPE, WARN, 0);
			break;

		  default:
			rfrmt = result->sym.value.sym_op.agfrmt;
			rfrml = result->sym.value.sym_op.agfrml;
			break;
		}
		break;

	  case AGHEAD:
		/*
		** can get format info from the AOP node because
		** it already has format info computed
		*/
		if (result->left->sym.type == AOP)
		{
			/* no by-list */
			rfrmt = result->left->sym.value.sym_op.opfrmt;
			rfrml = result->left->sym.value.sym_op.opfrml;
		}
		else
		{
			/* skip over by-list */
			rfrmt = result->left->right->sym.value.sym_resdom.resfrmt;
			rfrml = result->left->right->sym.value.sym_resdom.resfrml;
		}
		break;

	  case RESDOM:
		format(result->right);
		return;

	  case INT:
	  case FLOAT:
	  case CHAR:
		rfrmt = result->sym.type;
		rfrml = result->sym.len;
		break;

	  case COP:
		for (cpt = Coptab; cpt->copname; cpt++)
		{
			if (result->sym.value.sym_op.opno == cpt->copnum)
			{
				rfrmt = cpt->coptype;
				rfrml = cpt->coplen;
				break;
			}
		}
		if (!cpt->copname)
			syserr("bad cop in format(%d)", result->sym.value.sym_op.opno);
		break;

	  case UOP:
		switch (result->sym.value.sym_op.opno)
		{
		  case opATAN:
		  case opCOS:
# ifdef	xV6_UNIX
		  case opGAMMA:
# endif
		  case opLOG:
		  case opSIN:
		  case opSQRT:
		  case opEXP:
			format(result->left);
			if (Trfrmt == CHAR)
				/*
				** no character expr in FOP
				** if more ops are added, must change error message				*/
				par_error(FOPTYPE, WARN, 0);

		  case opFLOAT8:
			/* float8 is type conversion and can have char values */
			rfrmt = FLOAT;
			rfrml = 8;
			break;

		  case opFLOAT4:
			rfrmt = FLOAT;
			rfrml = 4;
			break;

		  case opINT1:
			rfrmt = INT;
			rfrml = 1;
			break;

		  case opINT2:
			rfrmt = INT;
			rfrml = 2;
			break;

		  case opINT4:
			rfrmt = INT;
			rfrml = 4;
			break;

		  case opASCII:
			format(result->left);
			rfrmt = CHAR;
			rfrml = Trfrml;
			if (Trfrmt == INT)
			{
				if (Trfrml == 2)
					rfrml = Out_arg.i2width;
				else if (Trfrml == 4)
					rfrml = Out_arg.i4width;
				else if (Trfrml == 1)
					rfrml = Out_arg.i1width;
				else
					syserr("bad length %d for INT", Trfrml);
				break;
			}
			if (Trfrmt == FLOAT)
			{
				if (Trfrml == 8)
					rfrml = Out_arg.f8width;
				else if (Trfrml == 4)
					rfrml = Out_arg.f4width;
				else
					syserr("bad length %d for FLOAT", Trfrml);
				break;
			}
			if (Trfrmt == CHAR)
				break;
			syserr("bad frmt in opASCII %d", Trfrmt);

		  case opNOT:
			if (!Qlflag)
				syserr("opNOT in targ list");
			return;

		  case opMINUS:
		  case opPLUS:
			format(result->right);
			if (Trfrmt == CHAR)
				/* no chars for these unary ops */
				par_error(UOPTYPE, WARN, 0);
			return;

		  case opABS:
			format(result->left);
			if (Trfrmt == CHAR)
				/* no char values in fcn */
				par_error(FOPTYPE, WARN, 0);
			return;

		  default:
			syserr("bad UOP in format %d", result->sym.value.sym_op.opno);
		}
		break;

	  case BOP:
		switch (result->sym.value.sym_op.opno)
		{

		  case opEQ:
		  case opNE:
		  case opLT:
		  case opLE:
		  case opGT:
		  case opGE:
			if (!Qlflag)
				syserr("LBOP in targ list");
			format(result->right);
			rfrmt = Trfrmt;
			format(result->left);
			if ((rfrmt == CHAR) != (Trfrmt == CHAR))
				/* type conflict on relational operator */
				par_error(RELTYPE, WARN, 0);
			return;

		  case opADD:
		  case opSUB:
		  case opMUL:
		  case opDIV:
			format(result->left);
			rfrmt = Trfrmt;
			rfrml = Trfrml;
			format(result->right);
			if (rfrmt == CHAR || Trfrmt == CHAR)
				/* no opns on characters allowed */
				par_error(NUMTYPE, WARN, 0);
			if (rfrmt == FLOAT || Trfrmt == FLOAT)
			{
				if (rfrmt == FLOAT && Trfrmt == FLOAT)
				{
					if (Trfrml < rfrml)
						rfrml = Trfrml;
				}
				else if (Trfrmt == FLOAT)
					rfrml = Trfrml;
				rfrmt = FLOAT;
			}
			else
				if (Trfrml > rfrml)
					rfrml = Trfrml;
			break;

		  case opMOD:
			format(result->left);
			rfrmt = Trfrmt;
			rfrml = Trfrml;
			format(result->right);
			if (rfrmt != INT || Trfrmt != INT)
				/* mod operator not defined */
				par_error(MODTYPE, WARN, 0);
			if (Trfrml > rfrml)
				rfrml = Trfrml;
			break;

		  case opPOW:
			format(result->right);
			rfrmt = Trfrmt;
			rfrml = Trfrml;
			format(result->left);
			if (rfrmt == CHAR || Trfrmt == CHAR)
				/* no char values for pow */
				par_error(NUMTYPE, WARN, 0);
			if ((rfrmt == FLOAT && rfrml == 4) || (Trfrmt == FLOAT && Trfrml == 4))
			{
				rfrmt = FLOAT;
				rfrml = 4;
			}
			else
			{
				rfrmt = FLOAT;
				rfrml = 8;
			}
			break;

		  case opCONCAT:
			format(result->left);
			rfrmt = Trfrmt;
			rfrml = Trfrml;
			format(result->right);
			if (rfrmt != CHAR || Trfrmt != CHAR)
				/* only character domains allowed */
				par_error(CONCATTYPE, WARN, 0);
			rfrml += Trfrml;
			break;

		  default:
			syserr("bad BOP in format %d", result->sym.value.sym_op.opno);
		}
	}
	Trfrmt = rfrmt;
	Trfrml = rfrml;
# ifdef	xPTR2
	tTfp(52, 2, "format>>: Trfrmt = %d, Trfrml = %d.\n", Trfrmt, Trfrml);
# endif
}
