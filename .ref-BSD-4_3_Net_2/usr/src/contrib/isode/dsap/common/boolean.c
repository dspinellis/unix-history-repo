/* SYNTAX boolean ::= 'TRUE' | 'FALSE' */

#include "quipu/util.h"
#include "quipu/attr.h"
#include "psap.h"

strprint();
sfree();
lexequ();

static PE boolenc (x)
char *x;
{
	return (bool2prim (lexequ (x,"TRUE") ? 0 : 1));
}

static char * booldec (pe)
PE pe;
{
        if (! test_prim_pe (pe,PE_CLASS_UNIV,PE_PRIM_BOOL))
		return (NULLCP);

	if (prim2flag (pe) == 1)
		return (strdup ("TRUE"));
	else
		return (strdup ("FALSE"));
}

static char * boolget (x)
char * x;
{
	if ((lexequ (x,"TRUE") == 0) || (lexequ (x,"FALSE") == 0))
		return (strdup(x));

	parse_error ("TRUE or FALSE expected (%s)",x);
	return (NULLCP);
}

boolean_syntax ()
{
	(void) add_attribute_syntax ("boolean",
		(IFP) boolenc,	(IFP) booldec,
		(IFP) boolget,	strprint,
		(IFP) strdup,	lexequ,
		sfree,		NULLCP,
		NULLIFP,	FALSE);
}

