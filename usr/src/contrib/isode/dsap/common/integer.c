/* LINTLIBRARY */

#include "quipu/util.h"
#include "quipu/attr.h"
#include "psap.h"

static PE intenc (x)
int * x;
{
	return (int2prim(*x));
}

static int * intdec (pe)
PE pe;
{
int * x;

        if (! test_prim_pe (pe,PE_CLASS_UNIV,PE_PRIM_INT))
		return (0);

	x = (int *) smalloc (sizeof (int));
	*x = prim2num(pe);

	return x;

}

/* ARGSUSED */
static intprint (ps,x,format)
PS ps;
int * x,format;
{
	ps_printf (ps,"%d",*x);
}

static int * intdup (x)
int *x;
{
int *y;

	y = (int *) smalloc (sizeof (int));
	*y = *x;
	
	return (y);
}

static intcmp (x,y)
int *x,*y;
{
	return ( *x == *y ? 0 : (*x > *y ? 1 : -1) );
}

static intfree (x)
int * x;
{
	free ((char *) x);
}

static int * intparse (str)
char * str;
{
int atoi();
int * x;

	x = (int *) smalloc (sizeof (int));
	*x = atoi (str);

	return (x);
}

integer_syntax ()
{
	(void) add_attribute_syntax ("integer",
		(IFP) intenc,	(IFP) intdec,
		(IFP) intparse,	intprint,
		(IFP) intdup,	intcmp,
		intfree,	NULLCP,
		NULLIFP,	FALSE);
}
