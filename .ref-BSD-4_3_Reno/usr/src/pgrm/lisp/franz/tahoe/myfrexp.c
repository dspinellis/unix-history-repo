/* file: myfrexp.c
** functions: myfrexp()
** origins: 68k.c
** comments: this looks like a lame way out to me but *I* certainly don't
** want to write a version of this routine for any processor.
*/

#include "global.h"

myfrexp()
{error("myfrexp called", FALSE);
}


/*
** Comment from bigmath.c:
**
**	myfrexp (value, exp, hi, lo)
**		double value;
**		int *exp, *hi, *lo;
**
**	myfrexp returns three values, exp, hi, lo,
**	Such that value = 2**exp*tmp, where tmp = (hi*2**-23+lo*2**-53)
**	is uniquely determined subect to .5< abs(tmp) <= 1.0
**
*/
