#include "../h/param.h"
#include "../h/mtpr.h"

/*ARGSUSED*/
/*VARARGS1*/
mtpr(regno, value)
{

	asm("	mtpr	8(ap),4(ap)");
}

/*ARGSUSED*/
mfpr(regno)
{

	asm("	mfpr	4(ap),r0");
#ifdef lint
	return (0);
#endif
}

/*
 * Copy bytes within kernel
 */
/*ARGSUSED*/
bcopy(from, to, count)
	caddr_t from, to;
	unsigned count;
{

	asm("	movc3	12(ap),*4(ap),*8(ap)");
}
