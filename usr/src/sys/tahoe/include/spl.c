/*	spl.c	1.1	85/07/21	*/

#include "../machine/mtpr.h"

spl0()
{
	register int oldipl;

	oldipl = mfpr(IPL);
	mtpr(0, IPL);
	return (oldipl);
}

spl1()
{
	register int oldipl;

	oldipl = mfpr(IPL);
	mtpr(1, IPL);
	return (oldipl);
}

splnet()
{
	register int oldipl;

	oldipl = mfpr(IPL);
	mtpr(0xC, IPL);
	return (oldipl);
}

spl8()
{
	register int oldipl;

	oldipl = mfpr(IPL);
	mtpr(0x10+8, IPL);
	return (oldipl);
}

#ifdef notdef	/* use splx() instead */
spl5()
{
	register int oldipl;

	oldipl = mfpr(IPL);
	mtpr(0x10+5, IPL);
	return (oldipl);
}

spl4()
{
	register int oldipl;

	oldipl = mfpr(IPL);
	mtpr(0x10+4, IPL);
	return (oldipl);
}

spl6()
{
	register int oldipl;

	oldipl = mfpr(IPL);
	mtpr(0x10+6, IPL);
	return (oldipl);
}
#endif

splimp()
{
	register int oldipl;

	oldipl = mfpr(IPL);
	mtpr(0x10+8, IPL);
	return (oldipl);
}

splsoftclock()
{
	register int oldipl;

	oldipl = mfpr(IPL);
	mtpr(8, IPL);
	return (oldipl);
}

splx(oldipl)
	int oldipl;
{

	mtpr(oldipl, IPL);
}
