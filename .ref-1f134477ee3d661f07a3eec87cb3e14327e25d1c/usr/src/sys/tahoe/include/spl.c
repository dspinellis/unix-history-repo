/*	spl.c	1.2	85/07/29	*/

#include "../tahoe/mtpr.h"

spl0()
{
	register int oldipl;

	oldipl = mfpr(IPL);
	mtpr(IPL, 0);
	return (oldipl);
}

spl1()
{
	register int oldipl;

	oldipl = mfpr(IPL);
	mtpr(IPL, 1);
	return (oldipl);
}

splnet()
{
	register int oldipl;

	oldipl = mfpr(IPL);
	mtpr(IPL, 0xC);
	return (oldipl);
}

spl8()
{
	register int oldipl;

	oldipl = mfpr(IPL);
	mtpr(IPL, 0x10+8);
	return (oldipl);
}

#ifdef notdef	/* use splx() instead */
spl5()
{
	register int oldipl;

	oldipl = mfpr(IPL);
	mtpr(IPL, 0x10+5);
	return (oldipl);
}

spl4()
{
	register int oldipl;

	oldipl = mfpr(IPL);
	mtpr(IPL, 0x10+4);
	return (oldipl);
}

spl6()
{
	register int oldipl;

	oldipl = mfpr(IPL);
	mtpr(IPL, 0x10+6);
	return (oldipl);
}
#endif

splimp()
{
	register int oldipl;

	oldipl = mfpr(IPL);
	mtpr(IPL, 0x10+8);
	return (oldipl);
}

splsoftclock()
{
	register int oldipl;

	oldipl = mfpr(IPL);
	mtpr(IPL, 8);
	return (oldipl);
}

splx(oldipl)
	int oldipl;
{

	mtpr(IPL, oldipl);
}
