#
/*
 *	Copyright 1973 Bell Telephone Laboratories Inc
 */

/*
 *	Memory special file
 *	minor device 0 is physical memory
 *	minor device 1 is kernel memory
 *	minor device 2 is EOF/RATHOLE
 */

#include "../param.h"
#include "../user.h"
#include "../conf.h"
#include "../seg.h"

mmread(dev)
{
	register c, bn, on;
	int a;

	if(dev.d_minor == 2)
		return;
	do {
		bn = lshift(u.u_offset, -6);
		on = u.u_offset[1] & 077;
		a = UISA->r[0];
		spl7();
		UISA->r[0] = bn;
		if(dev.d_minor == 1)
			UISA->r[0] = KISA->r[(bn>>7)&07] + (bn & 0177);
		c = fubyte(on);
		UISA->r[0] = a;
		spl0();
	} while(u.u_error==0 && passc(c)>=0);
}

mmwrite(dev)
{
	register c, bn, on;
	int a;

	if(dev.d_minor == 2) {
		c = u.u_count;
		u.u_count = 0;
		u.u_base =+ c;
		dpadd(u.u_offset, c);
		return;
	}
	for(;;) {
		bn = lshift(u.u_offset, -6);
		on = u.u_offset[1] & 077;
		if ((c=cpass())<0 || u.u_error!=0)
			break;
		a = UISA->r[0];
		spl7();
		UISA->r[0] = bn;
		if(dev.d_minor == 1)
			UISA->r[0] = KISA->r[(bn>>7)&07] + (bn & 0177);
		subyte(on, c);
		UISA->r[0] = a;
		spl0();
	}
}
