#include "/sys/nsys/param.h"
#include "/sys/nsys/user.h"

#define	UISA	0177640
struct
{
	int	integ;
};
mmread()
{
	int a, c, bn, on;

	do {
		bn = ldiv(u.u_offset[0], u.u_offset[1], 64);
		on = lrem(u.u_offset[0], u.u_offset[1], 64);
		a = UISA->integ;
		spl7();
		UISA->integ = bn;
		c = fubyte(on);
		UISA->integ = a;
		spl0();
	} while(u.u_error==0 && passc(c)>=0);
}

mmwrite()
{
	int a, c, bn, on;

	for(;;) {
		bn = ldiv(u.u_offset[0], u.u_offset[1], 64);
		on = lrem(u.u_offset[0], u.u_offset[1], 64);
		if (cpass(&c)<0 || u.u_error!=0)
			break;
		a = UISA->integ;
		spl7();
		UISA->integ = bn;
		subyte(on, c);
		UISA->integ = a;
		spl0();
	}
}
