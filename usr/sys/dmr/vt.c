#
/*
 */

/*
 * VT01 driver via DR11C to 11/20
 */

#include "../param.h"
#include "../user.h"

int	vtflag;

struct vtreg {
	int	csr;
	int	buf;
};

#define	VTADDR	0167770
#define	RQINT	01
#define	BIENABL	040
#define	SEOF	0100000
#define	VTPRI	8

vtopen(dev, flag)
{
	if (!flag)
		u.u_error = ENXIO;
	else
		VTADDR->csr = BIENABL;
}

vtclose()
{
	VTADDR->buf = SEOF;
	VTADDR->csr =| RQINT;
}

vtwrite()
{
	register int c;
	int register count;

	while ((c=cpass()) >= 0) {
	    retry:
		for (count=0; count<10; count++)
			if ((VTADDR->csr&RQINT)==0) {
				VTADDR->buf = c&0377;
				VTADDR->csr =| RQINT;
				goto contin;
			}
		spl5();
		if (VTADDR->csr&RQINT) {
			vtflag++;
			sleep(VTADDR, VTPRI);
		}
		spl0();
		goto retry;
    contin:;
	}
}

vtintr()
{
	VTADDR->csr =& ~RQINT;
	if (vtflag) {
		vtflag = 0;
		wakeup(VTADDR);
	}
}
