/*
 * Screw Works interface via DC-11
 */

#include "../h/types.h"
#include "../h/tty.h"
#include "../h/uba.h"

#define	VSADDR	((struct device *)(UBA0_DEV+0174140))
#define	CDLEAD	01
#define	B1200	030
#define	STOP1	0400
#define	CLSEND	02
#define	RQSEND	01

#define	GO	026
#define	COUNT	120

struct device {
	short	vsrcsr;
	short	vsrbuf;
	short	vsxcsr;
	short	vsxbuf;
};

struct {
	struct	clist	iq;
	int	vtime;
	struct	clist	oq;
} vs;


vsopen(dev)
{
	register c;

	c = VSADDR->vsrcsr;		/* touch register */
	VSADDR->vsrcsr = IENABLE|B1200|CDLEAD;
	VSADDR->vsxcsr = STOP1|IENABLE|B1200;
	vschar(GO);
}

vsclose(dev)
{
	vschar(GO);
	VSADDR->vsrcsr &= ~IENABLE;
	while (getc(&vs.iq) >= 0);
}

vswrite(dev)
{
	register int count, c;

	count = 0;
	while ((c=cpass()) >= 0) {
		if (--count <= 0) {
			count = COUNT;
			vschar(GO);
		}
		vschar(c);
	}
	vschar(GO);
}

vschar(c)
{

	spl5();
	while (vs.oq.c_cc > 120) {
		vsxintr();
		sleep(&vs.oq, TTIPRI);
	}
	putc(c, &vs.oq);
	vsxintr();
	spl0();
}

vstimo()
{
	vs.vtime = 0;
	vsxintr();
}

vsxintr()
{
	static lchar;
	register c;
	register short *xcsr;

	xcsr = &VSADDR->vsxcsr;
	if (*xcsr&DONE) {
		if (lchar==GO) {
			*xcsr &= ~RQSEND;
			lchar = -1;
			if (vs.oq.c_cc==0) {
				wakeup(&vs.oq);
				return;
			}
		}
		if ((*xcsr&CLSEND) == 0) {
			*xcsr |= RQSEND;
			if ((*xcsr&CLSEND) == 0) {
				if (vs.vtime==0) {
					vs.vtime++;
					timeout(vstimo, 0, 60);
				}
				return;
			}
		}
		if ((c = getc(&vs.oq)) >= 0)
			VSADDR->vsxbuf = lchar = c;
		else
			*xcsr &= ~RQSEND;
		if (vs.oq.c_cc <= 15)
			wakeup(&vs.oq);
	}
}

vsread(dev)
{
	register int c;

	spl5();
	while ((c = getc(&vs.iq)) < 0)
		sleep(&vs.iq, TTIPRI);
	spl0();
	passc("?0*#?546?213?879?"[c&017]);
}

vsrintr()
{
	register int c;

	c = VSADDR->vsrbuf;	/* Register must be read (?) */
	c = VSADDR->vsrbuf;
	if (vs.iq.c_cc<=10)
		putc(c, &vs.iq);
	wakeup(&vs.iq);
}
