#
/*
 * DR11A interface to voice response unit
 */

#include "/sys/nsys/param.h"
#include "/sys/nsys/tty.h"
#include "/sys/nsys/user.h"

#define	DRAADDR	0167760

struct {
	int	dracsr;
	int	draxbuf;
	int	drarbuf;
};

struct {
	struct	clist	iq;
	struct	clist	oq;
} draa;

#define	AIENABL	0100
#define	BIENABL	040
#define	PDRAA	9

daopen(dev, flag)
{
	DRAADDR->dracsr = AIENABL | BIENABL;
}

daclose(dev, flag)
{
	DRAADDR->dracsr = 0;
}

daread(dev)
{
	register int c;

	spl5();
	while (draa.oq.c_cc)
		sleep(&draa.oq, PDRAA);
	while (draa.iq.c_cc==0)
		sleep(&draa.iq, PDRAA);
	spl0();
	while (u.u_count!=0 && (c = getc(&draa.iq)) >= 0)
		passc(c);
}

dawrite(dev)
{
	int c;

	spl5();
	while (cpass(&c) >= 0 && draa.oq.c_cc <= 30)
		putc(c, &draa.oq);
	spl0();
}

draaint()
{
	register int c;

	if (draa.iq.c_cc <= 30) {
		c = DRAADDR->drarbuf;
		putc(c, &draa.iq);
		putc(c>>8, &draa.iq);
	}
	wakeup(&draa.iq);
}

drabint()
{
	register int c;

	if (draa.oq.c_cc > 0) {
		c = getc(&draa.oq);
		c =| getc(&draa.oq)<<8;
		DRAADDR->draxbuf = c;
	}
	wakeup(&draa.oq);
}
