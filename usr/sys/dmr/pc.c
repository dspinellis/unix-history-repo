#
/*
 */

/*
 * PC-11 Paper tape reader/punch driver
 */

#include "../param.h"
#include "../conf.h"
#include "../user.h"

#define	PCADDR	0177550

#define	CLOSED	0
#define	WAITING	1
#define	READING	2
#define	EOF	3

#define	RDRENB	01
#define	IENABLE	0100
#define	DONE	0200
#define	BUSY	04000
#define	ERROR	0100000

#define	PCIPRI	30
#define	PCOPRI	40
#define	PCOLWAT	50
#define	PCOHWAT	100
#define	PCIHWAT	250

struct {
	int pcrcsr;
	int pcrbuf;
	int pcpcsr;
	int pcpbuf;
};

struct clist {
	int	cc;
	int	cf;
	int	cl;
};

struct pc11 {
	int	pcstate;
	struct	clist pcin;
	struct	clist pcout;
} pc11;

pcopen(dev, flag)
{
	extern lbolt;

	if (flag==0) {
		if (pc11.pcstate!=CLOSED) {
			u.u_error = ENXIO;
			return;
		}
		pc11.pcstate = WAITING;
		while(pc11.pcstate==WAITING) {
			PCADDR->pcrcsr = IENABLE|RDRENB;
			sleep(&lbolt, PCIPRI);
		}
	} else {
		PCADDR->pcpcsr =| IENABLE;
		pcleader();
	}
}

pcclose(dev, flag)
{
	if (flag==0) {
		spl4();
		while (getc(&pc11.pcin) >= 0);
		PCADDR->pcrcsr = 0;
		pc11.pcstate = CLOSED;
		spl0();
	} else
		pcleader();
}

pcread()
{
	register int c;

	spl4();
	do {
		while ((c = getc(&pc11.pcin)) < 0) {
			if (pc11.pcstate==EOF)
				goto out;
			if ((PCADDR->pcrcsr&(ERROR|BUSY|DONE))==0)
				PCADDR->pcrcsr =| IENABLE|RDRENB;
			sleep(&pc11.pcin, PCIPRI);
		}
	} while (passc(c)>=0);
out:
	spl0();
}

pcwrite()
{
	register int c;

	while ((c=cpass())>=0)
		pcoutput(c);
}

pcstart()
{
	register int c;

	if (PCADDR->pcpcsr&DONE && (c = getc(&pc11.pcout)) >= 0)
		PCADDR->pcpbuf = c;
}

pcrint()
{
	if (pc11.pcstate==WAITING) {
		if (PCADDR->pcrcsr&ERROR)
			return;
		pc11.pcstate = READING;
	}
	if (pc11.pcstate==READING) {
		if (PCADDR->pcrcsr&ERROR)
			pc11.pcstate = EOF;
		else {
			putc(PCADDR->pcrbuf, &pc11.pcin);
			if (pc11.pcin.cc < PCIHWAT)
				PCADDR->pcrcsr =| IENABLE|RDRENB;
		}
		wakeup(&pc11.pcin);
	}
}

pcpint()
{

	pcstart();
	if (pc11.pcout.cc <= PCOLWAT)
		wakeup(&pc11.pcout);
}

pcoutput(c)
{
	if (PCADDR->pcpcsr&ERROR) {
		u.u_error = EIO;
		return;
	}
	if (pc11.pcout.cc >= PCOHWAT)
		sleep(&pc11.pcout, PCOPRI);
	putc(c, &pc11.pcout);
	spl4();
	pcstart();
	spl0();
}

pcleader()
{
	register int i;

	i = 100;
	do
		pcoutput(0);
	while (--i);
}
