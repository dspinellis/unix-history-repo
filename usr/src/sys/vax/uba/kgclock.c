/*	kgclock.c	4.3	83/03/03	*/

#ifdef KGCLOCK		/* kl-11 as profiling clock */

#include "../h/param.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/buf.h"
#include "../vaxuba/ubavar.h"
#include "../machine/psl.h"

int	phz = 0;
int	kgprobe(), kgattach();
struct	uba_device *kginfo[1];
u_short	kgstd[] = { 0177560, 0 };
struct	uba_driver kgdriver =
    { kgprobe, 0, kgattach, 0, kgstd, "kg", kginfo };

struct klregs {
	u_short	fill[2];
	u_short	tcsr;
	u_short	tbuf;
};
#define	KLSTRT	0300		/* intr enbl + done */
struct	klregs *klbase;

kgprobe(reg)
caddr_t reg;
{
	register int br, cvec;	/* value-result */
	register struct klregs *klp = (struct klregs *)reg;

	klp->tcsr = KLSTRT;
	DELAY(100000);
	klp->tcsr = 0;
}

kgattach(ui)
	struct uba_device *ui;
{

	klbase = (struct klregs *)ui->ui_addr;
}

/*
 * start the sampling clock
 */
startkgclock()
{

	if (klbase)
		klbase->tcsr = KLSTRT;	/* enable interrupts */
}

/* ARGSUSED */
kgclock(dev, r0, r1, r2, r3, r4 ,r5, pc, ps)
	caddr_t pc;
	int ps;
{
	register int k;
	extern time_t time;
	static time_t otime = 0;
	static time_t calibrate;

	klbase->tbuf = 0377;	/* reprime clock (scope sync too) */
	if (phz == 0) {
		if (otime == 0) {
			otime = time + 1;
			calibrate = 0;
		}
		if (time >= otime)
			calibrate++;
		if (time >= otime + 4) {
			phz = calibrate / 4;
			otime = 0;
		}
		return;
	}
	gatherstats(pc, ps);	/* this routine lives in kern_clock.c */
}
#endif KGCLOCK
