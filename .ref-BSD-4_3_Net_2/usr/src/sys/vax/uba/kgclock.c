/*-
 * Copyright (c) 1982, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)kgclock.c	7.4 (Berkeley) 5/9/91
 */

#include "kg.h"
#if NKG > 0
/*
 * KL-11 as profiling clock
 */
#include "../include/pte.h"
#include "../include/psl.h"

#include "sys/param.h"
#include "sys/map.h"
#include "sys/buf.h"
#include "sys/time.h"
#include "sys/kernel.h"

#include "ubavar.h"

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

int	usekgclock = 1;		/* if zero, kgclock is disabled */

kgprobe(reg)
	caddr_t reg;
{
	register int br, cvec;	/* value-result */
	register struct klregs *klp = (struct klregs *)reg;

	klp->tcsr = KLSTRT;
	DELAY(100000);
	klp->tcsr = 0;
	return (sizeof(struct klregs));
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

	if (klbase && usekgclock && phz == 0)
		klbase->tcsr = KLSTRT;	/* enable interrupts */
}

/* ARGSUSED */
kgclock(dev, r0, r1, r2, r3, r4 ,r5, pc, ps)
	caddr_t pc;
	int ps;
{
	register int k;
	static long otime;
	static long calibrate;

	if (usekgclock == 0) {
		phz = 0;
		otime = 0;
		return;
	}
	klbase->tbuf = 0377;	/* reprime clock (scope sync too) */
	if (phz == 0) {
		if (otime == 0) {
			otime = time.tv_sec + 1;
			calibrate = 0;
		}
		if (time.tv_sec >= otime)
			calibrate++;
		if (time.tv_sec >= otime + 4) {
			phz = calibrate / 4;
			otime = 0;
		}
		return;
	}
	gatherstats(pc, ps);	/* this routine lives in kern_clock.c */
}
#endif
