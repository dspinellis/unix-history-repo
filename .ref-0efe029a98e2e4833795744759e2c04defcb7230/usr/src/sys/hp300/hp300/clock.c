/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: clock.c 1.17 89/11/30$
 *
 *	@(#)clock.c	7.4 (Berkeley) %G%
 */

#include "sys/param.h"
#include "sys/user.h"
#include "sys/kernel.h"
#include "../dev/hilreg.h"
#include "clockreg.h"

#include "../include/psl.h"
#include "../include/cpu.h"

#if defined(GPROF) && defined(PROFTIMER)
#include "sys/gprof.h"
#endif

int    clkstd[1];

static int month_days[12] = {
	31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
};
struct bbc_tm *gmt_to_bbc();
u_char bbc_registers[13];
u_char write_bbc_reg(), read_bbc_reg();
struct hil_dev *bbcaddr = NULL;

/*
 * Machine-dependent clock routines.
 *
 * Startrtclock restarts the real-time clock, which provides
 * hardclock interrupts to kern_clock.c.
 *
 * Inittodr initializes the time of day hardware which provides
 * date functions.
 *
 * Resettodr restores the time of day hardware after a time change.
 *
 * A note on the real-time clock:
 * We actually load the clock with CLK_INTERVAL-1 instead of CLK_INTERVAL.
 * This is because the counter decrements to zero after N+1 enabled clock
 * periods where N is the value loaded into the counter.
 */

/*
 * Start the real-time clock.
 */
startrtclock()
{
	register struct clkreg *clk;

	clkstd[0] = IOV(0x5F8000);
	clk = (struct clkreg *) clkstd[0];

	clk->clk_cr2 = CLK_CR1;
	clk->clk_cr1 = CLK_RESET;
	clk->clk_cr2 = CLK_CR3;
	clk->clk_cr3 = 0;
	clk->clk_msb1 = (CLK_INTERVAL-1) >> 8 & 0xFF;
	clk->clk_lsb1 = (CLK_INTERVAL-1) & 0xFF;
	clk->clk_msb2 = 0;
	clk->clk_lsb2 = 0;
	clk->clk_msb3 = 0;
	clk->clk_lsb3 = 0;
	clk->clk_cr2 = CLK_CR1;
	clk->clk_cr1 = CLK_IENAB;
}

/*
 * Returns number of usec since last recorded clock "tick"
 * (i.e. clock interrupt).
 */
clkread()
{
	register struct clkreg *clk = (struct clkreg *) clkstd[0];
	register int high, low;

	high = clk->clk_msb1;
	low = clk->clk_lsb1;
	if (high != clk->clk_msb1)
		high = clk->clk_msb1;

	high = (CLK_INTERVAL-1) - ((high << 8) | low);
	/*
	 * Pending interrupt indicates that the counter has wrapped
	 * since we went to splhigh().  Need to compensate.
	 */
	if (clk->clk_sr & CLK_INT1)
		high += CLK_INTERVAL;
	return((high * tick) / CLK_INTERVAL);
}

#include "clock.h"
#if NCLOCK > 0
/*
 * /dev/clock: mappable high resolution timer.
 *
 * This code implements a 32-bit recycling counter (with a 4 usec period)
 * using timers 2 & 3 on the 6840 clock chip.  The counter can be mapped
 * RO into a user's address space to achieve low overhead (no system calls),
 * high-precision timing.
 *
 * Note that timer 3 is also used for the high precision profiling timer
 * (PROFTIMER code above).  Care should be taken when both uses are
 * configured as only a token effort is made to avoid conflicting use.
 */
#include "sys/proc.h"
#include "sys/ioctl.h"
#include "sys/malloc.h"
#include "clockioctl.h"
#include "vm/vm_param.h"
#include "vm/vm_pager.h"
#include "vm/vm_prot.h"
#include "sys/specdev.h"
#include "sys/vnode.h"
#include "sys/mman.h"

int clockon = 0;		/* non-zero if high-res timer enabled */
#ifdef PROFTIMER
int  profprocs = 0;		/* # of procs using profiling timer */
#endif
#ifdef DEBUG
int clockdebug = 0;
#endif

/*ARGSUSED*/
clockopen(dev, flags)
	dev_t dev;
{
#ifdef PROFTIMER
#ifdef GPROF
	/*
	 * Kernel profiling enabled, give up.
	 */
	if (profiling)
		return(EBUSY);
#endif
	/*
	 * If any user processes are profiling, give up.
	 */
	if (profprocs)
		return(EBUSY);
#endif
	if (!clockon) {
		startclock();
		clockon++;
	}
	return(0);
}

/*ARGSUSED*/
clockclose(dev, flags)
	dev_t dev;
{
	(void) clockunmmap(dev, (caddr_t)0);
	stopclock();
	clockon = 0;
	return(0);
}

/*ARGSUSED*/
clockioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	int error = 0;
	
	switch (cmd) {

	case CLOCKMAP:
		error = clockmmap(dev, (caddr_t *)data);
		break;

	case CLOCKUNMAP:
		error = clockunmmap(dev, *(caddr_t *)data);
		break;

	case CLOCKGETRES:
		*(int *)data = CLK_RESOLUTION;
		break;

	default:
		error = EINVAL;
		break;
	}
	return(error);
}

/*ARGSUSED*/
clockmap(dev, off, prot)
	dev_t dev;
{
	return((off + (IOBASE+CLKBASE+CLKSR-1)) >> PGSHIFT);
}

clockmmap(dev, addrp)
	dev_t dev;
	caddr_t *addrp;
{
	struct proc *p = u.u_procp;		/* XXX */
	int error;
	struct vnode vn;
	struct specinfo si;
	int flags;

	flags = MAP_FILE|MAP_SHARED;
	if (*addrp)
		flags |= MAP_FIXED;
	else
		*addrp = (caddr_t)0x1000000;	/* XXX */
	vn.v_type = VCHR;			/* XXX */
	vn.v_specinfo = &si;			/* XXX */
	vn.v_rdev = dev;			/* XXX */
	error = vm_mmap(u.u_procp->p_map, (vm_offset_t *)addrp,
			PAGE_SIZE, VM_PROT_ALL, flags, (caddr_t)&vn, 0);
	return(error);
}

clockunmmap(dev, addr)
	dev_t dev;
	caddr_t addr;
{
	struct proc *p = u.u_procp;		/* XXX */
	int rv;

	if (addr == 0)
		return(EINVAL);		/* XXX: how do we deal with this? */
	rv = vm_deallocate(u.u_procp->p_map, (vm_offset_t)addr, PAGE_SIZE);
	return(rv == KERN_SUCCESS ? 0 : EINVAL);
}

startclock()
{
	register struct clkreg *clk = (struct clkreg *)clkstd[0];

	clk->clk_msb2 = -1; clk->clk_lsb2 = -1;
	clk->clk_msb3 = -1; clk->clk_lsb3 = -1;

	clk->clk_cr2 = CLK_CR3;
	clk->clk_cr3 = CLK_OENAB|CLK_8BIT;
	clk->clk_cr2 = CLK_CR1;
	clk->clk_cr1 = CLK_IENAB;
}

stopclock()
{
	register struct clkreg *clk = (struct clkreg *)clkstd[0];

	clk->clk_cr2 = CLK_CR3;
	clk->clk_cr3 = 0;
	clk->clk_cr2 = CLK_CR1;
	clk->clk_cr1 = CLK_IENAB;
}
#endif

#ifdef PROFTIMER
/*
 * This code allows the hp300 kernel to use one of the extra timers on
 * the clock chip for profiling, instead of the regular system timer.
 * The advantage of this is that the profiling timer can be turned up to
 * a higher interrupt rate, giving finer resolution timing. The profclock
 * routine is called from the lev6intr in locore, and is a specialized
 * routine that calls addupc. The overhead then is far less than if
 * hardclock/softclock was called. Further, the context switch code in
 * locore has been changed to turn the profile clock on/off when switching
 * into/out of a process that is profiling (startprofclock/stopprofclock).
 * This reduces the impact of the profiling clock on other users, and might
 * possibly increase the accuracy of the profiling. 
 */
int  profint   = PRF_INTERVAL;	/* Clock ticks between interrupts */
int  profscale = 0;		/* Scale factor from sys clock to prof clock */
char profon    = 0;		/* Is profiling clock on? */

/* profon values - do not change, locore.s assumes these values */
#define PRF_NONE	0x00
#define	PRF_USER	0x01
#define	PRF_KERNEL	0x80

initprofclock()
{
#if NCLOCK > 0
	/*
	 * If the high-res timer is running, force profiling off.
	 * Unfortunately, this gets reflected back to the user not as
	 * an error but as a lack of results.
	 */
	if (clockon) {
		u.u_prof.pr_scale = 0;
		return;
	}
	/*
	 * Keep track of the number of user processes that are profiling
	 * by checking the scale value.
	 *
	 * XXX: this all assumes that the profiling code is well behaved;
	 * i.e. profil() is called once per process with pcscale non-zero
	 * to turn it on, and once with pcscale zero to turn it off.
	 * Also assumes you don't do any forks or execs.  Oh well, there
	 * is always adb...
	 */
	if (u.u_prof.pr_scale)
		profprocs++;
	else
		profprocs--;
#endif
	/*
	 * The profile interrupt interval must be an even divisor
	 * of the CLK_INTERVAL so that scaling from a system clock
	 * tick to a profile clock tick is possible using integer math.
	 */
	if (profint > CLK_INTERVAL || (CLK_INTERVAL % profint) != 0)
		profint = CLK_INTERVAL;
	profscale = CLK_INTERVAL / profint;
}

startprofclock()
{
	register struct clkreg *clk = (struct clkreg *)clkstd[0];

	clk->clk_msb3 = (profint-1) >> 8 & 0xFF;
	clk->clk_lsb3 = (profint-1) & 0xFF;

	clk->clk_cr2 = CLK_CR3;
	clk->clk_cr3 = CLK_IENAB;
}

stopprofclock()
{
	register struct clkreg *clk = (struct clkreg *)clkstd[0];

	clk->clk_cr2 = CLK_CR3;
	clk->clk_cr3 = 0;
}

#ifdef GPROF
/*
 * profclock() is expanded in line in lev6intr() unless profiling kernel.
 * Assumes it is called with clock interrupts blocked.
 */
profclock(pc, ps)
	caddr_t pc;
	int ps;
{
	/*
	 * Came from user mode.
	 * If this process is being profiled record the tick.
	 */
	if (USERMODE(ps)) {
		if (u.u_prof.pr_scale)
			addupc(pc, &u.u_prof, 1);
	}
	/*
	 * Came from kernel (supervisor) mode.
	 * If we are profiling the kernel, record the tick.
	 */
	else if (profiling < 2) {
		register int s = pc - s_lowpc;

		if (s < s_textsize)
			kcount[s / (HISTFRACTION * sizeof (*kcount))]++;
	}
	/*
	 * Kernel profiling was on but has been disabled.
	 * Mark as no longer profiling kernel and if all profiling done,
	 * disable the clock.
	 */
	if (profiling && (profon & PRF_KERNEL)) {
		profon &= ~PRF_KERNEL;
		if (profon == PRF_NONE)
			stopprofclock();
	}
}
#endif
#endif

/*
 * Initialize the time of day register, based on the time base which is, e.g.
 * from a filesystem.
 */
inittodr(base)
	time_t base;
{
	u_long timbuf = base;	/* assume no battery clock exists */
	static int bbcinited = 0;

	/* XXX */
	if (!bbcinited) {
		if (badbaddr(&BBCADDR->hil_stat))
			printf("WARNING: no battery clock\n");
		else
			bbcaddr = BBCADDR;
		bbcinited = 1;
	}

	/*
	 * bbc_to_gmt converts and stores the gmt in timbuf.
	 * If an error is detected in bbc_to_gmt, or if the filesystem
	 * time is more recent than the gmt time in the clock,
	 * then use the filesystem time and warn the user.
 	 */
	if (!bbc_to_gmt(&timbuf) || timbuf < base) {
		printf("WARNING: bad date in battery clock\n");
		timbuf = base;
	}
	if (base < 5*SECYR) {
		printf("WARNING: preposterous time in file system");
		timbuf = 6*SECYR + 186*SECDAY + SECDAY/2;
		printf(" -- CHECK AND RESET THE DATE!\n");
	}
	
	/* Battery clock does not store usec's, so forget about it. */
	time.tv_sec = timbuf;
}

resettodr()
{
	register int i;
	register struct bbc_tm *tmptr;

	tmptr = gmt_to_bbc(time.tv_sec);

	decimal_to_bbc(0, 1,  tmptr->tm_sec);
	decimal_to_bbc(2, 3,  tmptr->tm_min);
	decimal_to_bbc(4, 5,  tmptr->tm_hour);
	decimal_to_bbc(7, 8,  tmptr->tm_mday);
	decimal_to_bbc(9, 10, tmptr->tm_mon);
	decimal_to_bbc(11, 12, tmptr->tm_year);

	/* Some bogusness to deal with seemingly broken hardware. Nonsense */
	bbc_registers[5] = ((tmptr->tm_hour / 10) & 0x03) + 8;

	write_bbc_reg(15, 13);	/* reset prescalar */

	for (i = 0; i <= NUM_BBC_REGS; i++)
	  	if (bbc_registers[i] != write_bbc_reg(i, bbc_registers[i])) {
			printf("Cannot set battery backed clock\n");
			break;
		}
}

struct bbc_tm *
gmt_to_bbc(tim)
	long tim;
{
	register int i;
	register long hms, day;
	static struct bbc_tm rt;

	day = tim / SECDAY;
	hms = tim % SECDAY;

	/* Hours, minutes, seconds are easy */
	rt.tm_hour = hms / 3600;
	rt.tm_min  = (hms % 3600) / 60;
	rt.tm_sec  = (hms % 3600) % 60;

	/* Number of years in days */
	for (i = STARTOFTIME - 1900; day >= days_in_year(i); i++)
	  	day -= days_in_year(i);
	rt.tm_year = i;
	
	/* Number of months in days left */
	if (leapyear(rt.tm_year))
		days_in_month(FEBRUARY) = 29;
	for (i = 1; day >= days_in_month(i); i++)
		day -= days_in_month(i);
	days_in_month(FEBRUARY) = 28;
	rt.tm_mon = i;

	/* Days are what is left over (+1) from all that. */
	rt.tm_mday = day + 1;  
	
	return(&rt);
}

bbc_to_gmt(timbuf)
	u_long *timbuf;
{
	register int i;
	register u_long tmp;
	int year, month, day, hour, min, sec;

	read_bbc();

	sec = bbc_to_decimal(1, 0);
	min = bbc_to_decimal(3, 2);

	/*
	 * Hours are different for some reason. Makes no sense really.
	 */
	hour  = ((bbc_registers[5] & 0x03) * 10) + bbc_registers[4];
	day   = bbc_to_decimal(8, 7);
	month = bbc_to_decimal(10, 9);
	year  = bbc_to_decimal(12, 11) + 1900;

	range_test(hour, 0, 23);
	range_test(day, 1, 31);
	range_test(month, 1, 12);
	range_test(year, STARTOFTIME, 2000);

	tmp = 0;

	for (i = STARTOFTIME; i < year; i++)
		tmp += days_in_year(i);
	if (leapyear(year) && month > FEBRUARY)
		tmp++;

	for (i = 1; i < month; i++)
	  	tmp += days_in_month(i);
	
	tmp += (day - 1);
	tmp = ((tmp * 24 + hour) * 60 + min) * 60 + sec;

	*timbuf = tmp;
	return(1);
}

read_bbc()
{
  	register int i, read_okay;

	read_okay = 0;
	while (!read_okay) {
		read_okay = 1;
		for (i = 0; i <= NUM_BBC_REGS; i++)
			bbc_registers[i] = read_bbc_reg(i);
		for (i = 0; i <= NUM_BBC_REGS; i++)
			if (bbc_registers[i] != read_bbc_reg(i))
				read_okay = 0;
	}
}

u_char
read_bbc_reg(reg)
	int reg;
{
	u_char data = reg;

	if (bbcaddr) {
		send_hil_cmd(bbcaddr, BBC_SET_REG, &data, 1, NULL);
		send_hil_cmd(bbcaddr, BBC_READ_REG, NULL, 0, &data);
	}
	return(data);
}

u_char
write_bbc_reg(reg, data)
	u_int data;
{
	u_char tmp;

	tmp = (u_char) ((data << HIL_SSHIFT) | reg);

	if (bbcaddr) {
		send_hil_cmd(bbcaddr, BBC_SET_REG, &tmp, 1, NULL);
		send_hil_cmd(bbcaddr, BBC_WRITE_REG, NULL, 0, NULL);
		send_hil_cmd(bbcaddr, BBC_READ_REG, NULL, 0, &tmp);
	}
	return(tmp);
}	
