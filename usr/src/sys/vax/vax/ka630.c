/*
 *	@(#)ka630.c	6.1 (Berkeley) %G%
 */
#if defined(VAX630)
/* ka630.c routines for the ka630 clock chip... */
#include "param.h"
#include "time.h"
#include "kernel.h"
#include "vmmac.h"

#include "mtpr.h"
#include "cpu.h"
#include "clock.h"
#include "pte.h"
#include "ka630.h"

/*
 * These two fuctions handle the tod clock
 * This code is defunct at the end of the century.
 * Will Unix still be here then??
 */

struct cldevice cldevice;
struct ka630cpu ka630cpu;

short dayyr[12] = { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, };
/* Starts the tod clock. Called from clkstart... */
ka630tod(base)
	time_t base;
{
	register int tmp1, tmp2;
	struct pte *pte = &Clockmap[0];
	register struct cldevice *claddr = &cldevice;
	struct ka630cpu *ka630addr = &ka630cpu;

	/* Enable system page for registers */
	*(int *)pte = PG_V|PG_KW|btop(0x200b8000);
	pte = &Ka630map[0];
	*(int *)pte = PG_V|PG_KW|btop(0x20080000);
	mtpr(TBIA, 0);
	/*
	 * Clear restart and boot in progress flags in the CPMBX. This has
	 * nothing to do with the clock except that it the CPMBX reg. is a
	 * byte in the clock's ram.
	 */
	claddr->cpmbx=(u_short)((claddr->cpmbx&KA630CLK_LANG)|KA630CLK_REBOOT);
	/*
	 * Enable memory parity error detection. again nothing to do with the
	 * tod clock except for being a convenient place.
	 */
	ka630addr->ka630_mser = KA630MSER_PAREN;
	claddr->csr1 = KA630CLK_SET;
	while ((claddr->csr0 & KA630CLK_UIP) != 0)
		;
	/* If the clock is valid, use it. */
	if ((claddr->csr3 & KA630CLK_VRT) != 0 &&
	    (claddr->csr1 & KA630CLK_ENABLE) == KA630CLK_ENABLE) {
		/* Convert yr,mon,day,hr,min,sec to sec past Jan.1, 1970. */
		tmp2 = 0;
		for (tmp1 = 70; tmp1 < claddr->yr; tmp1++) {
			tmp2 += 365;
			/* I just luv leap years... */
			if (LEAPYEAR(tmp1))
				tmp2++;
		}
		tmp2 += (dayyr[claddr->mon-1]+claddr->day-1);
		if (LEAPYEAR(claddr->yr) && claddr->mon > 2)
			tmp2++;
		/* Finally got days past Jan. 1,1970. the rest is easy.. */
		time.tv_sec = tmp2*SECDAY+claddr->hr*HRSEC+
			claddr->min*MINSEC+claddr->sec;
		tmp1 = claddr->csr2;
		claddr->csr0 = KA630CLK_RATE;
		claddr->csr1 = KA630CLK_ENABLE;
	} else if (base < 5*SECYR) {
		printf("WARNING: preposterous time in file system\n");
		time.tv_sec = 6*SECYR+186*SECDAY+SECDAY/2;
		ka630stod();
	} else {
		printf("WARNING: Time set via file system\n");
		time.tv_sec = base;
		ka630stod();
	}
}
/* Set the time of day clock, called via. stime system call.. */
ka630stod()
{
	register int tmp1, tmp3;
	register struct cldevice *claddr = &cldevice;
	long tmp2, tmp4;

	claddr->csr1 = KA630CLK_SET;
	while ((claddr->csr0 & KA630CLK_UIP) != 0)
		;
	/* The reverse of above, sec. past Jan. 1,1970 to yr, mon... */
	tmp2 = time.tv_sec/HRSEC;
	tmp4 = tmp2 = tmp2/24;
	tmp1 = 69;
	while (tmp2 >= 0) {
		tmp3 = tmp2;
		tmp2 -= 365;
		tmp1++;
		if (LEAPYEAR(tmp1))
			tmp2--;
	}
	/* Got the year... */
	claddr->yr = tmp1;
	tmp1 = -1;
	do {
		tmp2 = tmp3-dayyr[++tmp1];
		if (LEAPYEAR(claddr->yr) && tmp1 > 1)
			tmp2--;
	} while (tmp2 >= 0);
	/* Finally, got the rest... */
	claddr->mon = tmp1;
	claddr->day = tmp3-dayyr[tmp1-1]+1;
	if (LEAPYEAR(claddr->yr) && tmp1 > 2)
		claddr->day--;
	tmp2 = time.tv_sec-(tmp4*SECDAY);
	claddr->hr = tmp2/HRSEC;
	tmp2 = tmp2%HRSEC;
	claddr->min = tmp2/MINSEC;
	tmp2 = tmp2%MINSEC;
	claddr->sec = tmp2;
	tmp1 = claddr->csr2;
	tmp1 = claddr->csr3;
	claddr->csr0 = KA630CLK_RATE;
	claddr->csr1 = KA630CLK_ENABLE;
}
#endif
