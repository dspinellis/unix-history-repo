/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)machdep.c	7.1 (Berkeley) %G%
 */

#include <sys/param.h>
#include <luna68k/include/reg.h>

straytrap(addr)
	register int addr;
{
	printf("stray trap, addr 0x%x\n", addr);
}

int	*nofault = 0;

badaddr(addr)
	register caddr_t addr;
{
	register int i;
	label_t	faultbuf;

#ifdef lint
	i = *addr; if (i) return(0);
#endif
	nofault = (int *) &faultbuf;
	if (setjmp((label_t *)nofault)) {
		nofault = (int *) 0;
		return(1);
	}
	i = *(volatile short *)addr;
	nofault = (int *) 0;
	return(0);
}

regdump(rp, sbytes)
  int *rp; /* must not be register */
  int sbytes;
{
	static int doingdump = 0;
	register int i;
	int s;
	extern char *hexstr();

	if (doingdump)
		return;
	s = splhigh();
	doingdump = 1;
/*	printf("pid = %d, pc = %s, ", u.u_procp->p_pid, hexstr(rp[PC], 8));	*/
	printf("pc = %s, ", hexstr(rp[PC], 8));
	printf("ps = %s, ", hexstr(rp[PS], 4));
	printf("sfc = %s, ", hexstr(getsfc(), 4));
	printf("dfc = %s\n", hexstr(getdfc(), 4));
/*
	printf("p0 = %x@%s, ",
	       u.u_pcb.pcb_p0lr, hexstr((int)u.u_pcb.pcb_p0br, 8));
	printf("p1 = %x@%s\n\n",
	       u.u_pcb.pcb_p1lr, hexstr((int)u.u_pcb.pcb_p1br, 8));
*/
	printf("Registers:\n     ");
	for (i = 0; i < 8; i++)
		printf("        %d", i);
	printf("\ndreg:");
	for (i = 0; i < 8; i++)
		printf(" %s", hexstr(rp[i], 8));
	printf("\nareg:");
	for (i = 0; i < 8; i++)
		printf(" %s", hexstr(rp[i+8], 8));
	if (sbytes > 0) {
/*		if (rp[PS] & PSL_S) {	*/
			printf("\n\nKernel stack (%s):",
			       hexstr((int)(((int *)&rp)-1), 8));
			dumpmem(((int *)&rp)-1, sbytes, 0);
/*
		} else {
			printf("\n\nUser stack (%s):", hexstr(rp[SP], 8));
			dumpmem((int *)rp[SP], sbytes, 1);
		}
*/
	}
	doingdump = 0;
	splx(s);
}

/*	#define KSADDR	((int *)&(((char *)&u)[(UPAGES-1)*NBPG]))	*/

dumpmem(ptr, sz, ustack)
 register int *ptr;
 int sz;
{
	register int i, val;
	extern char *hexstr();

	for (i = 0; i < sz; i++) {
		if ((i & 7) == 0)
			printf("\n%s: ", hexstr((int)ptr, 6));
		else
			printf(" ");
/*
		if (ustack == 1) {
			if ((val = fuword(ptr++)) == -1)
				break;
		} else {
			if (ustack == 0 && (ptr < KSADDR || ptr > KSADDR+(NBPG/4-1)))
				break;
*/
			val = *ptr++;
/*		}	*/
		printf("%s", hexstr(val, 8));
	}
	printf("\n");
}

char *
hexstr(val, len)
	register int val;
{
	static char nbuf[9];
	register int x, i;

	if (len > 8)
		return("");
	nbuf[len] = '\0';
	for (i = len-1; i >= 0; --i) {
		x = val & 0xF;
		if (x > 9)
			nbuf[i] = x - 10 + 'A';
		else
			nbuf[i] = x + '0';
		val >>= 4;
	}
	return(nbuf);
}
