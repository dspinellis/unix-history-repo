/*
 * code to manage AT bus
 * @(#)isa.c	1.1 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "conf.h"
#include "file.h"
#include "dir.h"
#include "user.h"
#include "buf.h"
#include "vm.h"
#include "uio.h"
#include "machine/pte.h"

/* stuff needed for virtual to physical calculations */
extern char Sysbase;
static unsigned long sbase = (unsigned long) &Sysbase;

struct buf *dma_bounce[8];
#define MAXDMASZ 512

/* XXX temporary */
kernel_space(x)
unsigned long x;
{
	if ((x >= sbase) & (x < sbase + 0x800000)) return 1;
	else return 0;
}


/****************************************************************************/
/*                                 at_dma                                   */
/* set up DMA read/write operation and virtual address addr for nbytes      */
/****************************************************************************/
at_dma(read,addr,nbytes, chan)
int read;
unsigned long addr;
int nbytes;
{
	unsigned long phys;
	int s,raw;
	caddr_t bounce;

	if (kernel_space(addr)) raw = 0;
	else raw = 1;

	if(raw) {
		if (dma_bounce[chan] == 0)
			dma_bounce[chan] = geteblk(MAXDMASZ);
		bounce = dma_bounce[chan]->b_un.b_addr;
	}

	/* copy bounce buffer on write */
	if (raw && !read) bcopy(addr,bounce,nbytes);

	/* Set read/write bytes */
	if (read) {
		outb(0xC,0x46); outb(0xB,0x46);
	} else {
		outb(0xC,0x4A); outb(0xB,0x4A);
	}
	/* Send start address */
	if (raw) phys = (unsigned long) bounce;
	else phys = addr;
	/* translate to physical */
	phys = phys - sbase;
	outb(0x4,phys & 0xFF);
	outb(0x4,(phys>>8) & 0xFF);
	outb(0x81,(phys>>16) & 0xFF);
	/* Send count */
	nbytes--;
	outb(0x5,nbytes & 0xFF);
	outb(0x5,(nbytes>>8) & 0xFF);
	/* set channel 2 */
	outb(0x0A,chan);
}
