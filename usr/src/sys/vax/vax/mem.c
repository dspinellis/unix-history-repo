/*	mem.c	4.3	81/03/09	*/

/*
 * Memory special file
 *	minor device 0 is physical memory
 *	minor device 1 is kernel memory 
 *	minor device 2 is EOF/RATHOLE
 *	minor device 3 is unibus memory (addressed by shorts)
 */

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/conf.h"
#include "../h/buf.h"
#include "../h/systm.h"
#include "../h/pte.h"
#include "../h/mtpr.h"
#include "../h/vm.h"
#include "../h/cmap.h"

mmread(dev)
{
	register int o;
	register unsigned c, v;

	switch (minor(dev)) {

	case 0:
		while (u.u_count != 0 && u.u_error == 0) {
			if (fubyte(u.u_base) == -1)
				goto fault;
			v = btop(u.u_offset);
			if (v >= physmem)
				goto fault;
			*(int *)mmap = v | (PG_V | PG_KR);
			mtpr(TBIS, vmmap);
			o = (int)u.u_offset & PGOFSET;
			c = min((unsigned)(NBPG - o), u.u_count);
			c = min(c, (unsigned)(NBPG - ((int)u.u_base&PGOFSET)));
			if (copyout((caddr_t)&vmmap[o], u.u_base, c))
				goto fault;
			u.u_count -= c;
			u.u_base += c;
			u.u_offset += c;
		}
		return;

	case 1:
		if ((caddr_t)u.u_offset < (caddr_t)&umbabeg &&
		    (caddr_t)u.u_offset + u.u_count >= (caddr_t)&umbabeg)
			goto fault;
		if ((caddr_t)u.u_offset >= (caddr_t)&umbabeg &&
		    (caddr_t)u.u_offset < (caddr_t)&umbaend)
			goto fault;
		if (!kernacc((caddr_t)u.u_offset, u.u_count, B_READ))
			goto fault;
		if (copyout((caddr_t)u.u_offset, u.u_base, u.u_count))
			goto fault;
		c = u.u_count;
		u.u_count = 0;
		u.u_base += c;
		u.u_offset += c;
		return;

	case 2:
		return;

	case 3:
		if (!kernacc((caddr_t)u.u_offset, u.u_count, B_READ))
			goto fault;
		if (!useracc(u.u_base, u.u_count, B_WRITE))
			goto fault;
		UNIcpy((caddr_t)u.u_offset, u.u_base, u.u_count, B_READ);
		c = u.u_count;
		u.u_count = 0;
		u.u_base += c;
		u.u_offset += c;
		return;
	}
fault:
	u.u_error = EFAULT;
	return;
}

mmwrite(dev)
{
	register int o;
	register unsigned c, v;

	switch (minor(dev)) {

	case 0:
		while (u.u_count != 0 && u.u_error == 0) {
			if (fubyte(u.u_base) == -1)
				goto fault;
			v = btop(u.u_offset);
			if (v >= physmem)
				goto fault;
			*(int *)mmap = v | (PG_V | PG_KW);
			mtpr(TBIS, vmmap);
			o = (int)u.u_offset & PGOFSET;
			c = min((unsigned)(NBPG - o), u.u_count);
			c = min(c, (unsigned)(NBPG - ((int)u.u_base&PGOFSET)));
			if (copyin(u.u_base, (caddr_t)&vmmap[o], c))
				goto fault;
			u.u_count -= c;
			u.u_base += c;
			u.u_offset += c;
		}
		return;

	case 1:
		if (!kernacc((caddr_t)u.u_offset, u.u_count, B_WRITE))
			goto fault;
		if (copyin(u.u_base, (caddr_t)u.u_offset, u.u_count))
			goto fault;
		u.u_base += u.u_count;
		u.u_offset += u.u_count;
		u.u_count = 0;
		return;

	case 2:
		u.u_offset += u.u_count;
		u.u_count = 0;
		return;

	case 3:
		if (!kernacc((caddr_t)u.u_offset, u.u_count, B_WRITE))
			goto fault;
		if (!useracc(u.u_base, u.u_count, B_READ))
			goto fault;
		UNIcpy((caddr_t)u.u_offset, u.u_base, u.u_count, B_WRITE);
		u.u_base += u.u_count;
		u.u_offset += u.u_count;
		u.u_count = 0;
		return;
	}
fault:
	u.u_error = EFAULT;
	return;
}

/*
 * UNIBUS Address Space <--> User Space transfer
 */
UNIcpy(uniadd, usradd, bknt, direct)
	caddr_t uniadd, usradd;
	unsigned bknt;
{
	register short *from, *to;
	register int i;
 
	if (direct == B_READ) {
		from = (short *) uniadd;
		to = (short *) usradd;
	} else {
		from = (short *) usradd;
		to = (short *) uniadd;
	}
	for (i = (bknt>>1); i > 0; i--)
		*to++ = *from++;
}
