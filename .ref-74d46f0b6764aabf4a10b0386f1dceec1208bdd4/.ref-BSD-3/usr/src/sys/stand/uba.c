#include "../h/param.h"
#include "../h/inode.h"
#include "../h/pte.h"
#include "../h/uba.h"
#include "../h/vm.h"
#include "saio.h"

ubasetup(io, bdp)
register struct iob *io;
{
	register int i;
	int npf;
	unsigned v;
	register struct pte *pte;
	int o, vaddr, temp;

	v = btop(io->i_ma);
	o = (int)io->i_ma & PGOFSET;
	npf = btoc(io->i_cc + o) +1;
	pte = &(((struct uba_regs *)PHYSUBA0)->uba_map[0]);
	temp = (bdp << 21) | MRV;
	if (bdp && (o & 01))
		temp |= BO;
	v &= 0x1fffff;			/* drop to physical addr */
	while (--npf != 0)
		*(int *)pte++ = v++ | temp;
	*(int *)pte++ = 0;
	return ((bdp << 28) | o);
}

ubafree(mr)
	int mr;
{
	register int bdp, reg, npf, a;
 
	bdp = (mr >> 28) & 0x0f;
	if (bdp)
		((struct uba_regs *)PHYSUBA0)->uba_dpr[bdp] |= BNE;
}
