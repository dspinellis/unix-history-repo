/*	idc.c	4.1	82/05/27	*/

/*
 * IDC (RB730)
 *
 * This driver is full of kludges!
 * It depends heavily on the 1K file system.
 */

#include "../h/param.h"
#include "../h/idcreg.h"
#include "../h/inode.h"
#include "../h/pte.h"
#include "../h/ubareg.h"
#include "saio.h"
#include "savax.h"

u_short	idcstd[] = { 0175606 };
short	rb02_off[] = { 0, 400, 0, -1, -1, -1, -1, -1 };
short	rb80_off[] = { 0, 37, 0, -1, -1, -1, 115, 305 };

int idc_type[4];

idcopen(io)
	register struct iob *io;
{
	register struct idcdevice *idcaddr;
	register int i;

	idcaddr = (struct idcdevice *)((caddr_t)ubauba(io->i_unit) + 0x200);
	if (io->i_boff < 0 || io->i_boff > 7)
		_stop("idc bad unit");
	idcaddr->idcmpr = IDCGS_GETSTAT;
	idcaddr->idccsr = IDC_GETSTAT|(io->i_unit<<8);
	idcwait(idcaddr);
	i = idcaddr->idcmpr;
	idcaddr->idccsr = IDC_CRDY|(1<<(io->i_unit+16));
	idcwait(idcaddr);
	idcaddr->idccsr = (io->i_unit<<8)|IDC_RHDR;
	idcwait(idcaddr);
	if (idcaddr->idccsr & IDC_ERR) {
		printf("idc error: idccsr %x\n", idcaddr->idccsr);
		_stop("idc fatal error");
	}
	i = idcaddr->idcmpr;
	i = idcaddr->idcmpr;
	if (idcaddr->idccsr & IDC_R80) {
		idc_type[io->i_unit] = 1;
		io->i_boff = rb80_off[io->i_boff];
	} else {
		idc_type[io->i_unit] = 0;
		io->i_boff = rb02_off[io->i_boff];
	}
	if (io->i_boff < 0)
		_stop("idc bad unit");
}

idcstrategy(io, func)
	register struct iob *io;
{
	register struct idcdevice *idcaddr;
	int com;
	daddr_t bn;
	short dn, cn, sn, tn;
	int ubinfo, errcnt = 0;

	idcaddr = (struct idcdevice *)((caddr_t)ubauba(io->i_unit) + 0x200);
retry:
	ubinfo = ubasetup(io, 1);
	bn = io->i_bn;
	dn = io->i_unit;
	if (io->i_cc != 1024) printf("idc: count %d != 1024\n", io->i_cc);
	if (idc_type[dn]) {
		cn = bn/(NRB80SECT*NRB80TRK);
		sn = bn%NRB80SECT;
		tn = (bn / NRB80SECT) % NRB80TRK;
		if (sn == NRB80SECT)
			io->i_cc = 512;
	} else {
		bn *= 2;
		cn = bn/(NRB02SECT*NRB02TRK);
		sn = bn%NRB02SECT;
		tn = (bn / NRB02SECT) % NRB02TRK;
	}
	cn += io->i_boff;
	idcaddr->idccsr = IDC_CRDY|IDC_SEEK|(dn<<8)|(1<<(dn+16));
	idcaddr->idcdar = (cn<<16)|(tn<<8)|sn;
	idcaddr->idccsr = IDC_SEEK|(dn<<8);
	idcwait(idcaddr);
	idcaddr->idccsr &= ~IDC_ATTN;
	com = dn<<8;
	if (func == READ)
		com |= IDC_READ;
	else
		com |= IDC_WRITE;
	idcaddr->idccsr = IDC_CRDY|com;
	idcaddr->idcbar = ubinfo&0x3ffff;
	idcaddr->idcbcr = -io->i_cc;
	idcaddr->idcdar = (cn<<16)|(tn<<8)|sn;
	idcaddr->idccsr = com;
	idcwait(idcaddr);
	ubafree(io, ubinfo);
	if (idcaddr->idccsr & IDC_ERR) {
		printf("idc error: (cyl,trk,sec)=(%d,%d,%d) csr=%b\n",
		    cn, tn, sn, idcaddr->idccsr, IDCCSR_BITS);
		if (errcnt == 10) {
			printf("idc: unrecovered error\n");
			return (-1);
		}
		errcnt++;
		goto retry;
	}
	if (errcnt)
		printf("idc: recovered by retry\n");
	if (idc_type[dn] && sn == NRB80SECT) {
		io->i_bn++;
		goto retry;
	}
	return (1024);
}

idcwait(idcaddr)
	register struct idcdevice *idcaddr;
{
	register int i;

	while ((idcaddr->idccsr & (IDC_CRDY|IDC_DRDY)) != (IDC_CRDY|IDC_DRDY))
		for (i = 10; i; i--)
			;
}
