/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)idc.c	7.2 (Berkeley) %G%
 */

/*
 * IDC (RB730)
 */
#include "../machine/pte.h"

#include "param.h"
#include "inode.h"
#include "fs.h"

#include "../vaxuba/idcreg.h"
#include "../vaxuba/ubareg.h"

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
	if ((unsigned)io->i_boff > 7) {
		printf("idc bad unit");
		return (EUNIT);
	}
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
		return (EIO);
	}
	i = idcaddr->idcmpr;
	i = idcaddr->idcmpr;
	if (idcaddr->idccsr & IDC_R80) {
		idc_type[io->i_unit] = 1;
		io->i_boff = rb80_off[io->i_boff] * NRB80SECT * NRB80TRK;
	} else {
		idc_type[io->i_unit] = 0;
		io->i_boff = rb02_off[io->i_boff] * NRB02SECT/2 * NRB02TRK;
	}
	if (io->i_boff < 0) {
		printf("idc%d: bad unit type", io->i_unit);
		return (EUNIT);
	}
	return (0);
}

idcstrategy(io, func)
	register struct iob *io;
{
	register struct idcdevice *idcaddr;
	int com;
	daddr_t bn;
	short dn, cn, sn, tn;
	short ccleft, thiscc = 0;
	int ubinfo, errcnt = 0;

	idcaddr = (struct idcdevice *)((caddr_t)ubauba(io->i_unit) + 0x200);
	ubinfo = ubasetup(io, 1);
	bn = io->i_bn;
	ccleft = io->i_cc;
retry:
	dn = io->i_unit;
	if (idc_type[dn]) {
		cn = bn/(NRB80SECT*NRB80TRK);
		sn = bn%NRB80SECT;
		tn = (bn / NRB80SECT) % NRB80TRK;
		thiscc = (NRB80SECT - sn) * 512;
	} else {
		cn = 2*bn/(NRB02SECT*NRB02TRK);
		sn = (2*bn)%NRB02SECT;
		tn = (2*bn / NRB02SECT) % NRB02TRK;
		thiscc = (NRB02SECT - sn) * 256;
	}
	thiscc = MIN(thiscc, ccleft);
	ccleft -= thiscc;
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
	idcaddr->idcbcr = -thiscc;
	idcaddr->idcdar = (cn<<16)|(tn<<8)|sn;
	idcaddr->idccsr = com;
	idcwait(idcaddr);
	if (idcaddr->idccsr & IDC_ERR) {
		printf("idc%d error: (cyl,trk,sec)=(%d,%d,%d) csr=%b\n",
		    dn, cn, tn, sn, idcaddr->idccsr, IDCCSR_BITS);
		if (errcnt == 10) {
			printf("idc: unrecovered error\n");
			ubafree(io, ubinfo);
			return (-1);
		}
		errcnt++;
		goto retry;
	}
	if (errcnt)
		printf("idc: recovered by retry\n");
	if (ccleft) {
		bn += thiscc/NBPG;
		ubinfo += thiscc;
		goto retry;
	}
	ubafree(io, ubinfo);
	return (io->i_cc);
}

idcwait(idcaddr)
	register struct idcdevice *idcaddr;
{
	register int i;

	while ((idcaddr->idccsr & (IDC_CRDY|IDC_DRDY)) != (IDC_CRDY|IDC_DRDY))
		for (i = 10; i; i--)
			;
}

/*ARGSUSED*/
idcioctl(io, cmd, arg)
	struct iob *io;
	int cmd;
	caddr_t arg;
{

	return (ECMD);
}
