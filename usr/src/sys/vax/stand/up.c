/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)up.c	6.4 (Berkeley) %G%
 */

/*
 * UNIBUS peripheral standalone driver
 * with ECC correction and bad block forwarding.
 * Also supports header operation and write
 * check for data and/or header.
 */
#include "../h/param.h" 
#include "../h/inode.h"
#include "../h/fs.h"
#include "../h/dkbad.h"
#include "../h/vmmac.h"

#include "../vax/pte.h"
#include "../vaxuba/upreg.h"
#include "../vaxuba/ubareg.h"

#include "saio.h"
#include "savax.h"

#define MAXBADDESC	126	/* max number of bad sectors recorded */
#define SECTSIZ		512	/* sector size in bytes */
#define HDRSIZ		4	/* number of bytes in sector header */

#define MAXECC		5	/* max # bad bits allowed on ecc w/ F_ECCLM */

u_short	ubastd[] = { 0776700 };

char	up_gottype[MAXNUBA*8];
char	up_type[MAXNUBA*8];
extern	struct st upst[];

struct  dkbad upbad[MAXNUBA*8];		/* bad sector table */
int 	sectsiz;			/* real sector size */
int	updebug[MAXNUBA*8];
#define	UPF_BSEDEBUG	01	/* debugging bad sector forwarding */
#define	UPF_ECCDEBUG	02	/* debugging ecc correction */

u_char	up_offset[16] = {
	UPOF_P400, UPOF_M400, UPOF_P400, UPOF_M400,
	UPOF_P800, UPOF_M800, UPOF_P800, UPOF_M800, 
	UPOF_P1200, UPOF_M1200, UPOF_P1200, UPOF_M1200,
	0, 0, 0, 0
};

upopen(io)
	register struct iob *io;
{
	register unit = io->i_unit;
	register struct updevice *upaddr;
	register struct st *st;

	if (io->i_boff < 0 || io->i_boff > 7)
		_stop("up bad unit");
	upaddr = (struct updevice *)ubamem(unit, ubastd[0]);
	upaddr->upcs2 = unit % 8;
	while ((upaddr->upcs1 & UP_DVA) == 0)
		;
	if (up_gottype[unit] == 0) {
		register int i;
		struct iob tio;

		up_type[unit] = upmaptype(unit, upaddr);
		if (up_type[unit] < 0)
			_stop("unknown drive type");
		st = &upst[up_type[unit]];
		if (st->off[io->i_boff] == -1)
			_stop("up bad unit");
		/*
		 * Read in the bad sector table.
		 */
		tio = *io;
		tio.i_bn = st->nspc * st->ncyl - st->nsect;
		tio.i_ma = (char *)&upbad[tio.i_unit];
		tio.i_cc = sizeof (struct dkbad);
		tio.i_flgs |= F_RDDATA;
		for (i = 0; i < 5; i++) {
			if (upstrategy(&tio, READ) == sizeof (struct dkbad))
				break;
			tio.i_bn += 2;
		}
		if (i == 5) {
			printf("Unable to read bad sector table\n");
			for (i = 0; i < MAXBADDESC; i++) {
				upbad[unit].bt_bad[i].bt_cyl = -1;
				upbad[unit].bt_bad[i].bt_trksec = -1;
			}
		}	
		up_gottype[unit] = 1;
	}
	st = &upst[up_type[unit]];
	io->i_boff = st->off[io->i_boff] * st->nspc;
	io->i_flgs &= ~F_TYPEMASK;
}

upstrategy(io, func)
	register struct iob *io;
{
	int cn, tn, sn, o;
	register unit = io->i_unit;
	daddr_t bn;
	int recal, info, waitdry;
	register struct updevice *upaddr =
	    (struct updevice *)ubamem(unit, ubastd[0]);
	register struct st *st = &upst[up_type[unit]];
	int doprintf = 0;

	sectsiz = SECTSIZ;
	if (io->i_flgs & (F_HDR|F_HCHECK))
		sectsiz += HDRSIZ;
	upaddr->upcs2 = unit % 8;
	if ((upaddr->upds & UPDS_VV) == 0) {
		upaddr->upcs1 = UP_DCLR|UP_GO;
		upaddr->upcs1 = UP_PRESET|UP_GO;
		upaddr->upof = UPOF_FMT22;
	}
	if ((upaddr->upds & UPDS_DREADY) == 0)
		_stop("up not ready");
	info = ubasetup(io, 1);
	upaddr->upwc = -io->i_cc / sizeof (short);
	recal = 0;
	io->i_errcnt = 0;

restart: 
	o = io->i_cc + (upaddr->upwc * sizeof (short));
	upaddr->upba = info + o;
	bn = io->i_bn + o / sectsiz;
	if (doprintf && updebug[unit] & (UPF_ECCDEBUG|UPF_BSEDEBUG))
		printf("wc=%d o=%d i_bn=%d bn=%d\n",
			upaddr->upwc, o, io->i_bn, bn);
	while((upaddr->upds & UPDS_DRY) == 0)
		;
	if (upstart(io, bn) != 0) {
		ubafree(io, info);
		return (-1);
	}
	do {
		DELAY(25);
	} while ((upaddr->upcs1 & UP_RDY) == 0);
	/*
	 * If transfer has completed, free UNIBUS
	 * resources and return transfer size.
	 */
	if ((upaddr->upds&UPDS_ERR) == 0 && (upaddr->upcs1&UP_TRE) == 0)
		goto done;
	if (updebug[unit] & (UPF_ECCDEBUG|UPF_BSEDEBUG)) {
		printf("up error: (cyl,trk,sec)=(%d,%d,%d) ",
		  upaddr->updc, upaddr->upda>>8, upaddr->upda&0xff);
		printf("cs2=%b er1=%b er2=%b wc=%d\n",
	    	  upaddr->upcs2, UPCS2_BITS, upaddr->uper1, 
		  UPER1_BITS, upaddr->uper2, UPER2_BITS, upaddr->upwc);
	}
	waitdry = 0;
	while ((upaddr->upds&UPDS_DRY) == 0 && ++waitdry < sectsiz)
		DELAY(5);
	if (upaddr->uper1&UPER1_WLE) {
		/*
		 * Give up on write locked devices immediately.
		 */
		printf("up%d: write locked\n", unit);
		return (-1);
	}
	if (++io->i_errcnt > 27) {
		/*
		 * After 28 retries (16 without offset, and
		 * 12 with offset positioning) give up.
		 * But first, if the error is a header CRC,
		 * check if a replacement sector exists in
		 * the bad sector table.
		 */
		if ((upaddr->uper1&UPER1_HCRC) && (io->i_flgs&F_NBSF) == 0 &&
		     upecc(io, BSE) == 0)
			goto success;
		io->i_error = EHER;
		if (upaddr->upcs2 & UPCS2_WCE)
			io->i_error = EWCK;
hard:
		bn = io->i_bn +
			(io->i_cc + upaddr->upwc * sizeof (short)) / sectsiz;
		cn = bn/st->nspc;
		sn = bn%st->nspc;
		tn = sn/st->nsect;
		sn = sn%st->nsect;
		printf(
		  "up error: (cyl,trk,sec)=(%d,%d,%d) cs2=%b er1=%b er2=%b\n",
		   cn, tn, sn,
		   upaddr->upcs2, UPCS2_BITS, upaddr->uper1, 
		   UPER1_BITS, upaddr->uper2, UPER2_BITS);
		upaddr->upcs1 = UP_TRE|UP_DCLR|UP_GO;
		io->i_errblk = bn;
		if (io->i_errcnt >= 16) {
			upaddr->upof = UPOF_FMT22;
			upaddr->upcs1 = UP_RTC|UP_GO;
			while ((upaddr->upds&UPDS_DRY) == 0)
				DELAY(25);
		}
		return (io->i_cc + upaddr->upwc * sizeof(short));
	}
	if (upaddr->uper2 & UPER2_BSE) {
		if ((io->i_flgs&F_NBSF) == 0 && upecc(io, BSE) == 0)
			goto success;
		io->i_error = EBSE;
		goto hard;
	}
	/*
	 * ECC error. If a soft error, correct it;
	 * otherwise fall through and retry the transfer.
	 */
	if ((upaddr->uper1 & (UPER1_DCK|UPER1_ECH|UPER1_HCRC)) == UPER1_DCK) {
		if (upecc(io, ECC) == 0)
#ifdef F_SEVRE
		    if (io->i_flgs & F_SEVRE)
			return (-1);
		    else
#endif
			goto success;
		io->i_error = EECC;
		goto hard;
	} 
#ifdef F_SEVRE
	if (io->i_flgs & F_SEVRE)
		goto hard;
#endif
	/*
	 * Clear drive error and, every eight attempts,
	 * (starting with the fourth)
	 * recalibrate to clear the slate.
	 */
	upaddr->upcs1 = UP_TRE|UP_DCLR|UP_GO;
	if ((io->i_errcnt&07) == 4 ) {
		upaddr->upcs1 = UP_RECAL|UP_GO;
		while ((upaddr->upds&UPDS_DRY) == 0)
			DELAY(25);
		upaddr->updc = cn;
		upaddr->upcs1 = UP_SEEK|UP_GO;
		while ((upaddr->upds&UPDS_DRY) == 0)
			DELAY(25);
	}
	if (io->i_errcnt >= 16 && (func & READ)) {
		upaddr->upof = up_offset[io->i_errcnt & 017] | UPOF_FMT22;
		upaddr->upcs1 = UP_OFFSET|UP_GO;
		while ((upaddr->upds&UPDS_DRY) == 0)
			DELAY(25);
	}
	goto restart;

success:
#define	rounddown(x, y)	(((x) / (y)) * (y))
	upaddr->upwc = rounddown(upaddr->upwc, sectsiz / sizeof (short));
	if (upaddr->upwc) {
		doprintf++;
		goto restart;
	}
done:
	/*
	 * Release UNIBUS 
	 */
	ubafree(io, info);
	/*
	 * If we were offset positioning,
	 * return to centerline.
	 */
	if (io->i_errcnt >= 16) {
		upaddr->upof = UPOF_FMT22;
		upaddr->upcs1 = UP_RTC|UP_GO;
		while ((upaddr->upds&UPDS_DRY) == 0)
			DELAY(25);
	}
	return (io->i_cc);
}

/*
 * Correct an ECC error, and restart the
 * i/o to complete the transfer (if necessary). 
 * This is quite complicated because the transfer
 * may be going to an odd memory address base and/or
 * across a page boundary.
 */
upecc(io, flag)
	register struct iob *io;
	int flag;
{
	register i, unit = io->i_unit;
	register struct updevice *up = 
		(struct updevice *)ubamem(unit, ubastd[0]);
	register struct st *st;
	caddr_t addr;
	int bn, twc, npf, mask, cn, tn, sn;
	daddr_t bbn;

	/*
	 * Npf is the number of sectors transferred
	 * before the sector containing the ECC error;
	 * bn is the current block number.
	 */
	twc = up->upwc;
	npf = ((twc * sizeof(short)) + io->i_cc) / sectsiz;
	if (flag == ECC)
		npf--;
	if (updebug[unit] & UPF_ECCDEBUG)
		printf("npf=%d mask=0x%x ec1=%d wc=%d\n",
			npf, up->upec2, up->upec1, twc);
	bn = io->i_bn + npf;
	st = &upst[up_type[unit]];
	cn = bn/st->nspc;
	sn = bn%st->nspc;
	tn = sn/st->nsect;
	sn = sn%st->nsect;

	/*
	 * ECC correction.
	 */
	if (flag == ECC) {
		int bit, o, ecccnt;

		ecccnt = 0;
		mask = up->upec2;
		printf("up%d: soft ecc sn%d\n", unit, bn);
		/*
		 * Compute the byte and bit position of
		 * the error.  o is the byte offset in
		 * the transfer at which the correction
		 * applied.
		 */
		i = up->upec1 - 1;		/* -1 makes 0 origin */
		bit = i & 07;
		o = (i & ~07) >> 3;
		up->upcs1 = UP_TRE|UP_DCLR|UP_GO;
		/*
		 * Correct while possible bits remain of mask.
		 * Since mask contains 11 bits, we continue while
		 * the bit offset is > -11.  Also watch out for
		 * end of this block and the end of the transfer.
		 */
		while (o < sectsiz && (npf*sectsiz)+o < io->i_cc && bit > -11) {
			/*
			 * addr =
			 *  (base address of transfer) +
			 *  (# sectors transferred before the error) *
			 *    (sector size) +
			 *  (byte offset to incorrect data)
			 */
			addr = io->i_ma + (npf * sectsiz) + o;
			/*
			 * No data transfer occurs with a write check,
			 * so don't correct the resident copy of data.
			 */
			if ((io->i_flgs & (F_CHECK|F_HCHECK)) == 0) {
				if (updebug[unit] & UPF_ECCDEBUG)
					printf("addr=0x%x old=0x%x ", addr,
						(*addr&0xff));
				*addr ^= (mask << bit);
				if (updebug[unit] & UPF_ECCDEBUG)
					printf("new=0x%x\n", (*addr&0xff));
			}
			o++, bit -= 8;
			if ((io->i_flgs&F_ECCLM) && ++ecccnt > MAXECC)
				return (1);
		}
#ifdef F_SEVRE
		if (io->i_flgs & F_SEVRE) {
			io->i_error = EECC;
			io->i_bn = bn;
			return(1);
		}
#endif
		return (0);
	}

	/*
	 * Bad sector forwarding.
	 */
	if (flag == BSE) {
		/*
		 * If not in bad sector table,
		 * indicate a hard error to caller.
		 */
		up->upcs1 = UP_TRE|UP_DCLR|UP_GO;
		if ((bbn = isbad(&upbad[unit], cn, tn, sn)) < 0)
			return (1);
		bbn = (st->ncyl * st->nspc) - st->nsect - 1 - bbn;
		twc = up->upwc + sectsiz;
		up->upwc = - (sectsiz / sizeof (short));
		if (updebug[unit] & UPF_BSEDEBUG)
			printf("revector sn %d to %d\n", sn, bbn);
		/*
	 	 * Clear the drive & read the replacement
		 * sector.  If this is in the middle of a
		 * transfer, then set up the controller
		 * registers in a normal fashion. 
	 	 * The UNIBUS address need not be changed.
	 	 */
		while ((up->upcs1 & UP_RDY) == 0) 
			;
		if (upstart(io, bbn))
			return (1);		/* error */
		io->i_errcnt = 0;		/* success */
		do {
			DELAY(25);
		} while ((up->upcs1 & UP_RDY) == 0) ;
		if ((up->upds & UPDS_ERR) || (up->upcs1 & UP_TRE)) {
			up->upwc = twc - sectsiz;
			return (1);
		}
	}
	if (twc)
		up->upwc = twc;
	return (0);
}

upstart(io, bn)
	register struct iob *io;
	daddr_t bn;
{
	register struct updevice *upaddr = 
		(struct updevice *)ubamem(io->i_unit, ubastd[0]);
	register struct st *st = &upst[up_type[io->i_unit]];
	int sn, tn;

	sn = bn%st->nspc;
	tn = sn/st->nsect;
	sn %= st->nsect;
	upaddr->updc = bn/st->nspc;
	upaddr->upda = (tn << 8) + sn;
	switch (io->i_flgs & F_TYPEMASK) {

	case F_RDDATA:
		upaddr->upcs1 = UP_RCOM|UP_GO;
		break;

	case F_WRDATA:
		upaddr->upcs1 = UP_WCOM|UP_GO;
		break;

	case F_HDR|F_RDDATA:	
		upaddr->upcs1 = UP_RHDR|UP_GO;
		break;

	case F_HDR|F_WRDATA:
		upaddr->upcs1 = UP_WHDR|UP_GO;
		break;

	case F_CHECK|F_WRDATA:
	case F_CHECK|F_RDDATA:
		upaddr->upcs1 = UP_WCDATA|UP_GO;
		break;

	case F_HCHECK|F_WRDATA:
	case F_HCHECK|F_RDDATA:
		upaddr->upcs1 = UP_WCHDR|UP_GO;
		break;

	default:
		io->i_error = ECMD;
		io->i_flgs &= ~F_TYPEMASK;
		return (1);
	}
	return (0);
}

/*ARGSUSED*/
upioctl(io, cmd, arg)
	struct iob *io;
	int cmd;
	caddr_t arg;
{
	int unit = io->i_unit, flag;
	struct st *st = &upst[up_type[unit]], *tmp;

	switch(cmd) {

	case SAIODEBUG:
		flag = (int)arg;
		if (flag > 0)
			updebug[unit] |= flag;
		else
			updebug[unit] &= ~flag;
		return (0);

	case SAIODEVDATA:
		tmp = (struct st *)arg;
		*tmp = *st;
		return (0);
	}
	return (ECMD);
}
