/*	up.c	4.2	82/12/30	*/

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

u_short	ubastd[] = { 0776700 };

char	up_gottype[MAXNUBA*8] = { 0 };
char	up_type[MAXNUBA*8] = { 0 };
short	up_off[] = { 0, 27, 68, -1, -1, -1, -1, 82 };
short	fj_off[] = { 0, 50, 0, -1, -1, -1, -1, 155 };
/* this is called upam instead of am because hp.c has a similar array */
short	upam_off[] = { 0, 32, 0, 668, 723, 778, 668, 98 };

#define	NUPTYPES	3

struct upst {
	short nsect;
	short ntrak;
	short nspc;
	short ncyl;
	short *off;
} upst[NUPTYPES] = {
	32,	19,	32*19,	823,	up_off,		/* 9300/equiv */
	32,	10,	32*10,	823,	fj_off,		/* Fuji 160 */
	32,	16,	32*16,	1024,	upam_off,	/* Capricorn */
};

u_char	up_offset[16] = {
	UPOF_P400, UPOF_M400, UPOF_P400, UPOF_M400,
	UPOF_P800, UPOF_M800, UPOF_P800, UPOF_M800, 
	UPOF_P1200, UPOF_M1200, UPOF_P1200, UPOF_M1200,
	0, 0, 0, 0
};

struct  dkbad upbad[MAXNUBA*8];		/* bad sector table */

upopen(io)
	register struct iob *io;
{
	register struct updevice *upaddr;
	register struct upst *st;

	if (io->i_boff < 0 || io->i_boff > 7 || st->off[io->i_boff] == -1)
		_stop("up bad unit");
	upaddr = (struct updevice *)ubamem(io->i_unit, ubastd[0]);
	while ((upaddr->upcs1 & UP_DVA) == 0) /* infinite wait */
		;
	st = &upst[up_type[io->i_unit]];
	if (up_gottype[io->i_unit] == 0) {
		register int i;
		struct iob tio;

		upaddr->uphr = UPHR_MAXTRAK;
		for (st = upst; st < &upst[NUPTYPES]; st++)
			if (upaddr->uphr == st->ntrak - 1) {
				up_type[io->i_unit] = st - upst;
				break;
			}
		if (st == &upst[NUPTYPES]) {
			printf("up%d: uphr=%x\n", upaddr->uphr);
			_stop("unknown drive type");
		}
		upaddr->upcs2 = UPCS2_CLR;
#ifdef DEBUG
		printf("Unittype=%d\n",up_type[io->i_unit]);
#endif

		/*
		 * Read in the bad sector table:
		 *	copy the contents of the io structure
		 *	to tio for use during the bb pointer
		 *	read operation.
		 */
		tio = *io;
		tio.i_bn = st->nspc * st->ncyl - st->nsect;
		tio.i_ma = (char *)&upbad[tio.i_unit];
		tio.i_cc = sizeof (upbad);
		tio.i_flgs |= F_RDDATA;
		for (i = 0; i < 5; i++) {
			if (upstrategy(&tio, READ) == sizeof (upbad))
				break;
			tio.i_bn += 2;
		}
		if (i == 5) {
			printf("Unable to read bad sector table\n");
			for (i = 0; i < 126; i++) {
				upbad[io->i_unit].bt_bad[i].bt_cyl = -1;
				upbad[io->i_unit].bt_bad[i].bt_trksec = -1;
			}
		}	
		up_gottype[io->i_unit] = 1;
	}
	io->i_boff = st->off[io->i_boff] * st->nspc;
	io->i_flgs &= ~F_TYPEMASK;
}

upstrategy(io, func)
	register struct iob *io;
{
	int unit, cn, tn, sn;
	daddr_t bn;
	int recal, info, waitdry;
	register struct updevice *upaddr =
	    (struct updevice *)ubamem(io->i_unit, ubastd[0]);
	register struct upst *st = &upst[up_type[io->i_unit]];

	if (func != READ && func != WRITE) {
		io->i_error = ECMD;
		return (-1);
	}
	unit = io->i_unit;
	io->i_errcnt = 0;
	recal = 3;
	upaddr->upcs2 = unit;
	if ((upaddr->upds & UPDS_VV) == 0) {
		upaddr->upcs1 = UP_DCLR|UP_GO;
		upaddr->upcs1 = UP_PRESET|UP_GO;
		upaddr->upof = UPOF_FMT22;
	}
	if ((upaddr->upds & UPDS_DREADY) != UPDS_DREADY)
		_stop("up not ready");
	info = ubasetup(io, 1);
	upaddr->upwc = -io->i_cc / sizeof (short);
	upaddr->upba = info;
readmore: 
	bn = io->i_bn + btop(io->i_cc + upaddr->upwc*sizeof(short));
	while((upaddr->upds & UPDS_DRY) == 0)
		;
	if (upstart(io, bn) != 0) 
		return (-1);
	do {
		DELAY(25);
	} while ((upaddr->upcs1 & UP_RDY) == 0);

	if (((upaddr->upds&UPDS_ERR) | (upaddr->upcs1&UP_TRE)) == 0 )
		return(io->i_cc);

#ifdef LOGALLERRS
	printf("uper: (c,t,s)=(%d,%d,%d) cs2=%b er1=%b er2=%b wc=%x\n",
			upaddr->updc, upaddr->upda>>8, (upaddr->upda&0x1f-1),
		    	upaddr->upcs2, UPCS2_BITS, upaddr->uper1, 
			UPER1_BITS, upaddr->uper2, UPER2_BITS,-upaddr->upwc);
#endif
	waitdry = 0;
	while ((upaddr->upds & UPDS_DRY) == 0 && ++waitdry < 512)
		DELAY(5);
	if (++io->i_errcnt > 27) {
		/*
		 * After 28 retries (16 without offset, and
		 * 12 with offset positioning) give up.
		 */
		io->i_error = EHER;
hard:
		if (upaddr->upcs2 & UPCS2_WCE)
			io->i_error = EWCK;
		bn = io->i_bn + btop(io->i_cc + upaddr->upwc*sizeof(short));
		cn = bn/st->nspc;
		sn = bn%st->nspc;
		tn = sn/st->nsect;
		sn = sn%st->nsect;
		printf("up error: (cyl,trk,sec)=(%d,%d,%d) cs2=%b er1=%b er2=%b\n",
		    	cn, tn, sn,
		    	upaddr->upcs2, UPCS2_BITS, upaddr->uper1, 
			UPER1_BITS, upaddr->uper2, UPER2_BITS);
		upaddr->upcs1 = UP_TRE|UP_DCLR|UP_GO;
		return (io->i_cc + upaddr->upwc*sizeof(short));
	} else 
		if (upaddr->uper1&UPER1_WLE) {
			/*
			 * Give up on write locked devices
			 * immediately.
			 */
			printf("up%d: write locked\n", unit);
			return(-1);
		}
#ifndef NOBADSECT
	else if (upaddr->uper2 & UPER2_BSE) {
		if (upecc( io, BSE)) 
			goto success;
		else {
			io->i_error = EBSE;
			goto hard;
		}
	}
#endif
	else {
		/*
		 * Retriable error.
		 * If a soft ecc, correct it 
		 * Otherwise fall through and retry the transfer
		 */
		if ((upaddr->uper1&(UPER1_DCK|UPER1_ECH))==UPER1_DCK) {
			upecc(io, ECC);
			goto success;
		} else
			io->i_active = 0; /* force retry */
	}
	/*
	 * Clear drive error and, every eight attempts,
	 * (starting with the fourth)
	 * recalibrate to clear the slate.
	 */
	upaddr->upcs1 = UP_TRE|UP_DCLR|UP_GO;
	if ((io->i_errcnt&07) == 4 && io->i_active == 0) {
		upaddr->upcs1 = UP_RECAL|UP_GO;
		recal = 0;
		goto nextrecal;
	}
	/*
	 * Advance recalibration finite state machine
	 * if recalibrate in progress, through
	 *	RECAL
	 *	SEEK
	 *	OFFSET (optional)
	 *	RETRY
	 */
	switch (recal) {

	case 1:
		upaddr->updc = cn;
		upaddr->upcs1 = UP_SEEK|UP_GO;
		goto nextrecal;

	case 2:
		if (io->i_errcnt < 16 || (func & READ) == 0)
			goto donerecal;
		upaddr->upof = up_offset[io->i_errcnt & 017] | UPOF_FMT22;
		upaddr->upcs1 = UP_OFFSET|UP_GO;
	nextrecal:
		recal++;
		io->i_active = 1;
		goto readmore;

	donerecal:
	case 3:
		recal = 0;
		io->i_active = 0;
		break;
	}
	/*
	 * If still ``active'', then don't need any more retries.
	 */
	if (io->i_active) {
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
		goto readmore;
	}
success:
	io->i_active = 1;
	if (upaddr->upwc != 0)
		goto readmore;
	/*
	 * Release unibus 
	 */
	ubafree(io, info);
	return (io->i_cc);
}

/*
 * Correct an ECC error, and restart the i/o to complete
 * the transfer if necessary.  This is quite complicated because
 * the transfer may be going to an odd memory address base and/or
 * across a page boundary.
 */
upecc(io, flag)
	register struct iob *io;
	int flag;
{
	register struct updevice *up = 
		(struct updevice *)ubamem(io->i_unit, ubastd[0]);
	register struct upst *st;
	register int i;
	caddr_t addr;
	int bit, byte, npf, mask;
	int bn, twc, bbn;

	/*
	 * Npf is the number of sectors transferred before the sector
	 * containing the ECC error, bn is the current block number
	 */
	npf = btop((up->upwc * sizeof(short)) + io->i_cc);
	mask = up->upec2;
#ifdef UPECCDEBUG
	printf("npf %d mask 0x%x pos %d wc 0x%x\n",npf,mask,up->upec1,-up->upwc);
#endif
	bn = io->i_bn + npf + 1 ;
	st = &upst[up_type[io->i_unit]];
	twc = up->upwc;
	io->i_active = 2;
	/*
	 * action taken depends on the flag
	 */
	if (flag == ECC) {
		mask = up->upec2;
		printf("up%d: soft ecc sn%d\n", io->i_unit, io->i_bn + npf +1);
		/*
		 * Compute the
		 * byte and bit position of the error.  The variable i
		 * is the byte offset in the transfer.
		 */
		i = up->upec1 - 1;		/* -1 makes 0 origin */
		bit = i&07;
		i = (i&~07)>>3;
		byte = i;
		up->upcs1 = UP_TRE|UP_DCLR|UP_GO;
		/*
		 * Correct while possible bits remain of mask.  Since mask
		 * contains 11 bits, we continue while the bit offset is > -11.
		 * Also watch out for end of this block and the end of the whole
		 * transfer.
		 */
		while (i < 512 && (int)ptob(npf)+i < io->i_cc && bit > -11) {
			/*
			 * addr = vax base addr + (number of sectors transferred
			 *	  before the error sector times the sector size)
			 *	  + byte number
			 */
			addr = io->i_ma + (npf*512) + byte;
#ifdef UPECCDEBUG
			printf("addr %x old: %x ",addr, *addr);
#endif
			*addr ^= (mask << bit);
#ifdef UPECCDEBUG
			printf("new: %x\n", *addr);
#endif
			byte++;
			i++;
			bit -= 8;
		}
#ifndef NOBADSECT
	} else if (flag == BSE) {
		/*
		 * if not in bad sector table, return 0
		 */
		if ((bbn = isbad(&upbad[io->i_unit], st, bn)) < 0)
			return(0);
		up->upcs1 = UP_TRE|UP_DCLR|UP_GO;
		bbn = st->ncyl * st->nspc -st->nsect - 1 - bbn;
		twc = up->upwc + 512;
		up->upwc = -(512 / sizeof (short));
#ifdef UPECCDEBUG
		printf("revector to block %d\n", bbn);
#endif
		/*
	 	* Clear the drive & read the replacement sector.
	 	* If this is in the middle of a transfer, then set up the
	 	* controller registers in a normal fashion. 
	 	* The ub-address need not be changed.
	 	*/
		while (up->upcs1 & UP_RDY == 0) 
			;
		up->upcs1 = UP_TRE|UP_DCLR|UP_GO;
		if (upstart(io, bbn) != 0)
			return (0);
		io->i_errcnt = 0;
		do {
			DELAY(25);
		} while ( up->upcs1 & UP_RDY == 0) ;
		if (up->upds & UPDS_ERR || up->upcs1 & UP_TRE) {
			up->upwc = twc -512;
			return (0);
		}
	}
	if (twc != 0)
		up->upwc = twc;
	return (1);
}

#ifndef NOBADSECT
/*
 * Search the bad sector table looking for
 * the specified sector.  Return index if found.
 * Return -1 if not found.
 */
isbad(bt, st, blno)
	register struct dkbad *bt;
	register struct upst *st;
{
	register int i;
	register long blk, bblk;
	int trk, sec;

	sec = blno % st->nspc;
	trk = sec / st->nsect;
	sec %= st->nsect;
	blk = ((long)(blno/st->nspc) << 16) + (trk << 8) + sec;
	for (i = 0; i < 126; i++) {
		bblk = ((long)bt->bt_bad[i].bt_cyl << 16) +
			bt->bt_bad[i].bt_trksec;
		if (blk == bblk)
			return (i);
		if (blk < bblk || bblk < 0)
			break;
	}
	return (-1);
}
#endif

upstart(io, bn)
	register struct iob *io;
	daddr_t bn;
{
	register struct updevice *upaddr = 
		(struct updevice *)ubamem(io->i_unit, ubastd[0]);
	register struct upst *st = &upst[up_type[io->i_unit]];
	int sn, tn;

	sn = bn%st->nspc;
	tn = sn/st->nsect;
	sn %= st->nsect;
	upaddr->updc = bn/st->nspc;
	upaddr->upda = (tn << 8) + sn;
	switch (io->i_flgs&F_TYPEMASK) {

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

	return (ECMD);
}
