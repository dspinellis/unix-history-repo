/*	up.c	4.6	83/02/16	*/

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
#define MAXECC		5	/* max number of bad bits accepted in
				 * a soft ecc error when F_ECCLM is set */
#define	NUPTYPES	3

u_short	ubastd[] = { 0776700 };

char	up_gottype[MAXNUBA*8] = { 0 };
char	up_type[MAXNUBA*8] = { 0 };
short	up_off[] = { 0, 27, 68, -1, -1, -1, -1, 82 };
short	fj_off[] = { 0, 50, 0, -1, -1, -1, -1, 155 };
/* this is called upam instead of am because hp.c has a similar array */
short	upam_off[] = { 0, 32, 0, 668, 723, 778, 668, 98 };

struct st upst[NUPTYPES] = {
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
int 	sectsiz;			/* real sector size */

upopen(io)
	register struct iob *io;
{
	register unit = io->i_unit;
	register struct updevice *upaddr;
	register struct st *st = &upst[up_type[unit]];

	if (io->i_boff < 0 || io->i_boff > 7 || st->off[io->i_boff] == -1)
		_stop("up bad unit");
	upaddr = (struct updevice *)ubamem(unit, ubastd[0]);
	while ((upaddr->upcs1 & UP_DVA) == 0)
		;
	if (up_gottype[unit] == 0) {
		register int i;
		struct iob tio;

		upaddr->uphr = UPHR_MAXTRAK;
		for (st = upst; st < &upst[NUPTYPES]; st++)
			if (upaddr->uphr == st->ntrak - 1) {
				up_type[unit] = st - upst;
				break;
			}
		if (st == &upst[NUPTYPES]) {
			printf("up%d: uphr=%x\n", unit, upaddr->uphr);
			_stop("unknown drive type");
		}
		upaddr->upcs2 = UPCS2_CLR;
#ifdef DEBUG
		printf("Unittype=%d\n",up_type[unit]);
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
	io->i_boff = st->off[io->i_boff] * st->nspc;
	io->i_flgs &= ~F_TYPEMASK;
}

upstrategy(io, func)
	register struct iob *io;
{
	int cn, tn, sn;
	register unit = io->i_unit;
	daddr_t bn;
	int recal, info, waitdry;
	register struct updevice *upaddr =
	    (struct updevice *)ubamem(unit, ubastd[0]);
	register struct st *st = &upst[up_type[unit]];

	sectsiz = SECTSIZ;
	if (io->i_flgs & (F_HDR|F_HCHECK))
		sectsiz += HDRSIZ;
	upaddr->upcs2 = unit;
	if ((upaddr->upds & UPDS_VV) == 0) {
		upaddr->upcs1 = UP_DCLR|UP_GO;
		upaddr->upcs1 = UP_PRESET|UP_GO;
		upaddr->upof = UPOF_FMT22;
	}
	if ((upaddr->upds & UPDS_DREADY) == 0)
		_stop("up not ready");
	info = ubasetup(io, 1);
	upaddr->upwc = -io->i_cc / sizeof (short);
	upaddr->upba = info;
	recal = 0;
	io->i_errcnt = 0;

restart: 
	bn = io->i_bn + (io->i_cc + upaddr->upwc * sizeof(short)) / sectsiz;
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
	if ((upaddr->upds&UPDS_ERR) == 0 && (upaddr->upcs1&UP_TRE) == 0) {
		ubafree(io, info);
		return (io->i_cc);
	}
#ifdef LOGALLERRS
	printf("uper: (c,t,s)=(%d,%d,%d) cs2=%b er1=%b er2=%b wc=%x\n",
		upaddr->updc, upaddr->upda>>8, (upaddr->upda&0x1f-1),
	    	upaddr->upcs2, UPCS2_BITS, upaddr->uper1, 
		UPER1_BITS, upaddr->uper2, UPER2_BITS,-upaddr->upwc);
#endif
	waitdry = 0;
	while ((upaddr->upds & UPDS_DRY) == 0 && ++waitdry < sectsiz)
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
		 */
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
		return (io->i_cc + upaddr->upwc * sizeof(short));
	}
	if (upaddr->uper2 & UPER2_BSE) {
		short wc = upaddr->upwc;
		if ((io->i_flgs&F_NBSF) == 0 && upecc(io, BSE) == 0) {
			if (wc != upaddr->upwc)
				printf("wc %x upwc %x\n", wc, upaddr->upwc);
			goto success;
		}
		io->i_error = EBSE;
		goto hard;
	}
	/*
	 * Retriable error.
	 * If a soft ecc, correct it 
	 * Otherwise fall through and retry the transfer
	 */
	if (upaddr->uper1 & UPER1_DCK) {
		/*
		 * If a write check command is active, all
		 * ecc errors give UPER1_ECH.
		 */
		if ((upaddr->uper1 & UPER1_ECH) == 0 || 
		    (upaddr->upcs2 & UPCS2_WCE)) {
			if (upecc(io, ECC) == 0)
				goto success;
			io->i_error = EECC;
			goto hard;
		}
	} 
	/*
	 * Clear drive error and, every eight attempts,
	 * (starting with the fourth)
	 * recalibrate to clear the slate.
	 */
	upaddr->upcs1 = UP_TRE|UP_DCLR|UP_GO;
	if ((io->i_errcnt&07) == 4 ) {
		upaddr->upcs1 = UP_RECAL|UP_GO;
		recal = 1;
		goto restart;
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
		recal = 2;
		goto restart;

	case 2:
		if (io->i_errcnt < 16 || (func & READ) == 0)
			goto donerecal;
		upaddr->upof = up_offset[io->i_errcnt & 017] | UPOF_FMT22;
		upaddr->upcs1 = UP_OFFSET|UP_GO;
		recal = 3;
		goto restart;

	donerecal:
	case 3:
		recal = 0;
		break;
	}
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
	goto restart;

success:
	if (upaddr->upwc != 0)
		goto restart;
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
	register struct st *st;
	register int i;
	caddr_t addr;
	int bn, twc, npf, mask, cn, tn, sn;
	daddr_t bbn;

	/*
	 * Npf is the number of sectors transferred before the sector
	 * containing the ECC error, bn is the current block number
	 */
	twc = up->upwc;
	npf = ((twc * sizeof(short)) + io->i_cc)/sectsiz;
#ifdef UPECCDEBUG
	printf("npf %d mask 0x%x pos %d wc 0x%x\n",npf,mask,up->upec1,-up->upwc);
#endif
	bn = io->i_bn + npf ;
	st = &upst[up_type[io->i_unit]];
	cn = bn/st->nspc;
	sn = bn%st->nspc;
	tn = sn/st->nsect;
	sn = sn%st->nsect;
	/*
	 * action taken depends on the flag
	 */
	if (flag == ECC) {
		int bit, byte, ecccnt;

		ecccnt = 0;
		mask = up->upec2;
		printf("up%d: soft ecc sn%d\n", io->i_unit, bn);
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
		while (i < sectsiz && (npf*sectsiz)+i < io->i_cc && bit > -11) {
			/*
			 * addr = vax base addr + (number of sectors transferred
			 *	  before the error sector times the sector size)
			 *	  + byte number
			 */
			addr = io->i_ma + (npf * sectsiz) + byte;
#ifdef UPECCDEBUG
			printf("addr %x old: %x ",addr, (*addr&0xff));
#endif
			if ((io->i_flgs & (F_CHECK|F_HCHECK)) == 0)
				*addr ^= (mask << bit);
#ifdef UPECCDEBUG
			printf("new: %x\n", (*addr&0xff));
#endif
			byte++;
			i++;
			bit -= 8;
			if ((io->i_flgs&F_ECCLM) && ++ecccnt > MAXECC)
				return (1);
		}
		return (0);
	}
	if (flag == BSE) {
		/*
		 * if not in bad sector table, return 1 (= hard error)
		 */
		up->upcs1 = UP_TRE|UP_DCLR|UP_GO;
		if ((bbn = isbad(&upbad[io->i_unit], cn, tn, sn)) < 0)
			return (1);
		bbn = st->ncyl * st->nspc -st->nsect - 1 - bbn;
		twc = up->upwc + sectsiz;
		up->upwc = - (sectsiz / sizeof (short));
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
		if (upstart(io, bbn) != 0)
			return (1);		/* error */
		io->i_errcnt = 0;		/* success */
		do {
			DELAY(25);
		} while ( up->upcs1 & UP_RDY == 0) ;
		if (up->upds & UPDS_ERR || up->upcs1 & UP_TRE) {
			up->upwc = twc -sectsiz;
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
	struct st *st = &upst[up_type[io->i_unit]], *tmp;

	switch(cmd) {

	case SAIODEVDATA:
		tmp = (struct st *)arg;
		*tmp = *st;
		return (0);
	}
	return (ECMD);
}
