/*	hp.c	4.2	83/01/17	*/

/*
 * RP??/RM?? disk driver
 * with ECC handling and bad block forwarding.
 * Also supports header io operations and
 * commands to write check header and data.
 */

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "../h/dkbad.h"

#include "../vax/pte.h"
#include "../vaxmba/hpreg.h"
#include "../vaxmba/mbareg.h"

#include "saio.h"
#include "savax.h"

#define	MASKREG(reg)	((reg)&0xffff)

#define MAXBADDESC	126
#define SECTSIZ 	512	/* sector size in bytes */
#define HDRSIZ		4	/* number of bytes in sector header */
#define MAXECC		5	/* the maximum number of bad bits accepted in
				 * an ecc error when F_ECCLM is set */

char	hp_type[MAXNMBA*8] = { 0 };

/* THIS SHOULD BE READ IN OFF THE PACK, PER DRIVE */
short	hp6_off[8] =	{ 0, 38, 0, -1, -1, -1, 118, -1 };
short	rm3_off[8] =	{ 0, 100, 0, -1, -1, -1, 310, -1 };
short	rm5_off[8] =	{ 0, 27, 0, 562, 589, 681, 562, 82 };
short	rm80_off[8] =	{ 0, 37, 0, -1, -1, -1, 115, 305 };
short	hp7_off[8] = 	{ 0, 10, 0, 330, 340, 500, 330, 50 };
short	ml_off[8] =	{ 0, -1, -1, -1, -1, -1, -1, -1 };
short	si9775_off[8] =	{ 0, 13, 0, -1, -1, -1, 40, 441 };
short	si9730_off[8] = { 0, 50, 0, -1, -1, -1, -1, 155 };
short	hpam_off[8] =	{ 0, 32, 0, 668, 723, 778, 668, 98 };
/* END SHOULD BE READ IN */

short	hptypes[] =
    { MBDT_RM03, MBDT_RM05, MBDT_RP06, MBDT_RM80, MBDT_RP05, MBDT_RP07,
      MBDT_ML11A, MBDT_ML11B, -1/*9755*/, -1/*9730*/, -1/*Capr*/, MBDT_RM02, 0};

#define RP06 (hptypes[UNITTODRIVE(unit)] <= MBDT_RP06)
#define ML11 (hptypes[UNITTODRIVE(unit)] <= MBDT_ML11A)
#define RM80 (hptypes[UNITTODRIVE(unit)] <= MBDT_RM80)

u_char	hp_offset[16] = {
    HPOF_P400, HPOF_M400, HPOF_P400, HPOF_M400,
    HPOF_P800, HPOF_M800, HPOF_P800, HPOF_M800,
    HPOF_P1200, HPOF_M1200, HPOF_P1200, HPOF_M1200,
    0, 0, 0, 0,
};

struct st hpst[] = {
	32,	5,	32*5,	823,	rm3_off,	/* RM03 */
	32,	19,	32*19,	823,	rm5_off,	/* RM05 */
	22,	19,	22*19,	815,	hp6_off,	/* RP06 */
	31,	14, 	31*14,	559,	rm80_off,	/* RM80 */
	22,	19,	22*19,	411,	hp6_off,	/* RP06 */
	50,	32,	50*32,	630,	hp7_off,	/* RP07 */
	1,	1,	1,	1,	ml_off,		/* ML11A */
	1,	1,	1,	1,	ml_off,		/* ML11B */
	32,	40,	32*40,	843,	si9775_off,	/* 9775 */
	32,	10,	32*10,	823,	si9730_off,	/* 9730 */
	32,	16,	32*16,	1024,	hpam_off,	/* AMPEX capricorn */
	1,	1,	1,	1,	0,		/* rm02 - not used */
};
struct dkbad hpbad[MAXNMBA*8];
int sectsiz;

hpopen(io)
	register struct iob *io;
{
	register unit = io->i_unit;
	struct hpdevice *hpaddr = (struct hpdevice *)mbadrv(unit);
	register struct st *st;

	mbainit(UNITTOMBA(unit));
	if (hp_type[unit] == 0) {
		register type = hpaddr->hpdt & MBDT_TYPE;
		register int i;
		struct iob tio;

		for (i = 0; hptypes[i]; i++)
			if (hptypes[i] == type)
				goto found;
		_stop("unknown drive type");
found:
		switch (i) {
		case 0: case 1:	{	/* rm03 or rm05 */
			register hpsn = hpaddr->hpsn;

			if ((hpsn & SIMB_LU) != unit)
				break;
			switch ((hpsn & SIMB_MB) &~ (SIMB_S6|SIRM03|SIRM05)) {
			case SI9775D:
				i = 8;	/* si9775 */
				break;
			case SI9730D:
				i = 9;	/* si9730 */
				break;
			case SI9766:
				i = 1;	/* rm05 */
				hpaddr->hpcs1 = HP_RECAL|HP_GO;
				DELAY(100000);
				break;
			case SI9762:
				i = 0;	/* rm03 */
				break;
			}
			break;
		}

		case 11:		/* rm02 */
			hpaddr->hpcs1 = HP_NOP;
			hpaddr->hphr = HPHR_MAXTRAK;
			if (MASKREG(hpaddr->hphr) == 15)
				i = 10;		/* ampex capricorn */
			else
				i = 0;		/* rm03 */
			break;
		
		case 6: case 7:		/* ml11a ml11b */
			i = 6;			/* ml11a */
			break;
		}
		hp_type[unit] = i;
		/*
		 * Read in the bad sector table:
		 *	copy the contents of the io structure
		 *	to tio for use during the bb pointer
		 *	read operation.
		 */
		st = &hpst[hp_type[unit]];
		tio = *io;
		tio.i_bn = st->nspc * st->ncyl - st->nsect;
		tio.i_ma = (char *)&hpbad[tio.i_unit];
		tio.i_cc = sizeof (hpbad);
		tio.i_flgs |= F_RDDATA;
		for (i = 0; i < 5; i++) {
			if (hpstrategy(&tio, READ) == sizeof (hpbad))
				break;
			tio.i_bn += 2;
		}
		if (i == 5) {
			printf("Unable to read bad sector table\n");
			for (i = 0; i < MAXBADDESC; i++) {
				hpbad[unit].bt_bad[i].bt_cyl = -1;
				hpbad[unit].bt_bad[i].bt_trksec = -1;
			}
		}	
	}
	if (io->i_boff < 0 || io->i_boff > 7 ||
	    st->off[io->i_boff]== -1)
		_stop("hp bad minor");
	io->i_boff = st->off[io->i_boff] * st->nspc;
}

hpstrategy(io, func)
	register struct iob *io;
{
	register unit = io->i_unit;
	struct mba_regs *mba = mbamba(unit);
	daddr_t bn;
	struct hpdevice *hpaddr = (struct hpdevice *)mbadrv(unit);
	struct st *st = &hpst[hp_type[unit]];
	int cn, tn, sn, bytecnt, bytesleft; 
	daddr_t startblock;
	char *membase;
	int er1, er2, hprecal;

	sectsiz = SECTSIZ;
	if ((io->i_flgs & (F_HDR|F_HCHECK)) != 0)
		sectsiz += HDRSIZ;
	if ((hpaddr->hpds & HPDS_VV) == 0) {
		hpaddr->hpcs1 = HP_DCLR|HP_GO;
		hpaddr->hpcs1 = HP_PRESET|HP_GO;
		if (hp_type[unit] != 6)		/* any but ml11 */
			hpaddr->hpof = HPOF_FMT22;
	}
	io->i_errcnt = 0;
	bytecnt = io->i_cc;
	membase = io->i_ma;
	startblock = io->i_bn;
	hprecal = 1;
readmore:
	bn = io->i_bn;
	cn = bn/st->nspc;
	sn = bn%st->nspc;
	tn = sn/st->nsect;
	sn = sn%st->nsect;

	while ((hpaddr->hpds & HPDS_DRY) == 0)
		;
	if (hp_type[unit] == 6)		/* ml11 */
		hpaddr->hpda = bn;
	else {
		hpaddr->hpdc = cn;
		hpaddr->hpda = (tn << 8) + sn;
	}
	if (mbastart(io, func) != 0)		/* start transfer */
		return (-1);

	while ((hpaddr->hpds & HPDS_DRY) == 0)
		;
	if (((hpaddr->hpds&HPDS_ERR) | (mba->mba_sr&MBSR_EBITS)) == 0 )
		return(bytecnt);

	/* ------- error handling ------- */

	if (bytesleft = MASKREG(mba->mba_bcr>>16)) 
		bytesleft |= 0xffff0000;	/* sign ext */
	bn = io->i_bn + (io->i_cc + bytesleft)/sectsiz;
	cn = bn/st->nspc;
	sn = bn%st->nspc;
	tn = sn/st->nsect;
	sn = sn%st->nsect;
	er1 = MASKREG(hpaddr->hper1);
	er2 = MASKREG(hpaddr->hper2);
#ifdef HPDEBUG
	printf("hp error: (cyl,trk,sec)=(%d,%d,%d) ds=%b \n",
		cn, tn, sn, MASKREG(hpaddr->hpds), HPDS_BITS);
	printf("er1=%b er2=%b",
		er1, HPER1_BITS,
		er2, HPER2_BITS);
	printf("\n");
#endif
	if (er1 & HPER1_HCRC) {
		er1 &= ~(HPER1_HCE|HPER1_FER);
		er2 &= ~HPER2_BSE;
	}
	if (er1&HPER1_WLE) {
		printf("hp%d: write locked\n", unit);
		return(-1);
	} else if ((er1&0xffff) == HPER1_FER && RP06) {
		goto badsect;

	} else if (++io->i_errcnt > 27 ||
		   er1 & HPER1_HARD ||
		   (!ML11 && (er2 & HPER2_HARD))) {
		io->i_error = EHER;
		if ((mba->mba_sr & (MBSR_WCKUP | MBSR_WCKLWR)) != 0)
			io->i_error = EWCK;
hard:
		io->i_errblk = bn;
		printf("hp error: (cyl,trk,sec)=(%d,%d,%d) ds=%b \n",
			   cn, tn, sn, MASKREG(hpaddr->hpds), HPDS_BITS);
		printf("er1=%b er2=%b",
			   er1, HPER1_BITS,
			   er2, HPER2_BITS);
		if (hpaddr->hpmr)
			printf(" mr=%o", hpaddr->hpmr&0xffff);
		if (hpaddr->hpmr2)
			printf(" mr2=%o", hpaddr->hpmr2&0xffff);
		printf("\n");
		return(-1);

	} else if ((er2 & HPER2_BSE) && !ML11) {
badsect:
		if ((io->i_flgs & F_NBSF) != 0) {
			io->i_error = EBSE;	
			goto hard;
		}
		if (hpecc(io, BSE) == 0)
			goto success;
		else {
			io->i_error = EBSE;
			goto hard;
		}
	} else if (RM80 && er2&HPER2_SSE) {
	/* skip sector error */
		(void) hpecc(io, SSE);
		goto success;
	} else if ((er1&(HPER1_DCK|HPER1_ECH))==HPER1_DCK) {
		if ( hpecc(io, ECC) == 0)
			goto success;
		else {
			io->i_error = EECC;
			return(1);	
		}
	} else
		io->i_active = 0;		/* force retry */

	hpaddr->hpcs1 = HP_DCLR|HP_GO;
	while ((hpaddr->hpds & HPDS_DRY) == 0)
		;
	if (ML11) {
		if (io->i_errcnt >= 16)
			goto hard;
	} else if (((io->i_errcnt&07) == 4) && (io->i_active == 0)) {
		hpaddr->hpcs1 = HP_RECAL|HP_GO;
		hprecal = 0;
		goto nextrecal;
	}
	switch (hprecal) {

	case 1:
		hpaddr->hpdc = cn;
		hpaddr->hpcs1 = HP_SEEK|HP_GO;
		goto nextrecal;
	case 2:
		if (io->i_errcnt < 16 || (func & READ) == 0)
			goto donerecal;
		hpaddr->hpof = hp_offset[io->i_errcnt & 017]|HPOF_FMT22;
		hpaddr->hpcs1 = HP_OFFSET|HP_GO;
nextrecal:
		hprecal++;
		io->i_active = 1;
		goto try_again;
	donerecal:
	case 3:
		hprecal = 0;
		io->i_active = 0;
		goto try_again;
	}
	if (io->i_active) {
		if (io->i_errcnt >= 16) {
			hpaddr->hpcs1 = HP_RTC|HP_GO;
			while (hpaddr->hpds & HPDS_PIP)
				;
		}
	}
success:		 /* continue with the next block */
	bn++;
	if ((bn-startblock) * sectsiz < bytecnt) {

try_again:		/* re-read same block */
		io->i_bn = bn;
		mba->mba_sr = -1;
		io->i_ma = membase + (io->i_bn - startblock)*sectsiz;
		io->i_cc = bytecnt - (io->i_bn - startblock)*sectsiz;
#ifdef HPDEBUG
		printf("restart: bl %d, byte %d, mem 0x%x %d\n",
			io->i_bn, io->i_cc, io->i_ma, io->i_ma);
#endif
		goto readmore;
	}
	return (bytecnt);
}
hpecc(io, flag)
	register struct iob *io;
	int flag;
{
	register unit = io->i_unit;
	register struct mba_regs *mbp = mbamba(unit);
	register struct hpdevice *rp = (struct hpdevice *)mbadrv(unit);
	register struct st *st = &hpst[hp_type[unit]];
	int npf;
	int bn, cn, tn, sn;
	int bcr, tad;

	if (bcr = MASKREG(mbp->mba_bcr>>16))
		bcr |= 0xffff0000;		/* sxt */
	npf = (bcr + io->i_cc)/sectsiz;		/* number of sectors read */
	switch (flag) {
	case ECC:
		{
		register int i;
		caddr_t addr;
		int bit, byte, mask, ecccnt = 0;

		printf("hp%d: soft ecc sn%d\n", unit, io->i_bn + npf);
		mask = MASKREG(rp->hpec2);
		i = MASKREG(rp->hpec1) - 1;		/* -1 makes 0 origin */
		bit = i&07;
		i = (i&~07)>>3;
		byte = i;
		rp->hpcs1 = HP_DCLR | HP_GO;
		while (i <sectsiz && npf*sectsiz + i < io->i_cc && bit > -11) {
			addr = io->i_ma + (npf*sectsiz) + byte;
#ifdef HPECCDEBUG
			printf("addr %x old:%x ",addr, (*addr&0xff));
#endif
			if ((io->i_flgs & (F_CHECK|F_HCHECK)) == 0)
				*addr ^= (mask << bit);	/* don't 'correct' mem-
							 * ory during Wcheck */
#ifdef HPECCDEBUG
			printf("new:%x\n",(*addr&0xff));
#endif
			byte++;
			i++;
			bit -= 8;
			if ((ecccnt++>=MAXECC) && ((io->i_flgs&F_ECCLM) != 0))
				return(1);
		}
		return(0);
		}

	case SSE:		/* skip sector error */
				/* -----this section must be fixed------*/
		rp->hpcs1 = HP_DCLR | HP_GO;
		rp->hpof |= HPOF_SSEI;
		mbp->mba_bcr = -(io->i_cc - npf*sectsiz);
		/* presumably the disk address has already
		 * been incremented to point to the next sector	*/
		return(0);	

#ifndef NOBADSECT
	case BSE:
#ifdef HPDEBUG
		printf("hpecc: BSE @ bn %d\n", bn);
#endif
		rp->hpcs1 = HP_DCLR | HP_GO;
		bcr += SECTSIZ;
		tad = rp->hpda;
		if ((bn = isbad(&hpbad[unit], bcr, tad>>8, tad&0x7f)) < 0)
			return(1);
		bn = st->ncyl*st->nspc - st->nsect - 1 - bn;
		cn = bn/st->nspc;
		sn = bn%st->nspc;
		tn = sn/st->nsect;
		sn %= st->nsect;
		io->i_cc = -SECTSIZ;
		io->i_ma = (char *)((io->i_bn + npf -1)*SECTSIZ);
#ifdef HPDEBUG
		printf("revector to cn %d tn %d sn %d\n", cn, tn, sn);
#endif
		rp->hpdc = cn;
		rp->hpda = (tn<<8) + sn;
		mbp->mba_sr = -1;
		mbastart(io,io->i_flgs);
		io->i_errcnt = 0;	/* error has been corrected */
		while(rp->hpds & HPDS_DRY == 0)
			;
		if (rp->hpds&HPDS_ERR)
			return(1);
		else
			return(0);
	}
}
/*ARGSUSED*/
hpioctl(io, cmd, arg)
	struct iob *io;
	int cmd;
	caddr_t arg;
{

	struct st *st = &hpst[hp_type[io->i_unit]], *tmp;
	struct mba_drv *drv = mbadrv(io->i_unit);

	switch(cmd) {

	case SAIODEVDATA:
		if ((drv->mbd_dt&MBDT_TAP) == 0) {
			tmp = (struct st *)arg;
			*tmp = *st;
			return(0);
		}
		else 
			return(ECMD);

	default:
		return (ECMD);
	}
}

/* this routine is common to up & hp, move to a separate file */

/*
 * Search the bad sector table looking for
 * the specified sector.  Return index if found.
 * Return -1 if not found.
 */

isbad(bt, st, blno)
	register struct dkbad *bt;
	register struct st *st;
{
	register int i;
	register long blk, bblk;
	int trk, sec;

	sec = blno % st->nspc;
	trk = sec / st->nsect;
	sec %= st->nsect;
	blk = ((long)(blno/st->nspc) << 16) + (trk << 8) + sec;
	for (i = 0; i < MAXBADDESC; i++) {
		bblk = ((long)bt->bt_bad[i].bt_cyl << 16) +
			bt->bt_bad[i].bt_trksec;
		if (blk == bblk)
			return (i);
		if (blk < bblk || bblk < 0)
			break;
	}
	return (-1);
}
