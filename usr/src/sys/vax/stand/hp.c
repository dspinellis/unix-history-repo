/*	hp.c	4.14	83/02/17	*/

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
short	hpfj_off[8] =	{ 0, 19, 0, -1, -1, -1, 398, 59 };
/* END SHOULD BE READ IN */

short	hptypes[] = {
	MBDT_RM03,	MBDT_RM05,	MBDT_RP06,	MBDT_RM80,
	MBDT_RP05,	MBDT_RP07,	MBDT_ML11A,	MBDT_ML11B,
	-1 /*9755*/,	-1 /*9730*/,	-1 /*Cap*/,	-1 /* Eagle */,
	-1 /* Eagle */,	MBDT_RM02,	0
};

#define RP06 (hptypes[hp_type[unit]] <= MBDT_RP06)
#define ML11 (hptypes[hp_type[unit]] == MBDT_ML11A)
#define RM80 (hptypes[hp_type[unit]] == MBDT_RM80)

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
	32,	16,	32*16,	1024,	hpam_off,	/* capricorn */
	43,	20,	43*20,	842,	hpfj_off,	/* Eagle */
	48,	20,	48*20,	842,	hpfj_off,	/* modif. eagle */
	1,	1,	1,	1,	0,		/* rm02 - not used */
};

struct dkbad hpbad[MAXNMBA*8];
int sectsiz;

/*
 * When awaiting command completion, don't
 * hang on to the status register since
 * this ties up the controller.
 */
#define	HPWAIT(addr)	while ((((addr)->hpds)&HPDS_DRY)==0) DELAY(500);

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
		hpaddr->hpcs1 = HP_DCLR|HP_GO;		/* init drive */
		hpaddr->hpcs1 = HP_PRESET|HP_GO;
		if (!ML11)
			hpaddr->hpof = HPOF_FMT22;
		hp_type[unit] = hpmaptype(hpaddr, i, unit);
		/*
		 * Read in the bad sector table:
		 *	copy the contents of the io structure
		 *	to tio for use during the bb pointer
		 *	read operation.
		 */
		st = &hpst[i];
		tio = *io;
		tio.i_bn = st->nspc * st->ncyl - st->nsect;
		tio.i_ma = (char *)&hpbad[unit];
		tio.i_cc = sizeof (struct dkbad);
		tio.i_flgs |= F_RDDATA;
		for (i = 0; i < 5; i++) {
			if (hpstrategy(&tio, READ) == sizeof (struct dkbad))
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

hpmaptype(hpaddr, type, unit)
	register struct hpdevice *hpaddr;
	unsigned type;
	int unit;
{
	int ntracks, hpsn;

	/*
	 * Handle SI model byte stuff when
	 * we think it's an RM03 or RM05.
	 */
	if (type == 0 || type == 1) {
		hpsn = hpaddr->hpsn;
		if ((hpsn & SIMB_LU) != unit)
			return (type);
		switch ((hpsn & SIMB_MB) &~ (SIMB_S6|SIRM03|SIRM05)) {
		case SI9775D:
			return (8);
		case SI9730D:
			return (9);
		case SI9766:
			hpaddr->hpcs1 = HP_RECAL|HP_GO;
			DELAY(100000);
			return (1);

		case SI9762:
			return (0);
		}
		return (type);
	}
	/*
	 * RM03: EMULEX controller.  Map to correct
	 * drive type by checking the holding
	 * register for the disk geometry.
	 */
	if (type == 13) {
		hpaddr->hpcs1 = HP_NOP;
		hpaddr->hphr = HPHR_MAXTRAK;
		ntracks = MASKREG(hpaddr->hphr) + 1;
		if (ntracks == 16)
			return (10);	/* AMPEX capricorn */
		hpaddr->hphr = HPHR_MAXSECT;
		ntracks = MASKREG(hpaddr->hphr) + 1;
		if (ntracks == 48)
			return (12);	/* 48 sector Eagle */
		if (ntracks == 43)
			return (11);	/* 43 sector Eagle */
		printf("RM02 with %d sectors/track?\n", ntracks);
		return (type);
	}
	/*
	 * ML11's all map to the same type.
	 */
	if (type == 6 || type == 7)
		return (6);
	return (type);
}

int	ssect;		/* set to 1 if we are on a track with skip sectors */

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
		if (!ML11)
			hpaddr->hpof = HPOF_FMT22;
	}
	io->i_errcnt = 0;
	ssect = 0;
	bytecnt = io->i_cc;
	membase = io->i_ma;
	startblock = io->i_bn;
	hprecal = 0;
restart:
	bn = io->i_bn;
	cn = bn/st->nspc;
	sn = bn%st->nspc;
	tn = sn/st->nsect;
	sn = sn%st->nsect + ssect;

	HPWAIT(hpaddr);
	mba->mba_sr = -1;
	if (ML11)
		hpaddr->hpda = bn;
	else {
		hpaddr->hpdc = cn;
		hpaddr->hpda = (tn << 8) + sn;
	}
	if (mbastart(io, func) != 0)		/* start transfer */
		return (-1);
	HPWAIT(hpaddr);
	if ((hpaddr->hpds&HPDS_ERR) == 0 && (mba->mba_sr&MBSR_EBITS) == 0)
		return (bytecnt);

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
	printf("er1=%b er2=%b", er1, HPER1_BITS, er2, HPER2_BITS);
	printf("\nbytes left: %d, of 0x%x, da 0x%x",-bytesleft,
	hpaddr->hpof, hpaddr->hpda);
	printf("\n");
#endif
	if (er1 & HPER1_HCRC) {
		er1 &= ~(HPER1_HCE|HPER1_FER);
		er2 &= ~HPER2_BSE;
	}
	if (er1&HPER1_WLE) {
		printf("hp%d: write locked\n", unit);
		return (-1);
	}
	if (MASKREG(er1) == HPER1_FER && RP06)
		goto badsect;
	if (++io->i_errcnt > 27 || (er1 & HPER1_HARD) ||
	    (!ML11 && (er2 & HPER2_HARD))) {
hard0:
		io->i_error = EHER;
		if (mba->mba_sr & (MBSR_WCKUP|MBSR_WCKLWR))
			io->i_error = EWCK;
hard:
		io->i_errblk = bn + ssect;
		printf("hp error: (cyl,trk,sec)=(%d,%d,%d) ds=%b \n",
			   cn, tn, sn, MASKREG(hpaddr->hpds), HPDS_BITS);
		printf("er1=%b er2=%b", er1, HPER1_BITS, er2, HPER2_BITS);
		if (hpaddr->hpmr)
			printf(" mr1=%o", MASKREG(hpaddr->hpmr));
		if (hpaddr->hpmr2)
			printf(" mr2=%o", MASKREG(hpaddr->hpmr2));
#ifdef HPDEBUG
		printf("dc: %d, da: 0x%x",MASKREG(hpaddr->hpdc),
		  MASKREG(hpaddr->hpda));
#endif
		hpaddr->hpcs1 = HP_DCLR|HP_GO;
		printf("\n");
		return (-1);

	}
	if ((er2 & HPER2_BSE) && !ML11) {
badsect:
		if (!ssect && (er2&HPER2_SSE))
			goto skipsect;
		if (io->i_flgs & F_NBSF) {
			io->i_error = EBSE;	
			goto hard;
		}
		if (hpecc(io, BSE) == 0)
			goto success;
		io->i_error = EBSE;
		goto hard;
	}
	if (RM80 && er2&HPER2_SSE) {
skipsect:
		(void) hpecc(io, SSE);
		ssect = 1;
		goto success;
	}
	if ((er1 & (HPER1_DCK|HPER1_ECH)) == HPER1_DCK) {
		if (hpecc(io, ECC) == 0)
			goto success;
		io->i_error = EECC;
		return (-1);	
	} 
	if (ML11 && (io->i_errcnt >= 16))
		goto hard0;
	/* fall thru to retry */
	hpaddr->hpcs1 = HP_DCLR|HP_GO;
	HPWAIT(hpaddr);
	if (((io->i_errcnt&07) == 4) ) {
		hpaddr->hpcs1 = HP_RECAL|HP_GO;
		hprecal = 1;
		goto again;
	}
	switch (hprecal) {

	case 1:
		hpaddr->hpdc = cn;
		hpaddr->hpcs1 = HP_SEEK|HP_GO;
		hprecal = 2;
		goto again;

	case 2:
		if (io->i_errcnt < 16 || (io->i_flgs & F_READ) == 0)
			goto donerecal;
		hpaddr->hpof = hp_offset[io->i_errcnt & 017]|HPOF_FMT22;
		hpaddr->hpcs1 = HP_OFFSET|HP_GO;
		hprecal = 3;
		goto again;

	case 3:
	donerecal:
		hprecal = 0;
		goto again;
	}
	if (io->i_errcnt >= 16) {
		hpaddr->hpcs1 = HP_RTC|HP_GO;
		while (hpaddr->hpds & HPDS_PIP)
			;
	}
	goto again;

success:		 		/* continue with the next block */
	bn++;
	if ((bn-startblock) * sectsiz < bytecnt) {
again:					/* re-read same block */
		io->i_bn = bn;
		io->i_ma = membase + (io->i_bn - startblock)*sectsiz;
		io->i_cc = bytecnt - (io->i_bn - startblock)*sectsiz;
#ifdef HPDEBUG
		printf("restart: bl %d, byte %d, mem 0x%x hprecal %d\n",
			io->i_bn, io->i_cc, io->i_ma, hprecal);
#endif
		goto restart;
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
	int npf, bn, cn, tn, sn, bcr;

	if (bcr = MASKREG(mbp->mba_bcr>>16))
		bcr |= 0xffff0000;		/* sxt */
	npf = (bcr + io->i_cc) / sectsiz;	/* # sectors read */
	bn = io->i_bn + npf + ssect;		/* physical block #*/
	switch (flag) {

	case ECC: {
		register int i;
		caddr_t addr;
		int bit, byte, mask, ecccnt = 0;

		printf("hp%d: soft ecc sn%d\n", unit, bn);
		mask = MASKREG(rp->hpec2);
		i = MASKREG(rp->hpec1) - 1;	/* -1 makes 0 origin */
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
			if ((io->i_flgs & F_ECCLM) && ecccnt++ >= MAXECC)
				return (1);
		}
		return(0);
	}

	/*
	 * Skip sector error.
	 * Set skip-sector-inhibit and
	 * read next sector
	 */
	case SSE:
		rp->hpcs1 = HP_DCLR | HP_GO;
		HPWAIT(rp);
		rp->hpof |= HPOF_SSEI;
		return (0);	

	case BSE: {
		int bbn;

		rp->hpcs1 = HP_DCLR | HP_GO;
#ifdef HPDEBUG
		printf("hpecc: BSE @ bn %d\n", bn);
#endif
		cn = bn/st->nspc;
		sn = bn%st->nspc;
		tn = sn/st->nsect;
		sn = sn%st->nsect;
		bcr += sectsiz;
		if ((bbn = isbad(&hpbad[unit], cn, tn, sn)) < 0)
			return (1);
		bbn = st->ncyl*st->nspc - st->nsect - 1 - bbn;
		cn = bbn/st->nspc;
		sn = bbn%st->nspc;
		tn = sn/st->nsect;
		sn = sn%st->nsect;
		io->i_cc = sectsiz;
		io->i_ma += npf*sectsiz;
#ifdef HPDEBUG 
		printf("revector to cn %d tn %d sn %d mem: 0x%x\n", 
			cn, tn, sn, io->i_ma);
#endif 
		rp->hpof &= ~HPOF_SSEI;	/* clear skip sector inhibit if set */
		mbp->mba_sr = -1;
		rp->hpdc = cn;
		rp->hpda = (tn<<8) + sn;
		mbastart(io,io->i_flgs);
		io->i_errcnt = 0;	/* error has been corrected */
		HPWAIT(rp);
		return (rp->hpds&HPDS_ERR);
	}
	}
	printf("hpecc: flag=%d\n", flag);
	return (1);
}
/*ARGSUSED*/
hpioctl(io, cmd, arg)
	struct iob *io;
	int cmd;
	caddr_t arg;
{
	register unit = io->i_unit;
	struct st *st = &hpst[hp_type[unit]], *tmp;
	struct mba_drv *drv = mbadrv(unit);

	switch(cmd) {

	case SAIODEVDATA:
		if ((drv->mbd_dt&MBDT_TAP) == 0) {
			tmp = (struct st *)arg;
			*tmp = *st;
			return (0);
		}
		return (ECMD);

	case SAIOSSI:			/* skip-sector-inhibit */
		if (drv->mbd_dt&MBDT_TAP)
			return (ECMD);
		if ((io->i_flgs&F_SSI) == 0) {
			/* make sure this is done once only */
			io->i_flgs |= F_SSI;
			st->nsect++;
			st->nspc += st->ntrak;
		}
		return (0);

	case SAIONOSSI:			/* remove skip-sector-inhibit */
		if (io->i_flgs & F_SSI) {
			io->i_flgs &= ~F_SSI;
			drv->mbd_of &= ~HPOF_SSEI;
			st->nsect--;
			st->nspc -= st->ntrak;
		}
		return(0);

	case SAIOSSDEV:			/* drive have skip sector? */
		return (RM80 ? 0 : ECMD);
	}
	return (ECMD);
}
