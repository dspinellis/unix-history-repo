/*
 * Copyright (c) 1982, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ct.c	8.2 (Berkeley) %G%
 */

#include "ct.h"
#if NCT > 0
/*
 * CS80 cartridge tape driver (9144, 88140, 9145)
 *
 * Reminder:
 *	C_CC bit (character count option) when used in the CS/80 command
 *	'set options' will cause the tape not to stream.
 *
 * TODO:
 *	make filesystem compatible
 *	make block mode work according to mtio(4) spec. (if possible)
 *	merge with cs80 disk driver
 *	finish support of 9145
 */

#include <sys/param.h>
#include <sys/buf.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <sys/tprintf.h>
#include <sys/proc.h>

#include <hp/dev/device.h>

#include <hp300/dev/ctreg.h>

/* number of eof marks to remember */
#define EOFS	128

int	ctinit(), ctstart(), ctgo(), ctintr();
struct	driver ctdriver = {
	ctinit, "ct", ctstart, ctgo, ctintr,
};

struct	ct_softc {
	struct	hp_device *sc_hd;
	struct	ct_iocmd sc_ioc;
	struct	ct_rscmd sc_rsc;
	struct	ct_stat sc_stat;
	struct	ct_ssmcmd sc_ssmc;
	struct	ct_srcmd sc_src;
	struct	ct_soptcmd sc_soptc;
	struct	ct_ulcmd sc_ul;
	struct	ct_wfmcmd sc_wfm;
	struct	ct_clearcmd sc_clear;
	struct	buf *sc_bp;
	int	sc_blkno;
	int	sc_cmd;
	int	sc_resid;
	char	*sc_addr;
	int	sc_flags;
	short	sc_type;
	short	sc_punit;
	tpr_t	sc_tpr;
	struct	devqueue sc_dq;
	int	sc_eofp;
	int	sc_eofs[EOFS];
} ct_softc[NCT];

/* flags */
#define	CTF_OPEN	0x01
#define	CTF_ALIVE	0x02
#define	CTF_WRT		0x04
#define	CTF_CMD		0x08
#define	CTF_IO		0x10
#define	CTF_BEOF	0x20
#define	CTF_AEOF	0x40
#define	CTF_EOT		0x80
#define	CTF_STATWAIT	0x100
#define CTF_CANSTREAM	0x200
#define	CTF_WRTTN	0x400

struct	ctinfo {
	short	hwid;
	short	punit;
	char	*desc;
} ctinfo[] = {
	CT7946ID,	1,	"7946A",
	CT7912PID,	1,	"7912P",
	CT7914PID,	1,	"7914P",
	CT9144ID,	0,	"9144",
	CT9145ID,	0,	"9145",
};
int	nctinfo = sizeof(ctinfo) / sizeof(ctinfo[0]);

struct	buf cttab[NCT];
struct	buf ctbuf[NCT];

#define	CT_NOREW	4
#define	CT_STREAM	8
#define	UNIT(x)		(minor(x) & 3)
#define	ctpunit(x)	((x) & 7)

#ifdef DEBUG
int ctdebug = 0;
#define CDB_FILES	0x01
#define CT_BSF		0x02
#endif

ctinit(hd)
	register struct hp_device *hd;
{
	register struct ct_softc *sc = &ct_softc[hd->hp_unit];
	register struct buf *bp;

	for (bp = cttab; bp < &cttab[NCT]; bp++)
		bp->b_actb = &bp->b_actf;
	sc->sc_hd = hd;
	sc->sc_punit = ctpunit(hd->hp_flags);
	if (ctident(sc, hd) < 0)
		return(0);
	ctreset(sc, hd);
	sc->sc_dq.dq_ctlr = hd->hp_ctlr;
	sc->sc_dq.dq_unit = hd->hp_unit;
	sc->sc_dq.dq_slave = hd->hp_slave;
	sc->sc_dq.dq_driver = &ctdriver;
	sc->sc_flags |= CTF_ALIVE;
	return(1);
}

ctident(sc, hd)
	register struct ct_softc *sc;
	register struct hp_device *hd;
{
	struct ct_describe desc;
	u_char stat, cmd[3];
	char name[7];
	int id, i;

	/*
	 * Read device id and verify that:
	 * 1. It is a CS80 device
	 * 2. It is one of our recognized tape devices
	 * 3. It has the proper physical unit number
	 */
	id = hpibid(hd->hp_ctlr, hd->hp_slave);
	if ((id & 0x200) == 0)
		return(-1);
	for (i = 0; i < nctinfo; i++)
		if (id == ctinfo[i].hwid)
			break;
	if (i == nctinfo || sc->sc_punit != ctinfo[i].punit)
		return(-1);
	id = i;

	/*
	 * Collect device description.
	 * Right now we only need this to differentiate 7945 from 7946.
	 * Note that we always issue the describe command to unit 0.
	 */
	cmd[0] = C_SUNIT(0);
	cmd[1] = C_SVOL(0);
	cmd[2] = C_DESC;
	hpibsend(hd->hp_ctlr, hd->hp_slave, C_CMD, cmd, sizeof(cmd));
	hpibrecv(hd->hp_ctlr, hd->hp_slave, C_EXEC, &desc, 37);
	hpibrecv(hd->hp_ctlr, hd->hp_slave, C_QSTAT, &stat, sizeof(stat));
	bzero(name, sizeof(name));
	if (!stat) {
		register int n = desc.d_name;
		for (i = 5; i >= 0; i--) {
			name[i] = (n & 0xf) + '0';
			n >>= 4;
		}
	}
	switch (ctinfo[id].hwid) {
	case CT7946ID:
		if (bcmp(name, "079450", 6) == 0)
			return(-1);		/* not really a 7946 */
		/* fall into... */
	case CT9144ID:
	case CT9145ID:
		sc->sc_type = CT9144;
		sc->sc_flags |= CTF_CANSTREAM;
		break;

	case CT7912PID:
	case CT7914PID:
		sc->sc_type = CT88140;
		break;
	}
	printf("ct%d: %s %stape\n", hd->hp_unit, ctinfo[id].desc,
	       (sc->sc_flags & CTF_CANSTREAM) ? "streaming " : " ");
	return(id);
}

ctreset(sc, hd)
	register struct ct_softc *sc;
	register struct hp_device *hd;
{
	u_char stat;

	sc->sc_clear.unit = C_SUNIT(sc->sc_punit);
	sc->sc_clear.cmd = C_CLEAR;
	hpibsend(hd->hp_ctlr, hd->hp_slave, C_TCMD, &sc->sc_clear,
		sizeof(sc->sc_clear));
	hpibswait(hd->hp_ctlr, hd->hp_slave);
	hpibrecv(hd->hp_ctlr, hd->hp_slave, C_QSTAT, &stat, sizeof(stat));
	sc->sc_src.unit = C_SUNIT(CTCTLR);
	sc->sc_src.nop = C_NOP;
	sc->sc_src.cmd = C_SREL;
	sc->sc_src.param = C_REL;
	hpibsend(hd->hp_ctlr, hd->hp_slave, C_CMD, &sc->sc_src,
		sizeof(sc->sc_src));
	hpibswait(hd->hp_ctlr, hd->hp_slave);
	hpibrecv(hd->hp_ctlr, hd->hp_slave, C_QSTAT, &stat, sizeof(stat));
	sc->sc_ssmc.unit = C_SUNIT(sc->sc_punit);
	sc->sc_ssmc.cmd = C_SSM;
	sc->sc_ssmc.refm = REF_MASK;
	sc->sc_ssmc.fefm = FEF_MASK;
	sc->sc_ssmc.aefm = AEF_MASK;
	sc->sc_ssmc.iefm = IEF_MASK;
	hpibsend(hd->hp_ctlr, hd->hp_slave, C_CMD, &sc->sc_ssmc,
		sizeof(sc->sc_ssmc));
	hpibswait(hd->hp_ctlr, hd->hp_slave);
	hpibrecv(hd->hp_ctlr, hd->hp_slave, C_QSTAT, &stat, sizeof(stat));
	sc->sc_soptc.unit = C_SUNIT(sc->sc_punit);
	sc->sc_soptc.nop = C_NOP;
	sc->sc_soptc.cmd = C_SOPT;
	sc->sc_soptc.opt = C_SPAR;
	hpibsend(hd->hp_ctlr, hd->hp_slave, C_CMD, &sc->sc_soptc,
		sizeof(sc->sc_soptc));
	hpibswait(hd->hp_ctlr, hd->hp_slave);
	hpibrecv(hd->hp_ctlr, hd->hp_slave, C_QSTAT, &stat, sizeof(stat));
}

/*ARGSUSED*/
ctopen(dev, flag, type, p)
	dev_t dev;
	int flag, type;
	struct proc *p;
{
	register struct ct_softc *sc = &ct_softc[UNIT(dev)];
	u_char stat;
	int cc;

	if (UNIT(dev) >= NCT || (sc->sc_flags & CTF_ALIVE) == 0)
		return(ENXIO);
	if (sc->sc_flags & CTF_OPEN)
		return(EBUSY);
	sc->sc_soptc.unit = C_SUNIT(sc->sc_punit);
	sc->sc_soptc.nop = C_NOP;
	sc->sc_soptc.cmd = C_SOPT;
	if ((dev & CT_STREAM) && (sc->sc_flags & CTF_CANSTREAM))
		sc->sc_soptc.opt = C_SPAR | C_IMRPT;
	else
		sc->sc_soptc.opt = C_SPAR;
	/* 
	 * Check the return of hpibsend() and hpibswait().
	 * Drive could be loading/unloading a tape. If not checked,
	 * driver hangs. 
	 */
	cc = hpibsend(sc->sc_hd->hp_ctlr, sc->sc_hd->hp_slave,
	              C_CMD, &sc->sc_soptc, sizeof(sc->sc_soptc));
	if (cc != sizeof(sc->sc_soptc))
		return(EBUSY);
	hpibswait(sc->sc_hd->hp_ctlr, sc->sc_hd->hp_slave);
	cc = hpibrecv(sc->sc_hd->hp_ctlr, sc->sc_hd->hp_slave, C_QSTAT, 
	              &stat, sizeof(stat));
	if (cc != sizeof(stat))
		return(EBUSY);
	sc->sc_tpr = tprintf_open(p);
	sc->sc_flags |= CTF_OPEN;
	return(0);
}

/*ARGSUSED*/
ctclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct ct_softc *sc = &ct_softc[UNIT(dev)];

	if ((sc->sc_flags & (CTF_WRT|CTF_WRTTN)) == (CTF_WRT|CTF_WRTTN) &&
	    (sc->sc_flags & CTF_EOT) == 0 ) { /* XXX return error if EOT ?? */
		ctcommand(dev, MTWEOF, 2);
		ctcommand(dev, MTBSR, 1);
		if (sc->sc_eofp == EOFS - 1)
			sc->sc_eofs[EOFS - 1]--;
		else
			sc->sc_eofp--;
#ifdef DEBUG
		if(ctdebug & CT_BSF)
			printf("ct%d: ctclose backup eofs prt %d blk %d\n",
			       UNIT(dev), sc->sc_eofp, sc->sc_eofs[sc->sc_eofp]);
#endif
	}
	if ((minor(dev) & CT_NOREW) == 0)
		ctcommand(dev, MTREW, 1);
	sc->sc_flags &= ~(CTF_OPEN | CTF_WRT | CTF_WRTTN);
	tprintf_close(sc->sc_tpr);
#ifdef DEBUG
	if (ctdebug & CDB_FILES)
		printf("ctclose: flags %x\n", sc->sc_flags);
#endif
	return(0);	/* XXX */
}

ctcommand(dev, cmd, cnt)
	dev_t dev;
	int cmd;
	register int cnt;
{
	register struct ct_softc *sc = &ct_softc[UNIT(dev)];
	register struct buf *bp = &ctbuf[UNIT(dev)];
	register struct buf *nbp = 0;

	if (cmd == MTBSF && sc->sc_eofp == EOFS - 1) {
		cnt = sc->sc_eofs[EOFS - 1] - cnt;
		ctcommand(dev, MTREW, 1);
		ctcommand(dev, MTFSF, cnt);
		cnt = 2;
		cmd = MTBSR;
	}

	if (cmd == MTBSF && sc->sc_eofp - cnt < 0) {
		cnt = 1;
		cmd = MTREW;
	}

	sc->sc_flags |= CTF_CMD;
	sc->sc_bp = bp;
	sc->sc_cmd = cmd;
	bp->b_dev = dev;
	if (cmd == MTFSF) {
		nbp = (struct buf *)geteblk(MAXBSIZE);
		bp->b_un.b_addr = nbp->b_un.b_addr;
		bp->b_bcount = MAXBSIZE;
	}
again:
	bp->b_flags = B_BUSY;
	if (cmd == MTBSF) {
		sc->sc_blkno = sc->sc_eofs[sc->sc_eofp];
		sc->sc_eofp--;
#ifdef DEBUG
		if (ctdebug & CT_BSF)
			printf("ct%d: backup eof pos %d blk %d\n",
			       UNIT(dev), sc->sc_eofp, 
			       sc->sc_eofs[sc->sc_eofp]);
#endif
	}
	ctstrategy(bp);
	iowait(bp);
	if (--cnt > 0)
		goto again;
	bp->b_flags = 0;
	sc->sc_flags &= ~CTF_CMD;
	if (nbp)
		brelse(nbp);
}

ctstrategy(bp)
	register struct buf *bp;
{
	register struct buf *dp;
	register int s, unit;

	unit = UNIT(bp->b_dev);
	dp = &cttab[unit];
	bp->b_actf = NULL;
	s = splbio();
	bp->b_actb = dp->b_actb;
	*dp->b_actb = bp;
	dp->b_actb = &bp->b_actf;
	if (dp->b_active == 0) {
		dp->b_active = 1;
		ctustart(unit);
	}
	splx(s);
}

ctustart(unit)
	register int unit;
{
	register struct ct_softc *sc = &ct_softc[unit];
	register struct buf *bp;

	bp = cttab[unit].b_actf;
	sc->sc_addr = bp->b_un.b_addr;
	sc->sc_resid = bp->b_bcount;
	if (hpibreq(&sc->sc_dq))
		ctstart(unit);
}

ctstart(unit)
	register int unit;
{
	register struct ct_softc *sc = &ct_softc[unit];
	register struct buf *bp, *dp;
	register int i;

	bp = cttab[unit].b_actf;
again:
	if ((sc->sc_flags & CTF_CMD) && sc->sc_bp == bp) {
		switch(sc->sc_cmd) {

		case MTFSF:
			bp->b_flags |= B_READ;
			goto mustio;

		case MTBSF:
			goto gotaddr;

		case MTOFFL:
			sc->sc_blkno = 0;
			sc->sc_ul.unit = C_SUNIT(sc->sc_punit);
			sc->sc_ul.cmd = C_UNLOAD;
			hpibsend(sc->sc_hd->hp_ctlr, sc->sc_hd->hp_slave,
				C_CMD, &sc->sc_ul, sizeof(sc->sc_ul));
			break;

		case MTWEOF:
			sc->sc_blkno++;
			sc->sc_flags |= CTF_WRT;
			sc->sc_wfm.unit = C_SUNIT(sc->sc_punit);
			sc->sc_wfm.cmd = C_WFM;
			hpibsend(sc->sc_hd->hp_ctlr, sc->sc_hd->hp_slave,
				C_CMD, &sc->sc_wfm, sizeof(sc->sc_wfm));
			ctaddeof(unit);
			break;

		case MTBSR:
			sc->sc_blkno--;
			goto gotaddr;

		case MTFSR:
			sc->sc_blkno++;
			goto gotaddr;

		case MTREW:
			sc->sc_blkno = 0;
#ifdef DEBUG
			if(ctdebug & CT_BSF)
				printf("ct%d: clearing eofs\n", unit);
#endif
			for (i=0; i<EOFS; i++)
				sc->sc_eofs[i] = 0;
			sc->sc_eofp = 0;

gotaddr:
			sc->sc_ioc.saddr = C_SADDR;
			sc->sc_ioc.addr0 = 0;
			sc->sc_ioc.addr = sc->sc_blkno;
			sc->sc_ioc.unit = C_SUNIT(sc->sc_punit);
			sc->sc_ioc.nop2 = C_NOP;
			sc->sc_ioc.slen = C_SLEN;
			sc->sc_ioc.len = 0;
			sc->sc_ioc.nop3 = C_NOP;
			sc->sc_ioc.cmd = C_READ;
			hpibsend(sc->sc_hd->hp_ctlr, sc->sc_hd->hp_slave,
				C_CMD, &sc->sc_ioc, sizeof(sc->sc_ioc));
			break;
		}
	}
	else {
mustio:
		if ((bp->b_flags & B_READ) &&
		    sc->sc_flags & (CTF_BEOF|CTF_EOT)) {
#ifdef DEBUG
			if (ctdebug & CDB_FILES)
				printf("ctstart: before flags %x\n", sc->sc_flags);
#endif
			if (sc->sc_flags & CTF_BEOF) {
				sc->sc_flags &= ~CTF_BEOF;
				sc->sc_flags |= CTF_AEOF;
#ifdef DEBUG
				if (ctdebug & CDB_FILES)
					printf("ctstart: after flags %x\n", sc->sc_flags);
#endif
			}
			bp->b_resid = bp->b_bcount;
			iodone(bp);
			hpibfree(&sc->sc_dq);
			if (dp = bp->b_actf)
				dp->b_actb = bp->b_actb;
			else
				cttab[unit].b_actb = bp->b_actb;
			*bp->b_actb = dp;
			if ((bp = dp) == NULL) {
				cttab[unit].b_active = 0;
				return;
			}
			sc->sc_addr = bp->b_un.b_addr;
			sc->sc_resid = bp->b_bcount;
			if (hpibreq(&sc->sc_dq))
				goto again;
			return;
		}			
		sc->sc_flags |= CTF_IO;
		sc->sc_ioc.unit = C_SUNIT(sc->sc_punit);
		sc->sc_ioc.saddr = C_SADDR;
		sc->sc_ioc.addr0 = 0;
		sc->sc_ioc.addr = sc->sc_blkno;
		sc->sc_ioc.nop2 = C_NOP;
		sc->sc_ioc.slen = C_SLEN;
		sc->sc_ioc.len = sc->sc_resid;
		sc->sc_ioc.nop3 = C_NOP;
		if (bp->b_flags & B_READ)
			sc->sc_ioc.cmd = C_READ;
		else {
			sc->sc_ioc.cmd = C_WRITE;
			sc->sc_flags |= (CTF_WRT | CTF_WRTTN);
		}
		hpibsend(sc->sc_hd->hp_ctlr, sc->sc_hd->hp_slave, C_CMD,
			&sc->sc_ioc, sizeof(sc->sc_ioc));
	}
	hpibawait(sc->sc_hd->hp_ctlr);
}

ctgo(unit)
	register int unit;
{
	register struct ct_softc *sc = &ct_softc[unit];
	register struct buf *bp;

	bp = cttab[unit].b_actf;
	hpibgo(sc->sc_hd->hp_ctlr, sc->sc_hd->hp_slave, C_EXEC,
		sc->sc_addr, sc->sc_resid, bp->b_flags & B_READ);
}

/*
 * Hideous grue to handle EOF/EOT (mostly for reads)
 */
cteof(sc, bp)
	register struct ct_softc *sc;
	register struct buf *bp;
{
	long blks;

	/*
	 * EOT on a write is an error.
	 */
	if ((bp->b_flags & B_READ) == 0) {
		bp->b_resid = bp->b_bcount;
		bp->b_flags |= B_ERROR;
		bp->b_error = ENOSPC;
		sc->sc_flags |= CTF_EOT;
		return;
	}
	/*
	 * Use returned block position to determine how many blocks
	 * we really read and update b_resid.
	 */
	blks = sc->sc_stat.c_blk - sc->sc_blkno - 1;
#ifdef DEBUG
	if (ctdebug & CDB_FILES)
		printf("cteof: bc %d oblk %d nblk %d read %d, resid %d\n",
		       bp->b_bcount, sc->sc_blkno, sc->sc_stat.c_blk,
		       blks, bp->b_bcount - CTKTOB(blks));
#endif
	if (blks == -1) { /* 9145 on EOF does not change sc_stat.c_blk */
		blks = 0;
		sc->sc_blkno++;
	}
	else {
		sc->sc_blkno = sc->sc_stat.c_blk;
	}
	bp->b_resid = bp->b_bcount - CTKTOB(blks);
	/*
	 * If we are at physical EOV or were after an EOF,
	 * we are now at logical EOT.
	 */
	if ((sc->sc_stat.c_aef & AEF_EOV) ||
	    (sc->sc_flags & CTF_AEOF)) {
		sc->sc_flags |= CTF_EOT;
		sc->sc_flags &= ~(CTF_AEOF|CTF_BEOF);
	}
	/*
	 * If we were before an EOF or we have just completed a FSF,
	 * we are now after EOF.
	 */
	else if ((sc->sc_flags & CTF_BEOF) ||
		 (sc->sc_flags & CTF_CMD) && sc->sc_cmd == MTFSF) {
		sc->sc_flags |= CTF_AEOF;
		sc->sc_flags &= ~CTF_BEOF;
	}
	/*
	 * Otherwise if we read something we are now before EOF
	 * (and no longer after EOF).
	 */
	else if (blks) {
		sc->sc_flags |= CTF_BEOF;
		sc->sc_flags &= ~CTF_AEOF;
	}
	/*
	 * Finally, if we didn't read anything we just passed an EOF
	 */
	else
		sc->sc_flags |= CTF_AEOF;
#ifdef DEBUG
	if (ctdebug & CDB_FILES)
		printf("cteof: leaving flags %x\n", sc->sc_flags);
#endif
}

ctintr(unit)
	register int unit;
{
	register struct ct_softc *sc = &ct_softc[unit];
	register struct buf *bp, *dp;
	u_char stat;

	bp = cttab[unit].b_actf;
	if (bp == NULL) {
		printf("ct%d: bp == NULL\n", unit);
		return;
	}
	if (sc->sc_flags & CTF_IO) {
		sc->sc_flags &= ~CTF_IO;
		if (hpibustart(sc->sc_hd->hp_ctlr))
			ctgo(unit);
		return;
	}
	if ((sc->sc_flags & CTF_STATWAIT) == 0) {
		if (hpibpptest(sc->sc_hd->hp_ctlr, sc->sc_hd->hp_slave) == 0) {
			sc->sc_flags |= CTF_STATWAIT;
			hpibawait(sc->sc_hd->hp_ctlr);
			return;
		}
	} else
		sc->sc_flags &= ~CTF_STATWAIT;
	hpibrecv(sc->sc_hd->hp_ctlr, sc->sc_hd->hp_slave, C_QSTAT, &stat, 1);
#ifdef DEBUG
	if (ctdebug & CDB_FILES)
		printf("ctintr: before flags %x\n", sc->sc_flags);
#endif
	if (stat) {
		sc->sc_rsc.unit = C_SUNIT(sc->sc_punit);
		sc->sc_rsc.cmd = C_STATUS;
		hpibsend(sc->sc_hd->hp_ctlr, sc->sc_hd->hp_slave, C_CMD,
			&sc->sc_rsc, sizeof(sc->sc_rsc));
		hpibrecv(sc->sc_hd->hp_ctlr, sc->sc_hd->hp_slave, C_EXEC,
			&sc->sc_stat, sizeof(sc->sc_stat));
		hpibrecv(sc->sc_hd->hp_ctlr, sc->sc_hd->hp_slave, C_QSTAT,
			&stat, 1);
#ifdef DEBUG
		if (ctdebug & CDB_FILES)
			printf("ctintr: return stat 0x%x, A%x F%x blk %d\n",
			       stat, sc->sc_stat.c_aef,
			       sc->sc_stat.c_fef, sc->sc_stat.c_blk);
#endif
		if (stat == 0) {
			if (sc->sc_stat.c_aef & (AEF_EOF | AEF_EOV)) {
				cteof(sc, bp);
				ctaddeof(unit);
				goto done;
			}
			if (sc->sc_stat.c_fef & FEF_PF) {
				ctreset(sc, sc->sc_hd);
				ctstart(unit);
				return;
			}
			if (sc->sc_stat.c_fef & FEF_REXMT) {
				ctstart(unit);
				return;
			}
			if (sc->sc_stat.c_aef & 0x5800) {
				if (sc->sc_stat.c_aef & 0x4000)
					tprintf(sc->sc_tpr,
						"ct%d: uninitialized media\n",
						unit);
				if (sc->sc_stat.c_aef & 0x1000)
					tprintf(sc->sc_tpr,
						"ct%d: not ready\n", unit);
				if (sc->sc_stat.c_aef & 0x0800)
					tprintf(sc->sc_tpr,
						"ct%d: write protect\n", unit);
			} else {
				printf("ct%d err: v%d u%d ru%d bn%d, ",
				       unit,
				       (sc->sc_stat.c_vu>>4)&0xF,
				       sc->sc_stat.c_vu&0xF,
				       sc->sc_stat.c_pend,
				       sc->sc_stat.c_blk);
				printf("R0x%x F0x%x A0x%x I0x%x\n",
				       sc->sc_stat.c_ref,
				       sc->sc_stat.c_fef,
				       sc->sc_stat.c_aef,
				       sc->sc_stat.c_ief);
			}
		} else
			printf("ct%d: request status failed\n", unit);
		bp->b_flags |= B_ERROR;
		bp->b_error = EIO;
		goto done;
	} else
		bp->b_resid = 0;
	if (sc->sc_flags & CTF_CMD) {
		switch (sc->sc_cmd) {
		case MTFSF:
			sc->sc_flags &= ~(CTF_BEOF|CTF_AEOF);
			sc->sc_blkno += CTBTOK(sc->sc_resid);
			ctstart(unit);
			return;
		case MTBSF:
			sc->sc_flags &= ~(CTF_AEOF|CTF_BEOF|CTF_EOT);
			break;
		case MTBSR:
			sc->sc_flags &= ~CTF_BEOF;
			if (sc->sc_flags & CTF_EOT) {
				sc->sc_flags |= CTF_AEOF;
				sc->sc_flags &= ~CTF_EOT;
			} else if (sc->sc_flags & CTF_AEOF) {
				sc->sc_flags |= CTF_BEOF;
				sc->sc_flags &= ~CTF_AEOF;
			}
			break;
		case MTWEOF:
			sc->sc_flags &= ~CTF_BEOF;
			if (sc->sc_flags & (CTF_AEOF|CTF_EOT)) {
				sc->sc_flags |= CTF_EOT;
				sc->sc_flags &= ~CTF_AEOF;
			} else
				sc->sc_flags |= CTF_AEOF;
			break;
		case MTREW:
		case MTOFFL:
			sc->sc_flags &= ~(CTF_BEOF|CTF_AEOF|CTF_EOT);
			break;
		}
	} else {
		sc->sc_flags &= ~CTF_AEOF;
		sc->sc_blkno += CTBTOK(sc->sc_resid);
	}
done:
#ifdef DEBUG
	if (ctdebug & CDB_FILES)
		printf("ctintr: after flags %x\n", sc->sc_flags);
#endif
	if (dp = bp->b_actf)
		dp->b_actb = bp->b_actb;
	else
		cttab[unit].b_actb = bp->b_actb;
	*bp->b_actb = dp;
	iodone(bp);
	hpibfree(&sc->sc_dq);
	if (cttab[unit].b_actf == NULL) {
		cttab[unit].b_active = 0;
		return;
	}
	ctustart(unit);
}

ctread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = UNIT(dev);

	return(physio(ctstrategy, &ctbuf[unit], dev, B_READ, minphys, uio));
}

ctwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = UNIT(dev);

	return(physio(ctstrategy, &ctbuf[unit], dev, B_WRITE, minphys, uio));
}

/*ARGSUSED*/
ctioctl(dev, cmd, data, flag)
	dev_t dev;
	int cmd, flag;
	caddr_t data;
{
	register struct mtop *op;
	register int cnt;

	switch (cmd) {

	case MTIOCTOP:
		op = (struct mtop *)data;
		switch(op->mt_op) {

		case MTWEOF:
		case MTFSF:
		case MTBSR:
		case MTBSF:
		case MTFSR:
			cnt = op->mt_count;
			break;

		case MTREW:
		case MTOFFL:
			cnt = 1;
			break;

		default:
			return(EINVAL);
		}
		ctcommand(dev, op->mt_op, cnt);
		break;

	case MTIOCGET:
		break;

	default:
		return(EINVAL);
	}
	return(0);
}

/*ARGSUSED*/
ctdump(dev)
	dev_t dev;
{
	return(ENXIO);
}

ctaddeof(unit)
	int unit;
{
	register struct ct_softc *sc = &ct_softc[unit];
	
	if (sc->sc_eofp == EOFS - 1)
		sc->sc_eofs[EOFS - 1]++;
	else {
		sc->sc_eofp++;
		if (sc->sc_eofp == EOFS - 1)
			sc->sc_eofs[EOFS - 1] = EOFS;
		else
			/* save blkno */
			sc->sc_eofs[sc->sc_eofp] = sc->sc_blkno - 1;
	}
#ifdef DEBUG
	if (ctdebug & CT_BSF)
		printf("ct%d: add eof pos %d blk %d\n",
		       unit, sc->sc_eofp,
		       sc->sc_eofs[sc->sc_eofp]);
#endif
}
#endif
