/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: rd.c 1.44 92/12/26$
 *
 *	@(#)rd.c	7.18 (Berkeley) %G%
 */

/*
 * CS80/SS80 disk driver
 */
#include "rd.h"
#if NRD > 0

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/buf.h>
#include <sys/stat.h>
#include <sys/dkstat.h>
#include <sys/disklabel.h>
#include <sys/ioctl.h>
#include <sys/fcntl.h>

#include <hp/dev/device.h>
#include <hp300/dev/rdreg.h>
#include <hp300/dev/rdvar.h>
#ifdef USELEDS
#include <hp300/hp300/led.h>
#endif

#include <vm/vm_param.h>
#include <vm/lock.h>
#include <vm/vm_prot.h>
#include <vm/pmap.h>

int	rdinit(), rdstart(), rdgo(), rdintr();
void	rdstrategy();
struct	driver rddriver = {
	rdinit, "rd", rdstart, rdgo, rdintr,
};

struct	rd_softc rd_softc[NRD];
struct	buf rdtab[NRD];
int	rderrthresh = RDRETRY-1;	/* when to start reporting errors */

#ifdef DEBUG
/* error message tables */
char *err_reject[] = {
	0, 0,
	"channel parity error",		/* 0x2000 */
	0, 0,
	"illegal opcode",		/* 0x0400 */
	"module addressing",		/* 0x0200 */
	"address bounds",		/* 0x0100 */
	"parameter bounds",		/* 0x0080 */
	"illegal parameter",		/* 0x0040 */
	"message sequence",		/* 0x0020 */
	0,
	"message length",		/* 0x0008 */
	0, 0, 0
};

char *err_fault[] = {
	0,
	"cross unit",			/* 0x4000 */
	0,
	"controller fault",		/* 0x1000 */
	0, 0,
	"unit fault",			/* 0x0200 */
	0,
	"diagnostic result",		/* 0x0080 */
	0,
	"operator release request",	/* 0x0020 */
	"diagnostic release request",	/* 0x0010 */
	"internal maintenance release request",	/* 0x0008 */
	0,
	"power fail",			/* 0x0002 */
	"retransmit"			/* 0x0001 */
};

char *err_access[] = {
	"illegal parallel operation",	/* 0x8000 */
	"uninitialized media",		/* 0x4000 */
	"no spares available",		/* 0x2000 */
	"not ready",			/* 0x1000 */
	"write protect",		/* 0x0800 */
	"no data found",		/* 0x0400 */
	0, 0,
	"unrecoverable data overflow",	/* 0x0080 */
	"unrecoverable data",		/* 0x0040 */
	0,
	"end of file",			/* 0x0010 */
	"end of volume",		/* 0x0008 */
	0, 0, 0
};

char *err_info[] = {
	"operator release request",	/* 0x8000 */
	"diagnostic release request",	/* 0x4000 */
	"internal maintenance release request",	/* 0x2000 */
	"media wear",			/* 0x1000 */
	"latency induced",		/* 0x0800 */
	0, 0,
	"auto sparing invoked",		/* 0x0100 */
	0,
	"recoverable data overflow",	/* 0x0040 */
	"marginal data",		/* 0x0020 */
	"recoverable data",		/* 0x0010 */
	0,
	"maintenance track overflow",	/* 0x0004 */
	0, 0
};

struct	rdstats rdstats[NRD];
int	rddebug = 0x80;
#define RDB_FOLLOW	0x01
#define RDB_STATUS	0x02
#define RDB_IDENT	0x04
#define RDB_IO		0x08
#define RDB_ASYNC	0x10
#define RDB_ERROR	0x80
#endif

/*
 * Misc. HW description, indexed by sc_type.
 * Nothing really critical here, could do without it.
 */
struct rdidentinfo rdidentinfo[] = {
	{ RD7946AID,	0,	"7945A",	 108416 },
	{ RD9134DID,	1,	"9134D",	  29088 },
	{ RD9134LID,	1,	"9122S",	   1232 },
	{ RD7912PID,	0,	"7912P",	 128128 },
	{ RD7914PID,	0,	"7914P",	 258048 },
	{ RD7958AID,	0,	"7958A",	 255276 },
	{ RD7957AID,	0,	"7957A",	 159544 },
	{ RD7933HID,	0,	"7933H",	 789958 },
	{ RD9134LID,	1,	"9134L",	  77840 },
	{ RD7936HID,	0,	"7936H",	 600978 },
	{ RD7937HID,	0,	"7937H",	1116102 },
	{ RD7914CTID,	0,	"7914CT",	 258048 },
	{ RD7946AID,	0,	"7946A",	 108416 },
	{ RD9134LID,	1,	"9122D",	   1232 },
	{ RD7957BID,	0,	"7957B",	 159894 },
	{ RD7958BID,	0,	"7958B",	 297108 },
	{ RD7959BID,	0,	"7959B",	 594216 },
	{ RD2200AID,	0,	"2200A",	 654948 },
	{ RD2203AID,	0,	"2203A",	1309896 }
};
int numrdidentinfo = sizeof(rdidentinfo) / sizeof(rdidentinfo[0]);

rdinit(hd)
	register struct hp_device *hd;
{
	register struct rd_softc *rs = &rd_softc[hd->hp_unit];

	rs->sc_hd = hd;
	rs->sc_punit = rdpunit(hd->hp_flags);
	rs->sc_type = rdident(rs, hd);
	if (rs->sc_type < 0)
		return(0);
	rs->sc_dq.dq_ctlr = hd->hp_ctlr;
	rs->sc_dq.dq_unit = hd->hp_unit;
	rs->sc_dq.dq_slave = hd->hp_slave;
	rs->sc_dq.dq_driver = &rddriver;
	rs->sc_flags = RDF_ALIVE;
#ifdef DEBUG
	/* always report errors */
	if (rddebug & RDB_ERROR)
		rderrthresh = 0;
#endif
	return(1);
}

rdident(rs, hd)
	struct rd_softc *rs;
	struct hp_device *hd;
{
	struct rd_describe desc;
	u_char stat, cmd[3];
	int unit, lunit;
	char name[7];
	register int ctlr, slave, id, i;

	ctlr = hd->hp_ctlr;
	slave = hd->hp_slave;
	unit = rs->sc_punit;
	lunit = hd->hp_unit;

	/*
	 * Grab device id and make sure:
	 * 1. It is a CS80 device.
	 * 2. It is one of the types we support.
	 * 3. If it is a 7946, we are accessing the disk unit (0)
	 */
	id = hpibid(ctlr, slave);
#ifdef DEBUG
	if (rddebug & RDB_IDENT)
		printf("hpibid(%d, %d) -> %x\n", ctlr, slave, id);
#endif
	if ((id & 0x200) == 0)
		return(-1);
	for (i = 0; i < numrdidentinfo; i++)
		if (id == rdidentinfo[i].ri_hwid)
			break;
	if (i == numrdidentinfo || unit > rdidentinfo[i].ri_maxunum)
		return(-1);
	id = i;

	/*
	 * Reset drive and collect device description.
	 * Don't really use the description info right now but
	 * might come in handy in the future (for disk labels).
	 */
	rdreset(rs, hd);
	cmd[0] = C_SUNIT(unit);
	cmd[1] = C_SVOL(0);
	cmd[2] = C_DESC;
	hpibsend(ctlr, slave, C_CMD, cmd, sizeof(cmd));
	hpibrecv(ctlr, slave, C_EXEC, &desc, 37);
	hpibrecv(ctlr, slave, C_QSTAT, &stat, sizeof(stat));
	bzero(name, sizeof(name));
	if (!stat) {
		register int n = desc.d_name;
		for (i = 5; i >= 0; i--) {
			name[i] = (n & 0xf) + '0';
			n >>= 4;
		}
		/* use drive characteristics to calculate xfer rate */
		rs->sc_wpms = 1000000 * (desc.d_sectsize/2) / desc.d_blocktime;
	}
#ifdef DEBUG
	if (rddebug & RDB_IDENT) {
		printf("rd%d: name: %x ('%s')\n",
		       lunit, desc.d_name, name);
		printf("  iuw %x, maxxfr %d, ctype %d\n",
		       desc.d_iuw, desc.d_cmaxxfr, desc.d_ctype);
		printf("  utype %d, bps %d, blkbuf %d, burst %d, blktime %d\n",
		       desc.d_utype, desc.d_sectsize,
		       desc.d_blkbuf, desc.d_burstsize, desc.d_blocktime);
		printf("  avxfr %d, ort %d, atp %d, maxint %d, fv %x, rv %x\n",
		       desc.d_uavexfr, desc.d_retry, desc.d_access,
		       desc.d_maxint, desc.d_fvbyte, desc.d_rvbyte);
		printf("  maxcyl/head/sect %d/%d/%d, maxvsect %d, inter %d\n",
		       desc.d_maxcyl, desc.d_maxhead, desc.d_maxsect,
		       desc.d_maxvsectl, desc.d_interleave);
	}
#endif
	/*
	 * Take care of a couple of anomolies:
	 * 1. 7945A and 7946A both return same HW id
	 * 2. 9122S and 9134D both return same HW id
	 * 3. 9122D and 9134L both return same HW id
	 */
	switch (rdidentinfo[id].ri_hwid) {
	case RD7946AID:
		if (bcmp(name, "079450", 6) == 0)
			id = RD7945A;
		else
			id = RD7946A;
		break;

	case RD9134LID:
		if (bcmp(name, "091340", 6) == 0)
			id = RD9134L;
		else
			id = RD9122D;
		break;

	case RD9134DID:
		if (bcmp(name, "091220", 6) == 0)
			id = RD9122S;
		else
			id = RD9134D;
		break;
	}
	printf("rd%d: %s\n", lunit, rdidentinfo[id].ri_desc);
	return(id);
}

rdreset(rs, hd)
	register struct rd_softc *rs;
	register struct hp_device *hd;
{
	u_char stat;

	rs->sc_clear.c_unit = C_SUNIT(rs->sc_punit);
	rs->sc_clear.c_cmd = C_CLEAR;
	hpibsend(hd->hp_ctlr, hd->hp_slave, C_TCMD, &rs->sc_clear,
		sizeof(rs->sc_clear));
	hpibswait(hd->hp_ctlr, hd->hp_slave);
	hpibrecv(hd->hp_ctlr, hd->hp_slave, C_QSTAT, &stat, sizeof(stat));
	rs->sc_src.c_unit = C_SUNIT(RDCTLR);
	rs->sc_src.c_nop = C_NOP;
	rs->sc_src.c_cmd = C_SREL;
	rs->sc_src.c_param = C_REL;
	hpibsend(hd->hp_ctlr, hd->hp_slave, C_CMD, &rs->sc_src,
		sizeof(rs->sc_src));
	hpibswait(hd->hp_ctlr, hd->hp_slave);
	hpibrecv(hd->hp_ctlr, hd->hp_slave, C_QSTAT, &stat, sizeof(stat));
	rs->sc_ssmc.c_unit = C_SUNIT(rs->sc_punit);
	rs->sc_ssmc.c_cmd = C_SSM;
	rs->sc_ssmc.c_refm = REF_MASK;
	rs->sc_ssmc.c_fefm = FEF_MASK;
	rs->sc_ssmc.c_aefm = AEF_MASK;
	rs->sc_ssmc.c_iefm = IEF_MASK;
	hpibsend(hd->hp_ctlr, hd->hp_slave, C_CMD, &rs->sc_ssmc,
		sizeof(rs->sc_ssmc));
	hpibswait(hd->hp_ctlr, hd->hp_slave);
	hpibrecv(hd->hp_ctlr, hd->hp_slave, C_QSTAT, &stat, sizeof(stat));
#ifdef DEBUG
	rdstats[hd->hp_unit].rdresets++;
#endif
}

/*
 * Read or constuct a disklabel
 */
int
rdgetinfo(dev)
	dev_t dev;
{
	int unit = rdunit(dev);
	register struct rd_softc *rs = &rd_softc[unit];
	register struct disklabel *lp = &rs->sc_info.ri_label;
	register struct partition *pi;
	char *msg, *readdisklabel();

	/*
	 * Set some default values to use while reading the label
	 * or to use if there isn't a label.
	 */
	bzero((caddr_t)lp, sizeof *lp);
	lp->d_type = DTYPE_HPIB;
	lp->d_secsize = DEV_BSIZE;
	lp->d_nsectors = 32;
	lp->d_ntracks = 20;
	lp->d_secpercyl = 32*20;
	lp->d_npartitions = 3;
	lp->d_partitions[2].p_offset = 0;
	lp->d_partitions[2].p_size = LABELSECTOR+1;

	/*
	 * Now try to read the disklabel
	 */
	msg = readdisklabel(rdlabdev(dev), rdstrategy, lp);
	if (msg == NULL)
		return(0);

	pi = lp->d_partitions;
	printf("rd%d: WARNING: %s, ", unit, msg);
#ifdef COMPAT_NOLABEL
	printf("using old default partitioning\n");
	rdmakedisklabel(unit, lp);
#else
	printf("defining `c' partition as entire disk\n");
	pi[2].p_size = rdidentinfo[rs->sc_type].ri_nblocks;
#endif
	return(0);
}

int
rdopen(dev, flags, mode, p)
	dev_t dev;
	int flags, mode;
	struct proc *p;
{
	register int unit = rdunit(dev);
	register struct rd_softc *rs = &rd_softc[unit];
	int error, mask;

	if (unit >= NRD || (rs->sc_flags & RDF_ALIVE) == 0)
		return(ENXIO);

	/*
	 * Wait for any pending opens/closes to complete
	 */
	while (rs->sc_flags & (RDF_OPENING|RDF_CLOSING))
		sleep((caddr_t)rs, PRIBIO);

	/*
	 * On first open, get label and partition info.
	 * We may block reading the label, so be careful
	 * to stop any other opens.
	 */
	if (rs->sc_info.ri_open == 0) {
		rs->sc_flags |= RDF_OPENING;
		error = rdgetinfo(dev);
		rs->sc_flags &= ~RDF_OPENING;
		wakeup((caddr_t)rs);
		if (error)
			return(error);
	}
	if (rs->sc_hd->hp_dk >= 0) {
		/* guess at xfer rate based on 3600 rpm (60 rps) */
		if (rs->sc_wpms == 0)
			rs->sc_wpms = 60 * rs->sc_info.ri_label.d_nsectors
				* DEV_BSIZE / 2;
		dk_wpms[rs->sc_hd->hp_dk] = rs->sc_wpms;
	}

	mask = 1 << rdpart(dev);
	if (mode == S_IFCHR)
		rs->sc_info.ri_copen |= mask;
	else
		rs->sc_info.ri_bopen |= mask;
	rs->sc_info.ri_open |= mask;
	return(0);
}

int
rdclose(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	int unit = rdunit(dev);
	register struct rd_softc *rs = &rd_softc[unit];
	register struct rdinfo *ri = &rs->sc_info;
	int mask, s;

	mask = 1 << rdpart(dev);
	if (mode == S_IFCHR)
		ri->ri_copen &= ~mask;
	else
		ri->ri_bopen &= ~mask;
	ri->ri_open = ri->ri_bopen | ri->ri_copen;
	/*
	 * On last close, we wait for all activity to cease since
	 * the label/parition info will become invalid.  Since we
	 * might sleep, we must block any opens while we are here.
	 * Note we don't have to about other closes since we know
	 * we are the last one.
	 */
	if (ri->ri_open == 0) {
		rs->sc_flags |= RDF_CLOSING;
		s = splbio();
		while (rdtab[unit].b_active) {
			rs->sc_flags |= RDF_WANTED;
			sleep((caddr_t)&rdtab[unit], PRIBIO);
		}
		splx(s);
		rs->sc_flags &= ~(RDF_CLOSING|RDF_WLABEL);
		wakeup((caddr_t)rs);
	}
	return(0);
}

void
rdstrategy(bp)
	register struct buf *bp;
{
	int unit = rdunit(bp->b_dev);
	register struct rd_softc *rs = &rd_softc[unit];
	register struct buf *dp = &rdtab[unit];
	register struct partition *pinfo;
	register daddr_t bn;
	register int sz, s;

#ifdef DEBUG
	if (rddebug & RDB_FOLLOW)
		printf("rdstrategy(%x): dev %x, bn %x, bcount %x, %c\n",
		       bp, bp->b_dev, bp->b_blkno, bp->b_bcount,
		       (bp->b_flags & B_READ) ? 'R' : 'W');
#endif
	bn = bp->b_blkno;
	sz = howmany(bp->b_bcount, DEV_BSIZE);
	pinfo = &rs->sc_info.ri_label.d_partitions[rdpart(bp->b_dev)];
	if (bn < 0 || bn + sz > pinfo->p_size) {
		sz = pinfo->p_size - bn;
		if (sz == 0) {
			bp->b_resid = bp->b_bcount;
			goto done;
		}
		if (sz < 0) {
			bp->b_error = EINVAL;
			goto bad;
		}
		bp->b_bcount = dbtob(sz);
	}
	/*
	 * Check for write to write protected label
	 */
	if (bn + pinfo->p_offset <= LABELSECTOR &&
#if LABELSECTOR != 0
	    bn + pinfo->p_offset + sz > LABELSECTOR &&
#endif
	    !(bp->b_flags & B_READ) && !(rs->sc_flags & RDF_WLABEL)) {
		bp->b_error = EROFS;
		goto bad;
	}
	bp->b_cylin = bn + pinfo->p_offset;
	s = splbio();
	disksort(dp, bp);
	if (dp->b_active == 0) {
		dp->b_active = 1;
		rdustart(unit);
	}
	splx(s);
	return;
bad:
	bp->b_flags |= B_ERROR;
done:
	biodone(bp);
}

/*
 * Called from timeout() when handling maintenance releases
 */
void
rdrestart(arg)
	void *arg;
{
	int s = splbio();
	rdustart((int)arg);
	splx(s);
}

rdustart(unit)
	register int unit;
{
	register struct buf *bp;
	register struct rd_softc *rs = &rd_softc[unit];

	bp = rdtab[unit].b_actf;
	rs->sc_addr = bp->b_un.b_addr;
	rs->sc_resid = bp->b_bcount;
	if (hpibreq(&rs->sc_dq))
		rdstart(unit);
}

struct buf *
rdfinish(unit, rs, bp)
	int unit;
	register struct rd_softc *rs;
	register struct buf *bp;
{
	register struct buf *dp = &rdtab[unit];

	dp->b_errcnt = 0;
	dp->b_actf = bp->b_actf;
	bp->b_resid = 0;
	biodone(bp);
	hpibfree(&rs->sc_dq);
	if (dp->b_actf)
		return(dp->b_actf);
	dp->b_active = 0;
	if (rs->sc_flags & RDF_WANTED) {
		rs->sc_flags &= ~RDF_WANTED;
		wakeup((caddr_t)dp);
	}
	return(NULL);
}

rdstart(unit)
	register int unit;
{
	register struct rd_softc *rs = &rd_softc[unit];
	register struct buf *bp = rdtab[unit].b_actf;
	register struct hp_device *hp = rs->sc_hd;
	register int part;

again:
#ifdef DEBUG
	if (rddebug & RDB_FOLLOW)
		printf("rdstart(%d): bp %x, %c\n", unit, bp,
		       (bp->b_flags & B_READ) ? 'R' : 'W');
#endif
	part = rdpart(bp->b_dev);
	rs->sc_flags |= RDF_SEEK;
	rs->sc_ioc.c_unit = C_SUNIT(rs->sc_punit);
	rs->sc_ioc.c_volume = C_SVOL(0);
	rs->sc_ioc.c_saddr = C_SADDR;
	rs->sc_ioc.c_hiaddr = 0;
	rs->sc_ioc.c_addr = RDBTOS(bp->b_cylin);
	rs->sc_ioc.c_nop2 = C_NOP;
	rs->sc_ioc.c_slen = C_SLEN;
	rs->sc_ioc.c_len = rs->sc_resid;
	rs->sc_ioc.c_cmd = bp->b_flags & B_READ ? C_READ : C_WRITE;
#ifdef DEBUG
	if (rddebug & RDB_IO)
		printf("rdstart: hpibsend(%x, %x, %x, %x, %x)\n",
		       hp->hp_ctlr, hp->hp_slave, C_CMD,
		       &rs->sc_ioc.c_unit, sizeof(rs->sc_ioc)-2);
#endif
	if (hpibsend(hp->hp_ctlr, hp->hp_slave, C_CMD, &rs->sc_ioc.c_unit,
		     sizeof(rs->sc_ioc)-2) == sizeof(rs->sc_ioc)-2) {
		if (hp->hp_dk >= 0) {
			dk_busy |= 1 << hp->hp_dk;
			dk_seek[hp->hp_dk]++;
		}
#ifdef DEBUG
		if (rddebug & RDB_IO)
			printf("rdstart: hpibawait(%x)\n", hp->hp_ctlr);
#endif
		hpibawait(hp->hp_ctlr);
		return;
	}
	/*
	 * Experience has shown that the hpibwait in this hpibsend will
	 * occasionally timeout.  It appears to occur mostly on old 7914
	 * drives with full maintenance tracks.  We should probably
	 * integrate this with the backoff code in rderror.
	 */
#ifdef DEBUG
	if (rddebug & RDB_ERROR)
		printf("rd%d: rdstart: cmd %x adr %d blk %d len %d ecnt %d\n",
		       unit, rs->sc_ioc.c_cmd, rs->sc_ioc.c_addr,
		       bp->b_blkno, rs->sc_resid, rdtab[unit].b_errcnt);
	rdstats[unit].rdretries++;
#endif
	rs->sc_flags &= ~RDF_SEEK;
	rdreset(rs, hp);
	if (rdtab[unit].b_errcnt++ < RDRETRY)
		goto again;
	printf("rd%d: rdstart err: cmd 0x%x sect %d blk %d len %d\n",
	       unit, rs->sc_ioc.c_cmd, rs->sc_ioc.c_addr,
	       bp->b_blkno, rs->sc_resid);
	bp->b_flags |= B_ERROR;
	bp->b_error = EIO;
	bp = rdfinish(unit, rs, bp);
	if (bp) {
		rs->sc_addr = bp->b_un.b_addr;
		rs->sc_resid = bp->b_bcount;
		if (hpibreq(&rs->sc_dq))
			goto again;
	}
}

rdgo(unit)
	register int unit;
{
	register struct rd_softc *rs = &rd_softc[unit];
	register struct hp_device *hp = rs->sc_hd;
	struct buf *bp = rdtab[unit].b_actf;

	if (hp->hp_dk >= 0) {
		dk_busy |= 1 << hp->hp_dk;
		dk_xfer[hp->hp_dk]++;
		dk_wds[hp->hp_dk] += rs->sc_resid >> 6;
	}
#ifdef USELEDS
	if (inledcontrol == 0)
		ledcontrol(0, 0, LED_DISK);
#endif
	hpibgo(hp->hp_ctlr, hp->hp_slave, C_EXEC,
	       rs->sc_addr, rs->sc_resid, bp->b_flags & B_READ);
}

rdintr(unit)
	register int unit;
{
	register struct rd_softc *rs = &rd_softc[unit];
	register struct buf *bp = rdtab[unit].b_actf;
	register struct hp_device *hp = rs->sc_hd;
	u_char stat = 13;	/* in case hpibrecv fails */
	int rv, restart;
	
#ifdef DEBUG
	if (rddebug & RDB_FOLLOW)
		printf("rdintr(%d): bp %x, %c, flags %x\n", unit, bp,
		       (bp->b_flags & B_READ) ? 'R' : 'W', rs->sc_flags);
	if (bp == NULL) {
		printf("rd%d: bp == NULL\n", unit);
		return;
	}
#endif
	if (hp->hp_dk >= 0)
		dk_busy &= ~(1 << hp->hp_dk);
	if (rs->sc_flags & RDF_SEEK) {
		rs->sc_flags &= ~RDF_SEEK;
		if (hpibustart(hp->hp_ctlr))
			rdgo(unit);
		return;
	}
	if ((rs->sc_flags & RDF_SWAIT) == 0) {
#ifdef DEBUG
		rdstats[unit].rdpolltries++;
#endif
		if (hpibpptest(hp->hp_ctlr, hp->hp_slave) == 0) {
#ifdef DEBUG
			rdstats[unit].rdpollwaits++;
#endif
			if (hp->hp_dk >= 0)
				dk_busy |= 1 << hp->hp_dk;
			rs->sc_flags |= RDF_SWAIT;
			hpibawait(hp->hp_ctlr);
			return;
		}
	} else
		rs->sc_flags &= ~RDF_SWAIT;
	rv = hpibrecv(hp->hp_ctlr, hp->hp_slave, C_QSTAT, &stat, 1);
	if (rv != 1 || stat) {
#ifdef DEBUG
		if (rddebug & RDB_ERROR)
			printf("rdintr: recv failed or bad stat %d\n", stat);
#endif
		restart = rderror(unit);
#ifdef DEBUG
		rdstats[unit].rdretries++;
#endif
		if (rdtab[unit].b_errcnt++ < RDRETRY) {
			if (restart)
				rdstart(unit);
			return;
		}
		bp->b_flags |= B_ERROR;
		bp->b_error = EIO;
	}
	if (rdfinish(unit, rs, bp))
		rdustart(unit);
}

rdstatus(rs)
	register struct rd_softc *rs;
{
	register int c, s;
	u_char stat;
	int rv;

	c = rs->sc_hd->hp_ctlr;
	s = rs->sc_hd->hp_slave;
	rs->sc_rsc.c_unit = C_SUNIT(rs->sc_punit);
	rs->sc_rsc.c_sram = C_SRAM;
	rs->sc_rsc.c_ram = C_RAM;
	rs->sc_rsc.c_cmd = C_STATUS;
	bzero((caddr_t)&rs->sc_stat, sizeof(rs->sc_stat));
	rv = hpibsend(c, s, C_CMD, &rs->sc_rsc, sizeof(rs->sc_rsc));
	if (rv != sizeof(rs->sc_rsc)) {
#ifdef DEBUG
		if (rddebug & RDB_STATUS)
			printf("rdstatus: send C_CMD failed %d != %d\n",
			       rv, sizeof(rs->sc_rsc));
#endif
		return(1);
	}
	rv = hpibrecv(c, s, C_EXEC, &rs->sc_stat, sizeof(rs->sc_stat));
	if (rv != sizeof(rs->sc_stat)) {
#ifdef DEBUG
		if (rddebug & RDB_STATUS)
			printf("rdstatus: send C_EXEC failed %d != %d\n",
			       rv, sizeof(rs->sc_stat));
#endif
		return(1);
	}
	rv = hpibrecv(c, s, C_QSTAT, &stat, 1);
	if (rv != 1 || stat) {
#ifdef DEBUG
		if (rddebug & RDB_STATUS)
			printf("rdstatus: recv failed %d or bad stat %d\n",
			       rv, stat);
#endif
		return(1);
	}
	return(0);
}

/*
 * Deal with errors.
 * Returns 1 if request should be restarted,
 * 0 if we should just quietly give up.
 */
rderror(unit)
	int unit;
{
	struct rd_softc *rs = &rd_softc[unit];
	register struct rd_stat *sp;
	struct buf *bp;
	daddr_t hwbn, pbn;

	if (rdstatus(rs)) {
#ifdef DEBUG
		printf("rd%d: couldn't get status\n", unit);
#endif
		rdreset(rs, rs->sc_hd);
		return(1);
	}
	sp = &rs->sc_stat;
	if (sp->c_fef & FEF_REXMT)
		return(1);
	if (sp->c_fef & FEF_PF) {
		rdreset(rs, rs->sc_hd);
		return(1);
	}
	/*
	 * Unit requests release for internal maintenance.
	 * We just delay awhile and try again later.  Use expontially
	 * increasing backoff ala ethernet drivers since we don't really
	 * know how long the maintenance will take.  With RDWAITC and
	 * RDRETRY as defined, the range is 1 to 32 seconds.
	 */
	if (sp->c_fef & FEF_IMR) {
		extern int hz;
		int rdtimo = RDWAITC << rdtab[unit].b_errcnt;
#ifdef DEBUG
		printf("rd%d: internal maintenance, %d second timeout\n",
		       unit, rdtimo);
		rdstats[unit].rdtimeouts++;
#endif
		hpibfree(&rs->sc_dq);
		timeout(rdrestart, (void *)unit, rdtimo * hz);
		return(0);
	}
	/*
	 * Only report error if we have reached the error reporting
	 * threshhold.  By default, this will only report after the
	 * retry limit has been exceeded.
	 */
	if (rdtab[unit].b_errcnt < rderrthresh)
		return(1);

	/*
	 * First conjure up the block number at which the error occured.
	 * Note that not all errors report a block number, in that case
	 * we just use b_blkno.
 	 */
	bp = rdtab[unit].b_actf;
	pbn = rs->sc_info.ri_label.d_partitions[rdpart(bp->b_dev)].p_offset;
	if ((sp->c_fef & FEF_CU) || (sp->c_fef & FEF_DR) ||
	    (sp->c_ief & IEF_RRMASK)) {
		hwbn = RDBTOS(pbn + bp->b_blkno);
		pbn = bp->b_blkno;
	} else {
		hwbn = sp->c_blk;
		pbn = RDSTOB(hwbn) - pbn;
	}
	/*
	 * Now output a generic message suitable for badsect.
	 * Note that we don't use harderr cuz it just prints
	 * out b_blkno which is just the beginning block number
	 * of the transfer, not necessary where the error occured.
	 */
	printf("rd%d%c: hard error sn%d\n",
	       rdunit(bp->b_dev), 'a'+rdpart(bp->b_dev), pbn);
	/*
	 * Now report the status as returned by the hardware with
	 * attempt at interpretation (unless debugging).
	 */
	printf("rd%d %s error:",
	       unit, (bp->b_flags & B_READ) ? "read" : "write");
#ifdef DEBUG
	if (rddebug & RDB_ERROR) {
		/* status info */
		printf("\n    volume: %d, unit: %d\n",
		       (sp->c_vu>>4)&0xF, sp->c_vu&0xF);
		rdprinterr("reject", sp->c_ref, err_reject);
		rdprinterr("fault", sp->c_fef, err_fault);
		rdprinterr("access", sp->c_aef, err_access);
		rdprinterr("info", sp->c_ief, err_info);
		printf("    block: %d, P1-P10: ", hwbn);
		printf("%s", hexstr(*(u_int *)&sp->c_raw[0], 8));
		printf("%s", hexstr(*(u_int *)&sp->c_raw[4], 8));
		printf("%s\n", hexstr(*(u_short *)&sp->c_raw[8], 4));
		/* command */
		printf("    ioc: ");
		printf("%s", hexstr(*(u_int *)&rs->sc_ioc.c_pad, 8));
		printf("%s", hexstr(*(u_short *)&rs->sc_ioc.c_hiaddr, 4));
		printf("%s", hexstr(*(u_int *)&rs->sc_ioc.c_addr, 8));
		printf("%s", hexstr(*(u_short *)&rs->sc_ioc.c_nop2, 4));
		printf("%s", hexstr(*(u_int *)&rs->sc_ioc.c_len, 8));
		printf("%s\n", hexstr(*(u_short *)&rs->sc_ioc.c_cmd, 4));
		return(1);
	}
#endif
	printf(" v%d u%d, R0x%x F0x%x A0x%x I0x%x\n",
	       (sp->c_vu>>4)&0xF, sp->c_vu&0xF,
	       sp->c_ref, sp->c_fef, sp->c_aef, sp->c_ief);
	printf("P1-P10: ");
	printf("%s", hexstr(*(u_int *)&sp->c_raw[0], 8));
	printf("%s", hexstr(*(u_int *)&sp->c_raw[4], 8));
	printf("%s\n", hexstr(*(u_short *)&sp->c_raw[8], 4));
	return(1);
}

int
rdread(dev, uio, flags)
	dev_t dev;
	struct uio *uio;
	int flags;
{

	return (physio(rdstrategy, NULL, dev, B_READ, minphys, uio));
}

int
rdwrite(dev, uio, flags)
	dev_t dev;
	struct uio *uio;
	int flags;
{

	return (physio(rdstrategy, NULL, dev, B_WRITE, minphys, uio));
}

int
rdioctl(dev, cmd, data, flag, p)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
	struct proc *p;
{
	int unit = rdunit(dev);
	register struct rd_softc *sc = &rd_softc[unit];
	register struct disklabel *lp = &sc->sc_info.ri_label;
	int error, flags;

	switch (cmd) {
	case DIOCGDINFO:
		*(struct disklabel *)data = *lp;
		return (0);

	case DIOCGPART:
		((struct partinfo *)data)->disklab = lp;
		((struct partinfo *)data)->part =
			&lp->d_partitions[rdpart(dev)];
		return (0);

        case DIOCWLABEL:
                if ((flag & FWRITE) == 0)
                        return (EBADF);
		if (*(int *)data)
			sc->sc_flags |= RDF_WLABEL;
		else
			sc->sc_flags &= ~RDF_WLABEL;
		return (0);

        case DIOCSDINFO:
                if ((flag & FWRITE) == 0)
                        return (EBADF);
		return (setdisklabel(lp, (struct disklabel *)data,
				     (sc->sc_flags & RDF_WLABEL) ? 0
				     : sc->sc_info.ri_open));

        case DIOCWDINFO:
		if ((flag & FWRITE) == 0)
			return (EBADF);
		error = setdisklabel(lp, (struct disklabel *)data,
				     (sc->sc_flags & RDF_WLABEL) ? 0
				     : sc->sc_info.ri_open);
		if (error)
			return (error);
		flags = sc->sc_flags;
		sc->sc_flags = RDF_ALIVE | RDF_WLABEL;
		error = writedisklabel(rdlabdev(dev), rdstrategy, lp);
		sc->sc_flags = flags;
		return (error);
	}
	return(EINVAL);
}

int
rdsize(dev)
	dev_t dev;
{
	register int unit = rdunit(dev);
	register struct rd_softc *rs = &rd_softc[unit];
	int psize, didopen = 0;

	if (unit >= NRD || (rs->sc_flags & RDF_ALIVE) == 0)
		return(-1);

	/*
	 * We get called very early on (via swapconf)
	 * without the device being open so we may need
	 * to handle it here.
	 */
	if (rs->sc_info.ri_open == 0) {
		if (rdopen(dev, FREAD|FWRITE, S_IFBLK, NULL))
			return(-1);
		didopen = 1;
	}
	psize = rs->sc_info.ri_label.d_partitions[rdpart(dev)].p_size;
	if (didopen)
		(void) rdclose(dev, FREAD|FWRITE, S_IFBLK, NULL);
	return (psize);
}

#ifdef DEBUG
rdprinterr(str, err, tab)
	char *str;
	short err;
	char *tab[];
{
	register int i;
	int printed;

	if (err == 0)
		return;
	printf("    %s error field:", str, err);
	printed = 0;
	for (i = 0; i < 16; i++)
		if (err & (0x8000 >> i))
			printf("%s%s", printed++ ? " + " : " ", tab[i]);
	printf("\n");
}
#endif

/*
 * Non-interrupt driven, non-dma dump routine.
 */
int
rddump(dev)
	dev_t dev;
{
	int part = rdpart(dev);
	int unit = rdunit(dev);
	register struct rd_softc *rs = &rd_softc[unit];
	register struct hp_device *hp = rs->sc_hd;
	register struct partition *pinfo;
	register daddr_t baddr;
	register int maddr, pages, i;
	char stat;
	extern int lowram, dumpsize;
#ifdef DEBUG
	extern int pmapdebug;
	pmapdebug = 0;
#endif

	/* is drive ok? */
	if (unit >= NRD || (rs->sc_flags & RDF_ALIVE) == 0)
		return (ENXIO);
	pinfo = &rs->sc_info.ri_label.d_partitions[part];
	/* dump parameters in range? */
	if (dumplo < 0 || dumplo >= pinfo->p_size ||
	    pinfo->p_fstype != FS_SWAP)
		return (EINVAL);
	pages = dumpsize;
	if (dumplo + ctod(pages) > pinfo->p_size)
		pages = dtoc(pinfo->p_size - dumplo);
	maddr = lowram;
	baddr = dumplo + pinfo->p_offset;
	/* HPIB idle? */
	if (!hpibreq(&rs->sc_dq)) {
		hpibreset(hp->hp_ctlr);
		rdreset(rs, rs->sc_hd);
		printf("[ drive %d reset ] ", unit);
	}
	for (i = 0; i < pages; i++) {
#define NPGMB	(1024*1024/NBPG)
		/* print out how many Mbs we have dumped */
		if (i && (i % NPGMB) == 0)
			printf("%d ", i / NPGMB);
#undef NPBMG
		rs->sc_ioc.c_unit = C_SUNIT(rs->sc_punit);
		rs->sc_ioc.c_volume = C_SVOL(0);
		rs->sc_ioc.c_saddr = C_SADDR;
		rs->sc_ioc.c_hiaddr = 0;
		rs->sc_ioc.c_addr = RDBTOS(baddr);
		rs->sc_ioc.c_nop2 = C_NOP;
		rs->sc_ioc.c_slen = C_SLEN;
		rs->sc_ioc.c_len = NBPG;
		rs->sc_ioc.c_cmd = C_WRITE;
		hpibsend(hp->hp_ctlr, hp->hp_slave, C_CMD,
			 &rs->sc_ioc.c_unit, sizeof(rs->sc_ioc)-2);
		if (hpibswait(hp->hp_ctlr, hp->hp_slave))
			return (EIO);
		pmap_enter(kernel_pmap, (vm_offset_t)vmmap, maddr,
		    VM_PROT_READ, TRUE);
		hpibsend(hp->hp_ctlr, hp->hp_slave, C_EXEC, vmmap, NBPG);
		(void) hpibswait(hp->hp_ctlr, hp->hp_slave);
		hpibrecv(hp->hp_ctlr, hp->hp_slave, C_QSTAT, &stat, 1);
		if (stat)
			return (EIO);
		maddr += NBPG;
		baddr += ctod(1);
	}
	return (0);
}
#endif
