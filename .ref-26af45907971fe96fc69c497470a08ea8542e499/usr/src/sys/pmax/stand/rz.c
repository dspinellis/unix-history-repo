/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Van Jacobson of Lawrence Berkeley Laboratory and Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)rz.c	7.2 (Berkeley) %G%
 */

#include <stand/stand.h>
#include <sys/param.h>
#include <sys/disklabel.h>

struct	rz_softc {
	int	sc_fd;			/* PROM file id */
	int	sc_ctlr;		/* controller number */
	int	sc_unit;		/* disk unit number */
	int	sc_part;		/* disk partition number */
	struct	disklabel sc_label;	/* disk label for this disk */
};

int
rzstrategy(devdata, rw, bn, reqcnt, addr, cnt)
	void *devdata;
	int rw;
	daddr_t bn;
	u_int reqcnt;
	char *addr;
	u_int *cnt;	/* out: number of bytes transfered */
{
	register struct rz_softc *sc = (struct rz_softc *)devdata;
	register int part = sc->sc_part;
	register struct partition *pp = &sc->sc_label.d_partitions[part];
	register int s;
	long offset;

	offset = bn * DEV_BSIZE;
	/*
	 * Partial-block transfers not handled.
	 */
	if (reqcnt & (DEV_BSIZE - 1)) {
		*cnt = 0;
		return (EINVAL);
	}

	offset += pp->p_offset * DEV_BSIZE;
	if (prom_lseek(sc->sc_fd, offset, 0) < 0)
		return (EIO);
	s = prom_read(sc->sc_fd, addr, reqcnt);
	if (s < 0)
		return (EIO);

	*cnt = s;
	return (0);
}

int
rzopen(f, ctlr, unit, part)
	struct open_file *f;
	int ctlr, unit, part;
{
	register struct rz_softc *sc;
	register struct disklabel *lp;
	register int i;
	char *msg;
	char buf[DEV_BSIZE];
	int cnt;
	static char device[] = "rz(0,0,0)";

	if (unit >= 8 || part >= 8)
		return (ENXIO);
	device[5] = '0' + unit;
	/* NOTE: only support reads for now */
	if ((i = prom_open(device, 0)) < 0)
		return (ENXIO);

	sc = alloc(sizeof(struct rz_softc));
	bzero(sc, sizeof(struct rz_softc));
	f->f_devdata = (void *)sc;

	sc->sc_fd = i;
	sc->sc_ctlr = ctlr;
	sc->sc_unit = unit;
	sc->sc_part = part;

	/* try to read disk label and partition table information */
	lp = &sc->sc_label;
	lp->d_secsize = DEV_BSIZE;
	lp->d_secpercyl = 1;
	lp->d_npartitions = MAXPARTITIONS;
	lp->d_partitions[part].p_offset = 0;
	lp->d_partitions[part].p_size = 0x7fffffff;
	i = rzstrategy(sc, F_READ, (daddr_t)LABELSECTOR, DEV_BSIZE, buf, &cnt);
	if (i || cnt != DEV_BSIZE) {
		printf("rz%d: error reading disk label\n", unit);
		goto bad;
	} else {
		msg = getdisklabel(buf, lp);
		if (msg) {
			printf("rz%d: %s\n", unit, msg);
			goto bad;
		}
	}

	if (part >= lp->d_npartitions || lp->d_partitions[part].p_size == 0) {
	bad:
		free(sc, sizeof(struct rz_softc));
		return (ENXIO);
	}
	return (0);
}

rzclose(f)
	struct open_file *f;
{
	free(f->f_devdata, sizeof(struct rz_softc));
	f->f_devdata = (void *)0;
	return (0);
}
