/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)sbusvar.h	7.1 (Berkeley) %G%
 *
 * from: $Header: sbusvar.h,v 1.5 92/06/17 06:59:44 torek Exp $ (LBL)
 */

/*
 * S-bus variables.
 */
struct sbusdev {
	struct	device *sd_dev;		/* backpointer to generic */
	struct	sbusdev *sd_bchain;	/* forward link in bus chain */
	void	(*sd_reset) __P((struct device *));
};

/*
 * Sbus driver attach arguments.
 */
struct sbus_attach_args {
	struct	romaux sa_ra;		/* name, node, addr, etc */
	int	sa_slot;		/* Sbus slot number */
	int	sa_offset;		/* offset within slot */
};

/* variables per Sbus */
struct sbus_softc {
	struct	device sc_dev;		/* base device */
	int	sc_clockfreq;		/* clock frequency (in Hz) */
	struct	sbusdev *sc_sbdev;	/* list of all children */
};

int	sbusdev_match __P((struct cfdata *, void *));
void	sbus_establish __P((struct sbusdev *, struct device *));
