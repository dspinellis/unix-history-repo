/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)sbusvar.h	7.3 (Berkeley) %G%
 *
 * from: $Header: sbusvar.h,v 1.6 92/11/26 02:28:14 torek Exp $ (LBL)
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
