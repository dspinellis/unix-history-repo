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
 *	California, Lawrence Berkeley Laboratories.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)sun_disklabel.c	7.3 (Berkeley) %G%
 *
 * from: $Header: sun_disklabel.c,v 1.5 92/06/17 07:04:12 torek Exp $
 */

/*
 * SunOS disk label code.
 */

#include <sys/param.h>
#include <sys/disklabel.h>
#include <sys/device.h>
#include <sys/disk.h>
#include <sys/ioctl.h>

#include <sparc/sunos/sun_disklabel.h>

/*
 * Take a sector (cp) containing a SunOS disk label and set lp to a BSD
 * disk label.
 */
int
sun_disklabel(cp, lp)
	register caddr_t cp;
	register struct disklabel *lp;
{
	register u_short *sp;
	register struct sun_disklabel *sl;
	register int i, v;

	sp = (u_short *)(cp + sizeof(struct sun_disklabel));
	--sp;
	v = 0;
	while (sp >= (u_short *)cp)
		v ^= *sp--;
	if (v)
		return (0);
	sl = (struct sun_disklabel *)cp;
	lp->d_magic = 0;	/* denote as pseudo */
	lp->d_ncylinders = sl->sl_ncylinders;
	lp->d_acylinders = sl->sl_acylinders;
	v = (lp->d_ntracks = sl->sl_ntracks) *
	    (lp->d_nsectors = sl->sl_nsectors);
	lp->d_secpercyl = v;
	lp->d_npartitions = 8;
	for (i = 0; i < 8; i++) {
		lp->d_partitions[i].p_offset =
		    sl->sl_part[i].sdkp_cyloffset * v;
		lp->d_partitions[i].p_size = sl->sl_part[i].sdkp_nsectors;
	}
	return (1);
}

int
sun_dkioctl(dk, cmd, data, partition)
	struct dkdevice *dk;
	int cmd;
	caddr_t data;
	int partition;
{
	register struct partition *p;

	switch (cmd) {

	case DKIOCGGEOM:
#define geom ((struct sun_dkgeom *)data)
		bzero(data, sizeof(*geom));
		geom->sdkc_ncylinders = dk->dk_label.d_ncylinders;
		geom->sdkc_acylinders = dk->dk_label.d_acylinders;
		geom->sdkc_ntracks = dk->dk_label.d_ntracks;
		geom->sdkc_nsectors = dk->dk_label.d_nsectors;
		geom->sdkc_interleave = dk->dk_label.d_interleave;
		geom->sdkc_sparespercyl = dk->dk_label.d_sparespercyl;
		geom->sdkc_rpm = dk->dk_label.d_rpm;
		geom->sdkc_pcylinders =
		    dk->dk_label.d_ncylinders + dk->dk_label.d_acylinders;
#undef	geom
		break;

	case DKIOCINFO:
		/* Homey don't do DKIOCINFO */
		bzero(data, sizeof(struct sun_dkctlr));
		break;

	case DKIOCGPART:
		if (dk->dk_label.d_secpercyl == 0)
			return (ERANGE);	/* XXX */
		p = &dk->dk_label.d_partitions[partition];
		if (p->p_offset % dk->dk_label.d_secpercyl != 0)
			return (ERANGE);	/* XXX */
#define part ((struct sun_dkpart *)data)
		part->sdkp_cyloffset = p->p_offset / dk->dk_label.d_secpercyl;
		part->sdkp_nsectors = p->p_size;
#undef	part
		break;

	default:
		return (-1);
	}
	return (0);
}
