#ifndef lint
static char sccsid[] = "@(#)config.c	1.5 (Berkeley/CCI) %G%";
#endif

/*
 * Drive configuration information.
 */
#include "vdfmt.h"

struct	flawpat defpats = {
	0x0264c993, 0x04c99326, 0x0993264c, 0x13264c98,
	0x264c9930, 0x4c993260, 0x993264c0, 0x3264c980,
	0x64c99300, 0xc9932600, 0x93264c00, 0x264c9800,
	0x4c993000, 0x99326000, 0x3264c000, 0x54c98000
};
struct	flawpat cdcpats = {
	0x0d9b366c, 0x1b366cd8, 0x366cd9b0, 0x6cd9b360,
	0xd9b366c0, 0xb366cd80, 0x66cd9b00, 0xcd9b3600,
	0x9b366300, 0x366cd800, 0x6cd9b000, 0xd9b36000,
	0xb366c000, 0x66cd8000, 0xcd9b0000, 0x9b360000
};

struct	disklabel vdproto[] = {
	{ DISKMAGIC, DTYPE_SMD, 0, "xsd", "newly formatted",
		512, 48, 24, 711, 48*24, 48*24*711,
		0, 0, 0, 3600, 1, 0, 0, 900, 0, 0,
		0, 30240, (long)&defpats, 0, 0, 0, 0, 0, 0, 0,
		DISKMAGIC, 0, 1, BBSIZE, SBSIZE,
		{ 48*24*711, 0 }
	},
	{ DISKMAGIC, DTYPE_SMD, 0, "ncc", "newly formatted",
		512, 66, 23, 850, 66*23, 66*23*850,
		0, 0, 0, 3600, 1, 0, 0, 900, 0, 0,
		0, 40960, (long)&defpats, 0, 0, 0, 0, 0, 0, 0,
		DISKMAGIC, 0, 1, BBSIZE, SBSIZE,
		{ 66*23*850, 0 }
	},
	{ DISKMAGIC, DTYPE_SMD, 0, "2361a", "newly formatted",
		512, 64, 20, 842, 64*20, 64*20*842,
		0, 0, 0, 3600, 1, 0, 0, 900, 0, 0,
		0, 40960, (long)&defpats, 0, 0, 0, 0, 0, 0, 0,
		DISKMAGIC, 0, 1, BBSIZE, SBSIZE,
		{ 64*20*842, 0 }
	},
	{ DISKMAGIC, DTYPE_SMD, 0, "egl", "newly formatted",
		512, 44, 20, 842, 44*20, 44*20*842,
		0, 0, 0, 3961, 1, 0, 0, 900, 0, 0,
		0, 28160, (long)&defpats, 0, 0, 0, 0, 0, 0, 0,
		DISKMAGIC, 0, 1, BBSIZE, SBSIZE,
		{ 44*20*842, 0 }
	},
	{ DISKMAGIC, DTYPE_SMD, 0, "fuj", "newly formatted",
		512, 64, 10, 823, 64*10, 64*10*823,
		0, 0, 0, 3600, 1, 0, 0, 900, 0, 0,
		0, 40960, (long)&defpats, 0, 0, 0, 0, 0, 0, 0,
		DISKMAGIC, 0, 1, BBSIZE, SBSIZE,
		{ 64*10*823, 0 }
	},
	{ DISKMAGIC, DTYPE_SMD, 0, "xfd", "newly formatted",
		512, 32, 24, 711, 32*24, 32*24*711,
		0, 0, 0, 3600, 1, 0, 0, 900, 0, 0,
		0, 20160, (long)&cdcpats, 0, 0, 0, 0, 0, 0, 0,
		DISKMAGIC, 0, 1, BBSIZE, SBSIZE,
		{ 32*24*711, 0 }
	},

	{ DISKMAGIC, DTYPE_SMD, 0, "smd", "newly formatted",
		512, 32, 19, 823, 32*19, 32*19*823,
		0, 0, 0, 3600, 1, 0, 0, 900, 0, 0,
		0, 20160, (long)&cdcpats, 0, 0, 0, 0, 0, 0, 0,
		DISKMAGIC, 0, 1, BBSIZE, SBSIZE,
		{ 32*19*823, 0 }
	},
	{ DISKMAGIC, DTYPE_ESDI, 0, "mxd", "newly formatted",
		1024, 18, 15, 1224, 18*15, 18*15*1224,
		0, 0, 0, 3600, 1, 0, 0, 900, 0, 0,
		VD_ESDI, 20160, (long)&defpats, 0, 0, 0, 0, 0, 0, 0,
		DISKMAGIC, 0, 1, BBSIZE, SBSIZE,
		{ 18*15*1224, 0 }
	},
	{ DISKMAGIC, DTYPE_SMD, 0, "fsd", "newly formatted",
		512, 32, 10, 823, 32*10, 32*10*823,
		0, 0, 0, 3600, 1, 0, 0, 900, 0, 0,
		0, 20160, (long)&cdcpats, 0, 0, 0, 0, 0, 0, 0,
		DISKMAGIC, 0, 1, BBSIZE, SBSIZE,
		{ 32*10*823, 0 }
	},
};

int	ndrives = sizeof (vdproto) / sizeof (vdproto[0]);
int	smddrives = 5;			/* first 5 types are smd-e only */
