/*-
 * Copyright (c) 1992 Keith Muller.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Keith Muller of the University of California, San Diego.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)options.h	1.1 (Berkeley) %G%
 */

/*
 * Constants used to specify the legal sets of flags in pax. For each major
 * operation mode of pax, a set of illegal flags is defined. If any one of
 * those illegal flags are found set, we scream and exit
 */
#define NONE	"none"

/*
 * flags (one for each option).
 */
#define	AF	0x00000001
#define	BF	0x00000002
#define	CF	0x00000004
#define	DF	0x00000008
#define	FF	0x00000010
#define	IF	0x00000020
#define	KF	0x00000040
#define	LF	0x00000080
#define	NF	0x00000100
#define	OF	0x00000200
#define	PF	0x00000400
#define	RF	0x00000800
#define	SF	0x00001000
#define	TF	0x00002000
#define	UF	0x00004000
#define	VF	0x00008000
#define	WF	0x00010000
#define	XF	0x00020000
#define	CBF	0x00040000	/* nonstandard extension */
#define	CEF	0x00080000	/* nonstandard extension */
#define	CGF	0x00100000	/* nonstandard extension */
#define	CHF	0x00200000	/* nonstandard extension */
#define	CLF	0x00400000
#define	CTF	0x00800000	/* nonstandard extension */
#define	CUF	0x01000000	/* nonstandard extension */
#define	CXF	0x02000000
#define	CZF	0x04000000	/* nonstandard extension */

/*
 * ascii string indexed by bit position above (alter the above and you must
 * alter this string) used to tell the user what flags caused us to complain
 */
#define FLGCH	"abcdfiklnoprstuvwxBEGHLTUXZ"

/*
 * legal pax operation bit patterns
 */

#define ISLIST(x)	(((x) & (RF|WF)) == 0)
#define	ISEXTRACT(x)	(((x) & (RF|WF)) == RF)
#define ISARCHIVE(x)	(((x) & (AF|RF|WF)) == WF)
#define ISAPPND(x)	(((x) & (AF|RF|WF)) == (AF|WF))
#define	ISCOPY(x)	(((x) & (RF|WF)) == (RF|WF))
#define	ISWRITE(x)	(((x) & (RF|WF)) == WF)

/*
 * Illegal option flag subsets based on pax operation
 */

#define	BDLIST	(AF|BF|IF|KF|LF|OF|PF|RF|TF|UF|WF|XF|CBF|CHF|CLF|CXF|CZF)
#define	BDEXTR	(AF|BF|LF|TF|WF|XF|CBF|CHF|CLF|CXF)
#define	BDARCH	(CF|KF|LF|NF|PF|RF|CEF|CZF)
#define	BDCOPY	(AF|BF|FF|OF|XF|CBF|CEF)
