/*
 * Copyright (c) 1985, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dmzreg.h	7.2 (Berkeley) %G%
 */

/*
 * Registers for DMZ-32.
 * Defined in terms of dmx_octet common registers.
 */
struct dmzdevice {
	short	dmz_config;			/* configuration csr */
	short	dmz_diag;			/* diagnostic csr */
	struct	dmx_octet dmz_octet[3];		/* per-octet registers */
};


#define	DMZ(a)		(a/24)
#define	OCTET(a)	((a%24)/8)
#define	LINE(a)		((a%24)%8)

#define	DMZ_NOC_MASK	03
#define	DMZ_INTERFACE	000
