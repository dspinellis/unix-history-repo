/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kbdreg.h	7.1 (Berkeley) %G%
 */

/*
 * kbdreg.h --
 *
 */

struct kbd_keymap {
	int	km_type;
	int	km_code[2];
};

#define KC_CHAR		0x000000FF
#define KC_TYPE		0x0000FF00
#define	KC_CODE		0x00000000
#define	KC_SHIFT	0x00000100
#define	KC_IGNORE	0x0000FF00

#define KS_SHIFT	0
#define KS_CTRL		1
#define KS_META		2
