/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kbdmap.h	7.1 (Berkeley) %G%
 */

#define	ESC	'\033'
#define	DEL	'\177'

struct kbdmap {
	int	kbd_code;
	char	*kbd_desc;
	char	*kbd_keymap;
	char	*kbd_shiftmap;
	char	*kbd_ctrlmap;
	char	*kbd_ctrlshiftmap;
	char	**kbd_stringmap;
};

/* kbd_code */
#define KBD_SPECIAL	0x00		/* user defined */
#define KBD_US		0x1F		/* US ASCII */
#define KBD_UK		0x17		/* United Kingdom */

#define KBD_DEFAULT	KBD_US		/* default type */

#ifdef KERNEL
/* XXX: ITE interface */
char	*kbd_keymap;
char	*kbd_shiftmap;
char	*kbd_ctrlmap;
char	*kbd_ctrlshiftmap;
char	**kbd_stringmap;

extern struct kbdmap kbd_map[];
#endif
