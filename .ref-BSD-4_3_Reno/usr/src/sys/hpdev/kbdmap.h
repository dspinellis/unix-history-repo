/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)kbdmap.h	7.1 (Berkeley) 5/8/90
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
