/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
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
 * from: Utah $Hdr: hil.c 1.1 89/08/22$
 *
 *	@(#)hil.c	7.1 (Berkeley) 5/8/90
 */

/*
 * Keyboard routines for the standalone ITE.
 */
#include "samachdep.h"

#ifdef ITECONSOLE

#include "param.h"
#include "../hpdev/hilreg.h"
#include "../hpdev/kbdmap.h"
#include "../hpdev/itevar.h"

#ifndef SMALL

/*
 * HIL cooked keyboard keymaps.
 * Supports only unshifted, shifted and control keys.
 */
char	us_keymap[] = {
	NULL,	'`',	'\\',	ESC,	NULL,	DEL,	NULL,	NULL,  
	'\n',	'\t',	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,  
	NULL,	'\n',	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,  
	NULL,	'\t',	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	'\b',	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	ESC,	'\r',	NULL,	'\n',	'0',	'.',	',',	'+',
	'1',	'2',	'3',	'-',	'4',	'5',	'6',	'*',
	'7',	'8',	'9',	'/',	'E',	'(',	')',	'^',
	'1',	'2',	'3',	'4',	'5',	'6',	'7',	'8',
	'9',	'0',	'-',	'=',	'[',	']',	';',	'\'',
	',',	'.',	'/',	'\040',	'o',	'p',	'k',	'l',
	'q',	'w',	'e',	'r',	't',	'y',	'u',	'i',
	'a',	's',	'd',	'f',	'g',	'h',	'j',	'm',
	'z',	'x',	'c',	'v',	'b',	'n',	NULL,	NULL
};

char	us_shiftmap[] = {
	NULL,	'~',	'|',	DEL,	NULL,	DEL,	NULL,	NULL,
	'\n',	'\t',	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	'\n',	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	'\t',	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	DEL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	ESC,	'\r',	NULL,	'\n',	'0',	'.',	',',	'+',
	'1',	'2',	'3',	'-',	'4',	'5',	'6',	'*',
	'7',	'8',	'9',	'/',	'`',	'|',	'\\',	'~',
	'!',	'@',	'#',	'$',	'%',	'^',	'&',	'*',
	'(',	')',	'_',	'+',	'{',	'}',	':',	'\"',
	'<',	'>',	'?',	'\040',	'O',	'P',	'K',	'L',
	'Q',	'W',	'E',	'R',	'T',	'Y',	'U',	'I',
	'A',	'S',	'D',	'F',	'G',	'H',	'J',	'M',
	'Z',	'X',	'C',	'V',	'B',	'N',	NULL,	NULL
};

char	us_ctrlmap[] = {
	NULL,	'`',	'\034',	ESC,	NULL,	DEL,	NULL,	NULL,
	'\n',	'\t',	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	'\n',	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	'\t',	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	'\b',	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	ESC,	'\r',	NULL,	'\n',	'0',	'.',	',',	'+',
	'1',	'2',	'3',	'-',	'4',	'5',	'6',	'*',
	'7',	'8',	'9',	'/',	'E',	'(',	')',	'\036',
	'1',	'2',	'3',	'4',	'5',	'6',	'7',	'8',
	'9',	'0',	'-',	'=',	'\033',	'\035',	';',	'\'',
	',',	'.',	'/',	'\040',	'\017',	'\020',	'\013',	'\014',
	'\021',	'\027',	'\005',	'\022',	'\024',	'\031',	'\025',	'\011',
	'\001',	'\023',	'\004',	'\006',	'\007',	'\010',	'\012',	'\015',
	'\032',	'\030',	'\003',	'\026',	'\002',	'\016',	NULL,	NULL
};

#ifdef UK_KEYBOARD
char	uk_keymap[] = {
	NULL,	'`',	'<',	ESC,	NULL,	DEL,	NULL,	NULL,  
	'\n',	'\t',	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,  
	NULL,	'\n',	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,  
	NULL,	'\t',	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	'\b',	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	ESC,	'\r',	NULL,	'\n',	'0',	'.',	',',	'+',
	'1',	'2',	'3',	'-',	'4',	'5',	'6',	'*',
	'7',	'8',	'9',	'/',	'E',	'(',	')',	'^',
	'1',	'2',	'3',	'4',	'5',	'6',	'7',	'8',
	'9',	'0',	'+',	'\'',	'[',	']',	'*',	'\\',
	',',	'.',	'-',	'\040',	'o',	'p',	'k',	'l',
	'q',	'w',	'e',	'r',	't',	'y',	'u',	'i',
	'a',	's',	'd',	'f',	'g',	'h',	'j',	'm',
	'z',	'x',	'c',	'v',	'b',	'n',	NULL,	NULL
};

char	uk_shiftmap[] = {
	NULL,	'~',	'>',	DEL,	NULL,	DEL,	NULL,	NULL,
	'\n',	'\t',	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	'\n',	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	'\t',	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	DEL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	ESC,	'\r',	NULL,	'\n',	'0',	'.',	',',	'+',
	'1',	'2',	'3',	'-',	'4',	'5',	'6',	'*',
	'7',	'8',	'9',	'/',	'`',	'|',	'\\',	'~',
	'!',	'\"',	'#',	'$',	'%',	'&',	'^',	'(',
	')',	'=',	'?',	'/',	'{',	'}',	'@',	'|',
	';',	':',	'_',	'\040',	'O',	'P',	'K',	'L',
	'Q',	'W',	'E',	'R',	'T',	'Y',	'U',	'I',
	'A',	'S',	'D',	'F',	'G',	'H',	'J',	'M',
	'Z',	'X',	'C',	'V',	'B',	'N',	NULL,	NULL
};

char	uk_ctrlmap[] = {
	NULL,	'`',	'<',	ESC,	NULL,	DEL,	NULL,	NULL,
	'\n',	'\t',	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	'\n',	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	'\t',	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	'\b',	NULL,
	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,	NULL,
	ESC,	'\r',	NULL,	'\n',	'0',	'.',	',',	'+',
	'1',	'2',	'3',	'-',	'4',	'5',	'6',	'*',
	'7',	'8',	'9',	'/',	'E',	'(',	')',	'\036',
	'1',	'2',	'3',	'4',	'5',	'6',	'7',	'8',
	'9',	'0',	'+',	'\'',	'\033',	'\035',	'*',	'\034',
	',',	'.',	'/',	'\040',	'\017',	'\020',	'\013',	'\014',
	'\021',	'\027',	'\005',	'\022',	'\024',	'\031',	'\025',	'\011',
	'\001',	'\023',	'\004',	'\006',	'\007',	'\010',	'\012',	'\015',
	'\032',	'\030',	'\003',	'\026',	'\002',	'\016',	NULL,	NULL
};
#endif

/*
 * The keyboard map table.
 * Lookup is by hardware returned language code.
 */
struct kbdmap kbd_map[] = {
	KBD_US,		NULL,
	us_keymap,	us_shiftmap,	us_ctrlmap,	NULL,	NULL,

#ifdef UK_KEYBOARD
	KBD_UK,		NULL,
	uk_keymap,	uk_shiftmap,	uk_ctrlmap,	NULL,	NULL,
#endif

	0,		NULL,
	NULL,		NULL,		NULL,		NULL,	NULL,
};

char	*kbd_keymap = us_keymap;
char	*kbd_shiftmap = us_shiftmap;
char	*kbd_ctrlmap = us_ctrlmap;

kbdgetc()
{
	register int status, c;
	register struct hil_dev *hiladdr = HILADDR;

	status = hiladdr->hil_stat;
	if ((status & HIL_DATA_RDY) == 0)
		return(0);
	c = hiladdr->hil_data;
	switch ((status>>KBD_SSHIFT) & KBD_SMASK) {
	case KBD_SHIFT:
		c = kbd_shiftmap[c & KBD_CHARMASK];
		break;
	case KBD_CTRL:
		c = kbd_ctrlmap[c & KBD_CHARMASK];
		break;
	case KBD_KEY:
		c = kbd_keymap[c & KBD_CHARMASK];
		break;
	default:
		c = 0;
		break;
	}
	return(c);
}
#endif

kbdnmi()
{
	register struct hil_dev *hiladdr = HILADDR;

	HILWAIT(hiladdr);
	hiladdr->hil_cmd = HIL_CNMT;
	HILWAIT(hiladdr);
	hiladdr->hil_cmd = HIL_CNMT;
	HILWAIT(hiladdr);
	printf("\nboot interrupted\n");
}

kbdinit()
{
	register struct hil_dev *hiladdr = HILADDR;
	register struct kbdmap *km;
	u_char lang;

	HILWAIT(hiladdr);
	hiladdr->hil_cmd = HIL_SETARR;
	HILWAIT(hiladdr);
	hiladdr->hil_data = ar_format(KBD_ARR);
	HILWAIT(hiladdr);
	hiladdr->hil_cmd = HIL_READKBDLANG;
	HILDATAWAIT(hiladdr);
	lang = hiladdr->hil_data;
	for (km = kbd_map; km->kbd_code; km++)
		if (km->kbd_code == lang) {
			kbd_keymap = km->kbd_keymap;
			kbd_shiftmap = km->kbd_shiftmap;
			kbd_ctrlmap = km->kbd_ctrlmap;
		}
	HILWAIT(hiladdr);
	hiladdr->hil_cmd = HIL_INTON;
}
#endif
