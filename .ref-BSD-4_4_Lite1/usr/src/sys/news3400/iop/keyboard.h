/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * from: $Hdr: keyboard.h,v 4.300 91/06/09 06:42:48 root Rel41 $ SONY
 *
 *	@(#)keyboard.h	8.1 (Berkeley) 6/11/93
 */

/*
 *	Key flag
 */
#define	NORMAL		0x003f
#define		N	0x0001
#define		S	0x0002
#define		C	0x0004
#define		A	0x0008
#define		K	0x0010
#define		G	0x0010
#define		J	0x0020
#define		L	0x0040
#define		R	0x0080
#define		O	(L|R)

#define	PRG_FUNC	0x0100
#define	ALT_FUNC	0x0200
#define	CAP_LOCK	0x0400

#define	PSH_SHFT	0x0800
#define	SW_SHFT		0x1000
#define	KEY_PRESS	0x4000
#define	NOT_REPT	0x8000

#define		S_CTRL	0
#define		S_LSHFT	1
#define		S_RSHFT	2
#define		S_ALT	3
#define		S_CAPS	4
#define		S_AN	5
#define		S_KANA	6
#define		S_OCAPS	7
#define		S_OKANA	8
#define		S_ALTGR	9

/*
 *	Keyboard status
 */
#define	KBD_SHIFT	0x0003		/* shift mode		*/
#define	KBD_RSHIFT	0x0001
#define	KBD_LSHIFT	0x0002
#define	KBD_CTRL	0x0004		/* control mode		*/
#define	KBD_ALT		0x0008		/* alternate mode	*/
#define	KBD_CAPS	0x0010		/* capital lock mode	*/
#define	KBD_KANA	0x0020		/* kana mode		*/
#define	KBD_NOTREPT	0x0080		/* do not auto repeat	*/
#define	KBD_ALTGR	0x0800		/* European keyboard AltGr mode	*/

/*
 *	Lock type
 */
#define	CAPSLOCK	1
#define	SHIFTLOCK	2
#define	SHIFTLOCK2	3

/*
 *	Country number
 */
#define	K_JAPANESE_J		0			/* JIS */
#define	K_JAPANESE_O		1			/* OYAYUBI */
#define	K_GERMAN		2
#define	K_FRENCH		3
#define	K_UKENGLISH		4
#define	K_ITALIAN		5
#define	K_SPANISH		6
#define	K_SWEDISH_FINNISH	7
#define	K_DANISH		8
#define	K_NORWEGIAN		9
#define	K_PORTUGUESE		10
#define	K_USENGLISH		11

/*
 *	Key shift code
 */
#define	PF_NORMAL	0
#define	PF_SHIFT	1
#define	PF_CONTROL	2
#define	PF_ALTERNATE	3

typedef	struct key_table {
	short		key_flags;
	char		normal_code;
	char		shift_code;
	char		ctrl_code;
	char		alt_code;
	char		kana_code;
	char		kshft_code;
} Key_table;

typedef struct {
	int		key_number;
	Key_table	key_num_table;
} Key_tab_info;

/*
 *	IOCTL command
 */
#define	KBIOCBELL	_IOW('K', 1, int)		/* ring bell	*/
#define	KBIOCREPT	_IO('K', 3)			/* auto repeat on */
#define	KBIOCNRPT	_IO('K', 4)			/* auto repeat off */
#define	KBIOCSETLOCK	_IOW('K', 10, int)		/* set lock type */
#define	KBIOCSETTBL	_IOW('K', 11, Key_tab_info)	/* set key_table */
#define	KBIOCGETCNUM	_IOR('K', 12, int)		/* get country number */
#define	KBIOCSETCNUM	_IOW('K', 13, int)		/* set country number */
#define	KBIOCGETSTAT	_IOR('K', 17, int)		/* get status */
#define	KBIOCSETSTAT	_IOW('K', 18, int)		/* set status */
