/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: kbreg.h,v 4.300 91/06/09 06:42:47 root Rel41 $ SONY
 *
 *	@(#)kbreg.h	7.3 (Berkeley) %G%
 */

#ifndef __KEYBOARD__
#define __KEYBOARD__ 1

#define	key_any(x)	((x) != -1)
#define	key_down(x)	(((x) & 0x80) == 0)
#define	key_up(x)	((x) & 0x80)

typedef	struct key_string {
	int	key_length;
	char	*key_string;
} Key_string;

typedef struct pfk_table {
	int		pfk_addr;
	Key_string	pfk_normal;
	Key_string	pfk_shift;
} Pfk_table;

typedef	struct pfk_string {
	int		pfk_num;
	int		pfk_shift;
	Key_string	pfk_string;
} Pfk_string;

#define	KBDPRI		28

#define	KIOCSETS	0		/* set pfk string	*/
#define	KIOCBELL	1		/* ring bell		*/
#define	KIOCBACK	2		/* push back string	*/
#define	KIOCREPT	3		/* auto repeat on	*/
#define	KIOCNRPT	4		/* auto repeat off	*/
#define	KIOCGETS	5		/* get pfk string	*/
#define	KIOCRAW		6		/* get raw key data	*/
#define	KIOCSETE	7
#define	KIOCFLUSH	8
#define	KIOCNREAD	FIONREAD	/* return input char's	*/
#define	KIOCSETLOCK	10		/* set lock type	*/
#define	KIOCSETTBL	11		/* set key_table	*/
#define	KIOCGETCNUM	12		/* get country number	*/
#define	KIOCSETCNUM	13		/* set country number	*/
#define	KIOCDEFTBL	14		/* default key_table 	*/
#define	KIOCCHTBL	15		/* change key_table 	*/
#define	KIOCOYATBL	16		/* oya_key_table	*/
#define	KIOCGETSTAT	17		/* get keyboard status	*/
#define	KIOCSETSTAT	18		/* set keyboard status	*/

#define	OFF		0x80

extern int N_Pfk;
#define	N_PFK		N_Pfk

#define	N_KEY		93

/*
 * Programmable function key
 */
#define	F1		1
#define	F2		2
#define	F3		3
#define	F4		4
#define	F5		5
#define	F6		6
#define	F7		7
#define	F8		8
#define	F9		9
#define	F10		10

#define	PF1		11
#define	PF2		12
#define	PF3		13
#define	PF4		14
#define	PF5		15
#define	PF6		16
#define	PF7		17
#define	PF8		18
#define	PF9		19
#define	PF10		20
#define	PF11		21
#define	PF12		22

#define	NCNV		23
#define	CONV		24
#define	ENTER		25

#define	N0		26
#define	N1		27
#define	N2		28
#define	N3		29
#define	N4		30
#define	N5		31
#define	N6		32
#define	N7		33
#define	N8		34
#define	N9		35

#define	PERIOD		36
#define	MINUS		37
#define	PLUS		38
#define	COMMA		39
#define	NENTER		40
#define	UP		41
#define	DOWN		42
#define	RIGHT		43
#define	LEFT		44

#endif /* !__KEYBOARD__ */
