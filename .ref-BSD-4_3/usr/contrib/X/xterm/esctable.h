#include <X/mit-copyright.h>

/* $Header: esctable.h,v 10.4 86/02/01 16:06:18 tony Rel $ */

#define	DECKPAM		1
#define	DECKPNM		2
#define	DECTC1		3
#define	DECSC		4
#define	DECAC1		5
#define	S7C1T		6
#define	S8C1T		7
#define	DECRC		8
#define	LS2		9
#define	LS3		10
#define	LS3R		11
#define	LS2R		12
#define	LS1R		13
#define	DESIGNATE	14
#define	DA1		15
#define	TBC		16
#define	SET		17
#define	DECSET		18
#define	RST		19
#define	DECRST		20
#define	SGR		21
#define	CPR		22
#define	DECSTBM		23
#define	ICH		24
#define	CUU		25
#define	CUD		26
#define	CUF		27
#define	CUB		28
#define	HVP		29
#define	CUP		30
#define	ED		31
#define	EL		32
#define	IL		33
#define	DL		34
#define	DCH		35
#define SCRINC		36
#define DECALN		37
#define	TEKESCFF	38
#define	TEKESCSUB	39
#define TEKESCINQ	40
#define	TEKCSIUS	41


#define	KEY(type, private, final)	((final<<16) | (private<<8) | type)
#define WILD		-1

/*
 * the following table of longwords is organized as:
 *    - seq. type, final, and any private introducer (formed with KEY macro)
 *    - intermediate(s) or WILD (match any intermediates)
 *    - dispatch value
 *    - number of default parameters (followed by the default values, if any)
 *
 * NOTE: since this table is searched linearly for a match, the entries
 *	 should really be in order of frequency of use (for performance)
 */
long	esctable[] = {
	KEY(ESC, 0,	'='),	0,	DECKPAM,	0,
	KEY(ESC, 0,	'>'),	0,	DECKPNM,	0,
	KEY(ESC, 0,	'6'),	' ',	DECTC1,		0,
	KEY(ESC, 0,	'7'),	0,	DECSC,		0,
	KEY(ESC, 0,	'7'),	' ',	DECAC1,		0,
	KEY(ESC, 0,	'F'),	' ',	S7C1T,		0,
	KEY(ESC, 0,	'G'),	' ',	S8C1T,		0,
	KEY(ESC, 0,	'8'),	0,	DECRC,		0,
	KEY(ESC, 0,	'8'),	'#',	DECALN,		0,
	KEY(ESC, 0,	'n'),	0,	LS2,		0,
	KEY(ESC, 0,	'o'),	0,	LS3,		0,
	KEY(ESC, 0,	'|'),	0,	LS3R,		0,
	KEY(ESC, 0,	'}'),	0,	LS2R,		0,
	KEY(ESC, 0,	'~'),	0,	LS1R,		0,
	KEY(ESC, 0,	'B'),	WILD,	DESIGNATE,	0,
	KEY(ESC, 0,	'A'),	WILD,	DESIGNATE,	0,
	KEY(ESC, 0,	'0'),	WILD,	DESIGNATE,	0,
	KEY(ESC, 0,	'1'),	WILD,	DESIGNATE,	0,
	KEY(ESC, 0,	'2'),	WILD,	DESIGNATE,	0,
	KEY(ESC, 0,	'<'),	WILD,	DESIGNATE,	0,
	KEY(CSI, 0,	'c'),	0,	DA1,		1, 0,
	KEY(CSI, 0,	'g'),	0,	TBC,		1, 0,
	KEY(CSI, 0,	'h'),	0,	SET,		0,
	KEY(CSI, '?',	'h'),	0,	DECSET,		0,
	KEY(CSI, 0,	'l'),	0,	RST,		0,
	KEY(CSI, '?',	'l'),	0,	DECRST,		0,
	KEY(CSI, 0,	'm'),	0,	SGR,		1, 0,
	KEY(CSI, 0,	'n'),	0,	CPR,		1, 0,
	KEY(CSI, 0,	'r'),	0,	DECSTBM,	2, 1, 255,	
	KEY(CSI, 0,	't'),	0,	SCRINC,		1, 0,
	KEY(CSI, 0,	'@'),	0,	ICH,		1, 1,
	KEY(CSI, 0,	'A'),	0,	CUU,		1, 1,
	KEY(CSI, 0,	'B'),	0,	CUD,		1, 1,
	KEY(CSI, 0,	'C'),	0,	CUF,		1, 1,
	KEY(CSI, 0,	'D'),	0,	CUB,		1, 1,
	KEY(CSI, 0,	'f'),	0,	HVP,		2, 1, 1,
	KEY(CSI, 0,	'H'),	0,	CUP,		2, 1, 1,
	KEY(CSI, 0,	'J'),	0,	ED,		1, 0,
	KEY(CSI, 0,	'K'),	0,	EL,		1, 0,
	KEY(CSI, 0,	'L'),	0,	IL,		1, 1,
	KEY(CSI, 0,	'M'),	0,	DL,		1, 1,
	KEY(CSI, 0,	'P'),	0,	DCH,		1, 1,
	KEY(ESC, 0,	FF),	0,	TEKESCFF,	0,
	KEY(ESC, 0,	SUB),	0,	TEKESCSUB,	0,
	KEY(ESC, 0,	INQ),	0,	TEKESCINQ,	0,
	KEY(CSI, 0,	US),	0,	TEKCSIUS,	0,
	0
};
