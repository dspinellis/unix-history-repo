/*
 * Copyright (c) 1990 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 *      @(#)stvar.h	7.1 (Berkeley) %G%
 */

/*
 * stvar.h scsi tape driver
 */

struct exb_xsense {
	u_char  reserved8;
	u_char  reserved9;
	u_char  reserved10;
	u_char  reserved11;
	u_char  addsens;
	u_char  addsensq;
	u_char  reserved14;
	u_char  reserved15;
	u_char  rwerrcnt2;
	u_char  rwerrcnt1;
	u_char  rwerrcnt0;
	u_char  pf: 1,
		bpe: 1,
		fpe: 1,
		me: 1,
		eco: 1,
		tme: 1,
		tnp: 1,
		bot: 1;
	u_char  xfr: 1,
		tmd: 1,
 		wp: 1,
		fmke: 1,
       		ure: 1,
		we1: 1,
		sse: 1,
		fe: 1;
	u_char  rsvd: 6,
		wseb: 1,
		wseo: 1;
	u_char  reserved22;
	u_char  tplft2;
	u_char  tplft1;
	u_char  tplft0;
};

/* xsense sense key */
#define XSK_NOSENCE	0x0
#define XSK_NOTUSED1	0x1
#define XSK_NOTRDY	0x2
#define XSK_MEDERR	0x3
#define XSK_HRDWERR	0x4
#define XSK_ILLREQ	0x5
#define XSK_UNTATTEN	0x6
#define XSK_DATAPROT	0x7
#define XSK_BLNKCHK	0x8
#define XSK_VENDOR	0x9
#define XSK_CPYABORT	0xa
#define XSK_ABORTCMD	0xb
#define XSK_NOTUSEDC	0xc
#define XSK_VOLOVER	0xd
#define XSK_NOTUSEDE	0xe
#define XSK_REVERVED	0xf

struct  exb_inquiry { 
	char	venderunique[16];
};

struct	st_mode {
	u_char	sdl;	
	u_char  medtype;
	u_char  wp: 1,		/* write protect */
		bfmd: 3,	/* buffered write mode */
		speed: 4;
	u_char	bdl;
	u_char	dencod;
	u_char	numblk2;	/* number of blocks */
	u_char	numblk1;
	u_char	numblk0;
	u_char	rsvd1;
	u_char	blklen2;	/* block length */
	u_char	blklen1;
	u_char	blklen0;
};

struct st_mode_exvup {
	/* vender unique */
	u_char	ct: 1,		/* international cartridge */
		rs1: 1,
		nd: 1,		/* no disconnect, date transfer */
		rs2: 1,
		nbe: 1,		/* no busy enable */
		ebd: 1,		/* even byte disconnect */
		pe: 1,		/* parity enable */
		nal: 1;		/* no auto load */
	u_char	rsvd: 7,
		p5: 1;
	u_char	motionthres;
	u_char	reconthres;
	u_char	gapthres;
};

struct mode_select_data {
	u_char	rsvd1;
	u_char	rsvd2;
	u_char	rsvd3: 1,
		buff: 3,
		speed: 4;
	u_char  blkdeslen;
	u_char	density;
	u_char	blks2;
	u_char	blks1;
	u_char	blks0;
	u_char	rsvd4;
	u_char	blklen2;
	u_char	blklen1;
	u_char	blklen0;
	u_char	vupb;
	u_char	rsvd5: 7,
		p5: 1;
	u_char	motionthres;
	u_char	reconthres;
 	u_char	gapthres;
};

struct mode_sense {
	struct st_mode md;
	struct st_mode_exvup ex;
};

#define EXDS_BITS \
"\20\20\
\7MOVED\6LEOT\5CMD\4WRTTN\3WMODE\2OPEN\1ALIV"

#define EXER_BITS \
"\20\20VAL\17FMK\16EOM\15ILI\14KEY3\13KEY2\12KEY1\11KEY0\
\10RETRY7\7RETRY6\6RETRY5\5RETRY4\4RETRY3\3RETRY2\2RETRY1\1RETRY0"

