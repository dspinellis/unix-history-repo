#include <X/mit-copyright.h>

/* $Header: chartable.h,v 10.3 86/02/01 16:06:06 tony Rel $ */

#define	CIGNORE		0
#define BUFFER_MODE	1
#define	CLINEFEED	2
#define	CRETURN		4
#define	CPRINTING	6
#define FLUSH		8
#define	CRI		10
#define	CESC		12
#define	CCANCEL		14
#define	CLS1		16
#define	CLS0		18
#define	CTEKINIT	20
#define	CTEKALPH	22
#define	CCSI		24
#define	CDCS		26
#define	CTAB		28
#define	CFORMFEED	30
#define	CBACKSPACE	32
#define	CSS2		34
#define	CSS3		36
#define	CIND		38
#define	CNEL		40
#define	CBELL		42

/*
 * Decode table		
 */
#define ctable (&chartable[1])
unsigned char	chartable[257] = {
	FLUSH,				/* EOF code (-1)		*/
	CIGNORE,			/* NUL				*/
	CIGNORE,			/* SOH				*/
	CIGNORE,			/* STX				*/
	CIGNORE,			/* ETX				*/
	CIGNORE,			/* EOT				*/
	CIGNORE,			/* ENQ				*/
	CIGNORE,			/* ACK				*/
	CBELL,				/* BEL				*/
	CBACKSPACE,			/* BS				*/
	CTAB,				/* HT				*/
	CLINEFEED,			/* LF				*/
	CLINEFEED,			/* VT				*/
	CFORMFEED,			/* FF				*/
	CRETURN,			/* CR				*/
	CLS1,				/* SO	(Locking shift 1)	*/
	CLS0,				/* SI	(Locking shift 0)	*/
	CIGNORE,			/* DLE				*/
	CIGNORE,			/* DC1				*/
	CIGNORE,			/* DC2				*/
	CIGNORE,			/* DC3				*/
	CIGNORE,			/* DC4				*/
	CIGNORE,			/* NAK				*/
	CIGNORE,			/* SYN				*/
	CIGNORE,			/* ETB				*/
	CCANCEL,			/* CAN				*/
	CIGNORE,			/* EM				*/
	CCANCEL,			/* SUB				*/
	CESC,				/* ESC	(ESCAPE introducer)	*/
	CTEKINIT,			/* FS	(Tek point plot mode)	*/
	CTEKINIT,			/* GS	(Tek vector plot mode)	*/
	CTEKINIT,			/* RS	(Tek incr. plot mode)	*/
	CTEKALPH,			/* US	(Tek alpha mode)	*/

	CPRINTING,			/* SP				*/
	CPRINTING,			/* !				*/
	CPRINTING,			/* "				*/
	CPRINTING,			/* #				*/
	CPRINTING,			/* $				*/
	CPRINTING,			/* %				*/
	CPRINTING,			/* &				*/
	CPRINTING,			/* '				*/
	CPRINTING,			/* (				*/
	CPRINTING,			/* )				*/
	CPRINTING,			/* *				*/
	CPRINTING,			/* +				*/
	CPRINTING,			/* ,				*/
	CPRINTING,			/* -				*/
	CPRINTING,			/* .				*/
	CPRINTING,			/* /				*/
	CPRINTING,			/* 0				*/
	CPRINTING,			/* 1				*/
	CPRINTING,			/* 2				*/
	CPRINTING,			/* 3				*/
	CPRINTING,			/* 4				*/
	CPRINTING,			/* 5				*/
	CPRINTING,			/* 6				*/
	CPRINTING,			/* 7				*/
	CPRINTING,			/* 8				*/
	CPRINTING,			/* 9				*/
	CPRINTING,			/* :				*/
	CPRINTING,			/* ;				*/
	CPRINTING,			/* <				*/
	CPRINTING,			/* =				*/
	CPRINTING,			/* >				*/
	CPRINTING,			/* ?				*/

	CPRINTING,			/* @				*/
	CPRINTING,			/* A				*/
	CPRINTING,			/* B				*/
	CPRINTING,			/* C				*/
	CPRINTING,			/* D				*/
	CPRINTING,			/* E				*/
	CPRINTING,			/* F				*/
	CPRINTING,			/* G				*/
	CPRINTING,			/* H				*/
	CPRINTING,			/* I				*/
	CPRINTING,			/* J				*/
	CPRINTING,			/* K				*/
	CPRINTING,			/* L				*/
	CPRINTING,			/* M				*/
	CPRINTING,			/* N				*/
	CPRINTING,			/* O				*/
	CPRINTING,			/* P				*/
	CPRINTING,			/* Q				*/
	CPRINTING,			/* R				*/
	CPRINTING,			/* S				*/
	CPRINTING,			/* T				*/
	CPRINTING,			/* U				*/
	CPRINTING,			/* V				*/
	CPRINTING,			/* W				*/
	CPRINTING,			/* X				*/
	CPRINTING,			/* Y				*/
	CPRINTING,			/* Z				*/
	CPRINTING,			/* [				*/
	CPRINTING,			/* \				*/
	CPRINTING,			/* ]				*/
	CPRINTING,			/* ^				*/
	CPRINTING,			/* _				*/

	CPRINTING,			/* `				*/
	CPRINTING,			/* a				*/
	CPRINTING,			/* b				*/
	CPRINTING,			/* c				*/
	CPRINTING,			/* d				*/
	CPRINTING,			/* e				*/
	CPRINTING,			/* f				*/
	CPRINTING,			/* g				*/
	CPRINTING,			/* h				*/
	CPRINTING,			/* i				*/
	CPRINTING,			/* j				*/
	CPRINTING,			/* k				*/
	CPRINTING,			/* l				*/
	CPRINTING,			/* m				*/
	CPRINTING,			/* n				*/
	CPRINTING,			/* o				*/
	CPRINTING,			/* p				*/
	CPRINTING,			/* q				*/
	CPRINTING,			/* r				*/
	CPRINTING,			/* s				*/
	CPRINTING,			/* t				*/
	CPRINTING,			/* u				*/
	CPRINTING,			/* v				*/
	CPRINTING,			/* w				*/
	CPRINTING,			/* x				*/
	CPRINTING,			/* y				*/
	CPRINTING,			/* z				*/
	CPRINTING,			/* {				*/
	CPRINTING,			/* |				*/
	CPRINTING,			/* }				*/
	CPRINTING,			/* ~				*/
	CIGNORE,			/* DEL				*/
	
	CIGNORE,			/* Reserved.			*/
	CIGNORE,			/* Reserved.			*/
	CIGNORE,			/* Reserved.			*/
	CIGNORE,			/* Reserved.			*/
	CIND,				/* IND				*/
	CNEL,				/* NEL				*/
	CIGNORE,			/* SSA				*/
	CIGNORE,			/* ESA				*/
	CIGNORE,			/* HTS				*/
	CIGNORE,			/* HTJ				*/
	CIGNORE,			/* VTS				*/
	CIGNORE,			/* PLD				*/
	CIGNORE,			/* PLU				*/
	CRI,				/* RI				*/
	CSS2,				/* SS2				*/
	CSS3,				/* SS3				*/
	CDCS,				/* DCS				*/
	CIGNORE,			/* PU1				*/
	CIGNORE,			/* PU2				*/
	CIGNORE,			/* STS				*/
	CIGNORE,			/* CCH				*/
	CIGNORE,			/* MW				*/
	CIGNORE,			/* SPA				*/
	CIGNORE,			/* EPA				*/
	CIGNORE,			/* Reserved			*/
	CIGNORE,			/* Reserved			*/
	CIGNORE,			/* Reserved			*/
	CCSI,				/* CSI				*/
	CCANCEL,			/* ST				*/
	CDCS,				/* OSC				*/
	CDCS,				/* PM				*/
	CDCS,				/* APC				*/

	CPRINTING,			/* SP				*/
	CPRINTING,			/* Inverted !			*/
	CPRINTING,			/* Cent				*/
	CPRINTING,			/* Pound Sterling		*/
	CPRINTING,			/* 				*/
	CPRINTING,			/* Yen				*/
	CPRINTING,			/* 				*/
	CPRINTING,			/* Section sign			*/
	CPRINTING,			/* Blob				*/
	CPRINTING,			/* Copyright			*/
	CPRINTING,			/* Fem. ordinal			*/
	CPRINTING,			/* <<				*/
	CPRINTING,			/* 				*/
	CPRINTING,			/* 				*/
	CPRINTING,			/* 				*/
	CPRINTING,			/* 				*/
	CPRINTING,			/* Degree			*/
	CPRINTING,			/* +/-				*/
	CPRINTING,			/* Superscript 2		*/
	CPRINTING,			/* Superscript 3		*/
	CPRINTING,			/* 				*/
	CPRINTING,			/* Micro			*/
	CPRINTING,			/* Paragraph			*/
	CPRINTING,			/* Dot				*/
	CPRINTING,			/* 				*/
	CPRINTING,			/* Superscript 1		*/
	CPRINTING,			/* Masc. ordinal		*/
	CPRINTING,			/* >>				*/
	CPRINTING,			/* 1/4				*/
	CPRINTING,			/* 1/2				*/
	CPRINTING,			/* 				*/
	CPRINTING,			/* Inverted ?			*/

	CPRINTING,			/* A grave			*/
	CPRINTING,			/* A acute			*/
	CPRINTING,			/* A curcumflex			*/
	CPRINTING,			/* A tilde			*/
	CPRINTING,			/* A diaresis			*/
	CPRINTING,			/* A ring			*/
	CPRINTING,			/* AE				*/
	CPRINTING,			/* C cedilla			*/
	CPRINTING,			/* E grave			*/
	CPRINTING,			/* E acute			*/
	CPRINTING,			/* E curcumflex			*/
	CPRINTING,			/* E diaresis			*/
	CPRINTING,			/* I grave			*/
	CPRINTING,			/* I acute			*/
	CPRINTING,			/* I curcumflex			*/
	CPRINTING,			/* I diaresis			*/
	CPRINTING,			/* 				*/
	CPRINTING,			/* N tilde			*/
	CPRINTING,			/* O grave			*/
	CPRINTING,			/* O acute			*/
	CPRINTING,			/* O circumflex			*/
	CPRINTING,			/* O tilde			*/
	CPRINTING,			/* O diaresis			*/
	CPRINTING,			/* OE				*/
	CPRINTING,			/* O slash			*/
	CPRINTING,			/* U grave			*/
	CPRINTING,			/* U acute			*/
	CPRINTING,			/* U circumflex			*/
	CPRINTING,			/* U diaresis			*/
	CPRINTING,			/* Y diaresis			*/
	CPRINTING,			/* 				*/
	CPRINTING,			/* German "s"			*/

	CPRINTING,			/* a grave			*/
	CPRINTING,			/* a acute			*/
	CPRINTING,			/* a curcumflex			*/
	CPRINTING,			/* a tilde			*/
	CPRINTING,			/* a diaresis			*/
	CPRINTING,			/* a ring			*/
	CPRINTING,			/* ae				*/
	CPRINTING,			/* c cedilla			*/
	CPRINTING,			/* e grave			*/
	CPRINTING,			/* e acute			*/
	CPRINTING,			/* e curcumflex			*/
	CPRINTING,			/* e diaresis			*/
	CPRINTING,			/* i grave			*/
	CPRINTING,			/* i acute			*/
	CPRINTING,			/* i circumflex			*/
	CPRINTING,			/* i diaresis			*/
	CPRINTING,			/* 				*/
	CPRINTING,			/* n tilde			*/
	CPRINTING,			/* o grave			*/
	CPRINTING,			/* o acute			*/
	CPRINTING,			/* o curcumflex			*/
	CPRINTING,			/* o tilde			*/
	CPRINTING,			/* o diaresis			*/
	CPRINTING,			/* oe				*/
	CPRINTING,			/* o slash			*/
	CPRINTING,			/* u grave			*/
	CPRINTING,			/* u acute			*/
	CPRINTING,			/* u circumflex			*/
	CPRINTING,			/* u diaresis			*/
	CPRINTING,			/* y diaresis			*/
	CPRINTING,			/* 				*/
	CIGNORE				/* Right delete			*/
};
