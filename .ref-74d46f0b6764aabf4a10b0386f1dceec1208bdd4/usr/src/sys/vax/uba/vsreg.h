/* @(#)vsreg.h	7.1 (MIT) %G% */
 /****************************************************************************
 *									    *
 *  Copyright (c) 1983, 1984 by						    *
 *  DIGITAL EQUIPMENT CORPORATION, Maynard, Massachusetts.		    *
 *  All rights reserved.						    *
 * 									    *
 *  This software is furnished on an as-is basis and may be used and copied *
 *  only with inclusion of the above copyright notice. This software or any *
 *  other copies thereof may be provided or otherwise made available to     *
 *  others only for non-commercial purposes.  No title to or ownership of   *
 *  the software is hereby transferred.					    *
 * 									    *
 *  The information in this software is  subject to change without notice   *
 *  and  should  not  be  construed as  a commitment by DIGITAL EQUIPMENT   *
 *  CORPORATION.							    *
 * 									    *
 *  DIGITAL assumes no responsibility for the use  or  reliability of its   *
 *  software on equipment which is not supplied by DIGITAL.		    *
 * 									    *
 *									    *
 ****************************************************************************/

/* 
 * vsreg.h - VS100 Registers and Bits
 * 
 * Author:	Christopher A. Kent
 *		Digital Equipment Corporation
 *		Western Research Lab
 * Date:	Tue Jun 14 1983
 */

struct vsdevice{
	u_short	vs_csr0;		/* Control and Status */
	u_short	vs_csr1;		/* Interrupt Reason */
	u_short	vs_csr2;		/* Keyboard Receive */
	u_short	vs_csr3;		/* Function Parameter Low */
	u_short	vs_csr4;		/* Function Parameter High */
	u_short	vs_csr5;		/* Cursor Position X */
	u_short	vs_csr6;		/* Cursor Position Y */
	u_short	vs_csr7;		/* Interrupt Vector */
	u_short	vs_csr8;		/* Spare 1 */
	u_short	vs_csr9;		/* Spare 2 */
	u_short	vs_csra;		/* Spare 3 */
	u_short	vs_csrb;		/* Spare 4 */
	u_short	vs_csrc;		/* Spare 5 */
	u_short	vs_csrd;		/* Spare 6 */
	u_short	vs_csre;		/* Spare 7 */
	u_short	vs_csrf;		/* Interrupt Vector (2Bs) */
};

/* 
 * CSR0 - Control and Status
 */

#define	VS_LNK_TRNS	0100000		/* Link Transition */
#define	VS_LNK_AVL	0040000		/* Link Available */
#define	VS_LNK_ERR	0020000		/* Link Error */
#define	VS_XMIT_ON	0010000		/* Transmitter On */
#define	VS_MNT_MODE	0004000		/* Maintenance Mode */
#define	VS_CRC_DIS	0002000		/* CRC Disable */
#define	VS_MNT_DONE	0001000		/* Maintenance Done */
#define	VS_SPARE	0000400		/* Spare */
#define	VS_OWN		0000200		/* Owner */
#define	VS_IE		0000100		/* Interrupt Enable */
#define	VS_FCN		0000076		/* Function Code */
#define	VS_GO		0000001		/* GO! */

struct vs_csr{
    union{
	u_short	_register;
	struct{
	    unsigned _go : 1;
	    unsigned _function : 5;
	    unsigned _ie : 1;
	    unsigned _own : 1;
	    unsigned _spare : 1;
	    unsigned _mainDone : 1;
	    unsigned _CRCdisable : 1;
	    unsigned _mainMode : 1;
	    unsigned _xmitOn : 1;
	    unsigned _linkErr : 1;
	    unsigned _linkAvail : 1;
	    unsigned _linkTran : 1;
	}_bits;
    }_X;
};

#define	csr_reg		_X._register
#define csr_go		_X._bits._go
#define csr_ie		_X._bits._ie
#define	csr_own		_X._bits._own
#define	csr_mainDone	_X._bits._mainDone
#define	csr_CRCdisable	_X._bits._CRCdisable
#define csr_mainMode	_X._bits._mainMode
#define	csr_xmitOn	_X._bits._xmitOn
#define	csr_linkErr	_X._bits._linkErr
#define	csr_linkAvail	_X._bits._linkAvail
#define	csr_linkTran	_X._bits._linkTran

/* Function Codes */

#define	VS_INIT		01		/* Initialize Display */
#define	VS_SEND		02		/* Send Packet */
#define	VS_START	03		/* Start Microcode */
#define	VS_ABORT	04		/* Abort Command Chain */
#define	VS_PWRUP	05		/* Power Up Reset */
/**/
#define	VS_ENABBA	020		/* Enable BBA */
#define	VS_DISBBA	021		/* Disable BBA */
#define	VS_INFINITE	022		/* Inifinite Retries */
#define	VS_FINITE	023		/* Finite Retries */

/* amount to shift to get function code into right place */

#define	VS_FCSHIFT	01

/* 
 * CSR1 - Interrupt Reason
 */

#define	vs_irr		vs_csr1

#define	VS_ERROR	0100000		/* Any error */
#define	VS_REASON	0077777		/* Reason Mask */
#define	VSIRR_BITS \
"\20\20ERROR\10PWRUP\6TABLET\5MOUSE\4BUTTON\3START\2DONE\1INIT"

#define	VS_INT_US	0
#define	VS_INT_ID	01
#define	VS_INT_CD	02
#define	VS_INT_SE	04
#define	VS_INT_BE	010
#define	VS_INT_MM	020
#define	VS_INT_TM	040
#define	VS_INT_PWR	0200

struct vs_intr{
    union{
	u_short	 _register;		/* whole register */
	struct{
	    unsigned _reason : 14;	/* Reason bits */
	    unsigned _diagnostic : 1;	/* Diagnostic Error bit */
	    unsigned _error : 1;	/* Error bit */
	}_bits;
    }_X;
};

#define	intr_reg	_X._register
#define	intr_reason	_X._bits._reason
#define	intr_diagnostic	_X._bits._diagnostic	/* not in rev 2b */
#define	intr_error	_X._bits._error

/* 
 * CSR2 - Keyboard Receive
 */

#define	vs_krr		vs_csr2

#define	VS_KBDEV	0007000		/* Device mask */
#define	VS_KBT		0000400		/* Transition direction */
#define	VS_KBKEY	0000377		/* Key mask */

struct vs_kbd{
    union{
	u_short	 _register;		/* whole register */
	struct{
	    unsigned _key : 8;		/* Key number */
	    unsigned _transition : 1;	/* Transition direction */
	    unsigned _device : 3;	/* Device */
	    unsigned _x : 4;		/* Unused */
	}_bits;
    }_X;
};

#define	kbd_reg		_X._register
#define	kbd_key		_X._bits._key
#define	kbd_transition	_X._bits._transition
#define	kbd_device	_X._bits._device

#define	VS_KBTUP	0		/* up */
#define	VS_KBTDOWN	1		/* down */

/* 
 * CSR3/4 Function Parameter Address
 */

#define	vs_pr1		vs_csr3
#define	vs_pr2		vs_csr4

struct vs_fparm{
    union{
	struct{
	    u_short _plow;	/* low 16 bits of address */
	    u_short _phigh;	/* high 16 bits of address */
	}_parts;
	caddr_t _pall;
    }_X;
};
#define	fparm_low	_X._parts._plow
#define	fparm_high	_X._parts._phigh
#define	fparm_all	_X._pall

/* 
 * CSR5/6 - Cursor position
 */

#define	vs_cxr		vs_csr5
#define	vs_cyr		vs_csr6


/* 
 * CSR 7 - Interrupt vector in fiber cable machines
 */

#define	vs_ivr		vs_csr7

/* 
 * CSR 8 through 14 Spare
 */

#define	vs_spr2		vs_csr8
#define	vs_spr3		vs_csr9
#define	vs_spr4		vs_csra
#define	vs_spr5		vs_csrb
#define	vs_spr6		vs_csrc
#define	vs_spr7		vs_csrd
#define	vs_spr8		vs_csre

/* 
 * CSR 15 - Interrupt vector in rev 2B
 */

#define	vs_ivr2		vs_csrf
