/* $Header: lk201.h,v 10.5 86/12/17 20:38:13 swick Exp $ */
/*
 * The Sun X drivers are a product of Sun Microsystems, Inc. and are provided
 * for unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify these drivers without charge, but are not authorized
 * to license or distribute them to anyone else except as part of a product or
 * program developed by the user.
 * 
 * THE SUN X DRIVERS ARE PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND
 * INCLUDING THE WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE
 * PRACTICE.
 *
 * The Sun X Drivers are provided with no support and without any obligation
 * on the part of Sun Microsystems, Inc. to assist in their use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THE SUN X
 * DRIVERS OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

/*-
 * Copyright (c) 1986 by Sun Microsystems,  Inc.
 *
 * @(#)lk201.h 2.1 86/01/28
 *
 */

/*
 *	Bogus mapping to the LK201 keycodes.  This is really squalid.
 */
static u_short LK201[] = {
	0324|ControlMask,		/* 000 nul */
	0302|ControlMask,		/* 001 soh */
	0331|ControlMask,		/* 002 stx */
	0316|ControlMask,		/* 003 etx */
	0315|ControlMask,		/* 004 eot */
	0314|ControlMask,		/* 005 enq */
	0322|ControlMask,		/* 006 ack */
	0330|ControlMask,		/* 007 bel */
	0335|ControlMask,		/* 010 bs  */
	0276|ControlMask,		/* 011 ht  */
	0342|ControlMask,		/* 012 nl  */
	0347|ControlMask,		/* 013 vt  */
	0354|ControlMask,		/* 014 np  */
	0275|ControlMask,		/* 015 cr  */
	0336|ControlMask,		/* 016 so  */
	0353|ControlMask,		/* 017 si  */
	0360|ControlMask,		/* 020 dle */
	0301|ControlMask,		/* 021 dc1 */
	0321|ControlMask,		/* 022 dc2 */
	0307|ControlMask,		/* 023 dc3 */
	0327|ControlMask,		/* 024 dc4 */
	0341|ControlMask,		/* 025 nak */
	0323|ControlMask,		/* 026 syn */
	0306|ControlMask,		/* 027 etb */
	0310|ControlMask,		/* 030 can */
	0334|ControlMask,		/* 031 em  */
	0303|ControlMask,		/* 032 sub */
	0372|ControlMask,		/* 033 esc */
	0367|ControlMask,		/* 034 fs  */
	0366|ControlMask,		/* 035 gs  */
	0277|ControlMask,		/* 036 rs  */
	0371|ControlMask,		/* 037 us  */
	0324,		/* 040 sp  */
	0300|ShiftMask,		/* 041  !  */
	0373|ShiftMask,		/* 042  "  */
	0313|ShiftMask,		/* 043  #  */
	0320|ShiftMask,		/* 044  $  */
	0326|ShiftMask,		/* 045  %  */
	0340|ShiftMask,		/* 046  &  */
	0373,		/* 047  '  */	
	0352|ShiftMask,		/* 050  (  */
	0357|ShiftMask,		/* 051  )  */
	0345|ShiftMask,		/* 052  *  */
	0365|ShiftMask,		/* 053  +  */
	0350,		/* 054  ,  */	
	0371,		/* 055  -  */	
	0355,		/* 056  .  */	
	0363,		/* 057  /  */	
	0357,		/* 060  0  */
	0300,		/* 061  1  */
	0305,		/* 062  2  */
	0313,		/* 063  3  */
	0320,		/* 064  4  */
	0326,		/* 065  5  */
	0333,		/* 066  6  */
	0340,		/* 067  7  */
	0345,		/* 070  8  */
	0352,		/* 071  9  */
	0362|ShiftMask,		/* 072  :  */	
	0362,		/* 073  ;  */
	0350|ShiftMask,		/* 074  <  */	
	0365,		/* 075  =  */
	0355|ShiftMask,		/* 076  >  */	
	0363|ShiftMask,		/* 077  ?  */	
	0305|ShiftMask,		/* 100  @  */
	0302|ShiftMask,		/* 101  A  */
	0331|ShiftMask,		/* 102  B  */
	0316|ShiftMask,		/* 103  C  */
	0315|ShiftMask,		/* 104  D  */
	0314|ShiftMask,		/* 105  E  */
	0322|ShiftMask,		/* 106  F  */
	0330|ShiftMask,		/* 107  G  */
	0335|ShiftMask,		/* 110  H  */
	0346|ShiftMask,		/* 111  I  */
	0342|ShiftMask,		/* 112  J  */
	0347|ShiftMask,		/* 113  K  */
	0354|ShiftMask,		/* 114  L  */
	0343|ShiftMask,		/* 115  M  */
	0336|ShiftMask,		/* 116  N  */
	0353|ShiftMask,		/* 117  O  */
	0360|ShiftMask,		/* 120  P  */
	0301|ShiftMask,		/* 121  Q  */
	0321|ShiftMask,		/* 122  R  */
	0307|ShiftMask,		/* 123  S  */
	0327|ShiftMask,		/* 124  T  */
	0341|ShiftMask,		/* 125  U  */
	0323|ShiftMask,		/* 126  V  */
	0306|ShiftMask,		/* 127  W  */
	0310|ShiftMask,		/* 130  X  */
	0334|ShiftMask,		/* 131  Y  */
	0303|ShiftMask,		/* 132  Z  */
	0372,		/* 133  [  */	
	0367,		/* 134  \  */	
	0366,		/* 135  ]  */	
	0333|ShiftMask,		/* 136  ^  */
	0371|ShiftMask,		/* 137  _  */
	0277,		/* 140  `  */
	0302,		/* 141  a  */
	0331,		/* 142  b  */
	0316,		/* 143  c  */
	0315,		/* 144  d  */
	0314,		/* 145  e  */
	0322,		/* 146  f  */
	0330,		/* 147  g  */
	0335,		/* 150  h  */
	0346,		/* 151  i  */
	0342,		/* 152  j  */
	0347,		/* 153  k  */
	0354,		/* 154  l  */
	0343,		/* 155  m  */
	0336,		/* 156  n  */
	0353,		/* 157  o  */
	0360,		/* 160  p  */
	0301,		/* 161  q  */
	0321,		/* 162  r  */
	0307,		/* 163  s  */
	0327,		/* 164  t  */
	0341,		/* 165  u  */
	0323,		/* 166  v  */
	0306,		/* 167  w  */
	0310,		/* 170  x  */
	0334,		/* 171  y  */
	0303,		/* 172  z  */
	0372|ShiftMask,		/* 173  {  */	
	0367|ShiftMask,		/* 174  | */	
	0366|ShiftMask,		/* 175  }  */	
	0277|ShiftMask,		/* 176  ~  */	
	0274,		/* 177 del */
};


#ifdef	RAW_KBD
static u_short TopKeys[16] = {
	0126,	/* F1 */
	0127,	/* F2 */
	0130,	/* F3 */
	0131,	/* F4 */
	0132,	/* F5 */
	0144,	/* F6 */
	0145,	/* F7 */
	0146,	/* F8 */
	0147,	/* F9 */
	0150,	/* F10 */
	0161,	/* F11/ESC */
	0162,	/* F12/BS */
	0163,	/* F13/LF */
	0164,	/* F14 */
	0174,	/* F15 */
	0175,	/* F16 */
};
	

static u_short RightKeys[16] = {
	0176,	/* R1 */
	0177,	/* R2 */
	0200,	/* R3 (F17) */
	0201,	/* R4 (F18) */
	0202,	/* R5 (F19) */
	0203,	/* R6 (F20) */
	0204,	/* R7 */
	0252,	/* R8 (UPARROW) */
	0205,	/* R9 */
	0247,	/* R10 (LEFTARROW) */
	0206,	/* R11 */
	0250,	/* R12 (RIGHTARROW) */
	0207,	/* R13 */
	0251,	/* R14 (DOWNARROW) */
	0210,	/* R15 */
	0211,	/* R16 */
};

static u_short LeftKeys[16] = {
	0212,	/* L1 (E1) */
	0213,	/* L2 (E2) */
	0214,	/* L3 (E3) */
	0215,	/* L4 (E4) */
	0216,	/* L5 (E5) */
	0217,	/* L6 (E6) */
	0241,	/* L7 (PF1) */
	0242,	/* L8 (PF2) */
	0243,	/* L9 (PF3) */
	0244,	/* L10(PF4) */
	0245,	/* L11/ALT */
	0246,	/* L12 */
	0253,	/* L13 */
	0254,	/* L14 */
	0255,	/* L15 */
	0262,	/* L16 */
};

static u_short BotKeys[16] = {
	0220,	/* B1 */
	0221,	/* B2 */
	0222,	/* B3 (KEYPAD_0) */
	0223,	/* B4 */
	0224,	/* B5 (KEYPAD_PERIOD) */
	0225,	/* B6 (ENTER) */
	0226,	/* B7 (KEYPAD_1) */
	0227,	/* B8 (KEYPAD_2) */
	0230,	/* B9 (KEYPAD_3) */
	0231,	/* B10 (KEYPAD_4) */
	0232,	/* B11 (KEYPAD_5) */
	0233,	/* B12 (KEYPAD_6) */
	0234,	/* B13 (KEYPAD_COMMA) */
	0235,	/* B14 (KEYPAD_7) */
	0236,	/* B15 (KEYPAD_8) */
	0237,	/* B16 (KEYPAD_9) */
};
#endif
