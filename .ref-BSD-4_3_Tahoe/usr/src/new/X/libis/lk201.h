/*
 *	$Source: /u1/X/libis/RCS/lk201.h,v $
 *	$Header: lk201.h,v 1.1 86/11/17 14:35:05 swick Rel $
 */

#include "is-copyright.h"

/*
 *	Ascii mapping to the LK201 keycodes.
 *
 *	Copyright (c) 1986, Integrated Solutions, Inc.
 */

static u_short LK201[] = {
    0324|ControlMask,	/* 000 nul */
    0302|ControlMask,	/* 001 soh */
    0331|ControlMask,	/* 002 stx */
    0316|ControlMask,	/* 003 etx */
    0315|ControlMask,	/* 004 eot */
    0314|ControlMask,	/* 005 enq */
    0322|ControlMask,	/* 006 ack */
    0330|ControlMask,	/* 007 bel */
    0335|ControlMask,	/* 010 bs  */
    0276|ControlMask,	/* 011 ht  */
    0342|ControlMask,	/* 012 nl  */
    0347|ControlMask,	/* 013 vt  */
    0354|ControlMask,	/* 014 np  */
    0275|ControlMask,	/* 015 cr  */
    0336|ControlMask,	/* 016 so  */
    0353|ControlMask,	/* 017 si  */
    0360|ControlMask,	/* 020 dle */
    0301|ControlMask,	/* 021 dc1 */
    0321|ControlMask,	/* 022 dc2 */
    0307|ControlMask,	/* 023 dc3 */
    0327|ControlMask,	/* 024 dc4 */
    0341|ControlMask,	/* 025 nak */
    0323|ControlMask,	/* 026 syn */
    0306|ControlMask,	/* 027 etb */
    0310|ControlMask,	/* 030 can */
    0334|ControlMask,	/* 031 em  */
    0303|ControlMask,	/* 032 sub */
    0372|ControlMask,	/* 033 esc */
    0367|ControlMask,	/* 034 fs  */
    0366|ControlMask,	/* 035 gs  */
    0277|ControlMask,	/* 036 rs  */
    0371|ControlMask,	/* 037 us  */
    0324,		/* 040 sp  */
    0300|ShiftMask,	/* 041  !  */
    0373|ShiftMask,	/* 042  "  */
    0313|ShiftMask,	/* 043  #  */
    0320|ShiftMask,	/* 044  $  */
    0326|ShiftMask,	/* 045  %  */
    0340|ShiftMask,	/* 046  &  */
    0373,		/* 047  '  */
    0352|ShiftMask,	/* 050  (  */
    0357|ShiftMask,	/* 051  )  */
    0345|ShiftMask,	/* 052  *  */
    0365|ShiftMask,	/* 053  +  */
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
    0362|ShiftMask,	/* 072  :  */
    0362,		/* 073  ;  */
    0350|ShiftMask,	/* 074  <  */
    0365,		/* 075  =  */
    0355|ShiftMask,	/* 076  >  */
    0363|ShiftMask,	/* 077  ?  */
    0305|ShiftMask,	/* 100  @  */
    0302|ShiftMask,	/* 101  A  */
    0331|ShiftMask,	/* 102  B  */
    0316|ShiftMask,	/* 103  C  */
    0315|ShiftMask,	/* 104  D  */
    0314|ShiftMask,	/* 105  E  */
    0322|ShiftMask,	/* 106  F  */
    0330|ShiftMask,	/* 107  G  */
    0335|ShiftMask,	/* 110  H  */
    0346|ShiftMask,	/* 111  I  */
    0342|ShiftMask,	/* 112  J  */
    0347|ShiftMask,	/* 113  K  */
    0354|ShiftMask,	/* 114  L  */
    0343|ShiftMask,	/* 115  M  */
    0336|ShiftMask,	/* 116  N  */
    0353|ShiftMask,	/* 117  O  */
    0360|ShiftMask,	/* 120  P  */
    0301|ShiftMask,	/* 121  Q  */
    0321|ShiftMask,	/* 122  R  */
    0307|ShiftMask,	/* 123  S  */
    0327|ShiftMask,	/* 124  T  */
    0341|ShiftMask,	/* 125  U  */
    0323|ShiftMask,	/* 126  V  */
    0306|ShiftMask,	/* 127  W  */
    0310|ShiftMask,	/* 130  X  */
    0334|ShiftMask,	/* 131  Y  */
    0303|ShiftMask,	/* 132  Z  */
    0372,		/* 133  [  */
    0367,		/* 134  \  */
    0366,		/* 135  ]  */
    0333|ShiftMask,	/* 136  ^  */
    0371|ShiftMask,	/* 137  _  */
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
    0372|ShiftMask,	/* 173  {  */
    0367|ShiftMask,	/* 174  |  */
    0366|ShiftMask,	/* 175  }  */
    0277|ShiftMask,	/* 176  ~  */
    0274,		/* 177 del */
};
