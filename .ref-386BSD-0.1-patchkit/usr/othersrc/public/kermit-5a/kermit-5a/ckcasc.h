/*
  File CKCASC.H
  Mnemonics for ASCII control characters (and Space) for use with C-Kermit.
*/
/*
  Author: Frank da Cruz (fdc@columbia.edu, FDCCU@CUVMA.BITNET),
  Columbia University Center for Computing Activities.
  First released January 1985.
  Copyright (C) 1985, 1992, Trustees of Columbia University in the City of New
  York.  Permission is granted to any individual or institution to use this
  software as long as it is not sold for profit.  This copyright notice must be
  retained.  This software may not be included in commercial products without
  written permission of Columbia University.
*/
#ifndef CKCASC_H
#define CKCASC_H

#define NUL  '\0'     /* Null */
#define SOH  001      /* Start of header */
#define STX  002      /* Ctrl-B */
#define ENQ  005      /* ENQ */
#define BEL  007      /* Bell (Beep) */
#define BS   010      /* Backspace */
#define HT   011      /* Horizontal Tab */
#define LF   012      /* Linefeed */
#define NL   '\n'     /* Newline */
#define FF   014      /* Formfeed */
#define CR   015      /* Carriage Return */
#define SO   016      /* Shift Out */
#define SI   017      /* Shift In */
#define DLE  020      /* Datalink Escape */
#define ESC  033      /* Escape */
#define XON  021      /* XON */
#define XOFF 023      /* XOFF */
#define SUB  032      /* SUB */
#define XGS  '\x1D'   /* Group Separator,  Ctrl-Rightbracket */
#define US   '\x1F'   /* Unit Separator,   Ctrl-Underscore */
#define XFS  '\x1C'   /* Field Separator,  Ctrl-Backslash */
#define XRS  036      /* Record Separator, Ctrl-Circumflex */
#define SYN  '\x16'   /* SYN, Ctrl-V */
#define CAN  030      /* CAN, Ctrl-X */
#define SP   040      /* Space */
#define DEL  0177     /* Delete (Rubout) */
#define RUB  0177     /* Delete (Rubout) */
#ifdef COMMENT
/*
  These aren't needed yet, so let's not cause any unnecessary conflicts.
*/
#define CSI  0233     /* 8-bit Control Sequence Introducer */
#define SS2  0216     /* 8-bit Single Shift 2 */
#define SS3  0217     /* 8-bit Single Shift 3 */
#define DCS  0220     /* 8-bit Device Control String Introducer */
#define ST8  0234     /* 8-bit String Terminator */
#define OSC  0235     /* 8-bit Operating System Command */
#define PM8  0236     /* 8-bit Privace Message */
#define APC  0237     /* 8-bit Application Program Command */
#endif /* COMMENT */
#endif /* CKCASC_H */

