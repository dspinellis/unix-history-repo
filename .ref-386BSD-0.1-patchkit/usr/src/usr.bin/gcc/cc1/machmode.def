/* This file contains the definitions and documentation for the
   machine modes used in the the GNU compiler.
   Copyright (C) 1987 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* This file defines all the MACHINE MODES used by GNU CC.

   A machine mode specifies a size and format of data
   at the machine level.

   Each RTL expression has a machine mode.

   At the syntax tree level, each ..._TYPE and each ..._DECL node
   has a machine mode which describes data of that type or the
   data of the variable declared.  */

/* The first argument is the internal name of the machine mode
   used in the C source.
   By convention these are in UPPER_CASE, except for the word  "mode".

   The second argument  is the name of the machine mode in the
   external ASCII format used for reading and printing RTL and trees.
   By convention these names in UPPER_CASE.

   Third argument states the kind of representation:
   MODE_INT - integer
   MODE_FLOAT - floating
   MODE_COMPLEX_INT - pair of integers
   MODE_COMPLEX_FLOAT - pair of floats
   MODE_FUNCTION - Algol or Pascal function-variable incl. static chain
   MODE_RANDOM - anything else

   Fourth argument is the relative size of the object.
   It is zero when the size is meaningless or not determined.
   On most machines, this is also the actual size in bytes.
   However, the general rule is that the size of SImode in bytes
   is UNITS_PER_WORD and the other sizes are proportional to that.
   (If UNITS_PER_WORD is less than 4, some modes would be less than
   one byte.  Their sizes are rounded up to 1.)

   Fifth arg is the relative size of subunits of the object.
   It is same as the fourth argument except for complexes and EPmode,
   since they are really made of two equal size subunits.

   Sixth arg is next wider natural mode of the same class,
   for widening multiply and narrowing divide.  0 if there is none.  */

/* The compiler assumes that a mode may be widened to another
   HIGHER NUMBERED mode if both those modes AND ALL MODES IN BETWEEN
   are in the same mode class.  Thus, you must should assign all
   MODE_INT and MODE_FLOAT modes in separate contiguous blocks.  */

/* VOIDmode is used when no mode needs to be specified,
   as for example on CONST_INT RTL expressions.  */
DEF_MACHMODE (VOIDmode, "VOID", MODE_RANDOM, 0, 0, 0)

DEF_MACHMODE (QImode, "QI", MODE_INT, 1, 1, HImode)		/* int types */
DEF_MACHMODE (HImode, "HI", MODE_INT, 2, 2, SImode)
/* Pointers on some machines use this type to distinguish them from ints.
   Useful if a pointer is 4 bytes but has some bits that are not significant,
   so it is really not quite as wide as an integer.  */
DEF_MACHMODE (PSImode, "PSI", MODE_INT, 4, 4, 0)
DEF_MACHMODE (SImode, "SI", MODE_INT, 4, 4, DImode)
DEF_MACHMODE (PDImode, "PDI", MODE_INT, 8, 8, 0)
DEF_MACHMODE (DImode, "DI", MODE_INT, 8, 8, TImode)
DEF_MACHMODE (TImode, "TI", MODE_INT, 16, 16, 0)
DEF_MACHMODE (QFmode, "QF", MODE_FLOAT, 1, 1, 0)
DEF_MACHMODE (HFmode, "HF", MODE_FLOAT, 2, 2, 0)	/* floating types */
DEF_MACHMODE (SFmode, "SF", MODE_FLOAT, 4, 4, 0)
DEF_MACHMODE (DFmode, "DF", MODE_FLOAT, 8, 8, 0)
DEF_MACHMODE (XFmode, "XF", MODE_FLOAT, 12, 12, 0)  /* IEEE extended float */
DEF_MACHMODE (TFmode, "TF", MODE_FLOAT, 16, 16, 0)
DEF_MACHMODE (CQImode, "CQI", MODE_COMPLEX_INT, 2, 1, 0)
DEF_MACHMODE (CHImode, "CHI", MODE_COMPLEX_INT, 4, 2, 0)  /* complex ints */
DEF_MACHMODE (CSImode, "CSI", MODE_COMPLEX_INT, 8, 4, 0)
DEF_MACHMODE (CDImode, "CDI", MODE_COMPLEX_INT, 16, 8, 0)
DEF_MACHMODE (CTImode, "CTI", MODE_COMPLEX_INT, 32, 16, 0)
DEF_MACHMODE (CQFmode, "CQF", MODE_COMPLEX_FLOAT, 2, 1, 0)
DEF_MACHMODE (CHFmode, "CHF", MODE_COMPLEX_FLOAT, 4, 2, 0) /* complex floats */
DEF_MACHMODE (CSFmode, "CSF", MODE_COMPLEX_FLOAT, 8, 4, 0)
DEF_MACHMODE (CDFmode, "CDF", MODE_COMPLEX_FLOAT, 16, 8, 0)
DEF_MACHMODE (CXFmode, "CXF", MODE_COMPLEX_FLOAT, 24, 12, 0)
DEF_MACHMODE (CTFmode, "CTF", MODE_COMPLEX_FLOAT, 32, 16, 0)

/* BImode is used only in FIELD_DECL nodes for bit fields
   whose size and alignment are not such as to fit any other mode.  */
DEF_MACHMODE (BImode, "BI", MODE_INT, 0, 0, 0)	/* signed bit field */

/* BLKmode is used for structures, arrays, etc.
   that fit no more specific mode.  */
DEF_MACHMODE (BLKmode, "BLK", MODE_RANDOM, 0, 0, 0)

/* Function-variable that includes a static chain.  */
DEF_MACHMODE (EPmode, "EP", MODE_RANDOM, 8, 4, 0)

/* The symbol Pmode stands for one of the above machine modes (usually SImode).
   The tm file specifies which one.  It is not a distinct mode.  */

/*
Local variables:
mode:c
version-control: t
End:
*/
