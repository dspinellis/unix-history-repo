/* Declarations for interface to insn recognizer and insn-output.c.
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

/* Recognize an insn and return its insn-code,
   which is the sequence number of the DEFINE_INSN that it matches.
   If the insn does not match, return -1.  */

extern int recog_memoized ();

/* Extract the operands from an insn that has been recognized.  */

extern void insn_extract ();

/* The following vectors hold the results from insn_extract.  */

/* Indexed by N, gives value of operand N.  */
extern rtx recog_operand[];

/* Indexed by N, gives location where operand N was found.  */
extern rtx *recog_operand_loc[];

/* Indexed by N, gives location where the Nth duplicate-appearance of
   an operand was found.  This is something that matched MATCH_DUP.  */
extern rtx *recog_dup_loc[];

/* Indexed by N, gives the operand number that was duplicated in the
   Nth duplicate-appearance of an operand.  */
extern char recog_dup_num[];

/* Tables defined in insn-output.c that give information about
   each insn-code value.  */

/* These are vectors indexed by insn-code.  Details in genoutput.c.  */

extern char *insn_template[];

extern char *(*insn_outfun[]) ();

extern int insn_n_operands[];

extern int insn_n_dups[];

#ifdef INSN_MACHINE_INFO
extern INSN_MACHINE_INFO insn_machine_info[];
#endif

/* Indexed by insn code number, gives # of constraint alternatives.  */

extern int insn_n_alternatives[];

/* These are two-dimensional arrays indexed first by the insn-code
   and second by the operand number.  Details in genoutput.c.  */

extern char *insn_operand_constraint[][MAX_RECOG_OPERANDS];

extern char insn_operand_address_p[][MAX_RECOG_OPERANDS];

extern enum machine_mode insn_operand_mode[][MAX_RECOG_OPERANDS];

extern char insn_operand_strict_low[][MAX_RECOG_OPERANDS];

extern int (*insn_operand_predicate[][MAX_RECOG_OPERANDS]) ();
