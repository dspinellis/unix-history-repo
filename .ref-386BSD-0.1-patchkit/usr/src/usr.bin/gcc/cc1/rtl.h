/* Register Transfer Language (RTL) definitions for GNU C-Compiler
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


#undef FFS  /* Some systems predefine this symbol; don't let it interfere.  */

/* Register Transfer Language EXPRESSIONS CODES */

#define RTX_CODE	enum rtx_code
enum rtx_code  {

#define DEF_RTL_EXPR(ENUM, NAME, FORMAT)   ENUM ,
#include "rtl.def"		/* rtl expressions are documented here */
#undef DEF_RTL_EXPR

  LAST_AND_UNUSED_RTX_CODE};	/* A convienent way to get a value for
				   NUM_RTX_CODE.
				   Assumes default enum value assignement.  */

#define NUM_RTX_CODE ((int)LAST_AND_UNUSED_RTX_CODE)
				/* The cast here, saves many elsewhere.  */

extern int rtx_length[];
#define GET_RTX_LENGTH(CODE)		(rtx_length[(int)(CODE)])

extern char *rtx_name[];
#define GET_RTX_NAME(CODE)		(rtx_name[(int)(CODE)])

extern char *rtx_format[];
#define GET_RTX_FORMAT(CODE)		(rtx_format[(int)(CODE)])


/* Get the definition of `enum machine_mode' */

#ifndef HAVE_MACHINE_MODES

#define DEF_MACHMODE(SYM, NAME, TYPE, SIZE, UNIT, WIDER)  SYM,

enum machine_mode {
#include "machmode.def"
MAX_MACHINE_MODE };

#undef DEF_MACHMODE

#define HAVE_MACHINE_MODES

#endif /* not HAVE_MACHINE_MODES */

#ifndef NUM_MACHINE_MODES
#define NUM_MACHINE_MODES (int) MAX_MACHINE_MODE
#endif

/* Get the name of mode MODE as a string.  */

extern char *mode_name[];
#define GET_MODE_NAME(MODE)		(mode_name[(int)(MODE)])

enum mode_class { MODE_RANDOM, MODE_INT, MODE_FLOAT,
		  MODE_COMPLEX_INT, MODE_COMPLEX_FLOAT, MODE_FUNCTION };

/* Get the general kind of object that mode MODE represents
   (integer, floating, complex, etc.)  */

extern enum mode_class mode_class[];
#define GET_MODE_CLASS(MODE)		(mode_class[(int)(MODE)])

/* Get the size in bytes of an object of mode MODE.  */

extern int mode_size[];
#define GET_MODE_SIZE(MODE)		(mode_size[(int)(MODE)])

/* Get the size in bytes of the basic parts of an object of mode MODE.  */

extern int mode_unit_size[];
#define GET_MODE_UNIT_SIZE(MODE)	(mode_unit_size[(int)(MODE)])

/* Get the size in bits of an object of mode MODE.  */

#define GET_MODE_BITSIZE(MODE)  (BITS_PER_UNIT * mode_size[(int)(MODE)])

/* Get a bitmask containing 1 for all bits in a word
   that fit within mode MODE.  */

#define GET_MODE_MASK(MODE)  \
   ((GET_MODE_BITSIZE (MODE) >= HOST_BITS_PER_INT)  \
    ? -1 : ((1 << GET_MODE_BITSIZE (MODE)) - 1))

/* Get the next wider natural mode (eg, QI -> HI -> SI -> DI -> TI).  */

extern enum machine_mode mode_wider_mode[];
#define GET_MODE_WIDER_MODE(MODE)	(mode_wider_mode[(int)(MODE)])

/* Common union for an element of an rtx.  */

typedef union rtunion_def
{
  int rtint;
  char *rtstr;
  struct rtx_def *rtx;
  struct rtvec_def *rtvec;
  enum machine_mode rttype;
} rtunion;

/* RTL expression ("rtx").  */

typedef struct rtx_def
{
#ifdef SHORT_ENUM_BUG
  unsigned short code;
#else
  /* The kind of expression this is.  */
  enum rtx_code code : 16;
#endif
  /* The kind of value the expression has.  */
  enum machine_mode mode : 8;
  /* 1 in an INSN if it can alter flow of control
     within this function.  Not yet used!  */
  unsigned int jump : 1;
  /* 1 in an INSN if it can call another function.  Not yet used!  */
  unsigned int call : 1;
  /* 1 in a MEM or REG if value of this expression will never change
     during the current function, even though it is not
     manifestly constant.
     1 in a SYMBOL_REF if it addresses something in the per-function
     constants pool.  */
  unsigned int unchanging : 1;
  /* 1 in a MEM expression if contents of memory are volatile.  */
  /* 1 in an INSN, CALL_INSN, JUMP_INSN, CODE_LABEL or BARRIER
     if it is deleted.  */
  /* 1 in a REG expression if corresponds to a variable declared by the user.
     0 for an internally generated temporary.  */
  unsigned int volatil : 1;
  /* 1 in a MEM referring to a field of a structure (not a union!).
     0 if the MEM was a variable or the result of a * operator in C;
     1 if it was the result of a . or -> operator (on a struct) in C.  */
  unsigned int in_struct : 1;
  /* 1 if this rtx is used.  This is used for copying shared structure.
     See `unshare_all_rtl'.
     This bit is used to detect that event.  */
  unsigned int used : 1;
  /* Nonzero if this rtx came from procedure integration.
     In a REG, nonzero means this reg refers to the return value
     of the current function.  */
  unsigned integrated : 1;
  /* The first element of the operands of this rtx.
     The number of operands and their types are controlled
     by the `code' field, according to rtl.def.  */
  rtunion fld[1];
} *rtx;

#define NULL_RTX (rtx) 0

/* Define macros to access the `code' field of the rtx.  */

#ifdef SHORT_ENUM_BUG
#define GET_CODE(RTX)		((enum rtx_code) ((RTX)->code))
#define PUT_CODE(RTX, CODE)	((RTX)->code = ((short) (CODE)))
#else
#define GET_CODE(RTX)		((RTX)->code)
#define PUT_CODE(RTX, CODE)	((RTX)->code = (CODE))
#endif

#define GET_MODE(RTX)		((RTX)->mode)
#define PUT_MODE(RTX, MODE)	((RTX)->mode = (MODE))

#define RTX_INTEGRATED_P(RTX) ((RTX)->integrated)
#define RTX_UNCHANGING_P(RTX) ((RTX)->unchanging)

/* RTL vector.  These appear inside RTX's when there is a need
   for a variable number of things.  The principle use is inside
   PARALLEL expressions.  */

typedef struct rtvec_def{
  unsigned num_elem;		/* number of elements */
  rtunion elem[1];
} *rtvec;

#define NULL_RTVEC (rtvec) 0

#define GET_NUM_ELEM(RTVEC)		((RTVEC)->num_elem)
#define PUT_NUM_ELEM(RTVEC, NUM)	((RTVEC)->num_elem = (unsigned) NUM)

/* 1 if X is a REG.  */

#define REG_P(X) (GET_CODE (X) == REG)

/* 1 if X is a constant value that is an integer.  */

#define CONSTANT_P(X)   \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT						\
   || GET_CODE (X) == CONST)

/* General accessor macros for accessing the fields of an rtx.  */

#define XEXP(RTX, N)	((RTX)->fld[N].rtx)
#define XINT(RTX, N)	((RTX)->fld[N].rtint)
#define XSTR(RTX, N)	((RTX)->fld[N].rtstr)
#define XVEC(RTX, N)	((RTX)->fld[N].rtvec)
#define XVECLEN(RTX, N)	((RTX)->fld[N].rtvec->num_elem)
#define XVECEXP(RTX,N,M)((RTX)->fld[N].rtvec->elem[M].rtx)

/* ACCESS MACROS for particular fields of insns.  */

/* Holds a unique number for each insn.
   These are not necessarily sequentially increasing.  */
#define INSN_UID(INSN)	((INSN)->fld[0].rtint)

/* Chain insns together in sequence.  */
#define PREV_INSN(INSN)	((INSN)->fld[1].rtx)
#define NEXT_INSN(INSN)	((INSN)->fld[2].rtx)

/* The body of an insn.  */
#define PATTERN(INSN)	((INSN)->fld[3].rtx)

/* Code number of instruction, from when it was recognized.
   -1 means this instruction has not been recognized yet.  */
#define INSN_CODE(INSN) ((INSN)->fld[4].rtint)

/* Set up in flow.c; empty before then.
   Holds a chain of INSN_LIST rtx's whose first operands point at
   previous insns with direct data-flow connections to this one.
   That means that those insns set variables whose next use is in this insn.
   They are always in the same basic block as this insn.  */
#define LOG_LINKS(INSN)		((INSN)->fld[5].rtx)

/* 1 if insn has been deleted.  */
#define INSN_DELETED_P(INSN) ((INSN)->volatil)

/* Holds a list of notes on what this insn does to various REGs.
   It is a chain of EXPR_LIST rtx's, where the second operand
   is the chain pointer and the first operand is the REG being described.
   The mode field of the EXPR_LIST contains not a real machine mode
   but a value that says what this note says about the REG:
     REG_DEAD means that the REG dies in this insn.
     REG_INC means that the REG is autoincremented or autodecremented.
   Note that one insn can have both REG_DEAD and REG_INC for the same register
   if the register is preincremented or predecremented in the insn
   and not needed afterward.  This can probably happen.
     REG_EQUIV describes the insn as a whole; it says that the
   insn sets a register to a constant value or to be equivalent to
   a memory address.  If the
   register is spilled to the stack then the constant value
   should be substituted for it.  The contents of the REG_EQUIV
   is the constant value or memory address, which may be different
   from the source of the SET although it has the same value. 
     REG_EQUAL is like REG_EQUIV except that the destination
   is only momentarily equal to the specified rtx.  Therefore, it
   cannot be used for substitution; but it can be used for cse.
     REG_RETVAL means that this insn copies the return-value of
   a library call out of the hard reg for return values.  This note
   is actually an INSN_LIST and it points to the first insn involved
   in setting up arguments for the call.  flow.c uses this to delete
   the entire library call when its result is dead.
     REG_LIBCALL is the inverse of REG_RETVAL: it goes on the first insn
   of the library call and points at the one that has the REG_RETVAL.
     REG_WAS_0 says that the register set in this insn held 0 before the insn.
   The contents of the note is the insn that stored the 0.
   If that insn is deleted or patched to a NOTE, the REG_WAS_0 is inoperative.
   The REG_WAS_0 note is actually an INSN_LIST, not an EXPR_LIST.
     REG_NONNEG means that the register is always nonnegative during
   the containing loop.  This is used in branches so that decrement and
   branch instructions terminating on zero can be matched.
     REG_UNSET identifies a pseudo-reg used in this insn and never set.  */

#define REG_NOTES(INSN)	((INSN)->fld[6].rtx)

/* Don't forget to change reg_note_name in rtl.c.  */
enum reg_note { REG_DEAD = 1, REG_INC = 2, REG_EQUIV = 3, REG_WAS_0 = 4,
		REG_EQUAL = 5, REG_RETVAL = 6, REG_LIBCALL = 7,
		REG_NONNEG = 8, REG_UNSET = 9 };

/* Extract the reg-note kind from an EXPR_LIST.  */
#define REG_NOTE_KIND(LINK) ((enum reg_note) GET_MODE (LINK))

/* Names for REG_NOTE's in EXPR_LIST insn's.  */

extern char *reg_note_name[];
#define GET_REG_NOTE_NAME(MODE) (reg_note_name[(int)(MODE)])

/* The label-number of a code-label.  The assembler label
   is made from `L' and the label-number printed in decimal.
   Label numbers are unique in a compilation.  */
#define CODE_LABEL_NUMBER(INSN)	((INSN)->fld[3].rtint)

#define LINE_NUMBER NOTE

/* In a NOTE that is a line number, this is a string for the file name
   that the line is in.  */

#define NOTE_SOURCE_FILE(INSN)  ((INSN)->fld[3].rtstr)

/* In a NOTE that is a line number, this is the line number.
   Other kinds of NOTEs are identified by negative numbers here.  */
#define NOTE_LINE_NUMBER(INSN) ((INSN)->fld[4].rtint)

/* Codes that appear in the NOTE_LINE_NUMBER field
   for kinds of notes that are not line numbers.  */

/* This note indicates the end of the real body of the function,
   after moving the parms into their homes, etc.  */
#define NOTE_INSN_FUNCTION_BEG 0

/* This note is used to get rid of an insn
   when it isn't safe to patch the insn out of the chain.  */
#define NOTE_INSN_DELETED -1
#define NOTE_INSN_BLOCK_BEG -2
#define NOTE_INSN_BLOCK_END -3
#define NOTE_INSN_LOOP_BEG -4
#define NOTE_INSN_LOOP_END -5
/* This kind of note is generated at the end of the function body,
   just before the return insn or return label.
   In an optimizing compilation it is deleted by the first jump optimization,
   after enabling that optimizer to determine whether control can fall
   off the end of the function body without a return statement.  */
#define NOTE_INSN_FUNCTION_END -6
/* This kind of note is generated just after each call to `setjmp', et al.  */
#define NOTE_INSN_SETJMP -7
/* Generated at the place in a loop that `continue' jumps to.  */
#define NOTE_INSN_LOOP_CONT -8
/* Don't forget to change note_insn_name in rtl.c.  */

#define NOTE_DECL_NAME(INSN) ((INSN)->fld[3].rtstr)
#define NOTE_DECL_CODE(INSN) ((INSN)->fld[4].rtint)
#define NOTE_DECL_RTL(INSN) ((INSN)->fld[5].rtx)
#define NOTE_DECL_IDENTIFIER(INSN) ((INSN)->fld[6].rtint)
#define NOTE_DECL_TYPE(INSN) ((INSN)->fld[7].rtint)

/* Names for NOTE insn's other than line numbers.  */

extern char *note_insn_name[];
#define GET_NOTE_INSN_NAME(NOTE_CODE) (note_insn_name[-(NOTE_CODE)])

/* In jump.c, each label contains a count of the number
   of LABEL_REFs that point at it, so unused labels can be deleted.  */
#define LABEL_NUSES(LABEL) ((LABEL)->fld[4].rtint)

/* In jump.c, each JUMP_INSN can point to a label that it can jump to,
   so that if the JUMP_INSN is deleted, the label's LABEL_NUSES can
   be decremented and possibly the label can be deleted.  */
#define JUMP_LABEL(INSN)   ((INSN)->fld[7].rtx)

/* Once basic blocks are found in flow.c,
   each CODE_LABEL starts a chain that goes through
   all the LABEL_REFs that jump to that label.
   The chain eventually winds up at the CODE_LABEL; it is circular.  */
#define LABEL_REFS(LABEL) ((LABEL)->fld[4].rtx)

/* This is the field in the LABEL_REF through which the circular chain
   of references to a particular label is linked.
   This chain is set up in flow.c.  */

#define LABEL_NEXTREF(REF) ((REF)->fld[1].rtx)

/* Once basic blocks are found in flow.c,
   Each LABEL_REF points to its containing instruction with this field.  */

#define CONTAINING_INSN(RTX) ((RTX)->fld[2].rtx)

/* For a REG rtx, REGNO extracts the register number.  */

#define REGNO(RTX) ((RTX)->fld[0].rtint)

/* For a REG rtx, REG_FUNCTION_VALUE_P is nonzero if the reg
   is the current function's return value.  */

#define REG_FUNCTION_VALUE_P(RTX) ((RTX)->integrated)

/* 1 in a REG rtx if it corresponds to a variable declared by the user.  */
#define REG_USERVAR_P(RTX) ((RTX)->volatil)

/* For a CONST_INT rtx, INTVAL extracts the integer.  */

#define INTVAL(RTX) ((RTX)->fld[0].rtint)

/* For a SUBREG rtx, SUBREG_REG extracts the value we want a subreg of.
   SUBREG_WORD extracts the word-number.  */

#define SUBREG_REG(RTX) ((RTX)->fld[0].rtx)
#define SUBREG_WORD(RTX) ((RTX)->fld[1].rtint)

/* Access various components of an ASM_OPERANDS rtx.  */

#define ASM_OPERANDS_TEMPLATE(RTX) XSTR ((RTX), 0)
#define ASM_OPERANDS_OUTPUT_CONSTRAINT(RTX) XSTR ((RTX), 1)
#define ASM_OPERANDS_OUTPUT_IDX(RTX) XINT ((RTX), 2)
#define ASM_OPERANDS_INPUT_VEC(RTX) XVEC ((RTX), 3)
#define ASM_OPERANDS_INPUT_CONSTRAINT_VEC(RTX) XVEC ((RTX), 4)
#define ASM_OPERANDS_INPUT(RTX, N) XVECEXP ((RTX), 3, (N))
#define ASM_OPERANDS_INPUT_CONSTRAINT(RTX, N) XSTR (XVECEXP ((RTX), 4, (N)), 0)
#define ASM_OPERANDS_INPUT_MODE(RTX, N) GET_MODE (XVECEXP ((RTX), 4, (N)))
#define ASM_OPERANDS_SOURCE_FILE(RTX) XSTR ((RTX), 5)
#define ASM_OPERANDS_SOURCE_LINE(RTX) XINT ((RTX), 6)

/* For a MEM rtx, 1 if it's a volatile reference.
   Also in an ASM_OPERANDS rtx.  */
#define MEM_VOLATILE_P(RTX) ((RTX)->volatil)

/* For a MEM rtx, 1 if it refers to a structure or union component.  */
#define MEM_IN_STRUCT_P(RTX) ((RTX)->in_struct)

/* For a SET rtx, SET_DEST is the place that is set
   and SET_SRC is the value it is set to.  */
#define SET_DEST(RTX) ((RTX)->fld[0].rtx)
#define SET_SRC(RTX) ((RTX)->fld[1].rtx)

/* 1 in a SYMBOL_REF if it addresses this function's constants pool.  */
#define CONSTANT_POOL_ADDRESS_P(RTX) ((RTX)->unchanging)
/* 1 in a SYMBOL_REF if it is the name of an external symbol.  */
#define EXTERNAL_SYMBOL_P(RTX) ((RTX)->volatil)

/* For an INLINE_HEADER rtx, FIRST_FUNCTION_INSN is the first insn
   of the function that is not involved in copying parameters to
   pseudo-registers.  FIRST_PARM_INSN is the very first insn of
   the function, including the parameter copying.
   We keep this around in case we must splice
   this function into the assembly code at the end of the file.
   FIRST_LABELNO is the first label number used by the function (inclusive).
   LAST_LABELNO is the last label used by the function (exclusive).
   MAX_REGNUM is the largest pseudo-register used by that function.

   We want this to lay down like an INSN.  The PREV_INSN field
   is always NULL.  The NEXT_INSN field always points to the
   first function insn of the function being squirreled away.  */

#define FIRST_FUNCTION_INSN(RTX) ((RTX)->fld[2].rtx)
#define FIRST_PARM_INSN(RTX) ((RTX)->fld[3].rtx)
#define FIRST_LABELNO(RTX) ((RTX)->fld[4].rtint)
#define LAST_LABELNO(RTX) ((RTX)->fld[5].rtint)
#define MAX_PARMREG(RTX) ((RTX)->fld[6].rtint)
#define MAX_REGNUM(RTX) ((RTX)->fld[7].rtint)
#define FUNCTION_ARGS_SIZE(RTX) ((RTX)->fld[8].rtint)

/* Generally useful functions.  */

extern rtx rtx_alloc ();
extern rtvec rtvec_alloc ();
extern rtx find_reg_note ();
extern rtx gen_rtx ();
extern rtx copy_rtx ();
extern rtvec gen_rtvec ();
extern rtvec gen_rtvec_v ();
extern rtx gen_reg_rtx ();
extern rtx gen_label_rtx ();
extern rtx gen_inline_header_rtx ();
extern rtx gen_lowpart ();
extern rtx gen_highpart ();
extern int subreg_lowpart_p ();
extern rtx make_safe_from ();
extern rtx memory_address ();
extern rtx get_insns ();
extern rtx get_last_insn ();
extern rtx start_sequence ();
extern rtx gen_sequence ();
extern rtx expand_expr ();
extern rtx output_constant_def ();
extern rtx immed_real_const ();
extern rtx immed_real_const_1 ();
extern rtx immed_double_const ();
extern rtx force_const_double_mem ();
extern rtx force_const_mem ();
extern rtx get_parm_real_loc ();
extern rtx assign_stack_local ();
extern rtx protect_from_queue ();
extern void emit_queue ();
extern rtx emit_move_insn ();
extern rtx emit_insn ();
extern rtx emit_jump_insn ();
extern rtx emit_call_insn ();
extern rtx emit_call_insn_before ();
extern rtx emit_insn_before ();
extern rtx emit_insn_after ();
extern rtx emit_label ();
extern rtx emit_barrier ();
extern rtx emit_barrier_after ();
extern rtx emit_note ();
extern rtx emit_line_note ();
extern rtx emit_line_note_force ();
extern rtx prev_real_insn ();
extern rtx next_real_insn ();
extern rtx next_nondeleted_insn ();
extern rtx plus_constant ();
extern rtx find_equiv_reg ();
extern rtx delete_insn ();
extern rtx adj_offsettable_operand ();

/* Maximum number of parallel sets and clobbers in any insn in this fn.
   Always at least 3, since the combiner could put that many togetherm
   and we want this to remain correct for all the remaining passes.  */

extern int max_parallel;

extern int asm_noperands ();
extern char *decode_asm_operands ();

#ifdef BITS_PER_WORD
/* Conditional is to detect when config.h has been included.  */
extern enum reg_class reg_preferred_class ();
#endif

extern rtx get_first_nonparm_insn ();

/* Standard pieces of rtx, to be substituted directly into things.  */
extern rtx pc_rtx;
extern rtx cc0_rtx;
extern rtx const0_rtx;
extern rtx const1_rtx;
extern rtx fconst0_rtx;
extern rtx dconst0_rtx;

/* Returns a constant 0 rtx in mode MODE.  */

#define CONST0_RTX(MODE) \
 ((MODE == SFmode) ? fconst0_rtx			\
  : ((MODE == DFmode) ? dconst0_rtx			\
     : ((GET_MODE_CLASS (MODE) == MODE_INT) ? const0_rtx	\
        : (abort (), NULL_RTX))))

/* All references to certain hard regs, except those created
   by allocating pseudo regs into them (when that's possible),
   go through these unique rtx objects.  */
extern rtx stack_pointer_rtx;
extern rtx frame_pointer_rtx;
extern rtx arg_pointer_rtx;
extern rtx struct_value_rtx;
extern rtx struct_value_incoming_rtx;
extern rtx static_chain_rtx;
extern rtx static_chain_incoming_rtx;
