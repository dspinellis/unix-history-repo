/* Subroutines for insn-output.c for MIPS
   Contributed by A. Lichnewsky, lich@inria.inria.fr.
   Changes by     Michael Meissner, meissner@osf.org.
   Copyright (C) 1989, 1990 Free Software Foundation, Inc.

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


#include <stdio.h>
#include "tree.h"
#include "flags.h"

extern void  debug_rtx ();
extern void  abort_with_insn ();

extern FILE *asm_out_file;
extern tree current_function_decl;

/* Global variables for machine-dependent things.  */

char *reg_numchar[] = REGISTER_NUMCHAR;

/* Threshold for data being put into the small data/bss area, instead
   of the normal data area (references to the small data/bss area take
   1 instruction, and use the global pointer, references to the normal
   data area takes 2 instructions).  */
int mips_section_threshold = -1;

/* Count the number of .file directives, so that .loc is up to date.  */
int num_source_filenames = 0;

/* Count the number of words that are pushed to pass arguments.  */
int stack_args_pushed = 0;

/* # bytes for args preallocated by function_prolog. */
int stack_args_preallocated = 0;

/* Count of the number of functions created so far, in order to make
   unique labels for omitting the frame pointer.  */
int number_functions_processed = 0;

/* Count the number of sdb related labels are generated (to find block
   start and end boundaries).  */
int sdb_label_count = 0;

/* Next label # for each statment for Silicon Graphics IRIS systems. */
int sym_lineno = 0;

/* Non-zero if inside of a function, because the stupid MIPS asm can't
   handle .files inside of functions.  */
int inside_function = 0;

/* String to be used for the unique name given to the difference between
   the stack pointer and frame pointer when the frame pointer is to be
   omitted.  */
char *sp_fp_difference = 0;

/* Files to separate the text and the data output, so that all of the data
   can be emitted before the text, which will mean that the assembler will
   generate smaller code, based on the global pointer.  */
FILE *asm_out_data_file;
FILE *asm_out_text_file;

/* Linked list of all externals that are to be emitted when optimizing
   for the global pointer if they haven't been declared by the end of
   the program with an appropriate .comm or initialization.  */

struct extern_list {
  struct extern_list *next;	/* next external */
  char *name;			/* name of the external */
  int size;			/* size in bytes */
} *extern_head = 0;

/* Name of the current function.  */
char *current_function_name;

/* Size of the frame allocated for this function.  */
int current_function_total_framesize;

/* Number of bytes used to hold saved registers.  */
int current_function_saved_reg_size;

/* Return truth value of whether OP can be used as an operands
   where a register or 16 bit unsigned integer is needed.  */

int
uns_arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && SMALL_INT_UNSIGNED (op)));
}

/* Return truth value of whether OP can be used as an operands
   where a 16 bit integer is needed  */

int
arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && SMALL_INT (op)));
}

/* Return truth value of whether OP can be used as an operand in a two
   address arithmetic insn (such as set 123456,%o4) of mode MODE.  */

int
arith32_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode) || GET_CODE (op) == CONST_INT);
}

/* Return truth value of whether OP is a integer which fits in 16 bits  */

int
small_int (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && SMALL_INT (op));
}


/* Argument support functions.  */

/* Initialize CUMULATIVE_ARGS for a function.  */

void
init_cumulative_args (cum, fntype)
     CUMULATIVE_ARGS cum;	/* argument info to initialize */
     tree fntype;		/* tree ptr for function decl */
{
  tree param, next_param;

  if (TARGET_DEBUGE_MODE)
    {
      fprintf (stderr, "\ninit_cumulative_args\n");
      if (fntype != (tree)0)
	{
	  putc ('\n', stderr);
	  debug_tree (fntype);
	  putc ('\n', stderr);
	}
    }

  cum->gp_reg_found = 0;
  cum->arg_number = 0;
  cum->arg_words = 0;

  /* Determine if this function has variable arguments.  This is
     indicated by the last argument being 'void_type_mode' if there
     are no variable arguments.  The standard MIPS calling sequence
     passes all arguments in the general purpose registers in this
     case. */

  for (param = (fntype) ? TYPE_ARG_TYPES (fntype) : 0;
       param != (tree)0;
       param = next_param)
    {
      next_param = TREE_CHAIN (param);
      if (next_param == (tree)0 && TREE_VALUE (param) != void_type_node)
	cum->gp_reg_found = 1;
    }

  /* Determine if the function is returning a structure, if so,
     advance by one argument.  */

  if (fntype
      && (TREE_CODE (fntype) == FUNCTION_TYPE || TREE_CODE (fntype) == METHOD_TYPE)
      && TREE_TYPE (fntype) != 0)
    {
      tree ret_type = TREE_TYPE (fntype);
      enum tree_code ret_code = TREE_CODE (ret_type);

      if (ret_code == RECORD_TYPE || ret_code == UNION_TYPE)
	{
	  cum->gp_reg_found = 1;
	  cum->arg_number = 1;
	  cum->arg_words = 1;
	}
    }
}

/* Advance the argument to the next argument position.  */

void
function_arg_advance (cum, mode, type, named)
     CUMULATIVE_ARGS cum;	/* current arg information */
     enum machine_mode mode;	/* current arg mode */
     tree type;			/* type of the argument or 0 if lib support */
{
  if (TARGET_DEBUGE_MODE)
    fprintf (stderr,
	     "function_adv( {gp reg found = %d, arg # = %2d, words = %2d}, %4s, 0x%.8x, %d )\n",
	     cum->gp_reg_found, cum->arg_number, cum->arg_words, GET_MODE_NAME (mode),
	     type, named);

  cum->arg_number++;
  switch (mode)
    {
    default:
      error ("Illegal mode given to function_arg_advance");
      break;

    case VOIDmode:
      break;

    case BLKmode:
      cum->gp_reg_found = 1;
      cum->arg_words += (int_size_in_bytes (type) + 3) / 4;
      break;

    case SFmode:
      cum->arg_words++;
      break;

    case DFmode:
      cum->arg_words += 2;
      break;

    case DImode:
      cum->gp_reg_found = 1;
      cum->arg_words += 2;
      break;

    case QImode:
    case HImode:
    case SImode:
      cum->gp_reg_found = 1;
      cum->arg_words++;
      break;
    }
}

/* Return a RTL expression containing the register for the given mode,
   or 0 if the argument is too be passed on the stack.  */

struct rtx_def *
function_arg (cum, mode, type, named)
     CUMULATIVE_ARGS cum;	/* current arg information */
     enum machine_mode mode;	/* current arg mode */
     tree type;			/* type of the argument or 0 if lib support */
     int named;			/* != 0 for normal args, == 0 for ... args */
{
  int regbase = -1;
  int bias = 0;

  if (TARGET_DEBUGE_MODE)
    fprintf (stderr,
	     "function_arg( {gp reg found = %d, arg # = %2d, words = %2d}, %4s, 0x%.8x, %d ) = ",
	     cum->gp_reg_found, cum->arg_number, cum->arg_words, GET_MODE_NAME (mode),
	     type, named);

  switch (mode)
    {
    default:
      error ("Illegal mode given to function_arg");
      break;

    case SFmode:
      if (cum->gp_reg_found || cum->arg_number >= 2)
	regbase = GP_ARG_FIRST;
      else {
	regbase = FP_ARG_FIRST;
	if (cum->arg_words == 1)	/* first arg was float */
	  bias = 1;			/* use correct reg */
      }

      break;

    case DFmode:
      cum->arg_words += (cum->arg_words & 1);
      regbase = (cum->gp_reg_found) ? GP_ARG_FIRST : FP_ARG_FIRST;
      break;

    case VOIDmode:
    case BLKmode:
    case QImode:
    case HImode:
    case SImode:
    case DImode:
      regbase = GP_ARG_FIRST;
      break;
    }

  if (cum->arg_words >= MAX_ARGS_IN_REGISTERS)
    {
      if (TARGET_DEBUGE_MODE)
	fprintf (stderr, "<stack>\n");

      return 0;
    }

  if (regbase == -1)
    abort ();

  if (TARGET_DEBUGE_MODE)
    fprintf (stderr, "%s\n", reg_numchar[ regbase + cum->arg_number + bias ]);

  return gen_rtx (REG, mode, regbase + cum->arg_words + bias);
}


int
function_arg_partial_nregs (cum, mode, type, named)
     CUMULATIVE_ARGS cum;	/* current arg information */
     enum machine_mode mode;	/* current arg mode */
     tree type;			/* type of the argument or 0 if lib support */
     int named;			/* != 0 for normal args, == 0 for ... args */
{
  if (mode == BLKmode && cum->arg_number < MAX_ARGS_IN_REGISTERS)
    {
      int words = (int_size_in_bytes (type) + 3) / 4;

      if (words + cum->arg_words < MAX_ARGS_IN_REGISTERS)
	return 0;		/* structure fits in registers */

      if (TARGET_DEBUGE_MODE)
	fprintf (stderr, "function_arg_partial_nregs = %d\n",
		 MAX_ARGS_IN_REGISTERS - cum->arg_words);

      return MAX_ARGS_IN_REGISTERS - cum->arg_words;
    }

  else if (mode == DImode && cum->arg_number == MAX_ARGS_IN_REGISTERS-1)
    {
      if (TARGET_DEBUGE_MODE)
	fprintf (stderr, "function_arg_partial_nregs = 1\n");

      return 1;
    }

  return 0;
}


/* Routines to merge the compare and branch operators into a single entity.  */

static  rtx branch_cmp_op[2];
static  enum machine_mode branch_cmp_mode;

/* Save the mode and operands on the current compare operator.  */

void
compare_collect (mode, op0, op1)
     enum machine_mode mode;
     rtx op0;
     rtx op1;
{
  if (TARGET_DEBUGD_MODE)
    {
      fprintf (stderr, "compare_collect mode = %s, operands::",
	       GET_MODE_NAME (mode));
      debug_rtx (op0);
      debug_rtx (op1);
    }
  branch_cmp_op[0] = op0;
  branch_cmp_op[1] = op1;
  branch_cmp_mode = mode;
}

/* Return the mode and operands saved with compare_collect for use
   in a branch operator.  */

void
compare_restore (operands, mode, insn)
     rtx *operands;
     enum machine_mode *mode;
     rtx insn;
{
  if (!branch_cmp_op[0] || !branch_cmp_op[1])
    abort_with_insn (insn, "Compare_restore did not follow compare_collect");

  if (TARGET_DEBUGD_MODE)
    {
      fprintf (stderr,
	       "compare_restore returning mode = %s, operands:%X,%X:",
	       GET_MODE_NAME (branch_cmp_mode),
	       branch_cmp_op[0],
	       branch_cmp_op[1]);

      debug_rtx (branch_cmp_op[0]);
      debug_rtx (branch_cmp_op[1]);
    }

  operands[0] = branch_cmp_op[0];
  operands[1] = branch_cmp_op[1];
  *mode = branch_cmp_mode;

  /* If the next insn is not a JUMP (after accounting for line numbers),
     zero out the branch_cmp_array.  Switch statements implemented as if's
     tend to have multiple jumps.  */
  do
    {
      insn = NEXT_INSN (insn);
    }
  while (insn && GET_CODE (insn) == NOTE);

  if (!insn || GET_CODE (insn) != JUMP_INSN)
    {
      branch_cmp_op[0] = NULL;
      branch_cmp_op[1] = NULL;
      branch_cmp_mode = VOIDmode;
    }

}

/* Print the options used in the assembly file.  */

static struct {char *name; int value;} target_switches []
  = TARGET_SWITCHES;

void
print_options (out)
     FILE *out;
{
  int line_len;
  int len;
  int j;
  char **p;
  int mask = TARGET_DEFAULT;
  extern char **save_argv;
  extern char *version_string, *language_string;

#if 0
  /* Allow assembly language comparisons with -mdebug eliminating the
     compiler version number and switch lists.  */
  if (!TARGET_DEBUG_MODE)
    {
      fprintf (out, "\n # %s %s", language_string, version_string);
#ifdef TARGET_VERSION_INTERNAL
      TARGET_VERSION_INTERNAL (out);
#endif
#ifdef __GNUC__
      fprintf (out, " compiled by GNU C\n\n");
#else
      fprintf (out, " compiled by CC\n\n");
#endif

      fprintf (out, " # Cc1 defaults:");
      line_len = 32767;
      for (j = 0; j < sizeof target_switches / sizeof target_switches[0]; j++)
	if (target_switches[j].name[0] != '\0'
	    && target_switches[j].value > 0
	    && (target_switches[j].value & mask) == target_switches[j].value)
	  {
	    len = strlen (target_switches[j].name) + 1;
	    if (len + line_len > 79)
	      {
		line_len = 2;
		fputs ("\n #", out);
	      }
	    fprintf (out, " -m%s", target_switches[j].name);
	    line_len += len;
	  }

      fprintf (out, "\n\n # Cc1 arguments (-G value = %d):",
	       mips_section_threshold);

      line_len = 32767;
      for (p = &save_argv[1]; *p != (char *)0; p++)
	if (**p == '-')
	  {
	    len = strlen (*p) + 1;
	    if (len + line_len > 79)
	      {
		line_len = 2;
		fputs ("\n #", out);
	      }
	    fprintf (out, " %s", *p);
	    line_len += len;
	  }
      fputs ("\n\n", out);
    }

#endif
}



/* Abort after printing out a specific insn.  */

void
abort_with_insn (insn, reason)
     rtx insn;
     char *reason;
{
  error (reason);
  debug_rtx (insn);
  abort ();
}

/* Write a message to stderr (for use in macros expanded in files that do not
   include stdio.h).  */

void
trace (s, s1, s2)
     char *s, *s1, *s2;
{
  fprintf (stderr, s, s1, s2);
}


/* Set up the threshold for data to go into the small data area, instead
   of the normal data area, and detect any conflicts in the switches.  */

void
overide_options ()
{
  register int i;

  i = TARGET_GVALUE;
  if (i >= 6)
    i += 3;
  mips_section_threshold = (i != 0) ? 1 << i : 0;
}

/* If optimizing for the global pointer, keep track of all of
   the externs, so that at the end of the file, we can emit
   the appropriate .extern declaration for them, before writing
   out the text section.  We assume that all names passed to
   us are in the permanent obstack, so that they will be valid
   at the end of the compilation.

   If we have -G 0, or the extern size is unknown, don't bother
   emitting the .externs.  */

int
mips_output_external (file, decl, name)
     FILE *file;
     tree decl;
     char *name;
{
  extern char *permalloc ();
  register struct extern_list *p;
  int len;

  if (TARGET_GP_OPT
      && mips_section_threshold != 0
      && ((TREE_CODE (decl)) != FUNCTION_DECL)
      && ((len = int_size_in_bytes (TREE_TYPE (decl))) > 0))
    {
      p = (struct extern_list *)permalloc ((long) sizeof (struct extern_list));
      p->next = extern_head;
      p->name = name;
      p->size = len;
      extern_head = p;
    }
  return 0;
}

/* If we are optimizing the global pointer, emit the text section now
   and any small externs which did not have .comm, etc that are
   needed.  Also, give a warning if the data area is more than 32K and
   -pic because 3 instructions are needed to reference the data
   pointers.  */

int
mips_asm_file_end (file)
     FILE *file;
{
  char buffer[8192];
  tree name_tree;
  struct extern_list *p;
  int len;
  extern tree lookup_name ();

  if (TARGET_GP_OPT)
    {
      if (extern_head)
	fputs ("\n", file);

      for (p = extern_head; p != 0; p = p->next)
	{
	  name_tree = get_identifier (p->name);
	  if (!TREE_ADDRESSABLE (name_tree))
	    {
	      TREE_ADDRESSABLE (name_tree) = 1;
	      fprintf (file, "\t.extern\t%s, %d\n", p->name, p->size);
	    }
	}

      fprintf (file, "\n\t.text\n");
      rewind (asm_out_text_file);
      if (ferror (asm_out_text_file))
	fatal_io_error ("write of text assembly file in mips_asm_file_end");

      while ((len = fread (buffer, 1, sizeof (buffer), asm_out_text_file)) > 0)
	if (fwrite (buffer, 1, len, file) != len)
	  pfatal_with_name ("write of final assembly file in mips_asm_file_end");

      if (len < 0)
	pfatal_with_name ("read of text assembly file in mips_asm_file_end");

      if (fclose (asm_out_text_file) != 0)
	pfatal_with_name ("close of tempfile in mips_asm_file_end");
    }
}

/* Fix references to the frame pointer to be off of the stack pointer.  */

struct rtx_def *
mips_fix_frame_pointer (oldaddr, depth)
     rtx oldaddr;
     int depth;
{
  rtx newaddr;
  rtx sp_diff_rtx;
  char temp[40];
  int frame_offset = 0;
  extern rtx eliminate_constant_term ();

  newaddr = eliminate_constant_term (oldaddr, &frame_offset);
  if (newaddr != frame_pointer_rtx)
    return oldaddr;

  if (sp_fp_difference == (char *)0)
    {
      sprintf (temp, "$Ls%d", number_functions_processed);
      sp_fp_difference = IDENTIFIER_POINTER (get_identifier (temp));
    }

  sp_diff_rtx = gen_rtx (SYMBOL_REF, SImode, sp_fp_difference);
  if (frame_offset + depth == 0)
    newaddr = gen_rtx (PLUS, Pmode, stack_pointer_rtx, sp_diff_rtx);
  else
    newaddr = gen_rtx (PLUS, Pmode,
		       stack_pointer_rtx,
		       gen_rtx (CONST, Pmode,
				gen_rtx (PLUS, Pmode,
					 sp_diff_rtx,
					 gen_rtx (CONST_INT, VOIDmode,
						  frame_offset + depth))));

  if (TARGET_DEBUGC_MODE)
    {
      fprintf (stderr,
	       "\n==================== FIX_FRAME, depth = %d, sp prealloc = %d, offset = %d\n",
	       depth, stack_args_preallocated, frame_offset);

      fprintf (stderr, "old INSN:");
      debug_rtx (oldaddr);

      fprintf (stderr, "\nnew INSN:");
      debug_rtx (newaddr);
    }

  return newaddr;
}


/* Set up the stack and frame (if desired) for the function.  */

void
function_prologue (file, size)
     FILE *file;
     int size;
{
  extern char call_used_regs[];
  extern char *reg_numchar[];
  extern tree current_function_decl;
  int regno;
  int mask;
  int fmask;
  int push_loc;
  int tsize;
  int num_regs;
  char **reg_name_ptr = (TARGET_NAME_REGS) ? reg_names : reg_numchar;
  char *base_str;
  char *sp_str = reg_name_ptr[STACK_POINTER_REGNUM];
  char *fp_str = (!frame_pointer_needed)
			? sp_str
			: reg_name_ptr[FRAME_POINTER_REGNUM];
  tree fndecl = current_function_decl; /* current... is tooo long */
  tree fntype = TREE_TYPE (fndecl);
  tree fnargs = (TREE_CODE (fntype) != METHOD_TYPE)
			? DECL_ARGUMENTS (fndecl)
			: 0;
  tree next_arg;
  tree cur_arg;
  char *arg_name = (char *)0;
  CUMULATIVE_ARGS args_so_far;


  inside_function = 1;


  if (write_symbols != NO_DEBUG)
    ASM_OUTPUT_SOURCE_LINE (file,
			    DECL_SOURCE_LINE (current_function_decl));

  fprintf (file, "\t.ent\t%s\n%s:\n", current_function_name,
	   current_function_name);

  fprintf (file, " #PROLOGUE\n");

  /* Determine the last argument, and get it's name.  */
  for (cur_arg = fnargs; cur_arg != (tree)0; cur_arg = next_arg)
    {
      next_arg = TREE_CHAIN (cur_arg);
      if (next_arg == (tree)0)
	{
	  if (DECL_NAME (cur_arg))
	    arg_name = IDENTIFIER_POINTER (DECL_NAME (cur_arg));

	  break;
	}
    }

  /* If this function is a varargs function, store any registers that
     would normally hold arguments ($4 - $7) on the stack.  */
  if ((TYPE_ARG_TYPES (fntype) != 0
       && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype))) != void_type_node))
      || (arg_name
	  && (strcmp (arg_name, "__builtin_va_alist") == 0
	      || strcmp (arg_name, "va_alist") == 0)))
    {
      tree parm;

      regno = 4;
      INIT_CUMULATIVE_ARGS (args_so_far, fntype);
          
      for (parm = fnargs; (parm && (regno <= 7)); parm = TREE_CHAIN (parm))
	{
	  rtx entry_parm;
	  enum machine_mode passed_mode;
	  tree type;

	  type = DECL_ARG_TYPE (parm);
	  passed_mode = TYPE_MODE (type);
	  entry_parm = FUNCTION_ARG (args_so_far, passed_mode,
				     DECL_ARG_TYPE (parm), 1);

	  if (entry_parm)
	    {
	      int words;

	      /* passed in a register, so will get homed automatically */
	      if (GET_MODE (entry_parm) == BLKmode)
		words = (int_size_in_bytes (type) + 3) / 4;
	      else
		words = (GET_MODE_SIZE (GET_MODE (entry_parm)) + 3) / 4;

	      regno = REGNO (entry_parm) + words - 1;
	    }
	  else
	    {
	      regno = 8;
	      break;
	    }

	  FUNCTION_ARG_ADVANCE (args_so_far, passed_mode,
				DECL_ARG_TYPE (parm), 1);
	}

      switch (regno)
	{
	case 4:
	  fprintf(file, "\tsd\t%s,0(%s)\t#varargs: home regs 4-5\n",
		  reg_name_ptr[4], sp_str);

	  fprintf(file, "\tsd\t%s,8(%s)\t#varargs: home regs 6-7\n",
		  reg_name_ptr[6], sp_str);
	  break;

	case 5:
	  fprintf(file, "\tsw\t%s,4(%s)\t#varargs: home reg  5\n",
		  reg_name_ptr[5], sp_str);

	  fprintf(file, "\tsd\t%s,8(%s)\t#varargs: home regs 6-7\n",
		  reg_name_ptr[6], sp_str);
	  break;

	case 6:
	  fprintf(file, "\tsd\t%s,8(%s)\t#varargs: home regs 6-7\n",
		  reg_name_ptr[6], sp_str);
	  break;

	case 7: 
	  fprintf(file, "\tsw\t%s,12(%s)\t#varargs: home reg 7\n",
		  reg_name_ptr[7], sp_str);
	  break;

	default:
	  break;
	}
    }

  mask = 0;
  fmask = 0;
  num_regs = 0;
  push_loc = stack_args_preallocated;
  tsize = AL_ADJUST_ALIGN (size) + stack_args_preallocated;

  for (regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    if (MUST_SAVE_REGISTER (regno))
      {
	tsize += 4;
	num_regs += 4;
	mask |= 1 << (regno - GP_REG_FIRST);
      }

  tsize = AL_ADJUST_ALIGN (tsize);
  num_regs = AL_ADJUST_ALIGN (num_regs);
  for (regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno += 2)
    if (regs_ever_live[regno] && !call_used_regs[regno])
      {
	tsize += 8;
	num_regs += 8;
	fmask |= 1 << (regno - FP_REG_FIRST);
      }

  if (tsize)
    tsize -= STARTING_FRAME_OFFSET;


  if (!frame_pointer_needed && sp_fp_difference != (char *)0)
    fprintf (file,"%s\t= %d\t\t\t#Difference between SP & FP\n\n",
	     sp_fp_difference, tsize);

  current_function_total_framesize = tsize;
  current_function_saved_reg_size = num_regs;
  if (tsize > 0)
    {
      if (tsize <= 32767)
	fprintf (file,
		 "\tsubu\t%s,%s,%d\t# temp= %d, regs= %d, args= %d, sfo= %d\n",
		 sp_str, sp_str, tsize, size, num_regs,
		 stack_args_preallocated, STARTING_FRAME_OFFSET);
      else
	fprintf (file,
		 "\tli\t%s,%d\n\tsubu\t%s,%s,%s\t# temp= %d, regs= %d, args= %d, sfo= %d\n",
		 reg_name_ptr[MIPS_TEMP1_REGNUM], tsize, sp_str, sp_str,
		 reg_name_ptr[MIPS_TEMP1_REGNUM], size, num_regs,
		 stack_args_preallocated, STARTING_FRAME_OFFSET);
    }

  fprintf (file, "\t.frame\t%s,%d,%s\n", fp_str,
	   (frame_pointer_needed) ? 0 : tsize,
	   reg_name_ptr[31]);

  if (push_loc > 32767 && num_regs > 0)
    {
      if ((tsize - (push_loc + num_regs)) <= 32767)
	{
	  base_str = reg_name_ptr[MIPS_TEMP1_REGNUM];
	  push_loc = tsize - push_loc;
	}
      else 
	{
	  base_str = reg_name_ptr[MIPS_TEMP2_REGNUM];
	  fprintf (file, "\tli\t%s,%d\n", base_str, push_loc);
	  push_loc = 0;
	}
    }
  else
    base_str = sp_str;

  for  (regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    if ((mask & (1 << (regno - GP_REG_FIRST))) != 0)
      {
	fprintf (file, "\tsw\t%s,%d(%s)\n", reg_name_ptr[regno], push_loc,
		 base_str);
	push_loc += 4;
      }

  fprintf (file, "\t.mask\t0x%08x,%d\n", mask, push_loc - tsize - 4);

  push_loc = AL_ADJUST_ALIGN (push_loc);
  for  (regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno += 2)
    if ((fmask & (1 << (regno - FP_REG_FIRST))) != 0)
      {
	fprintf (file, "\ts.d\t%s,%d(%s)\n", reg_name_ptr[regno], push_loc,
		 base_str);
	push_loc += 8;
      }

  fprintf (file, "\t.fmask\t0x%08x,%d\n", fmask, push_loc - tsize - 4);

  if (frame_pointer_needed)
    {
      if (tsize <= 32767)
	fprintf (file, "\taddu\t%s,%s,%d\t# set up frame pointer\n", fp_str, sp_str, tsize);
      else
	fprintf (file, "\taddu\t%s,%s,%s\t# set up frame pointer\n", fp_str, sp_str,
		 reg_name_ptr[MIPS_TEMP1_REGNUM]);
    }

  fprintf (file," #END PROLOGUE\n");
}


/* Do any necessary cleanup after a function to restore stack, frame, and regs. */

void
function_epilogue (file, size)
     FILE *file;
     int size;
{
  extern FILE *asm_out_data_file, *asm_out_file;
  extern char call_used_regs[];
  extern char *reg_numchar[];
  extern char *current_function_name;
  extern int frame_pointer_needed;
  int regno;
  int push_loc = stack_args_preallocated;
  int tsize = current_function_total_framesize;
  int num_regs = current_function_saved_reg_size;
  char **reg_name_ptr = (TARGET_NAME_REGS) ? reg_names : reg_numchar;
  char *sp_str = reg_name_ptr[STACK_POINTER_REGNUM];
  char *t1_str = reg_name_ptr[MIPS_TEMP1_REGNUM];
  char *base_str;


  fprintf (file," #EPILOGUE\n");

  if (tsize > 32767)
    fprintf (file, "\tli\t%s,%d\n", t1_str, tsize);

  if (frame_pointer_needed)
    {
      char *fp_str = reg_name_ptr[FRAME_POINTER_REGNUM];
      if (tsize > 32767)
	fprintf (file,"\tsubu\t%s,%s,%s\t# sp not trusted  here\n",
		 sp_str, fp_str, t1_str);
      else
	fprintf (file,"\tsubu\t%s,%s,%d\t# sp not trusted  here\n",
		 sp_str, fp_str, tsize);
    }

  if (push_loc > 32767 && num_regs > 0)
    {
      if ((tsize - (push_loc + num_regs)) <= 32767)
	{
	  base_str = t1_str;
	  push_loc = tsize - push_loc;
	}
      else 
	{
	  base_str = reg_name_ptr[MIPS_TEMP2_REGNUM];
	  fprintf (file, "\tli\t%s,%d\n", base_str, push_loc);
	  push_loc = 0;
	}
    }
  else
    base_str = sp_str;

  for  (regno = GP_REG_FIRST; regno <= GP_REG_LAST; regno++)
    if (MUST_SAVE_REGISTER (regno))
      {
	fprintf (file,"\tlw\t%s,%d(%s)\n", reg_name_ptr[regno], push_loc,
		 base_str);
	push_loc += 4;
      }

  push_loc = AL_ADJUST_ALIGN (push_loc);
  for  (regno = FP_REG_FIRST; regno <= FP_REG_LAST; regno += 2)
    if  (regs_ever_live[regno] && !call_used_regs[regno])
      {
	fprintf (file, "\tl.d\t%s,%d(%s)\n", reg_name_ptr[regno], push_loc,
		 base_str);
	push_loc += 8;
      }

  if (tsize > 32767)
    fprintf (file, "\taddu\t%s,%s,%s\n", sp_str, sp_str, t1_str);

  else if (tsize > 0)
    fprintf (file, "\taddu\t%s,%s,%d\n", sp_str, sp_str, tsize);

  fprintf (file,"\tj\t%s\n", reg_name_ptr[31]);
  fprintf (file," #END EPILOGUE\n");
  fprintf (file,"\t.end\t%s\n", current_function_name);

  /* Reset state info for each function.  */
  stack_args_pushed = 0;
  stack_args_preallocated = 0;
  inside_function = 0;
  sp_fp_difference = (char *)0;
  number_functions_processed++;

  /* Restore the output file if optimizing the GP (optimizing the GP causes
     the text to be diverted to a tempfile, so that data decls come before
     references to the data).  */

  if (TARGET_GP_OPT)
    asm_out_file = asm_out_data_file;
}
