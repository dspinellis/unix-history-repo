/*-
 * This code is derived from software copyrighted by the Free Software
 * Foundation.
 *
 * Modified 1991 by Donn Seeley at UUNET Technologies, Inc.
 */

#ifndef lint
static char sccsid[] = "@(#)toplev.c	6.4 (Berkeley) 5/8/91";
#endif /* not lint */

/* Top level of GNU C compiler
   Copyright (C) 1987, 1988, 1989 Free Software Foundation, Inc.

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


/* This is the top level of cc1.
   It parses command args, opens files, invokes the various passes
   in the proper order, and counts the time used by each.
   Error messages and low-level interface to malloc also handled here.  */

#include "config.h"
#include <sys/types.h>
#include <stdio.h>
#include <signal.h>
#include <setjmp.h>

#include <sys/stat.h>

#ifdef USG
#undef FLOAT
#include <sys/param.h>
/* This is for hpux.  It is a real screw.  They should change hpux.  */
#undef FLOAT
#include <sys/times.h>
#include <time.h>   /* Correct for hpux at least.  Is it good on other USG?  */
#undef FFS  /* Some systems define this in param.h.  */
#else
#ifndef VMS
#include <sys/time.h>
#include <sys/resource.h>
#endif
#endif

#include "input.h"
#include "tree.h"
#include "c-tree.h"
#include "rtl.h"
#include "flags.h"

extern int yydebug;

extern FILE *finput;

extern int reload_completed;
extern int rtx_equal_function_value_matters;

extern void init_lex ();
extern void init_decl_processing ();
extern void init_tree ();
extern void init_rtl ();
extern void init_optabs ();
extern void init_reg_sets ();
extern void dump_flow_info ();
extern void dump_local_alloc ();

void rest_of_decl_compilation ();
void error ();
void error_with_file_and_line ();
void fancy_abort ();
void set_target_switch ();
void print_target_switch_defaults ();

/* Bit flags that specify the machine subtype we are compiling for.
   Bits are tested using macros TARGET_... defined in the tm-...h file
   and set by `-m...' switches.  */

int target_flags;

/* Name of current original source file (what was input to cpp).
   This comes from each #-command in the actual input.  */

char *input_filename;

/* Name of top-level original source file (what was input to cpp).
   This comes from the #-command at the beginning of the actual input.
   If there isn't any there, then this is the cc1 input file name.  */

char *main_input_filename;

/* Current line number in real source file.  */

int lineno;

/* Stack of currently pending input files.  */

struct file_stack *input_file_stack;

/* Incremented on each change to input_file_stack.  */
int input_file_stack_tick;

/* FUNCTION_DECL for function now being parsed or compiled.  */

extern tree current_function_decl;

/* Name to use as base of names for dump output files.  */

char *dump_base_name;

/* Flags saying which kinds of debugging dump have been requested.  */

int rtl_dump = 0;
int rtl_dump_and_exit = 0;
int jump_opt_dump = 0;
int cse_dump = 0;
int loop_dump = 0;
int flow_dump = 0;
int combine_dump = 0;
int local_reg_dump = 0;
int global_reg_dump = 0;
int jump2_opt_dump = 0;
int dbr_sched_dump = 0;

/* 1 => write gdb debugging output (using symout.c).  -g
   2 => write dbx debugging output (using dbxout.c).  -G
   3 => write sdb debugging output (using sdbout.c).  -g.  */

enum debugger write_symbols = NO_DEBUG;

/* Nonzero means can use our own extensions to DBX format.
   Relevant only with write_symbols == DBX_DEBUG.  */

int use_gdb_dbx_extensions;

/* Nonzero means do optimizations.  -opt.  */

int optimize = 0;

/* Nonzero means `char' should be signed.  */

int flag_signed_char;

/* Nonzero means give an enum type only as many bytes as it needs.  */

int flag_short_enums;

/* Nonzero for -fcaller-saves: allocate values in regs that need to
   be saved across function calls, if that produces overall better code.
   Optional now, so people can test it.  */

#ifdef DEFAULT_CALLER_SAVES
int flag_caller_saves = 1;
#else
int flag_caller_saves = 0;
#endif

/* Nonzero for -fpcc-struct-return: return values the same way PCC does.  */

int flag_pcc_struct_return = 0;

/* Nonzero for -fforce-mem: load memory value into a register
   before arithmetic on it.  This makes better cse but slower compilation.  */

int flag_force_mem = 0;

/* Nonzero for -fforce-addr: load memory address into a register before
   reference to memory.  This makes better cse but slower compilation.  */

int flag_force_addr = 0;

/* Nonzero for -fdefer-pop: don't pop args after each function call;
   instead save them up to pop many calls' args with one insns.  */

int flag_defer_pop = 1;

/* Nonzero for -ffloat-store: don't allocate floats and doubles
   in extended-precision registers.  */

int flag_float_store = 0;

/* Nonzero for -fcombine-regs:
   allow instruction combiner to combine an insn
   that just copies one reg to another.  */

int flag_combine_regs = 0;

/* Nonzero enables strength-reduction in loop.c.  */

int flag_strength_reduce = 0;

/* Nonzero for -fwritable-strings:
   store string constants in data segment and don't uniquize them.  */

int flag_writable_strings = 0;

/* Nonzero means don't put addresses of constant functions in registers.
   Used for compiling the Unix kernel, where strange substitutions are
   done on the assembly output.  */

int flag_no_function_cse = 0;

/* Nonzero for -fomit-frame-pointer:
   don't make a frame pointer in simple functions that don't require one.  */

int flag_omit_frame_pointer = 0;

/* Nonzero to inhibit use of define_optimization peephole opts.  */

int flag_no_peephole = 0;

/* Nonzero means all references through pointers are volatile.  */

int flag_volatile;

/* Nonzero means just do syntax checking; don't output anything.  */

int flag_syntax_only = 0;

/* Nonzero means do stupid register allocation.  -noreg.
   This and `optimize' are controlled by different switches in cc1,
   but normally cc controls them both with the -O switch.  */

int obey_regdecls = 0;

/* Don't print functions as they are compiled and don't print
   times taken by the various passes.  -quiet.  */

int quiet_flag = 0;

/* Don't print warning messages.  -w.  */

int inhibit_warnings = 0;

/* Do print extra warnings (such as for uninitialized variables).  -W.  */

int extra_warnings = 0;

/* Nonzero to warn about unused local variables.  */

int warn_unused;

/* Nonzero means warn about all declarations which shadow others.   */

int warn_shadow;

/* Warn if a switch on an enum fails to have a case for every enum value.  */

int warn_switch;

/* Nonzero means warn about any identifiers that match in the first N
   characters.  The value N is in `id_clash_len'.  */

int warn_id_clash;
int id_clash_len;

/* Number of error messages and warning messages so far.  */

int errorcount = 0;
int warningcount = 0;
int sorrycount = 0;

/* Name of program invoked, sans directories.  */

char *progname;

/* Nonzero if generating code to do profiling.  */

int profile_flag = 0;

/* Nonzero if generating code to do profiling on a line-by-line basis.  */

int profile_block_flag;

/* Nonzero for -pedantic switch: warn about anything
   that standard spec forbids.  */

int pedantic = 0;

/* Nonzero for -finline-functions: ok to inline functions that look like
   good inline candidates.  */

int flag_inline_functions;

/* Nonzero for -fkeep-inline-functions: even if we make a function
   go inline everywhere, keep its defintion around for debugging
   purposes.  */

int flag_keep_inline_functions;

/* Nonzero means make the text shared if supported.  */

int flag_shared_data;

/* Nonzero means schedule into delayed branch slots if supported.  */

int flag_delayed_branch;

/* Copy of arguments to main.  */
int save_argc;
char **save_argv;

/* Name for output file of assembly code, specified with -o.  */

char *asm_file_name;

/* Name for output file of GDB symbol segment, specified with -symout.  */

char *sym_file_name;

/* Table of language-independent -f options.
   STRING is the option name.  VARIABLE is the address of the variable.
   ON_VALUE is the value to store in VARIABLE
    if `-fSTRING' is seen as an option.
   (If `-fno-STRING' is seen as an option, the opposite value is stored.)  */

struct { char *string; int *variable; int on_value;} f_options[] =
{
  {"float-store", &flag_float_store, 1},
  {"volatile", &flag_volatile, 1},
  {"defer-pop", &flag_defer_pop, 1},
  {"omit-frame-pointer", &flag_omit_frame_pointer, 1},
  {"strength-reduce", &flag_strength_reduce, 1},
  {"writable-strings", &flag_writable_strings, 1},
  {"peephole", &flag_no_peephole, 0},
  {"force-mem", &flag_force_mem, 1},
  {"force-addr", &flag_force_addr, 1},
  {"combine-regs", &flag_combine_regs, 1},
  {"function-cse", &flag_no_function_cse, 0},
  {"inline-functions", &flag_inline_functions, 1},
  {"keep-inline-functions", &flag_keep_inline_functions, 1},
  {"syntax-only", &flag_syntax_only, 1},
  {"shared-data", &flag_shared_data, 1},
  {"caller-saves", &flag_caller_saves, 1},
  {"pcc-struct-return", &flag_pcc_struct_return, 1},
  {"delayed-branch", &flag_delayed_branch, 1}
};

/* Output files for assembler code (real compiler output)
   and debugging dumps.  */

FILE *asm_out_file;
FILE *rtl_dump_file;
FILE *jump_opt_dump_file;
FILE *cse_dump_file;
FILE *loop_dump_file;
FILE *flow_dump_file;
FILE *combine_dump_file;
FILE *local_reg_dump_file;
FILE *global_reg_dump_file;
FILE *jump2_opt_dump_file;
FILE *dbr_sched_dump_file;

/* Time accumulators, to count the total time spent in various passes.  */

int parse_time;
int varconst_time;
int integration_time;
int jump_time;
int cse_time;
int loop_time;
int flow_time;
int combine_time;
int local_alloc_time;
int global_alloc_time;
int dbr_sched_time;
int final_time;
int symout_time;
int dump_time;

/* Return time used so far, in microseconds.  */

int
gettime ()
{
#ifdef USG
  struct tms tms;
#else
#ifndef VMS
  struct rusage rusage;
#else /* VMS */
  struct
    {
      int proc_user_time;
      int proc_system_time;
      int child_user_time;
      int child_system_time;
    } vms_times;
#endif
#endif

  if (quiet_flag)
    return 0;

#ifdef USG
  times (&tms);
  return (tms.tms_utime + tms.tms_stime) * (1000000 / HZ);
#else
#ifndef VMS
  getrusage (0, &rusage);
  return (rusage.ru_utime.tv_sec * 1000000 + rusage.ru_utime.tv_usec
	  + rusage.ru_stime.tv_sec * 1000000 + rusage.ru_stime.tv_usec);
#else /* VMS */
  times (&vms_times);
  return (vms_times.proc_user_time + vms_times.proc_system_time) * 10000;
#endif
#endif
}

#define TIMEVAR(VAR, BODY)    \
do { int otime = gettime (); BODY; VAR += gettime () - otime; } while (0)

void
print_time (str, total)
     char *str;
     int total;
{
  fprintf (stderr,
	   "time in %s: %d.%06d\n",
	   str, total / 1000000, total % 1000000);
}

/* Count an error or warning.  Return 1 if the message should be printed.  */

int
count_error (warningp)
     int warningp;
{
  if (warningp && inhibit_warnings)
    return 0;

  if (warningp)
    warningcount++;
  else
    errorcount++;

  return 1;
}

/* Print a fatal error message.  NAME is the text.
   Also include a system error message based on `errno'.  */

void
pfatal_with_name (name)
     char *name;
{
  fprintf (stderr, "%s: ", progname);
  perror (name);
  exit (35);
}

void
fatal_io_error (name)
     char *name;
{
  fprintf (stderr, "%s: %s: I/O error\n", progname, name);
  exit (35);
}

void
fatal (s, v)
     char *s;
     int v;
{
  error (s, v);
  exit (34);
}

/* Called from insn-extract to give a better error message when we
   don't have an insn to match what we are looking for, rather
   than just calling abort().  */

void
fatal_insn_not_found (insn)
     rtx insn;
{
  error ("The following insn was not recognizable:", 0);
  debug_rtx (insn);
  abort ();
}

static int need_error_newline;

/* Function of last error message;
   more generally, function such that if next error message is in it
   then we don't have to mention the function name.  */
static tree last_error_function = NULL;

/* Used to detect when input_file_stack has changed since last described.  */
static int last_error_tick;

/* Called when the start of a function definition is parsed,
   this function prints on stderr the name of the function.  */

void
announce_function (decl)
     tree decl;
{
  if (! quiet_flag)
    {
      fprintf (stderr, " %s", DECL_PRINT_NAME (decl));
      fflush (stderr);
      need_error_newline = 1;
      last_error_function = current_function_decl;
    }
}

/* Prints out, if necessary, the name of the current function
   which caused an error.  Called from all error and warning functions.  */

void
report_error_function (file)
     char *file;
{
  struct file_stack *p;

  if (need_error_newline)
    {
      fprintf (stderr, "\n");
      need_error_newline = 0;
    }

  if (last_error_function != current_function_decl)
    {
      if (file)
	fprintf (stderr, "%s: ", file);

      if (current_function_decl == NULL)
	fprintf (stderr, "At top level:\n");
      else if (TREE_CODE (TREE_TYPE (current_function_decl)) == METHOD_TYPE)
	fprintf (stderr, "In method %s:\n",
		 DECL_PRINT_NAME (current_function_decl));
      else
	fprintf (stderr, "In function %s:\n",
		 DECL_PRINT_NAME (current_function_decl));

      last_error_function = current_function_decl;
    }
  if (input_file_stack && input_file_stack->next != 0
      && input_file_stack_tick != last_error_tick)
    {
      fprintf (stderr, "In file included");
      for (p = input_file_stack->next; p; p = p->next)
	{
	  fprintf (stderr, " from %s:%d", p->name, p->line);
	  if (p->next)
	    fprintf (stderr, ",");
	}
      fprintf (stderr, ":\n");
      last_error_tick = input_file_stack_tick;
    }
}

/* Report an error at the current line number.
   S and V are a string and an arg for `printf'.  */

void
error (s, v, v2)
     char *s;
     int v;			/* @@also used as pointer */
     int v2;			/* @@also used as pointer */
{
  error_with_file_and_line (input_filename, lineno, s, v, v2);
}

/* Report an error at line LINE of file FILE.
   S and V are a string and an arg for `printf'.  */

void
error_with_file_and_line (file, line, s, v, v2)
     char *file;
     int line;
     char *s;
     int v;
     int v2;
{
  count_error (0);

  report_error_function (file);

  if (file)
    fprintf (stderr, "%s:%d: ", file, line);
  else
    fprintf (stderr, "%s: ", progname);
  fprintf (stderr, s, v, v2);
  fprintf (stderr, "\n");
}

/* Report an error at the declaration DECL.
   S and V are a string and an arg which uses %s to substitute the declaration name.  */

void
error_with_decl (decl, s, v)
     tree decl;
     char *s;
     int v;
{
  count_error (0);

  report_error_function (DECL_SOURCE_FILE (decl));

  fprintf (stderr, "%s:%d: ",
	   DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl));

  if (DECL_PRINT_NAME (decl))
    fprintf (stderr, s, DECL_PRINT_NAME (decl), v);
  else if (DECL_NAME (decl))
    fprintf (stderr, s, IDENTIFIER_POINTER (DECL_NAME (decl)), v);
  else
    fprintf (stderr, s, "((anonymous))", v);
  fprintf (stderr, "\n");
}

/* Report an error at argument #NUM.
   S is a string that uses %s to substitute the error type.
   E is the string for the error type.  */
void
error_with_arg (s, e, num)
     char *s, *e;
     int num;
{
  char *w;
  static char argument[] = "argument #%d: ";

  if (num <= 0)
    {
      error (s, e);
      return;
    }

  w = alloca (strlen (s) + sizeof (argument));
  (void) strcat (strcpy (w, argument), s);
  error (w, num, e);
}

/* Report an error at the line number of the insn INSN.
   S and V are a string and an arg for `printf'.
   This is used only when INSN is an `asm' with operands,
   and each ASM_OPERANDS records its own source file and line.  */

void
error_for_asm (insn, s, v, v2)
     rtx insn;
     char *s;
     int v;			/* @@also used as pointer */
     int v2;			/* @@also used as pointer */
{
  rtx temp;
  char *filename;
  int line;
  rtx body = PATTERN (insn);
  rtx asmop;

  /* Find the (or one of the) ASM_OPERANDS in the insn.  */
  if (GET_CODE (body) == SET && GET_CODE (SET_SRC (body)) == ASM_OPERANDS)
    asmop = SET_SRC (body);
  else if (GET_CODE (body) == ASM_OPERANDS)
    asmop = body;
  else if (GET_CODE (body) == PARALLEL
	   && GET_CODE (XVECEXP (body, 0, 0)) == SET)
    asmop = SET_SRC (XVECEXP (body, 0, 0));
  else if (GET_CODE (body) == PARALLEL
	   && GET_CODE (XVECEXP (body, 0, 0)) == ASM_OPERANDS)
    asmop = XVECEXP (body, 0, 0);

  filename = ASM_OPERANDS_SOURCE_FILE (asmop);
  line = ASM_OPERANDS_SOURCE_LINE (asmop);

  error_with_file_and_line (filename, line, s, v, v2);
}

/* Report a warning at line LINE.
   S and V are a string and an arg for `printf'.  */

void
warning_with_file_and_line (file, line, s, v, v2)
     char *file;
     int line;
     char *s;
     int v;
     int v2;
{
  if (count_error (1) == 0)
    return;

  report_error_function (file);

  if (file)
    fprintf (stderr, "%s:%d: ", file, line);
  else
    fprintf (stderr, "%s: ", progname);

  fprintf (stderr, "warning: ");
  fprintf (stderr, s, v, v2);
  fprintf (stderr, "\n");
}

/* Report a warning at the current line number.
   S and V are a string and an arg for `printf'.  */

void
warning (s, v, v2)
     char *s;
     int v;			/* @@also used as pointer */
     int v2;
{
  warning_with_file_and_line (input_filename, lineno, s, v, v2);
}

/* Report a warning at the declaration DECL.
   S is string which uses %s to substitute the declaration name.
   V is a second parameter that S can refer to.  */

void
warning_with_decl (decl, s, v)
     tree decl;
     char *s;
     int v;
{
  if (count_error (1) == 0)
    return;

  report_error_function (DECL_SOURCE_FILE (decl));

  fprintf (stderr, "%s:%d: ",
	   DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl));

  fprintf (stderr, "warning: ");
  if (DECL_PRINT_NAME (decl))
    fprintf (stderr, s, DECL_PRINT_NAME (decl), v);
  else if (DECL_NAME (decl))
    fprintf (stderr, s, IDENTIFIER_POINTER (DECL_NAME (decl)), v);
  else
    fprintf (stderr, s, "((anonymous))", v);
  fprintf (stderr, "\n");
}

/* Report a warning at argument #NUM.
   S is a string that uses %s to substitute the error type.
   E is the string for the error type.  */
void
warning_with_arg (s, e, num)
     char *s, *e;
     int num;
{
  char *w;
  static char argument[] = "argument #%d: ";

  if (num <= 0)
    {
      warning (s, e);
      return;
    }

  w = alloca (strlen (s) + sizeof (argument));
  (void) strcat (strcpy (w, argument), s);
  warning (w, num, e);
}

/* Apologize for not implementing some feature.
   S, V, and V2 are a string and args for `printf'.  */

void
sorry (s, v, v2)
     char *s;
     int v, v2;
{
  sorrycount++;
  if (input_filename)
    fprintf (stderr, "%s:%d: ", input_filename, lineno);
  else
    fprintf (stderr, "%s: ", progname);

  fprintf (stderr, "sorry, not implemented: ");
  fprintf (stderr, s, v, v2);
  fprintf (stderr, "\n");
}

/* Apologize for not implementing some feature, then quit.
   S, V, and V2 are a string and args for `printf'.  */

void
really_sorry (s, v, v2)
     char *s;
     int v, v2;
{
  if (input_filename)
    fprintf (stderr, "%s:%d: ", input_filename, lineno);
  else
    fprintf (stderr, "c++: ");

  fprintf (stderr, "sorry, not implemented: ");
  fprintf (stderr, s, v, v2);
  fatal (" (fatal)\n");
}

/* More 'friendly' abort that prints the line and file.
   config.h can #define abort fancy_abort if you like that sort of thing.  */

void
fancy_abort ()
{
  fatal ("Internal gcc abort.");
}

/* When `malloc.c' is compiled with `rcheck' defined,
   it calls this function to report clobberage.  */

void
botch (s)
{
  abort ();
}

/* Same as `malloc' but report error if no memory available.  */

int
xmalloc (size)
     unsigned size;
{
  register int value = (int) malloc (size);
  if (value == 0)
    fatal ("Virtual memory exhausted.");
  return value;
}

/* Same as `realloc' but report error if no memory available.  */

int
xrealloc (ptr, size)
     char *ptr;
     int size;
{
  int result = realloc (ptr, size);
  if (!result)
    fatal ("Virtual memory exhausted.");
  return result;
}

/* Return the logarithm of X, base 2, considering X unsigned,
   if X is a power of 2.  Otherwise, returns -1.  */

int
exact_log2 (x)
     register unsigned int x;
{
  register int log = 0;
  for (log = 0; log < HOST_BITS_PER_INT; log++)
    if (x == (1 << log))
      return log;
  return -1;
}

/* Given X, an unsigned number, return the largest int Y such that 2**Y <= X.
   If X is 0, return -1.  */

int
floor_log2 (x)
     register unsigned int x;
{
  register int log = 0;
  for (log = 0; log < HOST_BITS_PER_INT; log++)
    if ((x & ((-1) << log)) == 0)
      return log - 1;
  return HOST_BITS_PER_INT - 1;
}

int float_handled;
jmp_buf float_handler;

/* Specify where to longjmp to when a floating arithmetic error happens.
   If HANDLER is 0, it means don't handle the errors any more.  */

void
set_float_handler (handler)
     jmp_buf handler;
{
  float_handled = (handler != 0);
  if (handler)
    bcopy (handler, float_handler, sizeof (float_handler));
}

/* Signals actually come here.  */

static void
float_signal ()
{
  if (float_handled == 0)
    abort ();
  float_handled = 0;
  longjmp (float_handler, 1);
}

/* Handler for SIGPIPE.  */

static void
pipe_closed ()
{
  fatal ("output pipe has been closed");
}

/* Compile an entire file of output from cpp, named NAME.
   Write a file of assembly output and various debugging dumps.  */

static void
compile_file (name)
     char *name;
{
  tree globals;
  int start_time;
  int dump_base_name_length;

  int name_specified = name != 0;

  if (dump_base_name == 0)
    dump_base_name = name ? name : "gccdump";
  dump_base_name_length = strlen (dump_base_name);

  parse_time = 0;
  varconst_time = 0;
  integration_time = 0;
  jump_time = 0;
  cse_time = 0;
  loop_time = 0;
  flow_time = 0;
  combine_time = 0;
  local_alloc_time = 0;
  global_alloc_time = 0;
  dbr_sched_time = 0;
  final_time = 0;
  symout_time = 0;
  dump_time = 0;

  /* Open input file.  */

  if (name == 0 || !strcmp (name, "-"))
    {
      finput = stdin;
      name = "stdin";
    }
  else
    finput = fopen (name, "r");
  if (finput == 0)
    pfatal_with_name (name);

  /* Initialize data in various passes.  */

  init_tree ();
  init_lex ();
  init_rtl ();
  init_emit_once ();
  init_decl_processing ();
  init_optabs ();

  /* If rtl dump desired, open the output file.  */
  if (rtl_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".rtl");
      rtl_dump_file = fopen (dumpname, "w");
      if (rtl_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If jump_opt dump desired, open the output file.  */
  if (jump_opt_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".jump");
      jump_opt_dump_file = fopen (dumpname, "w");
      if (jump_opt_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If cse dump desired, open the output file.  */
  if (cse_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".cse");
      cse_dump_file = fopen (dumpname, "w");
      if (cse_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If loop dump desired, open the output file.  */
  if (loop_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".loop");
      loop_dump_file = fopen (dumpname, "w");
      if (loop_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If flow dump desired, open the output file.  */
  if (flow_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".flow");
      flow_dump_file = fopen (dumpname, "w");
      if (flow_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If combine dump desired, open the output file.  */
  if (combine_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 10);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".combine");
      combine_dump_file = fopen (dumpname, "w");
      if (combine_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If local_reg dump desired, open the output file.  */
  if (local_reg_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".lreg");
      local_reg_dump_file = fopen (dumpname, "w");
      if (local_reg_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If global_reg dump desired, open the output file.  */
  if (global_reg_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".greg");
      global_reg_dump_file = fopen (dumpname, "w");
      if (global_reg_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If jump2_opt dump desired, open the output file.  */
  if (jump2_opt_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 7);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".jump2");
      jump2_opt_dump_file = fopen (dumpname, "w");
      if (jump2_opt_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* If dbr_sched dump desired, open the output file.  */
  if (dbr_sched_dump)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 7);
      strcpy (dumpname, dump_base_name);
      strcat (dumpname, ".dbr");
      dbr_sched_dump_file = fopen (dumpname, "w");
      if (dbr_sched_dump_file == 0)
	pfatal_with_name (dumpname);
    }

  /* Open assembler code output file.  */

  if (! name_specified && asm_file_name == 0)
    asm_out_file = stdout;
  else
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      int len = strlen (dump_base_name);
      strcpy (dumpname, dump_base_name);
      if (len > 2 && ! strcmp (".c", dumpname + len - 2))
	dumpname[len - 2] = 0;
      else if (len > 2 && ! strcmp (".i", dumpname + len - 2))
	dumpname[len - 2] = 0;
      else if (len > 3 && ! strcmp (".co", dumpname + len - 3))
	dumpname[len - 3] = 0;
      strcat (dumpname, ".s");
      if (asm_file_name == 0)
	{
	  asm_file_name = (char *) malloc (strlen (dumpname) + 1);
	  strcpy (asm_file_name, dumpname);
	}
      if (!strcmp (asm_file_name, "-"))
	asm_out_file = stdout;
      else
	asm_out_file = fopen (asm_file_name, "w");
      if (asm_out_file == 0)
	pfatal_with_name (asm_file_name);
    }

  input_filename = name;

  /* the beginning of the file is a new line; check for # */
  /* With luck, we discover the real source file's name from that
     and put it in input_filename.  */
  ungetc (check_newline (), finput);

  /* If the input doesn't start with a #line, use the input name
     as the official input file name.  */
  if (main_input_filename == 0)
    main_input_filename = name;

  /* Put an entry on the input file stack for the main input file.  */
  input_file_stack
    = (struct file_stack *) xmalloc (sizeof (struct file_stack));
  input_file_stack->next = 0;
  input_file_stack->name = input_filename;

  ASM_FILE_START (asm_out_file);

  /* Output something to inform GDB that this compilation was by GCC.  */
#ifndef ASM_IDENTIFY_GCC
  fprintf (asm_out_file, "gcc_compiled.:\n");
#else
  ASM_IDENTIFY_GCC (asm_out_file);
#endif

  /* If GDB symbol table desired, open the GDB symbol output file.  */
  if (write_symbols == GDB_DEBUG)
    {
      register char *dumpname = (char *) xmalloc (dump_base_name_length + 6);
      int len = strlen (dump_base_name);
      strcpy (dumpname, dump_base_name);
      if (len > 2 && ! strcmp (".c", dumpname + len - 2))
	dumpname[len - 2] = 0;
      else if (len > 2 && ! strcmp (".i", dumpname + len - 2))
	dumpname[len - 2] = 0;
      else if (len > 3 && ! strcmp (".co", dumpname + len - 3))
	dumpname[len - 3] = 0;
      strcat (dumpname, ".sym");
      if (sym_file_name == 0)
	sym_file_name = dumpname;
      symout_init (sym_file_name, asm_out_file, main_input_filename);
    }

  /* If dbx symbol table desired, initialize writing it
     and output the predefined types.  */
#ifdef DBX_DEBUGGING_INFO
  if (write_symbols == DBX_DEBUG)
    dbxout_init (asm_out_file, main_input_filename);
#endif
#ifdef SDB_DEBUGGING_INFO
  if (write_symbols == SDB_DEBUG)
    sdbout_init (asm_out_file, main_input_filename);
#endif

  /* Initialize yet another pass.  */

  init_final (main_input_filename);

  start_time = gettime ();

  /* Call the parser, which parses the entire file
     (calling rest_of_compilation for each function).  */

  yyparse ();

  /* Compilation is now finished except for writing
     what's left of the symbol table output.  */

  parse_time += gettime () - start_time;

  parse_time -= integration_time;
  parse_time -= varconst_time;

  globals = getdecls ();

  /* Really define vars that have had only a tentative definition.
     Really output inline functions that must actually be callable
     and have not been output so far.  */

  {
    tree decl;
    for (decl = globals; decl; decl = TREE_CHAIN (decl))
      {
	if (TREE_CODE (decl) == VAR_DECL && TREE_STATIC (decl)
	    && ! TREE_ASM_WRITTEN (decl))
	  {
	    /* Don't write out static consts, unless we needed
	       to take their address for some reason.  */
	    if (! TREE_READONLY (decl)
		|| TREE_PUBLIC (decl)
		|| TREE_ADDRESSABLE (decl))
	      rest_of_decl_compilation (decl, 0, 1, 1);
	    /* Otherwise maybe mention them just for the debugger.  */
#ifdef DBX_DEBUGGING_INFO
	    else if (DECL_INITIAL (decl) && write_symbols == DBX_DEBUG)
	      TIMEVAR (varconst_time, dbxout_symbol (decl, 0));
#endif
#ifdef SDB_DEBUGGING_INFO
	    else if (DECL_INITIAL (decl) && write_symbols == SDB_DEBUG)
	      TIMEVAR (varconst_time, sdbout_symbol (decl, 0));
#endif
	  }
	if (TREE_CODE (decl) == FUNCTION_DECL
	    && ! TREE_ASM_WRITTEN (decl)
	    && DECL_INITIAL (decl) != 0
	    && TREE_ADDRESSABLE (decl)
	    && ! TREE_EXTERNAL (decl))
	  output_inline_function (decl);

	/* Warn about any function declared static but not defined.  */
	if (warn_unused
	    && TREE_CODE (decl) == FUNCTION_DECL
	    && DECL_INITIAL (decl) == 0
	    && TREE_EXTERNAL (decl)
	    && ! TREE_PUBLIC (decl))
	  warning_with_decl (decl, "`%s' declared but never defined");
	/* Warn about statics fns or vars defined but not used,
	   but not about inline functions
	   since unused inline statics is normal practice.  */
	if (warn_unused
	    && (TREE_CODE (decl) == FUNCTION_DECL
		|| TREE_CODE (decl) == VAR_DECL)
	    && ! TREE_EXTERNAL (decl)
	    && ! TREE_PUBLIC (decl)
	    && ! TREE_USED (decl)
	    && ! TREE_INLINE (decl)
	    /* The TREE_USED bit for file-scope decls
	       is kept in the identifier, to handle multiple
	       external decls in different scopes.  */
	    && ! TREE_USED (DECL_NAME (decl)))
	  warning_with_decl (decl, "`%s' defined but not used");
      }
  }

  /* Do dbx symbols */
#ifdef DBX_DEBUGGING_INFO
  if (write_symbols == DBX_DEBUG)
    TIMEVAR (symout_time,
	     {
	       dbxout_tags (gettags ());
	       dbxout_types (get_permanent_types ());
	     });
#endif

#ifdef SDB_DEBUGGING_INFO
  if (write_symbols == SDB_DEBUG)
    TIMEVAR (symout_time,
	     {
	       sdbout_tags (gettags ());
	       sdbout_types (get_permanent_types ());
	     });
#endif

  /* Do gdb symbols */
  if (write_symbols == GDB_DEBUG)
    TIMEVAR (symout_time,
	     {
	       struct stat statbuf;
	       fstat (fileno (finput), &statbuf);
	       symout_types (get_permanent_types ());
	       symout_top_blocks (globals, gettags ());
	       symout_finish (name, statbuf.st_ctime);
	     });

  /* Output some stuff at end of file if nec.  */

  end_final (main_input_filename);

#ifdef ASM_FILE_END
  ASM_FILE_END (asm_out_file);
#endif

  /* Close the dump files.  */

  if (rtl_dump)
    fclose (rtl_dump_file);

  if (jump_opt_dump)
    fclose (jump_opt_dump_file);

  if (cse_dump)
    fclose (cse_dump_file);

  if (loop_dump)
    fclose (loop_dump_file);

  if (flow_dump)
    fclose (flow_dump_file);

  if (combine_dump)
    {
      dump_combine_total_stats (combine_dump_file);
      fclose (combine_dump_file);
    }

  if (local_reg_dump)
    fclose (local_reg_dump_file);

  if (global_reg_dump)
    fclose (global_reg_dump_file);

  if (jump2_opt_dump)
    fclose (jump2_opt_dump_file);

  if (dbr_sched_dump)
    fclose (dbr_sched_dump_file);

  /* Close non-debugging input and output files.  Take special care to note
     whether fclose returns an error, since the pages might still be on the
     buffer chain while the file is open.  */

  fclose (finput);
  if (ferror (asm_out_file) != 0 || fclose (asm_out_file) != 0)
    fatal_io_error (asm_file_name);

  /* Print the times.  */

  if (! quiet_flag)
    {
      fprintf (stderr,"\n");
      print_time ("parse", parse_time);
      print_time ("integration", integration_time);
      print_time ("jump", jump_time);
      print_time ("cse", cse_time);
      print_time ("loop", loop_time);
      print_time ("flow", flow_time);
      print_time ("combine", combine_time);
      print_time ("local-alloc", local_alloc_time);
      print_time ("global-alloc", global_alloc_time);
      print_time ("dbranch", dbr_sched_time);
      print_time ("final", final_time);
      print_time ("varconst", varconst_time);
      print_time ("symout", symout_time);
      print_time ("dump", dump_time);
    }
}

/* This is called from finish_decl (within yyparse)
   for each declaration of a function or variable.
   This does nothing for automatic variables.
   Otherwise, it sets up the RTL and outputs any assembler code
   (label definition, storage allocation and initialization).

   DECL is the declaration.  If ASMSPEC is nonzero, it specifies
   the assembler symbol name to be used.  TOP_LEVEL is nonzero
   if this declaration is not within a function.  */

void
rest_of_decl_compilation (decl, asmspec, top_level, at_end)
     tree decl;
     char *asmspec;
     int top_level;
     int at_end;
{
  /* Declarations of variables, and of functions defined elsewhere.  */

  if (TREE_STATIC (decl) || TREE_EXTERNAL (decl))
    TIMEVAR (varconst_time,
	     {
	       make_decl_rtl (decl, asmspec, top_level);
	       /* Don't output anything
		  when a tentative file-scope definition is seen.
		  But at end of compilation, do output code for them.  */
	       if (! (! at_end && top_level
		      && (DECL_INITIAL (decl) == 0
			  || DECL_INITIAL (decl) == error_mark_node)))
		 assemble_variable (decl, top_level, write_symbols, at_end);
	     });
  else if (TREE_REGDECL (decl) && asmspec != 0)
    {
      if (decode_reg_name (asmspec) >= 0)
	{
	  DECL_RTL (decl) = 0;
	  make_decl_rtl (decl, asmspec, top_level);
	}
      else
	error ("invalid register name `%s' for register variable", asmspec);
    }
#ifdef DBX_DEBUGGING_INFO
  else if (write_symbols == DBX_DEBUG && TREE_CODE (decl) == TYPE_DECL)
    TIMEVAR (varconst_time, dbxout_symbol (decl, 0));
#endif
#ifdef SDB_DEBUGGING_INFO
  else if (write_symbols == SDB_DEBUG && top_level
	   && TREE_CODE (decl) == TYPE_DECL)
    TIMEVAR (varconst_time, sdbout_symbol (decl, 0));
#endif

  if (top_level)
    {
      if (write_symbols == GDB_DEBUG)
	{
	  TIMEVAR (symout_time,
		   {
		     /* The initizations make types when they contain
			string constants.  The types are on the temporary
			obstack, so output them now before they go away.  */
		     symout_types (get_temporary_types ());
		   });
	}
      else
	/* Clean out the temporary type list, since the types will go away.  */
	get_temporary_types ();
    }
}

/* This is called from finish_function (within yyparse)
   after each top-level definition is parsed.
   It is supposed to compile that function or variable
   and output the assembler code for it.
   After we return, the tree storage is freed.  */

void
rest_of_compilation (decl)
     tree decl;
{
  register rtx insns;
  int start_time = gettime ();
  int tem;

  /* If we are reconsidering an inline function
     at the end of compilation, skip the stuff for making it inline.  */

  if (DECL_SAVED_INSNS (decl) == 0)
    {

      /* If requested, consider whether to make this function inline.  */
      if (flag_inline_functions || TREE_INLINE (decl))
	{
	  TIMEVAR (integration_time,
		   {
		     int specd = TREE_INLINE (decl);
		     char *lose = function_cannot_inline_p (decl);
		     if (lose != 0 && specd)
		       warning_with_decl (decl, lose);
		     if (lose == 0)
		       save_for_inline (decl);
		     else
		       TREE_INLINE (decl) = 0;
		   });
	}

      insns = get_insns ();

      /* Dump the rtl code if we are dumping rtl.  */

      if (rtl_dump)
	TIMEVAR (dump_time,
		 {
		   fprintf (rtl_dump_file, "\n;; Function %s\n\n",
			    IDENTIFIER_POINTER (DECL_NAME (decl)));
		   if (DECL_SAVED_INSNS (decl))
		     fprintf (rtl_dump_file, ";; (integrable)\n\n");
		   print_rtl (rtl_dump_file, insns);
		   fflush (rtl_dump_file);
		 });

      /* If function is inline, and we don't yet know whether to
	 compile it by itself, defer decision till end of compilation.
	 finish_compilation will call rest_of_compilation again
	 for those functions that need to be output.  */

      if (((! TREE_PUBLIC (decl) && ! TREE_ADDRESSABLE (decl)
	    && ! flag_keep_inline_functions)
	   || TREE_EXTERNAL (decl))
	  && TREE_INLINE (decl))
	goto exit_rest_of_compilation;
    }

  if (rtl_dump_and_exit || flag_syntax_only)
    {
      get_temporary_types ();
      goto exit_rest_of_compilation;
    }

  TREE_ASM_WRITTEN (decl) = 1;

  insns = get_insns ();

  /* Copy any shared structure that should not be shared.  */

  unshare_all_rtl (insns);

  /* See if we have allocated stack slots that are not directly addressable.
     If so, scan all the insns and create explicit address computation
     for all references to such slots.  */
/*   fixup_stack_slots (); */

  /* Do jump optimization the first time, if -opt.
     Also do it if -W, but in that case it doesn't change the rtl code,
     it only computes whether control can drop off the end of the function.  */

  if (optimize || extra_warnings || warn_return_type
      /* If function is `volatile', we should warn if it tries to return.  */
      || TREE_THIS_VOLATILE (decl))
    TIMEVAR (jump_time, jump_optimize (insns, 0, 0));

  /* Dump rtl code after jump, if we are doing that.  */

  if (jump_opt_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (jump_opt_dump_file, "\n;; Function %s\n\n",
			IDENTIFIER_POINTER (DECL_NAME (decl)));
	       print_rtl (jump_opt_dump_file, insns);
	       fflush (jump_opt_dump_file);
	     });

  /* Perform common subexpression elimination.
     Nonzero value from `cse_main' means that jumps were simplified
     and some code may now be unreachable, so do
     jump optimization again.  */

  if (optimize)
    {
      TIMEVAR (cse_time, reg_scan (insns, max_reg_num (), 0));

      TIMEVAR (cse_time, tem = cse_main (insns, max_reg_num ()));

      if (tem)
	TIMEVAR (jump_time, jump_optimize (insns, 0, 0));
    }

  /* Dump rtl code after cse, if we are doing that.  */

  if (cse_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (cse_dump_file, "\n;; Function %s\n\n",
			IDENTIFIER_POINTER (DECL_NAME (decl)));
	       print_rtl (cse_dump_file, insns);
	       fflush (cse_dump_file);
	     });

  if (loop_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (loop_dump_file, "\n;; Function %s\n\n",
			IDENTIFIER_POINTER (DECL_NAME (decl)));
	     });

  /* Move constant computations out of loops.  */

  if (optimize)
    {
      TIMEVAR (loop_time,
	       {
		 reg_scan (insns, max_reg_num (), 1);
		 loop_optimize (insns, loop_dump ? loop_dump_file : 0);
	       });
    }

  /* Dump rtl code after loop opt, if we are doing that.  */

  if (loop_dump)
    TIMEVAR (dump_time,
	     {
	       print_rtl (loop_dump_file, insns);
	       fflush (loop_dump_file);
	     });

  /* Now we choose between stupid (pcc-like) register allocation
     (if we got the -noreg switch and not -opt)
     and smart register allocation.  */

  if (optimize)		/* Stupid allocation probably won't work */
    obey_regdecls = 0;	/* if optimizations being done.  */

  regclass_init ();

  /* Print function header into flow dump now
     because doing the flow analysis makes some of the dump.  */

  if (flow_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (flow_dump_file, "\n;; Function %s\n\n",
			IDENTIFIER_POINTER (DECL_NAME (decl)));
	     });

  if (obey_regdecls)
    {
      TIMEVAR (flow_time,
	       {
		 regclass (insns, max_reg_num ());
		 stupid_life_analysis (insns, max_reg_num (),
				       flow_dump_file);
	       });
    }
  else
    {
      /* Do control and data flow analysis,
	 and write some of the results to dump file.  */

      TIMEVAR (flow_time, flow_analysis (insns, max_reg_num (),
					 flow_dump_file));
      if (extra_warnings)
	uninitialized_vars_warning (DECL_INITIAL (decl));
    }

  /* Dump rtl after flow analysis.  */

  if (flow_dump)
    TIMEVAR (dump_time,
	     {
	       print_rtl (flow_dump_file, insns);
	       fflush (flow_dump_file);
	     });

  /* If -opt, try combining insns through substitution.  */

  if (optimize)
    TIMEVAR (combine_time, combine_instructions (insns, max_reg_num ()));

  /* Dump rtl code after insn combination.  */

  if (combine_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (combine_dump_file, "\n;; Function %s\n\n",
			IDENTIFIER_POINTER (DECL_NAME (decl)));
	       dump_combine_stats (combine_dump_file);
	       print_rtl (combine_dump_file, insns);
	       fflush (combine_dump_file);
	     });

  /* Unless we did stupid register allocation,
     allocate pseudo-regs that are used only within 1 basic block.  */

  if (!obey_regdecls)
    TIMEVAR (local_alloc_time,
	     {
	       regclass (insns, max_reg_num ());
	       local_alloc ();
	     });

  /* Dump rtl code after allocating regs within basic blocks.  */

  if (local_reg_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (local_reg_dump_file, "\n;; Function %s\n\n",
			IDENTIFIER_POINTER (DECL_NAME (decl)));
	       dump_flow_info (local_reg_dump_file);
	       dump_local_alloc (local_reg_dump_file);
	       print_rtl (local_reg_dump_file, insns);
	       fflush (local_reg_dump_file);
	     });

  if (global_reg_dump)
    TIMEVAR (dump_time,
	     fprintf (global_reg_dump_file, "\n;; Function %s\n\n",
		      IDENTIFIER_POINTER (DECL_NAME (decl))));

  /* Unless we did stupid register allocation,
     allocate remaining pseudo-regs, then do the reload pass
     fixing up any insns that are invalid.  */

  TIMEVAR (global_alloc_time,
	   {
	     if (!obey_regdecls)
	       global_alloc (global_reg_dump ? global_reg_dump_file : 0);
	     else
	       reload (insns, 0,
		       global_reg_dump ? global_reg_dump_file : 0);
	   });

  if (global_reg_dump)
    TIMEVAR (dump_time,
	     {
	       dump_global_regs (global_reg_dump_file);
	       print_rtl (global_reg_dump_file, insns);
	       fflush (global_reg_dump_file);
	     });

  rtx_equal_function_value_matters = 1;
  reload_completed = 1;

  /* One more attempt to remove jumps to .+1
     left by dead-store-elimination.
     Also do cross-jumping this time
     and delete no-op move insns.  */

  if (optimize)
    {
      TIMEVAR (jump_time, jump_optimize (insns, 1, 1));
    }

  /* Dump rtl code after jump, if we are doing that.  */

  if (jump2_opt_dump)
    TIMEVAR (dump_time,
	     {
	       fprintf (jump2_opt_dump_file, "\n;; Function %s\n\n",
			IDENTIFIER_POINTER (DECL_NAME (decl)));
	       print_rtl (jump2_opt_dump_file, insns);
	       fflush (jump2_opt_dump_file);
	     });

  /* If a scheduling pass for delayed branches is to be done,
     call the scheduling code. */

#ifdef HAVE_DELAYED_BRANCH
  if (optimize && flag_delayed_branch)
    {
      TIMEVAR (dbr_sched_time, dbr_schedule (insns, dbr_sched_dump_file));
      if (dbr_sched_dump)
	{
	  TIMEVAR (dump_time,
		 {
		   fprintf (dbr_sched_dump_file, "\n;; Function %s\n\n",
			    IDENTIFIER_POINTER (DECL_NAME (decl)));
		   print_rtl (dbr_sched_dump_file, insns);
		   fflush (dbr_sched_dump_file);
		 });
	}
    }
#endif

  /* Now turn the rtl into assembler code.  */

  TIMEVAR (final_time,
	   {
	     assemble_function (decl);
	     final_start_function (insns, asm_out_file,
				   write_symbols, optimize);
	     final (insns, asm_out_file,
		    write_symbols, optimize, 0);
	     final_end_function (insns, asm_out_file,
				 write_symbols, optimize);
	     fflush (asm_out_file);
	   });

  /* Write GDB symbols if requested */

  if (write_symbols == GDB_DEBUG)
    {
      TIMEVAR (symout_time,
	       {
		 symout_types (get_permanent_types ());
		 symout_types (get_temporary_types ());

		 DECL_BLOCK_SYMTAB_ADDRESS (decl)
		   = symout_function (DECL_INITIAL (decl),
				      DECL_ARGUMENTS (decl), 0);
		 symout_function_end ();
	       });
    }
  else
    get_temporary_types ();

  /* Write DBX symbols if requested */

#ifdef DBX_DEBUGGING_INFO
  if (write_symbols == DBX_DEBUG)
    TIMEVAR (symout_time, dbxout_function (decl));
#endif

 exit_rest_of_compilation:

  rtx_equal_function_value_matters = 0;
  reload_completed = 0;

  /* Clear out the real_constant_chain before some of the rtx's
     it runs through become garbage.  */

  clear_const_double_mem ();

  /* The parsing time is all the time spent in yyparse
     *except* what is spent in this function.  */

  parse_time -= gettime () - start_time;
}

/* Entry point of cc1.  Decode command args, then call compile_file.
   Exit code is 35 if can't open files, 34 if fatal error,
   33 if had nonfatal errors, else success.  */

int
main (argc, argv, envp)
     int argc;
     char **argv;
     char **envp;
{
  register int i;
  char *filename = 0;
  int print_mem_flag = 0;
  char *p;

  /* save in case md file wants to emit args as a comment.  */
  save_argc = argc;
  save_argv = argv;

  p = argv[0] + strlen (argv[0]);
  while (p != argv[0] && p[-1] != '/') --p;
  progname = p;

#ifdef RLIMIT_STACK
  /* Get rid of any avoidable limit on stack size.  */
  {
    struct rlimit rlim;

    /* Set the stack limit huge so that alloca does not fail. */
    getrlimit (RLIMIT_STACK, &rlim);
    rlim.rlim_cur = rlim.rlim_max;
    setrlimit (RLIMIT_STACK, &rlim);
  }
#endif /* RLIMIT_STACK */

  signal (SIGFPE, float_signal);

  signal (SIGPIPE, pipe_closed);

  /* Initialize whether `char' is signed.  */
  flag_signed_char = DEFAULT_SIGNED_CHAR;
#ifdef DEFAULT_SHORT_ENUMS
  /* Initialize how much space enums occupy, by default.  */
  flag_short_enums = DEFAULT_SHORT_ENUMS;
#endif

  /* This is zeroed by -O.  */
  obey_regdecls = 1;

  /* Initialize register usage now so switches may override.  */
  init_reg_sets ();

  target_flags = 0;
  set_target_switch ("");

  for (i = 1; i < argc; i++)
    if (argv[i][0] == '-' && argv[i][1] != 0)
      {
	register char *str = argv[i] + 1;
	if (str[0] == 'Y')
	  str++;

	if (str[0] == 'm')
	  set_target_switch (&str[1]);
	else if (!strcmp (str, "dumpbase"))
	  {
	    dump_base_name = argv[++i];
	  }
	else if (str[0] == 'd')
	  {
	    register char *p = &str[1];
	    while (*p)
	      switch (*p++)
		{
		case 'c':
		  combine_dump = 1;
		  break;
		case 'd':
		  dbr_sched_dump = 1;
		  break;
		case 'f':
		  flow_dump = 1;
		  break;
		case 'g':
		  global_reg_dump = 1;
		  break;
		case 'j':
		  jump_opt_dump = 1;
		  break;
		case 'J':
		  jump2_opt_dump = 1;
		  break;
		case 'l':
		  local_reg_dump = 1;
		  break;
		case 'L':
		  loop_dump = 1;
		  break;
		case 'm':
		  print_mem_flag = 1;
		  break;
		case 'r':
		  rtl_dump = 1;
		  break;
		case 's':
		  cse_dump = 1;
		  break;
		case 'y':
		  yydebug = 1;
		  break;
		}
	  }
	else if (str[0] == 'f')
	  {
	    int j;
	    register char *p = &str[1];
	    int found = 0;

	    /* Some kind of -f option.
	       P's value is the option sans `-f'.
	       Search for it in the table of options.  */

	    for (j = 0;
		 !found && j < sizeof (f_options) / sizeof (f_options[0]);
		 j++)
	      {
		if (!strcmp (p, f_options[j].string))
		  {
		    *f_options[j].variable = f_options[j].on_value;
		    /* A goto here would be cleaner,
		       but breaks the vax pcc.  */
		    found = 1;
		  }
		if (p[0] == 'n' && p[1] == 'o' && p[2] == '-'
		    && ! strcmp (p+3, f_options[j].string))
		  {
		    *f_options[j].variable = ! f_options[j].on_value;
		    found = 1;
		  }
	      }

	    if (found)
	      ;
	    else if (!strncmp (p, "fixed-", 6))
	      fix_register (&p[6], 1, 1);
	    else if (!strncmp (p, "call-used-", 10))
	      fix_register (&p[10], 0, 1);
	    else if (!strncmp (p, "call-saved-", 11))
	      fix_register (&p[11], 0, 0);
	    else if (! lang_decode_option (argv[i]))
	      error ("Invalid option `%s'", argv[i]);
	  }
	else if (!strcmp (str, "noreg"))
	  ;
	else if (!strcmp (str, "opt"))
	  optimize = 1, obey_regdecls = 0;
	else if (!strcmp (str, "O"))
	  optimize = 1, obey_regdecls = 0;
	else if (!strcmp (str, "pedantic"))
	  pedantic = 1;
	else if (lang_decode_option (argv[i]))
	  ;
	else if (!strcmp (str, "quiet"))
	  quiet_flag = 1;
	else if (!strcmp (str, "version"))
	  {
	    extern char *version_string, *language_string;
	    fprintf (stderr, "%s version %s", language_string, version_string);
#ifdef TARGET_VERSION
	    TARGET_VERSION;
#endif
#ifdef __GNUC__
#ifndef __VERSION__
#define __VERSION__ "[unknown]"
#endif
	    fprintf (stderr, " compiled by GNU C version %s.\n", __VERSION__);
#else
	    fprintf (stderr, " compiled by CC.\n");
#endif
	    print_target_switch_defaults ();
	  }
	else if (!strcmp (str, "w"))
	  inhibit_warnings = 1;
	else if (!strcmp (str, "W"))
	  extra_warnings = 1;
	else if (!strcmp (str, "Wunused"))
	  warn_unused = 1;
	else if (!strcmp (str, "Wshadow"))
	  warn_shadow = 1;
	else if (!strcmp (str, "Wswitch"))
	  warn_switch = 1;
	else if (!strncmp (str, "Wid-clash-", 10))
	  {
	    char *endp = str + 10;

	    while (*endp)
	      {
		if (*endp >= '0' && *endp <= '9')
		  endp++;
		else
		  error ("Invalid option `%s'", argv[i]);
	      }
	    warn_id_clash = 1;
	    id_clash_len = atoi (str + 10);
	  }
	else if (!strcmp (str, "p"))
	  profile_flag = 1;
	else if (!strcmp (str, "a"))
	  {
#if !defined (BLOCK_PROFILER) || !defined (FUNCTION_BLOCK_PROFILER)
	    warning ("`-a' option (basic block profile) not supported");
#else
	    profile_block_flag = 1;
#endif
	  }
	else if (!strcmp (str, "gg"))
	  write_symbols = GDB_DEBUG;
#ifdef DBX_DEBUGGING_INFO
	else if (!strcmp (str, "g0"))
	  write_symbols = DBX_DEBUG;
	else if (!strcmp (str, "G0"))
	  write_symbols = DBX_DEBUG;
	else if (!strcmp (str, "g"))
	  {
	    write_symbols = DBX_DEBUG;
	    use_gdb_dbx_extensions = 1;
	  }
	else if (!strcmp (str, "G"))
	  {
	    write_symbols = DBX_DEBUG;
	    use_gdb_dbx_extensions = 1;
	  }
#endif
#ifdef SDB_DEBUGGING_INFO
	else if (!strcmp (str, "g"))
	  write_symbols = SDB_DEBUG;
	else if (!strcmp (str, "G"))
	  write_symbols = SDB_DEBUG;
	else if (!strcmp (str, "g0"))
	  write_symbols = SDB_DEBUG;
	else if (!strcmp (str, "G0"))
	  write_symbols = SDB_DEBUG;
#endif
	else if (!strcmp (str, "symout"))
	  {
	    if (write_symbols == NO_DEBUG)
	      write_symbols = GDB_DEBUG;
	    sym_file_name = argv[++i];
	  }
	else if (!strcmp (str, "o"))
	  {
	    asm_file_name = argv[++i];
	  }
	else
	  error ("Invalid option `%s'", argv[i]);
      }
    else
      filename = argv[i];

#ifdef OVERRIDE_OPTIONS
  /* Some machines may reject certain combinations of options.  */
  OVERRIDE_OPTIONS;
#endif

  /* Now that register usage is specified, convert it to HARD_REG_SETs.  */
  init_reg_sets_1 ();

  compile_file (filename);

#ifndef USG
#ifndef VMS
  if (print_mem_flag)
    {
      extern char **environ;
      char *lim = (char *) sbrk (0);

      fprintf (stderr, "Data size %d.\n",
	       (int) lim - (int) &environ);
      fflush (stderr);

      system ("ps v");
    }
#endif /* not VMS */
#endif /* not USG */

  if (errorcount)
    exit (FATAL_EXIT_CODE);
  if (sorrycount)
    exit (FATAL_EXIT_CODE);
  exit (SUCCESS_EXIT_CODE);
  return 34;
}

/* Decode -m switches.  */

/* Here is a table, controlled by the tm-...h file, listing each -m switch
   and which bits in `target_switches' it should set or clear.
   If VALUE is positive, it is bits to set.
   If VALUE is negative, -VALUE is bits to clear.
   (The sign bit is not used so there is no confusion.)  */

struct {char *name; int value;} target_switches []
  = TARGET_SWITCHES;

/* Decode the switch -mNAME.  */

void
set_target_switch (name)
     char *name;
{
  register int j;
  for (j = 0; j < sizeof target_switches / sizeof target_switches[0]; j++)
    if (!strcmp (target_switches[j].name, name))
      {
	if (target_switches[j].value < 0)
	  target_flags &= ~-target_switches[j].value;
	else
	  target_flags |= target_switches[j].value;
	return;
      }
  error ("Invalid option `%s'", name);
}

/* Print default target switches for -version.  */

void
print_target_switch_defaults ()
{
  register int j;
  register int mask = TARGET_DEFAULT;
  fprintf (stderr, "default target switches:");
  for (j = 0; j < sizeof target_switches / sizeof target_switches[0]; j++)
    if (target_switches[j].name[0] != '\0'
	&& target_switches[j].value > 0
	&& (target_switches[j].value & mask) == target_switches[j].value)

      fprintf (stderr, " -m%s", target_switches[j].name);

  fprintf (stderr, "\n");
}
