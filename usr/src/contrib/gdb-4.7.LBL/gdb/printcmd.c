/* Print values for GNU debugger GDB.
   Copyright 1986, 1987, 1988, 1989, 1990, 1991 Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "defs.h"
#include <string.h>
#include "frame.h"
#include "symtab.h"
#include "gdbtypes.h"
#include "value.h"
#include "language.h"
#include "expression.h"
#include "gdbcore.h"
#include "gdbcmd.h"
#include "target.h"
#include "breakpoint.h"
#include "demangle.h"

extern int asm_demangle;	/* Whether to demangle syms in asm printouts */
extern int addressprint;	/* Whether to print hex addresses in HLL " */

struct format_data
{
  int count;
  char format;
  char size;
};

/* Last specified output format.  */

static char last_format = 'x';

/* Last specified examination size.  'b', 'h', 'w' or `q'.  */

static char last_size = 'w';

/* Default address to examine next.  */

static CORE_ADDR next_address;

/* Last address examined.  */

static CORE_ADDR last_examine_address;

/* Contents of last address examined.
   This is not valid past the end of the `x' command!  */

static value last_examine_value;

/* Number of auto-display expression currently being displayed.
   So that we can deleted it if we get an error or a signal within it.
   -1 when not doing one.  */

int current_display_number;

/* Flag to low-level print routines that this value is being printed
   in an epoch window.  We'd like to pass this as a parameter, but
   every routine would need to take it.  Perhaps we can encapsulate
   this in the I/O stream once we have GNU stdio. */

int inspect_it = 0;

struct display
{
  /* Chain link to next auto-display item.  */
  struct display *next;
  /* Expression to be evaluated and displayed.  */
  struct expression *exp;
  /* Item number of this auto-display item.  */
  int number;
  /* Display format specified.  */
  struct format_data format;
  /* Innermost block required by this expression when evaluated */
  struct block *block;
  /* Status of this display (enabled or disabled) */
  enum enable status;
};

/* Chain of expressions whose values should be displayed
   automatically each time the program stops.  */

static struct display *display_chain;

static int display_number;

/* Prototypes for local functions */

static void
delete_display PARAMS ((int));

static void
enable_display PARAMS ((char *, int));

static void
disable_display_command PARAMS ((char *, int));

static void
disassemble_command PARAMS ((char *, int));

static int
containing_function_bounds PARAMS ((CORE_ADDR, CORE_ADDR *, CORE_ADDR *));

static void
printf_command PARAMS ((char *, int));

static void
print_frame_nameless_args PARAMS ((struct frame_info *, CORE_ADDR, long,
				   int, int, FILE *));

static void
display_info PARAMS ((char *, int));

static void
do_one_display PARAMS ((struct display *));

static void
undisplay_command PARAMS ((char *, int));

static void
free_display PARAMS ((struct display *));

static void
display_command PARAMS ((char *, int));

static void
ptype_command PARAMS ((char *, int));

static struct type *
ptype_eval PARAMS ((struct expression *));

static void
whatis_command PARAMS ((char *, int));

static void
whatis_exp PARAMS ((char *, int));

static void
x_command PARAMS ((char *, int));

static void
address_info PARAMS ((char *, int));

static void
set_command PARAMS ((char *, int));

static void
output_command PARAMS ((char *, int));

static void
call_command PARAMS ((char *, int));

static void
inspect_command PARAMS ((char *, int));

static void
print_command PARAMS ((char *, int));

static void
print_command_1 PARAMS ((char *, int, int));

static void
validate_format PARAMS ((struct format_data, char *));

static void
do_examine PARAMS ((struct format_data, CORE_ADDR));

static void
print_formatted PARAMS ((value, int, int));

static struct format_data
decode_format PARAMS ((char **, int, int));


/* Decode a format specification.  *STRING_PTR should point to it.
   OFORMAT and OSIZE are used as defaults for the format and size
   if none are given in the format specification.
   If OSIZE is zero, then the size field of the returned value
   should be set only if a size is explicitly specified by the
   user.
   The structure returned describes all the data
   found in the specification.  In addition, *STRING_PTR is advanced
   past the specification and past all whitespace following it.  */

static struct format_data
decode_format (string_ptr, oformat, osize)
     char **string_ptr;
     int oformat;
     int osize;
{
  struct format_data val;
  register char *p = *string_ptr;

  val.format = '?';
  val.size = '?';
  val.count = 1;

  if (*p >= '0' && *p <= '9')
    val.count = atoi (p);
  while (*p >= '0' && *p <= '9') p++;

  /* Now process size or format letters that follow.  */

  while (1)
    {
      if (*p == 'b' || *p == 'h' || *p == 'w' || *p == 'g')
	val.size = *p++;
#ifdef LONG_LONG
      else if (*p == 'l')
	{
	  val.size = 'g';
	  p++;
	}
#endif
      else if ((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z'))
	val.format = *p++;
      else
	break;
    }

#ifndef LONG_LONG
  /* Make sure 'g' size is not used on integer types.
     Well, actually, we can handle hex.  */
  if (val.size == 'g' && val.format != 'f' && val.format != 'x')
    val.size = 'w';
#endif

  while (*p == ' ' || *p == '\t') p++;
  *string_ptr = p;

  /* Set defaults for format and size if not specified.  */
  if (val.format == '?')
    {
      if (val.size == '?')
	{
	  /* Neither has been specified.  */
	  val.format = oformat;
	  val.size = osize;
	}
      else
	/* If a size is specified, any format makes a reasonable
	   default except 'i'.  */
	val.format = oformat == 'i' ? 'x' : oformat;
    }
  else if (val.size == '?')
    switch (val.format)
      {
      case 'a':
      case 's':
      case 'A':
	/* Addresses must be words.  */
	val.size = osize ? 'w' : osize;
	break;
      case 'f':
	/* Floating point has to be word or giantword.  */
	if (osize == 'w' || osize == 'g')
	  val.size = osize;
	else
	  /* Default it to giantword if the last used size is not
	     appropriate.  */
	  val.size = osize ? 'g' : osize;
	break;
      case 'c':
	/* Characters default to one byte.  */
	val.size = osize ? 'b' : osize;
	break;
      default:
	/* The default is the size most recently specified.  */
	val.size = osize;
      }

  return val;
}

/* Print value VAL on stdout according to FORMAT, a letter or 0.
   Do not end with a newline.
   0 means print VAL according to its own type.
   SIZE is the letter for the size of datum being printed.
   This is used to pad hex numbers so they line up.  */

static void
print_formatted (val, format, size)
     register value val;
     register int format;
     int size;
{
  int len = TYPE_LENGTH (VALUE_TYPE (val));

  if (VALUE_LVAL (val) == lval_memory)
    next_address = VALUE_ADDRESS (val) + len;

  switch (format)
    {
    case 's':
      next_address = VALUE_ADDRESS (val)
	+ value_print (value_addr (val), stdout, format, Val_pretty_default);
      break;

    case 'i':
      wrap_here ("");	/* Force output out, print_insn not using _filtered */
      next_address = VALUE_ADDRESS (val)
	+ print_insn (VALUE_ADDRESS (val), stdout);
      break;

    default:
      if (format == 0
	  || TYPE_CODE (VALUE_TYPE (val)) == TYPE_CODE_ARRAY
	  || TYPE_CODE (VALUE_TYPE (val)) == TYPE_CODE_STRUCT
	  || TYPE_CODE (VALUE_TYPE (val)) == TYPE_CODE_UNION
	  || VALUE_REPEATED (val))
	value_print (val, stdout, format, Val_pretty_default);
      else
	print_scalar_formatted (VALUE_CONTENTS (val), VALUE_TYPE (val),
				format, size, stdout);
    }
}

/* Print a scalar of data of type TYPE, pointed to in GDB by VALADDR,
   according to letters FORMAT and SIZE on STREAM.
   FORMAT may not be zero.  Formats s and i are not supported at this level.

   This is how the elements of an array or structure are printed
   with a format.  */

void
print_scalar_formatted (valaddr, type, format, size, stream)
     char *valaddr;
     struct type *type;
     int format;
     int size;
     FILE *stream;
{
  LONGEST val_long;
  int len = TYPE_LENGTH (type);

  /* If no size is specified, try to guess a reasonable one from the type. */
  if (size == 0 && len <= 4) {
    /* XXX This array is machine dependent. (FIXME) */
    static unsigned char sizetab[] = { 0, 'b', 'h', 0, 'w' };
    size = sizetab[len];
  }
  if (size == 'g' && sizeof (LONGEST) < 8
      && format == 'x')
    {
      /* ok, we're going to have to get fancy here.  Assumption: a
         long is four bytes.  FIXME.  */
      unsigned long v1, v2;

      v1 = unpack_long (builtin_type_long, valaddr);
      v2 = unpack_long (builtin_type_long, valaddr + 4);

#if TARGET_BYTE_ORDER == LITTLE_ENDIAN
      /* Swap the two for printing */
      {
        unsigned long tmp;

        tmp = v1;
        v1 = v2;
        v2 = tmp;
      }
#endif
  
      switch (format)
	{
	case 'x':
	  fprintf_filtered (stream, local_hex_format_custom("08x%08"), v1, v2);
	  break;
	default:
	  error ("Output size \"g\" unimplemented for format \"%c\".",
		 format);
	}
      return;
    }
      
  val_long = unpack_long (type, valaddr);

  /* If value is unsigned, truncate it in case negative.  */
  if (format != 'd')
    {
      if (len == sizeof (char))
	val_long &= (1 << 8 * sizeof(char)) - 1;
      else if (len == sizeof (short))
	val_long &= (1 << 8 * sizeof(short)) - 1;
      else if (len == sizeof (long))
	val_long &= (unsigned long) - 1;
    }

  switch (format)
    {
    case 'x':
      if (!size)
	{
	  /* no size specified, like in print.  Print varying # of digits. */
#if defined (LONG_LONG)
	  fprintf_filtered (stream, local_hex_format_custom("ll"), val_long);
#else /* not LONG_LONG.  */
	  fprintf_filtered (stream, local_hex_format_custom("l"), val_long);
#endif /* not LONG_LONG.  */
	}
      else
#if defined (LONG_LONG)
      switch (size)
	{
	case 'b':
	  fprintf_filtered (stream, local_hex_format_custom("02ll"), val_long);
	  break;
	case 'h':
	  fprintf_filtered (stream, local_hex_format_custom("04ll"), val_long);
	  break;
	case 'w':
	  fprintf_filtered (stream, local_hex_format_custom("08ll"), val_long);
	  break;
	case 'g':
	  fprintf_filtered (stream, local_hex_format_custom("016ll"), val_long);
	  break;
	default:
	  error ("Undefined output size \"%c\".", size);
	}
#else /* not LONG_LONG.  */
      switch (size)
	{
	case 'b':
	  fprintf_filtered (stream, local_hex_format_custom("02"), val_long);
	  break;
	case 'h':
	  fprintf_filtered (stream, local_hex_format_custom("04"), val_long);
	  break;
	case 'w':
	  fprintf_filtered (stream, local_hex_format_custom("08"), val_long);
	  break;
	case 'g':
	  fprintf_filtered (stream, local_hex_format_custom("016"), val_long);
	  break;
	default:
	  error ("Undefined output size \"%c\".", size);
	}
#endif /* not LONG_LONG */
      break;

    case 'd':
#ifdef LONG_LONG
      fprintf_filtered (stream, "%lld", val_long);
#else
      fprintf_filtered (stream, "%d", val_long);
#endif
      break;

    case 'u':
#ifdef LONG_LONG
      fprintf_filtered (stream, "%llu", val_long);
#else
      fprintf_filtered (stream, "%u", val_long);
#endif
      break;

    case 'o':
      if (val_long)
#ifdef LONG_LONG
	fprintf_filtered (stream, local_octal_format_custom("ll"), val_long);
#else
	fprintf_filtered (stream, local_octal_format(), val_long);
#endif
      else
	fprintf_filtered (stream, "0");
      break;

    case 'a':
      print_address (unpack_pointer (type, valaddr), stream);
      break;

    case 'A':
      print_address_symbolic (unpack_pointer (type, valaddr), stream, 
			      asm_demangle, " ");
      break;

    case 'c':
      value_print (value_from_longest (builtin_type_char, val_long), stream, 0,
		   Val_pretty_default);
      break;

    case 'f':
      if (len == sizeof (float))
	type = builtin_type_float;
      else if (len == sizeof (double))
	type = builtin_type_double;
      print_floating (valaddr, type, stream);
      break;

    case 0:
      abort ();

    case 't':
      /* Binary; 't' stands for "two".  */
      {
        char bits[8*(sizeof val_long) + 1];
	char *cp = bits;
	int width;

        if (!size)
	  width = 8*(sizeof val_long);
        else
          switch (size)
	    {
	    case 'b':
	      width = 8;
	      break;
	    case 'h':
	      width = 16;
	      break;
	    case 'w':
	      width = 32;
	      break;
	    case 'g':
	      width = 64;
	      break;
	    default:
	      error ("Undefined output size \"%c\".", size);
	    }

        bits[width] = '\0';
        while (width-- > 0)
          {
            bits[width] = (val_long & 1) ? '1' : '0';
            val_long >>= 1;
          }
	if (!size)
	  {
	    while (*cp && *cp == '0')
	      cp++;
	    if (*cp == '\0')
	      cp--;
	  }
        fprintf_filtered (stream, cp);
      }
      break;

    default:
      error ("Undefined output format \"%c\".", format);
    }
}

/* Specify default address for `x' command.
   `info lines' uses this.  */

void
set_next_address (addr)
     CORE_ADDR addr;
{
  next_address = addr;

  /* Make address available to the user as $_.  */
  set_internalvar (lookup_internalvar ("_"),
		   value_from_longest (lookup_pointer_type (builtin_type_void),
				    (LONGEST) addr));
}

/* Optionally print address ADDR symbolically as <SYMBOL+OFFSET> on STREAM,
   after LEADIN.  Print nothing if no symbolic name is found nearby.
   DO_DEMANGLE controls whether to print a symbol in its native "raw" form,
   or to interpret it as a possible C++ name and convert it back to source
   form. */

void
print_address_symbolic (addr, stream, do_demangle, leadin)
     CORE_ADDR addr;
     FILE *stream;
     int do_demangle;
     char *leadin;
{
  int name_location;
  register struct minimal_symbol *msymbol = lookup_minimal_symbol_by_pc (addr);

  /* If nothing comes out, don't print anything symbolic.  */
  
  if (msymbol == NULL)
    return;

  fputs_filtered (leadin, stream);
  fputs_filtered ("<", stream);
  if (do_demangle)
    fputs_demangled (msymbol -> name, stream, DMGL_ANSI | DMGL_PARAMS);
  else
    fputs_filtered (msymbol -> name, stream);
  name_location = msymbol -> address;
  if (addr - name_location)
    fprintf_filtered (stream, "+%d>", addr - name_location);
  else
    fputs_filtered (">", stream);
}

/* Print address ADDR symbolically on STREAM.
   First print it as a number.  Then perhaps print
   <SYMBOL + OFFSET> after the number.  */

void
print_address (addr, stream)
     CORE_ADDR addr;
     FILE *stream;
{
#ifdef ADDR_BITS_REMOVE
  fprintf_filtered (stream, local_hex_format(), ADDR_BITS_REMOVE(addr));
#else
  fprintf_filtered (stream, local_hex_format(), addr);
#endif
    print_address_symbolic (addr, stream, asm_demangle, " ");
}

/* Print address ADDR symbolically on STREAM.  Parameter DEMANGLE
   controls whether to print the symbolic name "raw" or demangled.
   Global setting "addressprint" controls whether to print hex address
   or not.  */

void
print_address_demangle (addr, stream, do_demangle)
     CORE_ADDR addr;
     FILE *stream;
     int do_demangle;
{
  if (addr == 0) {
    fprintf_filtered (stream, "0");
  } else if (addressprint) {
    fprintf_filtered (stream, local_hex_format(), addr);
    print_address_symbolic (addr, stream, do_demangle, " ");
  } else {
    print_address_symbolic (addr, stream, do_demangle, "");
  }
}


static int asm_symbolic = 1;

/* Examine data at address ADDR in format FMT.
   Fetch it from memory and print on stdout.  */

static void
do_examine (fmt, addr)
     struct format_data fmt;
     CORE_ADDR addr;
{
  register char format = 0;
  register char size;
  register int count = 1;
  struct type *val_type;
  register int i;
  register int maxelts;

  format = fmt.format;
  size = fmt.size;
  count = fmt.count;
  next_address = addr;

  /* String or instruction format implies fetch single bytes
     regardless of the specified size.  */
  if (format == 's' || format == 'i')
    size = 'b';

  if (size == 'b')
    val_type = builtin_type_char;
  else if (size == 'h')
    val_type = builtin_type_short;
  else if (size == 'w')
    val_type = builtin_type_long;
  else if (size == 'g')
#ifndef LONG_LONG
    val_type = builtin_type_double;
#else
    val_type = builtin_type_long_long;
#endif

  maxelts = 8;
  if (size == 'w')
    maxelts = 4;
  if (size == 'g')
    maxelts = 2;
  if (format == 's' || format == 'i')
    maxelts = 1;

  /* Print as many objects as specified in COUNT, at most maxelts per line,
     with the address of the next one at the start of each line.  */

  while (count > 0)
    {
      if (asm_symbolic)
	print_address (next_address, stdout);
      else
	{
#ifdef ADDR_BITS_REMOVE
          fprintf_filtered (stdout, local_hex_format(), ADDR_BITS_REMOVE(next_address));
#else
          fprintf_filtered (stdout, local_hex_format(), next_address);
#endif
          fputs_filtered (" ", stdout);
	}
      printf_filtered (":");
      for (i = maxelts;
	   i > 0 && count > 0;
	   i--, count--)
	{
	  printf_filtered ("  ");
	  /* Note that print_formatted sets next_address for the next
	     object.  */
	  last_examine_address = next_address;
	  last_examine_value = value_at (val_type, next_address);
	  print_formatted (last_examine_value, format, size);
	}
      printf_filtered ("\n");
      fflush (stdout);
    }
}

static void
validate_format (fmt, cmdname)
     struct format_data fmt;
     char *cmdname;
{
  if (fmt.size != 0)
    error ("Size letters are meaningless in \"%s\" command.", cmdname);
  if (fmt.count != 1)
    error ("Item count other than 1 is meaningless in \"%s\" command.",
	   cmdname);
  if (fmt.format == 'i' || fmt.format == 's')
    error ("Format letter \"%c\" is meaningless in \"%s\" command.",
	   fmt.format, cmdname);
}

static void
print_command_1 (exp, inspect, voidprint)
     char *exp;
     int inspect;
     int voidprint;
{
  struct expression *expr;
  register struct cleanup *old_chain = 0;
  register char format = 0;
  register value val;
  struct format_data fmt;
  int cleanup = 0;

  /* Pass inspect flag to the rest of the print routines in a global (sigh). */
  inspect_it = inspect;

  if (exp && *exp == '/')
    {
      exp++;
      fmt = decode_format (&exp, last_format, 0);
      validate_format (fmt, "print");
      last_format = format = fmt.format;
    }
  else
    {
      fmt.count = 1;
      fmt.format = 0;
      fmt.size = 0;
    }

  if (exp && *exp)
    {
      extern int objectprint;
      struct type *type;
      expr = parse_expression (exp);
      old_chain = make_cleanup (free_current_contents, &expr);
      cleanup = 1;
      val = evaluate_expression (expr);

      /* C++: figure out what type we actually want to print it as.  */
      type = VALUE_TYPE (val);

      if (objectprint
	  && (   TYPE_CODE (type) == TYPE_CODE_PTR
	      || TYPE_CODE (type) == TYPE_CODE_REF)
	  && (   TYPE_CODE (TYPE_TARGET_TYPE (type)) == TYPE_CODE_STRUCT
	      || TYPE_CODE (TYPE_TARGET_TYPE (type)) == TYPE_CODE_UNION))
	{
	  value v;

	  v = value_from_vtable_info (val, TYPE_TARGET_TYPE (type));
	  if (v != 0)
	    {
	      val = v;
	      type = VALUE_TYPE (val);
	    }
	}
    }
  else
    val = access_value_history (0);

  if (voidprint || (val && VALUE_TYPE (val) &&
                    TYPE_CODE (VALUE_TYPE (val)) != TYPE_CODE_VOID))
    {
      int histindex = record_latest_value (val);

      if (inspect)
	printf ("\031(gdb-makebuffer \"%s\"  %d '(\"", exp, histindex);
      else
	if (histindex >= 0) printf_filtered ("$%d = ", histindex);

      print_formatted (val, format, fmt.size);
      printf_filtered ("\n");
      if (inspect)
	printf("\") )\030");
    }

  if (cleanup)
    do_cleanups (old_chain);
  inspect_it = 0;	/* Reset print routines to normal */
}

/* ARGSUSED */
static void
print_command (exp, from_tty)
     char *exp;
     int from_tty;
{
  print_command_1 (exp, 0, 1);
}

/* Same as print, except in epoch, it gets its own window */
/* ARGSUSED */
static void
inspect_command (exp, from_tty)
     char *exp;
     int from_tty;
{
  extern int epoch_interface;

  print_command_1 (exp, epoch_interface, 1);
}

/* Same as print, except it doesn't print void results. */
/* ARGSUSED */
static void
call_command (exp, from_tty)
     char *exp;
     int from_tty;
{
  print_command_1 (exp, 0, 0);
}

/* ARGSUSED */
static void
output_command (exp, from_tty)
     char *exp;
     int from_tty;
{
  struct expression *expr;
  register struct cleanup *old_chain;
  register char format = 0;
  register value val;
  struct format_data fmt;

  if (exp && *exp == '/')
    {
      exp++;
      fmt = decode_format (&exp, 0, 0);
      validate_format (fmt, "output");
      format = fmt.format;
    }

  expr = parse_expression (exp);
  old_chain = make_cleanup (free_current_contents, &expr);

  val = evaluate_expression (expr);

  print_formatted (val, format, fmt.size);

  do_cleanups (old_chain);
}

/* ARGSUSED */
static void
set_command (exp, from_tty)
     char *exp;
     int from_tty;
{
  struct expression *expr = parse_expression (exp);
  register struct cleanup *old_chain
    = make_cleanup (free_current_contents, &expr);
  evaluate_expression (expr);
  do_cleanups (old_chain);
}

/* ARGSUSED */
static void
address_info (exp, from_tty)
     char *exp;
     int from_tty;
{
  register struct symbol *sym;
  register struct minimal_symbol *msymbol;
  register long val;
  register long basereg;
  int is_a_field_of_this;	/* C++: lookup_symbol sets this to nonzero
				   if exp is a field of `this'. */

  if (exp == 0)
    error ("Argument required.");

  sym = lookup_symbol (exp, get_selected_block (), VAR_NAMESPACE, 
		       &is_a_field_of_this, (struct symtab **)NULL);
  if (sym == 0)
    {
      if (is_a_field_of_this)
	{
	  printf ("Symbol \"%s\" is a field of the local class variable `this'\n", exp);
	  return;
	}

      msymbol = lookup_minimal_symbol (exp, (struct objfile *) NULL);

      if (msymbol != NULL)
	printf ("Symbol \"%s\" is at %s in a file compiled without debugging.\n",
		exp, local_hex_string(msymbol -> address));
      else
	error ("No symbol \"%s\" in current context.", exp);
      return;
    }

  printf ("Symbol \"%s\" is ", SYMBOL_NAME (sym));
  val = SYMBOL_VALUE (sym);
  basereg = SYMBOL_BASEREG (sym);

  switch (SYMBOL_CLASS (sym))
    {
    case LOC_CONST:
    case LOC_CONST_BYTES:
      printf ("constant");
      break;

    case LOC_LABEL:
      printf ("a label at address %s", local_hex_string(SYMBOL_VALUE_ADDRESS (sym)));
      break;

    case LOC_REGISTER:
      printf ("a variable in register %s", reg_names[val]);
      break;

    case LOC_STATIC:
      printf ("static storage at address %s", local_hex_string(SYMBOL_VALUE_ADDRESS (sym)));
      break;

    case LOC_REGPARM:
      printf ("an argument in register %s", reg_names[val]);
      break;
      
    case LOC_ARG:
      if (SYMBOL_BASEREG_VALID (sym))
	{
	  printf ("an argument at offset %ld from register %s",
		  val, reg_names[basereg]);
	}
      else
	{
	  printf ("an argument at offset %ld", val);
	}
      break;

    case LOC_LOCAL_ARG:
      if (SYMBOL_BASEREG_VALID (sym))
	{
	  printf ("an argument at offset %ld from register %s",
		  val, reg_names[basereg]);
	}
      else
	{
	  printf ("an argument at frame offset %ld", val);
	}
      break;

    case LOC_LOCAL:
      if (SYMBOL_BASEREG_VALID (sym))
	{
	  printf ("a local variable at offset %ld from register %s",
		  val, reg_names[basereg]);
	}
      else
	{
	  printf ("a local variable at frame offset %ld", val);
	}
      break;

    case LOC_REF_ARG:
      printf ("a reference argument at offset %ld", val);
      break;

    case LOC_TYPEDEF:
      printf ("a typedef");
      break;

    case LOC_BLOCK:
      printf ("a function at address %s",
	      local_hex_string(BLOCK_START (SYMBOL_BLOCK_VALUE (sym))));
      break;

    default:
      printf ("of unknown (botched) type");
      break;
    }
  printf (".\n");
}

static void
x_command (exp, from_tty)
     char *exp;
     int from_tty;
{
  struct expression *expr;
  struct format_data fmt;
  struct cleanup *old_chain;
  struct value *val;

  fmt.format = last_format;
  fmt.size = last_size;
  fmt.count = 1;

  if (exp && *exp == '/')
    {
      exp++;
      fmt = decode_format (&exp, last_format, last_size);
    }

  /* If we have an expression, evaluate it and use it as the address.  */

  if (exp != 0 && *exp != 0)
    {
      expr = parse_expression (exp);
      /* Cause expression not to be there any more
	 if this command is repeated with Newline.
	 But don't clobber a user-defined command's definition.  */
      if (from_tty)
	*exp = 0;
      old_chain = make_cleanup (free_current_contents, &expr);
      val = evaluate_expression (expr);
      if (TYPE_CODE (VALUE_TYPE (val)) == TYPE_CODE_REF)
	val = value_ind (val);
      /* In rvalue contexts, such as this, functions are coerced into
	 pointers to functions.  This makes "x/i main" work.  */
      if (/* last_format == 'i'
	  && */ TYPE_CODE (VALUE_TYPE (val)) == TYPE_CODE_FUNC
	  && VALUE_LVAL (val) == lval_memory)
	next_address = VALUE_ADDRESS (val);
      else
	next_address = value_as_pointer (val);
      do_cleanups (old_chain);
    }

  do_examine (fmt, next_address);

  /* If the examine succeeds, we remember its size and format for next time.  */
  last_size = fmt.size;
  last_format = fmt.format;

  /* Set a couple of internal variables if appropriate. */
  if (last_examine_value)
    {
      /* Make last address examined available to the user as $_.  Use
	 the correct pointer type.  */
      set_internalvar (lookup_internalvar ("_"),
	       value_from_longest (
		 lookup_pointer_type (VALUE_TYPE (last_examine_value)),
				   (LONGEST) last_examine_address));
      
      /* Make contents of last address examined available to the user as $__.*/
      set_internalvar (lookup_internalvar ("__"), last_examine_value);
    }
}

/* Commands for printing types of things.  */

/* Print type of EXP, or last thing in value history if EXP == NULL.
   show is passed to type_print.  */
static void
whatis_exp (exp, show)
     char *exp;
     int show;
{
  struct expression *expr;
  register value val;
  register struct cleanup *old_chain;

  if (exp)
    {
      expr = parse_expression (exp);
      old_chain = make_cleanup (free_current_contents, &expr);
      val = evaluate_type (expr);
    }
  else
    val = access_value_history (0);

  printf_filtered ("type = ");
  type_print (VALUE_TYPE (val), "", stdout, show);
  printf_filtered ("\n");

  if (exp)
    do_cleanups (old_chain);
}

/* ARGSUSED */
static void
whatis_command (exp, from_tty)
     char *exp;
     int from_tty;
{
  /* Most of the time users do not want to see all the fields
     in a structure.  If they do they can use the "ptype" command.
     Hence the "-1" below.  */
  whatis_exp (exp, -1);
}

/* Simple subroutine for ptype_command.  */
static
struct type *
ptype_eval(exp)
   struct expression *exp;
{
   if(exp->elts[0].opcode==OP_TYPE)
      return exp->elts[1].type;
   else
      return 0;
}

/* TYPENAME is either the name of a type, or an expression.  */
/* ARGSUSED */
static void
ptype_command (typename, from_tty)
     char *typename;
     int from_tty;
{
  register struct type *type;
  struct expression *expr;
  register struct cleanup *old_chain;

  if (typename)
  {
     expr = parse_expression (typename);
     old_chain = make_cleanup (free_current_contents, &expr);
     type = ptype_eval (expr);

     if(type)
     {
	printf_filtered ("type = ");
	type_print (type, "", stdout, 1);
	printf_filtered ("\n");
	do_cleanups (old_chain);
     }
     else
     {
	do_cleanups (old_chain);
	whatis_exp (typename, 1);
     }
  }
  else
     whatis_exp (typename, 1);
}

#if MAINTENANCE_CMDS

/* Dump details of a type specified either directly or indirectly.
   Uses the same sort of type lookup mechanism as ptype_command()
   and whatis_command(). */

void
maintenance_print_type (typename, from_tty)
     char *typename;
     int from_tty;
{
  register value val;
  register struct type *type;
  register struct cleanup *old_chain;
  struct expression *expr;

  if (typename != NULL)
  {
    expr = parse_expression (typename);
    old_chain = make_cleanup (free_current_contents, &expr);
    if (expr -> elts[0].opcode == OP_TYPE)
      {
	/* The user expression names a type directly, just use that type. */
	type = expr -> elts[1].type;
      }
    else
      {
	/* The user expression may name a type indirectly by naming an
	   object of that type.  Find that indirectly named type. */
	val = evaluate_type (expr);
	type = VALUE_TYPE (val);
      }
    if (type != NULL)
      {
	recursive_dump_type (type, 0);
      }
    do_cleanups (old_chain);
  }
}

#endif	/* MAINTENANCE_CMDS */


/* Add an expression to the auto-display chain.
   Specify the expression.  */

static void
display_command (exp, from_tty)
     char *exp;
     int from_tty;
{
  struct format_data fmt;
  register struct expression *expr;
  register struct display *new;

  if (exp == 0)
    {
      do_displays ();
      return;
    }

  if (*exp == '/')
    {
      exp++;
      fmt = decode_format (&exp, 0, 0);
      if (fmt.size && fmt.format == 0)
	fmt.format = 'x';
      if (fmt.format == 'i' || fmt.format == 's')
	fmt.size = 'b';
    }
  else
    {
      fmt.format = 0;
      fmt.size = 0;
      fmt.count = 0;
    }

  innermost_block = 0;
  expr = parse_expression (exp);

  new = (struct display *) xmalloc (sizeof (struct display));

  new->exp = expr;
  new->block = innermost_block;
  new->next = display_chain;
  new->number = ++display_number;
  new->format = fmt;
  new->status = enabled;
  display_chain = new;

  if (from_tty && target_has_execution)
    do_one_display (new);

  dont_repeat ();
}

static void
free_display (d)
     struct display *d;
{
  free ((PTR)d->exp);
  free ((PTR)d);
}

/* Clear out the display_chain.
   Done when new symtabs are loaded, since this invalidates
   the types stored in many expressions.  */

void
clear_displays ()
{
  register struct display *d;

  while (d = display_chain)
    {
      free ((PTR)d->exp);
      display_chain = d->next;
      free ((PTR)d);
    }
}

/* Delete the auto-display number NUM.  */

static void
delete_display (num)
     int num;
{
  register struct display *d1, *d;

  if (!display_chain)
    error ("No display number %d.", num);

  if (display_chain->number == num)
    {
      d1 = display_chain;
      display_chain = d1->next;
      free_display (d1);
    }
  else
    for (d = display_chain; ; d = d->next)
      {
	if (d->next == 0)
	  error ("No display number %d.", num);
	if (d->next->number == num)
	  {
	    d1 = d->next;
	    d->next = d1->next;
	    free_display (d1);
	    break;
	  }
      }
}

/* Delete some values from the auto-display chain.
   Specify the element numbers.  */

static void
undisplay_command (args, from_tty)
     char *args;
     int from_tty;
{
  register char *p = args;
  register char *p1;
  register int num;

  if (args == 0)
    {
      if (query ("Delete all auto-display expressions? "))
	clear_displays ();
      dont_repeat ();
      return;
    }

  while (*p)
    {
      p1 = p;
      while (*p1 >= '0' && *p1 <= '9') p1++;
      if (*p1 && *p1 != ' ' && *p1 != '\t')
	error ("Arguments must be display numbers.");

      num = atoi (p);

      delete_display (num);

      p = p1;
      while (*p == ' ' || *p == '\t') p++;
    }
  dont_repeat ();
}

/* Display a single auto-display.  
   Do nothing if the display cannot be printed in the current context,
   or if the display is disabled. */

static void
do_one_display (d)
     struct display *d;
{
  int within_current_scope;

  if (d->status == disabled)
    return;

  if (d->block)
    within_current_scope = contained_in (get_selected_block (), d->block);
  else
    within_current_scope = 1;
  if (!within_current_scope)
    return;

  current_display_number = d->number;

  printf_filtered ("%d: ", d->number);
  if (d->format.size)
    {
      CORE_ADDR addr;
      
      printf_filtered ("x/");
      if (d->format.count != 1)
	printf_filtered ("%d", d->format.count);
      printf_filtered ("%c", d->format.format);
      if (d->format.format != 'i' && d->format.format != 's')
	printf_filtered ("%c", d->format.size);
      printf_filtered (" ");
      print_expression (d->exp, stdout);
      if (d->format.count != 1)
	printf_filtered ("\n");
      else
	printf_filtered ("  ");
      
      addr = value_as_pointer (evaluate_expression (d->exp));
      if (d->format.format == 'i')
	addr = ADDR_BITS_REMOVE (addr);
      
      do_examine (d->format, addr);
    }
  else
    {
      if (d->format.format)
	printf_filtered ("/%c ", d->format.format);
      print_expression (d->exp, stdout);
      printf_filtered (" = ");
      print_formatted (evaluate_expression (d->exp),
		       d->format.format, d->format.size);
      printf_filtered ("\n");
    }

  fflush (stdout);
  current_display_number = -1;
}

/* Display all of the values on the auto-display chain which can be
   evaluated in the current scope.  */

void
do_displays ()
{
  register struct display *d;

  for (d = display_chain; d; d = d->next)
    do_one_display (d);
}

/* Delete the auto-display which we were in the process of displaying.
   This is done when there is an error or a signal.  */

void
disable_display (num)
     int num;
{
  register struct display *d;

  for (d = display_chain; d; d = d->next)
    if (d->number == num)
      {
	d->status = disabled;
	return;
      }
  printf ("No display number %d.\n", num);
}
  
void
disable_current_display ()
{
  if (current_display_number >= 0)
    {
      disable_display (current_display_number);
      fprintf (stderr, "Disabling display %d to avoid infinite recursion.\n",
	       current_display_number);
    }
  current_display_number = -1;
}

static void
display_info (ignore, from_tty)
     char *ignore;
     int from_tty;
{
  register struct display *d;

  if (!display_chain)
    printf ("There are no auto-display expressions now.\n");
  else
      printf_filtered ("Auto-display expressions now in effect:\n\
Num Enb Expression\n");

  for (d = display_chain; d; d = d->next)
    {
      printf_filtered ("%d:   %c  ", d->number, "ny"[(int)d->status]);
      if (d->format.size)
	printf_filtered ("/%d%c%c ", d->format.count, d->format.size,
		d->format.format);
      else if (d->format.format)
	printf_filtered ("/%c ", d->format.format);
      print_expression (d->exp, stdout);
      if (d->block && !contained_in (get_selected_block (), d->block))
	printf_filtered (" (cannot be evaluated in the current context)");
      printf_filtered ("\n");
      fflush (stdout);
    }
}

static void
enable_display (args, from_tty)
     char *args;
     int from_tty;
{
  register char *p = args;
  register char *p1;
  register int num;
  register struct display *d;

  if (p == 0)
    {
      for (d = display_chain; d; d = d->next)
	d->status = enabled;
    }
  else
    while (*p)
      {
	p1 = p;
	while (*p1 >= '0' && *p1 <= '9')
	  p1++;
	if (*p1 && *p1 != ' ' && *p1 != '\t')
	  error ("Arguments must be display numbers.");
	
	num = atoi (p);
	
	for (d = display_chain; d; d = d->next)
	  if (d->number == num)
	    {
	      d->status = enabled;
	      goto win;
	    }
	printf ("No display number %d.\n", num);
      win:
	p = p1;
	while (*p == ' ' || *p == '\t')
	  p++;
      }
}

/* ARGSUSED */
static void
disable_display_command (args, from_tty)
     char *args;
     int from_tty;
{
  register char *p = args;
  register char *p1;
  register struct display *d;

  if (p == 0)
    {
      for (d = display_chain; d; d = d->next)
	d->status = disabled;
    }
  else
    while (*p)
      {
	p1 = p;
	while (*p1 >= '0' && *p1 <= '9')
	  p1++;
	if (*p1 && *p1 != ' ' && *p1 != '\t')
	  error ("Arguments must be display numbers.");
	
	disable_display (atoi (p));

	p = p1;
	while (*p == ' ' || *p == '\t')
	  p++;
      }
}


/* Print the value in stack frame FRAME of a variable
   specified by a struct symbol.  */

void
print_variable_value (var, frame, stream)
     struct symbol *var;
     FRAME frame;
     FILE *stream;
{
  value val = read_var_value (var, frame);
  value_print (val, stream, 0, Val_pretty_default);
}

/* Print the arguments of a stack frame, given the function FUNC
   running in that frame (as a symbol), the info on the frame,
   and the number of args according to the stack frame (or -1 if unknown).  */

/* References here and elsewhere to "number of args according to the
   stack frame" appear in all cases to refer to "number of ints of args
   according to the stack frame".  At least for VAX, i386, isi.  */

/* Print arguments of a stack frame that have debugging symbols. */

static int
print_frame_named_args(func, fi, offp, stream)
  struct symbol *func;
  struct frame_info *fi;
  long *offp;
  FILE *stream;
{
  struct block *b = SYMBOL_BLOCK_VALUE (func);
  int nsyms = BLOCK_NSYMS (b);
  int first = 1;
  register int i;
  register struct symbol *sym;
  register value val;
  int arg_size;
  /* Number of ints of arguments that we have printed so far.  */
  int args_printed = 0;

  for (i = 0; i < nsyms; i++)
    {
      QUIT;
      sym = BLOCK_SYM (b, i);

      /* Keep track of the highest stack argument offset seen, and
	 skip over any kinds of symbols we don't care about.  */

      switch (SYMBOL_CLASS (sym)) {
      case LOC_ARG:
      case LOC_REF_ARG:
	{
	  long current_offset = SYMBOL_VALUE (sym);

	  arg_size = TYPE_LENGTH (SYMBOL_TYPE (sym));
	  
	  /* Compute address of next argument by adding the size of
	     this argument and rounding to an int boundary.  */
	  current_offset
	    = ((current_offset + arg_size + sizeof (int) - 1)
	       & ~(sizeof (int) - 1));

	  /* If this is the highest offset seen yet, remember it.  */
	  if (current_offset > *offp)
	    *offp = current_offset;

	  /* Add the number of ints we're about to print to args_printed.  */
	  args_printed += (arg_size + sizeof (int) - 1) / sizeof (int);
	}

      /* We care about types of symbols, but don't need to keep track of
	 stack offsets in them.  */
      case LOC_REGPARM:
      case LOC_LOCAL_ARG:
	break;

      /* Other types of symbols we just skip over.  */
      default:
	continue;
      }

      /* We have to re-look-up the symbol because arguments often have
	 two entries (one a parameter, one a register or local), and the one
	 we want is the non-parm, which lookup_symbol will find for
	 us.  After this, sym could be any SYMBOL_CLASS...  */
#ifdef IBM6000_TARGET
      /* AIX/RS6000 implements a concept of traceback tables, in which case
         it creates nameless parameters. Looking for those parameter symbols
         will result in an error. */

      if ( *SYMBOL_NAME (sym))
#endif
      sym = lookup_symbol (SYMBOL_NAME (sym),
		    b, VAR_NAMESPACE, (int *)NULL, (struct symtab **)NULL);

      /* Print the current arg.  */
      if (! first)
	fprintf_filtered (stream, ", ");
      wrap_here ("    ");
      fprint_symbol (stream, SYMBOL_NAME (sym));
      fputs_filtered ("=", stream);

      /* Avoid value_print because it will deref ref parameters.  We just
	 want to print their addresses.  Print ??? for args whose address
	 we do not know.  We pass 2 as "recurse" to val_print because our
	 standard indentation here is 4 spaces, and val_print indents
	 2 for each recurse.  */
      val = read_var_value (sym, FRAME_INFO_ID (fi));
      if (val)
        val_print (VALUE_TYPE (val), VALUE_CONTENTS (val), VALUE_ADDRESS (val),
		   stream, 0, 0, 2, Val_no_prettyprint);
      else
	fputs_filtered ("???", stream);
      first = 0;
    }
  return args_printed;
}

/* Print the arguments of a stack frame, given the function FUNC
   running in that frame (as a symbol), the info on the frame,
   and the number of args according to the stack frame (or -1 if unknown).  */

/* References here and elsewhere to "number of args according to the
   stack frame" appear in all cases to refer to "number of ints of args
   according to the stack frame".  At least for VAX, i386, isi.  */

static int default_funargs = 4;

void
print_frame_args (func, fi, num, stream)
     struct symbol *func;
     struct frame_info *fi;
     int num;
     FILE *stream;
{
  long offset = FRAME_ARGS_SKIP;

  if (func)
      num -= print_frame_named_args(func, fi, &offset, stream);
  else if (num < 0)
    /* If we don't have a debugging symbol for this routine, and we
       can't tell how many args there are, we should at least print 
       something useful */
    num = default_funargs;
  
  /* Don't print nameless args in situations where we don't know
     enough about the stack to find them.  */
  if (num > 0)
    {
      CORE_ADDR addr = FRAME_ARGS_ADDRESS (fi);

      if (addr)
        print_frame_nameless_args (fi, addr, offset, num, func == 0, stream);
    }
}

/* Print nameless args on STREAM.
   ARGSADDR is the address of the arglist, START is the offset
   of the first nameless arg, and NUM is the number of nameless args to
   print.  FIRST is nonzero if this is the first argument (not just
   the first nameless arg).  */
static void
print_frame_nameless_args (fi, argsaddr, start, num, first, stream)
     struct frame_info *fi;
     CORE_ADDR argsaddr;
     long start;
     int num;
     int first;
     FILE *stream;
{
  int i;
  LONGEST v;
  extern int output_format;
  for (i = 0; i < num; i++)
    {
      QUIT;
      if (!first)
	fprintf_filtered (stream, ", ");
      first = 0;
#ifdef NAMELESS_ARG
      NAMELESS_ARG(fi, i, v);
#else
      v = read_memory_integer (argsaddr + start, sizeof (int));
#endif

#ifdef PRINT_TYPELESS_INTEGER
      PRINT_TYPELESS_INTEGER (stream, builtin_type_int, v);
#else
      print_scalar_formatted ((char *)&v, builtin_type_int, 
			      output_format ? output_format : 'x', 0, stream);
#endif
      start += sizeof (int);
    }
}

/* ARGSUSED */
static void
printf_command (arg, from_tty)
     char *arg;
     int from_tty;
{
  register char *f;
  register char *s = arg;
  char *string;
  value *val_args;
  int nargs = 0;
  int allocated_args = 20;
  char *arg_bytes;

  val_args = (value *) xmalloc (allocated_args * sizeof (value));

  if (s == 0)
    error_no_arg ("format-control string and values to print");

  /* Skip white space before format string */
  while (*s == ' ' || *s == '\t') s++;

  /* A format string should follow, enveloped in double quotes */
  if (*s++ != '"')
    error ("Bad format string, missing '\"'.");

  /* Parse the format-control string and copy it into the string STRING,
     processing some kinds of escape sequence.  */

  f = string = (char *) alloca (strlen (s) + 1);
  while (*s != '"')
    {
      int c = *s++;
      switch (c)
	{
	case '\0':
	  error ("Bad format string, non-terminated '\"'.");
	  /* doesn't return */

	case '\\':
	  switch (c = *s++)
	    {
	    case '\\':
	      *f++ = '\\';
	      break;
	    case 'n':
	      *f++ = '\n';
	      break;
	    case 't':
	      *f++ = '\t';
	      break;
	    case 'r':
	      *f++ = '\r';
	      break;
	    case '"':
	      *f++ = '"';
	      break;
	    default:
	      /* ??? TODO: handle other escape sequences */
	      error ("Unrecognized \\ escape character in format string.");
	    }
	  break;

	default:
	  *f++ = c;
	}
    }

  /* Skip over " and following space and comma.  */
  s++;
  *f++ = '\0';
  while (*s == ' ' || *s == '\t') s++;

  if (*s != ',' && *s != 0)
    error ("Invalid argument syntax");

  if (*s == ',') s++;
  while (*s == ' ' || *s == '\t') s++;

  {
    /* Now scan the string for %-specs and see what kinds of args they want.
       argclass[I] classifies the %-specs so we can give vprintf something
       of the right size.  */
 
    enum argclass {int_arg, string_arg, double_arg, long_long_arg};
    enum argclass *argclass;
    int nargs_wanted;
    int argindex;
    int lcount;
    int i;
 
    argclass = (enum argclass *) alloca (strlen (s) * sizeof *argclass);
    nargs_wanted = 0;
    f = string;
    while (*f)
      if (*f++ == '%')
	{
	  lcount = 0;
	  while (strchr ("0123456789.hlL-+ #", *f)) 
	    {
	      if (*f == 'l' || *f == 'L')
		lcount++;
	      f++;
	    }
	  if (*f == 's')
	    argclass[nargs_wanted++] = string_arg;
	  else if (*f == 'e' || *f == 'f' || *f == 'g')
	    argclass[nargs_wanted++] = double_arg;
	  else if (lcount > 1)
	    argclass[nargs_wanted++] = long_long_arg;
	  else if (*f != '%')
	    argclass[nargs_wanted++] = int_arg;
	  f++;
	}
 
    /* Now, parse all arguments and evaluate them.
       Store the VALUEs in VAL_ARGS.  */
 
    while (*s != '\0')
      {
	char *s1;
	if (nargs == allocated_args)
	  val_args = (value *) xrealloc ((char *) val_args,
					 (allocated_args *= 2)
					 * sizeof (value));
	s1 = s;
	val_args[nargs] = parse_to_comma_and_eval (&s1);
 
	/* If format string wants a float, unchecked-convert the value to
	   floating point of the same size */
 
	if (argclass[nargs] == double_arg)
	  {
	    if (TYPE_LENGTH (VALUE_TYPE (val_args[nargs])) == sizeof (float))
	      VALUE_TYPE (val_args[nargs]) = builtin_type_float;
	    if (TYPE_LENGTH (VALUE_TYPE (val_args[nargs])) == sizeof (double))
	      VALUE_TYPE (val_args[nargs]) = builtin_type_double;
	  }
	nargs++;
	s = s1;
	if (*s == ',')
	  s++;
      }
 
    if (nargs != nargs_wanted)
      error ("Wrong number of arguments for specified format-string");
 
    /* Now lay out an argument-list containing the arguments
       as doubles, integers and C pointers.  */
 
    arg_bytes = (char *) alloca (sizeof (double) * nargs);
    argindex = 0;
    for (i = 0; i < nargs; i++)
      {
	if (argclass[i] == string_arg)
	  {
	    char *str;
	    CORE_ADDR tem;
	    int j;
	    tem = value_as_pointer (val_args[i]);
 
	    /* This is a %s argument.  Find the length of the string.  */
	    for (j = 0; ; j++)
	      {
		char c;
		QUIT;
		read_memory (tem + j, &c, 1);
		if (c == 0)
		  break;
	      }
 
	    /* Copy the string contents into a string inside GDB.  */
	    str = (char *) alloca (j + 1);
	    read_memory (tem, str, j);
	    str[j] = 0;
 
	    /* Pass address of internal copy as the arg to vprintf.  */
	    *((int *) &arg_bytes[argindex]) = (int) str;
	    argindex += sizeof (int);
	  }
	else if (VALUE_TYPE (val_args[i])->code == TYPE_CODE_FLT)
	  {
	    *((double *) &arg_bytes[argindex]) = value_as_double (val_args[i]);
	    argindex += sizeof (double);
	  }
	else
#ifdef LONG_LONG
	  if (argclass[i] == long_long_arg)
	    {
	      *(long long *) &arg_bytes[argindex] = value_as_long (val_args[i]);
	      argindex += sizeof (long long);
	    }
	  else
#endif
	    {
	      *((long *) &arg_bytes[argindex]) = value_as_long (val_args[i]);
	      argindex += sizeof (long);
	    }
      }
  }

  /* There is not a standard way to make a va_list, so we need
     to do various things for different systems.  */
#if defined (__INT_VARARGS_H)
  {
    va_list list;

    list.__va_arg = 0;
    list.__va_stk = (int *) arg_bytes;
    list.__va_reg = (int *) arg_bytes;
    vprintf (string, list);
  }
#else /* No __INT_VARARGS_H.  */
  vprintf (string, arg_bytes);
#endif /* No __INT_VARARGS_H.  */
}

/* Helper function for asdump_command.  Finds the bounds of a function
   for a specified section of text.  PC is an address within the
   function which you want bounds for; *LOW and *HIGH are set to the
   beginning (inclusive) and end (exclusive) of the function.  This
   function returns 1 on success and 0 on failure.  */

static int
containing_function_bounds (pc, low, high)
     CORE_ADDR pc, *low, *high;
{
  int scan;

  if (!find_pc_partial_function (pc, 0, low))
    return 0;

  scan = *low;
  do {
    scan++;
    if (!find_pc_partial_function (scan, 0, high))
      return 0;
  } while (*low == *high);

  return 1;
}

/* Dump a specified section of assembly code.  With no command line
   arguments, this command will dump the assembly code for the
   function surrounding the pc value in the selected frame.  With one
   argument, it will dump the assembly code surrounding that pc value.
   Two arguments are interpeted as bounds within which to dump
   assembly.  */

/* ARGSUSED */
static void
disassemble_command (arg, from_tty)
     char *arg;
     int from_tty;
{
  CORE_ADDR low, high;
  CORE_ADDR pc;
  char *space_index;

  if (!arg)
    {
      if (!selected_frame)
	error ("No frame selected.\n");

      pc = get_frame_pc (selected_frame);
      if (!containing_function_bounds (pc, &low, &high))
	error ("No function contains pc specified by selected frame.\n");
    }
  else if (!(space_index = (char *) strchr (arg, ' ')))
    {
      /* One argument.  */
      pc = parse_and_eval_address (arg);
      if (!containing_function_bounds (pc, &low, &high))
	error ("No function contains specified pc.\n");
    }
  else
    {
      /* Two arguments.  */
      *space_index = '\0';
      low = parse_and_eval_address (arg);
      high = parse_and_eval_address (space_index + 1);
    }

  printf_filtered ("Dump of assembler code ");
  if (!space_index)
    {
      char *name;
      find_pc_partial_function (pc, &name, 0);
      printf_filtered ("for function %s:\n", name);
    }
  else
    printf_filtered ("from %s ", local_hex_string(low));
    printf_filtered ("to %s:\n", local_hex_string(high));

  /* Dump the specified range.  */
  for (pc = low; pc < high; )
    {
      QUIT;
      print_address (pc, stdout);
      printf_filtered (":\t");
      pc += print_insn (pc, stdout);
      printf_filtered ("\n");
    }
  printf_filtered ("End of assembler dump.\n");
  fflush (stdout);
}

/*
 * A command to dump out memory as an array of chars.  Output is
 * much easier to read than "x /c".
 */
static void
chardump_command (arg, from_tty)
      char *arg;
      int from_tty;
{
      register CORE_ADDR addr;
      register char *cp;
      register int len, cc;
      char buf[64];

      if (arg == 0)
	      error("Must supply address argument.");
      cp = (char *)strchr(arg, ' ');
      addr = parse_and_eval_address(arg);
      /* XXX Should take length arg as an expression. */
      len = (cp != 0) ? atoi(cp) : 8 * sizeof(buf);
      while (len >= 0) {
#ifndef MIN
#define MIN(a,b) ((a)<(b)?(a):(b))
#endif
	      cc = MIN(len, sizeof(buf));
	      read_memory(addr, buf, sizeof(buf));
	      cp = buf;
	      while (--cc >= 0) {
		      register char c = *cp;

		      if (c == 0)
			      *cp = '*';
		      else if (!isprint(c))
			      *cp = '.';
		      ++cp;
	      }
	      len -= sizeof(buf);
	      addr += sizeof(buf);
      }
}

void
_initialize_printcmd ()
{
  current_display_number = -1;

  add_info ("address", address_info,
	   "Describe where variable VAR is stored.");

  add_com ("x", class_vars, x_command,
	   "Examine memory: x/FMT ADDRESS.\n\
ADDRESS is an expression for the memory address to examine.\n\
FMT is a repeat count followed by a format letter and a size letter.\n\
Format letters are o(octal), x(hex), d(decimal), u(unsigned decimal),\n\
 f(float), a(address), i(instruction), c(char) and s(string).\n\
Size letters are b(byte), h(halfword), w(word), g(giant, 8 bytes).\n\
  g is meaningful only with f, for type double.\n\
The specified number of objects of the specified size are printed\n\
according to the format.\n\n\
Defaults for format and size letters are those previously used.\n\
Default count is 1.  Default address is following last thing printed\n\
with this command or \"print\".");

  add_com ("disassemble", class_vars, disassemble_command,
	   "Disassemble a specified section of memory.\n\
Default is the function surrounding the pc of the selected frame.\n\
With a single argument, the function surrounding that address is dumped.\n\
Two arguments are taken as a range of memory to dump.");

  add_com ("ptype", class_vars, ptype_command,
	   "Print definition of type TYPE.\n\
Argument may be a type name defined by typedef, or \"struct STRUCTNAME\"\n\
or \"union UNIONNAME\" or \"enum ENUMNAME\".\n\
The selected stack frame's lexical context is used to look up the name.");

  add_com ("whatis", class_vars, whatis_command,
	   "Print data type of expression EXP.");

#if 0
  add_com ("whereis", class_vars, whereis_command,
	   "Print line number and file of definition of variable.");
#endif
  
  add_info ("display", display_info,
	    "Expressions to display when program stops, with code numbers.");

  add_cmd ("undisplay", class_vars, undisplay_command,
	   "Cancel some expressions to be displayed when program stops.\n\
Arguments are the code numbers of the expressions to stop displaying.\n\
No argument means cancel all automatic-display expressions.\n\
\"delete display\" has the same effect as this command.\n\
Do \"info display\" to see current list of code numbers.",
		  &cmdlist);

  add_com ("display", class_vars, display_command,
	   "Print value of expression EXP each time the program stops.\n\
/FMT may be used before EXP as in the \"print\" command.\n\
/FMT \"i\" or \"s\" or including a size-letter is allowed,\n\
as in the \"x\" command, and then EXP is used to get the address to examine\n\
and examining is done as in the \"x\" command.\n\n\
With no argument, display all currently requested auto-display expressions.\n\
Use \"undisplay\" to cancel display requests previously made.");

  add_cmd ("display", class_vars, enable_display, 
	   "Enable some expressions to be displayed when program stops.\n\
Arguments are the code numbers of the expressions to resume displaying.\n\
No argument means enable all automatic-display expressions.\n\
Do \"info display\" to see current list of code numbers.", &enablelist);

  add_cmd ("display", class_vars, disable_display_command, 
	   "Disable some expressions to be displayed when program stops.\n\
Arguments are the code numbers of the expressions to stop displaying.\n\
No argument means disable all automatic-display expressions.\n\
Do \"info display\" to see current list of code numbers.", &disablelist);

  add_cmd ("display", class_vars, undisplay_command, 
	   "Cancel some expressions to be displayed when program stops.\n\
Arguments are the code numbers of the expressions to stop displaying.\n\
No argument means cancel all automatic-display expressions.\n\
Do \"info display\" to see current list of code numbers.", &deletelist);

  add_com ("printf", class_vars, printf_command,
	"printf \"printf format string\", arg1, arg2, arg3, ..., argn\n\
This is useful for formatted output in user-defined commands.");
  add_com ("output", class_vars, output_command,
	   "Like \"print\" but don't put in value history and don't print newline.\n\
This is useful in user-defined commands.");

  add_prefix_cmd ("set", class_vars, set_command,
"Perform an assignment VAR = EXP.\n\
You must type the \"=\".  VAR may be a debugger \"convenience\" variable\n\
(names starting with $), a register (a few standard names starting with $),\n\
or an actual variable in the program being debugged.  EXP is any expression.\n\
Use \"set variable\" for variables with names identical to set subcommands.\n\
\nWith a subcommand, this command modifies parts of the gdb environment.\n\
You can see these environment settings with the \"show\" command.",
		  &setlist, "set ", 1, &cmdlist);

  /* "call" is the same as "set", but handy for dbx users to call fns. */
  add_com ("call", class_vars, call_command,
	   "Call a function in the inferior process.\n\
The argument is the function name and arguments, in the notation of the\n\
current working language.  The result is printed and saved in the value\n\
history, if it is not void.");

  add_cmd ("variable", class_vars, set_command,
	   "Perform an assignment VAR = EXP.\n\
You must type the \"=\".  VAR may be a debugger \"convenience\" variable\n\
(names starting with $), a register (a few standard names starting with $),\n\
or an actual variable in the program being debugged.  EXP is any expression.\n\
This may usually be abbreviated to simply \"set\".",
	   &setlist);

  add_com ("print", class_vars, print_command,
	   concat ("Print value of expression EXP.\n\
Variables accessible are those of the lexical environment of the selected\n\
stack frame, plus all those whose scope is global or an entire file.\n\
\n\
$NUM gets previous value number NUM.  $ and $$ are the last two values.\n\
$$NUM refers to NUM'th value back from the last one.\n\
Names starting with $ refer to registers (with the values they would have\n\
if the program were to return to the stack frame now selected, restoring\n\
all registers saved by frames farther in) or else to debugger\n\
\"convenience\" variables (any such name not a known register).\n\
Use assignment expressions to give values to convenience variables.\n",
		   "\n\
{TYPE}ADREXP refers to a datum of data type TYPE, located at address ADREXP.\n\
@ is a binary operator for treating consecutive data objects\n\
anywhere in memory as an array.  FOO@NUM gives an array whose first\n\
element is FOO, whose second element is stored in the space following\n\
where FOO is stored, etc.  FOO must be an expression whose value\n\
resides in memory.\n",
		   "\n\
EXP may be preceded with /FMT, where FMT is a format letter\n\
but no count or size letter (see \"x\" command).", NULL));
  add_com_alias ("p", "print", class_vars, 1);

  add_com ("inspect", class_vars, inspect_command,
"Same as \"print\" command, except that if you are running in the epoch\n\
environment, the value is printed in its own window.");

  add_com ("chardump", class_vars, chardump_command,
"Dump out a range of memory as characters.  Two arguments are starting\n\
address (as an arbitrary expression) and constant integer length.");

  add_show_from_set(
    add_set_cmd ("default-funargs", class_support, var_zinteger,
                       (char *)&default_funargs,
"Set the number of arguments to be printed for functions with\n\
no debugging info.\n",
                 &setlist), &showlist);

  add_show_from_set
    (add_set_cmd ("asm-symbolic", class_support, var_boolean, 
		  (char *)&asm_symbolic,
	"Set printing of symbolic offsets in disassmebly listings",
		  &setprintlist),
     &showprintlist);
}
