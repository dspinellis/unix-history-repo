/* Source-language-related definitions for GDB.
   Copyright 1991, 1992 Free Software Foundation, Inc.
   Contributed by the Department of Computer Science at the State University
   of New York at Buffalo.

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

#if !defined (LANGUAGE_H)
#define LANGUAGE_H 1

#ifdef __STDC__		/* Forward decls for prototypes */
struct value;
/* enum exp_opcode;	ANSI's `wisdom' didn't include forward enum decls. */
#endif

/* This used to be included to configure GDB for one or more specific
   languages.  Now it is shortcutted to configure for all of them.  FIXME.  */
/* #include "lang_def.h" */
#define	_LANG_c
#define	_LANG_m2

/* range_mode ==
   range_mode_auto:   range_check set automatically to default of language.
   range_mode_manual: range_check set manually by user.  */

extern enum range_mode {range_mode_auto, range_mode_manual} range_mode;

/* range_check ==
   range_check_on:    Ranges are checked in GDB expressions, producing errors.
   range_check_warn:  Ranges are checked, producing warnings.
   range_check_off:   Ranges are not checked in GDB expressions.  */

extern enum range_check
  {range_check_off, range_check_warn, range_check_on} range_check;

/* type_mode ==
   type_mode_auto:   type_check set automatically to default of language
   type_mode_manual: type_check set manually by user. */

extern enum type_mode {type_mode_auto, type_mode_manual} type_mode;

/* type_check ==
   type_check_on:    Types are checked in GDB expressions, producing errors.
   type_check_warn:  Types are checked, producing warnings.
   type_check_off:   Types are not checked in GDB expressions.  */

extern enum type_check
  {type_check_off, type_check_warn, type_check_on} type_check;

/* Structure tying together assorted information about a language.  */

struct language_defn {
  char *	   la_name;		/* Name of the language */
  enum language    la_language;		/* its symtab language-enum (defs.h) */
  struct type ** const
		  *la_builtin_type_vector;  /* Its builtin types */
  enum range_check la_range_check;	/* Default range checking */
  enum type_check  la_type_check;	/* Default type checking */
  int            (*la_parser) PARAMS((void));	/* Parser function */
  void           (*la_error) PARAMS ((char *)); /* Parser error function */
  struct type	 **la_longest_int;	/* Longest signed integral type */
  struct type	 **la_longest_unsigned_int; /* Longest uns integral type */
  struct type	 **la_longest_float;	/* Longest floating point type */
  char		  *la_hex_format;	/* Hexadecimal printf format str */
  char		  *la_hex_format_pre;	/* Prefix for custom format string */
  char		  *la_hex_format_suf;	/* Suffix for custom format string */
  char		  *la_octal_format;	/* Octal printf format str */
  char		  *la_octal_format_pre;	/* Prefix for custom format string */
  char		  *la_octal_format_suf;	/* Suffix for custom format string */
const struct op_print
		  *la_op_print_tab;	/* Table for printing expressions */
/* Add fields above this point, so the magic number is always last. */
  long 		   la_magic;		/* Magic number for compat checking */
};

#define LANG_MAGIC	910823L

/* Pointer to the language_defn for our current language.  This pointer
   always points to *some* valid struct; it can be used without checking
   it for validity.  */

extern const struct language_defn *current_language;

/* Pointer to the language_defn expected by the user, e.g. the language
   of main(), or the language we last mentioned in a message, or C.  */

extern const struct language_defn *expected_language;

/* language_mode == 
   language_mode_auto:   current_language automatically set upon selection
			 of scope (e.g. stack frame)
   language_mode_manual: current_language set only by user.  */

extern enum language_mode
  {language_mode_auto, language_mode_manual} language_mode;

/* These macros define the behaviour of the expression 
   evaluator.  */

/* Should we strictly type check expressions? */
#define STRICT_TYPE (type_check != type_check_off)

/* Should we range check values against the domain of their type? */
#define RANGE_CHECK (range_check != range_check_off)

/* "cast" really means conversion */
/* FIXME -- should be a setting in language_defn */
#define CAST_IS_CONVERSION (current_language->la_language == language_c)

extern void
language_info PARAMS ((int));

extern void
set_language PARAMS ((enum language));


/* This page contains functions that return things that are
   specific to languages.  Each of these functions is based on
   the current setting of working_lang, which the user sets
   with the "set language" command. */

/* Returns some built-in types */
#define	longest_int()		(*current_language->la_longest_int)
#define	longest_unsigned_int()	(*current_language->la_longest_unsigned_int)
#define	longest_float()		(*current_language->la_longest_float)

/* Hexadecimal number formatting is in defs.h because it is so common
   throughout GDB.  */

/* Return a format string for printf that will print a number in the local
   (language-specific) octal format.  Result is static and is
   overwritten by the next call.  local_octal_format_custom takes printf
   options like "08" or "l" (to produce e.g. %08x or %lx).  */

#define local_octal_format() (current_language->la_octal_format)

extern char *
local_octal_format_custom PARAMS ((char *));

/* Type predicates */

extern int
simple_type PARAMS ((struct type *));

extern int
ordered_type PARAMS ((struct type *));

extern int
same_type PARAMS ((struct type *, struct type *));

extern int
integral_type PARAMS ((struct type *));

extern int
numeric_type PARAMS ((struct type *));

extern int
character_type PARAMS ((struct type *));

extern int
boolean_type PARAMS ((struct type *));

extern int
float_type PARAMS ((struct type *));

extern int
pointer_type PARAMS ((struct type *));

extern int
structured_type PARAMS ((struct type *));

/* Checks Binary and Unary operations for semantic type correctness */
/* FIXME:  Does not appear to be used */
#define unop_type_check(v,o) binop_type_check((v),NULL,(o))

extern void
binop_type_check PARAMS ((struct value *, struct value *, int));

/* Error messages */

extern void
op_error PARAMS ((char *fmt, enum exp_opcode, int));

#define type_op_error(f,o) \
   op_error((f),(o),type_check==type_check_on ? 1 : 0)
#define range_op_error(f,o) \
   op_error((f),(o),range_check==range_check_on ? 1 : 0)

extern void
type_error ();

void
range_error ();

/* Data:  Does this value represent "truth" to the current language?  */

extern int
value_true PARAMS ((struct value *));

/* Misc:  The string representing a particular enum language.  */

extern char *
language_str PARAMS ((enum language));

/* Add a language to the set known by GDB (at initialization time).  */

extern void
add_language PARAMS ((const struct language_defn *));

extern enum language
get_frame_language PARAMS ((void));		/* In stack.c */

#endif	/* defined (LANGUAGE_H) */
