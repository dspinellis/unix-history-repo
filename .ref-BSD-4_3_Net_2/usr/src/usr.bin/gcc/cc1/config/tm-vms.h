/* Output variables, constants and external declarations, for GNU compiler.
   Copyright (C) 1988 Free Software Foundation, Inc.

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

#define NO_EXTERNAL_INDIRECT_ADDRESS  /* Alter some macro definitions.  */
#include "tm-vax.h"

#undef CPP_PREDEFINES
#undef TARGET_VERSION
#undef TARGET_DEFAULT
#undef CALL_USED_REGISTERS
#undef MAYBE_VMS_FUNCTION_PROLOGUE

/* Predefine this in CPP because VMS limits the size of command options
   and GNU CPP is not used on VMS except with GNU C.  */
#define CPP_PREDEFINES "-Dvax -Dvms -DVMS -D__GNU__ -D__GNUC__"

/* By default, allow $ to be part of an identifier.  */
#define DOLLARS_IN_IDENTIFIERS 1

#define TARGET_DEFAULT 1
#define TARGET_VERSION fprintf (stderr, " (vax vms)");

#define CALL_USED_REGISTERS {1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1}

#define __MAIN_NAME " main ("
/*
 * The MAYBE_VMS_FUNCTION_PROLOGUE macro works for both gcc and g++.  It
 * first checks to see if the current routine is "main", which will only
 * happen for GCC, and add the jsb if it is.  If is not the case then try and 
 * see if __MAIN_NAME is part of current_function_name, which will only happen
 * if we are running g++, and add the jsb if it is.  In gcc there should never
 * be a space in the function name, and in g++ there is always a "(" in the
 * function name, thus there should never be any confusion.
 */
#define MAYBE_VMS_FUNCTION_PROLOGUE(FILE)	\
{ extern char *current_function_name;		\
  if (!strcmp ("main", current_function_name))	\
    fprintf(FILE, "\tjsb _c$main_args\n"); 	\
  else {					\
    char *p = current_function_name;		\
    while (*p != '\0')				\
      if (*p == *__MAIN_NAME)			\
        if (strncmp(p, __MAIN_NAME, (sizeof __MAIN_NAME)-1) == 0) {\
          fprintf(FILE, "\tjsb _c$main_args\n");\
          break;				\
        } else					\
          p++;					\
      else					\
        p++;					\
     };						\
}

#define ASM_OUTPUT_EXTERNAL(FILE,DECL,NAME)		\
{ if (DECL_INITIAL (DECL) == 0 && TREE_CODE (DECL) != FUNCTION_DECL)	\
    {							\
      if (TREE_READONLY (decl) && ! TREE_VOLATILE (decl))	\
	const_section ();				\
      else						\
	data_section ();				\
      fputs (".comm ", (FILE));				\
      assemble_name ((FILE), (NAME));			\
      fprintf ((FILE), ",0\n");				\
    }							\
}

#define NO_DOLLAR_IN_LABEL

#define EXTRA_SECTIONS in_const

#define EXTRA_SECTION_FUNCTIONS				\
const_section ()					\
{							\
  if (in_section != in_const) {				\
    fprintf(asm_out_file,".const\n");			\
    in_section = in_const;				\
  }							\
}

#define SELECT_SECTION(T)						\
{									\
  if (TREE_CODE (T) == VAR_DECL)					\
    {									\
      if (TREE_READONLY (T) && ! TREE_VOLATILE (T))			\
	{								\
	  if (TREE_PUBLIC (T))						\
	    const_section ();						\
	  else								\
	    text_section ();						\
	}								\
      else								\
	data_section ();						\
    }									\
  if (*tree_code_type[(int) TREE_CODE (T)] == 'c')			\
    {									\
      if ((TREE_CODE (T) == STRING_CST && flag_writable_strings))	\
	data_section ();						\
      else								\
	text_section ();						\
    }									\
}

