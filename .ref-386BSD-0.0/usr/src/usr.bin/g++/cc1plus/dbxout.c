/* Output dbx-format symbol table information from GNU compiler.
   Copyright (C) 1987, 1988 Free Software Foundation, Inc.

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


/* Output dbx-format symbol table data.
   This consists of many symbol table entries, each of them
   a .stabs assembler pseudo-op with four operands:
   a "name" which is really a description of one symbol and its type,
   a "code", which is a symbol defined in stab.h whose name starts with N_,
   an unused operand always 0,
   and a "value" which is an address or an offset.
   The name is enclosed in doublequote characters.

   Each function, variable, typedef, and structure tag
   has a symbol table entry to define it.
   The beginning and end of each level of name scoping within
   a function are also marked by special symbol table entries.

   The "name" consists of the symbol name, a colon, a kind-of-symbol letter,
   and a data type number.  The data type number may be followed by
   "=" and a type definition; normally this will happen the first time
   the type number is mentioned.  The type definition may refer to
   other types by number, and those type numbers may be followed
   by "=" and nested definitions.

   This can make the "name" quite long.
   When a name is more than 80 characters, we split the .stabs pseudo-op
   into two .stabs pseudo-ops, both sharing the same "code" and "value".
   The first one is marked as continued with a double-backslash at the
   end of its "name".

   The kind-of-symbol letter distinguished function names from global
   variables from file-scope variables from parameters from auto
   variables in memory from typedef names from register variables.
   See `dbxout_symbol'.

   The "code" is mostly redundant with the kind-of-symbol letter
   that goes in the "name", but not entirely: for symbols located
   in static storage, the "code" says which segment the address is in,
   which controls how it is relocated.

   The "value" for a symbol in static storage
   is the core address of the symbol (actually, the assembler
   label for the symbol).  For a symbol located in a stack slot
   it is the stack offset; for one in a register, the register number.
   For a typedef symbol, it is zero.

   If DEBUG_SYMS_TEXT is defined, all debugging symbols must be
   output while in the text section.

   For more on data type definitions, see `dbxout_type'.  */

#include "config.h"
#include "tree.h"
#include "cplus-tree.h"
#include "rtl.h"
#include "flags.h"
#include <stdio.h>
#include "regs.h"

/* Typical USG systems don't have stab.h, and they also have
   no use for DBX-format debugging info.  */

#ifdef DBX_DEBUGGING_INFO

#ifdef DEBUG_SYMS_TEXT
#define FORCE_TEXT text_section ();
#else
#define FORCE_TEXT
#endif

#ifdef USG
#include "stab.h"  /* If doing DBX on sysV, use our own stab.h.  */
#else
#include <stab.h>  /* On BSD, use the system's stab.h.  */
#endif /* not USG */

/* Stream for writing to assembler file.  */

static FILE *asmfile;

enum typestatus {TYPE_UNSEEN, TYPE_XREF, TYPE_DEFINED};

/* Vector recording the status of describing C data types.
   When we first notice a data type (a tree node),
   we assign it a number using next_type_number.
   That is its index in this vector.
   The vector element says whether we have yet output
   the definition of the type.  TYPE_XREF says we have
   output it as a cross-reference only.  */

enum typestatus *typevec;

/* Number of elements of space allocated in `typevec'.  */

static int typevec_len;

/* In dbx output, each type gets a unique number.
   This is the number for the next type output.
   The number, once assigned, is in the TYPE_SYMTAB_ADDRESS field.  */

static int next_type_number;

/* In dbx output, we must assign symbol-blocks id numbers
   in the order in which their beginnings are encountered.
   We output debugging info that refers to the beginning and
   end of the ranges of code in each block
   with assembler labels LBBn and LBEn, where n is the block number.
   The labels are generated in final, which assigns numbers to the
   blocks in the same way.  */

static int next_block_number;

/* These variables are for dbxout_symbol to communicate to
   dbxout_finish_symbol.
   current_sym_code is the symbol-type-code, a symbol N_... define in stab.h.
   current_sym_value and current_sym_addr are two ways to address the
   value to store in the symtab entry.
   current_sym_addr if nonzero represents the value as an rtx.
   If that is zero, current_sym_value is used.  This is used
   when the value is an offset (such as for auto variables,
   register variables and parms).  */

static int current_sym_code;
static int current_sym_value;
static rtx current_sym_addr;

/* Number of chars of symbol-description generated so far for the
   current symbol.  Used by CHARS and CONTIN.  */

static int current_sym_nchars;

/* Report having output N chars of the current symbol-description.  */

#define CHARS(N) (current_sym_nchars += (N))

/* Break the current symbol-description, generating a continuation,
   if it has become long.  */

#ifndef DBX_CONTIN_LENGTH
#define DBX_CONTIN_LENGTH 80
#endif

#if DBX_CONTIN_LENGTH > 0
#define CONTIN  \
  do {if (current_sym_nchars > DBX_CONTIN_LENGTH) dbxout_continue ();} while (0)
#else
#define CONTIN
#endif

void dbxout_types ();
void dbxout_tags ();
void dbxout_args ();
void dbxout_symbol ();
static void dbxout_type_name ();
static void dbxout_type ();
static void dbxout_finish_symbol ();
static void dbxout_continue ();

/* At the beginning of compilation, start writing the symbol table.
   Initialize `typevec' and output the standard data types of C.  */

void
dbxout_init (asm_file, input_file_name)
     FILE *asm_file;
     char *input_file_name;
{
  asmfile = asm_file;

  typevec_len = 100;
  typevec = (enum typestatus *) xmalloc (typevec_len * sizeof typevec[0]);
  bzero (typevec, typevec_len * sizeof typevec[0]);

  /* Used to put `Ltext:' before the reference, but that loses on sun 4.  */
  fprintf (asmfile,
	   "\t.stabs \"%s\",%d,0,0,Ltext\nLtext:\n",
	   input_file_name, N_SO);

  next_type_number = 1;
  next_block_number = 2;

  /* Make sure that types `int' and `char' have numbers 1 and 2.
     Definitions of other integer types will refer to those numbers.  */

  dbxout_symbol (TYPE_NAME (integer_type_node), 0);
  dbxout_symbol (TYPE_NAME (char_type_node), 0);

  /* Get all permanent types not yet gotten, and output them.  */

  dbxout_types (get_permanent_types ());
}

/* Change by Bryan Boreham, Kewill, Sun Aug 13 15:31:25 1989.
   Added to support unexecing of compiler.    */

void
re_init_dbxout_for_unexec (asm_file, input_file_name)
     FILE *asm_file;
     char *input_file_name;
{
  asmfile = asm_file;
}

/* Continue a symbol-description that gets too big.
   End one symbol table entry with a double-backslash
   and start a new one, eventually producing something like
   .stabs "start......\\",code,0,value
   .stabs "...rest",code,0,value   */

static void
dbxout_continue ()
{
#ifdef DBX_CONTIN_CHAR
  fprintf (asmfile, "%c", DBX_CONTIN_CHAR);
#else
  fprintf (asmfile, "\\\\");
#endif
  dbxout_finish_symbol ();
  fprintf (asmfile, ".stabs \"");
  current_sym_nchars = 0;
}

static void
dbxout_type_fields (type)
     tree type;
{
  tree tem;
  for (tem = TYPE_FIELDS (type); tem; tem = TREE_CHAIN (tem))
    {
      /* Output the name, type, position (in bits), size (in bits)
	 of each field.  */
      if (DECL_ANON_UNION_ELEM (tem))
	dbxout_type_fields (TREE_TYPE (tem));
      /* Omit here local type decls until we know how to support them.  */
      else if (TREE_CODE (tem) == TYPE_DECL)
	continue;
      /* Omit here the nameless fields that are used to skip bits.  */
      else if (DECL_NAME (tem) != 0 && TREE_CODE (tem) != CONST_DECL)
	{
	  /* Continue the line if necessary,
	     but not before the first field.  */
	  if (tem != TYPE_FIELDS (type))
	    CONTIN;

	  if (use_gdb_dbx_extensions
	      && flag_minimal_debug
	      && TREE_CODE (tem) == FIELD_DECL
	      && (DECL_VIRTUAL_P (tem) || DECL_VBASE_P (tem)))
	    {
	      CHARS (3 + TYPE_NAME_LENGTH (DECL_FCONTEXT (tem)));
	      fprintf (asmfile, "$v%c",
		       DECL_VIRTUAL_P (tem) ? 'f' : 'b');
	      dbxout_type (DECL_FCONTEXT (tem), 0);
	      fprintf (asmfile, ":");
	      dbxout_type (TREE_TYPE (tem), 0);
	      fprintf (asmfile, ",%d;", DECL_OFFSET (tem));
	      continue;
	    }

	  fprintf (asmfile, "%s:", IDENTIFIER_POINTER (DECL_NAME (tem)));
	  CHARS (2 + IDENTIFIER_LENGTH (DECL_NAME (tem)));

	  if (use_gdb_dbx_extensions)
	    {
	      putc ('/', asmfile);
#ifdef TREE_PRIVATE
	      putc ((TREE_PRIVATE (tem) ? '0'
		     : TREE_PROTECTED (tem) ? '1' : '2'),
		    asmfile);
#endif
	      CHARS (2);
	    }

	  dbxout_type (TREE_TYPE (tem), 0);
	  if (TREE_CODE (tem) == VAR_DECL && TREE_STATIC (tem))
	    {
	      if (use_gdb_dbx_extensions)
		{
		  char *name = DECL_ASSEMBLER_NAME (tem);

		  /* Adding 1 here only works on systems
		     which flush an initial underscore from
		     the .stabs entry.  This loses for static names
		     which have an initial leading '_' on systems which
		     don't use leading underscores.  */
		  if (name[0] == '_')
		    name += 1;

		  fprintf (asmfile, ":%s;", name);
		  CHARS (strlen (name));
		}
	      else
		{
		  fprintf (asmfile, ",0,0;");
		}
	    }
#if 0
	  else if (TREE_CODE (tem) == VAR_DECL || TREE_CODE (tem) == CONST_DECL)
	    {
	      /* GDB 3.2 can't understand these declarations yet.  */
	      continue;
	    }
#endif
	  else
	    {
	      fprintf (asmfile, ",%d,%d;", DECL_OFFSET (tem),
		       TREE_INT_CST_LOW (DECL_SIZE (tem)) * DECL_SIZE_UNIT (tem));
	    }
	  CHARS (23);
	}
    }
}

/* Output a reference to a type.  If the type has not yet been
   described in the dbx output, output its definition now.
   For a type already defined, just refer to its definition
   using the type number.

   If FULL is nonzero, and the type has been described only with
   a forward-reference, output the definition now.
   If FULL is zero in this case, just refer to the forward-reference
   using the number previously allocated.  */

static void
dbxout_type (type, full)
     tree type;
     int full;
{
  register tree fields, tem, method_vec;
  tree *methods, *end;
  char *vfield_name = 0;
  tree virtual_basetype = 0;

  /* If there was an input error and we don't really have a type,
     avoid crashing and write something that is at least valid
     by assuming `int'.  */
  if (type == error_mark_node)
    type = integer_type_node;
  else /* if (TYPE_SIZE (type) == 0) */
    type = TYPE_MAIN_VARIANT (type);

  if (TYPE_SYMTAB_ADDRESS (type) == 0)
    {
      /* Type has no dbx number assigned.  Assign next available number.  */
      TYPE_SYMTAB_ADDRESS (type) = next_type_number++;

      /* Make sure type vector is long enough to record about this type.  */

      if (next_type_number == typevec_len)
	{
	  typevec = (enum typestatus *) xrealloc (typevec, typevec_len * 2 * sizeof typevec[0]);
	  bzero (typevec + typevec_len, typevec_len * sizeof typevec[0]);
	  typevec_len *= 2;
	}
    }

  /* Output the number of this type, to refer to it.  */
  fprintf (asmfile, "%d", TYPE_SYMTAB_ADDRESS (type));
  CHARS (3);

  /* If this type's definition has been output or is now being output,
     that is all.  */

  switch (typevec[TYPE_SYMTAB_ADDRESS (type)])
    {
    case TYPE_UNSEEN:
      break;
    case TYPE_XREF:
      if (! full)
	return;
      break;
    case TYPE_DEFINED:
      return;
    }

#ifdef DBX_NO_XREFS
  /* For systems where dbx output does not allow the `=xsNAME:' syntax,
     leave the type-number completely undefined rather than output
     a cross-reference.  */
  if (TREE_CODE (type) == RECORD_TYPE || TREE_CODE (type) == UNION_TYPE
      || TREE_CODE (type) == ENUMERAL_TYPE)

    if ((TYPE_NAME (type) != 0 && !full)
	|| TYPE_SIZE (type) == 0)
      {
	typevec[TYPE_SYMTAB_ADDRESS (type)] = TYPE_XREF;
	return;
      }
#endif

  /* Output a definition now.  */

  fprintf (asmfile, "=");
  CHARS (1);

  /* Mark it as defined, so that if it is self-referent
     we will not get into an infinite recursion of definitions.  */

  typevec[TYPE_SYMTAB_ADDRESS (type)] = TYPE_DEFINED;

  switch (TREE_CODE (type))
    {
    case VOID_TYPE:
    case LANG_TYPE:
      /* For a void type, just define it as itself; ie, "5=5".
	 This makes us consider it defined
	 without saying what it is.  The debugger will make it
	 a void type when the reference is seen, and nothing will
	 ever override that default.  */
      fprintf (asmfile, "%d", TYPE_SYMTAB_ADDRESS (type));
      CHARS (3);
      break;

    case INTEGER_TYPE:
      if (type == char_type_node && ! TREE_UNSIGNED (type))
	/* Output the type `char' as a subrange of itself!
	   I don't understand this definition, just copied it
	   from the output of pcc.  */
	fprintf (asmfile, "r2;0;127;");
      else
	/* Output other integer types as subranges of `int'.  */
	fprintf (asmfile, "r1;%d;%d;",
		 TREE_INT_CST_LOW (TYPE_MIN_VALUE (type)),
		 TREE_INT_CST_LOW (TYPE_MAX_VALUE (type)));
      CHARS (25);
      break;

    case REAL_TYPE:
      /* This must be magic.  */
      fprintf (asmfile, "r1;%d;0;",
	       TREE_INT_CST_LOW (size_in_bytes (type)));
      CHARS (16);
      break;

    case ARRAY_TYPE:
      /* Output "a" followed by a range type definition
	 for the index type of the array
	 followed by a reference to the target-type.
	 ar1;0;N;M for an array of type M and size N.  */
      fprintf (asmfile, "ar1;0;%d;",
	       (TYPE_DOMAIN (type)
		? TREE_INT_CST_LOW (TYPE_MAX_VALUE (TYPE_DOMAIN (type)))
	        : -1));
      CHARS (17);
      dbxout_type (TREE_TYPE (type), 0);
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
      {
	int i, n_baseclasses = CLASSTYPE_N_BASECLASSES (type);

	/* Output a structure type.  */
	if ((TYPE_NAME (type) != 0 && !full)
	    || TYPE_SIZE (type) == 0)
	  {
	    /* If the type is just a cross reference, output one
	       and mark the type as partially described.
	       If it later becomes defined, we will output
	       its real definition.
	       If the type has a name, don't nest its name within
	       another type's definition; instead, output an xref
	       and let the definition come when the name is defined.  */
	    fprintf (asmfile, (TREE_CODE (type) == RECORD_TYPE) ? "xs" : "xu");
	    CHARS (3);
	    dbxout_type_name (type);
	    fprintf (asmfile, ":");
	    typevec[TYPE_SYMTAB_ADDRESS (type)] = TYPE_XREF;
	    break;
	  }
	tem = size_in_bytes (type);
	fprintf (asmfile, (TREE_CODE (type) == RECORD_TYPE) ? "s%d" : "u%d",
		 TREE_INT_CST_LOW (tem));

	if (use_gdb_dbx_extensions)
	  {
	    if (n_baseclasses)
	      {
		fprintf (asmfile, "!%d,", n_baseclasses);
		CHARS (8);
	      }
	  }
	for (i = 1; i <= n_baseclasses; i++)
	  {
	    tree basetype = CLASSTYPE_BASECLASS (type, i);
	    if (use_gdb_dbx_extensions)
	      {
		putc (CLASSTYPE_VIA_VIRTUAL (type, i) ? '1'
		      : '0',
		      asmfile);
		putc (CLASSTYPE_VIA_PUBLIC (type, i) ? '2'
		      : '0',
		      asmfile);
		fprintf (asmfile, "%d,", DECL_OFFSET (TYPE_NAME (basetype)));
		CHARS (15);
		dbxout_type (basetype, 0);
		putc (';', asmfile);
	      }
	    else
	      {
		/* Print out the base class information with fields
		   which have the same names at the types they hold.  */
		dbxout_type_name (basetype);
		putc (':', asmfile);
		dbxout_type (basetype, full);
		fprintf (asmfile, ",%d,%d;",
			 DECL_OFFSET (TYPE_NAME (basetype)),
			 TREE_INT_CST_LOW (TYPE_SIZE (basetype)) * DECL_SIZE_UNIT (basetype));
		CHARS (20);
	      }
	  }
      }

      CHARS (11);

      /* Write out the field declarations.  */
      dbxout_type_fields (type);

      /* C++: put out the method names and their parameter lists */
      /* We do constructors, destructor, if any, followed by the method names.  */
      method_vec = use_gdb_dbx_extensions ? CLASSTYPE_METHOD_VEC (type) : NULL_TREE;
      methods = 0;
      end = 0;
      if (use_gdb_dbx_extensions
	  && TREE_CODE (type) == RECORD_TYPE
	  && (TYPE_HAS_DESTRUCTOR (type) | TYPE_HAS_CONSTRUCTOR (type)))
	{
	  tree dtor;

	  methods = &TREE_VEC_ELT (method_vec, 0);
	  end = TREE_VEC_END (method_vec);
	  /* Destructors lie in a special place.  */
	  if (TYPE_HAS_DESTRUCTOR (type))
	    {
	      dtor = TREE_VEC_ELT (method_vec, 0);
	      tem = TREE_CHAIN (dtor);
	    }
	  else
	    {
	      dtor = NULL_TREE;
	      tem = *methods;
	    }

	  CHARS (2);

	  if (tem)
	    {
	      if (TREE_OPERATOR (tem))
		/* Operators do not belong here.  This is for
		   constructors and destructors only.  */
		abort ();
	      else
		{
		  fprintf (asmfile, "%s::", IDENTIFIER_POINTER (DECL_ORIGINAL_NAME (tem)));
		  CHARS (IDENTIFIER_LENGTH (DECL_ORIGINAL_NAME (tem)) + 3);
		}

	      while (tem)
		{
		  /* Output the name of the field (after overloading), as
		     well as the name of the field before overloading, along
		     with its parameter list.  */
		  char c;
		  char *debug_name = IDENTIFIER_POINTER (DECL_NAME (tem));
		  int old_minimal_debug = flag_minimal_debug;

		  CONTIN;

		  if (tem == dtor)
		    /* Always output destructors with full information.  */
		    flag_minimal_debug = 0;

		  dbxout_type (TREE_TYPE (tem), 0);
		  flag_minimal_debug = old_minimal_debug;

		  if (DECL_VIRTUAL_P (tem))
		    c = '*';
		  else if (DECL_STATIC_FUNCTION_P (tem))
		    c = '?';
		  else
		    c = '.';

		  if (flag_minimal_debug && tem != dtor)
		    {
		      /* Cut down on debugging information by not outputting
			 the parts of the name we can just as easily
			 have the debugger figure out.  */

		      /* Get past '__'.  */
		      debug_name += 2;
		      /* Get past const and volatile qualifiers.  */
		      while (*debug_name == 'C' || *debug_name == 'V')
			debug_name++;
		      /* Get past numeric type length prefix.  */
		      while (*debug_name >= '0' && *debug_name <= '9')
			debug_name++;
		      /* Get past type of `this'.  */
		      debug_name += IDENTIFIER_LENGTH (DECL_NAME (TYPE_NAME (type)));
		    }
		  fprintf (asmfile, ":%s;%c%c", debug_name,
			   TREE_PRIVATE (tem) ? '0' : TREE_PROTECTED (tem) ? '1' : '2', c);
		  CHARS (IDENTIFIER_LENGTH (DECL_NAME (tem)) + 5
			 - (debug_name - IDENTIFIER_POINTER (DECL_NAME (tem))));
		  if (DECL_VIRTUAL_P (tem))
		    {
		      fprintf (asmfile, "%d;",
			       TREE_INT_CST_LOW (DECL_VINDEX (tem)));
		      CHARS (8);
		    }
		  if (tem == dtor)
		    break;
		  tem = TREE_CHAIN (tem);
		  if (tem == NULL_TREE)
		    tem = dtor;
		}
	      putc (';', asmfile);
	    }
	  if (methods != end)
	    methods++;
	}
      else if (method_vec != NULL_TREE)
	{
	  methods = &TREE_VEC_ELT (method_vec, 1);
	  end = TREE_VEC_END (method_vec);
	}

      for (; methods != end; methods++)
	{
	  tem = *methods;

	  if (tem)
	    {
	      if (TREE_OPERATOR (tem))
		{
		  char *name1 = operator_name_string (DECL_NAME (tem));
		  fprintf (asmfile, "op$::%s.", name1);
		  CHARS (strlen (name1) + 6);
		}
	      else
		{
		  tree name = DECL_ORIGINAL_NAME (tem);
		  fprintf (asmfile, "%s::", IDENTIFIER_POINTER (name));
		  CHARS (IDENTIFIER_LENGTH (name) + 3);
		}

	      for (; tem; tem = TREE_CHAIN (tem))
		/* Output the name of the field (after overloading), as
		   well as the name of the field before overloading, along
		   with its parameter list */
		{
		  /* @@ */
		  char c;
		  char *debug_name = IDENTIFIER_POINTER (DECL_NAME (tem));

		  CONTIN;

		  dbxout_type (TREE_TYPE (tem), 0);
		  if (DECL_VIRTUAL_P (tem))
		    c = '*';
		  else if (DECL_STATIC_FUNCTION_P (tem))
		    c = '?';
		  else
		    c = '.';

		  if (flag_minimal_debug)
		    {
		      debug_name += IDENTIFIER_LENGTH (DECL_ORIGINAL_NAME (tem)) + 2;
		      while (*debug_name == 'C' || *debug_name == 'V')
			debug_name++;
		      while (*debug_name >= '0' && *debug_name <= '9')
			debug_name++;
		      debug_name += IDENTIFIER_LENGTH (DECL_NAME (TYPE_NAME (type)));
		    }

		  fprintf (asmfile, ":%s;%c%c", debug_name,
			   TREE_PRIVATE (tem) ? '0' : TREE_PROTECTED (tem) ? '1' : '2', c);
		  CHARS (IDENTIFIER_LENGTH (DECL_NAME (tem)) + 6
			 - (debug_name - IDENTIFIER_POINTER (DECL_NAME (tem))));
		  if (DECL_VIRTUAL_P (tem))
		    {
		      fprintf (asmfile, "%d;",
			       TREE_INT_CST_LOW (DECL_VINDEX (tem)));
		      CHARS (8);
		    }
		}
	      putc (';', asmfile);
	      CHARS (1);
	    }
	}

      putc (';', asmfile);

      if (use_gdb_dbx_extensions && TREE_CODE (type) == RECORD_TYPE)
	{
	  /* Tell GDB+ that it may keep reading.  */
	  putc ('~', asmfile);
	  if (TYPE_HAS_DESTRUCTOR (type) && TYPE_HAS_CONSTRUCTOR (type))
	    putc ('=', asmfile);
	  else if (TYPE_HAS_DESTRUCTOR (type))
	    putc ('-', asmfile);
	  else if (TYPE_HAS_CONSTRUCTOR (type))
	    putc ('+', asmfile);

	  if (CLASSTYPE_VSIZE (type))
	    {
	      putc ('%', asmfile);
	      dbxout_type (DECL_FCONTEXT (CLASSTYPE_VFIELD (type)), 0);
	      fprintf (asmfile, ";");
	    }
	  else
	    {
	      putc (';', asmfile);
	      CHARS (3);
	    }
	}
      break;

    case ENUMERAL_TYPE:
      if ((TYPE_NAME (type) != 0
	   && !full
	   && ((TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
		&& ! ANON_AGGRNAME_P (DECL_NAME (TYPE_NAME (type))))
	       || (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE
		   && ! ANON_AGGRNAME_P (TYPE_NAME (type)))))
	  || TYPE_SIZE (type) == 0)
	{
	  fprintf (asmfile, "xe");
	  CHARS (3);
	  dbxout_type_name (type);
	  typevec[TYPE_SYMTAB_ADDRESS (type)] = TYPE_XREF;
	  fprintf (asmfile, ":");
	  return;
	}
      putc ('e', asmfile);
      CHARS (1);
      for (tem = TYPE_VALUES (type); tem; tem = TREE_CHAIN (tem))
	{
	  fprintf (asmfile, "%s:%d,", IDENTIFIER_POINTER (TREE_PURPOSE (tem)),
		   TREE_INT_CST_LOW (TREE_VALUE (tem)));
	  CHARS (11 + IDENTIFIER_LENGTH (TREE_PURPOSE (tem)));
	  if (TREE_CHAIN (tem) != 0)
	    CONTIN;
	}
      putc (';', asmfile);
      CHARS (1);
      break;

    case POINTER_TYPE:
      putc ('*', asmfile);
      CHARS (1);
      dbxout_type (TREE_TYPE (type), 0);
      break;

    case METHOD_TYPE:
      if (use_gdb_dbx_extensions)
	{
	  putc ('#', asmfile);
	  CHARS (1);
	  if (flag_minimal_debug)
	    {
	      putc ('#', asmfile);
	      dbxout_type (TREE_TYPE (type), 0);
	      putc (';', asmfile);
	      CHARS (1);
	    }
	  else
	    {
	      dbxout_type (TYPE_METHOD_BASETYPE (type), 0);
	      putc (',', asmfile);
	      CHARS (1);
	      dbxout_type (TREE_TYPE (type), 0);
	      dbxout_args (TYPE_ARG_TYPES (type));
	      putc (';', asmfile);
	      CHARS (1);
	    }
	}
      else
	{
	  /* Should print as an int, because it is really
	     just an offset.  */
	  dbxout_type (integer_type_node, 0);
	}
      break;

    case OFFSET_TYPE:
      if (use_gdb_dbx_extensions)
	{
	  putc ('@', asmfile);
	  CHARS (1);
	  dbxout_type (TYPE_OFFSET_BASETYPE (type), 0);
	  putc (',', asmfile);
	  CHARS (1);
	  dbxout_type (TREE_TYPE (type), 0);
	}
      else
	{
	  /* Should print as an int, because it is really
	     just an offset.  */
	  dbxout_type (integer_type_node, 0);
	}
      break;

    case REFERENCE_TYPE:
      putc (use_gdb_dbx_extensions ? '&' : '*', asmfile);
      CHARS (1);
      dbxout_type (TREE_TYPE (type), 0);
      break;

    case FUNCTION_TYPE:
      putc ('f', asmfile);
      CHARS (1);
      dbxout_type (TREE_TYPE (type), 0);
      break;

    default:
      abort ();
    }
}

/* Output the name of type TYPE, with no punctuation.
   Such names can be set up either by typedef declarations
   or by struct, enum and union tags.  */

static void
dbxout_type_name (type)
     register tree type;
{
  tree t;
  if (TYPE_NAME (type) == 0)
    abort ();
  if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
    {
      t = TYPE_NAME (type);
    }
  else if (TREE_CODE (TYPE_NAME (type)) == TYPE_DECL)
    {
      t = DECL_NAME (TYPE_NAME (type));
    }
  else
    abort ();

  fprintf (asmfile, "%s", IDENTIFIER_POINTER (t));
  CHARS (IDENTIFIER_LENGTH (t));
}

/* Output a .stabs for the symbol defined by DECL,
   which must be a ..._DECL node in the normal namespace.
   It may be a CONST_DECL, a FUNCTION_DECL, a PARM_DECL or a VAR_DECL.
   LOCAL is nonzero if the scope is less than the entire file.  */

void
dbxout_symbol (decl, local)
     tree decl;
     int local;
{
  int letter = 0;
  tree type = TREE_TYPE (decl);
  char *name;
  int regno = -1;

  /* If global, first output all types and all
     struct, enum and union tags that have been created
     and not yet output.  */

  if (local == 0)
    {
      /* Send out the tags first.  */
      dbxout_tags (gettags ());
      dbxout_types (get_permanent_types ());
    }

  current_sym_code = 0;
  current_sym_value = 0;
  current_sym_addr = 0;

  /* The output will always start with the symbol name,
     so count that always in the length-output-so-far.  */

  if (DECL_NAME (decl) == 0)
    return;

  current_sym_nchars = 2 + IDENTIFIER_LENGTH (DECL_NAME (decl));

  switch (TREE_CODE (decl))
    {
    case CONST_DECL:
      /* Enum values are defined by defining the enum type.  */
      break;

    case FUNCTION_DECL:
      if (DECL_RTL (decl) == 0)
	return;
      if (TREE_EXTERNAL (decl))
	break;
      if (GET_CODE (DECL_RTL (decl)) != MEM
	  || GET_CODE (XEXP (DECL_RTL (decl), 0)) != SYMBOL_REF)
	break;
      FORCE_TEXT;
      fprintf (asmfile, ".stabs \"%s:%c",
	       IDENTIFIER_POINTER (DECL_NAME (decl)),
	       TREE_PUBLIC (decl) ? 'F' : 'f');

      current_sym_code = N_FUN;
      current_sym_addr = XEXP (DECL_RTL (decl), 0);

      if (TREE_TYPE (TREE_TYPE (decl)))
	dbxout_type (TREE_TYPE (TREE_TYPE (decl)), 0);
      else
	dbxout_type (void_type_node, 0);
      dbxout_finish_symbol ();
      break;

    case TYPE_DECL:
#if 0
      /* This seems all wrong.  Outputting most kinds of types gives no name
	 at all.  A true definition gives no name; a cross-ref for a
	 structure can give the tag name, but not a type name.
	 It seems that no typedef name is defined by outputting a type.  */

      /* If this typedef name was defined by outputting the type,
	 don't duplicate it.  */
      if (typevec[TYPE_SYMTAB_ADDRESS (type)] == TYPE_DEFINED
	  && TYPE_NAME (TREE_TYPE (decl)) == decl)
	return;
#endif
      /* Don't output the same typedef twice.
         And don't output what language-specific stuff doesn't want output.  */
      if (TREE_ASM_WRITTEN (decl)
	  || lang_output_debug_info (TREE_TYPE (decl)) == 0)
	return;

      /* Output typedef name.  */
      FORCE_TEXT;
      fprintf (asmfile, ".stabs \"%s:t",
	       IDENTIFIER_POINTER (DECL_NAME (decl)));

      current_sym_code = N_LSYM;

      dbxout_type (TREE_TYPE (decl), 1);
      dbxout_finish_symbol ();

      /* Prevent duplicate output of a typedef.  */
      TREE_ASM_WRITTEN (decl) = 1;
      break;
      
    case PARM_DECL:
      /* Parm decls go in their own separate chains
	 and are output by dbxout_reg_parms and dbxout_parms.  */
      abort ();

    case RESULT_DECL:
      /* Named return value, treat like a VAR_DECL.  */
    case VAR_DECL:
      if (DECL_RTL (decl) == 0)
	return;
      /* Don't mention a variable that is external.
	 Let the file that defines it describe it.  */
      if (TREE_EXTERNAL (decl))
	break;

      /* If the variable is really a constant, inform dbx of such.  */
      if (TREE_STATIC (decl) && TREE_READONLY (decl)
	  && DECL_INITIAL (decl) != 0
	  && (DECL_FIELD_CONTEXT (decl) == NULL_TREE
	      || TREE_CODE (DECL_FIELD_CONTEXT (decl)) == LET_STMT))
	{
	  if (TREE_PUBLIC (decl) == 0)
	    {
	      /* The sun4 assembler does not grok this.  */
	      name = IDENTIFIER_POINTER (DECL_NAME (decl));
	      if (TREE_CODE (TREE_TYPE (decl)) == INTEGER_TYPE
		  || TREE_CODE (TREE_TYPE (decl)) == ENUMERAL_TYPE)
		{
		  int ival = TREE_INT_CST_LOW (DECL_INITIAL (decl));
		  fprintf (asmfile, ".stabs \"%s:c=i%d\",0x%x,0,0,0\n",
			   name, ival, N_LSYM);
		  return;
		}
	      else if (TREE_CODE (TREE_TYPE (decl)) == REAL_TYPE)
		{
		  /* don't know how to do this yet.  */
		}
	      break;
	    }
	  /* else it is something we handle like a normal variable.  */
	}

      /* Don't mention a variable at all
	 if it was completely optimized into nothingness.

	 If DECL was from an inline function, then it's rtl
	 is not identically the rtl that was used in this
	 particular compilation.  */
      if (GET_CODE (DECL_RTL (decl)) == REG)
	{
	  regno = REGNO (DECL_RTL (decl));
	  if (regno >= FIRST_PSEUDO_REGISTER)
	    regno = reg_renumber[REGNO (DECL_RTL (decl))];
	  if (regno < 0)
	    break;
	}

      /* The kind-of-variable letter depends on where
	 the variable is and on the scope of its name:
	 G and N_GSYM for static storage and global scope,
	 S for static storage and file scope,
	 V for static storage and local scope,
	    for those two, use N_LCSYM if data is in bss segment,
	    N_STSYM if in data segment, N_FUN otherwise.
	    (We used N_FUN originally, then changed to N_STSYM
	    to please GDB.  However, it seems that confused ld.
	    Now GDB has been fixed to like N_FUN, says Kingdon.)
	 no letter at all, and N_LSYM, for auto variable,
	 r and N_RSYM for register variable.  */

      if (GET_CODE (DECL_RTL (decl)) == MEM
	  && GET_CODE (XEXP (DECL_RTL (decl), 0)) == SYMBOL_REF)
	{
	  if (TREE_PUBLIC (decl))
	    {
	      letter = 'G';
	      current_sym_code = N_GSYM;
	    }
	  else
	    {
	      current_sym_addr = XEXP (DECL_RTL (decl), 0);

	      letter = TREE_PERMANENT (decl) ? 'S' : 'V';

	      if (!DECL_INITIAL (decl))
		current_sym_code = N_LCSYM;
	      else if (TREE_READONLY (decl) && ! TREE_VOLATILE (decl))
		/* This is not quite right, but it's the closest
		   of all the codes that Unix defines.  */
		current_sym_code = N_FUN;
	      else
		current_sym_code = N_STSYM;
	    }
	}
      else if (regno >= 0)
	{
	  letter = 'r';
	  current_sym_code = N_RSYM;
	  current_sym_value = DBX_REGISTER_NUMBER (regno);
	}
      else if (GET_CODE (DECL_RTL (decl)) == MEM
	       && (GET_CODE (XEXP (DECL_RTL (decl), 0)) == MEM
		   || (GET_CODE (XEXP (DECL_RTL (decl), 0)) == REG
		       && REGNO (XEXP (DECL_RTL (decl), 0)) != FRAME_POINTER_REGNUM)))
	/* If the value is indirect by memory or by a register
	   that isn't the frame pointer
	   then it means the object is variable-sized and address through
	   that register or stack slot.  DBX has no way to represent this
	   so all we can do is output the variable as a pointer.
	   If it's not a parameter, ignore it.
	   (VAR_DECLs like this can be made by integrate.c.)  */
	{
	  if (GET_CODE (XEXP (DECL_RTL (decl), 0)) == REG)
	    {
	      letter = 'r';
	      current_sym_code = N_RSYM;
	      current_sym_value = DBX_REGISTER_NUMBER (REGNO (XEXP (DECL_RTL (decl), 0)));
	    }
	  else
	    {
	      current_sym_code = N_LSYM;
	      /* DECL_RTL looks like (MEM (MEM (PLUS (REG...) (CONST_INT...)))).
		 We want the value of that CONST_INT.  */
	      current_sym_value = INTVAL (XEXP (XEXP (XEXP (DECL_RTL (decl), 0), 0), 1));
	    }

	  /* Effectively do build_pointer_type, but don't cache this type,
	     since it might be temporary whereas the type it points to
	     might have been saved for inlining.  */
	  type = make_node (POINTER_TYPE);
	  TREE_TYPE (type) = TREE_TYPE (decl);
	}
      else if (GET_CODE (DECL_RTL (decl)) == MEM
	       && GET_CODE (XEXP (DECL_RTL (decl), 0)) == REG)
	{
	  current_sym_code = N_LSYM;
	  current_sym_value = 0;
	}
      else if (GET_CODE (DECL_RTL (decl)) == MEM
	       && GET_CODE (XEXP (DECL_RTL (decl), 0)) == PLUS
	       && GET_CODE (XEXP (XEXP (DECL_RTL (decl), 0), 1)) == CONST_INT)
	{
	  current_sym_code = N_LSYM;
	  /* DECL_RTL looks like (MEM (PLUS (REG...) (CONST_INT...)))
	     We want the value of that CONST_INT.  */
	  current_sym_value = INTVAL (XEXP (XEXP (DECL_RTL (decl), 0), 1));
	}
      else
	/* Address might be a MEM, when DECL is a variable-sized object.
	   Or it might be const0_rtx, meaning previous passes
	   want us to ignore this variable.  */
	break;

      /* Ok, start a symtab entry and output the variable name.  */
      FORCE_TEXT;
      /* One slight hitch: if this is a VAR_DECL which is a static
	 class member, we must put out the mangled name instead of the
	 DECL_NAME.  */
      /* Note also that static member (variable) names DO NOT begin
	 with underscores in .stabs directives.  */
      if (DECL_LANG_SPECIFIC (decl))
	{
	  name = DECL_ASSEMBLER_NAME (decl);

	  /* Adding 1 here only works on systems
	     which flush an initial underscore.  */
	  if (name[0] == '_')
	    name += 1;
	}
      else name = IDENTIFIER_POINTER (DECL_NAME (decl));

      fprintf (asmfile, ".stabs \"%s:", name);
      if (letter) putc (letter, asmfile);
      dbxout_type (type, 0);
      dbxout_finish_symbol ();
      break;
    }
}

static void
dbxout_finish_symbol ()
{
  fprintf (asmfile, "\",%d,0,0,", current_sym_code);
  if (current_sym_addr)
    output_addr_const (asmfile, current_sym_addr);
  else
    fprintf (asmfile, "%d", current_sym_value);
  putc ('\n', asmfile);
}

/* Output definitions of all the decls in a chain.  */

static void
dbxout_syms (syms)
     tree syms;
{
  while (syms)
    {
      dbxout_symbol (syms, 1);
      syms = TREE_CHAIN (syms);
    }
}

/* The following two functions output definitions of function parameters.
   Each parameter gets a definition locating it in the parameter list.
   Each parameter that is a register variable gets a second definition
   locating it in the register.

   Printing or argument lists in gdb uses the definitions that
   locate in the parameter list.  But reference to the variable in
   expressions uses preferentially the definition as a register.  */

/* Output definitions, referring to storage in the parmlist,
   of all the parms in PARMS, which is a chain of PARM_DECL nodes.  */

static void
dbxout_parms (parms)
     tree parms;
{
  for (; parms; parms = TREE_CHAIN (parms))
    {
      int regno = -1;

      if (GET_CODE (DECL_RTL (parms)) == REG)
	regno = REGNO (DECL_RTL (parms));
      if (regno >= FIRST_PSEUDO_REGISTER)
	regno = reg_renumber[regno];

      if (DECL_OFFSET (parms) >= 0)
	{
	  current_sym_code = N_PSYM;
	  current_sym_value = DECL_OFFSET (parms) / BITS_PER_UNIT;
	  current_sym_addr = 0;

	  FORCE_TEXT;
	  if (DECL_NAME (parms))
	    {
	      current_sym_nchars = 2 + IDENTIFIER_LENGTH (DECL_NAME (parms));

	      fprintf (asmfile, ".stabs \"%s:p",
		       IDENTIFIER_POINTER (DECL_NAME (parms)));
	    }
	  else
	    {
	      current_sym_nchars = 8;
	      fprintf (asmfile, ".stabs \"(anon):p");
	    }

	  if (regno >= 0)
	    dbxout_type (DECL_ARG_TYPE (parms), 0);
	  else
	    {
	      /* This is the case where the parm is passed as an int or double
		 and it is converted to a char, short or float and stored back
		 in the parmlist.  In this case, describe the parm
		 with the variable's declared type, and adjust the address
		 if the least significant bytes (which we are using) are not
		 the first ones.  */
#ifdef BYTES_BIG_ENDIAN
	      if (TREE_TYPE (parms) != DECL_ARG_TYPE (parms))
		current_sym_value += (GET_MODE_SIZE (TYPE_MODE (DECL_ARG_TYPE (parms)))
				      - GET_MODE_SIZE (GET_MODE (DECL_RTL (parms))));
#endif

	      if (GET_CODE (DECL_RTL (parms)) == MEM
		  && GET_CODE (XEXP (DECL_RTL (parms), 0)) == PLUS
		  && GET_CODE (XEXP (XEXP (DECL_RTL (parms), 0), 1)) == CONST_INT
		  && INTVAL (XEXP (XEXP (DECL_RTL (parms), 0), 1)) == current_sym_value)
		dbxout_type (TREE_TYPE (parms), 0);
	      else
		{
		  current_sym_value = DECL_OFFSET (parms) / BITS_PER_UNIT;
		  dbxout_type (DECL_ARG_TYPE (parms), 0);
		}
	    }
	  dbxout_finish_symbol ();
	}
      /* Parm was passed in registers.
	 If it lives in a hard register, output a "regparm" symbol
	 for the register it lives in.  */
      else if (regno >= 0)
	{
	  current_sym_code = N_RSYM;
	  current_sym_value = DBX_REGISTER_NUMBER (regno);
	  current_sym_addr = 0;

	  FORCE_TEXT;
	  if (DECL_NAME (parms))
	    {
	      current_sym_nchars = 2 + IDENTIFIER_LENGTH (DECL_NAME (parms));
	      fprintf (asmfile, ".stabs \"%s:P",
		       IDENTIFIER_POINTER (DECL_NAME (parms)));
	    }
	  else
	    {
	      current_sym_nchars = 8;
	      fprintf (asmfile, ".stabs \"(anon):P");
	    }

	  dbxout_type (DECL_ARG_TYPE (parms), 0);
	  dbxout_finish_symbol ();
	}
      else if (GET_CODE (DECL_RTL (parms)) == MEM
	       && XEXP (DECL_RTL (parms), 0) != const0_rtx)
	{
	  current_sym_code = N_LSYM;
	  /* DECL_RTL looks like (MEM (PLUS (REG...) (CONST_INT...))).
	     We want the value of that CONST_INT.  */
	  current_sym_value = INTVAL (XEXP (XEXP (DECL_RTL (parms), 0), 1));
	  current_sym_addr = 0;

	  FORCE_TEXT;
	  if (DECL_NAME (parms))
	    {
	      current_sym_nchars = 2 + IDENTIFIER_LENGTH (DECL_NAME (parms));
	      fprintf (asmfile, ".stabs \"%s:p",
		       IDENTIFIER_POINTER (DECL_NAME (parms)));
	    }
	  else
	    {
	      current_sym_nchars = 8;
	      fprintf (asmfile, ".stabs \"(anon):p");
	    }

#if 0				/* This is actually the case in which a parameter
				   is passed in registers but lives on the stack in a local slot.
				   The address we are using is already correct, so don't change it.  */

	  /* This is the case where the parm is passed as an int or double
	     and it is converted to a char, short or float and stored back
	     in the parmlist.  In this case, describe the parm
	     with the variable's declared type, and adjust the address
	     if the least significant bytes (which we are using) are not
	     the first ones.  */
#ifdef BYTES_BIG_ENDIAN
	  if (TREE_TYPE (parms) != DECL_ARG_TYPE (parms))
	    current_sym_value += (GET_MODE_SIZE (TYPE_MODE (DECL_ARG_TYPE (parms)))
				  - GET_MODE_SIZE (GET_MODE (DECL_RTL (parms))));
#endif
#endif				/* 0 */

	  dbxout_type (TREE_TYPE (parms), 0);
	  dbxout_finish_symbol ();
	}
    }
}

/* Output definitions, referring to registers,
   of all the parms in PARMS which are stored in registers during the function.
   PARMS is a chain of PARM_DECL nodes.  */

static void
dbxout_reg_parms (parms)
     tree parms;
{
  while (parms)
    {
      int regno = -1;

      if (GET_CODE (DECL_RTL (parms)) == REG)
	regno = REGNO (DECL_RTL (parms));
      if (regno >= FIRST_PSEUDO_REGISTER)
	regno = reg_renumber[regno];

      /* Report parms that live in registers during the function.  */
      if (regno >= 0
	  && DECL_OFFSET (parms) >= 0)
	{
	  current_sym_code = N_RSYM;
	  current_sym_value = DBX_REGISTER_NUMBER (regno);
	  current_sym_addr = 0;

	  FORCE_TEXT;
	  if (DECL_NAME (parms))
	    {
	      current_sym_nchars = 2 + IDENTIFIER_LENGTH (DECL_NAME (parms));
	      fprintf (asmfile, ".stabs \"%s:r",
		       IDENTIFIER_POINTER (DECL_NAME (parms)));
	    }
	  else
	    {
	      current_sym_nchars = 8;
	      fprintf (asmfile, ".stabs \"(anon):r");
	    }
	  dbxout_type (TREE_TYPE (parms), 0);
	  dbxout_finish_symbol ();
	}
      /* Report parms that live in memory but outside the parmlist.  */
      else if (GET_CODE (DECL_RTL (parms)) == MEM
	       && GET_CODE (XEXP (DECL_RTL (parms), 0)) == PLUS
	       && GET_CODE (XEXP (XEXP (DECL_RTL (parms), 0), 1)) == CONST_INT)
	{
	  int offset = DECL_OFFSET (parms) / BITS_PER_UNIT;
	  /* A parm declared char is really passed as an int,
	     so it occupies the least significant bytes.
	     On a big-endian machine those are not the low-numbered ones.  */
#ifdef BYTES_BIG_ENDIAN
	  if (offset != -1 && TREE_TYPE (parms) != DECL_ARG_TYPE (parms))
	    offset += (GET_MODE_SIZE (TYPE_MODE (DECL_ARG_TYPE (parms)))
		       - GET_MODE_SIZE (GET_MODE (DECL_RTL (parms))));
#endif
	  if (INTVAL (XEXP (XEXP (DECL_RTL (parms), 0), 1)) != offset)
	    {
	      current_sym_code = N_LSYM;
	      current_sym_value = INTVAL (XEXP (XEXP (DECL_RTL (parms), 0), 1));
	      current_sym_addr = 0;
	      FORCE_TEXT;
	      if (DECL_NAME (parms))
		{
		  current_sym_nchars = 2 + IDENTIFIER_LENGTH (DECL_NAME (parms));
		  fprintf (asmfile, ".stabs \"%s:",
			   IDENTIFIER_POINTER (DECL_NAME (parms)));
		}
	      else
		{
		  current_sym_nchars = 8;
		  fprintf (asmfile, ".stabs \"(anon):");
		}
	      dbxout_type (TREE_TYPE (parms), 0);
	      dbxout_finish_symbol ();
	    }
	}
      parms = TREE_CHAIN (parms);
    }
}

/* Given a chain of ..._TYPE nodes (as come in a parameter list),
   output definitions of those names, in raw form */

void
dbxout_args (args)
     tree args;
{
  while (args)
    {
      putc (',', asmfile);
      dbxout_type (TREE_VALUE (args), 0);
      CHARS (1);
      args = TREE_CHAIN (args);
    }
}

/* Given a chain of ..._TYPE nodes,
   find those which have typedef names and output those names.
   This is to ensure those types get output.  */

void
dbxout_types (types)
     register tree types;
{
  while (types)
    {
      if (TYPE_NAME (types)
	  && TREE_CODE (TYPE_NAME (types)) == TYPE_DECL
	  && ! TREE_ASM_WRITTEN (TYPE_NAME (types)))
	dbxout_symbol (TYPE_NAME (types), 1);
      types = TREE_CHAIN (types);
    }
}

/* Output the tags (struct, union and enum definitions with names) for a block,
   given a list of them (a chain of TREE_LIST nodes) in TAGS.
   We must check to include those that have been mentioned already with
   only a cross-reference.  */

void
dbxout_tags (tags)
     tree tags;
{
  register tree link;
  for (link = tags; link; link = TREE_CHAIN (link))
    {
      register tree type = TYPE_MAIN_VARIANT (TREE_VALUE (link));
      if (TREE_PURPOSE (link) != 0
	  && ! TREE_ASM_WRITTEN (link)
	  && TYPE_SIZE (type) != 0
	  && lang_output_debug_info (type))
	{
	  TREE_ASM_WRITTEN (link) = 1;
	  current_sym_code = N_LSYM;
	  current_sym_value = 0;
	  current_sym_addr = 0;
	  current_sym_nchars = 2 + IDENTIFIER_LENGTH (TREE_PURPOSE (link));

	  FORCE_TEXT;
	  fprintf (asmfile, ".stabs \"%s:T",
		   ANON_AGGRNAME_P (TREE_PURPOSE (link))
		   ? "" : IDENTIFIER_POINTER (TREE_PURPOSE (link)));

	  /* If there is a typedecl for this type with the same name
	     as the tag, output an abbreviated form for that typedecl.  */
	  if (use_gdb_dbx_extensions
	      && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	      && DECL_NAME (TYPE_NAME (type)) == TREE_PURPOSE (link))
	    {
	      putc ('t', asmfile);
	      TREE_ASM_WRITTEN (TYPE_NAME (type)) = 1;
	    }
	  dbxout_type (type, 1);
	  dbxout_finish_symbol ();

/* Change by Bryan Boreham, Kewill, Fri Sep 22 16:57:42 1989.
   Added to make sure all fully-output structs have typedefs.    */

	  if (!ANON_AGGRNAME_P (TREE_PURPOSE (link))
	      && (TREE_CODE (TYPE_NAME (type)) != TYPE_DECL
		  || DECL_NAME (TYPE_NAME (type)) != TREE_PURPOSE (link)))
	    {
	      fprintf (asmfile, ".stabs \"%s:t",
		       IDENTIFIER_POINTER (TREE_PURPOSE (link)));
	      
	      current_sym_code = N_LSYM;

	      dbxout_type (type, 1);
	      dbxout_finish_symbol ();
	    }
	}
    }
}

/* Output everything about a symbol block (that is to say, a LET_STMT node
   that represents a scope level),
   including recursive output of contained blocks.

   STMT is the LET_STMT node.
   DEPTH is its depth within containing symbol blocks.
   ARGS is usually zero; but for the outermost block of the
   body of a function, it is a chain of PARM_DECLs for the function parameters.
   We output definitions of all the register parms
   as if they were local variables of that block.

   Actually, STMT may be several statements chained together.
   We handle them all in sequence.  */

static void
dbxout_block (stmt, depth, args)
     register tree stmt;
     int depth;
     tree args;
{
  int blocknum;

  while (stmt)
    {
      switch (TREE_CODE (stmt))
	{
	case COMPOUND_STMT:
	case LOOP_STMT:
	  dbxout_block (STMT_BODY (stmt), depth, 0);
	  break;

	case IF_STMT:
	  dbxout_block (STMT_THEN (stmt), depth, 0);
	  dbxout_block (STMT_ELSE (stmt), depth, 0);
	  break;

	case LET_STMT:
	  /* Ignore LET_STMTs for blocks never really used to make RTL.  */
	  if (! TREE_USED (stmt))
	    break;
	  /* In dbx format, the syms of a block come before the N_LBRAC.  */
	  dbxout_tags (STMT_TYPE_TAGS (stmt));
	  dbxout_syms (STMT_VARS (stmt));
	  if (args)
	    dbxout_reg_parms (args);

	  /* Now output an N_LBRAC symbol to represent the beginning of
	     the block.  Use the block's tree-walk order to generate
	     the assembler symbols LBBn and LBEn
	     that final will define around the code in this block.  */
	  if (depth > 0)
	    {
	      char buf[20];
	      blocknum = next_block_number++;
	      ASM_GENERATE_INTERNAL_LABEL (buf, "LBB", blocknum);

	      if (TREE_LANG_FLAG_1 (stmt) == 0
		  && TREE_LANG_FLAG_2 (stmt) == 1)
		{
		  /* A catch block.  Must precede N_LBRAC.  */
		  tree decl = STMT_VARS (stmt);
		  char *name = DECL_NAME (decl) == NULL_TREE
		    ? "default" : EXCEPTION_NAME_LENGTH + IDENTIFIER_POINTER (DECL_NAME (decl));
		  fprintf (asmfile, ".stabs \"%s:C1\",%d,0,0,", name, N_CATCH);
		  assemble_name (asmfile, buf);
		  fprintf (asmfile, "\n");
		}

	      fprintf (asmfile, ".stabn %d,0,0,", N_LBRAC);
	      assemble_name (asmfile, buf);
	      fprintf (asmfile, "\n");
	    }

	  /* Output the subblocks.  */
	  dbxout_block (STMT_SUBBLOCKS (stmt), depth + 1, 0);

	  /* Refer to the marker for the end of the block.  */
	  if (depth > 0)
	    {
	      char buf[20];
	      ASM_GENERATE_INTERNAL_LABEL (buf, "LBE", blocknum);
	      fprintf (asmfile, ".stabn %d,0,0,", N_RBRAC);
	      assemble_name (asmfile, buf);
	      fprintf (asmfile, "\n");
	    }
	}
      stmt = TREE_CHAIN (stmt);
    }
}

/* Output dbx data for a function definition.
   This includes a definition of the function name itself (a symbol),
   definitions of the parameters (locating them in the parameter list)
   and then output the block that makes up the function's body
   (including all the auto variables of the function).  */

void
dbxout_function (decl)
     tree decl;
{
  extern tree value_identifier;

  dbxout_symbol (decl, 0);
  dbxout_parms (DECL_ARGUMENTS (decl));
  if (DECL_NAME (DECL_RESULT (decl)) != value_identifier)
    dbxout_symbol (DECL_RESULT (decl), 1);
  dbxout_block (DECL_INITIAL (decl), 0, DECL_ARGUMENTS (decl));
  
  /* If we made any temporary types in this fn that weren't
     output, output them now.  */
  dbxout_types (get_temporary_types ());
}

/* GNU C++ extensions.  */

/* At the start of the file, emit symbolic information to orient
   GDB for this particular file's exception handling implementation.
   EH_TYPE is the type name of the exception type.
   EH_DECL is the global root of the exception handler stack.  */

void
dbxout_eh_init (eh_type, eh_decl)
     tree eh_type, eh_decl;
{
}

#else /* not DBX_DEBUGGING_INFO */

void
dbxout_init (asm_file, input_file_name)
     FILE *asm_file;
     char *input_file_name;
{}

void
dbxout_symbol (decl, local)
     tree decl;
     int local;
{}

void
dbxout_types (types)
     register tree types;
{}

void
dbxout_tags (tags)
     tree tags;
{}

void
dbxout_function (decl)
     tree decl;
{}

void
dbxout_eh_init (eh_type, eh_decl)
     tree eh_type, eh_decl;
{
}

#endif /* DBX_DEBUGGING_INFO */
