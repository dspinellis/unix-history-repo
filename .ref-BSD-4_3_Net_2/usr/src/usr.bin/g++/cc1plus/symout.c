/* Output GDB-format symbol table information from GNU compiler.
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


#include "config.h"
#include "tree.h"
#include "symseg.h"
#include "rtl.h"
#include "gdbfiles.h"
#include <stdio.h>
#undef NULL
/* <...> used here so one can prevent use of ./stddef.h
   by changing the -I options used.  */
#include <stddef.h>

/* Get N_SO from stab.h if we can expect the file to exist.  */
#ifdef DBX_DEBUGGING_INFO
#ifdef USG
#include "stab.h"  /* If doing DBX on sysV, use our own stab.h.  */
#else
#include <stab.h>  /* On BSD, use the system's stab.h.  */
#endif /* not USG */
#endif

/* .stabs code for source file name.  */
#ifndef N_SO
#define	N_SO 0x64
#endif

/* Unix maximum on file name length.  Needed for getwd.  */
#define MAXNAMLEN 1024

/* Get the number to output for a reference to type TYPE.  */
#define TYPE_OUTPUT_ADDRESS(TYPE) \
  TYPE_SYMTAB_ADDRESS (TYPE_MAIN_VARIANT (TYPE))

/* Stream for writing symbol table file.  */
static FILE *symfile;

/* Name of symbol table file.  */
static char *symfile_name;

/* Stream for writing to assembler file.  */
static FILE *asmfile;

/* Address for allocating space in symbol table file.
   Changes in this variable are paired globally with writes to symfile,
   but often we allocate many structures, advancing next_address,
   before writing any of them.  */
static int next_address;

/* Chain recording all the types that have been output,
   giving the address-in-the-symseg of each one.  */

struct typevec_elt
{
  int address;
  struct typevec_elt *next;
};

static struct typevec_elt *typevec;

/* Number of types recorded so far in the chain.  */

static int total_types;

/* Lists of types to which forward references have been made.
   Separate lists for temporary and permanent types.  */

static tree temporary_fwd_refs;
static tree permanent_fwd_refs;

/* `blockvec' is a chain recording all the symbol-blocks that have been output,
   giving the address-in-the-symseg of each one.  */

struct blockvec_elt
{
  int address;
  struct blockvec_elt *next;
};

static struct blockvec_elt *blockvec;

/* Number of blocks recorded so far in the chain.  */

static int total_blocks;

static void symout_range_bounds ();
static void symout_array_domain ();
static void symout_record_fields ();
static void symout_enum_values ();
static void symout_record_field_names ();
static void symout_enum_value_names ();
static int subrange_p ();
static void symout_strings_skip ();
static void symout_strings_print ();

/* At the beginning of compilation, start writing the symbol table.
   Initialize the type and block chain.
   Also open and initialize the symseg file.  */

void
symout_init (filename, asm_file, sourcename)
     char *filename;
     FILE *asm_file;
     char *sourcename;
{
  struct symbol_root buffer;

#ifdef VMS
  fatal ("Cannot write GDB debugging format on VMS");
#endif

  asmfile = asm_file;
  fprintf (asmfile, ".text 0\n.gdbbeg 0\n.gdbbeg 1\n");
  fprintf (asmfile,
	   "Ltext:\t.stabs \"%s\",%d,0,0,Ltext\n",
	   sourcename, N_SO);
  fprintf (asmfile, ".data 0\nLdata:\n");
  ASM_OUTPUT_LOCAL (asmfile, "Lbss", 0, 0);
  fprintf (asmfile, ".gdbsym Ldata,%d\n",
	   (char *) &buffer.databeg - (char *) &buffer);
  fprintf (asmfile, ".gdbsym Lbss,%d\n",
	   (char *) &buffer.bssbeg - (char *) &buffer);

  symfile = fopen (filename, "w");
  if (symfile == 0)
    pfatal_with_name (filename);
  symfile_name = (char *) malloc (strlen (filename) + 1);
  strcpy (symfile_name, filename);

  typevec = 0;
  blockvec = 0;
  total_types = 0;
  total_blocks = 0;

  permanent_fwd_refs = 0;
  temporary_fwd_refs = 0;

  bzero (&buffer, sizeof buffer);
  fwrite (&buffer, sizeof buffer, 1, symfile);

  next_address = sizeof buffer;
}

/* Functions for outputting strings into the symbol table.
   The string to be output is effectively the concatenation of
   the two strings P1 and P2.  Their lengths are given as S1 and S2.
   If P1 or P2 is zero, that string is not used.

   A null character is output to terminate the string,
   and it is followed by more nulls as padding to a word boundary.  */

static void
symout_strings (p1, s1, p2, s2)
     char *p1;
     int s1;
     char *p2;
     int s2;
{
  symout_strings_print (p1, s1, p2, s2);
  symout_strings_skip (p1, s1, p2, s2);
}

/* Like symout_strings but only output; do not update next_address.  */

static void
symout_strings_print (p1, s1, p2, s2)
     char *p1;
     int s1;
     char *p2;
     int s2;
{
  register int total;

  if (p1 && s1 == 0)
    s1 = strlen (p1);
  if (p2 && s2 == 0)
    s2 = strlen (p2);

  if (p1)
    fwrite (p1, s1, 1, symfile);
  if (p2)
    fwrite (p2, s2, 1, symfile);
  putc (0, symfile);

  total = s1 + s2 + 1;
  while (total % sizeof (int))
    {
      putc (0, symfile);
      total++;
    }
}

/* Like symout_strings but just update next_address; do not output.  */

static void
symout_strings_skip (p1, s1, p2, s2)
     char *p1;
     int s1;
     char *p2;
     int s2;
{
  register int total;

  if (p1 && s1 == 0)
    s1 = strlen (p1);
  if (p2 && s2 == 0)
    s2 = strlen (p2);

  total = s1 + s2 + 1;
  while (total % sizeof (int))
    total++;

  next_address += total;
}

/* Call here to output a chain of types.
   After each function, this is done first for the chain of permanent types
   made during the function, and then for the chain of temporary types.
   This must be done before outputting the symbols and blocks of the function.

   At the end of compilation, this is done for all the permanent types
   made since the last function.

   Each permanent type is done once, at the beginning of the next function,
   or at the end of the compilation if no functions follow.
   Once a type has been processed here, its TYPE_SYMTAB_ADDRESS remains
   set up.  */

void
symout_types (types)
     tree types;
{
  struct typerec
  {
    int number;
    int address;
    int nfields;
    int fields_address;
    int name_address;
    char *name;
    char *name_prefix;
  };

  register int n_types, i;
  register struct typerec *records;
  register tree next;
  struct type buffer;
  int this_run_address = next_address;

  /* Count the number of types to be handled here.  */

  for (next = types, n_types = 0;
       next;
       next = TREE_CHAIN (next), n_types++);

  records = (struct typerec *) alloca (n_types * sizeof (struct typerec));

  /* Compute the amount of space each type needs, updating next_address
     and storing the address of the data for each type.  */

  for (next = types, i = 0;
       next;
       next = TREE_CHAIN (next), i++)
    {
      register struct typevec_elt *velt
	= (struct typevec_elt *) xmalloc (sizeof (struct typevec_elt));
      velt->next = typevec;
      typevec = velt;

      total_types++;

      if (TYPE_NAME (next))
	{
	  records[i].name_address = next_address;

	  if (TREE_CODE (TYPE_NAME (next)) == IDENTIFIER_NODE)
	    {
	      records[i].name = IDENTIFIER_POINTER (TYPE_NAME (next));
	      switch (TREE_CODE (next))
		{
		case RECORD_TYPE:
		  records[i].name_prefix = "struct ";
		  break;

		case UNION_TYPE:
		  records[i].name_prefix = "union ";
		  break;

		case ENUMERAL_TYPE:
		  records[i].name_prefix = "enum ";
		  break;
		}
	    }
	  else
	    {
	      records[i].name = IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (next)));
	      records[i].name_prefix = 0;
	    }
	  symout_strings_skip (records[i].name_prefix, 0,
			       records[i].name, 0);

	}
      else
	{
	  records[i].name = 0;
	  records[i].name_address = 0;
	  records[i].name_prefix = 0;
	}

      /* If this type was forward-referenced from a previous call
	 to symout_types, store this type's address into the reference.  */
      if (TYPE_POINTER_TO (next) != 0
	  && TYPE_SYMTAB_ADDRESS (TYPE_POINTER_TO (next)) != 0
	  && TYPE_SYMTAB_ADDRESS (TYPE_POINTER_TO (next)) < this_run_address)
	{
	  int pos = ftell (symfile);
	  int myaddr = next_address;
	  fflush (symfile);
	  fseek (symfile,
		 (TYPE_SYMTAB_ADDRESS (TYPE_POINTER_TO (next))
		  + offsetof (struct type, target_type)),
		 0);
	  fwrite (&myaddr, sizeof (int), 1, symfile);
	  fflush (symfile);
	  fseek (symfile, pos, 0);
	}

      records[i].address = next_address;
      TYPE_SYMTAB_ADDRESS (next) = next_address;
      velt->address = next_address;
      next_address += sizeof (struct type);
      records[i].nfields = 0;
      records[i].fields_address = 0;
      switch (TREE_CODE (next))
	{
	case ARRAY_TYPE:
	  records[i].nfields
	    = (TYPE_DOMAIN(next)
	       ? ! integer_zerop (TYPE_MIN_VALUE (TYPE_DOMAIN (next)))
	       : 0 );
	  break;

	case INTEGER_TYPE:
	  if (subrange_p (next))
	    buffer.nfields = 2;
	  break;

	case RECORD_TYPE:
	case UNION_TYPE:
	case ENUMERAL_TYPE:
	  records[i].nfields = list_length (TYPE_FIELDS (next));
	}
      if (records[i].nfields)
	records[i].fields_address = next_address;
      next_address += records[i].nfields * sizeof (struct field);
    }

  /* Now write the data whose space we have assigned.
     First fill the data into BUFFER, then write BUFFER.  */

  for (next = types, i = 0;
       next;
       next = TREE_CHAIN (next), i++)
    {
      if (records[i].name)
	symout_strings_print (records[i].name_prefix, 0,
			      records[i].name, 0);

      if (TREE_TYPE (next) != 0 && TYPE_OUTPUT_ADDRESS (TREE_TYPE (next)) == 0)
	{
	  /* We are making a forward-reference to our target type.
	     Make a list of all of these.  */
	  if (TREE_PERMANENT (next))
	    permanent_fwd_refs
	      = perm_tree_cons (TREE_TYPE (next), 0, permanent_fwd_refs);
	  else
	    temporary_fwd_refs
	      = tree_cons (TREE_TYPE (next), 0, temporary_fwd_refs);
	}

      if (TYPE_SIZE (next) == 0)
	buffer.length = 0;
      else
	buffer.length
	  = (TREE_INT_CST_LOW (TYPE_SIZE (next))
	     * TYPE_SIZE_UNIT (next) / BITS_PER_UNIT);

      buffer.name = (char *) records[i].name_address;
      buffer.target_type = (struct type *) (TREE_TYPE (next) ? TYPE_OUTPUT_ADDRESS (TREE_TYPE (next)) : 0);

      buffer.pointer_type = 0;
      buffer.function_type = 0;
      buffer.flags
	= ((TREE_CODE (next) == INTEGER_TYPE || TREE_CODE (next) == ENUMERAL_TYPE)
	   && TREE_UNSIGNED (next))
	  ? TYPE_FLAG_UNSIGNED : 0;
      buffer.nfields = records[i].nfields;
      buffer.fields = (struct field *) records[i].fields_address;

      switch (TREE_CODE (next))
	{
	case INTEGER_TYPE:
	  buffer.code = TYPE_CODE_INT;
	  if (buffer.nfields)
	    buffer.code = TYPE_CODE_RANGE;
	  break;

	case REAL_TYPE:
	  buffer.code = TYPE_CODE_FLT;
	  break;

	case VOID_TYPE:
	  buffer.code = TYPE_CODE_VOID;
	  break;

	case POINTER_TYPE:
	  buffer.code = TYPE_CODE_PTR;
	  break;

	case ARRAY_TYPE:
	  if (buffer.nfields == 0)
	    buffer.code = TYPE_CODE_ARRAY;
	  else
	    buffer.code = TYPE_CODE_PASCAL_ARRAY;
	  break;

	case RECORD_TYPE:
	  buffer.code = TYPE_CODE_STRUCT;
	  break;

	case UNION_TYPE:
	  buffer.code = TYPE_CODE_UNION;
	  break;

	case FUNCTION_TYPE:
	  buffer.code = TYPE_CODE_FUNC;
	  break;

	case ENUMERAL_TYPE:
	  buffer.code = TYPE_CODE_ENUM;
	  break;

	default:
	  abort ();
	}

      fwrite (&buffer, sizeof buffer, 1, symfile);

      /* Now write the `struct field's that certain kinds of type have.
	 This allocates space for the names of those fields,
	 incrementing next_address for the names.  */

      switch (TREE_CODE (next))
	{
	case ARRAY_TYPE:
	  if (buffer.nfields)
	    symout_array_domain (next);
	  break;

	case RECORD_TYPE:
	case UNION_TYPE:
	  symout_record_fields (next);
	  break;

	case ENUMERAL_TYPE:
	  symout_enum_values (next);
	  break;

	case INTEGER_TYPE:
	  if (buffer.nfields)
	    symout_range_bounds (next);
	}
    }

  /* Now output the strings referred to by the fields of certain types.
     (next_address was already updated for these strings.)  */

  for (next = types, i = 0;
       next;
       next = TREE_CHAIN (next), i++)
    {
      switch (TREE_CODE (next))
	{
	case RECORD_TYPE:
	case UNION_TYPE:
	  symout_record_field_names (next);
	  break;

	case ENUMERAL_TYPE:
	  symout_enum_value_names (next);
	  break;
	}
    }
}

/* Given a list of types TYPES, return a chain of just those
   that haven't been written in the symbol table.  */

static tree
filter_undefined_types (types)
     tree types;
{
  tree new = 0;
  tree next;

  for (next = types; next; next = TREE_CHAIN (next))
    if (TYPE_SYMTAB_ADDRESS (TREE_PURPOSE (next)) == 0)
      {
	TREE_CHAIN (TREE_PURPOSE (next)) = new;
	new = TREE_PURPOSE (next);
      }

  return new;
}

/* Return nonzero if TYPE's range of possible values
   is not the full range allowed by the number of bits it has.
   TYPE is assumed to be an INTEGER_TYPE or ENUMERAL_TYPE.  */

static int
subrange_p (type)
     tree type;
{
  int uns = TREE_UNSIGNED (type);

  if (TYPE_PRECISION (type) >= HOST_BITS_PER_INT)
    {
      if (uns)
	return integer_zerop (TYPE_MIN_VALUE (type))
	  && TREE_INT_CST_LOW (TYPE_MAX_VALUE (type)) == 0
	    && (TREE_INT_CST_HIGH (TYPE_MAX_VALUE (type))
		== (1 << (TYPE_PRECISION (type) - HOST_BITS_PER_INT)) - 1);
      return TREE_INT_CST_LOW (TYPE_MIN_VALUE (type)) == 0
	&& TREE_INT_CST_LOW (TYPE_MAX_VALUE (type)) == 0
	  && (TREE_INT_CST_HIGH (TYPE_MIN_VALUE (type))
	      == (-1) << (TYPE_PRECISION (type) - 1 - HOST_BITS_PER_INT))
	    && (TREE_INT_CST_HIGH (TYPE_MAX_VALUE (type))
		== (1 << (TYPE_PRECISION (type) - 1 - HOST_BITS_PER_INT)) - 1);
    }

  if (uns)
    {
      int mask;

      if (TYPE_PRECISION (type) == HOST_BITS_PER_INT)
	/* Shifting by 32 loses on some machines.  */
	mask = -1;
      else
	mask = (1 << TYPE_PRECISION (type)) - 1;

      return (integer_zerop (TYPE_MIN_VALUE (type))
	      && (TREE_INT_CST_LOW (TYPE_MAX_VALUE (type)) == mask));
    }
  else
    return ((TREE_INT_CST_LOW (TYPE_MIN_VALUE (type))
	     == (-1) << (TYPE_PRECISION (type) - 1))
	    && (TREE_INT_CST_LOW (TYPE_MAX_VALUE (type))
		== (1 << (TYPE_PRECISION (type) - 1)) - 1));
}

/* Functions to output the "fields" of various kinds of types.
   These assume that next_address has already been incremented to
   cover these fields, and the fields of all the other types being
   output in this batch; so next_address can be used to allocate
   space to store field names, etc.  */

static void
symout_array_domain (type)
     tree type;
{
  struct field buffer;

  buffer.bitpos = 0;
  buffer.bitsize = 0;
  buffer.type = (struct type *) TYPE_OUTPUT_ADDRESS (TYPE_DOMAIN (type));
  buffer.name = 0;
  fwrite (&buffer, sizeof (struct field), 1, symfile);
}

static void
symout_range_bounds (type)
     tree type;
{
  struct field buffer;

  buffer.bitpos = TREE_INT_CST_LOW (TYPE_MIN_VALUE (type));
  buffer.bitsize = 0;
  buffer.type = (struct type *) TYPE_OUTPUT_ADDRESS (type);
  buffer.name = 0;
  fwrite (&buffer, sizeof (struct field), 1, symfile);

  buffer.bitpos = TREE_INT_CST_LOW (TYPE_MAX_VALUE (type));
  buffer.bitsize = 0;
  buffer.type = (struct type *) TYPE_OUTPUT_ADDRESS (type);
  buffer.name = 0;
  fwrite (&buffer, sizeof (struct field), 1, symfile);
}

static void
symout_record_fields (type)
     tree type;
{
  struct field buffer;
  register tree field;

  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    {
      buffer.bitpos = DECL_OFFSET (field);
      buffer.bitsize
	= (TREE_PACKED (field)
	   ? TREE_INT_CST_LOW (DECL_SIZE (field)) * DECL_SIZE_UNIT (field)
	   : 0);
      buffer.type = (struct type *) TYPE_OUTPUT_ADDRESS (TREE_TYPE (field));
      if (DECL_NAME (field))
	{
	  buffer.name = (char *) next_address;
	  symout_strings_skip (0, IDENTIFIER_LENGTH (DECL_NAME (field)), 0, 0);
	}
      else
	buffer.name = 0;
      fwrite (&buffer, sizeof (struct field), 1, symfile);
    }
}

static void
symout_enum_values (type)
     tree type;
{
  struct field buffer;
  register tree link, value;

  for (link = TYPE_VALUES (type); link; link = TREE_CHAIN (link))
    {
      value = TREE_VALUE (link);
      buffer.bitpos = TREE_INT_CST_LOW (value);
      buffer.bitsize = 0;
      buffer.type = (struct type *) TYPE_OUTPUT_ADDRESS (type);
      buffer.name = (char *) next_address;
      symout_strings_skip (0, IDENTIFIER_LENGTH (TREE_PURPOSE (link)), 0, 0);
      fwrite (&buffer, sizeof buffer, 1, symfile);
    }
}

/* Output field names or value names for the fields of a type.
   This is called, for the types that need it, after the fields
   have been output for all the types in the batch.
   We do not update next_address here, because it has already been 
   updated for all the names in all the fields in all the types.  */

static void
symout_record_field_names (type)
     tree type;
{
  register tree field;

  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
    if (DECL_NAME (field))
      symout_strings_print (IDENTIFIER_POINTER (DECL_NAME (field)),
			    IDENTIFIER_LENGTH (DECL_NAME (field)),
			    0, 0);
}

static void
symout_enum_value_names (type)
     tree type;
{
  register tree value;

  for (value = TYPE_VALUES (type); value; value = TREE_CHAIN (value))
    symout_strings_print (IDENTIFIER_POINTER (TREE_PURPOSE (value)),
			  IDENTIFIER_LENGTH (TREE_PURPOSE (value)),
			  0, 0);
}

/* Output the symbols of a block, given the list of decl nodes.
   Store the file addresses at which the symbols are output
   into ADDR_BUFFER, a vector which has just the right length.

   If FILTER is 1, do only the private symbols in DECLS.
   If FILTER is 2, do only the public ones (but no externals).
   If FILTER is 0, do all (except external functions).  */

static void
symout_block_symbols (decls, addr_buffer, filter)
     tree decls;
     int *addr_buffer;
     int filter;
{
  register tree decl;
  struct symbol buffer;
  register int i;

  for (decl = decls, i = 0; decl; decl = TREE_CHAIN (decl))
    {
      register int name_address = next_address;

      if (filter == (TREE_PUBLIC (decl) ? 1 : 2))
	continue;

      /* Do not mention external functions.
	 Let their own files mention them.
	 In the top blocks, don't mention external anything.  */

      if (TREE_EXTERNAL (decl)
	  && (filter || TREE_CODE (TREE_TYPE (decl)) == FUNCTION_TYPE))
	continue;

      if (TREE_TYPE (decl) == error_mark_node)
	continue;

      symout_strings (IDENTIFIER_POINTER (DECL_NAME (decl)),
		      IDENTIFIER_LENGTH (DECL_NAME (decl)),
		      0, 0);
      addr_buffer[i] = next_address;
      buffer.name = (char *) name_address;
      buffer.namespace = VAR_NAMESPACE;
      buffer.type = (struct type *) TYPE_OUTPUT_ADDRESS (TREE_TYPE (decl));
      switch (TREE_CODE (decl))
	{
	case PARM_DECL:
	  buffer.class = LOC_ARG;
	  buffer.value.value = DECL_OFFSET (decl) / BITS_PER_UNIT;
	  break;

	case VAR_DECL:
	case RESULT_DECL:
	  if (TREE_STATIC (decl) || TREE_EXTERNAL (decl))
	    {
	      if (! TREE_PUBLIC (decl) || DECL_INITIAL (decl))
		{
		  char *str = XSTR (XEXP (DECL_RTL (decl), 0), 0);
		  fprintf (asmfile, "\t.gdbsym ");
		  ASM_OUTPUT_LABELREF (asmfile, str);
		  fprintf (asmfile, ",%d\n",
			   next_address + (char *)&buffer.value - (char *)&buffer);
		  buffer.class = LOC_STATIC;
		}
	      else
		/* Uninitialized public symbols are output as .comm;
		   Tell GDB to get address from loader global symbol.
		   Also come here for symbols declared extern.  */
		buffer.class = LOC_EXTERNAL;
	    }
	  else
	    {
	      if (GET_CODE (DECL_RTL (decl)) == REG)
		{
		  buffer.class = LOC_REGISTER;
		  buffer.value.value = REGNO (DECL_RTL (decl));
		  /* Detect vars that were optimized entirely away.  */
		  if (buffer.value.value == -1)
		    buffer.class = LOC_CONST;
		}
	      else if (GET_CODE (DECL_RTL (decl)) == MEM
		       && (GET_CODE (XEXP (DECL_RTL (decl), 0)) == MEM
			   || (GET_CODE (XEXP (DECL_RTL (decl), 0)) == REG
			       && REGNO (XEXP (DECL_RTL (decl), 0)) != FRAME_POINTER_REGNUM)))
		/* If the value is indirect by memory or by a register
		   that isn't the frame pointer
		   then it means the object is variable-sized and address through
		   that register or stack slot.
		   If we have a pointer-type (which we should, for an array),
		   output the variable as a pointer.
		   Otherwise ignore it, since it is hard to create the ptr
		   type now and output it, and -gg is being retired.  */
		{
		  tree ptype = TYPE_POINTER_TO (TREE_TYPE (TREE_TYPE (decl)));
		  if (ptype == 0
		      || TYPE_OUTPUT_ADDRESS (ptype) == 0)
		    continue;

		  buffer.type = (struct type *) TYPE_OUTPUT_ADDRESS (ptype);


		  if (GET_CODE (XEXP (DECL_RTL (decl), 0)) == REG)
		    {
		      buffer.class = LOC_REGISTER;
		      buffer.value.value = REGNO (DECL_RTL (decl));
		      /* Detect vars that were optimized entirely away.  */
		      if (buffer.value.value == -1)
			buffer.class = LOC_CONST;
		    }
		  else
		    {
		      register rtx addr = XEXP (DECL_RTL (decl), 0);
		      if (GET_CODE (addr) != PLUS && GET_CODE (addr) != MINUS)
			abort ();
		      if (GET_CODE (XEXP (addr, 1)) != CONST_INT)
			abort ();
		      buffer.class = LOC_LOCAL;
		      buffer.value.value = INTVAL (XEXP (addr, 1));
		      if (GET_CODE (addr) == MINUS)
			buffer.value.value = - buffer.value.value;
		    }
		}
	      /* Locals in memory are expected to be addressed as
		 (PLUS (REG ...) (CONST_INT ...)).
		 Bomb out if that is not so.  */
	      else if (GET_CODE (DECL_RTL (decl)) == MEM)
		{
		  register rtx addr = XEXP (DECL_RTL (decl), 0);
		  if (GET_CODE (addr) != PLUS && GET_CODE (addr) != MINUS)
		    abort ();
		  if (GET_CODE (XEXP (addr, 1)) != CONST_INT)
		    abort ();
		  buffer.class = LOC_LOCAL;
		  buffer.value.value = INTVAL (XEXP (addr, 1));
		  if (GET_CODE (addr) == MINUS)
		    buffer.value.value = - buffer.value.value;
		}
	      else
		abort ();
	    }
	  break;

	case TYPE_DECL:
	  buffer.class = LOC_TYPEDEF;
	  buffer.value.value = 0;
	  break;

	case CONST_DECL:
	  buffer.class = LOC_CONST;
	  buffer.value.value = TREE_INT_CST_LOW (DECL_INITIAL (decl));
	  break;

	case FUNCTION_DECL:
	  if (DECL_INITIAL (decl))
	    {
	      buffer.class = LOC_BLOCK;
	      buffer.value.value = DECL_BLOCK_SYMTAB_ADDRESS (decl);
	    }
	  else
	    buffer.class = LOC_EXTERNAL;
	}

      fwrite (&buffer, sizeof buffer, 1, symfile);
      next_address += sizeof buffer;
      i++;
    }
}

/* Output the tags (struct, union and enum definitions) for a block,
   given a list of them (a chain of TREE_LIST nodes) in TAGS.
   Store their addresses in the file into ADDR_BUFFER.  */

static void
symout_block_tags (tags, addr_buffer)
     tree tags;
     int *addr_buffer;
{
  register tree tag;
  struct symbol buffer;
  register int i;

  for (tag = tags, i = 0; tag; tag = TREE_CHAIN (tag), i++)
    {
      buffer.name = (char *) next_address;

      symout_strings (IDENTIFIER_POINTER (TREE_PURPOSE (tag)),
		      IDENTIFIER_LENGTH (TREE_PURPOSE (tag)),
		      0, 0);
      addr_buffer[i] = next_address;
      buffer.namespace = STRUCT_NAMESPACE;
      buffer.type = (struct type *) TYPE_OUTPUT_ADDRESS (TREE_VALUE (tag));
      buffer.class = LOC_TYPEDEF;
      buffer.value.value = 0;

      fwrite (&buffer, sizeof buffer, 1, symfile);
      next_address += sizeof buffer;
    }
}

/* Output all the data structure for a "block"
   (any binding contour).
   DECLS is the chain of declarations of variables in this block.
   TAGS is the list of struct, union and enum tag definitions of this block.
   SUPERBLOCK_ADDRESS is the symtab file address of the containing block's
   data structure.  */

int
symout_block (decls, tags, args, superblock_address)
     tree decls;
     tree tags;
     tree args;
     int superblock_address;
{
  register tree decl;
  register int i;
  register int *addr_buffer;
  struct block buffer;
  int n_decls, n_tags, n_args, total;
  register struct blockvec_elt *velt;
  int block_address;

  for (decl = decls, i = 0; decl; decl = TREE_CHAIN (decl))
    if (! TREE_EXTERNAL (decl)
	|| TREE_CODE (TREE_TYPE (decl)) != FUNCTION_TYPE)
      i++;

  n_decls = i;

  for (decl = args, i = 0; decl; decl = TREE_CHAIN (decl), i++);
  n_args = i;

  for (decl = tags, i = 0; decl; decl = TREE_CHAIN (decl), i++);
  n_tags = i;

  total = n_decls + n_args + n_tags;

  addr_buffer = (int *) alloca (total * sizeof (int));

  symout_block_symbols (args, addr_buffer, 0);
  symout_block_symbols (decls, addr_buffer + n_args, 0);
  symout_block_tags (tags, addr_buffer + n_decls + n_args);

  velt = (struct blockvec_elt *) xmalloc (sizeof (struct blockvec_elt));
  velt->next = blockvec;
  velt->address = next_address;
  blockvec = velt;

  buffer.startaddr = 0;
  buffer.endaddr = 0;
  buffer.superblock = (struct block *) superblock_address;
  buffer.function = 0;
  buffer.nsyms = total;

  block_address = next_address;
  fwrite (&buffer, sizeof buffer - sizeof buffer.sym, 1, symfile);
  next_address += sizeof buffer - sizeof buffer.sym;

  fwrite (addr_buffer, sizeof (int), total, symfile);
  next_address += total * sizeof (int);

  fprintf (asmfile, "\t.gdbblock %d,%d\n", total_blocks + 2, block_address);
  total_blocks++;

  return block_address;
}

/* Walk STMT, the body of a function, and output symtab data on
   all the blocks that compose it and all symbols inside them.
   ARGS is a chain of decls for argument variables of the function.
   SUPERBLOCK_ADDRESS is the address of symbol data for the
   innermost block containing STMT; it is used for recursive calls,
   and is always 0 for the outermost call (since the containing
   block for a function is output later than the function).  */

int
symout_function (stmt, args, superblock_address)
     register tree stmt;
     tree args;
     int superblock_address;
{
  int address = superblock_address;

  while (stmt)
    {
      switch (TREE_CODE (stmt))
	{
	case COMPOUND_STMT:
	case LOOP_STMT:
	  symout_function (STMT_BODY (stmt), 0, address);
	  break;

	case IF_STMT:
	  symout_function (STMT_THEN (stmt), 0, address);
	  symout_function (STMT_ELSE (stmt), 0, address);
	  break;

	case LET_STMT:
	  /* Ignore LET_STMTs for blocks never really used to make RTL.  */
	  if (! TREE_USED (stmt))
	    break;
	  address =
	    symout_block (STMT_VARS (stmt), STMT_TYPE_TAGS (stmt), args,
			  superblock_address);

	  symout_function (STMT_SUBBLOCKS (stmt), 0, address);
	}
      stmt = TREE_CHAIN (stmt);
    }
  return address;
}

symout_function_end ()
{
  /* Output dummy entries for any undefined structure references.  */
  symout_types (filter_undefined_types (temporary_fwd_refs));
  temporary_fwd_refs = 0;
}

/* Output all the data structure for a top two blocks in a compilation.
   The top block is for public (global) symbols;
   the next one is for private (this file only) symbols.

   DECLS is the chain of declarations of variables in this block.
   TAGS is the list of struct, union and enum tag definitions.  */

void
symout_top_blocks (decls, tags)
     tree decls;
     tree tags;
{
  register tree decl;
  register int i;
  register int *addr_buffer;
  struct block buffer;
  int n_decls, n_tags;
  register struct blockvec_elt *velt;
  int top_block_addr;

  /* First do the public-symbols block.  */

  for (decl = decls, i = 0; decl; decl = TREE_CHAIN (decl))
    if (TREE_PUBLIC (decl) && ! TREE_EXTERNAL (decl))
      i++;
  n_decls = i;

  addr_buffer = (int *) alloca (n_decls * sizeof (int));

  symout_block_symbols (decls, addr_buffer, 2);

  fprintf (asmfile, ".text 0\n\t.gdbend 0\n");
  fprintf (asmfile, "\t.gdbblock 0,%d\n", next_address);

  total_blocks++;
  velt = (struct blockvec_elt *) xmalloc (sizeof (struct blockvec_elt));
  velt->next = blockvec;
  velt->address = next_address;
  blockvec = velt;

  top_block_addr = next_address;

  buffer.startaddr = 0;
  buffer.endaddr = 0;
  buffer.superblock = 0;
  buffer.function = 0;
  buffer.nsyms = n_decls;;

  fwrite (&buffer, sizeof buffer - sizeof buffer.sym, 1, symfile);
  next_address += sizeof buffer - sizeof buffer.sym;

  fwrite (addr_buffer, sizeof (int), n_decls, symfile);
  next_address += n_decls * sizeof (int);

  /* Next do the private-symbols block.  */

  for (decl = decls, i = 0; decl; decl = TREE_CHAIN (decl))
    if (! TREE_PUBLIC (decl) && ! TREE_EXTERNAL (decl))
      i++;
  n_decls = i;

  for (decl = tags, i = 0; decl; decl = TREE_CHAIN (decl), i++);
  n_tags = i;

  addr_buffer = (int *) alloca ((n_decls + n_tags) * sizeof (int));

  symout_block_symbols (decls, addr_buffer, 1);
  symout_block_tags (tags, addr_buffer + n_decls);

  fprintf (asmfile, "\t.gdbend 1\n");
  fprintf (asmfile, "\t.gdbblock 1,%d\n", next_address);

  total_blocks++;
  velt = (struct blockvec_elt *) xmalloc (sizeof (struct blockvec_elt));
  velt->next = blockvec;
  velt->address = next_address;
  blockvec = velt;

  buffer.startaddr = 0;
  buffer.endaddr = 0;
  buffer.superblock = (struct block *) top_block_addr;
  buffer.function = 0;
  buffer.nsyms = n_decls + n_tags;;

  fwrite (&buffer, sizeof buffer - sizeof buffer.sym, 1, symfile);
  next_address += sizeof buffer - sizeof buffer.sym;

  fwrite (addr_buffer, sizeof (int), n_decls + n_tags, symfile);
  next_address += (n_decls + n_tags) * sizeof (int);
}

/* Output the source-line-number information.  */

/* Output a `struct source' for the source file described by F.
   Return the address-in-the-symseg of the `struct source'.  */

static int
symout_source_file (f)
     struct gdbfile *f;
{
  /* Make the `struct source' big enough for as many lines as
     this file has.  */
  int size = sizeof (struct source) + (f->nlines - 1) * sizeof (struct line);
  struct source *buffer
    = (struct source *) alloca (size);
  int addr;

  /* Use zero for the line data, since assembler will store the real data.  */
  bzero (buffer, size);

  /* Output the file's name as a string.  The assembler doesn't know this.  */
  buffer->name = (char *) next_address;
  symout_strings (f->name, 0, 0, 0);
  buffer->nlines = f->nlines;

  /* Write the structure.  */
  addr = next_address;
  fwrite (buffer, 1, size, symfile);
  next_address += size;

  /* Tell assembler where to write the real line-number data.  */
  fprintf (asmfile, "\t.gdblinetab %d,%d\n",
	   f->filenum, addr + sizeof (int));

  return addr;
}

/* Output the `struct sourcevector' which describes all the
   source files and points a `struct source' for each one.  */

static int
symout_sources ()
{
  register struct gdbfile *f;
  int nfiles = 0;
  struct sourcevector *s;
  int i;
  int size;
  int addr;

  /* Count number of files to determine size of the sourcevector.  */
  for (f = gdbfiles; f; f = f->next)
    ++nfiles;

  /* Allocate buffer for the sourcevector and record its length.  */
  size = sizeof (int) + nfiles * sizeof (struct source *);
  s = (struct sourcevector *) alloca (size);
  s->length = nfiles;

  /* Output a `struct source' for each file; put address into sourcevector.  */
  for (f = gdbfiles, i = 0; f; f = f->next, i++)
    s->source[i] = (struct source *) symout_source_file (f);

  /* Output the sourcevector.  */
  addr = next_address;
  fwrite (s, 1, size, symfile);
  next_address += size;
  return addr;
}

/* Call here at the end of compilation, after outputting all the
   blocks and symbols, to output the blockvector and typevector
   and close the symbol table file.  FILETIME is source file's
   creation time.  */

void
symout_finish (filename, filetime)
     char *filename;
     int filetime;
{
  int *blockvector = (int *) alloca ((total_blocks + 1) * sizeof (int));
  int *typevector;
  int now = time (0);
  register int i;
  struct symbol_root buffer;
  char dir[MAXNAMLEN];

  /* Output dummy entries for any undefined structure references.  */
  symout_types (filter_undefined_types (permanent_fwd_refs));

  typevector = (int *) alloca ((total_types + 1) * sizeof (int));

  buffer.language = language_c;
  buffer.blockvector = (struct blockvector *) next_address;

  /* The two blocks at the beginning of the chain
     are the file's private symbols block and public symbols block.
     They belong at the front of the blockvector, in that order.  */
  blockvector[2] = blockvec->address;
  blockvec = blockvec->next;
  blockvector[1] = blockvec->address;
  blockvec = blockvec->next;

  /* The rest of the blocks are in the chain in reverse order.  */
  for (i = total_blocks; i > 2; i--)
    {
      blockvector[i] = blockvec->address;
      blockvec = blockvec->next;
    }
  blockvector[0] = total_blocks;

  fwrite (blockvector, sizeof (int), total_blocks + 1, symfile);
  next_address += sizeof (int) * (total_blocks + 1);

  buffer.typevector = (struct typevector *) next_address;

  for (i = total_types; i > 0; i--)
    {
      typevector[i] = typevec->address;
      typevec = typevec->next;
    }
  typevector[0] = total_types;

  fwrite (typevector, sizeof (int), total_types + 1, symfile);
  next_address += sizeof (int) * (total_types + 1);

  buffer.sourcevector = (struct sourcevector *) symout_sources ();

  buffer.format = 1;
  buffer.textrel = 0;		/* These four will be set up by linker.  */
  buffer.datarel = 0;		/* Make them 0 now, which is right for */
  buffer.bssrel = 0;		/* looking at the .o file in gdb.  */
  buffer.ldsymoff = 0;

  buffer.version = (char *) next_address;
  symout_strings (ctime (&filetime), 0, 0, 0);

  buffer.compilation = (char *) next_address;
  symout_strings (ctime (&now), 0, 0, 0);

  buffer.filename = (char *) next_address;
  symout_strings (filename, 0, 0, 0);

  buffer.filedir = (char *) next_address;
#ifdef USG
  strcpy (dir, getcwd (dir, MAXNAMLEN));
#else
#ifndef VMS
  getwd (dir);
#else
  abort ();
#endif
#endif
  symout_strings (dir, 0, 0, 0);

  fflush (symfile);

  if (ferror (symfile) != 0)
    fatal_io_error (symfile_name);

  buffer.length = next_address;

  if (lseek (fileno (symfile), 0, 0) < 0)
    pfatal_with_name (symfile_name);
  if (write (fileno (symfile), &buffer, sizeof buffer) < 0)
    pfatal_with_name (symfile_name);
  close (fileno (symfile));
}
