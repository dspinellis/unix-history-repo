/* Output sdb-format symbol table information from GNU compiler.
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

#define MAYBE

#include "config.h"

#ifdef SDB_DEBUGGING_INFO

#include "tree.h"
#include "rtl.h"
#include <stdio.h>

#if defined(USG) && !defined(MIPS_DEBUGGING_INFO)
#include <syms.h>
/* #include <storclass.h>  used to be this instead of syms.h.  */

#else
/* For cross compilation, use the portable defintions from the COFF
   documentation.  */

#define  C_EFCN          -1
#define  C_NULL          0
#define  C_AUTO          1
#define  C_EXT           2
#define  C_STAT          3
#define  C_REG           4
#define  C_EXTDEF        5
#define  C_LABEL         6
#define  C_ULABEL        7
#define  C_MOS           8
#define  C_ARG           9
#define  C_STRTAG        10
#define  C_MOU           11
#define  C_UNTAG         12
#define  C_TPDEF         13
#define  C_USTATIC       14
#define  C_ENTAG         15
#define  C_MOE           16
#define  C_REGPARM       17
#define  C_FIELD         18
#define  C_BLOCK         100
#define  C_FCN           101
#define  C_EOS           102
#define  C_FILE          103

#define  C_LINE          104
#define  C_ALIAS         105
#define  C_HIDDEN        106

#define  T_NULL     0
#define  T_ARG      1
#define  T_CHAR     2
#define  T_SHORT    3
#define  T_INT      4
#define  T_LONG     5
#define  T_FLOAT    6
#define  T_DOUBLE   7
#define  T_STRUCT   8
#define  T_UNION    9
#define  T_ENUM     10
#define  T_MOE      11
#define  T_UCHAR    12
#define  T_USHORT   13
#define  T_UINT     14
#define  T_ULONG    15

#define  DT_NON      0
#define  DT_PTR      1
#define  DT_FCN      2
#define  DT_ARY      3

#define  N_BTMASK     017
#define  N_TMASK      060
#define  N_TMASK1     0300
#define  N_TMASK2     0360
#define  N_BTSHFT     4
#define  N_TSHIFT     2
#endif

/* Line number of beginning of current function, minus one.
   Negative means not in a function or not using sdb.  */

int sdb_begin_function_line = -1;

/* Counter to generate unique "names" for nameless struct members.  */

static int unnamed_struct_number = 0;

extern FILE *asm_out_file;

extern tree current_function_decl;

void sdbout_init ();
void sdbout_symbol ();
void sdbout_tags();
void sdbout_types();

static void sdbout_syms ();
static void sdbout_one_type ();
static int plain_type_1 ();

/* Random macros describing parts of SDB data.  */

/* Put something here if lines get too long */
#define CONTIN

/* Maximum number of dimensions the assembler will allow.  */
#ifndef SDB_MAX_DIM
#define SDB_MAX_DIM 4
#endif

#ifndef PUT_SDB_SCL
#define PUT_SDB_SCL(a) fprintf(asm_out_file, "\t.scl\t%d;", (a))
#endif

#ifndef PUT_SDB_INT_VAL
#define PUT_SDB_INT_VAL(a) fprintf (asm_out_file, "\t.val\t%d;", (a))
#endif

#ifndef PUT_SDB_VAL
#define PUT_SDB_VAL(a)				\
( fputs ("\t.val\t", asm_out_file),		\
  output_addr_const (asm_out_file, (a)),	\
  fputc (';', asm_out_file))
#endif

#ifndef PUT_SDB_DEF
#define PUT_SDB_DEF(a)				\
do { fprintf (asm_out_file, "\t.def\t");	\
     ASM_OUTPUT_LABELREF (asm_out_file, a); 	\
     fprintf (asm_out_file, ";"); } while (0)
#endif

#ifndef PUT_SDB_PLAIN_DEF
#define PUT_SDB_PLAIN_DEF(a) fprintf(asm_out_file,"\t.def\t.%s;",a)
#endif

#ifndef PUT_SDB_ENDEF
#define PUT_SDB_ENDEF fputs("\t.endef\n", asm_out_file)
#endif

#ifndef PUT_SDB_TYPE
#define PUT_SDB_TYPE(a) fprintf(asm_out_file, "\t.type\t0%o;", a)
#endif

#ifndef PUT_SDB_SIZE
#define PUT_SDB_SIZE(a) fprintf(asm_out_file, "\t.size\t%d;", a)
#endif

#ifndef PUT_SDB_START_DIM
#define PUT_SDB_START_DIM fprintf(asm_out_file, "\t.dim\t")
#endif

#ifndef PUT_SDB_NEXT_DIM
#define PUT_SDB_NEXT_DIM(a) fprintf(asm_out_file, "%d,", a)
#endif

#ifndef PUT_SDB_LAST_DIM
#define PUT_SDB_LAST_DIM(a) fprintf(asm_out_file, "%d;", a)
#endif

#ifndef PUT_SDB_TAG
#define PUT_SDB_TAG(a)				\
do { fprintf (asm_out_file, "\t.tag\t");	\
     ASM_OUTPUT_LABELREF (asm_out_file, a);	\
     fprintf (asm_out_file, ";"); } while (0)
#endif

#ifndef PUT_SDB_BLOCK_START
#define PUT_SDB_BLOCK_START(LINE)		\
  fprintf (asm_out_file,			\
	   "\t.def\t.bb;\t.val\t.;\t.scl\t100;\t.line\t%d;\t.endef\n",	\
	   (LINE))
#endif

#ifndef PUT_SDB_BLOCK_END
#define PUT_SDB_BLOCK_END(LINE)			\
  fprintf (asm_out_file,			\
	   "\t.def\t.eb;.val\t.;\t.scl\t100;\t.line\t%d;\t.endef\n",	\
	   (LINE))
#endif

#ifndef PUT_SDB_FUNCTION_START
#define PUT_SDB_FUNCTION_START(LINE)		\
  fprintf (asm_out_file,			\
	   "\t.def\t.bf;\t.val\t.;\t.scl\t101;\t.line\t%d;\t.endef\n",	\
	   (LINE))
#endif

#ifndef PUT_SDB_FUNCTION_END
#define PUT_SDB_FUNCTION_END(LINE)		\
  fprintf (asm_out_file,			\
	   "\t.def\t.ef;\t.val\t.;\t.scl\t101;\t.line\t%d;\t.endef\n",	\
	   (LINE))
#endif

#ifndef PUT_SDB_EPILOGUE_END
#define PUT_SDB_EPILOGUE_END(NAME)		\
  fprintf (asm_out_file,			\
	   "\t.def\t%s;\t.val\t.;\t.scl\t-1;\t.endef\n",	\
	   (NAME))
#endif

#ifndef SDB_GENERATE_FAKE
#define SDB_GENERATE_FAKE(BUFFER, NUMBER) \
  sprintf ((BUFFER), ".%dfake", (NUMBER));
#endif

/* Return the sdb tag identifier string for TYPE
   if TYPE has already been defined; otherwise return a null pointer.   */
  
#define KNOWN_TYPE_TAG(type) (char *)(TYPE_SYMTAB_ADDRESS (type))

/* Set the sdb tag identifier string for TYPE to NAME.  */

#define SET_KNOWN_TYPE_TAG(TYPE, NAME) \
  (TYPE_SYMTAB_ADDRESS (TYPE) = (int)(NAME))

/* Return the name (a string) of the struct, union or enum tag
   described by the TREE_LIST node LINK.  This is 0 for an anonymous one.  */

#define TAG_NAME(link) \
  (((link) && TREE_PURPOSE ((link)) \
    && IDENTIFIER_POINTER (TREE_PURPOSE ((link)))) \
   ? IDENTIFIER_POINTER (TREE_PURPOSE ((link))) : (char *) 0)

/* Ensure we don't output a negative line number.  */
#define MAKE_LINE_SAFE(line)  \
  if (line <= sdb_begin_function_line) line = sdb_begin_function_line + 1

/* Tell the assembler the source file name.
   On systems that use SDB, this is done whether or not -g,
   so it is called by ASM_FILE_START.

   ASM_FILE is the assembler code output file,
   INPUT_NAME is the name of the main input file.  */

void
sdbout_filename (asm_file, input_name)
     FILE *asm_file;
     char *input_name;
{
  int len = strlen (input_name);
  char *na = input_name + len;

  /* NA gets INPUT_NAME sans directory names.  */
  while (na > input_name)
    {
      if (na[-1] == '/')
	break;
      na--;
    }

#ifdef ASM_OUTPUT_SOURCE_FILENAME
  ASM_OUTPUT_SOURCE_FILENAME (asm_file, na);
#else
  fprintf (asm_file, "\t.file\t\"%s\"\n", na);
#endif
}

/* Set up for SDB output at the start of compilation.  */

void
sdbout_init ()
{
  /* Output all the initial permanent types.  */
  sdbout_types (nreverse (get_permanent_types ()));
}

#if 0

/* return the tag identifier for type
 */

{
char *
tag_of_ru_type (type,link)
     tree type,link;
{
  if (TYPE_SYMTAB_ADDRESS (type))
    return (char *)TYPE_SYMTAB_ADDRESS (type);
  if (link &&
      TREE_PURPOSE (link)
      && IDENTIFIER_POINTER (TREE_PURPOSE (link)))
    TYPE_SYMTAB_ADDRESS (type) =
      (int)IDENTIFIER_POINTER (TREE_PURPOSE (link));
  else
    return (char *) TYPE_SYMTAB_ADDRESS (type);
}
#endif

/* Return a unique string to name an anonymous type.  */

static char *
gen_fake_label ()
{
  char label[10];
  char *labelstr;
  SDB_GENERATE_FAKE (label, unnamed_struct_number);
  unnamed_struct_number++;
  labelstr = (char *) permalloc (strlen (label) + 1);
  strcpy (labelstr, label);
  return labelstr;
}

/* Return the number which describes TYPE for SDB.
   For pointers, etc., this function is recursive.
   Each record, union or enumeral type must already have had a
   tag number output.  */

/* The number is given by d6d5d4d3d2d1bbbb 
   where bbbb is 4 bit basic type, and di indicate  one of notype,ptr,fn,array.
   Thus, char *foo () has bbbb=T_CHAR
			  d1=D_FCN
			  d2=D_PTR
 N_BTMASK=     017       1111     basic type field.
 N_TSHIFT=       2                derived type shift
 N_BTSHFT=       4                Basic type shift */

/* Produce the number that describes a pointer, function or array type.
   PREV is the number describing the target, value or element type.
   DT_type describes how to transform that type.  */
#define PUSH_DERIVED_LEVEL(DT_type,PREV) \
  ((((PREV)&~N_BTMASK)<<N_TSHIFT)|(DT_type<<N_BTSHFT)|(PREV&N_BTMASK))

/* Number of elements used in sdb_dims.  */
static int sdb_n_dims = 0;

/* Table of array dimensions of current type.  */
static int sdb_dims[SDB_MAX_DIM];

/* Size of outermost array currently being processed.  */
static int sdb_type_size = -1;

static int
plain_type (type)
     tree type;
{
  int val = plain_type_1 (type);

  /* If we have already saved up some array dimensions, print them now.  */
  if (sdb_n_dims > 0)
    {
      int i;
      PUT_SDB_START_DIM;
      for (i = sdb_n_dims - 1; i > 0; i--)
	PUT_SDB_NEXT_DIM (sdb_dims[i]);
      PUT_SDB_LAST_DIM (sdb_dims[0]);
      sdb_n_dims = 0;

      sdb_type_size = int_size_in_bytes (type);
      /* Don't kill sdb if type is not laid out or has variable size.  */
      if (sdb_type_size < 0)
	sdb_type_size = 0;
    }
  /* If we have computed the size of an array containing this type,
     print it now.  */
  if (sdb_type_size >= 0)
    {
      PUT_SDB_SIZE (sdb_type_size);
      sdb_type_size = -1;
    }
  return val;
}

static void
sdbout_record_type_name (type)
     tree type;
{
  char *name = 0;

  if (KNOWN_TYPE_TAG (type))
    return;

  if (TYPE_NAME (type) != 0) 
    {
      tree t = 0;
      /* Find the IDENTIFIER_NODE for the type name.  */
      if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
	{
	  t = TYPE_NAME (type);
	}
      else if (TREE_CODE (TYPE_NAME (type)) == TYPE_DECL)
	{
	  t = DECL_NAME (TYPE_NAME (type));
	}

      /* Now get the name as a string, or invent one.  */
      if (t != 0)
	name = IDENTIFIER_POINTER (t);
    }

  if (name == 0)
    name = gen_fake_label ();

  SET_KNOWN_TYPE_TAG (type, name);
}

static int
plain_type_1 (type)
     tree type;
{
  if (type == 0)  
    type = void_type_node;
  if (type == error_mark_node)
    type = integer_type_node;
  type = TYPE_MAIN_VARIANT (type);

  switch (TREE_CODE (type))
    {
    case VOID_TYPE:
      return T_INT;
    case INTEGER_TYPE:
      switch (int_size_in_bytes (type))
	{
	case 4:
	  return (TREE_UNSIGNED (type) ? T_UINT : T_INT);
	case 1:
	  return (TREE_UNSIGNED (type) ? T_UCHAR : T_CHAR);
	case 2:
	  return (TREE_UNSIGNED (type) ? T_USHORT : T_SHORT);
	default:
	  return 0;
	}
    case REAL_TYPE:
      switch (int_size_in_bytes (type))
	{
	case 4:
	  return T_FLOAT;
	default:
	  return T_DOUBLE;
	}

    case ARRAY_TYPE:
      {
	int m;
	m = plain_type_1 (TREE_TYPE (type));
	if (sdb_n_dims < SDB_MAX_DIM)
	  sdb_dims[sdb_n_dims++]
	    = (TYPE_DOMAIN (type)
	       ? TREE_INT_CST_LOW (TYPE_MAX_VALUE (TYPE_DOMAIN (type))) + 1
	       : 0);
	return PUSH_DERIVED_LEVEL (DT_ARY, m);
      }

    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      {
	char *tag;
	sdbout_record_type_name (type);
	if (TREE_ASM_WRITTEN (type)
#ifdef MAYBE
	    && KNOWN_TYPE_TAG (type)
#endif
	    )
	  {
	    /* Output the referenced structure tag name
	       only if the .def has already been output.
	       At least on 386, the Unix assembler
	       cannot handle forward references to tags.  */
	    tag = KNOWN_TYPE_TAG (type);
	    PUT_SDB_TAG (tag);
	  }
	sdb_type_size = int_size_in_bytes (type);
	if (sdb_type_size < 0)
	  sdb_type_size = 0;
	return ((TREE_CODE (type) == RECORD_TYPE) ? T_STRUCT
		: (TREE_CODE (type) == UNION_TYPE) ? T_UNION
		: T_ENUM);
      }
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      {
	int m = plain_type_1 (TREE_TYPE (type));
	return PUSH_DERIVED_LEVEL (DT_PTR, m);
      }
    case FUNCTION_TYPE:
    case METHOD_TYPE:
      {
	int m = plain_type_1 (TREE_TYPE (type));
	return PUSH_DERIVED_LEVEL (DT_FCN, m);
      }
    default:
      return 0;
    }
}

/* Output the symbols defined in block number DO_BLOCK.
   Set NEXT_BLOCK_NUMBER to 0 before calling.

   This function works by walking the tree structure,
   counting blocks, until it finds the desired block.  */

static int do_block = 0;

static int next_block_number;

static void
sdbout_block (stmt)
     register tree stmt;
{
  while (stmt)
    {
      switch (TREE_CODE (stmt))
	{
	case COMPOUND_STMT:
	case LOOP_STMT:
	  sdbout_block (STMT_BODY (stmt));
	  break;

	case IF_STMT:
	  sdbout_block (STMT_THEN (stmt));
	  sdbout_block (STMT_ELSE (stmt));
	  break;

	case LET_STMT:
	  /* Ignore LET_STMTs for blocks never really used to make RTL.  */
	  if (! TREE_USED (stmt))
	    break;
	  /* When we reach the specified block, output its symbols.  */
	  if (next_block_number == do_block)
	    {
	      sdbout_tags (STMT_TYPE_TAGS (stmt));
	      sdbout_syms (STMT_VARS (stmt));
	    }

	  /* If we are past the specified block, stop the scan.  */
	  if (next_block_number > do_block)
	    return;

	  next_block_number++;

	  /* Scan the blocks within this block.  */
	  sdbout_block (STMT_SUBBLOCKS (stmt));
	}
      stmt = TREE_CHAIN (stmt);
    }
}

/* Call sdbout_symbol on each decl in the chain SYMS.  */

static void
sdbout_syms (syms)
     tree syms;
{
  while (syms)
    {
      sdbout_symbol (syms, 1);
      syms = TREE_CHAIN (syms);
    }
}

/* Output SDB information for a symbol described by DECL.
   LOCAL is nonzero if the symbol is not file-scope.  */

void
sdbout_symbol (decl, local)
     tree decl;
     int local;
{
  int letter = 0;
  tree type = TREE_TYPE (decl);
  rtx value;

  /* If global, first output all types and all
     struct, enum and union tags that have been created
     and not yet output.  */

  if (local == 0)
    {
      sdbout_tags (gettags ());
      sdbout_types (nreverse (get_permanent_types ()));
    }

#ifdef MAYBE
  sdbout_one_type (type);
#endif

  switch (TREE_CODE (decl))
    {
    case CONST_DECL:
      /* Enum values are defined by defining the enum type.  */
      return;

    case FUNCTION_DECL:
      if (TREE_EXTERNAL (decl))
	return;
      if (GET_CODE (DECL_RTL (decl)) != MEM
	  || GET_CODE (XEXP (DECL_RTL (decl), 0)) != SYMBOL_REF)
	return;
      PUT_SDB_DEF (IDENTIFIER_POINTER (DECL_NAME (decl)));
      PUT_SDB_VAL (XEXP (DECL_RTL (decl), 0));
      PUT_SDB_SCL (TREE_PUBLIC (decl) ? C_EXT : C_STAT);
      break;

    case TYPE_DECL:
      /* Output typedef name.  */
      PUT_SDB_DEF (IDENTIFIER_POINTER (DECL_NAME (decl)));
      PUT_SDB_SCL (C_TPDEF);
      break;
      
    case PARM_DECL:
      /* Parm decls go in their own separate chains
	 and are output by sdbout_reg_parms and sdbout_parms.  */
      abort ();

    case VAR_DECL:
      /* Don't mention a variable that is external.
	 Let the file that defines it describe it.  */
      if (TREE_EXTERNAL (decl))
	return;

      value = DECL_RTL (decl);

      /* Don't mention a variable at all
	 if it was completely optimized into nothingness.  */
      if (GET_CODE (value) == REG
	  && (REGNO (value) < 0
	      || REGNO (value) >= FIRST_PSEUDO_REGISTER))
	return;

      /* Ok, start a symtab entry and output the variable name.  */
      PUT_SDB_DEF (IDENTIFIER_POINTER (DECL_NAME (decl)));

      if (GET_CODE (value) == MEM
	  && GET_CODE (XEXP (value, 0)) == SYMBOL_REF)
	{
	  if (TREE_PUBLIC (decl))
	    {
	      PUT_SDB_VAL (XEXP (value, 0));
              PUT_SDB_SCL (C_EXT);
	    }
	  else
	    {
	      PUT_SDB_VAL (XEXP (value, 0));
              PUT_SDB_SCL (C_STAT);
	    }
	}
      else if (GET_CODE (value) == REG)
	{
	  PUT_SDB_INT_VAL (DBX_REGISTER_NUMBER (REGNO (value)));
	  PUT_SDB_SCL (C_REG);
	}
      else if (GET_CODE (value) == SUBREG)
	{
	  int offset = 0;
	  while (GET_CODE (value) == SUBREG)
	    {
	      offset += SUBREG_WORD (value);
	      value = SUBREG_REG (value);
	    }
	  PUT_SDB_INT_VAL (DBX_REGISTER_NUMBER (REGNO (value) + offset));
	  PUT_SDB_SCL (C_REG);
	}
      else if (GET_CODE (value) == MEM
	       && (GET_CODE (XEXP (value, 0)) == MEM
		   || (GET_CODE (XEXP (value, 0)) == REG
		       && REGNO (XEXP (value, 0)) != FRAME_POINTER_REGNUM)))
	/* If the value is indirect by memory or by a register
	   that isn't the frame pointer
	   then it means the object is variable-sized and address through
	   that register or stack slot.  DBX has no way to represent this
	   so all we can do is output the variable as a pointer.  */
	{
	  if (GET_CODE (XEXP (value, 0)) == REG)
	    {
	      PUT_SDB_INT_VAL (DBX_REGISTER_NUMBER (REGNO (XEXP (value, 0))));
	      PUT_SDB_SCL (C_REG);
	    }
	  else
	    {
	      /* DECL_RTL looks like (MEM (MEM (PLUS (REG...)
		 (CONST_INT...)))).
		 We want the value of that CONST_INT.  */
	      /* Encore compiler hates a newline in a macro arg, it seems.  */
	      PUT_SDB_INT_VAL (INTVAL (XEXP (XEXP (XEXP (value, 0), 0), 1)));
	      PUT_SDB_SCL (C_AUTO);
	    }

	  type = build_pointer_type (TREE_TYPE (decl));
	}
      else if (GET_CODE (value) == MEM
	       && GET_CODE (XEXP (value, 0)) == PLUS
	       && GET_CODE (XEXP (XEXP (value, 0), 0)) == REG
	       && GET_CODE (XEXP (XEXP (value, 0), 1)) == CONST_INT)
	{
	  /* DECL_RTL looks like (MEM (PLUS (REG...) (CONST_INT...))).
	     We want the value of that CONST_INT.  */
	  PUT_SDB_INT_VAL (INTVAL (XEXP (XEXP (value, 0), 1)));
	  PUT_SDB_SCL (C_AUTO);
	}
      else
	{
	  /* It is something we don't know how to represent for SDB.  */
	}
      break;
    }
  PUT_SDB_TYPE (plain_type (type));
  PUT_SDB_ENDEF;
}

/* Given a list of TREE_LIST nodes that point at types,
   output those types for SDB.
   We must check to include those that have been mentioned already with
   only a cross-reference.  */

void
sdbout_tags (tags)
     tree tags;
{
  register tree link;

  for (link = tags; link; link = TREE_CHAIN (link))
    {
      register tree type = TREE_VALUE (link);

      if (TREE_PURPOSE (link) != 0
	  && TYPE_SIZE (type) != 0)
	sdbout_one_type (type);
    }
}

/* Given a chain of ..._TYPE nodes, all of which have names,
   output definitions of those names, as typedefs.  */

void
sdbout_types (types)
     register tree types;
{
  register tree link;

  for (link = types; link; link = TREE_CHAIN (link))
    sdbout_one_type (link);
}

static void
sdbout_type (type)
     tree type;
{
  register tree tem;
  if (type == error_mark_node)
    type = integer_type_node;
  PUT_SDB_TYPE (plain_type (type));
}

/* Output types of the fields of type TYPE, if they are structs.
   Formerly did not chase through pointer types, since that could be circular.
   They must come before TYPE, since forward refs are not allowed.
   Now james@bigtex.cactus.org says to try them.  */

static void
sdbout_field_types (type)
     tree type;
{
  tree tail;
  for (tail = TYPE_FIELDS (type); tail; tail = TREE_CHAIN (tail))
    {
#ifdef MAYBE
      if (TREE_CODE (TREE_TYPE (tail)) == POINTER_TYPE)
	sdbout_one_type (TREE_TYPE (TREE_TYPE (tail)));
      else
#endif
	sdbout_one_type (TREE_TYPE (tail));
    }
}

/* Use this to put out the top level defined record and union types
   for later reference.  If this is a struct with a name, then put that
   name out.  Other unnamed structs will have .xxfake labels generated so
   that they may be referred to later.
   The label will be stored in the KNOWN_TYPE_TAG slot of a type.
   It may NOT be called recursively.  */

static void
sdbout_one_type (type)
     tree type;
{
  text_section ();

  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      type = TYPE_MAIN_VARIANT (type);
      /* Don't output a type twice.  */
      if (TREE_ASM_WRITTEN (type))
	return;

      /* Output nothing if type is not yet defined.  */
      if (TYPE_SIZE (type) == 0)
	return;

      TREE_ASM_WRITTEN (type) = 1;
#ifndef MAYBE
      /* Before really doing anything, output types we want to refer to.  */
      if (TREE_CODE (type) != ENUMERAL_TYPE)
	sdbout_field_types (type);
#endif

      sdbout_record_type_name (type);

      /* Output a structure type.  */
      {
	int size = int_size_in_bytes (type);
	int member_scl;
	tree tem;

	PUT_SDB_DEF (KNOWN_TYPE_TAG (type));

	switch (TREE_CODE (type))
	  {
	  case UNION_TYPE:
	    PUT_SDB_SCL (C_UNTAG);
	    PUT_SDB_TYPE (T_UNION);
	    member_scl = C_MOU;
	    break;

	  case RECORD_TYPE:
	    PUT_SDB_SCL (C_STRTAG);
	    PUT_SDB_TYPE (T_STRUCT);
	    member_scl = C_MOS;
	    break;

	  case ENUMERAL_TYPE:
	    PUT_SDB_SCL (C_ENTAG);
	    PUT_SDB_TYPE (T_ENUM);
	    member_scl = C_MOE;
	    break;
	  }

	PUT_SDB_SIZE (size);
	PUT_SDB_ENDEF;

	/* output the individual fields */

	if (TREE_CODE (type) == ENUMERAL_TYPE)
	  for (tem = TYPE_FIELDS (type); tem; tem = TREE_CHAIN (tem))
	    {
	      PUT_SDB_DEF (IDENTIFIER_POINTER (TREE_PURPOSE (tem)));
	      PUT_SDB_INT_VAL (TREE_INT_CST_LOW (TREE_VALUE (tem)));
	      PUT_SDB_SCL (C_MOE);
	      PUT_SDB_TYPE (T_MOE);
	      PUT_SDB_ENDEF;
	    }
      
	else			/* record or union type */
	  for (tem = TYPE_FIELDS (type); tem; tem = TREE_CHAIN (tem))
	    /* Output the name, type, position (in bits), size (in bits)
	       of each field.  */
	    /* Omit here the nameless fields that are used to skip bits.  */
	    if (DECL_NAME (tem) != 0)
	      {
		CONTIN;
		PUT_SDB_DEF (IDENTIFIER_POINTER (DECL_NAME (tem)));
		if (TREE_PACKED (tem))
		  {
		    PUT_SDB_INT_VAL (DECL_OFFSET (tem));
		    PUT_SDB_SCL (C_FIELD);
		    sdbout_type (TREE_TYPE (tem));
		    PUT_SDB_SIZE (TREE_INT_CST_LOW (DECL_SIZE (tem))
				  * DECL_SIZE_UNIT (tem));
		  }
		else
		  {
		    PUT_SDB_INT_VAL (DECL_OFFSET (tem) / BITS_PER_UNIT);
		    PUT_SDB_SCL (member_scl);
		    sdbout_type (TREE_TYPE (tem));
		  }
		PUT_SDB_ENDEF;
	      }
	/* output end of a structure,union, or enumeral definition */
   
	PUT_SDB_PLAIN_DEF ("eos");
	PUT_SDB_INT_VAL (size);
	PUT_SDB_SCL (C_EOS);
	PUT_SDB_TAG (KNOWN_TYPE_TAG (type));
	PUT_SDB_SIZE (size);
	PUT_SDB_ENDEF;
	break;
      }
    }
}

/* Output definitions of all parameters, referring when possible to the
   place where the parameters were passed rather than the copies used
   within the function.     This is done as part of starting the function.
   PARMS is a chain of PARM_DECL nodes.  */

static void
sdbout_parms (parms1)
     tree parms1;
{
  tree type;
  tree parms;

  for (parms = parms1; parms; parms = TREE_CHAIN (parms))
    {
      int current_sym_value = DECL_OFFSET (parms) / BITS_PER_UNIT;

      if (DECL_NAME (parms))
	PUT_SDB_DEF (IDENTIFIER_POINTER (DECL_NAME (parms)));
      else
	PUT_SDB_DEF (gen_fake_label ());

      if (GET_CODE (DECL_RTL (parms)) == REG
	  && REGNO (DECL_RTL (parms)) >= 0
	  && REGNO (DECL_RTL (parms)) < FIRST_PSEUDO_REGISTER)
	type = DECL_ARG_TYPE (parms);
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
	    current_sym_value +=
	      (GET_MODE_SIZE (TYPE_MODE (DECL_ARG_TYPE (parms)))
	       - GET_MODE_SIZE (GET_MODE (DECL_RTL (parms))));
#endif
	  if (GET_CODE (DECL_RTL (parms)) == MEM
	      && GET_CODE (XEXP (DECL_RTL (parms), 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (DECL_RTL (parms), 0), 1)) == CONST_INT
	      && (INTVAL (XEXP (XEXP (DECL_RTL (parms), 0), 1))
		  == current_sym_value))
	    type = TREE_TYPE (parms);
	  else
	    {
	      current_sym_value = DECL_OFFSET (parms) / BITS_PER_UNIT;
	      type = DECL_ARG_TYPE (parms);
	    }
	}
     
      PUT_SDB_INT_VAL (current_sym_value);
      PUT_SDB_SCL (C_ARG);
      PUT_SDB_TYPE (plain_type (type));
      PUT_SDB_ENDEF;
    }
}

/* Output definitions, referring to registers,
   of all the parms in PARMS which are stored in registers during the function.
   PARMS is a chain of PARM_DECL nodes.
   This is done as part of starting the function.  */

static void
sdbout_reg_parms (parms)
     tree parms;
{
  while (parms)
    {
      if (GET_CODE (DECL_RTL (parms)) == REG
	  && REGNO (DECL_RTL (parms)) >= 0
	  && REGNO (DECL_RTL (parms)) < FIRST_PSEUDO_REGISTER)
	{
	  PUT_SDB_DEF (IDENTIFIER_POINTER (DECL_NAME (parms)));
	  PUT_SDB_INT_VAL (DBX_REGISTER_NUMBER (REGNO (DECL_RTL (parms))));
  	  PUT_SDB_SCL (C_REG);
	  PUT_SDB_TYPE (plain_type (TREE_TYPE (parms), 0));
	  PUT_SDB_ENDEF;
	}
      else if (GET_CODE (DECL_RTL (parms)) == MEM
	       && GET_CODE (XEXP (DECL_RTL (parms), 0)) == PLUS
	       && GET_CODE (XEXP (XEXP (DECL_RTL (parms), 0), 1)) == CONST_INT)
	{
	  int offset = DECL_OFFSET (parms) / BITS_PER_UNIT;
	  /* A parm declared char is really passed as an int,
	     so it occupies the least significant bytes.
	     On a big-endian machine those are not the low-numbered ones.  */
#ifdef BYTES_BIG_ENDIAN
	  if (TREE_TYPE (parms) != DECL_ARG_TYPE (parms))
	    offset += (GET_MODE_SIZE (TYPE_MODE (DECL_ARG_TYPE (parms)))
		       - GET_MODE_SIZE (GET_MODE (DECL_RTL (parms))));
#endif
	  if (INTVAL (XEXP (XEXP (DECL_RTL (parms), 0), 1)) != offset)
	    {
	      PUT_SDB_DEF (IDENTIFIER_POINTER (DECL_NAME (parms)));	      
	      PUT_SDB_INT_VAL (INTVAL (XEXP (XEXP (DECL_RTL (parms), 0), 1)));
	      PUT_SDB_SCL (C_AUTO);
	      PUT_SDB_TYPE (plain_type (TREE_TYPE (parms)));
	      PUT_SDB_ENDEF;
	    }
	}
      parms = TREE_CHAIN (parms);
    }
}

/* Describe the beginning of an internal block within a function.
   Also output descriptions of variables defined in this block.

   N is the number of the block, by order of beginning, counting from 1,
   and not counting the outermost (function top-level) block.
   The blocks match the LET_STMTS in DECL_INITIAL (current_function_decl),
   if the count starts at 0 for the outermost one.  */

void
sdbout_begin_block (file, line, n)
     FILE *file;
     int line;
     int n;
{
  tree decl = current_function_decl;
  MAKE_LINE_SAFE (line);
  PUT_SDB_BLOCK_START (line - sdb_begin_function_line);
  if (n == 1)
    {
      /* Include the outermost LET_STMT's variables in block 1.  */
      next_block_number = 0;
      do_block = 0;
      sdbout_block (DECL_INITIAL (decl));
    }
  next_block_number = 0;
  do_block = n;
  sdbout_block (DECL_INITIAL (decl));
}

/* Describe the end line-number of an internal block within a function.  */
	 
void
sdbout_end_block (file, line)
     FILE *file;
     int line;
{
  MAKE_LINE_SAFE (line);
  PUT_SDB_BLOCK_END (line - sdb_begin_function_line);
}

/* Output sdb info for the current function name.
   Called from assemble_function.  */

void
sdbout_mark_begin_function ()
{
  sdbout_symbol (current_function_decl, 0);
}

/* Called at beginning of function body (after prologue).
   Record the function's starting line number, so we can output
   relative line numbers for the other lines.
   Describe beginning of outermost block.
   Also describe the parameter list.  */

void
sdbout_begin_function (line)
     int line;
{
  sdb_begin_function_line = line - 1;
  PUT_SDB_FUNCTION_START (line);
  sdbout_parms (DECL_ARGUMENTS (current_function_decl));
  sdbout_reg_parms (DECL_ARGUMENTS (current_function_decl));
}

/* Called at end of function (before epilogue).
   Describe end of outermost block.  */

void
sdbout_end_function (line)
     int line;
{
  MAKE_LINE_SAFE (line);
  PUT_SDB_FUNCTION_END (line - sdb_begin_function_line);

  /* Indicate we are between functions, for line-number output.  */
  sdb_begin_function_line = -1;
}

/* Output sdb info for the absolute end of a function.
   Called after the epilogue is output.  */

void
sdbout_end_epilogue ()
{
  char *name = IDENTIFIER_POINTER (DECL_NAME (current_function_decl));
  PUT_SDB_EPILOGUE_END (name);
}

#endif /* SDB_DEBUGGING_INFO */
