/* Front-end tree definitions for GNU compiler.
   Copyright (C) 1989 Free Software Foundation, Inc.

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


/* codes of tree nodes */

#define DEFTREECODE(SYM, STRING, TYPE, NARGS)   SYM,

enum tree_code {
#include "tree.def"

  LAST_AND_UNUSED_TREE_CODE	/* A convienent way to get a value for
				   NUM_TREE_CODE.  */
};

#undef DEFTREECODE

/* Number of tree codes.  */
#define NUM_TREE_CODES ((int)LAST_AND_UNUSED_TREE_CODE)

/* Indexed by enum tree_code, contains a character which is
   `e' for an expression, `r' for a reference, `c' for a constant,
   `d' for a decl, `t' for a type, `s' for a statement,
   and `x' for anything else (TREE_LIST, IDENTIFIER, etc).  */

extern char *tree_code_type[];

/* Number of argument-words in each kind of tree-node.  */

extern int tree_code_length[];

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

/* Codes that identify the various built in functions
   so that expand_call can identify them quickly.  */

enum built_in_function
{
  NOT_BUILT_IN,
  BUILT_IN_ALLOCA,
  BUILT_IN_ABS,
  BUILT_IN_FABS,
  BUILT_IN_LABS,
  BUILT_IN_FFS,
  BUILT_IN_DIV,
  BUILT_IN_LDIV,
  BUILT_IN_FFLOOR,
  BUILT_IN_FCEIL,
  BUILT_IN_FMOD,
  BUILT_IN_FREM,
  BUILT_IN_MEMCPY,
  BUILT_IN_MEMCMP,
  BUILT_IN_MEMSET,
  BUILT_IN_FSQRT,
  BUILT_IN_GETEXP,
  BUILT_IN_GETMAN,
  BUILT_IN_SAVEREGS,
  BUILT_IN_CLASSIFY_TYPE,
  BUILT_IN_NEXT_ARG,

  /* C++ extensions */
  BUILT_IN_NEW,
  BUILT_IN_VEC_NEW,
  BUILT_IN_DELETE,
  BUILT_IN_VEC_DELETE
};

/* The definition of tree nodes fills the next several pages.  */

/* A tree node can represent a data type, a variable, an expression
   or a statement.  Each node has a TREE_CODE which says what kind of
   thing it represents.  Some common codes are:
   INTEGER_TYPE -- represents a type of integers.
   ARRAY_TYPE -- represents a type of pointer.
   VAR_DECL -- represents a declared variable.
   INTEGER_CST -- represents a constant integer value.
   PLUS_EXPR -- represents a sum (an expression).

   As for the contents of a tree node: there are some fields
   that all nodes share.  Each TREE_CODE has various special-purpose
   fields as well.  The fields of a node are never accessed directly,
   always through accessor macros.  */

/* This type is used everywhere to refer to a tree node.  */

typedef union tree_node *tree;

#define NULL_TREE (tree) NULL

/* Every kind of tree node starts with this structure,
   so all nodes have these fields.

   See the accessor macros, defined below, for documentation of the fields.  */

struct tree_common
{
  int uid;
  union tree_node *chain;
  union tree_node *type;
  unsigned char code : 8;

  unsigned external_attr : 1;
  unsigned public_attr : 1;
  unsigned static_attr : 1;
  unsigned volatile_attr : 1;
  unsigned packed_attr : 1;
  unsigned readonly_attr : 1;
  unsigned literal_attr : 1;
  unsigned nonlocal_attr : 1;
  unsigned permanent_attr : 1;
  unsigned addressable_attr : 1;
  unsigned regdecl_attr : 1;
  unsigned this_vol_attr : 1;
  unsigned unsigned_attr : 1;
  unsigned asm_written_attr: 1;
  unsigned inline_attr : 1;
  unsigned used_attr : 1;
  unsigned lang_flag_1 : 1;
  unsigned lang_flag_2 : 1;
  unsigned lang_flag_3 : 1;
  unsigned lang_flag_4 : 1;
  /* There is room for four more attributes.  */
};

/* Define accessors for the fields that all tree nodes have
   (though some fields are not used for all kinds of nodes).  */

/* The unique id of a tree node distinguishes it from all other nodes
   in the same compiler run.  */
#define TREE_UID(NODE) ((NODE)->common.uid)

/* The tree-code says what kind of node it is.
   Codes are defined in tree.def.  */
#define TREE_CODE(NODE) ((enum tree_code) (NODE)->common.code)
#define TREE_SET_CODE(NODE, VALUE) ((NODE)->common.code = (int) (VALUE))

/* In all nodes that are expressions, this is the data type of the expression.
   In POINTER_TYPE nodes, this is the type that the pointer points to.
   In ARRAY_TYPE nodes, this is the type of the elements.  */
#define TREE_TYPE(NODE) ((NODE)->common.type)

/* Nodes are chained together for many purposes.
   Types are chained together to record them for being output to the debugger
   (see the function `chain_type').
   Decls in the same scope are chained together to record the contents
   of the scope.
   Statement nodes for successive statements used to be chained together.
   Often lists of things are represented by TREE_LIST nodes that
   are chained together.  */

#define TREE_CHAIN(NODE) ((NODE)->common.chain)

/* Define many boolean fields that all tree nodes have.  */

/* In a VAR_DECL or FUNCTION_DECL,
   nonzero means external reference:
   do not allocate storage, and refer to a definition elsewhere.  */
#define TREE_EXTERNAL(NODE) ((NODE)->common.external_attr)

/* In a VAR_DECL, nonzero means allocate static storage.
   In a FUNCTION_DECL, currently nonzero if function has been defined.  */
#define TREE_STATIC(NODE) ((NODE)->common.static_attr)

/* In a VAR_DECL or FUNCTION_DECL,
   nonzero means name is to be accessible from outside this module.  */
#define TREE_PUBLIC(NODE) ((NODE)->common.public_attr)

/* In VAR_DECL nodes, nonzero means address of this is needed.
   So it cannot be in a register.
   In a FUNCTION_DECL, nonzero means its address is needed.
   So it must be compiled even if it is an inline function.
   In CONSTRUCTOR nodes, it means the elements are all constants suitable
   for output as assembly-language initializers.
   In LABEL_DECL nodes, it means a goto for this label has been seen 
   from a place outside all binding contours that restore stack levels,
   or that an error message about jumping into such a binding contour
   has been printed for this label.
   In ..._TYPE nodes, it means that objects of this type must
   be fully addressable.  This means that pieces of this
   object cannot go into register parameters, for example.  */
#define TREE_ADDRESSABLE(NODE) ((NODE)->common.addressable_attr)

/* In VAR_DECL nodes, nonzero means declared `register'.  */
#define TREE_REGDECL(NODE) ((NODE)->common.regdecl_attr)

/* In any expression, nonzero means it has side effects or reevaluation
   of the whole expression could produce a different value.
   This is set if any subexpression is a function call, a side effect
   or a reference to a volatile variable.
   In a ..._DECL, this is set only if the declaration said `volatile'.
   In a ..._TYPE, nonzero means the type is volatile-qualified.  */
#define TREE_VOLATILE(NODE) ((NODE)->common.volatile_attr)

/* Nonzero means this expression is volatile in the C sense:
   its address should be of type `volatile WHATEVER *'.
   If this bit is set, so is `volatile_attr'.  */
#define TREE_THIS_VOLATILE(NODE) ((NODE)->common.this_vol_attr)

/* In a VAR_DECL, PARM_DECL or FIELD_DECL, or any kind of ..._REF node,
   nonzero means it may not be the lhs of an assignment.
   In a ..._TYPE node, means this type is const-qualified.  */
#define TREE_READONLY(NODE) ((NODE)->common.readonly_attr)

/* Nonzero in a FIELD_DECL means it is a bit-field; it may occupy
   less than a storage unit, and its address may not be taken, etc.
   This controls layout of the containing record.
   In a LABEL_DECL, nonzero means label was defined inside a binding
   contour that restored a stack level and which is now exited.  */
#define TREE_PACKED(NODE) ((NODE)->common.packed_attr)

/* Value of expression is constant.
   Always appears in all ..._CST nodes.
   May also appear in an arithmetic expression, an ADDR_EXPR or a CONSTRUCTOR
   if the value is constant.  */
#define TREE_LITERAL(NODE) ((NODE)->common.literal_attr)

/* Nonzero in a ..._DECL means this variable is ref'd from a nested function.
   Cannot happen in C because it does not allow nested functions, as of now.
   For VAR_DECL nodes, PARM_DECL nodes, and
   maybe FUNCTION_DECL or LABEL_DECL nodes.

   Also set in some languages for variables, etc., outside the normal
   lexical scope, such as class instance variables.  */
#define TREE_NONLOCAL(NODE) ((NODE)->common.nonlocal_attr)

/* Nonzero means permanent node;
   node will continue to exist for the entire compiler run.
   Otherwise it will be recycled at the end of the function.  */
#define TREE_PERMANENT(NODE) ((NODE)->common.permanent_attr)

/* In INTEGER_TYPE or ENUMERAL_TYPE nodes, means an unsigned type.
   In FIELD_DECL nodes, means an unsigned bit field.  */
#define TREE_UNSIGNED(NODE) ((NODE)->common.unsigned_attr)

/* Nonzero in a VAR_DECL means assembler code has been written.
   Nonzero in a FUNCTION_DECL means that the function has been compiled.
   This is interesting in an inline function, since it might not need
   to be compiled separately.  */
#define TREE_ASM_WRITTEN(NODE) ((NODE)->common.asm_written_attr)

/* Nonzero in a FUNCTION_DECL means this function can be substituted
   where it is called.  */
#define TREE_INLINE(NODE) ((NODE)->common.inline_attr)

/* Nonzero in a _DECL if the name is used in its scope.  */
#define TREE_USED(NODE) ((NODE)->common.used_attr)

#define TREE_LANG_FLAG_1(NODE) ((NODE)->common.lang_flag_1)
#define TREE_LANG_FLAG_2(NODE) ((NODE)->common.lang_flag_2)
#define TREE_LANG_FLAG_3(NODE) ((NODE)->common.lang_flag_3)
#define TREE_LANG_FLAG_4(NODE) ((NODE)->common.lang_flag_4)

/* Define additional fields and accessors for nodes representing constants.  */

/* In an INTEGER_CST node.  These two together make a 64 bit integer.
   If the data type is signed, the value is sign-extended to 64 bits
   even though not all of them may really be in use.
   In an unsigned constant shorter than 64 bits, the extra bits are 0.  */
#define TREE_INT_CST_LOW(NODE) ((NODE)->int_cst.int_cst_low)
#define TREE_INT_CST_HIGH(NODE) ((NODE)->int_cst.int_cst_high)

#define INT_CST_LT(A, B)  \
(TREE_INT_CST_HIGH (A) < TREE_INT_CST_HIGH (B)			\
 || (TREE_INT_CST_HIGH (A) == TREE_INT_CST_HIGH (B)		\
     && ((unsigned) TREE_INT_CST_LOW (A) < (unsigned) TREE_INT_CST_LOW (B))))

#define INT_CST_LT_UNSIGNED(A, B)  \
((unsigned) TREE_INT_CST_HIGH (A) < (unsigned) TREE_INT_CST_HIGH (B)	  \
 || ((unsigned) TREE_INT_CST_HIGH (A) == (unsigned) TREE_INT_CST_HIGH (B) \
     && ((unsigned) TREE_INT_CST_LOW (A) < (unsigned) TREE_INT_CST_LOW (B))))

struct tree_int_cst
{
  char common[sizeof (struct tree_common)];
  long int_cst_low;
  long int_cst_high;
};

/* In REAL_CST, STRING_CST, COMPLEX_CST nodes, and CONSTRUCTOR nodes,
   and generally in all kinds of constants that could
   be given labels (rather than being immediate).  */

#define TREE_CST_RTL(NODE) ((NODE)->real_cst.rtl)

/* In a REAL_CST node.  */
/* We can represent a real value as either a `double' or a string.
   Strings don't allow for any optimization, but they do allow
   for cross-compilation.  */

#define TREE_REAL_CST(NODE) ((NODE)->real_cst.real_cst)

#include "real.h"

struct tree_real_cst
{
  char common[sizeof (struct tree_common)];
  struct rtx_def *rtl;	/* acts as link to register transfer language
				   (rtl) info */
  REAL_VALUE_TYPE real_cst;
};

/* In a STRING_CST */
#define TREE_STRING_LENGTH(NODE) ((NODE)->string.length)
#define TREE_STRING_POINTER(NODE) ((NODE)->string.pointer)

struct tree_string
{
  char common[sizeof (struct tree_common)];
  struct rtx_def *rtl;	/* acts as link to register transfer language
				   (rtl) info */
  int length;
  char *pointer;
};

/* In a COMPLEX_CST node.  */
#define TREE_REALPART(NODE) ((NODE)->complex.real)
#define TREE_IMAGPART(NODE) ((NODE)->complex.imag)

struct tree_complex
{
  char common[sizeof (struct tree_common)];
  struct rtx_def *rtl;	/* acts as link to register transfer language
				   (rtl) info */
  union tree_node *real;
  union tree_node *imag;
};

/* Define fields and accessors for some special-purpose tree nodes.  */

#define IDENTIFIER_LENGTH(NODE) ((NODE)->identifier.length)
#define IDENTIFIER_POINTER(NODE) ((NODE)->identifier.pointer)

struct tree_identifier
{
  char common[sizeof (struct tree_common)];
  int length;
  char *pointer;
};

/* In a TREE_LIST node.  */
#define TREE_PURPOSE(NODE) ((NODE)->list.purpose)
#define TREE_VALUE(NODE) ((NODE)->list.value)

struct tree_list
{
  char common[sizeof (struct tree_common)];
  union tree_node *purpose;
  union tree_node *value;
};

/* Define fields and accessors for some nodes that represent expressions.  */

/* In a SAVE_EXPR node.  */
#define SAVE_EXPR_RTL(NODE) (*(struct rtx_def **) &(NODE)->exp.operands[1])

/* In a RTL_EXPR node.  */
#define RTL_EXPR_SEQUENCE(NODE) (*(struct rtx_def **) &(NODE)->exp.operands[0])
#define RTL_EXPR_RTL(NODE) (*(struct rtx_def **) &(NODE)->exp.operands[1])

/* In a CALL_EXPR node.  */
#define CALL_EXPR_RTL(NODE) (*(struct rtx_def **) &(NODE)->exp.operands[2])

/* In a CONSTRUCTOR node.  */
#define CONSTRUCTOR_ELTS(NODE) TREE_OPERAND (NODE, 1)

/* In expression and reference nodes.  */
#define TREE_OPERAND(NODE, I) ((NODE)->exp.operands[I])
#define TREE_COMPLEXITY(NODE, I) ((NODE)->exp.complexity)

struct tree_exp
{
  char common[sizeof (struct tree_common)];
  int complexity;
  union tree_node *operands[1];
};

/* Define fields and accessors for nodes representing data types.  */

/* See tree.def for documentation of the use of these fields.
   Look at the documentation of the various ..._TYPE tree codes.  */

#define TYPE_SIZE(NODE) ((NODE)->type.size)
#define TYPE_SIZE_UNIT(NODE) ((NODE)->type.size_unit)
#define TYPE_MODE(NODE) ((NODE)->type.mode)
#define TYPE_ALIGN(NODE) ((NODE)->type.align)
#define TYPE_VALUES(NODE) ((NODE)->type.values)
#define TYPE_DOMAIN(NODE) ((NODE)->type.values)
#define TYPE_FIELDS(NODE) ((NODE)->type.values)
#define TYPE_ARG_TYPES(NODE) ((NODE)->type.values)
#define TYPE_METHOD_BASETYPE(NODE) ((NODE)->type.max)
#define TYPE_OFFSET_BASETYPE(NODE) ((NODE)->type.max)
#define TYPE_SEP(NODE) ((NODE)->type.sep)
#define TYPE_SEP_UNIT(NODE) ((NODE)->type.sep_unit)
#define TYPE_POINTER_TO(NODE) ((NODE)->type.pointer_to)
#define TYPE_REFERENCE_TO(NODE) ((NODE)->type.reference_to)
#define TYPE_MIN_VALUE(NODE) ((NODE)->type.sep)
#define TYPE_MAX_VALUE(NODE) ((NODE)->type.max)
#define TYPE_PRECISION(NODE) ((NODE)->type.sep_unit)
#define TYPE_PARSE_INFO(NODE) ((NODE)->type.parse_info)
#define TYPE_SYMTAB_ADDRESS(NODE) ((NODE)->type.symtab_address)
#define TYPE_NAME(NODE) ((NODE)->type.name)
#define TYPE_NEXT_VARIANT(NODE) ((NODE)->type.next_variant)
#define TYPE_MAIN_VARIANT(NODE) ((NODE)->type.main_variant)
#define TYPE_BASETYPES(NODE) ((NODE)->type.basetypes)
#define TYPE_NONCOPIED_PARTS(NODE) ((NODE)->type.noncopied_parts)
#define TYPE_LANG_SPECIFIC(NODE) ((NODE)->type.lang_specific)

struct tree_type
{
  char common[sizeof (struct tree_common)];
  union tree_node *values;
  union tree_node *sep;
  union tree_node *size;

  enum machine_mode mode : 8;
  unsigned char size_unit;
  unsigned char align;
  unsigned char sep_unit;

  union tree_node *pointer_to;
  union tree_node *reference_to;
  int parse_info;
  int symtab_address;
  union tree_node *name;
  union tree_node *max;
  union tree_node *next_variant;
  union tree_node *main_variant;
  union tree_node *basetypes;
  union tree_node *noncopied_parts;
  /* Points to a structure whose details depend on the language in use.  */
  struct lang_type *lang_specific;
};

/* Define fields and accessors for nodes representing declared names.  */

#define DECL_VOFFSET(NODE) ((NODE)->decl.voffset)  /* In FIELD_DECLs and maybe PARM_DECLs.  */
#define DECL_RESULT_TYPE(NODE) ((NODE)->decl.voffset) /* In FUNCTION_DECLs.  */
#define DECL_VOFFSET_UNIT(NODE) ((NODE)->decl.voffset_unit)
#define DECL_OFFSET(NODE) ((NODE)->decl.offset)
#define DECL_FUNCTION_CODE(NODE) ((enum built_in_function) (NODE)->decl.offset)
#define DECL_SET_FUNCTION_CODE(NODE,VAL) ((NODE)->decl.offset = (int) (VAL))
#define DECL_NAME(NODE) ((NODE)->decl.name)
#define DECL_PRINT_NAME(NODE) ((NODE)->decl.print_name)
#define DECL_ASSEMBLER_NAME(NODE) ((NODE)->decl.assembler_name)
#define DECL_CONTEXT(NODE) ((NODE)->decl.context)
#define DECL_FIELD_CONTEXT(NODE) ((NODE)->decl.context)
#define DECL_ARGUMENTS(NODE) ((NODE)->decl.arguments)  /* In FUNCTION_DECL.  */
#define DECL_ARG_TYPE(NODE) ((NODE)->decl.arguments)   /* In PARM_DECL.  */
#define DECL_RESULT(NODE) ((NODE)->decl.result)
#define DECL_INITIAL(NODE) ((NODE)->decl.initial)
#define DECL_SOURCE_FILE(NODE) ((NODE)->decl.filename)
#define DECL_SOURCE_LINE(NODE) ((NODE)->decl.linenum)
#define DECL_SIZE(NODE) ((NODE)->decl.size)
#define DECL_SIZE_UNIT(NODE) ((NODE)->decl.size_unit)
#define DECL_ALIGN(NODE) ((NODE)->decl.align)
#define DECL_MODE(NODE) ((NODE)->decl.mode)
#define DECL_RTL(NODE) ((NODE)->decl.rtl)
#define DECL_BLOCK_SYMTAB_ADDRESS(NODE) ((NODE)->decl.block_symtab_address)
#define DECL_SYMTAB_INDEX(NODE) ((NODE)->decl.block_symtab_address)
#define DECL_SAVED_INSNS(NODE) ((NODE)->decl.saved_insns)
#define DECL_FRAME_SIZE(NODE) ((NODE)->decl.frame_size)
#define DECL_LANG_SPECIFIC(NODE) ((NODE)->decl.lang_specific)

struct tree_decl
{
  char common[sizeof (struct tree_common)];
  char *filename;
  int linenum;
  union tree_node *size;
  enum machine_mode mode : 8;
  unsigned char size_unit;
  unsigned char align;
  unsigned char voffset_unit;
  union tree_node *name;
  union tree_node *context;
  int offset;
  union tree_node *voffset;
  union tree_node *arguments;
  union tree_node *result;
  union tree_node *initial;
  char *print_name;
  char *assembler_name;
  struct rtx_def *rtl;	/* acts as link to register transfer language
				   (rtl) info */
  int frame_size;		/* For FUNCTION_DECLs: size of stack frame */
  struct rtx_def *saved_insns;	/* For FUNCTION_DECLs: points to insn that
				   constitutes its definition on the
				   permanent obstack.  */
  int block_symtab_address;
  /* Points to a structure whose details depend on the language in use.  */
  struct lang_decl *lang_specific;
};

/* Define fields and accessors for nodes representing statements.
   These are now obsolete for C, except for LET_STMT, which is used
   to record the structure of binding contours (and the names declared
   in each contour) for the sake of outputting debugging info.
   Perhaps they will be used once again for other languages.  */

/* For LABEL_STMT, GOTO_STMT, RETURN_STMT, LOOP_STMT,
   COMPOUND_STMT, ASM_STMT.  */
#define STMT_SOURCE_LINE(NODE) ((NODE)->stmt.linenum)
#define STMT_SOURCE_FILE(NODE) ((NODE)->stmt.filename)
#define STMT_BODY(NODE) ((NODE)->stmt.body)

struct tree_stmt
{
  char common[sizeof (struct tree_common)];
  char *filename;
  int linenum;
  union tree_node *body;
};

/* For IF_STMT.  */

/* #define STMT_SOURCE_LINE(NODE) */
/* #define STMT_SOURCE_FILE(NODE) */
#define STMT_COND(NODE) ((NODE)->if_stmt.cond)
#define STMT_THEN(NODE) ((NODE)->if_stmt.thenpart)
#define STMT_ELSE(NODE) ((NODE)->if_stmt.elsepart)

struct tree_if_stmt
{
  char common[sizeof (struct tree_common)];
  char *filename;
  int linenum;
  union tree_node *cond, *thenpart, *elsepart;
};

/* For LET_STMT and WITH_STMT.  */

/* #define STMT_SOURCE_LINE(NODE) */
/* #define STMT_SOURCE_FILE(NODE) */
/* #define STMT_BODY(NODE) */
#define STMT_VARS(NODE) ((NODE)->bind_stmt.vars)
#define STMT_SUPERCONTEXT(NODE) ((NODE)->bind_stmt.supercontext)
#define STMT_BIND_SIZE(NODE) ((NODE)->bind_stmt.bind_size)
#define STMT_TYPE_TAGS(NODE) ((NODE)->bind_stmt.type_tags)
#define STMT_SUBBLOCKS(NODE) ((NODE)->bind_stmt.subblocks)

struct tree_bind_stmt
{
  char common[sizeof (struct tree_common)];
  char *filename;
  int linenum;
  union tree_node *body, *vars, *supercontext, *bind_size, *type_tags;
  union tree_node *subblocks;
};

/* For CASE_STMT.  */

#define STMT_CASE_INDEX(NODE) ((NODE)->case_stmt.index)
#define STMT_CASE_LIST(NODE) ((NODE)->case_stmt.case_list)

struct tree_case_stmt
{
  char common[sizeof (struct tree_common)];
  char *filename;
  int linenum;
  union tree_node *index, *case_list;
};

/* Define the overall contents of a tree node.
   It may be any of the structures declared above
   for various types of node.  */

union tree_node
{
  struct tree_common common;
  struct tree_int_cst int_cst;
  struct tree_real_cst real_cst;
  struct tree_string string;
  struct tree_complex complex;
  struct tree_identifier identifier;
  struct tree_decl decl;
  struct tree_type type;
  struct tree_list list;
  struct tree_exp exp;
  struct tree_stmt stmt;
  struct tree_if_stmt if_stmt;
  struct tree_bind_stmt bind_stmt;
  struct tree_case_stmt case_stmt;
};

extern char *oballoc ();
extern char *permalloc ();

/* Lowest level primitive for allocating a node.
   The TREE_CODE is the only argument.  Contents are initialized
   to zero except for a few of the common fields.  */

extern tree make_node ();

/* Make a copy of a node, with all the same contents except
   for TREE_UID and TREE_PERMANENT.  (The copy is permanent
   iff nodes being made now are permanent.)  */

extern tree copy_node ();

/* Make a copy of a chain of TREE_LIST nodes.  */

extern tree copy_list ();

/* Return the (unique) IDENTIFIER_NODE node for a given name.
   The name is supplied as a char *.  */

extern tree get_identifier ();

/* Construct various types of nodes.  */

extern tree build_int_2 ();
extern tree build_real ();
extern tree build_real_from_string ();
extern tree build_real_from_int_cst ();
extern tree build_complex ();
extern tree build_string ();
extern tree build ();
extern tree build_nt ();
extern tree build_tree_list ();
extern tree build_op_identifier ();
extern tree build_decl ();
extern tree build_let ();

/* Construct various nodes representing data types.  */

extern tree make_signed_type ();
extern tree make_unsigned_type ();
extern void fixup_unsigned_type ();
extern tree build_pointer_type ();
extern tree build_reference_type ();
extern tree build_index_type ();
extern tree build_array_type ();
extern tree build_function_type ();
extern tree build_method_type ();
extern tree build_offset_type ();
extern tree array_type_nelts ();

/* Construct expressions, performing type checking.  */

extern tree build_binary_op ();
extern tree build_indirect_ref ();
extern tree build_unary_op ();

/* Given a type node TYPE, and CONSTP and VOLATILEP, return a type
   for the same kind of data as TYPE describes.
   Variants point to the "main variant" (which has neither CONST nor VOLATILE)
   via TYPE_MAIN_VARIANT, and it points to a chain of other variants
   so that duplicate variants are never made.
   Only main variants should ever appear as types of expressions.  */

extern tree build_type_variant ();

/* Given a ..._TYPE node, calculate the TYPE_SIZE, TYPE_SIZE_UNIT,
   TYPE_ALIGN and TYPE_MODE fields.
   If called more than once on one node, does nothing except
   for the first time.  */

extern void layout_type ();

/* Given a hashcode and a ..._TYPE node (for which the hashcode was made),
   return a canonicalized ..._TYPE node, so that duplicates are not made.
   How the hash code is computed is up to the caller, as long as any two
   callers that could hash identical-looking type nodes agree.  */

extern tree type_hash_canon ();

/* Given a VAR_DECL, PARM_DECL, RESULT_DECL or FIELD_DECL node,
   calculates the DECL_SIZE, DECL_SIZE_UNIT, DECL_ALIGN and DECL_MODE
   fields.  Call this only once for any given decl node.

   Second argument is the boundary that this field can be assumed to
   be starting at (in bits).  Zero means it can be assumed aligned
   on any boundary that may be needed.  */

extern void layout_decl ();

/* Fold constants as much as possible in an expression.
   Returns the simplified expression.
   Acts only on the top level of the expression;
   if the argument itself cannot be simplified, its
   subexpressions are not changed.  */

extern tree fold ();

/* combine (tree_code, exp1, exp2) where EXP1 and EXP2 are constants
   returns a constant expression for the result of performing
   the operation specified by TREE_CODE on EXP1 and EXP2.  */

extern tree combine ();

extern tree convert ();
extern tree convert_units ();
extern tree size_in_bytes ();
extern tree genop ();
extern tree build_int ();
extern tree get_pending_sizes ();

/* Type for sizes of data-type.  */

extern tree sizetype;

/* Concatenate two lists (chains of TREE_LIST nodes) X and Y
   by making the last node in X point to Y.
   Returns X, except if X is 0 returns Y.  */

extern tree chainon ();

/* Make a new TREE_LIST node from specified PURPOSE, VALUE and CHAIN.  */

extern tree tree_cons (), perm_tree_cons (), temp_tree_cons ();
extern tree saveable_tree_cons ();

/* Return the last tree node in a chain.  */

extern tree tree_last ();

/* Reverse the order of elements in a chain, and return the new head.  */

extern tree nreverse ();

/* Returns the length of a chain of nodes
   (number of chain pointers to follow before reaching a null pointer).  */

extern int list_length ();

/* integer_zerop (tree x) is nonzero if X is an integer constant of value 0 */

extern int integer_zerop ();

/* integer_onep (tree x) is nonzero if X is an integer constant of value 1 */

extern int integer_onep ();

/* integer_all_onesp (tree x) is nonzero if X is an integer constant
   all of whose significant bits are 1.  */

extern int integer_all_onesp ();

/* type_unsigned_p (tree x) is nonzero if the type X is an unsigned type
   (all of its possible values are >= 0).
   If X is a pointer type, the value is 1.
   If X is a real type, the value is 0.  */

extern int type_unsigned_p ();

/* staticp (tree x) is nonzero if X is a reference to data allocated
   at a fixed address in memory.  */

extern int staticp ();

/* Gets an error if argument X is not an lvalue.
   Also returns 1 if X is an lvalue, 0 if not.  */

extern int lvalue_or_else ();

/* save_expr (EXP) returns an expression equivalent to EXP
   but it can be used multiple times within context CTX
   and only evaluate EXP once.  */

extern tree save_expr ();

/* stabilize_reference (EXP) returns an reference equivalent to EXP
   but it can be used multiple times
   and only evaluate the subexpressions once.  */

extern tree stabilize_reference ();

/* Return EXP, stripped of any conversions to wider types
   in such a way that the result of converting to type FOR_TYPE
   is the same as if EXP were converted to FOR_TYPE.
   If FOR_TYPE is 0, it signifies EXP's type.  */

extern tree get_unwidened ();

/* Return OP or a simpler expression for a narrower value
   which can be sign-extended or zero-extended to give back OP.
   Store in *UNSIGNEDP_PTR either 1 if the value should be zero-extended
   or 0 if the value should be sign-extended.  */

extern tree get_narrower ();

/* Given PRECISION and UNSIGNEDP, return a suitable type-tree
   for an integer type with at least that precision.
   The definition of this resides in language-specific code
   as the repertoire of available types may vary.  */

extern tree type_for_size ();

/* Given an integer type T, return a type like T but unsigned.
   If T is unsigned, the value is T.
   The definition of this resides in language-specific code
   as the repertoire of available types may vary.  */

extern tree unsigned_type ();

/* Given an integer type T, return a type like T but signed.
   If T is signed, the value is T.
   The definition of this resides in language-specific code
   as the repertoire of available types may vary.  */

extern tree signed_type ();

/* Return the floating type node for a given floating machine mode.  */

extern tree get_floating_type ();

/* Given the FUNCTION_DECL for the current function,
   return zero if it is ok for this function to be inline.
   Otherwise return a warning message with a single %s
   for the function's name.  */

extern char *function_cannot_inline_p ();

/* Declare commonly used variables for tree structure.  */

/* An integer constant with value 0 */
extern tree integer_zero_node;

/* An integer constant with value 1 */
extern tree integer_one_node;

/* An integer constant with value 0 whose type is sizetype.  */
extern tree size_zero_node;

/* An integer constant with value 1 whose type is sizetype.  */
extern tree size_one_node;

/* A constant of type pointer-to-int and value 0 */
extern tree null_pointer_node;

/* A node of type ERROR_MARK.  */
extern tree error_mark_node;

/* The type node for the void type.  */
extern tree void_type_node;

/* The type node for the ordinary (signed) integer type.  */
extern tree integer_type_node;

/* The type node for the unsigned integer type.  */
extern tree unsigned_type_node;

/* The type node for the ordinary character type.  */
extern tree char_type_node;

/* Points to the name of the input file from which the current input
   being parsed originally came (before it went into cpp).  */
extern char *input_filename;

/* Current source line number in that file.  */
extern int lineno;

/* Nonzero for -pedantic switch: warn about anything
   that standard C forbids.  */
extern int pedantic;

/* Nonzero means can safely call expand_expr now;
   otherwise layout_type puts variable sizes onto `pending_sizes' instead.  */

extern int immediate_size_expand;

/* Points to the FUNCTION_DECL of the function whose body we are reading. */

extern tree current_function_decl;

/* Nonzero if function being compiled can call setjmp.  */

extern int current_function_calls_setjmp;

/* Nonzero means all ..._TYPE nodes should be allocated permanently.  */

extern int all_types_permanent;

/* In stmt.c */

extern tree expand_start_stmt_expr ();
extern tree expand_end_stmt_expr ();
extern void expand_expr_stmt (), clear_last_expr ();
extern void expand_label (), expand_goto (), expand_asm ();
extern void expand_start_cond (), expand_end_cond ();
extern void expand_start_else (), expand_end_else ();
extern void expand_start_loop (), expand_start_loop_continue_elsewhere ();
extern void expand_loop_continue_here ();
extern void expand_end_loop ();
extern int expand_continue_loop ();
extern int expand_exit_loop (), expand_exit_loop_if_false ();
extern int expand_exit_something ();

extern void expand_start_delayed_expr ();
extern tree expand_end_delayed_expr ();
extern void expand_emit_delayed_expr ();

extern void expand_null_return (), expand_return ();
extern void expand_start_bindings (), expand_end_bindings ();
extern void expand_start_case (), expand_end_case ();
extern int pushcase (), pushcase_range ();
extern void expand_start_function (), expand_end_function ();
