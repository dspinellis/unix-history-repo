/* Definitions for C++ parsing and type checking.
   Copyright (C) 1987 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@cygnus.com)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* Borrow everything that is C from c-tree.h,
   but do so by copy, not by inclusion, since c-tree.h defines
   lang_identifier.  */

#ifndef PARANOID
#define PARANOID 0
#endif

/* Language-dependent contents of an identifier.  */

struct lang_identifier
{
  struct tree_identifier ignore;
  tree global_value, local_value;
  tree class_value;
  tree class_template_info;
  struct lang_id2 *x;
};

struct lang_id2
{
  tree label_value, implicit_decl;
  tree type_desc, as_list, error_locus;
};

/* Macros for access to language-specific slots in an identifier.  */

#if !PARANOID
#define IDENTIFIER_GLOBAL_VALUE(NODE)	\
  (((struct lang_identifier *)(NODE))->global_value)
#define IDENTIFIER_CLASS_VALUE(NODE)	\
  (((struct lang_identifier *)(NODE))->class_value)
#define IDENTIFIER_LOCAL_VALUE(NODE)	\
  (((struct lang_identifier *)(NODE))->local_value)
#define IDENTIFIER_TEMPLATE(NODE)	\
  (((struct lang_identifier *)(NODE))->class_template_info)
#else
#define IDENTIFIER_LANG_SPECIFIC_PTR(NODE) \
  (my_friendly_assert (TREE_CODE (NODE) == IDENTIFIER_NODE, 325),	\
   (struct lang_identifier *) (NODE))
#define IDENTIFIER_GLOBAL_VALUE(NODE)	\
  (((struct lang_identifier *)(NODE))->global_value)
#define IDENTIFIER_CLASS_VALUE(NODE)	\
  (((struct lang_identifier *)(NODE))->class_value)
#define IDENTIFIER_LOCAL_VALUE(NODE)	\
  (((struct lang_identifier *)(NODE))->local_value)
#define IDENTIFIER_TEMPLATE(NODE)	\
  (IDENTIFIER_LANG_SPECIFIC_PTR (NODE) -> class_template_info)
#endif

#if !PARANOID
#define IDENTIFIER_TYPE_VALUE(NODE) (TREE_TYPE (NODE))
#define SET_IDENTIFIER_TYPE_VALUE(NODE,TYPE) (TREE_TYPE (NODE) = TYPE)
#else
#define IDENTIFIER_TYPE_VALUE(NODE) (*IDENTIFIER_TYPE_VALUE_PTR(NODE))
#ifdef __GNUC__
__inline
#endif
static tree * IDENTIFIER_TYPE_VALUE_PTR(NODE) tree NODE; { return
  (my_friendly_assert (TREE_CODE (NODE) == IDENTIFIER_NODE, 326),
   &TREE_TYPE (NODE)) ;}
#define SET_IDENTIFIER_TYPE_VALUE(NODE,TYPE) (IDENTIFIER_TYPE_VALUE(NODE)=TYPE)
#endif
#define IDENTIFIER_HAS_TYPE_VALUE(NODE) (TREE_TYPE (NODE) ? 1 : 0)
extern tree identifier_typedecl_value ();
#define IDENTIFIER_TYPEDECL_VALUE(NODE) identifier_typedecl_value (NODE)

#define IDENTIFIER_LABEL_VALUE(NODE)	\
  (((struct lang_identifier *)(NODE))->x \
   ? ((struct lang_identifier *)(NODE))->x->label_value : 0)
#define SET_IDENTIFIER_LABEL_VALUE(NODE,VALUE)	\
  (((struct lang_identifier *)(NODE))->x == 0 ? ((struct lang_identifier *)(NODE))->x = (struct lang_id2 *)perm_calloc (1, sizeof (struct lang_id2)) : 0, \
   ((struct lang_identifier *)(NODE))->x->label_value = (VALUE))
#define IDENTIFIER_IMPLICIT_DECL(NODE)	\
  (((struct lang_identifier *)(NODE))->x \
   ? ((struct lang_identifier *)(NODE))->x->implicit_decl : 0)
#define SET_IDENTIFIER_IMPLICIT_DECL(NODE,VALUE)	\
  (((struct lang_identifier *)(NODE))->x == 0 ? ((struct lang_identifier *)(NODE))->x = (struct lang_id2 *)perm_calloc (1, sizeof (struct lang_id2)) : 0, \
   ((struct lang_identifier *)(NODE))->x->implicit_decl = (VALUE))
#define IDENTIFIER_AS_DESC(NODE)		\
  (((struct lang_identifier *)(NODE))->x \
   ? ((struct lang_identifier *)(NODE))->x->type_desc : 0)
#define SET_IDENTIFIER_AS_DESC(NODE,DESC)	\
  (((struct lang_identifier *)(NODE))->x == 0 ? ((struct lang_identifier *)(NODE))->x = (struct lang_id2 *)perm_calloc (1, sizeof (struct lang_id2)) : 0, \
   ((struct lang_identifier *)(NODE))->x->type_desc = (DESC))
#define IDENTIFIER_AS_LIST(NODE)		\
  (((struct lang_identifier *)(NODE))->x \
   ? ((struct lang_identifier *)(NODE))->x->as_list : 0)
#define SET_IDENTIFIER_AS_LIST(NODE,LIST)	\
  (((struct lang_identifier *)(NODE))->x == 0 ? ((struct lang_identifier *)(NODE))->x = (struct lang_id2 *)perm_calloc (1, sizeof (struct lang_id2)) : 0, \
   ((struct lang_identifier *)(NODE))->x->as_list = (LIST))
#define IDENTIFIER_ERROR_LOCUS(NODE)	\
  (((struct lang_identifier *)(NODE))->x \
   ? ((struct lang_identifier *)(NODE))->x->error_locus : 0)
#define SET_IDENTIFIER_ERROR_LOCUS(NODE,VALUE)	\
  (((struct lang_identifier *)(NODE))->x == 0 ? ((struct lang_identifier *)(NODE))->x = (struct lang_id2 *)perm_calloc (1, sizeof (struct lang_id2)) : 0, \
   ((struct lang_identifier *)(NODE))->x->error_locus = (VALUE))

#define IDENTIFIER_VIRTUAL_P(NODE) TREE_LANG_FLAG_1(NODE)

/* Nonzero if this identifier is the prefix for a mangled C++ operator name.  */
#define IDENTIFIER_OPNAME_P(NODE) TREE_LANG_FLAG_2(NODE)

#define IDENTIFIER_TYPENAME_P(NODE)	\
  (! strncmp (IDENTIFIER_POINTER (NODE),			\
	      IDENTIFIER_POINTER (ansi_opname[(int) TYPE_EXPR]),	\
	      IDENTIFIER_LENGTH (ansi_opname[(int) TYPE_EXPR])))

/* Nonzero means reject anything that ANSI standard C forbids.  */
extern int pedantic;

/* In a RECORD_TYPE or UNION_TYPE, nonzero if any component is read-only.  */
#define C_TYPE_FIELDS_READONLY(type) TYPE_LANG_FLAG_0 (type)

/* in tree.c */
extern tree purpose_member (), value_member ();
extern tree binfo_member ();

/* in cp-typeck.c */
extern tree build_component_ref (), build_conditional_expr ();
extern tree build_x_compound_expr (), build_compound_expr ();
extern tree build_unary_op (), build_binary_op (), build_function_call ();
extern tree build_binary_op_nodefault ();
extern tree build_indirect_ref (), build_array_ref (), build_c_cast ();
extern tree build_modify_expr ();
extern tree c_sizeof (), c_alignof ();
extern tree store_init_value ();
extern tree digest_init ();
extern tree c_expand_start_case ();
extern tree default_conversion ();
extern int comptypes (), compparms (), compexcepttypes ();

/* Given two integer or real types, return the type for their sum.
   Given two compatible ANSI C types, returns the merged type.  */

extern tree common_type ();

/* in cp-decl.c */
extern tree build_label ();

/* If non-zero, a VAR_DECL whose cleanup will cause a throw to the
   next exception handler.  */
extern tree exception_throw_decl;

extern int start_function ();
extern void finish_function ();
extern void store_parm_decls ();
extern tree get_parm_info ();

extern void pushlevel ();
extern tree poplevel ();

extern tree groktypename(), lookup_name();

extern tree lookup_label(), define_label(), shadow_label ();

extern tree implicitly_declare(), getdecls(), gettags ();

extern tree start_decl();
extern void finish_decl();

extern tree start_struct(), finish_struct(), xref_tag(), xref_defn_tag();
extern tree finish_exception ();
extern tree grokfield(), grokbitfield ();

extern tree start_enum(), finish_enum();
extern tree build_enumerator();

extern tree make_index_type ();
extern tree make_anon_name ();

#if 0 /* not yet, should get fixed properly later */
extern tree make_type_decl ();

#endif
extern void cplus_decl_attributes ();

/* Functions in c-common.c: */

/* Concatenate a list of STRING_CST nodes into one STRING_CST.  */
extern tree combine_strings ();

/* Validate the expression after `case' and apply default promotions.  */
extern tree check_case_value ();

/* Print an error message for invalid operands to arith operation CODE.
   NOP_EXPR is used as a special case (see truthvalue_conversion).  */

extern void binary_op_error ();

/* Subroutine of build_binary_op_nodefault, used for comparison operations.
   See if the operands have both been converted from subword integer types
   and, if so, perhaps change them both back to their original type.  */

extern tree shorten_compare ();

/* Prepare expr to be an argument of a TRUTH_NOT_EXPR,
   or validate its data type for an `if' or `while' statement or ?..: exp. */
extern tree truthvalue_conversion ();

extern tree double_type_node, long_double_type_node, float_type_node;
extern tree char_type_node, unsigned_char_type_node, signed_char_type_node;
extern tree ptrdiff_type_node;

extern tree short_integer_type_node, short_unsigned_type_node;
extern tree long_integer_type_node, long_unsigned_type_node;
extern tree long_long_integer_type_node, long_long_unsigned_type_node;
extern tree unsigned_type_node;
extern tree string_type_node, char_array_type_node, int_array_type_node;
extern tree wchar_array_type_node;
extern tree wchar_type_node, signed_wchar_type_node, unsigned_wchar_type_node;
extern tree intQI_type_node, unsigned_intQI_type_node;
extern tree intHI_type_node, unsigned_intHI_type_node;
extern tree intSI_type_node, unsigned_intSI_type_node;
extern tree intDI_type_node, unsigned_intDI_type_node;

extern int current_function_returns_value;
extern int current_function_returns_null;
extern tree current_function_return_value;

extern tree ridpointers[];
extern tree ansi_opname[];
extern tree ansi_assopname[];

/* Nonzero means `$' can be in an identifier.  */

extern int dollars_in_ident;

/* Nonzero means allow type mismatches in conditional expressions;
   just make their values `void'.   */

extern int flag_cond_mismatch;

/* Nonzero means don't recognize the keyword `asm'.  */

extern int flag_no_asm;

/* For cross referencing.  */

extern int flag_gnu_xref;

/* For environments where you can use GNU binutils (as, ld in particular).  */

extern int flag_gnu_binutils;

/* Nonzero means ignore `#ident' directives.  */

extern int flag_no_ident;

/* Nonzero means warn about implicit declarations.  */

extern int warn_implicit;

/* Nonzero means warn about function definitions that default the return type
   or that use a null return and have a return-type other than void.  */

extern int warn_return_type, explicit_warn_return_type;

/* Nonzero means give string constants the type `const char *'
   to get extra warnings from them.  These warnings will be too numerous
   to be useful, except in thoroughly ANSIfied programs.  */

extern int warn_write_strings;

/* Nonzero means warn about sizeof(function) or addition/subtraction
   of function pointers.  */

extern int warn_pointer_arith;

/* Nonzero means warn for all old-style non-prototype function decls.  */

extern int warn_strict_prototypes;

/* Nonzero means warn about suggesting putting in ()'s.  */

extern int warn_parentheses;

/* Warn about a subscript that has type char.  */

extern int warn_char_subscripts;

/* Nonzero means warn about pointer casts that can drop a type qualifier
   from the pointer target type.  */

extern int warn_cast_qual;

/* Warn about traditional constructs whose meanings changed in ANSI C.  */

extern int warn_traditional;

/* Nonzero means warn about non virtual destructors in classes that have
   virtual functions. */

extern int warn_nonvdtor;

/* Nonzero means do some things the same way PCC does.  */

extern int flag_traditional;

/* Nonzero means to treat bitfields as unsigned unless they say `signed'.  */

extern int flag_signed_bitfields;

/* 3 means write out only virtuals function tables `defined'
   in this implementation file.
   2 means write out only specific virtual function tables
   and give them (C) public visibility.
   1 means write out virtual function tables and give them
   (C) public visibility.
   0 means write out virtual function tables and give them
   (C) static visibility (default).
   -1 means declare virtual function tables extern.  */

extern int write_virtuals;

/* INTERFACE_ONLY nonzero means that we are in an "interface"
   section of the compiler.  INTERFACE_UNKNOWN nonzero means
   we cannot trust the value of INTERFACE_ONLY.  If INTERFACE_UNKNOWN
   is zero and INTERFACE_ONLY is zero, it means that we are responsible
   for exporting definitions that others might need.  */
extern int interface_only, interface_unknown;

/* Nonzero means we should attempt to elide constructors when possible.  */

extern int flag_elide_constructors;

/* Nonzero means recognize and handle exception handling constructs.  */

extern int flag_handle_exceptions;

/* Nonzero means recognize and handle ansi-style exception handling constructs.  */

extern int flag_ansi_exceptions;

/* Nonzero means that member functions defined in class scope are
   inline by default.  */

extern int flag_default_inline;

/* Nonzero means emit cadillac protocol.  */

extern int flag_cadillac;

/* C++ language-specific tree codes.  */
#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) SYM,
enum cplus_tree_code {
  __DUMMY = LAST_AND_UNUSED_TREE_CODE,
#include "cp-tree.def"
  LAST_CPLUS_TREE_CODE
};
#undef DEFTREECODE

enum languages { lang_c, lang_cplusplus };

/* Macros to make error reporting functions' lives easier.  */
#if !PARANOID
#define TYPE_IDENTIFIER(NODE) (DECL_NAME (TYPE_NAME (NODE)))
#else
#define TYPE_IDENTIFIER(NODE) (*TYPE_IDENTIFIER_PTR (NODE))
#ifdef __GNUC__
__inline
#endif
static tree *
TYPE_IDENTIFIER_PTR(NODE) tree NODE; { return
  (my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (NODE)) == 't', 327),
   &DECL_NAME (TYPE_NAME (NODE))) ;}
#endif

#define TYPE_NAME_STRING(NODE) (IDENTIFIER_POINTER (TYPE_IDENTIFIER (NODE)))
#define TYPE_NAME_LENGTH(NODE) (IDENTIFIER_LENGTH (TYPE_IDENTIFIER (NODE)))

#define TYPE_ASSEMBLER_NAME_STRING(NODE) (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (TYPE_NAME  (NODE))))
#define TYPE_ASSEMBLER_NAME_LENGTH(NODE) (IDENTIFIER_LENGTH (DECL_ASSEMBLER_NAME (TYPE_NAME (NODE))))

#define IS_AGGR_TYPE_2(TYPE1,TYPE2) \
  (TREE_CODE (TYPE1) == TREE_CODE (TYPE2)	\
   && IS_AGGR_TYPE (TYPE1)&IS_AGGR_TYPE (TYPE2))

/* Macros which might want to be replaced by function calls.  */

#if 1
/* Virtual function addresses can be gotten from a virtual function
   table entry using this macro.  */
#define FNADDR_FROM_VTABLE_ENTRY(ENTRY) \
  TREE_VALUE (TREE_CHAIN (TREE_CHAIN (CONSTRUCTOR_ELTS (ENTRY))))
#define SET_FNADDR_FROM_VTABLE_ENTRY(ENTRY,VALUE) \
  (TREE_VALUE (TREE_CHAIN (TREE_CHAIN (CONSTRUCTOR_ELTS (ENTRY)))) = (VALUE))

#define FUNCTION_ARG_CHAIN(NODE) (TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (NODE))))
#define PROMOTES_TO_AGGR_TYPE(NODE,CODE)	\
  (((CODE) == TREE_CODE (NODE)			\
       && IS_AGGR_TYPE (TREE_TYPE (NODE)))	\
   || IS_AGGR_TYPE (NODE))

#else
extern tree fnaddr_from_vtable_entry ();
extern void set_fnaddr_from_vtable_entry ();
extern tree function_arg_chain ();
extern int promotes_to_aggr_type ();
extern int is_aggr_type_2 ();
#define FNADDR_FROM_VTABLE_ENTRY(ENTRY) (fnaddr_from_vtable_entry (ENTRY))
#define SET_FNADDR_FROM_VTABLE_ENTRY(ENTRY,VALUE) \
  (set_fnaddr_from_vtable_entry (ENTRY, VALUE))
/* #define TYPE_NAME_STRING(NODE) (type_name_string (NODE)) */
#define FUNCTION_ARG_CHAIN(NODE) (function_arg_chain (NODE))
#define PROMOTES_TO_AGGR_TYPE(NODE,CODE) (promotes_to_aggr_type (NODE, CODE))
/* #define IS_AGGR_TYPE_2(TYPE1, TYPE2) (is_aggr_type_2 (TYPE1, TYPE2)) */
#endif
/* Nonzero iff TYPE is uniquely derived from PARENT.  Under MI, PARENT can be an
   ambiguous base class of TYPE, and this macro will be false.  */
#define UNIQUELY_DERIVED_FROM_P(PARENT, TYPE) (get_base_distance (PARENT, TYPE, 0, 0) >= 0)

enum conversion_type { ptr_conv, constptr_conv, int_conv, real_conv, last_conversion_type };

/* Statistics show that while the GNU C++ compiler may generate
   thousands of different types during a compilation run, it
   generates relatively few (tens) of classtypes.  Because of this,
   it is not costly to store a generous amount of information
   in classtype nodes.  This struct must fill out to a multiple of 4 bytes.  */
struct lang_type
{
  struct
    {
      unsigned has_type_conversion : 1;
      unsigned has_int_conversion : 1;
      unsigned has_float_conversion : 1;
      unsigned has_init_ref : 1;
      unsigned gets_init_ref : 1;
      unsigned gets_init_aggr : 1;
      unsigned has_assignment : 1;
      unsigned gets_assignment : 1;

      unsigned needs_constructor : 1;
      unsigned has_default_ctor : 1;
      unsigned uses_multiple_inheritance : 1;
      unsigned const_needs_init : 1;
      unsigned ref_needs_init : 1;
      unsigned gets_const_init_ref : 1;
      unsigned has_const_assign_ref : 1;
      unsigned gets_const_assign_ref : 1;

      unsigned vtable_needs_writing : 1;
      unsigned has_assign_ref : 1;
      unsigned gets_assign_ref : 1;
      unsigned gets_new : 1;
      unsigned gets_delete : 1;
      unsigned has_call_overloaded : 1;
      unsigned has_array_ref_overloaded : 1;
      unsigned has_arrow_overloaded : 1;

      unsigned local_typedecls : 1;
      unsigned interface_only : 1;
      unsigned interface_unknown : 1;
      unsigned needs_virtual_reinit : 1;
      unsigned declared_exception : 1;
      unsigned declared_class : 1;
      unsigned being_defined : 1;
      unsigned redefined : 1;

      unsigned marked : 1;
      unsigned marked2 : 1;
      unsigned marked3 : 1;
      unsigned marked4 : 1;
      unsigned marked5 : 1;
      unsigned marked6 : 1;
      unsigned use_template : 2;

      unsigned debug_requested : 1;
      unsigned dynamic : 1;
      unsigned has_method_call_overloaded : 1;
      unsigned private_attr : 1;
      unsigned alters_visibilities : 1;
      unsigned got_semicolon : 1;
      unsigned dummy : 1;

      /* The MIPS compiler gets it wrong if this struct also
	 does not fill out to a multiple of 4 bytes.  */
      unsigned n_vancestors : 16;
    } type_flags;

  int cid;
  int n_ancestors;
  int vsize;
  int max_depth;

  union tree_node *vbinfo[2];
  union tree_node *baselink_vec;
  union tree_node *vfields;
  union tree_node *vbases;
  union tree_node *vbase_size;

  union tree_node *tags;
  char *memoized_table_entry;

  char *search_slot;

#ifdef ONLY_INT_FIELDS
  unsigned int mode : 8;
#else
  enum machine_mode mode : 8;
#endif

  unsigned char size_unit;
  unsigned char align;
  unsigned char sep_unit;

  union tree_node *sep;
  union tree_node *size;

  union tree_node *base_init_list;
  union tree_node *abstract_virtuals;
  union tree_node *as_list;
  union tree_node *id_as_list;
  union tree_node *binfo_as_list;
  union tree_node *vtbl_ptr;
  union tree_node *instance_variable;
  union tree_node *friend_classes;

  char *mi_matrix;
  union tree_node *conversions[last_conversion_type];

  union tree_node *dossier;

#ifdef SOS
  union tree_node *typename_as_string;
  union tree_node *dynamic_filename;
  union tree_node *dynamic_table;
#endif
};

/* Indicates whether a template should be (or has been) expanded for this
   class definition.  0=do, 1=did, 2=don't, 3=didn't.  */
#define CLASSTYPE_USE_TEMPLATE(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.use_template)

/* Fields used for storing information before the class is defined.
   After the class is defined, these fields hold other information.  */

/* List of friends which were defined inline in this class definition.  */
#define CLASSTYPE_INLINE_FRIENDS(NODE) (TYPE_NONCOPIED_PARTS (NODE))

/* Nonzero for _CLASSTYPE means that the _CLASSTYPE either has
   a special meaning for the assignment operator ("operator="),
   or one of its fields (or base members) has a special meaning
   defined.  */
#define TYPE_HAS_ASSIGNMENT(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_assignment)
#define TYPE_GETS_ASSIGNMENT(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.gets_assignment)

/* Nonzero for _CLASSTYPE means that operator new and delete are defined,
   respectively.  */
#define TREE_GETS_NEW(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.gets_new)
#define TREE_GETS_DELETE(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.gets_delete)

/* Nonzero for TREE_LIST or _TYPE node means that this node is class-local.  */
#define TREE_NONLOCAL_FLAG(NODE) (TREE_LANG_FLAG_0 (NODE))

/* Nonzero for a _CLASSTYPE node which we know to be private.  */
#define TYPE_PRIVATE_P(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.private_attr)

/* Nonzero means that this _CLASSTYPE node defines ways of converting
   itself to other types.  */
#define TYPE_HAS_CONVERSION(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_type_conversion)

/* Nonzero means that this _CLASSTYPE node can convert itself to an
   INTEGER_TYPE.  */
#define TYPE_HAS_INT_CONVERSION(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_int_conversion)

/* Nonzero means that this _CLASSTYPE node can convert itself to an
   REAL_TYPE.  */
#define TYPE_HAS_REAL_CONVERSION(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_float_conversion)

/* Nonzero means that this _CLASSTYPE node overloads operator=(X&).  */
#define TYPE_HAS_ASSIGN_REF(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_assign_ref)
#define TYPE_GETS_ASSIGN_REF(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.gets_assign_ref)
#define TYPE_HAS_CONST_ASSIGN_REF(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_const_assign_ref)
#define TYPE_GETS_CONST_ASSIGN_REF(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.gets_const_assign_ref)

/* Nonzero means that this _CLASSTYPE node has an X(X&) constructor.  */
#define TYPE_HAS_INIT_REF(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_init_ref)
#define TYPE_GETS_INIT_REF(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.gets_init_ref)
#define TYPE_GETS_CONST_INIT_REF(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.gets_const_init_ref)

/* Nonzero means that this _CLASSTYPE node has an X(X ...) constructor.
   Note that there must be other arguments, or this constructor is flagged
   as being erroneous.  */
#define TYPE_GETS_INIT_AGGR(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.gets_init_aggr)

/* Nonzero means that this type is being defined.  I.e., the left brace
   starting the definition of this type has been seen.  */
#define TYPE_BEING_DEFINED(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.being_defined)
/* Nonzero means that this type has been redefined.  In this case, if
   convenient, don't reprocess any methods that appear in its redefinition.  */
#define TYPE_REDEFINED(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.redefined)

/* Nonzero means that this _CLASSTYPE node overloads the method call
   operator.  In this case, all method calls go through `operator->()(...).  */
#define TYPE_OVERLOADS_METHOD_CALL_EXPR(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_method_call_overloaded)

/* The is the VAR_DECL that contains NODE's dossier.  */
#define CLASSTYPE_DOSSIER(NODE) (TYPE_LANG_SPECIFIC(NODE)->dossier)

/* Nonzero means that this _CLASSTYPE node overloads operator().  */
#define TYPE_OVERLOADS_CALL_EXPR(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_call_overloaded)

/* Nonzero means that this _CLASSTYPE node overloads operator[].  */
#define TYPE_OVERLOADS_ARRAY_REF(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_array_ref_overloaded)

/* Nonzero means that this _CLASSTYPE node overloads operator->.  */
#define TYPE_OVERLOADS_ARROW(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_arrow_overloaded)

/* Nonzero means that this _CLASSTYPE (or one of its ancestors) uses
   multiple inheritance.  If this is 0 for the root of a type
   hierarchy, then we can use more efficient search techniques.  */
#define TYPE_USES_MULTIPLE_INHERITANCE(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.uses_multiple_inheritance)

/* Nonzero means that this _CLASSTYPE (or one of its ancestors) uses
   virtual base classes.  If this is 0 for the root of a type
   hierarchy, then we can use more efficient search techniques.  */
#define TYPE_USES_VIRTUAL_BASECLASSES(NODE) (TREE_LANG_FLAG_3(NODE))

/* List of lists of member functions defined in this class.  */
#define CLASSTYPE_METHOD_VEC(NODE) TYPE_METHODS(NODE)

/* Pointer from any member function to the head of the list of
   member functions of the type that member function belongs to.  */
#define CLASSTYPE_BASELINK_VEC(NODE) (TYPE_LANG_SPECIFIC(NODE)->baselink_vec)

/* Mark bits for depth-first and breath-first searches.  */
#if !PARANOID
#define CLASSTYPE_MARKED(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.marked)
#define CLASSTYPE_MARKED2(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.marked2)
#define CLASSTYPE_MARKED3(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.marked3)
#define CLASSTYPE_MARKED4(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.marked4)
#define CLASSTYPE_MARKED5(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.marked5)
#define CLASSTYPE_MARKED6(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.marked6)
/* Macros to modify the above flags */
#define SET_CLASSTYPE_MARKED(NODE) (CLASSTYPE_MARKED(NODE) = 1)
#define CLEAR_CLASSTYPE_MARKED(NODE) (CLASSTYPE_MARKED(NODE) = 0)
#define SET_CLASSTYPE_MARKED2(NODE) (CLASSTYPE_MARKED2(NODE) = 1)
#define CLEAR_CLASSTYPE_MARKED2(NODE) (CLASSTYPE_MARKED2(NODE) = 0)
#define SET_CLASSTYPE_MARKED3(NODE) (CLASSTYPE_MARKED3(NODE) = 1)
#define CLEAR_CLASSTYPE_MARKED3(NODE) (CLASSTYPE_MARKED3(NODE) = 0)
#define SET_CLASSTYPE_MARKED4(NODE) (CLASSTYPE_MARKED4(NODE) = 1)
#define CLEAR_CLASSTYPE_MARKED4(NODE) (CLASSTYPE_MARKED4(NODE) = 0)
#define SET_CLASSTYPE_MARKED5(NODE) (CLASSTYPE_MARKED5(NODE) = 1)
#define CLEAR_CLASSTYPE_MARKED5(NODE) (CLASSTYPE_MARKED5(NODE) = 0)
#define SET_CLASSTYPE_MARKED6(NODE) (CLASSTYPE_MARKED6(NODE) = 1)
#define CLEAR_CLASSTYPE_MARKED6(NODE) (CLASSTYPE_MARKED6(NODE) = 0)
#else
#define CLASSTYPE_MARKED(NODE) (my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (NODE)) == 't', 328), TYPE_LANG_SPECIFIC(NODE)->type_flags.marked)
#define CLASSTYPE_MARKED2(NODE) (my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (NODE)) == 't', 329), TYPE_LANG_SPECIFIC(NODE)->type_flags.marked2)
#define CLASSTYPE_MARKED3(NODE) (my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (NODE)) == 't', 330), TYPE_LANG_SPECIFIC(NODE)->type_flags.marked3)
#define CLASSTYPE_MARKED4(NODE) (my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (NODE)) == 't', 331), TYPE_LANG_SPECIFIC(NODE)->type_flags.marked4)
#define CLASSTYPE_MARKED5(NODE) (my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (NODE)) == 't', 332), TYPE_LANG_SPECIFIC(NODE)->type_flags.marked5)
#define CLASSTYPE_MARKED6(NODE) (my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (NODE)) == 't', 333), TYPE_LANG_SPECIFIC(NODE)->type_flags.marked6)
/* Macros to modify the above flags */
#define SET_CLASSTYPE_MARKED(NODE) (my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (NODE)) == 't', 334), TYPE_LANG_SPECIFIC(NODE)->type_flags.marked = 1)
#define CLEAR_CLASSTYPE_MARKED(NODE) (my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (NODE)) == 't', 335), TYPE_LANG_SPECIFIC(NODE)->type_flags.marked = 0)
#define SET_CLASSTYPE_MARKED2(NODE) (my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (NODE)) == 't', 336), TYPE_LANG_SPECIFIC(NODE)->type_flags.marked2 = 1)
#define CLEAR_CLASSTYPE_MARKED2(NODE) (my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (NODE)) == 't', 337), TYPE_LANG_SPECIFIC(NODE)->type_flags.marked2 = 0)
#define SET_CLASSTYPE_MARKED3(NODE) (my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (NODE)) == 't', 338), TYPE_LANG_SPECIFIC(NODE)->type_flags.marked3 = 1)
#define CLEAR_CLASSTYPE_MARKED3(NODE) (my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (NODE)) == 't', 339), TYPE_LANG_SPECIFIC(NODE)->type_flags.marked3 = 0)
#define SET_CLASSTYPE_MARKED4(NODE) (my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (NODE)) == 't', 340), TYPE_LANG_SPECIFIC(NODE)->type_flags.marked4 = 1)
#define CLEAR_CLASSTYPE_MARKED4(NODE) (my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (NODE)) == 't', 341), TYPE_LANG_SPECIFIC(NODE)->type_flags.marked4 = 0)
#define SET_CLASSTYPE_MARKED5(NODE) (my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (NODE)) == 't', 342), TYPE_LANG_SPECIFIC(NODE)->type_flags.marked5 = 1)
#define CLEAR_CLASSTYPE_MARKED5(NODE) (my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (NODE)) == 't', 343), TYPE_LANG_SPECIFIC(NODE)->type_flags.marked5 = 0)
#define SET_CLASSTYPE_MARKED6(NODE) (my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (NODE)) == 't', 344), TYPE_LANG_SPECIFIC(NODE)->type_flags.marked6 = 1)
#define CLEAR_CLASSTYPE_MARKED6(NODE) (my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (NODE)) == 't', 345), TYPE_LANG_SPECIFIC(NODE)->type_flags.marked6 = 0)
#endif

#define CLASSTYPE_TAGS(NODE) (TYPE_LANG_SPECIFIC(NODE)->tags)
#define CLASSTYPE_NAMES(NODE) (TYPE_LANG_SPECIFIC(NODE)->names)

/* Remove when done merging.  */
#define CLASSTYPE_VFIELD(NODE) TYPE_VFIELD(NODE)

/* The number of virtual functions defined for this
   _CLASSTYPE node.  */
#define CLASSTYPE_VSIZE(NODE) (TYPE_LANG_SPECIFIC(NODE)->vsize)
/* The virtual base classes that this type uses.  */
#define CLASSTYPE_VBASECLASSES(NODE) (TYPE_LANG_SPECIFIC(NODE)->vbases)
/* The virtual function pointer fields that this type contains.  */
#define CLASSTYPE_VFIELDS(NODE) (TYPE_LANG_SPECIFIC(NODE)->vfields)

/* Number of baseclasses defined for this type.
   0 means no base classes.  */
#define CLASSTYPE_N_BASECLASSES(NODE) \
  (TYPE_BINFO_BASETYPES (NODE) ? TREE_VEC_LENGTH (TYPE_BINFO_BASETYPES(NODE)) : 0)

/* Memoize the number of super classes (base classes) tha this node
   has.  That way we can know immediately (albeit conservatively how
   large a multiple-inheritance matrix we need to build to find
   derivation information.  */
#define CLASSTYPE_N_SUPERCLASSES(NODE) (TYPE_LANG_SPECIFIC(NODE)->n_ancestors)
#define CLASSTYPE_N_VBASECLASSES(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.n_vancestors)

/* Record how deep the inheritance is for this class so `void*' conversions
   are less favorable than a conversion to the most base type.  */
#define CLASSTYPE_MAX_DEPTH(NODE) (TYPE_LANG_SPECIFIC(NODE)->max_depth)

/* Used for keeping search-specific information.  Any search routine
   which uses this must define what exactly this slot is used for.  */
#define CLASSTYPE_SEARCH_SLOT(NODE) (TYPE_LANG_SPECIFIC(NODE)->search_slot)

/* Entry for keeping memoization tables for this type to
   hopefully speed up search routines.  Since it is a pointer,
   it can mean almost anything.  */
#define CLASSTYPE_MTABLE_ENTRY(NODE) (TYPE_LANG_SPECIFIC(NODE)->memoized_table_entry)

/* This is the total size of the baseclasses defined for this type.
   Needed because it is desirable to layout such information
   before beginning to process the class itself, and we
   don't want to compute it second time when actually laying
   out the type for real.  */
#define CLASSTYPE_SIZE(NODE) (TYPE_LANG_SPECIFIC(NODE)->size)
#define CLASSTYPE_SIZE_UNIT(NODE) (TYPE_LANG_SPECIFIC(NODE)->size_unit)
#define CLASSTYPE_MODE(NODE) (TYPE_LANG_SPECIFIC(NODE)->mode)
#define CLASSTYPE_ALIGN(NODE) (TYPE_LANG_SPECIFIC(NODE)->align)

/* This is the space needed for virtual base classes.  NULL if
   there are no virtual basetypes.  */
#define CLASSTYPE_VBASE_SIZE(NODE) (TYPE_LANG_SPECIFIC(NODE)->vbase_size)

/* A cons list of structure elements which either have constructors
   to be called, or virtual function table pointers which
   need initializing.  Depending on what is being initialized,
   the TREE_PURPOSE and TREE_VALUE fields have different meanings:

   Member initialization: <FIELD_DECL, TYPE>
   Base class construction: <NULL_TREE, BASETYPE>
   Base class initialization: <BASE_INITIALIZATION, THESE_INITIALIZATIONS>
   Whole type: <MEMBER_INIT, BASE_INIT>.  */
#define CLASSTYPE_BASE_INIT_LIST(NODE) (TYPE_LANG_SPECIFIC(NODE)->base_init_list)

/* A cons list of virtual functions which cannot be inherited by
   derived classes.  When deriving from this type, the derived
   class must provide its own definition for each of these functions.  */
#define CLASSTYPE_ABSTRACT_VIRTUALS(NODE) (TYPE_LANG_SPECIFIC(NODE)->abstract_virtuals)

#define CLASSTYPE_ALTERS_VISIBILITIES_P(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.alters_visibilities)

/* Nonzero means that this aggr type has been `closed' by a semicolon.  */
#define CLASSTYPE_GOT_SEMICOLON(NODE) (TYPE_LANG_SPECIFIC (NODE)->type_flags.got_semicolon)

/* Nonzero means that the main virtual function table pointer needs to be
   set because base constructors have placed the wrong value there.
   If this is zero, it means that they placed the right value there,
   and there is no need to change it.  */
#define CLASSTYPE_NEEDS_VIRTUAL_REINIT(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.needs_virtual_reinit)

/* Nonzero means that if this type has virtual functions, that
   the virtual function table will be written out.  */
#define CLASSTYPE_VTABLE_NEEDS_WRITING(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.vtable_needs_writing)

/* Nonzero means that this type defines its own local type declarations.  */
#define CLASSTYPE_LOCAL_TYPEDECLS(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.local_typedecls)

/* Nonzero means that this type has an X() constructor.  */
#define TYPE_HAS_DEFAULT_CONSTRUCTOR(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_default_ctor)

/* Many routines need to cons up a list of basetypes for visibility
   checking.  This field contains a TREE_LIST node whose TREE_VALUE
   is the main variant of the type, and whose TREE_VIA_PUBLIC
   and TREE_VIA_VIRTUAL bits are correctly set.  */
#define CLASSTYPE_AS_LIST(NODE) (TYPE_LANG_SPECIFIC(NODE)->as_list)
/* Same, but cache a list whose value is the name of this type.  */
#define CLASSTYPE_ID_AS_LIST(NODE) (TYPE_LANG_SPECIFIC(NODE)->id_as_list)
/* Same, but cache a list whose value is the binfo of this type.  */
#define CLASSTYPE_BINFO_AS_LIST(NODE) (TYPE_LANG_SPECIFIC(NODE)->binfo_as_list)

/* Slot in which to cache a copy of the local vtable pointer.  */
#define CLASSTYPE_VTBL_PTR(NODE) (TYPE_LANG_SPECIFIC(NODE)->vtbl_ptr)

/* Hold the instance object associated with this method.  */
#define CLASSTYPE_INST_VAR(NODE) (TYPE_LANG_SPECIFIC(NODE)->instance_variable)

/* A list of class types with which this type is a friend.  */
#define CLASSTYPE_FRIEND_CLASSES(NODE) (TYPE_LANG_SPECIFIC(NODE)->friend_classes)

/* Keep an inheritance lattice around so we can quickly tell whether
   a type is derived from another or not.  */
#define CLASSTYPE_MI_MATRIX(NODE) (TYPE_LANG_SPECIFIC(NODE)->mi_matrix)

/* If there is exactly one conversion to a non-void, non-const pointer type,
   remember that here.  If there are more than one, put
   `error_mark_node' here.  If there are none, this holds NULL_TREE.  */
#define CLASSTYPE_CONVERSION(NODE,KIND) \
  (TYPE_LANG_SPECIFIC(NODE)->conversions[(int) KIND])

/* Nonzero means that class is "dynamic" in SOS sense.  (IRIA-specific.)  */
#define TYPE_DYNAMIC(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.dynamic)

#ifdef SOS
/* The name of this type as a STRING.  */
#define CLASSTYPE_TYPENAME_AS_STRING(NODE) (TYPE_LANG_SPECIFIC(NODE)->typename_as_string)
/* The name of the file which defines this type.  */
#define CLASSTYPE_DYNAMIC_FILENAME(NODE) (TYPE_LANG_SPECIFIC(NODE)->dynamic_filename)
/* The table of all member functions, linearized.  */
#define CLASSTYPE_DYNAMIC_TABLE(NODE) (TYPE_LANG_SPECIFIC(NODE)->dynamic_table)
#endif

/* Say whether this node was declared as a "class" or a "struct".  */
#define CLASSTYPE_DECLARED_CLASS(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.declared_class)
/* Say whether this node was declared as a "class" or a "struct".  */
#define CLASSTYPE_DECLARED_EXCEPTION(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.declared_exception)

/* Nonzero if this class has const members which have no specified initialization.  */
#define CLASSTYPE_READONLY_FIELDS_NEED_INIT(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.const_needs_init)

/* Nonzero if this class has ref members which have no specified initialization.  */
#define CLASSTYPE_REF_FIELDS_NEED_INIT(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.ref_needs_init)

/* Nonzero if this class is included from a header file which employs
   `#pragma interface', and it is not included in its implementation file.  */
#define CLASSTYPE_INTERFACE_ONLY(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.interface_only)

/* Same as above, but for classes whose purpose we do not know.  */
#define CLASSTYPE_INTERFACE_UNKNOWN(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.interface_unknown)

/* Nonzero if a _DECL node requires us to output debug info for this class.  */
#define CLASSTYPE_DEBUG_REQUESTED(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.debug_requested)

/* Additional macros for inheritance information.  */

#define CLASSTYPE_VBINFO(NODE,VIA_PUBLIC) \
  (TYPE_LANG_SPECIFIC (NODE)->vbinfo[VIA_PUBLIC])

/* When following an binfo-specific chain, this is the cumulative
   via-public flag.  */
#define BINFO_VIA_PUBLIC(NODE) TREE_LANG_FLAG_5 (NODE)

/* When building a matrix to determine by a single lookup
   whether one class is derived from another or not,
   this field is the index of the class in the table.  */
#define CLASSTYPE_CID(NODE) (TYPE_LANG_SPECIFIC(NODE)->cid)
#define BINFO_CID(NODE) CLASSTYPE_CID(BINFO_TYPE(NODE))

/* Nonzero means marked by DFS or BFS search, including searches
   by `get_binfo' and `get_base_distance'.  */
#define BINFO_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?CLASSTYPE_MARKED(BINFO_TYPE(NODE)):TREE_LANG_FLAG_0(NODE))
/* Macros needed because of C compilers that don't allow conditional
   expressions to be lvalues.  Grr!  */
#define SET_BINFO_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?SET_CLASSTYPE_MARKED(BINFO_TYPE(NODE)):(TREE_LANG_FLAG_0(NODE)=1))
#define CLEAR_BINFO_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?CLEAR_CLASSTYPE_MARKED(BINFO_TYPE(NODE)):(TREE_LANG_FLAG_0(NODE)=0))

/* Nonzero means marked in building initialization list.  */
#define BINFO_BASEINIT_MARKED(NODE) CLASSTYPE_MARKED2 (BINFO_TYPE (NODE))
/* Modifier macros */
#define SET_BINFO_BASEINIT_MARKED(NODE) SET_CLASSTYPE_MARKED2 (BINFO_TYPE (NODE))
#define CLEAR_BINFO_BASEINIT_MARKED(NODE) CLEAR_CLASSTYPE_MARKED2 (BINFO_TYPE (NODE))

/* Nonzero means marked in search through virtual inheritance hierarchy.  */
#define BINFO_VBASE_MARKED(NODE) CLASSTYPE_MARKED2 (BINFO_TYPE (NODE))
/* Modifier macros */
#define SET_BINFO_VBASE_MARKED(NODE) SET_CLASSTYPE_MARKED2 (BINFO_TYPE (NODE))
#define CLEAR_BINFO_VBASE_MARKED(NODE) CLEAR_CLASSTYPE_MARKED2 (BINFO_TYPE (NODE))

/* Nonzero means marked in search for members or member functions.  */
#define BINFO_FIELDS_MARKED(NODE) \
  (TREE_VIA_VIRTUAL(NODE)?CLASSTYPE_MARKED2 (BINFO_TYPE (NODE)):TREE_LANG_FLAG_2(NODE))
#define SET_BINFO_FIELDS_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?SET_CLASSTYPE_MARKED2(BINFO_TYPE(NODE)):(TREE_LANG_FLAG_2(NODE)=1))
#define CLEAR_BINFO_FIELDS_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?CLEAR_CLASSTYPE_MARKED2(BINFO_TYPE(NODE)):(TREE_LANG_FLAG_2(NODE)=0))

/* Nonzero means that this class is on a path leading to a new vtable.  */
#define BINFO_VTABLE_PATH_MARKED(NODE) \
  (TREE_VIA_VIRTUAL(NODE)?CLASSTYPE_MARKED3(BINFO_TYPE(NODE)):TREE_LANG_FLAG_3(NODE))
#define SET_BINFO_VTABLE_PATH_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?SET_CLASSTYPE_MARKED3(BINFO_TYPE(NODE)):(TREE_LANG_FLAG_3(NODE)=1))
#define CLEAR_BINFO_VTABLE_PATH_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?CLEAR_CLASSTYPE_MARKED3(BINFO_TYPE(NODE)):(TREE_LANG_FLAG_3(NODE)=0))

/* Nonzero means that this class has a new vtable.  */
#define BINFO_NEW_VTABLE_MARKED(NODE) \
  (TREE_VIA_VIRTUAL(NODE)?CLASSTYPE_MARKED4(BINFO_TYPE(NODE)):TREE_LANG_FLAG_4(NODE))
#define SET_BINFO_NEW_VTABLE_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?SET_CLASSTYPE_MARKED4(BINFO_TYPE(NODE)):(TREE_LANG_FLAG_4(NODE)=1))
#define CLEAR_BINFO_NEW_VTABLE_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?CLEAR_CLASSTYPE_MARKED4(BINFO_TYPE(NODE)):(TREE_LANG_FLAG_4(NODE)=0))

/* Nonzero means this class has initialized its virtual baseclasses.  */
#define BINFO_VBASE_INIT_MARKED(NODE) \
  (TREE_VIA_VIRTUAL(NODE)?CLASSTYPE_MARKED5(BINFO_TYPE(NODE)):TREE_LANG_FLAG_5(NODE))
#define SET_BINFO_VBASE_INIT_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?SET_CLASSTYPE_MARKED5(BINFO_TYPE(NODE)):(TREE_LANG_FLAG_5(NODE)=1))
#define CLEAR_BINFO_VBASE_INIT_MARKED(NODE) (TREE_VIA_VIRTUAL(NODE)?CLEAR_CLASSTYPE_MARKED5(BINFO_TYPE(NODE)):(TREE_LANG_FLAG_5(NODE)=0))

/* Accessor macros for the vfield slots in structures.  */

/* Get the assoc info that caused this vfield to exist.  */
#define VF_BINFO_VALUE(NODE) TREE_PURPOSE (NODE)

/* Get that same information as a _TYPE.  */
#define VF_BASETYPE_VALUE(NODE) TREE_VALUE (NODE)

/* Get the value of the top-most type dominating the non-`normal' vfields.  */
#define VF_DERIVED_VALUE(NODE) (VF_BINFO_VALUE (NODE) ? BINFO_TYPE (VF_BINFO_VALUE (NODE)) : NULL_TREE)

/* Get the value of the top-most type that's `normal' for the vfield.  */
#define VF_NORMAL_VALUE(NODE) TREE_TYPE (NODE)

/* Nonzero for TREE_LIST node means that this list of things
   is a list of parameters, as opposed to a list of expressions.  */
#define TREE_PARMLIST(NODE) ((NODE)->common.unsigned_flag) /* overloaded! */

/* Nonzero for FIELD_DECL node means that this FIELD_DECL is
   a member of an anonymous union construct.  The name of the
   union is .  */
#define TREE_ANON_UNION_ELEM(NODE) ((NODE)->decl.regdecl_flag) /* overloaded! */

/* For FUNCTION_TYPE or METHOD_TYPE, a list of the exceptions that
   this type can raise.  */
#define TYPE_RAISES_EXCEPTIONS(NODE) TYPE_NONCOPIED_PARTS (NODE)

struct lang_decl_flags
{
#ifdef ONLY_INT_FIELDS
  int language : 8;
#else
  enum languages language : 8;
#endif

  unsigned operator_attr : 1;
  unsigned constructor_attr : 1;
  unsigned returns_first_arg : 1;
  unsigned preserves_first_arg : 1;
  unsigned friend_attr : 1;
  unsigned static_function : 1;
  unsigned const_memfunc : 1;
  unsigned volatile_memfunc : 1;

  unsigned abstract_virtual : 1;
  unsigned permanent_attr : 1 ;
  unsigned constructor_for_vbase_attr : 1;
  unsigned dummy : 13;

  tree visibility;
  tree context;
};

struct lang_decl
{
  struct lang_decl_flags decl_flags;

  struct template_info *template_info;
  tree main_decl_variant;
  struct pending_inline *pending_inline_info;
  tree vbase_init_list;
  tree chain;

#ifdef SOS
  tree dynamic_index;
#endif
};

/* Non-zero if NODE is a _DECL with TREE_READONLY set.  */
#define TREE_READONLY_DECL_P(NODE) \
  (TREE_READONLY (NODE) && TREE_CODE_CLASS (TREE_CODE (NODE)) == 'd')

/* For FUNCTION_DECLs: return the language in which this decl
   was declared.  */
#define DECL_LANGUAGE(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.language)

/* For FUNCTION_DECLs: nonzero means that this function is a constructor.  */
#define DECL_CONSTRUCTOR_P(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.constructor_attr)
/* For FUNCTION_DECLs: nonzero means that this function is a constructor
   for an object with virtual baseclasses.  */
#define DECL_CONSTRUCTOR_FOR_VBASE_P(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.constructor_for_vbase_attr)

/* For FUNCTION_DECLs: nonzero means that the constructor
   is known to return a non-zero `this' unchanged.  */
#define DECL_RETURNS_FIRST_ARG(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.returns_first_arg)

/* Nonzero for FUNCTION_DECL means that this constructor is known to
   not make any assignment to `this', and therefore can be trusted
   to return it unchanged.  Otherwise, we must re-assign `current_class_decl'
   after performing base initializations.  */
#define DECL_PRESERVES_THIS(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.preserves_first_arg)

/* Nonzero for _DECL means that this decl appears in (or will appear
   in) as a member in a RECORD_TYPE or UNION_TYPE node.  It is also for
   detecting circularity in case members are multiply defined.  In the
   case of a VAR_DECL, it is also used to determine how program storage
   should be allocated.  */
#define DECL_IN_AGGR_P(NODE) (DECL_LANG_FLAG_3(NODE))

/* Nonzero for FUNCTION_DECL means that this decl is just a
   friend declaration, and should not be added to the list of
   member functions for this class.  */
#define DECL_FRIEND_P(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.friend_attr)

/* Nonzero for FUNCTION_DECL means that this decl is a static
   member function.  */
#define DECL_STATIC_FUNCTION_P(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.static_function)

/* Nonzero for FUNCTION_DECL means that this member function
   has `this' as const X *const.  */
#define DECL_CONST_MEMFUNC_P(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.const_memfunc)

/* Nonzero for FUNCTION_DECL means that this member function
   has `this' as volatile X *const.  */
#define DECL_VOLATILE_MEMFUNC_P(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.volatile_memfunc)

/* Nonzero for FUNCTION_DECL means that this member function
   exists only as part of an abstract class's interface.  */
#define DECL_ABSTRACT_VIRTUAL_P(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.abstract_virtual)

/* Nonzero if allocated on permanent_obstack.  */
#define LANG_DECL_PERMANENT(LANGDECL) ((LANGDECL)->decl_flags.permanent_attr)

/* The _TYPE context in which this _DECL appears.  This field is used
   only to compute visibility information.  */
#define DECL_CLASS_CONTEXT(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.context)

/* For a FUNCTION_DECL: the chain through which the next method
   in the method chain is found.  We now use TREE_CHAIN to
   link into the FIELD_DECL chain.  */
#if 1
#define DECL_CHAIN(NODE) (DECL_LANG_SPECIFIC(NODE)->chain)
#else
#define DECL_CHAIN(NODE) (TREE_CHAIN (NODE))
#endif

/* Points back to the decl which caused this lang_decl to be allocated.  */
#define DECL_MAIN_VARIANT(NODE) (DECL_LANG_SPECIFIC(NODE)->main_decl_variant)

/* For a FUNCTION_DECL: if this function was declared inline inside of
   a class declaration, this is where the text for the function is
   squirreled away.  */
#define DECL_PENDING_INLINE_INFO(NODE) (DECL_LANG_SPECIFIC(NODE)->pending_inline_info)

/* Holds information about how virtual base classes should be initialized
   by this constructor *if* this constructor is the one to perform
   such initialization.  */
#define DECL_VBASE_INIT_LIST(NODE) (DECL_LANG_SPECIFIC(NODE)->vbase_init_list)

/* For a TEMPLATE_DECL: template-specific information.  */
#define DECL_TEMPLATE_INFO(NODE) (DECL_LANG_SPECIFIC(NODE)->template_info)

/* Nonzero in INT_CST means that this int is negative by dint of
   using a twos-complement negated operand.  */
#define TREE_NEGATED_INT(NODE) (TREE_LANG_FLAG_0 (NODE))

/* Nonzero in any kind of _EXPR or _REF node means that it is a call
   to a storage allocation routine.  If, later, alternate storage
   is found to hold the object, this call can be ignored.  */
#define TREE_CALLS_NEW(NODE) (TREE_LANG_FLAG_1 (NODE))

/* Nonzero in any kind of _TYPE that uses multiple inheritance
   or virtual baseclasses.  */
#define TYPE_USES_COMPLEX_INHERITANCE(NODE) (TREE_LANG_FLAG_1 (NODE))

/* Nonzero in IDENTIFIER_NODE means that this name is overloaded, and
   should be looked up in a non-standard way.  */
#define TREE_OVERLOADED(NODE) (TREE_LANG_FLAG_0 (NODE))
#define DECL_OVERLOADED(NODE) (DECL_LANG_FLAG_4 (NODE))

/* Nonzero if this (non-TYPE)_DECL has its virtual attribute set.
   For a FUNCTION_DECL, this is when the function is a virtual function.
   For a VAR_DECL, this is when the variable is a virtual function table.
   For a FIELD_DECL, when the field is the field for the virtual function table.
   For an IDENTIFIER_NODE, nonzero if any function with this name
   has been declared virtual.

   For a _TYPE if it uses virtual functions (or is derived from
   one that does).  */
#define TYPE_VIRTUAL_P(NODE) (TREE_LANG_FLAG_2 (NODE))

/* Same, but tells if this field is private in current context.  */
#define DECL_PRIVATE(NODE) (DECL_LANG_FLAG_5 (NODE))

/* Same, but tells if this field is private in current context.  */
#define DECL_PROTECTED(NODE) (DECL_LANG_FLAG_6(NODE))

#define DECL_PUBLIC(NODE) (DECL_LANG_FLAG_7(NODE))

/* Record whether a typedef for type `int' was actually `signed int'.  */
#define C_TYPEDEF_EXPLICITLY_SIGNED(exp) DECL_LANG_FLAG_1 ((exp))

/* Nonzero if the type T promotes to itself.
   ANSI C states explicitly the list of types that promote;
   in particular, short promotes to int even if they have the same width.  */
#define C_PROMOTING_INTEGER_TYPE_P(t)				\
  (TREE_CODE ((t)) == INTEGER_TYPE				\
   && (TYPE_MAIN_VARIANT (t) == char_type_node			\
       || TYPE_MAIN_VARIANT (t) == signed_char_type_node	\
       || TYPE_MAIN_VARIANT (t) == unsigned_char_type_node	\
       || TYPE_MAIN_VARIANT (t) == short_integer_type_node	\
       || TYPE_MAIN_VARIANT (t) == short_unsigned_type_node))

/* Mark which labels are explicitly declared.
   These may be shadowed, and may be referenced from nested functions.  */
#define C_DECLARED_LABEL_FLAG(label) TREE_LANG_FLAG_1 (label)

/* Record whether a type or decl was written with nonconstant size.
   Note that TYPE_SIZE may have simplified to a constant.  */
#define C_TYPE_VARIABLE_SIZE(type) TREE_LANG_FLAG_4 (type)
#define C_DECL_VARIABLE_SIZE(type) DECL_LANG_FLAG_8 (type)

/* Nonzero for _TYPE means that the _TYPE defines
   at least one constructor.  */
#define TYPE_HAS_CONSTRUCTOR(NODE) (TYPE_LANG_FLAG_1(NODE))

/* When appearing in an INDIRECT_REF, it means that the tree structure
   underneath is actually a call to a constructor.  This is needed
   when the constructor must initialize local storage (which can
   be automatically destroyed), rather than allowing it to allocate
   space from the heap.

   When appearing in a SAVE_EXPR, it means that underneath
   is a call to a constructor.

   When appearing in a CONSTRUCTOR, it means that it was
   a GNU C constructor expression.

   When appearing in a FIELD_DECL, it means that this field
   has been duly initialized in its constructor.  */
#define TREE_HAS_CONSTRUCTOR(NODE) (TREE_LANG_FLAG_4(NODE))

/* Indicates that a NON_LVALUE_EXPR came from a C++ reference.
   Used to generate more helpful error message in case somebody
   tries to take its address.  */
#define TREE_REFERENCE_EXPR(NODE) (TREE_LANG_FLAG_3(NODE))

/* Nonzero for _TYPE means that the _TYPE defines a destructor.  */
#define TYPE_HAS_DESTRUCTOR(NODE) (TYPE_LANG_FLAG_2(NODE))

/* Nonzero for _TYPE node means that creating an object of this type
   will involve a call to a constructor.  This can apply to objects
   of ARRAY_TYPE if the type of the elements needs a constructor.  */
#define TYPE_NEEDS_CONSTRUCTING(NODE) (TYPE_LANG_FLAG_3(NODE))
#define TYPE_NEEDS_CONSTRUCTOR(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.needs_constructor)

/* Nonzero for _TYPE node means that destroying an object of this type
   will involve a call to a destructor.  This can apply to objects
   of ARRAY_TYPE is the type of the elements needs a destructor.  */
#define TYPE_NEEDS_DESTRUCTOR(NODE) (TYPE_LANG_FLAG_4(NODE))

/* Nonzero for VAR_DECL node means that `external' was specified in
   its declaration.  */
#define DECL_THIS_EXTERN(NODE) (DECL_LANG_FLAG_2(NODE))

/* Nonzero for SAVE_EXPR if used to initialize a PARM_DECL.  */
#define PARM_DECL_EXPR(NODE) (TREE_LANG_FLAG_2(NODE))

/* Nonzero in FUNCTION_DECL means it is really an operator.
   Just used to communicate formatting information to dbxout.c.  */
#define DECL_OPERATOR(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.operator_attr)

/* Define fields and accessors for nodes representing declared names.  */

/* C++: A derived class may be able to directly use the virtual
   function table of a base class.  When it does so, it may
   still have a decl node used to access the virtual function
   table (so that variables of this type can initialize their
   virtual function table pointers by name).  When such thievery
   is committed, know exactly which base class's virtual function
   table is the one being stolen.  This effectively computes the
   transitive closure.  */
#define DECL_VPARENT(NODE) ((NODE)->decl.arguments)

/* Make a slot so we can implement nested types.  This slot holds
   the IDENTIFIER_NODE that uniquely names the nested type.  This
   is for TYPE_DECLs only.  */
#if !PARANOID
#define DECL_NESTED_TYPENAME(NODE) ((NODE)->decl.arguments)
#else
#define DECL_NESTED_TYPENAME(NODE) (*DECL_NESTED_TYPENAME_PTR(NODE))
#ifdef __GNUC__
__inline
#endif
static tree * DECL_NESTED_TYPENAME_PTR(NODE) tree NODE; { return
  (my_friendly_assert (TREE_CODE_CLASS (TREE_CODE (NODE)) == 'd', 346),
   &(NODE)->decl.arguments) ;}
#endif

/* C++: all of these are overloaded!  These apply only to TYPE_DECLs.  */
#define DECL_FRIENDLIST(NODE) (DECL_INITIAL (NODE))
#define DECL_UNDEFINED_FRIENDS(NODE) ((NODE)->decl.result)
#define DECL_WAITING_FRIENDS(NODE) ((tree)(NODE)->decl.rtl)
#define SET_DECL_WAITING_FRIENDS(NODE,VALUE) ((NODE)->decl.rtl=(struct rtx_def*)VALUE)

/* The DECL_VISIBILITY is used to record under which context
   special visibility rules apply.  */
#define DECL_VISIBILITY(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.visibility)

/* C++: all of these are overloaded!
   These apply to PARM_DECLs and VAR_DECLs.  */
#define DECL_REFERENCE_SLOT(NODE) ((tree)(NODE)->decl.arguments)
#define SET_DECL_REFERENCE_SLOT(NODE,VAL) ((NODE)->decl.arguments=VAL)

/* For local VAR_DECLs, holds index into gc-protected obstack.  */
#define DECL_GC_OFFSET(NODE) ((NODE)->decl.result)

/* Accessor macros for C++ template decl nodes.  */
#define DECL_TEMPLATE_IS_CLASS(NODE)    (DECL_RESULT(NODE) == NULL_TREE)
#define DECL_TEMPLATE_PARMS(NODE)       DECL_ARGUMENTS(NODE)
/* For class templates.  */
#define DECL_TEMPLATE_MEMBERS(NODE)     DECL_INITIAL(NODE)
/* For function, method, class-data templates.  */
#define DECL_TEMPLATE_RESULT(NODE)      DECL_RESULT(NODE)
#define DECL_TEMPLATE_INSTANTIATIONS(NODE) DECL_VINDEX(NODE)

/* ...and for unexpanded-parameterized-type nodes.  */
#define UPT_TEMPLATE(NODE)      TREE_PURPOSE(TYPE_VALUES(NODE))
#define UPT_PARMS(NODE)         TREE_VALUE(TYPE_VALUES(NODE))

#ifdef SOS
#define DECL_DINDEX(NODE) (DECL_LANG_SPECIFIC(NODE)->dynamic_index)
#endif

/* An enumeration of the kind of tags that C++ accepts.  */
enum tag_types { record_type, class_type, union_type, enum_type, exception_type };

/* Zero means prototype weakly, as in ANSI C (no args means nothing).
   Each language context defines how this variable should be set.  */
extern int strict_prototype;
extern int strict_prototypes_lang_c, strict_prototypes_lang_cplusplus;

/* Non-zero means that if a label exists, and no other identifier
   applies, use the value of the label.  */
extern int flag_labels_ok;

/* Non-zero means to collect statistics which might be expensive
   and to print them when we are done.  */
extern int flag_detailed_statistics;

/* Non-zero means warn in function declared in derived class has the
   same name as a virtual in the base class, but fails to match the
   type signature of any virtual function in the base class.  */
extern int warn_overloaded_virtual;

/* in cp-decl{2}.c */
extern tree void_list_node;
extern tree void_zero_node;
extern tree default_function_type;
extern tree define_function ();
extern tree build_member_type ();
extern tree build_push_scope ();
extern void finish_builtin_type ();
extern tree vtable_entry_type;
extern tree __t_desc_type_node, __i_desc_type_node, __m_desc_type_node;
extern tree class_star_type_node;
extern tree build_vtable_entry ();
extern tree build_vfn_ref ();
extern tree finish_table ();

extern tree typedecl_for_tag ();
extern tree identifier_class_value ();
extern tree constructor_name ();

extern int complete_array_type ();
extern tree coerce_new_type (), coerce_delete_type ();

/* A node that is a list (length 1) of error_mark_nodes.  */
extern tree error_mark_list;

extern tree ptr_type_node;
extern tree class_type_node, record_type_node, union_type_node, enum_type_node;
extern tree exception_type_node, unknown_type_node;

extern tree get_temp_name (), get_temp_aggr (), get_temp_regvar ();
extern tree cleanup_after_call ();
extern tree build_type_conversion ();
extern tree convert_force ();
extern tree maybe_convert_decl_to_const ();
extern char *lang_printable_name ();
extern char *fndecl_as_string ();
extern char *build_overload_name ();

/* The largest size a virtual function table can be.
   Must be a (power of 2).  */
#ifndef VINDEX_MAX
#define VINDEX_MAX ((unsigned)128)
/* This is the integer ~ (vindex_max - 1).  */
#endif
extern tree vtbl_mask;

/* Array type `(void *)[]' */
extern tree vtbl_type_node;

extern tree get_parm_types ();
extern tree grokopexpr (), getaggrs (), groktypefield ();
extern tree grok_method_quals (), grok_enum_decls ();
extern void finish_anon_union();
extern tree long_long_integer_type_node, long_long_unsigned_type_node;
/* For building calls to `delete'.  */
extern tree integer_two_node, integer_three_node;
extern tree get_first_matching_virtual (), get_abstract_virtuals ();

/* in cp-typeck.c */
extern tree build_x_conditional_expr ();
extern tree merge_component_comparisons ();
extern tree build_x_unary_op (), build_x_binary_op ();
extern tree build_component_addr ();
extern tree build_x_function_call ();
extern tree require_complete_type ();
extern tree build_x_indirect_ref (), build_x_array_ref ();
extern tree build_x_modify_expr (), build_x_modify_op_expr ();

extern tree build_m_component_ref ();
extern tree build_component_type_expr ();
extern tree build_x_arrow ();
extern tree build_component_ref_1 ();
extern tree datatype (), unary_complex_lvalue (), target_type ();
extern tree build_return_stmt ();
extern tree convert_arguments (), commonparms ();
extern tree cplus_size_in_bytes ();
extern tree cplus_sizeof (), cplus_sizeof_nowarn ();
extern tree error_not_base_type ();

/* in cp-type2.c */
extern tree binfo_or_else ();
extern void my_friendly_abort ();
extern void error_with_aggr_type ();

/* in tree.c */
extern tree build_let ();
extern tree decl_type_context ();

/* in cp-tree.c */
extern tree build1 ();
extern tree build_cplus_new ();
extern tree build_cplus_array_type ();
extern tree build_cplus_method_type ();
extern tree build_classtype_variant ();
extern tree hash_tree_cons (), hash_tree_chain (), hash_chainon ();
extern tree list_hash_lookup_or_cons ();
extern tree layout_basetypes ();
extern tree copy_to_permanent ();
extern tree get_decl_list ();
extern tree break_out_cleanups ();
extern tree break_out_calls ();
extern tree array_type_nelts_total ();
extern tree array_type_nelts_top ();

/* in cp-except.c */
extern tree current_exception_type;
extern tree current_exception_decl;
extern tree current_exception_object;
extern tree build_exception_variant ();
extern tree lookup_exception_type (), lookup_exception_cname ();
extern tree lookup_exception_object ();
extern tree cplus_expand_start_catch ();
extern tree cplus_expand_end_try ();
extern void finish_exception_decl ();

/* in cp-class.c */
extern tree current_class_name;
extern tree current_class_type;

extern tree current_lang_name, lang_name_cplusplus, lang_name_c;

extern tree convert_pointer_to (), convert_pointer_to_vbase ();
extern tree convert_to_reference (), convert_to_aggr (), convert_aggr ();
extern tree build_x_new (), build_x_delete ();
extern tree build_new (), build_vec_new (), build_delete (), build_vec_delete ();
extern tree make_destructor_name ();
extern tree build_scoped_ref (), build_vfield_ref ();
extern tree build_method_call (), build_overload_call ();
extern tree build_type_pathname ();
extern tree start_method ();
extern tree finish_method ();

extern tree lookup_field (), lookup_nested_field (), lookup_fnfields ();

void pushclass (), popclass (), pushclasstype ();
extern tree build_operator_fnname (), build_opfncall (), build_type_conversion ();

/* Points to the name of that function. May not be the DECL_NAME
   of CURRENT_FUNCTION_DECL due to overloading */
extern tree original_function_name;

# define IS_AGGR_TYPE(t) (TYPE_LANG_FLAG_5 (t))

# define IS_AGGR_TYPE_CODE(t) \
  (t == RECORD_TYPE || t == UNION_TYPE)

extern tree build_decl_overload (), build_typename_overload ();
extern tree build_destructor_call ();
extern tree resolve_scope_to_name ();
extern tree build_scoped_method_call ();
extern tree current_class_name, current_class_type, current_class_decl, C_C_D;
extern tree current_vtable_decl;

/* in cp-init.c  */
extern tree resolve_offset_ref ();
extern void check_base_init ();
extern void do_member_init ();
extern tree global_base_init_list;
extern tree current_base_init_list, current_member_init_list;
#ifdef SOS
extern tree get_linktable_name (), get_dtable_name (), get_sos_dtable ();
#endif
extern tree get_member_function ();
extern tree build_member_call (), build_offset_ref ();
extern tree build_virtual_init ();

extern int current_function_assigns_this;
extern int current_function_just_assigned_this;
extern int current_function_parms_stored;

/* Here's where we control how name mangling takes place.  */

#define OPERATOR_ASSIGN_FORMAT "__a%s"
#define OPERATOR_FORMAT "__%s"
#define OPERATOR_TYPENAME_FORMAT "__op"

/* Cannot use '$' up front, because this confuses gdb
   (names beginning with '$' are gdb-local identifiers).

   Note that all forms in which the '$' is significant are long enough
   for direct indexing (meaning that if we know there is a '$'
   at a particular location, we can index into the string at
   any other location that provides distinguishing characters).  */

/* Define NO_DOLLAR_IN_LABEL in your favorite tm file if your assembler
   doesn't allow '$' in symbol names.  */
#ifndef NO_DOLLAR_IN_LABEL

#define JOINER '$'

#define VPTR_NAME "$v"
#define THROW_NAME "$eh_throw"
#define DESTRUCTOR_DECL_PREFIX "_$_"
#define IN_CHARGE_NAME "__in$chrg"
#define AUTO_VTABLE_NAME "__vtbl$me__"
#define AUTO_TEMP_NAME "_$tmp_"
#define AUTO_TEMP_FORMAT "_$tmp_%d"
#define VTBL_PTR_TYPE "$vtbl_ptr_type"
#define VTABLE_BASE "$vb"
#define VTABLE_NAME_FORMAT "_vt$%s"
#define VFIELD_BASE "$vf"
#define VFIELD_NAME "_vptr$"
#define VFIELD_NAME_FORMAT "_vptr$%s"
#define VBASE_NAME "_vb$"
#define VBASE_NAME_FORMAT "_vb$%s"
#define STATIC_NAME_FORMAT "_%s$%s"
#define FILE_FUNCTION_FORMAT "_GLOBAL_$D$%s"
#define ANON_AGGRNAME_FORMAT "$_%d"

#else	/* NO_DOLLAR_IN_LABEL */

#define JOINER '.'

#define VPTR_NAME ".v"
#define THROW_NAME ".eh_throw"
#define DESTRUCTOR_DECL_PREFIX "_._"
#define IN_CHARGE_NAME "__in.chrg"
#define AUTO_VTABLE_NAME "__vtbl.me__"
#define AUTO_TEMP_NAME "_.tmp_"
#define AUTO_TEMP_FORMAT "_.tmp_%d"
#define VTBL_PTR_TYPE ".vtbl_ptr_type"
#define VTABLE_BASE ".vb"
#define VTABLE_NAME_FORMAT "_vt.%s"
#define VFIELD_BASE ".vf"
#define VFIELD_NAME "_vptr."
#define VFIELD_NAME_FORMAT "_vptr.%s"
#define VBASE_NAME "_vb."
#define VBASE_NAME_FORMAT "_vb.%s"
#define STATIC_NAME_FORMAT "_%s.%s"
#define FILE_FUNCTION_FORMAT "_GLOBAL_.D.%s"

#define ANON_AGGRNAME_FORMAT "._%d"

#endif	/* NO_DOLLAR_IN_LABEL */

#define THIS_NAME "this"
#define DESTRUCTOR_NAME_FORMAT "~%s"
#define FILE_FUNCTION_PREFIX_LEN 9
#define VTABLE_DELTA_NAME "delta"
#define VTABLE_DELTA2_NAME "delta2"
#define VTABLE_INDEX_NAME "index"
#define VTABLE_PFN_NAME "pfn"
#define EXCEPTION_CLEANUP_NAME "exception cleanup"

#define THIS_NAME_P(ID_NODE) (strcmp(IDENTIFIER_POINTER (ID_NODE), "this") == 0)
#define VPTR_NAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[0] == JOINER \
			      && IDENTIFIER_POINTER (ID_NODE)[1] == 'v')
#define DESTRUCTOR_NAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[1] == JOINER)

#define VTABLE_NAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[3] == JOINER \
  && IDENTIFIER_POINTER (ID_NODE)[2] == 't'\
  && IDENTIFIER_POINTER (ID_NODE)[1] == 'v')

#define VBASE_NAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[3] == JOINER \
  && IDENTIFIER_POINTER (ID_NODE)[2] == 'b'\
  && IDENTIFIER_POINTER (ID_NODE)[1] == 'v')

#define OPERATOR_TYPENAME_P(ID_NODE) \
  (IDENTIFIER_POINTER (ID_NODE)[0] == '_'	\
   && IDENTIFIER_POINTER (ID_NODE)[1] == '_'	\
   && IDENTIFIER_POINTER (ID_NODE)[2] == 'o'	\
   && IDENTIFIER_POINTER (ID_NODE)[3] == 'p')

#define TEMP_NAME_P(ID_NODE) (!strncmp (IDENTIFIER_POINTER (ID_NODE), AUTO_TEMP_NAME, sizeof (AUTO_TEMP_NAME)-1))
#define VFIELD_NAME_P(ID_NODE) (!strncmp (IDENTIFIER_POINTER (ID_NODE), VFIELD_NAME, sizeof(VFIELD_NAME)-1))

/* For anonymous aggregate types, we need some sort of name to
   hold on to.  In practice, this should not appear, but it should
   not be harmful if it does.  */
#define ANON_AGGRNAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[0] == JOINER \
				  && IDENTIFIER_POINTER (ID_NODE)[1] == '_')
#define ANON_PARMNAME_FORMAT "_%d"
#define ANON_PARMNAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[0] == '_' \
				  && IDENTIFIER_POINTER (ID_NODE)[1] <= '9')

/* Define the sets of attributes that member functions and baseclasses
   can have.  These are sensible combinations of {public,private,protected}
   cross {virtual,non-virtual}.  */

enum visibility_type {
  visibility_default,
  visibility_public,
  visibility_private,
  visibility_protected,
  visibility_default_virtual,
  visibility_public_virtual,
  visibility_private_virtual
};

enum visibility_type compute_visibility ();

/* in cp-lex.c  */
extern tree current_unit_name, current_unit_language;
extern char *operator_name_string ();

/* Things for handling inline functions.  */

struct pending_inline
{
  struct pending_inline *next;	/* pointer to next in chain */
  int lineno;			/* line number we got the text from */
  char *filename;		/* name of file we were processing */
  tree fndecl;			/* FUNCTION_DECL that brought us here */
  int token;			/* token we were scanning */
  int token_value;		/* value of token we were scanning (YYSTYPE) */

  char *buf;			/* pointer to character stream */
  int len;			/* length of stream */
  tree parm_vec, bindings;	/* in case this is derived from a template */
  unsigned int can_free : 1;	/* free this after we're done with it? */
  unsigned int deja_vu : 1;	/* set iff we don't want to see it again.  */
  unsigned int interface : 2;	/* 0=interface 1=unknown 2=implementation */
};

extern tree combine_strings ();
extern int yylex ();

/* in cp-method.c */
extern struct pending_inline *pending_inlines;
extern char *print_fndecl_with_types ();
extern tree hack_identifier ();
extern tree hack_operator ();

/* 1 for -fall-virtual: make every member function (except
   constructors) lay down in the virtual function table.
   Calls can then either go through the virtual function table or not,
   depending on whether we know what function will actually be called.

   2 for -fSOS: make every member function (including constructors)
   lay down in the virtual function table.  All calls go through the
   virtual function table: this takes the place of using a linker.  */

extern int flag_all_virtual;

/* Positive values means that we cannot make optimizing assumptions about
   `this'.  Negative values means we know `this' to be of static type.  */

extern int flag_this_is_variable;

/* Controls whether enums and ints freely convert.
   1 means with complete freedom.
   0 means enums can convert to ints, but not vice-versa.  */

extern int flag_int_enum_equivalence;

/* Nonzero means layout structures so that we can do garbage collection.  */

extern int flag_gc;

/* Nonzero means generate 'dossiers' that give run-time type information.  */

extern int flag_dossier;

/* Current end of entries in the gc obstack for stack pointer variables.  */

extern int current_function_obstack_index;

/* Flag saying whether we have used the obstack in this function or not.  */

extern int current_function_obstack_usage;

enum overload_flags { NO_SPECIAL = 0, DTOR_FLAG, OP_FLAG, TYPENAME_FLAG };

extern tree default_conversion (), pushdecl (), pushdecl_top_level ();
extern tree push_overloaded_decl ();
extern void push_overloaded_decl_top_level ();
extern tree make_instance_name (), do_decl_overload ();
extern tree maybe_build_cleanup ();
extern tree build_instantiated_decl (), instantiate_type ();
extern tree require_instantiated_type ();
extern tree build_vtbl_ref ();
extern tree make_anon_parm_name ();
extern int resolves_to_fixed_type_p ();

extern tree do_friend ();
extern void grokclassfn ();

extern tree current_class_decl, C_C_D;	/* PARM_DECL: the class instance variable */

/* The following two can be derived from the previous one */
extern tree current_class_name;	/* IDENTIFIER_NODE: name of current class */
extern tree current_class_type;	/* _TYPE: the type of the current class */

/* Some macros for char-based bitfields.  */
#define B_SET(a,x) (a[x>>3] |= (1 << (x&7)))
#define B_CLR(a,x) (a[x>>3] &= ~(1 << (x&7)))
#define B_TST(a,x) (a[x>>3] & (1 << (x&7)))

/* These are uses as bits in flags passed to build_method_call
   to control its error reporting behavior.

   LOOKUP_PROTECT means flag visibility violations.
   LOOKUP_COMPLAIN mean complain if no suitable member function
     matching the arguments is found.
   LOOKUP_NORMAL is just a combination of these two.
   LOOKUP_AGGR requires the instance to be of aggregate type.
   LOOKUP_NONVIRTUAL means make a direct call to the member function found
   LOOKUP_GLOBAL means search through the space of overloaded functions,
     rather than the space of member functions.
   LOOKUP_HAS_IN_CHARGE means that the "in charge" variable is already
     in the parameter list.
   LOOKUP_PROTECTED_OK means that even if the constructor we find appears
     to be non-visible to current scope, call it anyway.
   LOOKUP_DYNAMIC means call dynamic functions, a la SOS.
   LOOKUP_NO_CONVERSION means that user-defined conversions are not
     permitted.  Built-in conversions are permitted.
   LOOKUP_DESTRUCTOR means explicit call to destructor.  */

#define LOOKUP_PROTECT (1)
#define LOOKUP_COMPLAIN (2)
#define LOOKUP_NORMAL (3)
#define LOOKUP_AGGR (4)
#define LOOKUP_NONVIRTUAL (8)
#define LOOKUP_GLOBAL (16)
#define LOOKUP_HAS_IN_CHARGE (32)
#define LOOKUP_SPECULATIVELY (64)
#define LOOKUP_PROTECTED_OK (128)
#define LOOKUP_DYNAMIC (256)
#define LOOKUP_NO_CONVERSION (512)
#define LOOKUP_DESTRUCTOR (512)

/* Anatomy of a DECL_FRIENDLIST (which is a TREE_LIST):
   purpose = friend name (IDENTIFIER_NODE);
   value = TREE_LIST of FUNCTION_DECLS;
   chain, type = EMPTY;  */
#define FRIEND_NAME(LIST) (TREE_PURPOSE (LIST))
#define FRIEND_DECLS(LIST) (TREE_VALUE (LIST))

extern tree get_temp_name (), get_temp_aggr (), get_temp_regvar ();
extern tree get_decl_list ();
extern tree build_method_call ();
extern tree build_type_conversion ();
extern tree build_functional_cast ();
extern tree decl_constant_value ();

/* These macros are for accessing the fields of TEMPLATE...PARM nodes.  */
#define TEMPLATE_TYPE_TPARMLIST(NODE) TREE_PURPOSE (TYPE_FIELDS (NODE))
#define TEMPLATE_TYPE_IDX(NODE) TREE_INT_CST_LOW (TREE_VALUE (TYPE_FIELDS (NODE)))
#define TEMPLATE_TYPE_SET_INFO(NODE,P,I) \
  (TYPE_FIELDS (NODE) = build_tree_list (P, build_int_2 (I, 0)))
#define TEMPLATE_CONST_TPARMLIST(NODE) (*(tree*)&TREE_INT_CST_LOW(NODE))
#define TEMPLATE_CONST_IDX(NODE) (TREE_INT_CST_HIGH(NODE))
#define TEMPLATE_CONST_SET_INFO(NODE,P,I) \
  (TEMPLATE_CONST_TPARMLIST (NODE) = saved_parmlist, \
   TEMPLATE_CONST_IDX (NODE) = I)

/* in cp-init.c */
extern tree resolve_offset_ref ();
extern tree build_vbase_delete ();

/* in cp-lex.c  */
extern char *operator_name_string ();
extern void compiler_error_with_decl ();
extern tree build_opid ();
extern tree do_identifier ();
extern tree arbitrate_lookup ();

/* Indexed by TREE_CODE, these tables give C-looking names to
   operators represented by TREE_CODES.  For example,
   opname_tab[(int) MINUS_EXPR] == "-".  */
extern char **opname_tab, **assignop_tab;

extern tree build_lang_decl (), build_lang_field_decl ();
extern tree make_lang_type ();
extern tree cons_up_default_function ();

/* in cp-convert.c  */
extern tree convert_from_reference ();

/* in cp-search.c  */
extern tree init_vbase_pointers ();
extern tree build_vbase_pointer (), build_vbase_path ();
extern tree lookup_fnfield (), next_baselink ();
extern tree build_vbase_vtables_init ();

extern tree get_binfo ();
extern tree get_vbase_types ();
extern tree get_baselinks ();
extern tree make_binfo (), copy_binfo ();
extern tree binfo_value (), virtual_member ();
extern tree virtual_offset ();
extern tree reverse_path ();

/* in cp-gc.c  */
tree protect_value_from_gc ();
tree build_headof ();
tree build_classof ();
tree build_t_desc ();
tree build_i_desc ();
tree build_m_desc ();

/* in cp-template.c  */
/* PARM_VEC is a vector of template parameters, either IDENTIFIER_NODEs or
   PARM_DECLs.  BINDINGS, if non-null, is a vector of bindings for those
   parameters.  */
struct template_info {
  /* Vector of template parameters, either PARM_DECLs or IDENTIFIER_NODEs.  */
  tree parm_vec;
  /* If non-null, a vector of bindings for the template parms.  */
  tree bindings;

  /* Text of template, and length.  */
  char *text;
  int length;
  /* Where it came from.  */
  char *filename;
  int lineno;

  /* What kind of aggregate -- struct, class, or null.  */
  tree aggr;
};

extern tree end_template_parm_list ();
extern tree process_template_parm ();
extern tree lookup_template_class ();
extern tree instantiate_template ();
extern tree instantiate_class_template ();
extern int processing_template_decl, processing_template_defn;

#define PRINT_LANG_DECL
#define PRINT_LANG_TYPE

#define UNKNOWN_TYPE LANG_TYPE

/* in cp-xref.c */
extern void GNU_xref_start_scope ();
extern void GNU_xref_end_scope ();

/* -- end of C++ */
