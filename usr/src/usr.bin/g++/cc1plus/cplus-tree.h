/*-
 * This code is derived from software copyrighted by the Free Software
 * Foundation.
 *
 * Modified 1991 by Donn Seeley at UUNET Technologies, Inc.
 *
 *	@(#)cplus-tree.h	6.4 (Berkeley) 5/8/91
 */

/* Definitions for C++ parsing and type checking.
   Copyright (C) 1987 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@mcc.com)

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */

/* Borrow everything that is C from c-tree.h,
   but do so by copy, not by inclusion, since c-tree.h defines
   lang_identifier.  */

/* Definitions for C parsing and type checking.
   Copyright (C) 1987 Free Software Foundation, Inc.

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

/* Language-dependent contents of an identifier.  */

struct lang_identifier
{
  struct tree_identifier ignore;
  tree global_value, local_value;
  tree class_value;
  struct lang_id2 *x;
};

struct lang_id2
{
  tree label_value, implicit_decl;
  tree error_locus;
};

/* Macros for access to language-specific slots in an identifier.  */

#define IDENTIFIER_GLOBAL_VALUE(NODE)		\
  (((struct lang_identifier *)(NODE))->global_value)
#define IDENTIFIER_CLASS_VALUE(NODE)		\
  (((struct lang_identifier *)(NODE))->class_value)
#define IDENTIFIER_LOCAL_VALUE(NODE)		\
  (((struct lang_identifier *)(NODE))->local_value)
#define IDENTIFIER_AS_LIST(NODE)		\
  ((tree)((struct lang_identifier *)(NODE))->x)
#define SET_IDENTIFIER_AS_LIST(NODE,LIST)	\
  (((struct lang_identifier *)(NODE))->x = (struct lang_id2*)(LIST))

#define IDENTIFIER_LABEL_VALUE(NODE)	\
  (((struct lang_identifier *)(NODE))->x \
   ? ((struct lang_identifier *)(NODE))->x->label_value : 0)
#define SET_IDENTIFIER_LABEL_VALUE(NODE,VALUE)	\
  (((struct lang_identifier *)(NODE))->x == 0 ? ((struct lang_identifier *)(NODE))->x = (struct lang_id2 *)permalloc (sizeof (struct lang_id2)) : 0, \
   ((struct lang_identifier *)(NODE))->x->label_value = (VALUE))
#define IDENTIFIER_IMPLICIT_DECL(NODE)	\
  (((struct lang_identifier *)(NODE))->x \
   ? ((struct lang_identifier *)(NODE))->x->implicit_decl : 0)
#define SET_IDENTIFIER_IMPLICIT_DECL(NODE,VALUE)	\
  (((struct lang_identifier *)(NODE))->x == 0 ? ((struct lang_identifier *)(NODE))->x = (struct lang_id2 *)permalloc (sizeof (struct lang_id2)) : 0, \
   ((struct lang_identifier *)(NODE))->x->implicit_decl = (VALUE))
#define IDENTIFIER_ERROR_LOCUS(NODE)	\
  (((struct lang_identifier *)(NODE))->x \
   ? ((struct lang_identifier *)(NODE))->x->error_locus : 0)
#define SET_IDENTIFIER_ERROR_LOCUS(NODE,VALUE)	\
  (((struct lang_identifier *)(NODE))->x == 0 ? ((struct lang_identifier *)(NODE))->x = (struct lang_id2 *)permalloc (sizeof (struct lang_id2)) : 0, \
   ((struct lang_identifier *)(NODE))->x->error_locus = (VALUE))

/* Nonzero means reject anything that ANSI standard C forbids.  */
extern int pedantic;

/* In a RECORD_TYPE or UNION_TYPE, nonzero if any component is read-only.  */
#define C_TYPE_FIELDS_READONLY(type) TYPE_SEP_UNIT (type)

/* in c-typeck.c */
extern tree build_component_ref(), build_conditional_expr();
extern tree build_x_compound_expr (), build_compound_expr();
extern tree build_unary_op(), build_binary_op(), build_function_call();
extern tree build_binary_op_nodefault ();
extern tree build_indirect_ref(), build_array_ref(), build_c_cast();
extern tree build_modify_expr();
extern tree c_sizeof (), c_alignof ();
extern tree store_init_value ();
extern tree digest_init ();
extern tree c_expand_start_case ();
extern tree default_conversion ();

/* Given two integer or real types, return the type for their sum.
   Given two compatible ANSI C types, returns the merged type.  */

extern tree commontype ();

/* in c-decl.c */
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

extern tree lookup_label(), define_label();

extern tree implicitly_declare(), getdecls(), gettags ();

extern tree start_decl();
extern void finish_decl();

extern tree start_struct(), finish_struct(), xref_tag();
extern tree finish_exception ();
extern tree grokfield(), grokbitfield ();

extern tree start_enum(), finish_enum();
extern tree build_enumerator();

extern tree make_index_type ();

extern tree double_type_node, long_double_type_node, float_type_node;
extern tree char_type_node, unsigned_char_type_node, signed_char_type_node;

extern tree short_integer_type_node, short_unsigned_type_node;
extern tree long_integer_type_node, long_unsigned_type_node;
extern tree long_long_integer_type_node, long_long_unsigned_type_node;
extern tree unsigned_type_node;
extern tree string_type_node, char_array_type_node, int_array_type_node;

extern int current_function_returns_value;
extern int current_function_returns_null;

extern tree ridpointers[];

/* Nonzero means `$' can be in an identifier.  */

extern int dollars_in_ident;

/* Nonzero means allow type mismatches in conditional expressions;
   just make their values `void'.   */

extern int flag_cond_mismatch;

/* Nonzero means don't recognize the keyword `asm'.  */

extern int flag_no_asm;

/* Nonzero means warn about implicit declarations.  */

extern int warn_implicit;

/* Nonzero means warn about function definitions that default the return type
   or that use a null return and have a return-type other than void.  */

extern int warn_return_type;

/* Nonzero means give string constants the type `const char *'
   to get extra warnings from them.  These warnings will be too numerous
   to be useful, except in thoroughly ANSIfied programs.  */

extern int warn_write_strings;

/* Nonzero means warn about sizeof(function) or addition/subtraction
   of function pointers.  */

extern int warn_pointer_arith;

/* Nonzero means warn for all old-style non-prototype function decls.  */

extern int warn_strict_prototypes;

/* Nonzero means warn about pointer casts that can drop a type qualifier
   from the pointer target type.  */

extern int warn_cast_qual;

/* Nonzero means do some things the same way PCC does.  */

extern int flag_traditional;

/* 2 means write out only specific virtual function tables
   and give them (C) public visibility.
   1 means write out virtual function tables and give them
   (C) public visibility.
   0 means write out virtual function tables and give them
   (C) static visibility.
   -1 means declare virtual function tables extern.  */

extern int write_virtuals;

/* Nonzero means that we are in an "interface" section of the compiler.  */
extern int interface_only;

/* Nonzero means we should attempt to elide constructors when possible.  */

extern int flag_elide_constructors;

/* Nonzero means if the type has methods, only output debugging
   information if methods are actually written to the asm file.  */

extern int flag_minimal_debug;

/* Nonzero means recognize and handle exception handling constructs.  */

extern int flag_handle_exceptions;

/* Nonzero means that member functions defined in class scope are
   inline by default.  */

extern int flag_default_inline;

/* Nonzero means recognize and handle exception handling constructs.  */

extern int flag_no_inline;

/* Nonzero means emit cadillac protocol.  */

extern int flag_cadillac;

#ifndef	HAVE_CADILLAC
#define	cadillac_finish_anon_union(decl)
#define	cadillac_finish_decl(decl)
#define	cadillac_finish_enum(enumtype)
#define	cadillac_finish_exception(e)
#define	cadillac_finish_function(fndecl)
#define	cadillac_finish_stmt()
#define	cadillac_finish_struct(t)
#define	cadillac_note_source()
#define	cadillac_pop_class()
#define	cadillac_pop_lang()
#define	cadillac_pop_source()
#define	cadillac_push_class(type)
#define	cadillac_push_lang(name)
#define	cadillac_push_source()
#define	cadillac_start()
#define	cadillac_start_decl(value)
#define	cadillac_start_enum(ref)
#define	cadillac_start_function(decl1)
#define	cadillac_start_struct(ref)
#define	cadillac_switch_source(flag)
#define	init_cadillac()
#endif

/* C++ language-specific tree codes.  */
#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) SYM,
enum cplus_tree_code {
  __DUMMY = LAST_AND_UNUSED_TREE_CODE,
#include "cplus-tree.def"
  LAST_CPLUS_TREE_CODE
};
#undef DEFTREECODE

enum languages { lang_c, lang_cplusplus };

/* Macros to make error reporting functions' lives easier.  */
#define TYPE_NAME_STRING(NODE) (IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (NODE))))
#define TYPE_NAME_LENGTH(NODE) (IDENTIFIER_LENGTH (DECL_NAME (TYPE_NAME (NODE))))

/* Virtual function addresses can be gotten from a virtual function
   table entry using this macro.  */
#define FNADDR_FROM_VTABLE_ENTRY(ENTRY) \
  TREE_VALUE (TREE_CHAIN (TREE_CHAIN (CONSTRUCTOR_ELTS (ENTRY))))

enum conversion_type { ptr_conv, constptr_conv, int_conv, real_conv, last_conversion_type };

/* Statistics show that while the GNU C++ compiler may generate
   thousands of different types during a compilation run, it
   generates relatively few (tens) of classtypes.  Because of this,
   it is not costly to store a generous amount of information
   in classtype nodes.  */
struct lang_type
{
  /* This must fill out to a multiple of 4 bytes.  */
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

      unsigned has_assign_ref : 1;
      unsigned gets_assign_ref : 1;
      unsigned gets_new : 1;
      unsigned gets_delete : 1;
      unsigned has_wrapper_pred : 1;
      unsigned has_method_call_overloaded : 1;
      unsigned has_call_overloaded : 1;
      unsigned has_array_ref_overloaded : 1;

      unsigned any_assigns_this : 1;
      unsigned none_assign_this : 1;
      unsigned marked : 1;
      unsigned marked2 : 1;
      unsigned marked3 : 1;
      unsigned marked4 : 1;
      unsigned marked5 : 1;
      unsigned marked6 : 1;

      unsigned virtual_attr : 1;
      unsigned needs_constructor : 1;
      unsigned declared_class : 1;
      unsigned private_attr : 1;
      unsigned const_needs_init : 1;
      unsigned ref_needs_init : 1;
      unsigned uses_virtual_base_classes : 1;
      unsigned uses_multiple_inheritance : 1;

      unsigned got_semicolon : 1;
      unsigned alters_visibilities : 1;
      unsigned needs_virtual_reinit : 1;
      unsigned asm_written : 1;
      unsigned declared_exception : 1;
      unsigned vtable_needs_writing : 1;
      unsigned local_typedecls : 1;
      unsigned gets_const_init_ref : 1;

      unsigned dynamic : 1;
      unsigned has_default_ctor : 1;
      unsigned gets_const_assign_ref : 1;
      unsigned has_const_assign_ref : 1;
      unsigned interface_only : 1;
      unsigned interface_unknown : 1;
      unsigned dummy3 : 2;
    } type_flags;

  unsigned n_parents : 16;
  int cid;
  int n_ancestors;
  int n_vancestors;
  int vsize;

  union tree_node *method_vec, *baselink_vec;
  union tree_node *offset;
  union tree_node *vfield, *vfields;
  union tree_node *vbases;
  union tree_node *vbase_size;

  union tree_node *tags;
  union tree_node *main_class_variant;
  union tree_node *this_class_variant;
  union tree_node *next_class_variant;
  void *memoized_table_entry;

  unsigned char *via_pub_or_virt;

  union tree_node **types;
  void *search_slot;

  enum machine_mode mode : 8;
  unsigned char size_unit;
  unsigned char align;
  unsigned char sep_unit;

  union tree_node *sep;
  union tree_node *size;

  union tree_node *base_init_list;
  union tree_node *abstract_virtuals;
  union tree_node *as_list;
  union tree_node *as_id_list;
  union tree_node *vtbl_ptr;
  union tree_node *instance_variable;
  union tree_node *friend_classes;

  char *mi_matrix;
  union tree_node *conversions[last_conversion_type];
  union tree_node *wrap_type;

#ifdef SOS
  union tree_node *typename_as_string;
  union tree_node *dynamic_filename;
  union tree_node *dynamic_table;
#endif
};

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

/* Nonzero for TREE_LIST or _CLASSTYPE node means that the path to the
   base class is via a `public' declaration, which preserves public
   fields from the base class as public.
   OVERLOADED.  */
#define TREE_VIA_PUBLIC(NODE) ((NODE)->common.used_attr) /* overloaded! */

/* Nonzero for TREE_LIST node means that the path to the
   base class is via a `protected' declaration, which preserves
   protected fields from the base class as protected.
   OVERLOADED.  */
#define TREE_VIA_PROTECTED(NODE) ((NODE)->common.literal_attr) /* overloaded! */

/* Nonzero for a _CLASSTYPE node which we know to be private.  */
#define TYPE_PRIVATE_P(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.private_attr)

/* Nonzero for a _CLASSTYPE node means that the derivation chain is via
   a `virtual' declaration.  */
#define TREE_VIA_VIRTUAL(NODE) ((NODE)->common.literal_attr) /* overloaded! */

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
   Note that there must be other arguments, or this constructor is flaged
   as being erroneous.  */
#define TYPE_GETS_INIT_AGGR(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.gets_init_aggr)

/* Nonzero means that this _CLASSTYPE node overloads the method call
   operator.  In this case, all method calls go through `operator->()(...).  */
#define TYPE_OVERLOADS_METHOD_CALL_EXPR(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_method_call_overloaded)

#define TYPE_WRAP_TYPE(NODE) (TYPE_LANG_SPECIFIC(NODE)->wrap_type)

#define TYPE_HAS_WRAPPER(NODE) (TYPE_LANG_SPECIFIC(NODE)->wrap_type == TYPE_MAIN_VARIANT (NODE))
#define TYPE_NEEDS_WRAPPER(NODE) (TYPE_LANG_SPECIFIC(NODE)->wrap_type != 0 && TYPE_LANG_SPECIFIC(NODE)->wrap_type != TYPE_MAIN_VARIANT (NODE))
#define TYPE_HAS_WRAPPER_PRED(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_wrapper_pred)

/* Nonzero means that this _CLASSTYPE node overloads operator().  */
#define TYPE_OVERLOADS_CALL_EXPR(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_call_overloaded)

/* Nonzero means that this _CLASSTYPE node overloads operator[].  */
#define TYPE_OVERLOADS_ARRAY_REF(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.has_array_ref_overloaded)

/* Nonzero means that this _CLASSTYPE (or one of its ancestors) uses
   multiple inheritance.  If this is 0 for the root of a type
   hierarchy, then we can use more efficient search techniques.  */
#define TYPE_USES_MULTIPLE_INHERITANCE(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.uses_multiple_inheritance)

/* Nonzero means that this _CLASSTYPE (or one of its ancestors) uses
   virtual base classes.  If this is 0 for the root of a type
   hierarchy, then we can use more efficient search techniques.  */
#define TYPE_USES_VIRTUAL_BASECLASSES(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.uses_virtual_base_classes)

/* List of lists of member functions defined in this class.  */
#define CLASSTYPE_METHOD_VEC(NODE) (TYPE_LANG_SPECIFIC(NODE)->method_vec)

/* Pointer from any member function to the head of the list of
   member functions of the type that member function belongs to.  */
#define CLASSTYPE_BASELINK_VEC(NODE) (TYPE_LANG_SPECIFIC(NODE)->baselink_vec)

/* Nonzero if the Nth baseclass of this class is via `public'.  */
#define CLASSTYPE_VIA_PUBLIC(NODE, N) (TYPE_LANG_SPECIFIC(NODE)->via_pub_or_virt[N]&1)
/* Nonzero if the Nth baseclass of this class is via `virtual'.  */
#define CLASSTYPE_VIA_VIRTUAL(NODE, N) ((TYPE_LANG_SPECIFIC(NODE)->via_pub_or_virt[N]&2)>>1)

/* Accessor macros for the above two constructs.  */
#define CLASSTYPE_VIAS(NODE) (TYPE_LANG_SPECIFIC(NODE)->via_pub_or_virt)
#define SET_CLASSTYPE_VIAS(NODE, N, PUB, VIRT) (TYPE_LANG_SPECIFIC(NODE)->via_pub_or_virt[N] = (PUB|(VIRT<<1)))

/* Mark bits for depth-first and breath-first searches.  */
#define CLASSTYPE_MARKED(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.marked)
#define CLASSTYPE_MARKED2(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.marked2)
#define CLASSTYPE_MARKED3(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.marked3)
#define CLASSTYPE_MARKED4(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.marked4)
#define CLASSTYPE_MARKED5(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.marked5)
#define CLASSTYPE_MARKED6(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.marked6)

/* When building a matrix to determine by a single lookup
   whether one class is derived from another or not,
   this field is the index of the class in the table.  */
#define CLASSTYPE_CID(NODE) (TYPE_LANG_SPECIFIC(TYPE_MAIN_VARIANT (NODE))->cid)

#define CLASSTYPE_TAGS(NODE) (TYPE_LANG_SPECIFIC(NODE)->tags)
#define CLASSTYPE_NAMES(NODE) (TYPE_LANG_SPECIFIC(NODE)->names)

/* When a class becomes a non-leftmost baseclass in a multiple
   inheritance hierarchy, the number of bytes that subobjects
   of this type are offset from the begining of the containing record.
   This is an INTEGER_CST which holds the value of
   DECL_OFFSET (TYPE_NAME (NODE)).

   Note that for virtual base classes, the offset is only meaningful
   for the construction and initialization of virtual baseclass pointers
   and virtual function table entries.  Otherwise, the offset of a
   virtual baseclass is irrelevant, since it is accessed through a
   pointer, and not via a delta.  */
#define CLASSTYPE_OFFSET(NODE) (TYPE_LANG_SPECIFIC (NODE)->offset)

/* The virtual function table pointer field.  */
#define CLASSTYPE_VFIELD(NODE) (TYPE_LANG_SPECIFIC(NODE)->vfield)
/* The number of virtual functions defined for this
   _CLASSTYPE node.  */
#define CLASSTYPE_VSIZE(NODE) (TYPE_LANG_SPECIFIC(NODE)->vsize)
/* The virtual base classes that this type uses.  */
#define CLASSTYPE_VBASECLASSES(NODE) (TYPE_LANG_SPECIFIC(NODE)->vbases)
/* The virtual function pointer fields that this type contains.  */
#define CLASSTYPE_VFIELDS(NODE) (TYPE_LANG_SPECIFIC(NODE)->vfields)

/* Number of baseclasses defined for this type.
   0 means no base classes.  */
#define CLASSTYPE_N_BASECLASSES(NODE) (TYPE_LANG_SPECIFIC(NODE)->n_parents)
/* Vector of base classes for this type.  This vector is
   indexed starting at 1.  */
#define CLASSTYPE_BASECLASSES(NODE) (TYPE_LANG_SPECIFIC(NODE)->types)
/* Accessor macro for the Nth baseclass of type NODE.  */
#define CLASSTYPE_BASECLASS(NODE, N) (TYPE_LANG_SPECIFIC(NODE)->types[N])

/* Memoize the number of super classes (base classes) tha this node
   has.  That way we can know immediately (albeit conservatively how
   large a multiple-inheritance matrix we need to build to find
   derivation information.  */
#define CLASSTYPE_N_SUPERCLASSES(NODE) (TYPE_LANG_SPECIFIC(NODE)->n_ancestors)
#define CLASSTYPE_N_VBASECLASSES(NODE) (TYPE_LANG_SPECIFIC(NODE)->n_vancestors)

/* Used for keeping search-specific information.  Any search routine
   which uses this must define what exactly this slot is used for.  */
#define CLASSTYPE_SEARCH_SLOT(NODE) (TYPE_LANG_SPECIFIC(NODE)->search_slot)

/* The canonical form of this base class.  It also happens to be
   TYPE_MAIN_VARIANT.  */
#define CLASSTYPE_MAIN_VARIANT(NODE) (TYPE_LANG_SPECIFIC(NODE)->main_class_variant)
/* For baseclasses with non-zero valued offsets, a chain of
   such versions of this baseclass.  */
#define CLASSTYPE_NEXT_VARIANT(NODE) (TYPE_LANG_SPECIFIC(NODE)->next_class_variant)

/* Entry for keeping memoization tables for this type to
   hopefully speed up search routines.  Since it is a pointer,
   it can mean almost anything.  */
#define CLASSTYPE_MTABLE_ENTRY(NODE) (TYPE_LANG_SPECIFIC(NODE)->memoized_table_entry)

/* This is the total size of the baseclasses defined for this type.
   Needed because it is desirable to layout such information
   before begining to process the class itself, and we
   don't want to compute it second time when actually laying
   out the type for real.  */
#define CLASSTYPE_SIZE(NODE) (TYPE_LANG_SPECIFIC(NODE)->size)
#define CLASSTYPE_SIZE_UNIT(NODE) (TYPE_LANG_SPECIFIC(NODE)->size_unit)
#define CLASSTYPE_MODE(NODE) (TYPE_LANG_SPECIFIC(NODE)->mode)
#define CLASSTYPE_ALIGN(NODE) (TYPE_LANG_SPECIFIC(NODE)->align)

/* This is the space needed for virtual base classes.  */
#define CLASSTYPE_VBASE_SIZE(NODE) (TYPE_LANG_SPECIFIC(NODE)->vbase_size)

/* A cons list of structure elements which either have constructors
   to be called, or virtual function table pointers which
   need initializing.  Depending on what is being initialized,
   the TREE_PURPOSE and TREE_VALUE fields have different meanings:

   Member initialization: <FIELD_DECL, TYPE>
   Base class construction: <NULL_TREE, BASETYPE>
   Base class initialization: <BASE_INITIALIZAION, THESE_INITIALIZATIONS>
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

/* Nonzero means that a member function has actually been output for
   this type.  */
#define CLASSTYPE_ASM_WRITTEN(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.asm_written)

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
#define CLASSTYPE_AS_ID_LIST(NODE) (TYPE_LANG_SPECIFIC(NODE)->as_id_list)

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
#define CLASSTYPE_CONVERSION(NODE,KIND) (TYPE_LANG_SPECIFIC(NODE)->conversions[KIND])

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

/* Depending on a class's "owner" they may have different properties,
   such as the offset which must be added to `this', and the
   virtual function table with which they are initialized.  This is
   managed by an association list of type TREE_LIST.  This is
   the interfact to the association list.  */

/* The class's virtual function table.  */
#define ASSOC_VTABLE(NODE) TREE_VEC_ELT ((NODE), 2)
#define CLASS_ASSOC_VTABLE(NODE) ASSOC_VTABLE (TYPE_BASETYPES (NODE))

/* The virtual functions in the virtual function table.  */
#define ASSOC_VIRTUALS(NODE) TREE_VEC_ELT ((NODE), 3)
#define CLASS_ASSOC_VIRTUALS(NODE) ASSOC_VIRTUALS (TYPE_BASETYPES (NODE))

/* The class's offset to be added to `this'.  */
#define ASSOC_OFFSET(NODE) TREE_VEC_ELT ((NODE), 1)
#define CLASS_ASSOC_OFFSET(NODE) ASSOC_OFFSET (TYPE_BASETYPES (NODE))

/* The association key.  */
#define ASSOC_VALUE(NODE) TREE_VEC_ELT ((NODE), 0)
/* And its specific value.  */
#define ASSOC_TYPE(NODE) TREE_TYPE (NODE)

#define CLASSTYPE_ASSOC(NODE) (TYPE_BASETYPES (NODE))

/* Nonzero for TREE_LIST node means that this list of things
   is a list of parameters, as opposed to a list of expressions.  */
#define TREE_PARMLIST(NODE) TREE_UNSIGNED (NODE) /* overloaded! */

/* Nonzero for FIELD_DECL node means that this FIELD_DECL is
   a member of an anonymous union construct.  The name of the
   union is .  */
#define TREE_ANON_UNION_ELEM(NODE) TREE_REGDECL (NODE) /* overloaded! */

/* For FUNCTION_TYPE or METHOD_TYPE, a list of the exceptions that
   this type can raise.  */
#define TYPE_RAISES_EXCEPTIONS(NODE) TYPE_NONCOPIED_PARTS (NODE)

struct lang_decl_flags
{
  enum languages language : 8;

  unsigned this_private : 1;
  unsigned this_protected : 1;
  unsigned this_public : 1;
  unsigned operator_attr : 1;
  unsigned overloaded_attr : 1;
  unsigned constructor_attr : 1;
  unsigned returns_first_arg : 1;
  unsigned preserves_first_arg : 1;

  unsigned in_aggr : 1;
  unsigned friend_attr : 1;
  unsigned static_function : 1;
  unsigned const_memfunc : 1;
  unsigned volatile_memfunc : 1;
  unsigned abstract_virtual : 1;
  unsigned compiler_generated : 1;
  unsigned permanent_attr : 1 ;
  unsigned constructor_for_vbase_attr : 1;
  unsigned dummy7 : 7;

  tree visibility;
};

struct lang_decl
{
  struct lang_decl_flags decl_flags;

  tree original_name;
  tree vindex;
  tree vcontext;
  tree main_decl_variant;
  struct pending_inline *pending_inline_info;
  union tree_node *vbase_init_list;

#ifdef SOS
  tree dynamic_index;
#endif
};

/* Non-zero if NODE is a _DECL with TREE_READONLY set.  */
#define TREE_READONLY_DECL_P(NODE) \
  (TREE_READONLY (NODE) && *(tree_code_type[TREE_CODE (NODE)]) == 'd')

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
   case of a VAR_DECL, it is also used to determince how program storage
   should be allocated.  */
#define DECL_IN_AGGR_P(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.in_aggr)

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

/* Nonzero for FUNCTION_DECL means that this member function
   was generated by the compiler.  This helps us give better
   error messages.  */
#define DECL_COMPILER_GENERATED_P(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.compiler_generated)

/* Nonzero if allocated on permanent_obstack.  */
#if 0
/* @@ Not currently used.  */
#define LANG_DECL_PERMANENT(LANGDECL) ((LANGDECL)->decl_flags.permanent_attr)
#endif

/* Nonzero for FIELD_DECL node means that this FIELD_DECL is
   a member of an anonymous union construct.  */
#define DECL_ANON_UNION_ELEM(NODE) TREE_REGDECL (NODE) /* overloaded! */

/* For a FUNCTION_DECL: the name of the function before being overloaded.  */
#define DECL_ORIGINAL_NAME(NODE) (DECL_LANG_SPECIFIC(NODE)->original_name)

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

/* Nonzero in INT_CST means that this int is negative by dint of
   using a twos-complement negated operand.  */
#define TREE_NEGATED_INT(NODE) (TREE_LANG_FLAG_1 (NODE))

/* Nonzero in any kind of _EXPR or _REF node means that it is a call
   to a storage allocation routine.  If, later, alternate storage
   is found to hold the object, this call can be ignored.  */
#define TREE_CALLS_NEW(NODE) (TREE_LANG_FLAG_2 (NODE))

/* Nonzero in IDENTIFIER_NODE means that this name is overloaded, and
   should be looked up in a non-standard way.  */
#define TREE_OVERLOADED(NODE) (TREE_LANG_FLAG_1 (NODE))
#define DECL_OVERLOADED(NODE) (DECL_LANG_SPECIFIC (NODE)->decl_flags.overloaded_attr)

/* Nonzero if this (non-TYPE)_DECL has its virtual attribute set.
   For a FUNCTION_DECL, this is when the function is a virtual function.
   For a VAR_DECL, this is when the variable is a virtual function table.
   For a FIELD_DECL, when the field is the field for the virtual function table.
   For an IDENTIFIER_NODE, nonzero if any function with this name
   has been declared virtual.  */
#define DECL_VIRTUAL_P(NODE) (TREE_LANG_FLAG_2 (NODE))

/* Nonzero for FIELD_DECL means that this field is a
   virtual baseclass field.  Used for printing debugging info.  */
#define DECL_VBASE_P(NODE) ((NODE)->common.external_attr)

/* Nonzero for FIELD_DECLs means that this field is private,
   and can only be accessed within the scope of the class
   which defines it (or its friends).  */
#define TREE_PRIVATE(NODE) (TREE_LANG_FLAG_3 (NODE))
/* Same, but tells if this field is private in current context.  */
#define TREE_FIELD_PRIVATE(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.this_private)

/* Nonzero for FIELD_DECLs means that this field is protected,
   and can only be accessed within the scope of the class
   which defines it, its friends, or if there is a path in
   the type hierarchy from the current class scope to
   the one that defines it. */
#define TREE_PROTECTED(NODE) (TREE_LANG_FLAG_4 (NODE))
/* Same, but tells if this field is private in current context.  */
#define TREE_FIELD_PROTECTED(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.this_protected)

#define TREE_FIELD_PUBLIC(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.this_public)

/* Nonzero for _TYPE means that the _TYPE defines
   at least one constructor.  */
#define TYPE_HAS_CONSTRUCTOR(NODE) (TREE_LANG_FLAG_1(NODE))

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
#define TREE_HAS_CONSTRUCTOR(NODE) (TREE_LANG_FLAG_1(NODE))

/* Nonzero for _TYPE means that the _TYPE defines a destructor.  */
#define TYPE_HAS_DESTRUCTOR(NODE) (TREE_LANG_FLAG_2(NODE))

/* Nonzero for _TYPE node means that creating an object of this type
   will involve a call to a constructor.  This can apply to objects
   of ARRAY_TYPE if the type of the elements needs a constructor.  */
#define TYPE_NEEDS_CONSTRUCTING(NODE) (TREE_LANG_FLAG_3(NODE))
#define TYPE_NEEDS_CONSTRUCTOR(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.needs_constructor)

/* Nonzero for _TYPE node means that destroying an object of this type
   will involve a call to a destructor.  This can apply to objects
   of ARRAY_TYPE is the type of the elements needs a destructor.  */
#define TYPE_NEEDS_DESTRUCTOR(NODE) (TREE_LANG_FLAG_4(NODE))

/* Nonzero for VAR_DECL node means that `external' was specified in
   its declaration.  */
#define DECL_EXTERNAL(NODE) (TREE_LANG_FLAG_1(NODE))

/* Nonzero for SAVE_EXPR if used to initialize a PARM_DECL.  */
#define PARM_DECL_EXPR(NODE) (TREE_LANG_FLAG_3(NODE))

/* Nonzero in FUNCTION_DECL means it is really an operator.
   Just used to communicate formatting information to dbxout.c.  */
#define TREE_OPERATOR(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.operator_attr)

/* Nonzero for _TYPEs means that the argument defines or uses a
   virtual function table for some of its methods.  */
#define TYPE_VIRTUAL_P(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.virtual_attr)

/* May be useful for optimization using strictness analysis.
   TYPE_ANY_ASSIGNS_THIS nonzero means that there is one constructor
   within the hierarchy of TYPE which is known to assign to `this'.

   TYPE_NONE_ASSIGNS_THIS nonzero means that it is known that
   no constructor within the hierarchy of TYPE makes an assignment
   to `this'.

   Both of these can be zero, in which case it just means that we don't
   have sufficient information yet.  They cannot, however, both be nonzero.  */
#define TYPE_ANY_ASSIGNS_THIS(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.any_assigns_this)
#define TYPE_NONE_ASSIGN_THIS(NODE) (TYPE_LANG_SPECIFIC(NODE)->type_flags.none_assign_this)

/* Define fields and accessors for nodes representing declared names.  */

/* C++: A derived class may be able to directly use the virtual
   function table of a base class.  When it does so, it may
   still have a decl node used to access the virtual function
   table (so that variables of this type can initialize their
   virtual function table pointers by name).  When such thievery
   is commited, know exactly which base class's virtual function
   table is the one being stolen.  This effectively computes the
   transitive closure.  */
#define DECL_VPARENT(NODE) ((NODE)->decl.arguments)

/* C++: all of these are overloaded!  These apply only to TYPE_DECLs.  */
#define DECL_FRIENDLIST(NODE) ((NODE)->decl.voffset)
#define DECL_UNDEFINED_FRIENDS(NODE) ((NODE)->decl.context)
#define DECL_WAITING_FRIENDS(NODE) ((tree)(NODE)->decl.rtl)
#define SET_DECL_WAITING_FRIENDS(NODE,VALUE) ((NODE)->decl.rtl=(struct rtx_def*)VALUE)

/* C++: all of these are overloaded! These apply only to FIELD_DECLs.  */
#define DECL_STATIC_NAME(NODE) ((tree)(NODE)->decl.offset)
#define SET_DECL_STATIC_NAME(NODE,VAL) ((NODE)->decl.offset = (int)VAL)

/* The DECL_VISIBILITY is used to record under which context
   special visibility rules apply.  */
#define DECL_VISIBILITY(NODE) (DECL_LANG_SPECIFIC(NODE)->decl_flags.visibility)

/* The DECL_VINDEX is used for FUNCTION_DECLS in two different ways.
   Before the struct containing the FUNCTION_DECL is laid out,
   DECL_VINDEX may point to a FUNCTION_DECL in a base class which
   is the FUNCTION_DECL which this FUNCTION_DECL will replace as a virtual
   function.  When the class is laid out, this pointer is changed
   to an INT_CST node which is suitable for use as an index
   into the virtual function table.  */
#define DECL_VINDEX(NODE) (DECL_LANG_SPECIFIC(NODE)->vindex)
#define SET_DECL_VINDEX(NODE,VAL) (DECL_LANG_SPECIFIC(NODE)->vindex = VAL)

/* DECL_VCONTEXT is the *first* baseclass in which this FUNCTION_DECL
   is defined.  Contrast this with DECL_CONTEXT (or DECL_FIELD_CONTEXT),
   which is the *last* such baseclass.  */
#define DECL_VCONTEXT(NODE) (DECL_LANG_SPECIFIC(NODE)->vcontext)

/* DECL_FCONTEXT is the *first* baseclass in which this FIELD_DECL
   is defined.  This only applies to vfield and vbase decls.  */
#define SET_DECL_FCONTEXT(NODE,VALUE) (DECL_LANG_SPECIFIC(NODE) = (struct lang_decl *)(VALUE))
#define DECL_FCONTEXT(NODE) ((tree)DECL_LANG_SPECIFIC(NODE))

/* For static member functions, which can never be virtual,
   we need to know to what class the method belongs.  This
   is where we stash the information.  */
#define DECL_STATIC_CONTEXT(NODE) (DECL_LANG_SPECIFIC(NODE)->vcontext)

/* C++: all of these are overloaded!
   These apply to PARM_DECLs and VAR_DECLs and RESULT_DECLs.  */
#define DECL_REFERENCE_SLOT(NODE) ((tree)(NODE)->decl.arguments)
#define SET_DECL_REFERENCE_SLOT(NODE,VAL) ((NODE)->decl.arguments=VAL)

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

/* in cplus-decl{2}.c */
extern tree void_list_node;
extern tree default_function_type;
extern tree define_function ();
extern tree build_member_type ();

extern tree vtable_entry_type;
extern tree build_vtable_entry ();
extern tree build_vfn_ref ();
extern tree finish_table ();

extern tree typedecl_for_tag ();
extern tree identifier_class_value ();

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

/* in cplus-typeck.c */
extern tree build_x_conditional_expr ();
extern tree merge_component_comparisons ();
extern tree build_x_unary_op (), build_x_binary_op ();
extern tree build_component_addr ();
extern tree build_x_function_call ();
extern tree build_x_indirect_ref (), build_x_array_ref ();
extern tree build_x_modify_expr (), build_x_modify_op_expr ();

extern tree build_m_component_ref ();
extern tree build_component_type_expr ();
extern tree build_x_arrow ();
extern tree build_component_ref_1 ();
extern tree datatype (), unary_complex_lvalue (), target_type ();
extern tree build_return_stmt ();
extern tree actualparameterlist (), commonparms ();
extern tree cplus_size_in_bytes ();
extern tree cplus_sizeof (), cplus_sizeof_nowarn ();
extern tree error_mark_list;

/* in cplus-type2.c */
extern tree basetype_or_else ();

/* in tree.c */
extern tree build_let ();
extern tree decl_type_context ();

/* in cplus-tree.c */
extern tree build1 ();
extern tree build_cplus_new ();
extern tree build_cplus_array_type ();
extern tree build_cplus_method_type ();
extern tree build_classtype_variant ();
extern tree hash_tree_cons (), hash_tree_chain ();
extern tree list_hash_lookup_or_cons ();
extern tree layout_basetypes ();
extern tree copy_to_permanent ();

/* in cplus-except.c */
extern tree current_exception_type;
extern tree current_exception_decl;
extern tree current_exception_object;
extern tree build_exception_variant ();
extern tree lookup_exception_type (), lookup_exception_cname ();
extern tree lookup_exception_object ();
extern tree cplus_expand_start_catch ();
extern tree cplus_expand_end_try ();

/* in cplus-class.c */
extern tree current_class_name;
extern tree current_class_type;
extern tree prev_class_type;

extern tree current_lang_name, lang_name_cplusplus, lang_name_c;

extern tree do_identifier (), hack_identifier ();
extern tree hack_operator (), hack_wrapper ();
extern tree convert_pointer_to (), convert_pointer_to_vbase ();
extern tree convert_to_reference (), convert_to_aggr (), convert_aggr ();
extern tree build_x_new (), build_x_delete ();
extern tree build_new (), build_vec_new (), build_delete (), build_vec_delete ();
extern tree make_destructor_name ();
extern tree build_scoped_ref (), build_vfield_ref ();
extern tree build_method_call (), build_overload_call ();
extern tree build_type_pathname ();
extern tree start_method (), start_type_method ();
extern tree finish_method ();

extern tree lookup_field (), lookup_fnfields ();

void pushclass (), popclass (), pushclasstype ();
extern tree build_operator_fnname (), build_opfncall (), build_type_conversion ();
extern tree build_wrapper ();

/* Points to the name of that function. May not be the DECL_NAME
   of CURRENT_FUNCTION_DECL due to overloading */
extern tree original_function_name;

# define IS_AGGR_TYPE(t) \
  (TREE_CODE (t) == RECORD_TYPE || TREE_CODE (t) == UNION_TYPE)

# define IS_AGGR_TYPE_CODE(t) \
  (t == RECORD_TYPE || t == UNION_TYPE)

extern tree build_decl_overload (), build_typename_overload ();
extern tree build_destructor_call ();
extern tree current_class_name, current_class_type, current_class_decl, C_C_D;
extern tree current_vtable_decl;

/* in cplus-init.c  */
extern tree resolve_offset_ref ();
extern tree purpose_member (), value_member ();
extern void check_base_init ();
extern void do_member_init ();
extern tree global_base_init_list;
extern tree current_base_init_list, current_member_init_list;
#ifdef SOS
extern tree get_linktable_name (), get_dtable_name (), get_sos_dtable ();
#endif
extern tree get_member_function ();
extern tree build_member_call (), build_offset_ref ();

extern int current_function_assigns_this;
extern int current_function_just_assigned_this;
extern int current_function_parms_stored;

/* Cannot use '$' up front, because this confuses gdb.
   Note that any format of this kind *must* make the
   format for `this' lexicgraphically less than any other
   parameter name, i.e. "$this" is less than anything else can be.

   Note that all forms in which the '$' is significant are long enough
   for direct indexing.  */

/* Define NO_DOLLAR_IN_LABEL in your favorite tm file if your assembler
   doesn't allow '$' in symbol names.  */
#ifndef NO_DOLLAR_IN_LABEL

#define JOINER '$'

#define THIS_NAME "$t"
#define VPTR_NAME "$v"
#define THROW_NAME "$eh_throw"
#define DESTRUCTOR_DECL_FORMAT "_$_%s"
#define WRAPPER_DECL_FORMAT "__W$%s"
#define WRAPPER_PRED_DECL_FORMAT "__P$%s"
#define ANTI_WRAPPER_DECL_FORMAT "__w$%s"
#define IN_CHARGE_NAME "__in$chrg"
#define AUTO_VTABLE_NAME "__vtbl$me__"
#define AUTO_TEMP_NAME "_$tmp_"
#define AUTO_TEMP_FORMAT "_$tmp_%d"
#define OPERATOR_ASSIGN_FORMAT "op$assign_%s"
#define OPERATOR_MODIFY_FORMAT "op$modify"
#define OPERATOR_METHOD_FORMAT "op$method_call"
#define OPERATOR_NEW_FORMAT "op$new"
#define OPERATOR_DELETE_FORMAT "op$delete"
#define OPERATOR_FORMAT "op$%s"
#define VTBL_PTR_TYPE "$vtbl_ptr_type"
#define VTABLE_NAME_FORMAT "_vt$%s"
#define VFIELD_NAME "_vptr$"
#define VFIELD_NAME_FORMAT "_vptr$%s"
#define VBASE_NAME "_vb$"
#define VBASE_NAME_FORMAT "_vb$%s"
#define STATIC_NAME_FORMAT "_%s$%s"
#define OPERATOR_TYPENAME_FORMAT "type$"
#define FILE_FUNCTION_FORMAT "_GLOBAL_$D$%s"
#define ANON_AGGRNAME_FORMAT "$_%d"

#else	/* NO_DOLLAR_IN_LABEL */

#define JOINER '.'

#define THIS_NAME ".t"
#define VPTR_NAME ".v"
#define THROW_NAME ".eh_throw"
#define DESTRUCTOR_DECL_FORMAT "_._%s"
#define WRAPPER_DECL_FORMAT "__W.%s"
#define WRAPPER_PRED_DECL_FORMAT "__P.%s"
#define ANTI_WRAPPER_DECL_FORMAT "__w.%s"
#define IN_CHARGE_NAME "__in.chrg"
#define AUTO_VTABLE_NAME "__vtbl.me__"
#define AUTO_TEMP_NAME "_.tmp_"
#define AUTO_TEMP_FORMAT "_.tmp_%d"
#define OPERATOR_ASSIGN_FORMAT "op.assign_%s"
#define OPERATOR_MODIFY_FORMAT "op.modify"
#define OPERATOR_METHOD_FORMAT "op.method_call"
#define OPERATOR_NEW_FORMAT "op.new"
#define OPERATOR_DELETE_FORMAT "op.delete"
#define OPERATOR_FORMAT "op.%s"
#define VTBL_PTR_TYPE ".vtbl_ptr_type"
#define VTABLE_NAME_FORMAT "_vt.%s"
#define VFIELD_NAME "_vptr."
#define VFIELD_NAME_FORMAT "_vptr.%s"
#define VBASE_NAME "_vb."
#define VBASE_NAME_FORMAT "_vb.%s"
#define STATIC_NAME_FORMAT "_%s.%s"
#define OPERATOR_TYPENAME_FORMAT "type."
#define FILE_FUNCTION_FORMAT "_GLOBAL_.D.%s"

#define ANON_AGGRNAME_FORMAT "._%d"

#endif	/* NO_DOLLAR_IN_LABEL */

#define DESTRUCTOR_NAME_FORMAT "~%s"
#define WRAPPER_NAME_FORMAT "()%s"
#define WRAPPER_PRED_NAME_FORMAT "()?%s"
#define ANTI_WRAPPER_NAME_FORMAT "~()%s"
#define OPERATOR_MODIFY_LENGTH 8
#define OPERATOR_METHOD_LENGTH 13
#define OPERATOR_NEW_LENGTH 5
#define OPERATOR_DELETE_LENGTH 8
#define EXCEPTION_NAME_LENGTH 12
#define FILE_FUNCTION_PREFIX_LEN 9
#define VTABLE_DELTA_NAME "delta"
#define VTABLE_DELTA2_NAME "delta2"
#define VTABLE_INDEX_NAME "index"
#define VTABLE_PFN_NAME "pfn"
#define EXCEPTION_CLEANUP_NAME "exception cleanup"

#define THIS_NAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[0] == JOINER \
			      && IDENTIFIER_POINTER (ID_NODE)[1] == 't')
#define VPTR_NAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[0] == JOINER \
			      && IDENTIFIER_POINTER (ID_NODE)[1] == 'v')
#define DESTRUCTOR_NAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[1] == JOINER)

#define WRAPPER_NAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[1] == '_' \
				 && IDENTIFIER_POINTER (ID_NODE)[2] == 'W' \
				 && IDENTIFIER_POINTER (ID_NODE)[3] == JOINER)
#define WRAPPER_PRED_NAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[1] == '_' \
				 && IDENTIFIER_POINTER (ID_NODE)[2] == 'P' \
				 && IDENTIFIER_POINTER (ID_NODE)[3] == JOINER)
#define ANTI_WRAPPER_NAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[1] == '_' \
				      && IDENTIFIER_POINTER (ID_NODE)[2] == 'w' \
				      && IDENTIFIER_POINTER (ID_NODE)[3] == JOINER)
#define WRAPPER_OR_ANTI_WRAPPER_NAME_P(ID_NODE) \
  (IDENTIFIER_POINTER (ID_NODE)[1] == '_' \
   && (IDENTIFIER_POINTER (ID_NODE)[2]|('W'^'w')) == 'w' \
   && IDENTIFIER_POINTER (ID_NODE)[3] == JOINER)

#define OPERATOR_NAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[2] == JOINER \
  && IDENTIFIER_POINTER (ID_NODE)[1])

#define VTABLE_NAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[3] == JOINER \
  && IDENTIFIER_POINTER (ID_NODE)[2] == 't'\
  && IDENTIFIER_POINTER (ID_NODE)[1] == 'v')

#define VBASE_NAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[3] == JOINER \
  && IDENTIFIER_POINTER (ID_NODE)[2] == 'b'\
  && IDENTIFIER_POINTER (ID_NODE)[1] == 'v')

#define OPERATOR_TYPENAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[4] == JOINER \
  && IDENTIFIER_POINTER (ID_NODE)[3] \
  && IDENTIFIER_POINTER (ID_NODE)[2] \
  && IDENTIFIER_POINTER (ID_NODE)[1])

#define TEMP_NAME_P(ID_NODE) (!strncmp (IDENTIFIER_POINTER (ID_NODE), AUTO_TEMP_NAME, sizeof (AUTO_TEMP_NAME)-1))
#define VFIELD_NAME_P(ID_NODE) (!strncmp (IDENTIFIER_POINTER (ID_NODE), VFIELD_NAME, sizeof(VFIELD_NAME)-1))

/* For anonymous aggregate types, we need some sort of name to
   hold on to.  In practice, this should not appear, but it should
   not be harmful if it does.  */
#define ANON_AGGRNAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[0] == JOINER)

#define ANON_PARMNAME_FORMAT "_%d"
#define ANON_PARMNAME_P(ID_NODE) (IDENTIFIER_POINTER (ID_NODE)[0] == '_' \
				  && IDENTIFIER_POINTER (ID_NODE)[1] <= '9')

enum visibility_type {
  visibility_default,
  visibility_public,
  visibility_private,
  visibility_protected,
  visibility_default_virtual,
  visibility_public_virtual,
  visibility_private_virtual,
};

enum visibility_type compute_visibility ();

/* in cplus-lex.c  */
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
};

extern tree combine_strings ();
extern int yylex ();

/* in cplus-method.c */
extern tree wrapper_name, wrapper_pred_name, anti_wrapper_name;
extern struct pending_inline *pending_inlines;
extern char *print_fndecl_with_types ();

/* 1 for -fall-virtual: make every member function (except
   constructors) lay down in the virtual function table.
   Calls can then either go through the virtual function table or not,
   depending on whether we know what function will actually be called.

   2 for -fSOS: make every member function (including constructors)
   lay down in the virtual function table.  All calls go through the
   virtual function table: this takes the place of using a linker.  */

extern int flag_all_virtual;

/* Nonzero means that we cannot make optimizing assumptions about `this'.  */

extern int flag_this_is_variable;

enum overload_flags { NO_SPECIAL = 0, DTOR_FLAG, OP_FLAG, TYPENAME_FLAG, WRAPPER_FLAG, WRAPPER_PRED_FLAG, ANTI_WRAPPER_FLAG };

extern tree default_conversion (), pushdecl (), pushdecl_top_level ();
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

/* The following structure is used when comparing various alternatives
   for overloading.  The unsigned quantity `strikes.i' is used
   for fast comparison of two possibilities.  This number is an
   aggregate of four constituents:

     EVIL: if this is non-zero, then the candidate should not be considered
     USER: if this is non-zero, then a user-defined type conversion is needed
     B_OR_D: if this is non-zero, then use a base pointer instead of the
             type of the pointer we started with.
     EASY: if this is non-zero, then we have a builtin conversion
           (such as int to long, int to float, etc) to do.

   If two candidates require user-defined type conversions, and the
   type conversions are not identical, then an ambiguity error
   is reported.

   If two candidates agree on user-defined type conversions,
   and one uses pointers of strictly higher type (derived where
   another uses base), then that alternative is silently chosen.

   If two candidates have a non-monotonic derived/base pointer
   relationship, and/or a non-monotonic easy conversion relationship,
   then a warning is emitted to show which paths are possible, and
   which one is being chosen.

   For example:

   int i;
   double x;

   overload f;
   int f (int, int);
   double f (double, double);

   f (i, x);	// draws a warning

   struct B
   {
     f (int);
   } *bb;
   struct D : B
   {
     f (double);
   } *dd;

   dd->f (x);	// exact match
   dd->f (i);	// draws warning

   Note that this technique really only works for 255 arguments.  Perhaps
   this is not enough.  */

struct candidate
{
  tree function;		/* A FUNCTION_DECL */

  unsigned char evil;		/* ~0 if this will never convert.  */
  unsigned char user;		/* ~0 if at least one user-defined type conv.  */
  unsigned short b_or_d;	/* count number of derived->base conv.  */
  unsigned short easy;		/* count number of builtin type conv.  */
  tree arg;			/* an _EXPR node that is first parm to function */
  unsigned short *harshness;	/* Indexed by argument number, encodes
				   evil, user, b_or_d, and easy strikes for
				   that argument.
				   At end of array, we store the index+1 where we
				   started using default parameters, or 0
				   if there are none.  */
  union
    {
      tree field;		/* If no evil strikes, the FUNCTION_DECL of
				   the function (if a member function).  */
      int bad_arg;		/* the index of the first bad argument:
				   0 if no bad arguements
				   > 0 is first bad argument
				   -1 if extra actual arguments
				   -2 if too few actual arguments.
				   -3 if const/non const method mismatch.  */
    } u;
};
int rank_for_overload ();
struct candidate *ideal_candidate ();
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
     to be non-visibile to current scope, call it anyway.
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
extern tree build_method_call ();
extern tree build_type_conversion ();
extern tree build_functional_cast ();
extern tree build_scoped_method_call ();
extern tree decl_constant_value ();

/* in cplus-init.c */
extern tree resolve_offset_ref ();
extern tree build_with_cleanup ();

/* in cplus-lex.c  */
extern char *operator_name_string ();

extern tree get_base_type ();
extern tree build_opid ();

/* Indexed by TREE_CODE, these tables give C-looking names to
   operators represented by TREE_CODES.  For example,
   opname_tab[(int) MINUS_EXPR] == "-".  */
extern char **opname_tab, **assignop_tab;

extern tree build_lang_decl (), build_lang_field_decl ();
extern tree make_lang_type ();
extern tree cons_up_default_function ();

/* in cplus-convert.c  */
extern tree convert_from_reference ();

/* in cplus-search.c  */
extern tree init_vbase_pointers ();
extern tree build_vbase_pointer (), build_vbase_path ();
extern tree lookup_fnfield (), next_baselink ();

extern tree get_base_type ();
extern tree get_vbase_types ();
extern tree get_baselinks ();
extern tree get_wrapper ();
extern tree make_assoc (), copy_assoc ();
extern tree assoc_value (), virtual_member ();

#define PRINT_LANG_DECL
#define PRINT_LANG_TYPE

#define UNKNOWN_TYPE LANG_TYPE

extern union tree_node ERROR_MARK_NODE;

#define error_mark_node (&ERROR_MARK_NODE)

/* -- end of C++ */
