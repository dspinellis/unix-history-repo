/* Symbol table definitions for GDB.
   Copyright (C) 1986, 1989, 1991, 1992 Free Software Foundation, Inc.

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

#if !defined (SYMTAB_H)
#define SYMTAB_H 1
#include "obstack.h"

/* See the comment in symfile.c about how current_objfile is used. */

extern struct objfile *current_objfile;

/* Some definitions and declarations to go with use of obstacks.  */
#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* Some macros for char-based bitfields.  */
#define B_SET(a,x) ((a)[(x)>>3] |= (1 << ((x)&7)))
#define B_CLR(a,x) ((a)[(x)>>3] &= ~(1 << ((x)&7)))
#define B_TST(a,x) ((a)[(x)>>3] & (1 << ((x)&7)))
#define B_TYPE		unsigned char
#define	B_BYTES(x)	( 1 + ((x)>>3) )
#define	B_CLRALL(a,x) memset ((a), 0, B_BYTES(x))


/* Define a simple structure used to hold some very basic information about
   all defined global symbols (text, data, bss, abs, etc).  The only two
   required pieces of information are the symbol's name and the address
   associated with that symbol.  In many cases, even if a file was compiled
   with no special options for debugging at all, as long as was not stripped
   it will contain sufficient information to build a useful minimal symbol
   table using this structure.  Even when a file contains enough debugging
   information to build a full symbol table, these minimal symbols are still
   useful for quickly mapping between names and addresses, and vice versa.
   They are also sometimes used to figure out what full symbol table entries
   need to be read in. */

struct minimal_symbol
{

  /* Name of the symbol.  This is a required field.  Storage for the name is
     allocated on the symbol_obstack for the associated objfile. */

  char *name;

  /* Address of the symbol.  This is a required field. */

  CORE_ADDR address;

  /* The info field is available for caching machine-specific information that
     The AMD 29000 tdep.c uses it to remember things it has decoded from the
     instructions in the function header, so it doesn't have to rederive the
     info constantly (over a serial line).  It is initialized to zero and
     stays that way until target-dependent code sets it.  Storage for any data
     pointed to by this field should be allocated on the symbol_obstack for
     the associated objfile.  The type would be "void *" except for reasons
     of compatibility with older compilers.  This field is optional. */

  char *info;

  /* Classification types for this symbol.  These should be taken as "advisory
     only", since if gdb can't easily figure out a classification it simply
     selects mst_unknown.  It may also have to guess when it can't figure out
     which is a better match between two types (mst_data versus mst_bss) for
     example.  Since the minimal symbol info is sometimes derived from the
     BFD library's view of a file, we need to live with what information bfd
     supplies. */

  enum minimal_symbol_type
    {
      mst_unknown = 0,		/* Unknown type, the default */
      mst_text,			/* Generally executable instructions */
      mst_data,			/* Generally initialized data */
      mst_bss,			/* Generally uninitialized data */
      mst_abs			/* Generally absolute (nonrelocatable) */
    } type;

};


/* All of the name-scope contours of the program
   are represented by `struct block' objects.
   All of these objects are pointed to by the blockvector.

   Each block represents one name scope.
   Each lexical context has its own block.

   The first two blocks in the blockvector are special.
   The first one contains all the symbols defined in this compilation
   whose scope is the entire program linked together.
   The second one contains all the symbols whose scope is the
   entire compilation excluding other separate compilations.
   In C, these correspond to global symbols and static symbols.

   Each block records a range of core addresses for the code that
   is in the scope of the block.  The first two special blocks
   give, for the range of code, the entire range of code produced
   by the compilation that the symbol segment belongs to.

   The blocks appear in the blockvector
   in order of increasing starting-address,
   and, within that, in order of decreasing ending-address.

   This implies that within the body of one function
   the blocks appear in the order of a depth-first tree walk.  */

struct blockvector
{
  /* Number of blocks in the list.  */
  int nblocks;
  /* The blocks themselves.  */
  struct block *block[1];
};

/* Special block numbers */
#define GLOBAL_BLOCK	0
#define	STATIC_BLOCK	1
#define	FIRST_LOCAL_BLOCK	2

struct block
{
  /* Addresses in the executable code that are in this block.
     Note: in an unrelocated symbol segment in a file,
     these are always zero.  They can be filled in from the
     N_LBRAC and N_RBRAC symbols in the loader symbol table.  */
  CORE_ADDR startaddr, endaddr;
  /* The symbol that names this block,
     if the block is the body of a function;
     otherwise, zero.
     Note: In an unrelocated symbol segment in an object file,
     this field may be zero even when the block has a name.
     That is because the block is output before the name
     (since the name resides in a higher block).
     Since the symbol does point to the block (as its value),
     it is possible to find the block and set its name properly.  */
  struct symbol *function;
  /* The `struct block' for the containing block, or 0 if none.  */
  /* Note that in an unrelocated symbol segment in an object file
     this pointer may be zero when the correct value should be
     the second special block (for symbols whose scope is one compilation).
     This is because the compiler outputs the special blocks at the
     very end, after the other blocks.   */
  struct block *superblock;
  /* A flag indicating whether or not the function corresponding
     to this block was compiled with gcc or not.  If there is no
     function corresponding to this block, this meaning of this flag
     is undefined.  (In practice it will be 1 if the block was created
     while processing a file compiled with gcc and 0 when not). */
  unsigned char gcc_compile_flag;
  /* Number of local symbols.  */
  int nsyms;
  /* The symbols.  */
  struct symbol *sym[1];
};

/* Represent one symbol name; a variable, constant, function or typedef.  */

/* Different name spaces for symbols.  Looking up a symbol specifies
   a namespace and ignores symbol definitions in other name spaces.

   VAR_NAMESPACE is the usual namespace.
   In C, this contains variables, function names, typedef names
   and enum type values.

   STRUCT_NAMESPACE is used in C to hold struct, union and enum type names.
   Thus, if `struct foo' is used in a C program,
   it produces a symbol named `foo' in the STRUCT_NAMESPACE.

   LABEL_NAMESPACE may be used for names of labels (for gotos);
   currently it is not used and labels are not recorded at all.  */

/* For a non-global symbol allocated statically,
   the correct core address cannot be determined by the compiler.
   The compiler puts an index number into the symbol's value field.
   This index number can be matched with the "desc" field of
   an entry in the loader symbol table.  */

enum namespace
{
  UNDEF_NAMESPACE, VAR_NAMESPACE, STRUCT_NAMESPACE, LABEL_NAMESPACE
};

/* An address-class says where to find the value of a symbol.  */

enum address_class
{
  LOC_UNDEF,		/* Not used; catches errors */
  LOC_CONST,		/* Value is constant int SYMBOL_VALUE, host byteorder */
  LOC_STATIC,		/* Value is at fixed address SYMBOL_VALUE_ADDRESS */
  LOC_REGISTER,		/* Value is in register */
  LOC_ARG,		/* Value is at spec'd offset in arglist */
  LOC_REF_ARG,		/* Value address is at spec'd offset in arglist. */
  LOC_REGPARM,		/* Value is at spec'd offset in register window */
  LOC_LOCAL,		/* Value is at spec'd offset in stack frame */
  LOC_TYPEDEF,		/* Value not used; definition in SYMBOL_TYPE
			   Symbols in the namespace STRUCT_NAMESPACE
			   all have this class.  */
  LOC_LABEL,		/* Value is address SYMBOL_VALUE_ADDRESS in the code */
  LOC_BLOCK,		/* Value is address SYMBOL_VALUE_BLOCK of a
			   `struct block'.  Function names have this class. */
  LOC_CONST_BYTES,	/* Value is a constant byte-sequence pointed to by
			   SYMBOL_VALUE_ADDRESS, in target byte order.  */
  LOC_LOCAL_ARG		/* Value is arg at spec'd offset in stack frame.
			   Differs from LOC_LOCAL in that symbol is an
			   argument; differs from LOC_ARG in that we find it
			   in the frame (FRAME_LOCALS_ADDRESS), not in the
			   arglist (FRAME_ARGS_ADDRESS).  Added for i960,
			   which passes args in regs then copies to frame.  */
};

struct symbol
{
  /* Symbol name */
  char *name;
  /* Name space code.  */
  enum namespace namespace;
  /* Address class */
  enum address_class class;
  /* Data type of value */
  struct type *type;

  /* Line number of definition.  */
  unsigned short line;
  
  /* constant value, or address if static, or register number,
     or offset in arguments, or offset in stack frame.  All of
     these are in host byte order (though what they point to might
     be in target byte order, e.g. LOC_CONST_BYTES).  */
  union
    {
      long value;		/* for LOC_CONST, LOC_REGISTER, LOC_ARG, 
				   LOC_REF_ARG, LOC_REGPARM, LOC_LOCAL */
      struct block *block;      /* for LOC_BLOCK */
      char *bytes;		/* for LOC_CONST_BYTES */
      CORE_ADDR address;	/* for LOC_STATIC, LOC_LABEL */
      struct symbol *chain;	/* for opaque typedef struct chain */
    }
  value;

  /* Some symbols require an additional value to be recorded on a per-
     symbol basis.  Stash those values here. */
  union
    {
      struct			/* for OP_BASEREG in DWARF location specs */
	{
	  short regno_valid;	/* 0 == regno invalid; !0 == regno valid */
	  short regno;		/* base register number {0, 1, 2, ...} */
	} basereg;
    }
  aux_value;
};


/* A partial_symbol records the name, namespace, and address class of
   symbols whose types we have not parsed yet.  For functions, it also
   contains their memory address, so we can find them from a PC value.
   Each partial_symbol sits in a partial_symtab, all of which are chained
   on a  partial symtab list and which points to the corresponding 
   normal symtab once the partial_symtab has been referenced.  */

struct partial_symbol
{
  /* Symbol name */
  char *name;
  /* Name space code.  */
  enum namespace namespace;
  /* Address class (for info_symbols) */
  enum address_class class;
  /* Value (only used for static functions currently).  Done this
     way so that we can use the struct symbol macros.
     Note that the address of a function is SYMBOL_VALUE_ADDRESS (pst)
     in a partial symbol table, but BLOCK_START (SYMBOL_BLOCK_VALUE (st))
     in a symbol table.  */
  union
    {
      long value;
      CORE_ADDR address;
    }
  value;
};

/* Source-file information.
   This describes the relation between source files and line numbers
   and addresses in the program text.  */

struct sourcevector
{
  int length;			/* Number of source files described */
  struct source *source[1];	/* Descriptions of the files */
};

/* Each item represents a line-->pc (or the reverse) mapping.  This is
   somewhat more wasteful of space than one might wish, but since only
   the files which are actually debugged are read in to core, we don't
   waste much space.

   Each item used to be an int; either minus a line number, or a
   program counter.  If it represents a line number, that is the line
   described by the next program counter value.  If it is positive, it
   is the program counter at which the code for the next line starts.  */

struct linetable_entry
{
  int line;
  CORE_ADDR pc;
};

struct linetable
{
  int nitems;
  struct linetable_entry item[1];
};

/* All the information on one source file.  */

struct source
{
  char *name;			/* Name of file */
  struct linetable contents;
};

/* How to relocate the symbols from each section in a symbol file.
   Each struct contains an array of offsets.
   The ordering and meaning of the offsets is file-type-dependent;
   typically it is indexed by section numbers or symbol types or
   something like that.

   To give us flexibility in changing the internal representation
   of these offsets, the ANOFFSET macro must be used to insert and
   extract offset values in the struct.  */

struct section_offsets
  {
    CORE_ADDR offsets[1];		/* As many as needed. */
  };

#define	ANOFFSET(secoff, whichone)	(secoff->offsets[whichone])

/* Each source file is represented by a struct symtab. 
   These objects are chained through the `next' field.  */

struct symtab
  {
    /* Chain of all existing symtabs.  */
    struct symtab *next;
    /* List of all symbol scope blocks for this symtab.  */
    struct blockvector *blockvector;
    /* Table mapping core addresses to line numbers for this file.
       Can be NULL if none.  */
    struct linetable *linetable;
    /* Name of this source file.  */
    char *filename;
    /* Directory in which it was compiled, or NULL if we don't know.  */
    char *dirname;
    /* This component says how to free the data we point to:
       free_contents => do a tree walk and free each object.
       free_nothing => do nothing; some other symtab will free
         the data this one uses.
       free_linetable => free just the linetable.  */
    enum free_code {free_nothing, free_contents, free_linetable}
      free_code;
    /* Pointer to one block of storage to be freed, if nonzero.  */
    /* This is IN ADDITION to the action indicated by free_code.  */
    char *free_ptr;
    /* Total number of lines found in source file.  */
    int nlines;
    /* Array mapping line number to character position.  */
    int *line_charpos;
    /* Language of this source file.  */
    enum language language;
    /* String of version information.  May be zero.  */
    char *version;
    /* Full name of file as found by searching the source path.
       0 if not yet known.  */
    char *fullname;

    /* Object file from which this symbol information was read.  */
    struct objfile *objfile;

    /* Anything extra for this symtab.  This is for target machines
       with special debugging info of some sort (which cannot just
       be represented in a normal symtab).  */
#if defined (EXTRA_SYMTAB_INFO)
    EXTRA_SYMTAB_INFO
#endif
  };

/* Each source file that has not been fully read in is represented by
   a partial_symtab.  This contains the information on where in the
   executable the debugging symbols for a specific file are, and a
   list of names of global symbols which are located in this file.
   They are all chained on partial symtab lists.

   Even after the source file has been read into a symtab, the
   partial_symtab remains around.  They are allocated on an obstack,
   psymbol_obstack.  FIXME, this is bad for dynamic linking or VxWorks-
   style execution of a bunch of .o's.  */

struct partial_symtab
{
  /* Chain of all existing partial symtabs.  */
  struct partial_symtab *next;
  /* Name of the source file which this partial_symtab defines */
  char *filename;

  /* Information about the object file from which symbols should be read.  */
  struct objfile *objfile;

  /* Set of relocation offsets to apply to each section.  */ 
  struct section_offsets *section_offsets;

  /* Range of text addresses covered by this file; texthigh is the
     beginning of the next section. */
  CORE_ADDR textlow, texthigh;
  /* Array of pointers to all of the partial_symtab's which this one
     depends on.  Since this array can only be set to previous or
     the current (?) psymtab, this dependency tree is guaranteed not
     to have any loops. */
  struct partial_symtab **dependencies;
  int number_of_dependencies;
  /* Global symbol list.  This list will be sorted after readin to
     improve access.  Binary search will be the usual method of
     finding a symbol within it. globals_offset is an integer offset
     within global_psymbols[].  */
  int globals_offset, n_global_syms;
  /* Static symbol list.  This list will *not* be sorted after readin;
     to find a symbol in it, exhaustive search must be used.  This is
     reasonable because searches through this list will eventually
     lead to either the read in of a files symbols for real (assumed
     to take a *lot* of time; check) or an error (and we don't care
     how long errors take).  This is an offset and size within
     static_psymbols[].  */
  int statics_offset, n_static_syms;
  /* Pointer to symtab eventually allocated for this source file, 0 if
     !readin or if we haven't looked for the symtab after it was readin.  */
  struct symtab *symtab;
  /* Pointer to function which will read in the symtab corresponding to
     this psymtab.  */
  void (*read_symtab) PARAMS ((struct partial_symtab *));
  /* Information that lets read_symtab() locate the part of the symbol table
     that this psymtab corresponds to.  This information is private to the
     format-dependent symbol reading routines.  For further detail examine
     the various symbol reading modules.  Should really be (void *) but is
     (char *) as with other such gdb variables.  (FIXME) */
  char *read_symtab_private;
  /* Non-zero if the symtab corresponding to this psymtab has been
     readin */
  unsigned char readin;
};

/* A fast way to get from a psymtab to its symtab (after the first time).  */
#define	PSYMTAB_TO_SYMTAB(pst)  ((pst)->symtab? 		\
				 (pst)->symtab: 		\
				 psymtab_to_symtab (pst) )

/* This symtab variable specifies the current file for printing source lines */

extern struct symtab *current_source_symtab;

/* This is the next line to print for listing source lines.  */

extern int current_source_line;

#define BLOCKVECTOR(symtab) (symtab)->blockvector

#define LINETABLE(symtab) (symtab)->linetable

/* Macros normally used to access components of symbol table structures.  */

#define BLOCKVECTOR_NBLOCKS(blocklist) (blocklist)->nblocks
#define BLOCKVECTOR_BLOCK(blocklist,n) (blocklist)->block[n]

#define BLOCK_START(bl) (bl)->startaddr
#define BLOCK_END(bl) (bl)->endaddr
#define BLOCK_NSYMS(bl) (bl)->nsyms
#define BLOCK_SYM(bl, n) (bl)->sym[n]
#define BLOCK_FUNCTION(bl) (bl)->function
#define BLOCK_SUPERBLOCK(bl) (bl)->superblock
#define BLOCK_GCC_COMPILED(bl) (bl)->gcc_compile_flag

/* Nonzero if symbols of block BL should be sorted alphabetically.  */
#define BLOCK_SHOULD_SORT(bl) ((bl)->nsyms >= 40)

#define SYMBOL_NAME(symbol) (symbol)->name
#define SYMBOL_NAMESPACE(symbol) (symbol)->namespace
#define SYMBOL_CLASS(symbol) (symbol)->class
#define SYMBOL_VALUE(symbol) (symbol)->value.value
#define SYMBOL_VALUE_ADDRESS(symbol) (symbol)->value.address
#define SYMBOL_VALUE_BYTES(symbol) (symbol)->value.bytes
#define SYMBOL_BLOCK_VALUE(symbol) (symbol)->value.block
#define SYMBOL_VALUE_CHAIN(symbol) (symbol)->value.chain
#define SYMBOL_TYPE(symbol) (symbol)->type
#define SYMBOL_LINE(symbol) (symbol)->line
#if 0
/* This currently fails because some symbols are not being initialized
   to zero on allocation, and no code is currently setting this value.
   Basereg handling will probably change significantly in the next release.
   FIXME -fnf */
#define SYMBOL_BASEREG_VALID(symbol) (symbol)->aux_value.basereg.regno_valid
#else
#define SYMBOL_BASEREG_VALID(symbol) 0
#endif
#define SYMBOL_BASEREG(symbol) (symbol)->aux_value.basereg.regno

/* The virtual function table is now an array of structures
   which have the form { int16 offset, delta; void *pfn; }. 

   In normal virtual function tables, OFFSET is unused.
   DELTA is the amount which is added to the apparent object's base
   address in order to point to the actual object to which the
   virtual function should be applied.
   PFN is a pointer to the virtual function.  */
  
#define VTBL_FNADDR_OFFSET 2

/* Macro that yields non-zero value iff NAME is the prefix
   for C++ operator names.  If you leave out the parenthesis
   here you will lose!

   Currently 'o' 'p' CPLUS_MARKER is used for both the symbol in the
   symbol-file and the names in gdb's symbol table.  */
#define OPNAME_PREFIX_P(NAME) ((NAME)[0] == 'o' && (NAME)[1] == 'p' \
			       && (NAME)[2] == CPLUS_MARKER)

#define VTBL_PREFIX_P(NAME) ((NAME)[3] == CPLUS_MARKER	\
			     && !strncmp ((NAME), "_vt", 3))

/* Functions that work on the objects described above */

extern struct symtab *
lookup_symtab PARAMS ((char *));

extern struct symbol *
lookup_symbol PARAMS ((const char *, const struct block *,
		       const enum namespace, int *, struct symtab **));

extern struct symbol *
lookup_block_symbol PARAMS ((const struct block *, const char *,
			     const enum namespace));

extern struct type *
lookup_struct PARAMS ((char *, struct block *));

extern struct type *
lookup_union PARAMS ((char *, struct block *));

extern struct type *
lookup_enum PARAMS ((char *, struct block *));

extern struct symbol *
block_function PARAMS ((struct block *));

extern struct symbol *
find_pc_function PARAMS ((CORE_ADDR));

extern int
find_pc_partial_function PARAMS ((CORE_ADDR, char **, CORE_ADDR *));

extern void
clear_pc_function_cache PARAMS ((void));

extern struct partial_symtab *
lookup_partial_symtab PARAMS ((char *));

extern struct partial_symtab *
find_pc_psymtab PARAMS ((CORE_ADDR));

extern struct symtab *
find_pc_symtab PARAMS ((CORE_ADDR));

extern struct partial_symbol *
find_pc_psymbol PARAMS ((struct partial_symtab *, CORE_ADDR));

extern int
find_pc_line_pc_range PARAMS ((CORE_ADDR, CORE_ADDR *, CORE_ADDR *));

extern int
contained_in PARAMS ((struct block *, struct block *));

extern void
reread_symbols PARAMS ((void));

/* Functions for dealing with the minimal symbol table, really a misc
   address<->symbol mapping for things we don't have debug symbols for.  */

extern void
prim_record_minimal_symbol PARAMS ((const char *, CORE_ADDR,
				    enum minimal_symbol_type));

extern void
prim_record_minimal_symbol_and_info PARAMS ((const char *, CORE_ADDR,
					     enum minimal_symbol_type,
					     char *info));

extern struct minimal_symbol *
lookup_minimal_symbol PARAMS ((const char *, struct objfile *));

extern struct minimal_symbol *
lookup_minimal_symbol_by_pc PARAMS ((CORE_ADDR));

extern void
init_minimal_symbol_collection PARAMS ((void));

extern void
discard_minimal_symbols PARAMS ((int));

extern void
install_minimal_symbols PARAMS ((struct objfile *));

struct symtab_and_line
{
  struct symtab *symtab;
  int line;
  CORE_ADDR pc;
  CORE_ADDR end;
};

struct symtabs_and_lines
{
  struct symtab_and_line *sals;
  int nelts;
};

/* Given a pc value, return line number it is in.
   Second arg nonzero means if pc is on the boundary
   use the previous statement's line number.  */

extern struct symtab_and_line
find_pc_line PARAMS ((CORE_ADDR, int));

/* Given a symtab and line number, return the pc there.  */

extern CORE_ADDR
find_line_pc PARAMS ((struct symtab *, int));

extern int 
find_line_pc_range PARAMS ((struct symtab *, int, CORE_ADDR *, CORE_ADDR *));

extern void
resolve_sal_pc PARAMS ((struct symtab_and_line *));

/* Given a string, return the line specified by it.
   For commands like "list" and "breakpoint".  */

extern struct symtabs_and_lines
decode_line_spec PARAMS ((char *, int));

extern struct symtabs_and_lines
decode_line_spec_1 PARAMS ((char *, int));

extern struct symtabs_and_lines
decode_line_1 PARAMS ((char **, int, struct symtab *, int));

/* Symmisc.c */

#if MAINTENANCE_CMDS

void
maintenance_print_symbols PARAMS ((char *, int));

void
maintenance_print_psymbols PARAMS ((char *, int));

void
maintenance_print_msymbols PARAMS ((char *, int));

void
maintenance_print_objfiles PARAMS ((char *, int));

#endif

extern void
free_symtab PARAMS ((struct symtab *));

/* Symbol-reading stuff in symfile.c and solib.c.  */

extern struct symtab *
psymtab_to_symtab PARAMS ((struct partial_symtab *));

extern void
clear_solib PARAMS ((void));

extern struct objfile *
symbol_file_add PARAMS ((char *, int, CORE_ADDR, int, int, int));

/* source.c */

extern int
identify_source_line PARAMS ((struct symtab *, int, int));

extern void
print_source_lines PARAMS ((struct symtab *, int, int, int));

extern void
forget_cached_source_info PARAMS ((void));

extern void
select_source_symtab PARAMS ((struct symtab *));

extern char **
make_symbol_completion_list PARAMS ((char *));

/* symtab.c */

extern void
clear_symtab_users_once PARAMS ((void));

extern struct partial_symtab *
find_main_psymtab PARAMS ((void));

/* blockframe.c */

extern struct blockvector *
blockvector_for_pc PARAMS ((CORE_ADDR, int *));

/* symfile.c */

extern enum language
deduce_language_from_filename PARAMS ((char *));

#endif /* !defined(SYMTAB_H) */
