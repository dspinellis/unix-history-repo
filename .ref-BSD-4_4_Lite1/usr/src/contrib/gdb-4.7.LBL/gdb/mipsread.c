/* Read a symbol table in MIPS' format (Third-Eye).
   Copyright 1986, 1987, 1989, 1990, 1991, 1992 Free Software Foundation, Inc.
   Contributed by Alessandro Forin (af@cs.cmu.edu) at CMU.  Major
   work by Per Bothner and John Gilmore at Cygnus Support.

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

/* This module provides three functions: mipscoff_symfile_init,
   which initializes to read a symbol file; mipscoff_new_init, which
   discards existing cached information when all symbols are being
   discarded; and mipscoff_symfile_read, which reads a symbol table
   from a file.

   mipscoff_symfile_read only does the minimum work necessary for letting the
   user "name" things symbolically; it does not read the entire symtab.
   Instead, it reads the external and static symbols and puts them in partial
   symbol tables.  When more extensive information is requested of a
   file, the corresponding partial symbol table is mutated into a full
   fledged symbol table by going back and reading the symbols
   for real.  mipscoff_psymtab_to_symtab() is called indirectly through
   a pointer in the psymtab to do this.

   ECOFF symbol tables are mostly written in the byte order of the
   target machine.  However, one section of the table (the auxiliary
   symbol information) is written in the host byte order.  There is a
   bit in the other symbol info which describes which host byte order
   was used.  ECOFF thereby takes the trophy from Intel `b.out' for
   the most brain-dead adaptation of a file format to byte order.

   This module can read all four of the known byte-order combinations,
   on any type of host.  However, it does make (and check) the assumption
   that the external form of a symbol table structure (on disk)
   occupies the same number of bytes as the internal form (in a struct).
   Fixing this is possible but requires larger structural changes.  */

#define	TM_FILE_OVERRIDE
#include "defs.h"
#include "tm-mips.h"
#include "symtab.h"
#include "gdbtypes.h"
#include "gdbcore.h"
#include "symfile.h"
#include "objfiles.h"
#include "obstack.h"
#include "buildsym.h"
#include "stabsread.h"

#ifdef USG
#include <sys/types.h>
#define L_SET 0
#define L_INCR 1
#endif

#include <sys/param.h>
#include <sys/file.h>
#include <sys/stat.h>

#include "gdb-stabs.h"

#include "coff/mips.h"		/* COFF-like aspects of ecoff files */
#include "coff/ecoff-ext.h"	/* External forms of ecoff sym structures */

#include "libbfd.h"		/* FIXME Secret internal BFD stuff (bfd_read) */
#include "libaout.h"		/* FIXME Secret internal BFD stuff for a.out */
#include "aout/aout64.h"
#include "aout/stab_gnu.h"	/* STABS information */

struct coff_exec {
	struct external_filehdr f;
	struct external_aouthdr a;
};

/* These must match the corresponding definition in gcc/config/xm-mips.h.
   At some point, these should probably go into a shared include file,
   but currently gcc and gdb do not share any directories. */

#define CODE_MASK 0x8F300
#define MIPS_IS_STAB(sym) (((sym)->index & 0xFFF00) == CODE_MASK)
#define MIPS_MARK_STAB(code) ((code)+CODE_MASK)
#define MIPS_UNMARK_STAB(code) ((code)-CODE_MASK)
#define STABS_SYMBOL "@stabs"

/* Each partial symbol table entry contains a pointer to private data for the
   read_symtab() function to use when expanding a partial symbol table entry
   to a full symbol table entry.

   For mipsread this structure contains the index of the FDR that this psymtab
   represents and a pointer to the symbol table header HDRR from the symbol
   file that the psymtab was created from.  */

#define PST_PRIVATE(p) ((struct symloc *)(p)->read_symtab_private)
#define FDR_IDX(p) (PST_PRIVATE(p)->fdr_idx)
#define CUR_HDR(p) (PST_PRIVATE(p)->cur_hdr)

struct symloc {
  int fdr_idx;
  HDRR *cur_hdr;
  EXTR **extern_tab; /* Pointer to external symbols for this file. */
  int extern_count; /* Size of extern_tab. */
};

/* Things we import explicitly from other modules */

extern int	     info_verbose;

/* Various complaints about symbol reading that don't abort the process */

struct complaint bad_file_number_complaint =
	{"bad file number %d", 0, 0};

struct complaint index_complaint =
	{"bad aux index at symbol %s", 0, 0};

struct complaint aux_index_complaint =
	{"bad proc end in aux found from symbol %s", 0, 0};

struct complaint unknown_ext_complaint =
	{"unknown external symbol %s", 0, 0};

struct complaint unknown_sym_complaint =
	{"unknown local symbol %s", 0, 0};

struct complaint unknown_st_complaint =
	{"with type %d", 0, 0};

struct complaint block_overflow_complaint =
	{"block containing %s overfilled", 0, 0};

struct complaint basic_type_complaint =
	{"cannot map MIPS basic type 0x%x", 0, 0};

struct complaint unknown_type_qual_complaint =
	{"unknown type qualifier 0x%x", 0, 0};

struct complaint array_bitsize_complaint =
	{"size of array target type not known, assuming %d bits", 0, 0};

struct complaint bad_tag_guess_complaint =
	{"guessed tag type of %s incorrectly", 0, 0};

struct complaint block_member_complaint =
	{"declaration block contains unhandled symbol type %d", 0, 0};

struct complaint stEnd_complaint =
	{"stEnd with storage class %d not handled", 0, 0};

struct complaint unknown_mips_symtype_complaint =
	{"unknown symbol type 0x%x", 0, 0};

struct complaint stab_unknown_complaint =
	{"unknown stabs symbol %s", 0, 0};

struct complaint pdr_for_nonsymbol_complaint =
	{"PDR for %s, but no symbol", 0, 0};

struct complaint pdr_static_symbol_complaint =
	{"can't handle PDR for static proc at 0x%x", 0, 0};

/* Macros and extra defs */

/* Already-parsed symbols are marked specially */

#define stParsed stType

/* Puns: hard to find whether -g was used and how */

#define MIN_GLEVEL GLEVEL_0
#define compare_glevel(a,b)					\
	(((a) == GLEVEL_3) ? ((b) < GLEVEL_3) :			\
	 ((b) == GLEVEL_3) ? -1 : (int)((b) - (a)))

/* When looking at .o files, avoid tripping over zero pointers.
   FIXME; places that use this should be fixed to convert from
   external to internal format, rather than examining in-place. */

#define	UNSAFE_DATA_ADDR(p)	((p) == 0)

/* Things that really are local to this module */

/* MIPS symtab header for the current file */

static HDRR	*cur_hdr;

/* Pointer to current file decriptor record, and its index */

static FDR	*cur_fdr;
static int	 cur_fd;

/* Index of current symbol */

static int	 cur_sdx;

/* Note how much "debuggable" this image is.  We would like
   to see at least one FDR with full symbols */

static max_gdbinfo;
static max_glevel;

/* When examining .o files, report on undefined symbols */

static int n_undef_symbols, n_undef_labels, n_undef_vars, n_undef_procs;

/* Pseudo symbol to use when putting stabs into the symbol table.  */

static char stabs_symbol[] = STABS_SYMBOL;

/* Extra builtin types */

struct type *builtin_type_complex;
struct type *builtin_type_double_complex;
struct type *builtin_type_fixed_dec;
struct type *builtin_type_float_dec;
struct type *builtin_type_string;

/* Forward declarations */

static void
fixup_symtab PARAMS ((HDRR *, char *, file_ptr, bfd *));

static void
read_mips_symtab PARAMS ((struct objfile *, struct section_offsets *));

static void
read_the_mips_symtab PARAMS ((bfd *, CORE_ADDR *));

static int
upgrade_type PARAMS ((struct type **, int, union aux_ext *, int));

static void
parse_partial_symbols PARAMS ((int, struct objfile *,
			       struct section_offsets *));

static int
cross_ref PARAMS ((union aux_ext *, struct type **, enum type_code, char **,
		   int));

static void
fixup_sigtramp PARAMS ((void));

static struct symbol *
new_symbol PARAMS ((char *));

static struct type *
new_type PARAMS ((char *));

static struct block *
new_block PARAMS ((int));

static struct symtab *
new_symtab PARAMS ((char *, int, int, struct objfile *));

static struct linetable *
new_linetable PARAMS ((int));

static struct blockvector *
new_bvect PARAMS ((int));

static struct type *
parse_type PARAMS ((union aux_ext *, int *, int));

static struct symbol *
mylookup_symbol PARAMS ((char *, struct block *, enum namespace,
			 enum address_class));

static struct block *
shrink_block PARAMS ((struct block *, struct symtab *));

static PTR
xzalloc PARAMS ((unsigned int));

static void
sort_blocks PARAMS ((struct symtab *));

static int
compare_blocks PARAMS ((const void *, const void *));

static struct partial_symtab *
new_psymtab PARAMS ((char *, struct objfile *));

#if 0
static struct partial_symtab *
parse_fdr PARAMS ((int, int, struct objfile *));
#endif

static void
psymtab_to_symtab_1 PARAMS ((struct partial_symtab *, char *));

static void
add_block PARAMS ((struct block *, struct symtab *));

static void
add_symbol PARAMS ((struct symbol *, struct block *));

static int
add_line PARAMS ((struct linetable *, int, CORE_ADDR, int));

static struct linetable *
shrink_linetable PARAMS ((struct linetable *));

static char *
mips_next_symbol_text PARAMS ((void));

/* Things we export to other modules */

/* Address bounds for the signal trampoline in inferior, if any */
/* FIXME:  Nothing really seems to use this.  Why is it here? */

CORE_ADDR sigtramp_address, sigtramp_end;

static void
mipscoff_new_init (ignore)
     struct objfile *ignore;
{
}

static void
mipscoff_symfile_init (objfile)
     struct objfile *objfile;
{
  if (objfile -> sym_private != NULL)
    {
      mfree (objfile -> md, objfile -> sym_private);
    }
  objfile -> sym_private = NULL;
}

static void
mipscoff_symfile_read (objfile, section_offsets, mainline)
     struct objfile *objfile;
     struct section_offsets *section_offsets;
     int mainline;
{
  init_minimal_symbol_collection ();
  make_cleanup (discard_minimal_symbols, 0);

  /* Now that the executable file is positioned at symbol table,
     process it and define symbols accordingly.  */

  read_mips_symtab(objfile, section_offsets);

  /* Install any minimal symbols that have been collected as the current
     minimal symbols for this objfile. */

  install_minimal_symbols (objfile);
}

/* Perform any local cleanups required when we are done with a particular
   objfile.  I.E, we are in the process of discarding all symbol information
   for an objfile, freeing up all memory held for it, and unlinking the
   objfile struct from the global list of known objfiles. */

static void
mipscoff_symfile_finish (objfile)
     struct objfile *objfile;
{
  if (objfile -> sym_private != NULL)
    {
      mfree (objfile -> md, objfile -> sym_private);
    }

  /* If we have a file symbol header lying around, blow it away.  */

  if (cur_hdr)
    {
      free ((PTR)cur_hdr);
    }
  cur_hdr = 0;
}

/* Allocate zeroed memory */

static PTR
xzalloc(size)
     unsigned int size;
{
  PTR p = xmalloc (size);

  memset (p, 0, size);
  return p;
}

/* Exported procedure: Builds a symtab from the PST partial one.
   Restores the environment in effect when PST was created, delegates
   most of the work to an ancillary procedure, and sorts
   and reorders the symtab list at the end */

static void
mipscoff_psymtab_to_symtab(pst)
	struct partial_symtab *pst;
{

	if (!pst)
		return;

	if (info_verbose) {
		printf_filtered("Reading in symbols for %s...", pst->filename);
		fflush(stdout);
	}
	/* Restore the header and list of pending typedefs */
	cur_hdr = CUR_HDR(pst);

	next_symbol_text_func = mips_next_symbol_text;

	psymtab_to_symtab_1(pst, pst->filename);

	/* Match with global symbols.  This only needs to be done once,
	   after all of the symtabs and dependencies have been read in.   */
	scan_file_globals (pst->objfile);

	if (info_verbose)
		printf_filtered("done.\n");
}

/* Exported procedure: Is PC in the signal trampoline code */

int
in_sigtramp(pc, ignore)
	CORE_ADDR pc;
	char *ignore;		/* function name */
{
	if (sigtramp_address == 0)
		fixup_sigtramp();
	return (pc >= sigtramp_address && pc < sigtramp_end);
}

/* File-level interface functions */

/* Read the symtab information from file ABFD into memory.  Also,
   return address just past end of our text segment in *END_OF_TEXT_SEGP.  */

static void
read_the_mips_symtab(abfd, end_of_text_segp)
	bfd		*abfd;
	CORE_ADDR	*end_of_text_segp;
{
	int             stsize, st_hdrsize;
	file_ptr        st_filptr;
	struct hdr_ext	hdr_ext;
	HDRR            st_hdr;
	/* Header for executable/object file we read symbols from */
	struct coff_exec filhdr;
	int val;

	/* We need some info from the initial headers */
	val = bfd_seek(abfd, (file_ptr) 0, L_SET);
	val = bfd_read((PTR)&filhdr, sizeof filhdr, 1, abfd);

	if (end_of_text_segp)
		*end_of_text_segp =
			bfd_h_get_32 (abfd, filhdr.a.text_start) +
			bfd_h_get_32 (abfd, filhdr.a.tsize);

	/* Find and read the symbol table header */
	st_hdrsize = bfd_h_get_32 (abfd, filhdr.f.f_nsyms);
	st_filptr  = bfd_h_get_32 (abfd, filhdr.f.f_symptr);
	if (st_filptr == 0)
		return;

	bfd_seek (abfd, st_filptr, L_SET);
	if (st_hdrsize != sizeof (hdr_ext)) {	/* Profanity check */
		error ("Wrong header size: %d, not %d", st_hdrsize,
			sizeof (hdr_ext));
	}
	if (bfd_read((PTR)&hdr_ext, st_hdrsize, 1, abfd) != st_hdrsize)
		goto readerr;
	ecoff_swap_hdr_in (abfd, &hdr_ext, &st_hdr);

	/* Find out how large the symbol table is */
	stsize = (st_hdr.cbExtOffset - (st_filptr + st_hdrsize))
		+ st_hdr.iextMax * cbEXTR;

	/* Allocate space for the symbol table.  Read it in.  */
	cur_hdr = (HDRR *) xmalloc(stsize + st_hdrsize);

	memcpy((PTR)cur_hdr, (PTR)&hdr_ext, st_hdrsize);
	if (bfd_read((char *)cur_hdr + st_hdrsize, stsize, 1, abfd) != stsize)
		goto readerr;

	/* Fixup file_pointers in it */
	fixup_symtab(cur_hdr, (char *) cur_hdr + st_hdrsize,
		     st_filptr + st_hdrsize, abfd);

	return;
readerr:
	error("Short read on %s", bfd_get_filename (abfd));
}


/* Turn all file-relative pointers in the symtab described by HDR
   into memory pointers, given that the symtab itself is located
   at DATA in memory and F_PTR in the file.

   Byte-swap all the data structures, in place, while we are at it --
   except AUX entries, which we leave in their original byte order.
   They will be swapped as they are used instead.  (FIXME:  we ought to
   do all the data structures that way.)  */

static void
fixup_symtab (hdr, data, f_ptr, abfd)
	HDRR *hdr;
	char *data;
	file_ptr f_ptr;
	bfd *abfd;
{
	int             f_idx, s_idx, i;
	FDR            *fh;
	SYMR	       *sh;
	PDR	       *pr;
	EXTR	       *esh;
	struct rfd_ext *rbase;

	/* This function depends on the external and internal forms
	   of the MIPS symbol table taking identical space.  Check this
	   assumption at compile-time.  
	   DO NOT DELETE THESE ENTRIES, OR COMMENT THEM OUT, JUST BECAUSE SOME
	   "LINT" OR COMPILER THINKS THEY ARE UNUSED!  Thank you.  */
	static check_hdr1[1 + sizeof (struct hdr_ext) - sizeof (HDRR)] = {0};
	static check_hdr2[1 + sizeof (HDRR) - sizeof (struct hdr_ext)] = {0};
	static check_fdr1[1 + sizeof (struct fdr_ext) - sizeof (FDR)] = {0};
	static check_fdr2[1 + sizeof (FDR) - sizeof (struct fdr_ext)] = {0};
	static check_pdr1[1 + sizeof (struct pdr_ext) - sizeof (PDR)] = {0};
	static check_pdr2[1 + sizeof (PDR) - sizeof (struct pdr_ext)] = {0};
	static check_sym1[1 + sizeof (struct sym_ext) - sizeof (SYMR)] = {0};
	static check_sym2[1 + sizeof (SYMR) - sizeof (struct sym_ext)] = {0};
	static check_ext1[1 + sizeof (struct ext_ext) - sizeof (EXTR)] = {0};
	static check_ext2[1 + sizeof (EXTR) - sizeof (struct ext_ext)] = {0};
	static check_rfd1[1 + sizeof (struct rfd_ext) - sizeof (RFDT)] = {0};
	static check_rfd2[1 + sizeof (RFDT) - sizeof (struct rfd_ext)] = {0};

	/* Swap in the header record.  */
	ecoff_swap_hdr_in (abfd, hdr, hdr);

	/*
	 * These fields are useless (and empty) by now:
	 *	hdr->cbDnOffset, hdr->cbOptOffset
	 * We use them for other internal purposes.
	 */
	hdr->cbDnOffset = 0;
	hdr->cbOptOffset = 0;

#define FIX(off) \
	if (hdr->off) hdr->off = (unsigned int)data + (hdr->off - f_ptr);

	FIX(cbLineOffset);
	FIX(cbPdOffset);
	FIX(cbSymOffset);
	FIX(cbOptOffset);
	FIX(cbAuxOffset);
	FIX(cbSsOffset);
	FIX(cbSsExtOffset);
	FIX(cbFdOffset);
	FIX(cbRfdOffset);
	FIX(cbExtOffset);
#undef	FIX

	/* Fix all the RFD's.  */
	rbase = (struct rfd_ext *)(hdr->cbRfdOffset);
	for (i = 0; i < hdr->crfd; i++) {
	  ecoff_swap_rfd_in (abfd, rbase+i, (pRFDT) rbase+i);
	}

	/* Fix all string pointers inside the symtab, and
	   the FDR records.  Also fix other miscellany.  */

	for (f_idx = 0; f_idx < hdr->ifdMax; f_idx++) {
		register unsigned code_offset;

		/* Header itself, and strings */
		fh = (FDR *) (hdr->cbFdOffset) + f_idx;

		/* Swap in the FDR */
		ecoff_swap_fdr_in (abfd, fh, fh);

		fh->issBase += hdr->cbSsOffset;
		if (fh->rss != -1)
			fh->rss = (long)fh->rss + fh->issBase;

		/* Local symbols */
		fh->isymBase = (int)((SYMR*)(hdr->cbSymOffset)+fh->isymBase);

		/* FIXME! Probably don't want to do this here! */
		for (s_idx = 0; s_idx < fh->csym; s_idx++) {
			sh = (SYMR*)fh->isymBase + s_idx;
			ecoff_swap_sym_in (abfd, sh, sh);

			sh->iss = (long) sh->iss + fh->issBase;
			sh->reserved = 0;
		}

		cur_fd = f_idx;

		/* cannot fix fh->ipdFirst because it is a short */
#define IPDFIRST(h,fh) \
		((long)h->cbPdOffset + fh->ipdFirst * sizeof(PDR))

		/* Optional symbols (actually used for partial_symtabs) */
		fh->ioptBase = 0;
		fh->copt = 0;

		/* Aux symbols */
		if (fh->caux)
			fh->iauxBase = hdr->cbAuxOffset + fh->iauxBase * sizeof(union aux_ext);
		/* Relative file descriptor table */
		fh->rfdBase = hdr->cbRfdOffset + fh->rfdBase * sizeof(RFDT);

		/* Line numbers */
		if (fh->cbLine)
			fh->cbLineOffset += hdr->cbLineOffset;

		/* Procedure symbols.  (XXX This should be done later) */
		code_offset = fh->adr;
		for (s_idx = 0; s_idx < fh->cpd; s_idx++) {
			unsigned name, only_ext;

			pr = (PDR*)(IPDFIRST(hdr,fh)) + s_idx;
			ecoff_swap_pdr_in (abfd, pr, pr);

			/* Simple rule to find files linked "-x" */
			only_ext = fh->rss == -1;
			if (only_ext) {
				if (pr->isym == -1) {
					/* static function */
					sh = (SYMR*)-1;
				} else {
					/* external */
					name = hdr->cbExtOffset + pr->isym * sizeof(EXTR);
					sh = &((EXTR*)name)->asym;
				}
			} else {
				/* Full symbols */
				sh = (SYMR*)fh->isymBase + pr->isym;
				/* Included code ? */
				if (s_idx == 0 && pr->adr != 0)
					code_offset -= pr->adr;
			}

			/* Turn index into a pointer */
			pr->isym = (long)sh;

			/* Fix line numbers */
			pr->cbLineOffset += fh->cbLineOffset;

			/* Relocate address */
			if (!only_ext)
				pr->adr += code_offset;
		}
	}

	/* External symbols: swap in, and fix string */
	for (s_idx = 0; s_idx < hdr->iextMax; s_idx++) {
		esh = (EXTR*)(hdr->cbExtOffset) + s_idx;
		ecoff_swap_ext_in (abfd, esh, esh);
		esh->asym.iss = esh->asym.iss + hdr->cbSsExtOffset;
	}
}


/* Find a file descriptor given its index RF relative to a file CF */

static FDR *
get_rfd (cf, rf)
	int cf, rf;
{
	register FDR   *f;

	f = (FDR *) (cur_hdr->cbFdOffset) + cf;
	/* Object files do not have the RFD table, all refs are absolute */
	if (f->rfdBase == 0)
		return (FDR *) (cur_hdr->cbFdOffset) + rf;
	cf = *((pRFDT) f->rfdBase + rf);
	return (FDR *) (cur_hdr->cbFdOffset) + cf;
}

/* Return a safer print NAME for a file descriptor */

static char *
fdr_name(name)
	char *name;
{
	if (name == (char *) -1)
		return "<stripped file>";
	if (UNSAFE_DATA_ADDR(name))
		return "<NFY>";
	return name;
}


/* Read in and parse the symtab of the file OBJFILE.  Symbols from
   different sections are relocated via the SECTION_OFFSETS.  */

static void
read_mips_symtab (objfile, section_offsets)
	struct objfile *objfile;
	struct section_offsets *section_offsets;
{
	CORE_ADDR end_of_text_seg;

	read_the_mips_symtab(objfile->obfd, &end_of_text_seg);

	parse_partial_symbols(end_of_text_seg, objfile, section_offsets);

#if 0
	/*
	 * Check to make sure file was compiled with -g.
	 * If not, warn the user of this limitation.
	 */
	if (compare_glevel(max_glevel, GLEVEL_2) < 0) {
		if (max_gdbinfo == 0)
			printf (
"\n%s not compiled with -g, debugging support is limited.\n",
				objfile->name);
		printf(
"You should compile with -g2 or -g3 for best debugging support.\n");
		fflush(stdout);
	}
#endif
}

/* Local utilities */

/* Map of FDR indexes to partial symtabs */

struct pst_map {
    struct partial_symtab *pst;	/* the psymtab proper */
    int n_globals; /* exported globals (external symbols) */
    int globals_offset;  /* cumulative */
};


/* Utility stack, used to nest procedures and blocks properly.
   It is a doubly linked list, to avoid too many alloc/free.
   Since we might need it quite a few times it is NOT deallocated
   after use. */

static struct parse_stack {
    struct parse_stack	*next, *prev;
    struct symtab	*cur_st;	/* Current symtab. */
    struct block	*cur_block;	/* Block in it. */
    int			 blocktype;	/* What are we parsing. */
    int			 maxsyms;	/* Max symbols in this block. */
    struct type		*cur_type;	/* Type we parse fields for. */
    int			 cur_field;	/* Field number in cur_type. */
    int			 procadr;	/* Start addres of this procedure */
    int			 numargs;	/* Its argument count */
} *top_stack;	/* Top stack ptr */


/* Enter a new lexical context */

static void
push_parse_stack()
{
	struct parse_stack *new;

	/* Reuse frames if possible */
	if (top_stack && top_stack->prev)
		new = top_stack->prev;
	else
		new = (struct parse_stack *) xzalloc(sizeof(struct parse_stack));
	/* Initialize new frame with previous content */
	if (top_stack) {
		register struct parse_stack *prev = new->prev;

		*new = *top_stack;
		top_stack->prev = new;
		new->prev = prev;
		new->next = top_stack;
	}
	top_stack = new;
}

/* Exit a lexical context */

static void
pop_parse_stack()
{
	if (!top_stack)
		return;
	if (top_stack->next)
		top_stack = top_stack->next;
}


/* Cross-references might be to things we haven't looked at
   yet, e.g. type references.  To avoid too many type
   duplications we keep a quick fixup table, an array
   of lists of references indexed by file descriptor */

static struct mips_pending {
	struct mips_pending	*next;		/* link */
	SYMR		*s;		/* the symbol */
	struct type	*t;		/* its partial type descriptor */
} **pending_list;


/* Check whether we already saw symbol SH in file FH as undefined */

static struct mips_pending *
is_pending_symbol(fh, sh)
	FDR *fh;
	SYMR *sh;
{
	int             f_idx = fh - (FDR *) cur_hdr->cbFdOffset;
	register struct mips_pending *p;

	/* Linear search is ok, list is typically no more than 10 deep */
	for (p = pending_list[f_idx]; p; p = p->next)
		if (p->s == sh)
			break;
	return p;
}

/* Add a new undef symbol SH of type T */

static void
add_pending(fh, sh, t)
	FDR *fh;
	SYMR *sh;
	struct type *t;
{
	int             f_idx = fh - (FDR *) cur_hdr->cbFdOffset;
	struct mips_pending *p = is_pending_symbol(fh, sh);

	/* Make sure we do not make duplicates */
	if (!p) {
		p = (struct mips_pending *) xmalloc(sizeof(*p));
		p->s = sh;
		p->t = t;
		p->next = pending_list[f_idx];
		pending_list[f_idx] = p;
	}
	sh->reserved = 1;	/* for quick check */
}

/* Throw away undef entries when done with file index F_IDX */
/* FIXME -- storage leak.  This is never called!!!   --gnu */

#if 0

static void
free_pending(f_idx)
	int f_idx;
{
	register struct mips_pending *p, *q;

	for (p = pending_list[f_idx]; p; p = q) {
		q = p->next;
		free((PTR)p);
	}
	pending_list[f_idx] = 0;
}

#endif

static char *
prepend_tag_kind(tag_name, type_code)
     char *tag_name;
     enum type_code type_code;
{
    char *prefix;
    char *result;
    switch (type_code) {
      case TYPE_CODE_ENUM:
	prefix = "enum ";
	break;
      case TYPE_CODE_STRUCT:
	prefix = "struct ";
	break;
      case TYPE_CODE_UNION:
	prefix = "union ";
	break;
      default:
	prefix = "";
    }

    result = (char*)obstack_alloc (&current_objfile->symbol_obstack,
				   strlen(prefix) + strlen(tag_name) + 1);
    sprintf(result, "%s%s", prefix, tag_name);
    return result;
}


/* Parsing Routines proper. */

/* Parse a single symbol. Mostly just make up a GDB symbol for it.
   For blocks, procedures and types we open a new lexical context.
   This is basically just a big switch on the symbol's type.
   Argument AX is the base pointer of aux symbols for this file (fh->iauxBase).
   BIGEND says whether aux symbols are big-endian or little-endian.
   Return count of SYMR's handled (normally one). */

static int
parse_symbol(sh, ax, bigend)
	SYMR *sh;
	union aux_ext *ax;
	int bigend;
{
	char *name;
	struct symbol  *s;
	struct block   *b;
	struct type    *t;
	struct field   *f;
	int count = 1;
	/* When a symbol is cross-referenced from other files/symbols
	   we mark it explicitly */
	int             pend = (sh->reserved == 1);
	enum address_class class;
	TIR		tir;

	switch (sh->st) {

	    case stNil:
		break;

	    case stGlobal:	/* external symbol, goes into global block */
		class = LOC_STATIC;
		b = BLOCKVECTOR_BLOCK(BLOCKVECTOR(top_stack->cur_st),
				      GLOBAL_BLOCK);
		s = new_symbol((char *)sh->iss);
		SYMBOL_VALUE_ADDRESS(s) = (CORE_ADDR)sh->value;
		goto data;

	    case stStatic:	/* static data, goes into current block. */
		class = LOC_STATIC;
		b = top_stack->cur_block;
		s = new_symbol((char *)sh->iss);
		SYMBOL_VALUE_ADDRESS(s) = (CORE_ADDR)sh->value;
		goto data;

	    case stLocal:	/* local variable, goes into current block */
		if (sh->sc == scRegister) {
			class = LOC_REGISTER;
			if (sh->value > 31)
				sh->value += FP0_REGNUM-32;
		} else
			class = LOC_LOCAL;
		b = top_stack->cur_block;
		s = new_symbol((char *)sh->iss);
		SYMBOL_VALUE(s) = sh->value;

data:		/* Common code for symbols describing data */
		SYMBOL_NAMESPACE(s) = VAR_NAMESPACE;
		SYMBOL_CLASS(s) = class;
		add_symbol(s, b);

		/* Type could be missing in a number of cases */
		if (sh->sc == scUndefined || sh->sc == scNil ||
		    sh->index == 0xfffff)
			SYMBOL_TYPE(s) = builtin_type_int;	/* undefined? */
		else
			SYMBOL_TYPE(s) = parse_type(ax + sh->index, 0, bigend);
		/* Value of a data symbol is its memory address */
		break;

	    case stParam:	/* arg to procedure, goes into current block */
		max_gdbinfo++;
		top_stack->numargs++;

		name = (char*)sh->iss;
		/* Special GNU C++ name.  */
		if (name[0] == CPLUS_MARKER && name[1] == 't' && name[2] == 0)
		    name = "this";	/* FIXME, not alloc'd in obstack */
		s = new_symbol(name);

		SYMBOL_NAMESPACE(s) = VAR_NAMESPACE;
		if (sh->sc == scRegister) {
			SYMBOL_CLASS(s) = LOC_REGPARM;
			if (sh->value > 31)
				sh->value += FP0_REGNUM-32;
		} else
			SYMBOL_CLASS(s) = LOC_ARG;
		SYMBOL_VALUE(s) = sh->value;
		SYMBOL_TYPE(s) = parse_type(ax + sh->index, 0, bigend);
		add_symbol(s, top_stack->cur_block);
#if 0
		/* FIXME:  This has not been tested.  See dbxread.c */
		/* Add the type of this parameter to the function/procedure
		   type of this block. */
		add_param_to_type(&top_stack->cur_block->function->type,s);
#endif
		break;

	    case stLabel:	/* label, goes into current block */
		s = new_symbol((char *)sh->iss);
		SYMBOL_NAMESPACE(s) = VAR_NAMESPACE;	/* so that it can be used */
		SYMBOL_CLASS(s) = LOC_LABEL;		/* but not misused */
		SYMBOL_VALUE_ADDRESS(s) = (CORE_ADDR)sh->value;
		SYMBOL_TYPE(s) = builtin_type_int;
		add_symbol(s, top_stack->cur_block);
		break;

	    case stProc:	/* Procedure, usually goes into global block */
	    case stStaticProc:	/* Static procedure, goes into current block */
		s = new_symbol((char *)sh->iss);
		SYMBOL_NAMESPACE(s) = VAR_NAMESPACE;
		SYMBOL_CLASS(s) = LOC_BLOCK;
		/* Type of the return value */
		if (sh->sc == scUndefined || sh->sc == scNil)
			t = builtin_type_int;
		else
			t = parse_type(ax + sh->index + 1, 0, bigend);
		b = top_stack->cur_block;
		if (sh->st == stProc) {
		    struct blockvector *bv = BLOCKVECTOR(top_stack->cur_st);
		    /* The next test should normally be true,
		       but provides a hook for nested functions
		       (which we don't want to make global). */
		    if (b == BLOCKVECTOR_BLOCK(bv, STATIC_BLOCK))
			b = BLOCKVECTOR_BLOCK(bv, GLOBAL_BLOCK);
		}
		add_symbol(s, b);

		/* Make a type for the procedure itself */
#if 0
		/* FIXME:  This has not been tested yet!  See dbxread.c */
		/* Generate a template for the type of this function.  The
		   types of the arguments will be added as we read the symbol
		   table. */
		bcopy(SYMBOL_TYPE(s),lookup_function_type(t),sizeof(struct type));
#else
		SYMBOL_TYPE(s) = lookup_function_type (t);
#endif

		/* Create and enter a new lexical context */
		b = new_block(top_stack->maxsyms);
		SYMBOL_BLOCK_VALUE(s) = b;
		BLOCK_FUNCTION(b) = s;
		BLOCK_START(b) = BLOCK_END(b) = sh->value;
		BLOCK_SUPERBLOCK(b) = top_stack->cur_block;
		add_block(b, top_stack->cur_st);

		/* Not if we only have partial info */
		if (sh->sc == scUndefined || sh->sc == scNil)
			break;

		push_parse_stack();
		top_stack->cur_block = b;
		top_stack->blocktype = sh->st;
		top_stack->cur_type = SYMBOL_TYPE(s);
		top_stack->cur_field = -1;
		top_stack->procadr = sh->value;
		top_stack->numargs = 0;

		sh->value = (long) SYMBOL_TYPE(s);
		break;

	    /* Beginning of code for structure, union, and enum definitions.
	       They all share a common set of local variables, defined here.  */
	    {
		enum type_code type_code;
		SYMR *tsym;
		int nfields;
		long max_value;
		struct field *f;

	    case stStruct:	/* Start a block defining a struct type */
		type_code = TYPE_CODE_STRUCT;
		goto structured_common;

	    case stUnion:	/* Start a block defining a union type */
		type_code = TYPE_CODE_UNION;
		goto structured_common;

	    case stEnum:	/* Start a block defining an enum type */
		type_code = TYPE_CODE_ENUM;
		goto structured_common;

	    case stBlock:	/* Either a lexical block, or some type */
		if (sh->sc != scInfo)
		  goto case_stBlock_code;	/* Lexical block */

		type_code = TYPE_CODE_UNDEF;	/* We have a type.  */

	    /* Common code for handling struct, union, enum, and/or as-yet-
	       unknown-type blocks of info about structured data.  `type_code'
	       has been set to the proper TYPE_CODE, if we know it.  */
	    structured_common:
		push_parse_stack();
		top_stack->blocktype = stBlock;

		s = new_symbol((char *)sh->iss);
		SYMBOL_NAMESPACE(s) = STRUCT_NAMESPACE;
		SYMBOL_CLASS(s) = LOC_TYPEDEF;
		SYMBOL_VALUE(s) = 0;
		add_symbol(s, top_stack->cur_block);

		/* First count the number of fields and the highest value. */
		nfields = 0;
		max_value = 0;
		for (tsym = sh+1; tsym->st != stEnd; tsym++)
		  {
		    if (tsym->st == stMember) {
			if (nfields == 0 && type_code == TYPE_CODE_UNDEF)
			    /* If the type of the member is Nil (or Void),
			       assume the tag is an enumeration. */
			    if (tsym->index == indexNil)
				type_code = TYPE_CODE_ENUM;
			    else {
				ecoff_swap_tir_in (bigend,
						   &ax[tsym->index].a_ti,
						   &tir);
				if (tir.bt == btNil || tir.bt == btVoid)
				    type_code = TYPE_CODE_ENUM;
			    }
			nfields++;
			if (tsym->value > max_value)
			    max_value = tsym->value;
		    }
		    else if (tsym->st == stBlock
			     || tsym->st == stUnion
			     || tsym->st == stEnum
			     || tsym->st == stStruct
			     || tsym->st == stParsed) {
			if (tsym->sc == scVariant) ; /*UNIMPLEMENTED*/
			if (tsym->index != 0)
			    tsym = ((SYMR*)cur_fdr->isymBase)
				+ tsym->index-1;
		    }
		    else complain (&block_member_complaint, (char *)tsym->st);
		  }

		/* In an stBlock, there is no way to distinguish structs,
		   unions, and enums at this point.  This is a bug in the
		   original design (that has been fixed with the
		   recent addition of the stStruct, stUnion, and stEnum
		   symbol types.)  The way you can tell is if/when you
		   see a variable or field of that type.  In that case
		   the variable's type (in the AUX table) says if the
		   type is struct, union, or enum,
		   and points back to the stBlock here.
		   So you can patch the tag kind up later - but only
		   if there actually is a variable or field of that type.

		   So until we know for sure, we will guess at this point.
		   The heuristic is:
		   If the first member has index==indexNil or a void type,
		   assume we have an enumeration.
		   Otherwise, if there is more than one member, and all
		   the members have offset 0, assume we have a union.
		   Otherwise, assume we have a struct.

		   The heuristic could guess wrong in the case of
		   of an enumeration with no members or a union
		   with one (or zero) members, or when all except the
		   last field of a struct have width zero.
		   These are uncommon and/or illegal situations, and
		   in any case guessing wrong probably doesn't matter much.

		   But if we later do find out we were wrong,
		   we fixup the tag kind.  Members of an enumeration
		   must be handled differently from struct/union fields,
		   and that is harder to patch up, but luckily we
		   shouldn't need to.  (If there are any enumeration
		   members, we can tell for sure it's an enum here.) */

		if (type_code == TYPE_CODE_UNDEF)
		    if (nfields > 1 && max_value == 0)
		      type_code = TYPE_CODE_UNION;
		    else
		      type_code = TYPE_CODE_STRUCT;

		/* If this type was expected, use its partial definition */
		if (pend)
		    t = is_pending_symbol(cur_fdr, sh)->t;
		else
		    t = new_type(prepend_tag_kind((char *)sh->iss,
						  type_code));

		TYPE_CODE(t) = type_code;
		TYPE_LENGTH(t) = sh->value;
		TYPE_NFIELDS(t) = nfields;
		TYPE_FIELDS(t) = f = (struct field*)
		  TYPE_ALLOC (t, nfields * sizeof (struct field));

		if (type_code == TYPE_CODE_ENUM) {
		    /* This is a non-empty enum. */
		    for (tsym = sh + 1; tsym->st == stMember; tsym++) {
			struct symbol *enum_sym;
			f->bitpos = tsym->value;
			f->type = t;
			f->name = (char*)tsym->iss;
			f->bitsize = 0;

			enum_sym = (struct symbol *)
			    obstack_alloc (&current_objfile->symbol_obstack,
					   sizeof (struct symbol));
			memset ((PTR)enum_sym, 0, sizeof (struct symbol));
			SYMBOL_NAME (enum_sym) = f->name;
			SYMBOL_CLASS (enum_sym) = LOC_CONST;
			SYMBOL_TYPE (enum_sym) = t;
			SYMBOL_NAMESPACE (enum_sym) = VAR_NAMESPACE;
			SYMBOL_VALUE (enum_sym) = tsym->value;
			add_symbol(enum_sym, top_stack->cur_block);

			/* Skip the stMembers that we've handled. */
			count++;
			f++;
		    }
		}
		SYMBOL_TYPE(s) = t;
		/* make this the current type */
		top_stack->cur_type = t;
		top_stack->cur_field = 0;
		/* Mark that symbol has a type, and say which one */
		sh->value = (long) t;
		break;

	    /* End of local variables shared by struct, union, enum, and
	       block (as yet unknown struct/union/enum) processing.  */
	    }

	    case_stBlock_code:
		/* beginnning of (code) block. Value of symbol
		   is the displacement from procedure start */
		push_parse_stack();
		top_stack->blocktype = stBlock;
		b = new_block(top_stack->maxsyms);
		BLOCK_START(b) = sh->value + top_stack->procadr;
		BLOCK_SUPERBLOCK(b) = top_stack->cur_block;
		top_stack->cur_block = b;
		add_block(b, top_stack->cur_st);
		break;

	    case stEnd:		/* end (of anything) */
		if (sh->sc == scInfo) {
			/* Finished with type */
			top_stack->cur_type = 0;
		} else if (sh->sc == scText &&
			   (top_stack->blocktype == stProc ||
			    top_stack->blocktype == stStaticProc)) {
		    /* Finished with procedure */
		    struct blockvector *bv = BLOCKVECTOR(top_stack->cur_st);
		    struct mips_extra_func_info *e;
		    struct block *b;
		    int i;

		    BLOCK_END(top_stack->cur_block) += sh->value; /* size */

		    /* Make up special symbol to contain procedure specific
		       info */
		    s = new_symbol(MIPS_EFI_SYMBOL_NAME);
		    SYMBOL_NAMESPACE(s) = LABEL_NAMESPACE;
		    SYMBOL_CLASS(s) = LOC_CONST;
		    SYMBOL_TYPE(s) = builtin_type_void;
		    e = (struct mips_extra_func_info *)
		      obstack_alloc (&current_objfile->symbol_obstack,
				     sizeof (struct mips_extra_func_info));
		    SYMBOL_VALUE(s) = (int)e;
		    e->numargs = top_stack->numargs;
		    add_symbol(s, top_stack->cur_block);

		    /* Reallocate symbols, saving memory */
		    b = shrink_block(top_stack->cur_block, top_stack->cur_st);

		    /* f77 emits proc-level with address bounds==[0,0],
		       So look for such child blocks, and patch them.  */
		    for (i = 0; i < BLOCKVECTOR_NBLOCKS(bv); i++) {
			struct block *b_bad = BLOCKVECTOR_BLOCK(bv,i);
			if (BLOCK_SUPERBLOCK(b_bad) == b
			 && BLOCK_START(b_bad) == top_stack->procadr
			 && BLOCK_END(b_bad) == top_stack->procadr) {
			    BLOCK_START(b_bad) = BLOCK_START(b);
			    BLOCK_END(b_bad) = BLOCK_END(b);
			}
		    }
		} else if (sh->sc == scText && top_stack->blocktype == stBlock) {
			/* End of (code) block. The value of the symbol
			   is the displacement from the procedure`s start
			   address of the end of this block. */
			BLOCK_END(top_stack->cur_block) = sh->value + top_stack->procadr;
			shrink_block(top_stack->cur_block, top_stack->cur_st);
		} else if (sh->sc == scText && top_stack->blocktype == stFile) {
			/* End of file.  Pop parse stack and ignore.  Higher
			   level code deals with this.  */
			;
		} else complain (&stEnd_complaint, (char *)sh->sc);

		pop_parse_stack();	/* restore previous lexical context */
		break;

	    case stMember:	/* member of struct or union */
		f = &TYPE_FIELDS(top_stack->cur_type)[top_stack->cur_field++];
		f->name = (char*)sh->iss;
		f->bitpos = sh->value;
		f->bitsize = 0;
		f->type = parse_type(ax + sh->index, &f->bitsize, bigend);
		break;

	    case stTypedef:	/* type definition */
		s = new_symbol((char *)sh->iss);
		SYMBOL_NAMESPACE(s) = VAR_NAMESPACE;
		SYMBOL_CLASS(s) = LOC_TYPEDEF;
		SYMBOL_BLOCK_VALUE(s) = top_stack->cur_block;
		add_symbol(s, top_stack->cur_block);
		SYMBOL_TYPE(s) = parse_type(ax + sh->index, 0, bigend);
		sh->value = (long) SYMBOL_TYPE(s);
		break;

	    case stFile:	/* file name */
		push_parse_stack();
		top_stack->blocktype = sh->st;
		break;

		/* I`ve never seen these for C */
	    case stRegReloc:
		break;		/* register relocation */
	    case stForward:
		break;		/* forwarding address */
	    case stConstant:
		break;		/* constant */
	    default:
		complain(&unknown_mips_symtype_complaint, (char *)sh->st);
		break;
	}
	sh->st = stParsed;
	return count;
}

/* Parse the type information provided in the raw AX entries for
   the symbol SH. Return the bitfield size in BS, in case.
   We must byte-swap the AX entries before we use them; BIGEND says whether
   they are big-endian or little-endian (from fh->fBigendian).  */

static struct type *
parse_type(ax, bs, bigend)
	union aux_ext	*ax;
	int	*bs;
	int	bigend;
{
	/* Null entries in this map are treated specially */
	static struct type **map_bt[] =
	{
		 &builtin_type_void,		/* btNil */
		 0,				/* btAdr */
		 &builtin_type_char,		/* btChar */
		 &builtin_type_unsigned_char,	/* btUChar */
		 &builtin_type_short,		/* btShort */
		 &builtin_type_unsigned_short,	/* btUShort */
		 &builtin_type_int,		/* btInt */
		 &builtin_type_unsigned_int,	/* btUInt */
		 &builtin_type_long,		/* btLong */
		 &builtin_type_unsigned_long,	/* btULong */
		 &builtin_type_float,		/* btFloat */
		 &builtin_type_double,		/* btDouble */
		 0,				/* btStruct */
		 0,				/* btUnion */
		 0,				/* btEnum */
		 0,				/* btTypedef */
		 0,				/* btRange */
		 0,				/* btSet */
		 &builtin_type_complex,		/* btComplex */
		 &builtin_type_double_complex,	/* btDComplex */
		 0,				/* btIndirect */
		 &builtin_type_fixed_dec,	/* btFixedDec */
		 &builtin_type_float_dec,	/* btFloatDec */
		 &builtin_type_string,		/* btString */
		 0,				/* btBit */
		 0,				/* btPicture */
		 &builtin_type_void,		/* btVoid */
		 &builtin_type_long_long,	/* btLongLong */
		 &builtin_type_unsigned_long_long,/* btULongLong */
	};

	TIR            t[1];
	struct type    *tp = 0;
	char           *fmt;
	union aux_ext *tax;
	enum type_code type_code;

	/* Use aux as a type information record, map its basic type.  */
	tax = ax;
	ecoff_swap_tir_in (bigend, &tax->a_ti, t);
	if (t->bt > (sizeof (map_bt)/sizeof (*map_bt))) {
		complain (&basic_type_complaint, (char *)t->bt);
		return builtin_type_int;
	}
	if (map_bt[t->bt]) {
		tp = *map_bt[t->bt];
		fmt = "%s";
	} else {
		tp = NULL;
		/* Cannot use builtin types -- build our own */
		switch (t->bt) {
		    case btAdr:
			tp = lookup_pointer_type (builtin_type_void);
			fmt = "%s";
			break;
		    case btStruct:
			type_code = TYPE_CODE_STRUCT;
			fmt = "struct %s";
			break;
		    case btUnion:
			type_code = TYPE_CODE_UNION;
			fmt = "union %s";
			break;
		    case btEnum:
			type_code = TYPE_CODE_ENUM;
			fmt = "enum %s";
			break;
		    case btRange:
			type_code = TYPE_CODE_RANGE;
			fmt = "%s";
			break;
		    case btSet:
			type_code = TYPE_CODE_SET;
			fmt = "set %s";
			break;
		    case btTypedef:
		    default:
			complain (&basic_type_complaint, (char *)t->bt);
			return builtin_type_int;
		}
	}

	/* Skip over any further type qualifiers (FIXME).  */
	if (t->continued) {
		/* This is the way it would work if the compiler worked */
		TIR t1[1];
		do {
			ax++;
			ecoff_swap_tir_in (bigend, ax, t1);
		} while (t1->continued);
	}

	/* Move on to next aux */
	ax++;

	if (t->fBitfield) {
		if (bs != 0)
			*bs = AUX_GET_WIDTH (bigend, ax);
		ax++;
	}

	/* All these types really point to some (common) MIPS type
	   definition, and only the type-qualifiers fully identify
	   them.  We'll make the same effort at sharing. */
	if (t->bt == btIndirect ||
	    t->bt == btStruct ||
	    t->bt == btUnion ||
	    t->bt == btEnum ||
	    t->bt == btTypedef ||
	    t->bt == btRange ||
	    t->bt == btSet) {
		char            name[256], *pn;

		/* Try to cross reference this type */
		ax += cross_ref(ax, &tp, type_code, &pn, bigend);
		/* reading .o file ? */
		if (UNSAFE_DATA_ADDR(tp))
		    tp = init_type(type_code, 0, 0, (char *) NULL,
				   (struct objfile *) NULL);
		/* SOMEONE OUGHT TO FIX DBXREAD TO DROP "STRUCT" */
		sprintf(name, fmt, pn);

		/* Usually, TYPE_CODE(tp) is already type_code.  The main
		   exception is if we guessed wrong re struct/union/enum. */
		if (TYPE_CODE(tp) != type_code) {
		    complain (&bad_tag_guess_complaint, name);
		    TYPE_CODE(tp) = type_code;
		}
		if (TYPE_NAME(tp) == NULL || strcmp(TYPE_NAME(tp), name) != 0)
		    TYPE_NAME(tp) = obsavestring(name, strlen(name),
						 &current_objfile -> type_obstack);
	}

	/* Deal with range types */
	if (t->bt == btRange) {
		TYPE_NFIELDS (tp) = 2;
		TYPE_FIELDS (tp) = (struct field *)
		  TYPE_ALLOC (tp, 2 * sizeof (struct field));
		TYPE_FIELD_NAME (tp, 0) = obsavestring ("Low", strlen ("Low"),
							&current_objfile -> type_obstack);
		TYPE_FIELD_BITPOS (tp, 0) = AUX_GET_DNLOW (bigend, ax);
		ax++;
		TYPE_FIELD_NAME (tp, 1) = obsavestring ("High", strlen ("High"),
							&current_objfile -> type_obstack);
		TYPE_FIELD_BITPOS (tp, 1) = AUX_GET_DNHIGH (bigend, ax);
		ax++;
	}

	/* Parse all the type qualifiers now. If there are more
	   than 6 the game will continue in the next aux */

#define PARSE_TQ(tq) \
	if (t->tq != tqNil) ax += upgrade_type(&tp, t->tq, ax, bigend);

again:	PARSE_TQ(tq0);
	PARSE_TQ(tq1);
	PARSE_TQ(tq2);
	PARSE_TQ(tq3);
	PARSE_TQ(tq4);
	PARSE_TQ(tq5);
#undef	PARSE_TQ

	if (t->continued) {
		tax++;
		ecoff_swap_tir_in (bigend, &tax->a_ti, t);
		goto again;
	}
	return tp;
}

/* Make up a complex type from a basic one.  Type is passed by
   reference in TPP and side-effected as necessary. The type
   qualifier TQ says how to handle the aux symbols at AX for
   the symbol SX we are currently analyzing.  BIGEND says whether
   aux symbols are big-endian or little-endian.
   Returns the number of aux symbols we parsed. */

static int
upgrade_type(tpp, tq, ax, bigend)
	struct type  **tpp;
	int	       tq;
	union aux_ext *ax;
	int	       bigend;
{
	int            off;
	struct type   *t;

	/* Used in array processing */
	int             rf, id;
	FDR            *fh;
	struct field   *f;
	int		lower, upper;
	RNDXR		rndx;

	switch (tq) {
	case tqPtr:
		t = lookup_pointer_type (*tpp);
		*tpp = t;
		return 0;

	case tqProc:
		t = lookup_function_type (*tpp);
		*tpp = t;
		return 0;

	case tqArray:
		off = 0;
		t = init_type(TYPE_CODE_ARRAY, 0, 0, (char *) NULL,
			      (struct objfile *) NULL);
		TYPE_TARGET_TYPE(t) = *tpp;

		/* Determine and record the domain type (type of index) */
		ecoff_swap_rndx_in (bigend, ax, &rndx);
		id = rndx.index;
		rf = rndx.rfd;
		if (rf == 0xfff) {
			ax++;
			rf = AUX_GET_ISYM (bigend, ax);
			off++;
		}
		fh = get_rfd(cur_fd, rf);

		/* Fields are kept in an array */
		/* FIXME - Memory leak! */
		if (TYPE_NFIELDS(t))
		    TYPE_FIELDS(t) = (struct field*)
			xrealloc((PTR) TYPE_FIELDS(t),
				 (TYPE_NFIELDS(t)+1) * sizeof(struct field));
		else
		    TYPE_FIELDS(t) = (struct field*)
			xzalloc(sizeof(struct field));
		f = &(TYPE_FIELD(t,TYPE_NFIELDS(t)));
		TYPE_NFIELDS(t)++;
		memset((PTR)f, 0, sizeof(struct field));

/* XXX */	f->type = parse_type(id + (union aux_ext *)fh->iauxBase,
				     &f->bitsize, bigend);

		ax++;
		lower = AUX_GET_DNLOW (bigend, ax);
		ax++;
		upper = AUX_GET_DNHIGH (bigend, ax);
		ax++;
		rf = AUX_GET_WIDTH (bigend, ax);	/* bit size of array element */

		/* Check whether supplied array element bit size matches
		   the known size of the element type.  If this complaint
		   ends up not happening, we can remove this code.  It's
		   here because we aren't sure we understand this *&%&$
		   symbol format.  */
		id = TYPE_LENGTH(TYPE_TARGET_TYPE(t)) << 3; /* bitsize */
		if (id == 0) {
			/* Most likely an undefined type */
			id = rf;
			TYPE_LENGTH(TYPE_TARGET_TYPE(t)) = id >> 3;
		}
		if (id != rf)
			complain (&array_bitsize_complaint, (char *)rf);

		TYPE_LENGTH(t) = (upper < 0) ? 0 :
			(upper - lower + 1) * (rf >> 3);
		*tpp = t;
		return 4 + off;

	case tqVol:
		/* Volatile -- currently ignored */
		return 0;

	case tqConst:
		/* Const -- currently ignored */
		return 0;

	default:
		complain (&unknown_type_qual_complaint, (char *)tq);
		return 0;
	}
}


/* Parse a procedure descriptor record PR.  Note that the procedure
   is parsed _after_ the local symbols, now we just insert the extra
   information we need into a MIPS_EFI_SYMBOL_NAME symbol that has already
   been placed in the procedure's main block.  Note also that images that
   have been partially stripped (ld -x) have been deprived
   of local symbols, and we have to cope with them here.
   The procedure's code ends at BOUND */

static void
parse_procedure (pr, bound, have_stabs)
	PDR *pr;
	int bound;
	int have_stabs;
{
    struct symbol *s, *i;
    SYMR *sh = (SYMR*)pr->isym;
    struct block *b;
    struct mips_extra_func_info *e;
    char *sh_name;

    /* Static procedure at address pr->adr.  Sigh. */
    if (sh == (SYMR*)-1) {
	complain (&pdr_static_symbol_complaint, (char *)pr->adr);
	return;
    }
    sh_name = (char*)sh->iss;
    if (have_stabs)
	s = lookup_symbol(sh_name, NULL, VAR_NAMESPACE, 0, NULL);
    else
	s = mylookup_symbol(sh_name, top_stack->cur_block,
			    VAR_NAMESPACE, LOC_BLOCK);

    if (s != 0) {
	    b = SYMBOL_BLOCK_VALUE(s);
    } else {
	    complain (&pdr_for_nonsymbol_complaint, sh_name);
#if 1
	return;
#else
/* FIXME -- delete.  We can't do symbol allocation now; it's all done.  */
	    s = new_symbol(sh_name);
	    SYMBOL_NAMESPACE(s) = VAR_NAMESPACE;
	    SYMBOL_CLASS(s) = LOC_BLOCK;
	    /* Donno its type, hope int is ok */
	    SYMBOL_TYPE(s) = lookup_function_type (builtin_type_int);
	    add_symbol(s, top_stack->cur_block);
	    /* Wont have symbols for this one */
	    b = new_block(2);
	    SYMBOL_BLOCK_VALUE(s) = b;
	    BLOCK_FUNCTION(b) = s;
	    BLOCK_START(b) = pr->adr;
	    BLOCK_END(b) = bound;
	    BLOCK_SUPERBLOCK(b) = top_stack->cur_block;
	    add_block(b, top_stack->cur_st);
#endif
    }

    i = mylookup_symbol(MIPS_EFI_SYMBOL_NAME, b, LABEL_NAMESPACE, LOC_CONST);

    if (i)
      {
	e = (struct mips_extra_func_info *)SYMBOL_VALUE(i);
	e->pdr = *pr;
	e->pdr.isym = (long)s;
      }
}

/* Parse the external symbol ES. Just call parse_symbol() after
   making sure we know where the aux are for it. For procedures,
   parsing of the PDRs has already provided all the needed
   information, we only parse them if SKIP_PROCEDURES is false,
   and only if this causes no symbol duplication.
   BIGEND says whether aux entries are big-endian or little-endian.

   This routine clobbers top_stack->cur_block and ->cur_st. */

static void
parse_external(es, skip_procedures, bigend)
	EXTR *es;
	int skip_procedures;
	int bigend;
{
	union aux_ext *ax;

	if (es->ifd != ifdNil) {
		cur_fd = es->ifd;
		cur_fdr = (FDR*)(cur_hdr->cbFdOffset) + cur_fd;
		ax = (union aux_ext *)cur_fdr->iauxBase;
	} else {
		cur_fdr = (FDR*)(cur_hdr->cbFdOffset);
		ax = 0;
	}

	/* Reading .o files */
	if (es->asym.sc == scUndefined || es->asym.sc == scNil) {
		char *what;
		switch (es->asym.st) {
		case stStaticProc:
		case stProc:	what = "procedure"; n_undef_procs++;  break;
		case stGlobal:	what = "variable";  n_undef_vars++;   break;
		case stLabel:	what = "label";     n_undef_labels++; break;
		default :	what = "symbol";                      break;
		}
		n_undef_symbols++;
		/* FIXME:  Turn this into a complaint? */
		if (info_verbose)
		    printf_filtered("Warning: %s `%s' is undefined (in %s)\n",
			 what, es->asym.iss, fdr_name((char *)cur_fdr->rss));
		return;
	}

	switch (es->asym.st) {
	case stProc:
		/* If we have full symbols we do not need more */
		if (skip_procedures)
			return;
		if (mylookup_symbol ((char *)es->asym.iss, top_stack->cur_block,
				     VAR_NAMESPACE, LOC_BLOCK))
			break;
		/* fall through */
	case stGlobal:
	case stLabel:
		/*
		 * Note that the case of a symbol with indexNil
		 * must be handled anyways by parse_symbol().
		 */
		parse_symbol(&es->asym, ax, bigend);
		break;
	default:
		break;
	}
}

/* Parse the line number info for file descriptor FH into
   GDB's linetable LT.  MIPS' encoding requires a little bit
   of magic to get things out.  Note also that MIPS' line
   numbers can go back and forth, apparently we can live
   with that and do not need to reorder our linetables */

static void
parse_lines(fh, lt)
	FDR *fh;
	struct linetable *lt;
{
	unsigned char *base = (unsigned char*)fh->cbLineOffset;
	int j, k;
	int delta, count, lineno = 0;
	PDR *pr;

	if (base == 0)
		return;

	/* Scan by procedure descriptors */
	j = 0, k = 0;
	for (pr = (PDR*)IPDFIRST(cur_hdr,fh); j < fh->cpd; j++, pr++) {
		int l, halt;

		/* No code for this one */
		if (pr->iline == ilineNil ||
		    pr->lnLow == -1 || pr->lnHigh == -1)
			continue;
		/*
		 *	Aurgh! To know where to stop expanding we
		 *	must look-ahead.
		 */
		for (l = 1; l < (fh->cpd - j); l++)
			if (pr[l].iline != -1)
				break;
		if (l == (fh->cpd - j))
			halt = fh->cline;
		else
			halt = pr[l].iline;
		/*
		 * When procedures are moved around the linenumbers
		 * are attributed to the next procedure up
		 */
		if (pr->iline >= halt) continue;

		base = (unsigned char*)pr->cbLineOffset;
		l = pr->adr >> 2;	/* in words */
		halt += (pr->adr >> 2) - pr->iline;
		for (lineno = pr->lnLow; l < halt;) {
			count = *base & 0x0f;
			delta = *base++ >> 4;
			if (delta >= 8)
				delta -= 16;
			if (delta == -8) {
				delta = (base[0] << 8) | base[1];
				if (delta >= 0x8000)
					delta -= 0x10000;
				base += 2;
			}
			lineno += delta;/* first delta is 0 */
			k = add_line(lt, lineno, l, k);
			l += count + 1;
		}
	}
}

/* Master parsing procedure for first-pass reading of file symbols
   into a partial_symtab.

   Parses the symtab described by the global symbolic header CUR_HDR.
   END_OF_TEXT_SEG gives the address just after the text segment for
   the symtab we are reading.  */

static void
parse_partial_symbols (end_of_text_seg, objfile, section_offsets)
     int end_of_text_seg;
     struct objfile *objfile;
     struct section_offsets *section_offsets;
{
    int			 f_idx, s_idx;
    HDRR		*hdr = cur_hdr;
    /* Running pointers */
    FDR			*fh;
    register EXTR	*esh;
    register SYMR	*sh;
    struct partial_symtab *pst;

    int past_first_source_file = 0;

    /* List of current psymtab's include files */
    char **psymtab_include_list;
    int includes_allocated;
    int includes_used;
    EXTR **extern_tab;
    struct pst_map * fdr_to_pst;
    /* Index within current psymtab dependency list */
    struct partial_symtab **dependency_list;
    int dependencies_used, dependencies_allocated;
    struct cleanup *old_chain;

    extern_tab = (EXTR**)obstack_alloc (&objfile->psymbol_obstack,
					sizeof(EXTR *) * hdr->iextMax);

    includes_allocated = 30;
    includes_used = 0;
    psymtab_include_list = (char **) alloca (includes_allocated *
					     sizeof (char *));
    next_symbol_text_func = mips_next_symbol_text;

    dependencies_allocated = 30;
    dependencies_used = 0;
    dependency_list =
	(struct partial_symtab **) alloca (dependencies_allocated *
					   sizeof (struct partial_symtab *));

    last_source_file = NULL;

    /*
     * Big plan:
     *
     * Only parse the Local and External symbols, and the Relative FDR.
     * Fixup enough of the loader symtab to be able to use it.
     * Allocate space only for the file's portions we need to
     * look at. (XXX)
     */

    max_gdbinfo = 0;
    max_glevel = MIN_GLEVEL;

    /* Allocate the map FDR -> PST.
       Minor hack: -O3 images might claim some global data belongs
       to FDR -1. We`ll go along with that */
    fdr_to_pst = (struct pst_map *)xzalloc((hdr->ifdMax+1) * sizeof *fdr_to_pst);
    old_chain = make_cleanup (free, fdr_to_pst);
    fdr_to_pst++;
    {
	struct partial_symtab * pst = new_psymtab("", objfile);
	fdr_to_pst[-1].pst = pst;
	FDR_IDX(pst) = -1;
    }

    /* Pass 1 over external syms: Presize and partition the list */
    for (s_idx = 0; s_idx < hdr->iextMax; s_idx++) {
	esh = (EXTR *) (hdr->cbExtOffset) + s_idx;
	fdr_to_pst[esh->ifd].n_globals++;
    }

    /* Pass 1.5 over files:  partition out global symbol space */
    s_idx = 0;
    for (f_idx = -1; f_idx < hdr->ifdMax; f_idx++) {
	fdr_to_pst[f_idx].globals_offset = s_idx;
	s_idx += fdr_to_pst[f_idx].n_globals;
	fdr_to_pst[f_idx].n_globals = 0;
    }

    /* Pass 2 over external syms: fill in external symbols */
    for (s_idx = 0; s_idx < hdr->iextMax; s_idx++) {
	enum minimal_symbol_type ms_type = mst_text;
	esh = (EXTR *) (hdr->cbExtOffset) + s_idx;

	extern_tab[fdr_to_pst[esh->ifd].globals_offset
		   + fdr_to_pst[esh->ifd].n_globals++] = esh;

	if (esh->asym.sc == scUndefined || esh->asym.sc == scNil)
		continue;

	switch (esh->asym.st) {
	case stProc:
		break;
	case stGlobal:
		ms_type = mst_data;
		break;
	case stLabel:
		break;
	default:
		ms_type = mst_unknown;
		complain (&unknown_ext_complaint, (char *)esh->asym.iss);
	}
	prim_record_minimal_symbol ((char *)esh->asym.iss,
				    esh->asym.value,
				    ms_type);
    }

    /* Pass 3 over files, over local syms: fill in static symbols */
    for (f_idx = 0; f_idx < hdr->ifdMax; f_idx++) {
	struct partial_symtab *save_pst;
	EXTR **ext_ptr;

	cur_fdr = fh = f_idx + (FDR *)(cur_hdr->cbFdOffset);

	if (fh->csym == 0) {
	    fdr_to_pst[f_idx].pst = NULL;
	    continue;
	}
	pst = start_psymtab_common (objfile, section_offsets, (char*)fh->rss,
				    fh->cpd ? fh->adr : 0,
				    objfile->global_psymbols.next,
				    objfile->static_psymbols.next);
	pst->read_symtab_private = (char *)
	    obstack_alloc (&objfile->psymbol_obstack, sizeof (struct symloc));

	save_pst = pst;
	/* Make everything point to everything. */
	FDR_IDX(pst) = f_idx;
	fdr_to_pst[f_idx].pst = pst;
	fh->ioptBase = (int)pst;

	CUR_HDR(pst) = cur_hdr;

	/* The way to turn this into a symtab is to call... */
	pst->read_symtab = mipscoff_psymtab_to_symtab;

	pst->texthigh = pst->textlow;

	/* For stabs-in-ecoff files, the second symbol must be @stab.
	   This symbol is emitted by mips-tfile to signal
	   that the current object file uses encapsulated stabs
	   instead of mips ecoff for local symbols.
	   (It is the second symbol because the first symbol is
	   the stFile used to signal the start of a file). */
	if (fh->csym >= 2
	    && strcmp((char *)(((SYMR *)fh->isymBase)[1].iss),
		      stabs_symbol) == 0) {
	    processing_gcc_compilation = 2;
	    for (cur_sdx = 2; cur_sdx < fh->csym; cur_sdx++) {
		int type_code;
		char *namestring;
		sh = cur_sdx + (SYMR *) fh->isymBase;
		type_code = MIPS_UNMARK_STAB(sh->index);
		if (!MIPS_IS_STAB(sh)) {
		    if (sh->st == stProc || sh->st == stStaticProc) {
			long procaddr = sh->value;
			sh = AUX_GET_ISYM (fh->fBigendian,
			       sh->index + (union aux_ext *)(fh->iauxBase))
			    + (SYMR *) fh->isymBase - 1;
			if (sh->st == stEnd) {
			    long high = procaddr + sh->value;
			    if (high > pst->texthigh)
				pst->texthigh = high;
			}
		    }
		    continue;
		}
#define SET_NAMESTRING() namestring = (char*)sh->iss
#define CUR_SYMBOL_TYPE type_code
#define CUR_SYMBOL_VALUE sh->value
#define START_PSYMTAB(ofile,secoff,fname,low,symoff,global_syms,static_syms)\
  pst = save_pst
#define END_PSYMTAB(pst,ilist,ninc,c_off,c_text,dep_list,n_deps) (void)0
#define HANDLE_RBRAC(val) \
  if ((val) > save_pst->texthigh) save_pst->texthigh = (val);
#include "partial-stab.h"
	    }
	}
	else {
	    processing_gcc_compilation = 0;
	    for (cur_sdx = 0; cur_sdx < fh->csym; ) {
		char *name;
		enum address_class class;
		sh = cur_sdx + (SYMR *) fh->isymBase;

		if (MIPS_IS_STAB(sh)) {
		    cur_sdx++;
		    continue;
		}

		if (sh->sc == scUndefined || sh->sc == scNil ||
		    sh->index == 0xfffff) {
		    /* FIXME, premature? */
		    cur_sdx++;
		    continue;
		}

		name = (char *)(sh->iss);

		switch (sh->st) {
		    long high;
		    long procaddr;
		    int new_sdx;

		  case stProc:		/* Asm labels apparently */
		  case stStaticProc:		/* Function */
		    ADD_PSYMBOL_TO_LIST(name, strlen(name),
					VAR_NAMESPACE, LOC_BLOCK,
					objfile->static_psymbols, sh->value);
		    /* Skip over procedure to next one. */
		    if (sh->index >= hdr->iauxMax)
		      {
			/* Should not happen, but does when cross-compiling
			   with the MIPS compiler.  FIXME -- pull later.  */
			complain (&index_complaint, name);
			new_sdx = cur_sdx+1;	/* Don't skip at all */
		      }
		    else
		      new_sdx = AUX_GET_ISYM (fh->fBigendian,
				sh->index + (union aux_ext *)fh->iauxBase);
		    procaddr = sh->value;

		    if (new_sdx <= cur_sdx)
		      {
			/* This should not happen either... FIXME.  */
			complain (&aux_index_complaint, name);
			new_sdx = cur_sdx + 1;	/* Don't skip backward */
		      }

		    cur_sdx = new_sdx;
		    sh = cur_sdx + (SYMR *) fh->isymBase - 1;
		    if (sh->st != stEnd)
			continue;
		    high = procaddr + sh->value;
		    if (high > pst->texthigh)
			pst->texthigh = high;
		    continue;

		  case stStatic:			/* Variable */
		    class = LOC_STATIC;
		    break;

		  case stTypedef:			/* Typedef */
		    class = LOC_TYPEDEF;
		    break;

		  case stConstant:		/* Constant decl */
		    class = LOC_CONST;
		    break;

		  case stUnion:
		  case stStruct:
		  case stEnum:
		  case stBlock:			/* { }, str, un, enum*/
		    if (sh->sc == scInfo) {
			ADD_PSYMBOL_TO_LIST(name, strlen(name),
					    STRUCT_NAMESPACE, LOC_TYPEDEF,
					    objfile->static_psymbols, sh->value);
		    }
		    /* Skip over the block */
		    cur_sdx = sh->index;
		    continue;

		  case stFile:			/* File headers */
		  case stLabel:			/* Labels */
		  case stEnd:			/* Ends of files */
		    goto skip;

		  case stLocal:			/* Local variables */
		    /* Normally these are skipped because we skip over
		       all blocks we see.  However, these can occur
		       as visible symbols in a .h file that contains code. */
		    goto skip;

		  default:
		    /* Both complaints are valid:  one gives symbol name,
		       the other the offending symbol type.  */
		    complain (&unknown_sym_complaint, (char *)sh->iss);
		    complain (&unknown_st_complaint, (char *)sh->st);
		    cur_sdx++;
		    continue;
		}
		/* Use this gdb symbol */
		ADD_PSYMBOL_TO_LIST(name, strlen(name),
				    VAR_NAMESPACE, class,
				    objfile->static_psymbols, sh->value);
	      skip:
		cur_sdx++;		/* Go to next file symbol */
	    }

	    /* Now do enter the external symbols. */
	    ext_ptr = &extern_tab[fdr_to_pst[f_idx].globals_offset];
	    cur_sdx = fdr_to_pst[f_idx].n_globals;
	    PST_PRIVATE(save_pst)->extern_count = cur_sdx;
	    PST_PRIVATE(save_pst)->extern_tab = ext_ptr;
	    for (; --cur_sdx >= 0; ext_ptr++) {
		register struct partial_symbol *psym;
		enum address_class class;

		if ((*ext_ptr)->ifd != f_idx)
		    abort();
		sh = &(*ext_ptr)->asym;
		switch (sh->st) {
		  case stProc:
		    class = LOC_BLOCK;
		    break;
		  case stLabel:
		    class = LOC_LABEL;
		    break;
		  default:
		    complain (&unknown_ext_complaint, (char *)sh->iss);
		    /* Fall through, pretend it's global.  */
		  case stGlobal:
		    class = LOC_STATIC;
		    break;
		}
		if (objfile->global_psymbols.next >=
		    objfile->global_psymbols.list + objfile->global_psymbols.size)
		    extend_psymbol_list (&objfile->global_psymbols, objfile);
		psym = objfile->global_psymbols.next++;
		SYMBOL_NAME (psym) = (char*)sh->iss;
		SYMBOL_NAMESPACE (psym) = VAR_NAMESPACE;
		SYMBOL_CLASS (psym) = class;
		SYMBOL_VALUE_ADDRESS (psym) = (CORE_ADDR)sh->value;
	    }
	}

	end_psymtab (save_pst, psymtab_include_list, includes_used,
		     -1, save_pst->texthigh,
		     dependency_list, dependencies_used);
	if (objfile -> ei.entry_point >= save_pst->textlow &&
	    objfile -> ei.entry_point <  save_pst->texthigh)
	  {
	    objfile -> ei.entry_file_lowpc = save_pst->textlow;
	    objfile -> ei.entry_file_highpc = save_pst->texthigh;
	  }
    }

    /* Mark the last code address, and remember it for later */
    hdr->cbDnOffset = end_of_text_seg;

    /* Now scan the FDRs for dependencies */
    for (f_idx = 0; f_idx < hdr->ifdMax; f_idx++) {
	int s_id0 = 0;
	fh = f_idx + (FDR *)(cur_hdr->cbFdOffset);
	pst = fdr_to_pst[f_idx].pst;

	/* This should catch stabs-in-ecoff. */
	if (fh->crfd <= 1)
		continue;

	if (fh->cpd == 0) {  /* If there are no functions defined here ... */
		/* ...then presumably a .h file: drop reverse depends .h->.c */
		for (; s_id0 < fh->crfd; s_id0++) {
			RFDT *rh = (RFDT *) (fh->rfdBase) + s_id0;
			if (*rh == f_idx) {
				s_id0++;	/* Skip self-dependency */
				break;
			}
		}
	}
	pst->number_of_dependencies = fh->crfd - s_id0;
	pst->dependencies = (struct partial_symtab **)
		obstack_alloc (&objfile->psymbol_obstack,
			       pst->number_of_dependencies *
			       sizeof (struct partial_symtab *));
	for (s_idx = s_id0; s_idx < fh->crfd; s_idx++) {
	    RFDT *rh = (RFDT *) (fh->rfdBase) + s_idx;
	    if (*rh < 0 || *rh >= hdr->ifdMax)
		complain(&bad_file_number_complaint, (char *)*rh);
	    else
		pst->dependencies[s_idx-s_id0] = fdr_to_pst[*rh].pst;
	}
    }
    do_cleanups (old_chain);
}


#if 0
/* Do the initial analisys of the F_IDX-th file descriptor.
   Allocates a partial symtab for it, and builds the list
   of dependent files by recursion. LEV says at which level
   of recursion we are called (to pretty up debug traces) */

static struct partial_symtab *
parse_fdr(f_idx, lev, objfile)
	int f_idx;
	int lev;
	struct objfile *objfile;
{
	register FDR *fh;
	register struct partial_symtab *pst;
	int s_idx, s_id0;

	fh = (FDR *) (cur_hdr->cbFdOffset) + f_idx;

	/* Use this to indicate into which symtab this file was parsed */
	if (fh->ioptBase)
		return (struct partial_symtab *) fh->ioptBase;

	/* Debuggability level */
	if (compare_glevel(max_glevel, fh->glevel) < 0)
		max_glevel = fh->glevel;

	/* Make a new partial_symtab */
	pst = new_psymtab(fh->rss, objfile);
	if (fh->cpd == 0){
		pst->textlow = 0;
		pst->texthigh = 0;
	} else {
		pst->textlow = fh->adr;
		pst->texthigh = fh->cpd;	/* To be fixed later */
	}

	/* Make everything point to everything. */
	FDR_IDX(pst) = f_idx;
	fdr_to_pst[f_idx].pst = pst;
	fh->ioptBase = (int)pst;

	/* Analyze its dependencies */
	if (fh->crfd <= 1)
		return pst;

	s_id0 = 0;
	if (fh->cpd == 0) {  /* If there are no functions defined here ... */
		/* ...then presumably a .h file: drop reverse depends .h->.c */
		for (; s_id0 < fh->crfd; s_id0++) {
			RFDT *rh = (RFDT *) (fh->rfdBase) + s_id0;
			if (*rh == f_idx) {
				s_id0++;	/* Skip self-dependency */
				break;
			}
		}
	}
	pst->number_of_dependencies = fh->crfd - s_id0;
	pst->dependencies = (struct partial_symtab **)
		obstack_alloc (&objfile->psymbol_obstack,
				 pst->number_of_dependencies *
				   sizeof (struct partial_symtab *));
	for (s_idx = s_id0; s_idx < fh->crfd; s_idx++) {
		RFDT *rh = (RFDT *) (fh->rfdBase) + s_idx;

		pst->dependencies[s_idx-s_id0] = parse_fdr(*rh, lev+1, objfile);
	}

	return pst;
}
#endif

static char*
mips_next_symbol_text ()
{
    cur_sdx++;
    return (char*)((SYMR *)cur_fdr->isymBase)[cur_sdx].iss;
}

/* Ancillary function to psymtab_to_symtab().  Does all the work
   for turning the partial symtab PST into a symtab, recurring
   first on all dependent psymtabs.  The argument FILENAME is
   only passed so we can see in debug stack traces what file
   is being read.

   This function has a split personality, based on whether the
   symbol table contains ordinary ecoff symbols, or stabs-in-ecoff.
   The flow of control and even the memory allocation differs.  FIXME.  */

static void
psymtab_to_symtab_1(pst, filename)
     struct partial_symtab *pst;
     char *filename;
{
    int i;
    struct symtab  *st;
    FDR *fh;
    struct linetable *lines;
    int bound;

    if (pst->readin)
	return;
    pst->readin = 1;

    /* Read in all partial symbtabs on which this one is dependent.
       NOTE that we do have circular dependencies, sigh.  We solved
       that by setting pst->readin before this point.  */

    for (i = 0; i < pst->number_of_dependencies; i++)
	if (!pst->dependencies[i]->readin) {
	    /* Inform about additional files to be read in.  */
	    if (info_verbose)
		{
		    fputs_filtered (" ", stdout);
		    wrap_here ("");
		    fputs_filtered ("and ", stdout);
		    wrap_here ("");
		    printf_filtered ("%s...",
				     pst->dependencies[i]->filename);
		    wrap_here ("");		/* Flush output */
		    fflush (stdout);
		}
	    /* We only pass the filename for debug purposes */
	    psymtab_to_symtab_1(pst->dependencies[i],
				pst->dependencies[i]->filename);
	}

    /* Now read the symbols for this symtab */

    current_objfile = pst->objfile;
    cur_fd = FDR_IDX(pst);
    fh = (cur_fd == -1) ? 0 : (FDR *) (cur_hdr->cbFdOffset) + FDR_IDX(pst);
    cur_fdr = fh;

    /* BOUND is the highest core address of this file's procedures */
    bound = (cur_fd == cur_hdr->ifdMax - 1) ?
		    cur_hdr->cbDnOffset :
		    fh[1].adr;

    /* See comment in parse_partial_symbols about the @stabs sentinel. */
    if (fh && fh->csym >= 2
	    && strcmp((char *)(((SYMR *)fh->isymBase)[1].iss), stabs_symbol)
	        == 0) {

	/*
	 * This symbol table contains stabs-in-ecoff entries.
	 */

	PDR *pr;

	/* We indicate that this is a GCC compilation so that certain features
	   will be enabled in stabsread/dbxread.  */
	processing_gcc_compilation = 2;
	/* Parse local symbols first */

	if (fh->csym <= 2)	/* FIXME, this blows psymtab->symtab ptr */
	  {
	    current_objfile = NULL;
	    return;
	  }
	for (cur_sdx = 2; cur_sdx < fh->csym; cur_sdx++) {
	    register SYMR	*sh = cur_sdx + (SYMR *) fh->isymBase;
	    char *name = (char*)sh->iss;
	    CORE_ADDR valu = sh->value;
	    if (MIPS_IS_STAB(sh)) {
		int type_code = MIPS_UNMARK_STAB(sh->index);
		process_one_symbol (type_code, 0, valu, name,
				    pst->section_offsets, pst->objfile);
		if (type_code == N_FUN) {
		    /* Make up special symbol to contain
		       procedure specific info */
		    struct mips_extra_func_info *e =
		      (struct mips_extra_func_info *)
			obstack_alloc(&current_objfile->symbol_obstack,
				      sizeof(struct mips_extra_func_info));
		    struct symbol *s = new_symbol(MIPS_EFI_SYMBOL_NAME);
		    SYMBOL_NAMESPACE(s) = LABEL_NAMESPACE;
		    SYMBOL_CLASS(s) = LOC_CONST;
		    SYMBOL_TYPE(s) = builtin_type_void;
		    SYMBOL_VALUE(s) = (int)e;
		    add_symbol_to_list (s, &local_symbols);
		}
	    }
	    else if (sh->st == stLabel && sh->index != indexNil) {
		/* Handle encoded stab line number. */
		record_line (current_subfile, sh->index, valu);
	    }
	    else complain (&stab_unknown_complaint, (char *)sh->iss);
	}
	st = end_symtab (pst->texthigh, 0, 0, pst->objfile);
	end_stabs ();

	/* Sort the symbol table now, we are done adding symbols to it.
	   We must do this before parse_procedure calls lookup_symbol.  */
	sort_symtab_syms(st);

	/* This may not be necessary for stabs symtabs.  FIXME.  */
	sort_blocks (st);

	/* Fill in procedure info next.  We need to look-ahead to
	   find out where each procedure's code ends.  */

	for (i = 0; i <= fh->cpd-1; i++) {
	    pr = (PDR *) (IPDFIRST(cur_hdr, fh)) + i;
	    parse_procedure (pr, i < fh->cpd-1 ? pr[1].adr : bound, 1);
	}
    } else {

	/*
	 * This symbol table contains ordinary ecoff entries.
	 */

	/* FIXME:  doesn't use pst->section_offsets.  */

	int f_max;
	int maxlines;
	EXTR **ext_ptr;

	processing_gcc_compilation = 0;

	/* How many symbols will we need */
	/* FIXME, this does not count enum values. */
	f_max = pst->n_global_syms + pst->n_static_syms;
	if (fh == 0) {
	    maxlines = 0;
	    st = new_symtab ("unknown", f_max, 0, pst->objfile);
	} else {
	    f_max += fh->csym + fh->cpd;
	    maxlines = 2 * fh->cline;
	    st = new_symtab (pst->filename, 2 * f_max, maxlines, pst->objfile);
	}

	lines = LINETABLE(st);
	pending_list = (struct mips_pending **) cur_hdr->cbOptOffset;
	if (pending_list == 0) {
	    pending_list = (struct mips_pending **)
		xzalloc(cur_hdr->ifdMax * sizeof(struct mips_pending *));
	    cur_hdr->cbOptOffset = (int)pending_list;
	}

	/* Get a new lexical context */

	push_parse_stack();
	top_stack->cur_st = st;
	top_stack->cur_block = BLOCKVECTOR_BLOCK(BLOCKVECTOR(st),
						 STATIC_BLOCK);
	BLOCK_START(top_stack->cur_block) = fh ? fh->adr : 0;
	BLOCK_END(top_stack->cur_block) = 0;
	top_stack->blocktype = stFile;
	top_stack->maxsyms = 2*f_max;
	top_stack->cur_type = 0;
	top_stack->procadr = 0;
	top_stack->numargs = 0;

	if (fh) {
	    SYMR *sh;
	    PDR *pr;

	    /* Parse local symbols first */

	    for (cur_sdx = 0; cur_sdx < fh->csym; ) {
		sh = (SYMR *) (fh->isymBase) + cur_sdx;
		cur_sdx += parse_symbol(sh, (union aux_ext *)fh->iauxBase,
					fh->fBigendian);
	    }

	    /* Linenumbers.  At the end, check if we can save memory */

	    parse_lines(fh, lines);
	    if (lines->nitems < fh->cline)
		lines = shrink_linetable(lines);

	    /* Fill in procedure info next.  We need to look-ahead to
	       find out where each procedure's code ends.  */

	    for (i = 0; i <= fh->cpd-1; i++) {
		pr = (PDR *) (IPDFIRST(cur_hdr, fh)) + i;
		parse_procedure(pr, i < fh->cpd-1 ? pr[1].adr : bound, 0);
	    }
	}

	LINETABLE(st) = lines;

	/* .. and our share of externals.
	   XXX use the global list to speed up things here. how?
	   FIXME, Maybe quit once we have found the right number of ext's? */
	top_stack->cur_st = st;
	top_stack->cur_block = BLOCKVECTOR_BLOCK(BLOCKVECTOR(top_stack->cur_st),
						 GLOBAL_BLOCK);
	top_stack->blocktype = stFile;
	top_stack->maxsyms =
	    cur_hdr->isymMax + cur_hdr->ipdMax + cur_hdr->iextMax;

	ext_ptr = PST_PRIVATE(pst)->extern_tab;
	for (i = PST_PRIVATE(pst)->extern_count; --i >= 0; ext_ptr++)
	    parse_external(*ext_ptr, 1, fh->fBigendian);

	/* If there are undefined, tell the user */
	if (n_undef_symbols) {
	    printf_filtered("File %s contains %d unresolved references:",
			    st->filename, n_undef_symbols);
	    printf_filtered("\n\t%4d variables\n\t%4d procedures\n\t%4d labels\n",
			    n_undef_vars, n_undef_procs, n_undef_labels);
	    n_undef_symbols = n_undef_labels = n_undef_vars = n_undef_procs = 0;

	}
	pop_parse_stack();

	/* Sort the symbol table now, we are done adding symbols to it.*/
	sort_symtab_syms(st);

	sort_blocks (st);
    }

    /* Now link the psymtab and the symtab.  */
    pst->symtab = st;

    current_objfile = NULL;
}

/* Ancillary parsing procedures. */

/* Lookup the type at relative index RN.  Return it in TPP
   if found and in any event come up with its name PNAME.
   BIGEND says whether aux symbols are big-endian or not (from fh->fBigendian).
   Return value says how many aux symbols we ate. */

static int
cross_ref(ax, tpp, type_code, pname, bigend)
     union aux_ext *ax;
     struct type **tpp;
     enum type_code type_code; /* Use to alloc new type if none is found. */
     char **pname;
     int bigend;
{
	RNDXR		rn[1];
	unsigned        rf;
	int		result = 1;

	ecoff_swap_rndx_in (bigend, ax, rn);

	/* Escape index means 'the next one' */
	if (rn->rfd == 0xfff) {
		result++;
		rf = AUX_GET_ISYM (bigend, ax + 1);
	} else {
		rf = rn->rfd;
	}

	if (rf == -1) {
		/* Ooops */
		*pname = "<undefined>";
	} else {
		/*
		 * Find the relative file descriptor and the symbol in it
		 */
		FDR            *fh = get_rfd(cur_fd, rf);
		SYMR           *sh;
		struct type    *t;

		/*
		 * If we have processed this symbol then we left a forwarding
		 * pointer to the corresponding GDB symbol.  If not, we`ll put
		 * it in a list of pending symbols, to be processed later when
		 * the file f will be.  In any event, we collect the name for
		 * the type here. Which is why we made a first pass at
		 * strings.
		 */
		sh = (SYMR *) (fh->isymBase) + rn->index;

		/* Careful, we might be looking at .o files */
		*pname = (UNSAFE_DATA_ADDR(sh->iss)) ? "<undefined>" :
			(char *) sh->iss;

		/* Have we parsed it ? */
		if ((!UNSAFE_DATA_ADDR(sh->value)) && (sh->st == stParsed)) {
			t = (struct type *) sh->value;
			*tpp = t;
		} else {
		    /* Avoid duplicates */
		    struct mips_pending *p = is_pending_symbol(fh, sh);
		    if (p)
			*tpp = p->t;
		    else {
			*tpp = init_type(type_code, 0, 0, (char *) NULL,
					 (struct objfile *) NULL);
			add_pending(fh, sh, *tpp);
		    }
		}
	}

	/* We used one auxent normally, two if we got a "next one" rf. */
	return result;
}


/* Quick&dirty lookup procedure, to avoid the MI ones that require
   keeping the symtab sorted */

static struct symbol *
mylookup_symbol (name, block, namespace, class)
     char *name;
     register struct block *block;
     enum namespace namespace;
     enum address_class class;
{
	register int    bot, top, inc;
	register struct symbol *sym;

	bot = 0;
	top = BLOCK_NSYMS(block);
	inc = name[0];
	while (bot < top) {
		sym = BLOCK_SYM(block, bot);
		if (SYMBOL_NAME(sym)[0] == inc
		    && SYMBOL_NAMESPACE(sym) == namespace
		    && SYMBOL_CLASS(sym) == class
		    && !strcmp(SYMBOL_NAME(sym), name))
			return sym;
		bot++;
	}
	block = BLOCK_SUPERBLOCK (block);
	if (block)
		return mylookup_symbol (name, block, namespace, class);
	return 0;
}


/* Add a new symbol S to a block B.
   Infrequently, we will need to reallocate the block to make it bigger.
   We only detect this case when adding to top_stack->cur_block, since
   that's the only time we know how big the block is.  FIXME.  */

static void
add_symbol(s,b)
	struct symbol *s;
	struct block *b;
{
	int nsyms = BLOCK_NSYMS(b)++;
	struct block *origb;
	struct parse_stack *stackp;

	if (b == top_stack->cur_block &&
	    nsyms >= top_stack->maxsyms) {
		complain (&block_overflow_complaint, s->name);
		/* In this case shrink_block is actually grow_block, since
		   BLOCK_NSYMS(b) is larger than its current size.  */
		origb = b;
		b = shrink_block (top_stack->cur_block, top_stack->cur_st);

		/* Now run through the stack replacing pointers to the
		   original block.  shrink_block has already done this
		   for the blockvector and BLOCK_FUNCTION.  */
		for (stackp = top_stack; stackp; stackp = stackp->next) {
			if (stackp->cur_block == origb) {
				stackp->cur_block = b;
				stackp->maxsyms = BLOCK_NSYMS (b);
			}
		}
	}
	BLOCK_SYM(b,nsyms) = s;
}

/* Add a new block B to a symtab S */

static void
add_block(b,s)
	struct block *b;
	struct symtab *s;
{
	struct blockvector *bv = BLOCKVECTOR(s);

	bv = (struct blockvector *)xrealloc((PTR) bv,
					    sizeof(struct blockvector) +
					         BLOCKVECTOR_NBLOCKS(bv)
						 * sizeof(bv->block));
	if (bv != BLOCKVECTOR(s))
		BLOCKVECTOR(s) = bv;

	BLOCKVECTOR_BLOCK(bv, BLOCKVECTOR_NBLOCKS(bv)++) = b;
}

/* Add a new linenumber entry (LINENO,ADR) to a linevector LT.
   MIPS' linenumber encoding might need more than one byte
   to describe it, LAST is used to detect these continuation lines */

static int
add_line(lt, lineno, adr, last)
	struct linetable *lt;
	int lineno;
	CORE_ADDR adr;
	int last;
{
	if (last == 0)
		last = -2;	/* make sure we record first line */

	if (last == lineno)	/* skip continuation lines */
		return lineno;

	lt->item[lt->nitems].line = lineno;
	lt->item[lt->nitems++].pc = adr << 2;
	return lineno;
}

/* Sorting and reordering procedures */

/* Blocks with a smaller low bound should come first */

static int
compare_blocks(arg1, arg2)
     const void *arg1, *arg2;
{
	register int addr_diff;
	struct block **b1 = (struct block **) arg1;
	struct block **b2 = (struct block **) arg2;

	addr_diff = (BLOCK_START((*b1))) - (BLOCK_START((*b2)));
	if (addr_diff == 0)
		return (BLOCK_END((*b1))) - (BLOCK_END((*b2)));
	return addr_diff;
}

/* Sort the blocks of a symtab S.
   Reorder the blocks in the blockvector by code-address,
   as required by some MI search routines */

static void
sort_blocks(s)
	struct symtab *s;
{
	struct blockvector *bv = BLOCKVECTOR(s);

	if (BLOCKVECTOR_NBLOCKS(bv) <= 2) {
		/* Cosmetic */
		if (BLOCK_END(BLOCKVECTOR_BLOCK(bv,GLOBAL_BLOCK)) == 0)
			BLOCK_START(BLOCKVECTOR_BLOCK(bv,GLOBAL_BLOCK)) = 0;
		if (BLOCK_END(BLOCKVECTOR_BLOCK(bv,STATIC_BLOCK)) == 0)
			BLOCK_START(BLOCKVECTOR_BLOCK(bv,STATIC_BLOCK)) = 0;
		return;
	}
	/*
	 * This is very unfortunate: normally all functions are compiled in
	 * the order they are found, but if the file is compiled -O3 things
	 * are very different.  It would be nice to find a reliable test
	 * to detect -O3 images in advance.
	 */
	if (BLOCKVECTOR_NBLOCKS(bv) > 3)
		qsort(&BLOCKVECTOR_BLOCK(bv,FIRST_LOCAL_BLOCK),
		      BLOCKVECTOR_NBLOCKS(bv) - FIRST_LOCAL_BLOCK,
		      sizeof(struct block *),
		      compare_blocks);

	{
		register CORE_ADDR high = 0;
		register int    i, j = BLOCKVECTOR_NBLOCKS(bv);

		for (i = FIRST_LOCAL_BLOCK; i < j; i++)
			if (high < BLOCK_END(BLOCKVECTOR_BLOCK(bv,i)))
				high = BLOCK_END(BLOCKVECTOR_BLOCK(bv,i));
		BLOCK_END(BLOCKVECTOR_BLOCK(bv,GLOBAL_BLOCK)) = high;
	}

	BLOCK_START(BLOCKVECTOR_BLOCK(bv,GLOBAL_BLOCK)) =
		BLOCK_START(BLOCKVECTOR_BLOCK(bv,FIRST_LOCAL_BLOCK));

	BLOCK_START(BLOCKVECTOR_BLOCK(bv,STATIC_BLOCK)) =
		BLOCK_START(BLOCKVECTOR_BLOCK(bv,GLOBAL_BLOCK));
	BLOCK_END  (BLOCKVECTOR_BLOCK(bv,STATIC_BLOCK)) =
		BLOCK_END  (BLOCKVECTOR_BLOCK(bv,GLOBAL_BLOCK));
}


/* Constructor/restructor/destructor procedures */

/* Allocate a new symtab for NAME.  Needs an estimate of how many symbols
   MAXSYMS and linenumbers MAXLINES we'll put in it */

static struct symtab *
new_symtab(name, maxsyms, maxlines, objfile)
	char *name;
	int maxsyms;
	int maxlines;
	struct objfile *objfile;
{
  struct symtab *s = allocate_symtab (name, objfile);

  LINETABLE(s) = new_linetable(maxlines);

  /* All symtabs must have at least two blocks */
  BLOCKVECTOR(s) = new_bvect(2);
  BLOCKVECTOR_BLOCK(BLOCKVECTOR(s), GLOBAL_BLOCK) = new_block(maxsyms);
  BLOCKVECTOR_BLOCK(BLOCKVECTOR(s), STATIC_BLOCK) = new_block(maxsyms);
  BLOCK_SUPERBLOCK( BLOCKVECTOR_BLOCK(BLOCKVECTOR(s),STATIC_BLOCK)) =
    BLOCKVECTOR_BLOCK(BLOCKVECTOR(s), GLOBAL_BLOCK);

  s->free_code = free_linetable;

  return (s);
}

/* Allocate a new partial_symtab NAME */

static struct partial_symtab *
new_psymtab(name, objfile)
	char *name;
	struct objfile *objfile;
{
  struct partial_symtab *psymtab;

  /* FIXME -- why (char *) -1 rather than NULL? */
  psymtab = allocate_psymtab (name == (char *) -1 ? "<no name>" : name,
			      objfile);

  /* Keep a backpointer to the file's symbols */

  psymtab -> read_symtab_private = (char *)
    obstack_alloc (&objfile->psymbol_obstack, sizeof (struct symloc));
  CUR_HDR(psymtab) = cur_hdr;

  /* The way to turn this into a symtab is to call... */
  psymtab->read_symtab = mipscoff_psymtab_to_symtab;
  return (psymtab);
}


/* Allocate a linetable array of the given SIZE.  Since the struct
   already includes one item, we subtract one when calculating the
   proper size to allocate.  */

static struct linetable *
new_linetable(size)
	int size;
{
	struct linetable *l;

	size = (size-1) * sizeof(l->item) + sizeof(struct linetable);
	l = (struct linetable *)xmalloc(size);
	l->nitems = 0;
	return l;
}

/* Oops, too big. Shrink it.  This was important with the 2.4 linetables,
   I am not so sure about the 3.4 ones.

   Since the struct linetable already includes one item, we subtract one when
   calculating the proper size to allocate.  */

static struct linetable *
shrink_linetable(lt)
	struct linetable * lt;
{

	return (struct linetable *) xrealloc ((PTR)lt,
					sizeof(struct linetable)
					+ (lt->nitems - 1) * sizeof(lt->item));
}

/* Allocate and zero a new blockvector of NBLOCKS blocks. */

static struct blockvector *
new_bvect(nblocks)
	int nblocks;
{
	struct blockvector *bv;
	int size;

	size = sizeof(struct blockvector) + nblocks * sizeof(struct block*);
	bv = (struct blockvector *) xzalloc(size);

	BLOCKVECTOR_NBLOCKS(bv) = nblocks;

	return bv;
}

/* Allocate and zero a new block of MAXSYMS symbols */

static struct block *
new_block(maxsyms)
	int maxsyms;
{
	int size = sizeof(struct block) + (maxsyms-1) * sizeof(struct symbol *);

	return (struct block *)xzalloc (size);
}

/* Ooops, too big. Shrink block B in symtab S to its minimal size.
   Shrink_block can also be used by add_symbol to grow a block.  */

static struct block *
shrink_block(b, s)
	struct block *b;
	struct symtab *s;
{
	struct block *new;
	struct blockvector *bv = BLOCKVECTOR(s);
	int i;

	/* Just reallocate it and fix references to the old one */

	new = (struct block *) xrealloc ((PTR)b, sizeof(struct block) +
		(BLOCK_NSYMS(b)-1) * sizeof(struct symbol *));

	/* Should chase pointers to old one.  Fortunately, that`s just
	   the block`s function and inferior blocks */
	if (BLOCK_FUNCTION(new) && SYMBOL_BLOCK_VALUE(BLOCK_FUNCTION(new)) == b)
		SYMBOL_BLOCK_VALUE(BLOCK_FUNCTION(new)) = new;
	for (i = 0; i < BLOCKVECTOR_NBLOCKS(bv); i++)
		if (BLOCKVECTOR_BLOCK(bv,i) == b)
			BLOCKVECTOR_BLOCK(bv,i) = new;
		else if (BLOCK_SUPERBLOCK(BLOCKVECTOR_BLOCK(bv,i)) == b)
			BLOCK_SUPERBLOCK(BLOCKVECTOR_BLOCK(bv,i)) = new;
	return new;
}

/* Create a new symbol with printname NAME */

static struct symbol *
new_symbol(name)
	char *name;
{
	struct symbol *s = (struct symbol *)
		obstack_alloc (&current_objfile->symbol_obstack, sizeof (struct symbol));

	memset ((PTR)s, 0, sizeof (*s));
	SYMBOL_NAME(s) = name;
	return s;
}

/* Create a new type with printname NAME */

static struct type *
new_type(name)
	char *name;
{
	struct type *t;

	t = alloc_type (current_objfile);
	TYPE_NAME(t) = name;
	TYPE_CPLUS_SPECIFIC(t) = (struct cplus_struct_type *)
				 &cplus_struct_default;
	return t;
}


/* Things used for calling functions in the inferior.
   These functions are exported to our companion
   mips-tdep.c file and are here because they play
   with the symbol-table explicitly. */

/* Sigtramp: make sure we have all the necessary information
   about the signal trampoline code. Since the official code
   from MIPS does not do so, we make up that information ourselves.
   If they fix the library (unlikely) this code will neutralize itself. */

static void
fixup_sigtramp()
{
	struct symbol  *s;
	struct symtab  *st;
	struct block   *b, *b0;

	sigtramp_address = -1;

	/* We know it is sold as sigvec */
	s = lookup_symbol("sigvec", 0, VAR_NAMESPACE, 0, NULL);

	/* Most programs do not play with signals */
	if (s == 0)
	  s = lookup_symbol("_sigtramp", 0, VAR_NAMESPACE, 0, NULL);
	else
	  {
	    b0 = SYMBOL_BLOCK_VALUE(s);

	    /* A label of sigvec, to be more precise */
	    s = lookup_symbol("sigtramp", b0, VAR_NAMESPACE, 0, NULL);
	  }

	/* But maybe this program uses its own version of sigvec */
	if (s == 0)
		return;

	/* Did we or MIPSco fix the library ? */
	if (SYMBOL_CLASS(s) == LOC_BLOCK)
	  {
	    sigtramp_address = BLOCK_START(SYMBOL_BLOCK_VALUE(s));
	    sigtramp_end = BLOCK_END(SYMBOL_BLOCK_VALUE(s));
	    return;
	  }

	sigtramp_address = SYMBOL_VALUE(s);
	sigtramp_end = sigtramp_address + 0x88;	/* black magic */

	/* But what symtab does it live in ? */
	st = find_pc_symtab(SYMBOL_VALUE(s));

	/*
	 * Ok, there goes the fix: turn it into a procedure, with all the
	 * needed info.  Note we make it a nested procedure of sigvec,
	 * which is the way the (assembly) code is actually written.
	 */
	SYMBOL_NAMESPACE(s) = VAR_NAMESPACE;
	SYMBOL_CLASS(s) = LOC_BLOCK;
	SYMBOL_TYPE(s) = init_type(TYPE_CODE_FUNC, 4, 0, (char *) NULL,
				   (struct objfile *) NULL);
	TYPE_TARGET_TYPE(SYMBOL_TYPE(s)) = builtin_type_void;

	/* Need a block to allocate MIPS_EFI_SYMBOL_NAME in */
	b = new_block(1);
	SYMBOL_BLOCK_VALUE(s) = b;
	BLOCK_START(b) = sigtramp_address;
	BLOCK_END(b) = sigtramp_end;
	BLOCK_FUNCTION(b) = s;
	BLOCK_SUPERBLOCK(b) = BLOCK_SUPERBLOCK(b0);
	add_block(b, st);
	sort_blocks(st);

	/* Make a MIPS_EFI_SYMBOL_NAME entry for it */
	{
		struct mips_extra_func_info *e =
			(struct mips_extra_func_info *)
			xzalloc(sizeof(struct mips_extra_func_info));

		e->numargs = 0;	/* the kernel thinks otherwise */
		/* align_longword(sigcontext + SIGFRAME) */
		e->pdr.frameoffset = 0x150;
		e->pdr.framereg = SP_REGNUM;
		e->pdr.pcreg = 31;
		e->pdr.regmask = -2;
		e->pdr.regoffset = -(41 * sizeof(int));
		e->pdr.fregmask = -1;
		e->pdr.fregoffset = -(37 * sizeof(int));
		e->pdr.isym = (long)s;

		current_objfile = st->objfile; /* Keep new_symbol happy */
		s = new_symbol(MIPS_EFI_SYMBOL_NAME);
		SYMBOL_VALUE(s) = (int) e;
		SYMBOL_NAMESPACE(s) = LABEL_NAMESPACE;
		SYMBOL_CLASS(s) = LOC_CONST;
		SYMBOL_TYPE(s) = builtin_type_void;
		current_objfile = NULL;
	}

	BLOCK_SYM(b,BLOCK_NSYMS(b)++) = s;
}


/* Fake up identical offsets for all sections.  */

struct section_offsets *
mipscoff_symfile_offsets (objfile, addr)
     struct objfile *objfile;
     CORE_ADDR addr;
{
  struct section_offsets *section_offsets;
  int i;

  section_offsets = (struct section_offsets *)
    obstack_alloc (&objfile -> psymbol_obstack,
		   sizeof (struct section_offsets) +
		          sizeof (section_offsets->offsets) * (SECT_OFF_MAX-1));

  for (i = 0; i < SECT_OFF_MAX; i++)
    ANOFFSET (section_offsets, i) = addr;

  return section_offsets;
}

/* Initialization */

static struct sym_fns ecoff_sym_fns =
{
  "ecoff",		/* sym_name: name or name prefix of BFD target type */
  5,			/* sym_namelen: number of significant sym_name chars */
  mipscoff_new_init,	/* sym_new_init: init anything gbl to entire symtab */
  mipscoff_symfile_init,/* sym_init: read initial info, setup for sym_read() */
  mipscoff_symfile_read,/* sym_read: read a symbol file into symtab */
  mipscoff_symfile_finish,/* sym_finish: finished with file, cleanup */
  mipscoff_symfile_offsets,/* sym_offsets: dummy FIXME til implem sym reloc */
  NULL			/* next: pointer to next struct sym_fns */
};


void
_initialize_mipsread ()
{
	add_symtab_fns (&ecoff_sym_fns);

	/* Missing basic types */

	builtin_type_string =
	    init_type(TYPE_CODE_PASCAL_ARRAY,
		      TARGET_CHAR_BIT / TARGET_CHAR_BIT,
		      0, "string",
		      (struct objfile *) NULL);
	builtin_type_complex =
	    init_type(TYPE_CODE_FLT,
		      TARGET_COMPLEX_BIT / TARGET_CHAR_BIT,
		      0, "complex",
		      (struct objfile *) NULL);
	builtin_type_double_complex =
	    init_type(TYPE_CODE_FLT,
		      TARGET_DOUBLE_COMPLEX_BIT / TARGET_CHAR_BIT,
		      0, "double complex",
		      (struct objfile *) NULL);
	builtin_type_fixed_dec =
	    init_type(TYPE_CODE_INT,
		      TARGET_INT_BIT / TARGET_CHAR_BIT,
		      0, "fixed decimal",
		      (struct objfile *) NULL);
	builtin_type_float_dec =
	    init_type(TYPE_CODE_FLT,
		      TARGET_DOUBLE_BIT / TARGET_CHAR_BIT,
		      0, "floating decimal",
		      (struct objfile *) NULL);
}
