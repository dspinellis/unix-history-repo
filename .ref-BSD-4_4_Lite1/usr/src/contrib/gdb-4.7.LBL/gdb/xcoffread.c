/* Read AIX xcoff symbol tables and convert to internal format, for GDB.
   Copyright 1986, 1987, 1988, 1989, 1990, 1991, 1992
   	     Free Software Foundation, Inc.
   Derived from coffread.c, dbxread.c, and a lot of hacking.
   Contributed by IBM Corporation.

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

#include "defs.h"
#include "bfd.h"

#if defined(IBM6000_HOST) && defined(IBM6000_TARGET)
/* Native only:  Need struct tbtable in <sys/debug.h> from host, and 
		 need xcoff_add_toc_to_loadinfo in rs6000-tdep.c from target. */

/* AIX XCOFF names have a preceeding dot `.' */
#define NAMES_HAVE_DOT 1

#include <sys/types.h>
#include <fcntl.h>
#include <ctype.h>

#include "obstack.h"
#include <sys/param.h>
#ifndef	NO_SYS_FILE
#include <sys/file.h>
#endif
#include <sys/stat.h>
#include <sys/debug.h>

#include "symtab.h"
#include "gdbtypes.h"
#include "symfile.h"
#include "objfiles.h"
#include "buildsym.h"
#include "stabsread.h"
#include "gdb-stabs.h"

#include "coff/internal.h"	/* FIXME, internal data from BFD */
#include "libcoff.h"		/* FIXME, internal data from BFD */
#include "coff/rs6000.h"	/* FIXME, raw file-format guts of xcoff */


/* Define this if you want gdb use the old xcoff symbol processing. This
   way it won't use common `define_symbol()' function and Sun dbx stab
   string grammar. And likely it won't be able to do G++ debugging. */

/* #define	NO_DEFINE_SYMBOL 1 */

/* Define this if you want gdb to ignore typdef stabs. This was needed for
   one of Transarc, to reduce the size of the symbol table. Types won't be
   recognized, but tag names will be. */

/* #define	NO_TYPEDEFS  1 */

/* Simplified internal version of coff symbol table information */

struct coff_symbol {
  char *c_name;
  int c_symnum;		/* symbol number of this entry */
  int c_nsyms;		/* 0 if syment only, 1 if syment + auxent */
  long c_value;
  int c_sclass;
  int c_secnum;
  unsigned int c_type;
};

/* The COFF line table, in raw form.  */
static char *linetab = NULL;		/* Its actual contents */
static long linetab_offset;		/* Its offset in the file */
static unsigned long linetab_size;	/* Its size */

/* last function's saved coff symbol `cs' */

static struct coff_symbol fcn_cs_saved;

static bfd *symfile_bfd;

/* Core address of start and end of text of current source file.
   This is calculated from the first function seen after a C_FILE
   symbol. */


static CORE_ADDR cur_src_end_addr;

/* Core address of the end of the first object file.  */

static CORE_ADDR first_object_file_end;

/* pointer to the string table */
static char *strtbl;

/* length of the string table */
static int  strtbl_len;

/* pointer to debug section */
static char *debugsec;

/* pointer to the a.out symbol table */
static char *symtbl;

/* initial symbol-table-debug-string vector length */

#define	INITIAL_STABVECTOR_LENGTH	40

/* Nonzero if within a function (so symbols should be local,
   if nothing says specifically).  */

int within_function;

/* Local variables that hold the shift and mask values for the
   COFF file that we are currently reading.  These come back to us
   from BFD, and are referenced by their macro names, as well as
   internally to the BTYPE, ISPTR, ISFCN, ISARY, ISTAG, and DECREF
   macros from ../internalcoff.h .  */

static unsigned	local_n_btshft;
static unsigned	local_n_tmask;

#undef	N_BTSHFT
#define	N_BTSHFT	local_n_btshft
#undef	N_TMASK
#define	N_TMASK		local_n_tmask
 
/* Local variables that hold the sizes in the file of various COFF structures.
   (We only need to know this to read them from the file -- BFD will then
   translate the data in them, into `internal_xxx' structs in the right
   byte order, alignment, etc.)  */

static unsigned	local_symesz;


/* coff_symfile_init()
   is the coff-specific initialization routine for reading symbols.
   It is passed a struct sym_fns which contains, among other things,
   the BFD for the file whose symbols are being read, and a slot for
   a pointer to "private data" which we fill with cookies and other
   treats for coff_symfile_read().
 
   We will only be called if this is a COFF or COFF-like file.
   BFD handles figuring out the format of the file, and code in symtab.c
   uses BFD's determination to vector to us.
 
   The ultimate result is a new symtab (or, FIXME, eventually a psymtab).  */

struct coff_symfile_info {
  file_ptr min_lineno_offset;		/* Where in file lowest line#s are */
  file_ptr max_lineno_offset;		/* 1+last byte of line#s in file */
};


static void
enter_line_range PARAMS ((struct subfile *, unsigned, unsigned,
			  CORE_ADDR, CORE_ADDR, unsigned *));

static void
free_debugsection PARAMS ((void));

static int
init_debugsection PARAMS ((bfd *));

static int
init_stringtab PARAMS ((bfd *, file_ptr, struct objfile *));

static void
xcoff_symfile_init PARAMS ((struct objfile *));

static void
xcoff_new_init PARAMS ((struct objfile *));

#ifdef __STDC__
struct section_offset;
#endif

static void
xcoff_symfile_read PARAMS ((struct objfile *, struct section_offset *, int));

static void
xcoff_symfile_finish PARAMS ((struct objfile *));

static struct section_offsets *
xcoff_symfile_offsets PARAMS ((struct objfile *, CORE_ADDR));

static int
init_lineno PARAMS ((bfd *, file_ptr, int));

static void
find_linenos PARAMS ((bfd *, sec_ptr, PTR));

static int
read_symbol_lineno PARAMS ((char *, int));

static int
read_symbol_nvalue PARAMS ((char *, int));

static struct symbol *
process_xcoff_symbol PARAMS ((struct coff_symbol *, struct objfile *));

static void
read_xcoff_symtab PARAMS ((struct objfile *, int));

static void
add_stab_to_list PARAMS ((char *, struct pending_stabs **));

static void
sort_syms PARAMS ((void));

static int
compare_symbols PARAMS ((const void *, const void *));

/* Call sort_syms to sort alphabetically
   the symbols of each block of each symtab.  */

static int
compare_symbols (s1p, s2p)
     const PTR s1p;
     const PTR s2p;
{
  /* Names that are less should come first.  */
  register struct symbol **s1 = (struct symbol **) s1p;
  register struct symbol **s2 = (struct symbol **) s2p;
  register int namediff = strcmp (SYMBOL_NAME (*s1), SYMBOL_NAME (*s2));
  if (namediff != 0) 
    return namediff;

  /* For symbols of the same name, registers should come first.  */
  return ((SYMBOL_CLASS (*s2) == LOC_REGISTER)
      - (SYMBOL_CLASS (*s1) == LOC_REGISTER));
}


/* Sort a vector of symbols by their value. */

static void
sort_syms ()
{
  register struct symtab *s;
  register struct objfile *objfile;
  register int i, nbl;
  register struct blockvector *bv;
  register struct block *b;

  for (objfile = object_files; objfile != NULL; objfile = objfile -> next)
    {
      for (s = objfile -> symtabs; s != NULL; s = s -> next)
	{
	  bv = BLOCKVECTOR (s);
	  nbl = BLOCKVECTOR_NBLOCKS (bv);
	  for (i = 0; i < nbl; i++)
	    {
	      b = BLOCKVECTOR_BLOCK (bv, i);
	      if (BLOCK_SHOULD_SORT (b))
		{
		  qsort (&BLOCK_SYM (b, 0), BLOCK_NSYMS (b),
			 sizeof (struct symbol *), compare_symbols);
		}
	    }
	}
    }
}


/* add a given stab string into given stab vector. */

static void
add_stab_to_list (stabname, stabvector)
char *stabname;
struct pending_stabs **stabvector;
{
  if ( *stabvector == NULL) {
    *stabvector = (struct pending_stabs *)
	xmalloc (sizeof (struct pending_stabs) + 
			INITIAL_STABVECTOR_LENGTH * sizeof (char*));
    (*stabvector)->count = 0;
    (*stabvector)->length = INITIAL_STABVECTOR_LENGTH;
  }
  else if ((*stabvector)->count >= (*stabvector)->length) {
    (*stabvector)->length += INITIAL_STABVECTOR_LENGTH;
    *stabvector = (struct pending_stabs *)
	xrealloc ((char *) *stabvector, sizeof (struct pending_stabs) + 
	(*stabvector)->length * sizeof (char*));
  }
  (*stabvector)->stab [(*stabvector)->count++] = stabname;
}


#if 0
/* for all the stabs in a given stab vector, build appropriate types 
   and fix their symbols in given symbol vector. */

void
patch_block_stabs (symbols, stabs)
struct pending *symbols;
struct pending_stabs *stabs;
{
  int ii;

  if (!stabs)
    return;

  /* for all the stab entries, find their corresponding symbols and 
     patch their types! */

  for (ii=0; ii < stabs->count; ++ii) {
    char *name = stabs->stab[ii];
    char *pp = (char*) index (name, ':');
    struct symbol *sym = find_symbol_in_list (symbols, name, pp-name);
    if (!sym) {
      ;
      /* printf ("ERROR! stab symbol not found!\n"); */	/* FIXME */
      /* The above is a false alarm. There are cases the we can have
         a stab, without its symbol. xlc generates this for the extern
	 definitions in inner blocks. */
    }
    else {
      pp += 2;

      if (*(pp-1) == 'F' || *(pp-1) == 'f')
	SYMBOL_TYPE (sym) = lookup_function_type (read_type (&pp));
      else
	SYMBOL_TYPE (sym) = read_type (&pp, objfile);
    }
  }
}
#endif


/* compare line table entry addresses. */

  static int
compare_lte (lte1, lte2)
  struct linetable_entry *lte1, *lte2;
{
  return lte1->pc - lte2->pc;
}

/* Give a line table with function entries are marked, arrange its functions
   in assending order and strip off function entry markers and return it in
   a newly created table. If the old one is good enough, return the old one. */

static struct linetable *
arrange_linetable (oldLineTb)
  struct linetable *oldLineTb;			/* old linetable */
{
  int ii, jj, 
      newline, 					/* new line count */
      function_count;				/* # of functions */

  struct linetable_entry *fentry;		/* function entry vector */
  int fentry_size;				/* # of function entries */
  struct linetable *newLineTb;			/* new line table */

#define NUM_OF_FUNCTIONS 20

  fentry_size = NUM_OF_FUNCTIONS;
  fentry = (struct linetable_entry*)
	malloc (fentry_size * sizeof (struct linetable_entry));

  for (function_count=0, ii=0; ii <oldLineTb->nitems; ++ii) {

    if (oldLineTb->item[ii].line == 0) {	/* function entry found. */

      if (function_count >= fentry_size) {	/* make sure you have room. */
	fentry_size *= 2;
	fentry = (struct linetable_entry*) 
	   realloc (fentry, fentry_size * sizeof (struct linetable_entry));
      }
      fentry[function_count].line = ii;
      fentry[function_count].pc = oldLineTb->item[ii].pc;
      ++function_count;
    }
  }

  if (function_count == 0) {
    free (fentry);
    return oldLineTb;
  }
  else if (function_count > 1)
    qsort (fentry, function_count, sizeof(struct linetable_entry), compare_lte);

  /* allocate a new line table. */
  newLineTb = (struct linetable*) malloc (sizeof (struct linetable) + 
	(oldLineTb->nitems - function_count) * sizeof (struct linetable_entry));

  /* if line table does not start with a function beginning, copy up until
     a function begin. */

  newline = 0;
  if (oldLineTb->item[0].line != 0)
    for (newline=0; 
	newline < oldLineTb->nitems && oldLineTb->item[newline].line; ++newline)
      newLineTb->item[newline] = oldLineTb->item[newline];

  /* Now copy function lines one by one. */

  for (ii=0; ii < function_count; ++ii) {
    for (jj = fentry[ii].line + 1;
	         jj < oldLineTb->nitems && oldLineTb->item[jj].line != 0; 
							 ++jj, ++newline)
      newLineTb->item[newline] = oldLineTb->item[jj];
  }
  free (fentry);
  newLineTb->nitems = oldLineTb->nitems - function_count;
  return newLineTb;  
}     



/* We try to detect the beginning of a compilation unit. That info will
   be used as an entry in line number recording routines (enter_line_range) */

static unsigned first_fun_line_offset;
static unsigned first_fun_bf;

#define mark_first_line(OFFSET, SYMNUM) \
  if (!first_fun_line_offset) {         \
    first_fun_line_offset = OFFSET;     \
    first_fun_bf = SYMNUM;              \
  }
  

/* include file support: C_BINCL/C_EINCL pairs will be kept in the 
   following `IncludeChain'. At the end of each symtab (end_symtab),
   we will determine if we should create additional symtab's to
   represent if (the include files. */


typedef struct _inclTable {
  char		*name;				/* include filename */
  int		begin, end;			/* offsets to the line table */
  struct subfile *subfile;
  unsigned	funStartLine;			/* start line # of its function */
} InclTable;

#define	INITIAL_INCLUDE_TABLE_LENGTH	20
static InclTable  *inclTable;			/* global include table */
static int	  inclIndx;			/* last entry to table */
static int	  inclLength;			/* table length */
static int	  inclDepth;			/* nested include depth */


static void
record_include_begin (cs)
struct coff_symbol *cs;
{
  /* In xcoff, we assume include files cannot be nested (not in .c files
     of course, but in corresponding .s files.) */

  if (inclDepth)
    fatal ("xcoff internal: pending include file exists.");

  ++inclDepth;

  /* allocate an include file, or make room for the new entry */
  if (inclLength == 0) {
    inclTable = (InclTable*) 
	xmalloc (sizeof (InclTable) * INITIAL_INCLUDE_TABLE_LENGTH);
    bzero (inclTable, sizeof (InclTable) * INITIAL_INCLUDE_TABLE_LENGTH);
    inclLength = INITIAL_INCLUDE_TABLE_LENGTH;
    inclIndx = 0;
  }
  else if (inclIndx >= inclLength) {
    inclLength += INITIAL_INCLUDE_TABLE_LENGTH;
    inclTable = (InclTable*) 
	xrealloc (inclTable, sizeof (InclTable) * inclLength);
    bzero (inclTable+inclLength-INITIAL_INCLUDE_TABLE_LENGTH, 
			sizeof (InclTable)*INITIAL_INCLUDE_TABLE_LENGTH);
  }

  inclTable [inclIndx].name  = cs->c_name;
  inclTable [inclIndx].begin = cs->c_value;
}


static void
record_include_end (cs)
struct coff_symbol *cs;
{
  InclTable *pTbl;  

  if (inclDepth == 0)
    fatal ("xcoff internal: Mismatch C_BINCL/C_EINCL pair found.");

  pTbl = &inclTable [inclIndx];
  pTbl->end = cs->c_value;

  --inclDepth;
  ++inclIndx;
}


/* given the start and end addresses of a compilation unit (or a csect, at times)
   process its lines and create appropriate line vectors. */

static void
process_linenos (start, end)
  CORE_ADDR start, end;
{
  char *pp;
  int offset, ii;

  struct subfile main_subfile;		/* subfile structure for the main
  					   compilation unit. */

  /* in the main source file, any time we see a function entry, we reset
     this variable to function's absolute starting line number. All the
     following line numbers in the function are relative to this, and
     we record absolute line numbers in record_line(). */

  int main_source_baseline = 0;

  
  unsigned *firstLine;
  CORE_ADDR addr;

  if (!(offset = first_fun_line_offset))
    goto return_after_cleanup;

  bzero (&main_subfile, sizeof (main_subfile));
  first_fun_line_offset = 0;

  if (inclIndx == 0)
    /* All source lines were in the main source file. None in include files. */

    enter_line_range (&main_subfile, offset, 0, start, end, 
    						&main_source_baseline);

  /* else, there was source with line numbers in include files */
  else {

    main_source_baseline = 0;
    for (ii=0; ii < inclIndx; ++ii) {

      struct subfile *tmpSubfile;

      /* if there is main file source before include file, enter it. */
      if (offset < inclTable[ii].begin) {
	enter_line_range
	  (&main_subfile, offset, inclTable[ii].begin - LINESZ, start, 0, 
	  					&main_source_baseline);
      }

      /* Have a new subfile for the include file */

      tmpSubfile = inclTable[ii].subfile = (struct subfile*) 
      				xmalloc (sizeof (struct subfile));

      bzero (tmpSubfile, sizeof (struct subfile));
      firstLine = &(inclTable[ii].funStartLine);

      /* enter include file's lines now. */
      enter_line_range (tmpSubfile, inclTable[ii].begin, 
      				inclTable[ii].end, start, 0, firstLine);

      offset = inclTable[ii].end + LINESZ;
    }

    /* all the include files' line have been processed at this point. Now,
       enter remaining lines of the main file, if any left. */
    if (offset < (linetab_offset + linetab_size + 1 - LINESZ)) {
      enter_line_range (&main_subfile, offset, 0, start, end, 
      						&main_source_baseline);
    }
  }

  /* Process main file's line numbers. */
  if (main_subfile.line_vector) {
    struct linetable *lineTb, *lv;

    lv = main_subfile.line_vector;

    /* Line numbers are not necessarily ordered. xlc compilation will
       put static function to the end. */

    lineTb = arrange_linetable (lv);
    if (lv == lineTb) {
      current_subfile->line_vector = (struct linetable *)
	xrealloc (lv, (sizeof (struct linetable)
			+ lv->nitems * sizeof (struct linetable_entry)));

    }
    else {
	free (lv);
	current_subfile->line_vector = lineTb;
    }

    current_subfile->line_vector_length = 
    			current_subfile->line_vector->nitems;
  }

    /* Now, process included files' line numbers. */

    for (ii=0; ii < inclIndx; ++ii) {

      if ( (inclTable[ii].subfile)->line_vector) { /* Useless if!!! FIXMEmgo */
        struct linetable *lineTb, *lv;

        lv = (inclTable[ii].subfile)->line_vector;

        /* Line numbers are not necessarily ordered. xlc compilation will
           put static function to the end. */

        lineTb = arrange_linetable (lv);

	push_subfile ();

	/* For the same include file, we might want to have more than one subfile.
	   This happens if we have something like:
   
  		......
	        #include "foo.h"
		......
	 	#include "foo.h"
		......

	   while foo.h including code in it. (stupid but possible)
	   Since start_subfile() looks at the name and uses an existing one if finds,
	   we need to provide a fake name and fool it. */

/*	start_subfile (inclTable[ii].name, (char*)0);  */
	start_subfile (" ?", (char*)0);
	current_subfile->name = 
		obsavestring (inclTable[ii].name, strlen (inclTable[ii].name),
			      &current_objfile->symbol_obstack);

        if (lv == lineTb) {
	  current_subfile->line_vector = (struct linetable *)
		xrealloc (lv, (sizeof (struct linetable)
			+ lv->nitems * sizeof (struct linetable_entry)));

	}
	else {
	  free (lv);
	  current_subfile->line_vector = lineTb;
	}

	current_subfile->line_vector_length = 
    			current_subfile->line_vector->nitems;
	start_subfile (pop_subfile (), (char*)0);
      }
    }

return_after_cleanup:

  /* We don't want to keep alloc/free'ing the global include file table. */
  inclIndx = 0;

  /* start with a fresh subfile structure for the next file. */
  bzero (&main_subfile, sizeof (struct subfile));
}

void
aix_process_linenos ()
{
  /* process line numbers and enter them into line vector */
  process_linenos (last_source_start_addr, cur_src_end_addr);
}


/* Enter a given range of lines into the line vector.
   can be called in the following two ways:
     enter_line_range (subfile, beginoffset, endoffset, startaddr, 0, firstLine)  or
     enter_line_range (subfile, beginoffset, 0, startaddr, endaddr, firstLine) */

static void
enter_line_range (subfile, beginoffset, endoffset, startaddr, endaddr, firstLine)
  struct subfile *subfile;
  unsigned   beginoffset, endoffset;	/* offsets to line table */
  CORE_ADDR  startaddr, endaddr;
  unsigned   *firstLine;
{
  char		*pp, *limit;
  CORE_ADDR	addr;

/* Do Byte swapping, if needed. FIXME! */
#define	P_LINENO(PP)  (*(unsigned short*)((struct external_lineno*)(PP))->l_lnno)
#define	P_LINEADDR(PP)	(*(long*)((struct external_lineno*)(PP))->l_addr.l_paddr)
#define	P_LINESYM(PP)	    (*(long*)((struct external_lineno*)(PP))->l_addr.l_symndx)

  pp = &linetab [beginoffset - linetab_offset];
  limit = endoffset ? &linetab [endoffset - linetab_offset]
  		      : &linetab [linetab_size -1];

  while (pp <= limit) {

    /* find the address this line represents */
    addr = P_LINENO(pp) ? 
      P_LINEADDR(pp) : read_symbol_nvalue (symtbl, P_LINESYM(pp)); 

    if (addr < startaddr || (endaddr && addr > endaddr))
      return;

    if (P_LINENO(pp) == 0) {
      *firstLine = read_symbol_lineno (symtbl, P_LINESYM(pp));
      record_line (subfile, 0, addr);
      --(*firstLine);
    }
    else
      record_line (subfile, *firstLine + P_LINENO(pp), addr);

    pp += LINESZ;
  }
}

typedef struct {
  int fsize;				/* file size */
  int fixedparms;			/* number of fixed parms */
  int floatparms;			/* number of float parms */
  unsigned int parminfo;		/* parameter info. 
  					   See /usr/include/sys/debug.h
					   tbtable_ext.parminfo */
  int framesize;			/* function frame size */
} TracebackInfo;


/* Given a function symbol, return its traceback information. */

  TracebackInfo *
retrieve_tracebackinfo (abfd, textsec, cs)
  bfd *abfd;
  sec_ptr textsec;
  struct coff_symbol *cs;
{
#define TBTABLE_BUFSIZ  2000
#define	MIN_TBTABSIZ	50		/* minimum buffer size to hold a
					   traceback table. */

  static TracebackInfo tbInfo;
  struct tbtable *ptb;

  static char buffer [TBTABLE_BUFSIZ];

  int  *pinsn;
  int  bytesread=0;			/* total # of bytes read so far */
  int  bufferbytes;			/* number of bytes in the buffer */

  int functionstart = cs->c_value - textsec->vma;

  bzero (&tbInfo, sizeof (tbInfo));

  /* keep reading blocks of data from the text section, until finding a zero
     word and a traceback table. */

  while (
	bufferbytes = (
		(TBTABLE_BUFSIZ < (textsec->_raw_size - functionstart - bytesread)) ? 
		 TBTABLE_BUFSIZ : (textsec->_raw_size - functionstart - bytesread))

	&& bfd_get_section_contents (abfd, textsec, buffer, 
				(file_ptr)(functionstart + bytesread), bufferbytes))
  {
    bytesread += bufferbytes;
    pinsn = (int*) buffer;

    /* if this is the first time we filled the buffer, retrieve function
       framesize info. */

    if (bytesread == bufferbytes) {

      /* skip over unrelated instructions */

      if (*pinsn == 0x7c0802a6)			/* mflr r0 */
        ++pinsn;
      if ((*pinsn & 0xfc00003e) == 0x7c000026)	/* mfcr Rx */
	++pinsn;
      if ((*pinsn & 0xfc000000) == 0x48000000)	/* bl foo, save fprs */
        ++pinsn;
      if ((*pinsn  & 0xfc1f0000) == 0xbc010000)	/* stm Rx, NUM(r1) */
        ++pinsn;

      do {
	int tmp = (*pinsn >> 16) & 0xffff;

	if (tmp ==  0x9421) {			/* stu  r1, NUM(r1) */
	  tbInfo.framesize = 0x10000 - (*pinsn & 0xffff);
	  break;
	}
	else if ((*pinsn == 0x93e1fffc) ||	/* st   r31,-4(r1) */
		 (tmp == 0x9001))		/* st   r0, NUM(r1) */
	;
	/* else, could not find a frame size. */
	else
	  return NULL;

      } while (++pinsn && *pinsn);

      if (!tbInfo.framesize)
        return NULL;      
    }

    /* look for a zero word. */

    while (*pinsn && (pinsn < (int*)(buffer + bufferbytes - sizeof(int))))
      ++pinsn;

    if (pinsn >= (int*)(buffer + bufferbytes))
      continue;

    if (*pinsn == 0) {

      /* function size is the amount of bytes we have skipped so far. */
      tbInfo.fsize = bytesread - (buffer + bufferbytes - (char*)pinsn);

      ++pinsn;

      /* if we don't have the whole traceback table in the buffer, re-read
         the whole thing. */

      if ((char*)pinsn > (buffer + bufferbytes - MIN_TBTABSIZ)) {

	/* In case if we are *very* close to the end of the text section
	   and cannot read properly from that point on, abort by returning
	   NULL.
	   Handle this case more graciously -- FIXME */

	if (!bfd_get_section_contents (
		abfd, textsec, buffer, 
		(file_ptr)(functionstart + 
		 bytesread - (buffer + bufferbytes - (char*)pinsn)),MIN_TBTABSIZ))
	  { printf ("Abnormal return!..\n"); return NULL; }

	ptb = (struct tbtable *)buffer;
      }
      else
        ptb = (struct tbtable *)pinsn;

      tbInfo.fixedparms = ptb->tb.fixedparms;
      tbInfo.floatparms = ptb->tb.floatparms;
      tbInfo.parminfo = ptb->tb_ext.parminfo;
      return &tbInfo;
    }
  }
  return NULL;
}

#if 0
/* Given a function symbol, return a pointer to its traceback table. */

  struct tbtable *
retrieve_traceback (abfd, textsec, cs, size)
  bfd *abfd;
  sec_ptr textsec;
  struct coff_symbol *cs;
  int *size;				/* return function size */
{
#define TBTABLE_BUFSIZ  2000
#define	MIN_TBTABSIZ	50		/* minimum buffer size to hold a
					   traceback table. */

  static char buffer [TBTABLE_BUFSIZ];

  int  *pinsn;
  int  bytesread=0;			/* total # of bytes read so far */
  int  bufferbytes;			/* number of bytes in the buffer */

  int functionstart = cs->c_value - textsec->filepos + textsec->vma;
  *size = 0;

  /* keep reading blocks of data from the text section, until finding a zero
     word and a traceback table. */

  while (bfd_get_section_contents (abfd, textsec, buffer, 
	(file_ptr)(functionstart + bytesread), 
	bufferbytes = (
		(TBTABLE_BUFSIZ < (textsec->size - functionstart - bytesread)) ? 
		 TBTABLE_BUFSIZ : (textsec->size - functionstart - bytesread))))
  {
    bytesread += bufferbytes;
    pinsn = (int*) buffer;

    /* look for a zero word. */

    while (*pinsn && (pinsn < (int*)(buffer + bufferbytes - sizeof(int))))
      ++pinsn;

    if (pinsn >= (int*)(buffer + bufferbytes))
      continue;

    if (*pinsn == 0) {

      /* function size is the amount of bytes we have skipped so far. */
      *size = bytesread - (buffer + bufferbytes - pinsn);

      ++pinsn;

      /* if we don't have the whole traceback table in the buffer, re-read
         the whole thing. */

      if ((char*)pinsn > (buffer + bufferbytes - MIN_TBTABSIZ)) {

	/* In case if we are *very* close to the end of the text section
	   and cannot read properly from that point on, abort for now.
	   Handle this case more graciously -- FIXME */

	if (!bfd_get_section_contents (
		abfd, textsec, buffer, 
		(file_ptr)(functionstart + 
		 bytesread - (buffer + bufferbytes - pinsn)),MIN_TBTABSIZ))
	/*   abort (); */ { printf ("abort!!!\n"); return NULL; }

	return (struct tbtable *)buffer;
      }
      else
        return (struct tbtable *)pinsn;
    }
  }
  return NULL;
}
#endif /* 0 */




/* Save the vital information for use when closing off the current file.
   NAME is the file name the symbols came from, START_ADDR is the first
   text address for the file, and SIZE is the number of bytes of text.  */

#define complete_symtab(name, start_addr) {	\
  last_source_file = savestring (name, strlen (name));	\
  last_source_start_addr = start_addr;			\
}


/* Refill the symbol table input buffer
   and set the variables that control fetching entries from it.
   Reports an error if no data available.
   This function can read past the end of the symbol table
   (into the string table) but this does no harm.  */

/* Reading symbol table has to be fast! Keep the followings as macros, rather
   than functions. */

#define	RECORD_MINIMAL_SYMBOL(NAME, ADDR, TYPE, ALLOCED)	\
{						\
  char *namestr;				\
  if (ALLOCED) 					\
    namestr = (NAME) + 1;			\
  else {					\
    (NAME) = namestr = 				\
    obstack_copy0 (&objfile->symbol_obstack, (NAME) + 1, strlen ((NAME)+1)); \
    (ALLOCED) = 1;						\
  }								\
  prim_record_minimal_symbol (namestr, (ADDR), (TYPE));		\
  misc_func_recorded = 1;					\
}


/* A parameter template, used by ADD_PARM_TO_PENDING. */

static struct symbol parmsym = {		/* default parameter symbol */
	"",					/* name */
	VAR_NAMESPACE,				/* namespace */
	LOC_ARG,				/* class */
	NULL,					/* type */
	0,					/* line number */
	0,					/* value */
};

/* Add a parameter to a given pending symbol list. */ 

#define	ADD_PARM_TO_PENDING(PARM, VALUE, PTYPE, PENDING_SYMBOLS)	\
{									\
  PARM = (struct symbol *)						\
      obstack_alloc (&objfile->symbol_obstack, sizeof (struct symbol));	\
  *(PARM) = parmsym;							\
  SYMBOL_TYPE (PARM) = PTYPE;						\
  SYMBOL_VALUE (PARM) = VALUE;						\
  add_symbol_to_list (PARM, &PENDING_SYMBOLS);				\
}


/* xcoff has static blocks marked in `.bs', `.es' pairs. They cannot be
   nested. At any given time, a symbol can only be in one static block.
   This is the base address of current static block, zero if non exists. */
   
static int static_block_base = 0;

/* true if space for symbol name has been allocated. */

static int symname_alloced = 0;

/* read the whole symbol table of a given bfd. */

static void
read_xcoff_symtab (objfile, nsyms)
     struct objfile *objfile;	/* Object file we're reading from */
     int nsyms;			/* # of symbols */
{
  bfd *abfd = objfile->obfd;
  char *raw_symbol;		/* Pointer into raw seething symbol table */
  char *raw_auxptr;		/* Pointer to first raw aux entry for sym */
  sec_ptr  textsec;		/* Pointer to text section */
  TracebackInfo *ptb;		/* Pointer to traceback table */

  struct internal_syment symbol[1];
  union internal_auxent main_aux[1];
  struct coff_symbol cs[1];
  CORE_ADDR file_start_addr = 0;
  CORE_ADDR file_end_addr = 0;

  int next_file_symnum = -1;
  int just_started = 1;
  int depth = 0;
  int toc_offset = 0;		/* toc offset value in data section. */
  int val;
  int fcn_last_line;
  int fcn_start_addr;
  long fcn_line_offset;
  size_t size;

  struct coff_symbol fcn_stab_saved;

  /* fcn_cs_saved is global because process_xcoff_symbol needs it. */
  union internal_auxent fcn_aux_saved;
  struct type *fcn_type_saved = NULL;
  struct context_stack *new;

  char *filestring = " _start_ ";	/* Name of the current file. */

  char *last_csect_name;		/* last seen csect's name and value */
  CORE_ADDR last_csect_val;
  int  misc_func_recorded;		/* true if any misc. function */

  current_objfile = objfile;

  /* Get the appropriate COFF "constants" related to the file we're handling. */
  N_TMASK = coff_data (abfd)->local_n_tmask;
  N_BTSHFT = coff_data (abfd)->local_n_btshft;
  local_symesz = coff_data (abfd)->local_symesz;

  last_source_file = NULL;
  last_csect_name = 0;
  last_csect_val = 0;
  misc_func_recorded = 0;

  start_stabs ();
  start_symtab (filestring, (char *)NULL, file_start_addr);
  symnum = 0;
  first_object_file_end = 0;

  /* Allocate space for the entire symbol table at once, and read it
     all in.  The bfd is already positioned at the beginning of
     the symbol table.  */

  size = coff_data (abfd)->local_symesz * nsyms;
  symtbl = xmalloc (size);

  val = bfd_read (symtbl, size, 1, abfd);
  if (val != size)
    perror_with_name ("reading symbol table");

  raw_symbol = symtbl;

  textsec = bfd_get_section_by_name (abfd, ".text");
  if (!textsec) {
    printf ("Unable to locate text section!\n");
  }

  while (symnum < nsyms) {

    QUIT;			/* make this command interruptable.  */

    /* READ_ONE_SYMBOL (symbol, cs, symname_alloced); */
    /* read one symbol into `cs' structure. After processing the whole symbol
       table, only string table will be kept in memory, symbol table and debug
       section of xcoff will be freed. Thus we can mark symbols with names
       in string table as `alloced'. */
    {
      int ii;

      /* Swap and align the symbol into a reasonable C structure.  */
      bfd_coff_swap_sym_in (abfd, raw_symbol, symbol);

      cs->c_symnum = symnum;
      cs->c_nsyms = symbol->n_numaux;
      if (symbol->n_zeroes) {
	symname_alloced = 0;
	/* We must use the original, unswapped, name here so the name field
	   pointed to by cs->c_name will persist throughout xcoffread.  If
	   we use the new field, it gets overwritten for each symbol.  */
	cs->c_name = ((struct external_syment *)raw_symbol)->e.e_name;
      } else if (symbol->n_sclass & 0x80) {
	cs->c_name = debugsec + symbol->n_offset;
	symname_alloced = 0;
      } else {	/* in string table */
	cs->c_name = strtbl + (int)symbol->n_offset;
	symname_alloced = 1;
      }
      cs->c_value = symbol->n_value;
      /* n_sclass is signed (FIXME), so we had better not mask off any
	 high bits it contains, since the values we will be comparing
	 it to are also signed (FIXME).  Defined in <coff/internal.h>.
	 At this point (3Jun92, gnu@cygnus.com) I think the fix is to
	 make the fields and values unsigned chars, but changing the next
	 line is a simple patch late in the release cycle, for now.  */
      cs->c_sclass = symbol->n_sclass /* & 0xff */;
      cs->c_secnum = symbol->n_scnum;
      cs->c_type = (unsigned)symbol->n_type;

      raw_symbol += coff_data (abfd)->local_symesz;
      ++symnum;

      raw_auxptr = raw_symbol;		/* Save addr of first aux entry */

      /* Skip all the auxents associated with this symbol.  */
      for (ii = symbol->n_numaux; ii; --ii ) {
        raw_symbol += coff_data (abfd)->local_auxesz;
	++symnum;
      }
    }

    /* if symbol name starts with ".$" or "$", ignore it. */
    if (cs->c_name[0] == '$' || (cs->c_name[1] == '$' && cs->c_name[0] == '.'))
      continue;

    if (cs->c_symnum == next_file_symnum && cs->c_sclass != C_FILE) {
      if (last_source_file)
	{
	  end_symtab (cur_src_end_addr, 1, 0, objfile);
	  end_stabs ();
	}

      start_stabs ();
      start_symtab ("_globals_", (char *)NULL, (CORE_ADDR)0);
      cur_src_end_addr = first_object_file_end;
      /* done with all files, everything from here on is globals */
    }

    /* if explicitly specified as a function, treat is as one. */
    if (ISFCN(cs->c_type) && cs->c_sclass != C_TPDEF) {
      bfd_coff_swap_aux_in (abfd, raw_auxptr, cs->c_type, cs->c_sclass,
			    main_aux);
      goto function_entry_point;
    }

    if ((cs->c_sclass == C_EXT || cs->c_sclass == C_HIDEXT) && cs->c_nsyms == 1)
    {
	/* dealing with a symbol with a csect entry. */

#   define	CSECT(PP)	((PP)->x_csect)
#   define	CSECT_LEN(PP)	(CSECT(PP).x_scnlen)
#   define	CSECT_ALIGN(PP)	(SMTYP_ALIGN(CSECT(PP).x_smtyp))
#   define	CSECT_SMTYP(PP)	(SMTYP_SMTYP(CSECT(PP).x_smtyp))
#   define	CSECT_SCLAS(PP)	(CSECT(PP).x_smclas)

	/* Convert the auxent to something we can access.  */
        bfd_coff_swap_aux_in (abfd, raw_auxptr, cs->c_type, cs->c_sclass,
			      main_aux);

	switch (CSECT_SMTYP (main_aux)) {

	case XTY_ER :
	  continue;			/* ignore all external references. */

	case XTY_SD :			/* a section description. */
	  {
	    switch (CSECT_SCLAS (main_aux)) {

	    case XMC_PR :			/* a `.text' csect.	*/
	      {

		/* A program csect is seen.
		 
		   We have to allocate one symbol table for each program csect. Normally
		   gdb prefers one symtab for each compilation unit (CU). In case of AIX, one
		   CU might include more than one prog csect, and they don't have to be
		   adjacent in terms of the space they occupy in memory. Thus, one single
		   CU might get fragmented in the memory and gdb's file start and end address
		   approach does not work!  */

		if (last_csect_name) {

		  /* if no misc. function recorded in the last seen csect, enter
		     it as a function. This will take care of functions like
		     strcmp() compiled by xlc. */

		  if (!misc_func_recorded) {
		     int alloced = 0;
		     RECORD_MINIMAL_SYMBOL (last_csect_name, last_csect_val,
					    mst_text, alloced);
		  }
		    

		  complete_symtab (filestring, file_start_addr);
		  cur_src_end_addr = file_end_addr;
		  end_symtab (file_end_addr, 1, 0, objfile);
		  end_stabs ();
		  start_stabs ();
		  start_symtab ((char *)NULL, (char *)NULL, (CORE_ADDR)0);
		}

		/* If this is the very first csect seen, basically `__start'. */
		if (just_started) {
		  first_object_file_end = cs->c_value + CSECT_LEN (main_aux);
		  just_started = 0;
		}

		file_start_addr = cs->c_value;
		file_end_addr = cs->c_value + CSECT_LEN (main_aux);

		if (cs->c_name && cs->c_name[0] == '.') {
		  last_csect_name = cs->c_name;
		  last_csect_val = cs->c_value;
		}
	      }
	      misc_func_recorded = 0;
	      continue;

	    case XMC_RW :
	      break;

	      /* If the section is not a data description, ignore it. Note that
		 uninitialized data will show up as XTY_CM/XMC_RW pair. */

	    case XMC_TC0:
	      if (toc_offset)
	        warning ("More than one xmc_tc0 symbol found.");
	      toc_offset = cs->c_value;
	      continue;

	    case XMC_TC	:		/* ignore toc entries	*/
	    default	:		/* any other XMC_XXX	*/
	      continue;
	    }
	  }
	  break;			/* switch CSECT_SCLAS() */

	case XTY_LD :
	  
	  /* a function entry point. */
	  if (CSECT_SCLAS (main_aux) == XMC_PR) {

function_entry_point:
	    RECORD_MINIMAL_SYMBOL (cs->c_name, cs->c_value, mst_text, 
				   symname_alloced);

	    fcn_line_offset = main_aux->x_sym.x_fcnary.x_fcn.x_lnnoptr;
	    fcn_start_addr = cs->c_value;

	    /* save the function header info, which will be used
	       when `.bf' is seen. */
	    fcn_cs_saved = *cs;
	    fcn_aux_saved = *main_aux;


	    ptb = NULL;

	    /* If function has two auxent, then debugging information is
	       already available for it. Process traceback table for
	       functions with only one auxent. */

	    if (cs->c_nsyms == 1)
	      ptb = retrieve_tracebackinfo (abfd, textsec, cs);

	    else if (cs->c_nsyms != 2)
	      abort ();

	    /* If there is traceback info, create and add parameters for it. */

	    if (ptb && (ptb->fixedparms || ptb->floatparms)) {

	      int parmcnt = ptb->fixedparms + ptb->floatparms;
	      char *parmcode = (char*) &ptb->parminfo;
	      int parmvalue = ptb->framesize + 0x18;	/* sizeof(LINK AREA) == 0x18 */
	      unsigned int ii, mask;

	      for (ii=0, mask = 0x80000000; ii <parmcnt; ++ii) {
		struct symbol *parm;

		if (ptb->parminfo & mask) {		/* float or double */
		  mask = mask >> 1;
		  if (ptb->parminfo & mask) {		/* double parm */
		    ADD_PARM_TO_PENDING
			(parm, parmvalue, builtin_type_double, local_symbols);
		    parmvalue += sizeof (double);
		  }
		  else {				/* float parm */
		    ADD_PARM_TO_PENDING
			(parm, parmvalue, builtin_type_float, local_symbols);
		    parmvalue += sizeof (float);
		  }
 		}
		else {		/* fixed parm, use (int*) for hex rep. */
		  ADD_PARM_TO_PENDING (parm, parmvalue,
				       lookup_pointer_type (builtin_type_int),
				       local_symbols);
		  parmvalue += sizeof (int);
		}
		mask = mask >> 1;
	      }
		
 	      /* Fake this as a function. Needed in process_xcoff_symbol() */
	      cs->c_type = 32;		
	      				   
	      finish_block(process_xcoff_symbol (cs, objfile), &local_symbols, 
			   pending_blocks, cs->c_value,
			   cs->c_value + ptb->fsize, objfile);
	    }
	    continue;
	  }
	  /* shared library function trampoline code entry point. */
	  else if (CSECT_SCLAS (main_aux) == XMC_GL) {

	    /* record trampoline code entries as mst_unknown symbol. When we
	       lookup mst symbols, we will choose mst_text over mst_unknown. */

#if 1
	    /* After the implementation of incremental loading of shared
	       libraries, we don't want to access trampoline entries. This
	       approach has a consequence of the necessity to bring the whole 
	       shared library at first, in order do anything with it (putting
	       breakpoints, using malloc, etc). On the other side, this is
	       consistient with gdb's behaviour on a SUN platform. */

	    /* Trying to prefer *real* function entry over its trampoline,
	       by assigning `mst_unknown' type to trampoline entries fails.
	       Gdb treats those entries as chars. FIXME. */

	    /* Recording this entry is necessary. Single stepping relies on
	       this vector to get an idea about function address boundaries. */

	    prim_record_minimal_symbol (0, cs->c_value, mst_unknown);
#else

	    /* record trampoline code entries as mst_unknown symbol. When we
	       lookup mst symbols, we will choose mst_text over mst_unknown. */

	    RECORD_MINIMAL_SYMBOL (cs->c_name, cs->c_value, mst_unknown,
				   symname_alloced);
#endif
	    continue;
	  }
	  break;

	default :		/* all other XTY_XXXs */
	  break;
	}			/* switch CSECT_SMTYP() */    }

    switch (cs->c_sclass) {

    case C_FILE:

      /* see if the last csect needs to be recorded. */

      if (last_csect_name && !misc_func_recorded) {

	  /* if no misc. function recorded in the last seen csect, enter
	     it as a function. This will take care of functions like
	     strcmp() compiled by xlc. */

	  int alloced = 0;
	  RECORD_MINIMAL_SYMBOL (last_csect_name, last_csect_val,
				mst_text, alloced);
      }

      /* c_value field contains symnum of next .file entry in table
	 or symnum of first global after last .file. */

      next_file_symnum = cs->c_value;

      /* complete symbol table for last object file containing
	 debugging information. */

      /* Whether or not there was a csect in the previous file, we have to call
	 `end_stabs' and `start_stabs' to reset type_vector, 
	 line_vector, etc. structures. */

      complete_symtab (filestring, file_start_addr);
      cur_src_end_addr = file_end_addr;
      end_symtab (file_end_addr, 1, 0, objfile);
      end_stabs ();
      start_stabs ();
      start_symtab (cs->c_name, (char *)NULL, (CORE_ADDR)0);
      last_csect_name = 0;

      /* reset file start and end addresses. A compilation unit with no text
         (only data) should have zero file boundaries. */
      file_start_addr = file_end_addr = 0;

      filestring = cs->c_name;
      break;


    case C_FUN:

#ifdef NO_DEFINE_SYMBOL
      /* For a function stab, just save its type in `fcn_type_saved', and leave
	 it for the `.bf' processing. */
      {
	char *pp = (char*) index (cs->c_name, ':');

	if (!pp || ( *(pp+1) != 'F' && *(pp+1) != 'f'))
	  fatal ("Unrecognized stab");
	pp += 2;

	if (fcn_type_saved)
	  fatal ("Unprocessed function type");

	fcn_type_saved = lookup_function_type (read_type (&pp, objfile));
      }
#else
      fcn_stab_saved = *cs;
#endif
      break;
    

    case C_FCN:
      if (strcmp (cs->c_name, ".bf") == 0) {

        bfd_coff_swap_aux_in (abfd, raw_auxptr, cs->c_type, cs->c_sclass,
			      main_aux);

	within_function = 1;

	/* Linenos are now processed on a file-by-file, not fn-by-fn, basis.
	   Metin did it, I'm not sure why.  FIXME.  -- gnu@cygnus.com */

	/* Two reasons:
	
	    1) xlc (IBM's native c compiler) postpones static function code
	       emission to the end of a compilation unit. This way it can
	       determine if those functions (statics) are needed or not, and
	       can do some garbage collection (I think). This makes line
	       numbers and corresponding addresses unordered, and we end up
	       with a line table like:
	       

 			lineno	addr
	        foo()	  10	0x100
			  20	0x200
			  30	0x300

		foo3()	  70	0x400
			  80	0x500
			  90	0x600

		static foo2()
			  40	0x700
			  50	0x800
			  60	0x900		

		and that breaks gdb's binary search on line numbers, if the
		above table is not sorted on line numbers. And that sort
		should be on function based, since gcc can emit line numbers
		like:
		
			10	0x100	- for the init/test part of a for stmt.
			20	0x200
			30	0x300
			10	0x400	- for the increment part of a for stmt.

 		arrange_linenos() will do this sorting.		


	     2)	aix symbol table might look like:
	
			c_file		// beginning of a new file
			.bi		// beginning of include file
			.ei		// end of include file
			.bi
			.ei

		basically, .bi/.ei pairs do not necessarily encapsulate
		their scope. They need to be recorded, and processed later
		on when we come the end of the compilation unit.
		Include table (inclTable) and process_linenos() handle
		that.
	*/
	mark_first_line (fcn_line_offset, cs->c_symnum);

	new = push_context (0, fcn_start_addr);

#ifdef NO_DEFINE_SYMBOL
	new->name = process_xcoff_symbol (&fcn_cs_saved, objfile);

	/* Between a function symbol and `.bf', there always will be a function
	   stab. We save function type when processing that stab. */

	if (fcn_type_saved == NULL) {
	  printf ("Unknown function type: symbol 0x%x\n", cs->c_symnum);
	  SYMBOL_TYPE (new->name) = lookup_function_type (builtin_type_int);
	}
	else {
	  SYMBOL_TYPE (new->name) = fcn_type_saved;
	  fcn_type_saved = NULL;
	}
#else
	new->name = define_symbol 
		(fcn_cs_saved.c_value, fcn_stab_saved.c_name, 0, 0, objfile);
#endif
      }
      else if (strcmp (cs->c_name, ".ef") == 0) {

        bfd_coff_swap_aux_in (abfd, raw_auxptr, cs->c_type, cs->c_sclass,
			      main_aux);

	/* the value of .ef is the address of epilogue code;
	   not useful for gdb */
	/* { main_aux.x_sym.x_misc.x_lnsz.x_lnno
	   contains number of lines to '}' */

	fcn_last_line = main_aux->x_sym.x_misc.x_lnsz.x_lnno;
	new = pop_context ();
	if (context_stack_depth != 0)
	  error ("invalid symbol data; .bf/.ef/.bb/.eb symbol mismatch, at symbol %d.",
	      symnum);

	finish_block (new->name, &local_symbols, new->old_blocks,
	    new->start_addr,
	    fcn_cs_saved.c_value +
	    fcn_aux_saved.x_sym.x_misc.x_fsize, objfile);
	within_function = 0;
      }
      break;

    case C_BSTAT	:		/* begin static block	*/
      static_block_base = read_symbol_nvalue (symtbl, cs->c_value);
      break;

    case C_ESTAT	:		/* end of static block	*/
      static_block_base = 0;
      break;

    case C_ARG		:		/* These are not implemented. */
    case C_REGPARM	:
    case C_TPDEF	:
    case C_STRTAG	:
    case C_UNTAG	:
    case C_ENTAG	:
      printf ("ERROR: Unimplemented storage class: %d.\n", cs->c_sclass);
      break;

    case C_HIDEXT	:		/* ignore these.. */
    case C_LABEL	:
    case C_NULL		:
      break;

    case C_BINCL	:		/* beginning of include file */

	/* In xlc output, C_BINCL/C_EINCL pair doesn't show up in sorted
	   order. Thus, when wee see them, we might not know enough info
	   to process them. Thus, we'll be saving them into a table 
	   (inclTable) and postpone their processing. */

	record_include_begin (cs);
	break;

    case C_EINCL	:		/* end of include file */
			/* see the comment after case C_BINCL. */
	record_include_end (cs);
	break;

    case C_BLOCK	:
      if (strcmp (cs->c_name, ".bb") == 0) {
	depth++;
	new = push_context (depth, cs->c_value);
      }
      else if (strcmp (cs->c_name, ".eb") == 0) {
	new = pop_context ();
	if (depth != new->depth)
	  error ("Invalid symbol data: .bb/.eb symbol mismatch at symbol %d.",
			 symnum);

	depth--;
	if (local_symbols && context_stack_depth > 0) {
	  /* Make a block for the local symbols within.  */
	  finish_block (new->name, &local_symbols, new->old_blocks,
				  new->start_addr, cs->c_value, objfile);
	}
	local_symbols = new->locals;
      }
      break;

    default		:
      process_xcoff_symbol (cs, objfile);
      break;
    }

  } /* while */

  if (last_source_file)
    {
      end_symtab (cur_src_end_addr, 1, 0, objfile);
      end_stabs ();
    }

  free (symtbl);
  current_objfile = NULL;

  /* Record the toc offset value of this symbol table into ldinfo structure.
     If no XMC_TC0 is found, toc_offset should be zero. Another place to obtain
     this information would be file auxiliary header. */

  xcoff_add_toc_to_loadinfo (toc_offset);
}

#define	SYMBOL_DUP(SYMBOL1, SYMBOL2)	\
  (SYMBOL2) = (struct symbol *)		\
  	obstack_alloc (&objfile->symbol_obstack, sizeof (struct symbol)); \
  *(SYMBOL2) = *(SYMBOL1);
  
 
#define	SYMNAME_ALLOC(NAME, ALLOCED)	\
  (ALLOCED) ? (NAME) : obstack_copy0 (&objfile->symbol_obstack, (NAME), strlen (NAME));


/* process one xcoff symbol. */

static struct symbol *
process_xcoff_symbol (cs, objfile)
  register struct coff_symbol *cs;
  struct objfile *objfile;
{
  struct symbol onesymbol;
  register struct symbol *sym = &onesymbol;
  struct symbol *sym2 = NULL;
  struct type *ttype;
  char *name, *pp, *qq;
  int struct_and_type_combined;
  int nameless;

  name = cs->c_name;
  if (name[0] == '.')
    ++name;

  bzero (sym, sizeof (struct symbol));

  /* default assumptions */
  SYMBOL_VALUE (sym) = cs->c_value;
  SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;

  if (ISFCN (cs->c_type)) {

    /* At this point, we don't know the type of the function and assume it 
       is int. This will be patched with the type from its stab entry later 
       on in patch_block_stabs () */

    SYMBOL_NAME (sym) = SYMNAME_ALLOC (name, symname_alloced);
    SYMBOL_TYPE (sym) = lookup_function_type (lookup_fundamental_type (objfile, FT_INTEGER));

    SYMBOL_CLASS (sym) = LOC_BLOCK;
    SYMBOL_DUP (sym, sym2);

    if (cs->c_sclass == C_EXT)
      add_symbol_to_list (sym2, &global_symbols);
    else if (cs->c_sclass == C_HIDEXT || cs->c_sclass == C_STAT)
      add_symbol_to_list (sym2, &file_symbols);
  }

  else {

    /* in case we can't figure out the type, default is `int'. */
    SYMBOL_TYPE (sym) = lookup_fundamental_type (objfile, FT_INTEGER);

    switch (cs->c_sclass)
    {
#if 0
    case C_FUN:
      if (fcn_cs_saved.c_sclass == C_EXT)
	add_stab_to_list (name, &global_stabs);
      else
	add_stab_to_list (name, &file_stabs);
      break;
#endif

    case C_DECL:      			/* a type decleration?? */

#if defined(NO_TYPEDEFS) || defined(NO_DEFINE_SYMBOL)
	qq =  (char*) strchr (name, ':');
	if (!qq)			/* skip if there is no ':' */
	  return NULL;

	nameless = (qq == name);

	struct_and_type_combined = (qq[1] == 'T' && qq[2] == 't');
	pp = qq + (struct_and_type_combined ? 3 : 2);


	/* To handle GNU C++ typename abbreviation, we need to be able to fill
	   in a type's name as soon as space for that type is allocated. */

	if (struct_and_type_combined && name != qq) {

	   int typenums[2];
	   struct type *tmp_type;
	   char *tmp_pp = pp;

	   read_type_number (&tmp_pp, typenums);
	   tmp_type = dbx_alloc_type (typenums, objfile);

	   if (tmp_type && !TYPE_NAME (tmp_type) && !nameless)
	     TYPE_NAME (tmp_type) = SYMBOL_NAME (sym) =
				obsavestring (name, qq-name,
					      &objfile->symbol_obstack);
	}
	ttype = SYMBOL_TYPE (sym) = read_type (&pp);

	/* if there is no name for this typedef, you don't have to keep its
	   symbol, since nobody could ask for it. Otherwise, build a symbol
	   and add it into symbol_list. */

	if (nameless)
	  return;

#ifdef NO_TYPEDEFS
	/* Transarc wants to eliminate type definitions from the symbol table.
	   Limited debugging capabilities, but faster symbol table processing
	   and less memory usage. Note that tag definitions (starting with
	   'T') will remain intact. */

	if (qq[1] != 'T' && (!TYPE_NAME (ttype) || *(TYPE_NAME (ttype)) == '\0')) {

	  if (SYMBOL_NAME (sym))
	      TYPE_NAME (ttype) = SYMBOL_NAME (sym);
	  else
	      TYPE_NAME (ttype) = obsavestring (name, qq-name);

	  return;
	}

#endif /* !NO_TYPEDEFS */

	/* read_type() will return null if type (or tag) definition was
	   unnnecessarily duplicated. Also, if the symbol doesn't have a name,
	   there is no need to keep it in symbol table. */
	/* The above argument no longer valid. read_type() never returns NULL. */

	if (!ttype)
	  return NULL;

	/* if there is no name for this typedef, you don't have to keep its
	   symbol, since nobody could ask for it. Otherwise, build a symbol
	   and add it into symbol_list. */

	if (qq[1] == 'T')
	    SYMBOL_NAMESPACE (sym) = STRUCT_NAMESPACE;
	else if (qq[1] == 't')
	    SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;
	else {
	    warning ("Unrecognized stab string.\n");
	    return NULL;
	}

	SYMBOL_CLASS (sym) = LOC_TYPEDEF;
	if (!SYMBOL_NAME (sym))
	    SYMBOL_NAME (sym) = obsavestring (name, qq-name);

	SYMBOL_DUP (sym, sym2);
	add_symbol_to_list 
	     (sym2, within_function ? &local_symbols : &file_symbols);

	/* For a combination of struct and type, add one more symbol
	   for the type. */

	if (struct_and_type_combined) {
	    SYMBOL_DUP (sym, sym2);
	    SYMBOL_NAMESPACE (sym2) = VAR_NAMESPACE;
	    add_symbol_to_list 
	       (sym2, within_function ? &local_symbols : &file_symbols);
	}

	/*  assign a name to the type node. */

	if (!TYPE_NAME (ttype) || *(TYPE_NAME (ttype)) == '\0') {
	  if (struct_and_type_combined)
	    TYPE_NAME (ttype) = SYMBOL_NAME (sym);
	  else if  (qq[1] == 'T')		/* struct namespace */
	    TYPE_NAME (ttype) = concat (
		TYPE_CODE (ttype) == TYPE_CODE_UNION ? "union " :
		TYPE_CODE (ttype) == TYPE_CODE_STRUCT? "struct " : "enum ",
		SYMBOL_NAME (sym), NULL);
	}
	break;

#else /* !NO_DEFINE_SYMBOL */
 	return define_symbol (cs->c_value, cs->c_name, 0, 0, objfile);
#endif

    case C_GSYM:
      add_stab_to_list (name, &global_stabs);
      break;

    case C_PSYM:
    case C_RPSYM:

#ifdef NO_DEFINE_SYMBOL
	if (*name == ':' || (pp = (char *) strchr (name, ':')) == NULL)
	  return NULL;
	SYMBOL_NAME (sym) = obsavestring (name, pp-name, &objfile -> symbol_obstack);
	SYMBOL_CLASS (sym) = (cs->c_sclass == C_PSYM) ? LOC_ARG : LOC_REGPARM;
	pp += 2;
	SYMBOL_TYPE (sym) = read_type (&pp, objfile);
	SYMBOL_DUP (sym, sym2);
	add_symbol_to_list (sym2, &local_symbols);
	break;
#else
	sym = define_symbol (cs->c_value, cs->c_name, 0, 0, objfile);
	SYMBOL_CLASS (sym) = (cs->c_sclass == C_PSYM) ? LOC_ARG : LOC_REGPARM;
	return sym;
#endif

    case C_STSYM:

#ifdef NO_DEFINE_SYMBOL
	if (*name == ':' || (pp = (char *) strchr (name, ':')) == NULL)
	  return NULL;
	SYMBOL_NAME (sym) = obsavestring (name, pp-name, &objfile -> symbol_obstack);
	SYMBOL_CLASS (sym) = LOC_STATIC;
	SYMBOL_VALUE (sym) += static_block_base;
	pp += 2;
	SYMBOL_TYPE (sym) = read_type (&pp, objfile);
	SYMBOL_DUP (sym, sym2);
	add_symbol_to_list 
	   (sym2, within_function ? &local_symbols : &file_symbols);
	break;
#else
	/* If we are going to use Sun dbx's define_symbol(), we need to
	   massage our stab string a little. Change 'V' type to 'S' to be
	   comparible with Sun. */

	if (*name == ':' || (pp = (char *) index (name, ':')) == NULL)
	  return NULL;

	++pp;
	if (*pp == 'V') *pp = 'S';
	sym = define_symbol (cs->c_value, cs->c_name, 0, 0, objfile);
	SYMBOL_VALUE (sym) += static_block_base;
	return sym;
#endif

    case C_LSYM:
	if (*name == ':' || (pp = (char *) strchr (name, ':')) == NULL)
	  return NULL;
	SYMBOL_NAME (sym) = obsavestring (name, pp-name, &objfile -> symbol_obstack);
	SYMBOL_CLASS (sym) = LOC_LOCAL;
	pp += 1;
	SYMBOL_TYPE (sym) = read_type (&pp, objfile);
	SYMBOL_DUP (sym, sym2);
	add_symbol_to_list (sym2, &local_symbols);
	break;

    case C_AUTO:
      SYMBOL_CLASS (sym) = LOC_LOCAL;
      SYMBOL_NAME (sym) = SYMNAME_ALLOC (name, symname_alloced);
      SYMBOL_DUP (sym, sym2);
      add_symbol_to_list (sym2, &local_symbols);
      break;

    case C_EXT:
      SYMBOL_CLASS (sym) = LOC_STATIC;
      SYMBOL_NAME (sym) = SYMNAME_ALLOC (name, symname_alloced);
      SYMBOL_DUP (sym, sym2);
      add_symbol_to_list (sym2, &global_symbols);
      break;

    case C_STAT:
      SYMBOL_CLASS (sym) = LOC_STATIC;
      SYMBOL_NAME (sym) = SYMNAME_ALLOC (name, symname_alloced);
      SYMBOL_DUP (sym, sym2);
      add_symbol_to_list 
	   (sym2, within_function ? &local_symbols : &file_symbols);
      break;

    case C_REG:
      printf ("ERROR! C_REG is not fully implemented!\n");
      SYMBOL_CLASS (sym) = LOC_REGISTER;
      SYMBOL_NAME (sym) = SYMNAME_ALLOC (name, symname_alloced);
      SYMBOL_DUP (sym, sym2);
      add_symbol_to_list (sym2, &local_symbols);
      break;

    case C_RSYM:

#ifdef NO_DEFINE_SYMBOL
	pp = (char*) strchr (name, ':');
	SYMBOL_CLASS (sym) = LOC_REGISTER;
	SYMBOL_VALUE (sym) = STAB_REG_TO_REGNUM (cs->c_value);
	if (pp) {
	  SYMBOL_NAME (sym) = obsavestring (name, pp-name, &objfile -> symbol_obstack);
	  pp += 2;
	  if (*pp)
	    SYMBOL_TYPE (sym) = read_type (&pp, objfile);
	}
	else
	  /* else this is not a stab entry, suppose the type is either
	     `int' or `float', depending on the register class. */

	  SYMBOL_TYPE (sym) = (SYMBOL_VALUE (sym) < 32)
	      ? lookup_fundamental_type (objfile, FT_INTEGER)
		  : lookup_fundamental_type (objfile, FT_FLOAT);

	SYMBOL_DUP (sym, sym2);
	add_symbol_to_list (sym2, &local_symbols);
	break;
#else
	if (pp) {
	  sym = define_symbol (cs->c_value, cs->c_name, 0, 0, objfile);
	  return sym;
	}
	else {
	  warning ("A non-stab C_RSYM needs special handling.");
	  return NULL;
	}
#endif

    default	:
      warning ("Unexpected storage class: %d.", cs->c_sclass);
      return NULL;
    }
  }
  return sym2;
}


static int
read_symbol_nvalue (symtable, symno)
     char *symtable;
     int symno;
{
  struct internal_syment symbol[1];

  bfd_coff_swap_sym_in (symfile_bfd, symtable + (symno*local_symesz), symbol);
  return symbol->n_value;  
}


static int
read_symbol_lineno (symtable, symno)
  char *symtable;
  int symno;
{
  struct internal_syment symbol[1];
  union internal_auxent main_aux[1];

  int ii;

  for (ii = 0; ii < 50; ii++) {
    bfd_coff_swap_sym_in (symfile_bfd,
			     symtable + (symno*local_symesz), symbol);
    if (symbol->n_sclass == C_FCN && 0 == strcmp (symbol->n_name, ".bf"))
      goto gotit;
    symno += symbol->n_numaux+1;
  }

  printf ("GDB Error: `.bf' not found.\n");
  return 0;

gotit:
  /* take aux entry and return its lineno */
  symno++;
  bfd_coff_swap_aux_in (symfile_bfd, symtable+(symno*local_symesz),
			symbol->n_type, symbol->n_sclass, main_aux);

  return main_aux->x_sym.x_misc.x_lnsz.x_lnno;
}

/* Support for line number handling */

/* This function is called for every section; it finds the outer limits
 * of the line table (minimum and maximum file offset) so that the
 * mainline code can read the whole thing for efficiency.
 */
static void
find_linenos(abfd, asect, vpinfo)
bfd *abfd;
sec_ptr asect;
PTR vpinfo; 
{
  struct coff_symfile_info *info;
  int size, count;
  file_ptr offset, maxoff;

  count = asect->lineno_count;

  if (strcmp (asect->name, ".text") || count == 0)
    return;

  size   = count * coff_data (symfile_bfd)->local_linesz;
  info   = (struct coff_symfile_info *)vpinfo;
  offset = asect->line_filepos;
  maxoff = offset + size;

  if (offset < info->min_lineno_offset || info->min_lineno_offset == 0)
    info->min_lineno_offset = offset;

  if (maxoff > info->max_lineno_offset)
    info->max_lineno_offset = maxoff;
}


/* Read in all the line numbers for fast lookups later.  Leave them in
   external (unswapped) format in memory; we'll swap them as we enter
   them into GDB's data structures.  */

static int
init_lineno (abfd, offset, size)
     bfd *abfd;
     file_ptr offset;
     int size;
{
  int val;

  if (bfd_seek(abfd, offset, L_SET) < 0)
    return -1;

  linetab = (char *) xmalloc(size);

  val = bfd_read(linetab, 1, size, abfd);
  if (val != size)
    return -1;

  linetab_offset = offset;
  linetab_size = size;
  make_cleanup (free, linetab);	/* Be sure it gets de-allocated. */
  return 0;
}

/* dbx allows the text of a symbol name to be continued into the
   next symbol name!  When such a continuation is encountered
   (a \ at the end of the text of a name)
   call this function to get the continuation.  */
/* So far, I haven't seen this happenning xlc output. I doubt we'll need this
   for xcoff. */

#undef next_symbol_text
#define	next_symbol_text() \
  printf ("Gdb Error: symbol names on multiple lines not implemented.\n")


/* xlc/dbx combination uses a set of builtin types, starting from -1. return
   the proper type node fora given builtin type #. */

struct type *
builtin_type (pp)
char **pp;
{
  int typenums[2];

  if (**pp != '-') {
    printf ("ERROR!, unknown built-in type!\n");
    return NULL;
  }
  *pp += 1;
  read_type_number (pp, typenums);

  /* default types are defined in dbxstclass.h. */
  switch ( typenums[1] ) {
  case 1: 
    return lookup_fundamental_type (current_objfile, FT_INTEGER);
  case 2: 
    return lookup_fundamental_type (current_objfile, FT_CHAR);
  case 3: 
    return lookup_fundamental_type (current_objfile, FT_SHORT);
  case 4: 
    return lookup_fundamental_type (current_objfile, FT_LONG);
  case 5: 
    return lookup_fundamental_type (current_objfile, FT_UNSIGNED_CHAR);
  case 6: 
    return lookup_fundamental_type (current_objfile, FT_SIGNED_CHAR);
  case 7: 
    return lookup_fundamental_type (current_objfile, FT_UNSIGNED_SHORT);
  case 8: 
    return lookup_fundamental_type (current_objfile, FT_UNSIGNED_INTEGER);
  case 9: 
    return lookup_fundamental_type (current_objfile, FT_UNSIGNED_INTEGER);
  case 10: 
    return lookup_fundamental_type (current_objfile, FT_UNSIGNED_LONG);
  case 11: 
    return lookup_fundamental_type (current_objfile, FT_VOID);
  case 12: 
    return lookup_fundamental_type (current_objfile, FT_FLOAT);
  case 13: 
    return lookup_fundamental_type (current_objfile, FT_DBL_PREC_FLOAT);
  case 14: 
    return lookup_fundamental_type (current_objfile, FT_EXT_PREC_FLOAT);
  case 15: 
    /* requires a builtin `integer' */
    return lookup_fundamental_type (current_objfile, FT_INTEGER);
  case 16: 
    return lookup_fundamental_type (current_objfile, FT_BOOLEAN);
  case 17: 
    /* requires builtin `short real' */
    return lookup_fundamental_type (current_objfile, FT_FLOAT);
  case 18: 
    /* requires builtin `real' */
    return lookup_fundamental_type (current_objfile, FT_FLOAT);
  default :
    printf ("ERROR! Unknown builtin type -%d\n", typenums[1]);
    return NULL;
  }
}

static void
xcoff_new_init (objfile)
     struct objfile *objfile;
{
}

static void
xcoff_symfile_init (objfile)
  struct objfile *objfile;
{
  bfd *abfd = objfile->obfd;

  /* Allocate struct to keep track of the symfile */
  objfile -> sym_private = xmmalloc (objfile -> md,
				     sizeof (struct coff_symfile_info));
  init_entry_point_info (objfile);
}

/* Perform any local cleanups required when we are done with a particular
   objfile.  I.E, we are in the process of discarding all symbol information
   for an objfile, freeing up all memory held for it, and unlinking the
   objfile struct from the global list of known objfiles. */

static void
xcoff_symfile_finish (objfile)
     struct objfile *objfile;
{
  if (objfile -> sym_private != NULL)
    {
      mfree (objfile -> md, objfile -> sym_private);
    }

  /* Start with a fresh include table for the next objfile. */

  if (inclTable)
    {
      free (inclTable);
      inclTable = NULL;
    }
  inclIndx = inclLength = inclDepth = 0;
}


static int
init_stringtab(abfd, offset, objfile)
     bfd *abfd;
     file_ptr offset;
     struct objfile *objfile;
{
  long length;
  int val;
  unsigned char lengthbuf[4];

  if (bfd_seek(abfd, offset, L_SET) < 0)
    return -1;

  val    = bfd_read((char *)lengthbuf, 1, sizeof lengthbuf, abfd);
  length = bfd_h_get_32(abfd, lengthbuf);

  /* If no string table is needed, then the file may end immediately
     after the symbols.  Just return with `strtbl' set to null. */

  if (val != sizeof length || length < sizeof length)
    return 0;

  /* Allocate string table from symbol_obstack. We will need this table
     as long as we have its symbol table around. */

  strtbl = (char*) obstack_alloc (&objfile->symbol_obstack, length);
  if (strtbl == NULL)
    return -1;

  bcopy(&length, strtbl, sizeof length);
  if (length == sizeof length)
    return 0;

  val = bfd_read(strtbl + sizeof length, 1, length - sizeof length, abfd);

  if (val != length - sizeof length || strtbl[length - 1] != '\0')
    return -1;

  return 0;
}

static int
init_debugsection(abfd)
     bfd *abfd;
{
  register sec_ptr secp;
  bfd_size_type length;

  if (debugsec) {
    free(debugsec);
    debugsec = NULL;
  }

  secp = bfd_get_section_by_name(abfd, ".debug");
  if (!secp)
    return 0;

  if (!(length = bfd_section_size(abfd, secp)))
    return 0;

  debugsec = (char *) xmalloc ((unsigned)length);
  if (debugsec == NULL)
    return -1;

  if (!bfd_get_section_contents(abfd, secp, debugsec, (file_ptr) 0, length)) {
    printf ("Can't read .debug section from symbol file\n");
    return -1;
  }
  return 0;
}

static void
free_debugsection()
{
  if (debugsec)
    free(debugsec);
  debugsec = NULL;
}


/* xcoff version of symbol file read. */

static void
xcoff_symfile_read (objfile, section_offset, mainline)
  struct objfile *objfile;
  struct section_offset *section_offset;
  int mainline;
{
  int num_symbols;			/* # of symbols */
  file_ptr symtab_offset;		/* symbol table and */
  file_ptr stringtab_offset;		/* string table file offsets */
  int val;
  bfd *abfd;
  struct coff_symfile_info *info;
  char *name;

  info = (struct coff_symfile_info *) objfile -> sym_private;
  symfile_bfd = abfd = objfile->obfd;
  name = objfile->name;

  num_symbols = bfd_get_symcount (abfd);	/* # of symbols */
  symtab_offset = obj_sym_filepos (abfd);	/* symbol table file offset */
  stringtab_offset = symtab_offset +
    num_symbols * coff_data(abfd)->local_symesz;

  info->min_lineno_offset = 0;
  info->max_lineno_offset = 0;
  bfd_map_over_sections (abfd, find_linenos, info);

  /* FIXME!  This stuff should move into symfile_init */
  if (info->min_lineno_offset != 0
      && info->max_lineno_offset > info->min_lineno_offset) {

    /* only read in the line # table if one exists */
    val = init_lineno(abfd, info->min_lineno_offset,
	(int) (info->max_lineno_offset - info->min_lineno_offset));

    if (val < 0)
      error("\"%s\": error reading line numbers\n", name);
  }

  val = init_stringtab(abfd, stringtab_offset, objfile);
  if (val < 0) {
    error ("\"%s\": can't get string table", name);
  }

  if (init_debugsection(abfd) < 0) {
    error ("Error reading .debug section of `%s'\n", name);
  }

  /* Position to read the symbol table.  Do not read it all at once. */
  val = bfd_seek(abfd, symtab_offset, L_SET);
  if (val < 0)
    perror_with_name(name);

  if (bfd_tell(abfd) != symtab_offset)
    fatal("bfd? BFD!");

  init_minimal_symbol_collection ();
  make_cleanup (discard_minimal_symbols, 0);

  /* Initialize load info structure. */
  if (mainline)
    xcoff_init_loadinfo ();

  /* Now that the executable file is positioned at symbol table,
     process it and define symbols accordingly. */

  read_xcoff_symtab(objfile, num_symbols);

  /* Free debug section. */
  free_debugsection ();

  /* Sort symbols alphabetically within each block.  */
  sort_syms ();

  /* Install any minimal symbols that have been collected as the current
     minimal symbols for this objfile. */

  install_minimal_symbols (objfile);

  /* Make a default for file to list.  */
  select_source_symtab (0);
}

/* XCOFF-specific parsing routine for section offsets.
   Plain and simple for now.  */

static
struct section_offsets *
xcoff_symfile_offsets (objfile, addr)
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
/* Register our ability to parse symbols for xcoff BFD files. */

static struct sym_fns xcoff_sym_fns =
{
  "aixcoff-rs6000",	/* sym_name: name or name prefix of BFD target type */
  15,			/* sym_namelen: number of significant sym_name chars */
  xcoff_new_init,	/* sym_new_init: init anything gbl to entire symtab */
  xcoff_symfile_init,	/* sym_init: read initial info, setup for sym_read() */
  xcoff_symfile_read,	/* sym_read: read a symbol file into symtab */
  xcoff_symfile_finish, /* sym_finish: finished with file, cleanup */
  xcoff_symfile_offsets, /* sym_offsets: xlate offsets ext->int form */
  NULL			/* next: pointer to next struct sym_fns */
};

void
_initialize_xcoffread ()
{
  add_symtab_fns(&xcoff_sym_fns);
}

#else /* IBM6000_HOST */
struct type *
builtin_type (ignore)
char **ignore;
{
    fatal ("GDB internal error: builtin_type called on non-RS/6000!");
}
#endif /* IBM6000_HOST */
