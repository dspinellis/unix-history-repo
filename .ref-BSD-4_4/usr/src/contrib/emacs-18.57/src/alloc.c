/* Storage allocation and gc for GNU Emacs Lisp interpreter.
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "config.h"
#include "lisp.h"
#ifndef standalone
#include "buffer.h"
#include "window.h"
#endif

#define max(A,B) ((A) > (B) ? (A) : (B))

/* Macro to verify that storage intended for Lisp objects is not
   out of range to fit in the space for a pointer.
   ADDRESS is the start of the block, and SIZE
   is the amount of space within which objects can start.  */
#define VALIDATE_LISP_STORAGE(address, size)			\
do								\
  {								\
    Lisp_Object val;						\
    XSET (val, Lisp_Cons, (char *) address + size);		\
    if ((char *) XCONS (val) != (char *) address + size)	\
      {								\
	free (address);						\
	memory_full ();						\
      }								\
  } while (0)

/* Number of bytes of consing done since the last gc */
int consing_since_gc;

/* Number of bytes of consing since gc before another gc should be done. */
int gc_cons_threshold;

/* value of consing_since_gc when undos were last truncated.  */
int consing_at_last_truncate;

/* Nonzero during gc */
int gc_in_progress;

#ifndef VIRT_ADDR_VARIES
extern
#endif /* VIRT_ADDR_VARIES */
 int malloc_sbrk_used;

#ifndef VIRT_ADDR_VARIES
extern
#endif /* VIRT_ADDR_VARIES */
 int malloc_sbrk_unused;

/* Two thresholds controlling how much undo information to keep.  */
int undo_threshold;
int undo_high_threshold;

/* Non-nil means defun should do purecopy on the function definition */
Lisp_Object Vpurify_flag;

/* Argument we give to Fsignal when memory is full.
   Preallocated since perhaps we can't allocate it when memory is full.  */
Lisp_Object memory_exhausted_message;

#ifndef HAVE_SHM
int pure[PURESIZE / sizeof (int)] = {0,};   /* Force it into data space! */
#define PUREBEG (char *) pure
#else
#define pure PURE_SEG_BITS   /* Use shared memory segment */
#define PUREBEG (char *)PURE_SEG_BITS
#endif /* not HAVE_SHM */

/* Index in pure at which next pure object will be allocated. */
int pureptr;

/* If nonzero, this is a warning delivered by malloc and not yet displayed.  */
char *pending_malloc_warning;

Lisp_Object
malloc_warning_1 (str)
     Lisp_Object str;
{
  Fprinc (str, Vstandard_output);
  write_string ("\nKilling some buffers may delay running out of memory.\n", -1);
  write_string ("However, certainly by the time you receive the 95% warning,\n", -1);
  write_string ("you should clean up, kill this Emacs, and start a new one.", -1);
  return Qnil;
}

/* malloc calls this if it finds we are near exhausting storage */
malloc_warning (str)
     char *str;
{
  pending_malloc_warning = str;
}

display_malloc_warning ()
{
  register Lisp_Object val;

  val = build_string (pending_malloc_warning);
  pending_malloc_warning = 0;
  internal_with_output_to_temp_buffer (" *Danger*", malloc_warning_1, val);
}

/* Called if malloc returns zero */
memory_full ()
{
  while (1)
    Fsignal (Qerror, memory_exhausted_message);
}

/* like malloc and realloc but check for no memory left */

long *
xmalloc (size)
     int size;
{
  register long *val;
  /* Avoid failure if malloc (0) returns 0.  */
  if (size == 0)
    size = 1;
  val = (long *) malloc (size);
  if (!val) memory_full ();
  return val;
}

long *
xrealloc (block, size)
     long *block;
     int size;
{
  register long *val;
  /* Avoid failure if malloc (0) returns 0.  */
  if (size == 0)
    size = 1;
  val = (long *) realloc (block, size);
  if (!val) memory_full ();
  return val;
}

/* Allocation of cons cells */
/* We store cons cells inside of cons_blocks, allocating a new
 cons_block with malloc whenever necessary.  Cons cells reclaimed by
 GC are put on a free list to be reallocated before allocating
 any new cons cells from the latest cons_block.

 Each cons_block is just under 1016 bytes long,
 since malloc really allocates in units of powers of two
 and uses 8 bytes for its own overhead. */

#define CONS_BLOCK_SIZE \
  ((1016 - sizeof (struct cons_block *)) / sizeof (struct Lisp_Cons))

struct cons_block
  {
    struct cons_block *next;
    struct Lisp_Cons conses[CONS_BLOCK_SIZE];
  };

struct cons_block *cons_block;
int cons_block_index;

struct Lisp_Cons *cons_free_list;

void
init_cons ()
{
  cons_block = (struct cons_block *) malloc (sizeof (struct cons_block));
  cons_block->next = 0;
  bzero (cons_block->conses, sizeof cons_block->conses);
  cons_block_index = 0;
  cons_free_list = 0;
}

/* Explicitly free a cons cell.  */
free_cons (ptr)
     struct Lisp_Cons *ptr;
{
  XFASTINT (ptr->car) = (int) cons_free_list;
  cons_free_list = ptr;
}

DEFUN ("cons", Fcons, Scons, 2, 2, 0,
  "Create a new cons, give it CAR and CDR as components, and return it.")
  (car, cdr)
     Lisp_Object car, cdr;
{
  register Lisp_Object val;

  if (cons_free_list)
    {
      XSET (val, Lisp_Cons, cons_free_list);
      cons_free_list = (struct Lisp_Cons *) XFASTINT (cons_free_list->car);
    }
  else
    {
      if (cons_block_index == CONS_BLOCK_SIZE)
	{
	  register struct cons_block *new = (struct cons_block *) malloc (sizeof (struct cons_block));
	  if (!new) memory_full ();
	  VALIDATE_LISP_STORAGE (new, sizeof *new);
	  new->next = cons_block;
	  cons_block = new;
	  cons_block_index = 0;
	  XSET (val, Lisp_Cons, &cons_block->conses[CONS_BLOCK_SIZE - 1]);
	}
      XSET (val, Lisp_Cons, &cons_block->conses[cons_block_index++]);
    }
  XCONS (val)->car = car;
  XCONS (val)->cdr = cdr;
  consing_since_gc += sizeof (struct Lisp_Cons);
  return val;
}

DEFUN ("list", Flist, Slist, 0, MANY, 0,
  "Return a newly created list whose elements are the arguments (any number).")
  (nargs, args)
     int nargs;
     register Lisp_Object *args;
{
  register Lisp_Object len, val, val_tail;

  XFASTINT (len) = nargs;
  val = Fmake_list (len, Qnil);
  val_tail = val;
  while (!NULL (val_tail))
    {
      XCONS (val_tail)->car = *args++;
      val_tail = XCONS (val_tail)->cdr;
    }
  return val;
}

DEFUN ("make-list", Fmake_list, Smake_list, 2, 2, 0,
  "Return a newly created list of length LENGTH, with each element being INIT.")
  (length, init)
     register Lisp_Object length, init;
{
  register Lisp_Object val;
  register int size;

  if (XTYPE (length) != Lisp_Int || XINT (length) < 0)
    length = wrong_type_argument (Qnatnump, length);
  size = XINT (length);

  val = Qnil;
  while (size-- > 0)
    val = Fcons (init, val);
  return val;
}

/* Allocation of vectors */

struct Lisp_Vector *all_vectors;

DEFUN ("make-vector", Fmake_vector, Smake_vector, 2, 2, 0,
  "Return a newly created vector of length LENGTH, with each element being INIT.")
  (length, init)
     register Lisp_Object length, init;
{
  register int sizei, index;
  register Lisp_Object vector;
  register struct Lisp_Vector *p;

  if (XTYPE (length) != Lisp_Int || XINT (length) < 0)
    length = wrong_type_argument (Qnatnump, length);
  sizei = XINT (length);

  p = (struct Lisp_Vector *) malloc (sizeof (struct Lisp_Vector) + (sizei - 1) * sizeof (Lisp_Object));
  if (p == 0)
    memory_full ();
  VALIDATE_LISP_STORAGE (p, 0);

  XSET (vector, Lisp_Vector, p);
  consing_since_gc += sizeof (struct Lisp_Vector) + (sizei - 1) * sizeof (Lisp_Object);

  p->size = sizei;
  p->next = all_vectors;
  all_vectors = p;

  for (index = 0; index < sizei; index++)
    p->contents[index] = init;

  return vector;
}

DEFUN ("vector", Fvector, Svector, 0, MANY, 0,
  "Return a newly created vector with our arguments (any number) as its elements.")
  (nargs, args)
     register int nargs;
     Lisp_Object *args;
{
  register Lisp_Object len, val;
  register int index;
  register struct Lisp_Vector *p;

  XFASTINT (len) = nargs;
  val = Fmake_vector (len, Qnil);
  p = XVECTOR (val);
  for (index = 0; index < nargs; index++)
    p->contents[index] = args[index];
  return val;
}

/* Allocation of symbols.
 Just like allocation of conses!

 Each symbol_block is just under 1016 bytes long,
 since malloc really allocates in units of powers of two
 and uses 8 bytes for its own overhead. */

#define SYMBOL_BLOCK_SIZE \
  ((1016 - sizeof (struct symbol_block *)) / sizeof (struct Lisp_Symbol))

struct symbol_block
  {
    struct symbol_block *next;
    struct Lisp_Symbol symbols[SYMBOL_BLOCK_SIZE];
  };

struct symbol_block *symbol_block;
int symbol_block_index;

struct Lisp_Symbol *symbol_free_list;

void
init_symbol ()
{
  symbol_block = (struct symbol_block *) malloc (sizeof (struct symbol_block));
  symbol_block->next = 0;
  bzero (symbol_block->symbols, sizeof symbol_block->symbols);
  symbol_block_index = 0;
  symbol_free_list = 0;
}

DEFUN ("make-symbol", Fmake_symbol, Smake_symbol, 1, 1, 0,
  "Return a newly allocated uninterned symbol whose name is NAME.\n\
Its value and function definition are void, and its property list is NIL.")
  (str)
     Lisp_Object str;
{
  register Lisp_Object val;
  register struct Lisp_Symbol *p;

  CHECK_STRING (str, 0);

  if (symbol_free_list)
    {
      XSET (val, Lisp_Symbol, symbol_free_list);
      symbol_free_list
	= (struct Lisp_Symbol *) XFASTINT (symbol_free_list->value);
    }
  else
    {
      if (symbol_block_index == SYMBOL_BLOCK_SIZE)
	{
	  struct symbol_block *new = (struct symbol_block *) malloc (sizeof (struct symbol_block));
	  if (!new) memory_full ();
	  VALIDATE_LISP_STORAGE (new, sizeof *new);
	  new->next = symbol_block;
	  symbol_block = new;
	  symbol_block_index = 0;
	}
      XSET (val, Lisp_Symbol, &symbol_block->symbols[symbol_block_index++]);
    }
  p = XSYMBOL (val);
  p->name = XSTRING (str);
  p->plist = Qnil;
  p->value = Qunbound;
  p->function = Qunbound;
  p->next = 0;
  consing_since_gc += sizeof (struct Lisp_Symbol);
  return val;
}

/* Allocation of markers.
 Works like allocation of conses. */

#define MARKER_BLOCK_SIZE \
  ((1016 - sizeof (struct marker_block *)) / sizeof (struct Lisp_Marker))

struct marker_block
  {
    struct marker_block *next;
    struct Lisp_Marker markers[MARKER_BLOCK_SIZE];
  };

struct marker_block *marker_block;
int marker_block_index;

struct Lisp_Marker *marker_free_list;

void
init_marker ()
{
  marker_block = (struct marker_block *) malloc (sizeof (struct marker_block));
  marker_block->next = 0;
  bzero (marker_block->markers, sizeof marker_block->markers);
  marker_block_index = 0;
  marker_free_list = 0;
}

DEFUN ("make-marker", Fmake_marker, Smake_marker, 0, 0, 0,
  "Return a newly allocated marker which does not point at any place.")
  ()
{
  register Lisp_Object val;
  register struct Lisp_Marker *p;

  if (marker_free_list)
    {
      XSET (val, Lisp_Marker, marker_free_list);
      marker_free_list
	= (struct Lisp_Marker *) XFASTINT (marker_free_list->chain);
    }
  else
    {
      if (marker_block_index == MARKER_BLOCK_SIZE)
	{
	  struct marker_block *new = (struct marker_block *) malloc (sizeof (struct marker_block));
	  if (!new) memory_full ();
	  VALIDATE_LISP_STORAGE (new, sizeof *new);
	  new->next = marker_block;
	  marker_block = new;
	  marker_block_index = 0;
	}
      XSET (val, Lisp_Marker, &marker_block->markers[marker_block_index++]);
    }
  p = XMARKER (val);
  p->buffer = 0;
  p->bufpos = 0;
  p->chain = Qnil;
  consing_since_gc += sizeof (struct Lisp_Marker);
  return val;
}

/* Allocation of strings */

/* Strings reside inside of string_blocks.  The entire data of the string,
 both the size and the contents, live in part of the `chars' component of a string_block.
 The `pos' component is the index within `chars' of the first free byte.

 first_string_block points to the first string_block ever allocated.
 Each block points to the next one with its `next' field.
 The `prev' fields chain in reverse order.
 The last one allocated is the one currently being filled.
 current_string_block points to it.

 The string_blocks that hold individual large strings
 go in a separate chain, started by large_string_blocks.  */


/* String blocks contain this many useful bytes.
   8184 is power of 2, minus 8 for malloc overhead. */
#define STRING_BLOCK_SIZE (8184 - sizeof (struct string_block_head))

/* A string bigger than this gets its own specially-made string block
 if it doesn't fit in the current one. */
#define STRING_BLOCK_OUTSIZE 1024

struct string_block_head
  {
    struct string_block *next, *prev;
    int pos;
  };

struct string_block
  {
    struct string_block *next, *prev;
    int pos;
    char chars[STRING_BLOCK_SIZE];
  };

/* This points to the string block we are now allocating strings.  */

struct string_block *current_string_block;

/* This points to the oldest string block, the one that starts the chain.  */

struct string_block *first_string_block;

/* Last string block in chain of those made for individual large strings.  */

struct string_block *large_string_blocks;

/* If SIZE is the length of a string, this returns how many bytes
   the string occupies in a string_block (including padding).  */

#define STRING_FULLSIZE(SIZE)   \
(((SIZE) + 2 * sizeof (int)) & ~(sizeof (int) - 1))

void
init_strings ()
{
  current_string_block = (struct string_block *) malloc (sizeof (struct string_block));
  first_string_block = current_string_block;
  consing_since_gc += sizeof (struct string_block);
  current_string_block->next = 0;
  current_string_block->prev = 0;
  current_string_block->pos = 0;
  large_string_blocks = 0;
}

static Lisp_Object make_uninit_string ();

DEFUN ("make-string", Fmake_string, Smake_string, 2, 2, 0,
  "Return a newly created string of length LENGTH, with each element being INIT.\n\
Both LENGTH and INIT must be numbers.")
  (length, init)
     Lisp_Object length, init;
{
  register Lisp_Object val;
  register unsigned char *p, *end, c;

  if (XTYPE (length) != Lisp_Int || XINT (length) < 0)
    length = wrong_type_argument (Qnatnump, length);
  CHECK_NUMBER (init, 1);
  val = make_uninit_string (XINT (length));
  c = XINT (init);
  p = XSTRING (val)->data;
  end = p + XSTRING (val)->size;
  while (p != end)
    *p++ = c;
  *p = 0;
  return val;
}

Lisp_Object
make_string (contents, length)
     char *contents;
     int length;
{
  register Lisp_Object val;
  val = make_uninit_string (length, 0);
  bcopy (contents, XSTRING (val)->data, length);
  return val;
}

Lisp_Object
build_string (str)
     char *str;
{
  return make_string (str, strlen (str));
}

static Lisp_Object
make_uninit_string (length)
     int length;
{
  register Lisp_Object val;
  register int fullsize = STRING_FULLSIZE (length);

  if (length < 0) abort ();

  if (fullsize <= STRING_BLOCK_SIZE - current_string_block->pos)
    /* This string can fit in the current string block */
    {
      XSET (val, Lisp_String,
	    (struct Lisp_String *) (current_string_block->chars + current_string_block->pos));
      current_string_block->pos += fullsize;
    }
  else if (fullsize > STRING_BLOCK_OUTSIZE)
    /* This string gets its own string block */
    {
      register struct string_block *new
	= (struct string_block *) malloc (sizeof (struct string_block_head) + fullsize);
      if (!new) memory_full ();
      VALIDATE_LISP_STORAGE (new, 0);
      consing_since_gc += sizeof (struct string_block_head) + fullsize;
      new->pos = fullsize;
      new->next = large_string_blocks;
      large_string_blocks = new;
      XSET (val, Lisp_String,
	    (struct Lisp_String *) ((struct string_block_head *)new + 1));
    }
  else
    /* Make a new current string block and start it off with this string */
    {
      register struct string_block *new
	= (struct string_block *) malloc (sizeof (struct string_block));
      if (!new) memory_full ();
      VALIDATE_LISP_STORAGE (new, sizeof *new);
      consing_since_gc += sizeof (struct string_block);
      current_string_block->next = new;
      new->prev = current_string_block;
      new->next = 0;
      current_string_block = new;
      new->pos = fullsize;
      XSET (val, Lisp_String,
	    (struct Lisp_String *) current_string_block->chars);
    }
    
  XSTRING (val)->size = length;
  XSTRING (val)->data[length] = 0;

  return val;
}

/* Must get an error if pure storage is full,
 since if it cannot hold a large string
 it may be able to hold conses that point to that string;
 then the string is not protected from gc. */

Lisp_Object
make_pure_string (data, length)
     char *data;
     int length;
{
  register Lisp_Object new;
  register int size = sizeof (int) + length + 1;

  if (pureptr + size > PURESIZE)
    error ("Pure Lisp storage exhausted");
  XSET (new, Lisp_String, PUREBEG + pureptr);
  XSTRING (new)->size = length;
  bcopy (data, XSTRING (new)->data, length);
  XSTRING (new)->data[length] = 0;
  pureptr += (size + sizeof (int) - 1)
	     / sizeof (int) * sizeof (int);
  return new;
}

Lisp_Object
pure_cons (car, cdr)
     Lisp_Object car, cdr;
{
  register Lisp_Object new;

  if (pureptr + sizeof (struct Lisp_Cons) > PURESIZE)
    error ("Pure Lisp storage exhausted");
  XSET (new, Lisp_Cons, PUREBEG + pureptr);
  pureptr += sizeof (struct Lisp_Cons);
  XCONS (new)->car = Fpurecopy (car);
  XCONS (new)->cdr = Fpurecopy (cdr);
  return new;
}

Lisp_Object
make_pure_vector (len)
     int len;
{
  register Lisp_Object new;
  register int size = sizeof (struct Lisp_Vector) + (len - 1) * sizeof (Lisp_Object);

  if (pureptr + size > PURESIZE)
    error ("Pure Lisp storage exhausted");

  XSET (new, Lisp_Vector, PUREBEG + pureptr);
  pureptr += size;
  XVECTOR (new)->size = len;
  return new;
}

DEFUN ("purecopy", Fpurecopy, Spurecopy, 1, 1, 0,
  "Make a copy of OBJECT in pure storage.\n\
Recursively copies contents of vectors and cons cells.\n\
Does not copy symbols.")
  (obj)
     register Lisp_Object obj;
{
  register Lisp_Object new, tem;
  register int i;

  if (NULL (Vpurify_flag))
    return obj;

  if ((PNTR_COMPARISON_TYPE) XPNTR (obj) < (PNTR_COMPARISON_TYPE) ((char *) pure + PURESIZE)
      && (PNTR_COMPARISON_TYPE) XPNTR (obj) >= (PNTR_COMPARISON_TYPE) pure)
    return obj;

#ifdef SWITCH_ENUM_BUG
  switch ((int) XTYPE (obj))
#else
  switch (XTYPE (obj))
#endif
    {
    case Lisp_Marker:
      error ("Attempt to copy a marker to pure storage");

    case Lisp_Cons:
      return pure_cons (XCONS (obj)->car, XCONS (obj)->cdr);

    case Lisp_String:
      return make_pure_string (XSTRING (obj)->data, XSTRING (obj)->size);

    case Lisp_Vector:
      new = make_pure_vector (XVECTOR (obj)->size);
      for (i = 0; i < XVECTOR (obj)->size; i++)
	{
	  tem = XVECTOR (obj)->contents[i];
	  XVECTOR (new)->contents[i] = Fpurecopy (tem);
	}
      return new;

    default:
      return obj;
    }
}

/* Recording what needs to be marked for gc.  */

struct gcpro *gcprolist;

#define NSTATICS 200

int staticidx = 0;

#ifdef __GNUC__
Lisp_Object *staticvec[NSTATICS] = {0};
#else
char staticvec1[NSTATICS * sizeof (Lisp_Object *)] = {0};
#define staticvec ((Lisp_Object **) staticvec1)
#endif

/* Put an entry in staticvec, pointing at the variable whose address is given */

void
staticpro (varaddress)
     Lisp_Object *varaddress;
{
  staticvec[staticidx++] = varaddress;
  if (staticidx >= NSTATICS)
    abort ();
}

struct catchtag
  {
    Lisp_Object tag;
    Lisp_Object val;
    struct catchtag *next;
/*    jmp_buf jmp;  /* We don't need this for GC purposes */
  };

extern struct catchtag *catchlist;

struct backtrace
  {
    struct backtrace *next;
    Lisp_Object *function;
    Lisp_Object *args;	/* Points to vector of args. */
    int nargs;		/* length of vector */
	       /* if nargs is UNEVALLED, args points to slot holding list of unevalled args */
    char evalargs;
  };

extern struct backtrace *backtrace_list;

/* Two flags that are set during GC in the `size' component
   of a string or vector.  On some machines, these flags
   are defined by the m- file to be different bits.  */

/* On vector, means it has been marked.
   On string size field or a reference to a string,
   means not the last reference in the chain.  */

#ifndef ARRAY_MARK_FLAG
#define ARRAY_MARK_FLAG ((MARKBIT >> 1) & ~MARKBIT)
#endif /* no ARRAY_MARK_FLAG */

/* Any slot that is a Lisp_Object can point to a string
   and thus can be put on a string's reference-chain
   and thus may need to have its ARRAY_MARK_FLAG set.
   This includes the slots whose markbits are used to mark
   the containing objects.  */

#if ARRAY_MARK_FLAG == MARKBIT
you lose
#endif

int total_conses, total_markers, total_symbols, total_string_size, total_vector_size;
int total_free_conses, total_free_markers, total_free_symbols;

static void mark_object (), mark_buffer ();
static void clear_marks (), gc_sweep ();
static void compact_strings ();

DEFUN ("garbage-collect", Fgarbage_collect, Sgarbage_collect, 0, 0, "",
  "Reclaim storage for Lisp objects no longer needed.\n\
Returns info on amount of space in use:\n\
 ((USED-CONSES . FREE-CONSES) (USED-SYMS . FREE-SYMS)\n\
  (USED-MARKERS . FREE-MARKERS) USED-STRING-CHARS USED-VECTOR-SLOTS)\n\
Garbage collection happens automatically if you cons more than\n\
gc-cons-threshold  bytes of Lisp data since previous garbage collection.")
  ()
{
  register struct gcpro *tail;
  register struct specbinding *bind;
  struct catchtag *catch;
  struct handler *handler;
  register struct backtrace *backlist;
  register Lisp_Object tem;
  char *omessage = echo_area_contents;

  register int i;

  if (!noninteractive)
    message1 ("Garbage collecting...");

  /* Don't keep command history around forever */
  tem = Fnthcdr (make_number (30), Vcommand_history);
  if (CONSP (tem))
    XCONS (tem)->cdr = Qnil;
  /* Likewise for undo information.  */
  truncate_all_undos ();

  gc_in_progress = 1;

/*  clear_marks ();  */

  /* In each "large string", set the MARKBIT of the size field.
     That enables mark_object to recognize them.  */
  {
    register struct string_block *b;
    for (b = large_string_blocks; b; b = b->next)
      ((struct Lisp_String *)(&b->chars[0]))->size |= MARKBIT;
  }

  /* Mark all the special slots that serve as the roots of accessibility.

     Usually the special slots to mark are contained in particular structures.
     Then we know no slot is marked twice because the structures don't overlap.
     In some cases, the structures point to the slots to be marked.
     For these, we use MARKBIT to avoid double marking of the slot.  */

  for (i = 0; i < staticidx; i++)
    mark_object (staticvec[i]);
  for (tail = gcprolist; tail; tail = tail->next)
    for (i = 0; i < tail->nvars; i++)
      if (!XMARKBIT (tail->var[i]))
	{
	  mark_object (&tail->var[i]);
	  XMARK (tail->var[i]);
	}
  for (bind = specpdl; bind != specpdl_ptr; bind++)
    {
      mark_object (&bind->symbol);
      mark_object (&bind->old_value);
    }
  for (catch = catchlist; catch; catch = catch->next)
    {
      mark_object (&catch->tag);
      mark_object (&catch->val);
    }  
  for (handler = handlerlist; handler; handler = handler->next)
    {
      mark_object (&handler->handler);
      mark_object (&handler->var);
    }  
  for (backlist = backtrace_list; backlist; backlist = backlist->next)
    {
      if (!XMARKBIT (*backlist->function))
	{
	  mark_object (backlist->function);
	  XMARK (*backlist->function);
	}
      if (backlist->nargs == UNEVALLED || backlist->nargs == MANY)
	i = 0;
      else
	i = backlist->nargs - 1;
      for (; i >= 0; i--)
	if (!XMARKBIT (backlist->args[i]))
	  {
	    mark_object (&backlist->args[i]);
	    XMARK (backlist->args[i]);
	  }
    }  

  gc_sweep ();

  /* Clear the mark bits that we set in certain root slots.  */

  for (tail = gcprolist; tail; tail = tail->next)
    for (i = 0; i < tail->nvars; i++)
      XUNMARK (tail->var[i]);
  for (backlist = backtrace_list; backlist; backlist = backlist->next)
    {
      XUNMARK (*backlist->function);
      if (backlist->nargs == UNEVALLED || backlist->nargs == MANY)
	i = 0;
      else
	i = backlist->nargs - 1;
      for (; i >= 0; i--)
	XUNMARK (backlist->args[i]);
    }  
  XUNMARK (buffer_defaults.name);
  XUNMARK (buffer_local_symbols.name);

/*  clear_marks (); */
  gc_in_progress = 0;

  consing_since_gc = 0;
  consing_at_last_truncate = 0;
  if (gc_cons_threshold < 10000)
    gc_cons_threshold = 10000;

  if (omessage)
    message1 (omessage);
  else if (!noninteractive)
    message1 ("Garbage collecting...done");

  return Fcons (Fcons (make_number (total_conses),
		       make_number (total_free_conses)),
		Fcons (Fcons (make_number (total_symbols),
			      make_number (total_free_symbols)),
		       Fcons (Fcons (make_number (total_markers),
				     make_number (total_free_markers)),
			      Fcons (make_number (total_string_size),
				     Fcons (make_number (total_vector_size),
					    Qnil)))));
}

#if 0
static void
clear_marks ()
{
  /* Clear marks on all conses */
  {
    register struct cons_block *cblk;
    register int lim = cons_block_index;
  
    for (cblk = cons_block; cblk; cblk = cblk->next)
      {
	register int i;
	for (i = 0; i < lim; i++)
	  XUNMARK (cblk->conses[i].car);
	lim = CONS_BLOCK_SIZE;
      }
  }
  /* Clear marks on all symbols */
  {
    register struct symbol_block *sblk;
    register int lim = symbol_block_index;
  
    for (sblk = symbol_block; sblk; sblk = sblk->next)
      {
	register int i;
	for (i = 0; i < lim; i++)
	  {
	    XUNMARK (sblk->symbols[i].plist);
	  }
	lim = SYMBOL_BLOCK_SIZE;
      }
  }
  /* Clear marks on all markers */
  {
    register struct marker_block *sblk;
    register int lim = marker_block_index;
  
    for (sblk = marker_block; sblk; sblk = sblk->next)
      {
	register int i;
	for (i = 0; i < lim; i++)
	  XUNMARK (sblk->markers[i].chain);
	lim = MARKER_BLOCK_SIZE;
      }
  }
  /* Clear mark bits on all buffers */
  {
    register struct buffer *nextb = all_buffers;

    while (nextb)
      {
	XUNMARK (nextb->name);
	nextb = nextb->next;
      }
  }
}
#endif

/* Mark reference to a Lisp_Object.  If the object referred to
   has not been seen yet, recursively mark all the references contained in it.

   If the object referenced is a short string, the referrencing slot
   is threaded into a chain of such slots, pointed to from
   the `size' field of the string.  The actual string size
   lives in the last slot in the chain.  We recognize the end
   because it is < (unsigned) STRING_BLOCK_SIZE.  */

static void
mark_object (objptr)
     Lisp_Object *objptr;
{
  register Lisp_Object obj;

  obj = *objptr;
  XUNMARK (obj);

 loop:

  if ((PNTR_COMPARISON_TYPE) XPNTR (obj) < (PNTR_COMPARISON_TYPE) ((char *) pure + PURESIZE)
      && (PNTR_COMPARISON_TYPE) XPNTR (obj) >= (PNTR_COMPARISON_TYPE) pure)
    return;

#ifdef SWITCH_ENUM_BUG
  switch ((int) XGCTYPE (obj))
#else
  switch (XGCTYPE (obj))
#endif
    {
    case Lisp_String:
      {
	register struct Lisp_String *ptr = XSTRING (obj);

	if (ptr->size & MARKBIT)
	  /* A large string.  Just set ARRAY_MARK_FLAG.  */
	  ptr->size |= ARRAY_MARK_FLAG;
	else
	  {
	    /* A small string.  Put this reference
	       into the chain of references to it.
	       The address OBJPTR is even, so if the address
	       includes MARKBIT, put it in the low bit
	       when we store OBJPTR into the size field.  */

	    if (XMARKBIT (*objptr))
	      {
		XFASTINT (*objptr) = ptr->size;
		XMARK (*objptr);
	      }
	    else
	      XFASTINT (*objptr) = ptr->size;
	    if ((int)objptr & 1) abort ();
	    ptr->size = (int) objptr & ~MARKBIT;
	    if ((int) objptr & MARKBIT)
	      ptr->size ++;
	  }
      }
      break;

    case Lisp_Vector:
    case Lisp_Window:
    case Lisp_Process:
    case Lisp_Window_Configuration:
      {
	register struct Lisp_Vector *ptr = XVECTOR (obj);
	register int size = ptr->size;
	register int i;

	if (size & ARRAY_MARK_FLAG) break;   /* Already marked */
	ptr->size |= ARRAY_MARK_FLAG; /* Else mark it */
	for (i = 0; i < size; i++)     /* and then mark its elements */
	  mark_object (&ptr->contents[i]);
      }
      break;

#if 0
    case Lisp_Temp_Vector:
      {
	register struct Lisp_Vector *ptr = XVECTOR (obj);
	register int size = ptr->size;
	register int i;

	for (i = 0; i < size; i++)     /* and then mark its elements */
	  mark_object (&ptr->contents[i]);
      }
      break;
#endif 0

    case Lisp_Symbol:
      {
	register struct Lisp_Symbol *ptr = XSYMBOL (obj);
	struct Lisp_Symbol *ptrx;

	if (XMARKBIT (ptr->plist)) break;
	XMARK (ptr->plist);
	XSETTYPE (*(Lisp_Object *) &ptr->name, Lisp_String);
	mark_object (&ptr->name);
	mark_object ((Lisp_Object *) &ptr->value);
	mark_object (&ptr->function);
	mark_object (&ptr->plist);
	ptr = ptr->next;
	if (ptr)
	  {
	    ptrx = ptr;		/* Use pf ptrx avoids compiler bug on Sun */
	    XSETSYMBOL (obj, ptrx);
	    goto loop;
	  }
      }
      break;

    case Lisp_Marker:
      XMARK (XMARKER (obj)->chain);
      /* DO NOT mark thru the marker's chain.
	 The buffer's markers chain does not preserve markers from gc;
	 instead, markers are removed from the chain when they are freed by gc. */
      break;

    case Lisp_Cons:
    case Lisp_Buffer_Local_Value:
    case Lisp_Some_Buffer_Local_Value:
      {
	register struct Lisp_Cons *ptr = XCONS (obj);
	if (XMARKBIT (ptr->car)) break;
	XMARK (ptr->car);
	mark_object (&ptr->car);
	objptr = &ptr->cdr;
	obj = ptr->cdr;
	goto loop;
      }

    case Lisp_Buffer:
      if (!XMARKBIT (XBUFFER (obj)->name))
	mark_buffer (obj);
      break;

    case Lisp_Int:
    case Lisp_Void:
    case Lisp_Subr:
    case Lisp_Intfwd:
    case Lisp_Boolfwd:
    case Lisp_Objfwd:
    case Lisp_Buffer_Objfwd:
    case Lisp_Internal_Stream:
    /* Don't bother with Lisp_Buffer_Objfwd,
       since all markable slots in current buffer marked anyway.  */
    /* Don't need to do Lisp_Objfwd, since the places they point
       are protected with staticpro.  */
      break;

    default:
      abort ();
    }
}

/* Mark the pointers in a buffer structure.  */

static void
mark_buffer (buf)
     Lisp_Object buf;
{
  Lisp_Object tem;
  register struct buffer *buffer = XBUFFER (buf);
  register Lisp_Object *ptr;

  /* This is the buffer's markbit */
  mark_object (&buffer->name);
  XMARK (buffer->name);

  for (ptr = &buffer->name + 1;
       (char *)ptr < (char *)buffer + sizeof (struct buffer);
       ptr++)
    mark_object (ptr);
}

/* Find all structures not marked, and free them. */

static void
gc_sweep ()
{
  total_string_size = 0;
  compact_strings ();

  /* Put all unmarked conses on free list */
  {
    register struct cons_block *cblk;
    register int lim = cons_block_index;
    register int num_free = 0, num_used = 0;

    cons_free_list = 0;
  
    for (cblk = cons_block; cblk; cblk = cblk->next)
      {
	register int i;
	for (i = 0; i < lim; i++)
	  if (!XMARKBIT (cblk->conses[i].car))
	    {
	      XFASTINT (cblk->conses[i].car) = (int) cons_free_list;
	      num_free++;
	      cons_free_list = &cblk->conses[i];
	    }
	  else
	    {
	      num_used++;
	      XUNMARK (cblk->conses[i].car);
	    }
	lim = CONS_BLOCK_SIZE;
      }
    total_conses = num_used;
    total_free_conses = num_free;
  }

  /* Put all unmarked symbols on free list */
  {
    register struct symbol_block *sblk;
    register int lim = symbol_block_index;
    register int num_free = 0, num_used = 0;

    symbol_free_list = 0;
  
    for (sblk = symbol_block; sblk; sblk = sblk->next)
      {
	register int i;
	for (i = 0; i < lim; i++)
	  if (!XMARKBIT (sblk->symbols[i].plist))
	    {
	      XFASTINT (sblk->symbols[i].value) = (int) symbol_free_list;
	      symbol_free_list = &sblk->symbols[i];
	      num_free++;
	    }
	  else
	    {
	      num_used++;
	      sblk->symbols[i].name
		= XSTRING (*(Lisp_Object *) &sblk->symbols[i].name);
	      XUNMARK (sblk->symbols[i].plist);
	    }
	lim = SYMBOL_BLOCK_SIZE;
      }
    total_symbols = num_used;
    total_free_symbols = num_free;
  }

#ifndef standalone
  /* Put all unmarked markers on free list.
     Dechain each one first from the buffer it points into. */
  {
    register struct marker_block *mblk;
    struct Lisp_Marker *tem1;
    register int lim = marker_block_index;
    register int num_free = 0, num_used = 0;

    marker_free_list = 0;
  
    for (mblk = marker_block; mblk; mblk = mblk->next)
      {
	register int i;
	for (i = 0; i < lim; i++)
	  if (!XMARKBIT (mblk->markers[i].chain))
	    {
	      Lisp_Object tem;
	      tem1 = &mblk->markers[i];  /* tem1 avoids Sun compiler bug */
	      XSET (tem, Lisp_Marker, tem1);
	      if (tem1->buffer)
		unchain_marker (tem);
	      XFASTINT (mblk->markers[i].chain) = (int) marker_free_list;
	      marker_free_list = &mblk->markers[i];
	      num_free++;
	    }
	  else
	    {
	      num_used++;
	      XUNMARK (mblk->markers[i].chain);
	    }
	lim = MARKER_BLOCK_SIZE;
      }

    total_markers = num_used;
    total_free_markers = num_free;
  }

  /* Free all unmarked buffers */
  {
    register struct buffer *buffer = all_buffers, *prev = 0, *next;

    while (buffer)
      if (!XMARKBIT (buffer->name))
	{
	  if (prev)
	    prev->next = buffer->next;
	  else
	    all_buffers = buffer->next;
	  next = buffer->next;
	  free (buffer);
	  buffer = next;
	}
      else
	{
	  XUNMARK (buffer->name);
	  prev = buffer, buffer = buffer->next;
	}
  }

#endif standalone

  /* Free all unmarked vectors */
  {
    register struct Lisp_Vector *vector = all_vectors, *prev = 0, *next;
    total_vector_size = 0;

    while (vector)
      if (!(vector->size & ARRAY_MARK_FLAG))
	{
	  if (prev)
	    prev->next = vector->next;
	  else
	    all_vectors = vector->next;
	  next = vector->next;
	  free (vector);
	  vector = next;
	}
      else
	{
	  vector->size &= ~ARRAY_MARK_FLAG;
	  total_vector_size += vector->size;
	  prev = vector, vector = vector->next;
	}
  }

  /* Free all "large strings" not marked with ARRAY_MARK_FLAG.  */
  {
    register struct string_block *sb = large_string_blocks, *prev = 0, *next;

    while (sb)
      if (!(((struct Lisp_String *)(&sb->chars[0]))->size & ARRAY_MARK_FLAG))
	{
	  if (prev)
	    prev->next = sb->next;
	  else
	    large_string_blocks = sb->next;
	  next = sb->next;
	  free (sb);
	  sb = next;
	}
      else
	{
	  ((struct Lisp_String *)(&sb->chars[0]))->size
	    &= ~ARRAY_MARK_FLAG & ~MARKBIT;
	  total_string_size += ((struct Lisp_String *)(&sb->chars[0]))->size;
	  prev = sb, sb = sb->next;
	}
  }
}

/* Compactify strings, relocate references to them, and
   free any string blocks that become empty.  */

static void
compact_strings ()
{
  /* String block of old strings we are scanning.  */
  register struct string_block *from_sb;
  /* A preceding string block (or maybe the same one)
     where we are copying the still-live strings to.  */
  register struct string_block *to_sb;
  int pos;
  int to_pos;

  to_sb = first_string_block;
  to_pos = 0;

  /* Scan each existing string block sequentially, string by string.  */
  for (from_sb = first_string_block; from_sb; from_sb = from_sb->next)
    {
      pos = 0;
      /* POS is the index of the next string in the block.  */
      while (pos < from_sb->pos)
	{
	  register struct Lisp_String *nextstr
	    = (struct Lisp_String *) &from_sb->chars[pos];

	  register struct Lisp_String *newaddr;
	  register int size = nextstr->size;

	  /* NEXTSTR is the old address of the next string.
	     Just skip it if it isn't marked.  */
	  if ((unsigned) size > STRING_BLOCK_SIZE)
	    {
	      /* It is marked, so its size field is really a chain of refs.
		 Find the end of the chain, where the actual size lives.  */
	      while ((unsigned) size > STRING_BLOCK_SIZE)
		{
		  if (size & 1) size ^= MARKBIT | 1;
		  size = *(int *)size & ~MARKBIT;
		}

	      total_string_size += size;

	      /* If it won't fit in TO_SB, close it out,
		 and move to the next sb.  Keep doing so until
		 TO_SB reaches a large enough, empty enough string block.
		 We know that TO_SB cannot advance past FROM_SB here
		 since FROM_SB is large enough to contain this string.
		 Any string blocks skipped here
		 will be patched out and freed later.  */
	      while (to_pos + STRING_FULLSIZE (size)
		     > max (to_sb->pos, STRING_BLOCK_SIZE))
		{
		  to_sb->pos = to_pos;
		  to_sb = to_sb->next;
		  to_pos = 0;
		}
	      /* Compute new address of this string
		 and update TO_POS for the space being used.  */
	      newaddr = (struct Lisp_String *) &to_sb->chars[to_pos];
	      to_pos += STRING_FULLSIZE (size);

	      /* Copy the string itself to the new place.  */
	      if (nextstr != newaddr)
		bcopy (nextstr, newaddr, size + 1 + sizeof (int));

	      /* Go through NEXTSTR's chain of references
		 and make each slot in the chain point to
		 the new address of this string.  */
	      size = newaddr->size;
	      while ((unsigned) size > STRING_BLOCK_SIZE)
		{
		  register Lisp_Object *objptr;
		  if (size & 1) size ^= MARKBIT | 1;
		  objptr = (Lisp_Object *)size;

		  size = XFASTINT (*objptr) & ~MARKBIT;
		  if (XMARKBIT (*objptr))
		    {
		      XSET (*objptr, Lisp_String, newaddr);
		      XMARK (*objptr);
		    }
		  else
		    XSET (*objptr, Lisp_String, newaddr);
		}
	      /* Store the actual size in the size field.  */
	      newaddr->size = size;
	    }
	  pos += STRING_FULLSIZE (size);
	}
    }

  /* Close out the last string block still used and free any that follow.  */
  to_sb->pos = to_pos;
  current_string_block = to_sb;

  from_sb = to_sb->next;
  to_sb->next = 0;
  while (from_sb)
    {
      to_sb = from_sb->next;
      free (from_sb);
      from_sb = to_sb;
    }

  /* Free any empty string blocks further back in the chain.
     This loop will never free first_string_block, but it is very
     unlikely that that one will become empty, so why bother checking?  */

  from_sb = first_string_block;
  while (to_sb = from_sb->next)
    {
      if (to_sb->pos == 0)
	{
	  if (from_sb->next = to_sb->next)
	    from_sb->next->prev = from_sb;
	  free (to_sb);
	}
      else
	from_sb = to_sb;
    }
}

truncate_all_undos ()
{
  register struct buffer *nextb = all_buffers;

  consing_at_last_truncate = consing_since_gc;

  while (nextb)
    {
      nextb->undo_list 
	= truncate_undo_list (nextb->undo_list, undo_threshold,
			      undo_high_threshold);
      nextb = nextb->next;
    }
}

/* Initialization */

init_alloc_once ()
{
  /* Used to do Vpurify_flag = Qt here, but Qt isn't set up yet!  */
  pureptr = 0;
  all_vectors = 0;
  init_strings ();
  init_cons ();
  init_symbol ();
  init_marker ();
  gcprolist = 0;
  staticidx = 0;
  consing_since_gc = 0;
  gc_cons_threshold = 100000;
#ifdef VIRT_ADDR_VARIES
  malloc_sbrk_unused = 1<<22;	/* A large number */
  malloc_sbrk_used = 100000;	/* as reasonable as any number */
#endif /* VIRT_ADDR_VARIES */
}

init_alloc ()
{
  gcprolist = 0;
}

void
syms_of_alloc ()
{
  memory_exhausted_message = Fcons (build_string ("Memory exhausted"), Qnil);
  staticpro (&memory_exhausted_message);

  DEFVAR_INT ("gc-cons-threshold", &gc_cons_threshold,
    "*Number of bytes of consing between garbage collections.");

  DEFVAR_INT ("pure-bytes-used", &pureptr,
    "Number of bytes of sharable Lisp data allocated so far.");

#if 0
  DEFVAR_INT ("data-bytes-used", &malloc_sbrk_used,
    "Number of bytes of unshared memory allocated in this session.");

  DEFVAR_INT ("data-bytes-free", &malloc_sbrk_unused,
    "Number of bytes of unshared memory remaining available in this session.");
#endif

  DEFVAR_LISP ("purify-flag", &Vpurify_flag,
    "Non-nil means loading Lisp code in order to dump an executable.");

  DEFVAR_INT ("undo-threshold", &undo_threshold,
    "Keep no more undo information once it exceeds this size.\n\
This threshold is applied when garbage collection happens.\n\
The size is counted as the number of bytes occupied,\n\
which includes both saved text and other data.");
  undo_threshold = 15000;
  DEFVAR_INT ("undo-high-threshold", &undo_high_threshold,
    "Don't keep more than this much size of undo information.\n\
A command which pushes past this size is itself forgotten.\n\
This threshold is applied when garbage collection happens.\n\
The size is counted as the number of bytes occupied,\n\
which includes both saved text and other data.");
  undo_high_threshold = 20000;

  defsubr (&Scons);
  defsubr (&Slist);
  defsubr (&Svector);
  defsubr (&Smake_list);
  defsubr (&Smake_vector);
  defsubr (&Smake_string);
  defsubr (&Smake_symbol);
  defsubr (&Smake_marker);
  defsubr (&Spurecopy);
  defsubr (&Sgarbage_collect);
}
