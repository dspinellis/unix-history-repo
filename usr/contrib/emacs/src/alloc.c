/* Storage allocation and gc for GNU Emacs Lisp interpreter.
   Copyright (C) 1985 Richard M. Stallman.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


#include "config.h"
#include "lisp.h"
#ifndef standalone
#include "buffer.h"
#include "window.h"
#endif

/* Number of bytes of consing done since the last gc */
int consing_since_gc;

/* Number of bytes of consing since gc before another gc should be done. */
int gc_cons_threshold;

/* Nonzero during gc */
int gc_in_progress;

#ifndef VIRT_ADDR_VARIES
/* Address below which pointers should not be traced */
extern char edata[];
#endif /* VIRT_ADDR_VARIES */

#ifndef VIRT_ADDR_VARIES
extern
#endif /* VIRT_ADDR_VARIES */
 int malloc_sbrk_used;

#ifndef VIRT_ADDR_VARIES
extern
#endif /* VIRT_ADDR_VARIES */
 int malloc_sbrk_unused;

/* Non-nil means defun should do purecopy on the function definition */
Lisp_Object Vpurify_flag;

int pure[PURESIZE / sizeof (int)] = {0,};   /* Force it into data space! */

#define PUREBEG (char *) pure

/* Index in pure at which next pure object will be allocated. */
int pureptr;

Lisp_Object
malloc_warning_1 (str)
     Lisp_Object str;
{
  return Fprinc (str, Vstandard_output);
}

/* malloc calls this if it finds we are near exhausting storage */
malloc_warning (str)
     char *str;
{
  Lisp_Object val;
  val = build_string (str);
  internal_with_output_to_temp_buffer (" *Danger*", malloc_warning_1, val);
}

/* Called if malloc returns zero */
memory_full ()
{
  error ("Memory exhausted");
}

/* like malloc and realloc but check for no memory left */

long *
xmalloc (size)
     int size;
{
  long *val = (long *) malloc (size);
  if (!val) memory_full ();
  return val;
}

long *
xrealloc (block, size)
     long *block;
     int size;
{
  long *val = (long *) realloc (block, size);
  if (!val) memory_full ();
  return val;
}

/* Allocation of cons cells */
/* We store cons cells inside of cons_blocks, allocating a new
 cons_block with malloc whenever necessary.  Cons cells reclaimed by
 GC are put on a free list to be reallocated before allocating
 any new cons cells from the latest cons_block.

 Each cons_block is just under 1020 bytes long,
 since malloc really allocates in units of powers of two
 and uses 4 bytes for its own overhead. */

#define CONS_BLOCK_SIZE \
  ((1020 - sizeof (struct cons_block *)) / sizeof (struct Lisp_Cons))

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
  XSETCONS (ptr->car, cons_free_list);
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
      cons_free_list = XCONS (cons_free_list->car);
    }
  else
    {
      if (cons_block_index == CONS_BLOCK_SIZE)
	{
	  register struct cons_block *new = (struct cons_block *) malloc (sizeof (struct cons_block));
	  if (!new) memory_full ();
	  new->next = cons_block;
	  cons_block = new;
	  cons_block_index = 0;
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
     Lisp_Object *args;
{
  Lisp_Object len, val, val_tail;

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
     Lisp_Object length, init;
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
     Lisp_Object length, init;
{
  register int sizei, index;
  register Lisp_Object vector;

  if (XTYPE (length) != Lisp_Int || XINT (length) < 0)
    length = wrong_type_argument (Qnatnump, length);
  sizei = XINT (length);

  XSET (vector, Lisp_Vector,
	(struct Lisp_Vector *) malloc (sizeof (struct Lisp_Vector) + (sizei - 1) * sizeof (Lisp_Object)));
  consing_since_gc += sizeof (struct Lisp_Vector) + (sizei - 1) * sizeof (Lisp_Object);
  if (!XVECTOR (vector))
    memory_full ();

  XVECTOR (vector)->size = sizei;
  XVECTOR (vector)->next = all_vectors;
  all_vectors = XVECTOR (vector);

  for (index = 0; index < sizei; index++)
    XVECTOR (vector)->contents[index] = init;

  return vector;
}

DEFUN ("vector", Fvector, Svector, 0, MANY, 0,
  "Return a newly created vector with our arguments (any number) as its elements.")
  (nargs, args)
     int nargs;
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

 Each symbol_block is just under 1020 bytes long,
 since malloc really allocates in units of powers of two
 and uses 4 bytes for its own overhead. */

#define SYMBOL_BLOCK_SIZE \
  ((1020 - sizeof (struct symbol_block *)) / sizeof (struct Lisp_Symbol))

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

  CHECK_STRING (str, 0);

  if (symbol_free_list)
    {
      XSET (val, Lisp_Symbol, symbol_free_list);
      symbol_free_list = XSYMBOL (symbol_free_list->value);
    }
  else
    {
      if (symbol_block_index == SYMBOL_BLOCK_SIZE)
	{
	  struct symbol_block *new = (struct symbol_block *) malloc (sizeof (struct symbol_block));
	  if (!new) memory_full ();
	  new->next = symbol_block;
	  symbol_block = new;
	  symbol_block_index = 0;
	}
      XSET (val, Lisp_Symbol, &symbol_block->symbols[symbol_block_index++]);
    }
  XSYMBOL (val)->name = XSTRING (str);
  XSYMBOL (val)->plist = Qnil;
  XSYMBOL (val)->value = Qunbound;
  XSYMBOL (val)->function = Qunbound;
  XSYMBOL (val)->next = 0;
  consing_since_gc += sizeof (struct Lisp_Symbol);
  return val;
}

/* Allocation of markers.
 Works like allocation of conses. */

#define MARKER_BLOCK_SIZE \
  ((1020 - sizeof (struct marker_block *)) / sizeof (struct Lisp_Marker))

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

  if (marker_free_list)
    {
      XSET (val, Lisp_Marker, marker_free_list);
      marker_free_list = XMARKER (marker_free_list->chain);
    }
  else
    {
      if (marker_block_index == MARKER_BLOCK_SIZE)
	{
	  struct marker_block *new = (struct marker_block *) malloc (sizeof (struct marker_block));
	  if (!new) memory_full ();
	  new->next = marker_block;
	  marker_block = new;
	  marker_block_index = 0;
	}
      XSET (val, Lisp_Marker, &marker_block->markers[marker_block_index++]);
    }
  XMARKER (val)->buffer = 0;
  XMARKER (val)->bufpos = 0;
  XMARKER (val)->modified = 0;
  XMARKER (val)->chain = Qnil;
  consing_since_gc += sizeof (struct Lisp_Marker);
  return val;
}

/* Allocation of strings */

/* Strings reside inside of string_blocks.  The entire data of the string,
 both the size and the contents, live in part of the `chars' component of a string_block.
 The `pos' component is the index within `chars' of the first free byte */

/* String blocks contain this many bytes.
  Power of 2, minus 4 for malloc overhead. */
#define STRING_BLOCK_SIZE (8188 - sizeof (struct string_block_head))

/* A string bigger than this gets its own specially-made string block
 if it doesn't fit in the current one. */
#define STRING_BLOCK_OUTSIZE 1024

struct string_block_head
  {
    struct string_block *next;
    int pos;
  };

struct string_block
  {
    struct string_block *next;
    int pos;
    char chars[STRING_BLOCK_SIZE];
  };

/* This points to the string block we are now allocating strings in
 which is also the beginning of the chain of all string blocks ever made */

struct string_block *current_string_block;

void
init_strings ()
{
  current_string_block = (struct string_block *) malloc (sizeof (struct string_block));
  consing_since_gc += sizeof (struct string_block);
  current_string_block->next = 0;
  current_string_block->pos = 0;
}

static Lisp_Object make_zero_string ();

DEFUN ("make-string", Fmake_string, Smake_string, 2, 2, 0,
  "Return a newly created string of length LENGTH, with each element being INIT.\n\
Both LENGTH and INIT must be numbers.")
  (length, init)
     Lisp_Object length, init;
{
  if (XTYPE (length) != Lisp_Int || XINT (length) < 0)
    length = wrong_type_argument (Qnatnump, length);
  CHECK_NUMBER (init, 1);
  return make_zero_string (XINT (length), XINT (init));
}

Lisp_Object
make_string (contents, length)
     char *contents;
     int length;
{
  Lisp_Object val;
  val = make_zero_string (length, 0);
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
make_zero_string (length, init)
     int length;
     register int init;
{
  register Lisp_Object val;
  register int fullsize = length + sizeof (int);
  register unsigned char *p, *end;

  if (length < 0) abort ();

  /* Round `fullsize' up to multiple of size of int; also add one for terminating zero */
  fullsize += sizeof (int);
  fullsize &= ~(sizeof (int) - 1);

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
      struct string_block *new = (struct string_block *) malloc (sizeof (struct string_block_head) + fullsize);
      if (!new) memory_full ();
      consing_since_gc += sizeof (struct string_block_head) + fullsize;
      new->pos = fullsize;
      new->next = current_string_block->next;
      current_string_block->next = new;
      XSET (val, Lisp_String,
	    (struct Lisp_String *) ((struct string_block_head *)new + 1));
    }
  else
    /* Make a new current string block and start it off with this string */
    {
      struct string_block *new = (struct string_block *) malloc (sizeof (struct string_block));
      if (!new) memory_full ();
      consing_since_gc += sizeof (struct string_block);
      new->next = current_string_block;
      current_string_block = new;
      new->pos = fullsize;
      XSET (val, Lisp_String,
	    (struct Lisp_String *) current_string_block->chars);
    }
    
  XSTRING (val)->size = length;
  p = XSTRING (val)->data;
  end = p + XSTRING (val)->size;
  while (p != end)
    *p++ = init;
  *p = 0;

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
  Lisp_Object new;
  int size = sizeof (int) + length + 1;

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
  Lisp_Object new;

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
  Lisp_Object new;
  int size = sizeof (struct Lisp_Vector) + (len - 1) * sizeof (Lisp_Object);

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
     Lisp_Object obj;
{
  Lisp_Object new, tem;
  int i;

#ifndef VIRT_ADDR_VARIES
  /* Need not trace pointers to pure storage */
  if (XUINT (obj) < (unsigned int) edata && XUINT (obj) >= 0)
    return obj;
#else /* VIRT_ADDR_VARIES */
  if (XUINT (obj) < (unsigned int) ((char *) pure + PURESIZE)
      && XUINT (obj) >= (unsigned int) pure)
    return obj;
#endif /* VIRT_ADDR_VARIES */

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

#define NSTATICS 100

char staticvec1[NSTATICS * sizeof (Lisp_Object *)] = {0};

int staticidx = 0;

#define staticvec ((Lisp_Object **) staticvec1)

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

/* On vector, means it has been marked.
 On string, means it has been copied.  */
static int most_negative_fixnum;

/* On string, means do not copy it.
 This is set in all copies, and perhaps will be used
 to indicate strings that there is no need to copy.  */
static int dont_copy_flag;

int total_conses, total_markers, total_symbols, total_string_size, total_vector_size;
int total_free_conses, total_free_markers, total_free_symbols;

/* Garbage collection: mark and sweep, except copy strings. */
static Lisp_Object mark_object ();
static void clear_marks (), gc_sweep ();

DEFUN ("garbage-collect", Fgarbage_collect, Sgarbage_collect, 0, 0, "",
  "Reclaim storage for Lisp objects no longer needed.\n\
Returns info on amount of space in use:\n\
 ((USED-CONSES . FREE-CONSES) (USED-SYMS . FREE-SYMS)\n\
  (USED-MARKERS . FREE-MARKERS) USED-STRING-CHARS USED-VECTOR-SLOTS)\n\
Garbage collection happens automatically if you cons more than\n\
gc-cons-threshold  bytes of Lisp data since previous garbage collection.")
  ()
{
  struct string_block *old_string_block;

  register struct gcpro *tail;
  register struct specbinding *bind;
  struct catchtag *catch;
  struct handler *handler;
  register struct backtrace *backlist;
  register Lisp_Object tem;
  char *omessage = minibuf_message;

  register int i;

  if (!noninteractive)
    message1 ("Garbage collecting...");

  /* Don't keep command history around forever */
  tem = Fnthcdr (make_number (30), Vcommand_history);
  if (LISTP (tem))
    XCONS (tem)->cdr = Qnil;

  gc_in_progress = 1;

  clear_marks ();
  old_string_block = current_string_block;
  current_string_block = 0;
  total_string_size = 0;
  init_strings ();

  for (tail = gcprolist; tail; tail = tail->next)
    {
      for (i = 0; i < tail->nvars; i++)
	{
	  tem = tail->var[i];
	  tail->var[i] = mark_object (tem);
	}
    }
  for (i = 0; i < staticidx; i++)
    {
      tem = *staticvec[i];
      *staticvec[i] = mark_object (tem);
    }
  for (bind = specpdl; bind != specpdl_ptr; bind++)
    {
      bind->symbol = mark_object (bind->symbol);
      bind->old_value = mark_object (bind->old_value);
    }
  for (catch = catchlist; catch; catch = catch->next)
    {
      catch->tag = mark_object (catch->tag);
      catch->val = mark_object (catch->val);
    }  
  for (handler = handlerlist; handler; handler = handler->next)
    {
      handler->handler = mark_object (handler->handler);
      handler->var = mark_object (handler->var);
    }  
  for (backlist = backtrace_list; backlist; backlist = backlist->next)
    {
      tem = *backlist->function;
      *backlist->function = mark_object (tem);
      if (backlist->nargs == UNEVALLED || backlist->nargs == MANY)
	{
	  tem = *backlist->args;
	  *backlist->args = mark_object (tem);
	}
      else
	for (i = 0; i < backlist->nargs; i++)
	  {
	    tem = backlist->args[i];
	    backlist->args[i] = mark_object (tem);
	  }
    }  

  gc_sweep (old_string_block);

  clear_marks ();
  gc_in_progress = 0;

  consing_since_gc = 0;
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

static void
clear_marks ()
{
  /* Clear marks on all strings */
  {
    register struct string_block *csb;
    register int pos;

    for (csb = current_string_block; csb; csb = csb->next)
      {
	pos = 0;
	while (pos < csb->pos)
	  {
	    register struct Lisp_String *nextstr
	      = (struct Lisp_String *) &csb->chars[pos];
	    register int fullsize;

	    nextstr->size &= ~dont_copy_flag;
	    fullsize = nextstr->size + sizeof (int);	
	    
	    fullsize += sizeof (int);
	    fullsize &= ~(sizeof (int) - 1);
	    pos += fullsize;
	  }
      }
  }
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
	  XUNMARK (sblk->symbols[i].plist);
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

/* Mark one Lisp object, and recursively mark all the objects it points to
 if this is the first time it is being marked.
 If the object is a string, it is copied (once, only) and the copy is returned.
 The original string's `size' is set to a value in which 1<<31 is set
   and the rest of which is the string address shifted right by one.
 If the object is not a string, it is returned unchanged. */

static Lisp_Object
mark_object (obj)
     Lisp_Object obj;
{
  Lisp_Object original;

  original = obj;

 loop:
#ifndef VIRT_ADDR_VARIES
  /* Need not trace pointers to pure storage */
  if (XUINT (obj) < (unsigned int) edata && XUINT (obj) >= 0)
    return original;
#else /* VIRT_ADDR_VARIES */
  if (XUINT (obj) < (unsigned int) ((char *) pure + PURESIZE)
      && XUINT (obj) >= (unsigned int) pure)
    return original;
#endif /* VIRT_ADDR_VARIES */

#ifdef SWITCH_ENUM_BUG
  switch ((int) XGCTYPE (obj))
#else
  switch (XGCTYPE (obj))
#endif
    {
    case Lisp_String:
      {
	register struct Lisp_String *ptr = XSTRING (obj);
	Lisp_Object tem;

	if (ptr->size & most_negative_fixnum)
	  {
	    XSETSTRING (obj, (struct Lisp_String *) (ptr->size & ~most_negative_fixnum));
	    return obj;
	  }
	if (ptr->size & dont_copy_flag)
	  return obj;
	total_string_size += ptr->size;
	tem = make_string (ptr->data, ptr->size);
	ptr->size = most_negative_fixnum | XINT (tem);
	XSTRING (tem)->size |= dont_copy_flag;
	return tem;
      }

    case Lisp_Vector:
    case Lisp_Window:
    case Lisp_Process:
      {
	register struct Lisp_Vector *ptr = XVECTOR (obj);
	register int size = ptr->size;
	register int i;
	Lisp_Object tem;

	if (size & most_negative_fixnum) break;   /* Already marked */
	ptr->size |= most_negative_fixnum; /* Else mark it */
	for (i = 0; i < size; i++)     /* and then mark its elements */
	  {
	    tem = ptr->contents[i];
	    ptr->contents[i] = mark_object (tem);
	  }
      }
      break;

    case Lisp_Temp_Vector:
      {
	register struct Lisp_Vector *ptr = XVECTOR (obj);
	register int size = ptr->size;
	register int i;
	Lisp_Object tem;

	for (i = 0; i < size; i++)     /* and then mark its elements */
	  {
	    tem = ptr->contents[i];
	    ptr->contents[i] = mark_object (tem);
	  }
      }
      break;

    case Lisp_Symbol:
      {
	register struct Lisp_Symbol *ptr = XSYMBOL (obj);
	struct Lisp_Symbol *ptrx;
	Lisp_Object tem;

	if (XMARKBIT (ptr->plist)) break;
	XMARK (ptr->plist);
	XSET (tem, Lisp_String, ptr->name);
	tem = mark_object (tem);
	ptr->name = XSTRING (tem);
	ptr->value = mark_object (ptr->value);
	ptr->function = mark_object (ptr->function);
	tem = ptr->plist;
	XUNMARK (tem);
	ptr->plist = mark_object (tem);
	XMARK (ptr->plist);
	ptr = ptr->next;
	if (ptr)
	  {
	    ptrx = ptr;		/* Use pf ptrx avoids compiled bug on Sun */
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
	Lisp_Object tem;
	register struct Lisp_Cons *ptr = XCONS (obj);
	if (XMARKBIT (ptr->car)) break;
	tem = ptr->car;
	XMARK (ptr->car);
	ptr->car = mark_object (tem);
	XMARK (ptr->car);
	if (XGCTYPE (ptr->cdr) != Lisp_String)
	  {
	    obj = ptr->cdr;
	    goto loop;
	  }
	ptr->cdr = mark_object (ptr->cdr);
      }
      break;
    
    case Lisp_Objfwd:
      *XOBJFWD (obj) = mark_object (*XOBJFWD (obj));
      break;

    case Lisp_Buffer:
      if (!XMARKBIT (XBUFFER (obj)->name))
	mark_buffer (obj);
      break;

    /* Don't bother with Lisp_Buffer_Objfwd,
       since all markable slots in current buffer marked anyway.  */
    }
  return original;
}

/* Mark the pointers in a buffer structure.  */

mark_buffer (buf)
     Lisp_Object buf;
{
  Lisp_Object tem;
  register struct buffer *buffer = XBUFFER (buf);

  buffer->number = mark_object (buffer->number);
  buffer->name = mark_object (buffer->name);
  XMARK (buffer->name);
  buffer->filename = mark_object (buffer->filename);
  buffer->directory = mark_object (buffer->directory);
  buffer->save_length = mark_object (buffer->save_length);
  buffer->auto_save_file_name = mark_object (buffer->auto_save_file_name);
  buffer->read_only = mark_object (buffer->read_only);
  /* buffer->markers does not preserve from gc: scavenger removes marker from
     the markers chain if it is freed.  See gc_sweep */
  buffer->mark = mark_object (buffer->mark);
  buffer->major_mode = mark_object (buffer->major_mode);
  buffer->mode_name = mark_object (buffer->mode_name);
  buffer->mode_line_format = mark_object (buffer->mode_line_format);
  buffer->keymap = mark_object (buffer->keymap);
  XSET (tem, Lisp_Vector, buffer->syntax_table_v);
  if (buffer->syntax_table_v)
    mark_object (tem);
  buffer->abbrev_table = mark_object (buffer->abbrev_table);
  buffer->case_fold_search = mark_object (buffer->case_fold_search);
  buffer->tab_width = mark_object (buffer->tab_width);
  buffer->fill_column = mark_object (buffer->fill_column);
  buffer->left_margin = mark_object (buffer->left_margin);
  buffer->auto_fill_hook = mark_object (buffer->auto_fill_hook);
  buffer->local_var_alist = mark_object (buffer->local_var_alist);
  buffer->truncate_lines = mark_object (buffer->truncate_lines);
  buffer->ctl_arrow = mark_object (buffer->ctl_arrow);
  buffer->selective_display = mark_object (buffer->selective_display);
  buffer->minor_modes = mark_object (buffer->minor_modes);
  buffer->overwrite_mode = mark_object (buffer->overwrite_mode);
  buffer->abbrev_mode = mark_object (buffer->abbrev_mode);

}

/* Find all structures not marked, and free them. */

static void
gc_sweep (old_string_block)
     struct string_block *old_string_block;
{
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
	      XSETCONS (cblk->conses[i].car, cons_free_list);
	      num_free++;
	      cons_free_list = &cblk->conses[i];
	    }
	  else num_used++;
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
	      XSETSYMBOL (sblk->symbols[i].value, symbol_free_list);
	      symbol_free_list = &sblk->symbols[i];
	      num_free++;
	    }
	  else num_used++;
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
	      unchain_marker (tem);
	      XSETMARKER (mblk->markers[i].chain, marker_free_list);
	      marker_free_list = &mblk->markers[i];
	      num_free++;
	    }
	  else num_used++;
	lim = MARKER_BLOCK_SIZE;
      }

    total_markers = num_used;
    total_free_markers = num_free;
  }

  /* Free all unmarked buffers */
  {
    register struct buffer *buffer = all_buffers, *prev = 0, *next = 0;

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
    register struct Lisp_Vector *vector = all_vectors, *prev = 0, *next = 0;
    total_vector_size = 0;

    while (vector)
      if (!(vector->size & most_negative_fixnum))
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
	  vector->size &= ~most_negative_fixnum;
	  total_vector_size += vector->size;
	  prev = vector, vector = vector->next;
	}
  }

  /* Free all old string blocks, since all strings still used have been copied. */
  {
    register struct string_block *sblk = old_string_block;
    while (sblk)
      {
	struct string_block *next = sblk->next;
	free (sblk);
	sblk = next;
      }
  }
}

/* Initialization */

init_alloc_once ()
{
  register int i, x;
  /* Compute an int in which only the sign bit is set.  */
  for (i = 0, x = 1; (x <<= 1) & ~1; i++)
    /*empty loop*/;
  most_negative_fixnum = 1 << i;
  dont_copy_flag = 1 << (i - 1);

  Vpurify_flag = Qt;

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
  DefIntVar ("gc-cons-threshold", &gc_cons_threshold,
    "*Number of bytes of consing between garbage collections.");

  DefIntVar ("pure-bytes-used", &pureptr,
    "Number of bytes of sharable Lisp data allocated so far.");

  DefIntVar ("data-bytes-used", &malloc_sbrk_used,
    "Number of bytes of unshared memory allocated in this session.");

  DefIntVar ("data-bytes-free", &malloc_sbrk_unused,
    "Number of bytes of unshared memory remaining available in this session.");

  DefLispVar ("purify-flag", &Vpurify_flag,
    "Non-nil means defun should purecopy the function definition.");

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
