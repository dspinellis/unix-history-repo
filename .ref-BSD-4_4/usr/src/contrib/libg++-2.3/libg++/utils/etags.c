/* Tags file maker to go with GNU Emacs
   Copyright (C) 1984, 1987, 1988, 1989, 1992 Free Software Foundation, Inc. and Ken Arnold

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

/*
 * Authors:
 *	Ctags originally by Ken Arnold.
 *	FORTRAN added by Jim Kleckner.
 *	Ed Pelegri-Llopart added C typedefs.
 *	Gnu Emacs TAGS format and modifications by RMS?
 *	Sam Kendall added C++.
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "getopt.h"

#ifdef	__GNUC__
#define	alloca	__builtin_alloca
#else
#ifdef	sparc
#include <alloca.h>
#else
extern char *alloca ();
#endif
#endif

extern char *malloc (), *realloc ();
extern char *getenv ();
extern char *index (), *rindex ();
extern char *strcpy (), *strncpy ();
extern int strcmp ();

#ifdef hpux
#define notdef
#endif

/* Define the symbol ETAGS to make the program "etags",
 which makes emacs-style tag tables by default.
 Define CTAGS to make the program "ctags" compatible with the usual one.
 Define neither one to get behavior that depends
 on the name with which the program is invoked
 (but we don't normally compile it that way).  */

#if !defined(ETAGS) && !defined(CTAGS)
/* If neither is defined, program can be run as either. */
#define ETAGS
#define CTAGS
#endif

/* On VMS, CTAGS is not useful, so always do ETAGS.  */
#ifdef VMS
#ifndef ETAGS
#define ETAGS
#endif
#endif

/* Exit codes for success and failure.  */
#ifdef VMS
#define	GOOD	(1)
#define BAD	(0)
#else
#define	GOOD	(0)
#define	BAD	(1)
#endif

/*
 * The FILEPOS abstract type, which represents a position in a file,
 * plus the following accessor functions:
 *
 *	long GET_CHARNO (pos)
 *				returns absolute char number.
 *	long GET_COOKIE (pos)
 *				returns ftell () cookie.
 *	void SET_FILEPOS (pos, fp, charno)
 *	    FILE *fp; long charno;
 *				sets `pos' from the current file
 *				position of `fp' and from `charno',
 *				which must be the absolute character
 *				number corresponding to the current
 *				position of `fp'.
 *
 * The `pos' parameter is an lvalue expression of type FILEPOS.
 * Parameters to the accessor functions are evaluated 0 or more times,
 * and so must have no side effects.
 *
 * FILEPOS objects can also be assigned and passed to and from
 * functions in the normal C manner.
 *
 * Implementation notes: the `+ 0' is to enforce rvalue-ness.
 */
#ifdef VMS
typedef struct
{
  long cookie;
  long charno;
} FILEPOS;

#define GET_CHARNO(pos)	((pos).charno + 0)
#define GET_COOKIE(pos)	((pos).cookie + 0)
#define SET_FILEPOS(pos, fp, cno) \
    ((void) ((pos).cookie = ftell (fp), (pos).charno = (cno)))
#else
#ifndef DEBUG
 /* UNIX real implementation */
typedef long FILEPOS;
#define GET_CHARNO(pos)	((pos) + 0)
#define GET_COOKIE(pos)	GET_CHARNO (pos)
#define SET_FILEPOS(pos, fp, cno)	((void) ((pos) = (cno)))
#else
 /* UNIX debugging implementation */
typedef struct
{
  long charno;
} FILEPOS;

#define GET_CHARNO(pos)	((pos).charno + 0)
#define GET_COOKIE(pos)	GET_CHARNO (pos)
#define SET_FILEPOS(pos, fp, cno)					\
    ((void) ((pos).charno = (cno),					\
	     (cno) != ftell (fp) ? (error ("SET_FILEPOS inconsistency"), 0) \
	     			 : 0))
#endif
#endif

#define streq(s, t)	(strcmp (s, t) == 0)
#define strneq(s, t, n)	(strncmp (s, t, n) == 0)
#define	reg	register
#define	logical	char

#define	TRUE	1
#define	FALSE	0

#define	iswhite(arg)	(_wht[arg])	/* T if char is white		*/
#define	begtoken(arg)	(_btk[arg])	/* T if char can start token	*/
#define	intoken(arg)	(_itk[arg])	/* T if char can be in token	*/
#define	endtoken(arg)	(_etk[arg])	/* T if char ends tokens	*/
#define	isgood(arg)	(_gd[arg])	/* T if char can be after ')'	*/

#define	max(I1,I2)	((I1) > (I2) ? (I1) : (I2))

struct nd_st
{				/* sorting structure			*/
  char *name;			/* function or type name	*/
  char *file;			/* file name			*/
  logical is_func;		/* use pattern or line no	*/
  logical rewritten;		/* list name separately		*/
  logical been_warned;		/* set if noticed dup		*/
  int lno;			/* line number tag is on	*/
  long cno;			/* character number line starts on */
  char *pat;			/* search pattern		*/
  struct nd_st *left, *right;	/* left and right sons		*/
};

long ftell ();
typedef struct nd_st NODE;

logical gotone,			/* found a func already on line	*/
 /* boolean "func" (see init)	*/
  header_file,			/* TRUE if .h file, FALSE o.w.  */
  _wht[0177], _etk[0177], _itk[0177], _btk[0177], _gd[0177];


char *concat ();
char *savenstr ();
char *savestr ();
char *xmalloc ();
char *xrealloc ();
int L_isdef ();
int PF_funcs ();
int total_size_of_entries ();
logical consider_token ();
logical tail ();
long readline ();
void Asm_funcs ();
void C_entries ();
void L_funcs ();
void L_getit ();
void PAS_funcs ();
void Scheme_funcs ();
void TEX_funcs ();
void add_node ();
void error ();
void fatal ();
void find_entries ();
void free_tree ();
void getit ();
void getline ();
void init ();
void initbuffer ();
void initbuffer ();
void pfnote ();
void process_file ();
void put_entries ();
void takeprec ();

/*
 * MACRO
 *	xnew -- allocate storage
 *
 * SYNOPSIS
 *	Type *xnew (int n, Type);
 */
#define xnew(n, Type)	((Type *) xmalloc ((n) * sizeof (Type)))



/*
 *	Symbol table stuff.
 *
 * Should probably be implemented with hash table; linked list for now.
 */

enum sym_type
{
  st_none, st_C_struct, st_C_enum, st_C_define, st_C_typedef, st_C_typespec
};

struct stab_entry
{
  char *sym;
  int symlen;
  enum sym_type type;
  struct stab_entry *next;
};

typedef struct stab_entry Stab_entry;
typedef Stab_entry *Stab;

/*
 * NAME
 *	Stab, Stab_entry, stab_create, stab_search, stab_find -- symbol table
 *
 * SYNOPSIS
 *	Types: Stab, Stab_entry, enum sym_type
 *
 *	Stab * stab_create ()
 *
 *	Stab_entry * stab_find (stab, sym)
 *	Stab *stab;
 *	char *sym;
 *
 *	Stab_entry * stab_search (stab, sym)
 *	Stab *stab;
 *	char *sym;
 *
 * DESCRIPTION
 *	stab_create creates a Stab, a symbol table object, and returns a
 *	pointer to it.  stab_find finds a symbol in a Stab; it returns a
 *	pointer to the Stab_entry if found, otherwise NULL.  stab_search
 *	is like stab_find, except that it creates a new Stab_entry,
 *	initialized with type = st_none, if one did not exist already
 *	(it never returns NULL).
 *
 *	A Stab_entry is a structure that contains at least the following
 *	members:
 *
 *		char *name;		// must not be modified
 *		enum sym_type type;	// should be set
 *
 *	The type field is initially set to st_none; it should be set to
 *	something else by the caller of stab_search.  Other possible values
 *	of an enum sym_type can be added.
 */

Stab *
stab_create ()
{
  Stab *sp;
  sp = xnew (1, Stab);
  *sp = NULL;			/* a Stab starts out as a null Stab_entry* */
  return sp;
}

Stab_entry *
stab_find (stab, sym, symlen)
     Stab *stab;
     register char *sym;
     register int symlen;
{
  register Stab_entry *se;
  for (se = *stab; se != NULL; se = se->next)
    {
      if (se->symlen == symlen && strneq (se->sym, sym, symlen))
	return se;
    }

  return NULL;
}

Stab_entry *
stab_search (stab, sym, symlen)
     register Stab *stab;
     char *sym;
     int symlen;
{
  register Stab_entry *se;
  se = stab_find (stab, sym, symlen);

  if (se == NULL)
    {
      /* make a new one */
      se = xnew (1, Stab_entry);
      se->sym = savenstr (sym, symlen);
      se->symlen = symlen;
      se->type = st_none;
      se->next = *stab;
      *stab = se;
    }

  return se;
}

/*
 * NAME
 *	stab_type -- type of a symbol table entry
 *
 * SYNOPSIS
 *	enum sym_type stab_type (Stab_entry *se);
 *
 * WARNING
 *	May evaluate its argument more than once.
 */

#define stab_type(se)	((se)==NULL ? st_none : (se)->type)



typedef int LINENO;

typedef struct
{
  char *p;
  int len;
  FILEPOS linestart;
  LINENO lineno;
  logical rewritten;
} TOKEN;


 /* typedefs are recognized using a simple finite automaton.
  * tydef is its state variable.
  */
typedef enum
{
  none, begin, middle, end
} TYST;

TYST tydef = none;


 /* struct tags for C++ are recognized using another simple
  * finite automaton.  `structdef' is its state variable.
  * This machinery is only invoked for C++; otherwise structdef
  * should remain snone.  However, this machinery can easily be
  * adapted to find structure tags in normal C code.
  */
typedef enum
{
  snone,			/* nothing seen yet */
  skeyseen,			/* struct-like keyword seen */
  stagseen,			/* struct-like tag seen */
  scolonseen,			/* colon seen after struct-like tag */
  sinbody			/* in a class body: recognize member func defs */
} STRUCTST;
STRUCTST structdef = snone;
/*
 * When structdef is stagseen, scolonseen, or sinbody, structtag is the
 * struct tag, and structkey is the preceding struct-like keyword.
 */
char structtag[512];
Stab_entry *structkey;

/*
 * Yet another little state machine to deal with preprocessor lines.
 */
typedef enum
{
  dnone,			/* nothing seen */
  dsharpseen,			/* '#' seen as first char on line */
  ddefineseen,			/* '#' and 'define' seen */
  dignorerest			/* ignore rest of line */
} DEFINEST;
DEFINEST definedef;

/*
 * LEVEL_OK_FOR_FUNCDEF allows C++ function definition within class body.
 * Currently tydef and structdef stuff (typedefs and struct definitions) are
 * only noticed when level==0, but that may change.
 *
 * Note that this macro may only be evaluated inside C_entries().  It is
 * for self-documentation only.
 */
#define LEVEL_OK_FOR_FUNCDEF()					\
	(level==0 || (c_ext && level==1 && structdef==sinbody))

/*
 * next_token_is_func
 *	set this to TRUE, and the next token considered is called a function.
 */
logical next_token_is_func;

/* C extensions.  Currently all listed extensions are C++ dialects, so
 * `c_ext' is used as an abbreviation for `c_ext&C_PLPL'.  If a non-C++
 * dialect is added, this must change.
 */
#define C_PLPL	0x1		/* C++ */
#define C_STAR	0x3		/* C* */

char searchar = '/';		/* use /.../ searches 		*/

LINENO lineno;			/* line number of current line */
long charno;			/* current character number */
FILEPOS linepos;		/* start of line (C only) */
FILEPOS prev_linepos;		/* start of previous line (C only) */

long linecharno;		/* charno of start of line; not used by C, but
				 * by every other language.
				 */

char *curfile,			/* current input file name		*/
 *outfile,			/* output file				*/
 *white = " \f\t\n",		/* white chars				*/
 *endtk = " \t\n\"'#()[]{}=-+%*/&|^~!<>;,.:?",	/* token ending chars			*/
 *begtk = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz$",	/* token starting chars			*/
 *intk = "ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz$0123456789",	/* valid in-token chars			*/
 *notgd = ",;";			/* non-valid after-function chars	*/

int append_to_tagfile;		/* -a: append to tags */
int emacs_tags_format;		/* emacs style output (no -e option any more) */
/* The following three default to 1 for etags, but to 0 for ctags.  */
int typedefs;			/* -t: create tags for typedefs */
int typedefs_and_cplusplus;	/* -T: create tags for typedefs, level */
				/* 0 struct/enum/union decls, and C++ */
				/* member functions */
int constantypedefs;		/* -d: create tags for C #define and enum */
				/* constants. Default under etags.  Enum */
				/* constants not implemented. */
				/* -D: opposite of -d.  Default under ctags. */
int update;			/* -u: update tags */
int vgrind_style;		/* -v: create vgrind style index output */
int no_warnings;		/* -w: suppress warnings */
int cxref_style;		/* -x: create cxref style output */
int cplusplus;			/* .[hc] means C++, not C */
int noindentypedefs;		/* -S: ignore indentation in C */

/* Name this program was invoked with.  */
char *progname;

struct option longopts[] = {
  { "append",			no_argument,	   NULL, 'a' },
  { "backward-search",		no_argument,	   NULL, 'B' }, 
  { "c++",			no_argument,	   NULL, 'C' },
  { "cxref",			no_argument,	   NULL, 'x' },
  { "defines",			no_argument,	   NULL, 'd' },
  { "forward-search",		no_argument,	   NULL, 'F' }, 
  { "help",			no_argument,	   NULL, 'H' },
  { "ignore-indentation",	no_argument,	   NULL, 'S' },
  { "include",			required_argument, NULL, 'i' },
  { "no-defines",		no_argument,	   NULL, 'D' },
  { "no-warn",			no_argument,	   NULL, 'w' },
  { "output",			required_argument, NULL, 'o' },
  { "typedefs",			no_argument,	   NULL, 't' },
  { "typedefs-and-c++",		no_argument,	   NULL, 'T' },
  { "update",			no_argument,	   NULL, 'u' }, 
  { "version",			no_argument,	   NULL, 'V' },
  { "vgrind",			no_argument,	   NULL, 'v' }, 
  { 0 }
};

FILE *inf,			/* ioptr for current input file		*/
 *outf;				/* ioptr for tags file			*/

NODE *head;			/* the head of the binary tree of tags	*/

int permit_duplicates = 1;	/* Nonzero means allow duplicate tags.  */

/* A `struct linebuffer' is a structure which holds a line of text.
 `readline' reads a line from a stream into a linebuffer
 and works regardless of the length of the line.  */

struct linebuffer
{
  long size;
  char *buffer;
};

struct linebuffer lb;		/* the current line */
struct linebuffer lb1;		/* sometimes, a previous line in which a token lies */
struct linebuffer filename_lb;	/* used to read in filenames */


void
print_version ()
{
#ifdef CTAGS
  printf ("CTAGS ");
#ifdef ETAGS
  printf ("and ");
#endif
#endif
#ifdef ETAGS
  printf ("ETAGS ");
#endif
  printf ("for Emacs version 19.0.\n");

  exit (0);
}

void
print_help ()
{
  printf ("These are the options accepted by %s.  You may use unambiguous\n\
abbreviations for the long option names.\n\n", progname);

  fputs ("\
-a, --append\n\
        Append tag entries to existing tags file.\n\
-C, --c++\n\
        Treat files with `.c' and `.h' extensions as C++ code, not C\n\
        code.  Files with `.C', `.H', `.cxx', `.hxx', or `.cc'\n\
        extensions are always assumed to be C++ code.\n\
-d, --defines\n\
        Create tag entries for #defines, too.", stdout);

#ifdef ETAGS
  fputs ("  This is the default\n\
        behavior.", stdout);
#endif

  fputs ("\n\
-D, --no-defines\n\
        Don't create tag entries for #defines.", stdout);

#ifdef CTAGS
  fputs ("  This is the default\n\
        behavior.", stdout);
#endif

  puts ("\n\
-o FILE, --output=FILE\n\
        Write the tags to FILE.\n\
-S, --ignore-indentation\n\
        Don't rely on indentation quite as much as normal.  Currently,\n\
        this means not to assume that a closing brace in the first\n\
        column is the final brace of a function or structure\n\
        definition.\n\
-t, --typedefs\n\
        Generate tag entries for typedefs.  This is the default\n\
        behavior.\n\
-T, --typedefs-and-c++\n\
        Generate tag entries for typedefs, struct/enum/union tags, and\n\
        C++ member functions.");

#ifdef ETAGS
  puts ("-i FILE, --include=FILE\n\
        Include a note in tag file indicating that, when searching for\n\
        a tag, one should also consult the tags file FILE after\n\
        checking the current file.");
#endif

#ifdef CTAGS
  puts ("-B, --backward-search\n\
        Write the search commands for the tag entries using '?', the\n\
        backward-search command.\n\
-F, --forward-search\n\
        Write the search commands for the tag entries using '/', the\n\
        forward-search command.\n\
-u, --update\n\
        Update the tag entries for the given files, leaving tag\n\
        entries for other files in place.  Currently, this is\n\
        implemented by deleting the existing entries for the given\n\
        files and then rewriting the new entries at the end of the\n\
        tags file.  It is often faster to simply rebuild the entire\n\
        tag file than to use this.\n\
-v, --vgrind\n\
        Generates an index of items intended for human consumption,\n\
        similar to the output of vgrind.  The index is sorted, and\n\
        gives the page number of each item.\n\
-x, --cxref\n\
        Like --vgrind, but in the style of cxref, rather than vgrind.\n\
        The output uses line numbers instead of page numbers, but\n\
        beyond that the differences are cosmetic; try both to see\n\
        which you like.\n\
-w, --no-warn\n\
        Suppress warning messages about entries defined in multiple\n\
        files.");
#endif

  puts ("-V, --version\n\
        Print the version of the program.\n\
-H, --help\n\
        Print this help message.");

  exit (0);
}


void
main (argc, argv)
     int argc;
     char *argv[];
{
  char cmd[100];
  int i;
  unsigned int nincluded_files = 0;
  char **included_files = (char **) alloca (argc * sizeof (char *));
  char *this_file;
#ifdef VMS
  char got_err;

  extern char *gfnames ();
  extern char *massage_name ();
#endif

  progname = argv[0];

#ifndef CTAGS
  emacs_tags_format = 1;
#else
  emacs_tags_format = 0;
#endif

  /*
   * If etags, always find typedefs and structure tags.  Why not?
   * Also default is to find macro constants.
   */
  if (emacs_tags_format)
    typedefs = typedefs_and_cplusplus = constantypedefs = 1;

  for (;;)
    {
      int opt;
      opt = getopt_long (argc, argv, "aCdDo:StTi:BFuvxwVH", longopts, 0);

      if (opt == EOF)
	break;

      switch (opt)
	{
	case '\0':
	  /* If getopt returns '\0', then it has already processed a
	     long-named option.  We should do nothing.  */
	  break;

	  /* Common options. */
	case 'a':
	  append_to_tagfile++;
	  break;
	case 'C':
	  cplusplus = 1;
	  break;
	case 'd':
	  constantypedefs = 1;
	  break;
	case 'D':
	  constantypedefs = 0;
	  break;
	case 'o':
	  if (outfile)
	    {
	      fprintf (stderr,
		       "%s: -o flag may only be given once\n", progname);
	      goto usage;
	    }
	  outfile = optarg;
	  break;
	case 'S':
	  noindentypedefs++;
	  break;
	case 't':
	  typedefs++;
	  break;
	case 'T':
	  typedefs++;
	  typedefs_and_cplusplus++;
	  break;
	case 'V':
	  print_version ();
	  break;
	case 'H':
	  print_help ();
	  break;

	  /* Etags options */
	case 'i':
	  if (!emacs_tags_format)
	    goto usage;
	  included_files[nincluded_files++] = optarg;
	  break;

	  /* Ctags options. */
	case 'B':
	  searchar = '?';
	  if (emacs_tags_format)
	    goto usage;
	  break;
	case 'F':
	  searchar = '/';
	  if (emacs_tags_format)
	    goto usage;
	  break;
	case 'u':
	  update++;
	  if (emacs_tags_format)
	    goto usage;
	  break;
	case 'v':
	  vgrind_style++;
	  /*FALLTHRU*/
	case 'x':
	  cxref_style++;
	  if (emacs_tags_format)
	    goto usage;
	  break;
	case 'w':
	  no_warnings++;
	  if (emacs_tags_format)
	    goto usage;
	  break;

	default:
	  goto usage;
	}
    }

  if (optind == argc)
    {
      fprintf (stderr, "%s: No input files specified.\n", progname);

    usage:
      fprintf (stderr, "%s: Try '%s --help' for a complete list of options.\n",
	       progname, progname);
      exit (BAD);
    }

  if (outfile == 0)
    {
      outfile = emacs_tags_format ? "TAGS" : "tags";
    }

  init ();			/* set up boolean "functions"		*/

  initbuffer (&lb);
  initbuffer (&lb1);
  initbuffer (&filename_lb);
  /*
   * loop through files finding functions
   */
  if (emacs_tags_format)
    {
      if (streq (outfile, "-"))
	outf = stdout;
      else
	outf = fopen (outfile, append_to_tagfile ? "a" : "w");
      if (!outf)
	{
	  perror (outfile);
	  exit (1);
	}
    }

#ifdef VMS
  argc -= optind;
  argv += optind;
  while (gfnames (&argc, &argv, &got_err) != NULL)
    {
      if (got_err)
	{
	  error ("Can't find file %s\n", this_file);
	  argc--, argv++;
	}
      else
	{
	  this_file = massage_name (this_file);
#if 0
	}
    }				/* solely to balance out the ifdef'd parens above */
#endif
#else
  for (; optind < argc; optind++)
    {
      this_file = argv[optind];
      if (1)
	{
#endif
	  /* Input file named "-" means read file names from stdin
	     and use them.  */
	  if (streq (this_file, "-"))
	    {
	      while (!feof (stdin))
		{
		  (void) readline (&filename_lb, stdin);
		  if (strlen (filename_lb.buffer) > 0)
		    process_file (filename_lb.buffer);
		}
	    }
	  else
	    process_file (this_file);
	}
    }

  if (emacs_tags_format)
    {
      while (nincluded_files-- > 0)
	fprintf (outf, "\f\n%s,include\n", *included_files++);

      (void) fclose (outf);
      exit (0);
    }

  if (cxref_style)
    {
      put_entries (head);
      exit (GOOD);
    }
  if (update)
    {
      /* update cannot be set under VMS, so we may assume that argc
	 and argv have not been munged.  */
      for (i = optind; i < argc; i++)
	{
	  sprintf (cmd,
		   "mv %s OTAGS;fgrep -v '\t%s\t' OTAGS >%s;rm OTAGS",
		   outfile, argv[i], outfile);
	  (void) system (cmd);
	}
      append_to_tagfile++;
    }
  outf = fopen (outfile, append_to_tagfile ? "a" : "w");
  if (outf == NULL)
    {
      perror (outfile);
      exit (GOOD);
    }
  put_entries (head);
  (void) fclose (outf);
  if (update)
    {
      sprintf (cmd, "sort %s -o %s", outfile, outfile);
      (void) system (cmd);
    }
  exit (GOOD);
}


/*
 * This routine is called on each file argument.
 */
void
process_file (file)
     char *file;
{
  struct stat stat_buf;

  stat (file, &stat_buf);
#ifndef S_ISREG
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#endif
#if !defined(S_ISLNK) && defined(S_IFLNK)
#define S_ISLNK(mode) (((mode) & S_IFMT) == S_IFLNK)
#endif
  if (!S_ISREG(stat_buf.st_mode)
#ifdef S_ISLNK
      && !S_ISLNK(stat_buf.st_mode)
#endif
      )
    {
      fprintf (stderr, "Skipping %s: it is not a regular file.\n", file);
      return;
    }

  if (streq (file, outfile) && !streq (outfile, "-"))
    {
      fprintf (stderr, "Skipping inclusion of %s in self.\n", file);
      return;
    }
  if (emacs_tags_format)
    {
      char *cp = rindex (file, '/');
      if (cp)
	++cp;
      else
	cp = file;
    }
  find_entries (file);
  if (emacs_tags_format)
    {
      fprintf (outf, "\f\n%s,%d\n",
	       file, total_size_of_entries (head));
      put_entries (head);
      free_tree (head);
      head = NULL;
    }
}

/*
 * This routine sets up the boolean psuedo-functions which work
 * by seting boolean flags dependent upon the corresponding character
 * Every char which is NOT in that string is not a white char.  Therefore,
 * all of the array "_wht" is set to FALSE, and then the elements
 * subscripted by the chars in "white" are set to TRUE.  Thus "_wht"
 * of a char is TRUE if it is the string "white", else FALSE.
 */
void
init ()
{
  reg char *sp;
  reg int i;

  for (i = 0; i < 0177; i++)
    {
      _wht[i] = _etk[i] = _itk[i] = _btk[i] = FALSE;
      _gd[i] = TRUE;
    }
  for (sp = white; *sp; sp++)
    _wht[*sp] = TRUE;
  for (sp = endtk; *sp; sp++)
    _etk[*sp] = TRUE;
  for (sp = intk; *sp; sp++)
    _itk[*sp] = TRUE;
  for (sp = begtk; *sp; sp++)
    _btk[*sp] = TRUE;
  for (sp = notgd; *sp; sp++)
    _gd[*sp] = FALSE;
  _wht[0] = _wht['\n'];
  _etk[0] = _etk['\n'];
  _btk[0] = _btk['\n'];
  _itk[0] = _itk['\n'];
  _gd[0] = _gd['\n'];
}

/*
 * This routine opens the specified file and calls the function
 * which finds the function and type definitions.
 */
void
find_entries (file)
     char *file;
{
  char *cp;
  void prolog_funcs ();

  inf = fopen (file, "r");
  if (inf == NULL)
    {
      perror (file);
      return;
    }
  curfile = savestr (file);
  cp = rindex (file, '.');

  header_file = (cp && (streq (cp + 1, "h")));

  /* .tex, .aux or .bbl implies LaTeX source code */
  if (cp && (streq (cp + 1, "tex") || streq (cp + 1, "aux")
	     || streq (cp + 1, "bbl")))
    {
      TEX_funcs (inf);
      goto close_and_return;
    }
  /* .l or .el or .lisp (or .cl or .clisp or ...) implies lisp source code */
  if (cp && (streq (cp + 1, "l")
	     || streq (cp + 1, "el")
	     || streq (cp + 1, "lsp")
	     || streq (cp + 1, "lisp")
	     || streq (cp + 1, "cl")
	     || streq (cp + 1, "clisp")))
    {
      L_funcs (inf);
      goto close_and_return;
    }
  /* .scm or .sm or .scheme or ... implies scheme source code */
  if (cp && (streq (cp + 1, "sm")
	     || streq (cp + 1, "scm")
	     || streq (cp + 1, "scheme")
	     || streq (cp + 1, "t")
	     || streq (cp + 1, "sch")
	     || streq (cp + 1, "SM")
	     || streq (cp + 1, "SCM")
	     /* The `SCM' or `scm' prefix with a version number */
             || (cp[-1] == 'm' && cp[-2] == 'c' && cp[-3] == 's'
		 && string_numeric_p (cp + 1))
             || (cp[-1] == 'M' && cp[-2] == 'C' && cp[-3] == 'S'
		 && string_numeric_p (cp + 1))))
    {
      Scheme_funcs (inf);
      fclose (inf);
      return;
    }
  /* Assume that ".s" or ".a" is assembly code. -wolfgang.  */
  if (cp && (cp[1] == 's' || cp[1] == 'a') && cp[2] == '\0')
    {
      Asm_funcs (inf);
      fclose (inf);
      return;
    }
  /* .C or .H or .cxx or .hxx or .cc: a C++ file */
  if (cp && (streq (cp + 1, "C")
	     || streq (cp + 1, "H")
	     || streq (cp + 1, "cxx")
	     || streq (cp + 1, "hxx")
	     || streq (cp + 1, "cc")))
    {
      C_entries (C_PLPL);	/* C++ */
      goto close_and_return;
    }
  /* .cs or .hs: a C* file */
  if (cp && (cp[1] == 'c' || cp[1] == 'h') && cp[2] == 's' && cp[3] == '\0')
    {
      C_entries (C_STAR);
      goto close_and_return;
    }
  /* .pl implies prolog source code */
  if (cp && !strcmp (cp + 1, "pl"))
    {
      prolog_funcs (inf);
      goto close_and_return;
    }
  /* .p or .pas: a Pascal file */
  if (cp && (streq (cp + 1, "p")
	     || streq (cp + 1, "pas")))
    {
      PAS_funcs (inf);
      goto close_and_return;
    }
  /* if not a .c or .h or .y file, try fortran */
  else if (cp && ((cp[1] != 'c'
		   && cp[1] != 'h'
		   && cp[1] != 'y')
		  || (cp[1] != 0 && cp[2] != 0)))
    {
      if (PF_funcs (inf) != 0)
	goto close_and_return;
      rewind (inf);		/* no fortran tags found, try C */
    }
  C_entries (cplusplus ? C_PLPL : 0);

close_and_return:
  (void) fclose (inf);
}

/* Nonzero if string STR is composed of digits.  */

int
string_numeric_p (str)
     char *str;
{
  while (*str)
    {
      if (*str < '0' || *str > '9')
	return 0;
    }
  return 1;
}

/* Record a tag. */
/* Should take a TOKEN* instead!! */

void
pfnote (name, is_func, rewritten, linestart, linelen, lno, cno)
     char *name;		/* tag name */
     logical is_func;		/* function or type name? */
     logical rewritten;		/* tag different from text of definition? */
     char *linestart;
     int linelen;
     int lno;
     long cno;
{
  register char *fp;
  register NODE *np;
  char tem[51];
  char c;

  np = (NODE *) malloc (sizeof (NODE));
  if (np == NULL)
    {
      if (!emacs_tags_format)
	{
	  /* It's okay to output early in etags -- it only disrupts the
	   * character count of the tag entries, which is no longer used
	   * by tags.el anyway.
	   */
	  error ("too many entries to sort");
	}
      put_entries (head);
      free_tree (head);
      head = NULL;
      np = xnew (1, NODE);
    }
  /* If ctags mode, change name "main" to M<thisfilename>. */
  if (!emacs_tags_format && !cxref_style && streq (name, "main"))
    {
      fp = rindex (curfile, '/');
      name = concat ("M", fp == 0 ? curfile : fp + 1, "");
      fp = rindex (name, '.');
      if (fp && fp[1] != '\0' && fp[2] == '\0')
	*fp = 0;
      rewritten = TRUE;
    }
  np->name = savestr (name);
  np->file = curfile;
  np->is_func = is_func;
  np->rewritten = rewritten;
  np->lno = lno;
  /* UNCOMMENT THE +1 HERE: */
  np->cno = cno /* + 1 */ ;	/* our char numbers are 0-base; emacs's are 1-base */
  np->left = np->right = 0;
  if (emacs_tags_format)
    {
      c = linestart[linelen];
      linestart[linelen] = 0;
    }
  else if (cxref_style == 0)
    {
      sprintf (tem, strlen (linestart) < 50 ? "%s$" : "%.50s", linestart);
      linestart = tem;
    }
  np->pat = savestr (linestart);
  if (emacs_tags_format)
    {
      linestart[linelen] = c;
    }

  add_node (np, &head);
}

/*
 * free_tree ()
 *	recurse on left children, iterate on right children.
 */
void
free_tree (node)
     register NODE *node;
{
  while (node)
    {
      register NODE *node_right = node->right;
      free_tree (node->left);
      free (node->name);
      free (node->pat);
      free ((char *) node);
      node = node_right;
    }
}

/*
 * add_node ()
 *	Adds a node to the tree of nodes.  In etags mode, we don't keep
 *	it sorted; we just keep a linear list.  In ctags mode, maintain
 *	an ordered tree, with no attempt at balancing.
 *
 *	add_node is the only function allowed to add nodes, so it can
 *	maintain state.
 */
void
add_node (node, cur_node_p)
     NODE *node, **cur_node_p;
{
  register int dif;
  register NODE *cur_node = *cur_node_p;
  static NODE *last_node = NULL;/* careful */

  if (cur_node == NULL)
    {
      *cur_node_p = node;
      last_node = node;
      return;
    }

  if (emacs_tags_format)
    {
      /* Etags Mode */
      if (!last_node)
	fatal ("internal error in add_node");
      last_node->right = node;
      last_node = node;
    }
  else
    {
      /* Ctags Mode */
      dif = strcmp (node->name, cur_node->name);

      /*
       * If this tag name matches an existing one, then
       * do not add the node, but maybe print a warning.
       */
      if (!dif)
	{
	  if (node->file == cur_node->file)
	    {
	      if (!no_warnings)
		{
		  fprintf (stderr, "Duplicate entry in file %s, line %d: %s\n",
			   node->file, lineno, node->name);
		  fprintf (stderr, "Second entry ignored\n");
		}
	      return;
	    }
	  if (!cur_node->been_warned && !no_warnings)
	    {
	      fprintf (stderr,
		  "Duplicate entry in files %s and %s: %s (Warning only)\n",
		       node->file, cur_node->file, node->name);
	    }
	  cur_node->been_warned = TRUE;
	  return;
	}

      /* Maybe refuse to add duplicate nodes.  */
      if (!permit_duplicates)
	{
	  if (!strcmp (node->name, cur_node->name)
	      && !strcmp (node->file, cur_node->file))
	    return;
	}

      /* Actually add the node */
      add_node (node, dif < 0 ? &cur_node->left : &cur_node->right);
    }
}

void
put_entries (node)
     reg NODE *node;
{
  reg char *sp;

  if (node == NULL)
    return;

  /* Output subentries that precede this one */
  put_entries (node->left);

  /* Output this entry */

  if (emacs_tags_format)
    {
      if (node->rewritten)
	{
	  fprintf (outf, "%s\177%s\001%d,%d\n",
		   node->name, node->pat, node->lno, node->cno);
	}
      else
	{
	  fprintf (outf, "%s\177%d,%d\n",
		   node->pat, node->lno, node->cno);
	}
    }
  else if (!cxref_style)
    {
      fprintf (outf, "%s\t%s\t",
	       node->name, node->file);

      if (node->is_func)
	{			/* a function */
	  putc (searchar, outf);
	  putc ('^', outf);

	  for (sp = node->pat; *sp; sp++)
	    {
	      if (*sp == '\\' || *sp == searchar)
		putc ('\\', outf);
	      putc (*sp, outf);
	    }
	  putc (searchar, outf);
	}
      else
	{			/* a typedef; text pattern inadequate */
	  fprintf (outf, "%d", node->lno);
	}
      putc ('\n', outf);
    }
  else if (vgrind_style)
    fprintf (stdout, "%s %s %d\n",
	     node->name, node->file, (node->lno + 63) / 64);
  else
    fprintf (stdout, "%-16s %3d %-16s %s\n",
	     node->name, node->lno, node->file, node->pat);

  /* Output subentries that follow this one */
  put_entries (node->right);
}

/* Length of a number's decimal representation. */
int
number_len (num)
     long num;
{
  int len = 0;
  if (!num)
    return 1;
  for (; num; num /= 10)
    ++len;
  return len;
}

/*
 * Return total number of characters that put_entries will output for
 * the nodes in the subtree of the specified node.  Works only if emacs_tags_format
 * is set, but called only in that case.  This count is irrelevant with
 * the new tags.el, but is still supplied for backward compatibility.
 */
int
total_size_of_entries (node)
     reg NODE *node;
{
  reg int total;

  if (node == NULL)
    return 0;

  total = 0;
  for (; node; node = node->right)
    {
      /* Count left subentries. */
      total += total_size_of_entries (node->left);

      /* Count this entry */
      total += strlen (node->pat) + 1;
      total += number_len ((long) node->lno) + 1 + number_len (node->cno) + 1;
      if (node->rewritten)
	total += 1 + strlen (node->name);	/* \001name */
    }

  return total;
}

/*
 * The C symbol tables.
 */

Stab *C_stab, *C_PLPL_stab, *C_STAR_stab;

/*
 * SYNOPSIS
 *	Stab *get_C_stab (int c_ext);
 */
#define get_C_stab(c_ext) ((c_ext&C_STAR) ? C_STAR_stab :		\
			   c_ext ? C_PLPL_stab :			\
			   C_stab)

void
add_keyword (stab, sym, type)
     Stab *stab;
     char *sym;
     enum sym_type type;
{
  stab_search (stab, sym, strlen (sym))->type = type;
}

Stab *
C_create_stab (c_ext)
     int c_ext;
{
  Stab *stab;

  stab = stab_create ();

  /* C, C++ and C* */
  if (c_ext & C_PLPL)
    add_keyword (stab, "class", st_C_struct);
  if (c_ext & C_STAR)
    add_keyword (stab, "domain", st_C_struct);
  add_keyword (stab, "union", st_C_struct);
  add_keyword (stab, "struct", st_C_struct);
  add_keyword (stab, "enum", st_C_enum);
  add_keyword (stab, "typedef", st_C_typedef);
  add_keyword (stab, "define", st_C_define);
  add_keyword (stab, "long", st_C_typespec);
  add_keyword (stab, "short", st_C_typespec);
  add_keyword (stab, "int", st_C_typespec);
  add_keyword (stab, "char", st_C_typespec);
  add_keyword (stab, "float", st_C_typespec);
  add_keyword (stab, "double", st_C_typespec);
  add_keyword (stab, "signed", st_C_typespec);
  add_keyword (stab, "unsigned", st_C_typespec);
  add_keyword (stab, "const", st_C_typespec);
  add_keyword (stab, "volatile", st_C_typespec);

  return stab;
}

void
C_create_stabs ()
{
  C_stab = C_create_stab (0);
  C_PLPL_stab = C_create_stab (C_PLPL);
  C_STAR_stab = C_create_stab (C_STAR | C_PLPL);
}

/*
 * C_entries ()
 *	This routine finds functions and typedefs in C syntax and adds them
 *	to the list.
 */

#define CNL_SAVE_DEFINEDEF						\
{									\
  prev_linepos = linepos;						\
  SET_FILEPOS (linepos, inf, charno);					\
  lineno++;								\
  charno += readline (&lb, inf);					\
  lp = lb.buffer;							\
}

#define CNL								\
{									\
  CNL_SAVE_DEFINEDEF;							\
  definedef = dnone;							\
}

void
C_entries (c_ext)
     int c_ext;			/* extension of C? */
{
  register int c;		/* latest char read; '\0' for end of line */
  register int tokoff;		/* offset in line of beginning of latest token */
  register int toklen;		/* length of latest token */
  register char *lp;		/* pointer one beyond the character `c' */
  logical incomm, inquote, inchar, midtoken;
  int level;			/* current curly brace level */
  char tokb[BUFSIZ];

  lineno = 0;
  charno = 0;
  lp = lb.buffer;
  *lp = 0;

  definedef = dnone;
  gotone = midtoken = inquote = inchar = incomm = FALSE;
  level = 0;
  tydef = none;
  next_token_is_func = 0;

  C_create_stabs ();

  while (!feof (inf))
    {
      c = *lp++;
      if (c == '\\')
	{
	  /* If we're at the end of the line, the next character is a
	     '\0'; don't skip it, because it's the thing that tells us
	     to read the next line.  */
	  if (*lp == 0)
	    continue;
	  lp++;
	  c = ' ';
	}
      else if (incomm)
	{
	  if (c == '*' && *lp == '/')
	    {
	      c = *lp++;
	      incomm = FALSE;
	    }
	}
      else if (inquote)
	{
	  if (c == '"')
	    inquote = FALSE;
	  else if (c == '\\')
	    c = *lp++;
	}
      else if (inchar)
	{
	  if (c == '\'')
	    inchar = FALSE;
	  continue;
	}
      else
	switch (c)
	  {
	  case '"':
	    inquote = TRUE;
	    continue;
	  case '\'':
	    inchar = TRUE;
	    continue;
	  case '/':
	    if (*lp == '*')
	      {
		lp++;
		incomm = TRUE;
	      }
	    else if (c_ext && *lp == '/')
	      {
		c = 0;
		break;
	      }
	    continue;
	  case '#':
	    if (lp == lb.buffer + 1 && definedef == dnone)
	      definedef = dsharpseen;
	    continue;

	    /*
	     * The next two are to help the strucdef state machine.
	     * They break when they are finished, so they don't interfere
	     * with anything else that is going on.
	     */
	  case ':':
	    if (structdef == stagseen)
	      structdef = scolonseen;
	    break;
	    /* Not a struct definition when semicolon seen in non-sinbody context. */
	  case ';':
	    if (structdef != snone && structdef != sinbody)
	      {
		structdef = snone;
		(void) strcpy (structtag, "<error 1>");
	      }
	    break;

	  case '{':
	    if (tydef == begin)
	      {
		tydef = middle;
	      }
	    switch (structdef)
	      {
	      case skeyseen:	/* unnamed struct */
		structtag[0] = '\0';
		/* FALLTHRU */
	      case stagseen:
	      case scolonseen:	/* named struct */
		structdef = sinbody;
		break;
	      }
	    level++;
	    continue;
	  case '}':
	    if (!noindentypedefs && lp == lb.buffer + 1)
	      level = 0;	/* reset level if first column */
	    else if (level > 0)
	      level--;
	    if (level == 0 && tydef == middle)
	      {
		tydef = end;
	      }
	    if (level == 0)
	      {
		structdef = snone;
		(void) strcpy (structtag, "<error 2>");
	      }
	    continue;
	  }
      if (LEVEL_OK_FOR_FUNCDEF () && !inquote && !incomm && gotone == FALSE)
	{
	  if (midtoken)
	    {
	      if (endtoken (c))
		{
		  if (c_ext && c == ':' && *lp == ':' && intoken (*(lp + 1)))
		    {
		      /*
		       * This handles :: in the middle, but not at beginning
		       * of an identifier.
		       */
		      lp += 2;
		      toklen += 3;
		    }
		  else
		    {
		      /* The following is no longer true,
			 now that we advance to the next line
			 at the end of processing the character.  */
		      /*
		       * We've just finished lexing an identifier.
		       * Note that if `c' is '\0', `lb' is the NEXT
		       * line, `lp' points to the beginning of it, and
		       * old pointers into `lb.buffer' may no longer be
		       * valid, since `lb.buffer' may have been
		       * reallocated.  In this case (which corresponds
		       * to an identifier followed immediately by a
		       * newline), we re-read the line into lb1.
		       *
		       * This would be faster if the previous line's
		       * buffer were always saved.
		       */
		      logical is_func;
		      char *tok_linebuf;
		      TOKEN tok;
		      logical bingo, tok_at_end_of_line;
		      char *lp_tmp;	/* addressable */

#if 0
		      if (c == '\0')
			{
			  getline (GET_COOKIE (prev_linepos));
			  tok_linebuf = lb1.buffer;
			  tok_at_end_of_line = TRUE;
			  tok.linestart = prev_linepos;
			  tok.lineno = lineno - 1;
			}
		      else
#endif
			{
			  tok_linebuf = lb.buffer;
			  tok_at_end_of_line = FALSE;
			  tok.linestart = linepos;
			  tok.lineno = lineno;
			}
		      tok.p = tok_linebuf + tokoff;
		      tok.len = toklen;
		      tok.rewritten = FALSE;
		      lp_tmp = lp;
		      bingo = consider_token (c, &lp_tmp, &tok,
					      &is_func, c_ext, level);
		      lp = lp_tmp;
		      if (bingo)
			{
			  if (GET_CHARNO (tok.linestart) != GET_CHARNO (linepos)
			      && !tok_at_end_of_line)
			    {
			      /*
			       * Resynchronize tok.p to point into the right
			       * linebuffer.
			       */
			      getline (GET_COOKIE (tok.linestart));
			      if (!tok.rewritten)
				tok.p = lb1.buffer + (tok.p - tok_linebuf);
			      tok_linebuf = lb1.buffer;
			    }
			  if (structdef == sinbody && definedef == dnone && is_func)
			    {	/* function defined in C++ class body */
			      sprintf (tokb, "%s::%.*s",
				       structtag[0] == '\0' ? "_anonymous_"
				       : structtag,
				       tok.len, tok.p);
			      tok.rewritten = TRUE;
			    }
			  else
			    {
			      sprintf (tokb, "%.*s", tok.len, tok.p);
			    }
			  pfnote (tokb, is_func, tok.rewritten, tok_linebuf,
			     tokoff + toklen + (tok_at_end_of_line ? 0 : 1),
				  tok.lineno, GET_CHARNO (tok.linestart));
			  gotone = is_func;	/* function */
			}
		      midtoken = FALSE;
		    }
		}
	      else if (intoken (c))
		toklen++;
	    }
	  else if (begtoken (c))
	    {
	      tokoff = lp - 1 - lb.buffer;
	      toklen = 1;
	      midtoken = TRUE;
	    }
	}
      /* Detect end of line, after having handled the last token on the line.  */
      if (c == 0)
	{
	  CNL;
	  gotone = FALSE;
	}
      if (c == ';' && tydef == end)	/* clean with typedefs */
	tydef = none;
    }
}

/*
 * consider_token ()
 *	checks to see if the current token is at the start of a
 *	function, or corresponds to a typedef.  It updates the input
 *	line pointer *LPP so that the '(' will be in it when it returns.
 *
 *	*IS_FUNC gets TRUE iff the token is a function.
 *	C_EXT is which language we are looking at.
 *
 *	In the future we will need some way to adjust where the end of
 *	the token is; for instance, implementing the C++ keyword
 *	`operator' properly will adjust the end of the token to be after
 *	whatever follows `operator'.
 *
 * Globals
 *	structdef	IN OUT
 *	definedef	IN OUT
 *	tydef		IN OUT
 */

logical
consider_token (c, lpp, tokp, is_func, c_ext, level)
     reg char c;		/* IN: first char after the token */
     char **lpp;		/* IN OUT: *lpp points to 2nd char after the token */
     reg TOKEN *tokp;		/* IN */
     logical *is_func;		/* OUT */
     int c_ext;			/* IN */
     int level;			/* IN */
{
  reg char *lp = *lpp;
  logical firsttok;		/* TRUE if have seen first token in ()'s */
  Stab_entry *tokse = stab_find (get_C_stab (c_ext), tokp->p, tokp->len);
  enum sym_type toktype = stab_type (tokse);

  *is_func = TRUE;		/* a function */

  /*
   * Advance the definedef state machine.  We set `gotone' for good measure;
   * it's redundant.
   */
  switch (definedef)
    {
    case dnone:
      /* We're not on a preprocessor line. */
      break;
    case dsharpseen:
      if (toktype == st_C_define)
	{
	  definedef = ddefineseen;
	  gotone = FALSE;
	}
      else
	{
	  definedef = dignorerest;
	  gotone = TRUE;
	}
      goto badone;
    case ddefineseen:
      /*
       * Make a tag for any macro.
       * This will flub up if there is a newline immediately following
       * the macro name.
       */
      *is_func = (c == '(');
      definedef = dignorerest;
      gotone = TRUE;
      if (!*is_func && !constantypedefs)
	goto badone;
      goto goodone;
    case dignorerest:
      goto badone;
    default:
      error ("internal error: definedef value");
    }

  /*
   * Skip whitespace and comments after the token.  This loop should
   * also skip C++ comments.
   */
  while (1)
    {
      /* At whitespace => skip it.  */
      if (iswhite (c))
	{
	  c = *lp++;
	}
      /* At a comment => skip to end of comment.  */
      else if (c == '/' && *lp == '*')
	{
	  /* If we find a comment, skip it.  */
	  while (!(c == '*' && *lp == '/'))
	    {
	      c = *lp++;
	      if (c == 0)
		{
		  lp--;
		  break;
		}
	    }
	  if (c == '*' && *lp == '/')
	    {
	      lp++;		/* lp now points past the '/' */
	      c = *lp++;	/* c is now the --whatever-- after the '/' */
	    }
	}
      else
	break;

      /* If we arrived at eof or eol, decide which one it is.
	 If it's eol, advance to the next line.  */

      if (c == 0)
	{
	  lp--;
	  break;
	}
    }

  /*
   * If you have custom token types, or when configuration files can
   * define custom token types, this switch will be larger.
   */
  switch (toktype)
    {
    case st_C_typedef:
      if (typedefs)
	{
	  tydef = begin;
	  goto badone;
	}
      break;
    case st_C_typespec:
      if (tydef == begin || tydef == end)
	{
	  tydef = end;
	  goto badone;
	}
      break;
    }

  /*
   * This structdef business is currently only invoked when level==0.
   * It should be recursively invoked whatever the level, and a stack of
   * states kept, to allow for definitions of structs within structs.
   *
   * This structdef business is NOT invoked when we are ctags and the
   * file is plain C.  This is because a struct tag may have the same
   * name as another tag, and this loses with ctags.
   *
   * This if statement deals with the tydef state machine as follows: if
   * tydef==begin and token is struct/union/class/enum, goto badone.
   * All the other code here is for the structdef state machine.
   */
  switch (toktype)
    {
    case st_C_struct:
    case st_C_enum:
      if (tydef == begin || (typedefs_and_cplusplus && level == 0 && structdef == snone))
	{
	  structdef = skeyseen;
	  structkey = tokse;
	}
      goto badone;
    }

  if (structdef == skeyseen)
    {
      /* If next char is '{' or (for C++) ':', found a structure tag. */
      if (c == '{' || (c_ext && c == ':'))
	{
	  /*
	   * We should do this slightly differently for straight C:
	   * instead of defining `tag', as we now do, we should define
	   * `struct tag'.  (Do this only if the find-tag defaulting is
	   * done on a sophisticated per-mode basis, so that if the user
	   * says meta-. anywhere in `struct foo', the default comes out
	   * `struct foo', not `struct' or `foo'.)  This will require
	   * remembering which keyword (struct/union/class/enum) we saw, as a
	   * Stab_entry* -- this will also make it possible to merge the
	   * skeyseen and senumseen states, if we want.
	   */
	  if (stab_type (structkey) == st_C_struct)
	    {
	      (void) strncpy (structtag, tokp->p, tokp->len);
	      structtag[tokp->len] = '\0';	/* for struct/union/class */
	      structdef = stagseen;
	    }
	  else
	    {
	      structtag[0] = '\0';	/* for enum */
	    }
	  *is_func = FALSE;	/* not a function */
	  goto goodone;
	}
      else
	{
	  /* Not a definition: reset structdef */
	  structdef = snone;
	  (void) strcpy (structtag, "<error 3>");
	}
      /* Now what?  And how does/should this stuff interact with tydef?? */
      /* Also maybe reset lp to *lpp for benefit of the function finding code. */
    }
  if (tydef == begin)
    {
      tydef = end;
      goto badone;
    }
  if (tydef == end)
    {
      *is_func = 0;
      goto goodone;
    }
  /* Detect GNUmacs's function-defining macros. */
  if (definedef == dnone)
    {
      if (strneq (tokp->p, "DEF", 3)
	  || strneq (tokp->p, "ENTRY", 5)
	  || strneq (tokp->p, "SYSCALL", 7)
	  || strneq (tokp->p, "PSEUDO", 6))
	{
	  next_token_is_func = TRUE;
	  goto badone;
	}
      else if (strneq (tokp->p, "EXFUN", 5))
	{
	  next_token_is_func = FALSE;
	  goto badone;
	}
    }
  if (next_token_is_func)
    {
      next_token_is_func = FALSE;
      goto goodone;
    }
  if (c != '(')
    goto badone;
  firsttok = FALSE;
  while ((c = *lp++) != ')')
    {
      if (c == 0)
	{
	  lp--;
	  break;
	}
      /*
	* This line used to confuse ctags:
	*	int	(*oldhup)();
	* This fixes it. A nonwhite char before the first
	* token, other than a / (in case of a comment in there)
	* makes this not a declaration.
	*/
      if (begtoken (c) || c == '/')
	firsttok++;
      else if (!iswhite (c) && !firsttok)
	goto badone;
    }
  while (iswhite (c = *lp++))
    {
      if (c == 0)
	{
	  lp--;
	  break;
	}
    }
  if (!isgood (c))
    goto badone;

goodone:
  *lpp = lp - 1;
  return TRUE;

badone:
  *lpp = lp - 1;
  return FALSE;
}

void
getline (atcookie)
     long atcookie;
{
  long saveftell = ftell (inf);

  (void) fseek (inf, atcookie, 0);
  (void) readline (&lb1, inf);
  (void) fseek (inf, saveftell, 0);
}

/* Fortran parsing */

char *dbp;
int pfcnt;

int
PF_funcs (fi)
     FILE *fi;
{
  lineno = 0;
  charno = 0;
  pfcnt = 0;

  while (!feof (fi))
    {
      lineno++;
      linecharno = charno;
      charno += readline (&lb, fi);
      dbp = lb.buffer;
      if (*dbp == '%')
	dbp++;			/* Ratfor escape to fortran */
      while (isspace (*dbp))
	dbp++;
      if (*dbp == 0)
	continue;
      switch (*dbp | ' ')
	{
	case 'i':
	  if (tail ("integer"))
	    takeprec ();
	  break;
	case 'r':
	  if (tail ("real"))
	    takeprec ();
	  break;
	case 'l':
	  if (tail ("logical"))
	    takeprec ();
	  break;
	case 'c':
	  if (tail ("complex") || tail ("character"))
	    takeprec ();
	  break;
	case 'd':
	  if (tail ("double"))
	    {
	      while (isspace (*dbp))
		dbp++;
	      if (*dbp == 0)
		continue;
	      if (tail ("precision"))
		break;
	      continue;
	    }
	  break;
	}
      while (isspace (*dbp))
	dbp++;
      if (*dbp == 0)
	continue;
      switch (*dbp | ' ')
	{
	case 'f':
	  if (tail ("function"))
	    getit ();
	  continue;
	case 's':
	  if (tail ("subroutine"))
	    getit ();
	  continue;
	case 'p':
	  if (tail ("program"))
	    {
	      getit ();
	      continue;
	    }
	  if (tail ("procedure"))
	    getit ();
	  continue;
	}
    }
  return (pfcnt);
}

logical
tail (cp)
     char *cp;
{
  register int len = 0;

  while (*cp && (*cp & ~' ') == ((*(dbp + len)) & ~' '))
    cp++, len++;
  if (*cp == 0)
    {
      dbp += len;
      return (1);
    }
  return (0);
}

void
takeprec ()
{
  while (isspace (*dbp))
    dbp++;
  if (*dbp != '*')
    return;
  dbp++;
  while (isspace (*dbp))
    dbp++;
  if (!isdigit (*dbp))
    {
      --dbp;			/* force failure */
      return;
    }
  do
    dbp++;
  while (isdigit (*dbp));
}

void
getit ()
{
  register char *cp;
  char c;
  char nambuf[BUFSIZ];

  while (isspace (*dbp))
    dbp++;
  if (*dbp == 0
      || (!isalpha (*dbp)
	  && *dbp != '_'
	  && *dbp != '$'))
    return;
  for (cp = dbp + 1; *cp && (isalpha (*cp) || isdigit (*cp)
			     || (*cp == '_') || (*cp == '$')); cp++)
    continue;
  c = cp[0];
  cp[0] = 0;
  (void) strcpy (nambuf, dbp);
  cp[0] = c;
  pfnote (nambuf, TRUE, FALSE, lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
  pfcnt++;
}

/* Handle a file of assembler code.  */

void
Asm_funcs (fi)
     FILE *fi;
{
  int i;
  register char c;

  lineno = 0;
  charno = 0;
  pfcnt = 0;

  while (!feof (fi))
    {
      lineno++;
      linecharno = charno;
      charno += readline (&lb, fi);
      dbp = lb.buffer;

      for (i = 0; ((c = dbp[i]) && !isspace (c)) && (c != ':'); i++)
	;

      if ((i > 0) && (c == ':'))
	getit ();
    }
}

/* Added by Mosur Mohan, 4/22/88 */
/* Pascal parsing                */

#define GET_NEW_LINE \
{ \
  linecharno = charno; lineno++; \
  charno += 1 + readline (&lb, inf); \
  dbp = lb.buffer; \
}

/*  Locates tags for procedures & functions.
 *  Doesn't do any type- or var-definitions.
 *  It does look for the keyword "extern" or "forward"
 *  immediately following the procedure statement;
 *  if found, the tag is skipped.
 */

void
PAS_funcs (fi)
     FILE *fi;
{
  struct linebuffer tline;	/* mostly copied from C_entries */
  long save_lcno;
  int save_lineno;
  char c, *cp;
  char nambuf[BUFSIZ];

  logical			/* each of these flags is TRUE iff: */
    incomm1,			/* point is inside {..} comment */
    incomm2,			/* point is inside (*..*) comment */
    inquote,			/* point is inside '..' string */
    get_tagname,		/* point is after PROCEDURE/FUNCTION */
  /*   keyword, so next item = potential tag */
    found_tag,			/* point is after a potential tag */
    inparms,			/* point is within parameter-list */
    verify_tag;			/* point has passed the parm-list, so the */
  /*   next token will determine whether    */
  /*   this is a FORWARD/EXTERN to be       */
  /*   ignored, or whether it is a real tag */

  lineno = 0;
  charno = 0;
  dbp = lb.buffer;
  *dbp = 0;
  initbuffer (&tline);

  incomm1 = incomm2 = inquote = FALSE;
  found_tag = FALSE;		/* have a proc name; check if extern */
  get_tagname = FALSE;		/* have found "procedure" keyword    */
  inparms = FALSE;		/* found '(' after "proc"            */
  verify_tag = FALSE;		/* check if "extern" is ahead        */

  /* long main loop to get next char */
  while (!feof (fi))
    {
      c = *dbp++;
      if (c == 0)		/* if end of line */
	{
	  GET_NEW_LINE;
	  if (*dbp == 0)
	    continue;
	  if (!((found_tag && verify_tag) ||
		get_tagname))
	    c = *dbp++;		/* only if don't need *dbp pointing */
	  /* to the beginning of the name of  */
	  /* the procedure or function        */
	}
      if (incomm1)		/* within { - } comments */
	{
	  if (c == '}')
	    incomm1 = FALSE;
	  continue;
	}
      else if (incomm2)		/* within (* - *) comments */
	{
	  if (c == '*')
	    {
	      while ((c = *dbp++) == '*')
		continue;
	      if (c == 0)
		GET_NEW_LINE;
	      if (c == ')')
		incomm2 = FALSE;
	    }
	  continue;
	}
      else if (inquote)
	{
	  if (c == '\'')
	    inquote = FALSE;
	  continue;
	}
      else
	switch (c)
	  {
	  case '\'':
	    inquote = TRUE;	/* found first quote */
	    continue;
	  case '{':		/* found open-{-comment */
	    incomm1 = TRUE;
	    continue;
	  case '(':
	    if (*dbp == '*')	/* found open-(*-comment */
	      {
		incomm2 = TRUE;
		dbp++;
	      }
	    else if (found_tag)	/* found '(' after tag, i.e., parm-list */
	      inparms = TRUE;
	    continue;
	  case ')':		/* end of parms list */
	    if (inparms)
	      inparms = FALSE;
	    continue;
	  case ';':
	    if ((found_tag) && (!inparms))	/* end of proc or fn stmt */
	      {
		verify_tag = TRUE;
		break;
	      }
	    continue;
	  }
      if ((found_tag) && (verify_tag) && (*dbp != ' '))
	{
	  /* check if this is an "extern" declaration */
	  if (*dbp == 0)
	    continue;
	  if ((*dbp == 'e') || (*dbp == 'E'))
	    {
	      if (tail ("extern"))	/* superfluous, really! */
		{
		  found_tag = FALSE;
		  verify_tag = FALSE;
		}
	    }
	  else if ((*dbp == 'f') || (*dbp == 'F'))
	    {
	      if (tail ("forward"))	/*  check for forward reference */
		{
		  found_tag = FALSE;
		  verify_tag = FALSE;
		}
	    }
	  if ((found_tag) && (verify_tag))	/* not external proc, so make tag */
	    {
	      found_tag = FALSE;
	      verify_tag = FALSE;
	      pfnote (nambuf, TRUE, FALSE,
		      tline.buffer, cp - tline.buffer + 1,
		      save_lineno, save_lcno);
	      continue;
	    }
	}
      if (get_tagname)		/* grab name of proc or fn */
	{
	  if (*dbp == 0)
	    continue;

	  /* save all values for later tagging */
	  tline.size = lb.size;
	  strcpy (tline.buffer, lb.buffer);
	  save_lineno = lineno;
	  save_lcno = linecharno;

	  /* grab block name */
	  for (cp = dbp + 1; *cp && (!endtoken (*cp)); cp++)
	    continue;
	  c = cp[0];
	  cp[0] = 0;
	  strcpy (nambuf, dbp);
	  cp[0] = c;
	  dbp = cp;		/* restore dbp to e-o-token */
	  get_tagname = FALSE;
	  found_tag = TRUE;
	  continue;

	  /* and proceed to check for "extern" */
	}
      if ((!incomm1) && (!incomm2) && (!inquote) &&
	  (!found_tag) && (!get_tagname))
	{
	  /* check for proc/fn keywords */
	  switch (c | ' ')
	    {
	    case 'p':
	      if (tail ("rocedure"))	/* c = 'p', dbp has advanced */
		get_tagname = TRUE;
	      continue;
	    case 'f':
	      if (tail ("unction"))
		get_tagname = TRUE;
	      continue;
	    }
	}
    }				/* while not e-o-f */
}

/*
 * lisp tag functions
 * just look for (def or (DEF
 */

void
L_funcs (fi)
     FILE *fi;
{
  lineno = 0;
  charno = 0;
  pfcnt = 0;

  while (!feof (fi))
    {
      lineno++;
      linecharno = charno;
      charno += readline (&lb, fi);
      dbp = lb.buffer;
      if (dbp[0] == '(')
	{
	  if (L_isdef (dbp))
	    {
	      while (!isspace (*dbp))
		dbp++;
	      while (isspace (*dbp))
		dbp++;
	      L_getit ();
	    }
	  else
	    {
	      /* Check for (foo::defmumble name-defined ... */
	      while (*dbp && *dbp != ':' && !isspace (*dbp)
		     && *dbp != '(' && *dbp != ')')
		dbp++;
	      if (*dbp == ':')
		{
		  while (*dbp == ':')
		    dbp++;

		  if (L_isdef (dbp))
		    {
		      while (!isspace (*dbp))
			dbp++;
		      while (isspace (*dbp))
			dbp++;
		      L_getit ();
		    }
		}
	    }
	}
    }
}

int
L_isdef (dbp)
     char *dbp;
{
  return ((dbp[1] == 'D' || dbp[1] == 'd') &&
	  (dbp[2] == 'E' || dbp[2] == 'e') &&
	  (dbp[3] == 'F' || dbp[3] == 'f'));
}

void
L_getit ()
{
  register char *cp;
  char c;
  char nambuf[BUFSIZ];

  if (*dbp == 0)
    return;
  for (cp = dbp + 1; *cp && *cp != '(' && *cp != ' '; cp++)
    continue;
  c = cp[0];
  cp[0] = 0;
  (void) strcpy (nambuf, dbp);
  cp[0] = c;
  pfnote (nambuf, TRUE, FALSE, lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
  pfcnt++;
}

/*
 * Scheme tag functions
 * look for (def... xyzzy
 * look for (def... (xyzzy
 * look for (def ... ((...(xyzzy ....
 * look for (set! xyzzy
 */

static void get_scheme ();

void
Scheme_funcs (fi)
     FILE *fi;
{
  lineno = 0;
  charno = 0;
  pfcnt = 0;

  while (!feof (fi))
    {
      lineno++;
      linecharno = charno;
      charno += readline (&lb, fi);
      dbp = lb.buffer;
      if (dbp[0] == '(' &&
	  (dbp[1] == 'D' || dbp[1] == 'd') &&
	  (dbp[2] == 'E' || dbp[2] == 'e') &&
	  (dbp[3] == 'F' || dbp[3] == 'f'))
	{
	  while (!isspace (*dbp))
	    dbp++;
	  /* Skip over open parens and white space */
	  while (*dbp && (isspace (*dbp) || *dbp == '('))
	    dbp++;
	  get_scheme ();
	}
      if (dbp[0] == '(' &&
	  (dbp[1] == 'S' || dbp[1] == 's') &&
	  (dbp[2] == 'E' || dbp[2] == 'e') &&
	  (dbp[3] == 'T' || dbp[3] == 't') &&
	  (dbp[4] == '!' || dbp[4] == '!') &&
	  (isspace (dbp[5])))
	{
	  while (!isspace (*dbp))
	    dbp++;
	  /* Skip over white space */
	  while (isspace (*dbp))
	    dbp++;
	  get_scheme ();
	}
    }
}

static void
get_scheme ()
{
  register char *cp;
  char c;
  char nambuf[BUFSIZ];

  if (*dbp == 0)
    return;
  /* Go till you get to white space or a syntactic break */
  for (cp = dbp + 1; *cp && *cp != '(' && *cp != ')' && !isspace (*cp); cp++)
    continue;
  /* Null terminate the string there. */
  c = cp[0];
  cp[0] = 0;
  /* Copy the string */
  strcpy (nambuf, dbp);
  /* Unterminate the string */
  cp[0] = c;
  /* Announce the change */
  pfnote (nambuf, TRUE, FALSE, lb.buffer, cp - lb.buffer + 1, lineno, linecharno);
  pfcnt++;
}

/* Find tags in TeX and LaTeX input files.  */

/* TEX_toktab is a table of TeX control sequences that define tags.
   Each TEX_tabent records one such control sequence.
   CONVERT THIS TO USE THE Stab TYPE!! */

struct TEX_tabent
{
  char *name;
  int len;
};

struct TEX_tabent *TEX_toktab = NULL;	/* Table with tag tokens */

/* Default set of control sequences to put into TEX_toktab.
   The value of environment var TEXTAGS is prepended to this.  */

static char *TEX_defenv =
":chapter:section:subsection:subsubsection:eqno:label:ref:cite:bibitem:typeout";

void TEX_mode ();
struct TEX_tabent *TEX_decode_env ();
void TEX_getit ();
int TEX_Token ();

static char TEX_esc = '\\';
static char TEX_opgrp = '{';
static char TEX_clgrp = '}';

/*
 * TeX/LaTeX scanning loop.
 */

void
TEX_funcs (fi)
     FILE *fi;
{
  char *lasthit;

  lineno = 0;
  charno = 0;
  pfcnt = 0;

  /* Select either \ or ! as escape character.  */
  TEX_mode (fi);

  /* Initialize token table once from environment. */
  if (!TEX_toktab)
    TEX_toktab = TEX_decode_env ("TEXTAGS", TEX_defenv);

  while (!feof (fi))
    {
      lineno++;
      linecharno = charno;
      charno += readline (&lb, fi);
      dbp = lb.buffer;
      lasthit = dbp;

      while (!feof (fi))
	{			/* Scan each line in file */
	  lineno++;
	  linecharno = charno;
	  charno += readline (&lb, fi);
	  dbp = lb.buffer;
	  lasthit = dbp;
	  while (dbp = index (dbp, TEX_esc))	/* Look at each escape in line */
	    {
	      register int i;

	      if (!*(++dbp))
		break;
	      linecharno += dbp - lasthit;
	      lasthit = dbp;
	      i = TEX_Token (lasthit);
	      if (0 <= i)
		{
		  TEX_getit (lasthit, TEX_toktab[i].len);
		  break;	/* We only save a line once */
		}
	    }
	}
    }
}

#define TEX_LESC '\\'
#define TEX_SESC '!'
#define TEX_cmt  '%'

/* Figure out whether TeX's escapechar is '\\' or '!' and set grouping */
/* chars accordingly. */

void
TEX_mode (f)
     FILE *f;
{
  int c;

  while ((c = getc (f)) != EOF)
    {
      /* Skip to next line if we hit the TeX comment char. */
      if (c == TEX_cmt)
	while (c != '\n')
	  c = getc (f);
      else if (c == TEX_LESC || c == TEX_SESC )
	break;
    }

  if (c == TEX_LESC)
    {
      TEX_esc = TEX_LESC;
      TEX_opgrp = '{';
      TEX_clgrp = '}';
    }
  else
    {
      TEX_esc = TEX_SESC;
      TEX_opgrp = '<';
      TEX_clgrp = '>';
    }
  rewind (f);
}

/* Read environment and prepend it to the default string. */
/* Build token table. */

struct TEX_tabent *
TEX_decode_env (evarname, defenv)
     char *evarname;
     char *defenv;
{
  register char *env, *p;
  extern char *savenstr (), *index ();

  struct TEX_tabent *tab;
  int size, i;

  /* Append default string to environment. */
  env = getenv (evarname);
  if (!env)
    env = defenv;
  else
    env = concat (env, defenv, "");

  /* Allocate a token table */
  for (size = 1, p = env; p;)
    if ((p = index (p, ':')) && *(++p))
      size++;
  tab = xnew (size, struct TEX_tabent);

  /* Unpack environment string into token table. Be careful about */
  /* zero-length strings (leading ':', "::" and trailing ':') */
  for (i = 0; *env;)
    {
      p = index (env, ':');
      if (!p)			/* End of environment string. */
	p = env + strlen (env);
      if (p - env > 0)
	{			/* Only non-zero strings. */
	  tab[i].name = savenstr (env, p - env);
	  tab[i].len = strlen (tab[i].name);
	  i++;
	}
      if (*p)
	env = p + 1;
      else
	{
	  tab[i].name = NULL;	/* Mark end of table. */
	  tab[i].len = 0;
	  break;
	}
    }
  return tab;
}

/* Record a tag defined by a TeX command of length LEN and starting at NAME.
   The name being defined actually starts at (NAME + LEN + 1).
   But we seem to include the TeX command in the tag name.  */

void
TEX_getit (name, len)
     char *name;
     int len;
{
  char *p = name + len;
  char nambuf[BUFSIZ];

  if (*name == 0)
    return;

  /* Let tag name extend to next group close (or end of line) */
  while (*p && *p != TEX_clgrp)
    p++;
  (void) strncpy (nambuf, name, p - name);
  nambuf[p - name] = 0;

  pfnote (nambuf, TRUE, FALSE, lb.buffer, strlen (lb.buffer), lineno, linecharno);
  pfcnt++;
}

/* If the text at CP matches one of the tag-defining TeX command names,
   return the index of that command in TEX_toktab.
   Otherwise return -1.  */

/* Keep the capital `T' in `Token' for dumb truncating compilers
   (this distinguishes it from `TEX_toktab' */
int
TEX_Token (cp)
     char *cp;
{
  int i;

  for (i = 0; TEX_toktab[i].len > 0; i++)
    if (strncmp (TEX_toktab[i].name, cp, TEX_toktab[i].len) == 0)
      return i;
  return -1;
}

/* Support for Prolog.  */

/* whole head (not only functor, but also arguments)
   is gotten in compound term. */

void
prolog_getit (s, lineno, linecharno)
     char *s;
     int lineno;
     long linecharno;
{
  char nambuf[BUFSIZ], *save_s, tmpc;
  int insquote, npar;

  save_s = s;
  insquote = FALSE;
  npar = 0;
  while (1)
    {
      if (*s == '\0')		/* syntax error. */
	return;
      else if (insquote && *s == '\'' && *(s + 1) == '\'')
	s += 2;
      else if (*s == '\'')
	{
	  insquote = !insquote;
	  s++;
	}
      else if (!insquote && *s == '(')
	{
	  npar++;
	  s++;
	}
      else if (!insquote && *s == ')')
	{
	  npar--;
	  s++;
	  if (npar == 0)
	    break;
	  else if (npar < 0)	/* syntax error. */
	    return;
	}
      else if (!insquote && *s == '.' && (isspace (*(s + 1)) || *(s + 1) == '\0'))
	{			/* fullstop. */
	  if (npar != 0)	/* syntax error. */
	    return;
	  s++;
	  break;
	}
      else
	s++;
    }
  tmpc = *s;
  *s = '\0';
  strcpy (nambuf, save_s);
  *s = tmpc;
  pfnote (nambuf, TRUE, save_s, strlen (nambuf), lineno, linecharno);
}

/* It is assumed that prolog predicate starts from column 0. */

void
prolog_funcs (fi)
     FILE *fi;
{
  void skip_comment (), prolog_getit ();

  lineno = linecharno = charno = 0;
  while (!feof (fi))
    {
      lineno++;
      linecharno += charno;
      charno = readline (&lb, fi) + 1;	/* 1 for newline. */
      dbp = lb.buffer;
      if (isspace (dbp[0]))	/* not predicate header. */
	continue;
      else if (dbp[0] == '%')	/* comment. */
	continue;
      else if (dbp[0] == '/' && dbp[1] == '*')	/* comment. */
	skip_comment (&lb, fi, &lineno, &linecharno);
      else			/* found. */
	prolog_getit (dbp, lineno, linecharno);
    }
}

void
skip_comment (plb, fi, plineno, plinecharno)
     struct linebuffer *plb;
     FILE *fi;
     int *plineno;		/* result */
     long *plinecharno;		/* result */
{
  while (!substr ("*/", plb->buffer))
    {
      (*plineno)++;
      *plinecharno += readline (plb, fi) + 1;
    }				/* 1 for newline. */
}

/* Return TRUE if 'sub' exists somewhere in 's'. */

int
substr (sub, s)
     char *sub;
     char *s;
{
  while (*s && (s = index (s, *sub)))
    if (prestr (sub, s))
      return (TRUE);
    else
      s++;
  return (FALSE);
}

/* Return TRUE if 'pre' is prefix of string 's'. */

int
prestr (pre, s)
     char *pre;
     char *s;
{
  if (*pre == '\0')
    return (TRUE);
  else if (*pre == *s)
    return (prestr (pre + 1, s + 1));
  else
    return (FALSE);
}

/* Initialize a linebuffer for use */

void
initbuffer (linebuffer)
     struct linebuffer *linebuffer;
{
  linebuffer->size = 200;
  linebuffer->buffer = xnew (200, char);
}

/*
 * Read a line of text from `stream' into `linebuffer'.
 * Return the number of characters read from `stream',
 * which is the length of the line including the newline, if any.
 */
long
readline (linebuffer, stream)
     struct linebuffer *linebuffer;
     register FILE *stream;
{
  char *buffer = linebuffer->buffer;
  register char *p = linebuffer->buffer;
  register char *pend;
  int newline;			/* 1 if ended with newline, 0 if ended with EOF */

  pend = p + linebuffer->size;	/* Separate to avoind 386/IX compiler bug.  */

  while (1)
    {
      register int c = getc (stream);
      if (p == pend)
	{
	  linebuffer->size *= 2;
	  buffer = (char *) xrealloc (buffer, linebuffer->size);
	  p += buffer - linebuffer->buffer;
	  pend = buffer + linebuffer->size;
	  linebuffer->buffer = buffer;
	}
      if (c < 0 || c == '\n')
	{
	  *p = 0;
	  newline = (c == '\n' ? 1 : 0);
	  break;
	}
      *p++ = c;
    }

  return p - buffer + newline;
}

char *
savestr (cp)
     char *cp;
{
  return savenstr (cp, strlen (cp));
}

char *
savenstr (cp, len)
     char *cp;
     int len;
{
  register char *dp;

  dp = xnew (len + 1, char);
  (void) strncpy (dp, cp, len);
  dp[len] = '\0';
  return dp;
}

#ifdef notdef
/*
 * Return the ptr in sp at which the character c last
 * appears; NULL if not found
 *
 * Identical to v7 rindex, included for portability.
 */

char *
rindex (sp, c)
     register char *sp, c;
{
  register char *r;

  r = NULL;
  do
    {
      if (*sp == c)
	r = sp;
  } while (*sp++);
  return (r);
}

/*
 * Return the ptr in sp at which the character c first
 * appears; NULL if not found
 *
 * Identical to v7 index, included for portability.
 */

char *
index (sp, c)
     register char *sp, c;
{
  do
    {
      if (*sp == c)
	return (sp);
  } while (*sp++);
  return (NULL);
}

#endif /* notdef */

/* Print error message and exit.  */

/* VARARGS1 */
void
fatal (s1, s2)
     char *s1, *s2;
{
  error (s1, s2);
  exit (1);
}

/* Print error message.  `s1' is printf control string, `s2' is arg for it. */

/* VARARGS1 */
void
error (s1, s2)
     char *s1, *s2;
{
  fprintf (stderr, "%s: ", progname);
  fprintf (stderr, s1, s2);
  fprintf (stderr, "\n");
}

/* Return a newly-allocated string whose contents concatenate those of s1, s2, s3.  */

char *
concat (s1, s2, s3)
     char *s1, *s2, *s3;
{
  int len1 = strlen (s1), len2 = strlen (s2), len3 = strlen (s3);
  char *result = xnew (len1 + len2 + len3 + 1, char);

  (void) strcpy (result, s1);
  (void) strcpy (result + len1, s2);
  (void) strcpy (result + len1 + len2, s3);
  *(result + len1 + len2 + len3) = 0;

  return result;
}

/* Like malloc but get fatal error if memory is exhausted.  */

char *
xmalloc (size)
     int size;
{
  char *result = malloc (size);
  if (!result)
    fatal ("virtual memory exhausted", 0);
  return result;
}

char *
xrealloc (ptr, size)
     char *ptr;
     int size;
{
  char *result = realloc (ptr, size);
  if (!result)
    fatal ("virtual memory exhausted");
  return result;
}
