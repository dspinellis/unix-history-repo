/* Makeinfo -- convert texinfo format files into info files
   Copyright (C) 1987 Free Software Foundation, Inc.

This file is part of GNU Info.

GNU Info is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Info is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Info; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* **************************************************************** */
/*								    */
/*			Include File Declarations       	    */
/*								    */
/* **************************************************************** */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>
#include <pwd.h>
#include <errno.h>
#include "getopt.h"

#if defined (VMS)
#include <perror.h>
#endif

#if defined (SYSV) || defined (VMS)
#include <string.h>
#else
#include <strings.h>
#endif

#include <fcntl.h>
#include <sys/file.h>

#if defined (SYSV)
#define bcopy(source, dest, count) memcpy (dest, source, count)
#endif

#if defined (__GNUC__)
#define alloca __builtin_alloca
#else
#if defined (sparc)
#include <alloca.h>
#else
extern char *alloca ();
#endif
#endif

/* Forward declarations. */
char *xmalloc (), *xrealloc ();
extern int in_fixed_width_font;

/* Some systems don't declare this function in pwd.h. */
struct passwd *getpwnam ();


/* **************************************************************** */
/*								    */
/*			      Global Defines  			    */
/*								    */
/* **************************************************************** */

/* Error levels */
#define NO_ERROR 0
#define SYNTAX	 2
#define FATAL	 4

/* Boolean values. */
#define true  1
#define false 0
typedef int boolean;

/* How to allocate permanent storage for STRING. */
#define savestring(x) \
  ((char *)strcpy (xmalloc (1 + ((x) ? strlen (x) : 0)), (x) ? (x) : ""))

/* C's standard macros don't check to make sure that the characters being
   changed are within range.  So I have to check explicitly. */

/* GNU Library doesn't have toupper().  Until GNU gets this fixed, I will
   have to do it. */
#ifndef toupper
#define toupper(c) ((c) - 32)
#endif

#define coerce_to_upper(c) ((islower(c) ? toupper(c) : (c)))
#define coerce_to_lower(c) ((isupper(c) ? tolower(c) : (c)))

#define control_character_bit 0x40 /* %01000000, must be off. */
#define meta_character_bit 0x080/* %10000000, must be on.  */
#define CTL(c) ((c) & (~control_character_bit))
#define UNCTL(c) coerce_to_upper(((c)|control_character_bit))
#define META(c) ((c) | (meta_character_bit))
#define UNMETA(c) ((c) & (~meta_character_bit))

#define whitespace(c) (((c) == '\t') || ((c) == ' '))
#define sentence_ender(c) ((c) == '.' || (c) == '?' || (c) == '!')
#define cr_or_whitespace(c) (((c) == '\t') || ((c) == ' ') || ((c) == '\n'))
#define member(c, s) (index (s, c) != NULL)

#define COMMAND_PREFIX '@'

/* Stuff for splitting large files. */
#define SPLIT_SIZE_THRESHOLD 70000	/* What's good enough for Stallman... */
#define DEFAULT_SPLIT_SIZE 50000/* Is probably good enough for me. */
boolean splitting = true;	/* Always true for now. */

typedef int FUNCTION ();	/* So I can say FUNCTION *foo; */


/* **************************************************************** */
/*								    */
/*			    Global Variables			    */
/*								    */
/* **************************************************************** */

/* Global pointer to argv[0]. */
char *progname;

/* The current input file state. */
char *input_filename;
char *input_text;
int size_of_input_text;
int input_text_offset;
int line_number;

#define curchar() input_text[input_text_offset]

#define command_char(c) ((!whitespace(c)) && \
			  ((c) != '\n') && \
			  ((c) != '{'))
#define skip_whitespace() while (input_text_offset != size_of_input_text \
				 && whitespace(curchar()))\
  input_text_offset++

/* And writing to the output. */

/* The output file name. */
char *output_filename, *pretty_output_filename;

/* Current output stream. */
FILE *output_stream;

/* Position in the output file. */
int output_position;

/* Output paragraph buffer. */
char *output_paragraph;

/* Offset into OUTPUT_PARAGRAPH. */
int output_paragraph_offset;

/* The output paragraph "cursor" horizontal position. */
int output_column = 0;

/* non-zero means output_paragraph contains text. */
boolean paragraph_is_open = false;

#define INITIAL_PARAGRAPH_SPACE 5000
int paragraph_buffer_len = INITIAL_PARAGRAPH_SPACE;

/* Filling.. */
/* True indicates that filling will take place on long lines. */
boolean filling_enabled = true;

/* Non-zero means that words are not to be split, even in long lines.  This
   gets changed for cm_w (). */
int non_splitting_words = 0;

/* True indicates that filling a line also indents the new line. */
boolean indented_fill = false;

/* The column at which long lines are broken. */
int fill_column = 72;

/* The amount of indentation to apply at the start of each line. */
int current_indent = 0;

/* The amount of indentation to add at the starts of paragraphs.
   0 means don't change existing indentation at paragraph starts.
   > 0 is amount to indent new paragraphs by.
   < 0 means indent to column zero by removing indentation if necessary.

   This is normally zero, but some people prefer paragraph starts to be
   somewhat more indented than paragraph bodies.  A pretty value for
   this is 3. */
int paragraph_start_indent = 3;

/* Non-zero means that the use of paragraph_start_indent is inhibited.
   @example uses this to line up the left columns of the example text. */
int inhibit_paragraph_indentation = 0;

/* Indentation that is pending insertion.  We have this for hacking lines
   which look blank, but contain whitespace.  We want to treat those as
   blank lines. */
int pending_indent = 0;

/* The amount that indentation increases/decreases by. */
int default_indentation_increment = 5;

/* True indicates that indentation is temporarily turned off. */
boolean no_indent = true;

/* Command name in the process of being hacked. */
char *command;

/* The index in our internal command table of the currently
   executing command. */
int command_index;

/* A stack of file information records.  If a new file is read in with
   "@input", we remember the old input file state on this stack. */
typedef struct fstack
{
  struct fstack *next;
  char *filename;
  char *text;
  int size;
  int offset;
  int line_number;
} FSTACK;

FSTACK *filestack = (FSTACK *) NULL;

/* Stuff for nodes. */
/* The current nodes node name */
char *current_node;

/* The current nodes section level. */
int current_section = 0;

/* The filename of the current input file.  This is never freed. */
char *node_filename = (char *)NULL;

/* What we remember for each node. */
typedef struct tentry
{
  struct tentry *next_ent;
  char *node;		/* name of this node. */
  char *prev;		/* name of "Prev:" for this node. */
  char *next;		/* name of "Next:" for this node. */
  char *up;		/* name of "Up:" for this node.   */
  int position;		/* output file position of this node. */
  int line_no;		/* defining line in source file. */
  char *filename;	/* The file that this node was found in. */
  int touched;		/* non-zero means this node has been referenced. */
  int flags;		/* Room for growth.  Right now, contains 1 bit. */
} TAG_ENTRY;

/* If node-a has a "Next" for node-b, but node-b has no "Prev" for node-a,
   we turn on this flag bit in node-b's tag entry.  This means that when
   it is time to validate node-b, we don't report an additional error
   if there was no "Prev" field. */
#define PREV_ERROR 0x1
#define NEXT_ERROR 0x2
#define UP_ERROR   0x4
#define NO_WARN	   0x8

TAG_ENTRY *tag_table = (TAG_ENTRY *) NULL;

/* Menu reference, *note reference, and validation hacking. */

/* The various references that we know about. */
enum reftype
{
  menu_reference, followed_reference
};

/* A structure to remember references with.  A reference to a node is
   either an entry in a menu, or a cross-reference made with [px]ref. */
typedef struct node_ref
{
  struct node_ref *next;
  char *node;			/* Name of node referred to. */
  char *containing_node;	/* Name of node containing this reference. */
  int line_no;			/* Line number where the reference occurs. */
  int section;			/* Section level where the reference occurs. */
  char *filename;		/* Name of file where the reference occurs. */
  enum reftype type;		/* Type of reference, either menu or note. */
} NODE_REF;

/* The linked list of such structures. */
NODE_REF *node_references = (NODE_REF *) NULL;

/* Flag which tells us whether to examine menu lines or not. */
int in_menu = 0;

/* Flags controlling the operation of the program. */

/* Default is to notify users of bad choices. */
boolean print_warnings = true;

/* Default is to check node references. */
boolean validating = true;

/* Number of errors that we tolerate on a given fileset. */
int max_error_level = 100;

/* Maximum number of references to a single node before complaining. */
int reference_warning_limit = 1000;

/* Non-zero means print out information about what is going on when it
   is going on. */
int verbose_mode = 0;

/* The list of commands that we hack in texinfo.  Each one
   has an associated function.  When the command is encountered in the
   text, the associated function is called with START as the argument.
   If the function expects arguments in braces, it remembers itself on
   the stack.  When the corresponding close brace is encountered, the
   function is called with END as the argument. */

#define START 0
#define END 1

typedef struct brace_element
{
  struct brace_element *next;
  FUNCTION *proc;
  int pos, line;
} BRACE_ELEMENT;

BRACE_ELEMENT *brace_stack = (BRACE_ELEMENT *) NULL;

/* Forward declarations. */

int
insert_self (), cm_tex (), cm_asterisk (), cm_dots (), cm_bullet (),
cm_TeX (), cm_copyright (), cm_code (), cm_samp (), cm_file (), cm_kbd (),
cm_key (), cm_ctrl (), cm_var (), cm_dfn (), cm_emph (), cm_strong (),
cm_cite (), cm_italic (), cm_bold (), cm_roman (), cm_title (), cm_w (),
cm_refill ();

int
cm_chapter (), cm_unnumbered (), cm_appendix (),
cm_section (), cm_unnumberedsec (), cm_appendixsec (),
cm_subsection (), cm_unnumberedsubsec (), cm_appendixsubsec (),
cm_subsubsection (), cm_unnumberedsubsubsec (), cm_appendixsubsubsec (),
cm_heading (), cm_chapheading (), cm_subheading (), cm_subsubheading (),
cm_majorheading ();

/* All @defxxx commands map to cm_defun (). */
int
cm_defun ();

int
cm_node (), cm_menu (), cm_xref (),
cm_pxref (), cm_inforef (), cm_quotation (), cm_display (), cm_itemize (),
cm_enumerate (), cm_table (), cm_itemx (), cm_noindent (), cm_setfilename (),
cm_comment (), cm_ignore (), cm_br (), cm_sp (), cm_page (), cm_group (),
cm_need (), cm_center (), cm_include (), cm_bye (), cm_item (), cm_end (),
cm_infoinclude (), cm_ifinfo (), cm_iftex (), cm_titlepage (),
cm_titlespec (),cm_kindex (), cm_cindex (), cm_findex (), cm_pindex (),
cm_vindex (), cm_tindex (), cm_asis (), cm_synindex (), cm_settitle (),
cm_setchapternewpage (), cm_printindex (), cm_minus (), cm_footnote (),
cm_force_abbreviated_whitespace (), cm_force_sentence_end (), cm_example (),
cm_smallexample (), cm_lisp (), cm_format (), cm_exdent (), cm_defindex (),
cm_defcodeindex (), cm_sc (), cm_result (), cm_expansion (), cm_equiv (),
cm_print (), cm_error (), cm_point (), cm_smallbook ();

int do_nothing ();
int misplaced_brace (), cm_obsolete ();

typedef struct
{
  char *name;
  FUNCTION *proc;
  boolean argument_in_braces;
} COMMAND;

/* Stuff for defining commands on the fly. */
COMMAND **user_command_array = (COMMAND **) NULL;
int user_command_array_len = 0;

static COMMAND CommandTable[] = {
  {"!", cm_force_sentence_end, false},
  {"'", insert_self, false},
  {"*", cm_asterisk, false},
  {".", cm_force_sentence_end, false},
  {":", cm_force_abbreviated_whitespace, false},
  {"?", cm_force_sentence_end, false},
  {"@", insert_self, false},
  {" ", insert_self, false},
  {"\n", insert_self, false},
  {"TeX", cm_TeX, true},
  {"`", insert_self, false},
  {"appendix", cm_appendix, false},
  {"appendixsec", cm_appendixsec, false},
  {"appendixsubsec", cm_appendixsubsec, false},
  {"appendixsubsubsec", cm_appendixsubsubsec, false},
  {"asis", cm_asis, true},
  {"b", cm_bold, true},
  {"br", cm_br, false},
  {"bullet", cm_bullet, true},
  {"bye", cm_bye, false},
  {"c", cm_comment, false},
  {"center", cm_center, false},
  {"chapheading", cm_chapheading, false},
  {"chapter", cm_chapter, false},
  {"cindex", cm_cindex, false},
  {"cite", cm_cite, true},
  {"code", cm_code, true},
  {"comment", cm_comment, false},
  {"contents", do_nothing, false},
  {"copyright", cm_copyright, true},
  {"ctrl", cm_ctrl, true},
  {"defcodeindex", cm_defcodeindex, false},
  {"defindex", cm_defindex, false},
  {"dfn", cm_dfn, true},

/* The `def' commands. */
  {"defun", cm_defun, false},
  {"defunx", cm_defun, false},
  {"defvar", cm_defun, false},
  {"defvarx", cm_defun, false},
  {"defopt", cm_defun, false},
  {"defoptx", cm_defun, false},
  {"deffn", cm_defun, false},
  {"deffnx", cm_defun, false},
  {"defspec", cm_defun, false},
  {"defspecx", cm_defun, false},
  {"defmac", cm_defun, false},
  {"defmacx", cm_defun, false},
/* The end of the `def' commands. */

  {"display", cm_display, false},
  {"dots", cm_dots, true},
  {"emph", cm_emph, true},
  {"end", cm_end, false},
  {"enumerate", cm_enumerate, false},
  {"equiv", cm_equiv, true},
  {"error", cm_error, true},
  {"example", cm_example, false},
  {"exdent", cm_exdent, false},
  {"expansion", cm_expansion, true},
  {"file", cm_file, true},
  {"findex", cm_findex, false},
  {"format", cm_format, false},
  {"group", cm_group, false},
  {"heading", cm_heading, false},
  {"i", cm_italic, true},
  {"iappendix", cm_appendix, false},
  {"iappendixsec", cm_appendixsec, false},
  {"iappendixsubsec", cm_appendixsubsec, false},
  {"iappendixsubsubsec", cm_appendixsubsubsec, false},
  {"ichapter", cm_chapter, false},
  {"ifinfo", cm_ifinfo, false},
  {"iftex", cm_iftex, false},
  {"ignore", cm_ignore, false},
  {"include", cm_include, false},
  {"inforef", cm_inforef, true},
  {"input", cm_include, false},
  {"isection", cm_section, false},
  {"isubsection", cm_subsection, false},
  {"isubsubsection", cm_subsubsection, false},
  {"item", cm_item, false},
  {"itemize", cm_itemize, false},
  {"itemx", cm_itemx, false},
  {"iunnumbered", cm_unnumbered, false},
  {"iunnumberedsec", cm_unnumberedsec, false},
  {"iunnumberedsubsec", cm_unnumberedsubsec, false},
  {"iunnumberedsubsubsec", cm_unnumberedsubsubsec, false},
  {"kbd", cm_kbd, true},
  {"key", cm_key, true},
  {"kindex", cm_kindex, false},
  {"lisp", cm_lisp, false},
  {"majorheading", cm_majorheading, false},
  {"menu", cm_menu},
  {"minus", cm_minus, true},
  {"need", cm_need, false},
  {"node", cm_node, false},
  {"noindent", cm_noindent, false},
  {"page", do_nothing, false},
  {"pindex", cm_pindex, false},
  {"point", cm_point, true},
  {"print", cm_print, true},
  {"printindex", cm_printindex, false},
  {"pxref", cm_pxref, true},
  {"quotation", cm_quotation, false},
  {"r", cm_roman, true},
  {"ref", cm_xref, true},
  {"refill", cm_refill, false},
  {"result", cm_result, true},
  {"samp", cm_samp, true},
  {"sc", cm_sc, true},
  {"section", cm_section, false},
  {"setchapternewpage", cm_setchapternewpage, false},
  {"setfilename", cm_setfilename, false},
  {"settitle", cm_settitle, false},
  {"smallexample", cm_smallexample, false},
  {"smallbook", cm_smallbook, false},
  {"sp", cm_sp, false},
  {"strong", cm_strong, true},
  {"subheading", cm_subheading, false},
  {"subsection", cm_subsection, false},
  {"subsubheading", cm_subsubheading, false},
  {"subsubsection", cm_subsubsection, false},
  {"summarycontents", do_nothing, false},
  {"syncodeindex", cm_synindex, false},
  {"synindex", cm_synindex, false},
  {"t", cm_title, true},
  {"table", cm_table, false},
  {"tex", cm_tex, false},
  {"tindex", cm_tindex, false},
  {"titlepage", cm_titlepage, false},
  {"titlespec", cm_titlespec, false},
  {"unnumbered", cm_unnumbered, false},
  {"unnumberedsec", cm_unnumberedsec, false},
  {"unnumberedsubsec", cm_unnumberedsubsec, false},
  {"unnumberedsubsubsec", cm_unnumberedsubsubsec, false},
  {"var", cm_var, true},
  {"vindex", cm_vindex, false},
  {"w", cm_w, true},
  {"xref", cm_xref, true},
  {"{", insert_self, false},
  {"}", insert_self, false},

  /* Now @include does what this was supposed to. */
  {"infoinclude", cm_infoinclude, false},
  {"footnote", cm_footnote, false}, /* self-arg eater */

  {(char *) NULL, (FUNCTION *) NULL}, false};

/* Non-zero means we are running inside of Emacs. */
int in_emacs = 0;

#ifndef MAKEINFO_MAJOR
#define MAKEINFO_MAJOR 1
#endif

#ifndef MAKEINFO_MINOR
#define MAKEINFO_MINOR 0
#endif

int major_version = MAKEINFO_MAJOR;
int minor_version = MAKEINFO_MINOR;

struct option long_options[] =
{
  { "no-validate", 0, &validating, false },	/* formerly -nv */
  { "no-warn", 0, &print_warnings, false },	/* formerly -nw */
  { "no-split", 0, &splitting, false },		/* formerly -ns */
  { "verbose", 0, &verbose_mode, 1 },		/* formerly -verbose */
  { "fill-column", 1, 0, 'f' },			/* formerly -fc */
  { "paragraph-indent", 1, 0, 'p' },		/* formerly -pi */
  { "error-limit", 1, 0, 'e' },			/* formerly -el */
  { "reference-limit", 1, 0, 'r' },		/* formerly -rl */
  { "footnote-style", 1, 0, 's' },		/* formerly -ft */
  { "version", 0, 0, 'V' },
  {NULL, 0, NULL, 0}
};
  
/* **************************************************************** */
/*								    */
/*			Main ()  Start of code  		    */
/*					        		    */
/* **************************************************************** */

/* For each file mentioned in the command line, process it, turning
   texinfo commands into wonderfully formatted output text. */
main (argc, argv)
     int argc;
     char **argv;
{
  char *t = (char *) getenv ("EMACS");
  int c;
  int ind;

  progname = argv[0];

  if (t && strcmp (t, "t") == 0)
    in_emacs++;

  /* Parse argument flags from the input line. */
  while ((c = getopt_long (argc, argv, "", long_options, &ind)) != EOF)
    {
      if (c == 0 && long_options[ind].flag == 0)
	c = long_options[ind].val;
      switch (c)
	{
	case 'f':
	  /* user specified fill_column? */
	  if (sscanf (optarg, "%d", &fill_column) != 1)
	    usage ();
	  break;

	case 'p':
	  /* User specified paragraph indent (paragraph_start_index)? */
	  if (sscanf (optarg, "%d", &paragraph_start_indent) != 1)
	    usage ();
	  break;

	case 'e':
	  /* User specified error level? */
	  if (sscanf (optarg, "%d", &max_error_level) != 1)
	    usage ();
	  break;

	case 'r':
	  /* User specified reference warning limit? */
	  if (sscanf (optarg, "%d", &reference_warning_limit) != 1)
	    usage ();
	  break;

	case 's':
	  /* User specified footnote style? */
	  set_footnote_style (optarg);
	  break;

	case 'V':		/* Use requested version info? */
	  fprintf (stderr, "Makeinfo verison %d.%d.\n",
		   major_version, minor_version);
	  exit (NO_ERROR);
	  break;

	case '?':
	  usage ();
	}
    }

  if (optind == argc)
    usage ();

  /* Remaining arguments are file names of texinfo files.
     Convert them, one by one. */
  while (optind != argc)
    convert (argv[optind++]);

  exit (NO_ERROR);
}


/* **************************************************************** */
/*								    */
/*			Generic Utilities			    */
/*								    */
/* **************************************************************** */

/* Just like malloc, but kills the program in case of fatal error. */
char *
xmalloc (nbytes)
     int nbytes;
{
  char *temp = (char *) malloc (nbytes);
  if (temp == (char *) NULL)
    {
      error ("Virtual memory exhausted! Needed %d bytes.", nbytes);
      exit (FATAL);
    }
  return (temp);
}

/* Like realloc (), but barfs if there isn't enough memory. */
char *
xrealloc (pointer, nbytes)
     char *pointer;
     int nbytes;
{
  pointer = (char *) realloc (pointer, nbytes);
  if (!pointer)
    {
      error ("Virtual memory exhausted in realloc ().");
      abort ();
    }
  return (pointer);
}

/* Tell the user how to use this program. */
usage ()
{
  fprintf (stderr, "Usage: %s [options] texinfo-file...\n\
\n\
This program accepts as input files of texinfo commands and text\n\
and outputs a file suitable for reading with GNU Info.\n\
\n\
The options are:\n\
`+no-validate' to suppress node cross reference validation.\n\
`+no-warn' to suppress warning messages (errors are still output).\n\
`+no-split' to suppress the splitting of large files.\n\
`+verbose' to print information about what is being done.\n\
`+version' to print the version number of Makeinfo.\n\
`+paragraph-indent NUM' to set the paragraph indent to NUM (default %d).\n\
`+fill-column NUM' to set the filling column to NUM (default %d).\n\
`+error-limit NUM' to set the error limit to NUM (default %d).\n\
`+reference-limit NUM' to set the reference warning limit to NUM (default %d).\n\
`+footnote-style STYLE' to set the footnote style to STYLE.  STYLE should\n\
  either be `MN' for `make node', or `BN' for `bottom node'.\n\n",
	   progname, paragraph_start_indent,
	   fill_column, max_error_level, reference_warning_limit);
  exit (FATAL);
}

/* **************************************************************** */
/*								    */
/*			Manipulating Lists      		    */
/*					        		    */
/* **************************************************************** */

typedef struct generic_list
{
  struct generic_list *next;
}            GENERIC_LIST;

/* Reverse the chain of structures in LIST.  Output the new head
   of the chain.  You should always assign the output value of this
   function to something, or you will lose the chain. */
GENERIC_LIST *
reverse_list (list)
     register GENERIC_LIST *list;
{
  register GENERIC_LIST *next;
  register GENERIC_LIST *prev = (GENERIC_LIST *) NULL;

  while (list)
    {
      next = list->next;
      list->next = prev;
      prev = list;
      list = next;
    }
  return (prev);
}


/* **************************************************************** */
/*								    */
/*			Pushing and Popping Files       	    */
/*								    */
/* **************************************************************** */

/* Find and load the file named FILENAME.  Return a pointer to
   the loaded file, or NULL if it can't be loaded. */
char *
find_and_load (filename)
     char *filename;
{
  struct stat fileinfo;
  int file, n, i, count = 0;
  char *result = (char *) NULL;

  if (stat (filename, &fileinfo) != 0)
    goto error_exit;

  file = open (filename, O_RDONLY);
  if (file < 0)
    goto error_exit;

  /* Load the file. */
  result = xmalloc (fileinfo.st_size);

  /* VMS stat lies about the st_size value.  The actual number of
     readable bytes is always less than this value.  The arcane
     mysteries of VMS/RMS are too much to probe, so this hack
    suffices to make things work. */
#if defined (VMS)
  while ((n = read (file, result+count, fileinfo.st_size)) > 0)
    count += n;
  if (n == -1)
#else
    count = fileinfo.st_size;
    if (read (file, result, fileinfo.st_size) != fileinfo.st_size)
#endif
  error_exit:
    {
      if (result)
	free (result);
      if (file != -1)
	close (file);
      return ((char *) NULL);
    }
  close (file);

  /* Set the globals to the new file. */
  input_text = result;
  size_of_input_text = fileinfo.st_size;
  input_filename = savestring (filename);
  node_filename = savestring (filename);
  input_text_offset = 0;
  line_number = 1;
  return (result);
}

/* Save the state of the current input file. */
pushfile ()
{
  FSTACK *newstack = (FSTACK *) xmalloc (sizeof (FSTACK));
  newstack->filename = input_filename;
  newstack->text = input_text;
  newstack->size = size_of_input_text;
  newstack->offset = input_text_offset;
  newstack->line_number = line_number;
  newstack->next = filestack;

  filestack = newstack;
  push_node_filename ();
}

/* Make the current file globals be what is on top of the file stack. */
popfile ()
{
  extern int executing_string;
  FSTACK *temp = filestack;

  if (!filestack)
    abort ();			/* My fault.  I wonder what I did? */

  /* Make sure that commands with braces have been satisfied. */
  if (!executing_string)
    discard_braces ();

  /* Get the top of the stack into the globals. */
  input_filename = filestack->filename;
  input_text = filestack->text;
  size_of_input_text = filestack->size;
  input_text_offset = filestack->offset;
  line_number = filestack->line_number;

  /* Pop the stack. */
  filestack = filestack->next;
  free (temp);
  pop_node_filename ();
}

/* Flush all open files on the file stack. */
flush_file_stack ()
{
  while (filestack)
    {
      free (input_filename);
      free (input_text);
      popfile ();
    }
}

int node_filename_stack_index = 0;
int node_filename_stack_size = 0;
char **node_filename_stack = (char **)NULL;

push_node_filename ()
{
  if (node_filename_stack_index + 1 > node_filename_stack_size)
    {
      if (!node_filename_stack)
	node_filename_stack =
	  (char **)xmalloc ((node_filename_stack_size += 10)
			    * sizeof (char *));
      else
	node_filename_stack =
	  (char **)xrealloc (node_filename_stack,
			     (node_filename_stack_size + 10)
			     * sizeof (char *));
    }

  node_filename_stack[node_filename_stack_index] = node_filename;
  node_filename_stack_index++;
}

pop_node_filename ()
{
  node_filename = node_filename_stack[--node_filename_stack_index];
}

/* Return just the simple part of the filename; i.e. the
   filename without the path information, or extensions.
   This conses up a new string. */
char *
filename_part (filename)
     char *filename;
{
  register int i = strlen (filename) - 1;

  while (i && filename[i] != '/')
    i--;
  if (filename[i] == '/')
    i++;

#ifdef REMOVE_OUTPUT_EXTENSIONS
  result = savestring (&filename[i]);

  /* See if there is an extension to remove.  If so, remove it. */
  if (rindex (result, '.'))
    *(rindex (result, '.')) = '\0';
  return (result);
#else
  return (savestring (&filename[i]));
#endif /* REMOVE_OUTPUT_EXTENSIONS */
}

/* Return the pathname part of filename.  This can be NULL. */
char *
pathname_part (filename)
     char *filename;
{
  char *expand_filename ();
  char *result = (char *) NULL;
  register int i;

  filename = expand_filename (filename, "");

  i = strlen (filename) - 1;

  while (i && filename[i] != '/')
    i--;
  if (filename[i] == '/')
    i++;

  if (i)
    {
      result = xmalloc (1 + i);
      strncpy (result, filename, i);
      result[i] = '\0';
    }
  free (filename);
  return (result);
}

/* Return the expansion of FILENAME. */
char *
expand_filename (filename, input_name)
     char *filename, *input_name;
{
  char *full_pathname ();
  filename = full_pathname (filename);

  if (filename[0] == '.')
    return (filename);

  if (filename[0] != '/' && input_name[0] == '/')
    {
      /* Make it so that relative names work. */
      char *result = xmalloc (1 + strlen (input_name)
			      + strlen (filename));
      int i = strlen (input_name) - 1;

      strcpy (result, input_name);
      while (result[i] != '/' && i)
	i--;
      if (result[i] == '/')
	i++;
      strcpy (&result[i], filename);
      free (filename);
      return (result);
    }
  return (filename);
}

/* Return the full path to FILENAME. */
char *
full_pathname (filename)
     char *filename;
{
  int initial_character;

  if (filename && (initial_character = *filename))
    {
      if (initial_character == '/')
	return (savestring (filename));
      if (initial_character != '~')
	{
	  return (savestring (filename));
	}
      else
	{
	  if (filename[1] == '/')
	    {
	      /* Return the concatenation of HOME and the rest of the string. */
	      char *temp_home = (char *) getenv ("HOME");
	      char *temp_name = xmalloc (strlen (&filename[2])
					 + 1
					 + temp_home ? strlen (temp_home)
					 : 0);
	      if (temp_home)
		strcpy (temp_name, temp_home);
	      strcat (temp_name, &filename[2]);
	      return (temp_name);
	    }
	  else
	    {
	      struct passwd *user_entry;
	      int i, c;
	      char *username = xmalloc (257);
	      char *temp_name;

	      for (i = 1; c = filename[i]; i++)
		{
		  if (c == '/')
		    break;
		  else
		    username[i - 1] = c;
		}
	      if (c)
		username[i - 1] = '\0';

	      user_entry = getpwnam (username);

	      if (!user_entry)
		return (savestring (filename));

	      temp_name = xmalloc (1 + strlen (user_entry->pw_dir)
				   + strlen (&filename[i]));
	      strcpy (temp_name, user_entry->pw_dir);
	      strcat (temp_name, &filename[i]);
	      return (temp_name);
	    }
	}
    }
  else
    {
      return (savestring (filename));
    }
}

/* **************************************************************** */
/*								    */
/*			Error Handling				    */
/*								    */
/* **************************************************************** */

/* Number of errors encountered. */
int errors_printed = 0;

/* Print the last error gotten from the file system. */
fs_error (filename)
     char *filename;
{
  perror (filename);
  return ((int) false);
}

/* Print an error message, and return false. */
error (format, arg1, arg2, arg3, arg4, arg5)
     char *format;
{
  remember_error ();
  fprintf (stderr, format, arg1, arg2, arg3, arg4, arg5);
  fprintf (stderr, "\n");
  return ((int) false);
}

/* Just like error (), but print the line number as well. */
line_error (format, arg1, arg2, arg3, arg4, arg5)
     char *format;
{
  remember_error ();
  fprintf (stderr, "%s:%d: ", input_filename, line_number);
  fprintf (stderr, format, arg1, arg2, arg3, arg4, arg5);
  fprintf (stderr, ".\n");
  return ((int) false);
}

warning (format, arg1, arg2, arg3, arg4, arg5)
     char *format;
{
  if (print_warnings)
    {
      fprintf (stderr, "%s:%d: Warning: ", input_filename, line_number);
      fprintf (stderr, format, arg1, arg2, arg3, arg4, arg5);
      fprintf (stderr, ".\n");
    }
  return ((int) false);
}

/* Remember that an error has been printed.  If this is the first
   error printed, then tell them which program is printing them.
   If more than max_error_level have been printed, then exit the
   program. */
remember_error ()
{
  errors_printed++;
  if (max_error_level && (errors_printed > max_error_level))
    {
      fprintf (stderr, "Too many errors!  Gave up.");
      flush_file_stack ();
      cm_bye ();
    }
}


/* **************************************************************** */
/*								    */
/*			Hacking Tokens and Strings		    */
/*								    */
/* **************************************************************** */

/* Return the next token as a string pointer.  We cons the
   string. */
char *
read_token ()
{
  int i, character;
  char *result;

  /* Hack special case.  If the first character to be read is
     self-delimiting, then that is the command itself. */

  character = curchar ();
  if (self_delimiting (character))
    {
      input_text_offset++;
      result = savestring (" ");
      *result = character;
      return (result);
    }

  for (i = 0; ((input_text_offset != size_of_input_text)
	       && (character = curchar ())
	       && command_char (character));
       i++, input_text_offset++);
  result = xmalloc (i + 1);
  strncpy (result, &input_text[input_text_offset - i], i);
  result[i] = '\0';
  return (result);
}

/* Return TRUE if CHARACTER is self-delimiting. */
boolean
self_delimiting (character)
     int character;
{
  return (member (character, "{}:.@*'`,!?; \n"));
}

/* Clear whitespace from the front and end of string. */
canon_white (string)
     char *string;
{
  int len = strlen (string);
  int x;

  if (!len)
    return;

  for (x = 0; x < len; x++)
    {
      if (!whitespace (string[x]))
	{
	  strcpy (string, string + x);
	  break;
	}
    }
  len = strlen (string);
  if (len)
    len--;
  while (len > -1 && cr_or_whitespace (string[len]))
    len--;
  string[len + 1] = '\0';
}

/* Bash STRING, replacing all whitespace with just one space. */
fix_whitespace (string)
     char *string;
{
  char *temp = xmalloc (strlen (string) + 1);
  int string_index = 0;
  int temp_index = 0;
  int c;

  canon_white (string);

  while (string[string_index])
    {
      c = temp[temp_index++] = string[string_index++];

      if (c == ' ' || c == '\n' || c == '\t')
	{
	  temp[temp_index - 1] = ' ';
	  while ((c = string[string_index]) && (c == ' ' ||
						c == '\t' ||
						c == '\n'))
	    string_index++;
	}
    }
  temp[temp_index] = '\0';
  strcpy (string, temp);
  free (temp);
}

/* Discard text until the desired string is found.  The string is
   included in the discarded text. */
discard_until (string)
     char *string;
{
  int temp = search_forward (string, input_text_offset);

  int tt = (temp < 0) ? size_of_input_text : temp + strlen (string);
  int from = input_text_offset;

  /* Find out what line we are on. */
  while (from != tt)
    if (input_text[from++] == '\n')
      line_number++;

  if (temp < 0)
    {
      input_text_offset = size_of_input_text - strlen (string);

      if (strcmp (string, "\n") != 0)
	{
	  line_error ("Expected `%s'", string);
	  return;
	}
    }
  else
    input_text_offset = temp;

  input_text_offset += strlen (string);
}

/* Read characters from the file until we are at MATCH.
   Place the characters read into STRING.
   On exit input_text_offset is after the match string.
   Return the length of STRING. */
get_until (match, string)
     char *match, **string;
{
  int len;
  int current_point = input_text_offset;
  int x = current_point;
  int new_point = search_forward (match, input_text_offset);

  if (new_point < 0)
    new_point = size_of_input_text;
  len = new_point - current_point;

  /* Keep track of which line number we are at. */
  while (x != new_point)
    if (input_text[x++] == '\n')
      line_number++;

  *string = xmalloc (len + 1);

  strncpy (*string, &input_text[current_point], len);
  (*string)[len] = '\0';

  /* Now leave input_text_offset in a consistent state. */
  input_text_offset = new_point + (strlen (match) - 1);
  if (input_text_offset > size_of_input_text)
    input_text_offset = size_of_input_text;
}

/* Read characters from the file until we are at MATCH or end of line.
   Place the characters read into STRING.  */
get_until_in_line (match, string)
     char *match, **string;
{
  int real_bottom = size_of_input_text;
  int temp = search_forward ("\n", input_text_offset);
  if (temp < 0)
    temp = size_of_input_text;
  size_of_input_text = temp;
  get_until (match, string);
  size_of_input_text = real_bottom;
}

get_rest_of_line (string)
     char **string;
{
  get_until ("\n", string);
  canon_white (*string);
  if (curchar () == '\n')
    {				/* as opposed to the end of the file... */
      line_number++;
      input_text_offset++;
    }
}

/* Read characters from the file until we are at MATCH or closing brace.
   Place the characters read into STRING.  */
get_until_in_braces (match, string)
     char *match, **string;
{
  int i, brace = 0;
  int match_len = strlen (match);
  char *temp;

  for (i = input_text_offset; i < size_of_input_text; i++)
    {
      if (input_text[i] == '{')
	brace++;
      if (input_text[i] == '}')
	brace--;
      if (input_text[i] == '\n')
	line_number++;
      if (brace < 0 ||
	  (brace == 0 && strncmp (input_text + i, match, match_len) == 0))
	break;
    }
  match_len = i - input_text_offset;
  temp = xmalloc (2 + match_len);
  strncpy (temp, input_text + input_text_offset, match_len);
  temp[match_len] = '\0';
  input_text_offset = i;
  *string = temp;
}

/* **************************************************************** */
/*								    */
/*			Converting the File     		    */
/*								    */
/* **************************************************************** */

/* Convert the file named by NAME.  The output is saved on the file
   named as the argument to the @setfilename command. */
convert (name)
     char *name;
{
  char *real_output_filename, *expand_filename (), *filename_part ();
  init_tag_table ();
  init_indices ();
  init_internals ();
  init_paragraph ();

  if (!find_and_load (name))
    {
      /* For some reason, the file couldn't be loaded.  Print a message
	 to that affect, and split. */
      fs_error (name);
      return;
    }
  else
    input_filename = savestring (name);

  /* Search this file looking for the special string which starts conversion.
     Once found, we may truly begin. */

  input_text_offset = search_forward ("@setfilename", 0);
  if (input_text_offset < 0)
    {
      error ("No `@setfilename' found in `%s'", name);
      goto finished;
    }
  else
    input_text_offset += strlen ("@setfilename");

  get_until ("\n", &output_filename);	/* no braces expected. */
  canon_white (output_filename);

  printf ("Making info file `%s' from `%s'.\n", output_filename, name);
  real_output_filename = expand_filename (output_filename, name);
  output_stream = fopen (real_output_filename, "w");
  if (output_stream == NULL)
    {
      fs_error (real_output_filename);
      goto finished;
    }

  /* Make the displayable filename from output_filename.  Only the root
     portion of the filename need be displayed. */
  pretty_output_filename = filename_part (output_filename);

  /* For this file only, count the number of newlines from the top of
     the file to here.  This way, we keep track of line numbers for
     error reporting.  Line_number starts at 1, since the user isn't
     zero-based. */
  {
    int temp = 0;
    line_number = 1;
    while (temp != input_text_offset)
      if (input_text[temp++] == '\n')
	line_number++;
  }

  add_word_args ("Info file %s, produced by Makeinfo, -*- Text -*-\n\
from input file %s.\n", output_filename, input_filename);
  close_paragraph ();

  reader_loop ();

finished:
  close_paragraph ();
  flush_file_stack ();
  if (output_stream != NULL)
    {
      output_pending_notes ();
      free_pending_notes ();
      if (tag_table != NULL)
	{
	  tag_table = (TAG_ENTRY *) reverse_list (tag_table);
	  write_tag_table ();
	}

      fclose (output_stream);

      /* If validating, then validate the entire file right now. */
      if (validating)
	validate_file (real_output_filename, tag_table);

      /* This used to test  && !errors_printed.
	 But some files might have legit warnings.  So split anyway.  */
      if (splitting)
	split_file (real_output_filename, 0);
    }
}

free_and_clear (pointer)
     char **pointer;
{
  if ((*pointer) != (char *) NULL)
    {
      free (*pointer);
      *pointer = (char *) NULL;
    }
}

 /* Initialize some state. */
init_internals ()
{
  free_and_clear (&current_node);
  free_and_clear (&output_filename);
  free_and_clear (&command);
  free_and_clear (&input_filename);
  free_node_references ();
  init_insertion_stack ();
  init_brace_stack ();
  command_index = 0;
  in_menu = 0;
}

init_paragraph ()
{
  free_and_clear (&output_paragraph);
  output_paragraph = xmalloc (paragraph_buffer_len);
  output_position = 0;
  output_paragraph[0] = '\0';
  output_paragraph_offset = 0;
  output_column = 0;
  paragraph_is_open = false;
  current_indent = 0;
}

/* Okay, we are ready to start the conversion.  Call the reader on
   some text, and fill the text as it is output.  Handle commands by
   remembering things like open braces and the current file position on a
   stack, and when the corresponding close brace is found, you can call
   the function with the proper arguments. */
reader_loop ()
{
  int character;
  boolean done = false;
  int dash_count = 0;

  while (!done)
    {
      if (input_text_offset >= size_of_input_text)
	{
	  if (filestack)
	    {
	      free (input_filename);
	      free (input_text);
	      popfile ();
	    }
	  else
	    break;
	}
      character = curchar ();

      if (!in_fixed_width_font &&
	  (character == '\'' || character == '`') &&
	  input_text[input_text_offset + 1] == character)
	{
	  input_text_offset++;
	  character = '"';
	}

      if (character == '-')
	{
	  dash_count++;
	  if (dash_count == 3 && !in_fixed_width_font)
	    {
	      input_text_offset++;
	      continue;
	    }
	}
      else
	{
	  dash_count = 0;
	}

      if (character == '\n')
	{
	  line_number++;
	  if (in_menu && input_text_offset + 1 < size_of_input_text)
	    {
	      glean_node_from_menu ();
	    }

	  /* If the following line is all whitespace, advance to the carriage
	     return on it. */
	  {
	    register int i = input_text_offset + 1;
	    
	    while (i < size_of_input_text && whitespace (input_text[i]))
	      i++;
	    
	        if (i == size_of_input_text || input_text[i] == '\n')
		  input_text_offset = i - 1;
	  }
	}
      
      switch (character)
	{
	case COMMAND_PREFIX:
	  read_command ();
	  if (strcmp (command, "bye") == 0)
	    {
	      done = true;
	      continue;
	    }
	  break;

	case '{':

	  /* Special case.  I'm not supposed to see this character by itself.
	     If I do, it means there is a syntax error in the input text.
	     Report the error here, but remember this brace on the stack so
	     you can ignore its partner. */

	  line_error ("Misplaced `{'");
	  remember_brace (misplaced_brace);

	  /* Don't advance input_text_offset since this happens in
	     remember_brace ().
	     input_text_offset++;
           */
	  break;

	case '}':
	  pop_and_call_brace ();
	  input_text_offset++;
	  break;

	default:
	  add_char (character);
	  input_text_offset++;
	}
    }
}

/* Find the command corresponding to STRING.  If the command
   is found, return a pointer to the data structure.  Otherwise
   return (-1). */
COMMAND *
get_command_entry (string)
     char *string;
{
  register int i;

  for (i = 0; CommandTable[i].name; i++)
    if (strcmp (CommandTable[i].name, string) == 0)
      return (&CommandTable[i]);

  /* This command is not in our predefined command table.  Perhaps
     it is a user defined command. */
  for (i = 0; i < user_command_array_len; i++)
    if (user_command_array[i] &&
	(strcmp (user_command_array[i]->name, string) == 0))
      return (user_command_array[i]);

  /* Nope, we never heard of this command. */
  return ((COMMAND *) - 1);
}

/* input_text_offset is right at the command prefix character.
   Read the next token to determine what to do. */
read_command ()
{
  COMMAND *entry;
  input_text_offset++;
  free_and_clear (&command);
  command = read_token ();

  entry = get_command_entry (command);
  if ((int) entry < 0)
    {
      line_error ("Unknown info command `%s'", command);
      return;
    }

  if (entry->argument_in_braces)
    remember_brace (entry->proc);

  (*(entry->proc)) (START);
}

/* Return the string which invokes PROC; a pointer to a function. */
char *
find_proc_name (proc)
     FUNCTION *proc;
{
  register int i;

  for (i = 0; CommandTable[i].name; i++)
    if (proc == CommandTable[i].proc)
      return (CommandTable[i].name);
  return ("NO_NAME!");
}

init_brace_stack ()
{
  brace_stack = (BRACE_ELEMENT *) NULL;
}

remember_brace (proc)
     FUNCTION *proc;
{
  if (curchar () != '{')
    line_error ("@%s expected `{..}'", command);
  else
    input_text_offset++;
  remember_brace_1 (proc, output_paragraph_offset);
}

/* Remember the current output position here.  Save PROC
   along with it so you can call it later. */
remember_brace_1 (proc, position)
     FUNCTION *proc;
     int position;
{
  BRACE_ELEMENT *new = (BRACE_ELEMENT *) xmalloc (sizeof (BRACE_ELEMENT));
  new->next = brace_stack;
  new->proc = proc;
  new->pos = position;
  new->line = line_number;
  brace_stack = new;
}

/* Pop the top of the brace stack, and call the associated function
   with the args END and POS. */
pop_and_call_brace ()
{
  BRACE_ELEMENT *temp;
  FUNCTION *proc;
  int pos;

  if (brace_stack == (BRACE_ELEMENT *) NULL)
    return (line_error ("Unmatched close bracket"));

  pos = brace_stack->pos;
  proc = brace_stack->proc;
  temp = brace_stack->next;
  free (brace_stack);
  brace_stack = temp;

  return ((*proc) (END, pos, output_paragraph_offset));
}

/* You call discard_braces () when you shouldn't have any braces on the stack.
   I used to think that this happens for commands that don't take arguments
   in braces, but that was wrong because of things like @code{foo @@}.  So now
   I only detect it at the beginning of nodes. */
discard_braces ()
{
  int temp_line_number = line_number;
  char *proc_name;

  if (!brace_stack)
    return;

  while (brace_stack)
    {
      line_number = brace_stack->line;
      proc_name = find_proc_name (brace_stack->proc);
      line_error ("@%s missing close brace", proc_name);
      line_number = temp_line_number;
      pop_and_call_brace ();
    }
}

get_char_len (character)
     int character;
{
  /* Return the printed length of the character. */
  int len;

  switch (character)
    {
    case '\t':
      len = (output_column + 8) & 0xf7;
      if (len > fill_column)
	len = fill_column - output_column;
      else
	len = len - output_column;
      break;

    case '\n':
      len = fill_column - output_column;
      break;

    default:
      if (character < ' ')
	len = 2;
      else
	len = 1;
    }
  return (len);
}

add_word_args (format, arg1, arg2, arg3, arg4, arg5)
     char *format;
{
  char buffer[1000];
  sprintf (buffer, format, arg1, arg2, arg3, arg4, arg5);
  add_word (buffer);
}

/* Add STRING to output_paragraph. */
add_word (string)
     char *string;
{
  while (*string)
    add_char (*string++);
}

boolean last_char_was_newline = true;
int last_inserted_character = 0;

/* Add the character to the current paragraph.  If filling_enabled is
   true, then do filling as well. */
add_char (character)
     int character;
{
  extern int must_start_paragraph;

  /* If we are adding a character now, then we don't have to
     ignore close_paragraph () calls any more. */
  if (must_start_paragraph)
    {
      must_start_paragraph = 0;
      if (current_indent > output_column)
	{
	  indent (current_indent - output_column);
	  output_column = current_indent;
	}
    }

  if (non_splitting_words && member (character, " \t\n"))
    character = ' ' | 0x80;

  switch (character)
    {

    case '\n':
      if (!filling_enabled)
	{
	  insert ('\n');

	  /* Should I be flushing output here? * /
          flush_output (); */

	  output_column = 0;
	  if (!no_indent)
	    indent (output_column = current_indent);
	  break;
	}
      else
	{
	  if (sentence_ender (last_inserted_character))
	    {
	      insert (' ');
	      output_column++;
	      last_inserted_character = character;
	    }
	}

      if (last_char_was_newline)
	{
	  close_paragraph ();
	  pending_indent = 0;
	}
      else
	{
	  last_char_was_newline = true;
	  insert (' ');
	  output_column++;
	}
      break;

    default:
      {
	int len = get_char_len (character);
	if ((character == ' ') && (last_char_was_newline))
	  {
	    if (!paragraph_is_open)
	      {
		pending_indent++;
		return;
	      }
	  }
	if (!paragraph_is_open)
	  {
	    start_paragraph ();

	    /* If the paragraph is supposed to be indented a certain way,
	       then discard all of the pending whitespace.  Otherwise, we
	       let the whitespace stay. */
	    if (!paragraph_start_indent)
	      indent (pending_indent);
	    pending_indent = 0;
	  }
	if ((output_column += len) >= fill_column)
	  {
	    if (filling_enabled)
	      {
		int temp = output_paragraph_offset - 1;
		while (temp > 0 && output_paragraph[--temp] != '\n')
		  {
		    if (output_paragraph[temp] == ' ')
		      {
			output_paragraph[temp++] = '\n';

			/* We have correctly broken the line where we want
			   to.  What we don't want is spaces following where
			   we have decided to break the line.  We get rid of
			   them. */
			{
			  int t1 = temp;
			  while (t1 < output_paragraph_offset
				 && whitespace (output_paragraph[t1]))
			    t1++;

			  if (t1 != temp)
			    {
			      strncpy (&output_paragraph[temp],
				       &output_paragraph[t1],
				       (output_paragraph_offset - t1));
			      output_paragraph_offset -= (t1 - temp);
			    }
			}

			/* Filled, but now indent if that is right. */
			if (indented_fill && current_indent)
			  {
			    int buffer_len = ((output_paragraph_offset - temp)
					      + current_indent);
			    char *temp_buffer = xmalloc (buffer_len);
			    int indentation = 0;

			    /* We have to shift any markers that are in
			       front of the wrap point. */
			    {
			      register BRACE_ELEMENT *stack = brace_stack;

			      while (stack)
				{
				  if (stack->pos > temp)
				    stack->pos += current_indent;
				  stack = stack->next;
				}
			    }

			    while (indentation != current_indent)
			      temp_buffer[indentation++] = ' ';

			    strncpy (&temp_buffer[current_indent],
				     &output_paragraph[temp],
				     buffer_len - current_indent);

			    if (output_paragraph_offset + buffer_len
				>= paragraph_buffer_len)
			      {
				char *tt =
				(char *) xrealloc (output_paragraph,
				      (paragraph_buffer_len += buffer_len));
				output_paragraph = tt;
			      }
			    strncpy (&output_paragraph[temp], temp_buffer, buffer_len);
			    output_paragraph_offset += current_indent;
			    free (temp_buffer);
			  }
			output_column = 0;
			while (temp != output_paragraph_offset)
			  output_column += get_char_len (output_paragraph[temp++]);
			output_column += len;
			break;
		      }
		  }
	      }
	  }
	insert (character);
	last_char_was_newline = false;
	last_inserted_character = character;
      }
    }
}

/* Insert CHARACTER into OUTPUT_PARAGRAPH. */
insert (character)
     int character;
{
  output_paragraph[output_paragraph_offset++] = character;
  if (output_paragraph_offset == paragraph_buffer_len)
    {
      output_paragraph =
	(char *) xrealloc (output_paragraph,
		      (paragraph_buffer_len += 100));
    }
}

/* Remove upto COUNT characters of whitespace from the
   the current output line.  If COUNT is less than zero,
   then remove until none left. */
kill_self_indent (count)
     int count;
{
  /* Handle infinite case first. */
  if (count < 0)
    {
      output_column = 0;
      while (output_paragraph_offset)
	{
	  if (whitespace (output_paragraph[output_paragraph_offset - 1]))
	    output_paragraph_offset--;
	  else
	    break;
	}
    }
  else
    {
      while (output_paragraph_offset && count--)
	if (whitespace (output_paragraph[output_paragraph_offset - 1]))
	  output_paragraph_offset--;
	else
	  break;
    }
}

flush_output ()
{
  register int i;

  if (!output_paragraph_offset)
    return;
  for (i = 0; i < output_paragraph_offset; i++)
    output_paragraph[i] &= 0x7f;

  fwrite (output_paragraph, 1, output_paragraph_offset, output_stream);
  output_position += output_paragraph_offset;
  output_paragraph_offset = 0;
}

/* How to close a paragraph controlling the number of lines between
   this one and the last one. */

/* Paragraph spacing is controlled by this variable.  It is the number of
   blank lines that you wish to appear between paragraphs.  A value of
   1 creates a single blank line between paragraphs. */
int paragraph_spacing = 1;


/* Close the current paragraph, leaving no blank lines between them. */
close_single_paragraph ()
{
  close_paragraph_with_lines (0);
}

close_paragraph_with_lines (lines)
     int lines;
{
  int old_spacing = paragraph_spacing;
  paragraph_spacing = lines;
  close_paragraph ();
  paragraph_spacing = old_spacing;
}

/* Non-zero means that start_paragraph () MUST be called before we pay
   any attention to close_paragraph () calls. */
int must_start_paragraph = 0;

/* Close the currently open paragraph. */
close_paragraph ()
{
  if (paragraph_is_open && !must_start_paragraph)
    {
      /* Gobble up blank lines that are extra... */
      register int tindex = output_paragraph_offset;
      register int c;
      while (tindex &&
	     ((c = output_paragraph[tindex - 1]) == ' ' || c == '\n'))
	output_paragraph[--tindex] = '\n';

      output_paragraph_offset = tindex;

      insert ('\n');
      {
	register int i;
	for (i = 0; i < paragraph_spacing; i++)
	  insert ('\n');
      }
      flush_output ();
      paragraph_is_open = false;
      no_indent = false;
    }
  last_char_was_newline = true;
}

/* Begin a new paragraph. */
start_paragraph ()
{
  close_paragraph ();		/* First close existing one. */

  paragraph_is_open = true;

  if (!must_start_paragraph)
    {
      output_column = 0;

      /* If doing indentation, then insert the appropriate amount. */
      if (!no_indent)
	{
	  if (inhibit_paragraph_indentation || paragraph_start_indent < 0)
	    output_column = current_indent;
	  else
	    output_column = current_indent + paragraph_start_indent;

	  indent (output_column);
	}
    }
  else
    must_start_paragraph = 0;
}

/* Insert the indentation specified by AMOUNT. */
indent (amount)
     int amount;
{
  while (--amount >= 0)
    insert (' ');
}

/* Search forward for STRING in input_text.
   FROM says where where to start. */
search_forward (string, from)
     char *string;
     int from;
{
  int len = strlen (string);

  while (from < size_of_input_text)
    {
      if (strnicmp (input_text + from, string, len) == 0)
	return (from);
      from++;
    }
  return (-1);
}

/* Whoops, Unix doesn't have stricmp, or strnicmp. */

/* Case independent string compare. */
stricmp (string1, string2)
     char *string1, *string2;
{
  char ch1, ch2;

  for (;;)
    {
      ch1 = *string1++;
      ch2 = *string2++;
      if (!(ch1 | ch2))
	return (0);

      ch1 = coerce_to_upper (ch1);
      ch2 = coerce_to_upper (ch2);

      if (ch1 != ch2)
	return (1);
    }
}

/* Compare at most COUNT characters from string1 to string2.  Case
   doesn't matter. */
strnicmp (string1, string2, count)
     char *string1, *string2;
{
  char ch1, ch2;

  while (count)
    {
      ch1 = *string1++;
      ch2 = *string2++;
      if (coerce_to_upper (ch1) == coerce_to_upper (ch2))
	count--;
      else
	break;
    }
  return (count);
}

enum insertion_type
{
  menu, quotation, lisp, example, smallexample, display,
  itemize, format, enumerate, table, group, ifinfo,
  defun, defvar, defopt, deffn, defspec, defmac,
  bad_type
};

char *insertion_type_names[] = {
	  "menu", "quotation", "lisp", "example", "smallexample", "display",
	  "itemize", "format", "enumerate", "table", "group", "ifinfo",
	  "defun", "defvar", "defopt", "deffn", "defspec", "defmac",
};

int insertion_level = 0;
typedef struct istack_elt
{
  struct istack_elt *next;
  char *item_function;
  int line_number;
  int filling_enabled;
  int indented_fill;
  enum insertion_type insertion;
  int inhibited;
} INSERTION_ELT;

INSERTION_ELT *insertion_stack = (INSERTION_ELT *) NULL;

init_insertion_stack ()
{
  insertion_stack = (INSERTION_ELT *) NULL;
}

/* Return the type of the current insertion. */
enum insertion_type
current_insertion_type ()
{
  if (!insertion_level)
    return (bad_type);
  else
    return (insertion_stack->insertion);
}

/* Return a pointer to the string which is the function
   to wrap around items. */
char *
current_item_function ()
{
  if (!insertion_level)
    return ((char *) NULL);
  else
    return (insertion_stack->item_function);
}

char *
get_item_function ()
{
  char *item_function;
  get_until ("\n", &item_function);
  canon_white (item_function);
  return (item_function);
}

 /* Push the state of the current insertion on the stack. */
push_insertion (type, item_function)
     enum insertion_type type;
     char *item_function;
{
  INSERTION_ELT *new = (INSERTION_ELT *) xmalloc (sizeof (INSERTION_ELT));

  new->item_function = item_function;
  new->filling_enabled = filling_enabled;
  new->indented_fill = indented_fill;
  new->insertion = type;
  new->line_number = line_number;
  new->inhibited = inhibit_paragraph_indentation;
  new->next = insertion_stack;
  insertion_stack = new;
  insertion_level++;
}

 /* Pop the value on top of the insertion stack into the
    global variables. */
pop_insertion ()
{
  INSERTION_ELT *temp = insertion_stack;
  if (temp == (INSERTION_ELT *) NULL)
    return;
  inhibit_paragraph_indentation = temp->inhibited;
  filling_enabled = insertion_stack->filling_enabled;
  indented_fill = insertion_stack->indented_fill;
  free_and_clear (&(temp->item_function));
  insertion_stack = insertion_stack->next;
  free (temp);
  insertion_level--;
}

 /* Return a pointer to the print name of this
    enumerated type. */
char *
insertion_type_pname (type)
     enum insertion_type type;
{
  if ((int) type < (int) bad_type)
    return (insertion_type_names[(int) type]);
  else
    return ("Broken-Type in insertion_type_pname");
}

/* Return the insertion_type associated with NAME.
   If the type is not one of the known ones, return BAD_TYPE. */
enum insertion_type
find_type_from_name (name)
     char *name;
{
  int index = 0;
  while (index < (int) bad_type)
    {
      if (stricmp (name, insertion_type_names[index]) == 0)
	return (enum insertion_type) index;
      index++;
    }
  return (bad_type);
}

do_nothing ()
{
}

int
defun_insertion (type)
     enum insertion_type type;
{
  return (type == defun ||
	  type == defvar ||
	  type == defopt ||
	  type == deffn ||
	  type == defspec ||
	  type == defmac);
}

/* Non-zero means that we are currently hacking the insides of an
   insertion which would use a fixed width font. */
int in_fixed_width_font = 0;

/* This is where the work for all the "insertion" style
   commands is done.  A huge switch statement handles the
   various setups, and generic code is on both sides. */
begin_insertion (type)
     enum insertion_type type;
{
  int no_discard = 0;

  close_paragraph ();

  if (defun_insertion (type))
    {
      push_insertion (type, savestring (""));
      no_discard = 1;
    }
  else
    push_insertion (type, get_item_function ());

  filling_enabled = false;	/* the general case for insertions. */
  inhibit_paragraph_indentation = 1;
  no_indent = false;

  switch (type)
    {
    case menu:
      add_word ("* Menu:\n");
      in_menu++;
      discard_until ("\n");
      input_text_offset--;
      /* discard_until () has already counted the newline.  Discount it. */
      line_number--;
      return;

      /* I think @quotation is meant to do filling.
	 If you don't want filling, then use @example. */
    case quotation:
      last_char_was_newline = 0;
      indented_fill = filling_enabled = true;
      current_indent += default_indentation_increment;
      break;

      /* Just like @example, but no indentation. */
    case format:
      in_fixed_width_font++;
      break;

    case display:
    case example:
    case smallexample:
    case lisp:
      last_char_was_newline = 0;
      current_indent += default_indentation_increment;
      in_fixed_width_font++;
      break;

    case table:
    case itemize:
      current_indent += default_indentation_increment;
      filling_enabled = indented_fill = true;

      /* Make things work for losers who forget the itemize syntax. */
      if (type == itemize)
	{
	  if (!(*insertion_stack->item_function))
	    {
	      free (insertion_stack->item_function);
	      insertion_stack->item_function = savestring ("*");
	    }
	}
      break;

    case enumerate:
      inhibit_paragraph_indentation = 0;
      current_indent += default_indentation_increment;
      start_numbering (1);
      filling_enabled = indented_fill = true;
      break;

    case group:
      inhibit_paragraph_indentation = 0;
      break;

    case ifinfo:
      /* Undo whatever we just did.  This is a no-op. */
      inhibit_paragraph_indentation = 0;
      filling_enabled = insertion_stack->filling_enabled;
      indented_fill = insertion_stack->indented_fill;
      break;

    case defun:
    case defvar:
    case defopt:
    case deffn:
    case defspec:
    case defmac:
      filling_enabled = indented_fill = true;
      current_indent += default_indentation_increment;
      break;
    }
  if (!no_discard)
    discard_until ("\n");
}

/* Try to end the quotation with the specified type.
   Like begin_insertion (), this uses a giant switch statement as
   well.  A big difference is that you *must* pass a valid type to
   this function, and a value of bad_type gets translated to match
   the value currently on top of the stack.  If, however, the value
   passed is a valid type, and it doesn't match the top of the
   stack, then we produce an error.  Should fix this, somewhat
   unclean. */
end_insertion (type)
     enum insertion_type type;
{
  enum insertion_type temp_type;

  if (!insertion_level)
    return;

  temp_type = current_insertion_type ();
  if (type == bad_type)
    type = temp_type;

  if (type != temp_type)
    {
      line_error ("Expected `%s', but saw `%s'.  Token unread",
	     insertion_type_pname (temp_type), insertion_type_pname (type));
      return;
    }
  pop_insertion ();

  switch (type)
    {

    case menu:
      in_menu--;		/* no longer hacking menus. */
      break;

    case enumerate:
      stop_numbering ();
      current_indent -= default_indentation_increment;
      break;

    case group:
    case ifinfo:
      break;

    case format:
    case example:
    case smallexample:
    case display:
    case lisp:
      in_fixed_width_font--;
      current_indent -= default_indentation_increment;
      break;

    default:
      current_indent -= default_indentation_increment;
      break;
    }
  close_paragraph ();
}

/* Insertions cannot cross certain boundaries, such as node beginnings.  In
   code that creates such boundaries, you should call discard_insertions ()
   before doing anything else.  It prints the errors for you, and cleans up
   the insertion stack. */
discard_insertions ()
{
  int real_line_number = line_number;
  while (insertion_stack)
    {
      if (insertion_stack->insertion == ifinfo)
	break;
      else
	{
	  char *offender = (char *) insertion_type_pname (insertion_stack->insertion);

	  line_number = insertion_stack->line_number;
	  line_error ("This `%s' doesn't have a matching `%cend %s'", offender,
		      COMMAND_PREFIX, offender);
	  pop_insertion ();
	}
    }
  line_number = real_line_number;
}

/* MAX_NS is the maximum nesting level for enumerations.  I picked 100
   which seemed reasonable.  This doesn't control the number of items,
   just the number of nested lists. */
#define max_ns 100
int number_stack[max_ns];
int number_offset = 0;
int the_current_number = 0;

start_numbering (at_number)
{
  if (number_offset + 1 == max_ns)
    {
      line_error ("Enumeration stack overflow");
      return;
    }
  number_stack[number_offset++] = the_current_number;
  the_current_number = at_number;
}

stop_numbering ()
{
  the_current_number = number_stack[--number_offset];
  if (number_offset < 0)
    number_offset = 0;
}

 /* Place a number into the output stream. */
number_item ()
{
  char temp[10];
  sprintf (temp, "%d. ", the_current_number);
  indent (output_column += (current_indent - strlen (temp)));
  add_word (temp);
  the_current_number++;
}

/* The actual commands themselves. */

/* Commands which insert themselves. */
insert_self ()
{
  add_word (command);
}

/* Force line break */
cm_asterisk ()
{
  /* Force a line break in the output. */
  insert ('\n');
  indent (output_column = current_indent);
}

/* Insert ellipsis. */
cm_dots (arg)
     int arg;
{
  if (arg == START)
    add_word ("...");
}

cm_bullet (arg)
     int arg;
{
  if (arg == START)
    add_char ('*');
}

cm_minus (arg)
     int arg;
{
  if (arg == START)
    add_char ('-');
}

/* Insert "TeX". */
cm_TeX (arg)
     int arg;
{
  if (arg == START)
    add_word ("TeX");
}

cm_copyright (arg)
     int arg;
{
  if (arg == START)
    add_word ("(C)");
}

cm_code (arg)
     int arg;
{
  extern int printing_index;

  if (printing_index)
    return;

  if (arg == START)
    add_char ('`');
  else
    add_word ("'");
}

cm_samp (arg)
     int arg;
{
  cm_code (arg);
}

cm_file (arg)
     int arg;
{
  cm_code (arg);
}

cm_kbd (arg)
     int arg;
{
  cm_code (arg);
}

cm_key (arg)
     int arg;
{
}

 /* Convert the character at position-1 into CTL. */
cm_ctrl (arg, position)
     int arg, position;
{
  if (arg == END)
    output_paragraph[position - 1] = CTL (output_paragraph[position - 1]);
}

/* Small Caps in makeinfo just does all caps. */
cm_sc (arg, start_pos, end_pos)
     int arg, start_pos, end_pos;
{
  if (arg == END)
    {
      while (start_pos < end_pos)
	{
	  output_paragraph[start_pos] =
	    coerce_to_upper (output_paragraph[start_pos]);
	  start_pos++;
	}
    }
}

/* @var in makeinfo just uppercases the text. */
cm_var (arg, start_pos, end_pos)
     int arg, start_pos, end_pos;
{
  if (arg == END)
    {
      while (start_pos < end_pos)
	{
	  output_paragraph[start_pos] =
	    coerce_to_upper (output_paragraph[start_pos]);
	  start_pos++;
	}
    }
}

cm_dfn (arg, position)
     int arg, position;
{
  add_char ('"');
}

cm_emph (arg)
     int arg;
{
  add_char ('*');
}

cm_strong (arg, position)
     int arg, position;
{
  cm_emph (arg);
}

cm_cite (arg, position)
     int arg, position;
{
  if (arg == START)
    add_word ("``");
  else
    add_word ("''");
}

cm_italic (arg)
{
}

cm_bold (arg)
{
  cm_italic (arg);
}

cm_roman (arg)
{
}

cm_title (arg)
{
  cm_italic (arg);
}

cm_refill ()
{
}

/* Prevent the argument from being split across two lines. */
cm_w (arg)
     int arg;
{
  if (arg == START)
    non_splitting_words++;
  else
    non_splitting_words--;
}


/* Explain that this command is obsolete, thus the user shouldn't
   do anything with it. */
cm_obsolete (arg)
{
  if (arg == START)
    warning ("The command `@%s' is obsolete", command);
}

/* Insert the text following input_text_offset up to the end of the line
   in a new, separate paragraph.  Directly underneath it, insert a
   line of WITH_CHAR, the same length of the inserted text. */
insert_and_underscore (with_char)
     int with_char;
{
  int len, i, old_no_indent;
  char *temp;

  close_paragraph ();
  filling_enabled =  indented_fill = false;
  old_no_indent = no_indent;
  no_indent = true;
  get_rest_of_line (&temp);

  len = output_position;
  execute_string ("%s\n", temp);
  free (temp);

  len = ((output_position + output_paragraph_offset) - 1) - len;
  for (i = 0; i < len; i++)
    add_char (with_char);
  insert ('\n');
  close_paragraph ();
  filling_enabled = true;
  no_indent = old_no_indent;
}

cm_chapter ()
{
  insert_and_underscore ('*');
}

cm_section ()
{
  insert_and_underscore ('=');
}

cm_subsection ()
{
  insert_and_underscore ('-');
}

cm_subsubsection ()
{
  insert_and_underscore ('.');
}

cm_unnumbered ()
{
  cm_chapter ();
}

cm_unnumberedsec ()
{
  cm_section ();
}

cm_unnumberedsubsec ()
{
  cm_subsection ();
}

cm_unnumberedsubsubsec ()
{
  cm_subsubsection ();
}

cm_appendix ()
{
  cm_chapter ();
}

cm_appendixsec ()
{
  cm_section ();
}

cm_appendixsubsec ()
{
  cm_subsection ();
}

cm_appendixsubsubsec ()
{
  cm_subsubsection ();
}

cm_majorheading ()
{
  cm_chapheading ();
}

cm_chapheading ()
{
  cm_chapter ();
}

cm_heading ()
{
  cm_section ();
}

cm_subheading ()
{
  cm_subsection ();
}

cm_subsubheading ()
{
  cm_subsubsection ();
}


/* **************************************************************** */
/*								    */
/*		   Adding nodes, and making tags		    */
/*								    */
/* **************************************************************** */

/* Start a new tag table. */
init_tag_table ()
{
  while (tag_table != (TAG_ENTRY *) NULL)
    {
      TAG_ENTRY *temp = tag_table;
      free (temp->node);
      free (temp->prev);
      free (temp->next);
      free (temp->up);
      tag_table = tag_table->next_ent;
      free (temp);
    }
}

write_tag_table ()
{
  return (write_tag_table_internal (false));	/* Not indirect. */
}

write_tag_table_indirect ()
{
  return (write_tag_table_internal (true));
}

/* Write out the contents of the existing tag table.
   INDIRECT_P says how to format the output. */
write_tag_table_internal (indirect_p)
     boolean indirect_p;
{
  TAG_ENTRY *node = tag_table;

  filling_enabled = false;
  must_start_paragraph = 0;
  close_paragraph ();
  if (!indirect_p)
    insert ('\n');
  add_word_args ("\037\nTag Table:\n%s", indirect_p ? "(Indirect)\n" : "");

  while (node != (TAG_ENTRY *) NULL)
    {
      add_word_args ("Node: %s\177%d\n", node->node, node->position);
      node = node->next_ent;
    }
  add_word ("\037\nEnd Tag Table\n");
  flush_output ();
}

char *
get_node_token ()
{
  char *string;

  get_until_in_line (",", &string);

  if (curchar () == ',')
    input_text_offset++;

  canon_white (string);

  /* Allow things like @@nodename. */
  normalize_node_name (string);

  return (string);
}

/* Given a node name in STRING, remove double @ signs, replacing them
   with just one. */
normalize_node_name (string)
     char *string;
{
  register int i, l = strlen (string);

  for (i = 0; i < l; i++)
    {
      if (string[i] == '@' && string[i + 1] == '@')
	{
	  strncpy (string + i, string + i + 1, l - i);
	  l--;
	}
    }
}

/* Look up NAME in the tag table, and return the associated
   tag_entry.  If the node is not in the table return NULL. */
TAG_ENTRY *
find_node (name)
     char *name;
{
  TAG_ENTRY *tag = tag_table;

  while (tag != (TAG_ENTRY *) NULL)
    {
      if (stricmp (tag->node, name) == 0)
	return (tag);
      tag = tag->next_ent;
    }
  return ((TAG_ENTRY *) NULL);
}

/* Remember NODE and associates. */
remember_node (node, prev, next, up, position, line_no, no_warn)
     char *node, *prev, *next, *up;
     int position, line_no, no_warn;
{
  /* Check for existence of this tag already. */
  if (validating)
    {
      register TAG_ENTRY *tag = find_node (node);
      if (tag)
	{
	  line_error ("Node `%s' multiply defined (%d is first definition)",
		      node, tag->line_no);
	  return;
	}
    }

  /* First, make this the current node. */
  current_node = node;

  /* Now add it to the list. */
  {
    TAG_ENTRY *new = (TAG_ENTRY *) xmalloc (sizeof (TAG_ENTRY));
    new->node = node;
    new->prev = prev;
    new->next = next;
    new->up = up;
    new->position = position;
    new->line_no = line_no;
    new->filename = node_filename;
    new->touched = 0;		/* not yet referenced. */
    new->flags = 0;
    if (no_warn)
      new->flags |= NO_WARN;
    new->next_ent = tag_table;
    tag_table = new;
  }
}

/* Here is a structure which associates sectioning commands with
   an integer, hopefully to reflect the `depth' of the current
   section. */
struct {
  char *name;
  int level;
} section_alist[] = {
  { "chapter", 1 },
  { "section", 2},
  { "subsec", 3},
  { "subsubsec", 4},
  { "unnumbered", 1},
  { "unnumberedsec", 2},
  { "unnumberedsubsec", 3},
  { "unnumberedsubsubsec", 4},
  { "appendix", 1},
  { "appendixsec", 2},
  { "appendixsubsec", 3},
  { "appendixsubsubsec", 4},
  { (char *)NULL, 0 }
};

/* Return an integer which identifies the type section present in TEXT. */
int
what_section (text)
     char *text;
{
  int i, j;
  char *t;

  for (j = 0; text[j] && whitespace (text[j]) || text[j] == '\n'; j++);
  if (text[j] != '@')
    return (-1);

  text = text + j + 1;

  /* Handle italicized sectioning commands. */
  if (*text == 'i')
    text++;

  for (i = 0; t = section_alist[i].name; i++)
    {
      if (strncmp (t, text, strlen (t)) == 0)
	return (section_alist[i].level);
    }
  return (-1);
}

/* The order is: nodename, nextnode, prevnode, upnode.
   The next, prev, and up fields can be defaulted.
   You must follow a node command which has those fields
   defaulted with a sectioning command (e.g. @chapter) giving
   the "level" of that node.  It is an error not to do so.
   The defaults come from the menu in this nodes parent. */
cm_node ()
{
  char *node, *prev, *next, *up;
  int new_node_pos, defaulting, this_section, no_warn = 0;
  extern int already_outputting_pending_notes;

  if (strcmp (command, "nwnode") == 0)
    no_warn = 1;

  /* Get rid of unmatched brace arguments from previous commands. */
  discard_braces ();

  /* There also might be insertions left lying around that haven't been
     ended yet.  Do that also. */
  discard_insertions ();

  if (!already_outputting_pending_notes)
    {
      close_paragraph ();
      output_pending_notes ();
      free_pending_notes ();
    }

  filling_enabled = indented_fill = false;
  new_node_pos = output_position + 1;

  node = get_node_token ();
  next = get_node_token ();
  prev = get_node_token ();
  up = get_node_token ();

  this_section = what_section (input_text + input_text_offset);

  /* ??? The first \n in the following string shouldn't be there, but I have
     to revamp the @example & @group things so that they always leave a \n
     as the last character output.  Until that time, this is the only way
     I can produce reliable output. */
  no_indent = true;
  add_word_args ("\n\037\nFile: %s,  Node: %s", pretty_output_filename, node);

  /* Check for defaulting of this node's next, prev, and up fields. */
  defaulting = ((strlen (next) == 0) &&
		(strlen (prev) == 0) &&
		(strlen (up) == 0));

  /* If we are defaulting, then look at the immediately following
     sectioning command (error if none) to determine the node's
     level.  Find the node that contains the menu mentioning this node
     that is one level up (error if not found).  That node is the "Up"
     of this node.  Default the "Next" and "Prev" from the menu. */
  if (defaulting)
    {
      NODE_REF *last_ref = (NODE_REF *)NULL;
      NODE_REF *ref = node_references;

      if (this_section < 0)
	{
	  char *polite_section_name = "chapter";
	  int i;

	  for (i = 0; section_alist[i].name; i++)
	    if (section_alist[i].level == current_section + 1)
	      {
		polite_section_name = section_alist[i].name;
		break;
	      }

	  line_error
	    ("Node `%s' requires a sectioning command (e.g. @%s)",
	     node, polite_section_name);
	}
      else
	{
	  while (ref)
	    {
	      if (ref->section == (this_section - 1) &&
		  ref->type == menu_reference &&
		  stricmp (ref->node, node) == 0)
		{
		  free (up);
		  up = savestring (ref->containing_node);

		  if (last_ref &&
		      strcmp
		      (last_ref->containing_node, ref->containing_node) == 0)
		    {
		      free (next);
		      next = savestring (last_ref->node);
		    }

		  if (ref->next &&
		      strcmp
		      (ref->next->containing_node, ref->containing_node) == 0)
		    {
		      free (prev);
		      prev = savestring (ref->next->node);
		    }
		  break;
		}
	      last_ref = ref;
	      ref = ref->next;
	    }
	}
    }

  if (*next)
    add_word_args (",  Next: %s", next);
  
  if (*prev)
    add_word_args (",  Prev: %s", prev);

  if (*up)
    add_word_args (",  Up: %s", up);

  insert ('\n');
  close_paragraph ();
  no_indent = false;

  if (!*node)
    {
      line_error ("No node name specified for `@%s' command", command);
      free (node);
      free (next);
      free (prev);
      free (up);
    }
  else
    {
      if (!*next) { free (next); next = (char *)NULL; }
      if (!*prev) { free (prev); prev = (char *)NULL; }
      if (!*up) { free (up); up = (char *)NULL; }
      remember_node (node, prev, next, up, new_node_pos, line_number, no_warn);
    }

  /* Change the section only if there was a sectioning command. */
  if (this_section >= 0)
    current_section = this_section;

  filling_enabled = true;
}

/* Validation of an info file.
   Scan through the list of tag entrys touching the Prev, Next, and Up
   elements of each.  It is an error not to be able to touch one of them,
   except in the case of external node references, such as "(DIR)".

   If the Prev is different from the Up,
   then the Prev node must have a Next pointing at this node.

   Every node except Top must have an Up.
   The Up node must contain some sort of reference, other than a Next,
   to this node.

   If the Next is different from the Next of the Up,
   then the Next node must have a Prev pointing at this node. */
validate_file (filename, tag_table)
     char *filename;
     TAG_ENTRY *tag_table;
{
  char *old_input_filename = input_filename;
  TAG_ENTRY *tags = tag_table;

  while (tags != (TAG_ENTRY *) NULL)
    {
      register TAG_ENTRY *temp_tag;

      input_filename = tags->filename;
      line_number = tags->line_no;

      /* If this node has a Next, then make sure that the Next exists. */
      if (tags->next)
	{
	  validate (tags->next, tags->line_no, "Next");

	  /* If the Next node exists, and there is no Up, then make
	     sure that the Prev of the Next points back. */
	  if (temp_tag = find_node (tags->next))
	    {
	      char *prev = temp_tag->prev;
	      if (!prev || (stricmp (prev, tags->node) != 0))
		{
		  line_error
		    ("Node `%s''s Next field not pointed back to", tags->node);
		  line_number = temp_tag->line_no;
		  input_filename = temp_tag->filename;
		  line_error
		    ("This node (`%s') is the one with the bad `Prev'",
		     temp_tag->node);
		  input_filename = tags->filename;
		  line_number = tags->line_no;
		  temp_tag->flags |= PREV_ERROR;
		}
	    }
	}

      /* Validate the Prev field if there is one, and we haven't already
	 complained about it in some way.  You don't have to have a Prev
	 field at this stage. */
      if (!(tags->flags & PREV_ERROR) && tags->prev)
	{
	  int valid = validate (tags->prev, tags->line_no, "Prev");

	  if (!valid)
	    tags->flags |= PREV_ERROR;
	  else
	    {
	      /* If the Prev field is not the same as the Up field,
		 then the node pointed to by the Prev field must have
		 a Next field which points to this node. */
	      if (tags->up && (stricmp (tags->prev, tags->up) != 0))
		{
		  temp_tag = find_node (tags->prev);
		  if (!temp_tag->next ||
		      (stricmp (temp_tag->next, tags->node) != 0))
		    {
		      line_error ("Node `%s''s Prev field not pointed back to",
				  tags->node);
		      line_number = temp_tag->line_no;
		      input_filename = temp_tag->filename;
		      line_error
			("This node (`%s') is the one with the bad `Next'",
			 temp_tag->node);
		      input_filename = tags->filename;
		      line_number = tags->line_no;
		      temp_tag->flags |= NEXT_ERROR;
		    }
		}
	    }
	}

      if (!tags->up && (stricmp (tags->node, "Top") != 0))
	line_error ("Node `%s' is missing an \"Up\" field", tags->node);
      else if (tags->up)
	{
	  int valid = validate (tags->up, tags->line_no, "Up");

	  /* If node X has Up: Y, then warn if Y fails to have a menu item
	     or note pointing at X, if Y isn't of the form "(Y)". */
	  if (valid && *tags->up != '(')
	    {
	      NODE_REF *nref, *tref, *list;
	      NODE_REF *find_node_reference ();

	      tref = (NODE_REF *) NULL;
	      list = node_references;

	      for (;;)
		{
		  if (!(nref = find_node_reference (tags->node, list)))
		    break;

		  if (stricmp (nref->containing_node, tags->up) == 0)
		    {
		      if (nref->type != menu_reference)
			{
			  tref = nref;
			  list = nref->next;
			}
		      else
			break;
		    }
		  list = nref->next;
		}

	      if (!nref)
		{
		  temp_tag = find_node (tags->up);
		  line_number = temp_tag->line_no;
		  filename = temp_tag->filename;
		  if (!tref)
		    line_error ("`%s' has an Up field of `%s', but `%s' has no menu item for `%s'",
				tags->node, tags->up, tags->up, tags->node);
		  line_number = tags->line_no;
		  filename = tags->filename;
		}
	    }
	}
      tags = tags->next_ent;
    }

  validate_other_references (node_references);
  /* We have told the user about the references which didn't exist.
     Now tell him about the nodes which aren't referenced. */

  tags = tag_table;
  while (tags != (TAG_ENTRY *) NULL)
    {
      /* Special hack.  If the node in question appears to have
         been referenced more than REFERENCE_WARNING_LIMIT times,
         give a warning. */
      if (tags->touched > reference_warning_limit)
	{
	  input_filename = tags->filename;
	  line_number = tags->line_no;
	  warning ("Node `%s' has been referenced %d times",
		   tags->node, tags->touched);
	}

      if (tags->touched == 0)
	{
	  input_filename = tags->filename;
	  line_number = tags->line_no;

	  /* Notice that the node "Top" is special, and doesn't have to
	     be referenced. */
	  if (stricmp (tags->node, "Top") != 0)
	    warning ("Unreferenced node `%s'", tags->node);
	}
      tags = tags->next_ent;
    }
  input_filename = old_input_filename;
}

/* Return 1 if tag correctly validated, or 0 if not. */
validate (tag, line, label)
     char *tag;
     int line;
     char *label;
{
  TAG_ENTRY *result;

  /* If there isn't a tag to verify, or if the tag is in another file,
     then it must be okay. */
  if (!tag || !*tag || *tag == '(')
    return (1);

  /* Otherwise, the tag must exist. */
  result = find_node (tag);

  if (!result)
    {
      line_number = line;
      line_error ("Validation error.  `%s' field points to node `%s', which doesn't exist",
		  label, tag);
      return (0);
    }
  result->touched++;
  return (1);
}

/* Split large output files into a series of smaller files.  Each file
   is pointed to in the tag table, which then gets written out as the
   original file.  The new files have the same name as the original file
   with a "-num" attached.  SIZE is the largest number of bytes to allow
   in any single split file. */
split_file (filename, size)
     char *filename;
     int size;
{
  char *root_filename, *root_pathname;
  char *the_file, *filename_part ();
  struct stat fileinfo;
  char *the_header;
  int header_size;

  /* Can only do this to files with tag tables. */
  if (!tag_table)
    return;

  if (size == 0)
    size = DEFAULT_SPLIT_SIZE;

  if ((stat (filename, &fileinfo) != 0) ||
      (fileinfo.st_size < SPLIT_SIZE_THRESHOLD))
    return;

  the_file = find_and_load (filename);
  if (!the_file)
    return;

  root_filename = filename_part (filename);
  root_pathname = pathname_part (filename);

  if (!root_pathname)
    root_pathname = savestring ("");

  /* Start splitting the file.  Walk along the tag table
     outputting sections of the file.  When we have written
     all of the nodes in the tag table, make the top-level
     pointer file, which contains indirect pointers and
     tags for the nodes. */
  {
    int which_file = 1;
    TAG_ENTRY *tags = tag_table;
    char *indirect_info = (char *)NULL;

    /* Remember the `header' of this file.  The first tag in the file is
       the bottom of the header; the top of the file is the start. */
    the_header = xmalloc (1 + (header_size = (tags->position - 2)));
    bcopy (the_file, the_header, header_size);

    while (tags)
      {
	int file_top, file_bot, limit;

	/* Have to include the Control-_. */
	file_top = file_bot = tags->position - 2;
	limit = file_top + size;

	/* If the rest of this file is only one node, then
	   that is the entire subfile. */
	if (!tags->next_ent)
	  {
	    int i = tags->position + 1;
	    char last_char = the_file[i];

	    while (i < fileinfo.st_size)
	      {
		if ((the_file[i] == '\037') &&
		    ((last_char == '\n') ||
		     (last_char == '\014')))
		  break;
		else
		  last_char = the_file[i];
		i++;
	      }
	    file_bot = i;
	    tags = tags->next_ent;
	    goto write_region;
	  }

	/* Otherwise, find the largest number of nodes that can fit in
	   this subfile. */
	for (; tags; tags = tags->next_ent)
	  {
	    if (!tags->next_ent)
	      {
		/* This entry is the last node.  Search forward for the end
	           of this node, and that is the end of this file. */
		int i = tags->position + 1;
		char last_char = the_file[i];

		while (i < fileinfo.st_size)
		  {
		    if ((the_file[i] == '\037') &&
			((last_char == '\n') ||
			 (last_char == '\014')))
		      break;
		    else
		      last_char = the_file[i];
		    i++;
		  }
		file_bot = i;

		if (file_bot < limit)
		  {
		    tags = tags->next_ent;
		    goto write_region;
		  }
		else
		  {
		    /* Here we want to write out everything before the last
		       node, and then write the last node out in a file
		       by itself. */
		    file_bot = tags->position;
		    goto write_region;
		  }
	      }

	    if (tags->next_ent->position > limit)
	      {
		if ((tags->position) - 2 == file_top)
		  tags = tags->next_ent;
		file_bot = tags->position;
	      write_region:
		{
		  int fd;
		  char *split_file = xmalloc (10 + strlen (root_pathname)
					      + strlen (root_filename));
		  sprintf (split_file,
		       "%s%s-%d", root_pathname, root_filename, which_file);

		  if (((fd = open (split_file, O_WRONLY | O_TRUNC | O_CREAT, 0666)) < 0)
		      || (write (fd, the_header, header_size) != header_size)
		      || (write (fd, the_file + file_top, file_bot - file_top)
			  != (file_bot - file_top))
		      || ((close (fd)) < 0))
		    {
		      perror (split_file);
		      close (fd);
		      exit (FATAL);
		    }

		  if (!indirect_info)
		    {
		      indirect_info = the_file + file_top;
		      sprintf (indirect_info, "\037\nIndirect:\n");
		      indirect_info += strlen (indirect_info);
		    }

		  sprintf (indirect_info, "%s-%d: %d\n",
			   root_filename, which_file, file_top);

		  free (split_file);
		  indirect_info += strlen (indirect_info);
		  which_file++;
		  break;
		}
	      }
	  }
      }

    /* We have sucessfully created the subfiles.  Now write out the
       original again.  We must use `output_stream', or
       write_tag_table_indirect () won't know where to place the output. */
    output_stream = fopen (filename, "w");
    if (!output_stream)
      {
	perror (filename);
	exit (FATAL);
      }

    {
      int distance = indirect_info - the_file;
      fwrite (the_file, 1, distance, output_stream);

      /* Inhibit newlines. */
      paragraph_is_open = false;

      write_tag_table_indirect ();
      fclose (output_stream);
      free (the_header);
      free (the_file);
      return;
    }
  }
}

/* Some menu hacking.  This is used to remember menu references while
   reading the input file.  After the output file has been written, if
   validation is on, then we use the contents of NODE_REFERENCES as a
   list of nodes to validate. */
char *
reftype_type_string (type)
     enum reftype type;
{
  switch (type)
    {
    case menu_reference:
      return ("Menu");
    case followed_reference:
      return ("Followed-Reference");
    default:
      return ("Internal-bad-reference-type");
    }
}

/* Remember this node name for later validation use. */
remember_node_reference (node, line, type)
     char *node;
     int line;
     enum reftype type;
{
  NODE_REF *temp = (NODE_REF *) xmalloc (sizeof (NODE_REF));

  temp->next = node_references;
  temp->node = savestring (node);
  temp->line_no = line;
  temp->section = current_section;
  temp->type = type;
  temp->containing_node = savestring (current_node);
  temp->filename = node_filename;

  node_references = temp;
}

validate_other_references (ref_list)
     register NODE_REF *ref_list;
{
  char *old_input_filename = input_filename;

  while (ref_list != (NODE_REF *) NULL)
    {
      input_filename = ref_list->filename;
      validate (ref_list->node, ref_list->line_no,
		reftype_type_string (ref_list->type));
      ref_list = ref_list->next;
    }
  input_filename = old_input_filename;
}

/* Find NODE in REF_LIST. */
NODE_REF *
find_node_reference (node, ref_list)
     char *node;
     register NODE_REF *ref_list;
{
  while (ref_list)
    {
      if (stricmp (node, ref_list->node) == 0)
	break;
      ref_list = ref_list->next;
    }
  return (ref_list);
}

free_node_references ()
{
  register NODE_REF *list, *temp;

  list = node_references;

  while (list)
    {
      temp = list;
      free (list->node);
      free (list->containing_node);
      list = list->next;
      free (temp);
    }
  node_references = (NODE_REF *) NULL;
}

#define menu_starter "* "
glean_node_from_menu ()
{
  /* This function gets called at the start of every line while inside of
     a menu.  It checks to see if the line starts with "* ", and if so,
     remembers the node reference that this menu refers to.

     input_text_offset is at the \n just before the line start. */

  int i, orig_offset = input_text_offset;
  char *nodename;

  if (strncmp (&input_text[input_text_offset + 1],
	       menu_starter,
	       strlen (menu_starter)) != 0)
    return;
  else
    input_text_offset += strlen (menu_starter) + 1;

  get_until_in_line (":", &nodename);
  if (curchar () == ':')
    input_text_offset++;
  canon_white (nodename);

  if (curchar () == ':')
    goto save_node;
  free (nodename);
  get_rest_of_line (&nodename);

  /* Special hack: If the nodename follows the menu item name,
     then we have to read the rest of the line in order to find
     out what the nodename is.  But we still have to read the
     line later, in order to process any formatting commands that
     might be present.  So un-count the carriage return that has just
     been counted. */
  line_number--;

  canon_white (nodename);
  for (i = 0; i < strlen (nodename); i++)
    {
      if (nodename[i] == '\t' ||
	  nodename[i] == '.' ||
	  nodename[i] == ',')
	{
	  nodename[i] = '\0';
	  break;
	}
    }
save_node:
  normalize_node_name (nodename);
  i = strlen (nodename);
  if (i && nodename[i - 1] == ':')
    nodename[i - 1] = '\0';

  remember_node_reference (nodename, line_number, menu_reference);
  free (nodename);
  input_text_offset = orig_offset;
}

cm_menu ()
{
  begin_insertion (menu);
}


/* **************************************************************** */
/*								    */
/*			Cross Reference Hacking			    */
/*								    */
/* **************************************************************** */

char *
get_xref_token ()
{
  char *string;

  get_until_in_braces (",", &string);
  if (curchar () == ',')
    input_text_offset++;
  fix_whitespace (string);
  normalize_node_name (string);
  return (string);
}

int px_ref_flag = 0;		/* Controls initial output string. */

/* Make a cross reference. */
cm_xref (arg)
{
  if (arg == START)
    {
      char *arg1, *arg2, *arg3, *arg4, *arg5;

      arg1 = get_xref_token ();
      arg2 = get_xref_token ();
      arg3 = get_xref_token ();
      arg4 = get_xref_token ();
      arg5 = get_xref_token ();

      add_word_args ("%s", px_ref_flag ? "*note " : "*Note ");

      if (*arg5 || *arg4)
	{
	  char *node_name;

	  if (!*arg2)
	    node_name = arg1;
	  else
	    node_name = arg2;

	  add_word_args ("%s: (%s)%s", arg2, arg4, arg1);
	  return;
	}
      else
	remember_node_reference (arg1, line_number, followed_reference);

      if (*arg3)
	{
	  if (!*arg2)
	    {
	      add_word_args ("%s: %s", arg3, arg1);
	    }
	  else
	    {
	      add_word_args ("%s: %s", arg2, arg1);
	    }
	  return;
	}

      if (*arg2)
	{
	  execute_string ("%s", arg2);
	  add_word_args (": %s", arg1);
	}
      else
	{
	  add_word_args ("%s::", arg1);
	}

    }
  else
    {

      /* Check to make sure that the next non-whitespace character is either
         a period or a comma. input_text_offset is pointing at the "}" which
         ended the xref or pxref command. */

      int temp = input_text_offset + 1;

      if (output_paragraph[output_paragraph_offset - 2] == ':' &&
	  output_paragraph[output_paragraph_offset - 1] == ':')
	return;
      while (temp < size_of_input_text)
	{
	  if (cr_or_whitespace (input_text[temp]))
	    temp++;
	  else
	    {
	      if (input_text[temp] == '.' ||
		  input_text[temp] == ',' ||
		  input_text[temp] == '\t')
		return;
	      else
		{
		  line_error ("Cross-reference must be terminated with a period or a comma");
		  return;
		}
	    }
	}
    }
}

cm_pxref (arg)
     int arg;
{
  if (arg == START)
    {
      px_ref_flag++;
      cm_xref (arg);
      px_ref_flag--;
    }
  else
    add_char ('.');
}

cm_inforef (arg)
     int arg;
{
  if (arg == START)
    {
      char *node, *pname, *file;

      node = get_xref_token ();
      pname = get_xref_token ();
      file = get_xref_token ();

      add_word_args ("*note %s: (%s)%s", pname, file, node);
    }
}

/* **************************************************************** */
/*								    */
/*			Insertion Command Stubs			    */
/*								    */
/* **************************************************************** */

cm_quotation ()
{
  begin_insertion (quotation);
}

cm_example ()
{
  begin_insertion (example);
}

cm_smallexample ()
{
  begin_insertion (smallexample);
}

cm_lisp ()
{
  begin_insertion (lisp);
}

cm_format ()
{
  begin_insertion (format);
}

cm_display ()
{
  begin_insertion (display);
}

cm_itemize ()
{
  begin_insertion (itemize);
}

cm_enumerate ()
{
  begin_insertion (enumerate);
}

cm_table ()
{
  begin_insertion (table);
}

cm_group ()
{
  begin_insertion (group);
}

cm_ifinfo ()
{
  begin_insertion (ifinfo);
}

cm_tex ()
{
  discard_until ("\n@end tex");
  discard_until ("\n");
}

cm_iftex ()
{
  discard_until ("\n@end iftex");
  discard_until ("\n");
}

cm_titlespec ()
{
  discard_until ("\n@end titlespec");
  discard_until ("\n");
}

cm_titlepage ()
{
  discard_until ("\n@end titlepage");
  discard_until ("\n");
}

cm_ignore ()
{
  discard_until ("\n@end ignore");
  discard_until ("\n");
}

/* **************************************************************** */
/*								    */
/*			@itemx, @item				    */
/*								    */
/* **************************************************************** */

/* Non-zero means a string is in execution, as opposed to a file. */
int executing_string = 0;

/* Execute the string produced by formatting the ARGs with FORMAT.  This
   is like submitting a new file with @include. */
execute_string (format, arg1, arg2, arg3, arg4, arg5)
     char *format;
{
  static char temp_string[4000];
  sprintf (temp_string, format, arg1, arg2, arg3, arg4, arg5);
  strcat (temp_string, "@bye\n");
  pushfile ();
  input_text_offset = 0;
  input_text = temp_string;
  input_filename = savestring (input_filename);
  size_of_input_text = strlen (temp_string);

  executing_string++;
  reader_loop ();

  popfile ();
  executing_string--;

  free_and_clear (&command);
  command = savestring ("not bye");
}

int itemx_flag = 0;

cm_itemx ()
{
  itemx_flag++;
  cm_item ();
  itemx_flag--;
}


cm_item ()
{
  char *rest_of_line, *item_func;

  /* Can only hack "@item" while inside of an insertion. */
  if (insertion_level)
    {
      get_until ("\n", &rest_of_line);
      canon_white (rest_of_line);
      item_func = current_item_function ();

      /* Okay, do the right thing depending on which insertion function
	 is active. */

      switch (current_insertion_type ())
	{
	case menu:
	case quotation:
	case example:
	case smallexample:
	case lisp:
	case format:
	case display:
	case group:
	case ifinfo:
	  line_error ("The `@%s' command is meaningless within a `@%s' block",
		      command,
		      insertion_type_pname (current_insertion_type ()));
	  break;

	case itemize:
	case enumerate:
	  if (itemx_flag)
	    {
	      line_error ("@itemx is not meaningful inside of a `%s' block",
			  insertion_type_pname (current_insertion_type ()));
	    }
	  else
	    {
	      start_paragraph ();
	      kill_self_indent (-1);
	      discard_until ("\n");
	      filling_enabled = indented_fill = true;

	      if (current_insertion_type () == itemize)
		{
		  indent (output_column = current_indent - 2);

		  /* I need some way to determine whether this command
		     takes braces or not.  I believe the user can type
		     either "@bullet" or "@bullet{}".  Of course, they
		     can also type "o" or "#" or whatever else they want. */
		  if (item_func && *item_func)
		    {
		      if (*item_func == '@')
			if (item_func[strlen (item_func) - 1] != '}')
			  execute_string ("%s{}", item_func);
			else
			  execute_string ("%s", item_func);
		      else
			execute_string ("%s", item_func);
		    }
		  insert (' ');
		  output_column++;
		}
	      else
		number_item ();

	      /* Special hack.  This makes close paragraph ignore you until
		 the start_paragraph () function has been called. */
	      must_start_paragraph = 1;
	    }
	  break;

	case table:
	  {
	    /* Get rid of extra characters. */
	    kill_self_indent (-1);

	    /* close_paragraph () almost does what we want.  The problem
	       is when paragraph_is_open, and last_char_was_newline, and
	       the last newline has been turned into a space, because
	       filling_enabled. I handle it here. */
	    if (last_char_was_newline && filling_enabled && paragraph_is_open)
	      insert ('\n');
	    close_paragraph ();

	    /* Indent on a new line, but back up one indentation level. */
	    /* force existing indentation. */
	    add_char ('i');
	    output_paragraph_offset--;
	    kill_self_indent (default_indentation_increment + 1);

	    /* Add item's argument to the line. */
	    filling_enabled = false;
	    if (!item_func && !(*item_func))
	      execute_string ("%s", rest_of_line);
	    else
	      execute_string ("%s{%s}", item_func, rest_of_line);

	    /* Start a new line, and let start_paragraph ()
	       do the indenting of it for you. */
	    close_single_paragraph ();
	    indented_fill = filling_enabled = true;
	  }
	}
      free (rest_of_line);
    }
  else
    line_error ("@%s found outside of an insertion block", command);
}


/* **************************************************************** */
/*								    */
/*			Defun and Friends       		    */
/*								    */
/* **************************************************************** */

/* The list of args that were passed to the def**** command. */
char **defun_args = (char **)NULL;

/* An alist mapping defun insertion types to the text that we use
   to describe them. */
struct {
  enum insertion_type type;
  char *title;
} type_title_alist[] = {
  { defun, "Function" },
  { defmac, "Macro" },
  { defspec, "Special form" },
  { defopt, "Option" },
  { deffn, (char *)NULL },
  { defvar, "Variable" },
  { (enum insertion_type)0, (char *)NULL }
};

/* Return the title string for this type of defun. */
char *
defun_title (type)
     enum insertion_type type;
{
  register int i;

  for (i = 0; type_title_alist[i].type || type_title_alist[i].title; i++)
    if (type_title_alist[i].type == type)
      return (type_title_alist[i].title);
  return (char *)NULL;
}

/* Return a list of words from the contents of STRING.
   You can group words with braces. */
char **
args_from_string (string)
     char *string;
{
  char **result = (char **) NULL;
  register int i, start, result_index, size;
  int len, skip_til_brace = 0;

  i = result_index = size = 0;

  /* Get a token, and stuff it into RESULT.  The tokens are split
     at spaces or tabs. */
  while (string[i])
    {
      /* Skip leading whitespace. */
      for (; string[i] && whitespace (string[i]); i++);

      start = i;

      if (!string[i])
	return (result);

      /* If this is a command which takes it's argument in braces, then
	 gobble the whole thing. */
      if (string[i] == COMMAND_PREFIX)
	{
	  register int j;
	  for (j = i; string[j] &&
	       !whitespace (string[j]) &&
	       string[j] != '{'; j++);

	  if (string[j] == '{')
	    {
	      while (string[j] && string[j] != '}')
		j++;

	      if (string[j])
		j++;

	      i = j;
	      goto add_arg;
	    }
	}
      
      if (string[i] == '{' && !whitespace (string[i + 1]))
	{
	  skip_til_brace = 1;
	  start = ++i;
	}

      /* Skip until whitespace or close brace. */
      while (string[i] &&
	     ((skip_til_brace && string[i] != '}') ||
	      (!skip_til_brace && !whitespace (string[i]))))
	i++;

    add_arg:
      len = i - start;
      if (result_index + 2 >= size)
	{
	  if (!size)
	    result = (char **) xmalloc ((size = 10) * (sizeof (char *)));
	  else
	    result =
	      (char **) xrealloc (result, ((size += 10) * (sizeof (char *))));
	}
      result[result_index] = (char *) xmalloc (1 + len);
      strncpy (result[result_index], string + start, len);
      result[result_index][len] = '\0';
      result_index++;
      result[result_index] = (char *) NULL;

      if (skip_til_brace)
	{
	  skip_til_brace = 0;
	  if (string[i])
	    i++;
	}
    }

  return (result);
}

get_defun_args ()
{
  register int i;
  char *line;

  get_rest_of_line (&line);

  if (defun_args)
    {
      for (i = 0; defun_args[i]; i++)
	free (defun_args[i]);
      free (defun_args);
    }

  defun_args = args_from_string (line);
  free (line);
}

insert_defun_arg (string, where)
     char *string;
     int where;
{
  register int i;

  for (i = 0; defun_args[i]; i++);
  defun_args = (char **)xrealloc (defun_args, (i + 2) * sizeof (char *));
  defun_args[i + 1] = (char *)NULL;

  for (; i != where; --i)
    defun_args[i] = defun_args[i - 1];

  defun_args[i] = savestring (string);
}

/* Make the defun type insertion.
   TYPE says which insertion this is.
   TITLE is the string to describe the object being described, or NULL
   for no title string.
   X_P says not to start a new insertion if non-zero. */
defun_internal (type, title, x_p)
     enum insertion_type type;
     char *title;
     int x_p;
{
  register int i = 0;
  char *type_name, *func_name;
  int old_no_indent = no_indent;

  get_defun_args ();

  if (title)
    insert_defun_arg (title, 0);

  if (defun_args[0])
    {
      type_name = defun_args[0];
      i++;

      if (defun_args[1])
	{
	  func_name = defun_args[1];
	  i++;
	}
      else
	func_name = "";
    }
  else
    type_name = "";

  no_indent = true;
  start_paragraph ();
  execute_string (" * %s: %s", type_name, func_name);
  no_indent = old_no_indent;

  for (; defun_args[i]; i++)
    {
      if (*defun_args[i] == '&')
	add_word_args (" %s", defun_args[i]);
      else
	execute_string (" @var{%s}", defun_args[i]);
    }
  add_char ('\n');
  if (type == defvar || type == defopt)
    execute_string ("@vindex %s\n", func_name);
  else
    execute_string ("@findex %s\n", func_name);

  if (!x_p)
    begin_insertion (type);
  else
    start_paragraph ();
}

/* Add an entry for a function, macro, special form, variable, or option.
   If the name of the calling command ends in `x', then this is an extra
   entry included in the body of an insertion of the same type. */
cm_defun ()
{
  int x_p;
  enum insertion_type type;
  char *title, *temp = savestring (command);

  x_p = (command[strlen (command) - 1] == 'x');

  if (x_p)
    temp[strlen (temp) - 1] = '\0';

  type = find_type_from_name (temp);
  free (temp);

  /* If we are adding to an already existing insertion, then make sure
     that we are already in an insertion of type TYPE. */
  if (x_p &&
      (!insertion_level || insertion_stack->insertion != type))
    {
      line_error ("Must be in a `%s' insertion in order to use `%s'x",
		  command, command);
      return;
    }

  title = defun_title (type);
  defun_internal (type, title, x_p);
}

/* End existing insertion block. */
cm_end ()
{
  char *temp;
  enum insertion_type type;

  if (!insertion_level)
    {
      line_error ("Unmatched `@%s'", command);
      return;
    }
  get_rest_of_line (&temp);
  canon_white (temp);

  if (strlen (temp) == 0)
    line_error ("`@%s' needs something after it", command);
  type = find_type_from_name (temp);
  if (type == bad_type)
    {
      line_error ("Bad argument to `%s', `%s', using `%s'",
	   command, temp, insertion_type_pname (current_insertion_type ()));
    }
  end_insertion (type);
  free (temp);
}


/* **************************************************************** */
/*								    */
/*			Other Random Commands		   	    */
/*								    */
/* **************************************************************** */

/* noindent () used to do something valueable, but it didn't follow the
   spec for noindent in the texinfo manual.  Now it does nothing, which,
   in the case of makeinfo, is correct. */
cm_noindent ()
{
/*  no_indent = true;
  indented_fill = false; */
}

/* I don't know exactly what to do with this.  Should I allow
   someone to switch filenames in the middle of output?  Since the
   file could be partially written, this doesn't seem to make sense.
   Another option: ignore it, since they don't *really* want to
   switch files.  Finally, complain, or at least warn. */
cm_setfilename ()
{
  char *filename;
  get_rest_of_line (&filename);
  /* warning ("`@%s %s' encountered and ignored", command, filename); */
  free (filename);
}

cm_comment ()
{
  discard_until ("\n");
}

cm_br ()
{
  close_paragraph ();
}

 /* Insert the number of blank lines passed as argument. */
cm_sp ()
{
  int lines;
  char *line;

/*  close_paragraph (); */
  get_rest_of_line (&line);

  sscanf (line, "%d", &lines);
  while (lines--)
    add_char ('\n');
  free (line);
}

cm_settitle ()
{
  discard_until ("\n");
}

cm_need ()
{
}

/* Start a new line with just this text on it.
   Then center the line of text.
   This always ends the current paragraph. */
cm_center ()
{
  char *line;

  close_paragraph ();
  filling_enabled = indented_fill = false;

  get_rest_of_line (&line);

  if (strlen (line) < fill_column)
    {
      int i = (fill_column - strlen (line)) / 2;
      while (i--)
	insert (' ');
    }
  execute_string (line);
  free (line);
  insert ('\n');
  close_paragraph ();
  filling_enabled = true;
}

/* Show what an expression returns. */
cm_result (arg)
     int arg;
{
  if (arg == END)
    add_word ("=>");
}

/* What an expression expands to. */
cm_expansion (arg)
     int arg;
{
  if (arg == END)
    add_word ("==>");
}

/* Indicates two expressions are equivalent. */
cm_equiv (arg)
     int arg;
{
  if (arg == END)
    add_word ("==");
}

/* What an expression may print. */
cm_print (arg)
     int arg;
{
  if (arg == END)
    add_word ("-|");
}

/* An error signaled. */
cm_error (arg)
     int arg;
{
  if (arg == END)
    add_word ("error-->");
}

/* The location of point in an example of a buffer. */
cm_point (arg)
     int arg;
{
  if (arg == END)
    add_word ("-!-");
}

/* Start a new line with just this text on it.
   The text is outdented one level if possible. */
cm_exdent ()
{
  char *line;
  int i = current_indent;

  if (current_indent)
    current_indent -= default_indentation_increment;

  get_rest_of_line (&line);
  close_single_paragraph ();
  add_word_args ("%s", line);
  current_indent = i;
  free (line);
  close_single_paragraph ();
}

cm_include ()
{
  cm_infoinclude ();
}

/* Remember this file, and move onto the next. */
cm_infoinclude ()
{
  char *filename;

  close_paragraph ();
  get_rest_of_line (&filename);
  pushfile ();

  /* In verbose mode we print info about including another file. */
  if (verbose_mode)
    {
      register int i = 0;
      register FSTACK *stack = filestack;

      for (i = 0, stack = filestack; stack; stack = stack->next, i++);

      i *= 2;

      printf ("%*s", i, "");
      printf ("%c%s %s\n", COMMAND_PREFIX, command, filename);
      fflush (stdout);
    }

  if (!find_and_load (filename))
    {
      extern char *sys_errlist[];
      extern int errno, sys_nerr;
      popfile ();

      /* Cannot "@include foo", in line 5 of "/wh/bar". */
      line_error ("`%c%s %s': %s", COMMAND_PREFIX, command, filename,
		  ((errno < sys_nerr) ?
		   sys_errlist[errno] : "Unknown file system error"));
    }
  free (filename);
}

/* The other side of a malformed expression. */
misplaced_brace ()
{
  line_error ("Misplaced `}'");
}

/* Don't let the filling algorithm insert extra whitespace here. */
cm_force_abbreviated_whitespace ()
{
}

/* Make the output paragraph end the sentence here, even though it
   looks like it shouldn't.  This also inserts the character which
   invoked it. */
cm_force_sentence_end ()
{
  add_char (META ((*command)));
}

/* Signals end of processing.  Easy to make this happen. */
cm_bye ()
{
  input_text_offset = size_of_input_text;
}

cm_asis ()
{
}

cm_setchapternewpage ()
{
  discard_until ("\n");
}

cm_smallbook ()
{
  discard_until ("\n");
}

/* **************************************************************** */
/*								    */
/*			Indexing Stuff				    */
/*								    */
/* **************************************************************** */


/* An index element... */
typedef struct index_elt
{
  struct index_elt *next;
  char *entry;			/* The index entry itself. */
  char *node;			/* The node from whence it came. */
  int code;			/* Non-zero means add `@code{...}' when
				   printing this element. */
} INDEX_ELT;

/* A list of short-names for each index, and the index to that index in our
   index array, the_indices.  In addition, for each index, it is remembered
   whether that index is a code index or not.  Code indices have @code{}
   inserted around the first word when they are printed with printindex. */
typedef struct
{
  char *name;
  int index;
  int code;
} INDEX_ALIST;

INDEX_ALIST **name_index_alist = (INDEX_ALIST **) NULL;

/* An array of pointers.  Each one is for a different index.  The
   "synindex" command changes which array slot is pointed to by a
   given "index". */
INDEX_ELT **the_indices = (INDEX_ELT **) NULL;

/* The number of defined indices. */
int defined_indices = 0;

/* We predefine these. */
#define program_index 0
#define function_index 1
#define concept_index 2
#define variable_index 3
#define datatype_index 4
#define key_index 5

init_indices ()
{
  int i;

  /* Create the default data structures. */

  /* Initialize data space. */
  if (!the_indices)
    {
      the_indices = (INDEX_ELT **) xmalloc ((1 + defined_indices) *
					    sizeof (INDEX_ELT *));
      the_indices[defined_indices] = (INDEX_ELT *) NULL;

      name_index_alist = (INDEX_ALIST **) xmalloc ((1 + defined_indices) *
						   sizeof (INDEX_ALIST *));
      name_index_alist[defined_indices] = (INDEX_ALIST *) NULL;
    }

  /* If there were existing indices, get rid of them now. */
  for (i = 0; i < defined_indices; i++)
    undefindex (name_index_alist[i]->name);

  /* Add the default indices. */
  defindex ("pg", 0);
  defindex ("fn", 1);		/* "fn" is a code index.  */
  defindex ("cp", 0);
  defindex ("vr", 0);
  defindex ("tp", 0);
  defindex ("ky", 0);

}

/* Find which element in the known list of indices has this name.
   Returns -1 if NAME isn't found. */
int
find_index_offset (name)
     char *name;
{
  register int i;
  for (i = 0; i < defined_indices; i++)
    if (name_index_alist[i] &&
	stricmp (name, name_index_alist[i]->name) == 0)
      return (name_index_alist[i]->index);
  return (-1);
}

/* Return a pointer to the entry of (name . index) for this name.
   Return NULL if the index doesn't exist. */
INDEX_ALIST *
find_index (name)
     char *name;
{
  int offset = find_index_offset (name);
  if (offset > -1)
    return (name_index_alist[offset]);
  else
    return ((INDEX_ALIST *) NULL);
}

/* Given an index name, return the offset in the_indices of this index,
   or -1 if there is no such index. */
translate_index (name)
     char *name;
{
  INDEX_ALIST *which = find_index (name);

  if (which)
    return (which->index);
  else
    return (-1);
}

/* Return the index list which belongs to NAME. */
INDEX_ELT *
index_list (name)
     char *name;
{
  int which = translate_index (name);
  if (which < 0)
    return ((INDEX_ELT *) - 1);
  else
    return (the_indices[which]);
}

/* Please release me, let me go... */
free_index (index)
     INDEX_ELT *index;
{
  INDEX_ELT *temp;

  while ((temp = index) != (INDEX_ELT *) NULL)
    {
      free (temp->entry);
      free (temp->node);
      index = index->next;
      free (temp);
    }
}

/* Flush an index by name. */
undefindex (name)
     char *name;
{
  int i;
  int which = find_index_offset (name);

  if (which < 0)
    return (which);

  i = name_index_alist[which]->index;


  free_index (the_indices[i]);
  the_indices[i] = (INDEX_ELT *) NULL;

  free (name_index_alist[which]->name);
  free (name_index_alist[which]);
  name_index_alist[which] = (INDEX_ALIST *) NULL;
}

/* Define an index known as NAME.  We assign the slot number.
   CODE if non-zero says to make this a code index. */
defindex (name, code)
     char *name;
     int code;
{
  register int i, slot;

  /* If it already exists, flush it. */
  undefindex (name);

  /* Try to find an empty slot. */
  slot = -1;
  for (i = 0; i < defined_indices; i++)
    if (!name_index_alist[i])
      {
	slot = i;
	break;
      }

  if (slot < 0)
    {
      /* No such luck.  Make space for another index. */
      slot = defined_indices;
      defined_indices++;

      name_index_alist = (INDEX_ALIST **) xrealloc (name_index_alist,
						    (1 + defined_indices)
						  * sizeof (INDEX_ALIST *));
      the_indices = (INDEX_ELT **) xrealloc (the_indices,
					     (1 + defined_indices)
					     * sizeof (INDEX_ELT *));
    }

  /* We have a slot.  Start assigning. */
  name_index_alist[slot] = (INDEX_ALIST *) xmalloc (sizeof (INDEX_ALIST));
  name_index_alist[slot]->name = savestring (name);
  name_index_alist[slot]->index = slot;
  name_index_alist[slot]->code = code;

  the_indices[slot] = (INDEX_ELT *) NULL;
}

/* Add the arguments to the current index command to the index NAME. */
index_add_arg (name)
     char *name;
{
  int which;
  char *index_entry;
  INDEX_ALIST *tem;

  tem = find_index (name);

  which = tem ? tem->index : -1;

  /* close_paragraph (); */
  get_rest_of_line (&index_entry);

  if (which < 0)
    {
      line_error ("Unknown index reference `%s'", name);
      free (index_entry);
    }
  else
    {
      INDEX_ELT *new = (INDEX_ELT *) xmalloc (sizeof (INDEX_ELT));
      new->next = the_indices[which];
      new->entry = index_entry;
      new->node = current_node;
      new->code = tem->code;
      the_indices[which] = new;
    }
}

#define INDEX_COMMAND_SUFFIX "index"

/* The function which user defined index commands call. */
gen_index ()
{
  char *name = savestring (command);
  if (strlen (name) >= strlen ("index"))
    name[strlen (name) - strlen ("index")] = '\0';
  index_add_arg (name);
  free (name);
}

/* Define a new index command.  Arg is name of index. */
cm_defindex ()
{
  gen_defindex (0);
}

cm_defcodeindex ()
{
  gen_defindex (1);
}

gen_defindex (code)
     int code;
{
  char *name;
  get_rest_of_line (&name);

  if (find_index (name))
    {
      line_error ("Index `%s' already exists", name);
      free (name);
      return;
    }
  else
    {
      char *temp = (char *) alloca (1 + strlen (name) + strlen ("index"));
      sprintf (temp, "%sindex", name);
      define_user_command (temp, gen_index, 0);
      defindex (name, code);
      free (name);
    }
}

/* Append LIST2 to LIST1.  Return the head of the list. */
INDEX_ELT *
index_append (head, tail)
     INDEX_ELT *head, *tail;
{
  register INDEX_ELT *t_head = head;

  if (!t_head)
    return (tail);

  while (t_head->next)
    t_head = t_head->next;
  t_head->next = tail;
  return (head);
}

/* Expects 2 args, on the same line.  Both are index abbreviations.
   Make the first one be a synonym for the second one, i.e. make the
   first one have the same index as the second one. */
cm_synindex ()
{
  int redirector, redirectee;
  char *temp;

  skip_whitespace ();
  get_until_in_line (" ", &temp);
  redirectee = find_index_offset (temp);
  skip_whitespace ();
  free_and_clear (&temp);
  get_until_in_line (" ", &temp);
  redirector = find_index_offset (temp);
  free (temp);
  if (redirector < 0 || redirectee < 0)
    {
      line_error ("Unknown index reference");
    }
  else
    {
      /* I think that we should let the user make indices synonymous to
         each other without any lossage of info.  This means that one can
         say @synindex cp dt anywhere in the file, and things that used to
         be in cp will go into dt. */
      INDEX_ELT *i1 = the_indices[redirectee], *i2 = the_indices[redirector];

      if (i1 || i2)
	{
	  if (i1)
	    the_indices[redirectee] = index_append (i1, i2);
	  else
	    the_indices[redirectee] = index_append (i2, i1);
	}

      name_index_alist[redirectee]->index =
	name_index_alist[redirector]->index;
    }
}

cm_pindex ()			/* Pinhead index. */
{
  index_add_arg ("pg");
}

cm_vindex ()			/* variable index */
{
  index_add_arg ("vr");
}

cm_kindex ()			/* key index */
{
  index_add_arg ("ky");
}

cm_cindex ()			/* concept index */
{
  index_add_arg ("cp");
}

cm_findex ()			/* function index */
{
  index_add_arg ("fn");
}

cm_tindex ()			/* data type index */
{
  index_add_arg ("tp");
}

/* Sorting the index. */
index_element_compare (element1, element2)
     INDEX_ELT **element1, **element2;
{
  /* This needs to ignore leading non-text characters. */
  return (strcmp ((*element1)->entry, (*element2)->entry));
}

/* Sort the index passed in INDEX, returning an array of
   pointers to elements.  The array is terminated with a NULL
   pointer.  We call qsort because it's supposed to be fast.
   I think this looks bad. */
INDEX_ELT **
sort_index (index)
     INDEX_ELT *index;
{
  INDEX_ELT *temp = index;
  INDEX_ELT **array;
  int count = 0;

  while (temp != (INDEX_ELT *) NULL)
    {
      count++;
      temp = temp->next;
    }

  /* We have the length.  Make an array. */

  array = (INDEX_ELT **) xmalloc ((count + 1) * sizeof (INDEX_ELT *));
  count = 0;
  temp = index;

  while (temp != (INDEX_ELT *) NULL)
    {
      array[count++] = temp;
      temp = temp->next;
    }
  array[count] = (INDEX_ELT *) NULL;	/* terminate the array. */

  /* Sort the array. */
  qsort (array, count, sizeof (INDEX_ELT *), index_element_compare);

  return (array);
}

/* Non-zero means that we are in the middle of printing an index. */
int printing_index = 0;

/* Takes one arg, a short name of an index to print.
   Outputs a menu of the sorted elements of the index. */
cm_printindex ()
{
  int item;
  INDEX_ELT *index;
  INDEX_ELT **array;
  char *index_name;
  int old_inhibitions = inhibit_paragraph_indentation;
  boolean previous_filling_enabled_value = filling_enabled;

  close_paragraph ();
  get_rest_of_line (&index_name);

  index = index_list (index_name);
  if ((int) index < 0)
    {
      line_error ("Unknown index name `%s'", index_name);
      free (index_name);
      return;
    }
  else
    free (index_name);

  array = sort_index (index);

  filling_enabled = false;
  inhibit_paragraph_indentation = 1;
  close_paragraph ();
  add_word ("* Menu:\n\n");

  printing_index = 1;
  for (item = 0; (index = array[item]); item++)
    {
      /* If this particular entry should be printed as a "code" index,
	 then wrap the entry with "@code{...}". */
      if (index->code)
	execute_string ("* @code{%s}: %s.\n", index->entry, index->node);
      else
	execute_string ("* %s: %s.\n", index->entry, index->node);
      flush_output ();
    }
  printing_index = 0;
  free (array);
  close_paragraph ();
  filling_enabled = previous_filling_enabled_value;
  inhibit_paragraph_indentation = old_inhibitions;
}


/* **************************************************************** */
/*								    */
/*		    Making User Defined Commands		    */
/*								    */
/* **************************************************************** */

define_user_command (name, proc, needs_braces_p)
     char *name;
     FUNCTION *proc;
     int needs_braces_p;
{
  int slot = user_command_array_len;
  user_command_array_len++;

  if (!user_command_array)
    user_command_array = (COMMAND **) xmalloc (1 * sizeof (COMMAND *));

  user_command_array = (COMMAND **) xrealloc (user_command_array,
					      (1 + user_command_array_len) *
					      sizeof (COMMAND *));

  user_command_array[slot] = (COMMAND *) xmalloc (sizeof (COMMAND));
  user_command_array[slot]->name = savestring (name);
  user_command_array[slot]->proc = proc;
  user_command_array[slot]->argument_in_braces = needs_braces_p;
}

/* Make ALIAS run the named FUNCTION.  Copies properties from FUNCTION. */
define_alias (alias, function)
     char *alias, *function;
{
}

/* Some support for footnotes. */

/* Footnotes are a new construct in Info.  We don't know the best method
   of implementing them for sure, so we present two possiblities.

MN   1) Make them look like followed references, with the reference
        destinations in a makeinfo manufactured node or,

BN   2) Make them appear at the bottom of the node that they originally
        appeared in.
*/

#define MN 0
#define BN 1

int footnote_style = MN;
boolean first_footnote_this_node = true;
int footnote_count = 0;

/* Set the footnote style based on he style identifier in STRING. */
set_footnote_style (string)
     char *string;
{
  if (stricmp (string, "MN") == 0)
    {
      footnote_style = MN;
      return;
    }

  if (stricmp (string, "BN") == 0)
    {
      footnote_style = BN;
      return;
    }
}

typedef struct fn
{
  struct fn *next;
  char *marker;
  char *note;
}  FN;

FN *pending_notes = (FN *) NULL;

/* A method for remembering footnotes.  Note that this list gets output
   at the end of the current node. */
remember_note (marker, note)
     char *marker, *note;
{
  FN *temp = (FN *) xmalloc (sizeof (FN));

  temp->marker = savestring (marker);
  temp->note = savestring (note);
  temp->next = pending_notes;
  pending_notes = temp;
  footnote_count++;
}

/* How to get rid of existing footnotes. */
free_pending_notes ()
{
  FN *temp;

  while ((temp = pending_notes) != (FN *) NULL)
    {
      free (temp->marker);
      free (temp->note);
      pending_notes = pending_notes->next;
      free (temp);
    }
  first_footnote_this_node = true;
  footnote_count = 0;
}

/* What to do when you see a @footnote construct. */

 /* Handle a "footnote".
    footnote *{this is a footnote}
    where "*" is the marker character for this note. */
cm_footnote ()
{
  char *marker;
  char *note;

  get_until ("{", &marker);
  canon_white (marker);

  /* Read the argument in braces. */
  if (curchar () != '{')
    {
      line_error ("`@%s' expected more than just `%s'.  It needs something in `{...}'", command, marker);
      free (marker);
      return;
    }
  else
    {
      int braces = 1;
      int temp = ++input_text_offset;
      int len;

      while (braces)
	{
	  if (temp == size_of_input_text)
	    {
	      line_error ("No closing brace for footnote `%s'", marker);
	      return;
	    }
	  if (input_text[temp] == '{')
	    braces++;
	  else if (input_text[temp] == '}')
	    braces--;
	  temp++;
	}

      len = (temp - input_text_offset) - 1;
      note = xmalloc (len + 1);
      strncpy (note, &input_text[input_text_offset], len);
      note[len] = '\0';
      input_text_offset = temp;
    }

  if (!current_node || !*current_node)
    {
      line_error ("Footnote defined without parent node");
      free (marker);
      free (note);
      return;
    }

  remember_note (marker, note);

  switch (footnote_style)
    {				/* your method should at least insert marker. */

    case MN:
      add_word_args ("(%s)", marker);
      if (first_footnote_this_node)
	{
	  char *temp_string = xmalloc ((strlen (current_node))
				       + (strlen ("-Footnotes")) + 1);
	  add_word_args (" (*note %s-Footnotes::)", current_node);
	  strcpy (temp_string, current_node);
	  strcat (temp_string, "-Footnotes");
	  remember_node_reference (temp_string, line_number, followed_reference);
	  free (temp_string);
	  first_footnote_this_node = false;
	}
      break;

    case BN:
      add_word_args ("(%s)", marker);
      break;

    default:
      break;
    }
  free (marker);
  free (note);
}

/* Non-zero means that we are currently in the process of outputting
   footnotes. */
int already_outputting_pending_notes = 0;

/* Output the footnotes.  We are at the end of the current node. */
output_pending_notes ()
{
  FN *footnote = pending_notes;

  if (!pending_notes)
    return;

  switch (footnote_style)
    {

    case MN:
      {
	char *old_current_node = current_node;
	char *old_command = savestring (command);

	already_outputting_pending_notes++;
	execute_string ("@node %s-Footnotes,,,%s\n", current_node, current_node);
	already_outputting_pending_notes--;
	current_node = old_current_node;
	free (command);
	command = old_command;
      }
      break;

    case BN:
      close_paragraph ();
      in_fixed_width_font++;
      execute_string ("---------- Footnotes ----------\n\n");
      in_fixed_width_font--;
      break;
    }

  /* Handle the footnotes in reverse order. */
  {
    FN **array = (FN **) xmalloc ((footnote_count + 1) * sizeof (FN *));

    array[footnote_count] = (FN *) NULL;

    while (--footnote_count > -1)
      {
	array[footnote_count] = footnote;
	footnote = footnote->next;
      }

    filling_enabled = true;
    indented_fill = true;

    while (footnote = array[++footnote_count])
      {

	switch (footnote_style)
	  {

	  case MN:
	  case BN:
	    execute_string ("(%s)  %s", footnote->marker, footnote->note);
	    close_paragraph ();
	    break;
	  }
      }
    close_paragraph ();
    free (array);
  }
}

/*
 * Local variables:
 * compile-command: "gcc -g -Bstatic -o makeinfo makeinfo.c getopt.c"
 * end:
 */

