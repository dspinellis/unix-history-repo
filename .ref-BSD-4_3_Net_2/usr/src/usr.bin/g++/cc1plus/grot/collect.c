/* Output tables which say what global initializers need
   to be called at program startup, and what global destructors
   need to be called at program termination, for GNU C++ compiler.
   Copyright (C) 1987 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@mcc.com)
   COFF Changes by Dirk Grunwald (grunwald@flute.cs.uiuc.edu)

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


/* This file contains all the code needed for the program `collect'.

   `collect' is run on all object files that are processed
   by GNU C++, to create a list of all the file-level
   initialization and destruction that need to be performed.
   It generates an assembly file which holds the tables
   which are walked by the global init and delete routines.
   The format of the tables are an integer length,
   followed by the list of function pointers for the
   routines to be called.

   Constructors are called in the order they are laid out
   in the table.  Destructors are called in the reverse order
   of the way they lie in the table.  */

#include "config.h"
int target_flags;	/* satisfy dependency in config.h */

#ifndef USE_COLLECT
main()
{
  exit(0);
}

#else


#include <sys/types.h>
#include <sys/stat.h>

#include <stdio.h>
#include <a.out.h>
#include <ar.h>

#ifdef UMAX
#include <sgs.h>
#endif

/*
 *	Define various output routines in terms on ASM_INT_OP,
 *	which should be defined in config.h -- if it's not, add it
 *	as the marker used to allocate an int on your architecture.
 */
#ifdef i386
#define ASM_INT_OP	ASM_LONG
#define NO_UNDERSCORES 1
#endif

#ifdef mips
#define NO_UNDERSCORES 1
/*
 * provide stubs for various things in tm-mips.h
 */
enum mips_sections cur_section;
int inside_function = 0;
int num_source_filenames = 0;
void print_options() { return; };
void data_section() { return; };
FILE *asm_out_text_file, *asm_out_data_file;
void pfatal_with_name(str)
char *str;
{
  fprintf(stderr,"collect: %s\n", str);
  exit(1);
}
void pic_label_is_global () {return;};
#endif

#ifndef ASM_OUTPUT_INT_CONST
#define ASM_OUTPUT_INT_CONST(FILE,VALUE)	\
  fprintf(FILE,"\t%s %d\n", ASM_INT_OP, VALUE)
#endif

#ifndef ASM_OUTPUT_LABELREF_AS_INT
#define ASM_OUTPUT_LABELREF_AS_INT(FILE,NAME)	\
  (fprintf(FILE,"\t%s", ASM_INT_OP), \
   ASM_OUTPUT_LABELREF(FILE,NAME), \
   fprintf(FILE,"\n"))
#endif

#ifndef ASM_OUTPUT_PTR_INT_SUM
#define ASM_OUTPUT_PTR_INT_SUM(FILE,NAME,VALUE)	\
  (fprintf(FILE,"\t%s", ASM_INT_OP), \
   ASM_OUTPUT_LABELREF(FILE,NAME), \
   fprintf(FILE,"+%d\n", VALUE))
#endif

#ifndef ASM_OUTPUT_SOURCE_FILENAME
#define ASM_OUTPUT_SOURCE_FILENAME(FILE,NAME)\
  fprintf (FILE, "\t.file\t\"%s\"\n", NAME);
#endif

extern int xmalloc ();
extern void free ();

#ifndef CTOR_TABLE_NAME
#define CTOR_TABLE_NAME "__CTOR_LIST__"
#endif

#ifndef DTOR_TABLE_NAME
#define DTOR_TABLE_NAME "__DTOR_LIST__"
#endif

/*	Define or undef this in your config.h. Should be defined for
 *	MIPS & Sun-386i.
 */

#ifdef NO_UNDERSCORES
#  ifndef CTOR_DTOR_MARKER_NAME
#    ifndef NO_DOLLAR_IN_LABEL
#      define CTOR_DTOR_MARKER_NAME "_GLOBAL_$"
#    else
#      define CTOR_DTOR_MARKER_NAME "_GLOBAL_."
#    endif
#    define CTOR_DTOR_MARKER_LENGTH 9
#    define CTOR_DTOR_MARKER_OFFSET 0
#  endif
#else
#  ifndef CTOR_DTOR_MARKER_NAME
#    ifndef NO_DOLLAR_IN_LABEL
#      define CTOR_DTOR_MARKER_NAME "__GLOBAL_$"
#    else
#      define CTOR_DTOR_MARKER_NAME "__GLOBAL_."
#    endif
#    define CTOR_DTOR_MARKER_LENGTH 10
#    define CTOR_DTOR_MARKER_OFFSET 1
#  endif
#endif

enum error_code { OK, BAD_MAGIC, NO_NAMELIST,
		  FOPEN_ERROR, FREAD_ERROR, FWRITE_ERROR,
		  RANDOM_ERROR, };

enum error_code process ();
enum error_code process_a (), process_o ();
enum error_code output_ctor_dtor_table ();
void assemble_name ();

/* Files for holding the assembly code for table of constructor
   function pointer addresses and list of destructor
   function pointer addresses.  */
static FILE *outfile;

/* Default outfile name, or take name from argv with -o option.  */
static char *outfile_name = "a.out";

/* For compatibility with toplev.c and tm-sun386.h  */
char *dump_base_name;
int optimize = 0;
char *language_string = "GNU C++";
char *main_input_filename;

/*
 * The list of constructors & destructors. We process all files & then
 * spit these out in output_ctor_dtor_table(), because we need to know
 *  the length of the list.
 */

struct ctor_dtor_list_elem
{
  struct ctor_dtor_list_elem *next;
  char *name;
} *dtor_chain, *ctor_chain;

int dtor_chain_length = 0;
int ctor_chain_length = 0;

main (argc, argv)
     int argc;
     char *argv[];
{
  int i, nerrs = 0;
  enum error_code code;
  FILE *fp;

  if (argc > 2 && !strcmp (argv[1], "-o"))
    {
      outfile_name = argv[2];
      i = 3;
    }
  else
    i = 1;

  if ((outfile = fopen (outfile_name, "w")) == NULL)
    {
      perror ("collect");
      exit (-1);
    }

  dump_base_name = main_input_filename = outfile_name;
  for (; i < argc; i++)
    {
      char buf[80];

      /* This is a library, skip it.  */
      if (argv[i][0] == '-' && argv[i][1] == 'l')
	continue;

      if ((fp = fopen (argv[i], "r")) == NULL)
	{
	  sprintf (buf, "collect `%s'", argv[i]);
	  perror (buf);
	  exit (-1);
	}

      switch (code = process (fp, argv[i]))
	{
	case OK:
	  break;

	case BAD_MAGIC:
	  fprintf (stderr, "file `%s' has a bad magic number for collect\n",
		   argv[i]);
	  exit (-1);

	case NO_NAMELIST:
	  fprintf (stderr, "file `%s' has a no namelist for collect\n",
		   argv[i]);
	  exit (-1);

	case RANDOM_ERROR:
	  fprintf (stderr, "random error while processing file `%s':\n",
		   argv[i]);
	  perror ("collect");
	  exit (-1);

	case FOPEN_ERROR:
	  fprintf (stderr, "fopen(3S) error while processing file `%s' in collect\n",
		   argv[i]);
	  exit (-1);

	case FREAD_ERROR:
	  fprintf (stderr, "fread(3S) error while processing file `%s' in collect\n",
		   argv[i]);
	  exit (-1);

	case FWRITE_ERROR:
	  fprintf (stderr, "fwrite(3S) error while processing file `%s' in collect\n",
		   argv[i]);
	  exit (-1);

	default:
	  abort ();
	}

      fclose (fp);

    }

  switch (code = output_ctor_dtor_table ())
    {
    case OK:
      fclose (outfile);
      exit (0);
    case FREAD_ERROR:
      perror ("fread(3S) failed in collect, at end");
      break;
    case FWRITE_ERROR:
      perror ("fwrite(3S) failed in collect, at end");
      break;
    case FOPEN_ERROR:
      perror ("fopen(3S) failed in collect, at end");
      break;
    case RANDOM_ERROR:
      fprintf (stderr, "random error in collect, at end");
      break;
    }
  exit (-1);
}

void
  add_ctor_dtor_elem(symbol_name)
char *symbol_name;
{
  /*
   *	In EXTENDED_COFF systems, it's possible to
   *	have multiple occurances of symbols ( or so it
   *	appears) in the symbol table. Sooo, we scan to
   *	eliminate duplicate entries. This can never hurt,
   *	and helps EXTENDED_COFF.
   */
  int exists;
  int is_ctor = (symbol_name[CTOR_DTOR_MARKER_LENGTH] == 'I');
  
  struct ctor_dtor_list_elem *p
    = ((is_ctor) ? ctor_chain : dtor_chain);
  
  exists = 0;
  while (p) {
    if (strcmp( symbol_name, p -> name ) == 0 ) {
      exists = 1;
      break;
    }
    p = p -> next;
  }
  if ( ! exists ) {
    
    struct ctor_dtor_list_elem *new = (struct ctor_dtor_list_elem *)
      xmalloc(sizeof(struct ctor_dtor_list_elem));
    
    new->name = (char *)xmalloc (strlen (symbol_name)
				 + CTOR_DTOR_MARKER_OFFSET + 2);
    
    strcpy(new -> name, symbol_name + CTOR_DTOR_MARKER_OFFSET);
    
    if ( is_ctor ) {
      new -> next = ctor_chain;
      ctor_chain = new;
      ctor_chain_length++;
    }
    else {
      new->next = dtor_chain;
      dtor_chain = new;
      dtor_chain_length++;
    }
  }
}


enum error_code
output_ctor_dtor_table ()
{
  int dtor_offset;

  /* Write out the CTOR tabel */
  ASM_FILE_START(outfile);

#ifdef EXTENDED_COFF
  fprintf (outfile, "%s\n", SDATA_SECTION_ASM_OP);
#else
  fprintf (outfile, "%s\n", DATA_SECTION_ASM_OP);
#endif
  ASM_GLOBALIZE_LABEL (outfile, CTOR_TABLE_NAME);
  ASM_OUTPUT_LABEL (outfile, CTOR_TABLE_NAME);
  ASM_OUTPUT_INT_CONST(outfile,ctor_chain_length);
  while ( ctor_chain ) {
    ASM_OUTPUT_LABELREF_AS_INT (outfile, ctor_chain->name);
    ctor_chain = ctor_chain -> next;
  }
  /* NULL-terminate the list of constructors.  -- not needed, but keep it */
  ASM_OUTPUT_INT_CONST (outfile, 0);

  /*
   * Now, lay out the destructors
   */

#ifdef EXTENDED_COFF
  fprintf (outfile, "%s\n", SDATA_SECTION_ASM_OP);
#else
  fprintf (outfile, "%s\n", DATA_SECTION_ASM_OP);
#endif
  ASM_GLOBALIZE_LABEL (outfile, DTOR_TABLE_NAME);
  ASM_OUTPUT_LABEL (outfile, DTOR_TABLE_NAME);
  ASM_OUTPUT_INT_CONST(outfile,dtor_chain_length);
  while (dtor_chain)
    {
      ASM_OUTPUT_LABELREF_AS_INT (outfile, dtor_chain->name);
      dtor_chain = dtor_chain->next;
    }
  ASM_OUTPUT_INT_CONST (outfile, 0);
  ASM_OUTPUT_INT_CONST (outfile, 0);

  fclose (outfile);
  return OK;
}


/*****************************************************************************
 *	
 *		COFF & EXTENDED COFF
 *	
 ****************************************************************************/
#if defined(COFF) || defined(EXTENDED_COFF)

#include <ldfcn.h>

#if defined(EXTENDED_COFF)
#   define GCC_SYMBOLS(X) (SYMHEADER(X).isymMax+SYMHEADER(X).iextMax)
#   define GCC_SYMENT SYMR
#   define GCC_OK_SYMBOL(X) ((X).st == stProc &&  (X).sc == scText)
#   define GCC_SYMINC(X) (1)
#else
#   define GCC_SYMBOLS(X) (HEADER(ldptr).f_nsyms)
#   define GCC_SYMENT SYMENT
#ifndef UNUSUAL_COFF_DEFINITION
#   define GCC_OK_SYMBOL(X) (!(((X).n_type & N_TMASK) != (DT_NON << N_BTSHFT)))
#else
#   define GCC_OK_SYMBOL(X) ((X).n_scnum == 1 && (X).n_sclass == C_EXT)
#endif
#   define GCC_SYMINC(X) ((X).n_numaux+1)
#endif

enum error_code
  process (fp, filename)
FILE *fp;
char *filename;
{
  LDFILE *ldptr = NULL;
  do {
    if ((ldptr = ldopen(filename, ldptr)) != NULL ) {
      
      if (!ISCOFF( HEADER(ldptr).f_magic ) ) {
	return BAD_MAGIC;
      }
      else {
	
	int symbols = GCC_SYMBOLS(ldptr);
	int symindex;
	
	for (symindex = 0;
	     symindex < symbols;
	     ) {
	  
	  GCC_SYMENT symbol;
	  
	  char *symbol_name;
	  extern char *ldgetname();
	  int returnCode;
	  
	  returnCode = ldtbread(ldptr, symindex, &symbol);
	  
	  if ( returnCode <= 0 ) {
	    break;
	  }
	  
	  symindex += GCC_SYMINC(symbol);
	  
	  if (! GCC_OK_SYMBOL(symbol)) continue;
	  symbol_name = ldgetname(ldptr, &symbol);
	  
	  /* Check to see if we have a CTOR/DTOR marker  */

	  if (! strncmp (CTOR_DTOR_MARKER_NAME, symbol_name,
			 CTOR_DTOR_MARKER_LENGTH))
	    add_ctor_dtor_elem(symbol_name);
	}
      }
    }
    else {
      return( RANDOM_ERROR );
    }
  } while ( ldclose(ldptr) == FAILURE ) ;
  return ( OK );
}

/****** taken from sdbout.c ******/


/* Tell the assembler the source file name.
   On systems that use SDB, this is done whether or not -g,
   so it is called by ASM_FILE_START.

   ASM_FILE is the assembler code output file,
   INPUT_NAME is the name of the main input file.  */

/* void */
sdbout_filename (asm_file, input_name)
     FILE *asm_file;
     char *input_name;
{
  int len = strlen (input_name);
  char *na = input_name + len;

  /* NA gets INPUT_NAME sans directory names.  */
  while (na > input_name)
    {
      if (na[-1] == '/')
	break;
      na--;
    }

  ASM_OUTPUT_SOURCE_FILENAME (asm_file, na);
}

#else

/*****************************************************************************
 *	
 *		BSD SYMBOL TABLES
 *	
 *****************************************************************************/

/* Figure out the type of file we need to process.
   Currently, only .o and .a formats are acceptable.  */
enum error_code
process (fp, filename)
     FILE *fp;
     char *filename;
{
  struct stat file_stat;
  union
    {
      char ar_form[SARMAG];
      struct exec a_out_form;
    } header;
  int size;

  if (fstat (fp->_file, &file_stat))
    return RANDOM_ERROR;

  size = file_stat.st_size;

  if (fread (header.ar_form, SARMAG, 1, fp) < 1)
    return RANDOM_ERROR;

  if (strncmp (ARMAG, header.ar_form, SARMAG))
    {
      fseek (fp, 0, 0);
      if (fread (&header.a_out_form, sizeof (struct exec), 1, fp) < 1)
	return RANDOM_ERROR;

      if (N_BADMAG (header.a_out_form))
	return BAD_MAGIC;

      return process_o (fp, &header.a_out_form, size);
    }
  return process_a (fp);
}

enum error_code
process_o (fp, header, size)
     FILE *fp;
     struct exec *header;
     int size;
{
  int symoff, symend;
#ifndef hp9000s300
  struct nlist *nelem, *nelems, *nend;
  char *strtab;
#else
  struct nlist_ *nelem, *nelems, *nend;
#endif /* hp9000s300 */

  if (N_BADMAG (*header))
    return BAD_MAGIC;

#ifndef hp9000s300
  symoff = N_SYMOFF (*header);
  symend = N_STROFF (*header);
#else
  symoff = LESYM_OFFSET (*header);
  symend = DNTT_OFFSET (*header);
#endif /* hp9000s300 */
  if (symoff == symend)
    return NO_NAMELIST;
  fseek (fp, symoff - sizeof (struct exec), 1);
#ifndef hp9000s300
  nelems = (struct nlist *)alloca (symend - symoff);
#else
  nelems = (struct nlist_ *)alloca (symend - symoff);
#endif /* hp9000s300 */
  if (fread (nelems, sizeof (char), symend - symoff, fp) < symend - symoff)
    return FREAD_ERROR;

#ifndef hp9000s300
  strtab = (char *)alloca ((char *)size - (char *)symend);
  if (fread (strtab, sizeof (char), (char *)size - (char *)symend, fp)
      < ((char *)size - (char *)symend) * sizeof (char))
    return FREAD_ERROR;

  nend = (struct nlist *)((char *)nelems + symend - symoff);
  for (nelem = nelems; nelem < nend; nelem++)
#else
  nend = (struct nlist_ *)((char *)nelems + symend - symoff);
  for (nelem = nelems; nelem < nend; )
#endif /* hp9000s300 */
    {
#ifndef hp9000s300
      int strindex = nelem->n_un.n_strx;
#else
      int symlen = nelem->n_length;
      char p[255];
      memcpy(p, (char *) (++nelem), symlen);
      p[symlen]='\0';
     
      /* printf("'%s'\n",p);   */
#endif /* hp9000s300 */

#ifndef hp9000s300
      if (strindex)
#else
      nelem   = (struct nlist_ *)((char *)nelem + symlen);
      
      if (symlen)
#endif /* hp9000s300 */
	{
#ifndef hp9000s300
	  char *p = strtab+strindex;
#endif /* hp9000s300 */

	  if (! strncmp ("__GLOBAL_$", p, 10))
	    add_ctor_dtor_elem(p);
	}
    }
  return OK;
}

enum error_code
process_a (fp)
     FILE *fp;
{
  struct ar_hdr header;
  struct exec exec_header;
  int size;
  enum error_code code;

  while (! feof (fp))
    {
      char c;
#ifdef hp9000s300
      int curpos;
#endif /* hp9000s300 */

      if (fread (&header, sizeof (struct ar_hdr), 1, fp) < 1)
	return RANDOM_ERROR;

      size = atoi (header.ar_size);
#ifdef hp9000s300
      curpos = ftell(fp);
#endif /* hp9000s300 */

#ifndef hp9000s300
      if (fread (&exec_header, sizeof (struct exec), 1, fp) < 1)
	return RANDOM_ERROR;
#else
      /* if the name starts with /, it's an index file */
      if (header.ar_name[0] != '/') {
      
        if (fread (&exec_header, sizeof (struct exec), 1, fp) < 1)
	  return RANDOM_ERROR;
#endif /* hp9000s300 */

      code = process_o (fp, &exec_header, size);
      if (code != OK)
	return code;
#ifdef hp9000s300
      } 
      
      if (fseek(fp,(long) curpos + size,0))
	return RANDOM_ERROR;
#endif /* hp9000s300 */
      if ((c = getc (fp)) == '\n')
	;
      else
	ungetc (c, fp);
      c = getc (fp);
      if (c != EOF)
	ungetc (c, fp);
    }
  return OK;
}
#endif

/* Output to FILE a reference to the assembler name of a C-level name NAME.
   If NAME starts with a *, the rest of NAME is output verbatim.
   Otherwise NAME is transformed in an implementation-defined way
   (usually by the addition of an underscore).
   Many macros in the tm file are defined to call this function.

   Swiped from `varasm.c'  */

void
assemble_name (file, name)
     FILE *file;
     char *name;
{
  if (name[0] == '*')
    fputs (&name[1], file);
  else
#ifdef NO_UNDERSCORES
    fprintf (file, "%s", name);
#else
    fprintf (file, "_%s", name);
#endif
}

int
xmalloc (size)
     int size;
{
  int result = malloc (size);
  if (! result)
    {
      fprintf (stderr, "Virtual memory exhausted\n");
      exit (-1);
    }
  return result;
}
#endif
