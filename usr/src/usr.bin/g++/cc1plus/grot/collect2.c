/* Build tables of static constructors and destructors and run ld. */

#include <stdio.h>

#ifdef convex

#define TEXT_SECTION_ASM_OP ".text"
#define DATA_SECTION_ASM_OP ".data"

#define ASM_GLOBALIZE_LABEL(FILE, LABEL) \
  fprintf (FILE, ".globl _%s\n", LABEL)

#define ASM_OUTPUT_LABEL(FILE, LABEL) \
  fprintf (FILE, "_%s:", LABEL)

#define ASM_OUTPUT_LABELREF(FILE, LABEL) \
  fprintf (FILE, "\tds.w _%s\n", LABEL)

#define ASM_OUTPUT_INT(FILE, INT) \
  fprintf (FILE, "\tds.w %d\n", INT)

#endif

#ifdef MASSCOMP

#define TEXT_SECTION_ASM_OP ".text"
#define DATA_SECTION_ASM_OP ".data"

#define ASM_GLOBALIZE_LABEL(FILE, LABEL) \
  fprintf (FILE, ".globl _%s\n", LABEL)

#define ASM_OUTPUT_LABEL(FILE, LABEL) \
  fprintf (FILE, "_%s:\n", LABEL)

#define ASM_OUTPUT_LABELREF(FILE, LABEL) \
  fprintf (FILE, "\t.long _%s\n", LABEL)

#define ASM_OUTPUT_INT(FILE, INT) \
  fprintf (FILE, "\t.long %d\n", INT)

#endif

#if defined (__GNUC__) || defined (sparc)
#define alloca __builtin_alloca
#endif

extern char *mktemp (char *template);

/* Linked lists of constructor and destructor names. */

struct id 
{
  char *name;
  struct id *next;
};

/* Main program. */

main (int argc, char *argv[])
{
  static char codexxx[] = "/tmp/codeXXXXXX";
  static char asxxx[] = "/tmp/asXXXXXX";
  char *codefile, *hookfile, hooksfile[40], hookofile[40];
  char *outfile = "a.out";
  char *arg, ldargs[1024], cmd[1024];
  FILE *inf, *outf;

  /* Make temp file names. */

  codefile = mktemp (codexxx);
  hookfile = mktemp (asxxx);
  sprintf (hooksfile, "%s.s", hookfile);
  sprintf (hookofile, "%s.o", hookfile);

  /* Parse arguments.  Remove output file spec, pass the rest to ld. */

  ldargs[0] = '\0';
  while (arg = *++argv)
    {
      if (! strcmp (arg, "-o"))
	outfile = *++argv;
      else
	{
#ifdef masscomp
          if ( *arg == '-' && *(arg+1) == 'L' )
	    {
	      strcat (ldargs, " -L ");
	      strcat (ldargs, arg+2);
	    }
	  else
	    {
	      strcat (ldargs, " ");
	      strcat (ldargs, arg);
	    }
#else
	  strcat (ldargs, " ");
	  strcat (ldargs, arg);
#endif
	}
    }

  /* Load the program, searching all libraries.
     Use -r to save the output as a relocatable file.
     Examine the namelist with nm and search it for static constructors
     and destructors to call.
     Write the constructor and destructor tables to a .s file. */

#ifdef masscomp
/*
  sprintf (cmd, "ld -r -o %s %s", codefile, ldargs);
  if (system(cmd))
     fatal( "##F-ld: Cannot perform link." );
  sprintf (cmd, "nm -p %s", codefile);
*/
  sprintf (cmd, "ld -r -o %s %s && nm -p %s", codefile, ldargs, codefile);
#else
  sprintf (cmd, "ld -r -o %s %s && nm -p %s", codefile, ldargs, codefile);
#endif

  if (! (inf = popen (cmd, "r")))
    fatal_perror ("Can't open pipe to ld\n");
  if (! (outf = fopen (hooksfile, "w")))
    fatal_perror ("Can't write %s\n", hooksfile);

  write_hooks (inf, outf);

  if (pclose (inf) || fclose (outf))
    fatal ("load failed");

  /* Assemble the constructor and destructor tables.
     Link the tables in with the rest of the program. */

  sprintf (cmd, "as -o %s %s && ld -o %s %s %s && rm %s %s %s",
	   hookofile, hooksfile, 
	   outfile, codefile, hookofile,
	   codefile, hooksfile, hookofile);
  exit (system (cmd));
}

/* Scan the name list of the loaded program for the symbols g++ uses
   for static constructors and destructors.  Write their addresses
   into tables which __main and exit will call.

   The constructor table __CTOR_LIST__ is an integer count followed by
   that many pointers to constructor functions.  The destructor table
   __DTOR_LIST__ is the same thing, followed by a zero word. */

write_hooks (FILE *inf, FILE *outf)
{
  char *p, buf[1024];
  struct id *newid;

  struct id *constructors = 0;
  struct id *destructors = 0;

  while (! feof (inf)) {

    /* Read a line of nm output and strip the trailing newline. */

    fgets (buf, sizeof buf, inf);
    p = buf + strlen (buf) - 1;
    if (*p == '\n')
      *p = '\0';

#ifdef masscomp
    /* If loader spits out an error/warning message then
    ** lets display it.
    */
    if (buf[0] == '#' && buf[1] == '#')
      {
	fprintf( stderr, "%s\n", buf );
	buf[0] = '\0';
      }
#endif

    /* If it contains a constructor or destructor name, add the name
       to the appropriate list. */

    for (p = buf; *p; p++)
      {
	while (*p && *p != '_')
	  p++;
	if (! strncmp (p, "_GLOBAL_$I$", 11))
	  {
	    newid = alloca (sizeof *newid);
	    newid->name = alloca (strlen (p) + 1);
	    strcpy (newid->name, p);
	    newid->next = constructors;
	    constructors = newid;
	    break;
	  }
	else if (! strncmp (p, "_GLOBAL_$D$", 11))
	  {
	    newid = alloca (sizeof *newid);
	    newid->name = alloca (strlen (p) + 1);
	    strcpy (newid->name, p);
	    newid->next = destructors;
	    destructors = newid;
	    break;
	  }
      }
  }

  /* Write the tables. */

  fprintf (outf, "%s\n", TEXT_SECTION_ASM_OP);
  ASM_GLOBALIZE_LABEL (outf, "__CTOR_LIST__");
  ASM_OUTPUT_LABEL (outf, "__CTOR_LIST__");
  ASM_OUTPUT_INT (outf, count_list (constructors));
  write_list (outf, constructors);
  
  fprintf (outf, "%s\n", DATA_SECTION_ASM_OP);
  ASM_GLOBALIZE_LABEL (outf, "__DTOR_LIST__");
  ASM_OUTPUT_LABEL (outf, "__DTOR_LIST__");
  ASM_OUTPUT_INT (outf, count_list (destructors));
  write_list (outf, destructors);
  ASM_OUTPUT_INT (outf, 0);
}
 
/* Length of list LIST. */

count_list (struct id *list)
{
  int count = 0;
  while (list)
    {
      count++;
      list = list->next;
    }
  return count;
}

/* Write the names on list LIST, in reverse order. */

write_list (FILE *outf, struct id *list)
{
  if (! list)
    return;
  write_list (outf, list->next);
  ASM_OUTPUT_LABELREF (outf, list->name);
}

/* Die when sys call fails. */

fatal_perror (string, arg)
{
  char buf[80];
  sprintf (buf, string, arg);
  perror (buf);
  exit (1);
}

/* Just die. */

fatal (string)
{
  fprintf (stderr, "\n");
  fprintf (stderr, string);
  fprintf (stderr, "\n");
  exit (1);
}
