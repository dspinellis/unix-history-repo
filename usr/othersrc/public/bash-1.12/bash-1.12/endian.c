/* endian.c -- A trick for determining the byte order of a machine. */
#include <stdio.h>

/* The name of this program, as taken from argv[0]. */
char *progname;

/* The name of the source file that this code is made from. */
char source_name[256];

/* The name of the define.  Either "BIG_ENDIAN" or "LITTLE_ENDIAN". */
char *endian_define;

/* Stuffed value of "ABCD" in a long, followed by a 0. */
long int str[2] = { 0x41424344, 0x0 };

/* Stuff "ABCD" into a long, and compare it against a character string
   "ABCD".  If the results are EQ, the machine is big endian like a 68000
   or Sparc, otherwise it is little endian, like a Vax, or 386. */
main (argc, argv)
     int argc;
     char **argv;
{
  register int i;
  FILE *stream = (FILE *)NULL;
  char *stream_name = "stdout";

  progname = argv[0];

  for (i = strlen (progname); i > 0; i--)
    if (progname[i] == '/')
      {
	progname = progname + i + 1;
	break;
      }

  strcpy (source_name, progname);
  for (i = strlen (source_name); i > 0; i--)
    if (source_name[i] == '.')
      {
	source_name[i] = '\0';
	break;
      }

  strcat (source_name, ".c");

  if (argc == 1)
    {
      stream_name = "stdout";
      stream = stdout;
    }
  else if (argc == 2)
    {
      stream_name = argv[1];
      stream = fopen (stream_name, "w");
    }
  else
    {
      fprintf (stderr, "Usage: %s [output-file]\n", progname);
      exit (1);
    }

  if (!stream)
    {
      fprintf (stderr, "%s: %s Cannot be opened or written to.\n",
	       progname, stream_name);
      exit (2);
    }

  if (strcmp ((char *)&str[0], "ABCD") == 0)
    endian_define = "BIG_ENDIAN";
  else
    endian_define = "LITTLE_ENDIAN";

  fprintf (stream, "/* %s - Define BIG or LITTLE endian. */\n\n", stream_name);
  fprintf (stream,
"/* This file was automatically created by `%s'.  You shouldn't\n\
   edit this file, because your changes will be overwritten.  Instead,\n\
   edit the source code file `%s'. */\n\n",
	   progname, source_name);

  fprintf (stream, "#if !defined (%s)\n", endian_define);
  fprintf (stream, "#  define %s\n", endian_define);
  fprintf (stream, "#endif /* %s */\n", endian_define);

  if (stream != stdout)
    fclose (stream);

  exit (0);
}
