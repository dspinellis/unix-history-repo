#include <stdio.h>

/* Break string in two parts to avoid buggy C compilers that ignore characters
   after nulls in strings.  */

char string1[] = "Testing distribution of nonprinting chars:\n\
Should be 0177: \177 Should be 0377: \377 Should be 0212: \212.\n\
Should be 0000: ";

char string2[] = ".\n\
This file is read by the `test-distribution' program.\n\
If you change it, you will make that program fail.\n";

char buf[300];

main ()
{
  int fd = open ("testfile", 0);

  if (fd < 0)
    {
      perror ("opening `testfile'");
      exit (1);
    }
  if (read (fd, buf, sizeof string1) != sizeof string1 ||
      strcmp (buf, string1) ||
      read (fd, buf, sizeof string2) != sizeof string2 - 1 ||
      strncmp (buf, string2, sizeof string2 - 1))
    {
      fprintf (stderr, "Data in file `testfile' has been damaged.\n\
Most likely this means that many nonprinting characters\n\
have been corrupted in the files of Emacs, and it will not work.\n");
      exit (1);
    }
  close (fd);
  exit (0);
}
