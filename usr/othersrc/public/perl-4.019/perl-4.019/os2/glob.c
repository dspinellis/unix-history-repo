/*
 * Globbing for OS/2.  Relies on the expansion done by the library
 * startup code. (dds)
 */

#include <stdio.h>
#include <string.h>

main(int argc, char *argv[])
{
  register i;

  for (i = 1; i < argc; i++)
  {
    fputs(IsFileSystemFAT(argv[i]) ? strlwr(argv[i]) : argv[i], stdout);
    putchar(0);
  }
}
