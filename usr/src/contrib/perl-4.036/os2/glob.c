/*
 * Globbing for OS/2.  Relies on the expansion done by the library
 * startup code.
 */

#define PERLGLOB
#include "director.c"

int main(int argc, char **argv)
{
  SHORT i;
  USHORT r;
  CHAR *f;

  for (i = 1; i < argc; i++)
  {
    f = IsFileSystemFAT(argv[i]) ? strlwr(argv[i]) : argv[i];
    DosWrite(1, f, strlen(f) + 1, &r);
  }
  return argc - 1;
}
