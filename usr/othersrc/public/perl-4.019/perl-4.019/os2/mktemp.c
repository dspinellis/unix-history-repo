/* MKTEMP.C using TMP environment variable */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <io.h>

void Mktemp(char *file)
{
  char fname[32], *tmp;

  tmp = getenv("TMP");

  if ( tmp != NULL )
  {
    strcpy(fname, file);
    strcpy(file, tmp);

    if ( file[strlen(file) - 1] != '\\' )
      strcat(file, "\\");

    strcat(file, fname);
  }

  mktemp(file);
}

/* End of MKTEMP.C */
