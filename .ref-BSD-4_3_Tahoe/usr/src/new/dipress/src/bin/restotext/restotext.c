/* restotext.c
 *
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 * Module: restotext
 * Owner: knox
 * stdout: text description
 * args:
 *   name         (name of the input res file)
 * 
 * Description:
 *    This program reads an RES file and writes out a line-by-line
 *    text description of the file.  The name of the RES file is the
 *    the first argument of the command line.  A text description
 *    of the image is written to the standard output.  The commands
 *    in the RES file are translated, but the RES file is not executed.
 * 
 *    The image raster will be read from the file "name.res",
 *    where name is read from the command line.  The ".res"
 *    extension will not be added if it is already present in the
 *    name.
 */

#ifndef  MC500
#define strrchr(x,y) rindex(x,y)
#endif

#include <stdio.h>

FILE *fpin;
extern int verbose;

main(argc, argv)
  int argc;
  char **argv;
  {
  int n;
  char *filename;
  n = 1;
  verbose = 0;
  if (argc > 1)
    {
    if (strcmp(argv[1], "-v") == 0) verbose = 1;
    if (strcmp(argv[1], "-V") == 0) verbose = 1;
    }
  if (verbose) n++;
  if ((argc-1) < n) { printf("restotext: No RES file name!\n"); exit(2); }
  filename = (char *) malloc(strlen(argv[n])+1+strlen(".res"));
  strcpy(filename, argv[n]);
  if (strcmp(".res", strrchr(filename, '.')) != 0) strcat(filename, ".res");
  fpin = fopen(filename, "r");
  if (fpin == NULL) { fprintf("restotext: Could not open %s!\n", filename); exit(2); }
  free(filename);
  parse(fpin);
  }


/* Change Log
 *
 * K. Knox,   28-Mar-85 15:04:13, Created first version.
 *
 *
 *
 */



