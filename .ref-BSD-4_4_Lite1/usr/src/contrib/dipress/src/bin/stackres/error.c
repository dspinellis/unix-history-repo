/*
 * (c) Copyright 1984, 1985, 1986 Xerox Corporation
 *
 * Print out an error string and then die.
 *
 */

#include <stdio.h>

/*VARARGS*/
error(string, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
  char *string;
  int a1, a2, a3, a4, a5, a6, a7, a8, a9, a10;
  {
  fprintf(stderr, string, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
  exit(2);
  }
