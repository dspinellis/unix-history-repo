/*
 * Copyright (c) 1984, 1985 Xerox Corp.
 *
 * Print out an error string and then die.
 *
 */

#include <stdio.h>

error(string, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
  char *string;
  int a1, a2, a3, a4, a5, a6, a7, a8, a9, a10;
  {
  fprintf(stderr, string, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
  exit(2);
  }
