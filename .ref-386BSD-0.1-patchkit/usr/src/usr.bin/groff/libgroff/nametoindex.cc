// -*- C++ -*-
/* Copyright (C) 1989, 1990 Free Software Foundation, Inc.
     Written by James Clark (jjc@jclark.uucp)

This file is part of groff.

groff is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 1, or (at your option) any later
version.

groff is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with groff; see the file LICENSE.  If not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <stdlib.h>
#include "errarg.h"
#include "error.h"
#include "cset.h"
#include "font.h"

#define FIRST_NUMBERED_CHARACTER 256
#define FIRST_NAMED_CHARACTER 512

int font::name_to_index(const char *s)
{
  static char **table;
  static int table_used;
  static int table_size;
  assert(s != 0 && s[0] != '\0');
  if (s[1] == '\0')
    return (unsigned char)s[0];
  /* char128 and \200 are synonyms */
  if (s[0] == 'c' && s[1] == 'h' && s[2] == 'a' && s[3] == 'r') {
    char *res;
    long n = strtol(s + 4, &res, 10);
    if (res != s + 4 && *res == '\0' && n >= 0 && n < 256)
      return int(n);
  }
  for (int i = 0; i < table_used; i++)
    if (strcmp(table[i], s) == 0)
      return i + FIRST_NAMED_CHARACTER;
  if (table_used >= table_size) {
    if (table_size == 0) {
      table_size = 24;
      table = new char*[table_size];
    }
    else {
      char **old_table = table;
      table = new char *[table_size*2];
      memcpy(table, old_table, table_size*sizeof(char*));
      table_size *= 2;
      delete old_table;
    }
  }
  table[table_used] = new char[strlen(s) + 1];
  strcpy(table[table_used], s);
  return table_used++ + FIRST_NAMED_CHARACTER;
}

int font::number_to_index(unsigned char n)
{
  return n + FIRST_NUMBERED_CHARACTER;
}
