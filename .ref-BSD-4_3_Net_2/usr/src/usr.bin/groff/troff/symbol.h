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

#define DONT_STORE 1
#define MUST_ALREADY_EXIST 2

class symbol {
  static const char **table;
  static int table_used;
  static int table_size;
  static char *block;
  static int block_size;
  const char *s;
public:
  symbol(const char *p, int how = 0);
  symbol();
  int hash() const;
  operator ==(symbol) const;
  operator !=(symbol) const;
  const char *contents() const;
  int is_null() const;
};


extern const symbol NULL_SYMBOL;

inline symbol::symbol() : s(0)
{
}

inline int symbol::operator==(symbol p) const
{
  return s == p.s;
}

inline int symbol::operator!=(symbol p) const
{
  return s != p.s;
}

// guaranteed to return a positive integer

inline int symbol::hash() const
{
  int i = int(s);
  return i < 0 ? -i : i;
}

inline const char *symbol::contents() const
{
  return s;
}

inline int symbol::is_null() const
{
  return s == 0;
}

symbol concat(symbol, symbol);
