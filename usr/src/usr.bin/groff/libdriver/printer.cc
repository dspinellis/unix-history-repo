// -*- C++ -*-
/* Copyright (C) 1989, 1990, 1991 Free Software Foundation, Inc.
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

#include "driver.h"

printer *pr = 0;

font_pointer_list::font_pointer_list(font *f, font_pointer_list *fp)
: p(f), next(fp)
{
}

printer::printer()
: font_table(0), nfonts(0), font_list(0)
{
}

printer::~printer()
{
  delete font_table;
  while (font_list) {
    font_pointer_list *tem = font_list;
    font_list = font_list->next;
    delete tem->p;
    delete tem;
  }
  if (ferror(stdout) || fflush(stdout) < 0)
    fatal("output error");
}

void printer::load_font(int n, const char *nm)
{
  assert(n >= 0);
  if (n >= nfonts) {
    if (nfonts == 0) {
      nfonts = 10;
      if (nfonts <= n)
	nfonts = n + 1;
      font_table = new font *[nfonts];
      for (int i = 0; i < nfonts; i++)
	font_table[i] = 0;
    }
    else {
      font **old_font_table = font_table;
      int old_nfonts = nfonts;
      nfonts *= 2;
      if (n >= nfonts)
	nfonts = n + 1;
      font_table = new font *[nfonts];
      for (int i = 0; i < old_nfonts; i++)
	font_table[i] = old_font_table[i];
      for (i = old_nfonts; i < nfonts; i++)
	font_table[i] = 0;
    }
  }
  font *f = find_font(nm);
  font_table[n] = f;
}

font *printer::find_font(const char *nm)
{
  for (font_pointer_list *p = font_list; p; p = p->next)
    if (strcmp(p->p->get_name(), nm) == 0)
      return p->p;
  font *f = make_font(nm);
  if (!f)
    fatal("sorry, I can't continue");
  font_list = new font_pointer_list(f, font_list);
  return f;
}

font *printer::make_font(const char *nm)
{
  return font::load_font(nm);
}

void printer::end_of_line()
{
}

void printer::special(char *, const environment *)
{
}

void printer::draw(int, int *, int, const environment *)
{
}

void printer::set_ascii_char(unsigned char c, const environment *env, 
			     int *widthp)
{
  char buf[2];
  buf[0] = c;
  buf[1] = '\0';
  set_special_char(buf, env, widthp);
}

void printer::set_special_char(const char *nm, const environment *env,
				  int *widthp)
{
  int i = font::name_to_index(nm);
  int fn = env->fontno;
  if (fn < 0 || fn >= nfonts) {
    error("bad font position `%1'", fn);
    return;
  }
  font *f = font_table[fn];
  if (f == 0) {
    error("no font mounted at `%1'", fn);
    return;
  }
  if (!f->contains(i)) {
    if (nm[0] != '\0' && nm[1] == '\0')
      error("font `%1' does not contain ascii character `%2'",
	    f->get_name(),
	    nm[0]);
    else
      error("font `%1' does not contain special character `%2'",
	    f->get_name(),
	    nm);
    return;
  }
  int w = f->get_width(i, env->size);
  if (widthp)
    *widthp = w;
  set_char(i, f, env, w);
}

void printer::set_numbered_char(int num, const environment *env, int *widthp)
{
  if (num < 0 || num >= 256) {
    error("argument to N command not between 0 and 255");
    return;
  }
  int i = font::number_to_index((unsigned char)num);
  int fn = env->fontno;
  if (fn < 0 || fn >= nfonts) {
    error("bad font position `%1'", fn);
    return;
  }
  font *f = font_table[fn];
  if (f == 0) {
    error("no font mounted at `%1'", fn);
    return;
  }
  if (!f->contains(i)) {
    error("font `%1' does not contain numbered character %2",
	  f->get_name(),
	  num);
    return;
  }
  int w = f->get_width(i, env->size);
  if (widthp)
    *widthp = w;
  set_char(i, f, env, w);
}
