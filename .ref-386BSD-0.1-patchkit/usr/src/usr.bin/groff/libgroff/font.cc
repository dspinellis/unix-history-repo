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

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include "errarg.h"
#include "error.h"
#include "cset.h"
#include "font.h"
#include "lib.h"

const char *const WS = " \t\n\r";

struct font_char_metric {
  char type;
  unsigned char code;
  int width;
  int height;
  int depth;
  int pre_math_space;
  int italic_correction;
  int subscript_correction;
};

struct font_kern_list {
  int i1;
  int i2;
  int amount;
  font_kern_list *next;

  font_kern_list(int, int, int, font_kern_list * = 0);
};

/* text_file */

struct text_file {
  FILE *fp;
  char *path;
  int lineno;
  int size;
  int skip_comments;
  char *buf;
  text_file(FILE *fp, char *p);
  ~text_file();
  int next();
  void error(const char *format, 
	     const errarg &arg1 = empty_errarg,
	     const errarg &arg2 = empty_errarg,
	     const errarg &arg3 = empty_errarg);
};

text_file::text_file(FILE *p, char *s) 
: lineno(0), buf(0), size(0), skip_comments(1), fp(p), path(s)
{
}

text_file::~text_file()
{
  delete buf;
  delete path;
  if (fp)
    fclose(fp);
}


int text_file::next()
{
  if (fp == 0)
    return 0;
  if (buf == 0) {
    buf = new char [512];
    size = 512;
  }
  for (;;) {
    if (fgets(buf, size, fp) == 0)
      return 0;
    lineno++;
    char *ptr = buf;
    while (csspace(*ptr))
      ptr++;
    if (*ptr != 0 && (!skip_comments || *ptr != '#'))
      return 1;
  }
}

void text_file::error(const char *format, 
		      const errarg &arg1,
		      const errarg &arg2,
		      const errarg &arg3)
{
  error_with_file_and_line(path, lineno, format, arg1, arg2, arg3);
}


/* font functions */

font::font(const char *s)
: special(0), ligatures(0), kern_hash_table(0), space_width(0),
  ch(0), ch_used(0), ch_size(0), ch_index(0), nindices(0)
{
  name = new char[strlen(s) + 1];
  strcpy(name, s);
  internalname = 0;
  slant = 0.0;
  // load();			// for testing
}

font::~font()
{
  delete ch;
  delete ch_index;
  if (kern_hash_table) {
    for (int i = 0; i < KERN_HASH_TABLE_SIZE; i++) {
      font_kern_list *kerns = kern_hash_table[i];
      while (kerns) {
	font_kern_list *tem = kerns;
	kerns = kerns->next;
	delete tem;
      }
    }
    delete kern_hash_table;
  }
  delete name;
  delete internalname;
}

static int scale_round(int n, int x, int y)
{
  assert(x >= 0 && y > 0);
  int y2 = y/2;
  if (x == 0)
    return 0;
  if (n >= 0) {
    if (n <= (INT_MAX - y2)/x)
      return (n*x + y2)/y;
  }
  else {
    if (-(unsigned)n <= (-(unsigned)INT_MIN - y2)/x)
      return (n*x - y2)/y;
  }
  return int(n*double(x)/double(y) + .5);
}

inline int font::scale(int w, int sz)
{
  return sz == unitwidth ? w : scale_round(w, sz, unitwidth);
}

int font::get_skew(int c, int point_size, int sl)
{
  int h = get_height(c, point_size);
  return int(h*tan((slant+sl)*M_PI/180.0) + .5);
}

int font::contains(int c)
{
  return c >= 0 && c < nindices && ch_index[c] >= 0;
}

int font::is_special()
{
  return special;
}

int font::get_width(int c, int point_size)
{
  assert(c >= 0 && c < nindices && ch_index[c] >= 0);
  return scale(ch[ch_index[c]].width, point_size);
}

int font::get_height(int c, int point_size)
{
  assert(c >= 0 && c < nindices && ch_index[c] >= 0);
  return scale(ch[ch_index[c]].height, point_size);
}

int font::get_depth(int c, int point_size)
{
  assert(c >= 0 && c < nindices && ch_index[c] >= 0);
  return scale(ch[ch_index[c]].depth, point_size);
}

int font::get_italic_correction(int c, int point_size)
{
  assert(c >= 0 && c < nindices && ch_index[c] >= 0);
  return scale(ch[ch_index[c]].italic_correction, point_size);
}

int font::get_left_italic_correction(int c, int point_size)
{
  assert(c >= 0 && c < nindices && ch_index[c] >= 0);
  return scale(ch[ch_index[c]].pre_math_space, point_size);
}

int font::get_subscript_correction(int c, int point_size)
{
  assert(c >= 0 && c < nindices && ch_index[c] >= 0);
  return scale(ch[ch_index[c]].subscript_correction, point_size);
}

int font::get_space_width(int point_size)
{
  return scale(space_width, point_size);
}

font_kern_list::font_kern_list(int c1, int c2, int n, font_kern_list *p)
     : i1(c1), i2(c2), amount(n), next(p)
{
}

inline int font::hash_kern(int i1, int i2)
{
  int n = ((i1 << 10) + i2) % KERN_HASH_TABLE_SIZE;
  return n < 0 ? -n : n;
}

void font::add_kern(int i1, int i2, int amount)
{
  if (!kern_hash_table) {
    kern_hash_table = new font_kern_list *[KERN_HASH_TABLE_SIZE];
    for (int i = 0; i < KERN_HASH_TABLE_SIZE; i++)
      kern_hash_table[i] = 0;
  }
  font_kern_list **p = kern_hash_table + hash_kern(i1, i2);
  *p = new font_kern_list(i1, i2, amount, *p);
}

int font::get_kern(int i1, int i2, int point_size)
{
  if (kern_hash_table) {
    for (font_kern_list *p = kern_hash_table[hash_kern(i1, i2)]; p; p = p->next)
      if (i1 == p->i1 && i2 == p->i2)
	return scale(p->amount, point_size);
  }
  return 0;
}

int font::has_ligature(int mask)
{
  return mask & ligatures;
}

int font::get_character_type(int c)
{
  assert(c >= 0 && c < nindices && ch_index[c] >= 0);
  return ch[ch_index[c]].type;
}

unsigned char font::get_code(int c)
{
  assert(c >= 0 && c < nindices && ch_index[c] >= 0);
  return ch[ch_index[c]].code;
}

const char *font::get_name()
{
  return name;
}

const char *font::get_internal_name()
{
  return internalname;
}

void font::alloc_ch_index(int index)
{
  if (nindices == 0) {
    nindices = 128;
    if (index >= nindices)
      nindices = index + 10;
    ch_index = new short[nindices];
    for (int i = 0; i < nindices; i++)
      ch_index[i] = -1;
  }
  else {
    int old_nindices = nindices;
    nindices *= 2;
    if (index >= nindices)
      nindices = index + 10;
    short *old_ch_index = ch_index;
    ch_index = new short[nindices];
    memcpy(ch_index, old_ch_index, sizeof(short)*old_nindices);
    for (int i = old_nindices; i < nindices; i++)
      ch_index[i] = -1;
    delete old_ch_index;
  }
}

void font::extend_ch()
{
  if (ch == 0)
    ch = new font_char_metric[ch_size = 16];
  else {
    int old_ch_size = ch_size;
    ch_size *= 2;
    font_char_metric *old_ch = ch;
    ch = new font_char_metric[ch_size];
    memcpy(ch, old_ch, old_ch_size*sizeof(font_char_metric));
    delete old_ch;
  }
}

void font::compact()
{
  for (int i = nindices - 1; i >= 0; i--)
    if (ch_index[i] >= 0)
      break;
  i++;
  if (i < nindices) {
    short *old_ch_index = ch_index;
    ch_index = new short[i];
    memcpy(ch_index, old_ch_index, i*sizeof(short));
    delete old_ch_index;
    nindices = i;
  }
  if (ch_used < ch_size) {
    font_char_metric *old_ch = ch;
    ch = new font_char_metric[ch_used];
    memcpy(ch, old_ch, ch_used*sizeof(font_char_metric));
    delete old_ch;
    ch_size = ch_used;
  }
}

void font::add_entry(int index, const font_char_metric &metric)
{
  assert(index >= 0);
  if (index >= nindices)
    alloc_ch_index(index);
  assert(index < nindices);
  if (ch_used + 1 >= ch_size)
    extend_ch();
  assert(ch_used + 1 < ch_size);
  ch_index[index] = ch_used;
  ch[ch_used++] = metric;
}

void font::copy_entry(int new_index, int old_index)
{
  assert(new_index >= 0 && old_index >= 0 && old_index < nindices);
  if (new_index >= nindices)
    alloc_ch_index(new_index);
  ch_index[new_index] = ch_index[old_index];
}

font *font::load_font(const char *s)
{
  font *f = new font(s);
  if (!f->load()) {
    delete f;
    return 0;
  }
  return f;
}

int font::load()
{
  char *path;
  FILE *fp;
  if ((fp = open_file(name, &path)) == NULL) {
    error("can't find font file `%1'", name);
    return 0;
  }
  text_file t(fp, path);
  t.skip_comments = 1;
  char *p;
  for (;;) {
    if (!t.next()) {
      t.error("missing charset command");
      return 0;
    }
    p = strtok(t.buf, WS);
    if (strcmp(p, "name") == 0) {
    }
    else if (strcmp(p, "spacewidth") == 0) {
      p = strtok(0, WS);
      int n;
      if (p == 0 || sscanf(p, "%d", &n) != 1 || n <= 0) {
	t.error("bad argument for spacewidth command");
	return 0;
      }
      space_width = n;
    }
    else if (strcmp(p, "slant") == 0) {
      p = strtok(0, WS);
      double n;
      if (p == 0 || sscanf(p, "%lf", &n) != 1 || n >= 90.0 || n <= -90.0) {
	t.error("bad argument for slant command", p);
	return 0;
      }
      slant = n;
    }
    else if (strcmp(p, "ligatures") == 0) {
      for (;;) {
	p = strtok(0, WS);
	if (p == 0 || strcmp(p, "0") == 0)
	  break;
	if (strcmp(p, "ff") == 0)
	  ligatures |= LIG_ff;
	else if (strcmp(p, "fi") == 0)
	  ligatures |= LIG_fi;
	else if (strcmp(p, "fl") == 0)
	  ligatures |= LIG_fl;
	else if (strcmp(p, "ffi") == 0)
	  ligatures |= LIG_ffi;
	else if (strcmp(p, "ffl") == 0)
	  ligatures |= LIG_ffl;
	else {
	  t.error("unrecognised ligature `%1'", p);
	  return 0;
	}
      }
    }
    else if (strcmp(p, "internalname") == 0) {
      p = strtok(0, WS);
      if (!p) {
	t.error("`internalname command requires argument");
	return 0;
      }
      internalname = new char[strlen(p) + 1];
      strcpy(internalname, p);
    }
    else if (strcmp(p, "special") == 0) {
      special = 1;
    }
    else if (strcmp(p, "kernpairs") != 0 && strcmp(p, "charset") != 0) {
      int nargv = 5;
      const char **argv = new char *[nargv];
      int argc = 0;
      do {
	if (argc < nargv) {
	  const char **old_argv = argv;
	  int old_nargv = nargv;
	  nargv *= 2;
	  argv = new char *[nargv];
	  memcpy(argv, old_argv, sizeof(char*)*old_nargv);
	  delete old_argv;
	}
	argv[argc++] = p;
	p = strtok(0, WS);
      } while (p != 0);
      handle_unknown_font_command(argc, argv);
      delete argv;
    }
    else
      break;
  }
  char *command = p;
  int had_charset = 0;
  t.skip_comments = 0;
  while (command) {
    if (strcmp(command, "kernpairs") == 0) {
      for (;;) {
	if (!t.next()) {
	  command = 0;
	  break;
	}
	char *c1 = strtok(t.buf, WS);
	if (c1 == 0)
	  continue;
	char *c2 = strtok(0, WS);
	if (c2 == 0) {
	  command = c1;
	  break;
	}
	p = strtok(0, WS);
	if (p == 0) {
	  t.error("missing kern amount");
	  return 0;
	}
	int n;
	if (sscanf(p, "%d", &n) != 1) {
	  t.error("bad kern amount `%1'", p);
	  return 0;
	}
	int i1 = name_to_index(c1);
	if (i1 < 0) {
	  t.error("illegal character `%1'", c1);
	  return 0;
	}
	int i2 = name_to_index(c2);
	if (i2 < 0) {
	  t.error("illegal character `%1'", c2);
	  return 0;
	}
	add_kern(i1, i2, n);
      }
    }
    else if (strcmp(command, "charset") == 0) {
      had_charset = 1;
      int last_index = -1;
      for (;;) {
	if (!t.next()) {
	  command = 0;
	  break;
	}
	char *nm = strtok(t.buf, WS);
	if (nm == 0)
	  continue;			// I dont think this should happen
	p = strtok(0, WS);
	if (p == 0) {
	  command = nm;
	  break;
	}
	if (p[0] == '"') {
	  if (last_index == -1) {
	    t.error("first charset entry is duplicate");
	    return 0;
	  }
	  if (strcmp(nm, "---") == 0) {
	    t.error("unnamed character cannot be duplicate");
	    return 0;
	  }
	  int index = name_to_index(nm);
	  if (index < 0) {
	    t.error("illegal character `%1'", nm);
	    return 0;
	  }
	  copy_entry(index, last_index);
	}
	else {
	  font_char_metric metric;
	  metric.height = 0;
	  metric.depth = 0;
	  metric.pre_math_space = 0;
	  metric.italic_correction = 0;
	  metric.subscript_correction = 0;
	  int nparms = sscanf(p, "%d,%d,%d,%d,%d,%d",
			      &metric.width, &metric.height, &metric.depth,
			      &metric.italic_correction,
			      &metric.pre_math_space,
			      &metric.subscript_correction);
	  if (nparms < 1) {
	    t.error("bad width for `%1'", nm);
	    return 0;
	  }
	  p = strtok(0, WS);
	  if (p == 0) {
	    t.error("missing character type for `%1'", nm);
	    return 0;
	  }
	  int type;
	  if (sscanf(p, "%d", &type) != 1) {
	    t.error("bad character type for `%1'", nm);
	    return 0;
	  }
	  if (type < 0 || type > 255) {
	    t.error("character code `%1' out of range", type);
	    return 0;
	  }
	  metric.type = type;
	  p = strtok(0, WS);
	  if (p == 0) {
	    t.error("missing code for `%1'", nm);
	    return 0;
	  }
	  char *ptr;
	  long code = strtol(p, &ptr, 0);
	  if (code == 0 && ptr == p) {
	    t.error("bad code `%1' for character `%2'", p, nm);
	    return 0;
	  }
	  if (code < 0 || code >= 256) {
	    t.error("code %1 for character `%2' not between 0 and 255",
		    int(code), nm);
	    return 0;
	  }
	  metric.code = (unsigned char)code;
	  if (strcmp(nm, "---") == 0) {
	    last_index = number_to_index(metric.code);
	    add_entry(last_index, metric);
	  }
	  else {
	    last_index = name_to_index(nm);
	    if (last_index < 0) {
	      t.error("illegal character `%1'", nm);
	      return 0;
	    }
	    add_entry(last_index, metric);
	    copy_entry(number_to_index(metric.code), last_index);
	  }
	}
      }
      if (last_index == -1) {
	t.error("I didn't seem to find any characters");
	return 0;
      }
    }
    else {
      t.error("unrecognised command `%1' after `kernpairs' or `charset' command", command);
      return 0;
    }
  }
  if (!had_charset) {
    t.error("missing charset command");
    return 0;
  }
  if (space_width == 0)
    space_width = scale_round(unitwidth, res, 72*3*sizescale);
  compact();
  return 1;
}

static struct {
  const char *command;
  int *ptr;
} table[] = {
  "res", &font::res,
  "hor", &font::hor,
  "vert", &font::vert,
  "unitwidth", &font::unitwidth,
  "paperwidth", &font::paperwidth,
  "paperlength", &font::paperlength,
  "spare1", &font::biggestfont,
  "biggestfont", &font::biggestfont,
  "spare2", &font::spare2,
  "sizescale", &font::sizescale
  };


int font::load_desc()
{
  int nfonts = 0;
  FILE *fp;
  char *path;
  if ((fp = open_file("DESC", &path)) == 0) {
    error("can't open `DESC'");
    return 0;
  }
  text_file t(fp, path);
  t.skip_comments = 1;
  res = 0;
  while (t.next()) {
    char *p = strtok(t.buf, WS);
    int found = 0;
    for (int i = 0; !found && i < sizeof(table)/sizeof(table[0]); i++)
      if (strcmp(table[i].command, p) == 0)
	found = 1;
    if (found) {
      char *q = strtok(0, WS);
      if (!q) {
	t.error("missing value for command `%1'", p);
	return 0;
      }
      //int *ptr = &(this->*(table[i-1].ptr));
      int *ptr = table[i-1].ptr;
      if (sscanf(q, "%d", ptr) != 1) {
	t.error("bad number `%1'", q);
	return 0;
      }
    }
    else if (strcmp("tcommand", p) == 0) {
      tcommand = 1;
    }
    else if (strcmp("family", p) == 0) {
      p = strtok(0, WS);
      if (!p) {
	t.error("family command requires an argument");
	return 0;
      }
      char *tem = new char[strlen(p)+1];
      strcpy(tem, p);
      family = tem;
    }
    else if (strcmp("fonts", p) == 0) {
      p = strtok(0, WS);
      if (!p || sscanf(p, "%d", &nfonts) != 1 || nfonts <= 0) {
	t.error("bad number of fonts `%1'", p);
	return 0;
      }
      font_name_table = new char *[nfonts+1]; 
      for (int i = 0; i < nfonts; i++) {
	p = strtok(0, WS);
	while (p == 0) {
	  if (!t.next()) {
	    t.error("end of file while reading list of fonts");
	    return 0;
	  }
	  p = strtok(t.buf, WS);
	}
	char *temp = new char[strlen(p)+1];
	strcpy(temp, p);
	font_name_table[i] = temp;
      }
      p = strtok(0, WS);
      if (p != 0) {
	t.error("font count does not match number of fonts");
	return 0;
      }
      font_name_table[nfonts] = 0;
    }
    else if (strcmp("sizes", p) == 0) {
      int n = 16;
      sizes = new int[n];
      int i = 0;
      for (;;) {
	p = strtok(0, WS);
	while (p == 0) {
	  if (!t.next()) {
	    t.error("list of sizes must be terminated by `0'");
	    return 0;
	  }
	  p = strtok(t.buf, WS);
	}
	int lower, upper;
	switch (sscanf(p, "%d-%d", &lower, &upper)) {
	case 1:
	  upper = lower;
	  // fall through
	case 2:
	  if (lower <= upper && lower >= 0)
	    break;
	  // fall through
	default:
	  t.error("bad size range `%1'", p);
	  return 0;
	}
	if (i + 2 > n) {
	  int *old_sizes = sizes;
	  sizes = new int[n*2];
	  memcpy(sizes, old_sizes, n*sizeof(int));
	  n *= 2;
	  delete old_sizes;
	}
	sizes[i++] = lower;
	if (lower == 0)
	  break;
	sizes[i++] = upper;
      }
      if (i == 1) {
	t.error("must have some sizes");
	return 0;
      }
    }
    else if (strcmp("styles", p) == 0) {
      int style_table_size = 5;
      style_table = new char *[style_table_size];
      for (int j = 0; j < style_table_size; j++)
	style_table[j] = 0;
      int i = 0;
      for (;;) {
	p = strtok(0, WS);
	if (p == 0)
	  break;
	// leave room for terminating 0
	if (i + 1 >= style_table_size) {
	  const char **old_style_table = style_table;
	  style_table_size *= 2;
	  style_table = new char*[style_table_size];
	  for (j = 0; j < i; j++)
	    style_table[j] = old_style_table[j];
	  for (; j < style_table_size; j++)
	    style_table[j] = 0;
	  delete old_style_table;
	}
	char *tem = new char[strlen(p) + 1];
	strcpy(tem, p);
	style_table[i++] = tem;
      }
    }
    else if (strcmp("charset", p) == 0)
      break;
  }
  if (res == 0) {
    t.error("missing `res' command");
    return 0;
  }
  if (unitwidth == 0) {
    t.error("missing `unitwidth' command");
    return 0;
  }
  if (font_name_table == 0) {
    t.error("missing `fonts' commmand");
    return 0;
  }
  if (sizes == 0) {
    t.error("missing `sizes' command");
    return 0;
  }
  if (sizescale < 1) {
    t.error("bad `sizescale' value");
    return 0;
  }
  if (hor < 1) {
    t.error("bad `hor' value");
    return 0;
  }
  if (vert < 1) {
    t.error("bad `vert' value");
    return 0;
  }
  return 1;
}      

void font::handle_unknown_font_command(int, const char **)
{
}

