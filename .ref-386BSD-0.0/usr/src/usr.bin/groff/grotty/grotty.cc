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

#include "driver.h"

#ifndef USHRT_MAX
#define USHRT_MAX 65535
#endif

#define DEFAULT_LINES_PER_PAGE 66

#define TAB_WIDTH 8

static int horizontal_tab_flag = 0;
static int form_feed_flag = 0;
static int bold_flag = 1;
static int underline_flag = 1;
static int overstrike_flag = 1;

enum { UNDERLINE_MODE = 01, BOLD_MODE = 02 };

// Mode to use for bold-underlining.
static unsigned char bold_underline_mode = BOLD_MODE|UNDERLINE_MODE;

class tty_font : public font {
  tty_font(const char *);
  unsigned char mode;
public:
  ~tty_font();
  unsigned char get_mode() { return mode; }
#if 0
  void handle_x_command(int argc, const char **argv);
#endif
  static tty_font *load_tty_font(const char *);
};

tty_font *tty_font::load_tty_font(const char *s)
{
  tty_font *f = new tty_font(s);
  if (!f->load()) {
    delete f;
    return 0;
  }
  const char *s = f->get_internal_name();
  long n;
  if (s != 0 && (n = strtol(s, 0, 0)) != 0)
    f->mode = int(n & (BOLD_MODE|UNDERLINE_MODE));
  if (!underline_flag)
    f->mode &= ~UNDERLINE_MODE;
  if (!bold_flag)
    f->mode &= ~BOLD_MODE;
  if ((f->mode & (BOLD_MODE|UNDERLINE_MODE)) == (BOLD_MODE|UNDERLINE_MODE))
    f->mode = (f->mode & ~(BOLD_MODE|UNDERLINE_MODE)) | bold_underline_mode;
  return f;
}

tty_font::tty_font(const char *nm)
: font(nm), mode(0)
{
}

tty_font::~tty_font()
{
}

#if 0
void tty_font::handle_x_command(int argc, const char **argv)
{
  if (argc >= 1 && strcmp(argv[0], "bold") == 0)
    mode |= BOLD_MODE;
  else if (argc >= 1 && strcmp(argv[0], "underline") == 0)
    mode |= UNDERLINE_MODE;
}
#endif

// hpos and vpos must be non-adjacent, to work round a bug in g++ 1.37.1

struct glyph {
  unsigned short hpos;
  unsigned short serial;
  unsigned short vpos;
  unsigned char code;
  unsigned char mode;
};

class tty_printer : public printer {
  enum { INITIAL_VEC_SIZE = 32 };
  glyph *vec;
  int vec_used;
  int vec_size;
  int lines_per_page;
  int columns_per_page;
public:
  tty_printer();
  ~tty_printer();
  void set_char(int, font *, const environment *, int);
  void begin_page(int) { }
  void end_page();
  font *make_font(const char *);
};

tty_printer::tty_printer()
: vec_used(0), vec_size(0), vec(0)
{
  if (font::paperlength == 0)
    lines_per_page = DEFAULT_LINES_PER_PAGE;
  else if (font::paperlength % font::vert != 0)
    fatal("paperlength not a multiple of vertical resolution");
  else
    lines_per_page = font::paperlength/font::vert;
  if (lines_per_page > USHRT_MAX || lines_per_page <= 0)
    fatal("ridiculous paperlength");
  columns_per_page = font::paperwidth/font::hor;
  // If columns_per_page is zero, we won't truncate.
  if (columns_per_page < 0)
    columns_per_page = 0;
}

tty_printer::~tty_printer()
{
  delete vec;
}

void tty_printer::set_char(int i, font *f, const environment *env, int w)
{
  int h = env->hpos;
  if (h % font::hor != 0)
    fatal("horizontal position not a multiple of horizontal resolution");
  h /= font::hor;
  if (h < 0) {
    error("character to the left of first column discarded");
    return;
  }
  if (columns_per_page != 0 && h >= columns_per_page) {
    error("character to the right of last column discarded");
    return;
  }
  if (h > USHRT_MAX) {
    error("character with ridiculously large horizontal position discarded");
    return;
  }
  int v = env->vpos;
  if (v % font::vert != 0)
    fatal("vertical position not a multiple of vertical resolution");
  v /= font::vert;
  // Note that the first output line corresponds to groff position font::vert.
  if (v <= 0) {
    error("character above first line discarded");
    return;
  }
  if (v > lines_per_page) {
    error("character below last line discarded");
    return;
  }
  if (w != font::hor)
    fatal("width of character not equal to horizontal resolution");
  if (vec_used >= vec_size) {
    if (vec_size == 0)
      vec_size = INITIAL_VEC_SIZE;
    else {
      if (vec_size > USHRT_MAX/2) {
	if (vec_size >= USHRT_MAX)
	  fatal("too many characters on the page");
	vec_size = USHRT_MAX;
      }
      else
	vec_size *= 2;
    }
    glyph *old_vec = vec;
    vec = new glyph [vec_size];
    if (vec_used)
      memcpy(vec, old_vec, vec_used*sizeof(glyph));
    delete old_vec;
  }
  // We need a stable sort, but qsort is not stable, so we fake it.
  vec[vec_used].serial = vec_used;
  vec[vec_used].hpos = h;
  vec[vec_used].vpos = v;
  vec[vec_used].code = f->get_code(i);
  vec[vec_used].mode = ((tty_font *)f)->get_mode();
  vec_used++;
}

extern "C" {
static int compare_glyph(void *p1, void *p2)
{
  int v1 = ((glyph *)p1)->vpos;
  int v2 = ((glyph *)p2)->vpos;
  if (v1 < v2)
    return -1;
  if (v1 > v2)
    return 1;
  int h1 = ((glyph *)p1)->hpos;
  int h2 = ((glyph *)p2)->hpos;
  if (h1 < h2)
    return -1;
  if (h1 > h2)
    return 1;
  if (((glyph *)p1)->serial < ((glyph *)p2)->serial)
    return -1;
  return 1;
}
}

void tty_printer::end_page()
{
  qsort(vec, vec_used, sizeof(glyph), compare_glyph);
  int hpos = 0;
  int vpos = 1;
  // We have already discarded characters with vpos < 1 or > lines_per_page.
  for (int i = 0; i < vec_used; i++) {
    assert(vpos <= vec[i].vpos);
    if (!overstrike_flag
	&& i + 1 < vec_used
	&& vec[i].hpos == vec[i + 1].hpos
	&& vec[i].vpos == vec[i + 1].vpos)
      continue;
    for (; vpos < vec[i].vpos; vpos++) {
      putchar('\n');
      hpos = 0;
    }
    if (hpos > vec[i].hpos) {
      putchar('\b');
      hpos--;
    }
    else {
      if (horizontal_tab_flag) {
	for (;;) {
	  int next_tab_pos = ((hpos + TAB_WIDTH) / TAB_WIDTH) * TAB_WIDTH;
	  if (next_tab_pos > vec[i].hpos)
	    break;
	  putchar('\t');
	  hpos = next_tab_pos;
	}
      }
      for (; hpos < vec[i].hpos; hpos++)
	putchar(' ');
    }
    assert(hpos == vec[i].hpos && vpos == vec[i].vpos);
    if (isalnum(vec[i].code) && vec[i].mode & UNDERLINE_MODE) {
      putchar('_');
      putchar('\b');
    }
    if (vec[i].mode & BOLD_MODE) {
      putchar(vec[i].code);
      putchar('\b');
    }
    putchar(vec[i].code);
    hpos++;
  }
  if (form_feed_flag) {
    if (hpos != 0) {
      putchar('\n');
      vpos++;
    }
    if (vpos <= lines_per_page)
      putchar('\f');
  }
  else {
    for (; vpos <= lines_per_page; vpos++)
      putchar('\n');
  }
  vec_used = 0;
}

font *tty_printer::make_font(const char *nm)
{
  return tty_font::load_tty_font(nm);
}

printer *make_printer()
{
  return new tty_printer;
}

static void usage();

int main(int argc, char **argv)
{
  program_name = argv[0];
  static char stderr_buf[BUFSIZ];
  setbuf(stderr, stderr_buf);
  int c;
  while ((c = getopt(argc, argv, "F:vhfbuoBU")) != EOF)
    switch(c) {
    case 'v':
      {
	extern const char *version_string;
	fprintf(stderr, "grotty version %s\n", version_string);
	fflush(stderr);
	break;
      }
    case 'b':
      // Do not embolden by overstriking.
      bold_flag = 0;
      break;
    case 'u':
      // Do not underline.
      underline_flag = 0;
      break;
    case 'o':
      // Do not overstrike (other than emboldening and underlining).
      overstrike_flag = 0;
      break;
    case 'B':
      // Do bold-underlining as bold.
      bold_underline_mode = BOLD_MODE;
      break;
    case 'U':
      // Do bold-underlining as underlining.
      bold_underline_mode = UNDERLINE_MODE;
      break;
    case 'h':
      // Use horizontal tabs.
      horizontal_tab_flag = 1;
      break;
    case 'f':
      form_feed_flag = 1;
      break;
    case 'F':
      font::command_line_font_dir(optarg);
      break;
    case '?':
      usage();
      break;
    default:
      assert(0);
    }
  if (optind >= argc)
    do_file("-");
  else {
    for (int i = optind; i < argc; i++)
      do_file(argv[i]);
  }
  delete pr;
  exit(0);
}

static void usage()
{
  fprintf(stderr, "usage: %s [-hfvbuoBU] [-F dir] [files ...]\n",
	  program_name);
  exit(1);
}
