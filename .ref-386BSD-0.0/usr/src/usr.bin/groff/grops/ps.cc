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
#include "stringclass.h"
#include "cset.h"

extern "C" {
  // Sun's stdlib.h fails to declare this.
  char *mktemp(char *);
}

static int landscape_flag = 0;
static int ncopies = 1;
static int linewidth = -1;

#define DEFAULT_LINEWIDTH 40	/* in ems/1000 */
#define FILL_MAX 1000

// Maximum number of definitions in the prologue (used for sizing the
// dictionary.)

const int MAX_PROLOGUE_DEFS = 50;
const char *const dict_name = "grops";
const char *const defs_dict_name = "DEFS";
const int DEFS_DICT_SPARE = 50;

#define PROLOGUE "prologue"

double degrees(double r)
{
  return r*180.0/M_PI;
}

double radians(double d)
{
  return d*M_PI/180.0;
}

inline double transform_fill(int fill)
{
  return 1 - fill/double(FILL_MAX);
}

class ps_output {
public:
  ps_output(FILE *, int max_line_length);
  ps_output &put_string(const char *, int);
  ps_output &put_number(int);
  ps_output &put_fix_number(int);
  ps_output &put_float(double);
  ps_output &put_symbol(const char *);
  ps_output &put_literal_symbol(const char *);
  ps_output &set_fixed_point(int);
  ps_output &simple_comment(const char *);
  ps_output &begin_comment(const char *);
  ps_output &comment_arg(const char *);
  ps_output &end_comment();
  ps_output &set_file(FILE *);
  ps_output &include_file(FILE *);
  ps_output &copy_file(FILE *);
  ps_output &end_line();
  ps_output &put_delimiter(char);
  ps_output &special(const char *);
private:
  FILE *fp;
  int col;
  int max_line_length;		// not including newline
  int need_space;
  int fixed_point;
};

ps_output::ps_output(FILE *f, int n)
: fp(f), max_line_length(n), col(0), need_space(0), fixed_point(0)
{
}

ps_output &ps_output::set_file(FILE *f)
{
  fp = f;
  col = 0;
  return *this;
}

ps_output &ps_output::include_file(FILE *infp)
{
  if (col != 0)
    putc('\n', fp);
  int c;
#ifdef BROKEN_SPOOLER
  // Strip the first line if it's a comment.  I believe
  // some spoolers get confused by %! in the middle of a file.
  if ((c = getc(infp)) == '%') {
    while ((c = getc(infp)) != '\n' && c != '\r' && c != EOF)
      ;
  }
  else if (c != EOF)
    ungetc(c, infp);
#endif /* BROKEN_SPOOLER */
  // We strip out lines beginning with STRIPENDUM.
#ifdef BROKEN_SPOOLER
#define STRIPENDUM "%%"
#else
#define STRIPENDUM "%%IncludeFont:"
#endif
  // Index into STRIPENDUM of next character to be matched.
  int match = 0;
  while ((c = getc(infp)) != EOF) {
    if (match >= 0) {
      if (c == STRIPENDUM[match]) {
	if (++match == sizeof(STRIPENDUM) - 1) {
	  match = -1;
	  while ((c = getc(infp)) != EOF)
	    if (c == '\r' || c == '\n') {
	      match = 0;
	      break;
	    }
	}
      }
      else {
	for (int i = 0; i < match; i++)
	  putc(STRIPENDUM[i], fp);
	putc(c, fp);
	match = (c == '\n' || c == '\r' ? 0 : -1);
      }
    }
    else {
      putc(c, fp);
      match = (c == '\n' || c == '\r' ? 0 : -1);
    }
  }
  for (int i = 0; i < match; i++)
    putc(STRIPENDUM[i], fp);
  if (match != 0)
    putc('\n', fp);
  int lastc = '\n';
  while ((c = getc(infp)) != EOF) {
    putc(c, fp);
    lastc = c;
  }
  if (lastc != '\n')
    putc('\n', fp);
  col = 0;
  need_space = 0;
  return *this;
}

ps_output &ps_output::copy_file(FILE *infp)
{
  int c;
  while ((c = getc(infp)) != EOF)
    putc(c, fp);
  return *this;
}

ps_output &ps_output::end_line()
{
  if (col != 0) {
    putc('\n', fp);
    col = 0;
  }
  return *this;
}

ps_output &ps_output::special(const char *s)
{
  if (s == 0 || *s == '\0')
    return *this;
  if (col != 0) {
    putc('\n', fp);
    col = 0;
  }
  fputs(s, fp);
  if (strchr(s, '\0')[-1] != '\n')
    putc('\n', fp);
  need_space = 0;
  return *this;
}

ps_output &ps_output::simple_comment(const char *s)
{
  if (col != 0)
    putc('\n', fp);
  putc('%', fp);
  putc('%', fp);
  fputs(s, fp);
  putc('\n', fp);
  col = 0;
  need_space = 0;
  return *this;
}

ps_output &ps_output::begin_comment(const char *s)
{
  if (col != 0)
    putc('\n', fp);
  putc('%', fp);
  putc('%', fp);
  fputs(s, fp);
  col = 2 + strlen(s);
  return *this;
}

ps_output &ps_output::end_comment()
{
  if (col != 0) {
    putc('\n', fp);
    col = 0;
  }
  need_space = 0;
  return *this;
}

ps_output &ps_output::comment_arg(const char *s)
{
  int len = strlen(s);
  if (col + len + 1 > max_line_length) {
    putc('\n', fp);
    fputs("%%+", fp);
    col = 3;
  }
  putc(' ',  fp);
  fputs(s, fp);
  col += len + 1;
  return *this;
}

ps_output &ps_output::set_fixed_point(int n)
{
  assert(n >= 0 && n <= 10);
  fixed_point = n;
  return *this;
}

ps_output &ps_output::put_delimiter(char c)
{
  if (col + 1 > max_line_length) {
    putc('\n', fp);
    col = 0;
  }
  putc(c, fp);
  col++;
  need_space = 0;
  return *this;
}

ps_output &ps_output::put_string(const char *s, int n)
{
  int len = 0;
  for (int i = 0; i < n; i++) {
    char c = s[i];
    if (isascii(c) && isprint(c)) {
      if (c == '(' || c == ')' || c == '\\')
	len += 2;
      else
	len += 1;
    }
    else
      len += 4;
  }
  if (len > n*2) {
    if (col + n*2 + 2 > max_line_length && n*2 + 2 <= max_line_length) {
      putc('\n', fp);
      col = 0;
    }
    if (col + 1 > max_line_length) {
      putc('\n', fp);
      col = 0;
    }
    putc('<', fp);
    col++;
    for (i = 0; i < n; i++) {
      if (col + 2 > max_line_length) {
	putc('\n', fp);
	col = 0;
      }
      fprintf(fp, "%02x", s[i] & 0377);
      col += 2;
    }
    putc('>', fp);
    col++;
  }
  else {
    if (col + len + 2 > max_line_length && len + 2 <= max_line_length) {
      putc('\n', fp);
      col = 0;
    }
    if (col + 2 > max_line_length) {
      putc('\n', fp);
      col = 0;
    }
    putc('(', fp);
    col++;
    for (i = 0; i < n; i++) {
      char c = s[i];
      if (isascii(c) && isprint(c)) {
	if (c == '(' || c == ')' || c == '\\')
	  len = 2;
	else
	  len = 1;
      }
      else
	len = 4;
      if (col + len + 1 > max_line_length) {
	putc('\\', fp);
	putc('\n', fp);
	col = 0;
      }
      switch (len) {
      case 1:
	putc(c, fp);
	break;
      case 2:
	putc('\\', fp);
	putc(c, fp);
	break;
      case 4:
	fprintf(fp, "\\%03o", c & 0377);
	break;
      default:
	assert(0);
      }
      col += len;
    }
    putc(')', fp);
    col++;
  }
  need_space = 0;
  return *this;
}

ps_output &ps_output::put_number(int n)
{
  char buf[1 + INT_DIGITS + 1];
  sprintf(buf, "%d", n);
  int len = strlen(buf);
  if (col > 0 && col + len + need_space > max_line_length) {
    putc('\n', fp);
    col = 0;
    need_space = 0;
  }
  if (need_space) {
    putc(' ', fp);
    col++;
  }
  fputs(buf, fp);
  col += len;
  need_space = 1;
  return *this;
}


ps_output &ps_output::put_fix_number(int i)
{
  const char *p = iftoa(i, fixed_point);
  int len = strlen(p);
  if (col > 0 && col + len + need_space > max_line_length) {
    putc('\n', fp);
    col = 0;
    need_space = 0;
  }
  if (need_space) {
    putc(' ', fp);
    col++;
  }
  fputs(p, fp);
  col += len;
  need_space = 1;
  return *this;
}

ps_output &ps_output::put_float(double d)
{
  char buf[128];
  sprintf(buf, "%.4f", d);
  int len = strlen(buf);
  if (col > 0 && col + len + need_space > max_line_length) {
    putc('\n', fp);
    col = 0;
    need_space = 0;
  }
  if (need_space) {
    putc(' ', fp);
    col++;
  }
  fputs(buf, fp);
  col += len;
  need_space = 1;
  return *this;
}

ps_output &ps_output::put_symbol(const char *s)
{
  int len = strlen(s);
  if (col > 0 && col + len + need_space > max_line_length) {
    putc('\n', fp);
    col = 0;
    need_space = 0;
  }
  if (need_space) {
    putc(' ', fp);
    col++;
  }
  fputs(s, fp);
  col += len;
  need_space = 1;
  return *this;
}

ps_output &ps_output::put_literal_symbol(const char *s)
{
  int len = strlen(s);
  if (col > 0 && col + len + 1 > max_line_length) {
    putc('\n', fp);
    col = 0;
  }
  putc('/', fp);
  fputs(s, fp);
  col += len + 1;
  need_space = 1;
  return *this;
}

class ps_font : public font {
  ps_font(const char *);
public:
  int encoding_index;
  char *encoding;
  char *reencoded_name;
  ~ps_font();
  void handle_unknown_font_command(int argc, const char **argv);
  static ps_font *load_ps_font(const char *);
};

ps_font *ps_font::load_ps_font(const char *s)
{
  ps_font *f = new ps_font(s);
  if (!f->load()) {
    delete f;
    return 0;
  }
  return f;
}

ps_font::ps_font(const char *nm)
: font(nm), encoding(0), reencoded_name(0), encoding_index(-1)
{
}

ps_font::~ps_font()
{
  delete encoding;
  delete reencoded_name;
}

void ps_font::handle_unknown_font_command(int argc, const char **argv)
{
  if (strcmp(argv[0], "encoding") == 0) {
    if (argc != 2)
      error("`encoding' command requires exactly 1 argument");
    else
      encoding = strsave(argv[1]);
  }
}


struct style {
  font *f;
  int point_size;
  int height;
  int slant;
  style();
  style(font *, int, int, int);
  int operator==(const style &) const;
  int operator!=(const style &) const;
};

style::style() : f(0)
{
}

style::style(font *p, int sz, int h, int sl)
: f(p), point_size(sz), height(h), slant(sl)
{
}

int style::operator==(const style &s) const
{
  return (f == s.f && point_size == s.point_size
	  && height == s.height && slant == s.slant);
}

int style::operator!=(const style &s) const
{
  return !(*this == s);
}

struct depend_list;

struct document_font {
  enum { LISTED = 01, NEEDED = 02, SUPPLIED = 04 };
  char *name;
  char *filename;
  int flags;
  document_font *next;
  depend_list *depends_on;
  int mark;
  document_font(const char *);
  ~document_font();
  void download(ps_output &);
};

struct depend_list {
  document_font *p;
  depend_list *next;
};


class ps_printer : public printer {
  FILE *tempfp;
  ps_output out;
  int res;
  int space_char_index;
  int pages_output;
  int paper_length;
  int equalise_spaces;
  enum { SBUF_SIZE = 256 };
  char sbuf[SBUF_SIZE];
  int sbuf_len;
  int sbuf_start_hpos;
  int sbuf_vpos;
  int sbuf_end_hpos;
  int sbuf_space_width;
  int sbuf_space_count;
  int sbuf_space_diff_count;
  int sbuf_space_code;
  int sbuf_kern;
  style sbuf_style;
  style output_style;
  int output_hpos;
  int output_vpos;
  int output_draw_point_size;
  int line_thickness;
  int output_line_thickness;
  int fill;
  unsigned char output_space_code;
  enum { MAX_DEFINED_STYLES = 50 };
  style defined_styles[MAX_DEFINED_STYLES];
  int ndefined_styles;
  int next_encoding_index;
  string defs;
  int ndefs;
  document_font *doc_fonts;

  void flush_sbuf();
  void set_style(const style &);
  void set_space_code(unsigned char c);
  int set_encoding_index(ps_font *);
  void do_exec(char *, const environment *);
  void do_import(char *, const environment *);
  void do_def(char *, const environment *);
  void do_mdef(char *, const environment *);
  void do_file(char *, const environment *);
  void set_line_thickness(const environment *);
  void fill_path();
  void merge_download_fonts();
  void merge_import_fonts(FILE *);
  void merge_ps_fonts();
  void print_font_comment();
  void print_supplied_font_comment();
  void print_needed_font_comment();
  void print_include_font_comments();
  document_font *lookup_doc_font(const char *);
  void encode_fonts();
  void download_fonts();
  void define_encoding(const char *, int);
  void reencode_font(ps_font *);
  void read_download_file();
public:
  ps_printer();
  ~ps_printer();
  void set_char(int i, font *f, const environment *env, int w);
  void draw(int code, int *p, int np, const environment *env);
  void begin_page(int);
  void end_page();
  void special(char *arg, const environment *env);
  font *make_font(const char *);
  void end_of_line();
};

ps_printer::ps_printer()
: pages_output(0),
  sbuf_len(0),
  output_hpos(-1),
  output_vpos(-1),
  out(0, 79),
  ndefined_styles(0),
  next_encoding_index(0),
  line_thickness(-1),
  fill(FILL_MAX + 1),
  ndefs(0),
  doc_fonts(0)
{
  static char temp_filename[] = "/tmp/gropsXXXXXX";
  mktemp(temp_filename);
  tempfp = fopen(temp_filename, "w+");
  if (tempfp == 0)
    fatal("can't open temporary file `%1': %2",
	  temp_filename, strerror(errno));
  if (unlink(temp_filename) < 0)
    error("can't unlink `%1': %2", temp_filename, strerror(errno));
  out.set_file(tempfp);
  if (linewidth < 0)
    linewidth = DEFAULT_LINEWIDTH;
  if (font::hor != 1)
    fatal("horizontal resolution must be 1");
  if (font::vert != 1)
    fatal("vertical resolution must be 1");
  if (font::res % (font::sizescale*72) != 0)
    fatal("res must be a multiple of 72*sizescale");
  int r = font::res;
  int point = 0;
  while (r % 10 == 0) {
    r /= 10;
    point++;
  }
  res = r;
  out.set_fixed_point(point);
  space_char_index = font::name_to_index("space");
  paper_length = font::paperlength;
  if (paper_length == 0)
    paper_length = 11*font::res;
  equalise_spaces = font::res >= 72000;
}

int ps_printer::set_encoding_index(ps_font *f)
{
  if (f->encoding_index >= 0)
    return f->encoding_index;
  for (font_pointer_list *p = font_list; p; p = p->next)
    if (p->p != f) {
      char *encoding = ((ps_font *)p->p)->encoding;
      int encoding_index = ((ps_font *)p->p)->encoding_index;
      if (encoding != 0 && encoding_index >= 0 
	  && strcmp(f->encoding, encoding) == 0) {
	return f->encoding_index = encoding_index;
      }
    }
  return f->encoding_index = next_encoding_index++;
}

void ps_printer::set_char(int i, font *f, const environment *env, int w)
{
  if (i == space_char_index)
    return;
  unsigned char code = f->get_code(i);
  style sty(f, env->size, env->height, env->slant);
  if (sty.slant != 0) {
    if (sty.slant > 80 || sty.slant < -80) {
      error("silly slant `%1' degrees", sty.slant);
      sty.slant = 0;
    }
  }
  if (sbuf_len > 0) {
    if (sbuf_len < SBUF_SIZE
	&& sty == sbuf_style
	&& sbuf_vpos == env->vpos) {
      if (sbuf_end_hpos == env->hpos) {
	sbuf[sbuf_len++] = code;
	sbuf_end_hpos += w + sbuf_kern;
	return;
      }
      if (sbuf_len == 1 && sbuf_kern == 0) {
	sbuf_kern = env->hpos - sbuf_end_hpos;
	sbuf_end_hpos = env->hpos + sbuf_kern + w;
	sbuf[sbuf_len++] = code;
	return;
      }
      /* If sbuf_end_hpos - sbuf_kern == env->hpos, we are better off
	 starting a new string. */
      if (sbuf_len < SBUF_SIZE - 1 && env->hpos >= sbuf_end_hpos
	  && (sbuf_kern == 0 || sbuf_end_hpos - sbuf_kern != env->hpos)) {
	if (sbuf_space_code < 0) {
	  if (f->contains(space_char_index)) {
	    sbuf_space_code = f->get_code(space_char_index);
	    sbuf_space_width = env->hpos - sbuf_end_hpos;
	    sbuf_end_hpos = env->hpos + w + sbuf_kern;
	    sbuf[sbuf_len++] = sbuf_space_code;
	    sbuf[sbuf_len++] = code;
	    sbuf_space_count++;
	    return;
	  }
	}
	else {
	  int diff = env->hpos - sbuf_end_hpos - sbuf_space_width;
	  if (diff == 0 || (equalise_spaces && (diff == 1 || diff == -1))) {
	    sbuf_end_hpos = env->hpos + w + sbuf_kern;
	    sbuf[sbuf_len++] = sbuf_space_code;
	    sbuf[sbuf_len++] = code;
	    sbuf_space_count++;
	    if (diff == 1)
	      sbuf_space_diff_count++;
	    else if (diff == -1)
	      sbuf_space_diff_count--;
	    return;
	  }
	}
      }
    }
    flush_sbuf();
  }
  sbuf_len = 1;
  sbuf[0] = code;
  sbuf_end_hpos = env->hpos + w;
  sbuf_start_hpos = env->hpos;
  sbuf_vpos = env->vpos;
  sbuf_style = sty;
  sbuf_space_code = -1;
  sbuf_space_width = 0;
  sbuf_space_count = sbuf_space_diff_count = 0;
  sbuf_kern = 0;
}

int is_small_h(int n)
{
  return n < (font::res*2)/72 && n > -(font::res*10)/72;
}

int is_small_v(int n)
{
  return n < (font::res*4)/72 && n > -(font::res*4)/72;
}

static char *make_encoding_name(int encoding_index)
{
  static char buf[3 + INT_DIGITS + 1];
  sprintf(buf, "ENC%d", encoding_index);
  return buf;
}

const char *const WS = " \t\n\r";

void ps_printer::define_encoding(const char *encoding, int encoding_index)
{
  char *vec[256];
  for (int i = 0; i < 256; i++)
    vec[i] = 0;
  char *path;
  FILE *fp = font::open_file(encoding, &path);
  if (fp == 0)
    fatal("can't open encoding file `%1'", encoding);
  int lineno = 1;
  char buf[256];
  while (fgets(buf, 512, fp) != 0) {
    char *p = buf;
    while (isascii(*p) && isspace(*p))
      p++;
    if (*p != '#' && *p != '\0' && (p = strtok(buf, WS)) != 0) {
      char *q = strtok(0, WS);
      int n;
      if (q == 0 || sscanf(q, "%d", &n) != 1 || n < 0 || n >= 256)
	fatal_with_file_and_line(path, lineno, "bad second field");
      vec[n] = new char[strlen(p) + 1];
      strcpy(vec[n], p);
    }
    lineno++;
  }
  delete path;
  out.put_literal_symbol(make_encoding_name(encoding_index));
  out.put_delimiter('[');
  for (i = 0; i < 256; i++) {
    if (vec[i] == 0)
      out.put_literal_symbol(".notdef");
    else
      out.put_literal_symbol(vec[i]);
  }
  out.put_delimiter(']').put_symbol("def");
}

void ps_printer::reencode_font(ps_font *f)
{
  out.put_literal_symbol(f->reencoded_name)
     .put_symbol(make_encoding_name(f->encoding_index))
     .put_literal_symbol(f->get_internal_name())
     .put_symbol("RE");
}

void ps_printer::encode_fonts()
{
  if (next_encoding_index == 0)
    return;
  char *done_encoding = new char[next_encoding_index];
  for (int i = 0; i < next_encoding_index; i++)
    done_encoding[i] = 0;
  for (font_pointer_list *f = font_list; f; f = f->next) {
    int encoding_index = ((ps_font *)f->p)->encoding_index;
    if (encoding_index >= 0) {
      assert(encoding_index < next_encoding_index);
      if (!done_encoding[encoding_index]) {
	done_encoding[encoding_index] = 1;
	define_encoding(((ps_font *)f->p)->encoding, encoding_index);
      }
      reencode_font((ps_font *)f->p);
    }
  }
  delete done_encoding;
}

void ps_printer::set_style(const style &sty)
{
  char buf[1 + INT_DIGITS + 1];
  for (int i = 0; i < ndefined_styles; i++)
    if (sty == defined_styles[i]) {
      sprintf(buf, "F%d", i);
      out.put_symbol(buf);
      return;
    }
  if (ndefined_styles >= MAX_DEFINED_STYLES)
    ndefined_styles = 0;
  sprintf(buf, "F%d", ndefined_styles);
  out.put_literal_symbol(buf);
  const char *psname = sty.f->get_internal_name();
  if (psname == 0)
    fatal("no internalname specified for font `%1'", sty.f->get_name());
  char *encoding = ((ps_font *)sty.f)->encoding;
  if (encoding != 0) {
    char *s = ((ps_font *)sty.f)->reencoded_name;
    if (s == 0) {
      int ei = set_encoding_index((ps_font *)sty.f);
      char *tem = new char[strlen(psname) + 1 + INT_DIGITS + 1];
      sprintf(tem, "%s@%d", psname, ei);
      psname = tem;
      ((ps_font *)sty.f)->reencoded_name = tem;
    }
    else
      psname = s;
  }
  out.put_fix_number((font::res/(72*font::sizescale))*sty.point_size);
  if (sty.height != 0 || sty.slant != 0) {
    int h = sty.height == 0 ? sty.point_size : sty.height;
    h *= font::res/(72*font::sizescale);
    int c = int(h*tan(radians(sty.slant)) + .5);
    out.put_fix_number(c).put_fix_number(h).put_literal_symbol(psname)
       .put_symbol("MF");
  }
  else {
    out.put_literal_symbol(psname).put_symbol("SF");
  }
  defined_styles[ndefined_styles++] = sty;
}

void ps_printer::set_space_code(unsigned char c)
{
  out.put_literal_symbol("SC").put_number(c).put_symbol("def");
}

void ps_printer::end_of_line()
{
  flush_sbuf();
  // this ensures that we do an absolute motion to the beginning of a line
  output_vpos = output_hpos = -1;
}

void ps_printer::flush_sbuf()
{
  enum {
    NONE,
    RELATIVE_H,
    RELATIVE_V,
    RELATIVE_HV,
    ABSOLUTE
    } motion = NONE;
  int space_flag = 0;
  if (sbuf_len == 0)
    return;
  if (output_style != sbuf_style) {
    set_style(sbuf_style);
    output_style = sbuf_style;
  }
  int extra_space = 0;
  if (output_hpos < 0 || output_vpos < 0
      || !is_small_h(output_hpos - sbuf_start_hpos)
      || !is_small_v(output_vpos - sbuf_vpos))
    motion = ABSOLUTE;
  else {
    if (output_hpos != sbuf_start_hpos)
      motion = RELATIVE_H;
    if (output_vpos != sbuf_vpos) {
      if  (motion != NONE)
	motion = RELATIVE_HV;
      else
	motion = RELATIVE_V;
    }
  }
  if (sbuf_space_code >= 0) {
    int w = sbuf_style.f->get_width(space_char_index, sbuf_style.point_size);
    if (w + sbuf_kern != sbuf_space_width) {
      if (sbuf_space_code != output_space_code) {
	set_space_code(sbuf_space_code);
	output_space_code = sbuf_space_code;
      }
      space_flag = 1;
      extra_space = sbuf_space_width - w - sbuf_kern;
      if (sbuf_space_diff_count > sbuf_space_count/2)
	extra_space++;
      else if (sbuf_space_diff_count < -(sbuf_space_count/2))
	extra_space--;
    }
  }
  if (space_flag)
    out.put_fix_number(extra_space);
  if (sbuf_kern != 0)
    out.put_fix_number(sbuf_kern);
  out.put_string(sbuf, sbuf_len);
  char sym[2];
  sym[0] = 'A' + motion*4 + space_flag + 2*(sbuf_kern != 0);
  sym[1] = '\0';
  switch (motion) {
  case NONE:
    break;
  case ABSOLUTE:
    out.put_fix_number(sbuf_start_hpos)
       .put_fix_number(sbuf_vpos);
    break;
  case RELATIVE_H:
    out.put_fix_number(sbuf_start_hpos - output_hpos);
    break;
  case RELATIVE_V:
    out.put_fix_number(sbuf_vpos - output_vpos);
    break;
  case RELATIVE_HV:
    out.put_fix_number(sbuf_start_hpos - output_hpos)
       .put_fix_number(sbuf_vpos - output_vpos);
    break;
  default:
    assert(0);
  }
  out.put_symbol(sym);
  output_hpos = sbuf_end_hpos;
  output_vpos = sbuf_vpos;
  sbuf_len = 0;
}


void ps_printer::set_line_thickness(const environment *env)
{
  if (line_thickness < 0) {
    if (output_draw_point_size != env->size) {
      // we ought to check for overflow here
      int lw = ((font::res/(72*font::sizescale))*linewidth*env->size)/1000;
      out.put_fix_number(lw).put_symbol("LW");
      output_draw_point_size = env->size;
      output_line_thickness = -1;
    }
  }
  else {
    if (output_line_thickness != line_thickness) {
      out.put_fix_number(line_thickness).put_symbol("LW");
      output_line_thickness = line_thickness;
      output_draw_point_size = -1;
    }
  }
}

void ps_printer::fill_path()
{
  if (fill > FILL_MAX)
    out.put_symbol("BL");
  else
    out.put_float(transform_fill(fill)).put_symbol("FL");
}

void ps_printer::draw(int code, int *p, int np, const environment *env)
{
  int fill_flag = 0;
  switch (code) {
  case 'C':
    fill_flag = 1;
    // fall through
  case 'c':
    // troff adds an extra argument to C
    if (np != 1 && !(code == 'C' && np == 2)) {
      error("1 argument required for circle");
      break;
    }
    out.put_fix_number(env->hpos + p[0]/2)
       .put_fix_number(env->vpos)
       .put_fix_number(p[0]/2)
       .put_symbol("DC");
    if (fill_flag) {
      fill_path();
    }
    else {
      set_line_thickness(env);
      out.put_symbol("ST");
    }
    break;
  case 'l':
    if (np != 2) {
      error("2 arguments required for line");
      break;
    }
    set_line_thickness(env);
    out.put_fix_number(p[0] + env->hpos)
       .put_fix_number(p[1] + env->vpos)
       .put_fix_number(env->hpos)
       .put_fix_number(env->vpos)
       .put_symbol("DL");
    break;
  case 'E':
    fill_flag = 1;
    // fall through
  case 'e':
    if (np != 2) {
      error("2 arguments required for ellipse");
      break;
    }
    out.put_fix_number(p[0])
       .put_fix_number(p[1])
       .put_fix_number(env->hpos + p[0]/2)
       .put_fix_number(env->vpos)
       .put_symbol("DE");
    if (fill_flag) {
      fill_path();
    }
    else {
      set_line_thickness(env);
      out.put_symbol("ST");
    }
    break;
  case 'P':
    fill_flag = 1;
    // fall through
  case 'p':
    {
      if (np & 1) {
	error("even number of arguments required for polygon");
	break;
      }
      if (np == 0) {
	error("no arguments for polygon");
	break;
      }
      out.put_fix_number(env->hpos)
	 .put_fix_number(env->vpos)
	 .put_symbol("MT");
      for (int i = 0; i < np; i += 2)
	out.put_fix_number(p[i])
	   .put_fix_number(p[i+1])
	   .put_symbol("RL");
      out.put_symbol("CL");
      if (fill_flag) {
	fill_path();
      }
      else {
	set_line_thickness(env);
	out.put_symbol("ST");
      }
      break;
    }
  case '~':
    {
      if (np & 1) {
	error("even number of arguments required for spline");
	break;
      }
      if (np == 0) {
	error("no arguments for spline");
	break;
      }
      out.put_fix_number(env->hpos)
	 .put_fix_number(env->vpos)
	 .put_symbol("MT");
      out.put_fix_number(p[0]/2)
	 .put_fix_number(p[1]/2)
	 .put_symbol("RL");
      /* tnum/tden should be between 0 and 1; the closer it is to 1
	 the tighter the curve will be to the guiding lines; 2/3
	 is the standard value */
      const int tnum = 2;
      const int tden = 3;
      for (int i = 0; i < np - 2; i += 2) {
	out.put_fix_number((p[i]*tnum)/(2*tden))
	   .put_fix_number((p[i + 1]*tnum)/(2*tden))
	   .put_fix_number(p[i]/2 + (p[i + 2]*(tden - tnum))/(2*tden))
	   .put_fix_number(p[i + 1]/2 + (p[i + 3]*(tden - tnum))/(2*tden))
	   .put_fix_number((p[i] - p[i]/2) + p[i + 2]/2)
	   .put_fix_number((p[i + 1] - p[i + 1]/2) + p[i + 3]/2)
	   .put_symbol("RC");
      }
      out.put_fix_number(p[np - 2] - p[np - 2]/2)
	 .put_fix_number(p[np - 1] - p[np - 1]/2)
	 .put_symbol("RL");
      set_line_thickness(env);
      out.put_symbol("ST");
    }
    break;
  case 'a':
    {
      if (np != 4) {
	error("4 arguments required for arc");
	break;
      }
      set_line_thickness(env);
      int x = p[0] + p[2];
      int y = p[1] + p[3];
      double n = p[0]*double(x) + p[1]*double(y);
      if (n == 0)
	out.put_fix_number(x + env->hpos)
	   .put_fix_number(y + env->vpos)
	   .put_fix_number(env->hpos)
	   .put_fix_number(env->vpos)
	   .put_symbol("DL");
      else {
	double k = (double(x)*x + double(y)*y)/(2.0*n);
	double cx = k*p[0];
	double cy = k*p[1];
	out.put_fix_number(env->hpos + int(cx))
	   .put_fix_number(env->vpos + int(cy))
	   .put_fix_number(int(sqrt(cx*cx + cy*cy)))
	   .put_float(degrees(atan2(-cy, -cx)))
	   .put_float(degrees(atan2(y - cy, x - cx)))
	   .put_symbol("DA");
      }
    }
    break;
  case 't':
    {
      if (np == 0) {
	line_thickness = -1;
      }
      else {
	// troff gratuitously adds an extra 0
	if (np != 1 && np != 2) {
	  error("0 or 1 argument required for thickness");
	  break;
	}
	line_thickness = p[0];
      }
      break;
    }
  case 'f':
    {
      if (np != 1 && np != 2) {
	error("1 argument required for fill");
	break;
      }
      fill = p[0];
      if (fill < 0 || fill > FILL_MAX) {
	// This means fill with the current color.
	fill = FILL_MAX + 1;
      }
      break;
    }      
  default:
    error("unrecognised drawing command `%1'", char(code));
    break;
  }

  output_hpos = output_vpos = -1;
}

void ps_printer::begin_page(int n)
{
  out.begin_comment("Page:").comment_arg(itoa(n));
  out.comment_arg(itoa(++pages_output)).end_comment();
  output_style.f = 0;
  output_space_code = 32;
  output_draw_point_size = -1;
  output_line_thickness = -1;
  output_hpos = output_vpos = -1;
  ndefined_styles = 0;
  out.put_symbol("BP");
}

void ps_printer::end_page()
{
  flush_sbuf();
  out.put_symbol("EP");
}

font *ps_printer::make_font(const char *nm)
{
  return ps_font::load_ps_font(nm);
}

ps_printer::~ps_printer()
{
  out.simple_comment("Trailer");
  out.put_symbol("end");
  out.end_line();
  if (fseek(tempfp, 0L, 0) < 0)
    fatal("fseek on temporary file failed");
  fputs("%!PS-Adobe-2.1\n", stdout);
  out.set_file(stdout);
  {
    extern const char *version_string;
    out.begin_comment("Creator:")
       .comment_arg("groff")
       .comment_arg("version")
       .comment_arg(version_string)
       .end_comment();
  }
  read_download_file();
  merge_ps_fonts();
  merge_download_fonts();
  print_font_comment();
  print_supplied_font_comment();
  print_needed_font_comment();
  out.begin_comment("Pages:").comment_arg(itoa(pages_output)).end_comment();
  out.simple_comment("EndComments");
  char *path;
  FILE *fp = font::open_file(PROLOGUE, &path);
  if (fp == 0)
    fatal("can't find `%1'", PROLOGUE);
  out.put_literal_symbol(dict_name);
  out.put_number(MAX_DEFINED_STYLES + MAX_PROLOGUE_DEFS).put_symbol("dict");
  out.put_symbol("def");
  out.put_symbol(dict_name).put_symbol("begin");
  out.include_file(fp);
  fclose(fp);
  if (ndefs > 0)
    ndefs += DEFS_DICT_SPARE;
  out.put_literal_symbol(defs_dict_name)
     .put_number(ndefs + 1)
     .put_symbol("dict")
     .put_symbol("def");
  out.put_symbol(defs_dict_name)
     .put_symbol("begin");
  out.put_literal_symbol("u")
     .put_delimiter('{')
     .put_fix_number(1)
     .put_symbol("mul")
     .put_delimiter('}')
     .put_symbol("bind")
     .put_symbol("def");
  defs += '\0';
  out.special(defs.contents());
  out.put_symbol("end");
  delete path;
  out.put_symbol("end");
#ifndef BROKEN_SPOOLER
  out.simple_comment("EndProlog");
#endif /* !BROKEN_SPOOLER */
  print_include_font_comments();
  download_fonts();
#ifndef BROKEN_SPOOLER
  out.simple_comment("BeginSetup");
#endif /* !BROKEN_SPOOLER */
  out.put_symbol(dict_name).put_symbol("begin");
  out.put_literal_symbol("#copies").put_number(ncopies).put_symbol("def");
  out.put_literal_symbol("RES").put_number(res).put_symbol("def");
  out.put_literal_symbol("PL").put_fix_number(paper_length).put_symbol("def");
  out.put_literal_symbol("LS")
     .put_symbol(landscape_flag ? "true" : "false")
     .put_symbol("def");
  encode_fonts();
#ifndef BROKEN_SPOOLER
  out.simple_comment("EndSetup");
#else /* !BROKEN_SPOOLER */
  out.simple_comment("EndProlog");
#endif /* BROKEN_SPOOLER */
  out.end_line();
  out.copy_file(tempfp);
  fclose(tempfp);
}

void ps_printer::special(char *arg, const environment *env)
{
  typedef void (ps_printer::*SPECIAL_PROCP)(char *, const environment *);
  static struct {
    const char *name;
    SPECIAL_PROCP proc;
  } proc_table[] = {
    "exec", &ps_printer::do_exec,
    "def", &ps_printer::do_def,
    "mdef", &ps_printer::do_mdef,
    "import", &ps_printer::do_import,
    "file", &ps_printer::do_file,
  };
  for (char *p = arg; *p == ' ' || *p == '\n'; p++)
    ;
  char *tag = p;
  for (; *p != '\0' && *p != ':' && *p != ' ' && *p != '\n'; p++)
    ;
  if (*p == '\0' || strncmp(tag, "ps", p - tag) != 0) {
    error("X command without `ps:' tag ignored");
    return;
  }
  p++;
  for (; *p == ' ' || *p == '\n'; p++)
    ;
  char *command = p;
  for (; *p != '\0' && *p != ' ' && *p != '\n'; p++)
    ;
  if (*command == '\0') {
    error("X command without `ps:' tag ignored");
    return;
  }
  for (int i = 0; i < sizeof(proc_table)/sizeof(proc_table[0]); i++)
    if (strncmp(command, proc_table[i].name, p - command) == 0) {
      flush_sbuf();
      (this->*(proc_table[i].proc))(p, env);
      return;
    }
  error("X command `%1' not recognised", command);
}

// A conforming PostScript document must not have lines longer
// than 255 characters (excluding line termination characters).

static int check_line_lengths(const char *p)
{
  for (;;) {
    const char *end = strchr(p, '\n');
    if (end == 0)
      end = strchr(p, '\0');
    if (end - p > 255)
      return 0;
    if (*end == '\0')
      break;
    p = end + 1;
  }
  return 1;
}

void ps_printer::do_exec(char *arg, const environment *env)
{
  while (csspace(*arg))
    arg++;
  if (*arg == '\0') {
    error("missing argument to X exec command");
    return;
  }
  if (!check_line_lengths(arg)) {
    error("lines in X exec command must not be more than 255 characters long");
    return;
  }
  out.put_fix_number(env->hpos)
     .put_fix_number(env->vpos)
     .put_symbol("EBEGIN")
     .special(arg)
     .put_symbol("EEND");
  output_hpos = output_vpos = -1;
  output_style.f = 0;
  output_draw_point_size = -1;
  output_line_thickness = -1;
  ndefined_styles = 0;
}

void ps_printer::do_file(char *arg, const environment *env)
{
  while (csspace(*arg))
    arg++;
  if (*arg == '\0') {
    error("missing argument to X file command");
    return;
  }
  const char *filename = arg;
  do {
    ++arg;
  } while (*arg != '\0' && *arg != ' ' && *arg != '\n');
  FILE *fp = fopen(filename, "r");
  if (!fp) {
    error("can't open `%1': %2", filename, strerror(errno));
    return;
  }
  out.put_fix_number(env->hpos)
     .put_fix_number(env->vpos)
     .put_symbol("EBEGIN")
     .include_file(fp)
     .put_symbol("EEND");
  fclose(fp);
  output_hpos = output_vpos = -1;
  output_style.f = 0;
  output_draw_point_size = -1;
  output_line_thickness = -1;
  ndefined_styles = 0;
}

void ps_printer::do_def(char *arg, const environment *)
{
  while (csspace(*arg))
    arg++;
  if (!check_line_lengths(arg)) {
    error("lines in X def command must not be more than 255 characters long");
    return;
  }
  defs += arg;
  if (*arg != '\0' && strchr(arg, '\0')[-1] != '\n')
    defs += '\n';
  ndefs++;
}

// Like def, but the first argument says how many definitions it contains.

void ps_printer::do_mdef(char *arg, const environment *)
{
  char *p;
  int n = (int)strtol(arg, &p, 10);
  if (n == 0 && p == arg) {
    error("first argument to X mdef must be an integer");
    return;
  }
  if (n < 0) {
    error("out of range argument `%1' to X mdef command", int(n));
    return;
  }
  arg = p;
  while (csspace(*arg))
    arg++;
  if (!check_line_lengths(arg)) {
    error("lines in X mdef command must not be more than 255 characters long");
    return;
  }
  defs += arg;
  if (*arg != '\0' && strchr(arg, '\0')[-1] != '\n')
    defs += '\n';
  ndefs += n;
}

void ps_printer::do_import(char *arg, const environment *env)
{
  while (*arg == ' ' || *arg == '\n')
    arg++;
  for (char *p = arg; *p != '\0' && *p != ' ' && *p != '\n'; p++)
    ;
  if (*p != '\0')
    *p++ = '\0';
  int parms[6];
  int nparms = 0;
  while (nparms < 6) {
    char *end;
    long n = strtol(p, &end, 10);
    if (n == 0 && end == p)
      break;
    parms[nparms++] = int(n);
    p = end;
  }
  if (csalpha(*p) && (p[1] == '\0' || p[1] == ' ' || p[1] == '\n')) {
    error("scaling indicators not allowed in arguments for X import command");
    return;
  }
  while (*p == ' ' || *p == '\n')
    p++;
  if (nparms < 5) {
    if (*p == '\0')
      error("too few arguments for X import command");
    else
      error("invalid argument `%1' for X import command", p);
    return;
  }
  if (*p != '\0') {
    error("superflous argument `%1' for X import command", p);
    return;
  }
  int llx = parms[0];
  int lly = parms[1];
  int urx = parms[2];
  int ury = parms[3];
  int desired_width = parms[4];
  int desired_height = parms[5];
  if (desired_width <= 0) {
    error("bad width argument `%1' for X import command: must be > 0",
	  desired_width);
    return;
  }
  if (nparms == 6 && desired_height <= 0) {
    error("bad height argument `%1' for X import command: must be > 0",
	  desired_height);
    return;
  }
  if (llx == urx) {
    error("llx and urx arguments for X import command must not be equal");
    return;
  }
  if (lly == ury) {
    error("lly and ury arguments for X import command must not be equal");
    return;
  }
  if (nparms == 5) {
    int old_wid = urx - llx;
    int old_ht = ury - lly;
    if (old_wid < 0)
      old_wid = -old_wid;
    if (old_ht < 0)
      old_ht = -old_ht;
    desired_height = int(desired_width*(double(old_ht)/double(old_wid)) + .5);
  }
  if (env->vpos - desired_height < 0)
    warning("top of imported graphic is above the top of the page");
  FILE *fp = fopen(arg, "r");
  if (fp == 0) {
    error("can't open `%1': %2", arg, strerror(errno));
    return;
  }
  merge_import_fonts(fp);
  out.put_literal_symbol("level1")
     .put_symbol("save")
     .put_symbol("def");
  out.put_number(llx)
     .put_number(lly)
     .put_fix_number(desired_width)
     .put_number(urx - llx)
     .put_fix_number(-desired_height)
     .put_number(ury - lly)
     .put_fix_number(env->hpos)
     .put_fix_number(env->vpos)
     .put_symbol("PICTURE");
  // Put our dictionary off the dictionary stack so that it isn't
  // zapped by unfriendly applications.
  out.put_symbol("end");
  // Disable showpage.
  out.put_literal_symbol("showpage")
     .put_delimiter('{')
     .put_delimiter('}')
     .put_symbol("def");
  out.begin_comment("BeginDocument:")
     .comment_arg(arg)
     .end_comment();
  out.include_file(fp);
  fclose(fp);
  out.simple_comment("EndDocument");
  // Clear junk off operand stack, to lessen chance of an invalidrestore.
  out.put_symbol("clear");
  out.put_symbol(dict_name)
     .put_symbol("begin");
  out.put_symbol("level1")
     .put_symbol("restore");
}

static void add_font(const char *name, int t, document_font **pp)
{
  for (; *pp; pp = &(*pp)->next)
    if (strcmp(name, (*pp)->name) == 0) {
      (*pp)->flags |= t;
      return;
    }
  *pp = new document_font(name);
  (*pp)->flags = t;
}

// Skip the rest of the current line, including any line termination
// characters.

static int skip_line(FILE *fp)
{
  // The spec says any combination of \n and \r is allowed as a line
  // termination.  It seems wrong to allow multiple \n's or multiple
  // \r's since this would mean we were swallowing blank lines.
  for (;;) {
    int c = getc(fp);
    if (c == '\n') {
      c = getc(fp);
      if (c == EOF)
	return 0;
      if (c != '\r')
	ungetc(c, fp);
      return 1;
    }
    if (c == '\r') {
      c = getc(fp);
      if (c == EOF)
	return 0;
      if (c != '\n')
	ungetc(c, fp);
      return 1;
    }
    if (c == EOF)
      break;
  }
  return 0;
}

static void parse_fonts_arg(FILE *fp, int type, document_font **font_listp,
			    int *at_endp)
{
  // More than enough room for a single name.
  char buf[256];
  int c = getc(fp);
  for (;;) {
    while (c == ' ' || c == '\t')
      c = getc(fp);
    if (c == EOF || c == '\n' || c == '\r')
      break;
    int i = 0;
    do {
      if (i >= sizeof(buf) - 1)
	return;
      if (c == '\0')
	return;
      buf[i++] = c;
      c = getc(fp);
    } while (c != '\n' && c != '\r' && c != ' ' && c != '\t' && c != EOF);
    buf[i++] = '\0';
    if (strcmp(buf, "(atend)") == 0 && at_endp != 0)
      *at_endp |= type;
    else
      add_font(buf, type, font_listp);
  }
  if (c == '\n' || c == '\r')
    ungetc(c, fp);
}

static document_font *read_document_fonts(FILE *fp)
{
  int last_comment = 0;
  int document_fonts_at_end = 0;
  // This says which comments we've seen in the header.  The first of each
  // is the significant one.
  int header_comments = 0;
  char buf[sizeof("DocumentSuppliedFonts:")];
#define MAGIC "%!PS-Adobe-"
  if (fread(buf, 1, sizeof(MAGIC)-1, fp) != sizeof(MAGIC)-1
      || memcmp(buf, MAGIC, sizeof(MAGIC)-1) != 0)
    return 0;
  document_font *font_list = 0;
  enum { HEADER, BODY, TRAILER } state = HEADER;
  int level = 0;
  while (skip_line(fp)) {
    if (state == BODY && document_fonts_at_end == 0)
      break;
    int c = getc(fp);
    if (c == '%')
      c = getc(fp);
    if (c == EOF)
      break;
    if (c != '%') {
      if (state == HEADER)
	state = BODY;
      if (c == '\n' || c == '\r')
	ungetc(c, fp);
      continue;
    }
    c = getc(fp);
    if (c == EOF)
      break;
    if (c == '+') {
      if (last_comment)
	parse_fonts_arg(fp, last_comment, &font_list, 0);
      continue;
    }
    last_comment = 0;
    int i = 0;
    while (c != '\r' && c != '\n' && c != ' ' && c != '\t' && c != EOF) {
      if (i >= sizeof(buf) - 1) {
	i = 0;
	break;
      }
      buf[i++] = c;
      if (c == ':')
	break;
      c = getc(fp);
    }
    if (c == '\r' || c == '\n')
      ungetc(c, fp);
    buf[i++] = '\0';
    if (strcmp(buf, "BeginDocument:") == 0)
      level++;
    else if (strcmp(buf, "EndDocument") == 0) {
      if (level > 0)
	level--;
    }
    else if (level == 0) {
      if (strcmp(buf, "Trailer") == 0)
	state = TRAILER;
      else if (strcmp(buf, "EndComments") == 0) {
	if (state == HEADER)
	  state = BODY;
      }
      else if (state == HEADER) {
	int comment_type = 0;
	if (strcmp(buf, "DocumentFonts:") == 0)
	  comment_type = document_font::LISTED;
	else if (strcmp(buf, "DocumentNeededFonts:") == 0)
	  comment_type = document_font::NEEDED;
	else if (strcmp(buf, "DocumentSuppliedFonts:") == 0)
	  comment_type = document_font::SUPPLIED;
	if (comment_type != 0 && !(header_comments & comment_type)) {
	  parse_fonts_arg(fp, comment_type, &font_list,
			  (comment_type == document_font::LISTED
			   ? &document_fonts_at_end
			   : 0));
	  last_comment = comment_type;
	  header_comments |= comment_type;
	}
      }
      else if (state == TRAILER && strcmp(buf, "DocumentFonts:") == 0) {
	parse_fonts_arg(fp, document_font::LISTED, &font_list, 0);
	last_comment = document_font::LISTED;
      }
    }
  }
  return font_list;
}

document_font *ps_printer::lookup_doc_font(const char *nm)
{
  for (document_font **p = &doc_fonts; *p; p = &(*p)->next)
    if (strcmp((*p)->name, nm) == 0)
      return *p;
  return *p = new document_font(nm);
}

void ps_printer::merge_download_fonts()
{
  for (document_font *p = doc_fonts; p; p = p->next)
    if (p->filename && (p->flags & document_font::NEEDED)) {
      char *path = 0;
      FILE *fp = font::open_file(p->filename, &path);
      delete path;
      if (fp) {
	document_font *depends = read_document_fonts(fp);
	fclose(fp);
	while (depends) {
	  document_font *tem = depends->next;
	  if (strcmp(p->name, depends->name) != 0) {
	    if ((depends->flags & document_font::LISTED)
		&& !(depends->flags & document_font::SUPPLIED))
	      depends->flags |= document_font::NEEDED;
	    // We ignore the LISTED bit from now on.
	    document_font *q = lookup_doc_font(depends->name);
	    if (q->filename && (depends->flags & document_font::NEEDED)) {
	      depend_list *dep = new depend_list;
	      dep->next = p->depends_on;
	      dep->p = q;
	      p->depends_on = dep;
	      if (!(q->flags & document_font::NEEDED)) {
		// Move q to the end of the list.
		for (document_font **pp = &doc_fonts;
		     *pp != q;
		     pp = &(*pp)->next)
		  ;
		*pp = q->next;
		q->next = 0;
		for (; *pp; pp = &(*pp)->next)
		  ;
		*pp = q;
	      }
	    }
	    q->flags |= depends->flags;
	  }
	  delete depends;
	  depends = tem;
	}
      }
      else {
	error("can't find font file `%1': %2", p->filename, strerror(errno));
	delete p->filename;
	p->filename = 0;
      }
    }
}

void ps_printer::merge_import_fonts(FILE *fp)
{
  document_font *p = read_document_fonts(fp);
  rewind(fp);
  while (p) {
    if ((p->flags & document_font::LISTED)
	&& !(p->flags & document_font::SUPPLIED))
      p->flags |= document_font::NEEDED;
    document_font *tem = p->next;
    document_font *q = lookup_doc_font(p->name);
    q->flags |= p->flags;
    delete p;
    p = tem;
  }
}

void ps_printer::merge_ps_fonts()
{
  for (font_pointer_list *f = font_list; f; f = f->next) {
    ps_font *psf = (ps_font *)(f->p);
    document_font *p = lookup_doc_font(psf->get_internal_name());
    p->flags |= document_font::NEEDED;
  }
}

void ps_printer::print_font_comment()
{
  out.begin_comment("DocumentFonts:");
  for (document_font *list = doc_fonts; list; list = list->next)
    if (list->flags)
      out.comment_arg(list->name);
  out.end_comment();
}

void ps_printer::print_supplied_font_comment()
{
  out.begin_comment("DocumentSuppliedFonts:");
  for (document_font *list = doc_fonts; list; list = list->next)
    if (((list->flags & document_font::NEEDED) && list->filename)
	|| ((list->flags & document_font::SUPPLIED)
#if 0
	    && !(list->flags & document_font::NEEDED)
#endif
	))
      out.comment_arg(list->name);
  out.end_comment();
}

void ps_printer::print_needed_font_comment()
{
  out.begin_comment("DocumentNeededFonts:");
  for (document_font *list = doc_fonts; list; list = list->next)
    if ((list->flags & document_font::NEEDED) && !list->filename)
      out.comment_arg(list->name);
  out.end_comment();
}

void ps_printer::print_include_font_comments()
{
  for (document_font *list = doc_fonts; list; list = list->next)
    if ((list->flags & document_font::NEEDED) && !list->filename)
      out.begin_comment("IncludeFont:").comment_arg(list->name).end_comment();
}
  
void ps_printer::download_fonts()
{
  for (document_font *p = doc_fonts; p; p = p->next)
    p->download(out);
}

void ps_printer::read_download_file()
{
  char *path = 0;
  FILE *fp = font::open_file("download", &path);
  if (!fp)
    fatal("can't find `download'");
  char buf[512];
  int lineno = 0;
  while (fgets(buf, sizeof(buf), fp)) {
    lineno++;
    char *p = strtok(buf, " \t\r\n");
    if (p == 0 || *p == '#')
      continue;
    char *q = strtok(0, " \t\r\n");
    if (!q)
      fatal_with_file_and_line(path, lineno, "missing filename");
    lookup_doc_font(p)->filename = strsave(q);
  }
  delete path;
  fclose(fp);
}

document_font::document_font(const char *s)
: name(strsave(s)), filename(0), flags(0), next(0), depends_on(0), mark(0)
{
  assert(name != 0);
}

document_font::~document_font()
{
  delete name;
  delete filename;
  while (depends_on) {
    depend_list *tem = depends_on->next;
    delete depends_on;
    depends_on = tem;
  }
}

void document_font::download(ps_output &out)
{
  if (!filename)
    return;
  if (!(flags & NEEDED))
      return;
  // Do a reverse topological sort on the dependency graph.
  if (mark == 0) {
    mark = -1;
    for (depend_list *p = depends_on; p; p = p->next)
      p->p->download(out);
    char *path = 0;
    FILE *fp = font::open_file(filename, &path);
    // This shouldn't normally happen because we checked that the file
    // exists in merge_download_fonts.
    if (!fp)
      fatal("can't open `%1': %2", filename, strerror(errno));
    out.begin_comment("BeginFont:").comment_arg(name).end_comment();
    out.begin_comment("BeginDocument:").comment_arg(filename).end_comment();
    out.include_file(fp);
    out.simple_comment("EndDocument");
    out.simple_comment("EndFont");
    fclose(fp);
    delete path;
    mark = 1;
  }
  else if (mark < 0)
    error("loop detected in font dependencies for `%1'", name);
}

printer *make_printer()
{
  return new ps_printer;
}

static void usage();

int main(int argc, char **argv)
{
  program_name = argv[0];
  static char stderr_buf[BUFSIZ];
  setbuf(stderr, stderr_buf);
  int c;
  while ((c = getopt(argc, argv, "F:lc:w:v")) != EOF)
    switch(c) {
    case 'v':
      {
	extern const char *version_string;
	fprintf(stderr, "grops version %s\n", version_string);
	fflush(stderr);
	break;
      }
    case 'c':
      if (sscanf(optarg, "%d", &ncopies) != 1 || ncopies <= 0) {
	error("bad number of copies `%s'", optarg);
	ncopies = 1;
      }
      break;
    case 'l':
      landscape_flag = 1;
      break;
    case 'F':
      font::command_line_font_dir(optarg);
      break;
    case 'w':
      if (sscanf(optarg, "%d", &linewidth) != 1 || linewidth < 0) {
	error("bad linewidth `%s'", optarg);
	linewidth = -1;
      }
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
  fprintf(stderr, "usage: %s [-l] [-c n] [-w n] [-F dir] [-v] [files ...]\n",
	  program_name);
  exit(1);
}
