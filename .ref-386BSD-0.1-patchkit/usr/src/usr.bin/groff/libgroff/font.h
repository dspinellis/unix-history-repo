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


struct font_kern_list;
struct font_char_metric;

class font {
public:
  enum {
    LIG_ff = 1,
    LIG_fi = 2,
    LIG_fl = 4,
    LIG_ffi = 8,
    LIG_ffl = 16
    };

  virtual ~font();
  int contains(int index);
  int is_special();
  int get_width(int index, int point_size);
  int get_height(int index, int point_size);
  int get_depth(int index, int point_size);
  int get_space_width(int point_size);
  int get_character_type(int index);
  int get_kern(int index1, int index2, int point_size);
  int get_skew(int index, int point_size, int slant);
  int has_ligature(int);
  int get_italic_correction(int index, int point_size);
  int get_left_italic_correction(int index, int point_size);
  int get_subscript_correction(int index, int point_size);
  unsigned char get_code(int i);
  const char *get_name();
  const char *get_internal_name();

  static font *load_font(const char *);
  static void command_line_font_dir(const char *path);
  static void forget_command_line_font_dirs();
  static void set_device_name(const char *);
  static const char *get_device_name();
  static FILE *open_file(const char *name, char **pathp);
  static int load_desc();
  static int name_to_index(const char *);
  static int number_to_index(unsigned char);

  static int res;
  static int hor;
  static int vert;
  static int unitwidth;
  static int paperwidth;
  static int paperlength;
  static int biggestfont;
  static int spare2;
  static int sizescale;
  static int tcommand;

  static const char **font_name_table;
  static const char **style_table;
  static const char *family;
  static int *sizes;
private:
  unsigned ligatures;
  font_kern_list **kern_hash_table;
  int space_width;
  short *ch_index;
  int nindices;
  font_char_metric *ch;
  int ch_used;
  int ch_size;
  int special;
  char *name;
  char *internalname;
  double slant;

  static char *dev_name;
  static char *cl_font_dirs;

  enum { KERN_HASH_TABLE_SIZE = 503 };

  void add_entry(int index, const font_char_metric &);
  void copy_entry(int new_index, int old_index);
  void add_kern(int index1, int index2, int amount);
  static int hash_kern(int i1, int i2);
  void alloc_ch_index(int);
  void extend_ch();
  void compact();

  static int scale(int w, int pointsize);
  virtual void handle_unknown_font_command(int argc, const char **argv);
protected:
  font(const char *);
  int load();
};
