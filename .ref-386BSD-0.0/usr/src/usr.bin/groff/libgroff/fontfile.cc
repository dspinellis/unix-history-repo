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
#include <assert.h>
#include <stdlib.h>
#include "font.h"
#include "lib.h"

#ifndef FONTPATH
#define FONTPATH "/usr/local/lib/font:/usr/lib/font"
#endif
const char *const FONT_ENV_VAR = "GROFF_FONT_PATH";

int font::res = 0;
int font::hor = 1;
int font::vert = 1;
int font::unitwidth = 0;
int font::paperwidth = 0;
int font::paperlength = 0;
int font::biggestfont = 0;
int font::spare2 = 0;
int font::sizescale = 1;
int font::tcommand = 0;
const char **font::font_name_table = 0;
int *font::sizes = 0;
char *font::dev_name = 0;
char *font::cl_font_dirs = 0;
const char *font::family = 0;
const char **font::style_table = 0;

void font::command_line_font_dir(const char *dir)
{
  if (cl_font_dirs == 0) {
    cl_font_dirs = new char[strlen(dir)+1];
    strcpy(cl_font_dirs, dir);
  }
  else {
    int len = strlen(cl_font_dirs);
    int need_colon = 0;
    if (len > 0 && cl_font_dirs[len-1] != ':')
      need_colon = 1;
    char *old_dirs = cl_font_dirs;
    cl_font_dirs = new char[len + need_colon + strlen(dir) + 1];
    strcpy(cl_font_dirs, old_dirs);
    if (need_colon)
      strcat(cl_font_dirs, ":");
    strcat(cl_font_dirs, dir);
    delete old_dirs;
  }
}

void font::forget_command_line_font_dirs()
{
  delete cl_font_dirs;
  cl_font_dirs = 0;
}

FILE *font::open_file(const char *name, char **pathp)
{
  assert(dev_name != 0);
  const char *dir_vec[3];
  dir_vec[0] = cl_font_dirs;
  dir_vec[1] = getenv(FONT_ENV_VAR);
  dir_vec[2] = FONTPATH;
  for (int i = 0; i < 3; i++)
    if (dir_vec[i] != 0) {
      const char *dirs = dir_vec[i];
      while (*dirs != '\0') {
	const char *p = strchr(dirs, ':');
	if (p != dirs) {
	  if (p == 0)
	    p = strchr(dirs, '\0');
	  int need_slash = 0;
	  if (p > dirs && p[-1] != '/')
	    need_slash = 1;
	  char *path = new char[(p - dirs) + need_slash + 3 
				+ strlen(dev_name) + 1 
				+ strlen(name) + 1];
	  memcpy(path, dirs, p - dirs);
	  path[p - dirs] = '\0';
	  if (need_slash)
	    strcat(path, "/");
	  strcat(path, "dev");
	  strcat(path, dev_name);
	  strcat(path, "/");
	  strcat(path, name);
	  FILE *fp = fopen(path, "r");
	  if (fp != 0) {
	    *pathp = path;
	    return fp;
	  }
	  delete path;
	  if (*p == '\0')
	    break;
	}
	dirs = p + 1;
      }
    }
  return 0;
}

void font::set_device_name(const char *s)
{
  dev_name = new char[strlen(s)+1];
  strcpy(dev_name, s);
}

const char *font::get_device_name()
{
  return dev_name;
}

