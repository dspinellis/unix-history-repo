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

#include "eqn.h"
#include "stringclass.h"

#ifndef DEVICE
#define DEVICE "ps"
#endif

extern int yyparse();

char start_delim = '\0';
char end_delim = '\0';
int non_empty_flag;
int inline_flag;
int draw_flag = 0;
int one_size_reduction_flag = 0;
int compatible_flag = 0;
int no_newline_in_delim_flag = 0;

int read_line(FILE *fp, string *p)
{
  p->clear();
  int c = -1;
  while ((c = getc(fp)) != EOF) {
    if (!illegal_input_char(c))
      *p += char(c);
    else
      error("illegal input character code `%1'", c);
    if (c == '\n')
      break;
  }
  current_lineno++;
  return p->length() > 0;
}

void do_file(const char *filename)
{
  string linebuf;
  string str;
  FILE *fp;
  if (strcmp(filename, "-") == 0)
    fp = stdin;
  else {
    fp = fopen(filename, "r");
    if (fp == 0)
      fatal("can't open `%1': %2", filename, strerror(errno));
  }
  printf(".lf 1 %s\n", filename);
  current_filename = filename;
  current_lineno = 0;
  while (read_line(fp, &linebuf)) {
    int i;
    if (linebuf.length() >= 4
	&& linebuf[0] == '.' && linebuf[1] == 'l' && linebuf[2] == 'f'
	&& (linebuf[3] == ' ' || linebuf[3] == '\n' || compatible_flag)) {
      put_string(linebuf, stdout);
      linebuf += '\0';
      if (interpret_lf_args(linebuf.contents() + 3))
	current_lineno--;
    }
    else if (linebuf.length() >= 4
	     && linebuf[0] == '.'
	     && linebuf[1] == 'E'
	     && linebuf[2] == 'Q'
	     && (linebuf[3] == ' ' || linebuf[3] == '\n' || compatible_flag)) {
      put_string(linebuf, stdout);
      int start_lineno = current_lineno + 1;
      str.clear();
      for (;;) {
	if (!read_line(fp, &linebuf))
	  fatal("end of file before .EN");
	if (linebuf.length() >= 3 && linebuf[0] == '.' && linebuf[1] == 'E') {
	  if (linebuf[2] == 'N'
	      && (linebuf.length() == 3 || linebuf[3] == ' '
		  || linebuf[3] == '\n' || compatible_flag))
	    break;
	  else if (linebuf[2] == 'Q' && linebuf.length() > 3
		   && (linebuf[3] == ' ' || linebuf[3] == '\n'
		       || compatible_flag))
	    fatal("nested .EQ");
	}
	str += linebuf;
      }
      str += '\0';
      start_string();
      init_lex(str.contents(), current_filename, start_lineno);
      non_empty_flag = 0;
      inline_flag = 0;
      yyparse();
      if (non_empty_flag) {
	printf(".lf %d\n", current_lineno - 1);
	output_string();
      }
      restore_compatibility();
      printf(".lf %d\n", current_lineno);
      put_string(linebuf, stdout);
    }
    else if (start_delim != '\0' && (i = linebuf.search(start_delim)) >= 0
	     && (i == 0 || linebuf[i - 1] != '\\')) {
      start_string();
      linebuf += '\0';
      char *ptr = &linebuf[0];
      inline_flag = 1;
      for (;;) {
	char *start = strchr(ptr, start_delim);
	if (start == 0) {
	  char *nl = strchr(ptr, '\n');
	  if (nl != 0)
	    *nl = '\0';
          do_text(ptr);
	  break;
        }
	if (no_newline_in_delim_flag && strchr(start + 1, end_delim) == 0) {
	  error("missing `%1'", end_delim);
	  char *nl = strchr(start + 1, '\n');
	  if (nl != 0)
	    *nl = '\0';
	  do_text(ptr);
	  break;
	}
	int start_lineno = current_lineno;
	*start = '\0';
	do_text(ptr);
	ptr = start + 1;
	str.clear();
	for (;;) {
	  char *end = strchr(ptr, end_delim);
	  if (end != 0) {
	    *end = '\0';
	    str += ptr;
	    ptr = end + 1;
	    break;
	  }
	  str += ptr;
	  if (!read_line(fp, &linebuf))
	    fatal("end of file before `%1'", end_delim);
	  linebuf += '\0';
	  ptr = &linebuf[0];
	}
	str += '\0';
	init_lex(str.contents(), current_filename, start_lineno);
	yyparse();
      }
      printf(".lf %d\n", current_lineno);
      output_string();
      restore_compatibility();
      printf(".lf %d\n", current_lineno + 1);
    }
    else {
      put_string(linebuf, stdout);
    }
  }
  if (fp != stdin)
    fclose(fp);
  current_filename = 0;
  current_lineno = 0;
}

void usage()
{
  fprintf(stderr, "usage: %s [ -vrDCN ] -dxx -fn -sn -pn -mn -Ts [ files ... ]\n",
	  program_name);
  exit(1);
}

int main(int argc, char **argv)
{
  program_name = argv[0];
  static char stderr_buf[BUFSIZ];
  setbuf(stderr, stderr_buf);
  int opt;
  const char *device = DEVICE;
  const char *tem = getenv("GROFF_TYPESETTER");
  if (tem)
    device = tem;
  while ((opt = getopt(argc, argv, "DCvd:f:p:s:m:T:rN")) != EOF)
    switch (opt) {
    case 'C':
      compatible_flag = 1;
      break;
    case 'v':
      {
	extern const char *version_string;
	fprintf(stderr, "GNU eqn version %s\n", version_string);
	fflush(stderr);
	break;
      }
    case 'd':
      if (optarg[0] == '\0' || optarg[1] == '\0')
	error("-d requires two character argument");
      else if (illegal_input_char(optarg[0]))
	error("bad delimiter `%1'", optarg[0]);
      else if (illegal_input_char(optarg[1]))
	error("bad delimiter `%1'", optarg[1]);
      else {
	start_delim = optarg[0];
	end_delim = optarg[1];
      }
      break;
    case 'f':
      set_gfont(optarg);
      break;
    case 'T':
      device = optarg;
      break;
    case 's':
      {
	int n;
	if (sscanf(optarg, "%d", &n) == 1)
	  set_gsize(n);
	else
	  error("bad size `%1'", optarg);
      }
      break;
    case 'p':
      {
	int n;
	if (sscanf(optarg, "%d", &n) == 1)
	  set_script_reduction(n);
	else
	  error("bad size `%1'", optarg);
      }
      break;
    case 'm':
      {
	int n;
	if (sscanf(optarg, "%d", &n) == 1)
	  set_minimum_size(n);
	else
	  error("bad size `%1'", optarg);
      }
      break;
    case 'r':
      one_size_reduction_flag = 1;
      break;
    case 'D':
      draw_flag = 1;
      break;
    case 'N':
      no_newline_in_delim_flag = 1;
      break;
    case '?':
      usage();
      break;
    default:
      assert(0);
    }
  init_table(device);
  init_char_table();
  if (optind >= argc)
    do_file("-");
  else
    for (int i = optind; i < argc; i++)
      do_file(argv[i]);
  if (ferror(stdout) || fflush(stdout) < 0)
    fatal("output error");
  exit(0);
}


