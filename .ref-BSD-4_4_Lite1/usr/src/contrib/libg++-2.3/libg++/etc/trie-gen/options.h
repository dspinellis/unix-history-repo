/* This may look like C code, but it is really -*- C++ -*- */

/* Handles parsing the Options provided to the user.

   Copyright (C) 1989 Free Software Foundation, Inc.
   written by Douglas C. Schmidt (schmidt@ics.uci.edu)

This file is part of GNU TRIE-GEN.

GNU TRIE-GEN is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU TRIE-GEN is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU TRIE-GEN; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* This module provides a uniform interface to the various options available
   to a user of the minimal-prefix trie generator.
   The overall design of this module was an experiment in using C++
   classes as a mechanism to enhance centralization of option and
   and error handling, which tend to get out of hand in a C program. */

#ifndef options_h
#define options_h 1

/* Enumerate the potential debugging Options. */

enum Option_Type
{
  DEBUG        = 01,  /* Enable debugging option. */
  COMPACT      = 02,  /* Compact the output tables. */
  FULL         = 04,  /* Generate a full table. */
  CONST        = 010, /* Make the generated tables readonly (const). */
};

/* Class manager for program options. */

class Options 
{
public:
                      Options (void);
                     ~Options (void);
  const char         *program_name (void);
  int                 operator[] (Option_Type option);
  void                operator() (int argc, char *argv[]);
  void                operator= (enum Option_Type);
  void                operator!= (enum Option_Type);
  static void         print_options (void);

private:
  static int          option_word;                        /* Holds the user-specified Options. */
  static int          argument_count;
  static char       **argument_vector;
  static void         usage (void);                       /* Prints proper program usage. */
};

/* Global option coordinator for the entire program. */
extern Options option;       

#ifdef __OPTIMIZE__
inline const char *
Options::program_name (void)
{
  return argument_vector[0];
}

inline int  
Options::operator[] (Option_Type option) /* True if option enable, else false. */
{ 
  return option_word & option;
}

inline void
Options::operator = (enum Option_Type opt) /* Enables option OPT. */
{
        option_word |= opt;
}

inline void
Options::operator != (enum Option_Type opt) /* Disables option OPT. */
{
        option_word &= ~opt;
}

#endif
#endif
