/* This may look like C code, but it is really -*- C++ -*- */

/* Handles parsing the Options provided to the user.

   Copyright (C) 1989 Free Software Foundation, Inc.
   written by Douglas C. Schmidt (schmidt@ics.uci.edu)

This file is part of GNU GPERF.

GNU GPERF is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU GPERF is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU GPERF; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* This module provides a uniform interface to the various options available
   to a user of the gperf hash function generator.  In addition to the
   run-time options, found in the Option_Type below, there is also the
   hash table Size and the Keys to be used in the hashing.
   The overall design of this module was an experiment in using C++
   classes as a mechanism to enhance centralization of option and
   and error handling, which tend to get out of hand in a C program. */

#ifndef options_h
#define options_h 1

#include "std-err.h"
#include "trace.h"

/* Enumerate the potential debugging Options. */

enum Option_Type
{
  DEBUG        = 01,            /* Enable debugging (prints diagnostics to Std_Err). */
  ORDER        = 02,            /* Apply ordering heuristic to speed-up search time. */
  ANSI         = 04,            /* Generate ANSI prototypes. */
  ALLCHARS     = 010,           /* Use all characters in hash function. */
  GNU          = 020,           /* Assume GNU extensions (primarily function inline). */
  TYPE         = 040,           /* Handle user-defined type structured keyword input. */
  RANDOM       = 0100,          /* Randomly initialize the associated values table. */
  DEFAULTCHARS = 0200,          /* Make default char positions be 1,$ (end of keyword). */
  SWITCH       = 0400,          /* Generate switch output to save space. */
  POINTER      = 01000,         /* Have in_word_set function return pointer, not boolean. */
  NOLENGTH     = 02000,         /* Don't include keyword length in hash computations. */
  LENTABLE     = 04000,         /* Generate a length table for string comparison. */
  DUP          = 010000,        /* Handle duplicate hash values for keywords. */
  FAST         = 020000,        /* Generate the hash function ``fast.'' */
  NOTYPE       = 040000,        /* Don't include user-defined type definition in output -- it's already defined elsewhere. */
  COMP         = 0100000,       /* Generate strncmp rather than strcmp. */
  GLOBAL       = 0200000,       /* Make the keyword table a global variable. */
  CONST        = 0400000,       /* Make the generated tables readonly (const). */
  CPLUSPLUS    = 01000000,      /* Generate C++ code. */
  C            = 02000000,      /* Generate C code. */
  ENUM	       = 04000000,	/* Use enum for constants. */
};

/* Define some useful constants (these don't really belong here, but I'm
   not sure where else to put them!).  These should be consts, but g++
   doesn't seem to do the right thing with them at the moment... ;-( */

enum 
{
  MAX_KEY_POS = 128 - 1,    /* Max size of each word's key set. */
  WORD_START = 1,           /* Signals the start of a word. */
  WORD_END = 0,             /* Signals the end of a word. */
  EOS = MAX_KEY_POS,        /* Signals end of the key list. */
};

/* Class manager for gperf program Options. */

class Options : private Std_Err
{
public:
                      Options (void);
                     ~Options (void);
  int                 operator[] (Option_Type option);
  void                operator() (int argc, char *argv[]);
  void                operator= (enum Option_Type);
  void                operator!= (enum Option_Type);
  static void         print_options (void);
  static void         set_asso_max (int r);
  static int          get_asso_max (void);
  static void         reset (void);
  static int          get (void);
  static int          get_iterations (void);
  static int          get_max_keysig_size (void);
  static void         set_keysig_size (int);
  static int          get_jump (void);
  static int          initial_value (void);
  static int          get_total_switches (void);
  static const char  *get_function_name (void);
  static const char  *get_key_name (void);
  static const char  *get_class_name (void);
  static const char  *get_hash_name (void);
  static const char  *get_delimiter (void);

private:
  static int          option_word;                        /* Holds the user-specified Options. */
  static int          total_switches;                     /* Number of switch statements to generate. */     
  static int          total_keysig_size;                 /* Total number of distinct key_positions. */
  static int          size;                               /* Range of the hash table. */
  static int          key_pos;                            /* Tracks current key position for Iterator. */
  static int          jump;                               /* Jump length when trying alternative values. */
  static int          initial_asso_value;                 /* Initial value for asso_values table. */
  static int          argument_count;                     /* Records count of command-line arguments. */
  static int          iterations;                         /* Amount to iterate when a collision occurs. */
  static char       **argument_vector;                    /* Stores a pointer to command-line vector. */
  static const char  *function_name;                      /* Names used for generated lookup function. */
  static const char  *key_name;                           /* Name used for keyword key. */
  static const char  *class_name;                         /* Name used for generated C++ class. */
  static const char  *hash_name;                          /* Name used for generated hash function. */
  static const char  *delimiters;                         /* Separates keywords from other attributes. */
  static char         key_positions[MAX_KEY_POS];         /* Contains user-specified key choices. */
  static int          key_sort (char *base, int len);     /* Sorts key positions in REVERSE order. */
  static void         usage (void);                       /* Prints proper program usage. */
};

/* Global option coordinator for the entire program. */
extern Options option;       

#ifdef __OPTIMIZE__

inline int  
Options::operator[] (Option_Type option) /* True if option enable, else false. */
{ 
  T (Trace t ("Options::operator[]");)
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

inline void 
Options::reset (void) /* Initializes the key Iterator. */
{ 
  T (Trace t ("Options::reset");)
  key_pos = 0;
}

inline int 
Options::get (void) /* Returns current key_position and advanced index. */
{ 
  T (Trace t ("Options::get");)
  return key_positions[key_pos++];
}

inline void 
Options::set_asso_max (int r) /* Sets the size of the table size. */
{ 
  T (Trace t ("Options::set_asso_max");)
  size = r;
}

inline int 
Options::get_asso_max (void) /* Returns the size of the table size. */
{ 
  T (Trace t ("Options::get_asso_max");)
  return size;
}

inline int 
Options::get_max_keysig_size (void) /* Returns total distinct key positions. */
{ 
  T (Trace t ("Options::get_max_keysig_size");)
  return total_keysig_size;
}

inline void
Options::set_keysig_size (int size) /* Sets total distinct key positions. */
{ 
  T (Trace t ("Options::set_keysig_size");)
  total_keysig_size = size;
}

inline int 
Options::get_jump (void) /* Returns the jump value. */
{ 
  T (Trace t ("Options::get_jump");)
  return jump;
}

inline const char *
Options::get_function_name (void) /* Returns the generated function name. */
{ 
  T (Trace t ("Options::get_function_name");)
  return function_name;
}

inline const char *
Options::get_key_name (void) /* Returns the keyword key name. */
{
  T (Trace t ("Options::get_key_name");)
  return key_name;
}

inline const char *
Options::get_hash_name (void) /* Returns the hash function name. */
{
  T (Trace t ("Options::get_hash_name");)
  return hash_name;
}

inline const char *
Options::get_class_name (void) /* Returns the generated class name. */
{
  T (Trace t ("Options::get_class_name");)
  return class_name;
}

inline int 
Options::initial_value (void) /* Returns the initial associated character value. */
{ 
  T (Trace t ("Options::initial_value");)
  return initial_asso_value;
}

inline int 
Options::get_iterations (void) /* Returns the iterations value. */
{ 
  T (Trace t ("Options::get_iterations");)
  return iterations;
}

inline const char *
Options::get_delimiter () /* Returns the string used to delimit keywords from other attributes. */
{
  T (Trace t ("Options::get_delimiter");)
  return delimiters;
}

inline int
Options::get_total_switches () /* Gets the total number of switch statements to generate. */
{
  T (Trace t ("Options::get_total_switches");)
  return total_switches;
}
#endif

#endif
