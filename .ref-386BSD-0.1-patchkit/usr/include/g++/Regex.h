// This may look like C code, but it is really -*- C++ -*-
/* 
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  
*/


#ifndef _Regex_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _Regex_h 1

struct re_pattern_buffer;       // defined elsewhere
struct re_registers;

class Regex
{
private:

                     Regex(const Regex&) {}  // no X(X&)
  void               operator = (const Regex&) {} // no assignment

protected:
  re_pattern_buffer* buf;
  re_registers*      reg;

public:
                     Regex(const char* t, 
                           int fast = 0, 
                           int bufsize = 40, 
                           const char* transtable = 0);

                    ~Regex();

  int                match(const char* s, int len, int pos = 0) const;
  int                search(const char* s, int len, 
                            int& matchlen, int startpos = 0) const;
  int                match_info(int& start, int& length, int nth = 0) const;

  int                OK() const;  // representation invariant
};

// some built in regular expressions

extern const Regex RXwhite;          // = "[ \n\t\r\v\f]+"
extern const Regex RXint;            // = "-?[0-9]+"
extern const Regex RXdouble;         // = "-?\\(\\([0-9]+\\.[0-9]*\\)\\|
                                     //    \\([0-9]+\\)\\|\\(\\.[0-9]+\\)\\)
                                     //    \\([eE][---+]?[0-9]+\\)?"
extern const Regex RXalpha;          // = "[A-Za-z]+"
extern const Regex RXlowercase;      // = "[a-z]+"
extern const Regex RXuppercase;      // = "[A-Z]+"
extern const Regex RXalphanum;       // = "[0-9A-Za-z]+"
extern const Regex RXidentifier;     // = "[A-Za-z_][A-Za-z0-9_]*"


#endif
