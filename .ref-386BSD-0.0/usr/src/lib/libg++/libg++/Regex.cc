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

/* 
  Regex class implementation
 */

#ifdef __GNUG__
#pragma implementation
#endif
#include <std.h>
#include <ctype.h>
#include <values.h>
#include <new.h>
#include <builtin.h>

// extern "C" {
#include <regex.h>
// }

#include <Regex.h>

Regex::~Regex()
{
  delete(buf->buffer);
  delete(buf->fastmap);
  delete(buf);
  delete(reg);
}

Regex::Regex(const char* t, int fast, int bufsize, 
               const char* transtable)
{
  int tlen = (t == 0)? 0 : strlen(t);
  buf = new re_pattern_buffer;
  reg = new re_registers;
  if (fast)
    buf->fastmap = new char[256];
  else
    buf->fastmap = 0;
  buf->translate = (char*)transtable;
  if (tlen > bufsize)
    bufsize = tlen;
  buf->allocated = bufsize;
  buf->buffer = new char [buf->allocated];
  char* msg = re_compile_pattern((char*)t, tlen, buf);
  if (msg != 0)
    (*lib_error_handler)("Regex", msg);
  else if (fast)
    re_compile_fastmap(buf);
}

int Regex::match_info(int& start, int& length, int nth) const
{
  if ((unsigned)(nth) >= RE_NREGS)
    return 0;
  else
  {
    start = reg->start[nth];
    length = reg->end[nth] - start;
    return start >= 0 && length >= 0;
  }
}

int Regex::search(const char* s, int len, int& matchlen, int startpos) const
{
  int matchpos, pos, range;
  if (startpos >= 0)
  {
    pos = startpos;
    range = len - startpos;
  }
  else
  {
    pos = len + startpos;
    range = -pos;
  }
  matchpos = re_search_2(buf, 0, 0, (char*)s, len, pos, range, reg, len);
  if (matchpos >= 0)
    matchlen = reg->end[0] - reg->start[0];
  else
    matchlen = 0;
  return matchpos;
}

int Regex::match(const char*s, int len, int p) const
{
  if (p < 0)
  {
    p += len;
    if (p > len)
      return -1;
    return re_match_2(buf, 0, 0, (unsigned char*)s, p, 0, reg, p);
  }
  else if (p > len)
    return -1;
  else
    return re_match_2(buf, 0, 0, (unsigned char*)s, len, p, reg, len);
}

int Regex::OK() const
{
// can't verify much, since we've lost the original string
  int v = buf != 0;             // have a regex buf
  v &= buf->buffer != 0;        // with a pat
  if (!v) (*lib_error_handler)("Regex", "invariant failure");
  return v;
}

/*
 some built-in Regular expressions
*/

const Regex RXwhite("[ \n\t\r\v\f]+", 1);
const Regex RXint("-?[0-9]+", 1);
const Regex RXdouble("-?\\(\\([0-9]+\\.[0-9]*\\)\\|\\([0-9]+\\)\\|\\(\\.[0-9]+\\)\\)\\([eE][---+]?[0-9]+\\)?", 1, 200);
const Regex RXalpha("[A-Za-z]+", 1);
const Regex RXlowercase("[a-z]+", 1);
const Regex RXuppercase("[A-Z]+", 1);
const Regex RXalphanum("[0-9A-Za-z]+", 1);
const Regex RXidentifier("[A-Za-z_][A-Za-z0-9_]*", 1);

