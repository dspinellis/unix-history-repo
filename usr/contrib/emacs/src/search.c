/* String search routines for GNU Emacs.
   Copyright (C) 1985 Richard M. Stallman.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


#include <ctype.h>
#include "config.h"
#include "lisp.h"
#include "syntax.h"
#include "buffer.h"
#include "commands.h"
#include "regex.h"

unsigned char downcase_table[0400] = {0};	/* folds upper to lower case */

/* We compile regexps into this buffer and then use it for searching. */

static struct re_pattern_buffer searchbuf;

char search_fastmap[0400];

/* Last regexp we compiled */

Lisp_Object last_regexp;

/* Every call to re_match, etc., must pass &search_regs as the regs argument
 unless you can show it is unnecessary (i.e., if re_match is certainly going
 to be called again before region-around-match can be called).  */

static struct re_registers search_regs;

/* error condition signalled when regexp compile_pattern fails */

Lisp_Object Qinvalid_regexp;

/* Compile a regexp and signal a Lisp error if anything goes wrong.  */

compile_pattern (pattern, bufp, translate)
     Lisp_Object pattern;
     struct re_pattern_buffer *bufp;
     char *translate;
{
  char *val;
  Lisp_Object dummy;

  if (EQ (pattern, last_regexp)
      && translate == bufp->translate)
    return;
  last_regexp = Qnil;
  bufp->translate = translate;
  val = re_compile_pattern (XSTRING (pattern)->data,
			    XSTRING (pattern)->size,
			    bufp);
  if (val)
    {
      dummy = build_string (val);
      while (1)
	Fsignal (Qinvalid_regexp, Fcons (dummy, Qnil));
    }
  last_regexp = pattern;
  return;
}

/* Error condition used for failing searches */
Lisp_Object Qsearch_failed;

Lisp_Object
signal_failure (arg)
     Lisp_Object arg;
{
  Fsignal (Qsearch_failed, Fcons (arg, Qnil));
  return Qnil;
}

DEFUN ("looking-at", Flooking_at, Slooking_at, 1, 1, 0,
  "t if text after point matches regular expression PAT.")
  (string)
     Lisp_Object string;
{
  Lisp_Object val;
  unsigned char *p1, *p2;
  int s1, s2;
  register int i;

  CHECK_STRING (string, 0);
  compile_pattern (string, &searchbuf,
		   !NULL (bf_cur->case_fold_search) ? (char *) downcase_table : 0);

  immediate_quit = 1;
  QUIT;			/* Do a pending quit right away, to avoid paradoxical behavior */

  /* Get pointers and sizes of the two strings
     that make up the visible portion of the buffer. */

  p1 = bf_p1 + bf_head_clip;
  s1 = bf_s1 - (bf_head_clip - 1);
  p2 = bf_p2 + bf_s1 + 1;
  s2 = bf_s2 - bf_tail_clip;
  if (s1 < 0)
    {
      p2 -= s1;
      s2 += s1;
      s1 = 0;
    }
  if (s2 < 0)
    {
      s1 += s2;
      s2 = 0;
    }
  
  val = (0 <= re_match_2 (&searchbuf, p1, s1, p2, s2,
			  point - FirstCharacter, &search_regs,
			  NumCharacters + 1 - FirstCharacter)
	 ? Qt : Qnil);
  for (i = 0; i < RE_NREGS; i++)
    {
      search_regs.start[i] += FirstCharacter - 1;
      search_regs.end[i] += FirstCharacter - 1;
    }
  immediate_quit = 0;
  return val;
}

DEFUN ("string-match", Fstring_match, Sstring_match, 2, 3, 0,
  "Return index of start of first match for REGEXP in STRING, or nil.\n\
If third arg START is non-nil, start search at that index in STRING.\n\
For index of first char beyond the match, do (match-end 0).\n\
match-end and match-beginning also give indices of substrings\n\
matched by parenthesis constructs in the pattern.")
  (regexp, string, start)
     Lisp_Object regexp, string, start;
{
  int val;
  int s;

  CHECK_STRING (regexp, 0);
  CHECK_STRING (string, 1);

  if (NULL (start))
    s = 0;
  else
    {
      CHECK_NUMBER (start, 2);
      s = XINT (start);
    }

  compile_pattern (regexp, &searchbuf,
		   !NULL (bf_cur->case_fold_search) ? (char *) downcase_table : 0);
  val = re_search (&searchbuf, XSTRING (string)->data, XSTRING (string)->size,
			       s, XSTRING (string)->size - s, &search_regs);
  /* Correct for propensity of match-beginning and match-end
     to add 1 to each of these (which is correct for buffer positions
     since they are origin-1, but not for indices in strings).  */
  for (s = 0; s < RE_NREGS; s++)
    {
      search_regs.start[s]--;
      search_regs.end[s]--;
    }
  if (val < 0) return Qnil;
  return make_number (val);
}

DEFUN ("scan-buffer", Fscan_buffer, Sscan_buffer, 3, 3, 0,
  "Scan from character number FROM for COUNT occurrences of character C.\n\
Returns the character number of the position after the character found.\n\
If not found, returns char number of beginning or end of buffer.\n\
Note that this does -not- take take  case-fold-search  into consideration.")
  (from, count, c)
     Lisp_Object from, count, c;
{
  CHECK_NUMBER_COERCE_MARKER (from, 0);
  CHECK_NUMBER (count, 1);
  CHECK_NUMBER (c, 2);

  return make_number (ScanBf (XINT (c), XINT (from), XINT (count)));
}

ScanBf (target, pos, cnt)
     register int target, pos, cnt;
{
  register int end;
  if (cnt > 0)
    {
      end = NumCharacters + 1;
      while (pos < end)
	{
	  if (CharAt (pos) == target && !--cnt)
	    return pos + 1;
	  pos++;
	}
      return pos;
    }
  if (cnt < 0)
    {
      end = FirstCharacter;
      do pos--;
      while (pos >= end &&
	     (CharAt (pos) != target || ++cnt));
    }
  return pos + 1;
}

DEFUN ("skip-chars-forward", Fskip_chars_forward, Sskip_chars_forward, 1, 2, 0,
  "Move point forward, stopping before a char not in CHARS, or at position LIM.\n\
CHARS is like the inside of a [...] in a regular expression\n\
except that ] is never special and \\ quotes ^, - or \\.\n\
Thus, with arg \"a-zA-Z\", this skips letters stopping before first nonletter.\n\
With arg \"^a-zA-Z\", skips nonletters stopping before first letter.")
  (string, lim)
     Lisp_Object string, lim;
{
  skip_chars (1, string, lim);
  return Qnil;
}

DEFUN ("skip-chars-backward", Fskip_chars_backward, Sskip_chars_backward, 1, 2, 0,
  "Move point backward, stopping after a char not in CHARS, or at position LIM.\n\
See skip-chars-forward for details.")
  (string, lim)
     Lisp_Object string, lim;
{
  skip_chars (0, string, lim);
  return Qnil;
}

skip_chars (forwardp, string, lim)
     int forwardp;
     Lisp_Object string, lim;
{
  register unsigned char *p, *pend;
  register unsigned char c;
  unsigned char fastmap[0400];
  int negate = 0;
  register int i;

  CHECK_STRING (string, 0);

  if (NULL (lim))
    XSETINT (lim, forwardp ? NumCharacters + 1 : FirstCharacter);
  else
    CHECK_NUMBER_COERCE_MARKER (lim, 1);

  p = XSTRING (string)->data;
  pend = p + XSTRING (string)->size;
  bzero (fastmap, sizeof fastmap);

  if (p != pend && *p == '^')
    {
      negate = 1; p++;
    }

  /* Find the characters specified and set their elements of fastmap.  */

  while (p != pend)
    {
      c = *p++;
      if (*p == '\\')
        {
	  if (p == pend) break;
	  c = *p++;
	}
      if (p != pend && *p == '-')
	{
	  p++;
	  if (p == pend) break;
	  while (c <= *p)
	    {
	      fastmap[c] = 1;
	      c++;
	    }
	  p++;
	}
      else
	fastmap[c] = 1;
    }

  /* If ^ was the first character, complement the fastmap. */

  if (negate)
    for (i = 0; i < sizeof fastmap; i++)
      fastmap[i] ^= 1;

  if (forwardp)
    {
      while (point < XINT (lim) && fastmap[CharAt (point)])
	PointRight (1);
    }
  else
    {
      while (point > XINT (lim) && fastmap[CharAt (point - 1)])
	PointLeft (1);
    }
}

/* Subroutines of Lisp buffer search functions. */

static Lisp_Object
search_command (string, bound, noerror, count, direction, RE)
     Lisp_Object string, bound, noerror, count;
     int direction;
     int RE;
{
  register int np;
  int lim;
  int n = direction;

  if (!NULL (count))
    {
      CHECK_NUMBER (count, 3);
      n *= XINT (count);
    }

  CHECK_STRING (string, 0);
  if (NULL (bound))
    lim = n > 0 ? NumCharacters + 1 : FirstCharacter;
  else
    {
      CHECK_NUMBER_COERCE_MARKER (bound, 1);
      lim = XINT (bound);
      if (n > 0 ? lim < point : lim > point)
	error ("Invalid search bound (wrong side of point)");
      if (lim > NumCharacters + 1)
	lim = NumCharacters + 1;
      if (lim < FirstCharacter)
	lim = FirstCharacter;
    }

  np = search_buffer (string, point, lim, n, RE,
		      !NULL (bf_cur->case_fold_search) ? downcase_table : 0);
  if (np == 0)
    {
      if (NULL (noerror))
	return signal_failure (string);
      if (!EQ (noerror, Qt))
	SetPoint (lim);
      return Qnil;
    }
  else
    SetPoint (np);

  return Qt;
}

/* search for the n'th occurrence of `string' in the current buffer,
   starting at position `from' and stopping at position `lim',
   treating `pat' as a literal string if `RE' is false or as
   a regular expression if `RE' is true.

   If `n' is positive, searching is forward and `lim' must be greater than `from'.
   If `n' is negative, searching is backward and `lim' must be less than `from'.

   Returns 0 if `n' occurrences are not found,
   or else the position at the beginning of the `n'th occurrence (if searching backward)
   or the end (if searching forward).  */

search_buffer (string, from, lim, n, RE, trt)
     Lisp_Object string;
     int from;
     register int lim;
     int n;
     int RE;
     unsigned char *trt;
{
  register int pos = from;
  unsigned char *pat = XSTRING (string)->data;
  register int len = XSTRING (string)->size;
  register int i, j;
  unsigned char *p1, *p2;
  int s1, s2;

  immediate_quit = 1;	/* Quit immediately if user types ^G,
			   because letting this function finish can take too long. */
  QUIT;			/* Do a pending quit right away, to avoid paradoxical behavior */

  if (RE)
    {
      compile_pattern (string, &searchbuf, (char *) trt);

      /* Get pointers and sizes of the two strings
	 that make up the visible portion of the buffer. */

      p1 = bf_p1 + bf_head_clip;
      s1 = bf_s1 - (bf_head_clip - 1);
      p2 = bf_p2 + bf_s1 + 1;
      s2 = bf_s2 - bf_tail_clip;
      if (s1 < 0)
	{
	  p2 -= s1;
	  s2 += s1;
	  s1 = 0;
	}
      if (s2 < 0)
	{
	  s1 += s2;
	  s2 = 0;
	}
    }

  while (n < 0)
    {
      if (!RE)
	{
          pos -= len;
	  if (trt && !(trt == downcase_table && !isalpha (pat[0])))
	    {
	      j = trt[pat[0]];
	      while (pos >= lim
		     && (j != trt[CharAt (pos)]
			 || bcmp_buffer_translated (pat, len, pos, trt)))
		pos--;
	    }
	  else
	    {
	      j = pat[0];
	      while (pos >= lim
		     && (j != CharAt (pos)
			 || bcmp_buffer_translated (pat, len, pos, trt)))
		pos--;
	    }

	  if (pos < lim)
	    {
	      immediate_quit = 0;
	      return 0;
	    }
	  search_regs.start[0] = pos - 1;
	  search_regs.end[0] = pos - 1 + len;
	}
      else
	{
	  if (re_search_2 (&searchbuf, p1, s1, p2, s2,
			   pos - FirstCharacter, lim - pos, &search_regs,
			   /* Don't allow match past current point */
			   pos - FirstCharacter)
	      >= 0)
	    {
	      j = FirstCharacter - 1;
	      for (i = 0; i < RE_NREGS; i++)
		{
		  search_regs.start[i] += j;
		  search_regs.end[i] += j;
		}
	      /* Set pos to the new position. */
	      pos = search_regs.start[0] + 1;
	    }
	  else
	    {
	      immediate_quit = 0;
	      return 0;
	    }
	}
      n++;
    }

  while (n > 0)
    {
      if (!RE)
	{
	  lim -= len;
	  if (trt && !(trt == downcase_table && !isalpha (pat[0])))
	    {
	      j = trt[pat[0]];
	      while (pos <= lim
		     && (j != trt[CharAt (pos)]
			 || bcmp_buffer_translated (pat, len, pos, trt)))
		pos++;
	    }
	  else
	    {
	      j = pat[0];
	      while (pos <= lim
		     && (j != CharAt (pos)
			 || bcmp_buffer_translated (pat, len, pos, trt)))
		pos++;
	    }

	  if (pos > lim)
	    {
	      immediate_quit = 0;
	      return 0;
	    }

	  lim += len;

	  search_regs.start[0] = pos - 1;
	  pos += len;
	  search_regs.end[0] = pos - 1;
	}
      else
	{
	  if (re_search_2 (&searchbuf, p1, s1, p2, s2,
			   pos - FirstCharacter, lim - pos, &search_regs,
			   lim - FirstCharacter)
	      >= 0)
	    {
	      j = FirstCharacter - 1;
	      for (i = 0; i < RE_NREGS; i++)
		{
		  search_regs.start[i] += j;
		  search_regs.end[i] += j;
		}
	      pos = 1 + search_regs.end[0];
	    }
	  else
	    {
	      immediate_quit = 0;
	      return 0;
	    }
	}
      n--;
    }
  immediate_quit = 0;
  return pos;
}

/* Return nonzero unless the `len' characters in the buffer starting at position `pos'
  match the `len' characters at `pat', with all characters going through the
  translate table `trt' if `trt' is nonzero.  */

static int
bcmp_buffer_translated (pat, len, pos, trt)
     unsigned char *pat;
     int len;
     int pos;
     register char *trt;
{
  int dist1 = 0;
  register int i;
  register unsigned char *p1, *p2;

  if (pos - 1 < bf_s1)
    {
      p1 = pat, p2 = &CharAt (pos);
      dist1 = bf_s1 - (pos - 1);
      if (dist1 > len) dist1 = len;
      i = dist1;

      if (trt)
	{
	  for (; i; i--)
	    if (trt[*p1++] != trt [*p2++]) return 1;
	}
      else
	{
	  for (; i; i--)
	    if (*p1++ != *p2++) return 1;
	}
    }

  if (dist1 < len)
    {
      p1 = pat + dist1, p2 = &CharAt (pos + dist1);
      i = len - dist1;

      if (trt)
	{
	  for (; i; i--)
	    if (trt[*p1++] != trt [*p2++]) return 1;
	}
      else
	{
	  for (; i; i--)
	    if (*p1++ != *p2++) return 1;
	}
    }
  return 0;
}

/* Given a string of words separated by word delimiters,
  compute a regexp that matches those exact words
  separated by arbitrary punctuation.  */

static Lisp_Object
wordify (string)
     Lisp_Object string;
{
  register unsigned char *p, *o;
  register int i, len, punct_count = 0, word_count = 0;
  Lisp_Object val;

  CHECK_STRING (string, 0);
  p = XSTRING (string)->data;
  len = XSTRING (string)->size;

  for (i = 0; i < len; i++)
    if (SYNTAX (p[i]) != Sword)
      {
	punct_count++;
	if (i > 0 && SYNTAX (p[i-1]) == Sword) word_count++;
      }
  if (SYNTAX (p[len-1]) == Sword) word_count++;
  if (!word_count) return build_string ("");

  val = make_string (p, len - punct_count + 5 * (word_count - 1) + 4);

  o = XSTRING (val)->data;
  *o++ = '\\';
  *o++ = 'b';

  for (i = 0; i < len; i++)
    if (SYNTAX (p[i]) == Sword)
      *o++ = p[i];
    else if (i > 0 && SYNTAX (p[i-1]) == Sword && --word_count)
      {
	*o++ = '\\';
	*o++ = 'W';
	*o++ = '\\';
	*o++ = 'W';
	*o++ = '*';
      }

  *o++ = '\\';
  *o++ = 'b';

  return val;
}

DEFUN ("search-backward", Fsearch_backward, Ssearch_backward, 1, 4,
  "sSearch backward: ",
  "Search backward from point for STRING.\n\
Set point to the beginning of the occurrence found, and return t.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must not extend before that position.\n\
Optional third argument, if t, means if fail just return nil (no error).\n\
 If not nil and not t, position at limit of search and return nil.\n\
Optional fourth argument is repeat count--search for successive occurrences.")
  (string, bound, noerror, count)
     Lisp_Object string, bound, noerror, count;
{
  return search_command (string, bound, noerror, count, -1, 0);
}

DEFUN ("search-forward", Fsearch_forward, Ssearch_forward, 1, 4, "sSearch: ",
  "Search forward from point for STRING.\n\
Set point to the end of the occurrence found, and return t.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must not extend after that position.\n\
Optional third argument, if t, means if fail just return nil (no error).\n\
  If not nil and not t, move to limit of search and return nil.\n\
Optional fourth argument is repeat count--search for successive occurrences.")
  (string, bound, noerror, count)
     Lisp_Object string, bound, noerror, count;
{
  return search_command (string, bound, noerror, count, 1, 0);
}

DEFUN ("word-search-backward", Fword_search_backward, Sword_search_backward, 1, 4,
  "sWord search backward: ",
  "Search backward from point for STRING, ignoring differences in punctuation.\n\
Set point to the beginning of the occurrence found, and return t.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must not extend before that position.\n\
Optional third argument, if t, means if fail just return nil (no error).\n\
  If not nil and not t, move to limit of search and return nil.\n\
Optional fourth argument is repeat count--search for successive occurrences.")
  (string, bound, noerror, count)
     Lisp_Object string, bound, noerror, count;
{
  return search_command (wordify (string), bound, noerror, count, -1, 1);
}

DEFUN ("word-search-forward", Fword_search_forward, Sword_search_forward, 1, 4,
  "sWord search: ",
  "Search forward from point for STRING, ignoring differences in punctuation.\n\
Set point to the end of the occurrence found, and return t.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must not extend after that position.\n\
Optional third argument, if t, means if fail just return nil (no error).\n\
  If not nil and not t, move to limit of search and return nil.\n\
Optional fourth argument is repeat count--search for successive occurrences.")
  (string, bound, noerror, count)
     Lisp_Object string, bound, noerror, count;
{
  return search_command (wordify (string), bound, noerror, count, 1, 1);
}

DEFUN ("re-search-backward", Fre_search_backward, Sre_search_backward, 1, 4,
  "sRE search backward: ",
  "Search backward from point for regular expression REGEXP.\n\
Set point to the beginning of the occurrence found, and return t.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must not extend before that position.\n\
Optional third argument, if t, means if fail just return nil (no error).\n\
  If not nil and not t, move to limit of search and return nil.\n\
Optional fourth argument is repeat count--search for successive occurrences.\n\
See also the functions match-beginning and match-end and replace-match.")
  (string, bound, noerror, count)
     Lisp_Object string, bound, noerror, count;
{
  return search_command (string, bound, noerror, count, -1, 1);
}

DEFUN ("re-search-forward", Fre_search_forward, Sre_search_forward, 1, 4,
  "sRE search: ",
  "Search forward from point for regular expression REGEXP.\n\
Set point to the end of the occurrence found, and return t.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must not extend after that position.\n\
Optional third argument, if t, means if fail just return nil (no error).\n\
  If not nil and not t, move to limit of search and return nil.\n\
Optional fourth argument is repeat count--search for successive occurrences.\n\
See also the functions match-beginning and match-end and replace-match.")
  (string, bound, noerror, count)
     Lisp_Object string, bound, noerror, count;
{
  return search_command (string, bound, noerror, count, 1, 1);
}

DEFUN ("replace-match", Freplace_match, Sreplace_match, 1, 3, 0,
  "Replace text matched by last search with NEWTEXT.\n\
If second arg FIXEDCASE is non-nil, do not alter case of replacement text.\n\
Otherwise convert to all caps or cap initials, like replaced text.\n\
If third arg LITERAL is non-nil, insert NEWTEXT literally.\n\
Otherwise treat \\ as special:\n\
  \\& in NEWTEXT means substitute original matched text,\n\
  \\<n> means substitute match for \\(...\\) number <n>,\n\
  \\\\ means insert one \\.\n\
Leaves point at end of replacement text.")
  (string, fixedcase, literal)
     Lisp_Object string, fixedcase, literal;
{
  enum { nochange, all_caps, cap_initial } case_action = nochange;
  register int pos, last;
  int some_multiletter_word = 0;
  int some_letter = 0;
  register char c, prevc;
  int inslen;

  if (search_regs.start[0] + 1 < FirstCharacter
      || search_regs.start[0] > search_regs.end[0]
      || search_regs.end[0] > NumCharacters)
    args_out_of_range(make_number (search_regs.start[0]),
		      make_number (search_regs.end[0]));

  if (NULL (fixedcase))
    {
      /* Decide how to casify by examining the matched text. */

      last = search_regs.end[0];
      prevc = '\n';
      case_action = all_caps;

      /* some_multiletter_word is set nonzero if any original word
	 is more than one letter long. */
      some_multiletter_word = 0;

      for (pos = search_regs.start[0] + 1; pos <= last; pos++)
	{
	  c = CharAt (pos);
	  if (c >= 'a' && c <= 'z')
	    {
	      /* Cannot be all caps if any original char is lower case */

	      case_action = cap_initial;
	      if (SYNTAX (prevc) != Sword)
		{
		  /* Cannot even be cap initials
		     if some original initial is lower case */
		  case_action = nochange;
		  break;
		}
	      else
		some_multiletter_word = 1;
	    }
	  else if (c >= 'A' && c <= 'Z')
	    {
	      some_letter = 1;
	      if (!some_multiletter_word && SYNTAX (prevc) == Sword)
		some_multiletter_word = 1;
	    }

	  prevc = c;
	}

      /* Do not make new text all caps
	 if the original text contained only single letter words. */
      if (case_action == all_caps && !some_multiletter_word)
	case_action = cap_initial;

      if (!some_letter) case_action = nochange;
    }

  SetPoint (search_regs.end[0] + 1);
  if (!NULL (literal))
    Finsert (1, &string);
  else
    {
      for (pos = 0; pos < XSTRING (string)->size; pos++)
	{
	  c = XSTRING (string)->data[pos];
	  if (c == '\\')
	    {
	      c = XSTRING (string)->data[++pos];
	      if (c == '&')
		place (search_regs.start[0] + 1,
		       search_regs.end[0] + 1);
	      else if (c >= '1' && c <= RE_NREGS + '0')
		place (search_regs.start[c - '0'] + 1,
		       search_regs.end[c - '0'] + 1);
	      else
		insert_char (c);
	    }
	  else
	    insert_char (c);
	}
    }

  inslen = point - (search_regs.end[0] + 1);
  del_range (search_regs.start[0] + 1, search_regs.end[0] + 1);

  if (case_action == all_caps)
    Fupcase_region (make_number (point - inslen), make_number (point));
  else if (case_action == cap_initial)
    {  /* Fcapitalize_region won't do; must not downcase anything.  */
      last = 0;
      for (pos = point - inslen; pos < point; pos++)
	{
	  c = CharAt (pos);
	  if (!last && (c >= 'a' && c <= 'z'))
	    CharAt (pos) = c ^ ('a' - 'A');
	  last = SYNTAX (c) == Sword;
	}
    }
  return Qnil;
}

static
place (l1, l2)
     int l1, l2;
{
  if (l1 < FirstCharacter)
    l1 = FirstCharacter;
  if (l1 >= NumCharacters)
    l1 = NumCharacters;
  if (l2 < l1) l2 = l1;
  if (l2 >= NumCharacters)
    l2 = NumCharacters;
  GapTo (point);
  InsCStr (&CharAt (l1), l2 - l1);
}

DEFUN ("match-beginning", Fmatch_beginning, Smatch_beginning, 1, 1, 0,
  "Return the character number of start of text matched by last regexp searched for.\n\
ARG, a number, specifies which parenthesized expression in the last regexp.\n\
Zero means the entire text matched by the whole regexp.")
  (num)
     Lisp_Object num;
{
  register    n;
  CHECK_NUMBER (num, 0);
  n = XINT (num);
  if (n < 0 || n >= RE_NREGS)
    error ("Out-of-bounds argument");
  return make_number (search_regs.start[n] + 1);
}

DEFUN ("match-end", Fmatch_end, Smatch_end, 1, 1, 0,
  "Return the character number of end of text matched by last regexp searched for.\n\
ARG, a number, specifies which parenthesized expression in the last regexp.\n\
Zero means the entire text matched by the whole regexp.")
  (num)
     Lisp_Object num;
{
  register    n;
  CHECK_NUMBER (num, 0);
  n = XINT (num);
  if (n < 0 || n >= RE_NREGS)
    error ("Out-of-bounds argument");
  return make_number (search_regs.end[n] + 1);
}

DEFUN ("match-data", Fmatch_data, Smatch_data, 0, 0, 0,
  "Return list containing all info on what the last search matched.\n\
Element 2N is (match-beginning N); element 2N + 1 is (match-end N).\n\
All are represented as markers.")
  ()
{
  Lisp_Object data[2 * RE_NREGS];
  int i;

  for (i = 0; i < RE_NREGS; i++)
    {
      data[2 * i] = Fmake_marker ();
      Fset_marker (data[2*i], make_number (search_regs.start[i] + 1), Qnil);
      data[2 * i + 1] = Fmake_marker ();
      Fset_marker (data[2*i + 1], make_number (search_regs.end[i] + 1), Qnil);
    }

  return Flist (2 * RE_NREGS, data);
}


DEFUN ("store-match-data", Fstore_match_data, Sstore_match_data, 1, 1, 0,
  "Set internal data on last search match from elements of LIST.\n\
LIST should have been created by calling match-data previously.")
  (list)
     register Lisp_Object list;
{
  register int i;
  register Lisp_Object marker;

  if (!LISTP (list))
    list = wrong_type_argument (Qlistp, list, 0);

  for (i = 0; i < RE_NREGS; i++)
    {
      marker = Fcar (list);
      CHECK_MARKER (marker, 0);
      search_regs.start[i] = marker_position (marker) - 1;
      list = Fcdr (list);

      marker = Fcar (list);
      CHECK_MARKER (marker, 0);
      search_regs.end[i] = marker_position (marker) - 1;
      list = Fcdr (list);
    }

  return Qnil;  
}

/* Quote a string to inactivate reg-expr chars */

DEFUN ("regexp-quote", Fregexp_quote, Sregexp_quote, 1, 1, 0,
  "Return a regexp string which matches exactly STRING and nothing else.")
  (str)
     Lisp_Object str;
{
  register unsigned char *p, *cp, *end;
  register int size;
  Lisp_Object ostr;

  CHECK_STRING (str, 0);
  size = XSTRING (str)->size;

  /* Increment `size' for the escapes we will need to insert */

  for (cp = XSTRING (str)->data, end = cp + size; cp != end; cp++)
    if (*cp == '[' || *cp == ']'
	|| *cp == '*' || *cp == '.' || *cp == '\\'
	|| *cp == '?' || *cp == '+'
	|| *cp == '^' || *cp == '$')
      size++;

  ostr = Fmake_string (make_number (size), make_number (0));

  /* Now copy the data into the new string, inserting escapes. */

  p = XSTRING (ostr)->data;
  for (cp = XSTRING (str)->data; cp != end; cp++)
    {
      if (*cp == '[' || *cp == ']'
	  || *cp == '*' || *cp == '.' || *cp == '\\'
	  || *cp == '?' || *cp == '+'
	  || *cp == '^' || *cp == '$')
	*p++ = '\\';
      *p++ = *cp;
    }
  return ostr;
}

syms_of_search ()
{
  register int i;

  for (i = 0; i < 0400; i++)
    downcase_table[i] = (i >= 'A' && i <= 'Z') ? i + 040 : i;

  searchbuf.allocated = 100;
  searchbuf.buffer = (char *) malloc (searchbuf.allocated);
  searchbuf.fastmap = search_fastmap;

  Qsearch_failed = intern ("search-failed");
  staticpro (&Qsearch_failed);
  Qinvalid_regexp = intern ("invalid-regexp");
  staticpro (&Qinvalid_regexp);

  Fput (Qsearch_failed, Qerror_conditions,
	Fcons (Qsearch_failed, Fcons (Qerror, Qnil)));
  Fput (Qsearch_failed, Qerror_message,
	build_string ("Search failed"));

  Fput (Qinvalid_regexp, Qerror_conditions,
	Fcons (Qinvalid_regexp, Fcons (Qerror, Qnil)));
  Fput (Qinvalid_regexp, Qerror_message,
	build_string ("Invalid regexp"));

  last_regexp = Qnil;
  staticpro (&last_regexp);

  defsubr (&Sstring_match);
  defsubr (&Slooking_at);
  defsubr (&Sscan_buffer);
  defsubr (&Sskip_chars_forward);
  defsubr (&Sskip_chars_backward);
  defsubr (&Ssearch_forward);
  defsubr (&Ssearch_backward);
  defsubr (&Sword_search_forward);
  defsubr (&Sword_search_backward);
  defsubr (&Sre_search_forward);
  defsubr (&Sre_search_backward);
  defsubr (&Sreplace_match);
  defsubr (&Smatch_beginning);
  defsubr (&Smatch_end);
  defsubr (&Smatch_data);
  defsubr (&Sstore_match_data);
  defsubr (&Sregexp_quote);
}
