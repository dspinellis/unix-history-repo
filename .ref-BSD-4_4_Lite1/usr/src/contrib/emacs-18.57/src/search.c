/* String search routines for GNU Emacs.
   Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "config.h"
#include "lisp.h"
#include "syntax.h"
#include "buffer.h"
#include "commands.h"
#include "regex.h"

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))

unsigned char downcase_table[01000] = {0};	/* folds upper to lower case */
	      /* A WHEEL WILL FALL OFF IF, IN A trt, CHARACTER A */
	      /* TRANSLATES INTO CHARACTER B AND CHARACTER B DOES NOT */
	      /* ALSO TRANSLATE INTO CHARACTER B. */ 
/* If that constraint is met, compute_trt_inverse will follow a */
 /* translation table with its inverse.  The inverse of a table */
 /* follows the table at table[0400].  The form of this is that if */
 /* table[a]=b then the chain starting at table[0400+b], linked by */
 /* link(x)=table[0400+x] and ended by b must include a. */

/* At present compute_trt_inverse is blinded and the inverse for this */
 /* particular table is created by a single-purpose loop. */
 /* compute_trt_inverse has been tested on the following cases: */
 /* trt[x]=x, trt[x]=(+ 3 (logand x, 0370)), trt[x]='a', and the */
 /* downcase table. */

/* We compile regexps into this buffer and then use it for searching. */

struct re_pattern_buffer searchbuf;

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
		   !NULL (current_buffer->case_fold_search) ? (char *) downcase_table : 0);

  immediate_quit = 1;
  QUIT;			/* Do a pending quit right away, to avoid paradoxical behavior */

  /* Get pointers and sizes of the two strings
     that make up the visible portion of the buffer. */

  p1 = BEGV_ADDR;
  s1 = GPT - BEGV;
  p2 = GAP_END_ADDR;
  s2 = ZV - GPT;
  if (s1 < 0)
    {
      p2 = p1;
      s2 = ZV - BEGV;
      s1 = 0;
    }
  if (s2 < 0)
    {
      s1 = ZV - BEGV;
      s2 = 0;
    }
  
  val = (0 <= re_match_2 (&searchbuf, p1, s1, p2, s2,
			  point - BEGV, &search_regs, ZV - BEGV)
	 ? Qt : Qnil);
  for (i = 0; i < RE_NREGS; i++)
    if (search_regs.start[i] >= 0)
      {
	search_regs.start[i] += BEGV;
	search_regs.end[i] += BEGV;
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
      int len = XSTRING (string)->size;

      CHECK_NUMBER (start, 2);
      s = XINT (start);
      if (s < 0 && -s <= len)
	s = len - s;
      else if (0 > s || s > len)
	args_out_of_range (string, start);
    }

  compile_pattern (regexp, &searchbuf,
		   !NULL (current_buffer->case_fold_search) ? (char *) downcase_table : 0);
  immediate_quit = 1;
  val = re_search (&searchbuf, XSTRING (string)->data, XSTRING (string)->size,
			       s, XSTRING (string)->size - s, &search_regs);
  immediate_quit = 0;
  if (val < 0) return Qnil;
  return make_number (val);
}

scan_buffer (target, pos, cnt, shortage)
     int *shortage, pos;
     register int cnt, target;
{
  int lim = ((cnt > 0) ? ZV - 1 : BEGV);
  int direction = ((cnt > 0) ? 1 : -1);
  register int lim0;
  unsigned char *base;
  register unsigned char *cursor, *limit;

  if (shortage != 0)
    *shortage = 0;

  immediate_quit = 1;

  if (cnt > 0)
    while (pos != lim + 1)
      {
	lim0 =  BufferSafeCeiling (pos);
	lim0 = min (lim, lim0);
	limit = &FETCH_CHAR (lim0) + 1;
	base = (cursor = &FETCH_CHAR (pos));
	while (1)
	  {
	    while (*cursor != target && ++cursor != limit)
	      ;
	    if (cursor != limit)
	      {
		if (--cnt == 0)
		  {
		    immediate_quit = 0;
		    return (pos + cursor - base + 1);
		  }
		else
		  if (++cursor == limit)
		    break;
	      }
	    else
	      break;
	  }
	pos += cursor - base;
      }
  else
    {
      pos--;			/* first character we scan */
      while (pos > lim - 1)
	{			/* we WILL scan under pos */
	  lim0 =  BufferSafeFloor (pos);
	  lim0 = max (lim, lim0);
	  limit = &FETCH_CHAR (lim0) - 1;
	  base = (cursor = &FETCH_CHAR (pos));
	  cursor++;
	  while (1)
	    {
	      while (--cursor != limit && *cursor != target)
		;
	      if (cursor != limit)
		{
		  if (++cnt == 0)
		    {
		      immediate_quit = 0;
		      return (pos + cursor - base + 1);
		    }
		}
	      else
		break;
	    }
	  pos += cursor - base;
	}
    }
  immediate_quit = 0;
  if (shortage != 0)
    *shortage = cnt * direction;
  return (pos + ((direction == 1 ? 0 : 1)));
}

int
find_next_newline (from, cnt)
     register int from, cnt;
{
  return (scan_buffer ('\n', from, cnt, (int *) 0));
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
    XFASTINT (lim) = forwardp ? ZV : BEGV;
  else
    CHECK_NUMBER_COERCE_MARKER (lim, 1);

  /* In any case, don't allow scan outside bounds of buffer.  */
  if (XFASTINT (lim) > ZV)
    XFASTINT (lim) = ZV;
  if (XFASTINT (lim) < BEGV)
    XFASTINT (lim) = BEGV;

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
      if (c == '\\')
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

  immediate_quit = 1;
  if (forwardp)
    {
      while (point < XINT (lim) && fastmap[FETCH_CHAR (point)])
	SET_PT (point + 1);
    }
  else
    {
      while (point > XINT (lim) && fastmap[FETCH_CHAR (point - 1)])
	SET_PT (point - 1);
    }
  immediate_quit = 0;
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
    lim = n > 0 ? ZV : BEGV;
  else
    {
      CHECK_NUMBER_COERCE_MARKER (bound, 1);
      lim = XINT (bound);
      if (n > 0 ? lim < point : lim > point)
	error ("Invalid search bound (wrong side of point)");
      if (lim > ZV)
	lim = ZV;
      if (lim < BEGV)
	lim = BEGV;
    }

  np = search_buffer (string, point, lim, n, RE,
		      !NULL (current_buffer->case_fold_search) ? downcase_table : 0);
  if (np <= 0)
    {
      if (NULL (noerror))
	return signal_failure (string);
      if (!EQ (noerror, Qt))
	{
	  if (lim < BEGV || lim > ZV)
	    abort ();
	  SET_PT (lim);
	}
      return Qnil;
    }

  if (np < BEGV || np > ZV)
    abort ();

  SET_PT (np);

  return Qt;
}

/* search for the n'th occurrence of `string' in the current buffer,
   starting at position `from' and stopping at position `lim',
   treating `pat' as a literal string if `RE' is false or as
   a regular expression if `RE' is true.

   If `n' is positive, searching is forward and `lim' must be greater than `from'.
   If `n' is negative, searching is backward and `lim' must be less than `from'.

   Returns -x if only `n'-x occurrences found (x > 0),
   or else the position at the beginning of the `n'th occurrence (if searching backward)
   or the end (if searching forward).  */

/* INTERFACE CHANGE ALERT!!!!  search_buffer now returns -x if only */
/* n-x occurences are found. */

search_buffer (string, pos, lim, n, RE, trt)
     Lisp_Object string;
     int pos;
     int lim;
     int n;
     int RE;
     register unsigned char *trt;
{
  int len = XSTRING (string)->size;
  unsigned char *base_pat = XSTRING (string)->data;
  register int *BM_tab;
  int *BM_tab_base;
  register int direction = ((n > 0) ? 1 : -1);
  register int dirlen;
  int infinity, limit, k, stride_for_teases;
  register unsigned char *pat, *cursor, *p_limit;  
  register int i, j;
  unsigned char *p1, *p2;
  int s1, s2;


  if (!len)
    return (0);

  if (RE)
    compile_pattern (string, &searchbuf, (char *) trt);
  
  if (RE			/* Here we detect whether the */
				/* generality of an RE search is */
				/* really needed. */
      && *(searchbuf.buffer) == (char) exactn /* first item is "exact match" */
      && searchbuf.buffer[1] + 2 == searchbuf.used) /*first is ONLY item */
    {
      RE = 0;			/* can do straight (non RE) search */
      pat = (base_pat = (unsigned char *) searchbuf.buffer + 2);
				/* trt already applied */
      len = searchbuf.used - 2;
    }
  else if (!RE)
    {
      pat = (unsigned char *) alloca (len);

      for (i = len; i--;)		/* Copy the pattern; apply trt */
	*pat++ = (((int) trt) ? trt [*base_pat++] : *base_pat++);
      pat -= len; base_pat = pat;
    }

  if (RE)
    {
      immediate_quit = 1;	/* Quit immediately if user types ^G,
				   because letting this function finish
				   can take too long. */
      QUIT;			/* Do a pending quit right away,
				   to avoid paradoxical behavior */
      /* Get pointers and sizes of the two strings
	 that make up the visible portion of the buffer. */

      p1 = BEGV_ADDR;
      s1 = GPT - BEGV;
      p2 = GAP_END_ADDR;
      s2 = ZV - GPT;
      if (s1 < 0)
	{
	  p2 = p1;
	  s2 = ZV - BEGV;
	  s1 = 0;
	}
      if (s2 < 0)
	{
	  s1 = ZV - BEGV;
	  s2 = 0;
	}
      while (n < 0)
	{
	  if (re_search_2 (&searchbuf, p1, s1, p2, s2,
			   pos - BEGV, lim - pos, &search_regs,
			   /* Don't allow match past current point */
			   pos - BEGV)
	      >= 0)
	    {
	      j = BEGV;
	      for (i = 0; i < RE_NREGS; i++)
		if (search_regs.start[i] >= 0)
		  {
		    search_regs.start[i] += j;
		    search_regs.end[i] += j;
		  }
	      /* Set pos to the new position. */
	      pos = search_regs.start[0];
	    }
	  else
	    {
	      immediate_quit = 0;
	      return (n);
	    }
	  n++;
	}
      while (n > 0)
	{
	  if (re_search_2 (&searchbuf, p1, s1, p2, s2,
			   pos - BEGV, lim - pos, &search_regs,
			   lim - BEGV)
	      >= 0)
	    {
	      j = BEGV;
	      for (i = 0; i < RE_NREGS; i++)
		if (search_regs.start[i] >= 0)
		  {
		    search_regs.start[i] += j;
		    search_regs.end[i] += j;
		  }
	      pos = search_regs.end[0];
	    }
	  else
	    {
	      immediate_quit = 0;
	      return (0 - n);
	    }
	  n--;
	}
      immediate_quit = 0;
      return (pos);
    }
  else				/* non-RE case */
    {
#ifdef C_ALLOCA
      int BM_tab_space[0400];
      BM_tab = &BM_tab_space[0];
#else
      BM_tab = (int *) alloca (0400 * sizeof (int));
#endif
      /* The general approach is that we are going to maintain that we know */
      /* the first (closest to the present position, in whatever direction */
      /* we're searching) character that could possibly be the last */
      /* (furthest from present position) character of a valid match.  We */
      /* advance the state of our knowledge by looking at that character */
      /* and seeing whether it indeed matches the last character of the */
      /* pattern.  If it does, we take a closer look.  If it does not, we */
      /* move our pointer (to putative last characters) as far as is */
      /* logically possible.  This amount of movement, which I call a */
      /* stride, will be the length of the pattern if the actual character */
      /* appears nowhere in the pattern, otherwise it will be the distance */
      /* from the last occurrence of that character to the end of the */
      /* pattern. */
      /* As a coding trick, an enormous stride is coded into the table for */
      /* characters that match the last character.  This allows use of only */
      /* a single test, a test for having gone past the end of the */
      /* permissible match region, to test for both possible matches (when */
      /* the stride goes past the end immediately) and failure to */
      /* match (where you get nudged past the end one stride at a time). */ 

      /* Here we make a "mickey mouse" BM table.  The stride of the search */
      /* is determined only by the last character of the putative match. */
      /* If that character does not match, we will stride the proper */
      /* distance to propose a match that superimposes it on the last */
      /* instance of a character that matches it (per trt), or misses */
      /* it entirely if there is none. */  

      dirlen = len * direction;
      infinity = dirlen - (lim + pos + len + len) * direction;
      if (direction < 0)
	pat = (base_pat += len - 1);
      BM_tab_base = BM_tab;
      BM_tab += 0400;
      j = dirlen;		/* to get it in a register */
      /* A character that does not appear in the pattern induces a */
      /* stride equal to the pattern length. */
      while (BM_tab_base != BM_tab)
	{
	  *--BM_tab = j;
	  *--BM_tab = j;
	  *--BM_tab = j;
	  *--BM_tab = j;
	}
      i = 0;
      while (i != infinity)
	{
	  j = pat[i]; i += direction;
	  if (i == dirlen) i = infinity;
	  if ((int) trt)
	    {
	      k = (j = trt[j]);
	      if (i == infinity)
		stride_for_teases = BM_tab[j];
	      BM_tab[j] = dirlen - i;
	      /* A translation table is followed by its inverse -- see */
	      /* comment following downcase_table for details */ 

	      while ((j = trt[0400+j]) != k)
		BM_tab[j] = dirlen - i;
	    }
	  else
	    {
	      if (i == infinity)
		stride_for_teases = BM_tab[j];
	      BM_tab[j] = dirlen - i;
	    }
	  /* stride_for_teases tells how much to stride if we get a */
	  /* match on the far character but are subsequently */
	  /* disappointed, by recording what the stride would have been */
	  /* for that character if the last character had been */
	  /* different. */
	}
      infinity = dirlen - infinity;
      pos += dirlen - ((direction > 0) ? direction : 0);
      /* loop invariant - pos points at where last char (first char if reverse)
	 of pattern would align in a possible match.  */
      while (n != 0)
	{
	  if ((lim - pos - (direction > 0)) * direction < 0)
	    return (n * (0 - direction));
	  /* First we do the part we can by pointers (maybe nothing) */
	  QUIT;
	  pat = base_pat;
	  limit = pos - dirlen + direction;
	  limit = ((direction > 0)
		   ? BufferSafeCeiling (limit)
		   : BufferSafeFloor (limit));
	  /* LIMIT is now the last (not beyond-last!) value
	     POS can take on without hitting edge of buffer or the gap.  */
	  limit = ((direction > 0)
		   ? min (lim - 1, min (limit, pos + 20000))
		   : max (lim, max (limit, pos - 20000)));
	  if ((limit - pos) * direction > 20)
	    {
	      p_limit = &FETCH_CHAR (limit);
	      p2 = (cursor = &FETCH_CHAR (pos));
	      /* In this loop, pos + cursor - p2 is the surrogate for pos */
	      while (1)		/* use one cursor setting as long as i can */
		{
		  if (direction > 0) /* worth duplicating */
		    {
		      /* Use signed comparison if appropriate
			 to make cursor+infinity sure to be > p_limit.
			 Assuming that the buffer lies in a range of addresses
			 that are all "positive" (as ints) or all "negative",
			 either kind of comparison will work as long
			 as we don't step by infinity.  So pick the kind
			 that works when we do step by infinity.  */
		      if ((int) (p_limit + infinity) > (int) p_limit)
			while ((int) cursor <= (int) p_limit)
			  cursor += BM_tab[*cursor];
		      else
			while ((unsigned int) cursor <= (unsigned int) p_limit)
			  cursor += BM_tab[*cursor];
		    }
		  else
		    {
		      if ((int) (p_limit + infinity) < (int) p_limit)
			while ((int) cursor >= (int) p_limit)
			  cursor += BM_tab[*cursor];
		      else
			while ((unsigned int) cursor >= (unsigned int) p_limit)
			  cursor += BM_tab[*cursor];
		    }
/* If you are here, cursor is beyond the end of the searched region. */
 /* This can happen if you match on the far character of the pattern, */
 /* because the "stride" of that character is infinity, a number able */
 /* to throw you well beyond the end of the search.  It can also */
 /* happen if you fail to match within the permitted region and would */
 /* otherwise try a character beyond that region */
		  if ((cursor - p_limit) * direction <= len)
		    break;	/* a small overrun is genuine */
		  cursor -= infinity; /* large overrun = hit */
		  i = dirlen - direction;
		  if ((int) trt)
		    {
		      while ((i -= direction) + direction != 0)
			if (pat[i] != trt[*(cursor -= direction)])
			  break;
		    }
		  else
		    {
		      while ((i -= direction) + direction != 0)
			if (pat[i] != *(cursor -= direction))
			  break;
		    }
		  cursor += dirlen - i - direction;	/* fix cursor */
		  if (i + direction == 0)
		    {
		      cursor -= direction;
		      search_regs.start[0]
			= pos + cursor - p2 + ((direction > 0)
					       ? 1 - len : 0);
		      search_regs.end[0] = len + search_regs.start[0];
		      if ((n -= direction) != 0)
			cursor += dirlen; /* to resume search */
		      else
			return ((direction > 0)
				? search_regs.end[0] : search_regs.start[0]);
		    }
		  else
		    cursor += stride_for_teases; /* <sigh> we lose -  */
		}
	      pos += cursor - p2;
	    }
	  else
	    /* Now we'll pick up a clump that has to be done the hard */
	    /* way because it covers a discontinuity */
	    {
	      limit = ((direction > 0)
		       ? BufferSafeCeiling (pos - dirlen + 1)
		       : BufferSafeFloor (pos - dirlen - 1));
	      limit = ((direction > 0)
		       ? min (limit + len, lim - 1)
		       : max (limit - len, lim));
	      /* LIMIT is now the last value POS can have
		 and still be valid for a possible match.  */
	      while (1)
		{
		  /* This loop can be coded for space rather than */
		  /* speed because it will usually run only once. */
		  /* (the reach is at most len + 21, and typically */
		  /* does not exceed len) */    
		  while ((limit - pos) * direction >= 0)
		    pos += BM_tab[FETCH_CHAR(pos)];
		  /* now run the same tests to distinguish going off the */
		  /* end, a match or a phoney match. */
		  if ((pos - limit) * direction <= len)
		    break;	/* ran off the end */
		  /* Found what might be a match.
		     Set POS back to last (first if reverse) char pos.  */
		  pos -= infinity;
		  i = dirlen - direction;
		  while ((i -= direction) + direction != 0)
		    {
		      pos -= direction;
		      if (pat[i] != (((int) trt)
				     ? trt[FETCH_CHAR(pos)]
				     : FETCH_CHAR (pos)))
			break;
		    }
		  /* Above loop has moved POS part or all the way
		     back to the first char pos (last char pos if reverse).
		     Set it once again at the last (first if reverse) char.  */
		  pos += dirlen - i- direction;
		  if (i + direction == 0)
		    {
		      pos -= direction;
		      search_regs.start[0]
			= pos + ((direction > 0) ? 1 - len : 0);
		      search_regs.end[0] = len + search_regs.start[0];
		      if ((n -= direction) != 0)
			pos += dirlen; /* to resume search */
		      else
			return ((direction > 0)
				? search_regs.end[0] : search_regs.start[0]);
		    }
		  else
		    pos += stride_for_teases;
		}
	      }
	  /* We have done one clump.  Can we continue? */
	  if ((lim - pos) * direction < 0)
	    return ((0 - n) * direction);
	}
      return pos;
    }
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
  "Search backward from point for match for regular expression REGEXP.\n\
Set point to the beginning of the match, and return t.\n\
The match found is the one starting last in the buffer\n\
and yet ending before the place the origin of the search.\n\
An optional second argument bounds the search; it is a buffer position.\n\
The match found must start at or after that position.\n\
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
  \\N means substitute match for \\(...\\) number N,\n\
  \\\\ means insert one \\.\n\
Leaves point at end of replacement text.")
  (string, fixedcase, literal)
     Lisp_Object string, fixedcase, literal;
{
  enum { nochange, all_caps, cap_initial } case_action;
  register int pos, last;
  int some_multiletter_word;
  int some_letter = 0;
  register int c, prevc;
  int inslen;

  CHECK_STRING (string, 0);

  case_action = nochange;	/* We tried an initialization */
				/* but some C compilers blew it */
  if (search_regs.start[0] < BEGV
      || search_regs.start[0] > search_regs.end[0]
      || search_regs.end[0] > ZV)
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

      for (pos = search_regs.start[0]; pos < last; pos++)
	{
	  c = FETCH_CHAR (pos);
	  if (LOWERCASEP (c))
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
	  else if (!NOCASEP (c))
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

  SET_PT (search_regs.end[0]);
  if (!NULL (literal))
    Finsert (1, &string);
  else
    {
      struct gcpro gcpro1;
      GCPRO1 (string);
      for (pos = 0; pos < XSTRING (string)->size; pos++)
	{
	  c = XSTRING (string)->data[pos];
	  if (c == '\\')
	    {
	      c = XSTRING (string)->data[++pos];
	      if (c == '&')
		Finsert_buffer_substring (Fcurrent_buffer (),
					  make_number (search_regs.start[0]),
					  make_number (search_regs.end[0]));
	      else if (c >= '1' && c <= RE_NREGS + '0')
		{
		  if (search_regs.start[c - '0'] >= 1)
		    Finsert_buffer_substring (Fcurrent_buffer (),
					      make_number (search_regs.start[c - '0']),
					      make_number (search_regs.end[c - '0']));
		}
	      else
		insert_char (c);
	    }
	  else
	    insert_char (c);
	}
      UNGCPRO;
    }

  inslen = point - (search_regs.end[0]);
  del_range (search_regs.start[0], search_regs.end[0]);

  if (case_action == all_caps)
    Fupcase_region (make_number (point - inslen), make_number (point));
  else if (case_action == cap_initial)
    upcase_initials_region (make_number (point - inslen), make_number (point));
  return Qnil;
}

static Lisp_Object
match_limit (num, beginningp)
     Lisp_Object num;
     int beginningp;
{
  register int n;

  CHECK_NUMBER (num, 0);
  n = XINT (num);
  if (n < 0 || n >= RE_NREGS)
    args_out_of_range (num, make_number (RE_NREGS));
  if (search_regs.start[n] < 0)
    return Qnil;
  return (make_number ((beginningp) ? search_regs.start[n]
		                    : search_regs.end[n]));
}

DEFUN ("match-beginning", Fmatch_beginning, Smatch_beginning, 1, 1, 0,
  "Return the character number of start of text matched by last regexp searched for.\n\
ARG, a number, specifies which parenthesized expression in the last regexp.\n\
 Value is nil if ARGth pair didn't match, or there were less than ARG pairs.\n\
Zero means the entire text matched by the whole regexp.")
  (num)
     Lisp_Object num;
{
  return match_limit (num, 1);
}

DEFUN ("match-end", Fmatch_end, Smatch_end, 1, 1, 0,
  "Return the character number of end of text matched by last regexp searched for.\n\
ARG, a number, specifies which parenthesized expression in the last regexp.\n\
 Value is nil if ARGth pair didn't match, or there were less than ARG pairs.\n\
Zero means the entire text matched by the whole regexp.")
  (num)
     Lisp_Object num;
{
  return match_limit (num, 0);
} 

DEFUN ("match-data", Fmatch_data, Smatch_data, 0, 0, 0,
  "Return list containing all info on what the last search matched.\n\
Element 2N is (match-beginning N); element 2N + 1 is (match-end N).\n\
All the elements are normally markers, or nil if the Nth pair didn't match.\n\
0 is also possible, when matching was done with `string-match',\n\
if a match began at index 0 in the string.")
  ()
{
  Lisp_Object data[2 * RE_NREGS];
  int i, len;

  len = -1;
  for (i = 0; i < RE_NREGS; i++)
    {
      int start = search_regs.start[i];
      if (start >= 0)
	{
	  if (start == 0)
	    data[2 * i] = 0;
	  else
	    {
	      data[2 * i] = Fmake_marker ();
	      Fset_marker (data[2 * i], make_number (start), Qnil);
	    }

	  if (search_regs.end[i] == 0)
	    data[2 * i + 1] = 0;
	  else
	    {
	      data[2 * i + 1] = Fmake_marker ();
	      Fset_marker (data[2 * i + 1],
			   make_number (search_regs.end[i]), Qnil);
	    }
	  len = i;
	}
      else
	data[2 * i] = data [2 * i + 1] = Qnil;
    }
  return Flist (2 * len + 2, data);
}


DEFUN ("store-match-data", Fstore_match_data, Sstore_match_data, 1, 1, 0,
  "Set internal data on last search match from elements of LIST.\n\
LIST should have been created by calling match-data previously.")
  (list)
     register Lisp_Object list;
{
  register int i;
  register Lisp_Object marker;

  if (!CONSP (list) && !NULL (list))
    list = wrong_type_argument (Qconsp, list, 0);

  for (i = 0; i < RE_NREGS; i++)
    {
      marker = Fcar (list);
      if (NULL (marker))
	{
	  search_regs.start[i] = -1;
	  list = Fcdr (list);
	}
      else
	{
	  if (XTYPE (marker) == Lisp_Marker
	      && XMARKER (marker)->buffer == 0)
	    XFASTINT (marker) = 0;

	  CHECK_NUMBER_COERCE_MARKER (marker, 0);
	  search_regs.start[i] = XINT (marker);
	  list = Fcdr (list);

	  marker = Fcar (list);
	  if (XTYPE (marker) == Lisp_Marker
	      && XMARKER (marker)->buffer == 0)
	    XFASTINT (marker) = 0;

	  CHECK_NUMBER_COERCE_MARKER (marker, 0);
	  search_regs.end[i] = XINT (marker);
	}
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
  register unsigned char *in, *out, *end;
  register unsigned char *temp;

  CHECK_STRING (str, 0);

  temp = (unsigned char *) alloca (XSTRING (str)->size * 2);

  /* Now copy the data into the new string, inserting escapes. */

  in = XSTRING (str)->data;
  end = in + XSTRING (str)->size;
  out = temp; 

  for (; in != end; in++)
    {
      if (*in == '[' || *in == ']'
	  || *in == '*' || *in == '.' || *in == '\\'
	  || *in == '?' || *in == '+'
	  || *in == '^' || *in == '$')
	*out++ = '\\';
      *out++ = *in;
    }

  return make_string (temp, out - temp);
}

/* This code should be unzapped when there comes to be multiple */
 /* translation tables.  It has been certified on various cases. */
/*
void
compute_trt_inverse (trt)
     register unsigned char *trt;
{
  register int i = 0400;
  register unsigned char c, q;

  while (i--)
    trt[0400+i] = i;
  i = 0400;
  while (i--)
    {
      if ((q = trt[i]) != (unsigned char) i)
	{
	  c = trt[q + 0400];
	  trt[q + 0400] = i;
	  trt[0400 + i] = c;
	}
    }
}
*/
  
syms_of_search ()
{
  register int i;

  for (i = 0; i < 0400; i++)
    {
      downcase_table[i] = (i >= 'A' && i <= 'Z') ? i + 040 : i;
/* We do this instead of using compute_trt_inverse to save space. */
 /* Does it? */
      downcase_table[0400+i]
	= ((i >= 'A' && i <= 'Z')
	   ? i + ('a' - 'A')
	   : ((i >= 'a' && i <= 'z')
	      ? i + ('A' - 'a')
	      : i));
    }
/* Use this instead when there come to be multiple translation tables. 
  compute_trt_inverse (downcase_table);    */

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
