/* Extended regular expression matching and search.
   Copyright (C) 1985 Richard M. Stallman

This program is distributed in the hope that it will be useful,
but without any warranty.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.

   Permission is granted to anyone to distribute verbatim copies
   of this program's source code as received, in any medium, provided that
   the copyright notice, the nonwarraty notice above
   and this permission notice are preserved,
   and that the distributor grants the recipient all rights
   for further redistribution as permitted by this notice,
   and informs him of these rights.

   Permission is granted to distribute modified versions of this
   program's source code, or of portions of it, under the above
   conditions, plus the conditions that all changed files carry
   prominent notices stating who last changed them and that the
   derived material, including anything packaged together with it and
   conceptually functioning as a modification of it rather than an
   application of it, is in its entirety subject to a permission
   notice identical to this one.

   Permission is granted to distribute this program (verbatim or
   as modified) in compiled or executable form, provided verbatim
   redistribution is permitted as stated above for source code, and
    A.  it is accompanied by the corresponding machine-readable
      source code, under the above conditions, or
    B.  it is accompanied by a written offer, with no time limit,
      to distribute the corresponding machine-readable source code,
      under the above conditions, to any one, in return for reimbursement
      of the cost of distribution.   Verbatim redistribution of the
      written offer must be permitted.  Or,
    C.  it is distributed by someone who received only the
      compiled or executable form, and is accompanied by a copy of the
      written offer of source code which he received along with it.

   Permission is granted to distribute this program (verbatim or as modified)
   in executable form as part of a larger system provided that the source
   code for this program, including any modifications used,
   is also distributed or offered as stated in the preceding paragraph.

In other words, you are welcome to use, share and improve this program.
You are forbidden to forbid anyone else to use, share and improve
what you give them.   Help stamp out software-hoarding!  */


/* To test, compile with -Dtest.
 This Dtestable feature turns this into a self-contained program
 which reads a pattern, describes how it compiles,
 then reads a string and searches for it.  */


#ifdef emacs

/* The `emacs' switch turns on certain special matching commands
 that make sense only in emacs. */

#include "config.h"
#include "lisp.h"
#include "buffer.h"
#include "syntax.h"

#else  /* not emacs */

/*
 * Define the syntax stuff, so we can do the \<...\> things.
 */
#define Sword 1

#define SYNTAX(c) syntax_table[c]

static char syntax_table[256];

#endif /* not emacs */

#include "regex.h"

/* Number of failure points to allocate space for initially,
 when matching.  If this number is exceeded, more space is allocated,
 so it is not a hard limit.  */

#ifndef NFAILURES
#define NFAILURES 80
#endif NFAILURES

/* width of a byte in bits */

#define BYTEWIDTH 8

/* These are the command codes that appear in compiled regular expressions, one per byte.
  Some command codes are followed by argument bytes.
  A command code can specify any interpretation whatever for its arguments.
  Zero-bytes may appear in the compiled regular expression. */

enum regexpcode
  {
    unused,
    exactn,    /* followed by one byte giving n, and then by n literal bytes */
    begline,   /* fails unless at beginning of line */
    endline,   /* fails unless at end of line */
    jump,	 /* followed by two bytes giving relative address to jump to */
    on_failure_jump,	 /* followed by two bytes giving relative address of place
		            to resume at in case of failure. */
    finalize_jump,	 /* Throw away latest failure point and then jump to address. */
    maybe_finalize_jump, /* Like jump but finalize if safe to do so.
			    This is used to jump back to the beginning
			    of a repeat.  If the command that follows
			    this jump is clearly incompatible with the
			    one at the beginning of the repeat, such that
			    we can be sure that there is no use backtracking
			    out of repetitions already completed,
			    then we finalize. */
    dummy_failure_jump,  /* jump, and push a dummy failure point.
			    This failure point will be thrown away
			    if an attempt is made to use it for a failure.
			    A + construct makes this before the first repeat.  */
    anychar,	 /* matches any one character */
    charset,     /* matches any one char belonging to specified set.
		    First following byte is # bitmap bytes.
		    Then come bytes for a bit-map saying which chars are in.
		    Bits in each byte are ordered low-bit-first.
		    A character is in the set if its bit is 1.
		    A character too large to have a bit in the map
		    is automatically not in the set */
    charset_not, /* similar but match any character that is NOT one of those specified */
    start_memory, /* starts remembering the text that is matched
		    and stores it in a memory register.
		    followed by one byte containing the register number.
		    Register numbers must be in the range 0 through NREGS. */
    stop_memory, /* stops remembering the text that is matched
		    and stores it in a memory register.
		    followed by one byte containing the register number.
		    Register numbers must be in the range 0 through NREGS. */
    duplicate,    /* match a duplicate of something remembered.
		    Followed by one byte containing the index of the memory register. */
    before_dot,	 /* Succeeds if before dot */
    at_dot,	 /* Succeeds if at dot */
    after_dot,	 /* Succeeds if after dot */
    begbuf,      /* Succeeds if at beginning of buffer */
    endbuf,      /* Succeeds if at end of buffer */
    wordchar,    /* Matches any word-constituent character */
    notwordchar, /* Matches any char that is not a word-constituent */
    wordbeg,	 /* Succeeds if at word beginning */
    wordend,	 /* Succeeds if at word end */
    wordbound,   /* Succeeds if at a word boundary */
    notwordbound, /* Succeeds if not at a word boundary */
    syntaxspec,  /* Matches any character whose syntax is specified.
		    followed by a byte which contains a syntax code, Sword or such like */
    notsyntaxspec /* Matches any character whose syntax differs from the specified. */
  };

#ifndef SIGN_EXTEND_CHAR
#define SIGN_EXTEND_CHAR(x) (x)
#endif

/* compile_pattern takes a regular-expression descriptor string in the user's format
  and converts it into a buffer full of byte commands for matching.

  pattern   is the address of the pattern string
  size      is the length of it.
  bufp	    is a  struct re_pattern_buffer *  which points to the info
	    on where to store the byte commands.
	    This structure contains a  char *  which points to the
	    actual space, which should have been obtained with malloc.
	    compile_pattern may use  realloc  to grow the buffer space.

  The number of bytes of commands can be found out by looking in
  the  struct re_pattern_buffer  that bufp pointed to,
  after compile_pattern returns.
*/

#define PATPUSH(ch) (*b++ = (char) (ch))

#define PATFETCH(c) \
 {if (p == pend) goto end_of_pattern; \
  c = * (unsigned char *) p++; \
  if (translate) c = translate[c]; }

#define PATFETCH_RAW(c) \
 {if (p == pend) goto end_of_pattern; \
  c = * (unsigned char *) p++; }

#define PATUNFETCH p--

#define EXTEND_BUFFER \
  { old_buffer = bufp->buffer; \
    if (bufp->allocated == (1<<16)) goto too_big; \
    bufp->allocated *= 2; \
    if (bufp->allocated > (1<<16)) bufp->allocated = (1<<16); \
    if (!(bufp->buffer = (char *) realloc (bufp->buffer, bufp->allocated))) \
      goto memory_exhausted; \
    c = bufp->buffer - old_buffer; \
    b += c; \
    if (fixup_jump) \
      fixup_jump += c; \
    if (laststart) \
      laststart += c; \
    begalt += c; \
    if (pending_exact) \
      pending_exact += c; \
  }

static int store_jump (), insert_jump ();

char *
re_compile_pattern (pattern, size, bufp)
     char *pattern;
     int size;
     struct re_pattern_buffer *bufp;
{
  register char *b = bufp->buffer;
  register char *p = pattern;
  char *pend = pattern + size;
  register unsigned c, c1;
  char *p1;
  unsigned char *translate = (unsigned char *) bufp->translate;

  /* Temporary used when buffer is made bigger. */

  char *old_buffer;

  /* address of the count-byte of the most recently inserted "exactn" command.
    This makes it possible to tell whether a new exact-match character
    can be added to that command or requires a new "exactn" command. */
     
  char *pending_exact = 0;

  /* address of the place where a forward-jump should go
    to the end of the containing expression.
    Each alternative of an "or", except the last, ends with a forward-jump
    of this sort. */

  char *fixup_jump = 0;

  /* address of start of the most recently finished expression.
    This tells postfix * where to find the start of its operand. */

  char *laststart = 0;

  /* In processing a repeat, 1 means zero matches is allowed */

  char zero_times_ok;

  /* In processing a repeat, 1 means many matches is allowed */

  char many_times_ok;

  /* address of beginning of regexp, or inside of last \( */

  char *begalt = b;

  /* Stack of information saved by \( and restored by \).
     Four stack elements are pushed by each \(:
       First, the value of b.
       Second, the value of fixup_jump.
       Third, the value of regnum.
       Fourth, the value of begalt.  */

  int stackb[40];
  int *stackp = stackb;
  int *stacke = stackb + 40;
  int *stackt;

  /* Counts \('s as they are encountered.  Remembered for the matching \),
     where it becomes the "register number" to put in the stop_memory command */

  int regnum = 1;

  bufp->fastmap_accurate = 0;

#ifndef emacs
  /*
   * Initialize the syntax table.
   */
   init_syntax_once();
#endif emacs

  while (p != pend)
    {
      if (b - bufp->buffer
	  > bufp->allocated - 10)
	/* Note that EXTEND_BUFFER clobbers c */
	EXTEND_BUFFER;

      PATFETCH (c);

      switch (c)
	{
	case '$':
	  /* $ means succeed if at end of line, but only in special contexts.
	    If randonly in the middle of a pattern, it is a normal character. */
	  if (p == pend || (*p == '\\' && (p[1] == ')' || p[1] == '|')))
	    {
	      PATPUSH (endline);
	      break;
	    }
	  goto normal_char;

	case '^':
	  /* ^ means succeed if at beg of line, but only if no preceding pattern. */
	  if (laststart) goto normal_char;
	  PATPUSH (begline);
	  break;

	case '*':
	case '+':
	case '?':
	  /* If there is no previous pattern, char not special. */
	  if (!laststart)
	    goto normal_char;
	  /* If there is a sequence of repetition chars,
	     collapse it down to equivalent to just one.  */
	  zero_times_ok = 0;
	  many_times_ok = 0;
	  while (1)
	    {
	      zero_times_ok |= c != '+';
	      many_times_ok |= c != '?';
	      if (p == pend)
		break;
	      PATFETCH (c);
	      if (!(c == '*' || c == '+' || c == '?'))
		{
		  PATUNFETCH;
		  break;
		}
	    }

	  /* Now we know whether 0 matches is allowed,
	     and whether 2 or more matches is allowed.  */
	  if (many_times_ok)
	    {
	      /* If more than one repetition is allowed,
		 put in a backward jump at the end.  */
	      store_jump (b, maybe_finalize_jump, laststart - 3);
	      b += 3;
	    }
	  insert_jump (on_failure_jump, laststart, b + 3, b);
	  pending_exact = 0;
	  b += 3;
	  if (!zero_times_ok)
	    {
	      /* At least one repetition required: insert before the loop
		 a skip over the initial on-failure-jump instruction */
	      insert_jump (dummy_failure_jump, laststart, laststart + 6, b);
	      b += 3;
	    }
	  break;

	case '.':
	  laststart = b;
	  PATPUSH (anychar);
	  break;

	case '[':
	  if (b - bufp->buffer
	      > bufp->allocated - 3 - (1 << BYTEWIDTH) / BYTEWIDTH)
	    /* Note that EXTEND_BUFFER clobbers c */
	    EXTEND_BUFFER;

	  laststart = b;
	  if (*p == '^')
	    PATPUSH (charset_not), p++;
	  else
	    PATPUSH (charset);
	  p1 = p;

	  PATPUSH ((1 << BYTEWIDTH) / BYTEWIDTH);
	  /* Clear the whole map */
	  bzero (b, (1 << BYTEWIDTH) / BYTEWIDTH);
	  /* Read in characters and ranges, setting map bits */
	  while (1)
	    {
	      PATFETCH (c);
	      if (c == ']' && p != p1 + 1) break;
	      if (*p == '-')
		{
		  PATFETCH (c1);
		  PATFETCH (c1);
		  while (c <= c1)
		    b[c / BYTEWIDTH] |= 1 << (c % BYTEWIDTH), c++;
		}
	      else
		{
		  b[c / BYTEWIDTH] |= 1 << (c % BYTEWIDTH);
		}
	    }
	  /* Discard any bitmap bytes that are all 0 at the end of the map.
	     Decrement the map-length byte too. */
	  while (b[-1] > 0 && b[b[-1] - 1] == 0)
	    b[-1]--;
	  b += b[-1];
	  break;

        case '\\':
	  if (p == pend) goto invalid_pattern;
	  PATFETCH_RAW (c);
	  switch (c)
	    {
	    case '(':
	      if (stackp == stacke) goto nesting_too_deep;
	      if (regnum < RE_NREGS)
	        {
		  PATPUSH (start_memory);
		  PATPUSH (regnum);
	        }
	      *stackp++ = b - bufp->buffer;
	      *stackp++ = fixup_jump ? fixup_jump - bufp->buffer + 1 : 0;
	      *stackp++ = regnum++;
	      *stackp++ = begalt - bufp->buffer;
	      fixup_jump = 0;
	      laststart = 0;
	      begalt = b;
	      break;

	    case ')':
	      if (stackp == stackb) goto unmatched_close;
	      begalt = *--stackp + bufp->buffer;
	      if (fixup_jump)
		store_jump (fixup_jump, jump, b);
	      if (stackp[-1] < RE_NREGS)
		{
		  PATPUSH (stop_memory);
		  PATPUSH (stackp[-1]);
		}
	      stackp -= 2;
	      fixup_jump = 0;
	      if (*stackp)
		fixup_jump = *stackp + bufp->buffer - 1;
	      laststart = *--stackp + bufp->buffer;
	      break;

	    case '|':
	      insert_jump (on_failure_jump, begalt, b + 6, b);
	      pending_exact = 0;
	      b += 3;
	      if (fixup_jump)
		store_jump (fixup_jump, jump, b);
	      fixup_jump = b;
	      b += 3;
	      laststart = 0;
	      begalt = b;
	      break;

#ifdef emacs
	    case '=':
	      PATPUSH (at_dot);
	      break;

	    case 's':	
	      laststart = b;
	      PATPUSH (syntaxspec);
	      PATFETCH (c);
	      PATPUSH (syntax_spec_code[c]);
	      break;

	    case 'S':
	      laststart = b;
	      PATPUSH (notsyntaxspec);
	      PATFETCH (c);
	      PATPUSH (syntax_spec_code[c]);
	      break;
#endif emacs

	    case 'w':
	      laststart = b;
	      PATPUSH (wordchar);
	      break;

	    case 'W':
	      laststart = b;
	      PATPUSH (notwordchar);
	      break;

	    case '<':
	      PATPUSH (wordbeg);
	      break;

	    case '>':
	      PATPUSH (wordend);
	      break;

	    case 'b':
	      PATPUSH (wordbound);
	      break;

	    case 'B':
	      PATPUSH (notwordbound);
	      break;

	    case '`':
	      PATPUSH (begbuf);
	      break;

	    case '\'':
	      PATPUSH (endbuf);
	      break;

	    case '1':
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
	    case '7':
	    case '8':
	    case '9':
	      c1 = c - '0';
	      if (c1 >= regnum)
		goto normal_char;
	      for (stackt = stackp - 2;  stackt > stackb;  stackt -= 4)
 		if (*stackt == c1)
		  goto normal_char;
	      laststart = b;
	      PATPUSH (duplicate);
	      PATPUSH (c1);
	      break;
	    default:
	      goto normal_char;
	    }
	  break;

	default:
	normal_char:
	  if (!pending_exact || pending_exact + *pending_exact + 1 != b
	      || *pending_exact == 0177 || *p == '*' || *p == '^'
	      || *p == '+' || *p == '?')
	    {
	      laststart = b;
	      PATPUSH (exactn);
	      pending_exact = b;
	      PATPUSH (0);
	    }
	  PATPUSH (c);
	  (*pending_exact)++;
	}
    }

  if (fixup_jump)
    store_jump (fixup_jump, jump, b);

  if (stackp != stackb) goto unmatched_open;

  bufp->used = b - bufp->buffer;
  return 0;

 invalid_pattern:
  return "Invalid regular expression";

 unmatched_open:
  return "Unmatched \\(";

 unmatched_close:
  return "Unmatched \\)";

 end_of_pattern:
  return "Premature end of regular expression";

 nesting_too_deep:
  return "Nesting too deep";

 too_big:
  return "Regular expression too big";

 memory_exhausted:
  return "Memory exhausted";
}

#ifndef emacs
init_syntax_once ()
{
   register int c;
   static int done = 0;

   if (done)
     return;

   bzero (syntax_table, sizeof syntax_table);

   for (c = 'a'; c <= 'z'; c++)
     syntax_table[c] = Sword;

   for (c = 'A'; c <= 'Z'; c++)
     syntax_table[c] = Sword;

   for (c = '0'; c <= '9'; c++)
     syntax_table[c] = Sword;

   done = 1;
}
#endif not emacs

/* Store where `from' points a jump operation to jump to where `to' points.
  `opcode' is the opcode to store. */

static int
store_jump (from, opcode, to)
     char *from, *to;
     char opcode;
{
  from[0] = opcode;
  from[1] = (to - (from + 3)) & 0377;
  from[2] = (to - (from + 3)) >> 8;
}

/* Open up space at char FROM, and insert there a jump to TO.
   CURRENT_END gives te end of the storage no in use,
   so we know how much data to copy up.
   OP is the opcode of the jump to insert.

   If you call this function, you must zero out pending_exact.  */

static int
insert_jump (op, from, to, current_end)
     char op;
     char *from, *to, *current_end;
{
  register char *pto = current_end + 3;
  register char *pfrom = current_end;
  while (pfrom != from)
    *--pto = *--pfrom;
  store_jump (from, op, to);
}

/* Given a pattern, compute a fastmap from it.
 The fastmap records which of the (1 << BYTEWIDTH) possible characters
 can start a string that matches the pattern.
 This fastmap is used by re_search to skip quickly over totally implausible text.

 The caller must supply the address of a (1 << BYTEWIDTH)-byte data area
 as bufp->fastmap.
 The other components of bufp describe the pattern to be used.  */

re_compile_fastmap (bufp)
     struct re_pattern_buffer *bufp;
{
  char *pattern = bufp->buffer;
  int size = bufp->used;
  register char *fastmap = bufp->fastmap;
  register char *p = pattern;
  register char *pend = pattern + size;
  register int j, k;
  unsigned char *translate = (unsigned char *) bufp->translate;

  char *stackb[NFAILURES];
  char **stackp = stackb;

  bzero (fastmap, (1 << BYTEWIDTH));
  bufp->fastmap_accurate = 1;
  bufp->can_be_null = 0;
      
  while (p)
    {
      if (p == pend)
	{
	  bufp->can_be_null = 1;
	  break;
	}
#ifdef SWITCH_ENUM_BUG
      switch ((int) ((enum regexpcode) *p++))
#else
      switch ((enum regexpcode) *p++)
#endif
	{
	case exactn:
	  if (translate)
	    fastmap[translate[p[1]]] = 1;
	  else
	    fastmap[p[1]] = 1;
	  break;

        case begline:
        case before_dot:
	case at_dot:
	case after_dot:
	case begbuf:
	case endbuf:
	case wordbound:
	case notwordbound:
	case wordbeg:
	case wordend:
	  continue;

	case endline:
	  if (translate)
	    fastmap[translate['\n']] = 1;
	  else
	    fastmap['\n'] = 1;
	  bufp->can_be_null = 1;
	  break;

	case finalize_jump:
	case maybe_finalize_jump:
	case jump:
	case dummy_failure_jump:
	  bufp->can_be_null = 1;
	  j = *p++ & 0377;
	  j += SIGN_EXTEND_CHAR (*p++) << 8;
	  p += j;
	  if (j > 0)
	    continue;
	  /* Jump backward reached implies we just went through
	     the body of a loop and matched nothing.
	     Opcode jumped to should be an on_failure_jump.
	     Just treat it like an ordinary jump.
	     For a * loop, it has pushed its failure point already;
	     if so, discard that as redundant.  */
	  if ((enum regexpcode) *p != on_failure_jump)
	    continue;
	  p++;
	  j = *p++ & 0377;
	  j += SIGN_EXTEND_CHAR (*p++) << 8;
	  p += j;
	  if (stackp != stackb && *stackp == p)
	    stackp--;
	  continue;
	  
	case on_failure_jump:
	  j = *p++ & 0377;
	  j += SIGN_EXTEND_CHAR (*p++) << 8;
	  *++stackp = p + j;
	  continue;

	case start_memory:
	case stop_memory:
	  p++;
	  continue;

	case duplicate:
	  bufp->can_be_null = 1;
	case anychar:
	  for (j = 0; j < (1 << BYTEWIDTH); j++)
	    fastmap[j] = 1;
	  return;

	case wordchar:
	  for (j = 0; j < (1 << BYTEWIDTH); j++)
	    if (SYNTAX (j) == Sword)
	      fastmap[j] = 1;
	  break;

	case notwordchar:
	  for (j = 0; j < (1 << BYTEWIDTH); j++)
	    if (SYNTAX (j) != Sword)
	      fastmap[j] = 1;
	  break;

#ifdef emacs
	case syntaxspec:
	  k = *p++;
	  for (j = 0; j < (1 << BYTEWIDTH); j++)
	    if (SYNTAX (j) == (enum syntaxcode) k)
	      fastmap[j] = 1;
	  break;

	case notsyntaxspec:
	  for (j = 0; j < (1 << BYTEWIDTH); j++)
	    if (SYNTAX (j) != (enum syntaxcode) k)
	      fastmap[j] = 1;
	  break;
#endif emacs

	case charset:
	  for (j = *p++ * BYTEWIDTH - 1; j >= 0; j--)
	    if (p[j / BYTEWIDTH] & (1 << (j % BYTEWIDTH)))
	      {
		if (translate)
		  fastmap[translate[j]] = 1;
		else
		  fastmap[j] = 1;
	      }
	  break;

	case charset_not:
	  /* Chars beyond end of map must be allowed */
	  for (j = *p * BYTEWIDTH; j < (1 << BYTEWIDTH); j++)
	    if (translate)
	      fastmap[translate[j]] = 1;
	    else
	      fastmap[j] = 1;

	  for (j = *p++ * BYTEWIDTH - 1; j >= 0; j--)
	    if (!(p[j / BYTEWIDTH] & (1 << (j % BYTEWIDTH))))
	      {
		if (translate)
		  fastmap[translate[j]] = 1;
		else
		  fastmap[j] = 1;
	      }
	  break;
	}

      /* Get here means we have successfully found the possible starting characters
	 of one path of the pattern.  We need not follow this path any farther.
	 Instead, look at the next alternative remembered in the stack. */
      if (stackp != stackb)
	p = *stackp--;
      else
	break;
    }
}

/* Like re_search_2, below, but only one string is specified. */

re_search (pbufp, string, size, startpos, range, regs)
     struct re_pattern_buffer *pbufp;
     char *string;
     int size, startpos, range;
     struct re_registers *regs;
{
  return re_search_2 (pbufp, 0, 0, string, size, startpos, range, regs, size);
}

/* Like re_match_2 but tries first a match starting at index `startpos',
 then at startpos + 1, and so on.
 `range' is the number of places to try before giving up.
 If `range' is negative, the starting positions tried are
  startpos, startpos - 1, etc.
 It is up to the caller to make sure that range is not so large
  as to take the starting position outside of the input strings.

The value returned is the position at which the match was found,
 or -1 if no match was found. */

int
re_search_2 (pbufp, string1, size1, string2, size2, startpos, range, regs, mstop)
     struct re_pattern_buffer *pbufp;
     char *string1, *string2;
     int size1, size2;
     int startpos;
     register int range;
     struct re_registers *regs;
     int mstop;
{
  register char *fastmap = pbufp->fastmap;
  register char *translate = pbufp->translate;
  int total = size1 + size2;

  /* Update the fastmap now if not correct already */
  if (fastmap && !pbufp->fastmap_accurate)
    re_compile_fastmap (pbufp);

  while (1)
    {
      /* If a fastmap is supplied, skip quickly over characters
	 that cannot possibly be the start of a match.
	 Note, however, that if the pattern can possibly match
	 the null string, we must test it at each starting point
	 so that we take the first null string we get.  */

      if (fastmap && startpos < total && !pbufp->can_be_null)
	{
	  if (range > 0)
	    {
	      register int lim = 0;
	      register char *p;
	      int irange = range;
	      if (startpos < size1 && startpos + range >= size1)
		lim = range - (size1 - startpos);

	      p = &(startpos >= size1 ? string2 - size1 : string1)[startpos];

	      if (translate)
		{
		  while (range > lim && !fastmap[translate[*p++]])
		    range--;
		}
	      else
		{
		  while (range > lim && !fastmap[*p++])
		    range--;
		}
	      startpos += irange - range;
	    }
	  else
	    {
	      register char c;
	      if (startpos >= size1) c = string2[startpos - size1];
	      else c = string1[startpos];
	      if (translate ? !fastmap[translate[c]] : !fastmap[c])
		goto advance;
	    }
	}

      if (range >= 0 && startpos == total
	  && fastmap && !pbufp->can_be_null)
	return -1;

      if (0 <= re_match_2 (pbufp, string1, size1, string2, size2, startpos, regs, mstop))
	return startpos;

    advance:
      if (!range) break;
      if (range > 0) range--, startpos++; else range++, startpos--;
    }
  return -1;
}

#ifndef emacs   /* emacs never uses this */
re_match (pbufp, string, size, pos, regs)
     struct re_pattern_buffer *pbufp;
     char *string;
     int size, pos;
     struct re_registers *regs;
{
  return re_match_2 (pbufp, 0, 0, string, size, pos, regs, size);
}
#endif /* emacs */

/* Match the pattern described by `pbufp'
  against data which is the virtual concatenation of `string1' and `string2'.
  `size1' and `size2' are the sizes of the two data strings.
  Start the match at position `pos'.
  Do not consider matching past the position `mstop'.

  If pbufp->fastmap is nonzero, then it had better be up to date.

  The reason that the data to match is specified as two components
  which are to be regarded as concatenated
  is so that this function can be used directly on the contents of an Emacs buffer.

  -1 is returned if there is no match.  Otherwise the value is the length
  of the substring which was matched.
*/

int
re_match_2 (pbufp, string1, size1, string2, size2, pos, regs, mstop)
     struct re_pattern_buffer *pbufp;
     char *string1, *string2;
     int size1, size2;
     int pos;
     struct re_registers *regs;
     int mstop;
{
  register char *p = pbufp->buffer;
  register char *pend = p + pbufp->used;
  /* End of first string */
  char *end1;
  /* End of second string */
  char *end2;
  /* Pointer just past last char to consider matching */
  char *end_match_1, *end_match_2;
  register char *d, *dend;
  register int mcnt;
  char *translate = pbufp->translate;

 /* Failure point stack.  Each place that can handle a failure further down the line
    pushes a failure point on this stack.  It consists of two char *'s.
    The first one pushed is where to resume scanning the pattern;
    the second pushed is where to resume scanning the strings.
    If the latter is zero, the failure point is a "dummy".
    If a failure happens and the innermost failure point is dormant,
    it discards that failure point and tries the next one. */

  char **stackb = (char **) alloca (2 * NFAILURES * sizeof (char *));
  char **stackp = stackb, **stacke = &stackb[2 * NFAILURES];

  /* Information on the "contents" of registers.
     These are pointers into the input strings; they record
     just what was matched (on this attempt) by some part of the pattern.
     The start_memory command stores the start of a register's contents
     and the stop_memory command stores the end.

     At that point, regstart[regnum] points to the first character in the register,
     regend[regnum] points to the first character beyond the end of the register,
     and regstart_segend[regnum] is either the same as regend[regnum]
     or else points to the end of the input string into which regstart[regnum] points.
     The latter case happens when regstart[regnum] is in string1 and
     regend[regnum] is in string2.  */

  char *regstart[RE_NREGS];
  char *regstart_segend[RE_NREGS];
  char *regend[RE_NREGS];

  /* Set up pointers to ends of strings.
     Don't allow the second string to be empty unless both are empty.  */
  if (!size2)
    {
      string2 = string1;
      size2 = size1;
      string1 = 0;
      size1 = 0;
    }
  end1 = string1 + size1;
  end2 = string2 + size2;

  /* Compute where to stop matching, within the two strings */
  if (mstop <= size1)
    {
      end_match_1 = string1 + mstop;
      end_match_2 = string2;
    }
  else
    {
      end_match_1 = end1;
      end_match_2 = string2 + mstop - size1;
    }

  /* Initialize \( and \) text positions to -1
     to mark ones that no \( or \) has been seen for.  */

  for (mcnt = 0; mcnt < sizeof (regstart) / sizeof (*regstart); mcnt++)
    regstart[mcnt] = (char *) -1;

  /* `p' scans through the pattern as `d' scans through the data.
     `dend' is the end of the input string that `d' points within.
     `d' is advanced into the following input string whenever necessary,
     but this happens before fetching;
     therefore, at the beginning of the loop,
     `d' can be pointing at the end of a string,
     but it cannot equal string2.  */

  if (pos <= size1)
    d = string1 + pos, dend = end_match_1;
  else
    d = string2 + pos - size1, dend = end_match_2;

/* Write PREFETCH; just before fetching a character with *d.  */
#define PREFETCH \
 while (d == dend)						    \
  { if (dend == end_match_2) goto fail;  /* end of string2 => failure */   \
    d = string2;  /* end of string1 => advance to string2. */       \
    dend = end_match_2; }

  /* This loop loops over pattern commands.
     It exits by returning from the function if match is complete,
     or it drops through if match fails at this starting point in the input data. */

  while (1)
    {
      if (p == pend)
	/* End of pattern means we have succeeded! */
	{
	  /* If caller wants register contents data back, convert it to indices */
	  if (regs)
	    {
	      bzero (regs, sizeof (*regs));

	      regend[0] = d;
	      regstart[0] = string1;
	      for (mcnt = 0; mcnt < RE_NREGS; mcnt++)
		{
		  if (mcnt && regstart[mcnt] == (char *) -1) continue;
		  if (regstart[mcnt] - string1 < 0 || regstart[mcnt] - string1 > size1)
		    regs->start[mcnt] = regstart[mcnt] - string2 + size1;
		  else
		    regs->start[mcnt] = regstart[mcnt] - string1;
		  if (regend[mcnt] - string1 < 0 || regend[mcnt] - string1 > size1)
		    regs->end[mcnt] = regend[mcnt] - string2 + size1;
		  else
		    regs->end[mcnt] = regend[mcnt] - string1;
		}
	      regs->start[0] = pos;
	    }
	  if (d - string1 >= 0 && d - string1 <= size1)
	    return d - string1 - pos;
	  else
	    return d - string2 + size1 - pos;
	}

      /* Otherwise match next pattern command */
#ifdef SWITCH_ENUM_BUG
      switch ((int) ((enum regexpcode) *p++))
#else
      switch ((enum regexpcode) *p++)
#endif
	{

	/* \( is represented by a start_memory, \) by a stop_memory.
	    Both of those commands contain a "register number" argument.
	    The text matched within the \( and \) is recorded under that number.
	    Then, \<digit> turns into a `duplicate' command which
	    is followed by the numeric value of <digit> as the register number. */

	case start_memory:
	  regstart[*p] = d;
	  regstart_segend[*p++] = dend;
	  break;

	case stop_memory:
	  regend[*p] = d;
	  if (regstart_segend[*p] == dend)
	    regstart_segend[*p] = d;
	  p++;
	  break;

	case duplicate:
	  {
	    int regno = *p++;   /* Get which register to match against */
	    register char *d2, *dend2;

	    d2 = regstart[regno];
	    dend2 = regstart_segend[regno];
	    while (1)
	      {
		/* Advance to next segment in register contents, if necessary */
		while (d2 == dend2)
		  {
		    if (dend2 == end_match_2) break;
		    if (dend2 == regend[regno]) break;
		    d2 = string2, dend2 = regend[regno];  /* end of string1 => advance to string2. */
		  }
		/* At end of register contents => success */
		if (d2 == dend2) break;

		/* Advance to next segment in data being matched, if necessary */
		PREFETCH;

		/* mcnt gets # consecutive chars to compare */
		mcnt = dend - d;
		if (mcnt > dend2 - d2)
		  mcnt = dend2 - d2;
		/* Compare that many; failure if mismatch, else skip them. */
		if (translate ? bcmp_translate (d, d2, mcnt, translate) : bcmp (d, d2, mcnt))
		  goto fail;
		d += mcnt, d2 += mcnt;
	      }
	  }
	  break;

	case anychar:
	  /* fetch a data character */
	  PREFETCH;
	  /* Match anything but a newline.  */
	  if ((translate ? translate[*d++] : *d++) == '\n')
	    goto fail;
	  break;

	case charset:
	case charset_not:
	  {
	    /* Nonzero for charset_not */
	    int not = 0;
	    register int c;
	    if (*(p - 1) == (char) charset_not)
	      not = 1;

	    /* fetch a data character */
	    PREFETCH;

	    if (translate)
	      c = translate [*(unsigned char *)d];
	    else
	      c = *(unsigned char *)d;

	    if (c < *p * BYTEWIDTH
		&& p[1 + c / BYTEWIDTH] & (1 << (c % BYTEWIDTH)))
	      not = !not;

	    p += 1 + *p;

	    if (!not) goto fail;
	    d++;
	    break;
	  }

	case begline:
	  if (d == string1 || d[-1] == '\n')
	    break;
	  goto fail;

	case endline:
	  if (d == end2
	      || (d == end1 ? (size2 == 0 || *string2 == '\n') : *d == '\n'))
	    break;
	  goto fail;

	/* "or" constructs ("|") are handled by starting each alternative
	    with an on_failure_jump that points to the start of the next alternative.
	    Each alternative except the last ends with a jump to the joining point.
	    (Actually, each jump except for the last one really jumps
	     to the following jump, because tensioning the jumps is a hassle.) */

	/* The start of a stupid repeat has an on_failure_jump that points
	   past the end of the repeat text.
	   This makes a failure point so that, on failure to match a repetition,
	   matching restarts past as many repetitions have been found
	   with no way to fail and look for another one.  */

	/* A smart repeat is similar but loops back to the on_failure_jump
	   so that each repetition makes another failure point. */

	case on_failure_jump:
	  if (stackp == stacke)
	    {
	      char **stackx = (char **) alloca (2 * (stacke - stackb) * sizeof (char *));
	      bcopy (stackb, stackx, (stacke - stackb) * sizeof (char *));
	      stackp += stackx - stackb;
	      stacke = stackx + 2 * (stacke - stackb);
	      stackb = stackx;
	    }
	  mcnt = *p++ & 0377;
	  mcnt += SIGN_EXTEND_CHAR (*p++) << 8;
	  *stackp++ = mcnt + p;
	  *stackp++ = d;
	  break;

	/* The end of a smart repeat has an maybe_finalize_jump back.
	   Change it either to a finalize_jump or an ordinary jump. */

	case maybe_finalize_jump:
	  mcnt = *p++ & 0377;
	  mcnt += SIGN_EXTEND_CHAR (*p++) << 8;
	  /* Compare what follows with the begining of the repeat.
	     If we can establish that there is nothing that they would
	     both match, we can change to finalize_jump */
	  if (p == pend)
	    p[-3] = (char) finalize_jump;
	  else if (*p == (char) exactn || *p == (char) endline)
	    {
	      register int c = *p == (char) endline ? '\n' : p[2];
	      register char *p1 = p + mcnt;
	      /* p1[0] ... p1[2] are an on_failure_jump.
		 Examine what follows that */
	      if (p1[3] == (char) exactn && p1[5] != c)
		p[-3] = (char) finalize_jump;
	      else if (p1[3] == (char) charset || p1[3] == (char) charset_not)
		{
		  int not = p1[3] == (char) charset_not;
		  if (c < p1[4] * BYTEWIDTH
		      && p1[5 + c / BYTEWIDTH] & (1 << (c % BYTEWIDTH)))
		    not = !not;
		  /* not is 1 if c would match */
		  /* That means it is not safe to finalize */
		  if (!not)
		    p[-3] = (char) finalize_jump;
		}
	    }
	  p -= 2;
	  if (p[-1] != (char) finalize_jump)
	    {
	      p[-1] = (char) jump;
	      goto nofinalize;
	    }

	/* The end of a stupid repeat has a finalize-jump
	   back to the start, where another failure point will be made
	   which will point after all the repetitions found so far. */

	case finalize_jump:
	  stackp -= 2;

	case jump:
	nofinalize:
	  mcnt = *p++ & 0377;
	  mcnt += SIGN_EXTEND_CHAR (*p++) << 8;
	  p += mcnt;
	  break;

	case dummy_failure_jump:
	  if (stackp == stacke)
	    {
	      char **stackx = (char **) alloca (2 * (stacke - stackb) * sizeof (char *));
	      bcopy (stackb, stackx, (stacke - stackb) * sizeof (char *));
	      stackp += stackx - stackb;
	      stacke = stackx + 2 * (stacke - stackb);
	      stackb = stackx;
	    }
	  *stackp++ = 0;
	  *stackp++ = 0;
	  goto nofinalize;

	case wordbound:
	  if (d == string1  /* Points to first char */
	      || d == end2  /* Points to end */
	      || (d == end1 && size2 == 0)) /* Points to end */
	    break;
	  if ((SYNTAX (((unsigned char *)d)[-1]) == Sword)
	      != (SYNTAX (d == end1 ? *(unsigned char *)string2 : *(unsigned char *)d) == Sword))
	    break;
	  goto fail;

	case notwordbound:
	  if (d == string1  /* Points to first char */
	      || d == end2  /* Points to end */
	      || (d == end1 && size2 == 0)) /* Points to end */
	    goto fail;
	  if ((SYNTAX (((unsigned char *)d)[-1]) == Sword)
	      != (SYNTAX (d == end1 ? *(unsigned char *)string2 : *(unsigned char *)d) == Sword))
	    goto fail;
	  break;

	case wordbeg:
	  if (d == end2  /* Points to end */
	      || (d == end1 && size2 == 0) /* Points to end */
	      || SYNTAX (*(unsigned char *) (d == end1 ? string2 : d)) != Sword) /* Next char not a letter */
	    goto fail;
	  if (d == string1  /* Points to first char */
	      || SYNTAX (((unsigned char *)d)[-1]) != Sword)  /* prev char not letter */
	    break;
	  goto fail;

	case wordend:
	  if (d == string1  /* Points to first char */
	      || SYNTAX (((unsigned char *)d)[-1]) != Sword)  /* prev char not letter */
	    goto fail;
	  if (d == end2  /* Points to end */
	      || (d == end1 && size2 == 0) /* Points to end */
	      || SYNTAX (d == end1 ? *(unsigned char *)string2 : *(unsigned char *)d) != Sword) /* Next char not a letter */
	    break;
	  goto fail;

#ifdef emacs
	case before_dot:
	  if (((d - string2 <= (unsigned) size2)
	       ? d - (char *) bf_p2 : d - (char *) bf_p1)
	      <= point)
	    goto fail;
	  break;

	case at_dot:
	  if (((d - string2 <= (unsigned) size2)
	       ? d - (char *) bf_p2 : d - (char *) bf_p1)
	      == point)
	    goto fail;
	  break;

	case after_dot:
	  if (((d - string2 <= (unsigned) size2)
	       ? d - (char *) bf_p2 : d - (char *) bf_p1)
	      >= point)
	    goto fail;
	  break;

	case wordchar:
	  mcnt = (int) Sword;
	  goto matchsyntax;

	case syntaxspec:
	  mcnt = *p++;
	matchsyntax:
	  PREFETCH;
	  if (SYNTAX (*(unsigned char *)d++) != (enum syntaxcode) mcnt) goto fail;
	  break;
	  
	case notwordchar:
	  mcnt = (int) Sword;
	  goto matchnotsyntax;

	case notsyntaxspec:
	  mcnt = *p++;
	matchnotsyntax:
	  PREFETCH;
	  if (SYNTAX (*(unsigned char *)d++) == (enum syntaxcode) mcnt) goto fail;
	  break;
#else
	case wordchar:
	  PREFETCH;
	  if (SYNTAX (*(unsigned char *)d++) == 0) goto fail;
	  break;
	  
	case notwordchar:
	  PREFETCH;
	  if (SYNTAX (*(unsigned char *)d++) != 0) goto fail;
	  break;
#endif not emacs

	case begbuf:
	  if (d == string1)	/* Note, d cannot equal string2 */
	    break;		/* unless string1 == string2.  */
	  goto fail;

	case endbuf:
	  if (d == end2 || (d == end1 && size2 == 0))
	    break;
	  goto fail;

	case exactn:
	  /* Match the next few pattern characters exactly.
	     mcnt is how many characters to match. */
	  mcnt = *p++;
	  if (translate)
	    {
	      do
		{
		  PREFETCH;
		  if (translate[*(unsigned char *)d++] != *p++) goto fail;
		}
	      while (--mcnt);
	    }
	  else
	    {
	      do
		{
		  PREFETCH;
		  if (*d++ != *p++) goto fail;
		}
	      while (--mcnt);
	    }
	  break;
	}
      continue;    /* Successfully matched one pattern command; keep matching */

      /* Jump here if any matching operation fails. */
    fail:
      if (stackp != stackb)
	/* A restart point is known.  Restart there and pop it. */
	{
	  if (!stackp[-2])
	    {   /* If innermost failure point is dormant, flush it and keep looking */
	      stackp -= 2;
	      goto fail;
	    }
	  d = *--stackp;
	  p = *--stackp;
	  if (d >= string1 && d <= end1)
	    dend = end_match_1;
	}
      else break;   /* Matching at this starting point really fails! */
    }
  return -1;         /* Failure to match */
}

static int
bcmp_translate (s1, s2, len, translate)
     char *s1, *s2;
     register int len;
     char *translate;
{
  register char *p1 = s1, *p2 = s2;
  while (len)
    {
      if (translate [*p1++] != translate [*p2++]) return 1;
      len--;
    }
  return 0;
}

/* Entry points compatible with bsd4.2 regex library */

#ifndef emacs

static struct re_pattern_buffer re_comp_buf;

char *
re_comp (s)
     char *s;
{
  if (!s)
    {
      if (!re_comp_buf.buffer)
	return "No previous regular expression";
      return 0;
    }

  if (!re_comp_buf.buffer)
    {
      if (!(re_comp_buf.buffer = (char *) malloc (200)))
	return "Memory exhausted";
      re_comp_buf.allocated = 200;
      if (!(re_comp_buf.fastmap = (char *) malloc (1 << BYTEWIDTH)))
	return "Memory exhausted";
    }
  return re_compile_pattern (s, strlen (s), &re_comp_buf);
}

int
re_exec (s)
     char *s;
{
  int len = strlen (s);
  return 0 <= re_search (&re_comp_buf, s, len, 0, len, 0);
}

#endif /* emacs */

#ifdef test

#include <stdio.h>

/* Indexed by a character, gives the upper case equivalent of the character */

static char upcase[0400] = 
  { 000, 001, 002, 003, 004, 005, 006, 007,
    010, 011, 012, 013, 014, 015, 016, 017,
    020, 021, 022, 023, 024, 025, 026, 027,
    030, 031, 032, 033, 034, 035, 036, 037,
    040, 041, 042, 043, 044, 045, 046, 047,
    050, 051, 052, 053, 054, 055, 056, 057,
    060, 061, 062, 063, 064, 065, 066, 067,
    070, 071, 072, 073, 074, 075, 076, 077,
    0100, 0101, 0102, 0103, 0104, 0105, 0106, 0107,
    0110, 0111, 0112, 0113, 0114, 0115, 0116, 0117,
    0120, 0121, 0122, 0123, 0124, 0125, 0126, 0127,
    0130, 0131, 0132, 0133, 0134, 0135, 0136, 0137,
    0140, 0101, 0102, 0103, 0104, 0105, 0106, 0107,
    0110, 0111, 0112, 0113, 0114, 0115, 0116, 0117,
    0120, 0121, 0122, 0123, 0124, 0125, 0126, 0127,
    0130, 0131, 0132, 0173, 0174, 0175, 0176, 0177,
    0200, 0201, 0202, 0203, 0204, 0205, 0206, 0207,
    0210, 0211, 0212, 0213, 0214, 0215, 0216, 0217,
    0220, 0221, 0222, 0223, 0224, 0225, 0226, 0227,
    0230, 0231, 0232, 0233, 0234, 0235, 0236, 0237,
    0240, 0241, 0242, 0243, 0244, 0245, 0246, 0247,
    0250, 0251, 0252, 0253, 0254, 0255, 0256, 0257,
    0260, 0261, 0262, 0263, 0264, 0265, 0266, 0267,
    0270, 0271, 0272, 0273, 0274, 0275, 0276, 0277,
    0300, 0301, 0302, 0303, 0304, 0305, 0306, 0307,
    0310, 0311, 0312, 0313, 0314, 0315, 0316, 0317,
    0320, 0321, 0322, 0323, 0324, 0325, 0326, 0327,
    0330, 0331, 0332, 0333, 0334, 0335, 0336, 0337,
    0340, 0341, 0342, 0343, 0344, 0345, 0346, 0347,
    0350, 0351, 0352, 0353, 0354, 0355, 0356, 0357,
    0360, 0361, 0362, 0363, 0364, 0365, 0366, 0367,
    0370, 0371, 0372, 0373, 0374, 0375, 0376, 0377
  };

main ()
{
  char pat[80];
  struct re_pattern_buffer buf;
  int i;
  char c;
  char fastmap[(1 << BYTEWIDTH)];

  buf.allocated = 40;
  buf.buffer = (char *) malloc (buf.allocated);
  buf.fastmap = fastmap;
  buf.translate = upcase;

  while (1)
    {
      gets (pat);

      if (*pat)
	{
          re_compile_pattern (pat, strlen(pat), &buf);

	  for (i = 0; i < buf.used; i++)
	    printchar (buf.buffer[i]);

	  putchar ('\n');

	  printf ("%d allocated, %d used.\n", buf.allocated, buf.used);

	  re_compile_fastmap (&buf);
	  printf ("Allowed by fastmap: ");
	  for (i = 0; i < (1 << BYTEWIDTH); i++)
	    if (fastmap[i]) printchar (i);
	  putchar ('\n');
	}

      gets (pat);	/* Now read the string to match against */

      i = re_match (&buf, pat, strlen (pat), 0, 0);
      printf ("Match value %d.\n", i);
    }
}

#ifdef NOTDEF
print_buf (bufp)
     struct re_pattern_buffer *bufp;
{
  int i;

  printf ("buf is :\n----------------\n");
  for (i = 0; i < bufp->used; i++)
    printchar (bufp->buffer[i]);
  
  printf ("\n%d allocated, %d used.\n", bufp->allocated, bufp->used);
  
  printf ("Allowed by fastmap: ");
  for (i = 0; i < (1 << BYTEWIDTH); i++)
    if (bufp->fastmap[i])
      printchar (i);
  printf ("\nAllowed by translate: ");
  if (bufp->translate)
    for (i = 0; i < (1 << BYTEWIDTH); i++)
      if (bufp->translate[i])
	printchar (i);
  printf ("\nfastmap is%s accurate\n", bufp->fastmap_accurate ? "" : "n't");
  printf ("can %s be null\n----------", bufp->can_be_null ? "" : "not");
}
#endif

printchar (c)
     char c;
{
  if (c < 041 || c >= 0177)
    {
      putchar ('\\');
      putchar (((c >> 6) & 3) + '0');
      putchar (((c >> 3) & 7) + '0');
      putchar ((c & 7) + '0');
    }
  else
    putchar (c);
}

error (string)
     char *string;
{
  puts (string);
  exit (1);
}

#endif test
