/* File I/O for GNU DIFF.
   Copyright (C) 1988, 1989 Free Software Foundation, Inc.

This file is part of GNU DIFF.

GNU DIFF is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU DIFF is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU DIFF; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "diff.h"

/* Rotate a value n bits to the left. */
#define UINT_BIT (sizeof (unsigned) * CHAR_BIT)
#define ROL(v, n) ((v) << (n) | (v) >> UINT_BIT - (n))

/* Given a hash value and a new character, return a new hash value. */
#define HASH(h, c) ((c) + ROL (h, 7))

/* Current file under consideration. */
struct file_data *current;

/* Check for binary files and compare them for exact identity.  */

/* Return 1 if BUF contains a non text character.
   SIZE is the number of characters in BUF.  */

static int
binary_file_p (buf, size)
     char *buf;
     int size;
{
  static const char textchar[] = {
    /* ISO 8859 */
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 1, 1, 1, 1, 1, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1,
  };
  while (--size >= 0)
    if (!textchar[*buf++ & 0377])
      return 1;
  return 0;
}

int binary_file_threshold = 512;

/* Slurp the current file completely into core.
   Return nonzero if it appears to be a binary file.  */

static int
slurp ()
{
  /* If we have a nonexistent file at this stage, treat it as empty.  */
  if (current->desc < 0)
    {
      current->bufsize = 0;
      current->buffered_chars = 0;
      current->buffer = 0;
    }
  /* If it's a regular file, we can just get the size out of the stat
     block and slurp it in all at once. */
  /* In all cases, we leave room in the buffer for 2 extra chars
     beyond those that current->bufsize describes:
     one for a newline (in case the text does not end with one)
     and one for a sentinel in find_identical_ends.  */
  else if ((current->stat.st_mode & S_IFMT) == S_IFREG)
    {
      current->bufsize = current->stat.st_size;
      current->buffer = (char *) xmalloc (current->bufsize + 2);
      current->buffered_chars
	= read (current->desc, current->buffer, current->bufsize);
      if (current->buffered_chars < 0)
	pfatal_with_name (current->name);
    }
  else
    {
      int cc;

      current->bufsize = 4096;
      current->buffer = (char *) xmalloc (current->bufsize + 2);
      current->buffered_chars = 0;
      
      /* Not a regular file; read it in a little at a time, growing the
	 buffer as necessary. */
      while ((cc = read (current->desc,
			 current->buffer + current->buffered_chars,
			 current->bufsize - current->buffered_chars))
	     > 0)
	{
	  current->buffered_chars += cc;
	  if (current->buffered_chars == current->bufsize)
	    {
	      current->bufsize = current->bufsize * 2;
	      current->buffer = (char *) xrealloc (current->buffer,
						   current->bufsize + 2);
	    }
	}
      if (cc < 0)
	pfatal_with_name (current->name);
    }
  
  /* Check first part of file to see if it's a binary file.  */
  if (! always_text_flag
      && binary_file_p (current->buffer,
			min (current->buffered_chars, binary_file_threshold)))
    return 1;

  /* If not binary, make sure text ends in a newline,
     but remember that we had to add one unless -B is in effect.  */
  if (current->buffered_chars > 0
      && current->buffer[current->buffered_chars - 1] != '\n')
    {
      current->missing_newline = !ignore_blank_lines_flag;
      current->buffer[current->buffered_chars++] = '\n';
    }
  else
    current->missing_newline = 0;

  /* Don't use uninitialized storage. */
  if (current->buffer != 0)
    current->buffer[current->buffered_chars] = '\0';

  return 0;
}

/* Split the file into lines, simultaneously computing the hash codes for
   each line. */

void
find_and_hash_each_line ()
{
  unsigned h;
  int i;
  unsigned char *p = (unsigned char *) current->prefix_end, *ip, c;

  /* Attempt to get a good initial guess as to the number of lines. */
  current->linbufsize = current->buffered_chars / 50 + 5;
  current->linbuf
    = (struct line_def *) xmalloc (current->linbufsize * sizeof (struct line_def));

  if (function_regexp || output_style == OUTPUT_IFDEF)
    {
      /* If the -C, -D or -F option is used, we need to find the lines
	 of the matching prefix.  At least we will need to find the last few,
	 but since we don't know how many, it's easiest to find them all.
	 If -D is specified, we need all the lines of the first file.  */
      current->buffered_lines = 0;
      p = (unsigned char *) current->buffer;
    }
  else
    {
      /* Skip the identical prefixes, except be prepared to handle context.
	 In fact, handle 1 more preceding line than the context says,
	 in case shift_boundaries moves things backwards in this file.  */
      current->buffered_lines = current->prefix_lines - context - 1;
      if (current->buffered_lines < 0)
	current->buffered_lines = 0;
      for (i = 0; i < context + 1; ++i)
	/* Unless we are at the beginning, */
	if ((char *) p != current->buffer)
	  /* Back up at least 1 char until at the start of a line.  */
	  while ((char *) --p != current->buffer && p[-1] != '\n')
	    ;
    }

  while ((char *) p < current->suffix_begin)
    {
      h = 0;
      ip = p;

      if (current->prefix_end <= (char *) p)
	{
	  /* Hash this line until we find a newline. */
	  if (ignore_case_flag)
	    {
	      if (ignore_all_space_flag)
		while ((c = *p) != '\n')
		  {
		    if (! isspace (c))
		      if (isupper (c))
			h = HASH (h, tolower (c));
		      else
			h = HASH (h, c);
		    ++p;
		  }
	      else if (ignore_space_change_flag)

		while ((c = *p) != '\n')
		  {
		    if (c == ' ' || c == '\t')
		      {
			while ((c = *p) == ' ' || c == '\t')
			  ++p;
			if (c == '\n')
			  break;
			h = HASH (h, ' ');
		      }
		    /* C is now the first non-space.  */
		    if (isupper (c))
		      h = HASH (h, tolower (c));
		    else
		      h = HASH (h, c);
		    ++p;
		  }
	      else
		while ((c = *p) != '\n')
		  {
		    if (isupper (c))
		      h = HASH (h, tolower (c));
		    else
		      h = HASH (h, c);
		    ++p;
		  }
	    }
	  else
	    {
	      if (ignore_all_space_flag)
		while ((c = *p) != '\n')
		  {
		    if (! isspace (c))
		      h = HASH (h, c);
		    ++p;
		  }
	      else if (ignore_space_change_flag)
		while ((c = *p) != '\n')
		  {
		    if (c == ' ' || c == '\t')
		      {
			while ((c = *p) == ' ' || c == '\t')
			  ++p;
			if (c == '\n')
			  break;
			h = HASH (h, ' ');
		      }
		    /* C is not the first non-space.  */
		    h = HASH (h, c);
		    ++p;
		  }
	      else
		while ((c = *p) != '\n')
		  {
		    h = HASH (h, c);
		    ++p;
		  }
	    }
	}
      else
	/* This line is part of the matching prefix,
	   so we don't need to hash it.  */
	while (*p != '\n')
	  ++p;
      
      /* Maybe increase the size of the line table. */
      if (current->buffered_lines >= current->linbufsize)
	{
	  while (current->buffered_lines >= current->linbufsize)
	    current->linbufsize *= 2;
	  current->linbuf
	    = (struct line_def *) xrealloc (current->linbuf,
					    current->linbufsize
					    * sizeof (struct line_def));
	}
      current->linbuf[current->buffered_lines].text = (char *) ip;
      current->linbuf[current->buffered_lines].length = p - ip + 1;
      current->linbuf[current->buffered_lines].hash = h;
      ++current->buffered_lines;
      ++p;
    }

  i = 0;
  while ((i < context || output_style == OUTPUT_IFDEF)
	 && (char *) p < current->buffer + current->buffered_chars)
    {
      ip = p;
      while (*p++ != '\n')
	;
      /* Maybe increase the size of the line table. */
      if (current->buffered_lines >= current->linbufsize)
	{
	  while (current->buffered_lines >= current->linbufsize)
	    current->linbufsize *= 2;
	  current->linbuf
	    = (struct line_def *) xrealloc (current->linbuf,
					    current->linbufsize
					    * sizeof (struct line_def));
	}
      current->linbuf[current->buffered_lines].text = (char *) ip;
      current->linbuf[current->buffered_lines].length = p - ip;
      current->linbuf[current->buffered_lines].hash = 0;
      ++current->buffered_lines;
      ++i;
    }

  if (ROBUST_OUTPUT_STYLE (output_style)
      && current->missing_newline
      && current->suffix_begin == current->buffer + current->buffered_chars)
    --current->linbuf[current->buffered_lines - 1].length;
}

/* Given a vector of two file_data objects, find the identical
   prefixes and suffixes of each object. */

static void
find_identical_ends (filevec)
     struct file_data filevec[];
{
  char *p0, *p1, *end0, *beg0;
  int lines;

  if (filevec[0].buffered_chars == 0 || filevec[1].buffered_chars == 0)
    {
      filevec[0].prefix_end = filevec[0].buffer;
      filevec[1].prefix_end = filevec[1].buffer;
      filevec[0].prefix_lines = filevec[1].prefix_lines = 0;
      filevec[0].suffix_begin = filevec[0].buffer + filevec[0].buffered_chars;
      filevec[1].suffix_begin = filevec[1].buffer + filevec[1].buffered_chars;
      filevec[0].suffix_lines = filevec[1].suffix_lines = 0;
      return;
    }

  /* Find identical prefix.  */

  p0 = filevec[0].buffer;
  p1 = filevec[1].buffer;
  lines = 0;

  /* Insert end "sentinels", in this case characters that are guaranteed
     to make the equality test false, and thus terminate the loop.  */

  if (filevec[0].buffered_chars < filevec[1].buffered_chars)
    p0[filevec[0].buffered_chars] = ~p1[filevec[0].buffered_chars];
  else
    p1[filevec[1].buffered_chars] = ~p0[filevec[1].buffered_chars];

  /* Loop until first mismatch, or to the sentinel characters.  */
  while (1)
    {
      char c = *p0++;
      if (c != *p1++)
	break;
      if (c == '\n')
	++lines;
    }

  /* Don't count missing newline as part of prefix in RCS mode. */
  if (ROBUST_OUTPUT_STYLE (output_style)
      && ((filevec[0].missing_newline
	   && p0 - filevec[0].buffer > filevec[0].buffered_chars)
	  ||
	  (filevec[1].missing_newline
	   && p1 - filevec[1].buffer > filevec[1].buffered_chars)))
    --p0, --p1, --lines;

  /* If the sentinel was passed, and lengths are equal, the
     files are identical.  */
  if (p0 - filevec[0].buffer > filevec[0].buffered_chars
      && filevec[0].buffered_chars == filevec[1].buffered_chars)
    {
      filevec[0].prefix_end = p0 - 1;
      filevec[1].prefix_end = p1 - 1;
      filevec[0].prefix_lines = filevec[1].prefix_lines = lines;
      filevec[0].suffix_begin = filevec[0].buffer;
      filevec[1].suffix_begin = filevec[1].buffer;
      filevec[0].suffix_lines = filevec[1].suffix_lines = lines;
      return;
    }

  /* Point at first nonmatching characters.  */
  --p0, --p1;

  /* Skip back to last line-beginning in the prefix.  */
  while (p0 != filevec[0].buffer && p0[-1] != '\n')
    --p0, --p1;

  /* Record the prefix.  */
  filevec[0].prefix_end = p0;
  filevec[1].prefix_end = p1;
  filevec[0].prefix_lines = filevec[1].prefix_lines = lines;
  
  /* Find identical suffix.  */

  /* P0 and P1 point beyond the last chars not yet compared.  */
  p0 = filevec[0].buffer + filevec[0].buffered_chars;
  p1 = filevec[1].buffer + filevec[1].buffered_chars;
  lines = 0;

  if (! ROBUST_OUTPUT_STYLE (output_style)
      || filevec[0].missing_newline == filevec[1].missing_newline)
    {
      end0 = p0;		/* Addr of last char in file 0.  */

      /* Get value of P0 at which we should stop scanning backward:
	 this is when either P0 or P1 points just past the last char
	 of the identical prefix.  */
      if (filevec[0].buffered_chars < filevec[1].buffered_chars)
	beg0 = filevec[0].prefix_end;
      else
	/* Figure out where P0 will be when P1 is at the end of the prefix.
	   Thus we only need to test P0.  */
	beg0 = (filevec[0].prefix_end
		+ filevec[0].buffered_chars - filevec[1].buffered_chars);

      /* Scan back until chars don't match or we reach that point.  */
      while (p0 != beg0)
	{
	  char c = *--p0;
	  if (c != *--p1)
	    {
	      /* Point at the first char of the matching suffix.  */
	      ++p0, ++p1;
	      break;
	    }
	  if (c == '\n')
	    ++lines;
	}

      /* Are we at a line-beginning in both files?  */
      if (p0 != end0
	  && !((p0 == filevec[0].buffer || p0[-1] == '\n')
	       &&
	       (p1 == filevec[1].buffer || p1[-1] == '\n')))
	{
	  /* No.  We counted one line too many.  */
	  --lines;
	  /* Advance to next place that is a line-beginning in both files.  */
	  do
	    {
	      ++p0, ++p1;
	    }
	  while (p0 != end0 && p0[-1] != '\n');
	}
    }

  /* Record the suffix.  */
  filevec[0].suffix_begin = p0;
  filevec[1].suffix_begin = p1;
  filevec[0].suffix_lines = filevec[1].suffix_lines = lines;
}

/* Lines are put into equivalence classes (of lines that match in line_cmp).
   Each equivalence class is represented by one of these structures,
   but only while the classes are being computed.
   Afterward, each class is represented by a number.  */
struct equivclass
{
  struct equivclass *next;	/* Next item in this bucket. */
  struct line_def line;	/* A line that fits this class. */
};

/* Hash-table: array of buckets, each being a chain of equivalence classes.  */
static struct equivclass **buckets;
  
/* Size of the bucket array. */
static int nbuckets;

/* Array in which the equivalence classes are allocated.
   The bucket-chains go through the elements in this array.
   The number of an equivalence class is its index in this array.  */
static struct equivclass *equivs;

/* Index of first free element in the array `equivs'.  */
static int equivs_index;

/* Size allocated to the array `equivs'.  */
static int equivs_alloc;

/* Largest primes less than some power of two, for nbuckets.  Values range
   from useful to preposterous.  If one of these numbers isn't prime
   after all, don't blame it on me, blame it on primes (6) . . . */
static int primes[] =
{
  509,
  1021,
  2039,
  4093,
  8191,
  16381,
  32749,
  65521,
  131071,
  262139,
  524287,
  1048573,
  2097143,
  4194301,
  8388593,
  16777213,
  33554393,
  67108859,			/* Preposterously large . . . */
  -1
};

/* Index of current nbuckets in primes. */
static int primes_index;

/* Find the equiv class associated with line N of the current file.  */

static int
find_equiv_class (n)
     int n;
{
  int bucket;
  struct equivclass *b, *p = NULL;

  /* Equivalence class 0 is permanently allocated to lines that were
     not hashed because they were parts of identical prefixes or
     suffixes. */
  if (n < current->prefix_lines
      || current->linbuf[n].text >= current->suffix_begin)
    return 0;

  /* Check through the appropriate bucket to see if there isn't already
     an equivalence class for this line. */
  bucket = current->linbuf[n].hash % nbuckets;
  b = buckets[bucket];
  while (b)
    {
      if (b->line.hash == current->linbuf[n].hash
	  && (b->line.length == current->linbuf[n].length
	      /* Lines of different lengths can match with certain options.  */
	      || length_varies)
	  && !line_cmp (&b->line, &current->linbuf[n]))
	return b - equivs;
      p = b, b = b->next;
    }

  /* Create a new equivalence class in this bucket. */

  p = &equivs[equivs_index++];
  p->next = buckets[bucket];
  buckets[bucket] = p;
  p->line = current->linbuf[n];

  return equivs_index - 1;
}

/* Given a vector of two file_data objects, read the file associated
   with each one, and build the table of equivalence classes.
   Return nonzero if either file appears to be a binary file.  */

int
read_files (filevec)
     struct file_data filevec[];
{
  int i, j;
  int binary = 0;
  int this_binary;

  current = &filevec[0];
  binary = this_binary = slurp ();

  current = &filevec[1];
  this_binary = slurp ();
  if (binary || this_binary)
    return 1;

  find_identical_ends (filevec);

  for (i = 0; i < 2; ++i)
    {
      current = &filevec[i];
      find_and_hash_each_line ();
    }

  /* This is guaranteed to be enough space.  */
  equivs_alloc = filevec[0].buffered_lines + filevec[1].buffered_lines + 1;
  equivs = (struct equivclass *) xmalloc (equivs_alloc * sizeof (struct equivclass));
  /* Equivalence class 0 is permanently safe for lines that were not
     hashed.  Real equivalence classes start at 1. */
  equivs_index = 1;
  
  primes_index = 0;
  while (primes[primes_index] < equivs_alloc / 3)
    primes_index++;

  buckets = (struct equivclass **) xmalloc (primes[primes_index] * sizeof (struct equivclass *));
  bzero (buckets, primes[primes_index] * sizeof (struct equivclass *));
  nbuckets = primes[primes_index];

  for (i = 0; i < 2; ++i)
    {
      current = &filevec[i];
      current->equivs
	= (int *) xmalloc (current->buffered_lines * sizeof (int));
      for (j = 0; j < current->buffered_lines; ++j)
	current->equivs[j] = find_equiv_class (j);
    }

  filevec[0].equiv_max = filevec[1].equiv_max = equivs_index;

  free (equivs);
  free (buckets);

  return 0;
}
