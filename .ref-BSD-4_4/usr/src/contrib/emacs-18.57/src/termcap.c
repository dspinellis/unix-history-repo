/* Work-alike for termcap, plus extra features.
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 1, or (at your option)
    any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

In other words, you are welcome to use, share and improve this program.
You are forbidden to forbid anyone else to use, share and improve
what you give them.   Help stamp out software-hoarding!  */



/* BUFSIZE is the initial size allocated for the buffer
   for reading the termcap file.
   It is not a limit.
   Make it large normally for speed.
   Make it variable when debugging, so can exercise
   increasing the space dynamically.  */

#include <sys/types.h>

#ifdef emacs
#include "config.h"
#endif

#ifndef BUFSIZE
#ifdef DEBUG
#define BUFSIZE bufsize

int bufsize = 128;
#else
#define BUFSIZE 2048
#endif
#endif

#ifndef emacs
static
memory_out ()
{
  write (2, "Virtual memory exhausted\n", 25);
  exit (1);
}

static int
xmalloc (size)
     int size;
{
  register tem = malloc (size);
  if (!tem)
    memory_out ();
  return tem;
}

static int
xrealloc (ptr, size)
     int ptr;
     int size;
{
  register tem = realloc (ptr, size);
  if (!tem)
    memory_out ();
  return tem;
}
#endif /* not emacs */

/* Looking up capabilities in the entry already found */

/* The pointer to the data made by tgetent is left here
   for tgetnum, tgetflag and tgetstr to find.  */

static char *term_entry;

static char *tgetst1 ();

/* This is the main subroutine that is used to search
   an entry for a particular capability */

static char *
find_capability (bp, cap)
     register char *bp, *cap;
{
  for (; *bp; bp++)
    if (bp[0] == ':'
	&& bp[1] == cap[0]
	&& bp[2] == cap[1])
      return &bp[4];
  return 0;
}

int
tgetnum (cap)
     char *cap;
{
  register char *ptr = find_capability (term_entry, cap);
  if (!ptr || ptr[-1] != '#')
    return -1;
  return atoi (ptr);
}

int
tgetflag (cap)
     char *cap;
{
  register char *ptr = find_capability (term_entry, cap);
  return 0 != ptr && ptr[-1] == ':';
}

/* Look up a string-valued capability `cap'.
   If `area' is nonzero, it points to a pointer to a block in which
   to store the string.  That pointer is advanced over the space used.
   If `area' is zero, space is allocated with `malloc'.  */

char *
tgetstr (cap, area)
     char *cap;
     char **area;
{
  register char *ptr = find_capability (term_entry, cap);
  if (!ptr || (ptr[-1] != '=' && ptr[-1] != '~'))
    return 0;
  return tgetst1 (ptr, area);
}

/* Table, indexed by a character in range 0100 to 0140 with 0100 subtracted,
   gives meaning of character following \, or a space if no special meaning.
   Eight characters per line within the string.  */

static char esctab[]
  = " \007\010  \033\014 \
      \012 \
  \015 \011 \013 \
        ";

/* Given a pointer to a string value inside a termcap entry (`ptr'),
   copy the value and process \ and ^ abbreviations.
   Copy into block that *area points to,
   or to newly allocated storage if area is 0.  */

static char *
tgetst1 (ptr, area)
     char *ptr;
     char **area;
{
  register char *p, *r;
  register int c;
  register int size;
  char *ret;
  register int c1;

  if (!ptr)
    return 0;

  /* `ret' gets address of where to store the string */
  if (!area)
    {
      /* Compute size of block needed (may overestimate) */
      p = ptr;
      while ((c = *p++) && c != ':' && c != '\n');
      ret = (char *) xmalloc (p - ptr + 1);
    }
  else
    ret = *area;

  /* Copy the string value, stopping at null or colon.  */
  /* Also process ^ and \ abbreviations.  */
  p = ptr;
  r = ret;
  while ((c = *p++) && c != ':' && c != '\n')
    {
      if (c == '^')
	c = *p++ & 037;
      else if (c == '\\')
	{
	  c = *p++;
	  if (c >= '0' && c <= '7')
	    {
	      c -= '0';
	      size = 0;

	      while (++size < 3 && (c1 = *p) >= '0' && c1 <= '7')
		{
		  c *= 8;
		  c += c1 - '0';
		  p++;
		}
	    }
	  else if (c >= 0100 && c < 0200)
	    {
	      c1 = esctab[(c & ~040) - 0100];
	      if (c1 != ' ')
		c = c1;
	    }
	}
      *r++ = c;
    }
  *r = 0;
  /* Update *area */
  if (area)
    *area = r + 1;
  return ret;
}

/* Outputting a string with padding */

short ospeed;
char PC;

/* Actual baud rate if positive;
   - baud rate / 100 if negative.  */

static short speeds[] =
  {
#ifdef VMS
    0, 50, 75, 110, 134, 150, -3, -6, -12, -18,
    -20, -24, -36, -48, -72, -96, -192
#else /* not VMS */
    0, 50, 75, 110, 135, 150, -2, -3, -6, -12,
    -18, -24, -48, -96, -192, -384
#endif /* not VMS */
  };

tputs (string, nlines, outfun)
     register char *string;
     int nlines;
     register int (*outfun) ();
{
  register int padcount = 0;

  if (string == (char *) 0)
    return;
  while (*string >= '0' && *string <= '9')
    {
      padcount += *string++ - '0';
      padcount *= 10;
    }
  if (*string == '.')
    {
      string++;
      padcount += *string++ - '0';
    }
  if (*string == '*')
    {
      string++;
      padcount *= nlines;
    }
  while (*string)
    (*outfun) (*string++);

  /* padcount is now in units of tenths of msec.  */
  padcount *= speeds[ospeed];
  padcount += 500;
  padcount /= 1000;
  if (speeds[ospeed] < 0)
    padcount = -padcount;
  else
    {
      padcount += 50;
      padcount /= 100;
    }

  while (padcount-- > 0)
    (*outfun) (PC);
}

/* Finding the termcap entry in the termcap data base */

struct buffer
  {
    char *beg;
    int size;
    char *ptr;
    int ateof;
    int full;
  };

/* Forward declarations of static functions */

static int scan_file ();
static char *gobble_line ();
static int compare_contin ();
static int name_match ();

#ifdef VMS

#include <rmsdef.h>
#include <fab.h>
#include <nam.h>

static int
legal_filename_p (fn)
     char *fn;
{
  struct FAB fab = cc$rms_fab;
  struct NAM nam = cc$rms_nam;
  char esa[NAM$C_MAXRSS];

  fab.fab$l_fna = fn;
  fab.fab$b_fns = strlen(fn);
  fab.fab$l_nam = &nam;
  fab.fab$l_fop = FAB$M_NAM;

  nam.nam$l_esa = esa;
  nam.nam$b_ess = sizeof esa;

  return SYS$PARSE(&fab, 0, 0) == RMS$_NORMAL;
}

#endif /* VMS */

/* Find the termcap entry data for terminal type `name'
   and store it in the block that `bp' points to.
   Record its address for future use.

   If `bp' is zero, space is dynamically allocated.  */

int
tgetent (bp, name)
     char *bp, *name;
{
  register char *tem;
  register int fd;
  struct buffer buf;
  register char *bp1;
  char *bp2;
  char *term;
  int malloc_size = 0;
  register int c;
  char *tcenv;			/* TERMCAP value, if it contais :tc=.  */
  char *indirect = 0;		/* Terminal type in :tc= in TERMCAP value.  */
  int filep;

  tem = (char *) getenv ("TERMCAP");
  if (tem && *tem == 0) tem = 0;

#ifdef VMS
  filep = tem && legal_filename_p (tem);
#else
  filep = tem && (*tem == '/');
#endif /* VMS */

  /* If tem is non-null and starts with / (in the un*x case, that is),
     it is a file name to use instead of /etc/termcap.
     If it is non-null and does not start with /,
     it is the entry itself, but only if
     the name the caller requested matches the TERM variable.  */

  if (tem && !filep && !strcmp (name, getenv ("TERM")))
    {
      indirect = tgetst1 (find_capability (tem, "tc"), 0);
      if (!indirect)
	{
	  if (!bp)
	    bp = tem;
	  else
	    strcpy (bp, tem);
	  goto ret;
	}
      else
	{			/* we will need to read /etc/termcap */
	  tcenv = tem;
 	  tem = 0;
	}
    }
  else
    indirect = (char *) 0;

  if (!tem)
#ifdef VMS
    tem = "emacs_library:[etc]termcap.dat";
#else
    tem = "/etc/termcap";
#endif

  /* Here we know we must search a file and tem has its name.  */

  fd = open (tem, 0, 0);
  if (fd < 0)
    return -1;

  buf.size = BUFSIZE;
  /* Add 1 to size to ensure room for terminating null.  */
  buf.beg = (char *) xmalloc (buf.size + 1);
  term = indirect ? indirect : name;

  if (!bp)
    {
      malloc_size = indirect ? strlen (tcenv) + 1 : buf.size;
      bp = (char *) xmalloc (malloc_size);
    }
  bp1 = bp;

  if (indirect)			/* copy the data from the environment variable */
    {
      strcpy (bp, tcenv);
      bp1 += strlen (tcenv);
    }

  while (term)
    {
      /* Scan file, reading it via buf, till find start of main entry */
      if (scan_file (term, fd, &buf) == 0)
	return 0;

      /* Free old `term' if appropriate.  */
      if (term != name)
	free (term);

      /* If `bp' is malloc'd by us, make sure it is big enough.  */
      if (malloc_size)
	{
	  malloc_size = bp1 - bp + buf.size;
	  tem = (char *) xrealloc (bp, malloc_size);
	  bp1 += tem - bp;
	  bp = tem;
	}

      bp2 = bp1;

      /* Copy the line of the entry from buf into bp.  */
      tem = buf.ptr;
      while ((*bp1++ = c = *tem++) && c != '\n')
	/* Drop out any \ newline sequence. */
	if (c == '\\' && *tem == '\n')
	  {
	    bp1--;
	    tem++;
	  }
      *bp1 = 0;

      /* Does this entry refer to another terminal type's entry?  */
      /* If something is found, copy it into heap and null-terminate it */
      term = tgetst1 (find_capability (bp2, "tc"), 0);
    }

  close (fd);
  free (buf.beg);

  if (malloc_size)
    {
      bp = (char *) xrealloc (bp, bp1 - bp + 1);
    }

 ret:
  term_entry = bp;
  if (malloc_size)
    return (int) bp;
  return 1;
}

/* Given file open on `fd' and buffer `bufp',
   scan the file from the beginning until a line is found
   that starts the entry for terminal type `string'.
   Returns 1 if successful, with that line in `bufp',
   or returns 0 if no entry found in the file.  */

static int
scan_file (string, fd, bufp)
     char *string;
     int fd;
     register struct buffer *bufp;
{
  register char *tem;
  register char *end;

  bufp->ptr = bufp->beg;
  bufp->full = 0;
  bufp->ateof = 0;
  *bufp->ptr = 0;

  lseek (fd, (off_t) 0, 0);

  while (!bufp->ateof)
    {
      /* Read a line into the buffer */
      end = 0;
      do
	{
	  /* if it is continued, append another line to it,
	     until a non-continued line ends */
	  end = gobble_line (fd, bufp, end);
	}
      while (!bufp->ateof && end[-2] == '\\');

      if (*bufp->ptr != '#'
	  && name_match (bufp->ptr, string))
	return 1;

      /* Discard the line just processed */
      bufp->ptr = end;
    }
  return 0;
}

/* Return nonzero if NAME is one of the names specified
   by termcap entry LINE.  */

static int
name_match (line, name)
     char *line, *name;
{
  register char *tem;

  if (!compare_contin (line, name))
    return 1;
  /* This line starts an entry.  Is it the right one?  */
  for (tem = line; *tem && *tem != '\n' && *tem != ':'; tem++)
    if (*tem == '|' && !compare_contin (tem + 1, name))
      return 1;

  return 0;
}

static int
compare_contin (str1, str2)
     register char *str1, *str2;
{
  register int c1, c2;
  while (1)
    {
      c1 = *str1++;
      c2 = *str2++;
      while (c1 == '\\' && *str1 == '\n')
	{
	  str1++;
	  while ((c1 = *str1++) == ' ' || c1 == '\t');
	}
      if (c2 == '\0')		/* end of type being looked up */
	{
	  if (c1 == '|' || c1 == ':') /* If end of name in data base, */
	    return 0;		/* we win. */
	  else
	    return 1;
        }
      else if (c1 != c2)
	return 1;
    }
}

/* Make sure that the buffer <- `bufp' contains a full line
   of the file open on `fd', starting at the place `bufp->ptr'
   points to.  Can read more of the file, discard stuff before
   `bufp->ptr', or make the buffer bigger.

   Returns the pointer to after the newline ending the line,
   or to the end of the file, if there is no newline to end it.

   Can also merge on continuation lines.  If `append_end' is
   nonzero, it points past the newline of a line that is
   continued; we add another line onto it and regard the whole
   thing as one line.  The caller decides when a line is continued.  */

static char *
gobble_line (fd, bufp, append_end)
     int fd;
     register struct buffer *bufp;
     char *append_end;
{
  register char *end;
  register int nread;
  register char *buf = bufp->beg;
  register char *tem;

  if (append_end == 0)
    append_end = bufp->ptr;

  while (1)
    {
      end = append_end;
      while (*end && *end != '\n') end++;
      if (*end)
        break;
      if (bufp->ateof)
	return buf + bufp->full;
      if (bufp->ptr == buf)
	{
	  if (bufp->full == bufp->size)
	    {
	      bufp->size *= 2;
	      /* Add 1 to size to ensure room for terminating null.  */
	      tem = (char *) xrealloc (buf, bufp->size + 1);
	      bufp->ptr = (bufp->ptr - buf) + tem;
	      append_end = (append_end - buf) + tem;
	      bufp->beg = buf = tem;
	    }
	}
      else
	{
	  append_end -= bufp->ptr - buf;
	  bcopy (bufp->ptr, buf, bufp->full -= bufp->ptr - buf);
	  bufp->ptr = buf;
	}
      if (!(nread = read (fd, buf + bufp->full, bufp->size - bufp->full)))
	bufp->ateof = 1;
      bufp->full += nread;
      buf[bufp->full] = 0;
    }
  return end + 1;
}

#ifdef TEST

#include <stdio.h>

main (argc, argv)
     int argc;
     char **argv;
{
  char *term;
  char *buf;

  term = argv[1];
  printf ("TERM: %s\n", term);

  buf = (char *) tgetent (0, term);
  if ((int) buf <= 0)
    {
      printf ("No entry.\n");
      return 0;
    }

  printf ("Entry: %s\n", buf);

  tprint ("cm");
  tprint ("AL");

  printf ("co: %d\n", tgetnum ("co"));
  printf ("am: %d\n", tgetflag ("am"));
}

tprint (cap)
     char *cap;
{
  char *x = tgetstr (cap, 0);
  register char *y;

  printf ("%s: ", cap);
  if (x)
    {
      for (y = x; *y; y++)
	if (*y <= ' ' || *y == 0177)
	  printf ("\\%0o", *y);
	else
	  putchar (*y);
      free (x);
    }
  else
    printf ("none");
  putchar ('\n');
}

#endif /* TEST */

