/* modechange.c -- file mode manipulation
   Copyright (C) 1989, 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by David MacKenzie <djm@ai.mit.edu> */

/* The ASCII mode string is compiled into a linked list of `struct
   modechange', which can then be applied to each file to be changed.
   We do this instead of re-parsing the ASCII string for each file
   because the compiled form requires less computation to use; when
   changing the mode of many files, this probably results in a
   performance gain. */

#include <sys/types.h>
#include <sys/stat.h>
#include "modechange.h"

#ifdef STDC_HEADERS
#include <stdlib.h>
#else
char *malloc ();
#endif

#ifndef NULL
#define NULL 0
#endif

#ifndef S_ISDIR
#define S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#endif

/* Return newly allocated memory to hold one element of type TYPE. */
#define talloc(type) ((type *) malloc (sizeof (type)))

#define isodigit(c) ((c) >= '0' && (c) <= '7')

static int oatoi ();

/* Return a linked list of file mode change operations created from
   MODE_STRING, an ASCII string that contains either an octal number
   specifying an absolute mode, or symbolic mode change operations with
   the form:
   [ugoa...][[+-=][rwxXstugo...]...][,...]
   MASKED_OPS is a bitmask indicating which symbolic mode operators (=+-)
   should not affect bits set in the umask when no users are given.
   Operators not selected in MASKED_OPS ignore the umask.

   Return MODE_INVALID if `mode_string' does not contain a valid
   representation of file mode change operations;
   return MODE_MEMORY_EXHAUSTED if there is insufficient memory. */

struct mode_change *
mode_compile (mode_string, masked_ops)
     register char *mode_string;
     unsigned masked_ops;
{
  struct mode_change *head;	/* First element of the linked list. */
  struct mode_change *change;	/* An element of the linked list. */
  int i;			/* General purpose temporary. */
  int umask_value;		/* The umask value (surprise). */
  unsigned short affected_bits;	/* Which bits in the mode are operated on. */
  unsigned short affected_masked; /* `affected_bits' modified by umask. */
  unsigned ops_to_mask;		/* Operators to actually use umask on. */

  i = oatoi (mode_string);
  if (i >= 0)
    {
      if (i > 07777)
	return MODE_INVALID;
      head = talloc (struct mode_change);
      if (head == NULL)
	return MODE_MEMORY_EXHAUSTED;
      head->next = NULL;
      head->op = '=';
      head->flags = 0;
      head->value = i;
      head->affected = 07777;	/* Affect all permissions. */
      return head;
    }

  umask_value = umask (0);
  umask (umask_value);		/* Restore the old value. */

  head = NULL;
  --mode_string;

  /* One loop iteration for each "ugoa...=+-rwxXstugo...[=+-rwxXstugo...]". */
  do
    {
      affected_bits = 0;
      ops_to_mask = 0;
      /* Turn on all the bits in `affected_bits' for each group given. */
      for (++mode_string;; ++mode_string)
	switch (*mode_string)
	  {
	  case 'u':
	    affected_bits |= 04700;
	    break;
	  case 'g':
	    affected_bits |= 02070;
	    break;
	  case 'o':
	    affected_bits |= 01007;
	    break;
	  case 'a':
	    affected_bits |= 07777;
	    break;
	  default:
	    goto no_more_affected;
	  }

    no_more_affected:
      /* If none specified, affect all bits, except perhaps those
	 set in the umask. */
      if (affected_bits == 0)
	{
	  affected_bits = 07777;
	  ops_to_mask = masked_ops;
	}

      while (*mode_string == '=' || *mode_string == '+' || *mode_string == '-')
	{
	  /* Add the element to the tail of the list, so the operations
	     are performed in the correct order. */
	  if (head == NULL)
	    {
	      head = talloc (struct mode_change);
	      if (head == NULL)
		return MODE_MEMORY_EXHAUSTED;
	      change = head;
	    }
	  else
	    {
	      change->next = talloc (struct mode_change);
	      if (change->next == NULL)
		{
		  mode_free (change);
		  return MODE_MEMORY_EXHAUSTED;
		}
	      change = change->next;
	    }

	  change->next = NULL;
	  change->op = *mode_string;	/* One of "=+-". */
	  affected_masked = affected_bits;
	  if (ops_to_mask & (*mode_string == '=' ? MODE_MASK_EQUALS
			     : *mode_string == '+' ? MODE_MASK_PLUS
			     : MODE_MASK_MINUS))
	    affected_masked &= ~umask_value;
	  change->affected = affected_masked;
	  change->value = 0;
	  change->flags = 0;

	  /* Set `value' according to the bits set in `affected_masked'. */
	  for (++mode_string;; ++mode_string)
	    switch (*mode_string)
	      {
	      case 'r':
		change->value |= 00444 & affected_masked;
		break;
	      case 'w':
		change->value |= 00222 & affected_masked;
		break;
	      case 'X':
		change->flags |= MODE_X_IF_ANY_X;
		/* Fall through. */
	      case 'x':
		change->value |= 00111 & affected_masked;
		break;
	      case 's':
		/* Set the setuid/gid bits if `u' or `g' is selected. */
		change->value |= 06000 & affected_masked;
		break;
	      case 't':
		/* Set the "save text image" bit if `o' is selected. */
		change->value |= 01000 & affected_masked;
		break;
	      case 'u':
		/* Set the affected bits to the value of the `u' bits
		   on the same file.  */
		if (change->value)
		  goto invalid;
		change->value = 00700;
		change->flags |= MODE_COPY_EXISTING;
		break;
	      case 'g':
		/* Set the affected bits to the value of the `g' bits
		   on the same file.  */
		if (change->value)
		  goto invalid;
		change->value = 00070;
		change->flags |= MODE_COPY_EXISTING;
		break;
	      case 'o':
		/* Set the affected bits to the value of the `o' bits
		   on the same file.  */
		if (change->value)
		  goto invalid;
		change->value = 00007;
		change->flags |= MODE_COPY_EXISTING;
		break;
	      default:
		goto no_more_values;
	      }
	no_more_values:;
	}
  } while (*mode_string == ',');
  if (*mode_string == 0)
    return head;
invalid:
  mode_free (head);
  return MODE_INVALID;
}

/* Return file mode OLDMODE, adjusted as indicated by the list of change
   operations CHANGES.  If OLDMODE is a directory, the type `X'
   change affects it even if no execute bits were set in OLDMODE.
   The returned value has the S_IFMT bits cleared. */

unsigned short
mode_adjust (oldmode, changes)
     unsigned oldmode;
     register struct mode_change *changes;
{
  unsigned short newmode;	/* The adjusted mode and one operand. */
  unsigned short value;		/* The other operand. */

  newmode = oldmode & 07777;

  for (; changes; changes = changes->next)
    {
      if (changes->flags & MODE_COPY_EXISTING)
	{
	  /* Isolate in `value' the bits in `newmode' to copy, given in
	     the mask `changes->value'. */
	  value = newmode & changes->value;

	  if (changes->value & 00700)
	    /* Copy `u' permissions onto `g' and `o'. */
	    value |= (value >> 3) | (value >> 6);
	  else if (changes->value & 00070)
	    /* Copy `g' permissions onto `u' and `o'. */
	    value |= (value << 3) | (value >> 3);
	  else
	    /* Copy `o' permissions onto `u' and `g'. */
	    value |= (value << 3) | (value << 6);

	  /* In order to change only `u', `g', or `o' permissions,
	     or some combination thereof, clear unselected bits.
	     This can not be done in mode_compile because the value
	     to which the `changes->affected' mask is applied depends
	     on the old mode of each file. */
	  value &= changes->affected;
	}
      else
	{
	  value = changes->value;
	  /* If `X', do not affect the execute bits if the file is not a
	     directory and no execute bits are already set. */
	  if ((changes->flags & MODE_X_IF_ANY_X)
	      && !S_ISDIR (oldmode)
	      && (newmode & 00111) == 0)
	    value &= ~00111;	/* Clear the execute bits. */
	}

      switch (changes->op)
	{
	case '=':
	  /* Preserve the previous values in `newmode' of bits that are
	     not affected by this change operation. */
	  newmode = (newmode & ~changes->affected) | value;
	  break;
	case '+':
	  newmode |= value;
	  break;
	case '-':
	  newmode &= ~value;
	  break;
	}
    }
  return newmode;
}

/* Free the memory used by the list of file mode change operations
   CHANGES. */

void
mode_free (changes)
     register struct mode_change *changes;
{
  register struct mode_change *next;

  while (changes)
    {
      next = changes->next;
      free (changes);
      changes = next;
    }
}

/* Return a positive integer containing the value of the ASCII
   octal number S.  If S is not an octal number, return -1.  */

static int
oatoi (s)
     char *s;
{
  register int i;

  if (*s == 0)
    return -1;
  for (i = 0; isodigit (*s); ++s)
    i = i * 8 + *s - '0';
  if (*s)
    return -1;
  return i;
}
