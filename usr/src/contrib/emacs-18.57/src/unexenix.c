/* Unexec for Xenix.
   Copyright (C) 1988 Free Software Foundation, Inc.

   Note that the GNU project considers support for Xenix operation
   a peripheral activity which should not be allowed to divert effort
   from development of the GNU system.  Changes in this code will be
   installed when Xenix users send them in, but aside from that
   we don't plan to think about it, or about whether other Emacs
   maintenance might break it.

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


/*
  On 80386 Xenix, segmentation screws prevent us from modifying the text
  segment at all.  We basically just plug a new value for "data segment
  size" into the countless headers and copy the other records straight
  through.  The data segment is ORG'ed at the xs_rbase value of the data
  segment's xseg record (always @ 0x1880000, thanks to the "sophisticated
  memory management hardware" of the chip) and extends to sbrk(0), exactly.
  This code is afraid to malloc (should it be?), and alloca has to be the
  wimpy, malloc-based version; consequently, data is usually copied in
  smallish chunks.

  gb@entity.com
*/

#include "config.h"
#include <sys/types.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <stdio.h>
#include <varargs.h>
#include <a.out.h>

static void fatal_unexec ();

#define READ(_fd, _buffer, _size, _error_message, _error_arg) \
	errno = EEOF; \
	if (read(_fd, _buffer, _size) != _size) \
	  fatal_unexec(_error_message, _error_arg);

#define WRITE(_fd, _buffer, _size, _error_message, _error_arg) \
	if (write(_fd, _buffer, _size) != _size) \
	  fatal_unexec(_error_message, _error_arg);

#define SEEK(_fd, _position, _error_message, _error_arg) \
	errno = EEOF; \
	if (lseek(_fd, _position, L_SET) != _position) \
	  fatal_unexec(_error_message, _error_arg);

extern int errno;
extern int sys_nerr;
extern char *sys_errlist[];
#define EEOF -1

#ifndef L_SET
#define L_SET 0
#endif

/* Should check the magic number of the old executable;
   not yet written.  */
check_exec (x)
     struct xexec *x;
{
}


unexec (new_name, a_name, data_start, bss_start, entry_address)
     char *new_name, *a_name;
     unsigned data_start, bss_start, entry_address;
{
  char *sbrk (), *datalim = sbrk (0), *data_org;
  long segpos, textseen, textpos, textlen, datapos, datadiff, datalen;

  struct xexec u_xexec,       /*  a.out header */
              *u_xexecp = &u_xexec;
  struct xext  u_xext,        /*  extended header */
              *u_xextp  = &u_xext;
  struct xseg  u_xseg,        /*  segment table entry */
              *u_xsegp  = &u_xseg;
  int i, nsegs, isdata = 0, infd, outfd;

  infd = open (a_name, O_RDONLY, 0);
  if (infd < 0) fatal_unexec ("opening %s", a_name);

  outfd = creat (new_name, 0666);
  if (outfd < 0) fatal_unexec ("creating %s", new_name);

  READ (infd, u_xexecp, sizeof (struct xexec),
	"error reading %s", a_name);
  check_exec (u_xexecp);
  READ (infd, u_xextp, sizeof (struct xext),
	"error reading %s", a_name);
  segpos = u_xextp->xe_segpos;
  nsegs = u_xextp->xe_segsize / sizeof (struct xseg);
  SEEK (infd, segpos, "seek error on %s", a_name);
  for (i = 0; i < nsegs; i ++)
    {
      READ (infd, u_xsegp, sizeof (struct xseg),
	    "error reading %s", a_name);
      switch (u_xsegp->xs_type)
	{
	case XS_TTEXT:
	  {
	    if (i == 0)
	      {
		textpos = u_xsegp->xs_filpos;
		textlen = u_xsegp->xs_psize;
		break;
	      }
	    fatal_unexec ("invalid text segment in %s", a_name);
	  }
	case XS_TDATA:
	  {
	    if (i == 1)
	      {
		datapos = u_xsegp->xs_filpos;
		datalen = datalim - (data_org = (char *)(u_xsegp->xs_rbase));
		datadiff = datalen - u_xsegp->xs_psize;
		break;
	      }
	    fatal_unexec ("invalid data segment in %s", a_name);
	  }
	default:
	  {
	    if (i > 1) break;
	    fatal_unexec ("invalid segment record in %s", a_name);
	  }
	}
    }
  u_xexecp->x_data = datalen;
  u_xexecp->x_bss = 0;
  WRITE (outfd, u_xexecp, sizeof (struct xexec),
	 "error writing %s", new_name);
  WRITE (outfd, u_xextp, sizeof (struct xext),
	 "error writing %s", new_name);
  SEEK (infd, segpos, "seek error on %s", a_name);
  SEEK (outfd, segpos, "seek error on %s", new_name);

  /* Copy the text segment record verbatim. */

  copyrec (infd, outfd, sizeof (struct xseg), a_name, new_name);

  /* Read, modify, write the data segment record. */

  READ (infd, u_xsegp, sizeof (struct xseg),
	"error reading %s", a_name);
  u_xsegp->xs_psize = u_xsegp->xs_vsize = datalen;
  u_xsegp->xs_attr &= (~XS_AITER & ~XS_ABSS);
  WRITE (outfd, u_xsegp, sizeof (struct xseg),
	 "error writing %s", new_name);

  /* Now copy any additional segment records, adjusting their
     file position field */

  for (i = 2; i < nsegs; i++)
    {
      READ (infd, u_xsegp, sizeof (struct xseg),
	    "error reading %s", a_name);
      u_xsegp->xs_filpos += datadiff;
      WRITE (outfd, u_xsegp, sizeof (struct xseg),
	     "error writing %s", new_name);
    }

  SEEK (infd, textpos, "seek error on %s", a_name);
  SEEK (outfd, textpos, "seek error on %s", new_name);
  copyrec (infd, outfd, textlen, a_name, new_name);

  SEEK (outfd, datapos, "seek error on %s", new_name);
  WRITE (outfd, data_org, datalen,
	 "write error on %s", new_name);

  for (i = 2, segpos += (2 * sizeof (struct xseg)); 
       i < nsegs;
       i++, segpos += sizeof (struct xseg))
    {
      SEEK (infd, segpos, "seek error on %s", a_name);
      READ (infd, u_xsegp, sizeof (struct xseg),
	    "read error on %s", a_name);
      SEEK (infd, u_xsegp->xs_filpos, "seek error on %s", a_name);
      /* We should be at eof in the output file here, but we must seek
         because the xs_filpos and xs_psize fields in symbol table
	 segments are inconsistent. */
      SEEK (outfd, u_xsegp->xs_filpos + datadiff, "seek error on %s", new_name);
      copyrec (infd, outfd, u_xsegp->xs_psize, a_name, new_name);
    }
  close (infd);
  close (outfd);
  mark_x (new_name);
  return 0;
}

copyrec (infd, outfd, len, in_name, out_name)
     int infd, outfd, len;
     char *in_name, *out_name;
{
  char buf[BUFSIZ];
  int chunk;

  while (len)
    {
      chunk = BUFSIZ;
      if (chunk > len)
	chunk = len;
      READ (infd, buf, chunk, "error reading %s", in_name);
      WRITE (outfd, buf, chunk, "error writing %s", out_name);
      len -= chunk;
    }
}

/*
 * mark_x
 *
 * After succesfully building the new a.out, mark it executable
 */
static
mark_x (name)
     char *name;
{
  struct stat sbuf;
  int um = umask (777);
  umask (um);
  if (stat (name, &sbuf) < 0)
    fatal_unexec ("getting protection on %s", name);
  sbuf.st_mode |= 0111 & ~um;
  if (chmod (name, sbuf.st_mode) < 0)
    fatal_unexec ("setting protection on %s", name);
}

static void
fatal_unexec (s, va_alist)
    va_dcl
{
  va_list ap;
  if (errno == EEOF)
    fputs ("unexec: unexpected end of file, ", stderr);
  else if (errno < sys_nerr)
    fprintf (stderr, "unexec: %s, ", sys_errlist[errno]);
  else
    fprintf (stderr, "unexec: error code %d, ", errno);
  va_start (ap);
  _doprnt (s, ap, stderr);
  fputs (".\n", stderr);
  exit (1);
}
