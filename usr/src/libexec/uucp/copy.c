/* copy.c
   Copy one file to another for the UUCP package.

   Copyright (C) 1991, 1992 Ian Lance Taylor

   This file is part of the Taylor UUCP package.

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

   The author of the program may be contacted at ian@airs.com or
   c/o AIRS, P.O. Box 520, Waltham, MA 02254.

   $Log: copy.c,v $
   Revision 1.7  1992/02/24  20:07:43  ian
   John Theus: some systems don't have <fcntl.h>

   Revision 1.6  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.5  1991/12/29  04:04:18  ian
   Added a bunch of extern definitions

   Revision 1.4  1991/12/11  03:59:19  ian
   Create directories when necessary; don't just assume they exist

   Revision 1.3  1991/12/01  02:23:12  ian
   Niels Baggesen: don't multiply include <unistd.h>

   Revision 1.2  1991/09/19  03:23:34  ian
   Chip Salzenberg: append to private debugging file, don't overwrite it

   Revision 1.1  1991/09/10  19:39:38  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char copy_rcsid[] = "$Id: copy.c,v 1.7 1992/02/24 20:07:43 ian Rel $";
#endif

#include <stdio.h>
#include <errno.h>

#include "system.h"
#include "sysdep.h"

/* External functions.  */
extern int fclose ();

/* Copy one file to another.  */

#if USE_STDIO

boolean
fcopy_file (zfrom, zto, fpublic, fmkdirs)
     const char *zfrom;
     const char *zto;
     boolean fpublic;
     boolean fmkdirs;
{
  FILE *efrom;
  FILE *eto;
  char ab[8192];
  int c;

  efrom = fopen (zfrom, BINREAD);
  if (efrom == NULL)
    {
      ulog (LOG_ERROR, "fopen (%s): %s", zfrom, strerror (errno));
      return FALSE;
    }
  eto = esysdep_fopen (zto, fpublic, FALSE, fmkdirs);
  if (eto == NULL)
    {
      (void) fclose (efrom);
      return FALSE;
    }

  while ((c = fread (ab, sizeof (char), sizeof ab, efrom)) != 0)
    {
      if (fwrite (ab, sizeof (char), c, eto) != c)
	{
	  ulog (LOG_ERROR, "fwrite: %s", strerror (errno));
	  (void) fclose (efrom);
	  (void) fclose (eto);
	  (void) remove (zto);
	  return FALSE;
	}
    }

  (void) fclose (efrom);

  if (fclose (eto) != 0)
    {
      ulog (LOG_ERROR, "fclose: %s", strerror (errno));
      (void) remove (zto);
      return FALSE;
    }

  return TRUE;
}

#else /* ! USE_STDIO */

#if HAVE_FCNTL_H
#include <fcntl.h>
#else
#if HAVE_SYS_FILE_H
#include <sys/file.h>
#endif
#endif

#ifndef O_RDONLY
#define O_RDONLY 0
#define O_WRONLY 1
#define O_RDWR 2
#endif

boolean
fcopy_file (zfrom, zto, fpublic, fmkdirs)
     const char *zfrom;
     const char *zto;
     boolean fpublic;
     boolean fmkdirs;
{
  int ofrom;
  int oto;
  char ab[8192];
  int c;

  ofrom = open (zfrom, O_RDONLY, 0);
  if (ofrom < 0)
    {
      ulog (LOG_ERROR, "open (%s): %s", zfrom, strerror (errno));
      return FALSE;
    }

  /* These file mode arguments are from the UNIX version of sysdep.h;
     each system dependent header file will need their own
     definitions.  */
  oto = creat (zto, fpublic ? IPUBLIC_FILE_MODE : IPRIVATE_FILE_MODE);
  if (oto < 0)
    {
      if (errno == ENOENT && fmkdirs)
	{
	  if (! fsysdep_make_dirs (zto, fpublic))
	    return FALSE;
	  oto = creat (zto,
		       fpublic ? IPUBLIC_FILE_MODE : IPRIVATE_FILE_MODE);
	}
      if (oto < 0)
	{
	  ulog (LOG_ERROR, "open (%s): %s", zto, strerror (errno));
	  (void) close (ofrom);
	  return FALSE;
	}
    }

  while ((c = read (ofrom, ab, sizeof ab)) > 0)
    {
      if (write (oto, ab, c) != c)
	{
	  ulog (LOG_ERROR, "write: %s", strerror (errno));
	  (void) close (ofrom);
	  (void) close (oto);
	  (void) remove (zto);
	  return FALSE;
	}
    }

  (void) close (ofrom);

  if (close (oto) < 0)
    {
      ulog (LOG_ERROR, "close: %s", strerror (errno));
      (void) remove (zto);
      return FALSE;
    }

  if (c < 0)
    {
      ulog (LOG_ERROR, "read: %s", strerror (errno));
      (void) remove (zto);
      return FALSE;
    }

  return TRUE;
}

#endif /* ! USE_STDIO */
