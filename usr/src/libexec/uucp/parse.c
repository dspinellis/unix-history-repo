/* parse.c
   Parse a UUCP command string.

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

   $Log: parse.c,v $
   Revision 1.3  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.2  1991/11/11  02:10:39  ian
   Forget to set pseq field to NULL

   Revision 1.1  1991/09/10  19:40:31  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char parse_rcsid[] = "$Id: parse.c,v 1.3 1992/02/08 03:54:18 ian Rel $";
#endif

/* Parse a UUCP command string into an scmd structure.  This is called
   by the 'g' protocol and the UNIX command file reading routines.  It
   destroys the string it is passed, and the scmd string pointers are
   left pointing into it.  It returns TRUE if the string is
   successfully parsed, FALSE otherwise.  */

boolean
fparse_cmd (zcmd, qcmd)
     char *zcmd;
     struct scmd *qcmd;
{
  char *z;

  z = strtok (zcmd, " \t\n");
  if (z == NULL)
    return FALSE;

  qcmd->bcmd = *z;
  if (qcmd->bcmd != 'S'
      && qcmd->bcmd != 'R'
      && qcmd->bcmd != 'X'
      && qcmd->bcmd != 'H')
    return FALSE;

  qcmd->pseq = NULL;

  /* Handle hangup commands specially.  If it's just "H", return
     the command 'H' to indicate a hangup request.  If it's "HY"
     return 'Y' and if it's "HN" return 'N'.  */

  if (qcmd->bcmd == 'H')
    {
      if (z[1] == '\0')
	return TRUE;
      else if (z[1] == 'Y')
	{
	  qcmd->bcmd = 'Y';
	  return TRUE;
	}
      else if (z[1] == 'N')
	{
	  qcmd->bcmd = 'N';
	  return TRUE;
	}
      else
	return FALSE;
    }

  if (z[1] != '\0')
    return FALSE;

  z = strtok ((char *) NULL, " \t\n");
  if (z == NULL)
    return FALSE;
  qcmd->zfrom = z;

  z = strtok ((char *) NULL, " \t\n");
  if (z == NULL)
    return FALSE;
  qcmd->zto = z;
      
  z = strtok ((char *) NULL, " \t\n");
  if (z == NULL)
    return FALSE;
  qcmd->zuser = z;

  z = strtok ((char *) NULL, " \t\n");
  if (z == NULL || *z != '-')
    return FALSE;
  qcmd->zoptions = z + 1;
      
  z = strtok ((char *) NULL, " \t\n");
  if (z != NULL)
    qcmd->ztemp = z;
  else if (qcmd->bcmd == 'S')
    return FALSE;
  else
    qcmd->ztemp = "";

  z = strtok ((char *) NULL, " \t\n");
  if (z != NULL)
    {
      char *zend;

      qcmd->imode = (int) strtol (z, &zend, 8);
      if (*zend != '\0')
	return FALSE;
    }
  else if (qcmd->bcmd == 'S')
    return FALSE;
  else
    qcmd->imode = 0666;

  z = strtok ((char *) NULL, " \t\n");
  if (z != NULL)
    qcmd->znotify = z;
  else
    qcmd->znotify = "";

  /* If the notify string is "", there may be a number of bytes.  */
  qcmd->cbytes = -1;
  if (strcmp (qcmd->znotify, "\"\"") == 0)
    {
      z = strtok ((char *) NULL, " \t\n");
      if (z != NULL)
	{
	  char *zend;

	  qcmd->znotify = "";
	  qcmd->cbytes = strtol (z, &zend, 10);
	  if (*zend != '\0')
	    return FALSE;
	}
    }

  return TRUE;
}
