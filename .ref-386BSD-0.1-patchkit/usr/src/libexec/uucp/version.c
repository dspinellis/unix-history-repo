/* version.c
   The version number string.

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

   $Log: version.c,v $
   Revision 1.6  1992/04/07  05:30:43  ian
   Changed version to 1.03

   Revision 1.5  1992/03/16  19:10:33  ian
   Changed version to beta 1.03

   Revision 1.4  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.3  1992/01/20  23:53:25  ian
   Changed version to 1.02

   Revision 1.2  1992/01/05  03:09:44  ian
   Changed abVersion to non const; changed version to beta 1.02

   Revision 1.1  1991/11/21  22:17:06  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char version_rcsid[] = "$Id: version.c,v 1.6 1992/04/07 05:30:43 ian Rel $";
#endif

char abVersion[] = "1.03";
