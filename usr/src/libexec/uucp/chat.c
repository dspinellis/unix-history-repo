/* chat.c
   Chat routine for the UUCP package.

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

   $Log: chat.c,v $
   Revision 1.26  1992/04/03  05:37:11  ian
   Minor cleanups for gcc 2.1

   Revision 1.25  1992/03/28  21:47:55  ian
   David J. MacKenzie: allow backslash to quote newline in config files

   Revision 1.24  1992/03/28  21:28:00  ian
   David J. MacKenzie: handle empty subexpect strings correctly

   Revision 1.23  1992/03/28  19:57:22  ian
   David J. MacKenzie: send port device for /Y rather than port name

   Revision 1.22  1992/03/17  01:03:03  ian
   Miscellaneous cleanup

   Revision 1.21  1992/03/16  21:21:59  ian
   Scott Ballantyne: go ahead and send an illegal send script character

   Revision 1.20  1992/03/16  00:47:15  ian
   Turn off DEBUG_PORT for chat script debugging

   Revision 1.19  1992/03/12  19:56:10  ian
   Debugging based on types rather than number

   Revision 1.18  1992/03/11  19:53:55  ian
   Improved chat script debugging

   Revision 1.17  1992/03/04  00:36:44  ian
   Michael Richardson: better chat script debugging

   Revision 1.16  1992/03/03  06:06:48  ian
   T. William Wells: don't complain about missing configuration files

   Revision 1.15  1992/02/19  19:36:07  ian
   Rearranged time functions

   Revision 1.14  1992/02/19  05:24:07  ian
   Bob Denny: if no trailing send string, don't send a carriage return

   Revision 1.13  1992/02/08  03:54:18  ian
   Include <string.h> only in <uucp.h>, added 1992 copyright

   Revision 1.12  1992/02/02  06:38:22  ian
   Michael Nolan: chat script strings might be separated by more than just ' '

   Revision 1.11  1992/01/05  03:11:06  ian
   Made fcsend static

   Revision 1.10  1991/12/28  03:49:23  ian
   Added HAVE_MEMFNS and HAVE_BFNS; changed uses of memset to bzero

   Revision 1.9  1991/12/17  23:14:08  ian
   T. William Wells: allow dialer complete and abort to be chat scripts

   Revision 1.8  1991/12/15  04:17:11  ian
   Added chat-seven-bit command to control parity bit stripping

   Revision 1.7  1991/12/15  03:42:33  ian
   Added tprocess_chat_cmd for all chat commands, and added CMDTABTYPE_PREFIX

   Revision 1.6  1991/12/13  04:27:33  ian
   Franc,ois Pinard: add some chat script debugging messages

   Revision 1.5  1991/12/07  17:58:38  ian
   Handle a chat script with nothing but a send string

   Revision 1.4  1991/12/06  21:10:27  ian
   Franc,ois Pinard: ccescape should never return a negative number

   Revision 1.3  1991/11/11  23:47:24  ian
   Added chat-program to run a program to do a chat script

   Revision 1.2  1991/11/11  19:32:03  ian
   Added breceive_char to read characters through protocol buffering

   Revision 1.1  1991/09/10  19:38:16  ian
   Initial revision

   */

#include "uucp.h"

#if USE_RCS_ID
char chat_rcsid[] = "$Id: chat.c,v 1.26 1992/04/03 05:37:11 ian Rel $";
#endif

#include <ctype.h>
#include <errno.h>

#include "port.h"
#include "system.h"

/* Local functions.  */

static int ccescape P((char *zbuf));
static int icexpect P((int cstrings, char **azstrings, int *aclens,
		       int ctimeout, boolean fstrip));
static boolean fcsend P((const char *zsend,
			 const struct ssysteminfo *qsys,
			 const struct sdialer *qdial,
			 const char *zphone,
			 boolean ftranslate));
static boolean fcecho_send P((const char *z, int clen));
static boolean fcphone P((const struct sdialer *qdial, const char *zphone,
			  boolean (*pfwrite) P((const char *zwrite,
						int cwrite)),
			  boolean ftranslate, boolean *pfquote));
static boolean fctranslate P((const char *zphone, const char **pzprefix,
			      const char **pzsuffix));
static boolean fcprogram P((const char *zprogram,
			    const struct ssysteminfo *qsys,
			    const struct sdialer *qdial,
			    const char *zphone, const char *zport,
			    long ibaud));

/* Run a chat script with the other system.  The chat script is a
   series of expect send pairs.  We wait for the expect string to show
   up, and then we send the send string.  The chat string for a system
   holds the expect and send strings separated by a single space.  */

boolean
fchat (qchat, qsys, qdial, zphone, ftranslate, zport, ibaud)
     const struct schat_info *qchat;
     const struct ssysteminfo *qsys;
     const struct sdialer *qdial;
     const char *zphone;
     boolean ftranslate;
     const char *zport;
     long ibaud;
{
  const char *zchat;
  int cstrings;
  char **azstrings;
  int *aclens;
  char *zbuf;

  /* First run the program, if any.  */
  if (qchat->zprogram != NULL)
    {
      if (! fcprogram (qchat->zprogram, qsys, qdial, zphone, zport, ibaud))
	return FALSE;
    }

  /* If there's no chat script, we're done.  */
  if (qchat->zchat == NULL)
    return TRUE;

  zchat = qchat->zchat;

  if (qchat->zfail == NULL)
    {
      cstrings = 1;
      azstrings = (char **) alloca (sizeof (char *));
      aclens = (int *) alloca (sizeof (int));
    }
  else
    {
      const char *zlook;
      char *zcopy, *z;

      /* We leave string number 0 for the chat script; after that
	 we want 1 more than the number of spaces in the chat_fail
	 string (the fencepost problem).  */
      cstrings = 2;
      for (zlook = qchat->zfail; *zlook != '\0'; zlook++)
	if (*zlook == ' ')
	  ++cstrings;

      azstrings = (char **) alloca (cstrings * sizeof (char *));
      aclens = (int *) alloca (cstrings * sizeof (int));

      zcopy = (char *) alloca (strlen (qchat->zfail) + 1);
      strcpy (zcopy, qchat->zfail);

      /* Get the strings into the array, and handle all the escape
	 characters.  */
      cstrings = 1;
      azstrings[1] = zcopy;
      for (z = zcopy; *z != '\0'; z++)
	{
	  if (*z == ' ')
	    {
	      *z++ = '\0';
	      aclens[cstrings] = ccescape (azstrings[cstrings]);
	      ++cstrings;
	      azstrings[cstrings] = z;
	    }
	}
      aclens[cstrings] = ccescape (azstrings[cstrings]);
      ++cstrings;
    }

  zbuf = (char *) alloca (strlen (zchat) + 1);

  while (*zchat != '\0')
    {
      int cchatlen;
      char *znext;

      /* Get this expect string into zbuf.  */
      cchatlen = strcspn (zchat, " \t");
      strncpy (zbuf, zchat, cchatlen);
      zbuf[cchatlen] = '\0';
      zchat += cchatlen;
      zchat += strspn (zchat, " \t");

      /* Separate out the first subexpect string.  */
      azstrings[0] = zbuf;
      znext = strchr (zbuf, '-');
      if (znext != NULL)
	*znext = '\0';
      aclens[0] = ccescape (azstrings[0]);

      /* Loop over subexpects and subsends.  */
      while (TRUE)
	{
	  char *zsub;

	  if (aclens[0] == 0
	      || (aclens[0] == 2
		  && strcmp (azstrings[0], "\"\"") == 0))
	    {
	      /* There is no subexpect sequence.  If there is a
		 subsend sequence we move on to it.  Otherwise we let
		 this expect succeed.  This is somewhat inconsistent,
		 but it seems to be the traditional approach.  */
	      if (znext == NULL)
		break;
	    }
	  else
	    {
	      int istr;

	      istr = icexpect (cstrings, azstrings, aclens,
			       qchat->ctimeout, qchat->fstrip);

	      /* If we found the string, break out of the
		 subexpect/subsend loop.  */
	      if (istr == 0)
		break;

	      /* If we got an error, return FALSE.  */
	      if (istr < -1)
		return FALSE;

	      /* If we found a failure string, log it and get out.  */
	      if (istr > 0)
		{
		  const char *zfail;
		  int clen;
		  char *zcopy;

		  zfail = qchat->zfail;
		  for (--istr; istr > 0; --istr)
		    zfail = strchr (zfail, ' ') + 1;
		  clen = strcspn (zfail, " ");
		  zcopy = (char *) alloca (clen + 1);
		  strncpy (zcopy, zfail, clen);
		  zcopy[clen] = '\0';
		  ulog (LOG_ERROR, "Chat script failed: Got \"%s\"",
			zcopy);
		  return FALSE;
		}

	      /* We timed out; look for a send subsequence.  If none,
		 the chat script has failed.  */
	      if (znext == NULL)
		{
		  ulog (LOG_ERROR, "Timed out in chat script");
		  return FALSE;
		}
	    }

	  /* Send the send subsequence.  A \"\" will send nothing.  An
	     empty string will send a carriage return.  */
	  ++znext;
	  zsub = znext;
	  znext = strchr (zsub, '-');
	  if (znext != NULL)
	    *znext = '\0';
	  if (! fcsend (zsub, qsys, qdial, zphone, ftranslate))
	    return FALSE;

	  /* If there is no expect subsequence, we are done.  */
	  if (znext == NULL)
	    break;

	  /* Move on to next expect subsequence.  */
	  ++znext;
	  azstrings[0] = znext;
	  znext = strchr (azstrings[0], '-');
	  if (znext != NULL)
	    *znext = '\0';
	  aclens[0] = ccescape (azstrings[0]);
	}

      /* We matched the expect string.  */
      if (*zchat == '\0')
	return TRUE;

      /* Copy the send string into zbuf.  */
      cchatlen = strcspn (zchat, " \t");
      strncpy (zbuf, zchat, cchatlen);
      zbuf[cchatlen] = '\0';
      zchat += cchatlen;
      zchat += strspn (zchat, " \t");

      if (*zbuf != '\0')
	{
	  if (! fcsend (zbuf, qsys, qdial, zphone, ftranslate))
	    return FALSE;
	}
    }

  /* The chat sequence has been completed.  */
  return TRUE;
}

/* Translate escape sequences within an expect string.  */

static int
ccescape (z)
     char *z;
{
  char *zto, *zfrom;
  
  zto = z;
  zfrom = z;
  while (*zfrom != '\0')
    {
      if (*zfrom != '\\')
	{
	  *zto++ = *zfrom++;
	  continue;
	}
      ++zfrom;
      switch (*zfrom)
	{
	case 'b':
	  *zto++ = '\b';
	  break;
	case 'n':
	  *zto++ = '\n';
	  break;
	case 'N':
	  *zto++ = '\0';
	  break;
	case 'r':
	  *zto++ = '\r';
	  break;
	case 's':
	  *zto++ = ' ';
	  break;
	case 't':
	  *zto++ = '\t';
	  break;
	case '\0':
	  --zfrom;
	  /* Fall through.  */
	case '\\':
	  *zto++ = '\\';
	  break;
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	  {
	    int i;

	    i = *zfrom - '0';
	    if (zfrom[1] >= '0' && zfrom[1] <= '7')
	      i = 8 * i + *++zfrom - '0';
	    if (zfrom[1] >= '0' && zfrom[1] <= '7')
	      i = 8 * i + *++zfrom - '0';
	    *zto++ = (char) i;
	  }
	  break;
	case 'x':
	  {
	    int i;

	    i = 0;
	    while (isxdigit (BUCHAR (zfrom[1])))
	      {
		if (isdigit (BUCHAR (zfrom[1])))
		  i = 16 * i + *++zfrom - '0';
		else if (isupper (BUCHAR (zfrom[1])))
		  i = 16 * i + *++zfrom - 'A';
		else
		  i = 16 * i + *++zfrom - 'a';
	      }
	    *zto++ = (char) i;
	  }
	  break;
	default:
	  ulog (LOG_ERROR,
		"Unrecognized escape sequence \\%c in expect string",
		*zfrom);
	  *zto++ = *zfrom;
	  break;
	}

      ++zfrom;
    }

  *zto = '\0';

  return zto - z;
}

/* Read characters and wait for one of a set of memory strings to come
   in.  This returns the index into the array of the string that
   arrives, or -1 on timeout, or -2 on error.  */

static int
icexpect (cstrings, azstrings, aclens, ctimeout, fstrip)
     int cstrings;
     char **azstrings;
     int *aclens;
     int ctimeout;
     boolean fstrip;
{
  int i;
  int cmin, cmax;
  char *zhave;
  int chave;
  long iendtime;
#if DEBUG > 1
  int cchars;
  int iolddebug;
#endif

  cmax = cmin = aclens[0];
  for (i = 1; i < cstrings; i++)
    {
      if (cmax < aclens[i])
	cmax = aclens[i];
      if (cmin > aclens[i])
	cmin = aclens[i];
    }

  zhave = (char *) alloca (cmax);
  chave = 0;

  iendtime = isysdep_time ((long *) NULL) + ctimeout;

#if DEBUG > 1
  cchars = 0;
  iolddebug = iDebug;
  if (FDEBUGGING (DEBUG_CHAT))
    {
      udebug_buffer ("icexpect: Looking for", azstrings[0],
		     aclens[0]);
      ulog (LOG_DEBUG_START, "icexpect: Got \"");
      iDebug &=~ (DEBUG_INCOMING | DEBUG_PORT);
    }
#endif

  while (TRUE)
    {
      int bchar;

      /* If we have no more time, get out.  */
      if (ctimeout <= 0)
	{
#if DEBUG > 1
	  if (FDEBUGGING (DEBUG_CHAT))
	    {
	      ulog (LOG_DEBUG_END, "\" (timed out)");
	      iDebug = iolddebug;
	    }
#endif
	  return -1;
	}

      /* Read one character at a time.  We could use a more complex
	 algorithm to read in larger batches, but it's probably not
	 worth it.  If the buffer is full, shift it left; we already
	 know that no string matches, and the buffer holds the largest
	 string, so this can't lose a match.  */
      if (chave >= cmax)
	{
	  xmemmove (zhave, zhave + 1, cmax - 1);
	  --chave;
	}

      /* The timeout/error return values from breceive_char are the
	 same as for this function.  */
      bchar = breceive_char (ctimeout, TRUE);
      if (bchar < 0)
	{
#if DEBUG > 1
	  if (FDEBUGGING (DEBUG_CHAT))
	    {
	      /* If there was an error, it will probably be logged in
		 the middle of our string, but this is only debugging
		 so it's not a big deal.  */
	      ulog (LOG_DEBUG_END, "\" (%s)",
		    bchar == -1 ? "timed out" : "error");
	      iDebug = iolddebug;
	    }
#endif
	  return bchar;
	}

      /* Strip the parity bit if desired.  */
      if (fstrip)
	bchar &= 0x7f;

      zhave[chave] = (char) bchar;
      ++chave;

#if DEBUG > 1
      if (FDEBUGGING (DEBUG_CHAT))
	{
	  char ab[5];

	  ++cchars;
	  if (cchars > 60)
	    {
	      ulog (LOG_DEBUG_END, "\"");
	      ulog (LOG_DEBUG_START, "icexpect: Got \"");
	      cchars = 0;
	    }
	  (void) cdebug_char (ab, bchar);
	  ulog (LOG_DEBUG_CONTINUE, "%s", ab);
	}
#endif

      /* See if any of the strings can be found in the buffer.  Since
	 we read one character at a time, the string can only be found
	 at the end of the buffer.  */
      for (i = 0; i < cstrings; i++)
	{
	  if (aclens[i] <= chave
	      && memcmp (zhave + chave - aclens[i], azstrings[i],
			 aclens[i]) == 0)
	    {
#if DEBUG > 1
	      if (FDEBUGGING (DEBUG_CHAT))
		{
		  if (i == 0)
		    ulog (LOG_DEBUG_END, "\" (found it)");
		  else
		    {
		      ulog (LOG_DEBUG_END, "\"");
		      udebug_buffer ("icexpect: Found", azstrings[i],
				     aclens[i]);
		    }
		  iDebug = iolddebug;
		}
#endif
	      return i;
	    }
	}

      ctimeout = (int) (iendtime - isysdep_time ((long *) NULL));
    }
}

#if DEBUG > 1

/* Debugging function for fcsend.  This takes the fquote variable, the
   length of the string (0 if this an informational string which can
   be printed directly) and the string itself.  It returns the new
   value for fquote.  The fquote variable is TRUE if the debugging
   output is in the middle of a quoted string.  */

static int cCsend_chars;
static int iColddebug;

static boolean fcsend_debug P((boolean, int, const char *));

static boolean
fcsend_debug (fquote, clen, zbuf)
     boolean fquote;
     int clen;
     const char *zbuf;
{
  int cwas;

  if (! FDEBUGGING (DEBUG_CHAT))
    return TRUE;

  cwas = cCsend_chars;
  if (clen > 0)
    cCsend_chars += clen;
  else
    cCsend_chars += strlen (zbuf);
  if (cCsend_chars > 60 && cwas > 10)
    {
      ulog (LOG_DEBUG_END, "%s", fquote ? "\"" : "");
      fquote = FALSE;
      ulog (LOG_DEBUG_START, "fcsend: Writing");
      cCsend_chars = 0;
    }

  if (clen == 0)
    {
      ulog (LOG_DEBUG_CONTINUE, "%s %s", fquote ? "\"" : "", zbuf);
      return FALSE;
    }
  else
    {
      int i;

      if (! fquote)
	ulog (LOG_DEBUG_CONTINUE, " \"");
      for (i = 0; i < clen; i++)
	{
	  char ab[5];

	  (void) cdebug_char (ab, zbuf[i]);
	  ulog (LOG_DEBUG_CONTINUE, "%s", ab);
	}

      return TRUE;
    }
}

/* Finish up the debugging information for fcsend.  */

static void ucsend_debug_end P((boolean, boolean));

static void
ucsend_debug_end (fquote, ferr)
     boolean fquote;
     boolean ferr;
{
  if (! FDEBUGGING (DEBUG_CHAT))
    return;

  if (fquote)
    ulog (LOG_DEBUG_CONTINUE, "\"");

  if (ferr)
    ulog (LOG_DEBUG_CONTINUE, " (error)");

  ulog (LOG_DEBUG_END, "%s", "");

  iDebug = iColddebug;
}

#else /* DEBUG <= 1 */

/* Use macro definitions to make fcsend look neater.  */

#define fcsend_debug(fquote, clen, zbuf) TRUE

#define ucsend_debug_end(fquote, ferror)

#endif /* DEBUG <= 1 */

/* Send a string out.  This has to parse escape sequences as it goes.
   Note that it handles the dialer escape sequences (\e, \E, \D, \T)
   although they make no sense for chatting with a system.  */

static boolean
fcsend (z, qsys, qdial, zphone, ftranslate)
     const char *z;
     const struct ssysteminfo *qsys;
     const struct sdialer *qdial;
     const char *zphone;
     boolean ftranslate;
{
  boolean fnocr;
  boolean (*pfwrite) P((const char *, int));
  char *zcallout_login;
  char *zcallout_pass;
  boolean fquote;

  if (strcmp (z, "\"\"") == 0)
    return TRUE;

  fnocr = FALSE;
  pfwrite = fport_write;
  zcallout_login = NULL;
  zcallout_pass = NULL;

#if DEBUG > 1
  if (FDEBUGGING (DEBUG_CHAT))
    {
      ulog (LOG_DEBUG_START, "fcsend: Writing");
      fquote = FALSE;
      cCsend_chars = 0;
      iColddebug = iDebug;
      iDebug &=~ (DEBUG_OUTGOING | DEBUG_PORT);
    }
#endif

  while (*z != '\0')
    {
      const char *zlook;
      boolean fsend;
      char bsend;

      zlook = z + strcspn (z, "\\BE");

      if (zlook > z)
	{
	  fquote = fcsend_debug (fquote, zlook - z, z);
	  if (! (*pfwrite) (z, zlook - z))
	    {
	      ucsend_debug_end (fquote, TRUE);
	      return FALSE;
	    }
	}

      if (*zlook == '\0')
	break;

      z = zlook;

      fsend = FALSE;
      switch (*z)
	{
	case 'B':
	  if (strncmp (z, "BREAK", 5) == 0)
	    {
	      fquote = fcsend_debug (fquote, 0, "break");
	      if (! fport_break ())
		{
		  ucsend_debug_end (fquote, TRUE);
		  return FALSE;
		}
	      z += 5;
	    }
	  else
	    {
	      fsend = TRUE;
	      bsend = 'B';
	      ++z;
	    }
	  break;
	case 'E':
	  if (strncmp (z, "EOT", 3) == 0)
	    {
	      fsend = TRUE;
	      bsend = '\004';
	    }
	  else
	    {
	      fsend = TRUE;
	      bsend = 'E';
	      ++z;
	    }
	  break;
	case '\\':
	  ++z;
	  switch (*z)
	    {
	    case 'b':
	      fsend = TRUE;
	      bsend = '\b';
	      break;
	    case 'c':
	      fnocr = TRUE;
	      break;
	    case 'd':
	      fquote = fcsend_debug (fquote, 0, "sleep");
	      usysdep_sleep (1);
	      break;
	    case 'e':
	      fquote = fcsend_debug (fquote, 0, "echo-check-off");
	      pfwrite = fport_write;
	      break;
	    case 'E':
	      fquote = fcsend_debug (fquote, 0, "echo-check-on");
	      pfwrite = fcecho_send;
	      break;
	    case 'K':
	      fquote = fcsend_debug (fquote, 0, "break");
	      if (! fport_break ())
		{
		  ucsend_debug_end (fquote, TRUE);
		  return FALSE;
		}
	      break;
	    case 'n':
	      fsend = TRUE;
	      bsend = '\n';
	      break;
	    case 'N':
	      fsend = TRUE;
	      bsend = '\0';
	      break;
	    case 'p':
	      fquote = fcsend_debug (fquote, 0, "pause");
	      usysdep_pause ();
	      break;
	    case 'r':
	      fsend = TRUE;
	      bsend = '\r';
	      break;
	    case 's':
	      fsend = TRUE;
	      bsend = ' ';
	      break;
	    case 't':
	      fsend = TRUE;
	      bsend = '\t';
	      break;
	    case '\0':
	      --z;
	      /* Fall through.  */
	    case '\\':
	      fsend = TRUE;
	      bsend = '\\';
	      break;
	    case '0': case '1': case '2': case '3': case '4':
	    case '5': case '6': case '7': case '8': case '9':
	      fsend = TRUE;
	      bsend = *z - '0';
	      if (z[1] >= '0' && z[1] <= '7')
		bsend = (char) (8 * bsend + *++z - '0');
	      if (z[1] >= '0' && z[1] <= '7')
		bsend = (char) (8 * bsend + *++z - '0');
	      break;
	    case 'x':
	      fsend = TRUE;
	      bsend = 0;
	      while (isxdigit (BUCHAR (z[1])))
		{
		  if (isdigit (BUCHAR (z[1])))
		    bsend = (char) (16 * bsend + *++z - '0');
		  else if (isupper (BUCHAR (z[1])))
		    bsend = (char) (16 * bsend + *++z - 'A');
		  else
		    bsend = (char) (16 * bsend + *++z - 'a');
		}
	      break;
	    case 'L':
	      {
		const char *zlog;

		if (qsys == NULL)
		  {
		    ucsend_debug_end (fquote, TRUE);
		    ulog (LOG_ERROR, "Illegal use of \\L");
		    return FALSE;
		  }
		zlog = qsys->zcall_login;
		if (zlog == NULL)
		  {
		    ucsend_debug_end (fquote, TRUE);
		    ulog (LOG_ERROR, "No login defined");
		    return FALSE;
		  }
		if (zlog[0] == '*' && zlog[1] == '\0')
		  {
		    if (zcallout_login == NULL
			&& ! fcallout_login (qsys, &zcallout_login,
					     &zcallout_pass))
		      {
			ucsend_debug_end (fquote, TRUE);
			return FALSE;
		      }
		    zlog = zcallout_login;
		  }
		fquote = fcsend_debug (fquote, 0, "login");
		fquote = fcsend_debug (fquote, strlen (zlog), zlog);
		if (! (*pfwrite) (zlog, strlen (zlog)))
		  {
		    ucsend_debug_end (fquote, TRUE);
		    return FALSE;
		  }
	      }
	      break;
	    case 'P':
	      {
		const char *zpass;

		if (qsys == NULL)
		  {
		    ucsend_debug_end (fquote, TRUE);
		    ulog (LOG_ERROR, "Illegal use of \\P");
		    return FALSE;
		  }
		zpass = qsys->zcall_password;
		if (zpass == NULL)
		  {
		    ucsend_debug_end (fquote, TRUE);
		    ulog (LOG_ERROR, "No password defined");
		    return FALSE;
		  }
		if (zpass[0] == '*' && zpass[1] == '\0')
		  {
		    if (zcallout_pass == NULL
			&& ! fcallout_login (qsys, &zcallout_login,
					     &zcallout_pass))
		      {
			ucsend_debug_end (fquote, TRUE);
			return FALSE;
		      }
		    zpass = zcallout_pass;
		  }
		fquote = fcsend_debug (fquote, 0, "password");
		fquote = fcsend_debug (fquote, strlen (zpass), zpass);
		if (! (*pfwrite) (zpass, strlen (zpass)))
		  {
		    ucsend_debug_end (fquote, TRUE);
		    return FALSE;
		  }
	      }
	      break;
	    case 'D':
	      if (qdial == NULL || zphone == NULL)
		{
		  ucsend_debug_end (fquote, TRUE);
		  ulog (LOG_ERROR, "Illegal use of \\D");
		  return FALSE;
		}
	      fquote = fcsend_debug (fquote, 0, "\\D");
	      if (! fcphone (qdial, zphone, pfwrite, ftranslate, &fquote))
		{
		  ucsend_debug_end (fquote, TRUE);
		  return FALSE;
		}
	      break;
	    case 'T':
	      if (qdial == NULL || zphone == NULL)
		{
		  ucsend_debug_end (fquote, TRUE);
		  ulog (LOG_ERROR, "Illegal use of \\T");
		  return FALSE;
		}
	      fquote = fcsend_debug (fquote, 0, "\\T");
	      if (! fcphone (qdial, zphone, pfwrite, TRUE, &fquote))
		{
		  ucsend_debug_end (fquote, TRUE);
		  return FALSE;
		}
	      break;
	    case 'M':
	      if (qdial == NULL)
		{
		  ucsend_debug_end (fquote, TRUE);
		  ulog (LOG_ERROR, "Illegal use of \\M");
		  return FALSE;
		}
	      fquote = fcsend_debug (fquote, 0, "ignore-carrier");
	      if (! fport_no_carrier ())
		{
		  ucsend_debug_end (fquote, TRUE);
		  return FALSE;
		}
	      break;
	    case 'm':
	      if (qdial == NULL)
		{
		  ucsend_debug_end (fquote, TRUE);
		  ulog (LOG_ERROR, "Illegal use of \\m");
		  return FALSE;
		}
	      fquote = fcsend_debug (fquote, 0, "need-carrier");
	      if (! fport_need_carrier ())
		{
		  ucsend_debug_end (fquote, TRUE);
		  return FALSE;
		}
	      break;
	    default:
	      /* This error message will screw up any debugging
		 information, but it's easily avoidable.  */
	      ulog (LOG_ERROR,
		    "Unrecognized escape sequence \\%c in send string",
		    *z);
	      fsend = TRUE;
	      bsend = *z;
	      break;
	    }
	  ++z;
	  break;
#if DEBUG > 0
	default:
	  ulog (LOG_FATAL, "fcsend: Can't happen");
	  break;
#endif
	}
      
      if (fsend)
	{
	  fquote = fcsend_debug (fquote, 1, &bsend);
	  if (! (*pfwrite) (&bsend, 1))
	    {
	      ucsend_debug_end (fquote, TRUE);
	      return FALSE;
	    }
	}
    }

  /* Clobber and free the login and password names that came from
     the call out file.  We probably shouldn't even keep them around
     this long.  */

  if (zcallout_login != NULL)
    {
      bzero (zcallout_login, strlen (zcallout_login));
      xfree ((pointer) zcallout_login);
    }
  if (zcallout_pass != NULL)
    {
      bzero (zcallout_pass, strlen (zcallout_pass));
      xfree ((pointer) zcallout_pass);
    }

  /* Output a final carriage return, unless there was a \c.  Don't
     bother to check for an echo.  */
  if (! fnocr)
    {
      char b;

      b = '\r';
      fquote = fcsend_debug (fquote, 1, &b);
      if (! fport_write (&b, 1))
	{
	  ucsend_debug_end (fquote, TRUE);
	  return FALSE;
	}
    }

  ucsend_debug_end (fquote, FALSE);

  return TRUE;
}

/* Write out a phone number with optional dialcode translation.  The
   pfquote argument is only used for debugging.  */

static boolean
fcphone (qdial, zphone, pfwrite, ftranslate, pfquote)
     const struct sdialer *qdial;
     const char *zphone;
     boolean (*pfwrite) P((const char *zwrite, int cwrite));
     boolean ftranslate;
     boolean *pfquote;
{
  const char *zprefix, *zsuffix;

  if (ftranslate)
    {
      if (! fctranslate (zphone, &zprefix, &zsuffix))
	return FALSE;
    }
  else
    {
      zprefix = zphone;
      zsuffix = NULL;
    }

  while (zprefix != NULL)
    {
      while (TRUE)
	{
	  const char *z;
	  const char *zstr;

	  z = zprefix + strcspn (zprefix, "=-");
	  if (z > zprefix)
	    {
	      *pfquote = fcsend_debug (*pfquote, z - zprefix, zprefix);
	      if (! (*pfwrite) (zprefix, z - zprefix))
		return FALSE;
	    }

	  if (*z == '=')
	    zstr = qdial->zdialtone;
	  else if (*z == '-')
	    zstr = qdial->zpause;
	  else			/* *z == '\0' */
	    break;

	  if (zstr != NULL)
	    {
	      *pfquote = fcsend_debug (*pfquote, strlen (zstr), zstr);
	      if (! (*pfwrite) (zstr, strlen (zstr)))
		return FALSE;
	    }

	  zprefix = z + 1;
	}

      zprefix = zsuffix;
      zsuffix = NULL;
    }

  return TRUE;
}

/* Given a phone number, run it through dial code translation
   returning two strings.  */

static boolean
fctranslate (zphone, pzprefix, pzsuffix)
     const char *zphone;
     const char **pzprefix;
     const char **pzsuffix;
{
  char *zdialcode, *zto;
  const char *zfrom;

  *pzprefix = zphone;
  *pzsuffix = NULL;

  if (zDialcodefile == NULL)
    return TRUE;

  zdialcode = (char *) alloca (strlen (zphone) + 1);
  zfrom = zphone;
  zto = zdialcode;
  while (*zfrom != '\0' && isalpha (BUCHAR (*zfrom)))
    *zto++ = *zfrom++;
  *zto = '\0';

  if (*zdialcode != '\0')
    {
      struct smulti_file *qmulti;
      struct scmdtab as[2];
      char *zpre;

      qmulti = qmulti_open (zDialcodefile);
      if (qmulti == NULL)
	return FALSE;

      as[0].zcmd = zdialcode;
      as[0].itype = CMDTABTYPE_STRING;
      as[0].pvar = (pointer) &zpre;
      as[0].ptfn = NULL;
      as[1].zcmd = NULL;

      zpre = NULL;

      uprocesscmds ((FILE *) NULL, qmulti, as, (const char *) NULL,
		    CMDFLAG_BACKSLASH);

      (void) fmulti_close (qmulti);

      if (zpre == NULL)
	ulog (LOG_ERROR, "Unknown dial code %s", zdialcode);
      else
	{
	  *pzprefix = zpre;
	  *pzsuffix = zfrom;
	}
    }

  return TRUE;
}

/* Write out a string making sure the each character is echoed back.  */

static boolean
fcecho_send (zwrite, cwrite)
     const char *zwrite;
     int cwrite;
{
  const char *zend;

  zend = zwrite + cwrite;

  for (; zwrite < zend; zwrite++)
    {
      int b;

      if (! fport_write (zwrite, 1))
	return FALSE;
      do
	{
	  /* We arbitrarily wait five seconds for the echo.  */
	  b = breceive_char (5, TRUE);
	  /* Now b == -1 on timeout, -2 on error.  */
	  if (b < 0)
	    {
	      if (b == -1)
		ulog (LOG_ERROR, "Character not echoed");
	      return FALSE;
	    }
	}
      while (b != BUCHAR (*zwrite));
    }

  return TRUE;
}

/* Run a chat program.  Expand any escape sequences and call a system
   dependent program to run it.  */

static boolean
fcprogram (zprogram, qsys, qdial, zphone, zport, ibaud)
     const char *zprogram;
     const struct ssysteminfo *qsys;
     const struct sdialer *qdial;
     const char *zphone;
     const char *zport;
     long ibaud;
{
  char *zbuf;
  int calc, clen;
  char *zto;
  const char *zfrom;
  char *zcallout_login;
  char *zcallout_pass;
  boolean fret;

  zcallout_login = NULL;
  zcallout_pass = NULL;

  /* Copy the string into memory expanding escape sequences.  */

  zbuf = (char *) xmalloc (1);
  calc = 1;
  clen = 0;
  zto = zbuf;
  for (zfrom = zprogram; *zfrom != '\0'; zfrom++)
    {
      const char *zadd;
      int cadd;

      if (*zfrom != '\\')
	{
	  if (clen + 2 > calc)
	    {
	      calc = clen + 80;
	      zbuf = (char *) xrealloc ((pointer) zbuf, calc);
	      zto = zbuf + clen;
	    }
	  *zto++ = *zfrom;
	  ++clen;
	  continue;
	}

      ++zfrom;
      switch (*zfrom)
	{
	case '\0':
	  --zfrom;
	  /* Fall through.  */
	case '\\':
	  zadd = "\\";
	  break;
	case 'L':
	  {
	    const char *zlog;

	    if (qsys == NULL)
	      {
		ulog (LOG_ERROR, "chat-program: Illegal use of \\L");
		return FALSE;
	      }
	    zlog = qsys->zcall_login;
	    if (zlog == NULL)
	      {
		ulog (LOG_ERROR, "chat-program: No login defined");
		return FALSE;
	      }
	    if (zlog[0] == '*' && zlog[1] == '\0')
	      {
		if (zcallout_login == NULL
		    && ! fcallout_login (qsys, &zcallout_login,
					 &zcallout_pass))
		  return FALSE;
		zlog = zcallout_login;
	      }
	    zadd = zlog;
	  }
	  break;
	case 'P':
	  {
	    const char *zpass;

	    if (qsys == NULL)
	      {
		ulog (LOG_ERROR, "chat-program: Illegal use of \\P");
		return FALSE;
	      }
	    zpass = qsys->zcall_password;
	    if (zpass == NULL)
	      {
		ulog (LOG_ERROR, "chat-program: No password defined");
		return FALSE;
	      }
	    if (zpass[0] == '*' && zpass[1] == '\0')
	      {
		if (zcallout_pass == NULL
		    && ! fcallout_login (qsys, &zcallout_login,
					 &zcallout_pass))
		  return FALSE;
		zpass = zcallout_pass;
	      }
	    zadd = zpass;
	  }
	  break;
	case 'D':
	  if (qdial == NULL || zphone == NULL)
	    {
	      ulog (LOG_ERROR, "chat-program: Illegal use of \\D");
	      return FALSE;
	    }
	  zadd = zphone;
	  break;
	case 'T':
	  {
	    const char *zprefix, *zsuffix;

	    if (qdial == NULL || zphone == NULL)
	      {
		ulog (LOG_ERROR, "chat-program: Illegal use of \\T");
		return FALSE;
	      }

	    if (! fctranslate (zphone, &zprefix, &zsuffix))
	      return FALSE;

	    if (zsuffix == NULL)
	      zadd = zprefix;
	    else
	      {
		int cprefix;

		cprefix = strlen (zprefix);
		if (clen + cprefix + 1 > calc)
		  {
		    calc = clen + cprefix + 20;
		    zbuf = (char *) xrealloc ((pointer) zbuf, calc);
		    zto = zbuf + clen;
		  }
		strcpy (zto, zprefix);
		zto += cprefix;
		clen += cprefix;
		zadd = zsuffix;
	      }
	  }
	  break;
	case 'Y':
	  if (zLdevice == NULL && zport == NULL)
	    {
	      ulog (LOG_ERROR, "chat-program: Illegal use of \\Y");
	      return FALSE;
	    }
	  /* zLdevice will generally make more sense than zport, but
	     it might not be set yet.  */
	  zadd = zLdevice;
	  if (zadd == NULL)
	    zadd = zport;
	  break;
	case 'Z':
	  if (qsys == NULL)
	    {
	      ulog (LOG_ERROR, "chat-program: Illegal use of \\Z");
	      return FALSE;
	    }
	  zadd = qsys->zname;
	  break;
	case 'S':
	  {
	    char *zalc;

	    if (ibaud == 0)
	      {
		ulog (LOG_ERROR, "chat-program: Illegal use of \\S");
		return FALSE;
	      }
	    zalc = (char *) alloca (15);
	    sprintf (zalc, "%ld", ibaud);
	    zadd = zalc;
	  }
	  break;
	default:
	  {
	    char *zset;

	    ulog (LOG_ERROR,
		  "chat-program: Unrecognized escape sequence \\%c",
		  *zfrom);
	    zset = (char *) alloca (2);
	    zset[0] = *zfrom;
	    zset[1] = '\0';
	    zadd = zset;
	  }
	  break;
	}

      cadd = strlen (zadd);
      if (clen + cadd + 1 > calc)
	{
	  calc = clen + cadd + 20;
	  zbuf = (char *) xrealloc ((pointer) zbuf, calc);
	  zto = zbuf + clen;
	}
      strcpy (zto, zadd);
      zto += cadd;
      clen += cadd;
    }

  *zto = '\0';

  /* Invoke the program.  */

  fret = fport_run_chat (zbuf);

  xfree ((pointer) zbuf);

  return fret;
}
