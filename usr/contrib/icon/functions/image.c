#include "../h/rt.h"
#include "../h/record.h"

/*
 * image(x) - return string image of object x.  Nothing fancy here,
 *  just plug and chug on a case-wise basis.
 */

Ximage(nargs, arg1, arg0)
int nargs;
struct descrip arg1, arg0;
   {
   register int len, outlen, rnlen;
   register char *s;
   register union block *bp;
   char *type;
   extern char *alcstr();
   extern struct descrip *cstos();
   char sbuf[MAXSTRING];
   FILE *fd;

   DeRef(arg1)

   if (NULLDESC(arg1)) {	/* &null */
      STRLOC(arg0) = "&null";
      STRLEN(arg0) = 5;
      return;
      }

   if (QUAL(arg1)) {
      /*
       * Get some string space.  The magic 2 is for the double quote at each
       *  end of the resulting string.
       */
      sneed(prescan(&arg1) + 2);
      len = STRLEN(arg1);
      s = STRLOC(arg1);
      outlen = 2;
      /*
       * Form the image by putting a " in the string space, calling
       *  doimage with each character in the string, and then putting
       *  a " at then end.  Note that doimage directly writes into the
       *  string space.  (Hence the indentation.)  This techinique is used
       *  several times in this routine.
       */
      STRLOC(arg0) = alcstr("\"", 1);
                     while (len-- > 0)
                         outlen += doimage(*s++, '"');
                         alcstr("\"", 1);
      STRLEN(arg0) = outlen;
      return;
      }

   switch (TYPE(arg1)) {
      case T_INTEGER:
#ifdef LONGS
      case T_LONGINT:
#endif LONGS
      case T_REAL:
         /*
          * Form a string representing the number and allocate it.
          */
         cvstr(&arg1, sbuf);
         len = STRLEN(arg1);
         sneed(len);
         STRLOC(arg0) = alcstr(STRLOC(arg1), len);
         STRLEN(arg0) = len;
         return;

      case T_CSET:
         /*
          * Check for distinguished csets by looking at the address of
          *  of the object to image.  If one is found, make a string
          *  naming it and return.
          */
         if (BLKLOC(arg1) == ((union block *) &k_ascii)) {
            STRLOC(arg0) = "&ascii";
            STRLEN(arg0) = 6;
            return;
            }
         else if (BLKLOC(arg1) == ((union block *) &k_cset)) {
            STRLOC(arg0) = "&cset";
            STRLEN(arg0) = 5;
            return;
            }
         else if (BLKLOC(arg1) == ((union block *) &k_lcase)) {
            STRLOC(arg0) = "&lcase";
            STRLEN(arg0) = 6;
            return;
            }
         else if (BLKLOC(arg1) == ((union block *) &k_ucase)) {
            STRLOC(arg0) = "&ucase";
            STRLEN(arg0) = 6;
            return;
            }
         /*
          * Convert the cset to a string and proceed as is done for
          *  string images but use a ' rather than " to bound the
          *  result string.
          */
         cvstr(&arg1, sbuf);
         sneed(prescan(&arg1) + 2);
         len = STRLEN(arg1);
         s = STRLOC(arg1);
         outlen = 2;
         STRLOC(arg0) = alcstr("'", 1);
                        while (len-- > 0)
                            outlen += doimage(*s++, '\'');
                        alcstr("'", 1);
         STRLEN(arg0) = outlen;
         return;

      case T_FILE:
         /*
          * Check for distinguished files by looking at the address of
          *  of the object to image.  If one is found, make a string
          *  naming it and return.
          */
         if ((fd = BLKLOC(arg1)->file.fd) == stdin) {
            STRLEN(arg0) = 6;
            STRLOC(arg0) = "&input";
            }
         else if (fd == stdout) {
            STRLEN(arg0) = 7;
            STRLOC(arg0) = "&output";
            }
         else if (fd == stderr) {
            STRLEN(arg0) = 7;
            STRLOC(arg0) = "&errout";
            }
         else {
            /*
             * The file is not a standard one, form a string of the form
             *  file(nm) where nm is the argument originally given to
             *  open.
             */
            sneed(prescan(&BLKLOC(arg1)->file.fname)+6);
            len = STRLEN(BLKLOC(arg1)->file.fname);
            s = STRLOC(BLKLOC(arg1)->file.fname);
            outlen = 6;
            STRLOC(arg0) = alcstr("file(", 5);
                           while (len-- > 0)
                              outlen += doimage(*s++, '\0');
                           alcstr(")", 1);
            STRLEN(arg0) = outlen;
            }
         return;

      case T_PROC:
         /*
          * Produce one of:
          *  "procedure name"
          *  "function name"
          *  "record constructor name"
          *
          * Note that the number of dynamic locals is used to determine
          *  what type of "procedure" is at hand.
          */
         len = STRLEN(BLKLOC(arg1)->proc.pname);
         s = STRLOC(BLKLOC(arg1)->proc.pname);
         switch (BLKLOC(arg1)->proc.ndynam) {
            default:  type = "procedure "; break;
            case -1:  type = "function "; break;
            case -2:  type = "record constructor "; break;
            }
         outlen = strlen(type);
         sneed(len + outlen);
         STRLOC(arg0) = alcstr(type, outlen);
                        alcstr(s, len);
         STRLEN(arg0) = len + outlen;
         return;

      case T_LIST:
         /*
          * Produce:
          *  "list(n)"
          * where n is the current size of the list.
          */
         bp = BLKLOC(arg1);
         sprintf(sbuf, "list(%d)", bp->list.cursize);
         len = strlen(sbuf);
         sneed(len);
         STRLOC(arg0) = alcstr(sbuf, len);
         STRLEN(arg0) = len;
         return;

      case T_LELEM:
         STRLEN(arg0) = 18;
         STRLOC(arg0) = "list element block";
         return;

      case T_TABLE:
         /*
          * Produce:
          *  "table(n)"
          * where n is the size of the table.
          */
         bp = BLKLOC(arg1);
         sprintf(sbuf, "table(%d)", bp->table.cursize);
         len = strlen(sbuf);
         sneed(len);
         STRLOC(arg0) = alcstr(sbuf, len);
         STRLEN(arg0) = len;
         return;

      case T_TELEM:
         STRLEN(arg0) = 19;
         STRLOC(arg0) = "table element block";
         return;

#ifdef SETS
      case T_SET:
         /*
          * Produce "set(n)" where n is size of the set.
          */
         bp = BLKLOC(arg1);
         sprintf(sbuf, "set(%d)", bp->set.setsize);
         len = strlen(sbuf);
         sneed(len);
         STRLOC(arg0) = alcstr(sbuf,len);
         STRLEN(arg0) = len;
         return;

      case T_SELEM:
         STRLEN(arg0) = 17;
         STRLOC(arg0) = "set element block";
         return;
#endif SETS

      case T_RECORD:
         /*
          * Produce:
          *  "record name(n)"
          * where n is the number of fields.
          */
         bp = BLKLOC(arg1);
         rnlen = STRLEN(bp->record.recptr->recname);
         sneed(15 + rnlen);	/* 15 = *"record " + *"(nnnnnn)" */
         bp = BLKLOC(arg1);
         sprintf(sbuf, "(%d)", bp->record.recptr->nfields);
         len = strlen(sbuf);
         STRLOC(arg0) = alcstr("record ", 7);
                        alcstr(STRLOC(bp->record.recptr->recname),
                               rnlen);
                        alcstr(sbuf, len);
         STRLEN(arg0) = 7 + len + rnlen;
         return;

      case T_ESTACK:
         /*
          * Produce:
          *  "co-expression(n)"
          * where n is the number of results that have been produced.
          */
         sneed(22);
         sprintf(sbuf, "(%d)", BLKLOC(arg1)->estack.nresults);
         len = strlen(sbuf);
         STRLOC(arg0) = alcstr("co-expression", 13);
                        alcstr(sbuf, len);
         STRLEN(arg0) = 13 + len;
         return;

      default:
         syserr("image: unknown type.");
      }
   }

Procblock(image,1)

/*
 * doimage(c,q) - allocate character c in string space, with escape
 *  conventions if c is unprintable, '\', or equal to q.
 *  Returns number of characters allocated.
 */

doimage(c, q)
int c, q;
   {
   static char *cbuf = "\\\0\0\0";
   extern char *alcstr();

   if (c >= ' ' && c < '\177') {
      /*
       * c is printable, but special case ", ', and \.
       */
      switch (c) {
         case '"':
            if (c != q) goto def;
            alcstr("\\\"", 2);
            return (2);
         case '\'':
            if (c != q) goto def;
            alcstr("\\'", 2);
            return (2);
         case '\\':
            alcstr("\\\\", 2);
            return (2);
         default:
         def:
            cbuf[0] = c;
            cbuf[1] = '\0';
            alcstr(cbuf,1);
            return (1);
         }
      }

   /*
    * c is some sort of unprintable character.  If it is one of the common
    *  ones, produce a special representation for it, otherwise, produce
    *  its octal value.
    */
   switch (c) {
      case '\b':			/*      backspace    */
         alcstr("\\b", 2);
         return (2);
      case '\177':			/*      delete       */
         alcstr("\\d", 2);
         return (2);
      case '\33':			/*      escape       */
         alcstr("\\e", 2);
         return (2);
      case '\f':			/*      form feed    */
         alcstr("\\f", 2);
         return (2);
      case '\n':			/*      new line     */
         alcstr("\\n", 2);
         return (2);
      case '\r':			/*      return       */
         alcstr("\\r", 2);
         return (2);
      case '\t':			/*      horizontal tab     */
         alcstr("\\t", 2);
         return (2);
      case '\13':			/*      vertical tab     */
         alcstr("\\v", 2);
         return (2);
      default:				/*      octal constant  */
         cbuf[0] = '\\';
         cbuf[1] = ((c&0300) >> 6) + '0';
         cbuf[2] = ((c&070) >> 3) + '0';
         cbuf[3] = (c&07) + '0';
         alcstr(cbuf, 4);
         return (4);
      }
   }

/*
 * prescan(d) - return upper bound on length of expanded string.  Note
 *  that the only time that prescan is wrong is when the string contains
 *  one of the "special" unprintable characters, e.g. tab.
 */
prescan(d)
struct descrip *d;
   {
   register int slen, len;
   register char *s, c;

   s = STRLOC(*d);
   len = 0;
   for (slen = STRLEN(*d); slen > 0; slen--)
      if ((c = (*s++)) < ' ' || c >= 0177)
         len += 4;
      else if (c == '"' || c == '\\' || c == '\'')
         len += 2;
      else
         len++;

   return (len);
   }
