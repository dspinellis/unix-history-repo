#include "../h/rt.h"
#include "../h/record.h"

/*
 * image(x) - return string giving image of object x.
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

   deref(&arg1);

   if (NULLDESC(arg1)) {
      STRLOC(arg0) = "&null";
      STRLEN(arg0) = 5;
      return;
      }

   if (QUAL(arg1)) {
      sneed(prescan(&arg1) + 2);
      len = STRLEN(arg1);
      s = STRLOC(arg1);
      outlen = 2;
      STRLOC(arg0) = alcstr("\"", 1);
                     while (len-- > 0)
                        outlen += doimage(*s++, '"');
                     alcstr("\"", 1);
      STRLEN(arg0) = outlen;
      return;
      }

   switch (TYPE(arg1)) {
      case T_INTEGER:
#ifndef BIT32
      case T_LONGINT:
#endif
      case T_REAL:
         cvstr(&arg1, sbuf);
	 len = STRLEN(arg1);
         sneed(len);
	 STRLOC(arg0) = alcstr(STRLOC(arg1), len);
	 STRLEN(arg0) = len;
         return;

      case T_CSET:
         if (BLKLOC(arg1) == &k_ascii) {
            STRLOC(arg0) = "&ascii";
            STRLEN(arg0) = 6;
            return;
            }
         else if (BLKLOC(arg1) == &k_cset) {
            STRLOC(arg0) = "&cset";
            STRLEN(arg0) = 5;
            return;
            }
         else if (BLKLOC(arg1) == &k_lcase) {
            STRLOC(arg0) = "&lcase";
            STRLEN(arg0) = 6;
            return;
            }
         else if (BLKLOC(arg1) == &k_ucase) {
            STRLOC(arg0) = "&ucase";
            STRLEN(arg0) = 6;
            return;
            }
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
         bp = BLKLOC(arg1);
         sprintf(sbuf, "list(%d)", bp->list.cursize);
         len = strlen(sbuf);
         sneed(len);
         STRLOC(arg0) = alcstr(sbuf, len);
         STRLEN(arg0) = len;
         return;

      case T_TABLE:
         bp = BLKLOC(arg1);
         sprintf(sbuf, "table(%d)", bp->table.cursize);
         len = strlen(sbuf);
         sneed(len);
         STRLOC(arg0) = alcstr(sbuf, len);
         STRLEN(arg0) = len;
         return;

      case T_RECORD:
         bp = BLKLOC(arg1);
         rnlen = STRLEN(bp->record.recptr->proc.recname);
         sneed(15 + rnlen);
         bp = BLKLOC(arg1);
         sprintf(sbuf, "(%d)", bp->record.recptr->proc.nfields);
         len = strlen(sbuf);
         STRLOC(arg0) = alcstr("record ", 7);
                        alcstr(STRLOC(bp->record.recptr->proc.recname),
                               rnlen);
                        alcstr(sbuf, len);
         STRLEN(arg0) = 7 + len + rnlen;
         return;

      case T_ESTACK:
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

struct b_iproc Bimage = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Ximage),
   1,
   -1,
   0,
   0,
   {5, "image"}
   };

/*
 * doimage(c,q) - allocate character c in string space, with escape
 * conventions if c is unprintable, '\', or equal to q.
 * Returns number of characters allocated.
 */

doimage(c, q)
int c, q;
   {
   static char *cbuf = "\\\0\0\0";
   extern char *alcstr();

   if (c >= ' ' && c < '\177') {
      switch (c) {
         case '"':                         /*      d. quote     */
            if (c != q) goto def;
	    alcstr("\\\"", 2);
            return (2);
         case '\'':                        /*      s. quote     */
            if (c != q) goto def;
            alcstr("\\'", 2);
            return (2);
         case '\\':                        /*      backslash    */
            alcstr("\\\\", 2);
            return (2);
         default:                          /*      normal ch.   */
	 def:
            cbuf[0] = c;
            cbuf[1] = '\0'; /* Do we need this? --whm */
            alcstr(cbuf,1);
            return (1);
         }
      }

   switch (c) {                         /* special character */
      case '\b':                        /*      backspace    */
         alcstr("\\b", 2);
         return (2);
      case '\177':                      /*      delete       */
         alcstr("\\d", 2);
         return (2);
      case '\33':                       /*      escape       */
         alcstr("\\e", 2);
         return (2);
      case '\f':                        /*      form feed    */
         alcstr("\\f", 2);
         return (2);
      case '\n':                        /*      new line     */
         alcstr("\\n", 2);
         return (2);
      case '\r':                        /*      return       */
         alcstr("\\r", 2);
         return (2);
      case '\t':                        /*      hor. tab     */
         alcstr("\\t", 2);
         return (2);
      case '\13':                       /*      ver. tab     */
         alcstr("\\v", 2);
         return (2);
      default:                          /*      octal cons.  */
      	 cbuf[0] = '\\';
         cbuf[1] = ((c&0300) >> 6) + '0';
         cbuf[2] = ((c&070) >> 3) + '0';
         cbuf[3] = (c&07) + '0';
         alcstr(cbuf, 4);
         return (4);
      }
   }

/*
 * prescan(d) - return upper bound on length of expanded string.
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
