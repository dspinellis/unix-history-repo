#include "../h/rt.h"
#include "../h/record.h"

#define STRINGLIMIT	16              /* Limit on length of imaged string */
#define LISTLIMIT	6		/* Limit on list items in image */

/*
 * outimage(f,d,restrict) - print image of d on file f.
 * If restrict is non-zero, fields of records will not be imaged.
 */

outimage(f, d, restrict)
FILE *f;
struct descrip *d;
int restrict;
   {
   register int i, j;
   register char *s;
   register union block *bp;
   char *type;
   FILE *fd;
   struct descrip q;
   extern char *blkname[];

outimg:
   if (NULLDESC(*d)) {
      if (restrict == 0)
         fprintf(f, "&null");
      return;
      }

   if (QUAL(*d)) {
      i = STRLEN(*d);
      s = STRLOC(*d);
      j = MIN(i, STRINGLIMIT);
      putc('"', f);
      while (j-- > 0)
         printimage(f, *s++, '"');
      if (i > STRINGLIMIT)
         fprintf(f, "...");
      putc('"', f);
      return;
      }

   if (VAR(*d) && !TVAR(*d)) {
      fprintf(f, "variable = ");
      d = VARLOC(*d);
      goto outimg;	/* tail recursion */
      }

   switch (TYPE(*d)) {

      case T_INTEGER:
         fprintf(f, "%d", INTVAL(*d));
         return;

#ifndef BIT32
      case T_LONGINT:
         fprintf(f, "%ld", BLKLOC(*d)->longint.intval);
         return;

#endif
      case T_REAL:
         fprintf(f, "%.8g", BLKLOC(*d)->real.realval);
         return;

      case T_CSET:
         if (BLKLOC(*d) == &k_ascii) {
            fprintf(f, "&ascii");
            return;
            }
         else if (BLKLOC(*d) == &k_cset) {
            fprintf(f, "&cset");
            return;
            }
         else if (BLKLOC(*d) == &k_lcase) {
            fprintf(f, "&lcase");
            return;
            }
         else if (BLKLOC(*d) == &k_ucase) {
            fprintf(f, "&ucase");
            return;
            }
	 putc('\'', f);
         j = STRINGLIMIT;
	 for (i = 0; i < 256; i++) {
	    if (tstb(i, BLKLOC(*d)->cset.bits)) {
               if (j-- <= 0) {
	          fprintf(f, "...");
	          break;
	          }
               printimage(f, i, '\'');
   	       }
	    }
	 putc('\'', f);
         return;

      case T_FILE:
         if ((fd = BLKLOC(*d)->file.fd) == stdin)
            fprintf(f, "&input");
         else if (fd == stdout)
            fprintf(f, "&output");
         else if (fd == stderr)
            fprintf(f, "&output");
         else {
            i = STRLEN(BLKLOC(*d)->file.fname);
            s = STRLOC(BLKLOC(*d)->file.fname);
            fprintf(f, "file(");
            while (i-- > 0)
               printimage(f, *s++, '\0');
	    putc(')', f);
            }
         return;

      case T_PROC:
         i = STRLEN(BLKLOC(*d)->proc.pname);
	 s = STRLOC(BLKLOC(*d)->proc.pname);
	 switch (BLKLOC(*d)->proc.ndynam) {
	    default:  type = "procedure"; break;
	    case -1:  type = "function"; break;
	    case -2:  type = "record constructor"; break;
	    }
         fprintf(f, "%s ", type);
	 while (i-- > 0)
            printimage(f, *s++, '\0');
         return;

      case T_LIST:
	 listimage(f, BLKLOC(*d), restrict);
         return;

      case T_TABLE:
         fprintf(f, "table(%d)", BLKLOC(*d)->table.cursize);
         return;

      case T_RECORD:
         bp = BLKLOC(*d);
	 i = STRLEN(bp->record.recptr->proc.recname);
	 s = STRLOC(bp->record.recptr->proc.recname);
         fprintf(f, "record ");
	 while (i-- > 0)
            printimage(f, *s++, '\0');
         j = bp->record.recptr->proc.nfields;
	 if (j <= 0)
	    fprintf(f, "()");
	 else if (restrict > 0)
	    fprintf(f, "(%d)", j);
	 else {
	    putc('(', f);
	    i = 0;
	    for (;;) {
	       outimage(f, &bp->record.fields[i], restrict+1);
	       if (++i >= j)
	          break;
	       putc(',', f);
	       }
	    putc(')', f);
	    }
         return;

      case T_TVSUBS:
	 bp = BLKLOC(*d);
	 outimage(f, VARLOC(bp->tvsubs.ssvar), restrict);
	 if (bp->tvsubs.sslen == 1)
	    fprintf(f, "[%d]", bp->tvsubs.sspos);
	 else
	    fprintf(f, "[%d+:%d]", bp->tvsubs.sspos, bp->tvsubs.sslen);
	 if (QUAL(*VARLOC(bp->tvsubs.ssvar))) {
   	    STRLEN(q) = bp->tvsubs.sslen;
   	    STRLOC(q) = STRLOC(*VARLOC(bp->tvsubs.ssvar)) + bp->tvsubs.sspos-1;
	    fprintf(f, " = ");
	    d = &q;
	    goto outimg;
	    }
	 return;

      case T_TVTBL:
	 bp = BLKLOC(*d);
	 outimage(f, &bp->tvtbl.tvtable, restrict);
	 putc('[', f);
	 outimage(f, &bp->tvtbl.tvtref, restrict);
	 putc(']', f);
	 return;

      case T_TVPOS:
	 fprintf(f, "&pos = %d", k_pos);
	 return;

      case T_TVRAND:
	 fprintf(f, "&random = %ld", k_random);
	 return;

      case T_TVTRACE:
	 fprintf(f, "&trace = %d", k_trace);
	 return;

      case T_ESTACK:
	 fprintf(f, "co-expression");
	 return;

      default:
         if (TYPE(*d) <= MAXTYPE)
   	    fprintf(f, "%s", blkname[TYPE(*d)]);
	 else
	    syserr("outimage: unknown type");
      }
   }

/*
 * printimage - print one character, with escape conventions.
 */

static printimage(f, c, q)
FILE *f;
int c, q;
   {
   if (c >= ' ' && c < '\177') {
      switch (c) {
         case '"':                      /*      d. quote     */
            if (c != q) goto def;
	    fprintf(f, "\\\"");
            return;
         case '\'':                     /*      s. quote     */
            if (c != q) goto def;
            fprintf(f, "\\'");
            return;
         case '\\':                     /*      backslash    */
            fprintf(f, "\\\\");
            return;
         default:                       /*      normal ch.   */
	 def:
            putc(c, f);
            return;
         }
      }

   switch (c) {                         /* special character */
      case '\b':                        /*      backspace    */
         fprintf(f, "\\b");
         return;
      case '\177':                      /*      delete       */
         fprintf(f, "\\d");
         return;
      case '\33':                       /*      escape       */
         fprintf(f, "\\e");
         return;
      case '\f':                        /*      form feed    */
         fprintf(f, "\\f");
         return;
      case '\n':                        /*      new line     */
         fprintf(f, "\\n");
         return;
      case '\r':                        /*      return       */
         fprintf(f, "\\r");
         return;
      case '\t':                        /*      hor. tab     */
         fprintf(f, "\\t");
         return;
      case '\13':                       /*      ver. tab     */
         fprintf(f, "\\v");
         return;
      default:                          /*      octal cons.  */
         fprintf(f, "\\%03o", c&0377);
         return;
      }
   }

/*
 * listimage - print an image of list.
 */

static listimage(f, lp, restrict)
FILE *f;
struct b_list *lp;
int restrict;
   {
   register int i, j;
   register struct b_listb *bp;
   int size, count;

   bp = BLKLOC(lp->listhead);
   size = lp->cursize;

   if (restrict > 0 && size > 0) {
      fprintf(f, "list(%d)", size);
      return;
      }

   putc('[', f);
   count = 1;
   i = 0;
   if (size > 0) {
      for (;;) {
         if (++i > bp->nused) {
	    i = 1;
	    bp = BLKLOC(bp->listnext);
	    }
	 if (count <= LISTLIMIT/2 || count > size - LISTLIMIT/2) {
            j = bp->first + i - 1;
            if (j >= bp->nelem)
	       j -= bp->nelem;
            outimage(f, &bp->lelem[j], restrict+1);
	    if (count >= size)
	       break;
            putc(',', f);
            }
         else if (count == LISTLIMIT/2 + 1)
	    fprintf(f, "...,");
	 count++;
         }
      }
   putc(']', f);
   }
