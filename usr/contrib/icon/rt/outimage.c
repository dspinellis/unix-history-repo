#include "../h/rt.h"
#include "../h/record.h"

#define STRINGLIMIT	16		/* limit on length of imaged string */
#define LISTLIMIT	 6		/* limit on list items in image */

/*
 * outimage - print image of d on file f.  If restrict is non-zero,
 *  fields of records will not be imaged.
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
      /*
       * *d is a string qualifier.  Print STRINGLIMIT characters of it
       *  using printimage and denote the presence of additional characters
       *  by terminating the string with "...".
       */
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
      /*
       * *d is a variable.  Print "variable =", dereference it and loop
       *  back to the top to cause the value of the variable to be imaged.
       */
      fprintf(f, "variable = ");
      d = VARLOC(*d);
      goto outimg;
      }

   switch (TYPE(*d)) {

      case T_INTEGER:
         fprintf(f, "%d", INTVAL(*d));
         return;

#ifdef LONGS
      case T_LONGINT:
         fprintf(f, "%ld", BLKLOC(*d)->longint.intval);
         return;
#endif LONGS
      case T_REAL:
         {
         char s[30];
         struct descrip junk;
         rtos(BLKLOC(*d)->realblk.realval, &junk, s);
         fprintf(f, "%s", s);
         return;
         }

      case T_CSET:
         /*
          * Check for distinguished csets by looking at the address of
          *  of the object to image.  If one is found, print its name.
          */
         if (BLKLOC(*d) == (union block *) &k_ascii) {
            fprintf(f, "&ascii");
            return;
            }
         else if (BLKLOC(*d) == (union block *) &k_cset) {
            fprintf(f, "&cset");
            return;
            }
         else if (BLKLOC(*d) == (union block *) &k_lcase) {
            fprintf(f, "&lcase");
            return;
            }
         else if (BLKLOC(*d) == (union block *) &k_ucase) {
            fprintf(f, "&ucase");
            return;
            }
         /*
          * Use printimage to print each character in the cset.  Follow
          *  with "..." if the cset contains more than STRINGLIMIT
          *  characters.
          */
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
         /*
          * Check for distinguished files by looking at the address of
          *  of the object to image.  If one is found, print its name.
          */
         if ((fd = BLKLOC(*d)->file.fd) == stdin)
            fprintf(f, "&input");
         else if (fd == stdout)
            fprintf(f, "&output");
         else if (fd == stderr)
            fprintf(f, "&output");
         else {
            /*
             * The file isn't a special one, just print "file(name)".
             */
            i = STRLEN(BLKLOC(*d)->file.fname);
            s = STRLOC(BLKLOC(*d)->file.fname);
            fprintf(f, "file(");
            while (i-- > 0)
               printimage(f, *s++, '\0');
            putc(')', f);
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
         /*
          * listimage does the work for lists.
          */
         listimage(f, BLKLOC(*d), restrict);
         return;

      case T_TABLE:
         /*
          * Print "table(n)" where n is the size of the table.
          */
         fprintf(f, "table(%d)", BLKLOC(*d)->table.cursize);
         return;
#ifdef SETS
      case T_SET:
        /*
         * print "set(n)" where n is the cardinality of the set
         */
        fprintf(f,"set(%d)",BLKLOC(*d)->set.setsize);
        return;
#endif SETS

      case T_RECORD:
         /*
          * If restrict is non-zero, print "record(n)" where n is the
          *  number of fields in the record.  If restrict is zero, print
          *  the image of each field instead of the number of fields.
          */
         bp = BLKLOC(*d);
         i = STRLEN(bp->record.recptr->recname);
         s = STRLOC(bp->record.recptr->recname);
         fprintf(f, "record ");
         while (i-- > 0)
            printimage(f, *s++, '\0');
         j = bp->record.recptr->nfields;
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
         /*
          * Produce "v[i+:j] = value" where v is the image of the variable
          *  containing the substring, i is starting position of the substring
          *  j is the length, and value is the string v[i+:j].  If the length
          *  (j) is one, just produce "v[i] = value".
          */
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
         /*
          * It is possible that descriptor d which thinks it is pointing
          *  at a TVTBL may actually be pointing at a TELEM which had
          *  been converted from a trapped variable. Check for this first
          *  and if it is a TELEM produce the outimage of its value.
          */
         if (bp->tvtbl.type == T_TELEM) {
            outimage(f,&bp->tvtbl.tvtval,restrict);
            return;
            }
         /*
          * It really was a TVTBL - Produce "t[s]" where t is the image of
          *  the table containing the element and s is the image of the
          *  subscript.
          */
         else {
            outimage(f, &bp->tvtbl.tvtable, restrict);
            putc('[', f);
            outimage(f, &bp->tvtbl.tvtref, restrict);
            putc(']', f);
            return;
            }

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
 * printimage - print character c on file f using escape conventions
 *  if c is unprintable, '\', or equal to q.
 */

static printimage(f, c, q)
FILE *f;
int c, q;
   {
   if (c >= ' ' && c < '\177') {
      /*
       * c is printable, but special case ", ', and \.
       */
      switch (c) {
         case '"':
            if (c != q) goto def;
            fprintf(f, "\\\"");
            return;
         case '\'':
            if (c != q) goto def;
            fprintf(f, "\\'");
            return;
         case '\\':
            fprintf(f, "\\\\");
            return;
         default:
         def:
            putc(c, f);
            return;
         }
      }

   /*
    * c is some sort of unprintable character.  If it one of the common
    *  ones, produce a special representation for it, otherwise, produce
    *  its octal value.
    */
   switch (c) {
      case '\b':                        /* backspace */
         fprintf(f, "\\b");
         return;
      case '\177':                        /* delete */
         fprintf(f, "\\d");
         return;
      case '\33':                        /* escape */
         fprintf(f, "\\e");
         return;
      case '\f':                        /* form feed */
         fprintf(f, "\\f");
         return;
      case '\n':                        /* new line */
         fprintf(f, "\\n");
         return;
      case '\r':                        /* return */
         fprintf(f, "\\r");
         return;
      case '\t':                        /* horizontal tab */
         fprintf(f, "\\t");
         return;
      case '\13':                        /* vertical tab */
         fprintf(f, "\\v");
         return;
      default:                                /* octal constant */
         fprintf(f, "\\%03o", c&0377);
         return;
      }
   }

/*
 * listimage - print an image of a list.
 */

static listimage(f, lp, restrict)
FILE *f;
struct b_list *lp;
int restrict;
   {
   register int i, j;
   register struct b_lelem *bp;
   int size, count;

   bp = (struct b_lelem *) BLKLOC(lp->listhead);
   size = lp->cursize;

   if (restrict > 0 && size > 0) {
      /*
       * Just give indication of size if the list isn't empty.
       */
      fprintf(f, "list(%d)", size);
      return;
      }

   /*
    * Print [e1,...,en] on f.  If more than LISTLIMIT elements are in the
    *  list, produce the first LISTLIMIT/2 elements, an ellipsis, and the
    *  last LISTLIMIT elements.
    */
   putc('[', f);
   count = 1;
   i = 0;
   if (size > 0) {
      for (;;) {
         if (++i > bp->nused) {
            i = 1;
            bp = (struct b_lelem *) BLKLOC(bp->listnext);
            }
         if (count <= LISTLIMIT/2 || count > size - LISTLIMIT/2) {
            j = bp->first + i - 1;
            if (j >= bp->nelem)
               j -= bp->nelem;
            outimage(f, &bp->lslots[j], restrict+1);
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
