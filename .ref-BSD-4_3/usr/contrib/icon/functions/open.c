#include "../h/rt.h"

/*
 * open(s1,s2) - open file s1 with specification s2.
 */
Xopen(nargs, arg2, arg1, arg0)
int nargs;
struct descrip arg2, arg1, arg0;
   {
   register int slen, i;
   register char *s;
   int status;
   char sbuf1[MAXSTRING], sbuf2[MAXSTRING], mode[3];
   FILE *f;
   extern struct b_file *alcfile();
   extern char *alcstr();
   extern FILE *fopen(), *popen();

   /*
    * s1 must be a string and a C string copy of it is also needed.
    *  Make it a string if it isn't one; make a C string if s1 is
    *  a string.
    */
   switch (cvstr(&arg1, sbuf1)) {
      case 1:
         sneed(STRLEN(arg1));
         STRLOC(arg1) = alcstr(STRLOC(arg1), STRLEN(arg1));
         break;
      case 2:
         qtos(&arg1, sbuf1);
         break;
      default:
         runerr(103, &arg1);
      }
   /*
    * s2 defaults to "r".
    */
   defstr(&arg2, sbuf2, &letr);

   hneed(sizeof(struct b_file));
   status = 0;

   /*
    * Scan s2, setting appropriate bits in status.  Produce a runerr
    *  if an unknown character is encountered.
    */
   s = STRLOC(arg2);
   slen = STRLEN(arg2);
   for (i = 0; i < slen; i++) {
      switch (*s++) {
         case 'a': case 'A':
            status |= FS_WRITE|FS_APPEND;
            continue;
         case 'b': case 'B':
            status |= FS_READ|FS_WRITE;
            continue;
         case 'c': case 'C':
            status |= FS_CREATE|FS_WRITE;
            continue;
         case 'p': case 'P':
            status |= FS_PIPE;
            continue;
         case 'r': case 'R':
            status |= FS_READ;
            continue;
         case 'w': case 'W':
            status |= FS_WRITE;
            continue;
         default:
            runerr(209, &arg2);
         }
      }

   /*
    * Construct a mode field for fopen/popen.
    */
   mode[0] = '\0';
   mode[1] = '\0';
   mode[2] = '\0';
   if ((status & (FS_READ|FS_WRITE)) == 0)   /* default: read only */
      status |= FS_READ;
   if (status & FS_CREATE)
      mode[0] = 'w';
   else if (status & FS_APPEND)
      mode[0] = 'a';
   else if (status & FS_READ)
      mode[0] = 'r';
   else
      mode[0] = 'w';
   if ((status & (FS_READ|FS_WRITE)) == (FS_READ|FS_WRITE))
      mode[1] = '+';

   /*
    * Open the file with fopen or popen.
    */
   if (status & FS_PIPE) {
      if (status != (FS_READ|FS_PIPE) && status != (FS_WRITE|FS_PIPE))
         runerr(209, &arg2);
      f = popen(sbuf1, mode);
      }
   else
      f = fopen(sbuf1, mode);
   /*
    * Fail if the file can't be opened.
    */
   if (f == NULL)
      fail();
   /*
    * If the file isn't a terminal and a buffer is available, assign
    *  it to the file.
    */
   if (!isatty(fileno(f))) {
      for (i = 0; i < numbufs; i++)
         if (bufused[i] == NULL)
            break;
      if (i < numbufs) {              /* Use buffer if any free. */
         setbuf(f, bufs[i]);
         bufused[i] = f;
         }
      else
         setbuf(f, NULL);
      }
   else
      setbuf(f, NULL);
   /*
    * Return the resulting file value.
    */
   arg0.type = D_FILE;
   BLKLOC(arg0) = (union block *) alcfile(f, status, &arg1);
   }

Procblock(open,2)
