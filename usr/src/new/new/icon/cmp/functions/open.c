#include "../h/rt.h"

/*
 * open(s1,s2) - open file s1 with specification s2.
 */
int globals;
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
   extern FILE *fopen(), *popen();

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
   defstr(&arg2, sbuf2, &letr);

   hneed(sizeof(struct b_file));
   status = 0;

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

   if (status & FS_PIPE) {
      if (status != (FS_READ|FS_PIPE) && status != (FS_WRITE|FS_PIPE))
         runerr(209, &arg2);
      f = popen(sbuf1, mode);
      }
   else
      f = fopen(sbuf1, mode);
   if (f == NULL)
      fail();
   if (!isatty(fileno(f))) {
      for (i = 0; i < numbufs; i++)
         if (bufused[i] == NULL)
            break;
      if (i < numbufs) {              /* use buffer if any free */
         setbuf(f, bufs[i]);
         bufused[i] = f;
         }
      else
         setbuf(f, NULL);
      }
   else
      setbuf(f, NULL);
   arg0.type = D_FILE;
   BLKLOC(arg0) = alcfile(f, status, &arg1);
   }

struct b_iproc Bopen = {
   T_PROC,
   sizeof(struct b_proc),
   EntryPoint(Xopen),
   2,
   -1,
   0,
   0,
   {4, "open"}
   };
