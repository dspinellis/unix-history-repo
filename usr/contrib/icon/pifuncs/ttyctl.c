/*
#	TTYCTL(3.icon)
#
#	Control of terminal attributes
#
#	Steven B. Wampler
#
#	Last modified 8/19/84
#
*/

#include "../h/rt.h"
#ifdef VAX
#include <sgtty.h>
#define TIOCQSIZ        (('t'<<8)|65)

int oldttyflags = -1234;

Xstty(nargs)                                            /* set tty device flags */
int nargs;
   {
   register int oldflags, i, n;
   long l;
   struct sgttyb oldmodes;
   struct descrip arg;

   ioctl(2,TIOCGETP,&oldmodes);                         /* old modes */
   oldflags = oldmodes.sg_flags;
   if (oldttyflags == -1234)                            /* save if 1st call */
      oldttyflags = oldflags;

   for (n = 1; n <= nargs; n++) {
      arg = ARG(n);
      if (cvint(&arg, &l) == NULL)
         runerr(101, &arg);
      i = (int)l;
      if (l < 0)                                        /* turn off flag */
         oldmodes.sg_flags &= ~-(int)l;
      else                                              /* turn on flag */
         oldmodes.sg_flags |= (int)l;
      }

   ioctl(2,TIOCSETP,&oldmodes);                         /* new modes */
   mkint(oldflags, &ARG(0));                            /* return old modes */
   }

Procblock(stty,-1)

Xrestty(nargs)                                          /* reset tty to original state */
int nargs;
   {                    /* if original state unknown, makes a guess */
   register int oldflags;
   struct sgttyb oldmodes;

   ioctl(2,TIOCGETP,&oldmodes);
   oldflags = oldmodes.sg_flags;
   oldmodes.sg_flags = (oldttyflags == -1234) ? 0330 : oldttyflags;
   ioctl(2,TIOCSETP,&oldmodes);
   mkint(oldflags, &ARG(0));                             /* return old modes */
   }

Procblock(restty,0)

Xkeyin(nargs)
int nargs;
  {
  long count;
  count = 0;
  ioctl(0, FIONREAD, &count);

  if (count == 0L)
     fail();

  ARG(0) = nulldesc;
  }

Procblock(keyin,0)
#endif VAX
