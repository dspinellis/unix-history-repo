#include <X/Xlib.h>

#define XREPBUFSIZE 64

typedef struct 
  {
    int rindex;
    int windex;
    int mindex;
    XEvent xrep[XREPBUFSIZE];
  }
XREPBUFFER;

extern int x_edges_specified;

#ifndef sigmask
#define sigmask(no) (1L << ((no) - 1))
#endif

#ifdef SIGIO

#define BLOCK_INPUT_DECLARE() int BLOCK_INPUT_mask;
#define BLOCK_INPUT() BLOCK_INPUT_mask = sigblock (sigmask (SIGIO))
#define UNBLOCK_INPUT() sigsetmask (BLOCK_INPUT_mask)
#define SIGNAL_INPUT() kill (XXpid, SIGIO)

#define RESIGNAL_INPUT()						\
do									\
{									\
  if (QLength () > 0)							\
    SIGNAL_INPUT ();							\
} while (0)

#define SIGNAL_INPUT_WHILE(flag)					\
do									\
{									\
  while (flag)								\
    SIGNAL_INPUT ();							\
} while (0)

#define UNBLOCK_INPUT_RESIGNAL()					\
do									\
{									\
  UNBLOCK_INPUT ();							\
  RESIGNAL_INPUT ();							\
} while (0)

#else /* SIGIO undefined */

#define BLOCK_INPUT_DECLARE()
#define BLOCK_INPUT()
#define UNBLOCK_INPUT()
#define SIGNAL_INPUT() /* input_available_signal (0) */
#define RESIGNAL_INPUT()
#define SIGNAL_INPUT_WHILE(bitblt) /* input_available_signal (0) */
#define UNBLOCK_INPUT_RESIGNAL()

#endif /* SIGIO */
