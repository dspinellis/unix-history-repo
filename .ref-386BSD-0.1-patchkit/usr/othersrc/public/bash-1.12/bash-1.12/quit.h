/* quit.h -- How to handle SIGINT gracefully. */

#ifndef __QUIT__
#define __QUIT__
/* Non-zero means SIGINT has already ocurred. */
extern int interrupt_state;
extern void throw_to_top_level ();

/* Macro to call a great deal.  SIGINT just sets above variable.  When
   it is safe, put QUIT in the code, and the "interrupt" will take place. */
#define QUIT if (interrupt_state) throw_to_top_level ()

#endif /* __QUIT__ */

