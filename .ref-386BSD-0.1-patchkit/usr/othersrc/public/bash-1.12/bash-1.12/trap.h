/* trap.h -- data structures used in the trap mechanism. */

#ifndef NSIG
#include <signal.h>
#endif

#ifndef NSIG
#define NSIG 64
#endif

#define NO_SIG -1
#define DEFAULT_SIG SIG_DFL
#define IGNORE_SIG  SIG_IGN

#define signal_object_p(x) (decode_signal (x) != NO_SIG)

extern char *trap_list[NSIG];
extern char *signal_name ();
extern int signal_decode ();

