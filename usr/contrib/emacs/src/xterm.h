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
