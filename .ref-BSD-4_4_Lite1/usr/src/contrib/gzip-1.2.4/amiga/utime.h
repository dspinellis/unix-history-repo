#ifndef _UTIME_H
#define _UTIME_H 1

#ifndef _TIME_H
#include <time.h>
#endif

struct utimbuf {
  time_t actime;
  time_t modtime;
};

extern int utime (char *path, struct utimbuf *times);

#endif
