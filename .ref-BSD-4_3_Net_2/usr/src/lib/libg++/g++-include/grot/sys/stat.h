
#ifndef stat_h
#pragma once

extern "C"
{
#ifdef VMS
#include "GNU_CC_INCLUDE:[sys]stat.h"
#else
#include "//usr/include/sys/stat.h"
#endif

#ifndef stat_h
#define stat_h 1
int stat (char *path, struct stat *buf);
int lstat (char *path, struct stat *buf);
int fstat (int fd, struct stat *buf);

#endif

}

#endif
