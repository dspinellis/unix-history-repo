
#ifndef grp_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif

#include <stdio.h>

extern "C" {
#define getgrent c_proto_getgrent
#define getgrgid c_proto_getgrgid
#define getgrnam c_proto_getgrnam
#define setgrent c_proto_setgrent
#define endgrent c_proto_endgrent
#define fgetgrent c_proto_fgetgrent

#define KERNEL

#include "//usr/include/grp.h"

#ifndef grp_h
#define grp_h 1
#endif

#undef getgrent
#undef getgrgid
#undef getgrnam
#undef KERNEL

extern struct group* getgrent();
extern struct group* fgetgrent(FILE*);
extern struct group* getgrgid(int);
extern struct group* getgrnam(const char*);
extern void          setgrent();
extern void          endgrent();

}

#endif
