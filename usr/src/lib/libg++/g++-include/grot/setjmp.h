#ifndef _setjmp_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif

extern "C" {

#define  setjmp  C_header_setjmp
#define  longjmp C_header_longjmp

#ifdef VMS
#include "gnu_cc_include:[000000]setjmp.h"
#else
#include "/usr/include/setjmp.h"
#endif

#undef setjmp
#undef longjmp

#ifndef _setjmp_h
#define _setjmp_h 1
#endif

extern int setjmp(jmp_buf);
extern void longjmp(jmp_buf, int);

}

#endif
