// This may look like C code, but it is really -*- C++ -*-
/* 
Copyright (C) 1989 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  
*/

#ifndef _signal_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif



extern "C" {

// This #define KERNEL hack gets around bad function prototypes on most
// systems. If not, you need to do some real work...

#ifndef masscomp
#define KERNEL
#endif

#include <//usr/include/sys/signal.h>

#ifndef masscomp
#undef KERNEL
#endif

#ifndef _signal_h
#define _signal_h 1
#endif

// The Interviews folks call this SignalHandler. Might as well conform.
// Beware: some systems think that SignalHandler returns int.
typedef void (*SignalHandler) (...);

extern SignalHandler signal(int sig, SignalHandler action);
extern SignalHandler sigset(int sig, SignalHandler action);
extern SignalHandler ssignal(int sig, SignalHandler action);
extern int           gsignal (int sig);
extern int           kill (int pid, int sig);

#ifndef hpux // Interviews folks claim that hpux doesn't like these
extern int           sigsetmask(int mask);
extern int           sigblock(int mask);
extern int           sigpause(int mask);
extern int           sigvec(int sig, struct sigvec* v, struct sigvec* prev);
#endif

// The Interviews version also has these ...

#define SignalBad ((SignalHandler)-1)
#define SignalDefault ((SignalHandler)0)
#define SignalIgnore ((SignalHandler)1)


}

#endif

