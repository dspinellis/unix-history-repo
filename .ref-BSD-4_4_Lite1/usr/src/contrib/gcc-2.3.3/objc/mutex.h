/* Copyright (C) 1992 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#ifndef _MUTEX_H
#define _MUTEX_H

#ifdef WANT_MUTEX
#if defined (NeXT)
#include  <cthreads.h>
#define MUTEX               mutex_t
#define MUTEX_ALLOC(mutex)  { *mutex = mutex_alloc (); }
#define MUTEX_INIT(mutex)   mutex_init (mutex)
#define MUTEX_FREE(mutex)   mutex_free (mutex)
#define MUTEX_LOCK(mutex)   mutex_lock (mutex)
#define MUTEX_UNLOCK(mutex) mutex_unlock (mutex)
#define MUTEX_ISLOCK(mutex)     mutex->lock /* Gak */

#elif defined (OSF)

#elif defined (mach)

#elif defined (sun)
/*
 * Sun lwp uses monitors.
 *
 * Untested.  8-Dec-91, dennisg.
 */
#include  <lwp/lwp.h>
#include  <assert.h>

#define MUTEX (mon_t)

inline MUTEX
MUTEX_ALLOC (mutex)
{
  MUTEX mon;
  
  mon_create (&mon);
  return mon;
}

#define MUTEX_INIT(mutex)
#define MUTEX_FREE(mutex)   mon_destroy (mutex)

inline void
MUTEX_LOCK (mutex)
{
  int level = mon_enter (mutex);
  assert (level);
}

#define MUTEX_UNLOCK(mutex) mon_exit (mutex)

inline int
MUTEX_ISLOCK (mutex)
{
  thread_t thread;

  /* Won't work? */
  return mon_waiters (mutex, &thread, NULL, 0);
}

#elif defined (COROUTINES)
/*
 * A environment where threads are implemented as a
 *  set of coroutines.
 */

#endif
#endif

#ifndef MUTEX
/*
 * These are default mutex handler routines.
 *  There is no mutex support.
 *
 * Let the optimizer get rid of mutexes.
 */

#define MUTEX void *
#define MUTEX_ALLOC(mutex)  (MUTEX)-1
#define MUTEX_INIT(mutex)   (mutex)
#define MUTEX_FREE(mutex)   (mutex)
#define MUTEX_LOCK(mutex)   (mutex)
#define MUTEX_UNLOCK(mutex) (mutex)
#define MUTEX_ISLOCK(mutex) 1
#endif

#endif /* not _MUTEX_H */
