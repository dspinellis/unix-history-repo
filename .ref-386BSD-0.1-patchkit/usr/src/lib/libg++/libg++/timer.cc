/* 
Copyright (C) 1990 Free Software Foundation
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

#ifdef __GNUG__
#pragma implementation
#endif
#include <builtin.h>
#include <sys/resource.h>

// Timing functions from Doug Schmidt...

/* no such thing as "negative time"! */
#define  TIMER_ERROR_VALUE -1.0   

// If this does not work on your system, change this to #if 0, and 
// report the problem

#if 1

#if defined(USG)
extern "C" {
#include <sys/types.h>
#include <sys/param.h>
#include <sys/times.h>
}
#else
#include <osfcn.h>
#endif

#if defined(USG)
static struct tms Old_Time;
static struct tms New_Time;
#else
static struct rusage Old_Time;
static struct rusage New_Time;
#endif
static int    Timer_Set = 0;

double start_timer()
{
   Timer_Set = 1;
#if defined(USG)
   times(&Old_Time);
   return((double) Old_Time.tms_utime / HZ);
#else
   getrusage(RUSAGE_SELF,&Old_Time);        /* set starting process time */
   return(Old_Time.ru_utime.tv_sec + (Old_Time.ru_utime.tv_usec / 1000000.0));
#endif
}

/* Returns process time since Last_Time.
   If parameter is 0.0, returns time since the Old_Time was set.
   Returns TIMER_ERROR_VALUE if `start_timer' is not called first.  */

double return_elapsed_time(double Last_Time)
{
   if (!Timer_Set) {
      return(TIMER_ERROR_VALUE);
   }   
   else {
    /* get process time */
#if defined(USG)
      times(&New_Time);
#else
      getrusage(RUSAGE_SELF,&New_Time);
#endif
      if (Last_Time == 0.0) {
#if defined(USG)
	 return((double) (New_Time.tms_utime - Old_Time.tms_utime) / HZ);
#else
         return((New_Time.ru_utime.tv_sec - Old_Time.ru_utime.tv_sec) + 
               ((New_Time.ru_utime.tv_usec - Old_Time.ru_utime.tv_usec) 
                / 1000000.0));
#endif
      }
      else {
#if defined(USG)
	 return((double) New_Time.tms_utime / HZ - Last_Time);
#else
         return((New_Time.ru_utime.tv_sec + 
                (New_Time.ru_utime.tv_usec / 1000000.0)) - Last_Time);
#endif
      }
   }
}

#ifdef VMS
void sys$gettim(unsigned int*) asm("sys$gettim");

getrusage(int dummy,struct rusage* time){
	double rtime;
	unsigned int systime[2];
	int i;
	sys$gettim(&systime[0]);
	rtime=systime[1];
	for(i=0;i<4;i++) rtime *= 256;
	rtime+= systime[0];
/* we subtract an offset to make sure that the number fits in a long int*/
	rtime=rtime/1.0e+7-4.144e+9;
	time->ru_utime.tv_sec= rtime;
	rtime=(rtime-time->ru_utime.tv_sec)*1.0e6;	
	time->ru_utime.tv_usec= rtime;
}
#endif
#else /* dummy them out */

double start_timer()
{
  return TIMER_ERROR_VALUE;
}

double return_elapsed_time(double)
{
  return TIMER_ERROR_VALUE;
}

#endif /* timing stuff */


