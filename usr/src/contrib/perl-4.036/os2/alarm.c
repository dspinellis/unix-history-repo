/*
 * This software is Copyright 1989 by Jack Hudler.
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction or this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made.
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk.
 *
 */

/****************************** Module Header ******************************\
* Module Name: alarm.c
* Created    : 11-08-89
* Author     : Jack Hudler  [jack@csccat.lonestar.org]
* Copyright  : 1988 Jack Hudler.
* Function   : Unix like alarm signal simulator.
\***************************************************************************/

/* Tested using OS2 1.2 with Microsoft C 5.1 and 6.0. */

#define INCL_DOSPROCESS
#define INCL_DOSSIGNALS
#define INCL_DOS
#include <os2.h>

#include <stdlib.h>
#include <stdio.h>
#include <signal.h>

#include "alarm.h"

#define ALARM_STACK 4096    /* This maybe over kill, but the page size is 4K */

static  PBYTE     pbAlarmStack;
static  SEL       selAlarmStack;
static  TID       tidAlarm;
static  PID       pidMain;
static  BOOL      bAlarmInit=FALSE;
static  BOOL      bAlarmRunning=FALSE;
static  USHORT    uTime;

static VOID FAR alarm_thread ( VOID )
{
    while(1)
    {
      if (bAlarmRunning)
      {
        DosSleep(1000L);
        uTime--;
        if (uTime==0L)
        {
          // send signal to the main process.. I could have put raise() here
          // however that would require the use of the multithreaded library,
          // and it does not contain raise()!
          // I tried it with the standard library, this signaled ok, but a
          // test printf in the signal would not work and even caused SEGV.
          // So I signal the process through OS/2 and then the process
          // signals itself.
          if (bAlarmRunning)
            DosFlagProcess(pidMain,FLGP_PID, PFLG_A,1);
          bAlarmRunning=FALSE;
        }
      }
      else
        DosSleep(500L);
    }
}

static VOID PASCAL FAR AlarmSignal(USHORT usSigArg,USHORT usSigNum)
{
    /*
     * this is not executed from the thread. The thread triggers Process
     * flag A which is in the main processes scope, this inturn triggers
     * (via the raise) SIGUSR1 which is defined to SIGALRM.
     */
    raise(SIGUSR1);
}

static void alarm_init(void)
{
    PFNSIGHANDLER pfnPrev;
    USHORT       pfAction;
    PIDINFO      pid;

    bAlarmInit = TRUE;

    if (!DosAllocSeg( ALARM_STACK, (PSEL) &selAlarmStack, SEG_NONSHARED ))
    {
      OFFSETOF(pbAlarmStack) = ALARM_STACK - 2;
      SELECTOROF(pbAlarmStack) = selAlarmStack;
      /* Create the thread */
      if (DosCreateThread( alarm_thread, &tidAlarm, pbAlarmStack ))
      {
        fprintf(stderr,"Alarm thread failed to start.\n");
        exit(1);
      }
      /* Setup the signal handler for Process Flag A */
      if (DosSetSigHandler(AlarmSignal,&pfnPrev,&pfAction,SIGA_ACCEPT,SIG_PFLG_A))
      {
        fprintf(stderr,"SigHandler Failed to install.\n");
        exit(1);
      }
      /* Save main process ID, we'll need it for triggering the signal */
      DosGetPID(&pid);
      pidMain = pid.pid;
    }
    else
      exit(1);
}

unsigned alarm(unsigned sec)
{
    if (!bAlarmInit) alarm_init();

    if (sec)
    {
      uTime = sec;
      bAlarmRunning = TRUE;
    }
    else
      bAlarmRunning = FALSE;

    return 0;
}

#ifdef TESTING
/* A simple test to see if it works */
BOOL  x;

void timeout(void)
{
    fprintf(stderr,"ALARM TRIGGERED!!\n");
    DosBeep(1000,500);
    x++;
}

void main(void)
{
    (void) signal(SIGALRM, timeout);
    (void) alarm(1L);
    printf("ALARM RUNNING!!\n");
    while(!x);
}
#endif
