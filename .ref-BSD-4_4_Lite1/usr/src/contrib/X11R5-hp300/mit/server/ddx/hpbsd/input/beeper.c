/*

Copyright (c) 1986, 1987 by Hewlett-Packard Company
Copyright (c) 1986, 1987 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

HEWLETT-PACKARD MAKES NO WARRANTY OF ANY KIND WITH REGARD
TO THIS SOFWARE, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
PURPOSE.  Hewlett-Packard shall not be liable for errors 
contained herein or direct, indirect, special, incidental or 
consequential damages in connection with the furnishing, 
performance, or use of this material.

This software is not subject to any license of the American
Telephone and Telegraph Company or of the Regents of the
University of California.

*/
/*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         beeper.c
; SCCS:         %A% %G% %U%
; Description:  Access Gator/Bobcat beeper
; Author:       Andreas Paepcke, HPLabs/ATL
; Created:      2-Aug-85
; Modified:     Thu Oct 15 12:53:00 1987 (Don Bennett) bennett@hpldpb
; Language:     C
; Package:      PSL
; Status:       Experimental (Do Not Distribute)
;
; (c) Copyright 1985, Hewlett-Packard Company, all rights reserved.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
*/

/* Public functions:

       beep

   We offer three voices and a noise source. Each sound is controllable
   in pitch, volume and duration. Pitch goes from 0 to 1023, volume goes
   from 0 to 15, duration is between 0 and 255 10msec intervalls. A
   duration of 0 turns the voice on continuously. A volume of 0 turns
   it off.
   The manufacturing specs give details
   on the programming interface. Here is a summary:

   The beeper is accessed through ioctl calls. The request argument is
   either "Send data to beeper" or "Read voice values from beeper". The
   argument is a pointer to a 4 byte buffer. These four bytes
   are defined here.

   R0-R3: Register address field. In the order R2, R1, R0:
     
     0 0 0: Voice 1 frequency
     0 0 1: Voice 1 attenuation
     0 1 0: Voice 2 frequency
     0 1 1: Voice 2 attenuation
     1 0 0: Voice 3 frequency
     1 0 1: Voice 3 attenuation
     1 1 0: Noise control
     1 1 1: Noise attentuation

  F0-F9: 10 bits pitch
  A0-A3: Attenuation
  D0-D7: Duration in 10msec's

  The placement of data in the buffer is a bit srewy:

  Byte 0 (Frequency 1):  1 R2 R1 R0 F3 F2 F1 F0     LSB
  Byte 1(Frequency 2):  0  0 F9 F8 F7 F6 F5 F4
  Byte 2 (Attenuator) :  1 R2 R1 R0 A3 A2 A1 A0
  Byte 3 (Duration)   : D7 D6 D5 D4 D3 D2 D1 D0

  The volume is inversely proportional to the attenuation. In order
  to provide rising numbers for rising loudness to the user, we
  expect a volume and modify to get the attenuation. The same goes
  for the pitch. In order to calculate frequency of the pitch,
  use: 

           83333/(1023-pitch)

  It is possible at any time to request the time any voice has
  left to run. This is done by:
 
  F4: Read voice1 timer
  F5: Read voice2 timer
  F6: Read voice3 timer
  F7: Read voice4 timer (noise)

  Noise is generated using a shift register. The following controls
  are possible for noise:
 
  - Attenuation
  - Duration
  - Periodic or white noise
  - 3 shift rates or output of voice 4 as shift rate

  Bytes 0 and 1 of the data buffer must both have identical contents
  to control the noise. Attenuation and duration are as in the other
  voices. Bytes 0 and 1 should look like this:

  1 R2 R1 R0 0 FB NF1 NF0   LSB

  R2, R1 and R0 must be 1, 1 and 0. If FB is 0, periodic noise
  is generated. If FB is 1, white noise is produced.

  NF1 and NF2 control the shift rate of the noise generator:

  NF1     NF2     Shift Rate
  --------------------------
  0       0       M/64
  0       1       M/128
  1       0       M/256
  1       1       Uses tone generator 3 output


  M is related to the clock rate.

  The voice start routines return 0 if all is well, -1 if we had
  trouble accessing the device file for the beeper and -2 if given
  parameters were out of range:
*/

#define NEED_EVENTS
#include <fcntl.h>

#include <sys/types.h>
#include "X.h"
#include "Xproto.h"
#include "input.h"
#include "inputstr.h"
#include "hildef.h"

#if defined(__apollo)
#include <apollo/base.h>
#endif /* __apollo */

#if defined(__hpux) || defined(__hp_osf) || defined(hp9000)

#if defined(hp9000)
#include <sys/ioctl.h>
#include <hilioctl.h>
#else
#if defined(__hp_osf)
#include <hp/hilioctl.h>
#else
#include <sys/hilioctl.h>
#endif /* __hp_osf */
#endif

#endif /* __hpux || __hp_osf */

/*********************************************************************
*DEFINES:                                                            *
*********************************************************************/

#define ALL_OK           0
#define ACCESS_PROBLEM  -1
#define BAD_RANGE       -2

#define VOICE1_FREQ_REG 0x80         /* Top nibbles for byte0 for all voices: */
#define VOICE2_FREQ_REG 0xA0
#define VOICE3_FREQ_REG 0xC0
#define NOISE_FREQ_REG  0xE0

#define VOICE1_VOL_REG  0x90         /* Top nibbles for byte2 for all voices: */
#define VOICE2_VOL_REG  0xB0
#define VOICE3_VOL_REG  0xD0
#define NOISE_VOL_REG   0xF0

#define MIN_VOICE       1             /* Legal ranges for parms from user: */
#define MAX_VOICE       3
#define MIN_PITCH       0
#define MAX_PITCH       1023
#define MIN_DURATION    0
#define MAX_DURATION    255
#define MIN_VOLUME      0
#define MAX_VOLUME      15
#define MIN_TYPE        0
#define MAX_TYPE        1
#define MIN_RATE        0
#define MAX_RATE        3

extern int beeper_fd;

/*********************************************************************
* PUBLIC FUNCTIONS:                                                  *
*********************************************************************/

/*****************************************************************************
*                                                                            *
* Beep using specified voice:                                                *
*                                                                            *
*   TAKES:                                                                   *
*                                                                            *
*   VOICE    : from 1 to 3                                                   *
*   PITCH    : from 0 to 1023 (incl)                                         *
*   VOLUME   : from 0 to 15   (incl). Zero turns voice off.                  *
*   DURATION : from 0 to 255  (incl). Zero turns voice on continuously.      *
*                                                                            *
*   RETURNS:                                                                 *
*                                                                            *
*   0        : All ok                                                        *
*   -1       : Cannot access beeper device file                              *
*   -2       : Parameter out of range                                        *
*                                                                            *
******************************************************************************/


beep(voice,pitch,volume,duration)
    int voice,pitch,volume,duration;
    {
    unsigned char buffer[4];
    char vol ;

#if defined(__apollo)
#endif /* __apollo */

    /* Check whether beeper device has already been opened: */

#if defined(__hpux) || defined(__hp_osf) || defined(hp9000)
    if (beeper_fd < 0)
	return(ACCESS_PROBLEM);

#if defined(__hp9000s300) || defined(__hp9000s700) || defined(__hp_osf) || defined(hp300)

    /* Check ranges of parameters: */
    if (
       (voice < MIN_VOICE)       ||
       (voice > MAX_VOICE)       ||
       (pitch < MIN_PITCH)       ||
       (pitch > MAX_PITCH)       ||
       (volume < MIN_VOLUME)     ||
       (volume > MAX_VOLUME)     ||
       (duration < MIN_DURATION) ||
       (duration > MAX_DURATION)
       )
	return(BAD_RANGE);

  /* Init the voice dependent data bytes. Note the inversion of user's
     volume and pitch specs to attenuation:
  */

    volume = MAX_VOLUME - volume;
    pitch  = MAX_PITCH  - pitch;
    switch (voice)
	{
	case 1: buffer[0] = VOICE1_FREQ_REG | (pitch & 0x0000000f);
            buffer[2] = VOICE1_VOL_REG  | (volume & 0x0000000f);
            break;
	case 2: buffer[0] = VOICE2_FREQ_REG | (pitch & 0x0000000f);
            buffer[2] = VOICE2_VOL_REG  | (volume & 0x0000000f);
            break;
	case 3: buffer[0] = VOICE3_FREQ_REG | (pitch & 0x0000000f);
            buffer[2] = VOICE3_VOL_REG  | (volume & 0x0000000f);
            break;
	}

    /* The high 6 bits of the pitch go into byte 1: */

    buffer[1] = 0x0000003f & (pitch >> 4);
    buffer[3] = duration; 			/* Duration: */

    if (ioctl(beeper_fd,EFTSBP,buffer) < 0)
	return(ACCESS_PROBLEM);
    return(ALL_OK);


#else		/* building for s800 */

   /*
    * map input range of 0 - 15 to driver range range of 0 - 255
    */

    if ( !volume ) 
	return;

    if ( voice == 2 )  /*  2 == CLICK_VOICE  which means key click */
	{
	/* doesn't appear there's any way to set the volume or duration 
         on 800 so I'll just pinch the volume for key clicks.  t.houser
	*/
	vol = (char) volume * (KBD_MAXVOLUME/45);
	}
    else                         /* beeper */
	{
	vol = (char) volume * (KBD_MAXVOLUME/15);
	}
    ioctl(beeper_fd, KBD_BEEP, &vol);
#endif /* __hp9000s300 */
#endif /* __hpux || __hp_osf */
 }


void
SetBellAttributes(pDevice, pCtrl)
register HPInputDevice *pDevice;
register KeybdCtrl *pCtrl;
{

#if defined(__apollo)

    /* The duration passed in in the pCtrl struct is a 16-bit value.
       The units is milliseconds, but the tone_$time function has
       units of 4 microseconds.
    */

    int *duration = (int *) pDevice->bell1;

    *duration = pCtrl->bell_duration * 250;	/* save in tone_$time units */
#endif /* __apollo */

#if defined(__hp9000s300) || defined(__hp9000s700) || defined(__hp_osf) || defined(hp300)
    register unsigned char duration;
    register int bellPitch;

    duration = (unsigned char) (pCtrl->bell_duration / 10);
    bellPitch = pCtrl->bell_pitch == 0 ? 1023 : (83333 / pCtrl->bell_pitch);
    if (bellPitch < 0)
	bellPitch = 0;
    if (bellPitch > 1023)
	bellPitch = 1023;
    pDevice->bell1[0] = VOICE1_FREQ_REG | (bellPitch & 0x0f);
    pDevice->bell2[0] = VOICE2_FREQ_REG | (bellPitch & 0x0f);
    pDevice->bell3[0] = VOICE3_FREQ_REG | (bellPitch & 0x0f);

    pDevice->bell1[1] = 0x03f & (bellPitch >> 4);
    pDevice->bell2[1] = 0x03f & (bellPitch >> 4);
    pDevice->bell3[1] = 0x03f & (bellPitch >> 4);


    if(duration) 
	{
	pDevice->bell1[3] = pDevice->bell2[3] = pDevice->bell3[3] = duration;
	}
    else 
	{
	pDevice->bell1[3] = pDevice->bell2[3] = pDevice->bell3[3] = 1;
	}
#endif /* building on __hp9000s300  or for s700 */
}


void
hpBell(volume, pDevice)
register int volume;
register DevicePtr pDevice;
{

    register int attenuation, loud;
    register HPInputDevice *pHPDev = (HPInputDevice *)
	 (((DeviceIntPtr)pDevice)->public.devicePrivate);

#if defined(__apollo)
    if (volume)
	{
	int	*duration = (int *) pHPDev->bell1;
	time_$clock_t beepTime;

	beepTime.high16 = 0;
	beepTime.low32 = *duration;

	tone_$time (&beepTime);
	}
#endif /* __apollo */

#if defined(__hpux) || defined(__hp_osf) || defined(hp9000)
    if (beeper_fd < 0) 
	return;

    /*
     * device independant code has already range checked the volume
     */
    if(volume) 
	{

#if defined(__hp9000s300) || defined(__hp9000s700) || defined(__hp_osf) || defined(hp300)
         /*
          * map input range of 0 - 100 to hardware attenuation range of 15 - 0
	  * we use a temp variable (loud) because of C's penchant for
	  * strange orders of evaluation ie to force the multiply before
	  * the divide
          */
	 loud = volume * MAX_VOLUME;
	 attenuation = (char) MAX_VOLUME - loud / 100;

         /*
          * we use all three voices to get a reasonably loud volume
          */
         pHPDev->bell1[2] = VOICE1_VOL_REG | attenuation;
         pHPDev->bell2[2] = VOICE2_VOL_REG | attenuation;
         pHPDev->bell3[2] = VOICE3_VOL_REG | attenuation;

         ioctl(beeper_fd, EFTSBP, pHPDev->bell1);
         ioctl(beeper_fd, EFTSBP, pHPDev->bell2);
         ioctl(beeper_fd, EFTSBP, pHPDev->bell3);
#else /* building for s800 */
         /*
          * map input range of 0 - 100 to driver range range of 0 - 255
          */
	 char vol;
	 vol = volume * KBD_MAXVOLUME / 100;

         ioctl(beeper_fd, KBD_BEEP, &vol);
#endif /* building on __hp9000s300 or for s700 */
	}
#endif /* __hpux || __hp_osf */

    return;
}
