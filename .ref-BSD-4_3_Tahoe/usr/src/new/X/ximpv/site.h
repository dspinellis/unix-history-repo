/* this file contains the site dependent items for the ipq, iprm, ipch
 * and impv routines.
 *
 * history 
 *
 * created by Ted Bentley
 * University of Alberta
 * February 1984
 * Color revisions: Martin Dubetz January 1985
 */

/* $Header: site.h,v 10.3 86/02/01 16:00:33 tony Rel $ */

#define REMOTE_HOST "alberta"	/* The host where the spool files are */
#define P_SERV	"impvserv"	/* The name of the server on that host */
#define SCREEN_FILE  "/usr/tmp/impvXXXXXX"	/* the disk file for screen images */
#define SAVE_PAGES	5	/* the number of screen images to save */
#define NOSPOOL 		/* no spool file, local previewing only*/
/*  #define XWIND		/* DO NOT TURN ON, MAKEFILE DOES IT */
				/*for VAXSTATION */
/*  #define IMAGEN300		/* for IMAGEN 300 dpi*/
#ifndef XWIND
#define COLOR			/* include the color slide option */
#endif

#ifdef IMAGEN300 

#define SIZED
#define MAXx 2550		/* maximum number of dots imagen 300 page */
#define MAXy 3300

#ifdef SUN120
#define LEFTE 126		/* left edge of pscreen to start at */

#else SUN120
#define LEFTE 62		/* left edge of pscreen to start at */

#endif SUN120

#endif IMAGEN300

#ifndef SIZED

#ifdef IMAGEN480
#define MAXx 4080		/* maximum number of dots imagen 480 page */
#define MAXy 5280
#define LEFTE 0			/* left edge of pscreen to start at */

#else IMAGEN480
#define MAXx 2040		/* maximum number of dots imagen 240 page */
#define MAXy 2640
#define LEFTE 0			/* left edge of pscreen to start at */

#endif IMAGEN480

#endif SIZED

/* these likely won't change */
#define SMAXx (MAXx / 2)	/* number of dots on a screen page */
#define SMAXy (MAXy / 2)
		/* calculate the screen size in bytes 2::1 compression */
#define SCREENSIZE (((SMAXx + 7) / 8) * SMAXy ) 
