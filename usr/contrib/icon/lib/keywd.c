#include "../h/rt.h"
#include "../h/keyword.h"
#ifdef SYSTIME
#include <sys/time.h>
#else SYSTIME
#include <time.h>
#endif SYSTIME
#include <sys/types.h>
#include <sys/times.h>

static char *day[] = {
   "Sunday", "Monday", "Tuesday", "Wednesday",
   "Thursday", "Friday", "Saturday"
   };

static char *month[] = {
   "January", "February", "March", "April", "May", "June",
   "July", "August", "September", "October", "November", "December"
   };

/*
 * keywd - process keyword.
 */

keywd(nargs, arg1)
int nargs;
struct descrip arg1;
   {
   register int hour, i;
   register char *merid;
   char sbuf[MAXSTRING];
   struct tm *tbuf, *localtime();
   struct tms tp;
   long time(), clock, runtim;
   char *alcstr();

   SetBound;

   /*
    * This is just plug and chug code.  For whatever keyword is desired,
    *  the appropriate value is dug out of the system and made into
    *  a suitable Icon value.
    *
    * A few special cases are worth noting:
    *  &fail - calls fail();
    *  &pos, &random, &trace - trapped variables are made for possible
    *   subsequent assignments.
    */
   switch (INTVAL(arg1)) {
      case K_ASCII:
         arg1.type = D_CSET;
         BLKLOC(arg1) = (union block *) &k_ascii;
         break;
      case K_CLOCK:
         sneed(8);
         time(&clock);
         tbuf = localtime(&clock);
         sprintf(sbuf,"%02d:%02d:%02d",tbuf->tm_hour,tbuf->tm_min,tbuf->tm_sec);
         STRLEN(arg1) = 8;
         STRLOC(arg1) = alcstr(sbuf,8);
         break;
      case K_CSET:
         arg1.type = D_CSET;
         BLKLOC(arg1) = (union block *) &k_cset;
         break;
      case K_DATE:
         sneed(10);
         time(&clock);
         tbuf = localtime(&clock);
         sprintf(sbuf, "%04d/%02d/%02d",
                      (tbuf->tm_year)+1900,tbuf->tm_mon+1,tbuf->tm_mday);
         STRLEN(arg1) = 10;
         STRLOC(arg1) = alcstr(sbuf,10);
         break;
      case K_DATELINE:
         time(&clock);
         tbuf = localtime(&clock);
         if ((hour = tbuf->tm_hour) >= 12) {
            merid = "pm";
            if (hour > 12)
               hour -= 12;
            }
         else {
            merid = "am";
            if (hour < 1)
               hour += 12;
            }
         sprintf(sbuf, "%s, %s %d, %d  %d:%02d %s",
                 day[tbuf->tm_wday], month[tbuf->tm_mon], tbuf->tm_mday,
                 1900 + tbuf->tm_year, hour, tbuf->tm_min, merid);
         sneed(i = strlen(sbuf));
         STRLEN(arg1) = i;
         STRLOC(arg1) = alcstr(sbuf, i);
         break;
      case K_ERROUT:
         arg1.type = D_FILE;
         BLKLOC(arg1) = (union block *) &k_errout;
         break;
      case K_FAIL:
         fail();
         break;
      case K_HOST:
         iconhost(sbuf);
         sneed(i = strlen(sbuf));
         STRLEN(arg1) = i;
         STRLOC(arg1) = alcstr(sbuf, i);
         break;
      case K_INPUT:
         arg1.type = D_FILE;
         BLKLOC(arg1) = (union block *) &k_input;
         break;
      case K_LCASE:
         arg1.type = D_CSET;
         BLKLOC(arg1) = (union block *) &k_lcase;
         break;
      case K_LEVEL:
         arg1.type = D_INTEGER;
         INTVAL(arg1) = k_level;
         break;
      case K_MAIN:
         arg1 = k_main;
         break;
      case K_NULL:
         arg1 = nulldesc;
         break;
      case K_OUTPUT:
         arg1.type = D_FILE;
         BLKLOC(arg1) = (union block *) &k_output;
         break;
      case K_POS:
         arg1.type = D_TVPOS;
         INTVAL(arg1) = k_pos;
         break;
      case K_RANDOM:
         arg1.type = D_TVRAND;
         BLKLOC(arg1) = (union block *) &k_random;
         break;
      case K_SOURCE:
         arg1 = BLKLOC(current)->estack.activator;
         break;
      case K_SUBJECT:
         arg1.type = D_VAR;
         BLKLOC(arg1) = (union block *) &k_subject;
         break;
      case K_TIME:
         times(&tp);
         runtim =
           1000 * ((tp.tms_utime - starttime) / (double)HZ);
         mkint(runtim, &arg1);
         break;
      case K_TRACE:
         arg1.type = D_TVTRACE;
         INTVAL(arg1) = k_trace;
         break;
      case K_UCASE:
         arg1.type = D_CSET;
         BLKLOC(arg1) = (union block *) &k_ucase;
         break;
      case K_VERSION:
         sneed(i = strlen(VERSION));
         STRLEN(arg1) = i;
         STRLOC(arg1) = VERSION;
         break;
      case K_OPTIONS:
         sneed(i = strlen(OPTIONS));
         STRLEN(arg1) = i;
         STRLOC(arg1) = OPTIONS;
         break;
      default:
         syserr("keyword: unknown keyword type.");
      }
   ClearBound;
   }
