#include "../h/rt.h"
#include <stdio.h>

#ifdef UNAME
#include <sys/utsname.h>
#endif UNAME

/*
 * iconhost - return some sort of host name into the buffer pointed at
 *  by hostname.  This code accommodates several different host name
 *  fetching schemes.
 */
iconhost(hostname)
char *hostname;
   {
#ifdef WHOHOST
   /*
    * The host name is in /usr/include/whoami.h. (V7, 4.[01]bsd)
    */
   whohost(hostname);
#endif WHOHOST

#ifdef UNAME
   {
   /*
    * Use the uname system call.  (System III & V)
    */
   struct utsname uts;
   uname(&uts);
   strcpy(hostname,uts.nodename);
   }
#endif UNAME

#ifdef GETHOST
   /*
    * Use the gethostname system call.  (4.2bsd)
    */
   gethostname(hostname,MAXSTRING);
#endif GETHOST

#ifdef HOSTSTR
   /*
    * The string constant HOSTSTR contains the host name.
    */
   strcpy(hostname,HOSTSTR);
#endif HOSTSTR
   }

#ifdef WHOHOST
#define   HDRFILE "/usr/include/whoami.h"
/*
 * whohost - look for a line of the form
 *  #define sysname "name"
 * in HDRFILE and return the name.
 */
whohost(hostname)
char *hostname;
   {
   char buf[BUFSIZ];
   FILE *fd;

   fd = fopen(HDRFILE, "r");
   if (fd == NULL) {
      sprintf(buf, "Cannot open %s, no value for &host\n", HDRFILE);
      syserr(buf);
   }
   setbuf(fd,NULL);

   for (;;) {   /* each line in the file */
      if (fgets(buf, sizeof buf, fd) == NULL) {
         sprintf(buf, "No #define for sysname in %s, no value for &host\n", HDRFILE);
         syserr(buf);
      }
      if (sscanf(buf,"#define sysname \"%[^\"]\"", hostname) == 1) {
         fclose(fd);
         return;
      }
   }
   }
#endif WHOHOST
