/*
 *      FIONREAD.C
 *      Test the FIONREAD ioctl system call
 *      klin, Mon Feb 24 20:29:50 1992
 *
 *      Defs:   Define BSD for BSD systems.
 *      Test:   Enter some chars (don't forget <RETURN> !)
 *              and see the result from ioctl(FIONREAD).
 *              If some characters were entered, there should be
 *              displayed a message with the number of entered chars
 *              and the entered string!
 */

#include <stdio.h>
#ifndef BSD
#include <sys/termio.h>         /* FIONREAD may be defined here */
#endif
#include <sys/ioctl.h>          /*          or here (like BSD)  */
#include <sys/filio.h>
/*
 *      Main routine
 */
main()
{
  char buf[256];
  int rv, nc;

  /* Terminate with BREAK ! */
  while(1) {
    printf("\nWAITING FOR INPUT ...\n");
    fflush(stdout);
    /* Wait 5 seconds for entering input */
    sleep(5);
    rv = ioctl(0, FIONREAD, &nc);
    printf("\nIOCTL(FIONREAD) rv=%d nc=%d ", rv, nc);
    /* OK: chars read! */
    if(nc > 0) {
      /* Fill and display read characters */
      gets(buf);
      printf("buf=[%s]\n", buf);
    }
    else
      /* Should not happen on input if ioctl(FIONREAD) works! */
      printf("NO INPUT\n");
  }

} /* main() */
