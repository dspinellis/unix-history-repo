/*
 *      SIGWINCH.C
 *      Test signal SIGWINCH and TIOCGWINSZ ioctl system call
 *      klin, Mon Feb 24 20:35:53 1992
 *
 *      Defs:   Define BSD for BSD systems.
 *      Test:   Start sigwinch under X in a xterm window and resize
 *              the window sometimes. After resizing there should be
 *              sent a signal SIGWINCH from the kernel which is catched
 *              by the sigwinch signal handler. The ioctl(TIOCGWINSZ)
 *              systemcalls should return the new size of the window.
 */

#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#ifndef BSD
#include <sys/termio.h>         /* struct winsize may be defined here */
#endif
#include <sys/ioctl.h>          /*                or here (like BSD)  */

/*
 *      Signal handler for signal SIGWINCH
 */
sigwinch(sig)
  int sig;
{
  struct winsize ws;
  int rv;

  signal(SIGWINCH, sigwinch);
  if(sig)
    printf("SIGWINCH-HANDLER: sig=%d\n", sig);
  rv = ioctl(0, TIOCGWINSZ, &ws);
  printf("IOCTL: rv=%d rows=%d cols=%d\n", rv, ws.ws_row, ws.ws_col);

} /* sigwinch() */

/*
 *      Main routine
 */
main()
{
  sigwinch(0);
  while(1)
    ;

} /* main() */
