/* mksignames.c -- Create and write an array of signal names. */

#include <stdio.h>
#include <signal.h>

#if !defined (NSIG)
# define NSIG 64
#endif

int main()
{
  char *signal_names[NSIG];
  char *signal_descs[NSIG];
  int i;

  for (i = 1; i < NSIG; i++)
    signal_descs[i] = signal_names[i] = (char *)NULL;

  /* `signal' 0 is what we do on exit. */
  signal_names[0] = "EXIT";
  signal_descs[0] = "Exit";

  /* Place signal names which can be aliases for more common signal
     names first.  This allows (for example) SIGEMT to overwrite SIGGRANT. */

#if defined (SIGGRANT)
  signal_names[SIGGRANT] = "SIGGRANT";
  signal_descs[SIGGRANT] = "HFT monitor mode granted";
#endif

#if defined (SIGRETRACT)
  signal_names[SIGRETRACT] = "SIGRETRACT";
  signal_descs[SIGRETRACT] = "HFT monitor mode retracted"!
#endif

#if defined (SIGHUP)
  signal_names[SIGHUP] = "SIGHUP";
  signal_descs[SIGHUP] = "Hangup";
#endif

#if defined (SIGINT)
  signal_names[SIGINT] = "SIGINT";
  signal_descs[SIGINT] = "Interrupt";
#endif

#if defined (SIGQUIT)
  signal_names[SIGQUIT] = "SIGQUIT";
  signal_descs[SIGQUIT] = "Quit";
#endif

#if defined (SIGILL)
  signal_names[SIGILL] = "SIGILL";
  signal_descs[SIGILL] = "Illegal instruction";
#endif

#if defined (SIGTRAP)
  signal_names[SIGTRAP] = "SIGTRAP";
  signal_descs[SIGTRAP] = "Trace/BPT trap";
#endif

#if defined (SIGIOT)
  signal_names[SIGIOT] = "SIGIOT";
  signal_descs[SIGIOT] = "IOT trap";
#endif

#if defined (SIGABRT)
  signal_names[SIGABRT] = "SIGABRT";
  signal_descs[SIGABRT] = "Abort";
#endif

#if defined (SIGEMT)
  signal_names[SIGEMT] = "SIGEMT";
  signal_descs[SIGEMT] = "EMT trap";
#endif

#if defined (SIGFPE)
  signal_names[SIGFPE] = "SIGFPE";
  signal_descs[SIGFPE] = "Floating point exception";
#endif

#if defined (SIGKILL)
  signal_names[SIGKILL] = "SIGKILL";
  signal_descs[SIGKILL] = "Killed";
#endif

#if defined (SIGBUS)
  signal_names[SIGBUS] = "SIGBUS";
  signal_descs[SIGBUS] = "Bus error";
#endif

#if defined (SIGSEGV)
  signal_names[SIGSEGV] = "SIGSEGV";
  signal_descs[SIGSEGV] = "Segmentation violation";
#endif

#if defined (SIGSYS)
  signal_names[SIGSYS] = "SIGSYS";
  signal_descs[SIGSYS] = "Bad system call";
#endif

#if defined (SIGPIPE)
  signal_names[SIGPIPE] = "SIGPIPE";
  signal_descs[SIGPIPE] = "Broken pipe";
#endif

#if defined (SIGALRM)
  signal_names[SIGALRM] = "SIGALRM";
  signal_descs[SIGALRM] = "Alarm clock";
#endif

#if defined (SIGTERM)
  signal_names[SIGTERM] = "SIGTERM";
  signal_descs[SIGTERM] = "Terminated";
#endif

#if defined (SIGCLD)
  signal_names[SIGCLD] = "SIGCLD";
  signal_descs[SIGCLD] = "Child exited or stopped";
#endif

#if defined (SIGPWR)
  signal_names[SIGPWR] = "SIGPWR";
  signal_descs[SIGPWR] = "Power failure imminent";
#endif

#if defined (SIGPOLL)
  signal_names[SIGPOLL] = "SIGPOLL";
  signal_descs[SIGPOLL] = "I/O possible";
#endif

#if defined (SIGURG)
  signal_names[SIGURG] = "SIGURG";
  signal_descs[SIGURG] = "Urgent I/O condition";
#endif

#if defined (SIGSTOP)
  signal_names[SIGSTOP] = "SIGSTOP";
  signal_descs[SIGSTOP] = "Stopped";
#endif

#if defined (SIGTSTP)
  signal_names[SIGTSTP] = "SIGTSTP";
  signal_descs[SIGTSTP] = "Suspended";
#endif

#if defined (SIGCONT)
  signal_names[SIGCONT] = "SIGCONT";
  signal_descs[SIGCONT] = "Continued";
#endif

#if defined (SIGCHLD)
  signal_names[SIGCHLD] = "SIGCHLD";
  signal_descs[SIGCHLD] = "Child exited or stopped";
#endif

#if defined (SIGTTIN)
  signal_names[SIGTTIN] = "SIGTTIN";
  signal_descs[SIGTTIN] = "Stopped (tty input)";
#endif

#if defined (SIGTTOU)
  signal_names[SIGTTOU] = "SIGTTOU";
  signal_descs[SIGTTOU] = "Stopped (tty output)";
#endif

#if defined (SIGIO)
  signal_names[SIGIO] = "SIGIO";
  signal_descs[SIGIO] = "I/O possible";
#endif

#if defined (SIGXCPU)
  signal_names[SIGXCPU] = "SIGXCPU";
  signal_descs[SIGXCPU] = "CPU time limit exceeded";
#endif

#if defined (SIGXFSZ)
  signal_names[SIGXFSZ] = "SIGXFSZ";
  signal_descs[SIGXFSZ] = "File size limit exceeded";
#endif

#if defined (SIGVTALRM)
  signal_names[SIGVTALRM] = "SIGVTALRM";
  signal_descs[SIGVTALRM] = "Alarm (virtual)";
#endif

#if defined (SIGPROF)
  signal_names[SIGPROF] = "SIGPROF";
  signal_descs[SIGPROF] = "Alarm (profile)";
#endif

#if defined (SIGWINCH)
  signal_names[SIGWINCH] = "SIGWINCH";
  signal_descs[SIGWINCH] = "Window changed";
#endif

#if defined (SIGLOST)
  signal_names[SIGLOST] = "SIGLOST";
  signal_descs[SIGLOST] = "Record lock";
#endif

#if defined (SIGUSR1)
  signal_names[SIGUSR1] = "SIGUSR1";
  signal_descs[SIGUSR1] = "User signal 1";
#endif

#if defined (SIGUSR2)
  signal_names[SIGUSR2] = "SIGUSR2";
  signal_descs[SIGUSR2] = "User signal 2";
#endif

#if defined (SIGMSG)
  signal_names[SIGMSG] = "SIGMSG";
  signal_descs[SIGMSG] = "HFT input data pending";
#endif

#if defined (SIGDANGER)
  signal_names[SIGDANGER] = "SIGDANGER";
  signal_descs[SIGDANGER] = "System crash imminent";
#endif

#if defined (SIGMIGRATE)
  signal_names[SIGMIGRATE] = "SIGMIGRATE";
  signal_descs[SIGMIGRATE] = "Migrate process to another CPU";
#endif

#if defined (SIGPRE)
  signal_names[SIGPRE] = "SIGPRE";
  signal_descs[SIGPRE] = "Programming error";
#endif

#if defined (SIGSOUND)
  signal_names[SIGSOUND] = "SIGSOUND";
  signal_descs[SIGSOUND] = "HFT sound sequence completed";
#endif

#if defined (SIGSAK)
  signal_names[SIGSAK] = "SIGSAK";
  signal_descs[SIGSAK] = "Secure Attention Key";
#endif

#if defined (SIGWINDOW)
  signal_names[SIGWINDOW] = "SIGWINDOW";
#endif

#if defined (SIGDIL)
  signal_names[SIGDIL] = "SIGDIL";
#endif

  printf("/* This file was generated automatically */\n\n");
  printf("#include <sys/cdefs.h>\n");
  printf("#include <signal.h>\n\n");

  printf("const char *const sys_signame[NSIG+1] = {\n");
  for (i = 0; i < NSIG; i++)
    if (signal_names[i] == (char *)NULL)
      printf("    \"SIGJUNK(%d)\",\n", i);
    else
      printf("    \"%s\",\n", signal_names[i]);
  printf("    (char *)0\n");
  printf("};\n\n");

  printf("const char *const sys_siglist[NSIG+1] = {\n");
  for (i = 0; i < NSIG; i++)
    if (signal_descs[i] == (char *)NULL)
      printf("    \"Unknown signal %d\",\n", i);
    else
      printf("    \"%s\",\n", signal_descs[i]);
  printf("    (char *)0\n");
  printf("};\n");

  return 0;
}
