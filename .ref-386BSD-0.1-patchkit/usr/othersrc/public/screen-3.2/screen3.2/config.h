/* from libove@libove.det.dec.com (Jay Vassos-Libove)
 ****************************************************************
 * $Id: config.bsd386,v 1.2 92/02/03 02:30:42 jnweiger Exp $ FAU
 */
#define POSIX
#define BSDJOBS
#define TERMIO
#undef TERMINFO
#undef SYSV
#define SIGVOID 
#define DIRENT
#define SUIDROOT
#define UTMPOK
#define LOGINDEFAULT	1
#undef GETUTENT
#define UTHOST
#undef USRLIMIT
#undef LOCKPTY
#undef NOREUID
#define  LOADAV_3DOUBLES
#undef LOADAV_3LONGS
#undef  LOADAV_4LONGS
#define GETTTYENT
#undef NFS_HACK
#undef LOCALSOCKDIR
#ifdef LOCALSOCKDIR
# ifndef TMPTEST
#  define SOCKDIR "/tmp/screens"
# else
#  define SOCKDIR "/tmp/testscreens"
# endif
#endif
#define USEBCOPY
#undef TOPSTAT
#define USEVARARGS
#undef NAMEDPIPE
#define LOCK
#define PASSWORD
#define COPY_PASTE
#define REMOTE_DETACH
#define POW_DETACH
#define NETHACK
#define ETCSCREENRC "/usr/local/etc/screenrc"

/* These get seen in screen.h, extern.h and other places to avoid conflicts
 * with the definitions and declarations in /usr/include/...
 * on BSD/386 from BSD, Inc.
 */
/* modified for 386BSD by rich@rice.edu */
#define SIG_T_DEFINED
#define PID_T_DEFINED
#define MEMFUNCS_DECLARED
#define WAITSTUFF_DECLARED
#define KILLSTUFF_DECLARED
#define REUID_DECLARED
#define CRYPT_DECLARED
#define MKNOD_DECLARED
#define PUTENV_DECLARED
#define SETPGID_DECLARED
#define GETHOSTNAME_DECLARED
#define VPRNT_DECLARED
#define NLIST_DECLARED
