/* Various tty state structures.
 * Each is an array, subscripted by one of "OFF" or "ON".
 */

#ifdef	UNIX

#if defined(SGTTY) || defined(BRLUNIX)
# include <sgtty.h>
# ifdef	BRLUNIX
extern struct sg_brl	sg[2];
# else
extern struct sgttyb	sg[2];
# endif /* BRLUNIX */
#endif

#ifdef TERMIO
# include <termio.h>
# include <sys/ioctl.h>
extern struct termio	sg[2];
#endif

#ifdef TERMIOS
# include <termios.h>
#ifdef BSD386
# include <sys/ioctl.h>
#endif
extern struct termios	sg[2];
# ifndef VDSUSP
#  define VDSUSP	VSUSP	/* non-Posixism in Irix3.3, may exist in others */
# endif
#endif

#ifdef	SYSVR4
#undef	TIOCSLTC	/* don't let BSD emulation mislead us */
#endif

# ifdef	TIOCSLTC
extern struct ltchars	ls[2];
# endif	/* TIOCSLTC */

#ifdef	SYSV
#undef	TIOCGETC	/* not appropriate for System V */
#endif

# ifdef	TIOCGETC
extern struct tchars	tc[2];
# endif

#endif	/* UNIX */
