/* Various tty state structures.
 * Each is an array, subscripted by one of "OFF" or "ON".
 */

#ifdef UNIX
# ifdef TIOCSLTC
extern struct ltchars	ls[2];
# endif /* TIOCSLTC */

# ifdef TIOCGETC
extern struct tchars	tc[2];
# endif

# ifdef BRLUNIX
extern struct sg_brl	sg[2];
# else
#  ifdef SYSV
extern struct termio	sg[2];
#  else /* SYSV */
extern struct sgttyb	sg[2];
#  endif /* SYSV */
# endif /* BRLUNIX */
#endif /* UNIX */
