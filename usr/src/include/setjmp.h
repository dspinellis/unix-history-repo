/*	setjmp.h	4.2	89/10/16	*/

#ifndef _SETJMP_
#define _SETJMP_

#ifdef vax
#define _JBLEN	10
#endif

#ifdef tahoe
#define _JBLEN	10
#endif

#ifdef hp300
#define _JBLEN	17
#endif

typedef int jmp_buf[_JBLEN];

#ifdef __STDC__
extern int setjmp(jmp_buf), _setjmp(jmp_buf);
extern void longjmp(jmp_buf, int), _longjmp(jmp_buf, int);
#else	/* !__STDC__ */
extern int setjmp(), _setjmp();
extern int longjmp(), _longjmp();
#endif	/* !__STDC__ */

#endif
