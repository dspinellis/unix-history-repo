/* Copyright (c) 1991
 *      Juergen Weigert (jnweiger@immd4.informatik.uni-erlangen.de)
 *      Michael Schroeder (mlschroe@immd4.informatik.uni-erlangen.de)
 * Copyright (c) 1987 Oliver Laumann
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program (see the file COPYING); if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Noteworthy contributors to screen's design and implementation:
 *	Wayne Davison (davison@borland.com)
 *	Patrick Wolfe (pat@kai.com, kailand!pat)
 *	Bart Schaefer (schaefer@cse.ogi.edu)
 *	Nathan Glasser (nathan@brokaw.lcs.mit.edu)
 *	Larry W. Virden (lwv27%cas.BITNET@CUNYVM.CUNY.Edu)
 *	Howard Chu (hyc@hanauma.jpl.nasa.gov)
 *	Tim MacKenzie (tym@dibbler.cs.monash.edu.au)
 *	Markku Jarvinen (mta@{cc,cs,ee}.tut.fi)
 *	Marc Boucher (marc@CAM.ORG)
 *
 ****************************************************************
 * $Id: extern.h,v 1.2 92/02/03 02:27:40 jnweiger Exp $ FAU
 */

/****************************************************************
 * Thanks to Christos S. Zoulas (christos@ee.cornell.edu) who 
 * mangled the screen source through 'gcc -Wall'.
 *
 * this is his extern.h
 ****************************************************************
 */

#ifndef MEMFUNCS_DECLARED /* bsd386 */
# ifndef SYSV
extern void bzero __P((char *, int));
# endif
# ifdef sun
extern char *memset __P((char *, int, size_t));
# endif
# ifndef bcopy
extern void bcopy __P((char *, char *, int));
# endif /* bcopy */
#endif /* MEMFUNCS_DECLARED */
struct rusage;
#ifndef WAITSTUFF_DECLARED
# ifdef BSDWAIT
union wait;
extern int wait3 __P((union wait *, int, struct rusage *));
# else
extern pid_t wait3 __P((int *, int, struct rusage *));
# endif
#endif /* WAITSTUFF_DECLARED */
extern int getdtablesize __P((void));
#ifndef REUID_DECLARED
# if !defined(NOREUID)
#  ifdef hpux
extern int setresuid __P((uid_t, uid_t, uid_t));
extern int setresgid __P((gid_t, gid_t, gid_t));
#  else
extern int setreuid __P((uid_t, uid_t));
extern int setregid __P((gid_t, gid_t));
#  endif
# endif
#endif /* REUID_DECLARED */
#ifndef CRYPT_DECLARED
extern char *crypt __P((char *, char *));
#endif /* CRYPT_DECLARED */
#ifdef sun
extern int getpgrp __P((int));
#endif
#ifndef MKNOD_DECLARED
# ifdef POSIX
extern int mknod __P((const char *, mode_t, dev_t));
# else
extern int mknod __P((char *, int, int));
# endif
#endif /* MKNOD_DECLARED */
#ifndef PUTENV_DECLARED
extern int putenv __P((char *));
#endif /* PUTENV_DECLARED */
#ifndef KILLSTUFF_DECLARED
extern int kill __P((pid_t, int));
# ifndef SYSV
extern int killpg __P((pid_t, int));
# endif
#endif /* KILLSTUFF_DECLARED */
extern int tgetent __P((char *, char *));
extern int tgetnum __P((char *));
extern int tgetflag __P((char *));
extern void tputs __P((char *, int, void (*)(int)));
#ifdef notdef
extern unsigned char     *_flsbuf __P((unsigned char, FILE *));
#endif
#ifndef NeXT
extern int _flsbuf __P((unsigned char, FILE *));
#endif
# ifdef POSIX
extern pid_t setsid __P((void));
#  ifndef SETPGID_DECLARED
extern int setpgid __P((pid_t, int));
#  endif /* SETPGID_DECLARED */
extern int tcsetpgrp __P((int, pid_t));
# endif /* POSIX */
extern pid_t getpid __P((void));
extern uid_t getuid __P((void)); 
extern uid_t geteuid __P((void));
extern gid_t getgid __P((void)); 
extern gid_t getegid __P((void));
extern int isatty __P((int)); 
#ifdef notdef
extern int chown __P((const char *, uid_t, gid_t)); 
#endif
#ifndef GETHOSTNAME_DECLARED
extern int gethostname __P((char *, size_t));
#endif /* GETHOSTNAME_DECLARED */
extern off_t lseek __P((int, off_t, int));
#if defined(sun) && !defined(__GNUC__)		/* sun's exit returns ??? */
extern int exit __P((int));
#else
extern void exit __P((int));
#endif
extern char *getwd __P((char *));
extern char *getenv __P((const char *));
extern time_t time __P((time_t *));

extern char *getlogin(), *getpass(), *ttyname();
extern int fflush(); 
#if !defined(__STDC__) || !defined(POSIX)
extern char *malloc(), *realloc();
#endif

extern char *Filename __P((char *));
extern char *MakeTermcap __P((int));
extern char *ProcessInput __P((char *, int *, char *, int *, int));
extern char *SaveStr __P((char *));
extern char *findcap __P((char *, char **, int));
extern char *strdup __P((const char *));
extern int ChangeScrollback __P((struct win *, int, int));
extern int ChangeWindowSize __P((struct win *, int, int));
extern int CompileKeys __P((char *, char *));
extern int CountUsers __P((void));
extern int FindSocket __P((int, int *));
extern int GetAvenrun __P((void));
extern int MakeClientSocket __P((int, char *));
extern int MakeServerSocket __P((void));
extern int MakeWindow __P((char *, char **, int, int, int, char *, int, int, char *));
extern int MarkRoutine __P((int));
extern int ParseEscape __P((char *));
extern void RcLine __P((char *));
extern int RecoverSocket __P((void));
extern int RemoveUtmp __P((struct win *));
extern int SetUtmp __P((struct win *, int));
extern int UserContext __P((void));
extern int UserStatus __P((void));
extern int display_help __P((void));
extern void display_copyright __P((void));
#ifdef DEBUG
extern SIGTYPE FEChld __P(SIGPROTOARG);
#endif
extern SIGTYPE SigHup __P(SIGPROTOARG);
extern void Activate __P((int));
extern void ChangeScreenSize __P((int, int, int));
extern void ChangeScrollRegion __P((int, int));
extern void CheckLP __P((int));
extern void CheckScreenSize __P((int));
extern void ClearDisplay __P((void));
extern void Detach __P((int));
extern void DisplayLine __P((char *, char *, char *, char *, char *, char *, int, int, int));
extern void DoScreen __P((char *, char **));
extern void DoSet __P((char **));
extern void ExitOverlayPage __P((void));
extern void FinishRc __P((char *));
extern void FinitTerm __P((void));
extern void FixLP __P((int, int));
extern void GetTTY __P((int, struct mode *));
extern void GotoPos __P((int, int));
extern void InitKmem __P((void));
extern void InitOverlayPage __P((void (*)(), void (*)(), int (*)(), int));
extern void InitTerm __P((int));
extern void InitTermcap __P((void));
extern void InitUtmp __P((void));
extern void InputAKA __P((void));
extern void InputColon __P((void));
extern void InsertMode __P((int));
extern void Kill __P((int, int));
extern void KillBuffers __P((void));
extern void MakeBlankLine __P((char *, int));
extern void MakeStatus __P((char *));
#ifdef USEVARARGS
extern void Msg __P((int, char *, ...));
#else
extern void Msg __P(());
#endif
extern void NewAutoFlow __P((struct win *, int));
extern void NewCharset __P((int));
extern void NewRendition __P(());
extern void PUTCHAR __P((int));
extern void INSERTCHAR __P((int));
extern void PutChar __P((int));
extern void PutStr __P((char *));
extern void ReInitUtmp __P((void));
extern void ReadFile __P((void));
extern void ReceiveMsg __P((int));
extern void Redisplay __P((int));
extern void RefreshLine __P((int, int, int));
#ifdef SVR4
struct utmpx;
extern void RemoveLoginSlot __P((slot_t, struct utmpx *));
#else
struct utmp;
extern void RemoveLoginSlot __P((slot_t, struct utmp *));
#endif
extern void RemoveStatus __P((void));
extern void Report __P((struct win *, char *, int, int));
extern void ResetScreen __P((struct win *));
extern void ResizeScreen __P((struct win *));
extern void RestoreAttr __P((void));
extern void RestoreLoginSlot __P((void));
extern void SaveSetAttr __P((int, int));
extern void ScrollRegion __P((int, int, int));
extern void SendCreateMsg __P((int, int, char **, int, int, int, int, char *));
#ifdef USEVARARGS
extern void SendErrorMsg __P((char *, ...));
#else
extern void SendErrorMsg __P(());
#endif
extern void SetFlow __P((int));
extern void SetLastPos __P((int, int));
extern void SetMode __P((struct mode *, struct mode *));
extern void SetOvlCurr __P((void));
extern void SetTTY __P((int, struct mode *));
extern void SlotToggle __P((int));
extern void StartRc __P((char *));
extern void SwitchWindow __P((int));
extern void UserReturn __P((int));
extern void WSresize __P((int, int));
extern void WriteFile __P((int));
extern void WriteString __P((struct win *, char *, int));
extern void bclear __P((char *, int));
#if !defined(MEMFUNCS_DECLARED) && !defined(bcopy)
extern void bcopy __P((char *, char *, int));
#endif /* !MEMFUNCS_DECLARED && !bcopy */
extern void eexit __P((int));
extern void exit_with_usage __P((char *));
extern void main __P((int, char **));
extern void screen_builtin_lck __P((void));
extern void KillWindow __P((int));
extern char *xrealloc __P((char *, int));
extern void AddLineToHist __P((struct win *, char **, char **, char **));
extern FILE *secfopen __P((char *, char *));
extern char *stripdev __P((char *));
extern int secopen __P((char *, int, int));
