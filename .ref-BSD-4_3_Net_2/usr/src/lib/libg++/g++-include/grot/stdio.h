// This may look like C code, but it is really -*- C++ -*-
/* 
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  
*/

/*
 *  Please check the following before installing this file:
 *
 *  Make sure USG is #defined if you are on a USG system!
 *
 *  Check the value of _NFILE against the one in your /usr/include/stdio.h.
 *  (USG only)
 *
 *  Check whether your libc.a sprintf function returns
 *  an int (as do most) versus a char* (BSD), and (un)comment
 *  the corresponding SPRINTF_RETURNS_INT line.
 *
 *  Check the value of BUFSIZ against the one in your /usr/include/stdio.h.
 *
 *  Carefully check the fields and order of _iobuf declaration against
 *  the one in your /usr/include/stdio.h. Xenix-based systems
 *  may need some re-ordering of _iobuf. fields.
 *
 *  Note that some _IOXXX #defines may not be present in your 
 *  /usr/include/stdio.h. This is ok, so long as the ones that
 *  are present in both are set to the same values.
 *
 *  Some of the prototypes refer to functions that may not be
 *  present in your libc.a. This is ok so long as you do not
 *  actually call such functions.
 *
 */

#ifndef _stdio_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _stdio_h 1

/*
   HAVE_VPRINTF should be set if vprintf is in libc.a
   HAVE_SETVBUF should be set if setvbuf is in libc.a
   HAVE_SETLINEBUF should be set if setlinebuf in libc.a

   The following are probably correct for the listed systems

*/

#ifdef SYSTEM_FIVE
#define USG
#endif

#if defined(USG)
#define  HAVE_VPRINTF
#define  HAVE_SETVBUF  
//#define  HAVE_SETLINEBUF

#elif defined(VMS)
#define  HAVE_VPRINTF
#define  HAVE_SETVBUF  
//#define  HAVE_SETLINEBUF
#define NO_LIBGXX_MALLOC
#define _NFILE 20
#define IOBUF_FLAG_TYPE char
#define IOBUF_BUFSIZ_FIELD	/* we aint got one either*/

#elif masscomp
#ifndef _NFILE
#define _NFILE 40
#endif
#ifdef _ATT
#define HAVE_VPRINTF
#define HAVE_SETVBUF  
#define USG
#endif
#ifdef _UCB
#undef HAVE_VPRINTF
#undef HAVE_SETLINEBUF
#define IOBUF_BUFSIZ_FIELD
#endif

#elif defined(vax) || defined(sony)
//#define HAVE_VPRINTF
//#define  HAVE_SETVBUF  
#define HAVE_SETLINEBUF

#elif defined(sun)
#define  HAVE_VPRINTF
#define  HAVE_SETVBUF  
#define  HAVE_SETLINEBUF

#elif defined(sequent)
//#define  HAVE_VPRINTF
//#define  HAVE_SETVBUF  
#define  HAVE_SETLINEBUF

#elif defined(DGUX)
#define HAVE_VPRINTF
#define HAVE_SETVBUF
#define HAVE_SETLINEBUF
#define IOBUF_FLAG_TYPE	int
#define IOBUF_FILE_TYPE	int
#define _NFILE 64
#define IOBUF_EXTRA_FIELDS	int _reserve[27];
#define SPRINTF_RETURNS_INT
#define BUFEND_ENTRY_TYPE unsigned char *
#define USG

#elif defined(TEKTRONIX_SYSV) || defined(hp9000s300) || defined(i386)
#define  HAVE_VPRINTF
#define  HAVE_SETVBUF  
//#define HAVE_SETLINEBUF
#define  USG

#elif defined(convex)
//#define HAVE_VPRINTF
//#define HAVE_SETVBUF
#define HAVE_SETLINEBUF
#define IOBUF_FILE_TYPE unsigned char
#define HAVE_VOID_DOPRNT


#endif

#ifdef hpux
#define IOBUF_FLAG_TYPE short
#endif

#ifdef i386
#define IOBUF_FLAG_TYPE char
#endif

#ifdef USG
#define IOBUF_BUFSIZ_FIELD	/* System V ain't got one */
#endif

/* Some default definitions for things not defined machine-specifically
   above.  */

#ifndef IOBUF_BUFSIZ_FIELD
#define IOBUF_BUFSIZ_FIELD int _bufsiz;
#endif

#ifndef IOBUF_FLAG_TYPE
#define IOBUF_FLAG_TYPE short
#endif

#ifndef IOBUF_FILE_TYPE
#define IOBUF_FILE_TYPE char
#endif

#ifndef IOBUF_EXTRA_FIELDS
#define IOBUF_EXTRA_FIELDS
#endif

#if defined(USG) && !defined(_NFILE)
#ifdef hpux
#define _NFILE 60
#elif
#define _NFILE 20
#endif
#endif

#ifdef USG
#define _bufend(p) _bufendtab[(p)->_file]
#define _bufsiz(p) (_bufend(p) - (p)->_base)
#ifndef BUFEND_ENTRY_TYPE
#define BUFEND_ENTRY_TYPE char *
#endif
extern "C" {
BUFEND_ENTRY_TYPE _bufendtab[];
}
#endif

/* check this -- hardly any systems need this these days */
/* #define SPRINTF_RETURNS_INT */

/* check and possibly redefine the following */

#ifndef VMS
#define BUFSIZ  1024            
#else
#define BUFSIZ 512
#endif

#ifdef masscomp 
extern struct _iobuf {
#ifdef BUFSIZ
#  undef BUFSIZ
#endif
#define BUFSIZ  4096

#ifdef mc68000
    unsigned char*    _ptr;
    int      _cnt;
#endif
    char*    _base;
    IOBUF_BUFSIZ_FIELD
    IOBUF_FLAG_TYPE	_flag;
    IOBUF_FILE_TYPE	_file;
    IOBUF_EXTRA_FIELDS
} _iob[];
#else /* not masscomp */
extern  struct  _iobuf {
    int      _cnt;
    char*    _ptr;
    char*    _base;
    IOBUF_BUFSIZ_FIELD
    IOBUF_FLAG_TYPE	_flag;
    IOBUF_FILE_TYPE	_file;
    IOBUF_EXTRA_FIELDS
} _iob[];
#endif

#ifndef VMS
typedef struct _iobuf FILE;
#else
typedef struct _iobuf *FILE;
typedef FILE *VMS_FILE;
#endif

#define _IOFBF    00000
#define _IOREAD   00001
#define _IOWRT    00002
#define _IONBF    00004
#define _IOMYBUF  00010
#define _IOEOF    00020
#define _IOERR    00040
#if defined(USG) && !defined(hpux)
#define _IOSTRG   00000  /* faked out for USG */
#define _IOLBF    00100
#define _IORW     00200
#define _IOAPPEND 00000 /* faked out for USG */
#elif defined(masscomp)
#define _IOSTRG		0x00		/* not used in this version of stdio */
#ifdef _IOERR
# undef _IOERR
#endif
#define	_IOERR		0x40		/* i/o error on buffer */
#define	_IOLBF		0x80		/* buffering is by line */
#define	_IORW		0x100		/* read or write ok */
#define _IOAPPEND	0x00		/* not used in this version of stdio */
#elif defined(hpux)
#define _IOSTRG   00000  /* faked out for USG */
#define _IOLBF    00200
#define _IORW     00400
#define _IOAPPEND 00000 /* faked out for USG */
#elif defined(VMS)
#define	_IOSTRG		0100			/* Doing I/O to a string */
#define	_IORW		0200			/* Open for read/write	 */
#define _IOAPPEND	00		/* VMS does not actually have this */
#else
#define _IOSTRG   00100
#define _IOLBF    00200
#define _IORW     00400
#define _IOAPPEND 01000
#endif
#ifdef DGUX
#define _IOPBF    00400  /* Boolean - ungetc() not called since fill/flush */
#endif

#define EOF       (-1)

#ifndef NULL
#define NULL      0
#endif

#ifndef VMS
#define stdin     (&_iob[0])
#define stdout    (&_iob[1])
#define stderr    (&_iob[2])

#define getc(p) (--(p)->_cnt>=0?(int)(*(unsigned char*)(p)->_ptr++):_filbuf(p))
#define putc(x,p) (--(p)->_cnt>=0? ((int)((unsigned char)((*(p)->_ptr++=(unsigned)(x))))):_flsbuf((unsigned)(x),p))

#define clearerr(p) ((p)->_flag &= ~(_IOERR|_IOEOF))
#define getchar()   getc(stdin)
#define putchar(x)  putc(x,stdout)
#define feof(p)     (((p)->_flag&_IOEOF)!=0)
#define ferror(p)   (((p)->_flag&_IOERR)!=0)
#define fileno(p)   ((p)->_file)

#else

#undef _IOFBF
#define _IOLBF 1
#define _IOFBF 2
#define F_RDLCK
/*	Also, stdin/stdout/stderr need to be defined
 *	[We also use a hack here that makes the GCC assembler modify
 *	 the psect attributes to match those of the VAX-11 "C" runtime]
 */
#define	stdin	$$PsectAttributes_NOSHR$$stdin
#define	stdout	$$PsectAttributes_NOSHR$$stdout
#define	stderr	$$PsectAttributes_NOSHR$$stderr

extern VMS_FILE stdin;
extern VMS_FILE stdout;
extern VMS_FILE stderr;

// 	Define the stdio macros

#define getc(p)		fgetc(p)
#define getchar()	fgetc(stdin)
#define putc(x,p)	fputc(x,p)
#define putchar(x)	fputc(x,stdout)
#define feof(p)		(((*p)->_flag&_IOEOF)!=0)
#define ferror(p)	(((*p)->_flag&_IOERR)!=0)
#define fileno(p)	((*p)->_file)
#define clearerr(p)	((*p)->_flag &= ~(_IOERR|_IOEOF))

#endif

extern "C" {

#ifdef VMS
int     c$$doprint(const char*, void*, FILE*);
int     c$$doscan(FILE*, const char*, ...);
int	c$$filbuf(FILE*);
int	c$$flsbuf(unsigned, FILE*);
#else
int    _doprnt(const char*, void*, FILE*);
int    _doscan(FILE*, const char*, ...);
int    _filbuf(FILE*);
int    _flsbuf(unsigned, FILE*);
#endif
int    fclose(FILE*);
FILE*  fdopen(int, const char*);
int    fflush(FILE*);
int    fgetc(FILE*);
char*  fgets(char*, int, FILE *);
FILE*  fopen(const char*, const char*);
int    fprintf(FILE*, const char* ...);
int    fputc(int, FILE*);
int    fputs(const char*, FILE*);
int    fread(void*, int, int, FILE*);
#ifdef VMS
FILE*  freopen(const char*, const char*, FILE* ...);
#else
FILE*  freopen(const char*, const char*, FILE*);
#endif
int    fscanf(FILE*, const char* ...);
int    fseek(FILE*, long, int);
long   ftell(FILE *);
int    fwrite(const void*, int, int, FILE*);
char*  gets(char*);
int    getw(FILE*);
int    pclose(FILE*);
FILE*  popen(const char*, const char*);
int    printf(const char* ...);
int    puts(const char*);
int    putw(int, FILE*);
int    rewind(FILE*);
int    scanf(const char* ...);
int    setbuf(FILE*, char*);
int    setbuffer(FILE*, char*, int);
int    setlinebuf(FILE*);
int    setvbuf(FILE*, char*, int, int);
int    sscanf(char*, const char* ...);
FILE*  tmpfile();
int    ungetc(int, FILE*);
int    vfprintf(FILE*, const char*, ...);

// Third arg to vprintf must be '...' for some machines, & doesn't
// hurt for others.

int    vprintf(const char*, ... );

#ifdef SPRINTF_RETURNS_INT
int    sprintf(char*, const char* ...);
int    vsprintf(char*, const char*, ...);
#else
char*  sprintf(char*, const char* ...);
char*  vsprintf(char*, const char*, ...);
#endif

}

#ifndef L_ctermid
#define L_ctermid	9 
#endif
#ifndef L_cuserid
#define L_cuserid	9
#endif
#ifndef P_tmpdir
#define	P_tmpdir    "/tmp/"
#endif
#ifndef L_tmpnam
#define	L_tmpnam    (sizeof(P_tmpdir) + 15)
#endif


#endif // _stdio_h
