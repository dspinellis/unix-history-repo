/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)stdio.h	5.8 (Berkeley) %G%
 */

#ifndef	_STDIO_H_
#define	_STDIO_H_

/*
 * NB: to fit things in six character monocase externals, the stdio
 * code uses the prefix `__s' for stdio objects, typically followed
 * by a three-character attempt at a mnemonic.
 */

#include <machine/types.h>

#ifdef	_SIZE_T_
typedef	_SIZE_T_	size_t;
#undef	_SIZE_T_
#endif

#ifndef NULL
#define	NULL	0
#endif

typedef long fpos_t;		/* Must match off_t <sys/types.h> */

#define	_FSTDIO			/* Define for new stdio with functions. */

/* stdio buffers */
struct __sbuf {
	unsigned char *_base;
	int	_size;
};

/*
 * stdio state variables.
 *
 * The following always hold:
 *
 *	if (_flags&(__SLBF|__SWR)) == (__SLBF|__SWR),
 *		_lbfsize is -_bf._size, else _lbfsize is 0
 *	if _flags&__SRD, _w is 0
 *	if _flags&__SWR, _r is 0
 *
 * This ensures that the getc and putc macros (or inline functions) never
 * try to write or read from a file that is in `read' or `write' mode.
 * (Moreover, they can, and do, automatically switch from read mode to
 * write mode, and back, on "r+" and "w+" files.)
 *
 * _lbfsize is used only to make the inline line-buffered output stream
 * code as compact as possible.
 *
 * _ub, _up, and _ur are used when ungetc() pushes back more characters
 * than fit in the current _bf, or when ungetc() pushes back a character
 * that does not match the previous one in _bf.  When this happens,
 * _ub._base becomes non-nil (i.e., a stream has ungetc() data iff
 * _ub._base!=NULL) and _up and _ur save the current values of _p and _r.
 */
typedef	struct __sFILE {
	unsigned char *_p;	/* current position in (some) buffer */
	int	_r;		/* read space left for getc() */
	int	_w;		/* write space left for putc() */
	short	_flags;		/* flags, below; this FILE is free if 0 */
	short	_file;		/* fileno, if Unix descriptor, else -1 */
	struct	__sbuf _bf;	/* the buffer (at least 1 byte, if !NULL) */
	int	_lbfsize;	/* 0 or -_bf._size, for inline putc */

	/* operations */
	void	*_cookie;	/* cookie passed to io functions */
#if __STDC__ || c_plusplus
	int	(*_read)(void *_cookie, char *_buf, int _n);
	int	(*_write)(void *_cookie, const char *_buf, int _n);
	fpos_t	(*_seek)(void *_cookie, fpos_t _offset, int _whence);
	int	(*_close)(void *_cookie);
#else
	int	(*_read)();
	int	(*_write)();
	fpos_t	(*_seek)();
	int	(*_close)();
#endif

	/* separate buffer for long sequences of ungetc() */
	struct	__sbuf _ub;	/* ungetc buffer */
	unsigned char *_up;	/* saved _p when _p is doing ungetc data */
	int	_ur;		/* saved _r when _r is counting ungetc data */

	/* tricks to meet minimum requirements even when malloc() fails */
	unsigned char _ubuf[3];	/* guarantee an ungetc() buffer */
	unsigned char _nbuf[1];	/* guarantee a getc() buffer */

	/* separate buffer for fgetline() when line crosses buffer boundary */
	struct	__sbuf _lb;	/* buffer for fgetline() */

	/* Unix stdio files get aligned to block boundaries on fseek() */
	int	_blksize;	/* stat.st_blksize (may be != _bf._size) */
	int	_offset;	/* current lseek offset */
} FILE;

extern FILE __sF[];

#define	__SLBF	0x0001		/* line buffered */
#define	__SNBF	0x0002		/* unbuffered */
#define	__SRD	0x0004		/* OK to read */
#define	__SWR	0x0008		/* OK to write */
	/* RD and WR are never simultaneously asserted */
#define	__SRW	0x0010		/* open for reading & writing */
#define	__SEOF	0x0020		/* found EOF */
#define	__SERR	0x0040		/* found error */
#define	__SMBF	0x0080		/* _buf is from malloc */
#define	__SAPP	0x0100		/* fdopen()ed in append mode */
#define	__SSTR	0x0200		/* this is an sprintf/snprintf string */
#define	__SOPT	0x0400		/* do fseek() optimisation */
#define	__SNPT	0x0800		/* do not do fseek() optimisation */
#define	__SOFF	0x1000		/* set iff _offset is in fact correct */
#define	__SMOD	0x2000		/* true => fgetline modified _p text */

/*
 * The following three definitions are for ANSI C, which took them
 * from System V, which brilliantly took internal interface macros and
 * made them official arguments to setvbuf(), without renaming them.
 * Hence, these ugly _IOxxx names are *supposed* to appear in user code.
 *
 * Although numbered as their counterparts above, the implementation
 * does not rely on this.
 */
#define	_IOFBF	0		/* setvbuf should set fully buffered */
#define	_IOLBF	1		/* setvbuf should set line buffered */
#define	_IONBF	2		/* setvbuf should set unbuffered */

#define	BUFSIZ	1024		/* size of buffer used by setbuf */
#define	EOF	(-1)

/*
 * FOPEN_MAX is a minimum maximum, and should be the number of descriptors
 * that the kernel can provide without allocation of a resource that can
 * fail without the process sleeping.  Do not use this for anything.
 */
#define	FOPEN_MAX	20	/* must be <= OPEN_MAX <sys/syslimits.h> */
#define	FILENAME_MAX	1024	/* must be <= PATH_MAX <sys/syslimits.h> */

/* System V/ANSI C; this is the wrong way to do this, do *not* use these. */
#ifndef _ANSI_SOURCE
#define	P_tmpdir	"/usr/tmp/"
#endif
#define	L_tmpnam	1024	/* XXX must be == PATH_MAX */
#define	TMP_MAX		308915776

#ifndef SEEK_SET
#define	SEEK_SET	0	/* set file offset to offset */
#endif
#ifndef SEEK_CUR
#define	SEEK_CUR	1	/* set file offset to current plus offset */
#endif
#ifndef SEEK_END
#define	SEEK_END	2	/* set file offset to EOF plus offset */
#endif

#define	stdin	(&__sF[0])
#define	stdout	(&__sF[1])
#define	stderr	(&__sF[2])

/*
 * Functions defined in ANSI C standard.
 */
#if __STDC__ || c_plusplus
int	remove(const char *);
int	rename(const char *_old, const char *_new);
FILE	*tmpfile(void);
char	*tmpnam(char *);
int	fclose(FILE *);
int	fflush(FILE *);
FILE	*fopen(const char *_name, const char *_type);
FILE	*freopen(const char *_name, const char *_type, FILE *_stream);
void	setbuf(FILE *, char *);
int	setvbuf(FILE *, char *, int, size_t);
int	fprintf(FILE *, const char *, ...);
int	fscanf(FILE *, const char *, ...);
int	printf(const char *, ...);
int	scanf(const char *, ...);
int	sprintf(char *, const char *, ...);
int	sscanf(char *, const char *, ...);
int	vfprintf(FILE *, const char *, _VA_LIST_);
int	vprintf(const char *, _VA_LIST_);
int	vsprintf(char *, const char *, _VA_LIST_);
int	fgetc(FILE *);
char	*fgets(char *, size_t, FILE *);
int	fputc(int, FILE *);
int	fputs(const char *, FILE *);
int	getc(FILE *);
int	getchar(void);
char	*gets(char *);
int	putc(int, FILE *);
int	putchar(int);
int	puts(const char *);
int	ungetc(int, FILE *);
int	fread(void *, size_t _size, size_t _n, FILE *);
int	fwrite(const void *, size_t _size, size_t _n, FILE *);
int	fgetpos(FILE *, fpos_t *);
int	fseek(FILE *, long, int);
int	fsetpos(FILE *, const fpos_t *);
long	ftell(const FILE *);
void	rewind(FILE *);
void	clearerr(FILE *);
int	feof(FILE *);
int	ferror(FILE *);
void	perror(const char *);
#else
int	remove();
int	rename();
FILE	*tmpfile();
char	*tmpnam();
int	fclose();
int	fflush();
FILE	*fopen();
FILE	*freopen();
void	setbuf();
int	setvbuf();
int	fprintf();
int	fscanf();
int	printf();
int	scanf();
int	sprintf();
int	sscanf();
int	vfprintf();
int	vprintf();
int	vsprintf();
int	fgetc();
char	*fgets();
int	fputc();
int	fputs();
int	getc();
int	getchar();
char	*gets();
int	putc();
int	putchar();
int	puts();
int	ungetc();
int	fread();
int	fwrite();
int	fgetpos();
int	fseek();
int	fsetpos();
long	ftell();
void	rewind();
void	clearerr();
int	feof();
int	ferror();
void	perror();
#endif

/*
 * Functions defined in POSIX 1003.1.
 */
#ifndef _ANSI_SOURCE
#define	L_cuserid	9	/* size for cuserid(); UT_NAMESIZE + 1 */
#define	L_ctermid	1024	/* size for ctermid(); PATH_MAX */

#if __STDC__ || c_plusplus
FILE	*fdopen(int, const char *);
int	fileno(FILE *);
#else
FILE	*fdopen();
int	fileno();
#endif
#endif

/*
 * Routines that are purely local.
 */
#if __STDC__ || c_plusplus
char	*fgetline(FILE *, size_t *);
int	fpurge(FILE *);
int	getw(FILE *);
int	pclose(FILE *);
FILE	*popen(const char *_name, const char *_type);
int	putw(int, FILE *);
void	setbuffer(FILE *, char *, int);
int	setlinebuf(FILE *);
int	snprintf(char *, size_t, const char *, ...);
int	vsnprintf(char *, size_t, const char *, _VA_LIST_);
#else
char	*fgetline();
int	fpurge();
int	getw();
int	pclose();
FILE	*popen();
int	putw();
void	setbuffer();
int	setlinebuf();
int	snprintf();
int	vsnprintf();
#endif
#endif /* _ANSI_SOURCE */

#ifndef _ANSI_SOURCE
/*
 * Stdio function-access interface.
 */
#if __STDC__ || c_plusplus
FILE	*funopen(const void *_cookie,
		int (*readfn)(void *_cookie, char *_buf, int _n),
		int (*writefn)(void *_cookie, const char *_buf, int _n),
		fpos_t (*seekfn)(void *_cookie, fpos_t _off, int _whence),
		int (*closefn)(void *_cookie));
#define	fropen(cookie, fn) funopen(cookie, fn, 0, 0, 0)
#define	fwopen(cookie, fn) funopen(cookie, 0, fn, 0, 0)
#else
FILE	*funopen();
#define	fropen(cookie, fn) \
	funopen(cookie, fn, (int (*)())0, (fpos_t (*)())0, (int (*)())0)
#define	fwopen(cookie, fn) \
	funopen(cookie, (int (*)())0, fn, (fpos_t (*)())0, (int (*)())0)
#endif

/*
 * Functions internal to the implementation.
 */
#if __STDC__ || c_plusplus
int	__srget(FILE *);
int	__swbuf(int, FILE *);
#else
int	__srget();
int	__swbuf();
#endif

/*
 * The __sfoo macros are here so that we can 
 * define function versions in the C library.
 */
#define	__sgetc(p) (--(p)->_r < 0 ? __srget(p) : (int)(*(p)->_p++))
#ifdef __GNUC__
static __inline int __sputc(int _c, FILE *_p) {
	if (--_p->_w >= 0 || (_p->_w >= _p->_lbfsize && (char)_c != '\n'))
		return (*_p->_p++ = _c);
	else
		return (__swbuf(_c, _p));
}
#else
/*
 * This has been tuned to generate reasonable code on the vax using pcc.
 */
#define	__sputc(c, p) \
	(--(p)->_w < 0 ? \
		(p)->_w >= (p)->_lbfsize ? \
			(*(p)->_p = (c)), *(p)->_p != '\n' ? \
				(int)*(p)->_p++ : \
				__swbuf('\n', p) : \
			__swbuf((int)(c), p) : \
		(*(p)->_p = (c), (int)*(p)->_p++))
#endif

#define	__sfeof(p)	(((p)->_flags & __SEOF) != 0)
#define	__sferror(p)	(((p)->_flags & __SERR) != 0)
#define	__sclearerr(p)	((void)((p)->_flags &= ~(__SERR|__SEOF)))
#define	__sfileno(p)	((p)->_file)

#define	feof(p)		__sfeof(p)
#define	ferror(p)	__sferror(p)
#define	clearerr(p)	__sclearerr(p)

#ifndef _ANSI_SOURCE
#define	fileno(p)	__sfileno(p)
#endif

#ifndef lint
#define	getc(fp)	__sgetc(fp)
#define putc(x, fp)	__sputc(x, fp)
#endif /* lint */

#define	getchar()	getc(stdin)
#define	putchar(x)	putc(x, stdout)
#endif /* _STDIO_H_ */
