/* Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.
   Distributed by Free Software Foundation, Inc.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* stream.c */
/* Stream package for Ghostscript interpreter */
#include <stdio.h>
#include "memory_.h"
#include "std.h"
#include "stream.h"
#include "scanchar.h"

/* Forward declarations */
	/* Generic */
/* Export these for filters */
void
  s_std_init(P5(stream *, byte *, uint, stream_procs *, int /*mode*/));
int
  s_std_null(P1(stream *)),
  s_std_noavailable(P2(stream *, long *)),
  s_std_close(P1(stream *));
	/* Strings */
private int
  s_string_read_buf(P1(stream *)),
  s_string_write_buf(P1(stream *)),
  s_string_available(P2(stream *, long *)),
  s_string_seek(P2(stream *, long));
private void
  s_string_init(P4(stream *, byte *, uint, stream_procs *));
	/* Files */
private int
  s_file_read_buf(P1(stream *)),
  s_file_available(P2(stream *, long *)),
  s_file_read_seek(P2(stream *, long)),
  s_file_read_close(P1(stream *));
private int
  s_file_write_buf(P1(stream *)),
  s_file_write_seek(P2(stream *, long)),
  s_file_write_flush(P1(stream *)),
  s_file_write_close(P1(stream *));

/* ------ Generic procedures ------ */

/* Standard stream initialization */
void
s_std_init(register stream *s, byte *ptr, uint len, stream_procs *pp,
  int mode)
{	s->cbuf = ptr;
	s->cptr = ptr - 1;
	s->endptr = (mode == s_mode_write ? s->cptr + len : s->cptr);
	s->mode = mode;
	s->end_status = 0;
	s->bsize = s->cbsize = len;
	s->strm = 0;			/* not a filter */
	s->procs = *pp;
}

/* Implement a stream procedure as a no-op. */
int
s_std_null(stream *s)
{	return 0;
}

/* Indicate an error when asked for available input bytes. */
int
s_std_noavailable(stream *s, long *pl)
{	return ERRC;
}

/* Standard stream finalization.  Disable the stream. */
int
s_std_close(stream *s)
{	s_disable(s);
	return 0;
}
void
s_disable(register stream *s)
{	s->bsize = 0;
	s->mode = 0;
	/****** SHOULD DO MORE THAN THIS ******/
}

/* ------ Implementation-independent procedures ------ */

/* Implement sgetc when the buffer is empty, */
/* by refilling the buffer and then reading a byte. */
int
spgetc(register stream *s)
{	int code;
	if ( s->end_status ) return s->end_status;
	code = (*s->procs.read_buf)(s);
	if ( code < 0 ) return code;
	if ( sendbufp(s) )
		return (s->end_status ? s->end_status : EOFC);
	return *++(s->cptr);
}

/* Implementing sputc when the buffer is full, */
/* by flushing the buffer and then writing the byte. */
int
spputc(register stream *s, byte b)
{	int code;
	if ( s->end_status ) return s->end_status;
	code = (*s->procs.write_buf)(s);
	if ( code < 0 ) return code;
	return sputc(s, b);
}

/* Push back a character onto a (read) stream. */
/* Return 0 on success, ERRC on failure. */
int
sungetc(register stream *s, byte c)
{	if ( !s_is_reading(s) || s->cptr < s->cbuf ) return ERRC;
	*(s->cptr)-- = c;
	return 0;
}

/* Read a string from a stream. */
/* Return the number of bytes read. */
uint
sgets(register stream *s, byte *str, uint rlen)
{	uint len = rlen;
	while ( len > 0 )
	   {	uint count = sbufavailable(s);
		if ( count == 0 )
		   {	int code;
			if ( s->end_status )
				return rlen - len;
			code = (*s->procs.read_buf)(s);
			if ( code < 0 || sendbufp(s) )
				return rlen - len;
			continue;
		   }
		if ( count > len ) count = len;
		memcpy(str, s->cptr + 1, count);
		s->cptr += count;
		str += count;
		len -= count;
	   }
	return rlen;
}

/* Write a string on a stream. */
/* Return the number of bytes written. */
uint
sputs(register stream *s, const byte *str, uint wlen)
{	uint len = wlen;
	if ( wlen > s->bsize && s->procs.write_buf == s_file_write_buf )
	   {	/* Write directly on the file. */
		uint write_count;
		(*s->procs.write_buf)(s);
		write_count = fwrite(str, 1, wlen, s->file);
		if ( s->position >= 0 )		/* file is positionable */
			s->position = ftell(s->file);
		return write_count;
	   }
	while ( len > 0 )
	   {	uint count = sbufavailable(s);
		if ( count > 0 )
		   {	if ( count > len ) count = len;
			memcpy(s->cptr + 1, str, count);
			s->cptr += count;
			str += count;
			len -= count;
		   }
		else
		   {	byte ch = *str++;
			sputc(s, ch);
			if ( s->end_status ) return wlen - len;
			len--;
		   }
	   }
	return wlen;
}

/* Read a hex string from a stream. */
/* Answer EOFC if we reached end-of-file before filling the string, */
/* 0 if we filled the string first, or ERRC on error. */
/* s->odd should be -1 initially: */
/* if an odd number of hex digits was read, s->odd is set to */
/* the odd digit value, otherwise s->odd is set to -1. */
/* If ignore_garbage is true, characters other than hex digits are ignored; */
/* if ignore_garbage is false, characters other than hex digits or */
/* whitespace return an error. */
int
sreadhex(stream *s, byte *str, uint rlen, uint *nread,
  int *odd_digit, int ignore_garbage)
{	byte *ptr = str;
	byte *limit = ptr + rlen;
	byte val1 = (byte)*odd_digit;
	byte val2;
	byte save_last;
	register byte _ds *decoder = scan_char_decoder;
	register byte *sptr, *endp;
	int ch;
	int code;
#define begin_inline(sptr, endp, s)\
  (sptr = s->cptr, endp = s->endptr - 1)
#define sgetc_inline(sptr, endp, s)\
  (sptr <= endp ? *++sptr :\
   (end_inline(sptr, s), ch = spgetc(s), begin_inline(sptr, endp, s), ch))
#define end_inline(sptr, s)\
  (s->cptr = sptr)
	if ( rlen == 0 )
	   {	*nread = 0;
		return 0;
	   }
	begin_inline(sptr, endp, s);
	if ( val1 <= 0xf ) goto d2;
d1:	/* Fast check for common case */
	if ( sptr >= endp ) goto x1;	/* no last char to save */
	save_last = *endp;
	*endp = ' ';			/* force exit from fast loop */
f1:	if ( (val1 = decoder[sptr[1]]) <= 0xf &&
	     (val2 = decoder[sptr[2]]) <= 0xf
	   )
	   {	sptr += 2;
		*ptr++ = (val1 << 4) + val2;
		if ( ptr < limit ) goto f1;
		*endp = save_last;
		goto px;
	   }
	*endp = save_last;
x1:	while ( (val1 = decoder[sgetc_inline(sptr, endp, s)]) > 0xf )
	   {	if ( val1 == ctype_eof )
		   {	code = ch; *odd_digit = -1; goto ended;	}
		else if ( val1 != ctype_space && !ignore_garbage )
		   {	sptr--; *odd_digit = -1; goto err;	}
	   }
d2:	while ( (val2 = decoder[sgetc_inline(sptr, endp, s)]) > 0xf )
	   {	if ( val2 == ctype_eof )
		   {	code = ch; *odd_digit = val1; goto ended;	}
		else if ( val2 != ctype_space && !ignore_garbage )
		   {	sptr--; *odd_digit = val1; goto err;	}
	   }
	*ptr++ = (val1 << 4) + val2;
	if ( ptr < limit ) goto d1;
px:	*nread = rlen;
	end_inline(sptr, s);
	return 0;
err:	code = ERRC;
ended:	*nread = ptr - str;
	end_inline(sptr, s);
	return code;
}

/* ------ String streams ------ */

/* Initialize a stream for reading a string. */
void
sread_string(register stream *s, byte *ptr, uint len)
{	static stream_procs p =
	   {	s_string_available, s_string_seek, s_std_null, s_std_null,
		s_string_read_buf, NULL
	   };
	s_string_init(s, ptr, len, &p);
	s->mode = s_mode_read;
}
/* Handle end-of-buffer when reading from a string. */
private int
s_string_read_buf(stream *s)
{	s->cptr = s->endptr;
	s->end_status = EOFC;
	return EOFC;
}
/* Return the number of available bytes when reading from a string. */
private int
s_string_available(stream *s, long *pl)
{	*pl = sbufavailable(s);
	if ( *pl == 0 ) *pl = -1;	/* EOF */
	return 0;
}

/* Initialize a stream for writing a string. */
void
swrite_string(register stream *s, byte *ptr, uint len)
{	static stream_procs p =
	   {	s_std_noavailable, s_string_seek, s_std_null, s_std_null,
		NULL, s_string_write_buf
	   };
	s_string_init(s, ptr, len, &p);
	s->mode = s_mode_write;
}
/* Handle end-of-buffer when writing a string. */
private int
s_string_write_buf(stream *s)
{	s->cptr = s->endptr;
	s->end_status = EOFC;
	return EOFC;
}

/* Seek in a string.  Return 0 if OK, ERRC if not. */
private int
s_string_seek(register stream *s, long pos)
{	if ( pos < 0 || pos > s->bsize ) return ERRC;
	s->cptr = s->cbuf + pos - 1;
	return 0;
}

/* Private initialization */
private void
s_string_init(register stream *s, byte *ptr, uint len, stream_procs *p)
{	s_std_init(s, ptr, len, p, s_mode_write);
	s->end_status = EOFC;		/* this is all there is */
	s->position = 0;
	s->file = 0;			/* not a file stream */
}

/* ------ File streams ------ */

/* Initialize a stream for reading an OS file. */
void
sread_file(register stream *s, FILE *file, byte *buf, uint len)
{	static stream_procs p =
	   {	s_file_available, s_file_read_seek,
		s_std_null, s_file_read_close,
		s_file_read_buf, NULL
	   };
	s_std_init(s, buf, len, &p, s_mode_read);
	s->file = file;
	s->position = (file == stdin ? -1 : 0);
}
/* Procedures for reading from a file */
private int
s_file_read_buf(register stream *s)
{	int nread;
	if ( s->position >= 0 )		/* file is positionable */
		s->position = ftell(s->file);
	nread = fread(s->cbuf, 1, s->bsize, s->file);
	s->cptr = s->cbuf - 1;
	s->end_status = (ferror(s->file) ? ERRC : feof(s->file) ? EOFC : 0);
	if ( nread <= 0 ) nread = 0;
	s->endptr = s->cptr + nread;
	return 0;
}
private int
s_file_available(register stream *s, long *pl)
{	*pl = sbufavailable(s);
	if ( sseekable(s) )
	   {	long pos, end;
		pos = ftell(s->file);
		if ( fseek(s->file, 0L, 2) ) return ERRC;
		end = ftell(s->file);
		if ( fseek(s->file, pos, 0) ) return ERRC;
		*pl += end - pos;
		if ( *pl == 0 ) *pl = -1;	/* EOF */
	   }
	else
	   {	if ( *pl == 0 && feof(s->file) ) *pl = -1;	/* EOF */
	   }
	return 0;
}
private int
s_file_read_seek(register stream *s, long pos)
{	uint end = s->endptr - s->cbuf + 1;
	long offset = pos - s->position;
	if ( offset >= 0 && offset <= end )
	   {	/* Staying within the same buffer */
		s->cptr = s->cbuf + offset - 1;
		return 0;
	   }
	if ( fseek(s->file, pos, 0) != 0 )
		return ERRC;
	s->endptr = s->cptr = s->cbuf - 1;
	s->end_status = 0;
	return 0;
}
private int
s_file_read_close(stream *s)
{	return fclose(s->file);
}

/* Initialize a stream for writing an OS file. */
void
swrite_file(register stream *s, FILE *file, byte *buf, uint len)
{	static stream_procs p =
	   {	s_std_noavailable, s_file_write_seek,
		s_file_write_flush, s_file_write_close,
		NULL, s_file_write_buf
	   };
	s_std_init(s, buf, len, &p, s_mode_write);
	s->file = file;
	s->position = (file == stdout || file == stderr ? -1 : 0);
   }
/* Procedures for writing on a file */
private int
s_file_write_buf(register stream *s)
{	uint count = s->cptr + 1 - s->cbuf;
	uint write_count = fwrite(s->cbuf, 1, count, s->file);
	if ( s->position >= 0 )		/* file is positionable */
		s->position = ftell(s->file);
	s->cptr = s->cbuf - 1;
	if ( write_count != count )
	   {	s->end_status = (ferror(s->file) ? ERRC : EOFC);
		s->endptr = s->cptr;
	   }
	else
		s->endptr = s->cptr + s->bsize;
	return 0;
}
private int
s_file_write_seek(stream *s, long pos)
{	/* Output files are not positionable */
	return ERRC;
}
private int
s_file_write_flush(register stream *s)
{	int result = s_file_write_buf(s);
	fflush(s->file);
	return result;
}
private int
s_file_write_close(register stream *s)
{	s_file_write_buf(s);
	return fclose(s->file);
}
