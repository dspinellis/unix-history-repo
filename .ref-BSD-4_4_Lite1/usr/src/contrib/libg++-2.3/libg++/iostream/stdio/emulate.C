//    This is part of the iostream library, providing input/output for C++.
//    Copyright (C) 1991 Per Bothner.
//
//    This library is free software; you can redistribute it and/or
//    modify it under the terms of the GNU Library General Public
//    License as published by the Free Software Foundation; either
//    version 2 of the License, or (at your option) any later version.
//
//    This library is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//    Library General Public License for more details.
//
//    You should have received a copy of the GNU Library General Public
//    License along with this library; if not, write to the Free
//    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

#include <stdioprivate.h>

// Handle GNU libc getc.
// Assume __bufp has the same offset as _gptr,
// and __get_limit has the same offset as _egptr.

extern "C" int __fillbf(FILE* fp)
{
    return ((streambuf*)fp)->sbumpc();
}

// Handle GNU libc putc.
// Assumes that __bufp >= __put_limit always, so __flshfp is
// called on every putc. We ensure __bufp >= __put_limit
// by requiring gptr() >= eback(), and giving _gptr the
// same offset as __bufp and _eback the same offset as __put_limit.
// We must also make sure that __linebuf is 0.
// This is taken care of if _filebuf->_fake is 0 (assuming standard struct layouts).

extern "C" int __flshfp(FILE *fp, int ch)
{
    return ((streambuf*)fp)->sputc(ch);
}

extern "C" void __libc_fatal(const char *msg) {abort();}

struct _iobuf {
    int _cnt;
    unsigned char *_ptr;
    unsigned char *_base;
    int _bufsiz;
    short int _flag;
    char _file;
};

typedef struct {
    int _magic;
    FILE *stream;
} forward_fp;
struct fake_iobuf {
    struct forward_fp forward;
    char dummy[sizeof(struct _iobuf)-sizeof(forward_fp)];
};

fake_iobuf _iob[3] = {
    { _OLD_STDIO_MAGIC, stdin },
    { _OLD_STDIO_MAGIC, stdout },
    { _OLD_STDIO_MAGIC, stderr }
};

extern "C" int _filbuf(forward_fp *fp)
{
    // First, compensate for getc's decrement.
    if (++fp->_magic == _OLD_STDIO_MAGIC)
	return getc(fp->stream);
    else
	return getc((FILE*)fp);
}

extern "C" int _flsbuf(int c, forward_fp* fp)
{
    // First, compensate for getc's decrement.
    if (++fp->_magic == _OLD_STDIO_MAGIC)
	return putc(c, fp->stream);
    else
	return putc(c, (FILE*)fp);
}

void __emulate_dummy()
{
    // These are in case someone calls a libc routine that calls these.
    fclose(fopen("foo","r"));
}
