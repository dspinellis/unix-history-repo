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

/* stream.h */
/* Definitions for Ghostscript stream package */
/* Requires stdio.h */

/*
 * Note that the stream package works with bytes, not chars.
 * This is to ensure unsigned representation on all systems.
 * A stream currently can only be read or written, not both.
 * Note also that the read procedure returns an int,
 * not a char or a byte, so we can use negative values for EOFC and ERRC.
 * We distinguish "data" from "signal" results (EOFC, ERRC) with a macro:
 */
#define char_is_data(c) ((c) >= 0)
#define char_is_signal(c) ((c) < 0)
typedef struct stream_s stream;
typedef struct {

		/* Store # available for reading. */
		/* Return 0 if OK, ERRC if error or not implemented. */
	int (*available)(P2(stream *, long *));

		/* Set position. */
		/* Return 0 if OK, ERRC if error or not implemented. */
	int (*seek)(P2(stream *, long));

		/* Flush buffered data. */
		/* Return 0 if OK, ERRC if error. */
	int (*flush)(P1(stream *));

		/* Flush data (if writing) & close stream. */
		/* Return 0 if OK, ERRC if error. */
	int (*close)(P1(stream *));

		/* Refill buffer and reset cptr. */
		/* Return ERRC if not implemented; */
		/* otherwise, set end_status appropriately and return 0. */
	int (*read_buf)(P1(stream *));

		/* Write buffer, reset cptr. */
		/* Return 0 if OK, ERRC if error or not implemented. */
	int (*write_buf)(P1(stream *));

} stream_procs;
/* Structs for specialized streams -- see below. */
struct lzw_decode_table_s;
struct lzw_encode_table_s;
struct stream_s {
	byte *cptr;			/* pointer to last byte */
					/* read or written */
	byte *endptr;			/* pointer to last byte */
					/* containing data for reading, */
					/* or to be filled for writing */
	byte *cbuf;			/* base of buffer */
	uint bsize;			/* size of buffer, 0 if closed */
	uint cbsize;			/* size of buffer */
	char mode;			/* 2 if reading, 1 if writing */
#define s_mode_read 2
#define s_mode_write 1
#define s_is_valid(s) ((s)->mode != 0)
#define s_is_reading(s) ((s)->mode == s_mode_read)
#define s_is_writing(s) ((s)->mode == s_mode_write)
	int end_status;			/* EOFC if at EOF when buffer */
					/* becomes empty, ERRC if error */
	long position;			/* file position of beginning of */
					/* buffer, -1 means not seekable */
	stream_procs procs;
	int num_format;			/* format for Level 2 */
					/* encoded number reader */
					/* (only used locally) */
	/* strm is non-zero iff this is a filter stream. */
	stream *strm;			/* the underlying stream */
	int strm_is_temp;		/* if true, strm is a temporary */
					/* stream and should be freed */
					/* when this stream is closed */
	/*
	 * If were were able to program in a real object-oriented style, 
	 * the remaining data would be per-subclass.  It's just too much
	 * of a nuisance to do this in C, so we allocate space for the
	 * private data of ALL subclasses.
	 */
	/* The following are for file streams. */
	FILE *file;			/* file handle for C library */
	int can_close;			/* 0 for stdin/out/err, */
					/* -1 for line/statementedit, */
					/* 1 for other files */
	stream *prev, *next;		/* keep track of all files */
	/* The following is used by several decoding filters. */
	int odd;			/* odd digit */
	/* The following are for RunLengthEncode filters. */
	ulong record_size;
	/* The following is for RunLengthEncode and PFBDecode. */
	ulong record_left;		/* bytes left in current record */
	/* The following are for PFBDecode. */
	int record_type;
	/* The following are for eexec streams. */
	ushort cstate;			/* encryption state */
	int binary;			/* true=binary, false=hex */
	/* The following are for LZW encoding/decoding streams. */
	int enhanced;			/* if true, use Aladdin's */
					/* enhanced compression algorithm */
					/* (Patent Pending) */
	byte bits;		/* most recent byte of input or */
				/* current byte of output */
	int bits_left;		/* # of unused low bits in above, [0..7] */
	struct lzw_decode_table_s *decode_table;	/* decoding table */
	struct lzw_encode_table_s *encode_table;	/* encoding table */
	uint next_code;			/* next code to be assigned */
	int code_size;			/* current # of bits per code */
	int prev_code;			/* previous code recognized */
					/* or assigned */
};

/* Stream functions.  Some of these are macros -- beware. */
/* Also note that unlike the C stream library, */
/* ALL stream procedures take the stream as the first argument. */
#define sendbufp(s) ((s)->cptr >= (s)->endptr)	/* not for clients */

/* Following are valid for all streams. */
/* flush is a no-op for read streams. */
/* close is NOT a no-op for non-file streams -- */
/* it actively disables them. */
/* The close routine must do a flush if needed. */
#define sseekable(s) ((s)->position >= 0)
#define serrorp(s) ((s)->cptr >= (s)->endptr && (s)->end_status == ERRC)
#define savailable(s,pl) (*(s)->procs.available)(s,pl)
#define sflush(s) (*(s)->procs.flush)(s)
#define sclose(s) (*(s)->procs.close)(s)

/* Following are only valid for read streams. */
extern int spgetc(P1(stream *));
#define sgetc(s) (!sendbufp(s) ? *++((s)->cptr) : spgetc(s))
extern uint sgets(P3(stream *, byte *, uint));
extern int sreadhex(P6(stream *, byte *, uint, uint *, int *, int));
extern int sungetc(P2(stream *, byte));	/* ERRC on error, 0 if OK */
#define sputback(s) ((s)->cptr--)
#define seofp(s) (sendbufp(s) && (s)->end_status == EOFC)

/* Following are only valid for write streams. */
extern int spputc(P2(stream *, byte));
#define sputc(s,c)\
  (!sendbufp(s) ? ((int)(*++((s)->cptr)=(c))) : spputc((s),(c)))
extern uint sputs(P3(stream *, const byte *, uint));

/* Following are only valid for positionable streams. */
#define stell(s) ((s)->cptr + 1 - (s)->cbuf + (s)->position)
#define sseek(s,pos) (*(s)->procs.seek)(s,(long)(pos))

/* Following are for high-performance clients. */
/* bufptr points to the next item, bufend points beyond the last item. */
#define sbufptr(s) ((s)->cptr + 1)
#define sbufend(s) ((s)->endptr + 1)
#define ssetbufptr(s,ptr) ((s)->cptr = (ptr) - 1)
#define sbufavailable(s) ((s)->endptr - (s)->cptr)

/* We cast EOFC and ERRC to int explicitly, because some compilers */
/* don't do this if the other arm of a conditional is a byte. */
/* Clients should use char_is_data and char_is_signal (see above) */
/* to test for exceptional results. */
#define EOFC ((int)(-1))
#define ERRC ((int)(-2))
/****** ERRC IS NOT RECOGNIZED IN MOST PLACES YET ******/

/* Stream creation procedures */
extern	void	sread_string(P3(stream *, byte *, uint)),
		swrite_string(P3(stream *, byte *, uint));
extern	void	sread_file(P4(stream *, FILE *, byte *, uint)),
		swrite_file(P4(stream *, FILE *, byte *, uint));

/* Standard stream initialization */
extern	void	s_std_init(P5(stream *, byte *, uint, stream_procs *, int /*mode*/));
/* Standard stream finalization */
extern	void	s_disable(P1(stream *));
/* Generic stream procedures exported for filters */
extern	int	s_std_null(P1(stream *)),
		s_std_noavailable(P2(stream *, long *)),
		s_std_close(P1(stream *));
