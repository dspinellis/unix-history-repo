/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

#define putchar(c)	putc(c, stdout)
#define putc(c, fp)	(--(fp)->f_cnt >= 0 ? (*(fp)->f_ptr++ = (c)) : _flush((c), fp))
#define getc(fp)	(((--(fp)->f_cnt < 0) ? filbuf(fp) : *(fp)->f_ptr++))

typedef struct {
	int	f_cnt,		/* number of characters left in buffer */
		f_bufsize,	/* size of what f_base points to */
		f_fd,		/* fildes */
		f_flags;	/* various flags */
	char	*f_ptr,		/* current offset */
		*f_base;	/* pointer to base */
	char	*f_name;	/* name of open file */
} File;

#define F_READ		01
#define F_WRITE		02
#define F_APPEND	04
#define F_MODE(x)	(x&07)
#define F_EOF		010
#define F_STRING	020
#define F_ERR		040
#define F_LOCK		0100	/* don't close this file upon error */
#define F_MYBUF		0200	/* f_alloc allocated the buffer, so
				   f_close knows to free it up */

extern long	io_chars;
extern int	io_lines;

extern File
	*stdout,
	*mru_file,

	*open_file(),
	*fd_open(),
	*f_open();
