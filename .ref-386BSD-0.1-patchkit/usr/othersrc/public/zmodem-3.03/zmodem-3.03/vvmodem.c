/*
 *  VMODEM
 *  VMS support for UMODEM and vvrb/vvsb programs
 *
 *	Defined herein are some utility routines to make the UNIX
 *	program UMODEM run under VAX/VMS C:
 *
 *		assign_channel	Calls the VMS System Service $ASSIGN
 *				to assign a channel to a device.
 *				The routine currently has the device
 *				"TT" hardwired into it.
 *		gtty		Gets terminal characteristics, almost
 *				like the UNIX GTTY system call.
 *		raw_read	Reads characters from the terminal
 *				without any echoing or interpretation
 *				and with an optional timeout period.
 *		raw_write	Writes a character to the terminal
 *				without any interpretation.
 *		raw_wbuf	Writes a buffer to the terminal
 *				without any interpretation.
 *		stty		Sets terminal characteristics, almost
 *				like the UNIX STTY system call.
 *
 *	Some of the ideas used here were obtained from code written
 *	by Max Benson and Robert Bruccoleri.
 *
 *  Walter Reiher
 *  Harvard University
 *  Department of Chemistry
 *  12 Oxford Street
 *  Cambridge, MA 02138
 *  March 11, 1983
 *
 *  Modified 4-20-88 Chuck Forsberg, Omen Technology INC
 *  17505-V NW Sauvie IS RD Portland OR 97231 omen!caf
 *   Added primitives for for ZMODEM use.
 */
#include descrip
#include iodef
#include rms
#include ssdef
#include stdio
#include "vmodem.h"

#define  TRUE	1
#define  FALSE	0

static char	tt_name[]	= "TT";
static short	tt_chan		= -1;		/*  Terminal channel number  */

struct	tt_io_iosb				/*  Terminal I/O IOSB  */
{
	short	status;
	short	byte_count;
	short	terminator;
	short	terminator_size;
};


struct	tt_io_tacf				/* Terminal I/O type ahead */
{
	short	byte_count;
	char	firstchar;
	char	something;
	short	whatever;
};

/*
 *	Terminator mask for PASSALL reads.
 *	Permits reads of all possible 8-bit characters.
 */
int	t_mask[32] =  {
	0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
	0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
	0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
	0,  0	};

struct	terminator_mask {
	short	size ;
	short	unused ;
	int	*mask ;
}

termin_mask	= { 32, 0, t_mask };

/*
 *	ASSIGN a channel to the logical name TT, which is usually
 *	the terminal.
 */
assign_channel()
{
	int	status;
	$DESCRIPTOR(tt_descriptor, tt_name);

	if (tt_chan == -1)
		status	= sys$assign(&tt_descriptor, &tt_chan, 0, 0);
	else
		status	= SS$_NORMAL;

	if (status != SS$_NORMAL || tt_chan == -1)
		fatal("ASSIGN_CHANNEL:  error in SYS$ASSIGN\n");

	return;
}

/*
 *	Gets terminal information from VMS.
 */
gtty(tt_characteristics)
struct	tt_info	*tt_characteristics;
{
	int c;
	int				status;
	extern unsigned Baudrate;
	int speeds[] = { 2400, 50, 75, 110, 134, 150, 300, 600, 1200, 1800,
	  2000, 2400, 3600, 4800, 7200, 9600, 19200 };

	if (tt_chan == -1)
		assign_channel();

	status	= sys$qiow(0, tt_chan, IO$_SENSEMODE,
	  &(tt_characteristics->dev_modes), NULL, 0,
	  &(tt_characteristics->dev_characteristics), 12,
	  0, 0, 0, 0);
	if (status != SS$_NORMAL ||
	  tt_characteristics->dev_modes.status != SS$_NORMAL)
		fatal("GTTY:  sense mode QIO error return.\n");

	c = tt_characteristics->dev_modes.t_speed % 17;
	Baudrate = speeds[c];

	return(status);
}

/*
 *	Return the number of characters waiting in TTY input buffer
 */
rdchk()
{
	int		status;
	struct	tt_io_iosb	iosb;
	struct	tt_io_tacf typeahead;

	status	= sys$qiow(0, tt_chan, IO$_SENSEMODE|IO$M_TYPEAHDCNT,
	  &iosb, NULL, 0,
	  &typeahead, 0,
	  0, 0, 0, 0);

	return(typeahead.byte_count);
}

/*
 *	Read NCHAR characters from the terminal without echoing or
 *	interpretation.
 *	If the argument SECONDS is non-zero, use that as the
 *	timeout period in seconds for the read.
 *
 *	Returns SS$_TIMEOUT in case of timeout or other error.
 *	  Otherwise, returns the number of characters read.
 */
raw_read(nchar, charbuf, seconds)
char		*charbuf;
int		nchar;
unsigned	seconds;
{
	short			function;
	int			status;
	struct	tt_io_iosb	iosb;

	if (tt_chan == -1)
		assign_channel();

	function	= IO$_READVBLK | IO$M_NOECHO | IO$M_NOFILTR;

	if (seconds)
		status	= sys$qiow(0, tt_chan, function | IO$M_TIMED,
		  &iosb, NULL, 0,
		  charbuf, nchar, seconds,
		  &termin_mask, NULL, 0);
	else
		status	= sys$qiow(0, tt_chan, function,
		  &iosb, NULL, 0,
		  charbuf, nchar, 0,
		  &termin_mask, NULL, 0);

	if (iosb.byte_count)
		return iosb.byte_count;
	return SS$_TIMEOUT;
}

/*
 *	Writes a character to the terminal without echoing or
 *	interpretation.
 */
raw_write(c)
char	c;
{
	int			status;
	struct	tt_io_iosb	iosb;

	if (tt_chan == -1)
		assign_channel();

	status	= sys$qiow(0, tt_chan,
	  IO$_WRITEVBLK | IO$M_CANCTRLO | IO$M_NOFORMAT,
	  &iosb, NULL, 0,
	  &c, 1, 0, 0, 0, 0);

	if (status != SS$_NORMAL || iosb.status != SS$_NORMAL)
		fatal("RAW_WRITE:  write QIO error return.\n");

	return;
}

/*
 *	Writes a buffer to the terminal without echoing or
 *	interpretation.
 */
raw_wbuf(nchar, charbuf)
char		*charbuf;
int		nchar;
{
	int			status;
	struct	tt_io_iosb	iosb;

	if (tt_chan == -1)
		assign_channel();

	status	= sys$qiow(0, tt_chan,
	  IO$_WRITEVBLK | IO$M_CANCTRLO | IO$M_NOFORMAT,
	  &iosb, NULL, 0,
	  charbuf, nchar, 0, 0, 0, 0);

	if (status != SS$_NORMAL || iosb.status != SS$_NORMAL)
		fatal("RAW_WRITE:  write QIO error return.\n");

	return;
}

/*
 *  Sets terminal information from VMS.
 *	 Modified 12-85 Larry Farr/Chuck Forsberg to not use
 *	 bad parity returned by VMS 4.
 */
stty(tt_characteristics)
struct	tt_info	*tt_characteristics;
{
	short			*f_ptr, /* *p_ptr, */ *s_ptr;
	int			status;
	struct	tt_mode_iosb	iosb;

	if (tt_chan == -1)
		assign_channel();

/*
 *	We do the following in order to get a full short, concatenating
 *	two adjacent chars:
 */
	s_ptr	= &(tt_characteristics->dev_modes.t_speed);	/*  Speeds  */
	f_ptr	= &(tt_characteristics->dev_modes.CR_fill);	/*  Fills  */
	/* p_ptr	= &(tt_characteristics->dev_modes.parity_flags); */

	status	= sys$qiow(0, tt_chan, IO$_SETMODE,
	  &iosb, NULL, 0,
	  &(tt_characteristics->dev_characteristics), 12,
	  /* *s_ptr, *f_ptr, *p_ptr, 0);	*/
	  *s_ptr, *f_ptr, 0, 0);
	if (status != SS$_NORMAL || iosb.status != SS$_NORMAL)
		printf("STTY:  set mode QIO returned %d\n", status);

	return(status);
}

