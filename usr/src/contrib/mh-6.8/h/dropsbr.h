/* dropsbr.h - definitions for maildrop-style files */


/* A file which is formatted like a maildrop may have a corresponding map
   file which is an index to the bounds of each message.  The first record
   of such an map is special, it contains:

	d_id    = number of messages in file
	d_size	= version number of map
	d_start = last message read
	d_stop  = size of file

    Each record after that contains:

	d_id	= BBoard-ID: of message, or similar info
	d_size	= size of message in ARPA Internet octets (\n == 2 octets)
	d_start	= starting position of message in file
	d_stop	= stopping position of message in file

   Note that d_st{art,op} do NOT include the message delimiters, so
   programs using the map can simply fseek to d_start and keep reading
   until the position is at d_stop.
 */

#define	DRVRSN	3

struct drop {
    int     d_id;
    int	    d_size;
    long    d_start;
    long    d_stop;
};


int	map_chk (), map_read (), map_write ();
char   *map_name ();

int	mbx_mmdf (), mbx_uucp ();
int	mbx_open (), mbx_Xopen (), mbx_copy (), mbx_size (), mbx_close ();
int	mbx_read (), mbx_write ();
