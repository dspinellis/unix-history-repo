# define FL_FFC 0x200  /* floppy function complete */
# define FL_ERR 0x80  /* error bit in floppy status byte */
# define FL_PERR  0x905  /* Floppy protocol error */
# define FL_DATA  0x100  /*  floppy data select code */
# define FL_RS 0x900  /* floppy read sector command */
# define FL_WS  0x901  /*  floppy write sector command */
# define FL_STAT  0x902  /* floppy get status command */
/*		*/
# define BLKSIZ 512  /*  tape block size */
# define RXFTRK  77  /* no. tracks/floppy */
# define RXSTRK  26  /*  no. sectors/track */
# define RXBYSEC  128  /*  no. bytes/sector  */
# define MAXSEC (RXFTRK*RXSTRK)  /* no. sectors/floppy */
