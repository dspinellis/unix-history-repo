/*
 *  VMODEM.H
 *  VMS support for UMODEM program
 *
 *	#INCLUDE files defining structures associated with terminal
 *	information structure TT_INFO.
 *	Information about the terminal is passed around in UMODEM in a
 *	STRUCT TT_INFO.
 *
 *  Walter Reiher
 *  Harvard University
 *  Department of Chemistry
 *  12 Oxford Street
 *  Cambridge, MA 02138
 *  March 10, 1983
 */

struct	tt_mode				/*  Info for a IO$_SETMODE call  */
{
	char			class;
	char			type;
	short			page_width;
	char			bcharacteristics[3];
	char			page_length;
	int			echaracteristics;
};

struct	tt_mode_iosb			/*  Terminal IO$_SENSEMODE IOSB  */
{
	short			status;
	char			t_speed;
	char			r_speed;
	char			CR_fill;
	char			LF_fill;
	char			parity_flags;
	char			unused2;
};

struct	tt_info				/*  Summary of terminal infomation  */
{
	struct	tt_mode		dev_characteristics;
	struct	tt_mode_iosb	dev_modes;
};
