/************************************************************
Copyright (c) 1992 by Hewlett-Packard Company, Palo Alto, California.

			All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of Hewlett-Packard not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

HEWLETT-PACKARD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
HEWLETT-PACKARD BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

********************************************************/

/***************************************************************************
 *
 * Constants and structs for dynamically loaded serial input device drivers.
 *
 */

#ifndef _X_SERIALDRV_H_
#ifndef hp9000
#include <dl.h>
#endif

#define MIN_KEYCODE		8
#define MAX_NM			64
#define DATA_IS_8_BITS		0x01
#define DATA_IS_16_BITS		0x02
#define DATA_IS_32_BITS		0x04
#define REPORTS_PROXIMITY	0x08
#define	ABSOLUTE_DATA		0x040
#define NON_CONTIGUOUS_DATA	0x080
#define FALSE			0
#define TRUE 			1
#define KEY_DATA 		0x01
#define BUTTON_DATA 		0x02
#define PROXIMITY_DATA 		0x04
#define MOTION_DATA 		0x08
#define INIT_SUCCESS		0
#define INIT_FAILURE		1
#define READ_SUCCESS		0
#define READ_FAILURE		1
#define WRITE_SUCCESS		0
#define WRITE_FAILURE		1
#define CLOSE_SUCCESS		0
#define IN_PROXIMITY		0
#define OUT_OF_PROXIMITY	1

#define _XSetDeviceMode		0
#define _XSetDeviceValuators	1
#define _XChangeDeviceControl	2
#define _XChangeFeedbackControl	3

typedef struct {
	int	class;
	int	led_mask;
	int	led_values;
} HPLedFeedbackControl;

typedef struct {
	int	*valuators;
	int	first_valuator;
	int	num_valuators;
} HPValuatorControl;

typedef struct {
	int	*resolutions;
	int	first_valuator;
	int	num_valuators;
} HPResolutionControl;

typedef int (*pfrb)();
typedef int (*ConfigureProc)();
typedef int (*InitProc)();
typedef int (*ReadProc)();
typedef int (*WriteProc)();
typedef int (*CloseProc)();

typedef struct _SerialProcs
    {
    char		*x_name;	/* filled in by driver		*/
    ConfigureProc	configure;	/* filled in by driver		*/
    ReadProc		read;		/* filled in by driver		*/
    WriteProc		write;		/* filled in by driver		*/
    CloseProc		close;		/* filled in by driver		*/
#ifndef hp9000
    shl_t		ldr_module_id;	/* filled in by X server	*/
#endif
    int			fd;		/* filled in by X server	*/
    char		driver_name[MAX_NM];/* filled in by X server	*/
    } SerialProcs; 

typedef struct _HPInputDeviceHeader
    {
    char	path[MAX_NM];	/* device path - filled in by X server  */
    int		resolution;	/* resolution in counts/cm         	*/
    int         max_x;        	/* maximum x value in counts    	*/
    int         max_y;          /* maximum y value in counts    	*/
    int         file_ds;        /* file descriptor              	*/
    u_char	flags; 		/* device characteristics		*/
    u_char	ax_num;		/* number of axes			*/
    u_char	num_buttons;   	/* number of buttons         		*/
    u_char 	num_keys; 	/* number of keys            		*/
    u_char 	min_kcode;	/* minimum keycode           		*/
    u_char 	max_kcode;	/* maximum keycode           		*/
    u_char 	num_leds;     	/* number of leds               	*/
    }HPInputDeviceHeader;
#endif /* _X_SERIALDRV_H_ */
