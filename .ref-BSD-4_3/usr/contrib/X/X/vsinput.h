#include <X/mit-copyright.h>

/* Copyright Massachusetts Institute of Technology 1985 */

/* $Header: vsinput.h,v 10.6 86/02/01 15:17:30 tony Rel $ */
/*
 * Event queue entries
 */

# ifndef _XINPUT_
# define _XINPUT_

typedef struct  _vs_event {
        u_short vse_x;          /* x position */
        u_short vse_y;          /* y position */
        u_short vse_time;       /* 10 millisecond units (button only) */
        char    vse_type;       /* button or motion? */
        u_char  vse_key;        /* the key (button only) */
        char    vse_direction;  /* which direction (button only) */
        char    vse_device;     /* which device (button only) */
} vsEvent;

/* vse_type field */
#define VSE_BUTTON      0               /* button moved */
#define VSE_MMOTION     1               /* mouse moved */
#define VSE_TMOTION     2               /* tablet moved */

/* vse_direction field */
#define VSE_KBTUP       0               /* up */
#define VSE_KBTDOWN     1               /* down */
#define VSE_KBTRAW	2		/* undetermined */

/* vse_device field */
#define VSE_MOUSE       1               /* mouse */
#define VSE_DKB         2               /* main keyboard */
#define VSE_TABLET      3               /* graphics tablet */
#define VSE_AUX         4               /* auxiliary */
#define VSE_CONSOLE     5               /* console */

/* The event queue */

typedef struct _vs_eventqueue {
	vsEvent *events;	/* input event buffer */
	int size;		/* size of event buffer */
	int head;		/* index into events */
	int tail;		/* index into events */
} vsEventQueue;

/* mouse cursor position */

typedef struct _vs_cursor {
        short x;
        short y;
} vsCursor;

/* mouse motion rectangle */

typedef struct _vs_box {
        short bottom;
        short right;
        short left;
        short top;
} vsBox;

# endif _XINPUT_
