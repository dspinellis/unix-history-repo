/*
 *  solKbd.h --
 *
 *	remade by A.Fujita, DEC-20-1992
 */

#include <luna68k/dev/kbio.h>

#define  XK_KATAKANA
#include "keysym.h"


#define KS_KANA		0x1
#define KS_CTRL_L	0x2
#define KS_CTRL_R	0x4
#define KS_META_L	0x8
#define KS_META_R	0x10
#define KS_ALT_L	0x20
#define KS_ALT_R	0x40
#define KS_SUPER_L	0x80
#define KS_SUPER_R	0x100
#define KS_HYPER_L	0x200
#define KS_HYPER_R	0x400

#define KANA_KEY	11
#define CAPSLOCK_KEY	14

#define AREPBUFSZ		32



extern int  solKbdProc();
extern int  solMouseProc();

#define MAXEVENTS	1024

#define KEYCODE_TB_SIZE	128

typedef	struct	_solDevPrvRec {
	/* device control */
	int fd;
	int kbd_type;
	int key_state;		/* kana key status */
	int flags;
	int num;

	/* ascii control */
	int offset;
	KeybdCtrl keybdCtrl;
	char semiEncodeDef[KEYCODE_TB_SIZE];
	char semiEncode[KEYCODE_TB_SIZE];
	KeyCode minkey,maxkey; 

	/* mouse control */
	int mouse_state;

}	solDevPrv,	*solDevPrvPtr;


typedef	struct	_solDevEvtRec {
	u_int	type;
	union {
		u_int	p;
		u_char	key;
		struct {
			u_char	state;
			char	x_delta;
			char	y_delta;
		}	ms;
	} u;
}	solDevEvt, *solDevEvtPtr;

#define key_code	u.key
#define	ms_state	u.ms.state
#define	ms_x_delta	u.ms.x_delta
#define	ms_y_delta	u.ms.y_delta


#define	EvtKey		1
#define	EvtMouse	2

#define	BUTTON_L	4
#define	BUTTON_M	2
#define	BUTTON_R	1
