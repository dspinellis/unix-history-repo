#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

/* This file is device dependent, but is common to several devices */

#ifndef lint
static char *rcsid_lk201_c = "$Header: lk201.c,v 10.6 86/02/01 15:16:27 tony Rel $";
#endif

#include <sys/types.h>
#include "vsinput.h"

#define KEYDOWN_ERROR	0x3d
#define POWERUP_ERROR 	0x3e
#define BASEKEY		0x41
#define MINSPECIAL	0xb3
#define ALLUPS		0xb3
#define METRONOME	0xb4
#define OUTPUT_ERROR	0xb5
#define INPUT_ERROR 	0xb6
#define MAXSPECIAL	0xba

static u_char lastkey;

#define NUMDIVS 14
static u_char divbeg[NUMDIVS] = {0xbf, 0x91, 0xbc, 0xbd, 0xb0, 0xad, 0xa6,
				 0xa9, 0x88, 0x56, 0x63, 0x6f, 0x7b, 0x7e};
static u_char divend[NUMDIVS] = {0xff, 0xa5, 0xbc, 0xbe, 0xb2, 0xaf, 0xa8,
				 0xac, 0x90, 0x62, 0x6e, 0x7a, 0x7d, 0x87};
/* initially set for keyboard defaults */
static int keymodes[8] = {0, 0, 0, 0, 0, 0x0000c000, 0, 0}; /* down/up keys */
static int keys[8]; /* down/up keys that are currently down */

/* Handle keyboard/button input from LK201/mouse */

ProcessInput (ev)
	register vsEvent *ev;
{
	register int idx, key, bits;

	if (ev->vse_direction != VSE_KBTRAW) {
	    Deal_with_input (ev);
	    return;
	}
	key = ev->vse_key;
	if (key > MAXSPECIAL || (key >= BASEKEY && key < MINSPECIAL)) {
	    lastkey = key;
	    idx = key >> 5;
	    key &= 0x1f;
	    key = 1 << key;
	    if (!(keymodes[idx] & key) || ((keys[idx] ^= key) & key))
		ev->vse_direction = VSE_KBTDOWN;
	    else
		ev->vse_direction = VSE_KBTUP;
	    Deal_with_input (ev);
	} else {
	    switch (key) {
		case METRONOME:
		    ev->vse_direction = VSE_KBTDOWN;
		    ev->vse_key = lastkey;
		    Deal_with_input (ev);
		    break;
		case ALLUPS:
		    idx = 7;
		    ev->vse_direction = VSE_KBTUP;
		    do {
			if (bits = keys[idx]) {
			    keys[idx] = 0;
			    key = 0;
			    do {
				if (bits & 1) {
				    ev->vse_key = (idx << 5) | key;
				    Deal_with_input (ev);
				}
				key++;
			    } while (bits >>= 1);
			}
		    } while (--idx >= 0);
		    break;
		case POWERUP_ERROR:
		case KEYDOWN_ERROR:
		case OUTPUT_ERROR:
		case INPUT_ERROR:
		    DeviceError ("keyboard error");
		    break;
	    }
	}
}

/* Put keyboard in autorepeat mode and return control command string.
 * autorepeat/down: main keyboard, numeric keypad, delete, cursors
 * up/down: all others
 */

char *AutoRepeatLKMode ()
{
	ResetLKModes (0x3f38);
	return ("\212\222\232\246\256\266\272\302\316\326\336\346\356\366");
}

/* Put all of keyboard in down/up mode and return control command string */

char *UpDownLKMode ()
{
	ResetLKModes (0x3fff);
	return ("\216\226\236\246\256\266\276\306\316\326\336\346\356\366");
}

ResetLKModes (modes)
	register int modes;
{
	register int i = 0;
	register int key, last;

	bzero ((caddr_t) keymodes, sizeof (keymodes));
	do {
	    if (modes & 1) {
		for (key = divbeg[i], last = divend[i]; key <= last; key++)
		    keymodes[key >> 5] |= 1 << (key & 0x1f);
	    }
	    modes >>= 1;
	} while (++i < NUMDIVS);
}
