/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * from: $Hdr: kb_encode.c,v 4.300 91/06/09 06:14:51 root Rel41 $ SONY
 *
 *	@(#)kb_encode.c	8.1 (Berkeley) 6/10/93
 */

#ifdef IPC_MRX
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <news3400/iop/keyboard.h>
#include <news3400/iop/kbreg.h>
#else
#include <sys/param.h>
#include <sys/systm.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <news3400/iop/keyboard.h>
#include <news3400/iop/kbreg.h>
#endif
#include <sys/malloc.h>

extern int tmode;
extern int country;
extern Pfk_table pfk_table[];
extern Pfk_table pfk_init[];
extern Key_table *key_table_addr;

int	kbd_status;
int	shifttype;
extern int iscaps;

static kbd_shift(), kbd_pfunc(), kbd_normal(), put_kana();

/*
 *	kbd_encode(c)
 *		int c;		keyboard address code
 *
 *	kbd_encode() converts keyboard address code to character code.
 *	kbd_encode() calls back machine dependent function
 *
 *		put_code(buf, cnt)
 *			char *buf;	encoded characters
 *			int cnt;	character count
 *
 *	to store encoded data.
 */
kbd_encode(c)
	register int c;
{
	register Key_table *kp;
	register int c_mask;

	c_mask = c & 0x7f;
	c &= 0xff;
	if (c_mask > N_KEY)
		return (0);
	kp = &key_table_addr[c_mask];
	if (c & OFF)
		kp->key_flags &= ~KEY_PRESS;
	else if ((kp->key_flags & KEY_PRESS) && 
	    ((kbd_status & KBD_NOTREPT) || (kp->key_flags & NOT_REPT)))
		return (0);
	else
		kp->key_flags |= KEY_PRESS;

	if (kp->key_flags & (PSH_SHFT|SW_SHFT)) {
		kbd_shift(c);
		return (0);
	}
	if ((kp->key_flags & ALT_FUNC) && (kbd_status & KBD_ALT) ||
	    (kp->key_flags & PRG_FUNC))
		return (kbd_pfunc(c));
	return (kbd_normal(c));
}


#define KFLAGSW(a, b)	((a) ? (kbd_status |= (b)) : (kbd_status &= ~(b)))
#define	LOCKTYPE(a, b)	(shifttype = ((a) ? (a) : (b)))

static
kbd_shift(c)
	register int c;
{
	register Key_table *kp = &key_table_addr[c & 0x7f];
	register int push = (c & OFF) == 0;

	switch (kp->normal_code) {

	case S_CTRL:
		KFLAGSW(push, KBD_CTRL);
		break;

	case S_RSHFT:
		KFLAGSW(push, KBD_RSHIFT);
		break;

	case S_LSHFT:
		KFLAGSW(push, KBD_LSHIFT);
		break;

	case S_ALT:
		KFLAGSW(push, KBD_ALT);
		break;

	case S_CAPS:
		if (push) {
			kbd_status ^= KBD_CAPS;
			LOCKTYPE(iscaps, CAPSLOCK);
		}
		break;

	case S_AN:
		if (push) {
			kbd_status &= ~KBD_KANA;
		}
		break;

	case S_KANA:
		if (push) {
			switch (country) {
			case K_JAPANESE_J:
				kbd_status |= KBD_KANA;
			default:
				break;
			}
		}
		break;

	case S_ALTGR:
		KFLAGSW(push, KBD_ALTGR);
		break;

	default:
		break;

	}
	return (0);
}

static
kbd_pfunc(c)
	register int c;
{
	register Pfk_table *kp;

	if (c & OFF)
		return (0);
	for (kp = pfk_table; kp < pfk_table + N_PFK; kp++) {
		if (kp->pfk_addr != c)
			continue;
		if (kbd_status & KBD_SHIFT)
			return (put_code(kp->pfk_shift.key_string,
			    kp->pfk_shift.key_length));
		return (put_code(kp->pfk_normal.key_string,
		    kp->pfk_normal.key_length));
	}
	return (0);
}

#define	PUT(cond, code, len)		((cond) ? put_code(code, len) : 0)
#define	PUT_KANA(cond, code, len)	((cond) ? put_kana(code, len) : 0)

static
kbd_normal(c)
	int c;
{
	register Key_table *kp = &key_table_addr[c & 0x7f];

	if (c & OFF)
		return (0);
	if (kbd_status & KBD_ALT)
		return (PUT(kp->key_flags & A, &kp->alt_code, 1));
	if (kbd_status & KBD_CTRL)
		return (PUT(kp->key_flags & C, &kp->ctrl_code, 1));
	if (kbd_status & KBD_ALTGR)
		return (PUT(kp->key_flags & G, &kp->kana_code, 1));
	if (kbd_status & KBD_KANA) {
		if (kbd_status & KBD_SHIFT)
			return (PUT_KANA(kp->key_flags & J, &kp->kshft_code, 1));
		return (PUT_KANA(kp->key_flags & K, &kp->kana_code, 1));
	}
	if (kbd_status & KBD_CAPS) {
		if ((kbd_status & KBD_SHIFT) && (kp->key_flags & S)) {
			if (kp->key_flags & CAP_LOCK) {
				switch (shifttype) {

				case CAPSLOCK:
					return (put_code(&kp->shift_code, 1));

				case SHIFTLOCK:
				case SHIFTLOCK2:
					return (put_code(&kp->normal_code, 1));

				default:
					return (0);
				}
			}
			switch (shifttype) {

			case CAPSLOCK:
			case SHIFTLOCK:
				return (put_code(&kp->shift_code, 1));

			case SHIFTLOCK2:
				return (put_code(&kp->normal_code, 1));

			default:
				return (0);
			}
		}
		if (kp->key_flags & N) {
			if (kp->key_flags & CAP_LOCK)
				return (put_code(&kp->shift_code, 1));
			switch (shifttype) {

			case CAPSLOCK:
			case SHIFTLOCK:
				return (put_code(&kp->normal_code, 1));

			case SHIFTLOCK2:
				return (put_code(&kp->shift_code, 1));

			default:
				return (0);
			}
		}
	}
	if (kbd_status & KBD_SHIFT)
 		return (PUT(kp->key_flags & S, &kp->shift_code, 1));
	return (PUT(kp->key_flags & N, &kp->normal_code, 1));
}

kbd_string(cmd, p)
	int cmd;
	register Pfk_string *p;
{
	register Key_string *pk;

	if (p->pfk_num < 0 || p->pfk_num >= N_PFK)
		return (0);
	switch (p->pfk_shift) {

	case PF_NORMAL:
		pk = &pfk_table[p->pfk_num].pfk_normal;
		break;

	case PF_SHIFT:
		pk = &pfk_table[p->pfk_num].pfk_shift;
		break;

	default:
		return (0);
	}
	switch (cmd) {

	case KIOCSETS:
		if (pk->key_string != NULL) {
			free(pk->key_string, M_DEVBUF);
			pk->key_string = NULL;
			pk->key_length = 0;
		}
		if (pk->key_length = p->pfk_string.key_length) {
			pk->key_string =
			    (char *)malloc(p->pfk_string.key_length, M_DEVBUF, M_WAITOK);
			bcopy(p->pfk_string.key_string, pk->key_string,
			    p->pfk_string.key_length);
		} else
			pk->key_string = NULL;
		bcopy(p->pfk_string.key_string, pk->key_string,
		    p->pfk_string.key_length);
		pk->key_length = p->pfk_string.key_length;
		break;

	case KIOCGETS:
		p->pfk_string.key_length =
		    min(p->pfk_string.key_length, pk->key_length);
		bcopy(pk->key_string, p->pfk_string.key_string,
		    p->pfk_string.key_length);
		break;

	default:
		return (0);
	}
	return (0);
}

kbd_init()
{
	int i;
	Pfk_string pfk_buf;

	for (i = 0; i < N_PFK; i++) {
		pfk_table[i].pfk_addr = pfk_init[i].pfk_addr;
		if (pfk_init[i].pfk_normal.key_length > 0) {
			pfk_buf.pfk_num = i;
			pfk_buf.pfk_shift = PF_NORMAL;
			pfk_buf.pfk_string = pfk_init[i].pfk_normal;
			kbd_string(KIOCSETS, &pfk_buf);
		}
		if (pfk_init[i].pfk_shift.key_length > 0) {
			pfk_buf.pfk_num = i;
			pfk_buf.pfk_shift = PF_SHIFT;
			pfk_buf.pfk_string = pfk_init[i].pfk_shift;
			kbd_string(KIOCSETS, &pfk_buf);
		}
	}
	kbd_status = 0;
}

kbd_repeat(f)
	int f;
{

	if (f)
		kbd_status &= ~KBD_NOTREPT;
	else
		kbd_status |= KBD_NOTREPT;
	return (0);
}


static
put2char(c1, c2)
	int c1, c2;
{
	char buf[2];

	buf[0] = c1;
	buf[1] = c2;
	return (put_code(buf, 2));
}

#define	SS2		0x8e

static
put_kana(s, len)
	register u_char *s;
	int len;
{
	register int i;
	register u_char *p;
	u_char eucbuf[8];

	if (len <= 0)
		return (0);
#ifdef KM_EUC
	if ((tmode == KM_EUC) && ((*s >= 0xa1) && (*s <= 0xdf))) {
		p = eucbuf;
		for (i = len; i > 0; i--) {
			*p++ = SS2;
			*p++ = *s++;
		}
		return (put_code(eucbuf, len * 2));
	}
#endif /* KM_EUC */
	return (put_code(s, len));
}
