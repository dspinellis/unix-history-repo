/*
 * Copyright (c) 1982, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 *	@(#)kbdmap.h	8.1 (Berkeley) 6/10/93
 */

#define	ESC	'\033'
#define	DEL	'\177'

struct kbdmap {
	int	kbd_code;
	char	*kbd_desc;
	char	*kbd_keymap;
	char	*kbd_shiftmap;
	char	*kbd_ctrlmap;
	char	*kbd_ctrlshiftmap;
	char	**kbd_stringmap;
};

/* kbd_code */
#define KBD_SPECIAL	0x00		/* user defined */
#define KBD_US		0x1F		/* US ASCII */
#define KBD_UK		0x17		/* United Kingdom */

#define KBD_DEFAULT	KBD_US		/* default type */

#ifdef KERNEL
/* XXX: ITE interface */
char	*kbd_keymap;
char	*kbd_shiftmap;
char	*kbd_ctrlmap;
char	*kbd_ctrlshiftmap;
char	**kbd_stringmap;

extern struct kbdmap kbd_map[];
#endif
