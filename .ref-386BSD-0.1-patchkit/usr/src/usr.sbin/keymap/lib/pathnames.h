/*
 * Contributed to 386bsd 0.1 and later versions
 *
 *	Copyright 1992 by Holger Veit
 *	May be freely used with Bill Jolitz's port of 
 *	386bsd and may be included in a 386bsd collection
 *	as long as binary and source are available and reproduce the above
 *	copyright.
 *
 *	You may freely modify this code and contribute improvements based
 *	on this code as long as you don't claim to be the original author.
 *	Commercial use of this source requires permittance of the copyright 
 *	holder. A general license for 386bsd will override this restriction.
 *
 *	Use at your own risk. The copyright holder or any person who makes
 *	this code available for the public (administrators of public archives
 *	for instance) are not responsible for any harm to hardware or software
 *	that might happen due to wrong application or program faults.
 *
 * You must have the codriver driver in the same package generated
 * into the 386bsd kernel, otherwise this program does not work.
 *
 *	@(#)pathnames.h	1.0 (386bsd contribution) 1.8.92
 */

#include <paths.h>

#define	_PATH_KEYCAP		"/usr/share/misc/keycap"
#define _PATH_CONSFONT		"/usr/share/font"
#define _PATH_KEYBOARD		"/dev/kbd"
#define _PATH_VIDEO		"/dev/vga"
