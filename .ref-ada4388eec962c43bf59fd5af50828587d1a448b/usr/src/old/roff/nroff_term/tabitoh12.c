/*-
 * %sccs.include.proprietary.c%
 *
 *	@(#)tabitoh12.c	4.2 (Berkeley) %G%
 */

#define INCH 240
/*
 * C:Itoh Prowriter (dot matrix) 12 pitch
 * nroff driving table
 * by G. Rochlin, 15feb83
 * Because the c:itoh will backspace only in incremental mode,
 * need to write a program to place commands Esc[ and Esc] around ^H.
 * If you want true underline mode instead of _^Hx_^Hy, etc.,
 * have the script first replace _^Hx with EscXxEscY, etc.
 * Further refinements are possible to clean up files and
 * minimize throughput (e.g., delete all EscYEscX pairs).
 * In the terminal initialization (twinit) and exit (twrest)
 * strings, EscE sets 12-pitch. Twrest clears by commanding
 * Esc Y, Esc", and Esc$ to clear ul, bold, and "greek",
 * Esc] to restore logic-seek print, Escf and EscT24 to restore
 * forward linefeed at 6 lines/inch, Esc< for bidirectional
 * print, EscE, and ^M (\015) to clear the print buffer.
 * Since the itoh has no keyboard, you have to run it through
 * a video terminal or micro printer port.
 * The first twinit code (Esc`) and the last twrest code
 * (Esca) set the (proper) "transparent" or "buffered" print
 * mode for tvi950 and tvi925 and Freedom 100. This mode
 * is necessary on intelligent terminals to keep all the Esc
 * codes in the driver tables from scrambling the terminal's
 * brains.  (If you have a dumb terminal, almost any print
 * mode should be safe. Smart terminals without buffered print,
 * such as the tvi920, present problems.)
 * If you have a different terminal,
 * the shell script should also replace these codes with those
 * appropriate for your machine.  If you are using an sed
 * stream for the script, make sure to use single quotes to
 * isolate the ` from the shell.
 */
#define INCH 240
struct {
	int bset;
	int breset;
	int Hor;
	int Vert;
	int Newline;
	int Char;
	int Em;
	int Halfline;
	int Adj;
	char *twinit;
	char *twrest;
	char *twnl;
	char *hlr;
	char *hlf;
	char *flr;
	char *bdon;
	char *bdoff;
	char *ploton;
	char *plotoff;
	char *up;
	char *down;
	char *right;
	char *left;
	char *codetab[256-32];
	int zzz;
	} t = {
/*bset*/	0,
/*breset*/	0177420,
/*Hor*/		INCH/24,
/*Vert*/	INCH/48,
/*Newline*/	INCH/6,
/*Char*/	INCH/12,
/*Em*/		INCH/12,
/*Halfline*/	INCH/12,
/*Adj*/		INCH/12,
/*twinit*/	"\033`\015\033E",
/*twrest*/	"\033Y\033\042\033$\033]\033f\033T24\033<\033E\015\033a\n",
/*twnl*/	"\015\n",
/*hlr*/		"\033[\033T12\033r\n\033T24\033]\033f",
/*hlf*/		"\033[\033T12\n\033T24\033]",
/*flr*/         "\033[\033r\n\033f\033]",
/*bdon*/	"\033!",
/*bdoff*/	"\033\042",
/*ploton*/	"\033>\033T03",
/*plotoff*/	"\033<\033T24",
/*up*/		"\033[\033r\n\033f\033]",
/*down*/	"\033[\n\033]",
/*right*/	"\033P \033E",
/*left*/	"\b\033P \033E",
/*codetab*/
#include "code.itoh"
