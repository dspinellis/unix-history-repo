/*	tabnec.c	4.2	85/02/14	*/
#define INCH 240
/*
 * NEC Spinwriter 5500 or 7700 10 Pitch
 * nroff driving table
 * Courier - 72/Manifold thimble
 * by Skip Walker, ucbvax!c:bodega, 21nov82
 */
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
/*breset*/	0,
/*Hor*/		INCH/120,
/*Vert*/	INCH/48,
/*Newline*/	INCH/6,
/*Char*/	INCH/10,
/*Em*/		INCH/10,
/*Halfline*/	INCH/12,
/*Adj*/		INCH/10,
/*twinit*/	"",
/*twrest*/	"",
/*twnl*/	"\n",
/*hlr*/		"\033]S\0339\033]W",
/*hlf*/		"\033]S\033ZA\033]W",
/*flr*/		"\0339",
/*bdon*/	"\033+",
/*bdoff*/	"\033,",
/*ploton*/	"\033]A\033]P",
/*plotoff*/	"\033]J\033]W",
/*up*/		"\0339",
/*down*/	"\033ZA",
/*right*/	" ",
/*left*/	"\b",
/*codetab*/
#include "code.nec"
