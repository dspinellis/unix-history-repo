/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)ntab.c	4.2 (Berkeley) %G%";
#endif /* not lint */

#define BYTE 8
#define PAIR(A,B) (A|(B<<BYTE))
/*
character name tables
modified for BTL special font version 4
and Commercial II
*/


int chtab [] = {
PAIR('h','y'), 0200,	/*hyphen*/
PAIR('b','u'), 0201,	/*bullet*/
PAIR('s','q'), 0202,	/*square*/
PAIR('e','m'), 0203,	/*3/4em*/
PAIR('r','u'), 0204,	/*rule*/
PAIR('1','4'), 0205,	/*1/4*/
PAIR('1','2'), 0206,	/*1/2*/
PAIR('3','4'), 0207,	/*3/4*/
PAIR('m','i'), 0302,	/*equation minus*/
PAIR('f','i'), 0211,	/*fi*/
PAIR('f','l'), 0212,	/*fl*/
PAIR('f','f'), 0213,	/*ff*/
PAIR('F','i'), 0214,	/*ffi*/
PAIR('F','l'), 0215,	/*ffl*/
PAIR('d','e'), 0216,	/*degree*/
PAIR('d','g'), 0217,	/*dagger*/
PAIR('s','c'), 0220,	/*section*/
PAIR('f','m'), 0221,	/*foot mark*/
PAIR('a','a'), 0222,	/*acute accent*/
PAIR('g','a'), 0223,	/*grave accent*/
PAIR('u','l'), 0224,	/*underrule*/
PAIR('s','l'), 0225,	/*slash (longer)*/
PAIR('*','a'), 0230,	/*alpha*/
PAIR('*','b'), 0231,	/*beta*/
PAIR('*','g'), 0232,	/*gamma*/
PAIR('*','d'), 0233,	/*delta*/
PAIR('*','e'), 0234,	/*epsilon*/
PAIR('*','z'), 0235,	/*zeta*/
PAIR('*','y'), 0236,	/*eta*/
PAIR('*','h'), 0237,	/*theta*/
PAIR('*','i'), 0240,	/*iota*/
PAIR('*','k'), 0241,	/*kappa*/
PAIR('*','l'), 0242,	/*lambda*/
PAIR('*','m'), 0243,	/*mu*/
PAIR('*','n'), 0244,	/*nu*/
PAIR('*','c'), 0245,	/*xi*/
PAIR('*','o'), 0246,	/*omicron*/
PAIR('*','p'), 0247,	/*pi*/
PAIR('*','r'), 0250,	/*rho*/
PAIR('*','s'), 0251,	/*sigma*/
PAIR('*','t'), 0252,	/*tau*/
PAIR('*','u'), 0253,	/*upsilon*/
PAIR('*','f'), 0254,	/*phi*/
PAIR('*','x'), 0255,	/*chi*/
PAIR('*','q'), 0256,	/*psi*/
PAIR('*','w'), 0257,	/*omega*/
PAIR('*','A'), 0101,	/*Alpha*/
PAIR('*','B'), 0102,	/*Beta*/
PAIR('*','G'), 0260,	/*Gamma*/
PAIR('*','D'), 0261,	/*Delta*/
PAIR('*','E'), 0105,	/*Epsilon*/
PAIR('*','Z'), 0132,	/*Zeta*/
PAIR('*','Y'), 0110,	/*Eta*/
PAIR('*','H'), 0262,	/*Theta*/
PAIR('*','I'), 0111,	/*Iota*/
PAIR('*','K'), 0113,	/*Kappa*/
PAIR('*','L'), 0263,	/*Lambda*/
PAIR('*','M'), 0115,	/*Mu*/
PAIR('*','N'), 0116,	/*Nu*/
PAIR('*','C'), 0264,	/*Xi*/
PAIR('*','O'), 0117,	/*Omicron*/
PAIR('*','P'), 0265,	/*Pi*/
PAIR('*','R'), 0120,	/*Rho*/
PAIR('*','S'), 0266,	/*Sigma*/
PAIR('*','T'), 0124,	/*Tau*/
PAIR('*','U'), 0270,	/*Upsilon*/
PAIR('*','F'), 0271,	/*Phi*/
PAIR('*','X'), 0130,	/*Chi*/
PAIR('*','Q'), 0272,	/*Psi*/
PAIR('*','W'), 0273,	/*Omega*/
PAIR('s','r'), 0274,	/*square root*/
PAIR('t','s'), 0275,	/*terminal sigma*/
PAIR('r','n'), 0276,	/*root en*/
PAIR('>','='), 0277,	/*>=*/
PAIR('<','='), 0300,	/*<=*/
PAIR('=','='), 0301,	/*identically equal*/
PAIR('~','='), 0303,	/*approx =*/
PAIR('a','p'), 0304,	/*approximates*/
PAIR('!','='), 0305,	/*not equal*/
PAIR('-','>'), 0306,	/*right arrow*/
PAIR('<','-'), 0307,	/*left arrow*/
PAIR('u','a'), 0310,	/*up arrow*/
PAIR('d','a'), 0311,	/*down arrow*/
PAIR('e','q'), 0312,	/*equation equal*/
PAIR('m','u'), 0313,	/*multiply*/
PAIR('d','i'), 0314,	/*divide*/
PAIR('+','-'), 0315,	/*plus-minus*/
PAIR('c','u'), 0316,	/*cup (union)*/
PAIR('c','a'), 0317,	/*cap (intersection)*/
PAIR('s','b'), 0320,	/*subset of*/
PAIR('s','p'), 0321,	/*superset of*/
PAIR('i','b'), 0322,	/*improper subset*/
PAIR('i','p'), 0323,	/*  " superset*/
PAIR('i','f'), 0324,	/*infinity*/
PAIR('p','d'), 0325,	/*partial derivative*/
PAIR('g','r'), 0326,	/*gradient*/
PAIR('n','o'), 0327,	/*not*/
PAIR('i','s'), 0330,	/*integral sign*/
PAIR('p','t'), 0331,	/*proportional to*/
PAIR('e','s'), 0332,	/*empty set*/
PAIR('m','o'), 0333,	/*member of*/
PAIR('p','l'), 0334,	/*equation plus*/
PAIR('r','g'), 0335,	/*registered*/
PAIR('c','o'), 0336,	/*copyright*/
PAIR('b','r'), 0337,	/*box vert rule*/
PAIR('c','t'), 0340,	/*cent sign*/
PAIR('d','d'), 0341,	/*dbl dagger*/
PAIR('r','h'), 0342,	/*right hand*/
PAIR('l','h'), 0343,	/*left hand*/
PAIR('*','*'), 0344,	/*math * */
PAIR('b','s'), 0345,	/*bell system sign*/
PAIR('o','r'), 0346,	/*or*/
PAIR('c','i'), 0347,	/*circle*/
PAIR('l','t'), 0350,	/*left top (of big curly)*/
PAIR('l','b'), 0351,	/*left bottom*/
PAIR('r','t'), 0352,	/*right top*/
PAIR('r','b'), 0353,	/*right bot*/
PAIR('l','k'), 0354,	/*left center of big curly bracket*/
PAIR('r','k'), 0355,	/*right center of big curly bracket*/
PAIR('b','v'), 0356,	/*bold vertical*/
PAIR('l','f'), 0357,	/*left floor (left bot of big sq bract)*/
PAIR('r','f'), 0360,	/*right floor (rb of ")*/
PAIR('l','c'), 0361,	/*left ceiling (lt of ")*/
PAIR('r','c'), 0362,	/*right ceiling (rt of ")*/
0,0};
