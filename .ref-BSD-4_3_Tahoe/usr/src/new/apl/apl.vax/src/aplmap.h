static char aplmap_h_Sccsid[] = "aplmap.h @(#)aplmap.h	1.2	10/1/82 Berkeley ";
#ifdef PURDUE_EE
/*
 *	Mapping arrays
 *
 * The following arrays perform character mapping between the APL
 * character set (LSI modified by Peter Hallenbeck) and the ASCII
 * character set.
 *
 */

	/* Map to the APL character set from ASCII */

char map_apl[] = {
	'!',	/* does not map */
	' ',	/* does not map */
	'(',	/* mapped pound sign (not equals) */
	'$',	/* dollar sign maps to itself (.le.) */
	'=',	/* mapped percent (division) */
	'&',	/* & becomes .ge. automatically */
	'K',	/* mapped single quote mark */
	'+',	/* mapped opening parenthesis */
	'`',	/* mapped closing parenthesis */
	'P',	/* mapped asterisk */
	':',	/* mapped plus */
	',',	/* , maps to itself */
	'*',	/* mapped minus */
	'.',	/* . maps to itself */
	'/',	/* / maps to itself */
	'0','1','2','3','4','5','6','7','8','9',  /* all digits ok */
	'>',	/* mapped colon */
	'<',	/* mapped semi-colon */
	'#',	/* mapped less-than */
	'%',	/* mapped equals */
	'\'',	/* mapped greater-than */
	'Q',	/* mapped question-mark */
	' ',	/* @ does not map */
	'A',	/* capital a maps to itself (alpha) */
	'B',	/* capital b equals decode */
	'C',	/* capital c equals inverted U */
	'D',	/* capital d equals floot */
	'E',	/* capital e equals epsilon */
	'F',	/* capital f maps to itself (underscore) */
	'G',	/* capital g equals del */
	'H',	/* capital h equals interted del */
	'I',	/* capital i equals iota */
	'J',	/* capital j equals circle */
	' ',	/* capital k does not map */
	'L',	/* capital l equals quad */
	'-',	/* mapped multiply */
	'N',	/* capital n equals encode */
	'O',	/* capital o equals big circle */
	'=',	/* mapped divide */
	')',	/* mapped "or" */
	'R',	/* capital r equals rho */
	'S',	/* capital s equals ceiling */
	' ',	/* capital t does not map */
	'U',	/* capital u equals drop */
	')',	/* mapped "or" */
	'W',	/* capital w equals omega */
	'-',	/* capital x equals times sign */
	'Y',	/* capital y equals take */
	'Z',	/* capital z equals inverted subset symbol */
	';',	/* mapped open brace */
	'?',	/* mapped backslash */
	'@',	/* mapped closing brace */
	'{',	/* mapped caret */
	'\\',	/* mapped underscore */
	'"',	/* raised minus */
	'a','b','c','d','e','f','g','h','i','j','k','l','m',
	'n','o','p','q','r','s','t','u','v','w','x','y','z',
	'\\',	/* mapped open brace */
	'M',	/* mapped mod (abs) */
	'|',	/* mapped closing brace */
	'T',	/* mapped "not" */
	'\177'	/* delete maps to itself */
};

	/* Map to ASCII from the APL character set */

char map_ascii[] = {
	'!',
	'`',
	'<',
	'$',
	'=',
	'&',
	'>',
	'#',
	'V',
	'-',
	'(',
	',',
	'X',
	'.',
	'/',
	'0','1','2','3','4','5','6','7','8','9',
	'+',
	'[',
	';',
	'%',
	':',
	'\\',
	']',
	'A',
	'B',
	'C',
	'D',
	'E',
	'F',
	'G',
	'H',
	'I',
	'J',
	'\'',
	'L',
	'|',
	'N',
	'O',
	'*',
	'?',
	'R',
	'S',
	'~',
	'U',
	'V',
	'W',
	'X',
	'Y',
	'Z',
	'$',
	'{',
	']',
	'\10',
	'_',
	')',
	'a','b','c','d','e','f','g','h','i','j','k','l','m',
	'n','o','p','q','r','s','t','u','v','w','x','y','z',
	'^',
	'}',
	'}',
	' ',
	'\177',
};
#else
/*
 *	Mapping arrays
 *
 * The following arrays perform character mapping between the APL
 * character set (Datamedia elite 1520) and the ASCII character
 * set.  This is a typewriter-paired keyboard.
 */

	/* Map to the APL character set from ASCII */

char map_apl[] = {
	'!',	/* does not map */
	' ',	/* does not map */
	'*',    /* mapped pound sign (not equals) */
	'$',    /* dollar sign maps to itself (.le.) */
	'+',    /* mapped percent (division) */
	'^',    /* & becomes .ge. */
	'K',	/* mapped single quote mark */
	':',    /* mapped opening parenthesis */
	'"',    /* mapped closing parenthesis */
	'P',	/* mapped asterisk */
	'-',    /* mapped plus */
	',',	/* , maps to itself */
	'_',    /* mapped minus */
	'.',	/* . maps to itself */
	'/',	/* / maps to itself */
	'0','1','2','3','4','5','6','7','8','9',  /* all digits ok */
	'>',    /* mapped colon */
	'<',	/* mapped semi-colon */
	'#',	/* mapped less-than */
	'%',	/* mapped equals */
	'&',    /* mapped greater-than */
	'Q',	/* mapped question-mark */
	' ',	/* @ does not map */
	'A',	/* capital a maps to itself (alpha) */
	'B',	/* capital b equals decode */
	'C',	/* capital c equals inverted U */
	'D',	/* capital d equals floot */
	'E',	/* capital e equals epsilon */
	'F',	/* capital f maps to itself (underscore) */
	'G',	/* capital g equals del */
	'H',	/* capital h equals interted del */
	'I',	/* capital i equals iota */
	'J',	/* capital j equals circle */
	' ',	/* capital k does not map */
	'L',	/* capital l equals quad */
	'=',    /* mapped multiply */
	'N',	/* capital n equals encode */
	'O',	/* capital o equals big circle */
	'+',    /* mapped divide */
	'(',    /* mapped "or" */
	'R',	/* capital r equals rho */
	'S',	/* capital s equals ceiling */
	' ',	/* capital t does not map */
	'U',	/* capital u equals drop */
	'(',    /* mapped "or" */
	'W',	/* capital w equals omega */
	'=',    /* capital x equals times sign */
	'Y',	/* capital y equals take */
	'Z',	/* capital z equals inverted subset symbol */
	';',    /* mapped open bracket */
	'?',	/* mapped backslash */
	'\'',   /* mapped closing bracket */
	')',    /* mapped caret */
	'[',    /* mapped underscore */
	'@',    /* raised minus */
	'a','b','c','d','e','f','g','h','i','j','k','l','m',
	'n','o','p','q','r','s','t','u','v','w','x','y','z',
	'[',    /* mapped open brace */
	'M',	/* mapped mod (abs) */
	']',    /* mapped closing brace */
	'T',	/* mapped "not" */
	'\177'	/* delete maps to itself */
};

	/* Map to ASCII from the APL character set */

char map_ascii[] = {
	'!',
	')',
	'<',
	'$',
	'=',
	'>',
	']',
	'V',
	'^',
	'#',
	'%',
	',',
	'+',
	'.',
	'/',
	'0','1','2','3','4','5','6','7','8','9',
	'(',
	'[',
	';',
	'X',
	':',
	'\\',
	'`',
	'A',
	'B',
	'C',
	'D',
	'E',
	'F',
	'G',
	'H',
	'I',
	'J',
	'\'',
	'L',
	'|',
	'N',
	'O',
	'*',
	'?',
	'R',
	'S',
	'~',
	'U',
	'V',
	'W',
	'X',
	'Y',
	'Z',
	'{',
	'{',
	'}',
	'&',
	'-',
	'J',
	'a','b','c','d','e','f','g','h','i','j','k','l','m',
	'n','o','p','q','r','s','t','u','v','w','x','y','z',
	'{',
	'}',
	'}',
	'$',
	'\177',
};
#endif
