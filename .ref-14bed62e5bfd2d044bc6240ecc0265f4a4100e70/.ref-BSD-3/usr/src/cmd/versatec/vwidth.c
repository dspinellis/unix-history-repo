#

#

/*
 *	Creates a width table for troff from a versatec font for a
 *		normal font.
 *	Usage: width font point_size
 *		where font is the file name of the versatec font and
 *		point_size is it point size.
 *	It is better to use as large a point size font as possible 
 *	to avoid round off.
 */

struct wtable
	{
	char charloc;
	char *name;
	} wtable[]
		{
		'\214',	"space",
		'!',	"!",
		'"',	"\"",
		'#',	"#",
		'$',	"$",
		'%',	"%",
		'&',	"&",
		'\'',	"'",
		'(',	"(",
		')',	")",
		'*',	"*",
		'+',	"+",
		',',	",",
		'-',	"- hyphen",
		'.',	".",
		'/',	"/",
		'0',	"0",
		'1',	"1",
		'2',	"2",
		'3',	"3",
		'4',	"4",
		'5',	"5",
		'6',	"6",
		'7',	"7",
		'8',	"8",
		'9',	"9",
		':',	":",
		';',	";",
		'<',	"<",
		'=',	"=",
		'>',	">",
		'?',	"?",
		'@',	"@",
		'A',	"A",
		'B',	"B",
		'C',	"C",
		'D',	"D",
		'E',	"E",
		'F',	"F",
		'G',	"G",
		'H',	"H",
		'I',	"I",
		'J',	"J",
		'K',	"K",
		'L',	"L",
		'M',	"M",
		'N',	"N",
		'O',	"O",
		'P',	"P",
		'Q',	"Q",
		'R',	"R",
		'S',	"S",
		'T',	"T",
		'U',	"U",
		'V',	"V",
		'W',	"W",
		'X',	"X",
		'Y',	"Y",
		'Z',	"Z",
		'[',	"[",
		'\\',	"\\",
		']',	"]",
		'^',	"^",
		'_',	"_",
		'\`',	"\`",
		'a',	"a",
		'b',	"b",
		'c',	"c",
		'd',	"d",
		'e',	"e",
		'f',	"f",
		'g',	"g",
		'h',	"h",
		'i',	"i",
		'j',	"j",
		'k',	"k",
		'l',	"l",
		'm',	"m",
		'n',	"n",
		'o',	"o",
		'p',	"p",
		'q',	"q",
		'r',	"r",
		's',	"s",
		't',	"t",
		'u',	"u",
		'v',	"v",
		'w',	"w",
		'x',	"x",
		'y',	"y",
		'z',	"z",
		'{',	"{",
		'|',	"|",
		'}',	"}",
		'~',	"~",
		'\206',	"narrow space",
		'-',	"hyphen",
		'\07',	"bullet",
		'\010',	"square",
		'\06',	"3/4 em dash",
		'\05',	"rule",
		'\021',	"1/4",
		'\022',	"1/2",
		'\023',	"3/4",
		'\04',	"minus",
		'\01',	"fi",
		'\02',	"fl",
		'\03',	"ff",
		'\011',	"ffi",
		'\012',	"ffl",
		'\013',	"degree",
		'\014',	"dagger",
		'\200',	"section (unimplem)",
		'\015',	"foot mark",
		'\200',	"acute acc (unimplem)",
		'\200',	"grave acc (unimplem)",
		'\200',	"underrule (unimplem)",
		'\200',	"slash (unimplem)",
		'\203',	"half narrow space",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\200',	"null",
		'\017',	"registered",
		'\016',	"copyright",
		'\200',	"null",
		'\020',	"cent",
		0,	0
		};

struct desc
	{
	short addr;
	short nbytes;
	char up;
	char down;
	char left;
	char right;
	short width;
	} desc[256];

main(argc, argv)
	int argc;
	char *argv[];
	{
	register int cl;
	register esc;
	register w;
	int i, psize;
	int fd, high;

	if(argc != 3)
		{
		printf("arg count\n");
		exit();
		}
	fd = open(argv[1], 0);
	psize = atoi(argv[2]);
	lseek(fd, 10, 0);
	read(fd, &desc, sizeof desc);
	high = desc['a'].up+1;
	printf("char XXw[256-32] {\n");
	for(i = 0; wtable[i].charloc != 0; i++)
		{
		cl = wtable[i].charloc & 0377;
		if(cl & 0200)
			w = cl & 0177;
		   else
			w = desc[cl].width*(54./25.)*(6./psize)+.5;
		esc = 0;
		if((cl >= '0' && cl <= '9') || (cl >= 'A' && cl <= 'Z') ||
					(cl >= 'a' && cl <= 'z'))
			{
			if(desc[cl].up > high)
				esc =| 0200;
			if(desc[cl].down > 0)
				esc =| 0100;
			}
		if(esc)
			printf("%d+0%o,\t/* %s */\n", w, esc, wtable[i].name);
		   else
			printf("%d,\t\t/* %s */\n", w, wtable[i].name);
		}
	printf("};\n");
	}
