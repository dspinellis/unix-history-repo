/*
 * The following is the structure which defines what a 3270 keystroke
 * can do.
 */

struct hits {
    unsigned char keynumber;
    struct hit {
	enum type { undefined = 0, illegal, character, function, aid } type;
	unsigned char code;	/* AID value; 3270 display code; function id */
    } hit[4];	/* plain, shifted, alted, shiftalted */
};

extern struct hits hits[];


/*
 * The following are the various functions which the keyboard can ask
 * the controller to perform.
 *
 * Note that this file (the following entries) are scanned by mkhit.c,
 * and that the format must remain more-or-less consistent
 * (#define\tFCN_name\t[\t]*TOKEN)
 */

#define	FCN_NULL		131  		/* Illegal sequence */
#define	FCN_VERTICAL_BAR	132
#define	FCN_CAPS_LOCK		134
#define	FCN_MAKE_SHIFT		135
#define	FCN_DVCNL		136
#define	FCN_MAKE_ALT		137
#define	FCN_SPACE		138
#define	FCN_LEFT2		139
#define	FCN_RIGHT2		140
#define	FCN_MONOCASE		141
#define	FCN_BREAK_SHIFT		142
#define	FCN_BREAK_ALT		143
#define	FCN_ATTN		144
#define	FCN_LPRT		145
#define	FCN_DP			146
#define	FCN_FM			147
#define	FCN_CURSEL		148
#define	FCN_CENTSIGN		149
#define	FCN_RESHOW		150
#define	FCN_EINP		151
#define	FCN_EEOF		152
#define	FCN_DELETE		153
#define	FCN_INSRT		154
#define	FCN_TAB			155
#define	FCN_BTAB		156
#define	FCN_COLTAB		157
#define	FCN_COLBAK		158
#define	FCN_INDENT		159
#define	FCN_UNDENT		160
#define	FCN_NL			161
#define	FCN_HOME		162
#define	FCN_UP			163
#define	FCN_DOWN		164
#define	FCN_RIGHT		165
#define	FCN_LEFT		166
#define	FCN_SETTAB		167
#define	FCN_DELTAB		168
#define	FCN_SETMRG		169
#define	FCN_SETHOM		170
#define	FCN_CLRTAB		171
#define	FCN_APLON		172
#define	FCN_APLOFF		173
#define	FCN_APLEND		174
#define	FCN_PCON		175
#define	FCN_PCOFF		176
#define	FCN_DISC		177
#define	FCN_INIT		178
#define	FCN_ALTK		179
#define	FCN_FLINP		180
#define	FCN_ERASE		181
#define	FCN_WERASE		182
#define	FCN_FERASE		183
#define	FCN_SYNCH		184
#define	FCN_RESET		185
#define	FCN_MASTER_RESET	186
#define	FCN_XOFF		187
#define	FCN_XON			188
#define	FCN_ESCAPE		189
#define	FCN_WORDTAB		190
#define	FCN_WORDBACKTAB		191
#define	FCN_WORDEND		192
#define	FCN_FIELDEND		193
#define	FCN_MAKE_CTRL		194
#define	FCN_TEST		195
