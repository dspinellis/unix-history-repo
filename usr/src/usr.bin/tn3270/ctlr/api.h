/*
 * This file contains header information used by the PC API routines.
 */

#define	API_INTERRUPT_NUMBER	0x7A		/* API Interrupt Number */

/*
 * Define the gate numbers.  These are returned via the Name Resolution
 * service.
 */

#define	GATE_SESSMGR	1
#define	GATE_KEYBOARD	2
#define	GATE_COPY	3
#define	GATE_OIAM	4

/*
 * The names which correspond to the above gate numbers.
 */

#define	NAME_SESSMGR	"SESSMGR "
#define	NAME_KEYBOARD	"KEYBOARD"
#define	NAME_COPY	"COPY    "
#define	NAME_OIAM	"OIAM    "


/*
 * Name Resolution is specified in AH.
 */

#define	NAME_RESOLUTION	0x81

/*
 * Codes specified in AL for various services.
 */

#define	QUERY_SESSION_ID		0x01
#define	QUERY_SESSION_PARMS		0x02
#define	QUERY_SESSION_CURSOR		0x0b

#define	CONNECT_TO_KEYBOARD		0x01
#define	DISCONNECT_FROM_KEYBOARD	0x02
#define	WRITE_KEYSTROKE			0x04
#define	DISABLE_INPUT			0x05
#define	ENABLE_INPUT			0x06

#define	COPY_STRING			0x01

#define	READ_OIA_GROUP			0x02

/*
 * For each service, we define the assoicated parameter blocks.
 */

/*
 * Supervisor Services
 */

typedef struct {
    char	gate_name[8];
} NameResolveParms;


/*
 * Session Information Services
 */

typedef struct {
    char
	short_name,
	type,
	session_id,
	reserved,
	long_name[8];
} NameArrayElement;

typedef struct {
    char
	length,
	number_matching_session;
    NameArrayElement
	name_array_element;		/* Variable number */
} NameArray;

typedef struct {
    char
	rc,
	function_id,
	option_code,
	data_code;
    NameArray far
	*name_array;
    char
	long_name[8];
} QuerySessionIdParms;

typedef struct {
    char
	rc,
	function_id,
	session_id,
	reserved,
	session_type,
	session_characteristics,
	rows,
	columns;
    char far
	*presentation_space;
} QuerySessionParametersParms;

#define	TYPE_WSCTL		0x01		/* Work Station Control */
#define	TYPE_DFT		0x02		/* DFT Host Session */
#define	TYPE_CUT		0x03		/* CUT Host Session */
#define	TYPE_NOTEPAD		0x04		/* Notepad Session */
#define	TYPE_PC			0x05		/* Personal Computer Session */

#define	CHARACTERISTIC_EAB	0x80		/* Extended Attribute Buffer */
#define	CHARACTERISTIC_PSS	0x40		/* Program Symbols Supported */

typedef struct {
    char
	rc,
	function_id,
	session_id,
	cursor_type,
	row_address,				/* from 0 */
	column_address;				/* from 0 */
} QuerySessionCursorParms;

#define	CURSOR_INHIBITED_AUTOSCROLL	0x10
#define	CURSOR_INHIBITED		0x04
#define	CURSOR_BLINKING			0x02
#define	CURSOR_BOX			0x01
typedef struct {
    char
	rc,
	function_id,
	session_id,
	reserved;
    int
	event_queue_id,
	input_queue_id;
    char
	intercept_options,
	first_connection_identifier;
} ConnectToKeyboardParms;

typedef struct {
    char
	rc,
	function_id,
	session_id,
	reserved;
    int
	connectors_task_id;
} DisconnectFromKeyboardParms;

typedef struct {
    char
	scancode,
	shift_state;
} KeystrokeEntry;

typedef struct {
    int
	length;			/* Length (in bytes) of list */
    KeystrokeEntry keystrokes;	/* Variable size */
} KeystrokeList;

typedef struct {
    char
	rc,
	function_id,
	session_id,
	reserved;
    int
	connectors_task_id;
    char
	options,
	number_of_keys_sent;
    union {
	KeystrokeEntry
	    keystroke_entry;
	KeystrokeList far
	    *keystroke_list;
    } keystroke_specifier;
} WriteKeystrokeParms;

typedef struct {
    char
	rc,
	function_id,
	session_id,
	reserved;
    int
	connectors_task_id;
} DisableInputParms;

typedef DisableInputParms EnableInputParms;

typedef struct {
    char
	session_id,
	reserved;
    char far
	*buffer;
    char
	characteristics,
	session_type;
    int
	begin;			/* Offset within buffer */
} BufferDescriptor;
    
typedef struct {
    char
	rc,
	function_id;
    BufferDescriptor
	source;
    int
	source_end;		/* Offset within source buffer */
    BufferDescriptor
	target;
    char
	copy_mode,
	reserved;
} CopyStringParms;

#define	COPY_MODE_7_COLOR		0x80	/* Else 4 color mode */
#define	COPY_MODE_FIELD_ATTRIBUTES	0x40	/* Else don't copy attributes */

typedef struct {
    char
	rc,
	function_id,
	session_id,
	reserved;
    char far
	*oia_buffer;
    char
	oia_group_number;
} ReadOiaGroupParms;

#define	OIA_ALL_GROUPS				0xFF
#define	OIA_ONLINE_OWNERSHIP			0x01
#	define	OIA_SETUP		0x80
#	define	OIA_TEST		0x40
#	define	OIA_SSCP_LU		0x20
#	define	OIA_LU_LU		0x10
#	define	OIA_UNOWNED		0x08
#	define	OIA_SUBSYSTEM_READY	0x04

#define	OIA_CHARACTER_SELECTION			0x02
#	define	OIA_EXTENDED_SELECT	0x80
#	define	OIA_APL			0x40
#	define	OIA_KANA		0x20
#	define	OIA_ALPHA		0x10
#	define	OIA_TEXT		0x08

#define	OIA_SHIFT_STATE				0x03
#	define	OIA_NUMERIC		0x80
#	define	OIA_UPPER_SHIFT		0x40

#define	OIA_PSS_GROUP_1				0x04
#define	OIA_HIGHLIGHT_GROUP_1			0x05
#define	OIA_COLOR_GROUP_1			0x06
#	define	OIA_SELECTABLE		0x80
#	define	OIA_FIELD_INHERIT	0x40

#define	OIA_INSERT				0x07
#	define	OIA_INSERT_MODE		0x80

/* We define this to be a 'long' followed by a 'char' (5 bytes) */

#define	OIA_INPUT_INHIBITED			0x08

#	define	OIA_NON_RESETTABLE	0x80000000
#	define	OIA_SECURITY_KEY	0x40000000
#	define	OIA_MACHINE_CHECK	0x20000000
#	define	OIA_COMM_CHECK		0x10000000
#	define	OIA_PROGRAM_CHECK	0x08000000
#	define	OIA_RETRY		0x04000000
#	define	OIA_DEVICE_NOT_WORKING	0x02000000
#	define	OIA_DEVICE_VERY_BUSY	0x01000000

#	define	OIA_DEVICE_BUSY		0x00800000
#	define	OIA_TERMINAL_WAIT	0x00400000
#	define	OIA_MINUS_SYMBOL	0x00200000
#	define	OIA_MINUS_FUNCTION	0x00100000
#	define	OIA_TOO_MUCH_ENTERED	0x00080000
#	define	OIA_NOT_ENOUGH_ENTERED	0x00040000
#	define	OIA_WRONG_NUMBER	0x00020000
#	define	OIA_NUMERIC_FIELD	0x00010000

#	define	OIA_OP_UNAUTHORIZED	0x00008000
#	define	OIA_OP_UNAUTHORIZED_MIN	0x00004000
#	define	OIA_INVALID_DEAD_KEY_COMBO 0x00002000
#	define	OIA_WRONG_PLACE		0x00001000

#	define	OIA_MESSAGE_PENDING	0x00000080
#	define	OIA_PARTITION_WAIT	0x00000040
#	define	OIA_SYSTEM_WAIT		0x00000020
#	define	OIA_HARDWARE_MISMATCH	0x00000010
#	define	OIA_LOGICAL_TERM_NOT_CONF 0x00000008


#	define	OIA_AUTOKEY_INHIBIT	0x80
#	define	OIA_API_INHIBIT		0x40

#define	OIA_PSS_GROUP_2				0x09
#	define	OIA_PS_SELECTED		0x80
#	define	OIA_PC_DISPLAY_DISABLE	0x40

#define	OIA_HIGHLIGHT_GROUP_2			0x0a
#define	OIA_COLOR_GROUP_2			0x0b
#	define	OIA_SELECTED		0x80

#define	OIA_COMMUNICATION_ERROR_REMINDER	0x0c
#	define	OIA_COMM_ERROR		0x80
#	define	OIA_RTM			0x40

#define	OIA_PRINTER_STATUS			0x0d
#	define	OIA_PRINT_NOT_CUSTOM	0x80
#	define	OIA_PRINTER_MALFUNCTION	0x40
#	define	OIA_PRINTER_PRINTING	0x20
#	define	OIA_ASSIGN_PRINTER	0x10
#	define	OIA_WHAT_PRINTER	0x08
#	define	OIA_PRINTER_ASSIGNMENT	0x04

#define	OIA_AUTOKEY_PLAY_RECORD_STATUS		0x10
#	define	OIA_PLAY		0x80
#	define	OIA_RECORD		0x40

#define	OIA_AUTOKEY_ABORT_PAUSE_STATUS		0x11
#	define	OIA_RECORDING_OVERFLOW	0x80
#	define	OIA_PAUSE		0x40

#define	OIA_ENLARGE_STATE			0x12
#	define	OIA_WINDOW_IS_ENLARGED	0x80



