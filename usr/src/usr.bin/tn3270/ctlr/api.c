/*
 * This file implements the API used in the PC version.
 */

#include <stdio.h>

#include "api.h"
#include "../general/general.h"

#include "../ascii/disp_asc.h"

#include "../ctlr/screen.h"
#include "../ctlr/oia.h"

#include "../general/globals.h"

/*
 * General utility routines.
 */

#if	defined(MSDOS)

#if	defined(LINT_ARGS)
static void movetous(char *, int, int, int);
static void movetothem(int, int, char *, int);
#endif	/* defined(LINT_ARGS) */

#define	access_api(foo,length)	(foo)
#define	unaccess_api(foo,goo,length)

static void
movetous(parms, es, di, length)
char *parms;
int es, di, length;
{
    char far *farparms = parms;

    movedata(es, di, (int) FP_SEG(farparms), (int) FP_OFF(farparms), length);
}

static void
movetothem(es, di, parms, length)
int es, di;
char *parms;
int length;
{
    char far *farparms = parms;

    movedata((int) FP_SEG(farparms), (int) FP_OFF(farparms), es, di, length);
}
#endif	/* defined(MSDOS) */

#if	defined(unix)
extern char *access_api(), *unaccess_api();
#endif	/* defined(unix) */


/*
 * Supervisor Services.
 */

static void
name_resolution(regs, sregs)
union REGS *regs;
struct SREGS *sregs;
{
    NameResolveParms parms;

    movetous((char *) &parms, sregs->es, regs->x.di, sizeof parms);

    regs->h.cl = 0;
    if (memcmp((char *)&parms, NAME_SESSMGR, sizeof parms.gate_name) == 0) {
	regs->x.dx = GATE_SESSMGR;
    } else if (memcmp((char *)&parms, NAME_KEYBOARD,
					sizeof parms.gate_name) == 0) {
	regs->x.dx = GATE_KEYBOARD;
    } else if (memcmp((char *)&parms, NAME_COPY, sizeof parms.gate_name) == 0) {
	regs->x.dx = GATE_COPY;
    } else if (memcmp((char *)&parms, NAME_OIAM, sizeof parms.gate_name) == 0) {
	regs->x.dx = GATE_OIAM;
    } else {
	regs->h.cl = 0x2e;	/* Name not found */
    }
    regs->h.ch = 0x12;
    regs->h.bh = 7;
}

/*
 * Session Information Services.
 */

static void
query_session_id(regs, sregs)
union REGS *regs;
struct SREGS *sregs;
{
    QuerySessionIdParms parms;

    movetous((char *)&parms, sregs->es, regs->x.di, sizeof parms);

    if ((parms.rc != 0) || (parms.function_id != 0)) {
	parms.rc = 0x0c;
    } else if (parms.option_code != 0x01) {
	parms.rc = 0x0d;	/* Invalid option code */
    } else if (parms.data_code != 0x45) {
	parms.rc = 0x0b;
    } else {
	NameArray list;
	NameArrayElement element;

	movetous((char *)&list, FP_SEG(parms.name_array),
			    FP_OFF(parms.name_array), sizeof list);
	if ((list.length < 14) || (list.length > 170)) {
	    parms.rc = 0x12;
	} else {
	    list.number_matching_session = 1;
	    list.name_array_element.short_name = parms.data_code;
	    list.name_array_element.type = TYPE_DFT;
	    list.name_array_element.session_id = 23;
	    memcpy(list.name_array_element.long_name, "ONLYSESS",
			    sizeof list.name_array_element.long_name);
	    movetothem(FP_SEG(parms.name_array),
		FP_OFF(parms.name_array), (char *)&list, sizeof list);
	    parms.rc = 0;
	}
    }
    parms.function_id = 0x6b;
    movetothem(sregs->es, regs->x.di, (char *)&parms, sizeof parms);
}

static void
query_session_parameters(regs, sregs)
union REGS *regs;
struct SREGS *sregs;
{
    QuerySessionParametersParms parms;

    movetous((char *)&parms, sregs->es, regs->x.di, sizeof parms);

    if ((parms.rc !=0) || (parms.function_id != 0)) {
	parms.rc = 0x0c;
    } else if (parms.session_id != 23) {
	parms.rc = 0x02;
    } else {
	parms.rc = 0;
	parms.session_type = TYPE_DFT;
	parms.session_characteristics = 0;	/* Neither EAB nor PSS */
	parms.rows = MaxNumberLines;
	parms.columns = MaxNumberColumns;
	parms.presentation_space = 0;
    }
    parms.function_id = 0x6b;
    movetothem(sregs->es, regs->x.di, (char *)&parms, sizeof parms);
}

static void
query_session_cursor(regs, sregs)
union REGS *regs;
struct SREGS *sregs;
{
    QuerySessionCursorParms parms;

    movetous((char *)&parms, sregs->es, regs->x.di, sizeof parms);

    if ((parms.rc != 0) || (parms.function_id != 0)) {
	parms.rc = 0x0c;
    } else if (parms.session_id != 23) {
	parms.rc = 0x02;
    } else {
	parms.rc = 0;
	parms.cursor_type = CURSOR_BLINKING;	/* XXX what is inhibited? */
	parms.row_address = ScreenLine(CursorAddress);
	parms.column_address = ScreenLineOffset(CursorAddress);
    }

    parms.function_id = 0x6b;
    movetothem(sregs->es, regs->x.di, (char *) &parms, sizeof parms);
}

/*
 * Keyboard Services.
 */


static void
connect_to_keyboard(regs, sregs)
union REGS *regs;
struct SREGS *sregs;
{
    ConnectToKeyboardParms parms;

    movetous((char *)&parms, sregs->es, regs->x.di, sizeof parms);

    if ((parms.rc != 0) || (parms.function_id != 0)) {
	parms.rc = 0x0c;
    } else if (parms.session_id != 23) {
	parms.rc = 0x02;
    } else if (parms.intercept_options != 0) {
	parms.rc = 0x01;
    } else {
	parms.rc = 0;
	parms.first_connection_identifier = 0;
    }
    parms.function_id = 0x62;

    movetothem(sregs->es, regs->x.di, (char *)&parms, sizeof parms);
}

static void
disconnect_from_keyboard(regs, sregs)
union REGS *regs;
struct SREGS *sregs;
{
    DisconnectFromKeyboardParms parms;

    movetous((char *)&parms, sregs->es, regs->x.di, sizeof parms);

    if ((parms.rc != 0) || (parms.function_id != 0)) {
	parms.rc = 0x0c;
    } else if (parms.session_id != 23) {
	parms.rc = 0x02;
    } else if (parms.connectors_task_id != 0) {
	parms.rc = 04;			/* XXX */
    } else {
	parms.rc = 0;
    }
    parms.function_id = 0x62;

    movetothem(sregs->es, regs->x.di, (char *)&parms, sizeof parms);
}

static void
write_keystroke(regs, sregs)
union REGS *regs;
struct SREGS *sregs;
{
    WriteKeystrokeParms parms;

    movetous((char *)&parms, sregs->es, regs->x.di, sizeof parms);

    if ((parms.rc != 0) || (parms.function_id != 0)) {
	parms.rc = 0x0c;
    } else if (parms.session_id != 23) {
	parms.rc = 0x02;
    } else if (parms.connectors_task_id != 0) {
	parms.rc = 0x04;
    } else {
	parms.number_of_keys_sent = 0;
	parms.rc = 0;
	if (parms.options == OPTION_SINGLE_KEYSTROKE) {
	    KeystrokeEntry *entry = &parms.keystroke_specifier.keystroke_entry;
	    
	    if (AcceptKeystroke(entry->scancode, entry->shift_state) == 0) {
		parms.rc = 0x10;		/* XXX needs 0x12 too! */
	    }
	    parms.number_of_keys_sent++;
	} else if (parms.options == OPTION_MULTIPLE_KEYSTROKES) {
	    KeystrokeList
		list,
		far *atlist = parms.keystroke_specifier.keystroke_list;
	    KeystrokeEntry
		entry[10],		/* 10 at a time */
		*ourentry,
		far *theirentry;
	    int
		todo;

	    movetous((char *)&list, FP_SEG(atlist),
				FP_OFF(atlist), sizeof *atlist);
	    todo = list.length/2;
	    ourentry = entry+(highestof(entry)+1);

	    while (todo) {
		if (ourentry > &entry[highestof(entry)]) {
		    int thistime;

		    thistime = todo;
		    if (thistime > numberof(entry)) {
			thistime = numberof(entry);
		    }
		    movetous((char *)entry, FP_SEG(theirentry),
			    FP_OFF(theirentry), thistime*sizeof *theirentry);
		    theirentry += thistime;
		    ourentry = entry;
		}
		if (AcceptKeystroke(ourentry->scancode,
						ourentry->shift_state) == 0) {
		    parms.rc = 0x10;		/* XXX needs 0x12 too! */
		    break;
		}
		parms.number_of_keys_sent++;
		ourentry++;
		todo--;
	    }
	} else {
	    parms.rc = 0x01;
	}
    }
    parms.function_id = 0x62;

    movetothem(sregs->es, regs->x.di, (char *)&parms, sizeof parms);
/* XXX */
}


static void
disable_input(regs, sregs)
union REGS *regs;
struct SREGS *sregs;
{
    DisableInputParms parms;

    movetous((char *)&parms, sregs->es, regs->x.di, sizeof parms);

    if ((parms.rc != 0) || (parms.function_id != 0)) {
	parms.rc = 0x0c;
    } else if (parms.session_id != 23) {
	parms.rc = 0x02;
    } else if (parms.connectors_task_id != 0) {
	parms.rc = 0x04;
    } else {
	SetOiaApiInhibit(&OperatorInformationArea);
	parms.rc = 0;
    }
    parms.function_id = 0x62;

    movetothem(sregs->es, regs->x.di, (char *)&parms, sizeof parms);
}

static void
enable_input(regs, sregs)
union REGS *regs;
struct SREGS *sregs;
{
    EnableInputParms parms;

    movetous((char *)&parms, sregs->es, regs->x.di, sizeof parms);

    if ((parms.rc != 0) || (parms.function_id != 0)) {
	parms.rc = 0x0c;
    } else if (parms.session_id != 23) {
	parms.rc = 0x02;
    } else if (parms.connectors_task_id != 0) {
	parms.rc = 0x04;
    } else {
	ResetOiaApiInhibit(&OperatorInformationArea);
	parms.rc = 0;
    }
    parms.function_id = 0x62;

    movetothem(sregs->es, regs->x.di, (char *)&parms, sizeof parms);
}

/*
 * Copy Services.
 */

copy_subroutine(target, source, parms, what_is_user)
BufferDescriptor *target, *source;
CopyStringParms *parms;
int what_is_user;
#define	USER_IS_TARGET	0
#define	USER_IS_SOURCE	1
{
#define	TARGET_NO_EAB		1
#define	SOURCE_NO_EAB		2
#define	TARGET_PC		4
#define	SOURCE_PC		8
#define	NO_FIELD_ATTRIBUTES	16
    int needtodo = 0;
    int length;
    int access_length;
    char far *input;
    char far *output;
    char far *access_pointer;

    if ((target->characteristics^source->characteristics)
		    &CHARACTERISTIC_EAB) {
	if (target->characteristics&CHARACTERISTIC_EAB) {
	    needtodo |= TARGET_NO_EAB;	/* Need to bump for EAB in target */
	} else {
	    needtodo |= SOURCE_NO_EAB;	/* Need to bump for EAB in source */
	}
    }
    if (target->session_type != source->session_type) {
	if (target->session_type == TYPE_PC) {
	    needtodo |= TARGET_PC;	/* scan codes to PC */
	} else {
	    needtodo |= SOURCE_PC;	/* PC to scan codes */
	}
    }
    if ((parms->copy_mode&COPY_MODE_FIELD_ATTRIBUTES) == 0) {
	needtodo |= NO_FIELD_ATTRIBUTES;
    }
    access_length = length = parms->source_end-source->begin;
    if (what_is_user == USER_IS_TARGET) {
	if (target->characteristics&CHARACTERISTIC_EAB) {
	    access_length *= 2;
	}
	input = (char far *) &Host[source->begin];
	access_pointer = target->buffer;
	output = access_api(target->buffer, access_length);
    } else {
	if (source->characteristics&CHARACTERISTIC_EAB) {
	    access_length *= 2;
	}
	access_pointer = source->buffer;
	input = access_api(source->buffer, access_length);
	output = (char far *) &Host[target->begin];
    }
    while (length--) {
	if (needtodo&TARGET_PC) {
	    *output++ = disp_asc[*input++];
	} else if (needtodo&SOURCE_PC) {
	    *output++ = asc_disp[*input++];
	} else {
	    *output++ = *input++;
	}
	if (needtodo&TARGET_NO_EAB) {
	    *input++;
	} else if (needtodo&SOURCE_NO_EAB) {
	    *output++ = 0;		/* Should figure out good EAB? */
	}
    }
    if (what_is_user == USER_IS_TARGET) {
	unaccess_api(target->buffer, access_pointer, access_length);
    } else {
	unaccess_api(source->buffer, access_pointer, access_length);
    }
}


static void
copy_string(regs, sregs)
union REGS *regs;
struct SREGS *sregs;
{
    CopyStringParms parms;
    BufferDescriptor *target = &parms.target, *source = &parms.source;
    int length;

    movetous((char *)&parms, sregs->es, regs->x.di, sizeof parms);

    if ((parms.rc != 0) || (parms.function_id !=0)) {
	parms.rc = 0x0c;
    } else if (target->session_id == 0) {	/* Target is buffer */
	if (source->session_id != 23) {		/* A no-no */
	    parms.rc = 0x2;
	} else {
	    if ((source->characteristics == target->characteristics) &&
		    (source->session_type == target->session_type)) {
		length = parms.source_end-source->begin;
		if (source->characteristics&CHARACTERISTIC_EAB) {
		    length *= 2;
		}
		movetothem( (int) FP_SEG(target->buffer),
			(int) FP_OFF(target->buffer),
			(char *)&Host[source->begin], length);
	    } else {
		copy_subroutine(target, source, &parms, USER_IS_TARGET);
	    }
	}
    } else if (source->session_id != 0) {
	    parms.rc = 0xd;
    } else {
	if ((source->characteristics == target->characteristics) &&
		(source->session_type == target->session_type)) {
	    length = parms.source_end-source->begin;
	    if (source->characteristics&CHARACTERISTIC_EAB) {
		length *= 2;
	    }
	    movetous((char *)&Host[target->begin],
			(int) FP_SEG(source->buffer),
			(int) FP_OFF(source->buffer), length);
	} else {
	    copy_subroutine(target, source, &parms, USER_IS_SOURCE);
	}
    }
    movetothem(sregs->es, regs->x.di, (char *)&parms, sizeof parms);
}
/*
 * Operator Information Area Services.
 */

static void
read_oia_group(regs, sregs)
union REGS *regs;
struct SREGS *sregs;
{
    ReadOiaGroupParms parms;

    movetous((char *)&parms, sregs->es, regs->x.di, sizeof parms);

    if ((parms.rc != 0) || (parms.function_id != 0)) {
	parms.rc = 0x0c;
    } else if (parms.session_id != 23) {
	parms.rc = 0x02;
    } else {
	int group = parms.oia_group_number;
	char *from;
	int size;

	if ((group != API_OIA_ALL_GROUPS) &&
		((group > API_OIA_LAST_LEGAL_GROUP) || (group < 0))) {
	} else {
	    if (group == API_OIA_ALL_GROUPS) {
		size = API_OIA_BYTES_ALL_GROUPS;
		from = (char *)&OperatorInformationArea;
	    } else if (group == API_OIA_INPUT_INHIBITED) {
		size = sizeof OperatorInformationArea.input_inhibited;
		from = (char *)&OperatorInformationArea.input_inhibited[0];
	    } else {
		size = 1;
		from = ((char *)&OperatorInformationArea)+group;
	    }
	    movetothem(FP_SEG(parms.oia_buffer), FP_OFF(parms.oia_buffer),
			from, size);
	}
    }
    parms.function_id = 0x6d;
    movetothem(sregs->es, regs->x.di, (char *)&parms, sizeof parms);
}

static void
unknown_op(regs, sregs)
union REGS *regs;
struct SREGS *sregs;
{
    regs->h.ch = 0x12;
    regs->h.cl = 0x05;
}


handle_api(regs, sregs)
union REGS *regs;
struct SREGS *sregs;
{
    if (regs->h.ah == NAME_RESOLUTION) {
	name_resolution(regs, sregs);
    } else if (regs->h.ah != 0x09) {
	regs->h.ch = 0x12;
	regs->h.cl = 0x0f;		/* XXX Invalid environmental access */
    } else if (regs->x.bx != 0x8020) {
	regs->h.ch = 0x12;
	regs->h.cl = 0x08;		/* XXX Invalid wait specified */
    } else if (regs->h.ch != 0) {
	regs->x.cx = 0x1206;		/* XXX Invalid priority */
    } else {
	switch (regs->x.dx) {
	case GATE_SESSMGR:
	    switch (regs->h.al) {
	    case QUERY_SESSION_ID:
		if (regs->h.cl != 0) {
		    regs->x.cx = 0x1206;
		} else {
		    regs->x.cx = 0x1200;
		    query_session_id(regs, sregs);
		}
		break;
	    case QUERY_SESSION_PARAMETERS:
		if (regs->h.cl != 0) {
		    regs->x.cx = 0x1206;
		} else {
		    regs->x.cx = 0x1200;
		    query_session_parameters(regs, sregs);
		}
		break;
	    case QUERY_SESSION_CURSOR:
		if (regs->h.cl != 0xff) {
		    regs->x.cx = 0x1206;
		} else {
		    regs->x.cx = 0x1200;
		    query_session_cursor(regs, sregs);
		}
		break;
	    default:
		unknown_op(regs, sregs);
		break;
	    }
	    break;
	case GATE_KEYBOARD:
	    if (regs->h.cl != 00) {
		regs->x.cx = 0x1206;
	    } else {
		regs->x.cx = 0x1200;
		switch (regs->h.al) {
		case CONNECT_TO_KEYBOARD:
		    connect_to_keyboard(regs, sregs);
		    break;
		case DISABLE_INPUT:
		    disable_input(regs, sregs);
		    break;
		case WRITE_KEYSTROKE:
		    write_keystroke(regs, sregs);
		    break;
		case ENABLE_INPUT:
		    enable_input(regs, sregs);
		    break;
		case DISCONNECT_FROM_KEYBOARD:
		    disconnect_from_keyboard(regs, sregs);
		    break;
		default:
		    unknown_op(regs, sregs);
		    break;
		}
	    }
	    break;
	case GATE_COPY:
	    if (regs->h.cl != 0xff) {
		regs->x.cx = 0x1206;
	    } else {
		regs->x.cx = 0x1200;
		switch (regs->h.al) {
		case COPY_STRING:
		    copy_string(regs, sregs);
		    break;
		default:
		    unknown_op(regs, sregs);
		    break;
		}
	    }
	    break;
	case GATE_OIAM:
	    if (regs->h.cl != 0xff) {
		regs->x.cx = 0x1206;
	    } else {
		regs->x.cx = 0x1200;
		switch (regs->h.al) {
		case READ_OIA_GROUP:
		    read_oia_group(regs, sregs);
		    break;
		default:
		    unknown_op(regs, sregs);
		    break;
		}
	    }
	    break;
	default:
	    regs->h.ch = 0x12;
	    regs->h.cl = 0x34;		/* Invalid GATE entry */
	    break;
	}
    }
}
