/*
 * This file implements the API used in the PC version.
 */

#include <stdio.h>

#include "api.h"
#include "../general/general.h"

#include "../ctlr/screen.h"
#include "../general/globals.h"

int ApiDisableInput = 0;

/*
 * Supervisor Services.
 */

static void
name_resolve(regs, sregs)
union REGS *regs;
struct SREGS *sregs;
{
    NameResolveParms parms;

    movetous((char *) &parms, sregs->es, regs->x.di, sizeof parms);

    regs->h.cl = 0;
    if (strcmp((char *)&parms, NAME_SESSMGR) == 0) {
	regs->x.dx = GATE_SESSMGR;
    } else if (strcmp((char *)&parms, NAME_KEYBOARD) == 0) {
	regs->x.dx = GATE_KEYBOARD;
    } else if (strcmp((char *)&parms, NAME_COPY) == 0) {
	regs->x.dx = GATE_COPY;
    } else if (strcmp((char *)&parms, NAME_OIAM) == 0) {
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

    if (parms.rc != 0) {
	regs->h.cl = 0x0c;
	return;
    }
    if (parms.option_code != 0x01) {
	regs->h.cl = 0x0d;	/* Invalid option code */
    } else if (parms.data_code != 0x45) {
	regs->h.cl = 0x0b;
    } else {
	NameArray list;
	NameArrayElement element;

	movetous((char *)&list, FP_SEG(parms.name_array),
			    FP_OFFSET(parms.name_array), sizeof list);
	if ((list.length < 14) || (list.length > 170)) {
	    parms.rc = 0x12;
	    regs->h.cl = 0x12;
	} else {
	    list.number_matching_session = 1;
	    list.name_array_element.short_name = parms.data_code;
	    list.name_array_element.type = TYPE_DFT;
	    list.name_array_element.session_id = 23;
	    memcpy(list.name_array_element.long_name, "ONLYSESS",
			    sizeof list.name_array_element.long_name);
	    movetothem(FP_SEG(parms.name_array),
		FP_OFFSET(parms.name_array), (char *)&list, sizeof list);
	    parms.rc = 0;
	    regs->h.cl = 0;
	}
    }
    parms.function_id = 0x6d;
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
	regs->h.cl = 0x0c;
	return;
    }
    if (parms.session_id != 23) {
	regs->h.cl = parms.rc = 0x02;
    } else {
	regs->h.cl = parms.rc = 0;
	parms.function_id = 0x6b;
	parms.session_type = TYPE_DFT;
	parms.session_characteristics = 0;	/* Neither EAB nor PSS */
	parms.rows = MaxNumberLines;
	parms.columns = MaxNumberColumns;
	parms.presentation_space = 0;
    }
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
	parms.function_id = 0x6b;
	parms.cursor_type = CURSOR_BLINKING;	/* XXX what is inhibited? */
	parms.row_address = ScreenLine(CursorAddress);
	parms.column_address = ScreenLineOffset(CursorAddress);
    }

    movetothem(sregs->es, regs->x.di, sizeof parms);
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
	ApiDisableInput = 1;
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
	ApiDisableInput = 0;
	parms.rc = 0;
    }
    parms.function_id = 0x62;

    movetothem(sregs->es, regs->x.di, (char *)&parms, sizeof parms);
}

/*
 * Copy Services.
 */

static void
copy_string(regs, sregs)
union REGS *regs;
struct SREGS *sregs;
{
    CopyStringParms parms;
    BufferDescriptor *target, *source;

    movetous((char *)&parms, sregs->es, regs->x.di, sizeof parms);

    if ((parms.rc != 0) || (parms.function_id !=0)) {
	parms.rc = 0x0c;
    }
    /* XXX do something! */
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
	char far *where = parms.oia_buffer;

	switch (group) {
	case OIA_ALL_GROUPS:
	case OIA_ONLINE_OWNERSHIP:
	    if (group != OIA_ALL_GROUPS) {
		break;
	    } /* else, fall through */
	case OIA_CHARACTER_SELECTION:
	    if (group != OIA_ALL_GROUPS) {
		break;
	    } /* else, fall through */
	case OIA_SHIFT_STATE:
	    if (group != OIA_ALL_GROUPS) {
		break;
	    } /* else, fall through */
	case OIA_PSS_GROUP_1:
	    if (group != OIA_ALL_GROUPS) {
		break;
	    } /* else, fall through */
	case OIA_HIGHLIGHT_GROUP_1:
	    if (group != OIA_ALL_GROUPS) {
		break;
	    } /* else, fall through */
	case OIA_COLOR_GROUP_1:
	    if (group != OIA_ALL_GROUPS) {
		break;
	    } /* else, fall through */
	case OIA_INSERT:
	    if (group != OIA_ALL_GROUPS) {
		break;
	    } /* else, fall through */
	case OIA_INPUT_INHIBITED:
	    if (group != OIA_ALL_GROUPS) {
		break;
	    } /* else, fall through */
	case OIA_PSS_GROUP_2:
	    if (group != OIA_ALL_GROUPS) {
		break;
	    } /* else, fall through */
	case OIA_HIGHLIGHT_GROUP_2:
	    if (group != OIA_ALL_GROUPS) {
		break;
	    } /* else, fall through */
	case OIA_COLOR_GROUP_2:
	    if (group != OIA_ALL_GROUPS) {
		break;
	    } /* else, fall through */
	case OIA_COMM_ERROR_REMINDER:
	    if (group != OIA_ALL_GROUPS) {
		break;
	    } /* else, fall through */
	case OIA_PRINTER_STATUS:
	    if (group != OIA_ALL_GROUPS) {
		break;
	    } /* else, fall through */
	case OIA_AUTOKEY_PLAY_RECORD_STATUS:
	    if (group != OIA_ALL_GROUPS) {
		break;
	    } /* else, fall through */
	case OIA_AUTOKEY_ABORT_PAUSE_STATUS:
	    if (group != OIA_ALL_GROUPS) {
		break;
	    } /* else, fall through */
	case OIA_ENLARGE_STATE:
	    if (group != OIA_ALL_GROUPS) {
		break;
	    } /* else, fall through */

	    /* oops, we are done! */
	    break;
	default:
	    break;
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
	regs->h.ch = 0x12;
	regs->h.cl = 0x07;		/* XXX Invalid reply specified */
    } else {
	switch (regs->x.dx) {
	case GATE_SESSMGR:
	    switch (regs->h.al) {
	    case QUERY_SESSION_ID:
		if (regs->h.cl != 0) {
		} else {
		    query_session_id(regs, sregs);
		}
		break;
	    case QUERY_SESSION_PARMS:
		if (regs->h.cl != 0) {
		} else {
		    query_session_parms(regs, sregs);
		}
		break;
	    case QUERY_SESSION_CURSOR:
		if (regs->h.cl != 0xff) {
		} else {
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
	    } else {
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
	    } else {
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
	    } else {
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
