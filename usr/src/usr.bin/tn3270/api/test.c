#include <stdio.h>

#include "../api/api.h"
#include "apilib.h"
#include "../ctlr/oia.h"

api_perror(string)
char *string;
{
    fprintf(stderr, "Error: [0x%x/0x%x:0x%x/0x%x] from %s.\n",
	api_sup_fcn_id, api_sup_errno,
	api_fcn_fcn_id, api_fcn_errno, string);
}


char *
session_type(type)
int	type;
{
    switch (type) {
    case TYPE_WSCTL:
	return "work station control";
    case TYPE_DFT:
	return "distributed function terminal";
    case TYPE_CUT:
	return "control unit terminal";
    case TYPE_NOTEPAD:
	return "notepad";
    case TYPE_PC:
	return "personal computer";
    default:
	return "(UNKNOWN)";
    }
}


main()
{
    int session_id;
    OIA oia;
    QuerySessionIdParms id;
    QuerySessionParametersParms pa;
    QuerySessionCursorParms cu;
    ConnectToKeyboardParms conn;
    DisconnectFromKeyboardParms disc;
    WriteKeystrokeParms wr;
    DisableInputParms disable;
    EnableInputParms enable;
    CopyStringParms copy;
    ReadOiaGroupParms re;
    NameArray namearray;

    if (api_init() == 0) {
	fprintf(stderr, "API function not available.\n");
	return 1;
    }

    id.rc = 0;
    id.function_id = 0;
    id.option_code = ID_OPTION_BY_NAME;
    id.data_code = 'E';
    id.name_array = &namearray;
    namearray.length = sizeof namearray;
    if (api_query_session_id(&id)) {
	api_perror("api_query_session_id");
    } else if (namearray.number_matching_session == 0) {
	fprintf(stderr, "query_session_id:  No matching sessions!\n");
    } else {
	printf("Session short name 0x%x, type is ",
				namearray.name_array_element.short_name);
	printf("%s", session_type(namearray.name_array_element.type));
	printf(", session ID is: 0x%x\n",
				namearray.name_array_element.session_id);
    }
    session_id = namearray.name_array_element.session_id;

    pa.rc = pa.function_id = 0;
    pa.session_id = session_id;
    if (api_query_session_parameters(&pa) == -1) {
	api_perror("api_query_session_parameters");
    } else {
	printf("Session type %s, ", session_type(pa.session_type));
	if (pa.session_characteristics&CHARACTERISTIC_EAB) {
	    printf(" has EAB, ");
	}
	if (pa.session_characteristics&CHARACTERISTIC_PSS) {
	    printf(" has PSS, ");
	}
	printf("%d rows, %d columns ", pa.rows, pa.columns);
	if (pa.presentation_space) {
	    printf("presentation space at 0x%x:0x%x.\n",
		FP_SEG(pa.presentation_space), FP_OFF(pa.presentation_space));
	} else {
	    printf("(no direct presentation space access).\n");
	}
    }

    cu.rc = cu.function_id = 0;
    cu.session_id = session_id;
    if (api_query_session_cursor(&cu) == -1) {
	api_perror("api_query_session_cursor");
    } else {
	printf("cursor");
	if (cu.cursor_type&CURSOR_INHIBITED_AUTOSCROLL) {
	    printf(" inhibited autoscroll");
	}
	if (cu.cursor_type&CURSOR_INHIBITED) {
	    printf(" inhibited");
	}
	if (cu.cursor_type&CURSOR_BLINKING) {
	    printf(" blinking");
	} else {
	    printf(" not blinking");
	}
	if (cu.cursor_type&CURSOR_BOX) {
	    printf(" box ");
	} else {
	    printf(" not box ");
	}
	printf("at row %d, column %d.\n", cu.row_address, cu.column_address);
    }

    re.rc = re.function_id = 0;
    re.session_id = session_id;
    re.oia_buffer = (char far *) &oia;
    re.oia_group_number = API_OIA_ALL_GROUPS;
    if (api_read_oia_group(&re) == -1) {
	api_perror("api_read_oia_group");
    } else {
	if (IsOiaReady3274(&oia)) {
	    printf("3274 ready, ");
	}
	if (IsOiaMyJob(&oia)) {
	    printf("my job, ");
	}
	if (IsOiaInsert(&oia)) {
	    printf("insert mode, ");
	}
	if (IsOiaSystemLocked(&oia)) {
	    printf("system locked, ");
	}
	if (IsOiaTWait(&oia)) {
	    printf("terminal wait, ");
	}
	printf("are some bits from the OIA.\n");
    }

    conn.rc = conn.function_id = 0;
    conn.session_id = session_id;
    conn.event_queue_id = conn.input_queue_id = 0;
    conn.intercept_options = 0;
    if (api_connect_to_keyboard(&conn) == -1) {
	api_perror("api_connect_to_keyboard");
    } else {
	if (conn.first_connection_identifier) {
	    printf("First keyboard connection.\n");
	} else {
	    printf("Not first keyboard connection.\n");
	}
    }

    disable.rc = disable.function_id = 0;
    disable.session_id = session_id;
    disable.connectors_task_id = 0;
    if (api_disable_input(&disable) == -1) {
	api_perror("api_disable_input");
    } else {
	printf("Disabled.\n");
    }

    wr.rc = wr.function_id = 0;
    wr.session_id = session_id;
    wr.connectors_task_id = 0;
    wr.options = OPTION_SINGLE_KEYSTROKE;
    wr.number_of_keys_sent = 0;
    wr.keystroke_specifier.keystroke_entry.scancode = 0x3a;
    wr.keystroke_specifier.keystroke_entry.shift_state = 0;
    if (api_write_keystroke(&wr) == -1) {
	api_perror("api_write_keystroke");
    } else {
	if (wr.number_of_keys_sent != 1) {
	    fprintf(stderr,
			"write_keystroke claims to have sent %d keystrokes.\n",
			wr.number_of_keys_sent);
	} else {
	    printf("Keystroke sent.\n");
	}
    }

    enable.rc = enable.function_id = 0;
    enable.session_id = session_id;
    enable.connectors_task_id = 0;
    if (api_enable_input(&enable) == -1) {
	api_perror("api_enable");
    } else {
	printf("Enabled.\n");
    }

    disc.rc = disc.function_id = 0;
    disc.session_id = session_id;
    disc.connectors_task_id = 0;
    if (api_disconnect_from_keyboard(&disc) == -1) {
	api_perror("api_disconnect_from_keyboard");
    } else {
	printf("Disconnected from keyboard.\n");
    }

    (void) api_finish();

    return 0;
}
