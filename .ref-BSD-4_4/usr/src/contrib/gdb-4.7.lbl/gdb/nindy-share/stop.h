/******************************************************************/
   Copyright 1990, 1992 Free Software Foundation, Inc.

   This code was donated by Intel Corp.

   Intel hereby grants you permission to copy, modify, and
   distribute this software and its documentation.  Intel grants
   this permission provided that the above copyright notice
   appears in all copies and that both the copyright notice and
   this permission notice appear in supporting documentation.  In
   addition, Intel grants this permission provided that you
   prominently mark as not part of the original any modifications
   made to this software or documentation, and that the name of
   Intel Corporation not be used in advertising or publicity
   pertaining to distribution of the software or the documentation
   without specific, written prior permission.

   Intel Corporation does not warrant, guarantee or make any
   representations regarding the use of, or the results of the use
   of, the software and documentation in terms of correctness,
   accuracy, reliability, currentness, or otherwise; and you rely
   on the software, documentation and results solely at your own
   risk.							  */
/******************************************************************/


/******************************************************************
 *
 *   REASONS WHY NINDY CAN STOP EXECUTING AN APPLICATION PROGRAM
 *
 * When NINDY stops executing an application program that was running
 * under remote host ("gdb") control, it signals the host by sending
 * a single ^P.  The host can then query as to the reason for the halt.
 * NINDY responds with two bytes of information.
 *
 * The first byte is a boolean flag that indicates whether or not
 * the application has exited.
 *
 * If the flag is true, the second byte contains the exit code.
 *
 * If the flag is false, the second byte contains a "reason for
 * stopping" code.  This file defines the possible values of that
 * code.
 *
 * There are three categories of reasons why the halt may have occurred:
 * faults, traces, and software interactions.  The first two categories
 * are processor-dependent; the values of these codes are tightly coupled
 * to the hardware and should not be changed without first examining
 * src/nindy/common/fault.c.  The software interactions involve
 * communication between NINDY and the host debugger;  their codes are
 * arbitrary.
 *
 ******************************************************************/

#define FAULT_PARALLEL	0x00
#define FAULT_UNKNOWN	0x01
#define FAULT_OPERATION	0x02
#define FAULT_ARITH	0x03
#define FAULT_FP	0x04
#define FAULT_CONSTR	0x05
#define FAULT_VM	0x06
#define FAULT_PROTECT	0x07
#define FAULT_MACHINE	0x08
#define FAULT_STRUCT	0x09
#define FAULT_TYPE	0x0a
	/* 0x0b reserved */
#define FAULT_PROCESS	0x0c
#define FAULT_DESC	0x0d
#define FAULT_EVENT	0x0e
	/* 0x0f reserved */

#define LAST_FAULT      0x0f

#define TRACE_STEP	0x10
#define TRACE_BRANCH	0x11
#define TRACE_CALL	0x12
#define TRACE_RET	0x13
#define TRACE_PRERET	0x14
#define TRACE_SVC	0x15
#define TRACE_BKPT	0x16

#define STOP_SRQ	0xfe
	/* Application program has service request to make of host */

#define STOP_GDB_BPT    0xff
	/* Application program has reached breakpoint (fmark) set by host */
