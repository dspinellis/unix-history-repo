/ C operator tables

.globl fltused; fltused=.
.globl	_instab

.data
_instab:
	40.; 1f; 3f; .text; 1:<add\0>; .data
	70.; 1b; 3f
	41.; 2f; 4f; .text; 2:<sub\0>; .data
	71.; 2b; 4f
	30.; 3f; 1b; .text; 3:<inc\0>; .data
	31.; 4f; 2b; .text; 4:<dec\0>; .data
	32.; 3b; 1b
	33.; 4b; 2b

	81.; 1f; 1f; .text; 1:<bic\0>; .data
	78.; 2f; 2f; .text; 2:<bis\0>; .data
	55.; 1b; 1b
	48.; 2b; 2b

	98.; 1f; 1f; .text; 1:<*$\0>; .data
	99.; 1b+2; 1b+2

	60.; 0f; 1f; .text; 0:<jeq\0>; 1:<jne\0>; .data
	61.; 1b; 0b
	62.; 2f; 5f; .text; 2:<jle\0>; 5:<jgt\0>; .data
	63.; 3f; 4f; .text; 3:<jlt\0>; 4:<jge\0>; .data
	64.; 4b; 3b
	65.; 5b; 2b
	66.; 6f; 9f; .text; 6:<jlos\0>; 9:<jhi\0>; .data
	67.; 7f; 8f; .text; 7:<jlo\0>; 8:<jhis\0>; .data
	68.; 8b; 7b
	69.; 9b; 6b

	260.; 0b; 1b
	261.; 1b; 0b
	262.; 2b; 5b
	263.; 3b; 4b
	264.; 4b; 3b
	265.; 5b; 2b
	266.; 0b; 1b
	267.; 7f; 8f; .text; 7:</nop\0>; 8:<jbr\0>; .data
	268.; 8b; 7b
	269.; 1b; 0b
	0
