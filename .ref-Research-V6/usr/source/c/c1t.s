/ C operator tables

.globl fltused; fltused=.
.globl	_instab
.globl	_branchtab

.data
_instab:
	80.; 1f; 2f; .text; 1:<mov\0>; 2:<clr\0>; .data
	60.; 1f; 2f; .text; 1: <cmp\0>; 2: <tst\0>; .data
	106.; 1b; 2b
	61.; 1b; 2b
	62.; 1b; 2b
	63.; 1b; 2b
	64.; 1b; 2b
	65.; 1b; 2b
	66.; 1b; 2b
	67.; 1b; 2b
	68.; 1b; 2b
	69.; 1b; 2b
	40.; 1f; 3f; .text; 1:<add\0>; .data
	70.; 1b; 3f
	41.; 2f; 4f; .text; 2:<sub\0>; .data
	71.; 2b; 4f
	30.; 1b; 3f; .text; 3:<inc\0>; .data
	31.; 2b; 4f; .text; 4:<dec\0>; .data
	32.; 1b; 3b
	33.; 2b; 4b
	42.; 5f; 5f; .text; 5:<mul>; .data
	72.; 5b; 5b
	43.; 6f; 6f; .text; 6:<div\0>; .data
	73.; 6b; 6b
	44.; 5b; 6b
	74.; 5b; 6b
	45.; 5f; 6f; .text; 	6:<asr\0>; .data
	75.; 5f; 6b
	46.; 5f; 6f; .text; 5:<ash\0>; 6:<asl\0>; .data
	76.; 5b; 6b
	47.; 5f; 5f; .text; 5:<bic\0>; 6:<bic $1,\0>; .data
	55.; 5b; 6b
	85.; 5b; 6b
	81.; 5f; 6f; .text; 5:<bit\0>; 6:<bit $1,\0>; .data
	48.; 5f; 6f; .text; 5:<bis\0>; 6:<bis $1,\0>; .data
	78.; 5b; 6b
	49.; 5f; 5f; .text; 5:<xor\0>; .data
	79.; 5b; 5b
	37.; 1f; 1f; .text; 1:<neg\0>; .data
	38.; 1f; 1f; .text; 1:<com\0>; .data

	98.; 1f; 1f; .text; 1:<*$\0>; .data
	99.; 1b+2; 1b+2
	91.; 1f; 1f; .text; 1: <ashc\0>; .data
	92.; 1b; 1b
	82.; 1f; 1f; .text; 1:<lmul\0>; .data
	83.; 1f; 1f; .text; 1:<ldiv\0>; .data
	84.; 1f; 1f; .text; 1:<lrem\0>; .data
	86.; 1f; 1f; .text; 1:<almul\0>; .data
	87.; 1f; 1f; .text; 1:<aldiv\0>; .data
	88.; 1f; 1f; .text; 1:<alrem\0>; .data
	0

.data
_branchtab:
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
