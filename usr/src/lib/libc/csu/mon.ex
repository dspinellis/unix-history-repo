"	@(#)mon.ex	4.3 (Berkeley) 12/10/82"
"	fix funny things done by mcount()"
"	fix its name, make it jsb'able, change registers to protect the caller"
g/_mcount/s//mcount/g
/mcount:/-
/.word/s/.word.*//
/the beginning of mcount()/mark a
/the end of mcount()/mark b
'a,'bs/r11/r5/g
'a,'bs/r10/r4/g
""
"	fix funny name for curbrk used by brk()"
g/_curbrk/s//curbrk/g
w
q
