"	fix funny things done by mcount()"
"	fix its name
g/_mcount/s//mcount/g
"	fix funny name for minbrk used by monstartup() to limit brk()"
g/_minbrk/s//minbrk/g
w
q
