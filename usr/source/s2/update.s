	sys	fork
		br 1f
	sys	exit
1:
	sys	sync
	mov	$30.,r0
	sys	sleep
	br	1b
sleep = 35.
sync = 36.
