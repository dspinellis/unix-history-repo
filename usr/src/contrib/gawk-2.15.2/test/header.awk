BEGIN{
	"date" | getline cur_time
	close ("date")
	print "This line printed on", cur_time
}
