#this program creates palindromic output - slightly modified from Gawk Manual
{
	rev($0, length)
}

function rev(str, len) {
	if (len == 0) {
		print " ", $0
		return
	}
	printf "%c", substr(str, len, 1)
	rev(str, len - 1)
}
