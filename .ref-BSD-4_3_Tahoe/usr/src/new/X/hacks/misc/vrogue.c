#include <signal.h>
#include <stdio.h>
#include <sgtty.h>
#include <sys/wait.h>
#include <sys/time.h>
#define BUFLEN 1024

int got_a_signal;

main(argc,argv)
int argc;
char *argv[];
{
    extern int  tstp_handler (), quit_handler (), chld_handler (),
    		int_handler ();
    extern int got_a_signal;
    struct ltchars   new_ltc, old_ltc;
    struct sigvec   vec;
    int     topipe[2], frompipe[2];
    int	    pipeflag, nfds, from_rogue, to_rogue, readfds;
    int     num, rogue_pid, old_mask;

    /* Since this IS a game, reduce our priority. */
    setpriority(0, getpid(), 4);

    /* Create the pipes that will connect us to rogue. */
    if (pipe (topipe) != 0) {
        printf ("Couldn't open pipe to rogue\n");
        exit (1);
    }
    if (pipe (frompipe) != 0) {
        printf ("Couldn't open pipe to rogue\n");
        exit (1);
    }

    if (rogue_pid = vfork ()) {
	/* Close the ends of the pipes that we don't need, and make better 
	   names for the ones we do. */
        close (topipe[0]);
        close (frompipe[1]);
        from_rogue = frompipe[0];
	to_rogue = topipe[1];

	/* Get the old interrupt chars and save them. */
        if (ioctl (0, TIOCGLTC, &old_ltc))
	    printf("Couldn't get old_ltc.\n");

	/* Copy the old interrupt chars and make the rest of them 
	   correspond to what rogue uses. */
        new_ltc = old_ltc;
        new_ltc.t_dsuspc = -1;
        if (ioctl (0, TIOCSLTC, &new_ltc))
	    printf("Couldn't remove ^Y.\n");

	/* Install our interrupt handlers */
        vec.sv_mask = 0;
        vec.sv_onstack = 0;
        vec.sv_handler = tstp_handler;
        if (sigvec (SIGTSTP, &vec, 0))
	    printf("Couldn't install TSTP handler.\n");
        vec.sv_handler = quit_handler;
        if (sigvec (SIGQUIT, &vec, 0))
	    printf("Couldn't install QUIT handler.\n");
        vec.sv_handler = int_handler;
        if (sigvec (SIGINT, &vec, 0))
	    printf("Couldn't install INT handler.\n");
        vec.sv_handler = chld_handler;
        if (sigvec (SIGCHLD, &vec, 0))
	    printf("Couldn't install CHLD handler.\n");

        pipeflag = 1 << from_rogue;
        nfds = from_rogue + 1;
        while (1) {
            readfds = pipeflag | 1;
            num = select (nfds, &readfds, 0, 0, 0);
	    if (num > 0)
                if (readfds & pipeflag) process_output(from_rogue);
                else process_input(to_rogue);

	    if (got_a_signal) {
                handle_signal(got_a_signal, rogue_pid, from_rogue, &old_ltc);
		got_a_signal = 0;
	    }
        }
    }
    else {
        /* We are the child process, so attach our ends of the pipe to stdin
	   and stdout. */
        if ((dup2 (topipe[0], 0)) == -1) {
            printf ("couldn't dup2 stdin pipe\n\n");
            exit (1);
        }
        if ((dup2 (frompipe[1], 1)) == -1) {
            printf ("couldn't dup2 stdout pipe\n\n");
            exit (1);
        }

	/* Close the ends of the pipe we don't need. */
        close (topipe[1]);
        close (frompipe[0]);

	/* Do it. */
        execv ("/usr/games/rogue", argv);
        printf ("Couldn't execv rogue!\n");
        exit (1);
    }
}

handle_signal(the_signal, rogue_pid, from_rogue, old_ltc)
int the_signal, rogue_pid, from_rogue;
struct ltchars *old_ltc;
{
    switch (the_signal) {
    case SIGQUIT:
        if (kill(rogue_pid, SIGQUIT)) {
	    normal_font();
	    printf("Couldn't QUIT rogue!\n");
	    exit(1);
	}
        break;
    case SIGTSTP:
        if (kill(rogue_pid, SIGTSTP)) {
	    normal_font();
	    printf("Couldn't TSTP rogue!\n");
	    exit(1);
	}
        break;
    case SIGINT:
        if (kill(rogue_pid, SIGINT)) {
	    normal_font();
	    printf("Couldn't INT rogue!\n");
	    exit(1);
	}
        break;
    case SIGCHLD: 
        {
            union wait status;
            int nfds, pipeflag;
            int num, readfds;
            struct timeval timeout;

	    /* For some reason, our child has stopped.  There is no need
	       (I think) to send it any more characters, so just finish
	       processing its output, and decide how we should stop. */


            nfds = from_rogue + 1;
            pipeflag = 1 << from_rogue;
            timeout.tv_sec = 0;
            timeout.tv_usec = 100000;
            do {
                readfds = pipeflag;
                num = select (nfds, &readfds, 0, 0, &timeout);
		if (num > 0) 
		    num = process_output(from_rogue);
            }
            while(num > 0);

	    normal_font();

            if (wait3(&status, WNOHANG | WUNTRACED, 0) > 0) {
                if (WIFSTOPPED(status)) {
                    if (kill(getpid(), SIGSTOP)) {
		        printf("Couldn't STOP myself!\n");
			exit(1);
		    }
                    if (kill(rogue_pid, SIGCONT)) {
		        printf("Couldn't CONT rogue!\n");
			exit(1);
		    }
                }
                else if (WIFEXITED(status)) {
		    ioctl(0,TIOCSLTC,old_ltc);
                    exit(status.w_retcode);
                }
                else if (WIFSIGNALED(status)) {
		    ioctl(0,TIOCSLTC,old_ltc);
                    printf("Rogue terminated.\n");
                    printf("termsig: %d\n",status.w_termsig);
                    printf("coredump: %d\n",status.w_coredump);
                    printf("retcode: %d\n",status.w_retcode);
		    exit(-1);
                }
                else {
		    ioctl(0,TIOCSLTC,old_ltc);
                    printf("Rogue's status is bizzare!\n");
                    exit(1);
                }
            }
        }
        break;
    default:
        break;
    }
}

quit_handler() {
    extern int got_a_signal;
    got_a_signal = SIGQUIT;
}

tstp_handler() {
    extern int got_a_signal;
    got_a_signal = SIGTSTP;
}

int_handler() {
    extern int got_a_signal;
    got_a_signal = SIGINT;
}

chld_handler() {
    extern int got_a_signal;
    got_a_signal = SIGCHLD;
}

normal_font() {
    write(1, "[m", 3);
}

special_font() {
    write (1, "[4m", 4);
}

int process_input(to_rogue)
int to_rogue;
{
    static char buf[BUFLEN];
    register int num, old_mask;

    old_mask = sigblock(1 << (SIGCHLD - 1));
    num = read (0, buf, BUFLEN);
    if (num <= 0) {
        printf("Error reading keyboard!\n");
	exit(1);
    }
    write (to_rogue, buf, num);
    sigsetmask(old_mask);
    return(num);
}

int process_output(from_rogue)
int from_rogue;
{
    static char buf[BUFLEN];
    static int state = 0;
    static int linenum = 1;
    static int special = 0;
    static int force = 0;
    static int accum = 0;
    int chr, i, start, num, something_changed, old_mask;

    old_mask = sigblock(1 << (SIGCHLD - 1));
    num = read (from_rogue, buf, BUFLEN);

    if (num <= 0) {
	sigsetmask(old_mask);
        return(num);
    }

    start = 0;
    for (i = 0; i < num; i++) {
        chr = buf[i];
        switch (state) {
        case 0: 
	/* State 0 is the normal idle state. */
            switch (chr) {
            case '': 
                state = 1;
                break;
            case '\n': 
                linenum++;
                something_changed = 1;
                break;
            case 'a': 
            case 'b': 
            case 'c': 
            case 'd': /* e is left out on purpose */
            case 'f': 
            case 'g': 
            case 'h': 
            case 'i': 
            case 'j': 
            case 'k': 
            case 'l': 
            case 'm': 
            case 'n': 
            case 'o': 
            case 'p': 
            case 'q': 
            case 'r': 
            case 's': 
            case 't': 
            case 'u': 
            case 'v': 
            case 'w': 
            case 'x': 
            case 'y': 
            case 'z': 
                if ((linenum > 1) && (linenum < 24) && (!force)) {
                    force = 1;
                    something_changed = 1;
                }
                break;
            case 'e': 
                if (force) {
                    state = 5;
                }
                if ((linenum > 1) && (linenum < 24) && (!force)) {
		    state = 5;
                    force = 1;
                    something_changed = 1;
                }
                break;
            default: 
                break;
            }
            break;
        case 1: 
	/* We've begun an escape sequence. */
            switch (chr) {
            case '[': 
                state = 2;
                break;
            default: 
                state = 0;
                break;
            }
            break;
        case 2: 
	/* We've seen "<esc>[" */
            switch (chr) {
            case '0': 
            case '1': 
            case '2': 
            case '3': 
            case '4': 
            case '5': 
            case '6': 
            case '7': 
            case '8': 
            case '9': 
                state = 3;
                accum = chr - '0';
                break;
            case ';': 
                state = 4;
                accum = 1;
                break;
            case 'A': 
                state = 0;
                linenum--;
                something_changed = 1;
                break;
            case 'H': 
                state = 0;
                linenum = 1;
                something_changed = 1;
                break;
	    case 'm':
		/* We've seen "<esc>[m"
	           Rogue is trying to get out of reverse video. */
		state = 0;
		break;
            default: 
                state = 0;
                break;
            }
            break;
        case 3: 
	/* We've seen "<esc>[n"
	   Continue parsing the argument. */
            switch (chr) {
            case '0': 
	    case '1': 
	    case '2': 
	    case '3': 
	    case '4': 
	    case '5': 
	    case '6': 
	    case '7': 
	    case '8': 
	    case '9': 
                accum = (10 * accum) + chr - '0';
                break;
            case ';': 
                state = 4;
                break;
	    case 'm':
	    /* Rogue is trying to use reverse video or something. */
	        state = 0;
		break;
            default: 
                state = 0;
                break;
            }
            break;
        case 4: 
	/* We've seen "<esc>[nn;"
	   Ignore the rest of the digits until the magic H completes the
	   sequence. */
            switch (chr) {
            case 'H': 
                state = 0;
                linenum = accum;
                something_changed = 1;
                break;
            case '0': 
            case '1': 
            case '2': 
            case '3': 
            case '4': 
            case '5': 
            case '6': 
            case '7': 
            case '8': 
            case '9': 
                break;
            default: 
                state = 0;
                break;
            }
            break;
        case 5: 
	/* We've seen an "e" on lines 2-23.   This means that we're in
           an inventory or something.  We're hoping for "e--", the end of
	   "--more--" or "--Press space to continue--". */
            switch (chr) {
            case '-': 
                state = 6;
                break;
            case '': 
                state = 1;
                break;
            case '\n': 
                state = 0;
                linenum++;
                something_changed = 1;
                break;
            default: 
                break;
            }
            break;
        case 6: 
	/* We've seen "e-".  If we see another "-" we can stop forcing. */
            switch (chr) {
            case '-': 
                state = 0;
                force = 0;
                something_changed = 1;
                break;
            case '': 
                state = 1;
                break;
            case '\n': 
                state = 0;
                linenum++;
                something_changed = 1;
                break;
            default: 
                break;
            }
            break;
        default: 
            break;
        }
        if (something_changed) {
            something_changed = 0;
	    if (force) {
	        /* Since we're just starting for force, force before outputting
		   the character just looked at. */
		if (special) {
		    write(1, buf + start, i - start);
		    normal_font();
		    start = i;
		    special = 0;
		}
            }
            else if ((linenum == 1) || (linenum >= 24)) {
                if (special) {
                    write (1, buf + start, i - start + 1);
		    normal_font();
                    start = i + 1;
                    special = 0;
                }
            }
            else {
                if (!special) {
                    write (1, buf + start, i - start + 1);
		    special_font();
                    start = i + 1;
                    special = 1;
                }
            }
        }
    }

    /* Output any characters remaining in the buffer. */
    if (i != start)
        write (1, buf + start, i - start);

    sigsetmask(old_mask);

    return(num);
}
