/*
 * Copyright (c) 1992 David I. Bell
 * Permission is granted to use, distribute, or modify this source,
 * provided that this copyright notice remains intact.
 *
 * Arbitrary precision calculator.
 */

#include <signal.h>
#include <pwd.h>
#include <sys/types.h>

#include "calc.h"
#include "func.h"
#include "opcodes.h"
#include "config.h"
#include "token.h"
#include "symbol.h"

/*
 * Common definitions
 */
long maxprint;		/* number of elements to print */
int abortlevel;		/* current level of aborts */
BOOL inputwait;		/* TRUE if in a terminal input wait */
jmp_buf jmpbuf;		/* for errors */

static int q_flag = FALSE;	/* TRUE => don't execute rc files */

char *calcpath;		/* $CALCPATH or default */
char *calcrc;		/* $CALCRC or default */
char *home;		/* $HOME or default */
static char *pager;	/* $PAGER or default */
char *shell;		/* $SHELL or default */

static void intint();	/* interrupt routine */
void givehelp();
static void initenv();	/* initialize/default special environment vars */

/* extern struct passwd *getpwuid(uid_t); */
extern struct passwd *getpwuid();
extern char *getenv();
extern uid_t geteuid();

/*
 * Top level calculator routine.
 */
main(argc, argv)
	char **argv;
{
	char *str;		/* current option string or expression */
	char cmdbuf[MAXCMD+1];	/* command line expression */

	initenv();
	argc--;
	argv++;
	while ((argc > 0) && (**argv == '-')) {
		for (str = &argv[0][1]; *str; str++) switch (*str) {
			case 'h':
				givehelp(DEFAULTCALCHELP);
				exit(0);
				break;
			case 'q':
				q_flag = TRUE;
				break;
			default:
				printf("Unknown option\n");
				exit(1);
		}
		argc--;
		argv++;
	}
	str = cmdbuf;
	*str = '\0';
	while (--argc >= 0) {
		*str++ = ' ';
		strcpy(str, *argv++);
		str += strlen(str);
		str[0] = '\n';
		str[1] = '\0';
	}
	str = cmdbuf;
	if (*str == '\0') {
		str = NULL;
		printf("C-style arbitrary precision calculator.\n");
		version(stdout);
		printf("[Type \"exit\" to exit, or \"help\" for help.]\n\n");
	}
	if (setjmp(jmpbuf) == 0) {
		initmasks();
		inittokens();
		initglobals();
		initfunctions();
		initstack();
		resetinput();
		cleardiversions();
		setfp(stdout);
		setmode(MODE_INITIAL);
		setdigits(DISPLAY_DEFAULT);
		maxprint = MAXPRINT_DEFAULT;
		_epsilon_ = atoq(EPSILON_DEFAULT);
		_epsilonprec_ = qprecision(_epsilon_);
		if (str) {
			if (q_flag == FALSE) {
				runrcfiles();
				q_flag = TRUE;
			}
			(void) openstring(str);
			getcommands();
			exit(0);
		}
	}
	if (str)
		exit(1);
	abortlevel = 0;
	_math_abort_ = FALSE;
	inputwait = FALSE;
	(void) signal(SIGINT, intint);
	cleardiversions();
	setfp(stdout);
	resetinput();
	if (q_flag == FALSE) {
		runrcfiles();
		q_flag = TRUE;
	}
	(void) openterminal();
	getcommands();
	exit(0);
	/*NOTREACHED*/
}


/*
 * initenv - obtain $CALCPATH, $CALCRC, $HOME, $PAGER and $SHELL values
 *
 * If $CALCPATH, $CALCRC, $PAGER or $SHELL do not exist, use the default
 * values.  If $PAGER or $SHELL is an empty string, also use a default value.
 * If $HOME does not exist, or is empty, use the home directory
 * information from the password file.
 */
static void
initenv()
{
	struct passwd *ent;		/* our password entry */

	/* determine the $CALCPATH value */
	calcpath = getenv(CALCPATH);
	if (calcpath == NULL)
		calcpath = DEFAULTCALCPATH;

	/* determine the $CALCRC value */
	calcrc = getenv(CALCRC);
	if (calcrc == NULL) {
		calcrc = DEFAULTCALCRC;
	}
	
	/* determine the $HOME value */
	home = getenv(HOME);
	if (home == NULL || home[0] == '\0') {
		ent = getpwuid(geteuid());
		if (ent == NULL) {
			/* just assume . is home if all else fails */
			home = ".";
		}
		home = (char *)malloc(strlen(ent->pw_dir)+1);
		strcpy(home, ent->pw_dir);
	}

	/* determine the $PAGER value */
	pager = getenv(PAGER);
	if (pager == NULL || *pager == '\0') {
		pager = DEFAULTCALCPAGER;
	}

	/* determine the $SHELL value */
	shell = getenv(SHELL);
	if (shell == NULL)
		shell = DEFAULTSHELL;
}

void
givehelp(type)
	char *type;		/* the type of help to give, NULL => index */
{
	char *helpcmd;		/* what to execute to print help */

	/* catch the case where we just print the index */
	if (type == NULL) {
		type = DEFAULTCALCHELP;		/* the help index file */
	}

	/* form the help command name */
	helpcmd = (char *)malloc(
		sizeof("if [ ! -d \"")+sizeof(HELPDIR)+1+strlen(type)+
		sizeof("\" ];then ")+
		strlen(pager)+1+1+sizeof(HELPDIR)+1+strlen(type)+1+1+
		sizeof(";else echo no such help;fi"));
	sprintf(helpcmd, 
	    "if [ -r \"%s/%s\" ];then %s \"%s/%s\";else echo no such help;fi", 
	    HELPDIR, type, pager, HELPDIR, type);

	/* execute the help command */
	system(helpcmd);
	free(helpcmd);
}


/*
 * Interrupt routine.
 */
/*ARGSUSED*/
static void
intint(arg)
	int arg;	/* to keep ANSI C happy */
{
	(void) signal(SIGINT, intint);
	if (inputwait || (++abortlevel >= ABORT_NOW))
		error("\nABORT");
	if (abortlevel >= ABORT_MATH)
		_math_abort_ = TRUE;
	printf("\n[Abort level %d]\n", abortlevel);
}

/* END CODE */
