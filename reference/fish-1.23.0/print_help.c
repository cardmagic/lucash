
/** \file print_help.c
	Print help message for the specified command
*/

#include <stdlib.h>
#include <stdio.h>


#include "print_help.h"

#define CMD_LEN 1024

void print_help( char *c, int fd )
{
	char cmd[ CMD_LEN];
	int printed = snprintf( cmd, CMD_LEN, "fish -c '__fish_print_help %s >&%d'", c, fd );
	
	if( printed < CMD_LEN )
		system( cmd );
		
}
