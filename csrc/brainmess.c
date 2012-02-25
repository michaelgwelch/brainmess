#include <stdio.h>
#include <stdlib.h>
#include "tape.h"
#include "program.h"
#include "execute.h"

void putChar(gchar ch)
{
    putchar(ch);
}

gchar getChar()
{
    return getchar();
}

int main(gint argc, gchar** args) 
{
    if (argc != 2) exit(-1);
    FILE* file = fopen(args[1], "r");
    if (file == NULL) exit(-2);
    
    Tape* t = tape_new();


    GString* program_string = g_string_sized_new(200);
    const CHARS = 100;
    gchar* buffer = malloc(CHARS * sizeof(gchar));
    while(!feof(file))
    {
        size_t chars = fread(buffer, sizeof(gchar), CHARS, file);
        g_string_append_len(program_string, buffer, chars);
    }

    fclose(file);
    free(buffer);

    Program* prog = program_new(program_string->str);
    g_string_free(program_string, TRUE);

    while(!program_end(prog))
    {
        gchar instruction = program_fetch(prog);
        execute(instruction, prog, t, getChar, putChar);
    }

    program_free(prog);
    tape_free(t);

    printf("\n");

}
