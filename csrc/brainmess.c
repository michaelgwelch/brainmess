#include <stdio.h>
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
    Tape* t = tape_new();

    FILE* file = fopen(args[1], "r");
    // Todo: use GString to build up and read whole file
    
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
