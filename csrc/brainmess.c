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
    if (argc > 1) printf("%s\n", args[1]);   

    FILE* file = fopen(args[1], "r");
    // Todo: use GString to build up and read whole file
    int CHARS = 32000;
    gchar* program_string = malloc(CHARS * sizeof(gchar));
    size_t chars = fread(program_string, sizeof(gchar), CHARS, file);

    if (!feof(file)) printf("Could not read the whole program");
    fclose(file);

    Program* prog = program_new(program_string);
    while(!program_end(prog))
    {
        gchar instruction = program_fetch(prog);
        execute(instruction, prog, t, getChar, putChar);
    }


    
    Tape* tt = tape_new();
    Program* p = program_new("[]");
    execute('>', p, tt, getChar, putChar);
    execute('+', p, tt, getChar, putChar);
    printf("Enter a character\n");
    execute(',', p, tt, getChar, putChar);
    printf("Is this your character?\n");
    execute('.', p, tt, getChar, putChar);
    printf("%s\n", tape_to_string(tt));
	
    Tape* t1 = tape_new();
    Tape* t2 = tape_new();

    tape_increment(t1);
    tape_increment(t2);
    tape_move_forward(t1);
    tape_move_forward(t2);
    tape_move_backward(t1);
    tape_move_backward(t1);
    tape_move_backward(t2);
    tape_move_backward(t2);
    printf("Are equal 1? %s\n", tape_equals(t1, t2) ? "YES" : "NO");

    Tape* tape = tape_new();
    tape_increment(tape);
    tape_increment(tape);
    tape_increment(tape);
    tape_move_forward(tape);
    tape_increment(tape);
    tape_increment(tape);
    tape_move_backward(tape);
    tape_move_backward(tape);
    tape_increment(tape);
    tape_move_forward(tape);
    tape_move_forward(tape);
    printf("%s\n", tape_to_string(tape)); 

    Tape* tape2 = tape_new();
    tape_move_backward(tape2);
    tape_increment(tape2);
    tape_move_forward(tape2);
    tape_increment(tape2);
    tape_increment(tape2);
    tape_increment(tape2);
    tape_move_forward(tape2);
    tape_increment(tape2);
    tape_increment(tape2);

    printf("%s\n", tape_to_string(tape2));

    gboolean equal = tape_equals(tape, tape2);
    printf("Are equal? %s\n", equal ? "YES" : "NO");
    tape_free(tape);
    tape_free(tape2);

    


}
