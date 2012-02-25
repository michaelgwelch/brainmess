#include <stdio.h>
#include "tape.h"
int main() {
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
    printf("%s\n", tape_to_string(tape)); 
}
