Brainmess
=========

This project contains multiple implementations of interpreters for the language
I call [Brainmess](http://en.wikipedia.org/wiki/Brainfuck). (Note, this links
to the wikipedia article that describes the language and gives it's *real*
name. The real name may by NSFW.)

I've normally taken the approach that the tape should be "infinite" in
both directions and therefore I tend to use a linked list.

I've used this programming exercise during Refactoring study groups, with
an emphasis on clean code and testability. So there are generally several
levels of abstraction to allow for ease of testing. (In C#, there are 
interfaces; in Haskell I/O functions are passed in; in C I used
function pointers, etc.)
