Brainmess
=========

This project contains multiple implementations of interpreters for the language
I call [Brainmess](http://en.wikipedia.org/wiki/Brainfuck). (Note, this links
to the wikipedia article that describes the language and gives it's *real*
name. The real name may by NSFW.)

I've normally taken the approach that the tape should be "infinite" in
both directions and therefore I tend to use a linked list.

I've used this programming exercise during Refactoring study groups, with
an emphasis on clean code and testability rather than on 
efficiency. Therefore, the implementations tend to be more "verbose" than
the implementations discussed at the Wikipedia site.

This abstraction presents itself as interfaces an extension methods in C#.
In Haskell, I created my own class to break the module circular dependencies.
In C I used header files to define modules with different implementations for
production and test.
