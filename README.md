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

<table>
    <tr>
        <td>Directory</td><td>Description</td>
    </tr>
    <tr>
        <td>java</td><td>A Java implementation. A few unit tests.
        The Java LinkedList works a little stranger than I expected.
        I wrote a strange ListTraveler class to adapt the iterator
        to my needs. Opens up in Eclipse.</td>
    </tr>
    <tr>
        <td>csharp</td><td>A C# implementation. A lot of unit tests. At one
        point it was 100% code coverage. Some refactorings have left
        the coverage somewhat less. Opens up in Visual Studio 2010. The
        code projects will open in MonoDevelop as well. The test projects
        won't as they are MSTest. I'm considering switching to another
        code coverage tool so I don't need to use MSTest.</td>
    </tr>
    <tr>
        <td>haskell</td><td>A Haskell implementation. Some unit tests.
        Compiles with ghc. Just type ghc brainmess.hs to compile.</td>
    </tr>
    <tr>
        <td>csrc</td><td>A C implementation. No unit tests.</td>
    </tr>
</table>
