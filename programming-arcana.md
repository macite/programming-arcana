---
author:
- Andrew Cain
title: The Programming Arcana
---

::: titlepage
:::

# Preface {#cha:preface .unnumbered}

> [W]{.lettrine}[elcome]{.smallcaps} neophyte! So you seek to master the
> arts of Magic. Well, you have come to the right place. Here we will
> give you the opportunity to explore magic, to understand its working
> and use. Before we start I have one piece of advice for you, practice,
> for it is only through practice that you can hope to learn these
> secret arts.



# Building Programs {#cha:building programs}

> [M]{.lettrine}[agic]{.smallcaps} requires both knowledge and tools.
> Our first lesson will uncover the tools of the Magi. Tools that you
> will need to use to practice magic. Here, take this ancient wand this
> orb and caldron. Each of these tools is essential to the working of
> even the most basic magic. Now lets see if you can wield that wand.
> Take it in your hand like this, and ...

Software development requires both knowledge and tools. This first
chapter will introduce you to the tools used by software developers.
Tools that you will need to use to practice creating programs.

When you have understood the material in this chapter you will be able
to make basic use of a Text Editor, the Terminal, and command line tools
to create programs from source code.

## Concepts Related to Building Programs {#sec:concepts_related_to_building_programs}

In this chapter you will learn about the basic tools you need to use to
create your own programs. You will see an example program, and then use
these tools to convert that code into an executable program. You will
then be able to run the program you created, and see it perform the
tasks you coded.

This chapter provides a background on what programs are, and the general
processes of how they are created. This will introduce you to the tools
you will be using, and fill in some details of what they are doing and
how to use them. The topics covered include the following:

-   : This section introduces you to the idea of what a program is, and
    what it contains. You will need to be familiar with programs and
    what they are, as you will need to be able to create your own.

-   : Talks about the kinds of instructions the computer understands,
    and why it is not very productive to work at this level. You need to
    understand that this exists behind the scenes, but do not need to be
    overly familiar with it.

-   : The next level of language is called Assembly. It is very close to
    machine code, but much easier to understand and use. However, this
    is still too low a level to be very productive. Just like machine
    code, you only need to know this exists

-   : This is the level we are going to be working with in this book.
    This code is much easier to work with than assembly, and allows you
    to create your own programs reasonably quickly once you have learnt
    the basics. These are the tools you are going to be working with
    throughout this text.

-   : This is a command line environment that lets you issue text
    commands to the user. You will use this to create and run your
    programs.

-   : A *classic* program used to check that you have everything working
    correctly. This section shows you the code that you can use this to
    check that you have all the tools setup correctly, and to check
    their usage.

-   : This final section will show you how to use these tools to compile
    and run your own Hello World program.
    Figure [1.1](#fig:run-1-helloworld){reference-type="ref"
    reference="fig:run-1-helloworld"} shows the Hello World program
    running from the Terminal.

![Hello World run from the
Terminal](./topics/programs-and-compilers/images/HelloWorld.png){#fig:run-1-helloworld
width="70%"}

### Programs {#sub:what_is_a_program_}

If you are going to learn to develop software you will need to become
intimately aware of what a program is. After all, as a developer you
will be creating your own programs.

A program is a file that contains instructions that get the computer to
perform a task. Programs are lists of commands[^2] telling the computer
what to do, and the order in which to do it. Each instruction is very
simple, but they can be executed very quickly, allowing computers to
perform quite remarkable feats.

![A program contains instructions that command the computer to perform a
task](./topics/programs-and-compilers/diagrams/Program.pdf){#fig:what-is-a-program
width="90%"}

#### What happens when a program runs? {#ssub:what_happens_when_a_program_runs_}

When you run a program, regardless of how it is started, the Operating
System loads it from disk into memory and then starts it running. It is
important that the file you try to run is a program. These are *special*
files that contain instructions the computer can understand.

![Programs are loaded from disk into memory, then
run](./topics/programs-and-compilers/diagrams/ProgramExe.pdf){#fig:what-is-a-program-exe
width="90%"}

### Machine Code {#sub:machine_code}

*What instructions do Computers understand?*

Computers do not really *understand* anything, computers are
**unintelligent**. They are a machine that respond in a set way to a
given number of instructions. The instructions that a computer uses is
called its *instruction set* and contain instructions to perform basic
mathematic operations, loading and storing data in memory, comparing
numeric values, and moving to the new instruction elsewhere in the
program. These very simple actions are performed very quickly, and can
be use to create everything you have ever seen a computer do.

The computers instructions can be seen as binary numbers, numbers made
from 0's and 1's. These values are like switches that are either off (0)
or on (1). Setting these *switches* to different sequences will cause
the computer to perform different actions. For example, the *switch*
combination `0000 0011`, may cause the computer to add two numbers
together. Any time you want the computer to perform this task you set
the switches to that combination. These binary instructions are called
**machine code**.

![The computer responds to machine code
instructions](./topics/programs-and-compilers/diagrams/MachineCode.pdf){#fig:machine-code
width="80%"}

#### Programming in Machine Code {#ssub:programming_in_machine_code}

Listing [\[lst:machine code\]](#lst:machine code){reference-type="ref"
reference="lst:machine code"} shows a chunk of the machine code for a
small program. These 1s and 0s are the codes used to instruct the
computer when this program is executed. Programs can be written directly
in machine code, but this is a time consuming task. This is further
complicated by the fact that machine code is unique to each kind of CPU.
This means that programming at this level is entirely dependent on the
kind of processor that you are targeting.

``` {#lst:machine code caption="128 bits from the 106,752 bits of Machine Code from a small program." label="lst:machine code"}
...
0110 0111 0111 0010 0000 0000 0110 0011 0100 1110 0101 1111 0100 0001 0101 1000
0110 0111 0111 0010 0000 0000 0111 0110 0101 1111 0101 1111 0110 1001 0101 1111
...
```

No one wants to have to work at this level of details, and fortunately
you do not need to. Software developers have created tools to help them
create programs without having to think about these low level details.
These tools make it possible to work at a **higher level of
abstraction**. They take the code you write, and do the hard work of
converting that to the machine code of the computer you want to run it
on.

   Binary    Hex    Binary    Hex    Binary    Hex    Binary    Hex
  -------- ------- -------- ------- -------- ------- -------- -------
    0000    **0**    0001    **1**    0010    **2**    0011    **3**
    0100    **4**    0101    **5**    0110    **6**    0111    **7**
    1000    **8**    1001    **9**    1010    **A**    1011    **B**
    1100    **C**    1101    **D**    1110    **E**    1111    **F**

  : Binary to Hexadecimal

![A HexEditor allows you to view the machine code of any
program](./topics/programs-and-compilers/images/HexEditor.png){#fig:hex-editor
width="55%"}

### Assembly {#sub:assembly}

The next level of abstraction up from machine code is called
**Assembly**, or **Assembler Code**. Here the numeric machine code
instructions are given symbolic names that are, to some degree, more
understandable for humans. The code `0000 0011` may be given the
symbolic name `add`, for example.

Programs written in this language cannot be executed directly by the
computer, it isn't machine code. Assembler code is converted to machine
code by a program called an **Assembler**. This program reads the
instructions from the assembler code and outputs machine code. So, for
example, anywhere it encounters `add` in the code it can output
`0000 0011`.

![The computer responds to machine code
instructions](./topics/programs-and-compilers/diagrams/Assembly.pdf){#fig:assembly
width="80%"}

#### Programming in Assembly {#ssub:programming_in_assembly}

The code in Listing [\[asmcode\]](#asmcode){reference-type="ref+page"
reference="asmcode"} shows an example of some assembler code. This is
the assembler code that was used to generate the machine code from
Listing [\[lst:machine code\]](#lst:machine code){reference-type="ref"
reference="lst:machine code"}. The machine code was 13,344 bytes in
size, where the same program in assembler code is only 658 bytes. The
assembler reads these 658 bytes, combines it with instructions from
program libraries, and outputs machine code.

``` {#asmcode caption="Assembler Sample" label="asmcode"}
.cstring
LC0:
  .ascii "Hello\0"
  .text
.globl _main
_main:
  pushl	%ebp
  movl	%esp, %ebp
  pushl	%ebx
  subl	$20, %esp
  call	___i686.get_pc_thunk.bx 
"L000001$pb":
  leal	LC0-"L000001$pb"(%ebx), %eax
  movl	%eax, (%esp)
  call	L_write_line$stub
  addl	$20, %esp
  popl	%ebx
  popl	%ebp
  ret
```

From a programmer's perspective, assembler code is much easier to work
with than machine code, though there are still issues with the use of
assembler code. Firstly Assembly is bound to the instruction set of the
CPU that you are targeting, meaning that if you want to support other
kinds of CPU you will need to rewrite the program. The other main issue
with assembler code is that while it is more understandable, you are
still working with the primitive instructions of the CPU. Working at
this level takes considerable effort to write even simple programs.

Assembly languages were first developed in the 1950s, and were known as
a **Second Generation**[^3] programming languages. This step forward did
make programming easier, but the tools have advanced since then and now
we can work at an even higher level of abstraction.

### Source Code and the Compiler {#sub:source_code_and_the_compiler}

The next step in programming language evolution moved from machine level
instructions to something more human readable. These languages, known as
**Third Generation Languages**, use move advanced programs than
assemblers to convert their instructions into machine code. Programs
written in these languages have their code converted to machine code by
a **compiler**.

A **Compiler** is a program that converts **Source Code** into machine
code that is saved into an executable file called a *Program*. The
program can then be executed independent of the compiler and the source
code.

Internally, a compiler will perform a number of steps, as shown in
Figure [1.7](#fig:compiler){reference-type="ref"
reference="fig:compiler"}.

1.  **Preprocessing**: The code is read from your source code files.
    This may involve some processing of the text itself, which includes
    things like ignoring any comments in the code.

2.  **Compiling**: The code is then converted into assembly
    instructions, and an assembly program is output.

3.  **Assembling**: The assembly version of the program is converted
    into machine code, and stored in **object files**.

4.  **Linking**: In the final step the compiler uses a **Linker** to
    join together the machine code from your program, with other machine
    code you have used from the programming libraries. This then outputs
    an executable program.

![Compilers turn Source Code into Machine
Code](./topics/programs-and-compilers/diagrams/Compiler.pdf){#fig:compiler
width="70%"}

#### Programming with a Third Generation Language {#ssub:programming_with_a_third_generation_language}

Listing [\[lst:hello-world-c-1\]](#lst:hello-world-c-1){reference-type="ref"
reference="lst:hello-world-c-1"} and
Listing [\[lst:hello-world-pas-1\]](#lst:hello-world-pas-1){reference-type="ref"
reference="lst:hello-world-pas-1"} show two examples of source code.
This code describes a small program that can be used to output a message
to the .

::: multicols
2

``` {#lst:hello-world-c-1 .c caption="Example C++ code" label="lst:hello-world-c-1"}
#include "splashkit.h"

int main()
{
    write_line("Hello World!");
    return 0;
}
```

``` {#lst:hello-world-pas-1 .pascal caption="Example Pascal code" label="lst:hello-world-pas-1"}
program HelloWorld;
uses SplashKit;

begin
  WriteLine('Hello World!');
end.
```
:::

The code shown in
Listing [\[lst:hello-world-c-1\]](#lst:hello-world-c-1){reference-type="ref"
reference="lst:hello-world-c-1"} shows the code for the C++ program that
was used to generate the assembler code, and machine code shown in the
previous code listings. This code must be converted by the C++ compiler
into machine code before it can be run. It is interesting to note the
size of the C++ file: it is only 50 bytes! The compiler converts this 50
bytes into the 13,344 bytes of machine code.

Listing [\[lst:hello-world-pas-1\]](#lst:hello-world-pas-1){reference-type="ref"
reference="lst:hello-world-pas-1"} shows the same program written in the
Pascal programming language. Like its equivalent C++ code, this must be
compiled to create a program you can run.

Programs written in a third generation programming language are much
easier to understand than their assembler or machine code counterparts.
It is also possible that this code can be compiled to run on different
types of CPU, making it more portable. Most modern programming languages
are third generation programming languages.

The code that a programmer writes in these languages is called **Source
Code**. Typically source code is saved into a text file with a file
extension that helps identify the language it is written in. For
example, programs written in the C++ language are saved into files with
a `.c` file extension whereas Pascal programs are saved into files with
a `.pas` extension.

### Terminal {#sub:terminal}

Once you have written some source code, you need to be able to compile
it. This means, you need to run the **compiler**, and give it your
source code files to compile. The best way to do this when you really
want to learn about programming, is to run the compiler directly
yourself. To do this you needed to use a **Terminal** program.

The Terminal is a program that gives you command line access to the
computer. With command line access you can enter text commands to start
programs. These programs can output details back to the Terminal for you
to read, and interactive programs can also read input from you via this
same Terminal.

![The Terminal program gives you command line access to your
computer](./topics/programs-and-compilers/diagrams/Terminal.pdf){#fig:terminal
width="80%"}

#### The Shell {#ssub:the_shell}

The **Terminal** program itself just provides a text environment,
allowing text input and output. Within this environment a **Shell**
program is run to interpret your commands. This is an interactive
program that will display a prompt to you, at which you enter your
commands.

There are a number of different Shell programs, each of which has its
own set of instructions. The Shell we are going to use in this book is
called **Bash**. This shell program is available on Linux, Mac OS, and
Windows. As a Unix shell it is native for Linux and Mac OS, and with
Windows you can install **MSys2** to use these commands.

![The Terminal running
Bash](./topics/programs-and-compilers/images/Bash.png){#fig:bash
width="60%"}

A shell program is very simple. It provides a text prompt at which you
can enter commands. The Shell then reads the text you entered, and
performs an action based on the text you entered. You can use the Shell
to perform operations like copying and deleting files, and starting
programs.

#### Using Bash {#ssub:bash}

To get started using Terminal you will need to know some Bash commands,
see Table [1.1](#tbl:bash-commands){reference-type="ref"
reference="tbl:bash-commands"}.

::: {#tbl:bash-commands}
  **Action**                **Command**   **Description**
  ------------------------- ------------- -----------------------------------------------------------------------------------
  Change Directory          `cd`          Moves the shell to a different working directory.
  Print Working Directory   `pwd`         Outputs the current working directory.
  List Files                `ls`          Outputs a list of files.
  Copy File(s)              `cp`          Copies files from one location to another.
  Move File(s)              `mv`          Moves files from one location to another.
  Delete File(s)            `rm`          Removes files from the computer. There is no recycle bin with this, so take care!
  Create a Directory        `mkdir`       Makes a new directory.

  : Some bash commands to get you started
:::

To get started with Bash, you need to understand a little bit about the
**file system**. Each operating system needs a way of storing its files,
and there are going to be lots of files stored on a computer. This means
that it would be cumbersome to try and keep these all in one place.
Instead, the Operating System places files in **directories** (also
known as *Folders*). A directory can contain files, and other
directories.

When you are working in Bash, you will have a **working directory**.
This is the directory where Bash will start searching for the files you
are interacting with. To start working with the compiler you will need
to be able to use the **change directory** command to move to the
directory that contains your source code files.

With the **Change Directory** command you tell Bash which directory you
want to move into, with the different parts of this path being separated
by forward slashes (/). Example commands to move to your Documents
directory are shown in Table [1.2](#tbl:dirs){reference-type="ref"
reference="tbl:dirs"}, with screenshots for Linux in
Figure [1.10](#fig:linux-files){reference-type="ref"
reference="fig:linux-files"}, Mac OS in
Figure [1.11](#fig:mac-files){reference-type="ref"
reference="fig:mac-files"}, and Windows in
Figure [1.12](#fig:win-files){reference-type="ref"
reference="fig:win-files"}.

::: {#tbl:dirs}
  **Operating System**   **CD Command**
  ---------------------- -------------------------------
  *Linux*                `cd /home/uname/Documents`
  or                     `cd \sim/Documents`
  *Mac OS*               `cd /Users/uname/Documents`
  or                     `cd \sim/Documents`
  *Windows*              `cd /c/Users/uname/Documents`

  : CD command to move into your documents directory on various
  Operating Systems. In these examples `uname` should be replaced by
  your user name. The examples in
  Figure [1.10](#fig:linux-files){reference-type="ref"
  reference="fig:linux-files"},
  Figure [1.11](#fig:mac-files){reference-type="ref"
  reference="fig:mac-files"}, and
  Figure [1.12](#fig:win-files){reference-type="ref"
  reference="fig:win-files"} are for the user `acain`.
:::

![Changing directories in Linux
(Ubuntu)](./topics/programs-and-compilers/images/LinuxFiles.png){#fig:linux-files
width="90%"}

![Changing directories in
MacOS](./topics/programs-and-compilers/images/MacFiles.png){#fig:mac-files
width="90%"}

![Changing directories in
Windows](./topics/programs-and-compilers/images/WindowsFiles.png){#fig:win-files
width="\\textwidth"}

### The First Program: Hello World {#sub:hello world}

There is one very special program that all developers create. This is
the first program a software developer creates when they start using a
new language, or technology. It is the famous **Hello World**!

This is a very simple program, it runs and outputs the text 'Hello
World' to the Terminal. So, why is this the first program? It makes sure
that everything is set up correctly. If 'Hello World' does not work,
then there is something wrong with your setup you need to check.

Listings
[\[lst:hello-world-c\]](#lst:hello-world-c){reference-type="ref"
reference="lst:hello-world-c"} and
[\[lst:hello-world-pas\]](#lst:hello-world-pas){reference-type="ref"
reference="lst:hello-world-pas"} show the code for the 'Hello World'
program written with the C++ and Pascal programming languages. Both
programs result in the same output when run: they write the text 'Hello
World!' to the Terminal. They both use the same basic programming
structures, and they both go about performing the task in the same way.
At this stage, however, they are both just fancy text. What we need to
do is use a special tool to convert these into *programs*, we need to
**compile** them.

### Summary {#sub:compilers_summary}

This chapter has introduced the concepts related to the tools you need
to use to create programs. Computers can execute **machine code**, but
this isn't very friendly for people to read or create. **Assembly code**
provided the first layer of abstraction over machine code, giving
symbolic names to the machine code instructions. This was a little
easier to work with, but still required considerable work to create
programs of even simple functionality. Most modern programs are written
from **third generation languages** such as **C++** and **Pascal**. With
these languages the develop writes source code that is converted by a
**compiler** into machine code.

![Overview of the topics covered in this
chapter](./topics/programs-and-compilers/diagrams/Summary.pdf){#fig:compilers_summary
width="90%"}

## Using these concepts: Compiling a Program {#sec:using_these_concepts_compiling_a_program}

Now that the concepts have been presented, let us have a look at how
these can be used to create a program. We will take the code from Hello
World, and use a compiler to turn this code into a program that we can
then execute. This process is the same for large and small programs.

### Installing the Tools {#subs:install}

Before we get started you will need to install a few tools on your
computer. The good news is that all of the tools we need are free (and
many are open source - meaning you could one day look at the code used
to create these and contribute back to helping continue to make these
tools awesome).

The tools we will include all of the tools we have covered in this
chapter:

-   A **terminal** and **shell** used to run scripts, and

-   A **compiler** used to convert code into an executable program you
    can run,

-   Visual Studio Code: A **text editor** designed to work with code,

-   **SplashKit** - a set of tools and libraries that we can use to make
    more interesting programs as you start to learn to program.

Installation instructions for Linux, macOS, and Windows can be found at
<https://www.splashkit.io/articles/installation/>. Follow the
instructions for your operating system, and at the language step you can
choose to install either the C++ or Pascal compiler. Make sure to check
that each step succeeds, and if you get stuck you can ask questions on
the SplashKit forum (<https://splashkit.discourse.group>).

If you cannot get these working, then there is a Virtual Machine that is
also available that comes with all of the software preinstalled. You can
download this VM, and open and run it using something like VirtualBox
(<https://www.virtualbox.org>) (which is free), VMware
(<https://www.vmware.com>), or Parallels (<https://www.parallels.com/>).

### Creating a Project with the SplashKit Manager: skm {#sub:skm}

SplashKit is an open source set of tools and libraries that can help you
make more interesting applications as you learn to program. Within
SplashKit the SplashKit Manager (**skm**) is a command line tool
designed to take your compiler call, and add the extra options needed to
link it with the SplashKit library. So when you use **skm**, you add the
text 'skm' to the start of the instruction in the Terminal. Let's have a
look at how this works.

The best way to get started with a SplashKit program is to create a new
folder (directory) in your file system that will be used to store the
code and other files related to that program. The SplashKit manager can
be used to create an empty program file along with files that store
setings for Visual Studio Code.

Now that you have the project started, we can edit the code to make the
program do something interesting.

### Making the Hello World Program {#sub:compiling_code}

Listing [\[lst:hello-world-c\]](#lst:hello-world-c){reference-type="ref"
reference="lst:hello-world-c"} and
Listing [\[lst:hello-world-pas\]](#lst:hello-world-pas){reference-type="ref"
reference="lst:hello-world-pas"} show the source code for the Hello
World program. To make this into a program you need to:

1.  Write the code into the program code file.

2.  Save the file.

3.  Compile it.

Step 1 and 2 can be accomplished with any text editor, but the best ones
to use highlight your code. Each programming language has rules that
determine how its code must be structured, known as the language's
**syntax**. Programming editors, such as Visual Studio Code, make use of
this structure to help visually format the code to make it easier to
read. This is called **syntax highlighting**. This highlighting can help
you identify any little mistakes you make.

Figure [1.14](#fig:cpp-example){reference-type="ref"
reference="fig:cpp-example"} shows the Hello World code in Visual Studio
Code with C++, which has a consistent look across all platforms. The
figure also includes the command line instructions needed to setup a new
SplashKit C++ project, and compile it.

![Editing and Compiling C++ code in Visual Studio
Code](./topics/programs-and-compilers/images/CppExample.png){#fig:cpp-example
width="80%"}

Figure [1.15](#fig:fpc-example){reference-type="ref"
reference="fig:fpc-example"} shows the Hello World code in Visual Studio
Code, which has a consistent look across all platforms. The figure also
includes the command line instructions needed to setup a new SplashKit
Pascal project, and compile it.

![Editing and Compiling Pascal code in Visual Studio
Code](./topics/programs-and-compilers/images/FpcExample.png){#fig:fpc-example
width="80%"}

Use Visual Studio Code to open the **folder** that contains your
project. This will give you access to that project's files and configure
Visual Studio Code using the settings that skm has provided.

#### Compiling the Code {#ssub:running_the_compiler}

Once you have saved the code it is time to compile your program. For
this you are going to need switch back to the (or open a new one if you
have closed it - reemmber to change into the directory where you saved
the source code file).

When you run the compiler you need to give it two kinds of information:
options, and the names of the files to compile. The compiler will read
the code in the files you give it, and convert this to machine code as
shown in
Section [1.1.4](#sub:source_code_and_the_compiler){reference-type="ref"
reference="sub:source_code_and_the_compiler"} . The exact command you
use depends on the language and compiler you are running.

Once the code compiles, you will have a program you can run! Running the
program will load it into memory, and start it running through the steps
you coded. In this case, the HelloWorld program will output the text
'`Hello World!`' to the Terminal. To run the program you need to use its
file name. The command you need to enter is shown in
Listing [\[lst:run-hello-world\]](#lst:run-hello-world){reference-type="ref"
reference="lst:run-hello-world"}, and in
Figure [1.16](#fig:run-helloworld){reference-type="ref"
reference="fig:run-helloworld"}. The `./` before the file tells Bash to
look in the current directory for the program.

``` {#lst:run-hello-world .bash caption="Bash command to run HelloWorld" label="lst:run-hello-world"}
./HelloWorld
```

![Hello World run from the
Terminal](./topics/programs-and-compilers/images/HelloWorld.png){#fig:run-helloworld
width="70%"}

#### When things do not work {#ssub:when_things_do_not_work}

Compilers are very specific about the code you give it. If the source
code you try to compile does not follow all of the syntax rules of the
language then the compiler will fail, and end with an error message.
These errors, called **syntax errors**, could be as small as missing a
semicolon (;), or misspelling a name. To get your program to compile you
will need to correct any syntax errors the compiler finds in your code.

![These Terminals show some syntax errors from programs that are missing
a single semicolon
(;)](./topics/programs-and-compilers/images/SyntaxErrors.png){#fig:syntax-errors
width="80%"}

Figure [1.17](#fig:syntax-errors){reference-type="ref"
reference="fig:syntax-errors"} shows an example output caused by
removing a single semicolon from the Hello World program's code. The
numbers in the error messages give you an idea of where the compiler got
to when it encountered the error. So the text `hello-world.c:6:` output
from the C compiler indicates that the compiler got to line 6 before it
encountered the error. The text `HelloWorld.pas(2,1)` output from the
Pascal compiler indicates that it got up to line 2 character 1 before it
encountered the error.

When the compiler encounters these issues it does not create the
executable program. You need to learn to use these error messages to
help you locate errors in your code, so that you can fix them, and then
run the compiler again to generate your program.

Here are a couple of things to check if you have compiler errors with
the Hello World program:

-   Spaces in filename - avoid spaces in file names. The terminal uses
    spaces to separate different values in its instructions. In this
    context a space will then separate two values, rather than being
    captured as a space within the filename. If you do want a space use
    hyphens, or change case. For example `hello-world.cpp` or
    `HelloWorld.cpp` - there is no space here, but you can see the
    different words clearly.

-   Missing semicolons - this is easy to do. Check for a missing `;` at
    the end of the previous line.

-   Identifier not found - check the name you are using in the code.
    Most likely it is a small error. Compilers are often case sensitive,
    so WriteLine and writeline are different names.

-   If you get errors with the hello world code, double check against
    the code provided.

## Using the C++ Compiler {#sec:building_programs_in_c}

The C++ programming language is very powerful and flexible. In this
section you will see the tools that you need to start programming with
C++ on your computer.

### Hello World in C++ {#sub:hello_world_in_c}

When you follow the SplashKit install instructions, select the C++
language and install the clang++ or g++ compiler.

Once you have the compiler installed, you can create your first program:
the famous **Hello World** discussed in
Section [1.1.6](#sub:hello world){reference-type="ref"
reference="sub:hello world"}. The C++ code for this is shown in
Listing [\[lst:hello-world-c-c\]](#lst:hello-world-c-c){reference-type="ref"
reference="lst:hello-world-c-c"}. This code tells the computer to
'write' the text *Hello World!* to the Terminal, and follow it with a
new 'line'. Do the following to create this program for yourself, see
the notes below for hints:

1.  Open a Terminal

2.  Navigate to where you want to save your code, and create a new
    folder using `mkdir` and the project name (without spaces).

3.  Move into the project folder using the `cd` command.

4.  Use `skm` to create a new C++ project.

5.  Open Visual Studio Code, and open the **folder** that contains your
    project code.

6.  Type[^4] in the text below, making sure you get every character
    correct.

7.  Compile the program using `skm clang++ program.cpp -o HelloWorld`

8.  Run the program using `./HelloWorld`

Well done, you have now created and run your first C++ program!

## Using the Pascal Compiler {#sec:the_pascal_compiler}

The Pascal language is a great first programming language. It is both
powerful and clear in its syntax. In this section you will see the tools
that you need to get started with Pascal.

### Hello World in Pascal {#sub:hello_world_in_pascal}

Now that you have the compiler installed you can create your first
program: the famous **Hello World** discussed in
Section [1.1.6](#sub:hello world){reference-type="ref"
reference="sub:hello world"}. The Pascal code for this is shown in
Listing [\[lst:hello-world-pas-pas\]](#lst:hello-world-pas-pas){reference-type="ref"
reference="lst:hello-world-pas-pas"}. This code tells the computer to
'write' the text *Hello World!* to the Terminal. Do the following to
create this program for yourself, see the notes below for hints:

1.  Open a Terminal

2.  Navigate to where you want to save your code, and create a new
    folder using `mkdir` and the project name (without spaces).

3.  Move into the project folder using the `cd` command.

4.  Use `skm` to create a new Pascal project.

5.  Open Visual Studio Code, and open the **folder** that contains your
    project code.

6.  Type[^5] in the text below, making sure you get every character
    correct.

7.  Compile the program using `skm fpc -S2 program.pas`

8.  Run the program using `./HelloWorld`

Well done, you have now created and run your first Pascal program!

## Understanding Program Compilation {#sec:understanding_program_compilation}

An in depth look at how programs and compilers work is beyond the scope
of this book. Instead, this chapter focuses on two aspects that can help
you understand why we need compilers, and how these are able to help us.
These topics are , followed by .

### Levels of Abstraction {#sub:levels_of_abstraction}

Programs are very complicated. Trying to keep all of the details about
how it works in focus all of the time would take a large amount of
effort, and slow you down. As humans we have developed strategies for
dealing with these kinds of situations. What we do is create *layers* of
abstraction.

A layer, or level, of abstraction provides a means of managing
complexity at a similar level of detail. This means that all aspects of
that level are similar, and related to each other. When you are working
at a certain level of abstraction, you think in terms of the tools and
artefacts that exist within that layer.

These levels build on top of each other, with each layer being built on
top of the lower layer, and providing the foundation for the next higher
layer. Within a layer of abstraction you do not need to think about the
lower levels of abstraction, most of the time. However, it is always
good to know the details of at least one level of abstraction below the
level you are working at.

#### Programming Abstraction Levels {#ssub:programming_abstraction_levels}

Computers are unintelligent. This is one of the first, and most
important, things you need to understand when starting to create your
own programs. A computer is a machine that can be programmed using .
These binary commands instruct the computer to perform basic actions
such as adding numbers, reading values from memory, storing values in
memory, jumping to new instructions elsewhere in memory, and other
simple tasks. While this is a very low level, it is not the lowest level
of abstraction.

Below machine code, there is the abstraction for **binary**. This is the
idea that you can have a number system based on two digits, 0 and 1. The
machine code level builds on this idea, creating a set of instructions
that can be used to tell the computer what to do. Below binary there is
the concept of **gates**, which are themselves built on top of
**electronic circuits**, which in turn are made from **discrete
electronic components**. Even these low level electronic components are
based upon something else, but to discuss this we would need to delve
into the realm of **semiconductors** and **Quantum Mechanics**. From
machine code you can also work up to , and then to .

Whenever you are working on something you need to be able to work at
that level of abstraction, and have an understanding of the level below,
and the level above. For programming this means that you will need to
understand of the tools provided to you by the language (a level below),
and you need to understand the features and functions of the program you
are creating (a level above).

In this book the *Understanding* section of each chapter will help you
see how the concepts covered work at a lower level of abstraction. Often
this means these sections are very detailed. You should read these
sections to understand how the abstractions (concepts) covered in the
chapter work, at a level of abstraction lower than you will be working
at.

### Computers and Intelligent Behaviour {#sub:computers_and_intelligent_behaviour}

If computers are unintelligent, how can they do the awesome things they
do?

That is a good question. Computers are unintelligent, but programs are
not. The computer does not *know* what it is doing any more than a
bicycle *knows* how to move. It is following a set of steps that was
coded into the program being executed. These steps, however, were
created by people, software developers. When a computer does something
cool, its because a person told it to do that.

This is why it can be really great fun to be a software developer. You
get to make computers do things you want them to. This creativity can be
really exhilarating when you finally get your program to do exactly what
you want.

![Computers are unintelligent, any interesting behaviour comes from
programs created by software
developers](./topics/programs-and-compilers/diagrams/ProgramIntelligence.pdf){#fig:program-intelligence
width="\\textwidth"}

## Exercises on Building Programs {#sec:examples_and_exercises_on_building_programs}

### Concept Questions {#sub:concept_questions_compiler}

Read over the concepts in this chapter and answer the following
questions:

1.  What is a ? What does it contain?

2.  What is ?

3.  Can you write programs in Machine Code? Explain any issues
    associated with this.

4.  What is ?

5.  Can you write programs in Assembly? Explain any issues associated
    with this.

6.  What tool is used to convert Assembly to Machine Code?

7.  What is Source Code?

8.  What are the advantages of programming at a Source Code level?

9.  What tool is used to convert Source Code to Machine Code?

10. Why do you need to convert Source Code and Assembly to Machine Code?

11. What is the ?

12. What is Bash?

13. Which command is used to change directories in Bash?

14. With Bash, how can you check which directory is the current working
    directory?

15. How can you list all of the files in the current directory?

16. Why program 'Hello World'? What does it do for you?

17. What is the name of the C or Pascal compiler we will be using in
    this book?

18. How intelligent is your computer?

### Code Writing Questions: Applying what you have learnt {#sub:code_writing_questions_applying_what_you_have_learnt_compiler}

Apply what you have learnt to the following tasks:

1.  Install the following programs:

    1.  A **Text Editor** that offers syntax highlighting for the
        language you are using. We recommend Visual Studio Code.

    2.  The **Compiler** for the language you are using.

    3.  **Bash** and the **Terminal** (on Windows only - this will
        already be available on Linux and Mac).

2.  Enter the code, compile, and run the 'Hello World' program.

3.  Try changing the code to output a different message. After you have
    made the changes you will need to re-compile and run the program.

### Extension Questions {#sub:extension_questions_compiler}

If you want to further your knowledge in this area you can try to answer
the following questions. The answers to these questions will require you
to think harder, and possibly look at other sources of information.

1.  What is the difference between a compiler and an interpreter? How
    are programs of interpreted languages, such as Python, executed?

2.  If you compile a program for an Intel CPU on UNIX, can you run this
    on a UNIX machine that has a PowerPC CPU?

3.  If you compile a program on a Windows machine can you run the
    resulting program on a UNIX machine with the same CPU architecture?

4.  What is a fat, or Universal, binary?

5.  What is a Virtual Machine?

# Program Creation {#cha:program_creation}

> [C]{.lettrine}[asting]{.smallcaps} spells crafted by others is
> alright, but the true power of magic can only be realised by crafting
> your own spells. You have done well mastering the tools, so now let us
> turn our attention to the study of the arcane knowledge of spell
> craft. To create your own spells you need to know...

Compiling programs crafted by others is alright, but the true power of
programming can only be realised by learning to craft your own programs.
Chapter [1](#cha:building programs){reference-type="ref"
reference="cha:building programs"} introduced you to the tools you need
to compile programs from source code, so now we can turn our attention
to the study program creation.

This chapter introduces the artefacts used to create programs, and how
you can code these using a programming language. You will start by
learning to create simple programs that output information to the
Terminal, and then we will look at making use of SplashKit to create
some more interesting programs.

When you have understood the material in this chapter you will be able
to write the code needed to declare a program, and within this program
create your own custom procedures to perform simple tasks. You will be
able to convert this code into an executable file using a compiler, and
then run the program you created. You will have made those first
important steps in your journey to master this arcane knowledge.

## Program Creation Concepts {#sec:program_creation_concepts}

Our first program is going to display some text to the Terminal. In this
section you will be introduced to the programming artefacts and
terminology you will need to use to create this program. This first step
is important and will require you to have installed a C++ or Pascal
compiler, see Chapter [1](#cha:building programs){reference-type="ref"
reference="cha:building programs"} for instructions.

A programming **artefact** is something that can be created and used
within your code. In this chapter we will look at creating programs, and
using a number of other artefacts. The following artefacts will be
covered in this chapter:

-   : A program is a sequence of instructions that when compiled creates
    an executable file that a user can run.

-   : A procedure is a named sequence of instructions that will get the
    computer to perform a task. When you want the task performed you can
    call the procedure.

-   : The program can use code from other Libraries. These libraries
    contain reusable Procedures and Types.

-   : A type defines how data is interpreted by the program. The
    programming language will support a number of basic types by
    default, and libraries can add other types.

In addition to these artefacts, you will need to understand some
programming **terminology**. The following terms are discussed in this
section:

-   : An **instruction** within the program.

-   : A **value** used in a statement.

-   : The **name** of an artefact.

This section also introduces the following kinds of instructions. You
can use these to get the computer to perform certain **actions** within
your program.

-   : The instruction to run a procedure.

We can then use these concepts, artefacts, and instructions to create a
program that will write some text to the Terminal as shown in Figure
[2.1](#fig:program-creation-helloworld){reference-type="ref"
reference="fig:program-creation-helloworld"}.

![Hello World run from the
Terminal](./topics/program-creation/images/HelloWorld.png){#fig:program-creation-helloworld
width="80%"}

### Program {#sub:program}

In most software projects the top level *artefact* you are aiming to
create is a **program**. Within your software a program is a list of
instructions the computer will perform when that program is run on the
computer.

When you create a program in your code you should be thinking about the
tasks you want the program to achieve, and the steps you must get the
computer to perform when the program is run. These then become the
instructions within the program, with each instruction being a of what
you want performed.

![A program contains instructions that command the computer to perform
actions](./topics/program-creation/diagrams/BasicProgramConcept.pdf){#fig:program-creation-program
width="\\textwidth"}

### Statement {#sub:statement}

When you are created a program you define the actions the computer will
perform when the program is run. Each of these *actions* is coded as a
**statement** within the program. This style of programming is known as
**imperative** programming. Imperative means to give authoritative
commands, and that is what we do in our programs. Our programs are lists
of *authoritative commands*, statements, that *tell* the computer the
actions it is to perform.

![A statement is a command for the computer to perform an
action](./topics/program-creation/diagrams/Statement.pdf){#fig:program-creation-statement
width="\\textwidth"}

### Procedure Call {#sub:procedure call}

A procedure call is a kind of that instructs the computer to run the
code in a . This statement uses the procedure's name to identify the
procedure that must be run. If the procedure called requires some data,
this data is *passed* to the procedure as part of the procedure call.

![A procedure calls runs a procedure, passing in values for the
procedure to
use](./topics/program-creation/diagrams/ProcedureCall.pdf){#fig:program-creation-procedure call
width="\\textwidth"}

### Procedure {#sub:procedure}

The computer is unintelligent, so performing anything meaningful
requires a large number of instructions. Coding all of these directly in
the program would be slow and time consuming. To avoid this programming
languages offer the capability to group the instructions to perform a
task into a **procedure**.

A procedure is a list of instructions that gets the computer to perform
a specific task. When a procedure is called it gets control of the
computer and instructs it to perform the steps needed. Often these steps
require data, so the procedure may need to be passed data when it is
called. When the procedure finishes its task, control returns back to
the code that called the procedure.

![A procedure contains instructions to perform a task, and may need to
be passed data in order to do
this](./topics/program-creation/diagrams/Procedure.pdf){#fig:program-creation-procedure
width="90%"}

### Expression {#sub:expression}

Some statements need data, this data can be calculated or provided as a
literal value in the code. The term **expression** is used in
programming to describe the places in a statement where data must be
supplied. At run time each expression becomes a value that is used by
the statement.

![An expression provides a **value** to be used in a
Statement.](./topics/program-creation/diagrams/Expression.pdf){#fig:program-creation-expression
width="\\textwidth"}

### Literal {#sec:program-creation-literal}

A Literal is a whole, or part of, an where the value is entered directly
into the code.

![Concepts related to
Literals.](./topics/program-creation/diagrams/Literal.pdf){#fig:program-creation-literal
width="80%"}

### Type {#sub:type}

All values within a program will have a **type**. The type indicates how
the data stored in the computers memory is interpreted by the program.
There are three basic data types available in a programming language, as
shown in Figure [2.8](#fig:program-creation-type){reference-type="ref"
reference="fig:program-creation-type"}.

-   **Textual** data such as '*Fred*', '*Hello World*', '*23*', and
    '*This is text!*'.

-   **Whole numbers** such as *1*, *0*, *-5*, and *37*.

-   **Real numbers** such as *0.5*, *-126.0*, *3.141516*, and *23.981*.

![A types define how values are interpreted and the operations that can
be performed on the
data.](./topics/program-creation/diagrams/Type.pdf){#fig:program-creation-type
width="90%"}

### Identifier {#sub:identifier}

An identifier is the technical term for the name/word that *identifies*
something for the compiler. These can be the **name** of a programming
artefact (such as a Program, Library, or Procedure) or words that have
special meaning for the compiler. You will use identifiers to name the
artefact you create, and to select the artefact you want to use.

![An Identifier is the name of a programming artefact such as a Program,
Library, or
Procedure.](./topics/program-creation/diagrams/Identifier.pdf){#fig:program-creation-identifier
width="\\textwidth"}

### Library {#sub:library}

A library is a collection of reusable code artefacts. Each programming
language has its own library, and your programs can make use of the code
available in this library.

![A library contains code that can be used by your
Program](./topics/program-creation/diagrams/Library.pdf){#fig:program-creation-library
width="\\textwidth"}

### Comments {#sub:comments}

A program's source code contains instructions for the actions the
computer must perform. However, this code is written and maintained by
people. It is often useful to be able to place comments in the code to
help someone reading that code understand how the code works or what it
is trying to achieve. This text is not something that should be
translated into machine code.

Programming languages support the ability for programmers to embed
*comments* into the source code that are ignored by the compiler.

### Procedure Declarations {#sub:proc_decl-procedure_declarations}

Procedures contain code that define the steps the computer performs when
the procedure is called. In your Program you can define your own
Procedures, allowing you to divide a program's tasks into separate
Procedures.

![Procedure
Declaration](./topics/program-creation/diagrams/ProcedureDeclaration.pdf){#fig:procedure-decl-procedure-decl
width="\\textwidth"}

### Summary {#sub:program_creation_concepts_summary}

This section has introduced a number of programming artefacts, some
programming terminology, and one kind of instruction. An overview of
these concepts is shown in Figure
[2.12](#fig:program-creation-summary){reference-type="ref"
reference="fig:program-creation-summary"}. The next section will look at
how you can use these concepts to design some small programs.

![Key Concepts introduced in this
Chapter](./topics/program-creation/diagrams/Summary.pdf){#fig:program-creation-summary
width="\\textwidth"}

## Using these Concepts {#sec:using_these_concepts_program_creation}

Armed with the knowledge you have gained in the Section
[2.1](#sec:program_creation_concepts){reference-type="ref"
reference="sec:program_creation_concepts"} you can now start to make
your own small program.

### Designing Output Test {#sub:designing_hello_world}

Our first programming task is to extended the classic 'Hello World'
program to also output some data values. We will call this program
**Output Test**. This program will allow you to see all of the different
concepts from this chapter in action. A description of the program is
shown in Table
[2.1](#tbl:program-creation-hello world description){reference-type="ref"
reference="tbl:program-creation-hello world description"}, and a sample
execution is shown in Figure
[2.13](#fig:program-creation-helloworld2){reference-type="ref"
reference="fig:program-creation-helloworld2"}.

::: {#tbl:program-creation-hello world description}
  **Program Description**   
  ------------------------- ------------------------------------------------------------------------------------
  **Name**                  *Output Test*
                            
  **Description**           Displays the text **'Output Test Program!'** on the Terminal, followed by
                            **' 1 + 1 = '** and the result of the calculation 1 + 1, and then
                            **' Area of a circle with radius 3 = '** and the result of calculating this value.

  : Description of the *Output Test* program.
:::

![Output Test executed from the
Terminal](./topics/program-creation/images/HelloWorld.png){#fig:program-creation-helloworld2
width="80%"}

To design and implement this program we need to follow a number of
steps:

1.  Understand the problem, and get some ideas on the tasks that need to
    be performed.

2.  Choose the artefacts we will create and use.

3.  Map these artefacts to code.

4.  Compile and run the program.

### Understanding Output Test {#sub:understanding_output_test}

The first step in creating a program is to analyse the problem, and find
related material that we can use to make sure we know what the computer
needs to do. You need to understand what the program needs to do before
you can start to design the solution.

For this program you need to understand how to calculate the answer of
the equation 1 + 1 (which is a trivial task), and also how to calculate
the area of a circle `a`, given its radius `r`. A quick search of the
internet and you can find the equation is $a=\pi r^2$. Using these
equations you have the knowhow needed to calculate the values needed to
be output.

### Choosing Artefacts for Output Test {#sub:choosing_artefacts_hello_world}

Designing a program is all about making decisions around how to organise
the program's instructions. This means **deciding** on which artefacts
you will **create**, and which you will **use**. This chapter has
already introduced the concept of **creating** a , and **using** s. With
these tools it is possible to design simple programs that will get the
computer to perform actions by running procedures in sequence.

Let us return to the design of the *Output Test* program. The program
will require:

-   The **creation** of a . This program will contain the instructions
    to write the three messages to the terminal.

-   The **use** of a procedure to write to the terminal. Programming
    languages provide a standard that, amongst other things, contains
    procedures to write data to the terminal. This means you can call
    one of these procedures, passing it the data you want written to the
    Terminal, and let it take care of the details of how this is done.

The design for this program can be documented using a programming
language neutral **pseudocode**[^6]. This is a structured textural
description of how the program works that is independent of the
programming language used to implement it.

Listing
[\[lst:program-creation-hello-pseudo\]](#lst:program-creation-hello-pseudo){reference-type="ref"
reference="lst:program-creation-hello-pseudo"} shows the pseudocode for
the *Output Test* program. This represents the **program** that needs to
be created, and shows the three **procedure calls** that need to be
made.

### Writing the Code for Output Test {#sub:writing_the_code_for_output_test}

The pseudocode in Listing
[\[lst:program-creation-hello-pseudo\]](#lst:program-creation-hello-pseudo){reference-type="ref"
reference="lst:program-creation-hello-pseudo"} contains the instructions
that will get the computer to perform the actions needed by the *Output
Test* program. These pseudocode instructions are not in a form that can
be used by the computer, which can only use machine code. To make this
into a program the pseudocode must be translated into source code, and
then to compile this into machine code.

Section [2.3](#sec:program-creation-in-c){reference-type="ref"
reference="sec:program-creation-in-c"} and
Section [2.4](#sec:program-creation-in-pas){reference-type="ref"
reference="sec:program-creation-in-pas"} , contain a description of the
syntax needed to create programs using the C and Pascal programming
languages. Each section outlines how to write the code so that it can be
understood by a compiler. The programming languages are expressed as a
number of related syntax rules. You will find the rules that you need to
create a program, the rules for calling a procedure, and other related
rules.

In this book the syntax rules are expressed using **syntax diagrams**.
An example is shown in
Figure [\[synt:basic-rule\]](#synt:basic-rule){reference-type="ref"
reference="synt:basic-rule"}. This diagram shows the syntax related to
two rules, *first rule*, and *second rule*, and shows the four main
parts of all the syntax diagrams.

1.  Text found at the start of a line (not contained in a box) is the
    name of a rule. There are two rules in Figure
    [\[synt:basic-rule\]](#synt:basic-rule){reference-type="ref"
    reference="synt:basic-rule"}: *first rule*, and *second rule*.

2.  Arrows show the order in which the parts of the rule are applied.
    They start at the rule name, and point in the direction you need to
    follow. Each box pointed to by an arrow represents either another
    rule to apply, or the text that must written.

3.  Rectangular boxes (nodes) on a line indicate points where other
    rules need to be applied. For example, the node ; within the *first
    rule* indicates that you **must** apply the *second rule* at this
    point.

4.  Boxes with rounded corners represent text that must be entered into
    the code. For example, the node ; within the *first rule* indicates
    that you **must** write the text '*write in the code*' at this point
    in your code.

In order to make use of these syntax diagrams, you must know what it is
that you *want* to write in code. The rules in
Figure [\[synt:basic-rule\]](#synt:basic-rule){reference-type="ref"
reference="synt:basic-rule"} indicate that you can write either a *first
rule* or a *second rule*. If you want to write a *first rule* you find
that rule in the diagram, and then follow its arrows. Reading the *first
rule* indicates that the *second rule* must be applied first.

The *second rule* tells you that you must write the text '*stuff to*'.
The vertical bar at the end of the line indicates the end of the *second
rule*. This means at this stage the code is:

::: verb
stuff to
:::

Having finished the *second rule*, you can return back to finish the
*first rule*. This indicates that you need to write '*write in the
code*' in the code. This is the last part of the *first rule*, and so
the code needed to write a *first rule* from the syntax diagram in
Figure [\[synt:basic-rule\]](#synt:basic-rule){reference-type="ref"
reference="synt:basic-rule"} is shown below.

::: verb
stuff to write in the code
:::

For a more realistic example have a look at the syntax diagram in
Figure [\[synt:example-identifier\]](#synt:example-identifier){reference-type="ref"
reference="synt:example-identifier"}.[^7] This shows the rules you need
to follow to code an [^8] in either C or Pascal.

There are three rules in Figure
[\[synt:example-identifier\]](#synt:example-identifier){reference-type="ref"
reference="synt:example-identifier"}, including the **identifier** rule
itself and rules for **letter** and **digit**. These rules show arrows
that give you *options*, and the ability to *repeat* parts of the rules.

-   A **letter** is one alphabetic character: i.e. one of 'A' to 'Z' or
    'a' to 'z'. This is an example of options in the syntax, where you
    follow **one** of the available arrows.

-   A **digit** is a single number: i.e. a number between '0' and '9'.

-   The **identifier** has a more complicated rule, with the following
    parts:

    1.  The first thing in an must be either a *letter* or an underscore
        ( \_ ).

    2.  Next you have the option of following the top line and ending
        the identifier, or following the downward arrow and including
        other letters, numbers, and underscores.

    3.  Following the downward arrow you have a new option where you can
        choose to have either a *letter*, a *digit*, or an underscore as
        the second character in your identifier.

    4.  Continuing after this option you have another option where you
        can return back to repeat the previous step, allowing you to
        have identifiers with more than one or two characters.

#### Using the Syntax Diagrams {#ssub:using_the_syntax_diagrams}

Syntax diagrams can help you to map a concept to actual code that needs
to be written in your source code. To use these diagrams you must first
know *what* it is that you want to create or use, and then you can *look
up* the related syntax. This is where pseudocode code comes into play.
It contains a description of the things that need to be created.

Listing
[\[lst:program-creation-hello-pseudo3\]](#lst:program-creation-hello-pseudo3){reference-type="ref"
reference="lst:program-creation-hello-pseudo3"} shows the pseudocode for
the Output Test program. This tells you what needs to be created; you
need to create a . This means you need to find the syntax that tells you
the rules of how a *Program* is written in a programming language.

When you write the code for the program you will need to know the
actions you want the computer to perform. The pseudocode in
Listing [\[lst:program-creation-hello-pseudo3\]](#lst:program-creation-hello-pseudo3){reference-type="ref"
reference="lst:program-creation-hello-pseudo3"} indicates that you need
to output text and calculated values to the terminal; this can be
achieved using s. You can lookup the language syntax for a procedure
call, and use this to write the required code.

#### How the Syntax is Presented {#ssub:how_the_syntax_is_presented}

The C and Pascal Syntax needed to create a program are shown in Section
[2.3](#sec:program-creation-in-c){reference-type="ref"
reference="sec:program-creation-in-c"} and Section
[2.4](#sec:program-creation-in-pas){reference-type="ref"
reference="sec:program-creation-in-pas"} . Each part of the Syntax is
presented on its own page that shows a syntax diagram followed by an
example and some notes. The best way to approach this is to do the
following:

1.  Find the page with the syntax rule you are interested in knowing
    about.

2.  Have a quick look at the **syntax diagram** and the rules it
    contains. Read each rule, and get a basic feel for how it is going
    to come together for your program.

3.  Read the **example** to see one way of using the rule. A syntax
    diagram can be used to create any number of variations of the rule.
    However examples show you at least one way these rules can be coded.

4.  Return to the diagram and make sure you can match each part of the
    example back to the rule that created it.

5.  Now look up any related rules that are not explained on this rule's
    page. For example, a will use the rule to code its instructions. The
    actual rule for a Statement will have its own page. When you read
    the rules for a Program you will need to also find the page with the
    rules for a Statement so that you know how to code these within the
    program.

As you follow this process it is also a good idea to take notes and to
try to use these rules in your own programs. Have your code editor open,
and see if you can follow the rules or mimic the examples to start
building your own program's code. You can also try typing in some of the
examples to see how they work.

Learning to program, and learning a language's syntax, takes time and
practice (like juggling). Reading about these concepts is one thing, but
being able to successfully apply this ideas is something different. Make
sure that you practice using these concepts and syntax.

### Compiling and Running Output Test {#sub:compiling_and_running_output_test}

Previous sections have shown pseudocode for the *Output Test* program.
These steps can be coded, using either C or Pascal, into a source code
file. In order to actually use these instructions you will need to
**compile** the source code file. This will produce an executable file
that you can run. To do this you can use the terminal and the command
line compiler as shown in
Chapter [1](#cha:building programs){reference-type="ref"
reference="cha:building programs"}.

-   On Ubuntu **Linux** you can find the Terminal in the **Accessories**
    folder within **Applications**. See Figure
    [2.14](#fig:program-creation-ubuntu-terminal){reference-type="ref"
    reference="fig:program-creation-ubuntu-terminal"}.

-   On **MacOS** you can find the Terminal in the **Utilities** folder
    within **Applications**. See Figure
    [2.15](#fig:program-creation-macos-terminal){reference-type="ref"
    reference="fig:program-creation-macos-terminal"}.

-   On **Windows** you will need to download and install *MSys2*, follow
    the steps in the SplashKit installer. The *Msys2 Shell* is then the
    equivalent of Terminal on the other operating systems. You will find
    this in **Program Files**, **MSys2**. See Figure
    [2.16](#fig:program-creation-mingw-shell){reference-type="ref"
    reference="fig:program-creation-mingw-shell"}.

![Terminal on Ubuntu
Linux](./topics/program-creation/images/UbuntuTerminal.png){#fig:program-creation-ubuntu-terminal
width="60%"}

![Terminal on
MacOS](./topics/program-creation/images/MacOSTerminal.png){#fig:program-creation-macos-terminal
width="60%"}

![MinGW Shell (Terminal) for
Windows](./topics/program-creation/images/MinGWShell.png){#fig:program-creation-mingw-shell
width="60%"}

Once you are in the Terminal you have the ability to run a number of
text based commands. These commands instruct the computer to perform
actions for you.

-   **pwd** stands for *Present Working Directory*, and shows you where
    you are in the file system.

-   **ls** stands for *List* and it prints out a list of the files that
    are in the current directory.

-   **cd** stands for *Change Directory* and it moves you to another
    directory.

To compile your program you need to do the following:

1.  Change into the directory where your code is located using the
    **cd** command. For example, if your code is in a *Code* folder in
    your *Documents* folder you would use:

    -   **Linux**: `cd /home/username/Documents/Code`

    -   **MacOS**: `cd /Users/username/Documents/Code`

    -   **Windows**: (in MinGW Shell)
        `cd /c/Users/username/Documents/Code`[^9]

2.  Run the compiler, passing in the name of the file you want to
    compile. See the language specific notes below

3.  Execute the program using `./OutputTest`

Figure
[2.17](#fig:program-creation-complete-linux-version){reference-type="ref"
reference="fig:program-creation-complete-linux-version"} shows an
example of the instructions needed to compile and run the C version of
the *Output Test* program on Linux. Figure
[2.18](#fig:program-creation-complete-linux-version-1){reference-type="ref"
reference="fig:program-creation-complete-linux-version-1"} shows an
example of the instructions needed to compile and run the Pascal version
of the *Output Test* program on Linux.

![Example of compiling and running a C version of Output Test on
Linux](./topics/program-creation/images/LinuxCompleteExample1.png){#fig:program-creation-complete-linux-version
width="62%"}

![Example of compiling and running a Pascal version of Output Test on
Linux](./topics/program-creation/images/LinuxCompleteExample.png){#fig:program-creation-complete-linux-version-1
width="62%"}

Figure
[2.19](#fig:program-creation-complete-macos-version){reference-type="ref"
reference="fig:program-creation-complete-macos-version"} shows an
example of the instructions needed to compile and run the C version of
the *Output Test* program on MacOS. Figure
[2.20](#fig:program-creation-complete-macos-version-1){reference-type="ref"
reference="fig:program-creation-complete-macos-version-1"} shows an
example of the instructions needed to compile and run the Pascal version
of the *Output Test* program on MacOS.

![Example of compiling and running a C version of Output Test on
MacOS](./topics/program-creation/images/MacOSCompleteExample1.png){#fig:program-creation-complete-macos-version
width="73%"}

![Example of compiling and running a Pascal version of Output Test on
MacOS](./topics/program-creation/images/MacOSCompleteExample.png){#fig:program-creation-complete-macos-version-1
width="73%"}

Figure
[2.21](#fig:program-creation-complete-windows-version){reference-type="ref"
reference="fig:program-creation-complete-windows-version"} shows an
example of the instructions needed to compile and run the C version of
the *Output Test* program on MacOS. Figure
[2.22](#fig:program-creation-complete-windows-version-1){reference-type="ref"
reference="fig:program-creation-complete-windows-version-1"} shows an
example of the instructions needed to compile and run the Pascal version
of the *Output Test* program on MacOS.

![Example of compiling and running a C version of Output Test on
Windows](./topics/program-creation/images/WindowsCompleteExample.png){#fig:program-creation-complete-windows-version
width="73%"}

![Example of compiling and running a C version of Output Test on
Windows](./topics/program-creation/images/WindowsCompleteExample1.png){#fig:program-creation-complete-windows-version-1
width="73%"}

#### Compiler Errors {#ssub:compiler_errors}

A compiler is a very sensitive piece of software; they require that you
follow the language's syntax *precisely*. One small mistake, and the
compiler will fail to compile your program and end with an error
message. This is a *good* thing! (Although it may not feel like it at
first.) Why? Because when programs become complicated we need the exact
details to help us, and to assure the program works the way we need it
to.

Here are some handy hints related to dealing with compiler errors.

1.  **Know that errors are inevitable**. You will get compiler errors!
    As you gain more experience you will get fewer errors, but it will
    always be a rare event that everything is exactly as it should be
    the first time.

2.  **Error messages may appear cryptic at first**. In many cases the
    compiler's error messages can appear cryptic, you need to learn to
    decode the messages the compiler gives you.

3.  **Start with the [first]{.underline} error message, and do not move
    on until it is fixed**. The compiler will read your code 'top down'.
    So the first error it outputs will be the first in the file. The
    problem is that the compiler will try to continue on, despite the
    error. This can mean that other errors further on in the output may
    actually be caused by the compiler being *confused* due to the first
    error. By fixing the first error you may also fix subsequent errors.

4.  **Deal with [one]{.underline} error at a time**. It is easy to feel
    overwhelmed when you see a huge list of errors, but do not be scared
    off! Start at the first error, and solve them one by one. Compile
    after fixing each problem to see if the others still exist.

5.  **Do not add more code until the errors are fixed**. Compile your
    program frequently. Add small pieces of functionality, and then fix
    any syntax errors before adding the next piece of functionality.
    Coding in this way makes sure that you do not get too many errors,
    and reduces the code you have to search in order to fix the problem.

6.  **Read the error message [carefully]{.underline}**. The message will
    try to tell you what has gone wrong, and once you understand what
    the messages are trying to say you will be able find and fix the
    issues quickly.

7.  **Work out what the error messages mean**. Understanding the error
    messages will mean that you will know what to look for, and will
    help you avoid these errors in the future. If your not sure what an
    error message means ask others developers or search for the message
    on the internet.

8.  **Find the [line]{.underline} and [character number]{.underline} of
    the error**. One important detail in the error message will be the
    line number of the error. This gives you a starting point to help
    you locate the problem. It is important to note that this is where
    the compiler got to when it noticed the error - this *does not* mean
    this is where the error actually is (but it is a good place to start
    looking). You may need to look back one or more lines to find the
    actual source of the problem.

9.  **Watch for typos**. It is easy to mistype an identifier. When this
    happens the compiler will not know what to do (and it will complain
    about you trying to use 'unknown' things). Make sure you check for
    these tiny typos when you get errors related to the compiler not
    being able to find an identifier.

10. **When you get stuck [ask for help]{.underline}**. Compiler errors
    are likely to be something small, but sometimes the causes are hard
    to find. If you get stuck, *ask for help*! Having access to other
    more experienced developers will be a valuable resource as you learn
    to program. Learning to program can be tough at times, and having
    someone who can help you when you get stuck will make all the
    difference.

## Program Creation in C++ {#sec:program-creation-in-c}

Section
[\[sec:using_these_concepts_program_creation\]](#sec:using_these_concepts_program_creation){reference-type="ref+page"
reference="sec:using_these_concepts_program_creation"} of this chapter
introduced an 'Output Test' program, and its design. The pseudocode from
this section is shown in Listing
[\[lst:program-creation-hello-pseudo 1\]](#lst:program-creation-hello-pseudo 1){reference-type="ref"
reference="lst:program-creation-hello-pseudo 1"}. In this Section you
will see the rules for translating this program's design into the C code
shown in Listing
[\[lst:program-c-output_test\]](#lst:program-c-output_test){reference-type="ref"
reference="lst:program-c-output_test"}.

### C++ Program (with Procedures) {#sub:c_program_with_procedures_}

[\[sub:program_in_c\]]{#sub:program_in_c label="sub:program_in_c"}

C++ does not have an explicit Program artefact. Rather, you create a
program by having a function called '`main`' in your code. Figure
[\[csynt:procedure-decl-program\]](#csynt:procedure-decl-program){reference-type="ref"
reference="csynt:procedure-decl-program"} shows the structure of the
syntax used to create a program using the C++ language.

Listing
[\[lst:program-creation-c-hello-world\]](#lst:program-creation-c-hello-world){reference-type="ref"
reference="lst:program-creation-c-hello-world"} shows a small C++
Program. You should be able to match this up with the syntax defined in
Figure
[\[csynt:procedure-decl-program\]](#csynt:procedure-decl-program){reference-type="ref"
reference="csynt:procedure-decl-program"}. This program does not include
any custom procedures, but does use a **header include** to include the
*splashit.h* header file. Following this is the `main` function that
includes the instructions that are run when the program is executed.

Listing
[\[lst:program-creation-c-hello-world\]](#lst:program-creation-c-hello-world){reference-type="ref"
reference="lst:program-creation-c-hello-world"} shows another example
C++ Program. This code includes two custom procedures: `say_hello` and
`say_is_anyone_there`. These procedures are called within `main`.

### C++ Statement {#sub:program-creation-c_statement}

In a you are commanding the computer to perform an action. There are
only a small number of statements you can choose from. At this stage the
only statement is the , technically the *procedure statement* in C++.
This is shown in Figure
[\[csynt:program-creation-statement\]](#csynt:program-creation-statement){reference-type="ref"
reference="csynt:program-creation-statement"}, where we can see that at
this stage all Statements are calls to s.

### C++ Procedure Call {#sub:program-creation-c_procedure_call}

A procedure call allows you to run the code in a Procedure, getting its
instructions to run before control returns back to the point where the
procedure was called.

### C++ Procedure Declaration {#sub:c_procedure_declaration}

The Syntax for a C++ Procedure Declaration is shown in Figure
[\[csynt:procedure-decl-procedure-decl\]](#csynt:procedure-decl-procedure-decl){reference-type="ref"
reference="csynt:procedure-decl-procedure-decl"}.

### C++ Identifier {#sub:c_identifier}

The C++ syntax is shown in Figure
[\[csynt:program-creation-identifier\]](#csynt:program-creation-identifier){reference-type="ref"
reference="csynt:program-creation-identifier"}. In C++, as in most
programming languages, the identifier must start with an underscore (\_)
or a letter; in other words your identifiers cannot start with a number
or contain other symbols. This is because the compiler needs a way of
distinguishing identifiers from numbers entered directly into the code.

::: {#tbl:program-creation-c identifiers and keywords}
   **Reserved Identifiers (Keywords)**                                                     **Example Identifiers**  
  ------------------------------------- ----------- ---------- ------------ ---------- -- ------------------------- --------------
                 `auto`                   `break`     `case`      `char`     `const`               printf               scanf
               `continue`                `default`     `do`      `double`     `else`               bitmap            sound_effect
                 `enum`                  `extern`    `float`      `for`       `goto`                name             draw_bitmap
                  `if`                     `int`      `long`    `register`   `return`                age               my_alien
                 `short`                 `signed`    `sizeof`    `static`    `struct`              height                test
                `switch`                 `typedef`   `union`    `unsigned`    `void`                alien               name3
               `volatile`                 `while`                                                   \_23                  i

  : C++ Keywords and other example identifiers
:::

### C++ Expression {#sub:program-creation-c_expression}

An in C++ is a mathematical calculation or a value. Each expression will
have a , and can contain a number of mathematic operators. Table
[2.3](#tbl:program-creation-c operators and expresions){reference-type="ref"
reference="tbl:program-creation-c operators and expresions"} lists the
operators that you can include in your expressions, listed in order of
precedence.[^10] The operators you can use depend on the kind of data
that you are using within the expression.

::: {#tbl:program-creation-c operators and expresions}
   **Operator**  **Description**                            **Example**
  -------------- ------------------------------------------ -----------------
     ` ( ) `     Parenthesis                                `(1 + 1) * 2`
     `% * /`     Modulo[^11], Multiplication and Division   `1 / 2 * 5 % 3`
      `+ -`      Addition and subtraction                   `10 + 3 - 4`

  : C++ Operators and Example Expressions
:::

::: {#tbl:program-creation-c example expresions}
      **Example Expression**         **Value**     **Type**
  ------------------------------ ----------------- ---------------
              ` 73 `                    73         `int`
             ` 2.1 `                    2.1        `float`
        ` "Hello World" `         \"Hello World\"  `string`[^12]
            ` "Fred" `               \"Fred\"      `string`
            ` 3 * 2 `                    6         `int`
          ` 1 + 3 * 2 `                  7         `int`
          ` (1 + 3) * 2`                 8         `int`
          ` 7 - 3 + 1 `                  5         `int`
            ` 3 / 2 `                 1[^13]       `int`
           ` 3.0 / 2.0`                 1.5        `float`
             ` 3 % 2`                    1         `int`
            ` 11 % 3`                    2         `int`
           ` 3 / 2.0 `               1.5[^14]      `float`
   ` 1 + (3 / 2.0) + 6 * 2 - 8`         6.5        `float`

  : Example C++ Expressions and their values
:::

### C++ Literal {#sub:program-creation-c_literal}

A literal is either a number or text value written directly in the code.
In other words, it is not *calculated* when the program runs - the value
entered is **literally** the value to be used. Figure
[\[csynt:program-creation-literal\]](#csynt:program-creation-literal){reference-type="ref"
reference="csynt:program-creation-literal"} shows the syntax for the
different literal values you can enter into your C++ code.

### C++ Types {#sub:program-creation-c_types}

s are used to define how data is interpreted and the operations that can
be performed on the data. Table
[2.5](#tbl:program-creation-c-types){reference-type="ref"
reference="tbl:program-creation-c-types"} shows the three basic types of
data, the associated C++ type, size in memory, and other related
information. Table
[2.6](#tbl:program-creation-c operators by type){reference-type="ref"
reference="tbl:program-creation-c operators by type"} shows the
operators that are permitted for each Type.

::: {#tbl:program-creation-c-types}
  **Whole Number Types**                                                     
  ------------------------ ----------------- ------------------------------- ----------------------
  *Name*                        *Size*         *Range (lowest .. highest)*   
  `short`                   2 bytes/16 bits         -32,767 .. 32,767        
  `int`                     4 bytes/32 bits     -2147483648 .. 2147483647    
  `int64_t`                 8 bytes/64 bits   -9,223,372,036,854,775,807 ..  
                                                9,223,372,036,854,775,807    
                                                                             
  **Real Number Types**                                                      
  *Name*                        *Size*         *Range (lowest .. highest)*    *Significant Digits*
  `float`                   4 bytes/32 bits         1.0e-38 .. 1.0e38                  6
  `double`                  8 bytes/64 bits        2.0e-308 .. 2.0e308                 10
                                                                             
  **Text Types**                                                             
  *Name*                        *Size*                 *Known As*            
  `char`                     1 byte/8 bits                                   
  `string`                   various[^15]               c-string             

  : C++ Data Types
:::

::: {#tbl:program-creation-c operators by type}
     **Type**      **Operations Permitted**  **Notes**
  --------------- -------------------------- -----------------------------------------
   Whole Numbers       `( ) + - / * %`       Division rounds down if all
                                             values are whole numbers.
   Real Numbers         `( ) + - / *`        
                                             
       Text                `( ) +`           You can use `+` for concatenation.[^16]

  : C++ Permitted Operators by Type
:::

### C++ SplashKit Terminal Output {#sub:c_console_output}

The C and C++ programming languages include standard libraries that
allow you to write text to the terminal. However, the way the C
procedures work is prone to errors (none of which will help you learn
about programming) and the C++ way is unique to C++ and not something
other languages have adopted. As the focus of this book is on learning
to program, not to learn to use any one particular language, we will use
the SplashKit procedures to output text. These avoid the issues inherent
in the C library, and use a more standard approach than that taken in
C++. You can revisit the standard libraries once you are confident in
programming in general.

SplashKit includes two procedures to output text to the terminal:
`write_line` and `write` .

::: {#tbl:program-creation-c printf parameters}
      **Procedure Prototype**      
  -------------------------------- -------------------------------------------------
                                   
   `void write_line(string text)`  
    `void write_line(int text)`    
   `void write_line(double text)`  
                                   
     `void write(string text)`     
       `void write(int text)`      
     `void write(double text)`     
                                   
           **Parameter**           **Description**
              ` text `             The text that is to be written to the Terminal.

  : Parameters that must be passed to write line.
:::

### C++ Comments {#sub:c_comments}

Comments allow you to embed documentation and explanatory text within
your program's code. The comments are skipped by the compiler, so they
have no affect on the program's machine code. You write comments to help
yourself and other people understand what you intend the program to do,
and any thoughts you want to record along with the code.

### Some Common C Errors with Program Creation {#sub:some_common_c_errors_with_program_creation}

The following list shows some of the more common errors you are likely
to encounter at this stage. The C compiler is particularly cryptic with
its error messages, but these few should help you with the more common
problems.

  -------------------------------------------------------------------------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  **`error: expected ‘;’ before ...`**                                             
                                                                                   Each statement in C must be ended with a semicolon (;). This error is indicating that the compiler has reached a point where it believes a statement should have ended. Look back at the previous line, it is likely that you forgot to put the ending semicolon.
                                                                                   
  **`warning: implicit declaration of function ‘…’`**                              
                                                                                   The compiler doesn't know about the procedure/function that you are calling at this point in the code. This could be caused by one of two common problems. Firstly you may have a typo in the name of the procedure you are calling, check the name carefully and remember that C is case sensitive. Secondly, you may have forgotten to include a library that you are using. If this is the case check the includes at the top of the file.
                                                                                   
  **`Undefined symbols: "_main", referenced from:…`**                              
                                                                                   The entry point for a C program must be called `main`. C is case sensitive, so `Main` and `main` are not the same. Check that you have a `main` function. This error is indicating that the compiler cannot find the program's entry point, it cannot find the `main` function.
                                                                                   
  **`warning: control reaches end of non-void function`**                          
                                                                                   You are missing the return at the end of the main function. Add the code `return 0;` .
                                                                                   
  **`error: filename: No such file or directory`**                                 
                                                                                   This error is likely to occur if you mistype the name of the library's header (.h) file that you are including. Check the `#include<...>` at the start of the code. The error here is indicating that the compiler cannot find the file. If the filename is spelt correctly then there may be an issue with your compiler's installation.
                                                                                   
  **`error: expected ‘=’, ‘,’, ‘;’, ‘asm’ or ‘__attribute__’ before ‘main’`**      
                                                                                   You must declare main has `int main()` . This error will occur if you have a typo in the `int` part.
                                                                                   
  **`error: expected ‘=’, ‘,’, ‘;’, ‘asm’ or ‘__attribute__’ before ‘{’ token`**   
                                                                                   This is the error message you get if you forget to put the parenthesis after `main` but before the open brace ( { ).
                                                                                   
  -------------------------------------------------------------------------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Program Creation in Pascal {#sec:program-creation-in-pas}

Section
[\[sec:using_these_concepts_program_creation\]](#sec:using_these_concepts_program_creation){reference-type="ref+page"
reference="sec:using_these_concepts_program_creation"} of this chapter
introduced an 'Output Test' program, and its design. The pseudocode from
this section is shown in Listing
[\[lst:program-creation-hello-pseudo 2\]](#lst:program-creation-hello-pseudo 2){reference-type="ref"
reference="lst:program-creation-hello-pseudo 2"}. In this Section you
will see the rules for translating this program's design into the C code
shown in Listing
[\[lst:program-c-output_test\]](#lst:program-c-output_test){reference-type="ref"
reference="lst:program-c-output_test"}.

### Pascal Program {#sub:program_in_pas}

Figure
[\[passynt:program-creation-program\]](#passynt:program-creation-program){reference-type="ref"
reference="passynt:program-creation-program"} shows the structure of the
syntax you can use to create a program using the Pascal language.

The code in Listing
[\[lst:program-creation-pas-hello-world\]](#lst:program-creation-pas-hello-world){reference-type="ref"
reference="lst:program-creation-pas-hello-world"} shows an example
Pascal Program. You should be able to match this up with the syntax
defined in
Figure [\[passynt:program-creation-program\]](#passynt:program-creation-program){reference-type="ref"
reference="passynt:program-creation-program"}. Notice that the uses
clause is skipped, this clause allows you to access code in external
libraries (called units in Pascal). By default all Pascal programs have
access to the `System` unit which contains the `WriteLn` procedure and
many other reusable artefacts.

### Pascal Statement {#sub:program-creation-pas_statement}

In a you are commanding the computer to perform an *action*. There are
only a small number of statements you can choose from. At this stage the
only statement we have discussed is the , formally known as the
*procedure statement* in Pascal. This is shown in Figure
[\[csynt:program-creation-statement\]](#csynt:program-creation-statement){reference-type="ref"
reference="csynt:program-creation-statement"}, where we can see that at
this stage all Statements are calls to s.

### Pascal Procedure Call {#sub:program-creation-pas_procedure_call}

A procedure call allows you to run the code in a procedure, getting its
instructions to run before control returns back to this point in the
program.

### Pascal Identifier {#sub:pas_identifier}

The Pascal syntax is shown in Figure
[\[passynt:program-creation-identifier\]](#passynt:program-creation-identifier){reference-type="ref"
reference="passynt:program-creation-identifier"}. In Pascal, as in most
programming languages, the identifier must start with an underscore (\_)
or a letter; in other words your identifiers cannot start with a number
or contain other symbols. This is because the compiler needs a way of
distinguishing identifiers from numbers entered directly into the code.

::: {#tbl:program-creation-pas identifiers and keywords}
   **Reserved Identifiers (Keywords)**                                                                              **Examples**
  ------------------------------------- ------------------ ------------------ ---------------- ------------------ -----------------
               `absolute`                     `and`             `array`             `as`             `asm`            `WriteLn`
                 `begin`                      `case`            `class`           `const`        `constructor`         `Write`
              `destructor`                  `dispose`       `dispinterface`        `div`              `do`            `ReadLn`
                `downto`                      `else`             `end`            `except`         `exports`          `Bitmap`
                 `exit`                      `false`             `file`        `finalization`      `finally`          `myAlien`
                  `for`                     `function`           `goto`             `if`        `implementation`        `age`
                  `in`                     `inherited`      `initialization`      `inline`        `interface`         `height`
                  `is`                       `label`           `library`           `mod`             `new`              `_23`
                  `nil`                       `not`             `object`            `of`              `on`               `i`
               `operator`                      `or`              `out`            `packed`        `procedure`          `name`
                `program`                   `property`          `raise`           `record`       `reintroduce`         `test`
                `repeat`                 `resourcestring`        `self`            `set`             `shl`            `height`
                  `shr`                      `string`            `then`         `threadvar`           `to`         `DrawRectangle`
                 `true`                       `try`              `type`            `unit`           `until`         `FillCircle`
                 `uses`                       `var`             `while`            `with`            `xor`          `CheckRange`

  : Pascal identifiers
:::

### Pascal Expression {#sub:program-creation-pas_expression}

An in Pascal is a mathematical calculation or a literal value. Each
expression will have a , and can contain a number of mathematic
operators. Table
[2.9](#tbl:program-creation-pas operators and expresions){reference-type="ref"
reference="tbl:program-creation-pas operators and expresions"} lists the
operators that you can include in your expressions, listed in order of
precedence.[^17] The operators you can use depend on the kind of data
that you are using within the expression.

::: {#tbl:program-creation-pas operators and expresions}
      **Operator**     **Description**                            **Example**
  -------------------- ------------------------------------------ ---------------
        ` ( ) `        Parenthesis                                `(1 + 1) * 2`
   `mod * / div`[^18]  Modulo[^19], Multiplication and Division   `1 / 2 * 5`
                                                                  `1 div 2 * 5`
         `+ -`         Addition and subtraction                   `10 + 3 - 4`

  : Pascal Operators and Example Expressions
:::

::: {#tbl:program-creation-pas example expresions}
      **Example Expression**        **Value**    **Type**
  ------------------------------ --------------- -----------
              ` 73 `                   73        `Integer`
             ` 2.1 `                   2.1       `Single`
        ` ‘Hello World’ `         'Hello World'  `String`
      ` ‘Hello ’ + ‘World’ `      'Hello World'  `String`
            ` ‘Fred’ `               'Fred'      `String`
            ` 3 * 2 `                   6        `Integer`
          ` 1 + 3 * 2 `                 7        `Integer`
          ` (1 + 3) * 2`                8        `Integer`
          ` 7 - 3 + 1 `                 5        `Integer`
            ` 3 / 2 `                  1.5       `Single`
           ` 3 div 2 `                  1        `Integer`
           ` 3 mod 2 `                  1        `Integer`
           ` 11 mod 3 `                 2        `Integer`
           ` 3.0 / 2.0`                1.5       `Single`
           ` 3 / 2.0 `                 1.5       `Single`
   ` 1 + (3 / 2.0) + 6 * 2 - 8`        6.5       `Single`

  : Pascal Example Expressions
:::

### Pascal Literal {#sub:program-creation-pas_literal}

A literal is either a number or text value stated directly in the code.
In other words, it is not *calculated* when the program runs - it is
already in the code. Figure
[\[passynt:program-creation-literal\]](#passynt:program-creation-literal){reference-type="ref"
reference="passynt:program-creation-literal"} shows the syntax for the
different literal values you can enter into your Pascal code.

### Pascal Types {#sub:program-creation-pas_types}

s are used to define how data is interpreted and the operations that can
be performed on the data. Table
[2.11](#tbl:program-creation-pas-types){reference-type="ref"
reference="tbl:program-creation-pas-types"} shows the three basic types
of data, the associated Pascal type, size in memory, and other related
information. Table
[2.12](#tbl:program-creation-pas operators by type){reference-type="ref"
reference="tbl:program-creation-pas operators by type"} shows the
operators that are permitted for each type.

::: {#tbl:program-creation-pas-types}
  **Whole Number Types**                                                                     
  ------------------------ --------------------- ------------------------------------------- ----------------------
  *Name*                          *Size*                 *Range (lowest .. highest)*         
  `Byte`                      1 bytes/8 bits                      0 .. 255                   
  `SmallInt`                  2 bytes/16 bits                 -32,767 .. 32,767              
  `Integer`                   4 bytes/32 bits             -2147483648 .. 2147483647          
  `Int64`                     8 bytes/64 bits           -9,223,372,036,854,775,807 ..        
                                                          9,223,372,036,854,775,807          
                                                                                             
  **Real Number Types**                                                                      
  *Name*                          *Size*                 *Range (lowest .. highest)*          *Significant Digits*
  `Single`                    4 bytes/32 bits                 1.0e-38 .. 1.0e38                        6
  `Double`                    8 bytes/64 bits                2.0e-308 .. 2.0e308                       10
  `Extended`                 10 bytes/80 bits               1.9e-4932 .. 1.1e4932                      20
                                                                                             
  **Text Types**                                                                             
  *Name*                          *Size*                           *Notes*                   
  `Char`                       1 byte/8 bits                                                 
  `String`                  256 bytes/2048 bits   Supports up to 255 characters by default,  
                                                   compiler options allow longer strings.    

  : Pascal Data Types
:::

::: {#tbl:program-creation-pas operators by type}
     **Type**      **Operations Permitted**  **Notes**
  --------------- -------------------------- ---------------------------------
   Whole Numbers    `( ) + - div / * mod`    
                                             
   Real Numbers         `( ) + - / *`        
                                             
       Text                `( ) + `          The `+` performs concatenation.
                                             

  : Pascal Permitted Operators by Type
:::

### Pascal Terminal Output {#sub:pas_console_output}

Pascal comes with a range of libraries (called units in Pascal) that
provide reusable programming artefacts, including reusable s. The
`System` unit is automatically included in all Pascal programs. This
unit includes artefacts you can use to perform input and output tasks,
including procedures to write output to the Terminal.

::: {#tbl:program-creation-pas write parameters}
   **Procedure Prototype**  
  ------------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                            
   `procedure Write( … )`   
                            
        **Parameter**       **Description**
            ` …`            The `Write` procedure takes a variable number of parameters. Each of the parameters are written to the Terminal in sequence. See notes for details on formatting numeric values.

  : Parameters that must be passed to `Write`
:::

::: {#tbl:program-creation-pas writeln parameters}
   **Procedure Prototype**  
  ------------------------- -------------------------------------------------------------------------------------------------------------------
                            
       `WriteLn( … )`       
                            
        **Parameter**       **Description**
            ` …`            Works in the same way as `WriteLn`, but also advances to a new line after writing each parameter to the Terminal.

  : Parameters that must be passed to `WriteLn`
:::

### Pascal Comments {#sub:pas_comments}

Comments allow you to embed documentation and explanatory text within
your program's code. The comments are skipped by the compiler, so they
have no affect on the program's machine code. You write comments to help
other people understand what you intend the program to do.

## Understanding Program Execution {#sec:understanding_program_execution}

The earlier Sections of this Chapter have covered the concepts and code
related to program creation, but have not looked at how these concepts
actually affect the computer when the program is run. This Section
illustrates the actions that occur inside the computer when your program
is executed. A good understanding of these concepts work will enable you
to use them effectively.

This Section will help you answer the following questions:

-   What happens when the program is started?

-   What happens when the code executes a procedure call?

### Starting a Program {#sub:starting_a_program}

Double clicking a program's icon, or launching it from the command line,
causes the program to run. This is as much as most normal users need to
know about using programs. However, as a Software Developer you need to
know more about what is actually happening as you will be the one who
defines what the computer does when your program runs.

Starting a program is the responsibility of the *Operating System*. When
the program is launched the following steps are performed. A discussion
of each of these steps follows.

1.  Space is allocated in memory for the Program, and partitioned into
    areas for the program's **code**, and the call **Stack**.

2.  The program's code is loaded into memory, into the **code** Section.

3.  A **frame** is added to the **Stack** with the location of the first
    instruction in the program.

4.  The computer starts running the instructions based on the current
    frame in the stack.

#### Allocating Memory for the Program {#ssub:allocating_memory_for_the_program}

To start the program the Computer first needs to get the program's
instructions into memory. This task is performed by the Operating System
when the program is launched. The Operating System allocates memory for
the program to use, and partitions this memory into different areas.
Each area will be used to store different kinds of information needed by
the program. An illustration of this is shown in Figure
[2.23](#fig:program-creation-visualise-helloworld-1){reference-type="ref"
reference="fig:program-creation-visualise-helloworld-1"}.

![Operating System prepares memory for the
Program](./topics/program-creation/images/ProgramExecution01.pdf){#fig:program-creation-visualise-helloworld-1
width="\\textwidth"}

#### Loading the Code {#ssub:loading_the_code}

Having allocated the program some memory, and partitioned this space
into the **Stack** and **Code** area, the Operating System then reads
the program's instructions from the executable file and loads these into
the Code area. This is illustrated in Figure
[2.24](#fig:program-creation-visualise-helloworld-2){reference-type="ref"
reference="fig:program-creation-visualise-helloworld-2"}.

![The Operating System loads the program's code into
memory](./topics/program-creation/images/ProgramExecution02.pdf){#fig:program-creation-visualise-helloworld-2
width="\\textwidth"}

#### Setting Up The First Instruction {#ssub:setting_up_the_first_instruction}

Now that the code is loaded into memory, the Operating System uses the
details saved in the executable file to setup the program's first
instruction. This will be loaded onto the Stack, which is responsible
for keeping track of the current instruction. The compiler will have
used the program's **entry point** to store these details when the
program was compiled. This is shown in Figure
[2.25](#fig:program-creation-visualise-helloworld-3){reference-type="ref"
reference="fig:program-creation-visualise-helloworld-3"}.

![The current instruction is loaded onto the
Stack](./topics/program-creation/images/ProgramExecution03.pdf){#fig:program-creation-visualise-helloworld-3
width="\\textwidth"}

#### Running The First Instruction {#ssub:running_the_first_instruction}

The Operating System has finally finished loaded the Program, and can
now start its instructions running. The CPU uses the **Current
Instruction** that is on the top of the Stack, locates the Code, and
runs the instruction. In this case this is a procedure call to a
Procedure that writes output to the Terminal. When this instruction
completes, the text *Output Test Program* will have appeared on the
Terminal for the user to see. The results of this are shown in Figure
[2.26](#fig:program-creation-visualise-helloworld-4){reference-type="ref"
reference="fig:program-creation-visualise-helloworld-4"}.

![The Computer runs the first instruction, outputting details to the
Terminal](./topics/program-creation/images/ProgramExecution04.pdf){#fig:program-creation-visualise-helloworld-4
width="\\textwidth"}

### Calling the Write Procedure {#sub:calling_a_procedure}

At this point the Computer has been instructed to
`Call the Write Procedure`. This procedure[^20] will output the data
passed to it to the Terminal. In order to do this, the instructions
within the called procedure need to be followed.

#### Calling Write for the first time {#ssub:calling_write_for_the_first_time}

Each Procedure contains instructions that when followed get the Computer
to perform a task. The procedure call sets up the Stack so that the
instructions within the Write Procedure are executed.

![The Write Procedure is called, and has its instructions
executed](./topics/program-creation/images/ProgramExecution05.pdf){#fig:program-creation-visualise-helloworld-5
width="90%"}

#### The Write Procedure Ends {#ssub:the_write_procedure_ends}

When the Write Procedure's instructions finish control needs to return
to the code that called it, in this case the program's code.

![The Write Procedure is called, and has its instructions
executed](./topics/program-creation/images/ProgramExecution06.pdf){#fig:program-creation-visualise-helloworld-6
width="\\textwidth"}

One way to visualise this is to picture the Program, and the Procedure,
as a book of instructions. Imagine you are told to follow the
instructions in a book. You get the book, place it on a table and read
the first instruction which tells you to perform the *Write Procedure*.
You leave the original book on the table, and fetch the *Write
Procedure* book and place it on top of the book on the table, thereby
creating a Stack of books. Now you can follow the instructions, one by
one, from the book on top of the Stack. When you finish the last
instruction you take the book off the top of the stack and return to the
book beneath it. This will enable you to perform the steps within the
Procedures without forgetting where you are up to in the earlier code.

#### The Second Call to Write {#ssub:the_second_call_to_write}

The second instruction in the program's code is another call to the
Write procedure.

![The Write Procedure is called a second
time](./topics/program-creation/images/ProgramExecution07.pdf){#fig:program-creation-visualise-helloworld-7
width="\\textwidth"}

#### The Second Write Call Ends {#ssub:the_second_write_call_ends}

When the second call to Write ends control returns to the Program, which
moves on to its final instruction.

![The second call to the Write Procedure ends, and control returns to
the
Program](./topics/program-creation/images/ProgramExecution08.pdf){#fig:program-creation-visualise-helloworld-8
width="\\textwidth"}

#### The Third Call to Write {#ssub:third_call_to_write}

The final instruction in the program is a third call to the Write
Procedure.

![The Write Procedure is called a third can final
time](./topics/program-creation/images/ProgramExecution09.pdf){#fig:program-creation-visualise-helloworld-9
width="\\textwidth"}

#### The Program Ends {#ssub:program_ends}

When the last instruction in the Write Procedure finishes control return
back to the Program, which has no more instructions. This means that the
program has finished.

![The Write Procedure ends, then the program
ends](./topics/program-creation/images/ProgramExecution10.pdf){#fig:program-creation-visualise-helloworld-10
width="\\textwidth"}

### Summary {#sub:program_creation_visualise_summary}

In this section you have seen the actions that occur behind the scenes
when your program is executed. The most important aspects are the fact
that the instructions run one at a time in **sequence**, and that a
procedure call results in the instructions within the Procedure running
until they end. Its also important to remember that the instructions
within the Procedure must finish before control returns to where the
call was made.

## Program Creation Examples {#sec:program_creation_examples}

### Seven Times Table {#sub:seven_times_table}

This program prints out the seven times table from 1 x 7 to 10 x 7. The
description of the program is in Table
[2.15](#tbl:program-creation-times-table){reference-type="ref"
reference="tbl:program-creation-times-table"}, the pseudocode in Listing
[\[lst:program-creation-seven-times-pseudo\]](#lst:program-creation-seven-times-pseudo){reference-type="ref"
reference="lst:program-creation-seven-times-pseudo"}, the C code in
Listing
[\[lst:program-creation-seven-times-c\]](#lst:program-creation-seven-times-c){reference-type="ref"
reference="lst:program-creation-seven-times-c"}, and the Pascal code in
Listing
[\[lst:program-creation-seven-times-pas\]](#lst:program-creation-seven-times-pas){reference-type="ref"
reference="lst:program-creation-seven-times-pas"}.

::: {#tbl:program-creation-times-table}
  **Program Description**   
  ------------------------- ------------------------------------------------------
  **Name**                  *Seven Times Table*
                            
  **Description**           Displays the Seven Times Table from 1 x 7 to 10 x 7.

  : Description of the Seven Times Table program
:::

### Circle Area {#sub:circle_area}

This program prints out the area of circles with different radius. The
description of the program is in Table
[2.16](#tbl:program-creation-circle-area){reference-type="ref"
reference="tbl:program-creation-circle-area"}, the pseudocode in Listing
[\[lst:program-creation-circle-areas-pseudo\]](#lst:program-creation-circle-areas-pseudo){reference-type="ref"
reference="lst:program-creation-circle-areas-pseudo"}, the C code in
Listing
[\[lst:program-creation-circle-areas-c\]](#lst:program-creation-circle-areas-c){reference-type="ref"
reference="lst:program-creation-circle-areas-c"}, and the Pascal code in
Listing
[\[lst:program-creation-circle-areas-pas\]](#lst:program-creation-circle-areas-pas){reference-type="ref"
reference="lst:program-creation-circle-areas-pas"}.

::: {#tbl:program-creation-circle-area}
  **Program Description**   
  ------------------------- -------------------------------------------------------------------------------------------
  **Name**                  *Circle Areas*
                            
  **Description**           Displays the Circle Areas for circles with radius from 1.0 to 5.0 with increments of 0.5.

  : Description of the Circle Areas program
:::

### Shape Drawing {#sub:shape_drawing}

This program draws some shapes to the screen using the **SwinGame**
Software Development Kit (SDK). The SwinGame SDK is a library that
provides a number of reusable code artefacts that you can use to create
2D games. This SDK is available for both C and Pascal, and work on
Linux, Mac, and Windows.

The description of the program is in Table
[2.17](#tbl:program-creation-shape-drawing){reference-type="ref"
reference="tbl:program-creation-shape-drawing"}, the pseudocode in
Listing
[\[lst:program-creation-shape-drawing-pseudo\]](#lst:program-creation-shape-drawing-pseudo){reference-type="ref"
reference="lst:program-creation-shape-drawing-pseudo"}, the C code in
Listing
[\[lst:program-creation-shape-drawing-c\]](#lst:program-creation-shape-drawing-c){reference-type="ref"
reference="lst:program-creation-shape-drawing-c"}, and the Pascal code
in Listing
[\[lst:program-creation-shape-drawing-pas\]](#lst:program-creation-shape-drawing-pas){reference-type="ref"
reference="lst:program-creation-shape-drawing-pas"}.

::: {#tbl:program-creation-shape-drawing}
  **Program Description**   
  ------------------------- --------------------------------------------------------
  **Name**                  *Shape Drawing*
                            
  **Description**           Draws a number of shapes to the screen using SwinGame.

  : Description of the Shape Drawing program
:::

The Lines from the program will do the following:

-   **`OpenGraphicsWindow`** opens a Window with the title 'Shape
    Drawing' that is 800 pixels wide by 600 pixels high.

-   **`ClearScreen`** clears the screen to black.

-   **`FillRectangle`** uses the color, the x, y location, and width and
    height to fill a rectangle.

-   **`RefreshScreen`** updates the screen to show what has been drawn.
    All SwinGame drawing is done offscreen, and only drawn to the screen
    when RefreshScreen is called.

-   **`Delay`** pauses the program for a number of milliseconds, so 500
    will wait for half a second.

-   **`FillCircle`** uses the color, given x, y location and radius to
    fill a circle.

-   **`FillTriangle`** fills a triangle with the given x, y points (6
    values for 3 points).

The SwinGame procedure for C are named using the standard C naming
scheme. The names are:

-   **`open_graphics_window`** opens a Window with the title 'Shape
    Drawing' that is 800 pixels wide by 600 pixels high.

-   **`load_default_colors`** loads default colors for use in your code.

-   **`clear_screen`** clears the screen to black.

-   **`fill_rectangle`** uses the color, the x, y location, and width
    and height to fill a rectangle.

-   **`refresh_screen`** updates the screen to show what has been drawn.
    All SwinGame drawing is done offscreen, and only drawn to the screen
    when RefreshScreen is called.

-   **`delay`** pauses the program for a number of milliseconds, so 500
    will wait for half a second.

-   **`fill_circle`** uses the color, given x, y location and radius to
    fill a circle.

-   **`fill_triangle`** fills a triangle with the given x, y points (6
    values for 3 points).

## Program Creation Exercises {#sec:program_creation_exercises}

### Concept Questions {#sub:concept_questions_prog}

Read over the concepts in this chapter and answer the following
questions:

1.  What is a ? What does it contain?

2.  A program is an artefact, something you can create in code. Why
    would you want to create a program your code?

3.  What is the entry point of a program?

4.  Do you have to write all of the code for your program, or are you
    able to use artefacts from elsewhere?

5.  What is a ?

6.  What statement was introduced in this chapter?

7.  What is a ?

8.  Where can you find procedures you may be interested in using?

9.  What is an ?

10. Where are expressions coded? Give an example of an expression being
    used in code.

11. What are the three broad kinds of s that a language will provide?

12. What are the names of the types in the language you are using? Name
    the main type you are likely to work with for each of the broad
    kinds of types.

13. What are the values of the following expressions? Which types could
    these values be used with (possibly multiple)? Note: some answers
    are dependent of the language you are using.

    ::: multicols
    3

    1.  5

    2.  3 + 5 \* 2

    3.  (3 + 5) \* 2

    4.  3.1415

    5.  1 / 2

    6.  1.0 / 2.0

    7.  2 + 1 / 2.0 \* 6

    8.  \"Fred Smith\" (C)

    9.  'Fred Smith' (Pascal)
    :::

14. Where can statements be coded in your program?

15. What is the role of an ? What artefacts be be identified?

16. What is a keyword?

17. Which of the following are valid identifiers?

    ::: multicols
    4

    1.  hello

    2.  \_123

    3.  fred

    4.  my name

    5.  my_name

    6.  begin

    7.  void

    8.  Main

    9.  1234

    10. a1

    11. 3.1415

    12. WOW_COOL
    :::

18. What is a , and what does it contain?

19. What happens to when you code is compiled? Why do languages include
    these? Why are they considered important by good developers?

20. What is the stack? What does it keep track of when your program is
    running?

21. What is loaded into memory when your program is started?

22. What happens in the computer when a procedure is called?

### Code Writing Questions: Applying what you have learnt {#sub:code_writing_questions_applying_what_you_have_learnt_prog}

Apply what you have learnt to the following tasks:

1.  Write a program that prints the 5 times table from 1 \* 5 to 10
    \* 5. See Table [2.18](#tbl:five-times){reference-type="ref"
    reference="tbl:five-times"}.

    -   Think about the artefacts you will create, and use.

    -   Write pseudocode for the program's instructions

    -   Convert your pseudocode to either C or Pascal

    -   Compile and Run your program, and check that the values are
        correctly calculated

    ::: {#tbl:five-times}
      **Program Description**   
      ------------------------- ----------------------------------------------------------------
      **Name**                  *Five Times Table*
                                
      **Description**           Displays the 5 times table from $1 \times 5$ to $10 \times 5$.

      : Description of the *Five Times Table* program.
    :::

2.  Write a program that prints the powers[^21] of 2 from $2^1$ to
    $2^8$. See Table [2.19](#tbl:two-powers){reference-type="ref"
    reference="tbl:two-powers"}.

    -   Think about the artefacts you will create, and use.

    -   Write pseudocode for the program's instructions

    -   Convert your pseudocode to either C or Pascal

    -   Compile and Run your program, and check that the values are
        correctly calculated

    ::: {#tbl:two-powers}
      **Program Description**   
      ------------------------- ---------------------------------------------------
      **Name**                  *Powers of Two*
                                
      **Description**           Displays the powers of 2 from $2^{1}$ to $2^{8}$.

      : Description of the *Five Times Table* program.
    :::

3.  Write a program that prints the 73 times table from 1 \* 73 to 10
    \* 73. See
    Table [2.20](#tbl:sevelty-three-times){reference-type="ref"
    reference="tbl:sevelty-three-times"}.

    -   Think about the artefacts you will create, and use.

    -   Write pseudocode for the program's instructions

    -   Convert your pseudocode to either C or Pascal

    -   Compile and Run your program, and check that the values are
        correctly calculated

    ::: {#tbl:sevelty-three-times}
      **Program Description**   
      ------------------------- -------------------------------------------------------------------
      **Name**                  *Seventy Three Times Table*
                                
      **Description**           Displays the 73 times table from $1 \times 73$ to $10 \times 73$.

      : Description of the *Seventy Three Times Table* program.
    :::

4.  Write a program that prints a table showing calculations of circle
    dimensions. This should output the radius, circle area, diameter,
    and circumference of circles with a radius of 1cm, 1.5cm, and 2cm.
    See Table [2.21](#tbl:circle-dimensions){reference-type="ref"
    reference="tbl:circle-dimensions"}.

    -   Find the necessary calculations and think about the artefacts
        you will use and create.

    -   Write pseudocode for the program's instructions.

    -   Convert your pseudocode to either C or Pascal.

    -   Compile and Run your program, and check that the values are
        correctly calculated.

    ::: {#tbl:circle-dimensions}
      **Program Description**   
      ------------------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------
      **Name**                  *Circle Dimensions*
                                
      **Description**           Displays a table of circle dimensions for circles with a radius of 1cm, 1.5cm, and 2cm. This will output the radius, circle area, diameter, and circumference of circles.

      : Description of the *Circle Dimensions* program.
    :::

5.  Write a program with SwinGame that draws a face using primitive
    shapes. See Table [2.22](#tbl:face-shape){reference-type="ref"
    reference="tbl:face-shape"}.

    -   Draw up an outline of the program. Work out the coordinates of
        the circles for the face, circles. Determine three points of the
        triangle for the mouth.

    -   Write up the pseudocode for the program's instructions. Remember
        to use the `RefreshScreen` and `Delay` procedures to see the
        results.

    -   Convert your pseudocode to either C or Pascal

    -   Compile and Run your program, and check that the values are
        correctly calculated

    ::: {#tbl:face-shape}
      **Program Description**   
      ------------------------- ---------------------------------------------
      **Name**                  *Face Shape*
                                
      **Description**           Displays face to the screen using SwinGame.

      : Description of the *Face Shape* program.
    :::

### Extension Questions {#sub:extension_questions_pro}

If you want to further your knowledge in this area you can try to answer
the following questions. The answers to these questions will require you
to think harder, and possibly look at other sources of information.

1.  C and Pascal are imperative programming languages. What does this
    mean, and how does it relate to the way you think about code?

2.  Artefacts are things that you can create. What artefacts where
    introduced in this chapter? If you could create these artefacts,
    what would you do with them?

# Storing and Using Data {#cha:storing_and_using_data}

> [N]{.lettrine}[ow]{.smallcaps} it is time to turn your attention to a
> the finer details of spell craft. So far the magic you are working has
> not made use of material components. Fetch the Dragon scales from off
> the chest in the corner, and place them in that goblet. Prepare you
> wand, and... ...

So far our programs have only been able to work with Literal values.
This Chapter introduces new artefacts you can use to store data, and
from which you can read the value stored called Variables and Constants.
It will also introduce a new artefact that can be used to calculate
values called a Function. Using these artefacts you will see how you can
store and manipulate values in your code.

When you have understood the material in this chapter you will be able
to write code that uses Variable and Constants to stores and manipulates
data. You will be able to share data between different Procedures, and
create and use Functions to calculate values.

## Concepts Related to Storing and Using Data {#sec:concepts_related_to_storing_and_using_data}

Chapter
[\[cha:procedure_declaration\]](#cha:procedure_declaration){reference-type="ref"
reference="cha:procedure_declaration"}, , showed you how you can create
your own procedures, with each procedure performing a task for the
program. The procedures that you created did use some data, but in each
case the data was entered directly into the code of the program, as a
Literal.

This next step introduces the idea of creating your own artefacts that
can be used to **store**, or **calculate** a value. Using these
artefacts you can start to work with values in a more dynamic way,
allowing you to get the computer to perform calculations, and to store
and manipulate values.

In this Chapter you will learn how to create the following programming
**artefacts**:

-   : You can **store** a value in a Variable, and **retrieve** the
    value from the Variable.

-   : Is similar to a Variable, except that its value cannot change
    after it is declared.

-   : Is similar to a , but is used to calculate a value rather than to
    produce a side effect.

You will learn about the following **terminology**:

-   : Variables declared within the program's code are called Global
    Variables.

-   : Variables declared within a Function or Procedure are called Local
    Variables.

-   : Parameters are variables that allow values to be passed into a
    Function or Procedure.

-   : See how Functions, Constant, and Variables can be used in
    Expressions.

Additionally, you will learn how to perform the following **actions**:

-   : You use an Assignment Statement to store a value in a Variable.

-   : This is part of an Expression, and is used to call the Function
    and to read the returned result.

You may need to revise the following programming artefacts:

-   : The idea of building your own Programs.

-   : Creating your own Procedure, as well as calling Procedures from
    libraries.

The following programming terminology will also be used in this Chapter:

-   : An instruction performed in your code.

-   : The name of an artefact, or the text used to identify something
    meaningful to the language.

This material also requires that you have a good understanding of the
following actions:

-   : A procedure call is an instruction to run a Procedure.

By the end of this material we will have worked through an example where
you create a program that calculates change for a vending machine. This
program will read the cost and amount paid from the user, and will then
output the number of coins that need to be returned. The output of
several runs of this program are shown in Figure
[3.1](#fig:storing-using-simeple-change){reference-type="ref"
reference="fig:storing-using-simeple-change"}.

![The Change Calculator running in the
Terminal](./topics/storing-using-data/images/SimpleChange.png){#fig:storing-using-simeple-change
width="90%"}

### Variable {#sub:variable}

A Variable is a **container** into which you can store a value, which
can then be retrieved later. The Variable allows you to store values you
want to work with in your program, you store values in the variable so
that you can read them back later. The variable's themselves are either
a , , or .

![Variables store a value that can be read and
changed](./topics/storing-using-data/diagrams/Variable.pdf){#fig:storing-using-variable
width="\\textwidth"}

### Constant {#sub:constant}

A Constant is just like a , but its value cannot be changed. Constants
are declared within the Program, and given a value when they are
created. Once they are created the value within the Constant cannot be
changed. This is useful for data where you do not want the value
changing during the program's execution.

![Constants have a value that cannot be
changed](./topics/storing-using-data/diagrams/Constant.pdf){#fig:constants
width="\\textwidth"}

### Local Variable {#sub:local_variable}

Variables can be declared at a number of different places in your code.
Variables that are declared within Procedures are called **Local
Variables**. Most of the variables in your code will be Local Variables.

![Variables declared within a Procedure are Local
Variables](./topics/storing-using-data/diagrams/LocalVariables.pdf){#fig:storing-using-data-local-variables
width="\\textwidth"}

### Global Variable {#sub:global_variable}

Variables and Constants can be declared within a Program. Variables
declared in this way are called Global Variables. It seems tempting to
use Global Variables to share values between procedures, but this is a
bad idea. Global Variables should be avoided, and for many programs are
unnecessary. The issue with Global Variables is that their values can be
changed from anywhere within the program's code. This can make it
difficult to locate the source of errors when globals are used.

While Global Variables should be avoided, Constants should be declared
globally. As these values can not change, the issues with Global
Variables do not affect Constants.

![Variables declared within a Program are Global
Variables](./topics/storing-using-data/diagrams/GlobalVariables.pdf){#fig:storing-using-data-global-variables
width="\\textwidth"}

### Parameter {#sub:parameter}

The instructions within a Procedure define the actions that occur when
that procedure is called. In most cases these instructions need to be
given values to work with. These values can be passed to the Procedure
using Parameters. The Parameter is a Variable that has its value set in
the procedure call.

![Parameters allow data to be passed to
Procedures](./topics/storing-using-data/diagrams/Parameter.pdf){#fig:parameters-parameters
width="\\textwidth"}

### Pass by Value and Pass by Reference {#sub:pass_by_value_and_pass_by_reference}

There are actually two ways that values can be passed to Parameters.
This relates back to the fact that Variables have two aspects: the Value
within the Variable, and the Variable itself. These two means of passing
parameters allow you to either pass a value, or pass a Variable.

![Parameters can accept data by reference or by
value](./topics/storing-using-data/diagrams/ByValByRef.pdf){#fig:parameters-by-ref-by-val
width="\\textwidth"}

### Statement (with Assignment) {#sub:statement_with_assignment_}

Statements are the actions that we can get the computer to perform. At
this stage we have covered the statements that run procedures, the , and
the statement to assign values to variables, the .

![A Statement may be an Assignment
statement](./topics/storing-using-data/diagrams/Statement.pdf){#fig:storing-using-data-statement
width="\\textwidth"}

### Assignment Statement {#sub:assignment_statement}

The Assignment Statement calculates a value, and stores it in a
Variable. You use an assignment statement to store values in variables.

![Assignment Statements assign values to
Variables](./topics/storing-using-data/diagrams/AssignmentStatement.pdf){#fig:storing-using-data-assignment-statement
width="\\textwidth"}

### Function {#sub:function}

Functions are used to calculate values. In many ways a Function is just
like a Procedure, it has a name, can be called, can accept parameters,
can have local variables, and performs a number of instructions when it
is called. Unlike a Procedure, however, Functions are used to calculate
values. When the function you called ends it returns back to you with a
value.

![A Function is just like a Procedure, except it calculates and returns
a
Value](topics/storing-using-data/diagrams/Function.pdf){#fig:function-decl-function
width="\\textwidth"}

### Function Call {#sub:function_call}

A Function Call is used to execute a , and to read the value that is
returned. This is similar to a , but unlike a procedure call it must be
done as part of an Expression.

![A Function Call is part of an Expression where the value is
calculated](topics/storing-using-data/diagrams/FunctionCall.pdf){#fig:storing-using-data-function-call
width="\\textwidth"}

### Expressions (with Function Calls, Variables, and Constants) {#sub:expressions_with_variables_}

You can **read** the values from Variables and Constants within
Expressions. The value of the expression is calculated by **reading**
the values from the Variables and Constants when the expression is
calculated[^22].

![Expressions can read values from Function Calls, Variables, and
Constants](./topics/storing-using-data/diagrams/Expression.pdf){#fig:expressions-with-variables
width="90%"}

### Program (with functions) {#sub:program_with_functions_}

You can declare your own Functions within the program's code.

![You can declare your own Functions in your program's
code](topics/storing-using-data/diagrams/ProgramWithFunctions.pdf){#fig:function-decl-programs
width="\\textwidth"}

### Summary {#sub:data_concepts_summary}

This section has introduced a number of programming artefacts, some
programming terminology, and one kind of instruction. An overview of
these concepts is shown in Figure
[3.14](#fig:data-summary){reference-type="ref"
reference="fig:data-summary"}. The next section will look at how you can
use these concepts to design some small programs.

![Key Concepts introduced in this
Chapter](./topics/storing-using-data/diagrams/Summary.pdf){#fig:data-summary
width="\\textwidth"}

## Using these Concepts {#sec:using_these_concepts_storing_using_data}

Variables, Constants, and Functions give us the ability to work with
data in new ways in our programs. Previously the data within the program
was limited to values entered directly into the code. Now with these
concepts we can interact more meaningfully with data, performing
calculations, and storing and manipulating values.

### Designing Change {#sub:designing_simple_change}

Table [3.1](#tbl:storing-data-prog){reference-type="ref"
reference="tbl:storing-data-prog"} contains a description of the next
program we are going to design. This program will calculate and output
the ideal change for a given transaction from a Vending Machine. In
designing this program we will make use of the concepts introduced in
this chapter; we will use a Function to calculate the coins to give,
Variables to store values such as the amount paid, Constants for the
values of the different coins, and Parameters to pass values to the
Function.

::: {#tbl:storing-data-prog}
  **Program Description**   
  ------------------------- --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  **Name**                  *Change Calculator*
                            
  **Description**           Calculates the idea change for a given transaction in a Vending Machine. The transaction involves reading the cost of the item purchased and the amount paid, and then outputting the number of each type of coin to give as change.

  : Description of the Change Calculator program.
:::

To design and implement this program we need to follow a number of
steps:

1.  Understand the problem, and get some ideas on the tasks that need to
    be performed.

2.  Choose the artefacts we will create and use

3.  Map these artefacts to code

4.  Compile and run the program

In step 1 you need to understand what it is that you want the program to
do. This will involve determining the tasks to be performed, the steps
involved in those tasks, and any data associated with them. Once you
have a good understanding of what you want to achieve you can start to
build a solution. In this step you determine the artefacts you want to
create, and try to locate those you could use from the available
libraries. Step 3 then turns your plans into source code that can be
compiled and run in Step 4.

![The Change Calculator running in the Terminal, from
Figure [3.1](#fig:storing-using-simeple-change){reference-type="ref"
reference="fig:storing-using-simeple-change"}](./topics/storing-using-data/images/SimpleChange.png){#fig:storing-using-simeple-change-1
width="40%"}

### Understanding the Change Calculator {#sub:understanding_simple_change}

Receiving change from a transaction is something that you should be
familiar with, and determining the ideal change is not an overly complex
task. While the task itself is common, it does not mean that you can
skip thinking about it. If you had to give \$6.50 in change you know
without thinking that you should give three \$2 coins, and one 50c coin.
What you need to do now is think through the steps that you perform
intuitively.

Think about all the steps for how you actually calculated the change to
be given. The first *secret* is to realise that you need to start with
the coin with the largest value, and then work down from there to the
coin with the smallest value. The number of coins you give each time can
then be calculated by dividing the amount of change to be given by the
value of the current coin. Lastly you need to reduce the amount of
change that remains to be given. These steps are shown in the pseudocode
in Listing
[\[lst:data-simple-change-pseudo\]](#lst:data-simple-change-pseudo){reference-type="ref"
reference="lst:data-simple-change-pseudo"}.

### Choosing Artefacts for the Change Calculator {#sub:choosing_artefacts_for_simple_change}

The design for any program should involve thinking about the data as
well as the tasks involved. You can start by dividing the program's
tasks into functions and procedure, and then look at the data that will
be needed by these in order to achieve their tasks.

#### Designing the tasks for the Change Calculator {#ssub:designing_the_tasks_for_the_change_calculator}

Think about the process of calculating change. At the start you need to
determine the amount of change that needs to be given. Next you need to
determine how many of each kind of coin you are going to give. These
steps can be coded into their own functions and procedures. These
functions and procedures are outlined in the following list, and shown
in the structure chart in
Figure [3.16](#fig:simple-change-structure){reference-type="ref"
reference="fig:simple-change-structure"}.

-   `Get Change Value`: This will be responsible for asking the user to
    enter the cost of the item, and the amount paid. It will then
    calculate the amount of change that needs to be given, in cents.

-   `Give Change`: This will be responsible for the calculations related
    to giving one kind of coin as change. This will involve getting the
    number of coins to give, updating the amount of change remaining,
    and outputting the details (for that coin) to the Terminal.

-   `Coins to Give`: This will be responsible for calculating the number
    of coins to give as change, given an amount of change and the value
    of the coin.

![Structure Chart for the Change
Calculator](./topics/storing-using-data/images/SimpleCalcStructure.pdf){#fig:simple-change-structure
width="60%"}

To understand how this is going to work you now need to think about how
the kinds if data these different tasks are going to need.

#### Designing the data for the Change Calculator {#ssub:designing_the_data_for_the_change_calculator}

Designing data is much like designing the procedures in your program.
You need to think about the solution and try to identify the different
values that are being used. Each of these values can then become either
a Variable, a Parameter, or a Constant. Looking over the steps for
calculating change you should be able to identify several different
values you will need to work with. You need to be able to store things
like the cost of the item being purchased, the amount of money paid, and
the amount of change you need to give. A good way to approach this is to
think about the values the program will output, as well as the values
the user will need to input and any intermediate values you will need to
perform the required calculations.

You can use the input/output/processing ideas to help you think about
the data that will be required for each function or procedure. Let us
examine each of the functions and procedures in turn.

##### Get Change Value {#par:get_change_value}

The first task we can examine if the `Get Change Value` . This task will
be responsible for determining how much change needs to be given to the
user. To design the data needed for this task you can think about its
inputs, outputs, and temporary values.

When you are designing a function or procedure it can be given input by
the calling code in the or . Inputs provided via the function or
procedure call are coded using s. To determine the parameters you need,
think about the information that this function or procedure will need to
be given to fulfil its responsibilities. In the case of the
`Get Change Value`, it does not require any input from the caller.

-   `Get Change Value` does not require any input from the calling code,
    so there is no need for any parameters with this function.

The `Get Change Value` task will be coded as a function because it has
some output. The result returned by a function is an output, it is what
makes it different from a procedure. When you call a function it runs
some steps and then returns a value, the value returned is the output
from the function. `Get Change Value` needs to return back the amount of
change, this is why it is a function in this design.

-   `Get Change Value` returns a number. The number returned is the
    value of the change to be given in cents.

Within the `Get Change Value` function there will need to be some values
that it uses to calculate its output. `Get Change Value` will need to
read the cost of the item from the user, and the amount paid. These two
values need to be stored somewhere, so the design of this function will
require two s: they can be called `Cost of Item` and `Payment`. These
will exist entirely within the function, being the values the function
requires to calculate its output.

-   `Get Change Value` will have two local variables: `Cost of Item` and
    `Payment`.

The pseudocode for this function is shown in
Listing [\[lst:get_change_value\]](#lst:get_change_value){reference-type="ref"
reference="lst:get_change_value"}.

##### Give Change {#par:give_change}

The next task to consider is the `Give Change` . This procedure will be
responsible for coordinating the actions for giving a certain coin in
change. Once again you need to think about the inputs it requires
(parameters), its outputs, and any values it will work with internally.

Put yourself in the place of the computer.[^23] You (as the computer)
have been *asked* to `Give Change`. What data will you need to complete
this request? At a minimum you will need to know the total change value
that is being given, and the value of the coin that you are
issuing.[^24]

There is one more input that you can only find by thinking about the
tasks the computer needs to perform in this code. A part of giving the
change will be to output a message, something like '3 x 20c' or '1 x
\$2'. The number of coins can be calculated within the procedure, but
the text is another issue. The computer will not know what text to
output. This data must, therefore, come as input into the procedure.

-   `Give Change` needs to be *given* the `Change Value` variable, and
    it needs to be told value of the coin and the coin's description.
    These will be coded as parameters, with `Change Value` being passed
    by reference.

`Give Change` will output text to the Terminal, but it also needs to
update the `Change Value` variable it is given. This can be considered
as output from the procedure. As the `Change Value` variable is passed
by reference, you can picture this as receiving the variable from the
caller. Any changes this procedure makes on its `Change Value` parameter
will actually be changing the value in the variable passed to the
`Give Change` procedure. In this way the procedure is able to output a
value.[^25]

-   `Give Change` will output the updated `Change Value`, as this is
    passed by reference.

Within `Give Change` you will need to store the number of coins to give
in change. This will become a local variable that can be used to
calculate the updated `Change Value` and to output the details to the
Terminal.

-   `Give Change` will have one local variable: `to give` used to store
    the number of this coin to be given in the change.

The pseudocode for this procedure is shown in
Listing [\[lst:give_change\]](#lst:give_change){reference-type="ref"
reference="lst:give_change"}.

##### Coins to Give {#par:coins_to_give}

The `Coins to Give` function is used to calculate the number of coins to
give. This code could have been written within the `Give Change`
procedure, but it was decided to code this in its own function.

`Coins to Give` will need to be told the value of the change, and the
value of the coin. Both of these parameters will be passed by value, as
it is only the values that are required. Internally `Coins to Give` will
not require any additional data, as it can calculate its output from the
two input values.

-   `Coins to Give` needs to be *told* the value of the change and the
    value of the coin, it can then use these values to calculate the
    number of these coins that need to be given in the change.

-   `Coins to Give` will output the number of the indicated coin that
    needs to be given in the change.

The pseudocode for this procedure is shown in
Listing [\[lst:coins_to_give\]](#lst:coins_to_give){reference-type="ref"
reference="lst:coins_to_give"}.

##### Entry Point (Main) {#par:entry_point_main_}

The program's code always performs the same task. It coordinates the
actions the program is performing. The pseudocode for this was shown in
Listing [\[lst:data-simple-change-pseudo\]](#lst:data-simple-change-pseudo){reference-type="ref"
reference="lst:data-simple-change-pseudo"}. This code will need to store
the value of the change. It will get the value to store in this by
calling `Get Change Value`. It will call `Give Change` for each of the
coin values, and pass this variable to the procedure for it to update.
These actions are shown in
Figure [3.17](#fig:simple-change-seq){reference-type="ref"
reference="fig:simple-change-seq"}.

-   The `Entry Point (Main)` needs a local variable to store the
    `Change Value`.

Thinking further about the program there are also the constant values of
the different coins. These values are almost taken for granted when you
think about giving change yourself, but remember the computer is
unintelligent so you need to specify *everything* for it. The values of
the coins can be coded using a constant for each coin. Using constants
is a better option than hard coding these values directly in the
program, as the name helps provide a context for the value when it is
used.

![Sequence Diagram for the Change
Calculator](./topics/storing-using-data/images/SimpleCalcSeq.pdf){#fig:simple-change-seq
width="\\textwidth"}

#### Change Calculator Design Overview {#ssub:change_calculator_design_overview}

The Change Calculator program contains the following artefacts:

-   **Constants**:

    -   `TWO_DOLLARS` with value 200, represents the value of a \$2 coin

    -   `ONE_DOLLAR` with value 100, represents the value of a \$1 coin

    -   `FIFTY_CENTS` with value 50, represents the value of a 50c coin

    -   `TWENTY_CENTS` with value 20, represents the value of a 20c coin

    -   `TEN_CENTS` with value 10, represents the value of a 10c coin

    -   `FIVE_CENTS` with value 5, represents the value of a 5c coin

-   **Functions**:

    -   `Get Change Value`: Calculates and returns the amount of change
        that needs to be given by the program. This function has the
        following:

        -   **Local Varaibles**:

            -   **`Cost of Item`**: Stores the value of the item being
                purchased. This design uses cents as its base to make
                the calculations easier, avoiding rounding issues
                involved in using floating point values.

            -   `Payment`: This is used to store the value of the
                payment being made. Once again this will be in cents.

    -   `Coins to Give`: Calculates the number of coins to give,
        returning how many of these coins should be dispensed as part of
        giving this change. This has the following:

        -   **Parameters**:

            -   `Change`: The value of the change that is to be given.

            -   `Coin Value`: The value of the coin that is being
                dispensed.

-   **Procedures**:

    -   `Give Change`: Calculates the change, and outputs the coin
        details to the Terminal. This shows the number and description
        of the change given. This requires the following:

        -   **Parameters**:

            -   **`Change Value`** (by reference): The variable that is
                storing the amount of change to be given.

            -   **`Coin Value`**: The value of the coin to issue.

            -   **`Coin Description`**: The description of the coin.

        -   **Local Variables**:

            -   `To Give`: Stores the number of the coin to give in
                change, used to update the `Change Value` and to output
                the message.

    -   **`Entry Point (Main)`**: this coordinates the overall activity
        of the program. Getting the amount of change using
        `Get Change Value`, and using `Give Change` to issue the change
        for each coin.

        -   **Variables**:

            -   `Change Value`: Keeps track of the current amount of
                change that has to be given to the user.

In addition to these artefacts the program will make use of some
procedures from the available libraries. This will include the
following:

-   **Output**: You need to use your language's procedures to write
    output to the Terminal.

-   **Input**: Languages also provide a means of reading values from the
    user. In C and Pascal these input procedures need to be passed the
    variables that you want the value assigned to. This uses pass by
    reference to enable the input procedure to store the values read
    into a variable for you.

#### Reviewing the design diagrams {#ssub:reviewing_the_design_diagrams}

Figure [3.16](#fig:simple-change-structure){reference-type="ref"
reference="fig:simple-change-structure"} shows the Structure Chart for
the Change Calculator. This shows the structure of the solution,
visually showing the functions and procedures and the calls between
them. This diagram has been enhanced to show the parameter values being
passed into the functions and procedures, and the values being returned.
These data flows are shown alongside the call arrow, with their own
smaller arrow to indicate the direction of the flow.

By reading
Figure [3.16](#fig:simple-change-structure){reference-type="ref"
reference="fig:simple-change-structure"} you can tell that
`Coins to Give` is going to be a function as it is returning data to
`Main`. `Give Change` could be a function or a procedure as it is
accepting and returning the `Change Value`, in this design it is a
procedure, and updates the value in the `Change Value` variable using
pass by reference. You can also see that `Coins to Give` and
`Give Change` both require parameters to accept the data being passed
into them.

The Structure Chart shows the static structure of the code, indicating
the calls between the functions and procedures, but not communicating
when these are called, or how many times. This dynamic information is
captured in the Sequence Diagram shown in Figure
[3.17](#fig:simple-change-seq){reference-type="ref"
reference="fig:simple-change-seq"}. This diagram shows the sequence in
which the function and procedure calls are performed. Notice that this
diagram has also been enhanced to show the data that flows into and out
of the functions and procedures.

The Sequence Diagram indicates how values are being returned. The return
of a function's value is shown by indicating the value on the returning
arrow. Notice the indication of this value on the arrow returning from
the call to `Get Change Value`, this indicates that `Get Change Value`
is a function. On the other hand, the brackets around the return value
from `Give Change` indicates that this is being performed by updating a
parameter that was passed using pass by reference. You can use this
information to determine how the design intends these to be coded. As
`Get Change Value` is directly returning a value it must be a function,
`Give Change` updates a parameter and does not return a value itself so
it must be a procedure.

#### Designing data in general {#ssub:designing_data_in_genera}

When you are designing a program you need to think about each function
or procedure, and determine if it requires data to be given to it to
enable it to perform its action. Any data it requires must then be
passed to it using Parameters. The key to understanding parameters is to
remember that each function/procedure is its own isolated domain. It can
have its own data, using local variables, and has its own instructions.
In most cases these instructions will need to be given some starting
data, some information upon which to act.

One strategy you can use to picture this is to put yourself in the place
of the function, or procedure. For example, what would you need to be
told in order to determine the number of coins to give in change for a
single coin type. You can not work out the answer without being told the
value of the change to be given, and the value of the coin. With these
two pieces of information you now have enough to calculate the required
output. For example, how many \$2 coins should be given for \$6.50 in
change. These two pieces of data can be used to calculate the answer 3.
This is exactly what is being coded in the `Coins to Give` function.

In this respect Procedures are the same. In order to give change, the
`Give Change` procedure needs some information. It can not act without
this information. If you needed to *give change*, you need to be told
something about the amount of change you are giving and the value of the
coin you are issuing. In this case, `Give Change` needs to be told the
`change value`, the `coin value`, and the description of that coin. With
these details the procedure can then code the steps needed to produce
its desired side effects.

Parameters are mostly used to pass a value into a function or procedure.
However, in some cases it is useful to pass in the *variable* rather
than the *value*. This is the case when you want the function or
procedure to *change* the value in the variable passed to it. Take the
`change value` parameter in `Give Change` as an example. When this
procedure issues some coins, the amount of change still to be given must
be updated. For example, when you issue '3 x \$2' for the \$6.50 in
change, the new change value will be 50c ($650 - (3 \times 200)$). By
passing the *variable* into `Give Change` it is possible for this code
to change the value in the callers variable, ensuring that the correct
change is given.

#### Modelling and Abstraction {#ssub:modelling_and_abstraction}

One of the main keys to understanding programs, and to creating good
designs, is to see how each artefact you create models something from
the problem. The procedures that you create model the processes,
performing actions that relate to the task at hand. The functions model
calculations that need to be performed, such as determining how many
coins should be given. In the same way the variables that you create
will store values related to the problem. Each variable **represents**
something, some information related to your program. The *thing* it
represents should be reflected in the name given to the variable.

The act of trying to model the problem in software is called
**abstraction**. It is the process of determining the features of the
problem that are essential to the solution, and capturing these in a way
that models reality but keeps only the details we care about. The
ability to create your own model is a skill that you learn with
experience, taking a lot of practice to really master.

### Writing the Code for the Change Calculator {#sub:writing_the_code_for_simple_change}

The pseudocode from Listing
[\[lst:data-simple-change-pseudo\]](#lst:data-simple-change-pseudo){reference-type="ref"
reference="lst:data-simple-change-pseudo"} shows the instructions, and
how these should be divided between the `Coins to Give` Function and
`Output Change Data` Procedure. At this stage these instructions need to
be translated into source code, so that they can be compiled and the
resulting program tested.

The following two sections, Section
[3.3](#sec:storing_and_using_data_in_c){reference-type="ref"
reference="sec:storing_and_using_data_in_c"} and Section
[3.4](#sec:storing_and_using_data_in_pascal){reference-type="ref"
reference="sec:storing_and_using_data_in_pascal"} , contain a
description of the syntax needed to create programs in the C and Pascal
programming languages that include Variable and Function declarations.

### Compiling and Running the Change Calculator {#sub:compiling_and_running_simple_change}

Once you have completed your program, you need to compile and test it.

1.  Open the **Terminal**[^26] program for your Operating System

2.  Use the `cd` command to move to the directory with your code, for
    example `cd /Users/acain/Documents/Code`

3.  Run the compiler with your program's code. See the language specific
    details below.

4.  Fix any compiler errors, using the tips from Section
    [2.2.5.1](#ssub:compiler_errors){reference-type="ref"
    reference="ssub:compiler_errors"} .

5.  Execute the program using `./SimpleChange` and check the results

#### Generating Test Data {#ssub:generating_test_data}

Now that our program is making greater use of data it becomes more
important to think about how to test your program. Testing is the
process of trying to locate issues with your code. There are two main
classifications for issues, **syntactic errors** and **semantic
errors**.

Syntactic errors indicate places in your code where you have not
correctly followed the syntax of the language. These are the easiest
kind of error to find as the compiler will not be able to compile the
program if its syntax is not correct. The errors that the compiler
report are all syntax errors. As you gain experience with a Language you
will find that you make fewer and fewer of these kinds of errors.

Semantic errors, on the other hand, will not be found by the compiler.
These are errors in the logic within the program. They are cases where
you have correctly structured the code, but the code itself does not get
the computer to perform the actions you require. These are the kinds of
errors that you need to learn to be able to detect yourself. Selecting
the right kind of test data is an important task when you start to think
about testing programs.

For the Change Calculator there are two inputs provided by the user. In
order to test this program you need to determine the values that are
passed to the `Cost of the Item` and the `Payment` inputs. You want to
choose values that can help you uncover any unexpected issues with the
programs code.

::: {#tab:simple_change_test_data}
  **Cost of Item**   **Payment**   **Expected Output**   **Reason**
  ------------------ ------------- --------------------- ----------------------------------------------------------------------------------------------
  \$2.50             \$5.00        1 x \$2, 1 x 50c      Basic test to check that the program works for a simple data input.
  \$0.15             \$4.00        1 of each coin        Generates \$3.85 in change, checking that each coin is used.
  \$0.05             \$0.10        1 x 5c                Check the output can be a single coin. Could also add tests to check other individual coins.
  \$0.60             \$1.00        2 x 20c               Check that 2 coins can be given.
  \$0.00             \$5.00        2 x \$2, 1 x \$1      Can it accept no cost of item, and give back payment?
  \$3.85             \$0.00        -1 of each coin       Check what happens if insufficient funds are provided.

  : Test Data for the Change Calculator
:::

The data we use to test an execution of a program is called a **Test
Case**. Each test case provides a set of input values, and the expected
results given this input. Example test cases for the Change Calculator
are shown in Table
[3.2](#tab:simple_change_test_data){reference-type="ref"
reference="tab:simple_change_test_data"}. This table shows the input
values, the expected output values, and some rational for why this test
case should be run. To perform the testing you run the program once for
each test case and check the output of the program against the expected
output. If there are any differences you know that their is a problem,
and need to check the program's code to find the source of the logic
errors.

## Storing and Using Data in C {#sec:storing_and_using_data_in_c}

### Implementing Change Calculator in C {#sub:implementing_simple_change_in_c}

Section
[3.2](#sec:using_these_concepts_storing_using_data){reference-type="ref"
reference="sec:using_these_concepts_storing_using_data"} of this chapter
introduced the 'Change Calculator' program, and its design. Its
implementation requires the definition of functions as well as
procedures. These functions and procedures accepted parameters and use
local variables.

This section of the chapter introduces the C syntax rules for
implementing these concepts using the C language. The C implementation
of the Change Calculator is shown in Listing
[\[lst:storing-data-c-simple-change\]](#lst:storing-data-c-simple-change){reference-type="ref"
reference="lst:storing-data-c-simple-change"}. To get support for pass
by reference you need to use C++, an extended version of the C language.
This means that the C version is an alternate design, with `Give Change`
becoming a function so that it can return the new change value. The C++
implementation is shown in
Section [3.3.2](#sub:implementing_change_calculator_using_cpp){reference-type="ref"
reference="sub:implementing_change_calculator_using_cpp"}.

``` {#lst:storing-data-c-simple-change .c caption="C code for the Change Calculator" label="lst:storing-data-c-simple-change"}
/*
* Program: simple-change.c
* Calculate the ideal change for a given transaction.
*/
#include "splashkit.h"

#define TWO_DOLLARS 200
#define ONE_DOLLAR 100
#define FIFTY_CENTS 50
#define TWENTY_CENTS 20
#define TEN_CENTS 10
#define FIVE_CENTS 5

int coins_to_give(int change, int coin_value)
{
    return change / coin_value;
}

int give_change(int change_value, int coin_value, string coin_desc)
{
    int to_give;
    
    to_give = coins_to_give(change_value, coin_value);
    write( to_string(to_give) + " x " + coin_desc + ", ");
    
    return change_value - to_give * coin_value;;
}

int get_change_value()
{
    string line;
    int cost_of_item;
    int payment;
    
    write("Cost of item (in cents): ");
    line = read_line();
    cost_of_item = convert_to_int(line);
    
    printf("Amount paid (in cents): ");
    line = read_line();
    payment = convert_to_int(line);
    
    return payment - cost_of_item;
}

int main()
{
    int change_value;
    change_value = get_change_value();
    
    write("Change: ");
    change_value = give_change(change_value, TWO_DOLLARS,  "$2");
    change_value = give_change(change_value, ONE_DOLLAR,   "$1");
    change_value = give_change(change_value, FIFTY_CENTS,  "50c");
    change_value = give_change(change_value, TWENTY_CENTS, "20c");
    change_value = give_change(change_value, TEN_CENTS,    "10c");
    change_value = give_change(change_value, FIVE_CENTS,   "5c");
    write_line();
    
    return 0;
}
```

### Implementing Change Calculator using C++ {#sub:implementing_change_calculator_using_cpp}

C++ contains a number of extensions that can make programming in C
easier. One of these extensions is the ability to use pass by reference.
The code in
Listing [\[lst:storing-data-cpp-simple-change\]](#lst:storing-data-cpp-simple-change){reference-type="ref"
reference="lst:storing-data-cpp-simple-change"} shows the C++
implementation of the Change Calculator. In this code `Give Change` is a
procedure with the `change_value` parameter being passed by reference.

``` {#lst:storing-data-cpp-simple-change .c++ caption="C++ code for the Change Calculator" label="lst:storing-data-cpp-simple-change"}
/*
* Program: simple-change.cpp
* Calculate the ideal change for a given transaction.
*/
#include <stdio.h>

#define TWO_DOLLARS 200
#define ONE_DOLLAR 100
#define FIFTY_CENTS 50
#define TWENTY_CENTS 20
#define TEN_CENTS 10
#define FIVE_CENTS 5

int coins_to_give(int change, int coin_value)
{
    return change / coin_value;
}

void give_change(int &change_value, int coin_value, const char *coin_desc)
{
    int to_give;
    
    to_give = coins_to_give(change_value, coin_value);
    change_value = change_value - to_give * coin_value;
    
    printf("%d x %s, ", to_give, coin_desc);
}

int get_change_value()
{
    int cost_of_item;
    int payment;
    
    printf("Cost of item (in cents): ");
    scanf("%d", &cost_of_item);
    
    printf("Amount paid (in cents): ");
    scanf("%d", &payment);
    
    return payment - cost_of_item;
}

int main()
{
    int change_value;
    change_value = get_change_value();
    
    printf("Change: ");
    give_change(change_value, TWO_DOLLARS,  "$2");
    give_change(change_value, ONE_DOLLAR,   "$1");
    give_change(change_value, FIFTY_CENTS,  "50c");
    give_change(change_value, TWENTY_CENTS, "20c");
    give_change(change_value, TEN_CENTS,    "10c");
    give_change(change_value, FIVE_CENTS,   "5c");
    printf("\n");
    
    return 0;
}
```

### C Variable Declaration {#sub:c_variable_declaration}

A Variable Declaration allows you to create a Variable in your Code. In
C you can declare variables in the program's code, and in Functions and
Procedures.

### C Program (with Global Variables and Constants) {#sub:c_program_with_global_variables}

You can declare global variables and constants within a C Program file.

### C Procedure Declaration (with Local Variables) {#sub:c_procedure_declaration_with_local_variables_}

The Functions and Procedures in C can contain declaration for s.

### C++ Assignment Statement {#sub:c_assignment_statement}

The assignment statement is used to store a value in a variable.

### C Procedure Declaration (with Parameters) {#sub:c_procedure_declaration_with_parameters_}

In C s can be declared in any Function or Procedure declaration.

#### C++ Reference Parameters {#ssub:c_reference_parameters}

C has limited support for pass by reference, but this feature was added
with the extensions in the C++ language. The following syntax shows how
to declare a parameter that will be passed by reference. Please note
that this is not standard C code, and will require you to use a C++
compiler.

### C Procedure Call (with pass by reference) {#sub:c_procedure_call_with_pass_by_reference}

Many languages support pass by reference in the compiler. This is where
the compiler manages the passing of the reference for you in the
background. Unfortunately C does not do this transparently and you need
to manually pass the reference yourself. Its good to know that the
concept remains the same, but it does mean that you must manually add
code to achieve this.

### C Terminal Input {#sub:c_terminal_input}

C comes with a range of s that provide reusable programming artefacts,
including reusable and s. The `stdio.h` refers to the Standard
Input/Output library, and including code to read input from the
Terminal. The `scanf` function is used to read data from the Terminal.

::: {#tbl:scanf parameters}
      **Function Prototype**     
  ------------------------------ ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                 
   `int scanf(char *format, …)`  
                                 
           **Returns**           
              `int`              The number of values read by `scanf`.
          **Parameter**          **Description**
            ` format `           The format specifier describing what is to be read from the Terminal. See Table [3.4](#tbl:format specifiers){reference-type="ref" reference="tbl:format specifiers"}.
                                 
               `…`               The variables into which the values will be read. There must be at least as many variables as format tags in the format specifier.

  : Parameters that must be passed to `scanf`
:::

The `scanf` function is controlled by the `format` parameter. This
parameter tells `scanf` what it must read from the input. Details of how
to construct the `format` String are shown in
Table [3.4](#tbl:format specifiers){reference-type="ref"
reference="tbl:format specifiers"}.

::: {#tbl:format specifiers}
                           **Description**                                                                                                                                                                                                                                                                                                                                                                 **Example Usage**
  ------------------------ ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- -----------------------------
       *white space*       Skips white space at this point in the input.                                                                                                                                                                                                                                                                                                                                   `scanf("{ } \%d", \&age);`
   *non white space*[^27]  Matches the input against characters, skipping this text in the input. Fails if input does not match. The example looks for 'age: ' and reads the following integer.                                                                                                                                                                                                            `scanf("age: \%d", \&age);`
        *format tag*       The tag indicates the kind of data to read and store in a Variable. This starts with a percent character. See Table [3.5](#tbl:scanf format tag){reference-type="ref" reference="tbl:scanf format tag"} and Figure [\[csynt:scanf-format-string\]](#csynt:scanf-format-string){reference-type="ref" reference="csynt:scanf-format-string"} for the syntax of the format tags.   `scanf("\%d", \&age);`

  : Elements of the Format String in `scanf`
:::

::: {#tbl:scanf format tag}
                 **Description**                                                                                                                           **Example Usage**
  -------------- ----------------------------------------------------------------------------------------------------------------------------------------- ------------------------------------------
       `*`       Read the data, but ignore it. Does not store the value in a Variable.                                                                     `scanf("\%*d");`
                                                                                                                                                           
    **Width**    **Description**                                                                                                                           **Example Usage**
     *number*    The maximum number of characters to read in the current operation.                                                                        `scanf("\%3d", \&age);`
                                                                                                                                                           
   **Modifier**  **Description**                                                                                                                           **Example Usage**
       `h`       Reads a `short int` for the `d` or `i` Types.                                                                                             `scanf("\%hi", \&age);`
       `l`       Reads a `long int` for the `d` or `i` Types, or a `double` for `f`.                                                                       `scanf("\%lf \%li", \&height, \&count);`
       `L`       Reads a `long double` for `f`.                                                                                                            `scanf("\%Lf", \&range);`
                                                                                                                                                           
     **Type**    **Data Read**                                                                                                                             **Example Usage**
       `c`       A single character.                                                                                                                       `scanf("\%c", \&ch);`
    `d` or `i`   Decimal integer. This is able to read a starting + or - if present.                                                                       `scanf("\%d", \&height);`
       `f`       Decimal floating point number. Can be signed, or in scientific notation.                                                                  `scanf("\%f", \&radius);`
       `s`       Text data. Should be preceded by the number of characters to read. The c-string must have sufficient space to store the data read[^28].   `scanf("\%40s", name);`
   `[pattern]`   Text data. As with `%s`, but this allows you to specify the pattern of characters that can be read.                                       `scanf("\%7[1234567890]", num_text);`
   `[^pattern]`  Text data. As with `%s`, but this allows you to specify the pattern of characters that can **not** be read.                               `scanf("%40[^\n]", name);`

  : Details for `scanf`'s Format Tag type, specifiers, modifiers, and
  width
:::

### C Program (with Functions) {#sub:c_program_with_functions}

### C Function Declaration {#sub:c_function_declaration}

### C Procedure Declaration (as Function) {#sub:c_procedure_declaration_as_function_}

C does not have a strong separation of Functions and Procedures.
Instead, in C all Procedures are Functions that return a special `void`
. This means that the standard C Syntax does not include a separate
definition for Procedure declarations. Even though C does not have
direct syntax for Procedures, the concept is still very important.

### C Function Call {#sub:c_function_call}

### Return Statement {#sub:return_statement}

### C Statement (with Return Statement) {#sub:c_statement_with_return_statement_}

The different statements of a Programming Language allow you to command
the Computer to perform different actions. We have covered three kinds
of statements so far.

## Storing and Using Data in Pascal {#sec:storing_and_using_data_in_pascal}

### Implementing Change Calculator in Pascal {#sub:implementing_simple_change_in_pas}

Section
[3.2](#sec:using_these_concepts_storing_using_data){reference-type="ref"
reference="sec:using_these_concepts_storing_using_data"} of this chapter
introduced the 'Change Calculator' program, and its design. Its
implementation requires the definition of functions as well as
procedures. These functions and procedures accepted parameters and use
local variables.

This section of the chapter introduces the Pascal syntax rules for
implementing these concepts using the Pascal language. The Pascal
implementation of the Change Calculator is shown in Listing
[\[lst:storing-data-pas-simple-change\]](#lst:storing-data-pas-simple-change){reference-type="ref"
reference="lst:storing-data-pas-simple-change"}.

``` {#lst:storing-data-pas-simple-change .pascal caption="Pascal code for the Change Calculator" label="lst:storing-data-pas-simple-change"}
// Calculate the ideal change for a given transaction.
program SimpleChange;

const 
    TWO_DOLLARS = 200;
    ONE_DOLLAR = 100;
    FIFTY_CENTS = 50;
    TWENTY_CENTS = 20;
    TEN_CENTS = 10;
    FIVE_CENTS = 5;

function CoinsToGive(change, coinValue: Integer): Integer;
begin
    result := change div coinValue;    // integer division... ignore remainder
end;

procedure GiveChange(var changeValue: Integer; coinValue: Integer; coinDesc: String);
var
    toGive: Integer;
begin
    toGive := CoinsToGive(changeValue, coinValue);
    changeValue := changeValue - toGive * coinValue;
    
    Write(toGive, ' x ', coinDesc, ', ');
end;

function GetChangeValue(): Integer;
var
    costOfItem, payment: Integer;
begin
    Write('Cost of item (in cents): ');
    ReadLn(costOfItem);
    
    Write('Amount paid (in cents): ');
    ReadLn(payment);
    
    result := payment - costOfItem;
end;

procedure Main();
var
    changeValue: Integer;
begin
    changeValue := GetChangeValue();
    
    Write('Change: ');
    GiveChange(changeValue, TWO_DOLLARS,  '$2');
    GiveChange(changeValue, ONE_DOLLAR,   '$1');
    GiveChange(changeValue, FIFTY_CENTS,  '50c');
    GiveChange(changeValue, TWENTY_CENTS, '20c');
    GiveChange(changeValue, TEN_CENTS,    '10c');
    GiveChange(changeValue, FIVE_CENTS,   '5c');
    WriteLn();
end;

begin
    Main();
end.
```

### Pascal Program (with Global Variables and Constants) {#sub:pas_program_with_global_variables}

You can declare global variables and constants within a Pascal Program
file.

### Pascal Procedure Declaration (with Local Variables) {#sub:pas_procedure_declaration_with_local_variables_}

The functions and procedures in Pascal can contain declaration for s.

### Pascal Assignment Statement {#sub:pas_assignment_statement}

The assignment statement is used to store a value in a variable.

### Pascal Procedure Declaration (with Parameters) {#sub:pas_procedure_declaration_with_parameters_}

In Pascal s can be declared in any function or procedure declaration.

### Pascal Terminal Input {#sub:pas_terminal_input}

Pascal comes with a range of s that provide reusable programming
artefacts, including reusable and s. The `System` unit includes
procedures to read input from the Terminal. The `ReadLn` procedure is
used to read data from the Terminal.

::: {#tbl:readln parameters}
   **Procedure Prototype**  
  ------------------------- ----------------------------------------------------------------------------------------------------------------------
                            
   `procedure ReadLn( … )`  
                            
        **Parameter**       **Description**
            ` …`            The `ReadLn` procedure takes a variable number of parameters. A value is read from the Terminal for each parameters.

  : Parameters that must be passed to `ReadLn`
:::

### Pascal Program (with Functions) {#sub:pas_program_with_functions}

Your Pascal program can contain definitions for your own s.

### Pascal Function Declaration {#sub:pas_function_declaration}

### Pascal Function Call {#sub:pas_function_call}

## Understanding Data {#sec:understanding_dat}

This Chapter has introduced the idea of creating Variables within your
program's code to store values. These variables can be stored in Global
or Local Variables, and passed to Functions or Procedures using
Parameters.

This Section will help you answer the following questions:

-   How do local and global variables work?

-   What happens with Parameters when Functions and Procedures are
    called?

-   How do Function's return a value?

-   How does Pass by Reference work?

-   How can I demonstrate or check the execution of a program with
    Variables?

### Variables {#sub:visualise_variables}

To get started let us have a look at the basic concept of Variables.
Rather than working through the Change Calculator program we can use a
simpler program to start with. Understanding this will then help you
understand what is happening in the Change Calculator. The program we
will use is in Listing
[\[plst:assignment-test\]](#plst:assignment-test){reference-type="ref"
reference="plst:assignment-test"}. This program will contain two
variables that are local to the `main` function in C or the `Main`
procedure in Pascal: `val` and `range`.

#### Visualising Variables in the Computer {#ssub:visualising_variables_in_the_computer}

To get started understanding this let us return to the Conceptual
Computer we have been using to explain how these concepts work.

-   Test Assignment's Program Launches, putting Main on the Stack.

-   The assignment statement stores a value in the `val` Variable.

-   The second assignment statements stores a value in the `range`
    Variable.

-   The values of the Variables are output to the Terminal.

#### Test Assignment's Program Launches, putting Main on the Stack. {#ssub:test_assignment_s_program_launches_putting_main_on_the_stack_}

When the program starts space is allocated on the Stack for the
program's Main Function or Procedure. When a Function or Procedure has
s, these are also allocated onto the Stack.

![Program's entry point is loaded onto the
Stack](./topics/storing-using-data/images/vis-local-var-1.pdf){#fig:vis-data-local-1
width="\\textwidth"}

#### The assignment statement stores a value in the `val` Variable. {#ssub:the_assignment_statement_stores_a_value_in_the_val_variable_}

The first action of this program is to store the value 23 in the `val`
Variable.

![The Assignment Statement assigns a value to the `val`
Variable](./topics/storing-using-data/images/vis-local-var-2.pdf){#fig:vis-data-local-2
width="\\textwidth"}

#### The second assignment statements stores a value in the `range` Variable. {#ssub:the_second_assignment_statements_stores_a_value_in_the_range_variable_}

The next step in the program is to store the value `1 + val / 100` in
the `range` Variable. This must read the value from the `val` Variable
to determine the value to be stored.

![The Assignment Statement assigns a value to the `val`
Variable](./topics/storing-using-data/images/vis-local-var-3.pdf){#fig:vis-data-local-3
width="\\textwidth"}

#### The values of the Variables are output to the Terminal. {#ssub:the_values_of_the_variables_are_output_to_the_terminal_}

The third, and final, instruction in the program's main function or
procedure is to output the values of each of these variables to the
Terminal. This uses the values from within the Variables to determine
what is output.

![The Assignment Statement assigns a value to the `val`
Variable](./topics/storing-using-data/images/vis-local-var-4.pdf){#fig:vis-data-local-4
width="\\textwidth"}

### Parameters, Locals and Globals {#sub:visualise_parameters}

There are three locations at which you can declare variables. *Global
Variables* are declared within the program, *Local Variables* are
declared within Functions and Procedures, and *Parameters* are used to
pass values into Functions and Procedures.

Listing
[\[plst:test_proc_data\]](#plst:test_proc_data){reference-type="ref"
reference="plst:test_proc_data"} is the code that we will use to
demonstrate how these Variables work when code is executed by the
Computer. This program does not have any meaningful purpose beyond
interacting with Global Variables, Local Variables, and Parameters, so
do not read too much into the actions being performed. What is important
is to examine how the different variables work when the code is
executed.

This Section will examine these Variables by looking at the following:

-   
-   
-   
-   
-   
-   

#### Memory Partitions include space for Global Variables {#ssub:memory_partitions_include_space_for_global_variables}

When the program is executed the Operating System allocates it space in
memory. This space is partitioned into different areas, with each area
storing an aspect of the program's data or instructions.

![Memory is partitioned into spaces for the Code, Stack, and Global
Variables](./topics/storing-using-data/images/vis-globals-1.pdf){#fig:vis-globals-1
width="90%"}

#### Program is loaded into memory {#ssub:vars-program_is_loaded_into_memory}

When the program is executed its global variables are allocated space,
and then the main code is executed.

![The Global Variables are allocated space, then the Entry Point is
loaded onto the
Stack](./topics/storing-using-data/images/vis-globals-2.pdf){#fig:vis-globals-2
width="\\textwidth"}

#### Initial Commands Occur {#ssub:initial_commands_occur}

The program executes each of the statements, one at a time. Figure
[3.24](#fig:vis-globals-3){reference-type="ref"
reference="fig:vis-globals-3"} shows the values in memory by the time
the Computer has completed the execution of the first two commands.

![The instructions store values in the different
variables](./topics/storing-using-data/images/vis-globals-3.pdf){#fig:vis-globals-3
width="\\textwidth"}

#### Passing Parameters in a Procedure Call {#ssub:passing_parameters_in_a_procedure_call}

At this point the Computer is executing the call to `Test Params`. This
involves passing a value to the `val` parameter.

![The *value* of the Argument is passed to the Parameter when the
Procedure is
called](./topics/storing-using-data/images/vis-globals-4.pdf){#fig:vis-globals-4
width="\\textwidth"}

#### Test Params Commands are Executed {#ssub:test_params_commands_are_executed}

The commands in `Test Params` are executed one at a time. These
instructions will interact with the Variables that are visible to the
`Test Params` Procedure, which include its Local Variables as well as
the Global Variables.

![Steps in `Test Params` execute, altering values in the
Variables](./topics/storing-using-data/images/vis-globals-5.pdf){#fig:vis-globals-5
width="\\textwidth"}

#### Control returns to Main when Test Params ends {#ssub:control_returns_to_main_when_test_params_ends}

When `Test Params` ends control returns to the program's main code. The
space allocated to `Test Params` is now released, including the space
allocated to the `val` Parameter and the `local` Variable.

![Control returns to Main when `Test Params`
ends](./topics/storing-using-data/images/vis-globals-6.pdf){#fig:vis-globals-6
width="\\textwidth"}

### Function Return Values {#sub:visualise_function_return_values}

The next kind of data is the result returned by a call to a Function. A
Function is just like a Procedure, except that it calculates a value and
is therefore called from within an Expression. To examine how this works
within the Computer we will see how the code in Listing
[\[plst:function-test\]](#plst:function-test){reference-type="ref"
reference="plst:function-test"} executes. This code declares a `Square`
function that calculates the square of a number (`val` passed in as a
Parameter).

This Section will examine how Functions return values by looking at the
following:

-   
-   
-   
-   

#### Test Functions is loaded into memory {#ssub:test_functions_is_loaded_into_memory}

When the program is executes the Operating System loads its code into
memory, and starts the main code running on the Stack.

![Code is loaded into Memory and Main is loaded onto the
Stack](./topics/storing-using-data/images/vis-func-1.pdf){#fig:vis-func-1
width="\\textwidth"}

#### Square Function is Called {#ssub:square_function_is_called}

The `Square` function is called in the same way that a Procedure would
be called. It is given space on the Stack, and its instructions are run.

![`Square` is called, with the value 5 passed to its `val`
Parameter](./topics/storing-using-data/images/vis-func-2.pdf){#fig:vis-func-2
width="\\textwidth"}

#### Square's result is calculated {#ssub:square_s_result_is_calculated}

Within the `Square` function its result is calculated and returned.

![`Square` prepares the value that will be returned to the
caller](./topics/storing-using-data/images/vis-func-3.pdf){#fig:vis-func-3
width="\\textwidth"}

#### Returned value is used {#ssub:returned_value_is_used}

When the Function ends, control is returned to the main code. This
called the Function in order to evaluate an Expression. That expression
will now continue to be evaluated using the value returned by the
Function.

![The result of calling `Square` is used within the Expression, and the
value is stored in
`answer`](./topics/storing-using-data/images/vis-func-4.pdf){#fig:vis-func-4
width="\\textwidth"}

### Pass by Reference {#sub:visualise_pass_by_reference}

Working with data involves working with Variables of one form or
another. When thinking about Variables it is important to realise the
two aspects of each Variable: the *Variable* and its *value*. The
*Variable* is a location in memory, it is a place at which you can store
a value. The *value* within the Variable is a separate concern. This
value can be read from the Variable in Expressions.

This Section will examine how Pass by Reference works by looking at the
following:

-   
-   
-   
-   

#### Main starts and a prompt is written for the user {#ssub:main_starts_and_a_prompt_is_written_for_the_user}

The Program starts as normal with the Operating System allocating space
for the Stack, Code, and Global Variables. Main is then loaded onto the
Stack and its instructions are executed.

![Code is loaded into Memory, Main is started, and the first instruction
outputs a
prompt](./topics/storing-using-data/images/vis-ref-1.pdf){#fig:vis-ref-1
width="\\textwidth"}

#### The Input Procedure is called to read a value into `age` {#ssub:the_input_procedure_is_called_to_read_a_value_into_age}

The next instruction in the main code is to call the Language's input
procedure to read a value from the user and to store that value in the
`age` variable. In order to achieve this you must pass the `age`
variable itself to this procedure. That will enable the input code to
store the value it reads into the variable that you give it.

![The input procedure starts, and the age Variable is passed to the
Parameters](./topics/storing-using-data/images/vis-ref-2.pdf){#fig:vis-ref-2
width="\\textwidth"}

#### The Input Procedure stores the value read from the user into `age` {#ssub:the_input_procedure_stores_the_value_read_from_the_user_into_}

The language's input procedures are responsible for reading values from
the user and storing these into Variables for you. The instructions in
this code will do the work to convert the data from the text the user
enters into the types required by the Variables the values are being
read into.

![Within the input code the address is used to find where to store the
value read from the
user](./topics/storing-using-data/images/vis-ref-3.pdf){#fig:vis-ref-3
width="\\textwidth"}

#### Control returns to Main and `age` has been updated with the value from the user {#ssub:control_returns_to_main_and_}

When control returns to the main code, the `age` variable has been
updated to include the values read from the user.

![Control returns to Main, and the value of age has been updated by the
input
code](./topics/storing-using-data/images/vis-ref-4.pdf){#fig:vis-ref-4
width="\\textwidth"}

### Hand Execution with Variables {#sub:hand_execution_with_variables}

One valuable skill you need to develop is the ability to step through a
program in the same way the computer will. You will find that this skill
will get more and more important as you progress to write larger
programs. You will use this skill to help you detect and correct any
**semantic errors**, in a process commonly referred to as **debugging**.

Semantic errors are errors in the logic of your program. This means that
the program compiles, but does not do what you want it to. The tests
that you use will help you locate these errors, but in many cases you
will need to work out *why* the error is occurring so that you can make
the required changes.

One of the better techniques to help you make these corrections is to
reading your code, and to think about the actions the computer is
performing. Often these errors only require one or two lines of code to
change, but before you can make these changes you need to identify
*where* the error is occurring, *why* it is occurring, and *how* you can
correct the issue. By reading your code, and hand executing it you have
the best chance of working out *where* the problem is, and *why* the
problem is occurring.

These activities are very popular with Programming Tests, both in
University subjects and job interviews. Being able to execute some code
by hand means that you really understand what the code is doing. Once
you have this level of understanding you just need to add some
imagination and experience to become a very valuable software developer.

The steps you need to perform are:

-   
-   
-   
-   

#### Locate the Function or Procedure to Test {#ssub:locate_the_function_or_procedure_to_test}

Hand executing an entire program would take a very long time. As a
result you want to narrow down your search to one or two Functions
and/or Procedures. You can use your Structure Charts, or knowledge of
the program's structure, to help you narrow down your options. Think
about where the test failed, and where the code for this is located in
your Program. With a small program you are likely to know immediately
where the issue is, but as the program size grows it will become more
difficult to know exactly where the problem is.

Listing [\[plst:to-test\]](#plst:to-test){reference-type="ref"
reference="plst:to-test"} contains the pseudocode for a Procedure that
will calculate and output the area of a Trapezoid. Unfortunately this
code contains a number of errors. The following sections will
demonstrate how to execute this procedure by hand. If this were a
programming test you may be asked a question such as:

> '*What is the value in area when this program is run and the user
> enters **5** for base 1, **7** for base 2, and **4** for height?*'

Executing this by hand will give you the necessary skills to be able to
answer questions such as this.

#### Turn off your brain {#ssub:turn_off_your_brain}

Once you have located the Function or Procedure that you need to test
you can start to execute it. The main obstacle in most peoples way is,
unfortunately, their brain. You are going to *think* about what you
*wanted* the program to do. This is something the computer *cannot* do.
The computer is unintelligent. It is a machine that does as you command.
To run this program as the computer does you will need to **stop
thinking** and start doing as commanded.

Rules:

-   Do not think about what you *want* or *expect* the program to do.

-   Perform the commands as they are, one at a time

-   You cannot remember anything that is not written down.

-   Focus only on the current command (Statement). Do not look at other
    Statements!

-   Perform the actions you are commanded to perform, and only those
    actions. Do not do more than you are commanded to.

-   Use the language's Statements to determine the actions you must
    perform:

    -   An will evaluate its expression, and assign a value to a
        variable.

    -   A runs the code in a Procedure, a runs the code in a Function.
        To optimise this step you can perform the actions within called
        Functions or Procedures without stepping through their code in
        most cases.

#### Setup your 'memory' {#ssub:setup_your_memory}

One of the rules when running your code by hand is that you can not
remember anything. The values you use must come from the 'memory' that
you setup for the Function or Procedure. So, the first step for
executing this code performs the actions that called the procedure: you
need to *allocate space* on the stack. You can simulate this using a
piece of paper. This piece of paper will be the 'memory' used by the
code as it executes. To set this memory up you will need to do the
following:

1.  Get a blank piece of paper.

2.  Write `Print Trapezoid Area` to remember that is the procedure you
    are in.

3.  Do the following for any Variables[^29] in the Function or
    Procedure:

    1.  **Draw a box** to represent the Variable. Make sure that it is
        large enough for you to write values inside.

    2.  **Write the name** of the Variable next to the box.

    3.  If the Variable is a Parameter, copy the value from the argument
        into the box, otherwise leave it empty.

When you have finished these steps for the code in Listing
[\[plst:to-test\]](#plst:to-test){reference-type="ref"
reference="plst:to-test"} you should have a piece of paper that looks
like the image in Figure [3.36](#fig:hand-exe-1){reference-type="ref"
reference="fig:hand-exe-1"}. The empty boxes represent the different
Variables, the fact we have not written a Value into these boxes tells
us that at this point the value has not been set, and therefore should
not be used.

![Draw space for all of the Function or Procedure's
Variables](./topics/storing-using-data/images/hand-exe-1.jpg){#fig:hand-exe-1
width="70%"}

#### Run the steps one at a time {#ssub:run_the_steps_one_at_a_time}

Now that the memory has been initialised it is time to run each
instruction in the code. You need to do this **one instruction at a
time**. It does not matter how large the code is, the computer will
always run it one Statement at a time. Do not think, just read the
Statement and perform the action.

1.  Step 1 calls the output procedure of the Language. This will output
    the text '*Enter base 1:* ' to the Terminal. You do not need to step
    into the internal actions of each Function or Procedure called, as
    long as you can replicate the actions that it would perform. If you
    had been asked '*What is output by the the call to
    `Print Trapezoid Area`?*' it would be important to put this in the
    answer, but as we just want to know the final result you can skip
    this.

2.  Step 2 reads a value from the user and stores it in `base1`. The
    user is responding to the prompt from step 1, and therefore enters
    *5*, the value indicated in the question. As the `base1` variable is
    passed to this call you now write **5** into the `base1` box,
    representing the fact that this Variable now has the value *5*. See
    Figure [3.37](#fig:hand-exe-2){reference-type="ref"
    reference="fig:hand-exe-2"}.

3.  Step 3 outputs the next prompt, '*Enter base 2:* '.

4.  Step 4 reads a value from the user, and stores it in **`base1`**.
    This is the source of the first error. This is most likely the
    result of a copy/paste error, where the developer has copied the
    instructions to prompt the user and read a value into `base1`, and
    then changed it to read a value into `base2`. When you copy and
    paste code you need to pay extra attention to making sure that the
    pasted code is changed correctly.When this code is run a new value
    will be written into the `base1` Variable. Cross out the old value,
    and write in the new value. The value entered in this case is **7**,
    as indicated in the question. See Figure
    [3.38](#fig:hand-exe-3){reference-type="ref"
    reference="fig:hand-exe-3"}.

5.  Step 5 outputs the prompt for height.

6.  Step 6 reads **4** from the user and stores it into the `height`
    Variable. See Figure [3.39](#fig:hand-exe-4){reference-type="ref"
    reference="fig:hand-exe-4"}

7.  Step 7 now reads the values from `height`, and `base1`. The equation
    for calculating the area of a Trapezoid is
    $height ( \frac{base 1 + base 2}{2} )$, this has been coded
    incorrectly again, with several errors. Firstly `base1` is read
    twice, and `base2` is not used at all. Second, using BODMAS the
    current code is actually performing the equation
    $height (base 1 + \frac{base 1}{2})$. Performing this calculation
    you will find that the value stored in `area` is
    $4 ( 7 + (7 / 2)) = 42$.

8.  Step 8 outputs the area to the Terminal, displaying '*Trapezoid area
    is 42*'. See Figure [3.40](#fig:hand-exe-5){reference-type="ref"
    reference="fig:hand-exe-5"}.

At this point the Procedure has ended, and you have collected all of the
details about how the program actually works. You need to use this
information to determine where the error is. If this failed to find the
problem then the problem may be located elsewhere in the program's code,
or you have misread something. Getting someone else to help you debug
your programs is always a good option, they are likely to see things
that you may miss as the code's developer.

![The value 5 is stored in the `base1`
Variable](./topics/storing-using-data/images/hand-exe-2.jpg){#fig:hand-exe-2
width="70%"}

![The value 7 is stored in the `base1`
Variable](./topics/storing-using-data/images/hand-exe-3.jpg){#fig:hand-exe-3
width="70%"}

![The value 4 has been stored in
`height`](./topics/storing-using-data/images/hand-exe-4.jpg){#fig:hand-exe-4
width="70%"}

![The value 42 is stored in
`area`](./topics/storing-using-data/images/hand-exe-5.jpg){#fig:hand-exe-5
width="70%"}

### Summary {#sub:visualise_data_summary}

In this Section you have seen how Variables work within the Computer,
and been introduced to the idea of Functions. With these concepts you
can now work more meaningfully with the data in your programs.

## Data Examples {#sec:data_examples}

### Times Table {#sub:times_table}

This program prints out the times table for a number entered by the
user, displaying from 1 x n to 10 x n. The description of the program is
in Table [3.7](#tbl:data-times-table){reference-type="ref"
reference="tbl:data-times-table"}, the pseudocode in Listing
[\[lst:data-times-pseudo\]](#lst:data-times-pseudo){reference-type="ref"
reference="lst:data-times-pseudo"}, the C code in Listing
[\[lst:data-times-c\]](#lst:data-times-c){reference-type="ref"
reference="lst:data-times-c"}, and the Pascal code in Listing
[\[lst:data-times-pas\]](#lst:data-times-pas){reference-type="ref"
reference="lst:data-times-pas"}.

::: {#tbl:data-times-table}
  **Program Description**   
  ------------------------- ------------------------------------------------
  **Name**                  *Times Table*
                            
  **Description**           Displays the Times Table from 1 x n to 10 x n.

  : Description of the Times Table program
:::

### Circle Area {#sub:circle_area_data}

This program prints out the area of a circle. The description of the
program is in Table [3.8](#tbl:data-circle-area){reference-type="ref"
reference="tbl:data-circle-area"}, the pseudocode in Listing
[\[lst:data-circle-areas-pseudo\]](#lst:data-circle-areas-pseudo){reference-type="ref"
reference="lst:data-circle-areas-pseudo"}, the C code in Listing
[\[lst:data-circle-areas-c\]](#lst:data-circle-areas-c){reference-type="ref"
reference="lst:data-circle-areas-c"}, and the Pascal code in Listing
[\[lst:data-circle-areas-pas\]](#lst:data-circle-areas-pas){reference-type="ref"
reference="lst:data-circle-areas-pas"}.

::: {#tbl:data-circle-area}
  **Program Description**   
  ------------------------- -------------------------------------------------------------------------------------------
  **Name**                  *Circle Areas*
                            
  **Description**           Displays the Circle Areas for circles with radius from 1.0 to 5.0 with increments of 0.5.

  : Description of the Circle Areas program
:::

#### Water Tank {#ssub:water_tank}

The *Water Tank* program draws four water tanks to the terminal. Each
water tank is drawn as a cylinder that fills a given area on the screen,
and shows its current water level. An example execution is shown in
Figure [3.41](#fig:water-tank-img){reference-type="ref"
reference="fig:water-tank-img"}.

::: {#tbl:data-water-tanks}
  **Program Description**   
  ------------------------- ------------------------------------------------------------------------------------------------------------------------------
  **Name**                  *Water Tank*
                            
  **Description**           Displays calculates and displays 'Water Tanks'. Each tank has a position on the screen, a width, height, and a percent full.

  : Description of the Water Tanks program
:::

![Example execution of the Water Tank
program](./topics/storing-using-data/examples/WaterTank.png){#fig:water-tank-img
width="70%"}

### Bicycle Race {#sub:bicycle_race}

The Bicycle Race program will simulate a thirty second sprint race
between a number of bicycles. The race has a standing start, and then
each racer accelerates as fast as they can for thirty seconds. The
winner is the racer who makes it the furthest.

::: {#tbl:data-bike-race}
  **Program Description**   
  ------------------------- --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  **Name**                  *Bike Race*
                            
  **Description**           Calculates the position of seven bikes at the end of a timed race, drawing the final positions. Each bike's position is calculated based on a random acceleration over the duration of the race.

  : Description of the Bike Race program
:::

-   You can calculate distance the racers cover using
    Equation [\[eq:acceleration\]](#eq:acceleration){reference-type="ref"
    reference="eq:acceleration"}.

    -   *s* is the distance covered.

    -   *u* is the starting speed

    -   *t* is time.

    -   *a* is acceleration.

    $$s = ut + \frac{a t^2}{2}
        \label{eq:acceleration}$$

-   This race has a standing start, so the initial speed of each racer
    will be 0.

-   The time for the race is 30 seconds, this is constant.

-   Each racer will have a randomly determined acceleration, with a
    maximum acceleration of 10 $pixels/second^2$

![Example execution of the Bike Race
program](./topics/storing-using-data/examples/BikeRace.png){#fig:bike-race-img
width="60%"}

## Data Exercises {#sec:data_exercises}

### Concept Questions {#sub:concept_questions_data}

Read over the concepts in this chapter and answer the following
questions:

1.  What is a ?

2.  What is the relationship between a variable and a value?

3.  What is the relationship between a variable and a type?

4.  Where can you use variables? Think both reading the value, and
    storing a new value.

5.  What does it mean if a variable appears on the right hand side of an
    assignment? What will happen to this variable when the code is run?

6.  What does it mean if a variable appears on the left hand side of an
    assignment? What will happen to this variable when the code is run?

7.  What is a ? How does it differ from a variable?

8.  What is a local variable? What code can access the value in a local
    variable?

9.  What is a global variable? What code can access the value in a
    global variable?

10. Why is it considered good practice to use local variable, but not
    global variables?

11. How do parameters help make procedures more powerful?

12. What are the two parameter passing mechanisms for passing
    parameters? How are they different?

13. When would you use each of the parameter passing mechanisms? For
    what kind of parameters?

14. How does the Terminal input procedure store a value in the variable
    you pass to it? What kind of parameter passing is involved here?

15. What statement was introduced in this chapter?

16. What does this statement allow you to do?

17. What is a function? How does it differ from a procedure?

18. A procedure call is a statement. What is a function call? Why is
    this different?

19. What does it mean when you say a function returns a value?

20. What are the values of the following expressions?

       **Question**  **Expression**   **Given**
      -------------- ---------------- -------------------------------
          \(a\)      `5`              
          \(b\)      `a`              `a` is 2.5
          \(c\)      `1 + 2 * 3`      
          \(d\)      `a + b`          `a` is 1 and `b` is 2
          \(e\)      `2 * a`          `a` is 3
          \(f\)      `a * 2 + b`      `a` is 1.5 and `b` is 2
          \(g\)      `a + 2 * b`      `a` is 1.5 and `b` is 2
          \(h\)      `(a + b) * c`    `a` is 1, b is 1 and `c` is 5
          \(i\)      `a + b * 2`      `a` is 1.0 and `b` is 2

21. When creating a program, types allow you to reason about the kind of
    data the program is using. The three most basic types of data are
    Double, Integer and String. Use the Double[^30] data type to
    represent any real numeric value; such as 1, 2.5, -75.201 etc. The
    Integer data type is used to represent any whole numeric value; such
    as 1, 0, -27 etc. Use the String data type for any textual data.
    There are many other data types, but these three are the most
    frequently used.

    When assigning a data type think about the following:

    -   How will the data be used?

    -   The range of values expected. Does the type have a sufficient
        range to cover this?

    -   Is precision important? Think carefully, especially with
        fractional values represented as floating point numbers (i.e.
        Double).

    What data type is most appropriate to store the following?

    ::: multicols
    2

    1.  A person's name

    2.  The number of students in a class

    3.  The average age of a group of people

    4.  A temperature in Celsius

    5.  The name of a subject

    6.  The runs scored in a cricket match

    7.  A student's ID number

    8.  The distance between planets (km)

    9.  A person's phone number

    10. The cost of an item
    :::

### Code Writing Questions: Applying what you have learnt {#sub:code_writing_questions_applying_what_you_have_learnt_data}

Apply what you have learnt to the following tasks:

1.  Take the times table program from
    Section [3.6.1](#sub:times_table){reference-type="ref"
    reference="sub:times_table"} and re-implement it so that there are
    two procedures: `Print Times Table`, and `Print Times Table Line`.
    Use these to print the 42, 73, and 126 times tables, as well as
    printing a times table the user requests.

    -   The `Print Times Table Line` procedure will take two parameters.
        The first will be the number, the second will be the times. This
        will output a single line for the table, e.g. ' 1 x 73 = 73'.

    -   The `Print Times Table` procedure will have a single parameter
        called `number`. It will output a header for the table, and then
        call `Print Times Table Line` ten times. In each call it will
        pass 1, 2, 3, etc. for the times parameter, and pass across its
        `number` value to the `number` parameter. After printing the
        last line it will output a footer for the table.

2.  Correct and then implement the Trapezoid Area procedure from
    Section [3.5.5](#sub:hand_execution_with_variables){reference-type="ref"
    reference="sub:hand_execution_with_variables"}. Adjust the
    implementation to call a `Trapezoid Area` function that is passed
    the two base values and the height, and returns the area. Create a
    small program to test this procedure.

3.  Implement the Change Calculation program, and test it function as
    you expect.

4.  Revisit your Circle Dimensions program from
    Chapter [2](#cha:program_creation){reference-type="ref"
    reference="cha:program_creation"} and adjust its implementation to
    make use of functions and procedures.

5.  Design the structure and then the code for a program that converts
    temperatures from Celsius to Fahrenheit. This should read the value
    to convert from the user, and output the results to the Terminal.

6.  Take the adjusted Face Shape program from
    Chapter [\[cha:procedure_declaration\]](#cha:procedure_declaration){reference-type="ref"
    reference="cha:procedure_declaration"}, and re-implement it so that
    the `Draw Face` procedure takes in an x and y coordinate for the
    location where the face will be drawn. Adjust the coordinates of the
    face's components in `Draw Face`, by the amounts in the `x` and `y`
    parameters. Use your new procedure to draw three faces to the screen
    at different positions.

7.  Write a `Swap` procedure that takes in two integer parameters
    (passed by reference) and swaps their values. Write a program to
    test this procedure. This should work so that if you call
    `Swap(a, b);` that the values in the `a` and `b` variables are
    swapped over. You can test this by printing the values before and
    after the call to the `Swap` procedure.

8.  Watch <http://www.youtube.com/watch?v=y2R3FvS4xr4>, which clearly
    demonstrates the importance of being able to calculate the airspeed
    velocity of a swallow. This can be calculated using an equation
    based on the Strouhal Number, see
    <http://www.style.org/strouhalflight>. Use this information to
    create a program that can be used to calculate the airspeed velocity
    of African and European Swallows. Use the following values:

    -   Strouhal Number of 0.33

    -   African Swallow: frequency 15hz, amplitude 21cm

    -   European Swallow: frequency 14hz, amplitude 22cm

### Extension Questions {#sub:extension_questions_data}

If you want to further your knowledge in this area you can try to answer
the following questions. The answers to these questions will require you
to think harder, and possibly look at other sources of information.

1.  Write a small program to experiment with parameter passing. Create
    in this program a procedure called `Print It` that takes a integer
    parameter and prints it to the Terminal. Also create a **Double It**
    procedure that takes an integer parameter passed by reference[^31]
    and has its value doubled in the procedure. Try the following (not
    all will work):

    1.  Call `Print It`, passing in a literal value like `5`.

    2.  Call `Double It`, passing in a literal value like `5`.

    3.  Call `Print It`, passing in a calculated expression like
        `a + b`.

    4.  Call `Double It`, passing in a calculated expression like
        `a + b`.

    5.  Call `Print It`, passing in a variable's value.

    6.  Call `Double It`, passing in a variable's value.

2.  Further adjust your Face Drawing program so that the caller can pass
    in a custom color, width, and height for the face.

3.  Adjust the bike race example from
    Section [3.6.3](#sub:bicycle_race){reference-type="ref"
    reference="sub:bicycle_race"} so that the racers have a rolling
    start. Each bike will then have a different initial speed,
    calculated as a random value. You will need to define a maximum
    starting speed, and recalculate the x scale factor to ensure that
    the bikes are all drawn to the screen.

# Control Flow {#cha:control_flow}

> [T]{.lettrine}[ime]{.smallcaps} has come for you to learn how to
> control the flow of the magical energies. This arcane knowledge will
> unlock great power. Making it possible to do things that until now
> seemed impossible. Close your eyes and picture the energy travelling
> through your spell. Take control of that flow by...

The focus so far has been on learning the programming artefacts that you
can create within your code. You have seen how to create s, s, s, and s.
The actual instructions that you can issue to the computer have been
limited to s, s, and s. This Chapter will introduce you to the other
actions that you can command the computer to perform. It will show you
how to control the flow of the instructions, unlocking great power, and
making it possible to do things that until now seemed impossible.

When you have understood the material in this chapter you will be able
to write code that commands the computer to perform a wider range of
actions by controlling the sequence in which the basic commands are
performed.

## Control Flow Concepts {#sec:control_flow_concepts}

Programming is about designing code that commands the computer to
perform actions. Earlier chapters have introduced the , , and artefacts
into which you can enter these instructions, but have not elaborated on
the actions that you can perform.

Most of a program's actual work will be carried out in s, and through s
and s. These are the main commands, allowing you to alter values stored
in memory and to execute stored instructions. The remaining commands
relate to controlling the order in which the computer performs the
instructions; called **control flow statements**.

This chapter introduces the following kinds of instructions. You can use
these to get the computer to perform certain **actions** within your
program.

-   : Run some code if a condition is true.

-   : Selectively run a branch of code.

-   : Group statements together.

-   : Loop after testing a condition.

-   : Loop then test a condition.

In addition to these actions, you will need have a look at an existing
**artefact**:

-   : An existing that has either a *true* or *false* value.

You may need to revise the following programming artefacts:

-   : The idea of building your own programs.

-   : Creating your own Procedure, as well as calling Procedures from
    libraries.

-   : Creating your own Functions, as well as calling Functions from
    libraries.

The following programming terminology will also be used in this Chapter:

-   : An instruction performed in your code.

-   : A kind of data used in your code.

The example for this chapter is a guessing game, where the user is
guessing a number between 1 and 100. An example of this program
executing is shown in Figure
[4.1](#fig:control-flow-guess-num){reference-type="ref"
reference="fig:control-flow-guess-num"}.

![Guess that Number run from the
Terminal](./topics/control-flow/images/GuessThatNumber.png){#fig:control-flow-guess-num
width="50%"}

### Boolean Data {#sub:boolean_data}

The Boolean[^32] Data Type is a used to represent **truth**. A Boolean
value will either be **true** or **false**. These values are used
extensively in the control flow statements to determine the action to
perform.

![Boolean data represents
truth](./topics/control-flow/diagrams/BooleanData.pdf){#fig:boolean-data
width="80%"}

#### Comparisons {#sub:comparisons}

Comparisons are a common way of getting Boolean values in your code.
These s allow you to compare two values to check for a given condition.
For example, the Expression shown in Figure
[4.2](#fig:boolean-data){reference-type="ref"
reference="fig:boolean-data"} is asking if the *value* in the `area`
variable is larger than `23.5`. The result of this expression will be
either `true` or `false` depending on the current value stored in
`area`. Table [4.1](#tbl:bool-expr-sample){reference-type="ref"
reference="tbl:bool-expr-sample"} lists some example values for this
expression, given different values stored in the `area` variable.

::: {#tbl:bool-expr-sample}
   **Value in `area`**   **`area > 23.5`**
  --------------------- -------------------
         `73.2`               `true`
         `-2.5`               `false`
         `23.5`               `false`

  : Example values for the expression `area > 23.5`
:::

Programming languages offer a range of different comparison operators.
These typically include comparisons to check if values are the same or
different, and to check if one value is larger or small than another.
The different operators for C and Pascal are listed in Table
[4.2](#tbl:comparisons){reference-type="ref"
reference="tbl:comparisons"}.

::: {#tbl:comparisons}
                                          **Description**                     **C**     **Pascal**
  --------------------- --------------------------------------------------- ---------- ------------
        **Equal**                    Are the values the same?                `a == b`    `a = b`
      **Not Equal**                  Are the values different?               `a != b`    `a <> b`
     **Larger Than**         Is the left value larger than the right?        `a > b`   
      **Less Than**          Is the left value smaller than the right?       `a < b`   
   **Larger Or Equal**   Is the left value equal or larger than the right?   `a >= b`  
    **Less Or Equal**    Is the left value smaller or equal to the right?    `a <= b`  

  : Comparison Operators
:::

#### Logical Operators {#sub:logical_operators}

The comparison operators allow you to compare *two* values. This is very
useful, but in itself is incomplete. What, for example, do you do when
you want to compare three or more values? While you are limited to two
values with the comparison operators, there are other operators that
allow you to **combine** Boolean expressions. This will enable you to
combine together multiple Boolean values into a single Expression.

There are four main *logical operators*: **and**, **or**, **xor**, and
**not**. Each of these operators works on two Boolean values, combining
them to give a new Boolean value. For example, the *and* operator allows
you to check if *both* of the expressions are true. The expression
`area > 0 and area < 10` will be true only when area is both larger than
zero and less then ten.

![Logical Operators combine Boolean
values](./topics/control-flow/diagrams/LogicalOperators.pdf){#fig:logical-operators
width="70%"}

::: {#tbl:logical-operators}
                         **Description**                **C**     **Pascal**
  --------- ----------------------------------------- ---------- ------------
   **And**            Are both values True?            `a && b`   `a and b`
   **Or**          Is at least one value True?         `a || b`    `a or b`
   **Xor**   Is one value True, and the other False?   `a ^ b`    `a xor b`
   **Not**             Is the value False?               `!a`      `not a`

  : Logical Operators
:::

::: {#tbl:example_logical_expr}
   area   `area > 0`   `area < 10`   `area > 0 … area < 10`           
  ------ ------------ ------------- ------------------------ -------- ---------
                                            **and**           **or**   **xor**
   `5`       True         True                True             True     False
   `27`      True         False              False             True     True
   `0`      False         True               False             True     True

  : Example Logical Expressions
:::

### Branching {#sub:branching}

There are two main ways of controlling the sequence of actions in a
program. The first of these is called **branching**, or **selection**.
Branching allows you to get the computer to take one of a number of
paths based on the value of a *condition*.

![Branching commands the computer to take one of a number of possible
paths](./topics/control-flow/diagrams/Branching.pdf){#fig:branching
width="80%"}

#### If Statement {#sub:if_statement}

The if statement is the most frequently used branching statement. It
allows you to selectively run code based on the value of a Boolean
expression (the condition). The if statement has an optional *else*
branch that is executed when the condition is false.

![If statement lets you selectively run a branch of
code](./topics/control-flow/diagrams/IfStatement.pdf){#fig:branching-if-statement
width="\\textwidth"}

#### Case Statement {#sub:case_statement}

The case statement is the second kind of branching statement. This
allows you to create paths that execute based on matching a value from
an expression. This allows one case statement to handle many alternative
paths.

![Case statement selectively runs multiple branches of
code](./topics/control-flow/diagrams/CaseStatement.pdf){#fig:branching-case-statement
width="\\textwidth"}

### Looping {#sub:looping}

There are two main ways of controlling the sequence of actions in a
program. The first was **branching**, the second is called **looping**,
or **repetition**. The language's looping statements allow you to have
actions repeated.

![Looping commands the computer to repeat a
path](./topics/control-flow/diagrams/Looping.pdf){#fig:looping
width="90%"}

#### Pre-Test Loop {#sub:pre_test_loop}

The Pre-Test Loop is a looping statement that allows code to be run 0 or
times. The loop checks the condition at the start, and if the condition
is True the loop's body is executed. At the end of the loops body the
computer jumps back to the condition, checking it again to determine if
the loop's body should execute again. If the condition is False when it
is checked the loop jumps ends, and control jumps to the next statement
in the code.

![The Pre-Test Loop checks the condition, then runs the loop's
body](./topics/control-flow/diagrams/PreTestLoop.pdf){#fig:looping-pre-test
width="\\textwidth"}

#### Post-Test Loop {#sub:post_test_loop}

The Post-Test Loop is a looping statement that allows code to be run 1
or times. The post-test loop places the condition after the body of the
loop. This means that the first time through the body of the loop must
execute before the condition is checked. When it gets to the end of the
body, the loop's condition is checked and the computer either jumps back
to the start of the loop to repeat the body, or the loop ends and
control flows on to the next statement in the code.

There are two common variants for the post-test loop: `do...while` and
`repeat...until`. These work in the same way, in that they test the
condition after the loop body, but the conditions they use will be
different. The **`do...while`** loop repeats the body of the loop when
its condition is **true**, **`repeat...until`** repeats the body of the
loop when its condition is **false**. When implementing a post-test loop
you must make sure that the condition you use matches the kind of loop
supported by your language.

![The Post-Test Loop runs the loops body, then checks the
condition](./topics/control-flow/diagrams/PostTestLoop.pdf){#fig:looping-post-test
width="90%"}

### Jumping {#sub:jump}

The jump statements allow you to alter the sequence of instructions in
the code, getting the computer to jump to another instruction.

![Jump Statements cause control to jump to another location in the
code](./topics/control-flow/diagrams/JumpStatements.pdf){#fig:looping-jump-statements
width="\\textwidth"}

#### Break {#sub:break}

The break statement is used to jump out of the current loop, in effect
terminating the loop early. This is useful for ending the current loop,
skipping all future cycles.

![The Break Statement allows you to end a loop
early](./topics/control-flow/diagrams/Break.pdf){#fig:break
width="\\textwidth"}

#### Continue {#sub:continue}

The continue statement is used to jump to the condition of the current
loop. This is useful for skipping the processing of the current loop,
but to allow the loop to continue for the next cycle.

![The continue Statement allows you to jump to the condition, skipping
the remainder of the code in the loop but allowing the loop to
continue](./topics/control-flow/diagrams/Continue.pdf){#fig:continue
width="\\textwidth"}

#### Exit {#sub:exit}

The exit statement, or the return in C, ends the current or . This is
useful for skipping the rest of the processing of the Function or
Procedure, exiting it early and returning to the calling code.

![Exit ends the current Function or
Procedure](./topics/control-flow/diagrams/Return.pdf){#fig:exit
width="\\textwidth"}

#### Goto {#sub:goto}

The last jump statement is the goto statement. This is an unstructured
jump, allowing you to jump anywhere in the code. Structured Programming
principles called for the abolition of the goto statement. This is a
statement you need to be aware of, but not one that should be used.

![The dangers of using goto, from
<http://xkcd.com/292/>](./topics/control-flow/images/goto.png){#fig:goto
width="\\textwidth"}

### Compound Statement {#sub:compound_statement}

and statements need to be able to include a number of instructions
within their paths. Often languages will manage this by indicating that
only a *single* statement can be included in any of these paths, and
then include the ability to code multiple statements in a *single
compound statement*.

![A compound statement is a statement that can contain other
statements](./topics/control-flow/diagrams/CompoundStatement.pdf){#fig:branching-compound-statement
width="\\textwidth"}

### Statement (Simple and Structured) {#sub:statement_with_loops_}

Statements are the actions that we can get the computer to perform.

![A Statement may also be a Looping or Jumping
Statement](./topics/control-flow/diagrams/Statement.pdf){#fig:looping-statement
width="88%"}

### Summary {#sub:control_flow_summary}

This section has introduced a number of new actions that you can use in
your code to create more dynamic programs.

![Key Concepts introduced in this
Chapter](./topics/control-flow/diagrams/Summary.pdf){#fig:control-flow-summary
width="\\textwidth"}

## Using these Concepts {#sec:control_flow_using_these_concepts}

The Control Flow Statements enable you to alter the purely sequential
order in which instructions are performed. Using these Statements you
can have the computer select a path to follow, or jump back and repeat
statements a number of times. These capabilities make it possible to
create programs that react to the data they receive, giving more
interactive results than were possible before.

### Designing Guess that Number {#sub:designing_guess_that_number}

Table [4.5](#tbl:control-flow-prog){reference-type="ref"
reference="tbl:control-flow-prog"} contains a description of the next
program we are going to design. This program plays a small guessing game
called *Guess that Number*. In this game the computer will think of a
target number between 1 and 100. The user then has 7 guesses to
determine the target number's value. At each guess the computer outputs
a message to indicate if the target number is greater than, less than,
or equal to the user's guess.

::: {#tbl:control-flow-prog}
  **Program Description**   
  ------------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  **Name**                  *Guess that Number*
                            
  **Description**           A simple guessing game where the computer 'thinks' of a number between 1 and 100, and the user has seven guesses to guess it. With each guess the computer indicates if the number it is thinking of is less than, or greater than the user's guess. If the user runs out of guesses they are shown the answer, and that round ends. After each round the user has the option to play again, or to quit the program.

  : Description of the Guess That Number program.
:::

As before, the process to design and implement this program will follow
a number of steps:

1.  Understand the problem, and get some ideas on the tasks that need to
    be performed.

2.  Choose the artefacts we will create and use

3.  Design the control flow for the procedural[^33] artefacts

4.  Map these artefacts to code

5.  Compile and run the program

The new step in this process will involve designing the flow of the
instructions within the program, and the Functions and Procedures that
need to be created. Previously each artefact had a sequential flow of
instructions, the introduction of the control flow statements has made
it possible to add branches and loops to this sequence. The order of
these actions needs to be considered for each sequence of instructions
in your code.

### Understanding Guess that Number {#sub:understanding_guess_that_number}

Guess that Number is a relatively common game involving one player
choosing a number, and the other trying to guess it. Each guess is
followed by an indication of whether the guess was less than or larger
than the target number. The game continues until the player guesses the
target number.

The data for this program will be relatively simple. The code will need
to keep track of the current **target number** which will be an integer.
The player will be able to enter a **guess**, also an integer. These two
numbers represent the data model for the game. It is a game that
involves a *target number* and *guess*.

The actions within the program involve **playing the game**, and
**performing a guess**. Playing the game is the process of performing
guesses until the player finally guesses the target number. This version
of the game will only allow the player to have seven guesses, so this
will need to be a part of this code in the program. The process of
performing a guess will involve getting the user's guess, and giving
them feedback indicating if they guessed the target number, of if they
were larger than, or less than the target.

### Choosing Artefacts for Guess that Number {#sub:choosing_artefacts_for_guess_that_number}

Software design is all about **abstraction**. This is the process of
determining the essential features of the problem and modelling that in
the artefacts that you create in your code. When designing the artefacts
that will make up your code you need to think about problem, and try to
create artefacts in your code that represent the actions and data you
imagine when thinking about the problem.

Our understanding of the Guess that Number game indicates that there are
two processes that need to be performed within the program: **play
game** and **perform guess**. These two processes can be coded as either
Functions or Procedures in the program's code. The `Play Game` code can
be implemented as a . It will be responsible for running the process of
the game, starting with telling the user it has 'thought of a number',
through to coordinating the guesses, ending only when the user gets the
answer of runs out of guesses.

`Perform Guess`, on the other hand, will need to be a as it must return
back a value indicating if the user has guessed the number. The code in
`Perform Guess` will be responsible for asking the user to enter a
guess, and then giving them the feedback on their guess. As this has the
details of the guess, its result is needed to allow `Play Game` to
determine if the user has guessed the number.

The `Perform Guess` code will also need to accept parameters to tell it
what the current `target` value is. This data will exist within the
`Play Game` code, so it will need a mechanism to pass that code to
`Perform Guess`. A is needed in `Perform Guess` to accept `target` will
enable this.

A nicety may be to allow tell the user which guess they are up to. Once
again, this information is stored in `Play Game`, so a second parameter
can be added to allow `Play Game` to pass in the `guess number` along
with the `target` number.

In addition to these it has been decided to add a `Print Line` procedure
to display a line of '-' characters. This will be displayed at the end
of the game before the user is asked if they want to play again. A
`length` parameter will enable the caller to indicate how many of these
characters are printed on the line.

The Structure Chart showing these artefacts is shown in Figure
[4.18](#fig:guess-game-structure){reference-type="ref"
reference="fig:guess-game-structure"}, and the Sequence Diagram is shown
in Figure [4.19](#fig:guess-game-seq){reference-type="ref"
reference="fig:guess-game-seq"}. Notice that there is some relationship
here between the artefacts that we are creating and the steps in the
game itself. The earlier it is to see the relationship between these
artefacts and the problem, the better job you have done abstracting your
solution.

![Structure Chart for the Guess that Number
Program](./topics/control-flow/diagrams/GuessThatNumStructure.pdf){#fig:guess-game-structure
width="60%"}

![Sequence Diagram for the Guess that Number
Program](./topics/control-flow/diagrams/GuessThatNumSeq.pdf){#fig:guess-game-seq
width="\\textwidth"}

### Designing Control Flow for Perform Guess {#sub:designing_control_flow_for_perform_guess}

Having chosen the artefacts to build, the next step is to design the
control flow that will enable these Functions and Procedures, and the
program itself, to achieve their goals. Each has a responsibility that
it must meet in order for the overall solution to work.

This section will go through designing the logic for the `Perform Guess`
Function. It will cover the following points:

-   
-   
-   
-   
-   
-   

#### Drawing control flow using a Flowchart {#ssub:flow_charts}

Before examining the control flows within the Functions and Procedures
of the Guess that Number program, let us have a look at a means of
visually representing flow of actions within code. A common means of
doing this is to use a **flowchart**. This is a diagram that depicts a
sequence of actions, and can be used to represent the sequence of action
actions that occur within your code.

The flowchart has four basic symbols as shown in Figure
[4.20](#fig:flow-symbols){reference-type="ref"
reference="fig:flow-symbols"}.

-   **Start/Stop**: This represents the start and end of the process.
    Typically the start node would have the name of the Function or
    Procedure within it.

-   **Flow**: The arrows between nodes represent control flow,
    indicating which action is to be performed next in the code.

-   **Process**: This node represents a task being performed. In our
    case this will map to one, or more, simple statements. The text in
    the Process node should indicate what is being performed at this
    stage.

-   **Decision**: This represents a point where the code needs to make a
    decision. This is used for the conditions in the and statements. A
    decision **must** have more than one flow coming out of it, and each
    flow should indicate the condition that triggers that path.

![Flowchart
Symbols](./topics/control-flow/diagrams/FlowParts.pdf){#fig:flow-symbols
width="50%"}

#### Use Structured Programming Principles to guide the design of the flowchart {#ssub:structured_programming}

Flowcharts can be used to represent any sequence of actions, but not all
sequences of actions will be easy to code.
Figure [4.21](#fig:unstructured-flow-example){reference-type="ref"
reference="fig:unstructured-flow-example"} is an example of a flowchart,
but not one that can be coded using the structured statements covered in
Chapter [4](#cha:control_flow){reference-type="ref"
reference="cha:control_flow"}.
Figure [4.22](#fig:structured-flow-example){reference-type="ref"
reference="fig:structured-flow-example"} shows a structured version of
this same algorithm. Notice that in this version there are identifiable
blocks, each having a single entry and a single exit. This structure
could easily be converted to code using structured statements.

Looking at these two flowcharts, the structured flowchart looks more
complicated than the unstructured version. This reflects the fact that
in some cases the structured version may be more complicated, but is
also an indication that this process may be better broken down into more
functions and procedures. For example, the repeated loop looking for
someone else to blame could be placed in its own procedure. If you find
it difficult to design the control flow using structured blocks try to
see if you can identify additional Functions and Procedures to help you
break the code down into smaller blocks.

![Unstructured
Flowchart](./topics/control-flow/diagrams/UnstructuredFlowSample.pdf){#fig:unstructured-flow-example
width="\\textwidth"}

![Structured version of Figure
[4.21](#fig:unstructured-flow-example){reference-type="ref"
reference="fig:unstructured-flow-example"}](./topics/control-flow/diagrams/StructuredFlowSample.pdf){#fig:structured-flow-example
width="\\textwidth"}

The structured programming principles indicate that code within a
Function or Procedure should be organised into **blocks**. These blocks
match to the structured statements in modern programming languages: the
and statements. Each of these blocks has a **single entry point** and a
**single exit point**, allowing them to combined together.

The addition of statements to the language allows the blocks to have
**multiple exit points**; one at the end of the block's code, the other
at a , , or . These statements give you extra flexibility, but still
work within the structured programming principles.

#### The Structured Programming blocks used to build the logic {#ssub:structured_programming_blocks}

In Structured Programming there are three kinds of blocks:

-   **Sequence**: one instruction follows the next in a sequence.

-   **Selection**: the ability to branch the sequence.

-   **Repetition**: repeat a block a number of times.

The flowchart snippets in the section on and showed how the flows work
for *selection* and *repetition*.
Figure [4.24](#fig:selection-flow){reference-type="ref"
reference="fig:selection-flow"} shows the three alternatives for
selection blocks,
Figure [4.25](#fig:repetition-flow){reference-type="ref"
reference="fig:repetition-flow"} shows the two alternatives for
repetition blocks, and
Figure [4.23](#fig:sequence-flow){reference-type="ref"
reference="fig:sequence-flow"} shows the standard sequence flow. The
design of any program's logic is a task in combining these blocks
together to perform the desired effect.

![Flows for Sequence
Blocks](./topics/control-flow/diagrams/SequenceFlow.pdf){#fig:sequence-flow
width="15%"}

![Flows for Selection
Blocks](./topics/control-flow/diagrams/SelectionFlow.pdf){#fig:selection-flow
width="90%"}

![Flows for Repetition
Blocks](./topics/control-flow/diagrams/RepetitionFlow.pdf){#fig:repetition-flow
width="55%"}

#### Combining blocks for the Perform Guess {#ssub:combining_blocks_for_the_guessing_game}

With the basic theory at hand, we can now start to design the control
flow for the Guess that Number program. This process will involve, once
again, the idea of **abstraction**. When designing the flow for a
program you first need to be able to perform the process yourself, even
if its just on paper, and then work out the steps that you undertook so
that you can code these within the program.

For the Guess that Number program we can start by designing the control
flow within the `Perform Guess` function. The specification of this is
shown in
Table [\[tbl:perform guess\]](#tbl:perform guess){reference-type="ref"
reference="tbl:perform guess"}. Think about the steps that need to be
performed to achieve this. If you had been asked to do this what would
you need to do?

::: tabular
\|c\|p9cm\|\
\
\
\
\
`Boolean` & True when the user has guessed the number, False otherwise.\
**Parameter** & **Description**\
`Guess Number` & The number of the current guess, used in the prompt
asking for the user to enter their guess.\
&\
`Target` & The number the user is aiming to guess.\
\
\
\
:::

The first task the Function needs to perform is to get the guess from
the user. This can be performed in a **sequence**: display a prompt,
read the value from the user. This first sequence is shown in
Figure [4.26](#fig:perform-guess-seq-1){reference-type="ref"
reference="fig:perform-guess-seq-1"}.

![Initial Sequence in
`Perform Guess`](./topics/control-flow/diagrams/PerformGuess1.pdf){#fig:perform-guess-seq-1
width="25%"}

The next step in this sequence is to give the user feedback based upon
their guess and the target number. This code requires a the ability to
*select* a given branch. The computer needs to output different messages
based upon the users guess. This can be achieved with a **selection**
block. Looking back at
Figure [4.24](#fig:selection-flow){reference-type="ref"
reference="fig:selection-flow"} there are three possible alternatives
for implementing this selection. The `if` with no `else` is not a valid
option as there are three paths we need to take. The `case` block is
also not valid as we are not matching a value, but comparing values to
each other. The last option is the `if-else` block, but this only has
two branches. It is not going to be possible to code all three options
within one block, but it can be achieved using two `if-else` blocks.

The first `if-else` block will check if the `target` is greater than the
user's `guess`. If this is true then the computer can take the first
branch and output the message 'The number is larger than ' and the value
from the user's guess. The flow chart for this part is shown in
Figure [4.27](#fig:perform-guess-seq-2){reference-type="ref"
reference="fig:perform-guess-seq-2"}. This block is the third task in
the sequence, this if block has a single entry, causes a branch in the
flow, and will have a single exit.

![First branch in
`Perform Guess`](./topics/control-flow/diagrams/PerformGuess2.pdf){#fig:perform-guess-seq-2
width="60%"}

Following the false path from the first decision, and we have a new
location into which to insert a block. At this point we know the target
is *not* larger than the user's guess. At this point you can include
another `if-else` block to check if the target is **less than** the
user's guess. This is shown in
Figure [4.28](#fig:perform-guess-seq-3){reference-type="ref"
reference="fig:perform-guess-seq-3"}.

![Second branch tests if the Target is less than the
guess](./topics/control-flow/diagrams/PerformGuess3.pdf){#fig:perform-guess-seq-3
width="70%"}

Taking the false path again, and now we have a location at which the
target value *must be* equal to the user's guess. The first condition
checked if the `target` was larger than the `guess`, which it was not.
The second condition checked if the `target` was less than the `guess`,
which it was not. So the only way this can be the case if is the
`target` and the `guess` are equal. This path can then be used to output
the 'Well done...' message. This is shown in
Figure [4.29](#fig:perform-guess-seq-4){reference-type="ref"
reference="fig:perform-guess-seq-4"}.

![The 'Well done...' message can be output on the third
path](./topics/control-flow/diagrams/PerformGuess4.pdf){#fig:perform-guess-seq-4
width="80%"}

The last action in the code is to return a Boolean result indicating if
the user's guess is equal to the target number.

![A Boolean result is returned from the
Function](./topics/control-flow/diagrams/PerformGuess5.pdf){#fig:perform-guess-seq-5
width="74%"}

Figure [4.31](#fig:perform-guess-seq-7){reference-type="ref"
reference="fig:perform-guess-seq-7"} shows the flowchart with
annotations highlighting the different blocks within the code. The
function starts off with a **sequence** that contains all of the code in
the Function. Within this there is the **selection**, that internally
contains another **selection**.

![Blocks in the `Perform Guess`
code](./topics/control-flow/diagrams/PerformGuess7.pdf){#fig:perform-guess-seq-7
width="85%"}

#### Setting the result using an Expression {#ssub:setting_the_result_using_an_expression}

Figure [4.32](#fig:perform-guess-seq-6){reference-type="ref"
reference="fig:perform-guess-seq-6"} shows the *trick* that is being
performed at the end of `Perform Guess`'s code. `Perform Guess` needs to
return a result indicating if the user has guessed the number of not.
This will be a Boolean value, with True indicating they guessed the
number. Initially it may seem that you need a **selection** block to
enable this, as shown on the right of
Figure [4.32](#fig:perform-guess-seq-6){reference-type="ref"
reference="fig:perform-guess-seq-6"}.

![Calculating `Perform Guess`'s
result](./topics/control-flow/diagrams/PerformGuess6.pdf){#fig:perform-guess-seq-6
width="\\textwidth"}

#### The Pseudocode for `Perform Guess` {#ssub:the_pseudocode_for_perform_guess}

Listing
[\[plst:perform_guess\]](#plst:perform_guess){reference-type="ref"
reference="plst:perform_guess"} contains the Pseudocode for the
`Perform Guess` logic from the flowchart in
Figure [4.30](#fig:perform-guess-seq-5){reference-type="ref"
reference="fig:perform-guess-seq-5"}. Notice how the indentation in this
mirrors the block structures in the flowchart. It is good practice to
indent your code in this way as it helps you, and any person who reads
your code, to see the structure of the logic. You will be able to avoid
many errors by making sure that you always indent your code so that it
highlights the code's structure.

### Designing Control Flow for Play Game {#sub:designing_control_flow_for_play_game}

The `Play Game` Procedure is another artefact that will require some
some thought to design its logic.
Table [\[tbl:play game\]](#tbl:play game){reference-type="ref"
reference="tbl:play game"} contains the specification for this
Procedure. It will be responsible for coordinating the actions of the
game, while `Perform Guess` coordinates the actions for a *single*
guess.

::: tabular
\|c\|p9cm\|\
\
\
\
\
\
\
:::

The implementation of this Procedure will require us to store some data.
The following s will be needed to store data within this Procedure:

-   `My Number`: This will store the computer's randomly chosen number.

-   `Guess Number`: This will store the current guess the user is at,
    allowing the computer to stop looping when the number of guesses is
    exceeds 7.

-   `Got It`: A Boolean value to indicate if the user did guess the
    number, allowing the computer to stop looping when the user guesses
    the number.

The flowchart for this is shown in
Figure [4.33](#fig:play-game){reference-type="ref"
reference="fig:play-game"}, and again in
Figure [4.34](#fig:play-game-diag1){reference-type="ref"
reference="fig:play-game-diag1"} with its blocks highlighted. This code
uses a **repetition** to ask the user to perform up to 7 guesses. The
condition on this loop occurs *after* the loop body as the user must
have at least one guess.

There is also a **selection** after the loop to output the answer if the
user ran out of guesses. This is only done when the user has not guessed
it themselves. This does not need to perform any other actions when the
user did guess the number, so the False branch has no additional
actions. In code this would be implemented with an without an **else**
branch.

![Logic for the `Play Game`
Procedure](./topics/control-flow/diagrams/PlayGame.pdf){#fig:play-game
width="52%"}

![Blocks in the `Play Game`
code](./topics/control-flow/diagrams/PlayGame1.pdf){#fig:play-game-diag1
width="65%"}

Listing [\[plst:play_game\]](#plst:play_game){reference-type="ref"
reference="plst:play_game"} contains the Pseudocode for the `Play Game`
Procedure. This uses Constants for `MAX_NUMBER` and a `MAX_GUESSES`,
this will make it easier to change the range of the numbers and the
associated number of guesses.

The flowchart for `Play Game` includes a , which can be coded as either
a `do...while` or a `repeat...until` loop. This two variations are shown
in step 7 of
Listing [\[plst:play_game\]](#plst:play_game){reference-type="ref"
reference="plst:play_game"}, with the while version appearing as a
comment on the following line. Both of these versions have the same
result, but do require different conditions. The two implementations of
this are shown in
Listing [\[clst:play_game\]](#clst:play_game){reference-type="ref"
reference="clst:play_game"} and
Listing [\[paslst:play_game\]](#paslst:play_game){reference-type="ref"
reference="paslst:play_game"}. It is important to understand that the
basic ideas of a **post-test loop** is the same regardless of whether it
is coded using `do...while` or `repeat...until`.

### Designing Control Flow for Print Line {#sub:designing_control_flow_for_print_line}

`Print Line` is a short Procedure used to print a line of '-' characters
to the Terminal. The flowchart for this Procedure is shown in
Figure [4.35](#fig:print-line){reference-type="ref"
reference="fig:print-line"}, and again with the blocks highlighted in
Figure [4.36](#fig:print-line-diag-1){reference-type="ref"
reference="fig:print-line-diag-1"}.

::: tabular
\|c\|p9cm\|\
\
\
\
**Parameter** & **Description**\
`Length` & The number of characters to print. Represents the length of
the line.\
\
\
\
:::

![Flowchart for the logic in `Print Line`
code](./topics/control-flow/diagrams/PrintLine.pdf){#fig:print-line
width="37%"}

![Blocks in the `Print Line`
code](./topics/control-flow/diagrams/PrintLine1.pdf){#fig:print-line-diag-1
width="57%"}

### Designing the Control Flow for Main {#sub:designing_the_control_flow_for_main}

The last Procedure is `Main`. This is responsible for coordinating the
actions of the program. It will call `Play Game` in a loop that
repeatedly plays the game until the user decides to quit. `Main` will
have one local variable called `again`. This will store a character, and
will be used to store the value read from the user's response to the
'*play again*' prompt.

![Flowchart for the `Main`
Procedure](./topics/control-flow/diagrams/Main.pdf){#fig:main
width="31%"}

![Blocks in the `Main`
code](./topics/control-flow/diagrams/Main1.pdf){#fig:main-1 width="55%"}

### Writing the Code for Guess That Number {#sub:writing_the_code_for_guess_that_number}

Flowcharts and Pseudocode communicate the same ideas. They describe the
actions that need to be performed within your code. The following two
sections, Section [4.3](#sec:control_flow_in_c){reference-type="ref"
reference="sec:control_flow_in_c"} and
Section [4.4](#sec:control_flow_in_pascal){reference-type="ref"
reference="sec:control_flow_in_pascal"} , contain a description of the
syntax needed to code these control flow statements in the C and Pascal
programming languages.

### Compiling and Running Guess that Number {#sub:compiling_and_running_guess_that_number}

Once you have completed the code for this program you need to compile
and run it. As this uses random numbers you cannot generate standard
test data in order to check the execution. Instead you should perform a
number of executions and test the different paths through the program.
The main condition you want to check are:

-   Test failing to get the number in seven guesses.

-   Test getting the number correct within seven guesses.

-   Check the output messages when your guess is less than the number
    (you can enter a guess below 0).

-   Check the output message when your guess is larger than the number
    (you can enter a guess larger than 100).

-   Check that the random sequence is different each time, if its not
    make sure you have seeded the random number generator.

## Control Flow in C {#sec:control_flow_in_c}

### Implementing the Guess that Number in C {#sub:c_guessing_game}

Section [4.2](#sec:control_flow_using_these_concepts){reference-type="ref"
reference="sec:control_flow_using_these_concepts"} of this Chapter
introduced the 'Guess that Number' program. This program contained a
Function to `Perform Guess` and Procedures to `Print Line` and
`Play Game`. Each of these involved some control flow in their logic, as
shown in the Flowcharts in
Section [4.2](#sec:control_flow_using_these_concepts){reference-type="ref"
reference="sec:control_flow_using_these_concepts"}. The full C
implementation of the Guess that Number program is shown in
Listing [\[lst:storing-data-c-guessing-game\]](#lst:storing-data-c-guessing-game){reference-type="ref"
reference="lst:storing-data-c-guessing-game"}.

``` {#lst:storing-data-c-guessing-game .c caption="C code for the Guessing Game" label="lst:storing-data-c-guessing-game"}
/*
* Program: guess-that-number.c
* This program is an implementation of the "guess that number"
* game. The computer randomly chooses a number and the player
* attempts to guess it. (It should never take more than 7 guesses)
*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <stdbool.h>

#define MAX_NUMBER 100
#define MAX_GUESSES 7

// Print a line onto the Terminal.
void print_line(int len)
{
    int i = 0;
    
    while ( i < len )
    {
        printf("-");
        i++;
    }
    
    printf("\n");
}

// Perform the steps for the guess. Reads the value entered by the user,
// outputs a message, and then returns true if the got it otherwise it returns
// false.
bool perform_guess(int num_guess, int target)
{
    int guess;
    
    printf("Guess %d: ", num_guess);
    scanf("%d", &guess);
    
    if (target < guess) printf("The number is less than %d\n", guess);
    else if (target > guess) printf("The number is larger than %d\n", guess);
    else printf("Well done... the number was %d\n", guess);
    
    return target == guess;
}

// Implements a simple guessing game. The program generates
// a random number, and the player tries to guess it.
void play_game()
{
    int my_number, num_guess;
    bool got_it;
    
    my_number = random() % MAX_NUMBER + 1;
    num_guess = 0; //Keep track of the number of guesses
    
    printf("I am thinking of a number between 1 and %d\n\n", MAX_NUMBER);
    
    do
    {
        num_guess++;
        got_it = perform_guess(num_guess, my_number);
    }
    while( num_guess < MAX_GUESSES && !got_it);
    
    if ( !got_it )
    {
        printf("You ran out of guesses... the number was %d\n", my_number);
    }
}

// Loops the guessing game until the user decided to quite.
int main()
{
    char again;
    
    srandom(time(0));
    
    do
    {
        play_game();
        
        printf("\n");
        print_line(50);
        printf("Do you want to play again [y/N]? ");
        scanf(" %c", &again);
    } while (again == 'y' || again == 'Y');
    
    printf("\nBye\n");
    return 0;
}
```

### C Boolean Data {#sub:c_boolean_data}

C has very flexible support for Boolean values. In C a `0` value is
considered to be false, and any other value is true. Modern C compilers
have now added support for an explicit Boolean type, `bool`. This type
requires the `stdbool.h` header file, which defines the `bool` type as
well as the values `true` and `false`.

::: {#tbl:c-boolean}
  **Boolean Type**                   
  ------------------ --------------- -------------------
  *Name*                 *Size*           *Values*
  `bool`              1 byte/8 bits   `true` or `false`

  : C Boolean Type
:::

::: {#tbl:c_comparison_op}
                                          **Description**                     **C**
  --------------------- --------------------------------------------------- ----------
        **Equal**                    Are the values the same?                `a == b`
      **Not Equal**                  Are the values different?               `a != b`
     **Larger Than**         Is the left value larger than the right?        `a > b`
      **Less Than**          Is the left value smaller than the right?       `a < b`
   **Larger Or Equal**   Is the left value equal or larger than the right?   `a >= b`
    **Less Or Equal**    Is the left value smaller or equal to the right?    `a <= b`

  : C Comparison Operators
:::

::: {#tbl:c-logical-operators}
                         **Description**                **C**
  --------- ----------------------------------------- ----------
   **And**            Are both values True?            `a && b`
   **Or**          Is at least one value True?         `a || b`
   **Xor**   Is one value True, and the other False?   `a ^ b`
   **Not**             Is the value False?               `!a`

  : Logical Operators
:::

### C Statement (with loops) {#sub:c_statement_with_loops_}

In addition to the and , C Statements may be , , or Statements.

### C If Statement {#sub:c_if_statement}

The if statement is a statement. This can be used to optionally run a
block of code, providing two alternate paths controlled by a Boolean
expression.

### C Case Statement {#sub:c_case_statement}

The case statement allows you to switch between a number of paths.

### C Compound Statement {#sub:c_compound_statement}

Most of the C structured statements only allow single statements within
each path. For example, the paths in the two branches of an can only
contain a single statement. The allows you to group together multiple
statements within a single *compound statement*.

### C While Loop {#sub:c_while_loop}

The while loop is C's , allowing a block to be repeated zero or more
times.

### C Do While Loop {#sub:c_do_while_loop}

The do while loop is C's , allowing a block to be repeated one or more
times.

### C Jump Statements {#sub:jump_statements}

The jump statements allow you to exit out of another structure.

## Control Flow in Pascal {#sec:control_flow_in_pascal}

### Implementing the Guess that Number in Pascal {#sub:pas_guessing_game}

Section [4.2](#sec:control_flow_using_these_concepts){reference-type="ref"
reference="sec:control_flow_using_these_concepts"} of this Chapter
introduced the 'Guess that Number' program. This program contained a
function to `Perform Guess` and procedures to `Print Line` and
`Play Game`. Each of these involved some control flow in their logic, as
shown in the flowcharts in
Section [4.2](#sec:control_flow_using_these_concepts){reference-type="ref"
reference="sec:control_flow_using_these_concepts"}. The full Pascal
implementation of the Guess that Number program is shown in
Listing [\[lst:storing-data-pas-guessing-game\]](#lst:storing-data-pas-guessing-game){reference-type="ref"
reference="lst:storing-data-pas-guessing-game"}.

``` {#lst:storing-data-pas-guessing-game .pascal caption="Pascal code for the Guessing Game" label="lst:storing-data-pas-guessing-game"}
// This program is an implementation of the 'guess that number'
// game. The computer randomly chooses a number and the player
// attempts to guess it. (It should never take more than 7 guesses)
program GuessThatNumber;

const
  MAX_NUMBER  = 100;
  MAX_GUESSES = 7;

// Print a line onto the Terminal.
procedure PrintLine(len: Integer);
var
  i: Integer = 0;
begin
    while ( i < len ) do
    begin
        Write('-');
        i += 1;
    end;
    WriteLn();
end;

// Perform the steps for the guess. Reads the value entered by the user,
// outputs a message, and then returns true if the got it otherwise it returns
// false.
function PerformGuess(numGuess, target: Integer): Boolean;
var
  guess: Integer;
begin
    Write('Guess ', numGuess, ': ');
    ReadLn(guess);
    
    if target < guess then WriteLn('The number is less than ', guess)
    else if target > guess then WriteLn('The number is larger than ', guess)
    else WriteLn('Well done... the number was ', guess);
    
    result := target = guess;   // return true when "target equals guess"
end;

// Implements a simple guessing game. The program generates
// a random number, and the player tries to guess it.
procedure PlayGame();
var
  myNumber, numGuess: Integer;
  gotIt: Boolean = False;
begin
    myNumber := Random(MAX_NUMBER) + 1;
    numGuess := 0; //Keep track of the number of guesses
    
    WriteLn('I am thinking of a number between 1 and ', MAX_NUMBER);
    WriteLn();
    
    repeat
        numGuess += 1;
        gotIt := PerformGuess(numGuess, myNumber);
    until (numGuess > MAX_GUESSES) or gotIt;
    
    if not gotIt then
    begin
        WriteLn('You ran out of guesses... the number was ', myNumber);
    end;
end;

// Loops the guessing game until the user decided to quite.
procedure Main();
var
  again: Char;
begin
    Randomize();
    
    repeat
        PlayGame();
        WriteLn();
        PrintLine(50);
        WriteLn('Do you want to play again [Y/n]? ');
        ReadLn(again);
    until (again = 'n') or (again = 'N');
    
    WriteLn('Bye');
end;

begin
  Main();
end.
```

### Pascal Boolean Data {#sub:pas_boolean_data}

Pascal includes a `Boolean` type, as well as `True` and `False` values.

::: {#tbl:pas-boolean}
  **Boolean Type**                   
  ------------------ --------------- -------------------
  *Name*                 *Size*           *Values*
  `Boolean`           1 byte/8 bits   `True` or `False`

  : Pascal Boolean Type
:::

::: {#tbl:pas_comparison_op}
                                          **Description**                     **C**
  --------------------- --------------------------------------------------- ----------
        **Equal**                    Are the values the same?                `a = b`
      **Not Equal**                  Are the values different?               `a <> b`
     **Larger Than**         Is the left value larger than the right?        `a > b`
      **Less Than**          Is the left value smaller than the right?       `a < b`
   **Larger Or Equal**   Is the left value equal or larger than the right?   `a >= b`
    **Less Or Equal**    Is the left value smaller or equal to the right?    `a <= b`

  : Pascal Comparison Operators
:::

::: {#tbl:pas-logical-operators}
                         **Description**               **Pascal**
  --------- ----------------------------------------- ------------
   **And**            Are both values True?            `a and b`
   **Or**          Is at least one value True?          `a or b`
   **Xor**   Is one value True, and the other False?   `a xor b`
   **Not**             Is the value False?              `not a`

  : Logical Operators
:::

### Pascal Statement (with loops) {#sub:pas_statement_with_loops_}

In addition to the and , Pascal statements may be , , or statements.

### Pascal If Statement {#sub:pas_if_statement}

The if statement is a statement. This can be used to optionally run a
block of code, providing two alternate paths controlled by a Boolean
expression.

### Pascal Case Statement {#sub:pas_case_statement}

The case statement allows you to switch between a number of paths.

### Pascal Compound Statement {#sub:pas_compound_statement}

Most of the Pascal structured statements only allow a single statement
within each path. For example, the paths in the two branches of an can
only contain a single statement. The allows you to group together
multiple statements within a single *compound statement*.

### Pascal While Loop {#sub:pas_while_loop}

The while loop is Pascal's , allowing a block to be repeated zero or
more times.

### Pascal Repeat Loop {#sub:pas_repeat_loop}

The repeat loop is Pascal's , allowing a block to be repeated one or
more times.

### Pascal Jump Statements {#sub:pas_jump_statements}

The jump statements allow you to exit out of another structure.

## Understanding Control Flow {#sec:understanding_control_flow}

This Chapter has introduced new statements that can be used to control
the sequence of actions the computer performs. These statements allow
you to add and paths to your code. The flowcharts presented in
Section [4.2](#sec:control_flow_using_these_concepts){reference-type="ref"
reference="sec:control_flow_using_these_concepts"} are a great way of
visualising the order in which the computer will execute the
instructions. To help you fully understand these concepts this section
will look at how these statements work within the computer.

### Understanding Branching in Perform Guess {#sub:understanding_branching_in_perform_guess}

Figure [4.39](#fig:perform-guess-understanding){reference-type="ref"
reference="fig:perform-guess-understanding"} shows the flowchart for the
`Perform Guess` that was developed in
Section [4.2.4](#sub:designing_control_flow_for_perform_guess){reference-type="ref"
reference="sub:designing_control_flow_for_perform_guess"} on . The
following sections show how the computer executes these actions. These
illustrations will start at the call into `Perform Guess`, skipping the
illustration of the steps that lead up to this call.

![Logic for the `Perform Guess` Procedure from
Figure [4.44](#fig:perform-guess-5){reference-type="ref"
reference="fig:perform-guess-5"}](./topics/control-flow/diagrams/PerformGuess5.pdf){#fig:perform-guess-understanding
width="50%"}

In the following illustrations `Perform Guess` will be called three
times with the `target` number being `37` in each case. The following
three guesses will be performed, ensuring that all paths through the
flowchart are covered.

1.  On the first guess the user enters a guess of 50, allowing for the
    left most branch of this flowchart to be followed.

2.  The second guess will be 25 to test the middle branch, taking the
    *else* branch of the first decision and the *true* branch of the
    second decision.

3.  Finally the third guess will be 37, testing the right most path
    through the code.

#### Perform Guess is called for guess 1 {#ssub:perform_guess_is_called_for_the_first_time}

In the Guess that Number program, the `Perform Guess` function is
responsible for reading in the user's guess and giving them feedback.
Figure [4.40](#fig:perform-guess-1){reference-type="ref"
reference="fig:perform-guess-1"} shows the `Perform Guess` code being
called for the first time, it is passed 1 to its `num guess` parameter
and 37 to its `target` parameter.

![Perform Guess is called for the first
time](./topics/control-flow/images/PerformGuess1.pdf){#fig:perform-guess-1
width="\\textwidth"}

#### Execution reaches the if branch for guess 1 {#ssub:execution_reaches_the_if_branch}

Execution of `Perform Guess` occurs as normal, each instruction is run
one after the other.

![Program's steps are executed up to step
4](./topics/control-flow/images/PerformGuess2.pdf){#fig:perform-guess-2
width="\\textwidth"}

#### If takes the True branch for guess 1 {#ssub:if_takes_the_true_branch_for_guess_1}

With the first guess the expression in the is `true`, `target` **is**
less than `guess`. The computer jumps into the *true* branch of the if
statement.

![The *true* branch is taken as `target` is less than
`guess`](./topics/control-flow/images/PerformGuess3.pdf){#fig:perform-guess-3
width="\\textwidth"}

#### Control jumps to the end of `Perform Guess` for guess 1 {#ssub:control_jumps_to_step_11_for_guess_1}

Once step 5 finishes, control jumps to step 11, skipping the *else*
branch of the if statement from step 4.

![`Perform Guess` finishes for guess 1, returning the result
`false`](./topics/control-flow/images/PerformGuess4.pdf){#fig:perform-guess-4
width="\\textwidth"}

#### `Perform Guess` is called again for guess 2 {#ssub:perform_guess_is_called_for_guess_2}

`Perform Guess` is called again, this time it is passed 2 for
`guess num` and 37 for `target`. This executes the code in
`Perform Guess`, one instruction at a time, eventually reaching step 4.

![`Perform Guess` is run for guess 2 and advanced to step
4](./topics/control-flow/images/PerformGuess5.pdf){#fig:perform-guess-5
width="\\textwidth"}

#### If takes the *else* branch for guess 2 {#ssub:if_takes_the_else_branch_for_guess_2}

With the second guess the expression in the is `false`, `target` **is
not** less than `guess`. The computer jumps into the *else* branch of
the if statement.

![Control jumps to step 7 as the target is not less than the
guess](./topics/control-flow/images/PerformGuess6.pdf){#fig:perform-guess-6
width="\\textwidth"}

#### The inner if's *true* branch is taken in guess 2 {#ssub:the_inner_if_s_true_branch_is_taken_in_guess_2}

The expression in step 7 is `true`, so the if statement directs the
computer into the *true* branch at step 8.

![Control continues into the *true* branch of the inner if
statement](./topics/control-flow/images/PerformGuess7.pdf){#fig:perform-guess-7
width="\\textwidth"}

#### Control jumps to the end of `Perform Guess` for guess 2 {#ssub:control_jumps_to_step_11_for_guess_2}

Once step 8 finishes, control jumps to step 11, skipping the *else*
branch of the if statement from step 7 and also ending the if statement
started at step 4.

![Guess 2 ends with `Perform Guess` returning
`False`](./topics/control-flow/images/PerformGuess8.pdf){#fig:perform-guess-8
width="\\textwidth"}

#### Perform Guess is called again for guess 3 {#ssub:perform_guess_is_called_again_for_guess_3}

`Perform Guess` is called again, this time it is passed 3 for
`guess num` and 37 for `target`. This executes the code in
`Perform Guess`, one instruction at a time, eventually reaching step 4.

![`Perform Guess` is run for guess 3 and advanced to step
4](./topics/control-flow/images/PerformGuess9.pdf){#fig:perform-guess-9
width="\\textwidth"}

#### If takes the *else* branch for guess 3 {#ssub:if_takes_the_else_branch_for_guess_3}

The `target` is **not** less than the user's guess, so the *else* branch
of the if statement at step 4 is taken, with the computer jumping to
step 7.

![The *else* branch is taken as `target` is not less than `guess` for
guess
3](./topics/control-flow/images/PerformGuess10.pdf){#fig:perform-guess-10
width="\\textwidth"}

#### The inner if's *else* branch is taken in guess 3 {#ssub:the_inner_if_s_else_branch_is_taken_in_guess_3}

The `target` is **not** larger than the user's guess, so the *else*
branch of the if statement at step 7 is taken, with the computer jumping
to step 10.

![The `target` is not larger than `guess`, so the *else* branch of the
if statement at step 7 is
taken.](./topics/control-flow/images/PerformGuess11.pdf){#fig:perform-guess-11
width="\\textwidth"}

#### Control jumps to the end of `Perform Guess` for guess 3 {#ssub:control_jumps_to_step_11_for_guess_3}

Once step 10 finishes, control jumps to step 11, ending the if
statements from step 7 and step  4.

![Program's entry point is loaded onto the
Stack](./topics/control-flow/images/PerformGuess12.pdf){#fig:perform-guess-12
width="\\textwidth"}

### Understanding Looping in Play Game {#sub:understanding_looping_in_play_game}

Figure [4.52](#fig:play-game-understanding){reference-type="ref"
reference="fig:play-game-understanding"} shows the flowchart for the
`Play Game` procedure that was developed in
Section [4.2.5](#sub:designing_control_flow_for_play_game){reference-type="ref"
reference="sub:designing_control_flow_for_play_game"} on . The following
sections outline how these actions are executed within the computer.

![Logic for the `Play Game` Procedure from
Figure [4.33](#fig:play-game){reference-type="ref"
reference="fig:play-game"}](./topics/control-flow/diagrams/PlayGame.pdf){#fig:play-game-understanding
width="38%"}

In the following illustrations `Play Game` will be called once, and will
perform three guesses. These three guesses match the calls illustrated
in
Section [4.5.1](#sub:understanding_branching_in_perform_guess){reference-type="ref"
reference="sub:understanding_branching_in_perform_guess"} . In each case
the details of the call into `Perform Guess` will not be recovered, but
you can refer back to the previous section if needed.

The illustrations will show how the loop in the flowchart is handled by
the computer. As this includes a `do...while`/`repeat...until` loop, the
explanations will present both boolean expressions. Please ensure that
you check the logic based on the implementation you will use.

#### `Play Game` is called {#ssub:play game_is_called}

At some point the `Play Game` procedure is called. This will be
responsible for coordinating the actions needed to play one game of
Guess that Number. It will call `Perform Guess` to do the work needed to
perform each individual guess.

![`Play Game` is called, it is allocated space on the Stack for its
local
variables](./topics/control-flow/images/PlayGame1.pdf){#fig:play-game-1
width="\\textwidth"}

#### The loop is entered {#ssub:control_runs_to_step_4}

Steps 1 to 3 execute and initialise the `my num` and `guess num` local
variables.

![](./topics/control-flow/images/PlayGame2.pdf){#fig:play-game-2
width="\\textwidth"}

#### `Perform Guess` is called, and returns false for guess 1 {#ssub:perform guess_returns_false_for_guess_1}

![](./topics/control-flow/images/PlayGame3.pdf){#fig:play-game-3
width="\\textwidth"}

#### Loop condition is checked at the end of guess 1, with the loop being repeated {#ssub:loop_condition_is_checked_at_the_end_of_guess_1}

At the end of the loop the condition is checked, in this case the loop
will run again.

![Condition indicates that the loop's body should be executed
again](./topics/control-flow/images/PlayGame4.pdf){#fig:play-game-4
width="\\textwidth"}

#### Control returns to the top of the loop to perform guess 2 {#ssub:control_returns_to_the_top_of_the_loop_to_perform_guess_2}

The loop repeats to perform guess 2.

![Control returns to step 5 to repeat the body of the
loop](./topics/control-flow/images/PlayGame5.pdf){#fig:play-game-5
width="\\textwidth"}

#### `Perform Guess` is called again, and returns false for guess 2 {#ssub:perform_guess_is_called_to_perform_guess_2}

In the body of the loop, step 5 increases the value in `guess num` to 2
then control continues to step 6. This step calls `Perform Guess`, to
allow the user to perform the second guess. This time around it is
passed 2 for the `guess num`, and 37 for the `target`. When
`Perform Guess` ends the result is false again, which is stored in
`got it`.

![`Perform Guess` is called again, it is passed 2 for its `guess num`
and `37` for its `target`
parameter](./topics/control-flow/images/PlayGame6.pdf){#fig:play-game-6
width="\\textwidth"}

#### Loop condition is checked at the end of guess 2, with the loop being repeated {#ssub:loop_condition_is_checked_at_the_end_of_guess_2}

The loop condition is checked again, and it indicates that the body
needs to be repeated again.

![](./topics/control-flow/images/PlayGame7.pdf){#fig:play-game-7
width="\\textwidth"}

#### `Perform Guess` is called a third time, and returns true for guess 3 {#ssub:perform_guess_is_called_to_perform_guess_3}

Just as with guess 2, the body of the loop is repeated. Step 5 increases
the value in `guess num` to 3 then control continues to step 6. This
step calls `Perform Guess`, to allow the user to perform the third
guess. This time around it is passed 3 for the `guess num`, and 37 for
the `target`. When `Perform Guess` ends the result is now true, which is
stored in `got it`.

![`Perform Guess` is called for the third time, this time the user
guesses the
number](./topics/control-flow/images/PlayGame8.pdf){#fig:play-game-8
width="\\textwidth"}

#### Loop condition is checked at the end of guess 3, ending the loop {#ssub:loop_condition_is_checked_at_the_end_of_guess_3}

The loop condition is checked again, and this time it indicates that the
loop should end.

![Condition is checked, this time it indicates that the loop should
end](./topics/control-flow/images/PlayGame9.pdf){#fig:play-game-9
width="\\textwidth"}

#### Branch is checked after the loop {#ssub:branch_is_checked_after_the_loop}

The branch after the loop's body outputs the number if the user did not
guess it.

![Branch after the loop outputs the answer if the users did not guess
the
number](./topics/control-flow/images/PlayGame10.pdf){#fig:play-game-10
width="\\textwidth"}

### Understanding looping in Print Line {#sub:understanding_looping_in_print_line}

Figure [4.63](#fig:print-line-understanding){reference-type="ref"
reference="fig:print-line-understanding"} shows the flowchart for the
`Print Line` procedure that was developed in
Section [4.2.6](#sub:designing_control_flow_for_print_line){reference-type="ref"
reference="sub:designing_control_flow_for_print_line"} on . The
following sections outline how these actions are executed within the
computer.

![Logic for the `Print Line` Procedure from
Figure [4.35](#fig:print-line){reference-type="ref"
reference="fig:print-line"}](./topics/control-flow/diagrams/PrintLine.pdf){#fig:print-line-understanding
width="40%"}

The illustrations will show a single execution of the `Print Line`
procedure, with 3 being passed to its `length` parameter. This call will
output a three line of dash characters, demonstrating how the differs
from the .

#### Print Line is called to print a line of three characters {#ssub:print_line_is_called_to_print_a_line_of_three_characters}

The illustration starts with the call to `Print Line`. It is called to
print a line of three dash (-) characters to the Terminal.

![`Print Line` starts, and is passed the value
3](./topics/control-flow/images/PrintLine1.pdf){#fig:print-line-1
width="\\textwidth"}

#### Execution proceeds to the while loop {#ssub:execution_proceeds_to_the_while_loop}

The first instructions are executed as normal, initialising the value of
the `i` variable. At the `while` loop the computer checks the condition
and determines that the loop should execute. This means that the next
instruction will be taken from Step 3 of the code.

![`i` is initialised, and the loop checks its
condition](./topics/control-flow/images/PrintLine2.pdf){#fig:print-line-2
width="\\textwidth"}

#### First dash is output to the Terminal {#ssub:first_dash_is_output_to_the_terminal}

The body of the loop is entered, and the first instruction (Step 3)
outputs a dash to the Terminal.

![The first dash is output to the
Terminal](./topics/control-flow/images/PrintLine3.pdf){#fig:print-line-3
width="\\textwidth"}

#### `i` is incremented, counting the number of times the loop has run {#ssub:i_is_incremented_to_count_the_loops}

After outputting the dash, the next instruction (Step 4) increments the
value of `i`. In this code `i` is counting the number of times the loop
has been performed. This is the end of the first loop, so now `i` has
the value 1.

![`i` is keeping track of the number of times the loop has been
performed](./topics/control-flow/images/PrintLine4.pdf){#fig:print-line-4
width="\\textwidth"}

#### Condition is checked again, and the body of the loop reentered {#ssub:condition_is_checked_again_and_the_body_of_the_loop_rentered}

The while loop checks the condition to determine if the body should be
executed again, or if the loop should end. In this case `i` is still
less than `length` so the body of the loop is executed again.

![Loop condition is checked to determine if the loop should run
again](./topics/control-flow/images/PrintLine5.pdf){#fig:print-line-5
width="\\textwidth"}

#### Loop 2 starts, outputting the second dash {#ssub:loop_2_starts_outputting_the_second_dash}

The loop body is run again with its first instruction (Step 3)
outputting another dash to the Terminal.

![The loop body is run again, and a second dash is
output](./topics/control-flow/images/PrintLine6.pdf){#fig:print-line-6
width="\\textwidth"}

#### `i` is incremented again, indicating that the loop has run twice {#ssub:i_is_incremented_again_indicating_that_the_loop_has_run_twice}

The last instruction in this sequence increments the value of `i`,
indicating that the loop has run twice. Once again, control returns
jumps back to the condition. It is the condition that will determine
when the loop ends, the end of the loop just indicates that control will
return back to the condition.

![`i` is incremented, and then control returns back to the loop's
condition](./topics/control-flow/images/PrintLine7.pdf){#fig:print-line-7
width="\\textwidth"}

#### Condition is checked again, and the loop runs a third time {#ssub:condition_is_checked_again_and_the_loop_runs_a_third_time}

When the condition is checked it determines if the loop body runs, or is
skipped. In this case `i` is still less than `length` so the body is run
a third time.

![While determines that the loop should run
again](./topics/control-flow/images/PrintLine8.pdf){#fig:print-line-8
width="\\textwidth"}

#### Another dash is output as the loop body runs a third time {#ssub:another_dash_is_output_as_the_loop_body_runs_a_third_time}

The loop body is entered a third time, and its sequence of instructions
is executed. This outputs a third dash to the Terminal.

![The third dash is output to the
Terminal](./topics/control-flow/images/PrintLine9.pdf){#fig:print-line-9
width="\\textwidth"}

#### `i` is incremented again, indicating the loop has been performed three times {#ssub:i_is_incremented_again_indicating_the_loop_has_been_performed_three_times}

When Step 4 is executed `i` is incremented again, and now has the value
3. As this is the end of the loop, control will jump back to condition.

![`i` is incremented again, and then control jumps back to the
condition](./topics/control-flow/images/PrintLine10.pdf){#fig:print-line-10
width="\\textwidth"}

#### Condition is checked a fourth time, and the loop body is skipped {#ssub:condition_is_checked_a_fourth_time_and_the_loop_body_is_skipped}

The condition is checked again, and this time `i` **is not** less than
`length` and so the loop body is not run again. Instead the computer
jumps to the first instruction after the loop, Step 5.

![The condition is now false, and so the computer jumps past the loop
body to Step
5](./topics/control-flow/images/PrintLine11.pdf){#fig:print-line-11
width="\\textwidth"}

#### Having completed the loop, the next instruction outputs a new line {#ssub:having_completed_the_loop_the_next_instruction_outputs_a_new_line}

Now that the loop has finished, control continues running the sequence
from the Procedure.

![Processing continues after the
loop](./topics/control-flow/images/PrintLine12.pdf){#fig:print-line-12
width="\\textwidth"}

## Control Flow Examples {#sec:control_flow_examples}

### Times Table {#sub:times_table_flow}

This program prints out the times table for a number entered by the
user, displaying from 1 x n to 10 x n. The description of the program is
in Table [4.12](#tbl:flow-times-table){reference-type="ref"
reference="tbl:flow-times-table"}, the pseudocode in Listing
[\[lst:flow-times-pseudo\]](#lst:flow-times-pseudo){reference-type="ref"
reference="lst:flow-times-pseudo"}, the C code in Listing
[\[lst:flow-times-c\]](#lst:flow-times-c){reference-type="ref"
reference="lst:flow-times-c"}, and the Pascal code in Listing
[\[lst:flow-times-pas\]](#lst:flow-times-pas){reference-type="ref"
reference="lst:flow-times-pas"}.

::: {#tbl:flow-times-table}
  **Program Description**   
  ------------------------- ------------------------------------------------
  **Name**                  *Times Table*
                            
  **Description**           Displays the Times Table from 1 x n to 10 x n.

  : Description of the Times Table program
:::

### Circle Area {#sub:circle_area_control_flow}

This program prints out the area of a circle. The description of the
program is in Table [4.13](#tbl:flow-circle-area){reference-type="ref"
reference="tbl:flow-circle-area"}, the pseudocode in Listing
[\[lst:flow-circle-areas-pseudo\]](#lst:flow-circle-areas-pseudo){reference-type="ref"
reference="lst:flow-circle-areas-pseudo"}, the C code in Listing
[\[lst:flow-circle-areas-c\]](#lst:flow-circle-areas-c){reference-type="ref"
reference="lst:flow-circle-areas-c"}, and the Pascal code in Listing
[\[lst:flow-circle-areas-pas\]](#lst:flow-circle-areas-pas){reference-type="ref"
reference="lst:flow-circle-areas-pas"}.

::: {#tbl:flow-circle-area}
  **Program Description**   
  ------------------------- -------------------------------------------------------------------------------------------
  **Name**                  *Circle Areas*
                            
  **Description**           Displays the Circle Areas for circles with radius from 1.0 to 5.0 with increments of 0.1.

  : Description of the Circle Areas program
:::

### Moving Rectangle {#sub:moving_rectangle}

This example SwinGame code will move a rectangle back and forth across
the screen.

::: {#tbl:flow-moving-rect}
  **Program Description**   
  ------------------------- ----------------------------------------------------------------------
  **Name**                  *Moving Rectangle*
                            
  **Description**           Displays a rectangle that is moved back and forth across the screen.

  : Description of the Moving Rectangle program
:::

![Example execution of the Moving Rectangle
program](./topics/control-flow/examples/MovingRect.png){#fig:moving-rect-img
width="80%"}

### Button Click in SwinGame {#sub:button_click_in_swingame}

This example SwinGame code draws a rectangle that the user can 'click'.

::: {#tbl:flow-button-click}
  **Program Description**   
  ------------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  **Name**                  *Button Click*
                            
  **Description**           Displays a rectangle the user can 'click'. Having the mouse held down over the rectangle changes it to a filled rectangle. Clicking the rectangle shows the text 'Clicked' in the top left corner.

  : Description of the Button Click program
:::

![Example execution of the Button Click
program](./topics/control-flow/examples/ButtonClick.png){#fig:button-click-img
width="80%"}

## Control Flow Exercises {#sec:control_flow_exercises}

### Concept Questions {#sub:concept_questions_flow}

Read over the concepts in this chapter and answer the following
questions:

1.  What values can a Boolean expression have?

2.  What are the values of the following expressions?

       **Question**  **Expression**                   **Given**
      -------------- -------------------------------- -----------------------------------------------------------
          \(a\)      `1 > 5`                          
          \(b\)      `1 < 5`                          
          \(c\)      `a > b`                          `a` is 1 and `b` is 2
          \(d\)      `(a > b) or (b > a)`             `a` is 1 and `b` is 2
          \(e\)      `(a > b) and (b > a)`            `a` is 2 and `b` is 2
          \(f\)      `a or b or c`                    `a` is False, `b` is False, and `c` is True
          \(g\)      `(a or b) and (c or d)`          `a` is False, `b` is True, `c` is True, and `d` is False
          \(h\)      `(a or b) and (c or d)`          `a` is False, `b` is True, `c` is False, and `d` is False
          \(i\)      `(a and b) or (c and d)`         `a` is False, `b` is True, `c` is True, and `d` is True
          \(j\)      `a xor b`                        `a` is True and `b` is True
          \(k\)      `(a or b) and (not (a and b))`   `a` is True and `b` is True
          \(l\)      `not True`                       
          \(m\)      `a and (not b)`                  `a` is True and `b` is False

3.  What are the two kinds of branching statements?

4.  What are the differences between these statements?

5.  When would you use each of these kinds of branching statements?

6.  What are the two different kinds of looping statements?

7.  What are the differences between these statements?

8.  When would you use each of these kinds of looping statements?

9.  How do Boolean expressions relate to branching and looping
    statements?

10. What are the four jumping statements?

11. What are the differences between these statements?

12. Why do you need compound statements? Where would these be used?

13. In structured programming, what are the three different kinds of
    blocks?

14. How many entry/exits are there from these blocks?

15. What are the principles of structured programming?

16. How does this influence the way you design programs?

17. Open a new SwinGame project and examine the startup code. How does
    this program keep the window open until the user closes it?

### Code Reading Questions {#sub:code_reading_questions_flow}

Use what you have learnt to read and understand the following code
samples, and answer the associated questions.

1.  Read the C code in
    Listing [\[clst:rect-move\]](#clst:rect-move){reference-type="ref"
    reference="clst:rect-move"}, or the Pascal code in
    Listing [\[plst:rect-move\]](#plst:rect-move){reference-type="ref"
    reference="plst:rect-move"}, and then answer the following
    questions:

    1.  What are `X_SPEED`, `width`, `height`, `x`, and `y`? How are
        they similar/different? What purpose does each of these play?
        What values are they assigned?

    2.  What does the loop in this code do?

    3.  Briefly explain what this code does, and suggest a name for the
        `????` procedure.

    4.  Provide an example of how you could call this procedure.

2.  Read the C code in
    Listing [\[clst:color-pick\]](#clst:color-pick){reference-type="ref"
    reference="clst:color-pick"}, or the Pascal code in
    Listing [\[plst:color-pick\]](#plst:color-pick){reference-type="ref"
    reference="plst:color-pick"}, and then answer the following
    questions:

    1.  What will be drawn to the screen when the user is not holding
        down any key?

    2.  When the user is holding down the G key what will be drawn?

    3.  When the user is holding down the R and B keys what will be
        drawn?

    4.  What condition causes the loop to end? How can a user end this
        loop?

    5.  Draw a flow chart to illustrate the possible paths through this
        code.

3.  Read the C code in
    Listing [\[clst:bar_pattern\]](#clst:bar_pattern){reference-type="ref"
    reference="clst:bar_pattern"}, or the Pascal code in
    Listing [\[plst:bar_pattern\]](#plst:bar_pattern){reference-type="ref"
    reference="plst:bar_pattern"}, and then answer the following
    questions:

    1.  What do the local variables and parameter keep track of?

    2.  How many times will the loop execute?

    3.  What is the purpose of the if statement within the loop?

    4.  What will be drawn to the screen when this procedure is called?

4.  Read the C code in
    Listing [\[clst:color-pick1\]](#clst:color-pick1){reference-type="ref"
    reference="clst:color-pick1"}, or the Pascal code in
    Listing [\[plst:color-pick1\]](#plst:color-pick1){reference-type="ref"
    reference="plst:color-pick1"}, and then answer the following
    questions:

    1.  What will be drawn to the screen when the user is not holding
        down any key?

    2.  When the user is holding down the G key what will be drawn?

    3.  When the user is holding down the R and B keys what will be
        drawn?

    4.  What condition causes the loop to end? How can a user end this
        loop?

    5.  Draw a flow chart to illustrate the possible paths through this
        code.

5.  Read the C code in
    Listing [\[clst:cursor\]](#clst:cursor){reference-type="ref"
    reference="clst:cursor"}, or the Pascal code in
    Listing [\[plst:cursor\]](#plst:cursor){reference-type="ref"
    reference="plst:cursor"}, and then answer the following questions:

    1.  What value is stored in the `dot x` variable each time through
        the loop?

    2.  Explain what will be drawn on the screen when this code is
        executed. How does this code respond to user actions?

    3.  Do the circles appear on top of, or below the frame rate?
        Explain.

    4.  How frequently are the actions in the loop performed?

    5.  How would putting a call to `delay` inside this loop effect the
        running of this program?

6.  Read the C code in
    Listing [\[clst:click_game\]](#clst:click_game){reference-type="ref"
    reference="clst:click_game"}, or the Pascal code in
    Listing [\[plst:click_game\]](#plst:click_game){reference-type="ref"
    reference="plst:click_game"}, and then answer the following
    questions:

    1.  What is the purpose of the if statement in this code? What is it
        testing?

    2.  When will the color of the rectangle change? What are the
        conditions that must be met?

    3.  If the user clicks the left button on their mouse will the color
        of the rectangle change? discuss

    4.  What would need to be added to keep a score of the number of
        times the user changed the color? explain

    ![The game in
    play](./topics/control-flow/exercises/ClickGame.png){#fig:click_game
    width="80%"}

7.  Read the C code in
    Listing [\[clst:grid\]](#clst:grid){reference-type="ref"
    reference="clst:grid"}, or the Pascal code in
    Listing [\[plst:grid\]](#plst:grid){reference-type="ref"
    reference="plst:grid"}, and then answer the following questions:

    1.  What would be better names for the `i` and `j` variables?

    2.  How many rectangles are drawn to the screen when this procedure
        is called?

    3.  What does this code draw to the screen when it is called?

    4.  How would you need to change the code to ensure that the
        rectangles were always square?

### Code Writing Questions: Applying what you have learnt {#sub:code_writing_questions_applying_what_you_have_learnt_flow}

Apply what you have learnt to the following tasks:

1.  Write a small program that will print the message 'Hello World' to
    the Terminal '1 MILLION TIMES'\... mmmmwwwahahahah[^34].

2.  Revisit your Circle Dimensions program from
    Chapter [3](#cha:storing_and_using_data){reference-type="ref"
    reference="cha:storing_and_using_data"} and adjust its
    implementation to make use of looping statements. The new output
    should display the radius, circle area, diameter, and circumference
    of circles with radius from 1.0cm to 1.0m at 0.25cm increments.

3.  Take your Times Table program from
    Chapter [3](#cha:storing_and_using_data){reference-type="ref"
    reference="cha:storing_and_using_data"}, and change it so that you
    can print a variable number of times. For example, have it print the
    5 times table from 1 x 5 to 10 x 5, the 73 times table from 1 x 73
    to 73 \* 73, and the 42 times table from 1 x 42 to 7 \* 42.

4.  Implement the Guess that Number program, and test that your
    implementation works successfully.

5.  Take the adjusted Face Shape program from
    Chapter [3](#cha:storing_and_using_data){reference-type="ref"
    reference="cha:storing_and_using_data"}, and re-implement it so that
    it draws a new randomly places face each time through the loop in
    `Main`. SwinGame include a `rnd` function that can be used to
    generate a random number between 0 and 1, you can then multiply this
    by the Screen Width or Screen Height to get a random position.

6.  Try implementing some of the code reading questions. See if you can
    combine a few of them together to create a small game.

7.  Write a function called `Read Integer` that will read a number from
    the user. If the user does not enter a number the function will
    output the message 'Please enter a number.', and will repeatedly ask
    the user to enter a number until they do so. Write a small program
    that tests if your function works correctly.

8.  Use your `Read Integer` function to read two values from the user,
    and then output the larger of the two values.

9.  Use your `Read Integer` function to read in a number, and then print
    out a custom message for different values. For example, the value
    `42` could have the message 'The meaning of life\...', the value
    `73` could have the message 'The best number, according to Dr.
    Sheldon Cooper', etc. Have at least five custom messages, and a
    default message when the user does not enter one of the selected
    values.

10. Update your *custom message* program, and have it ask the user if
    they want to quit at the end, allowing them to print multiple
    message values.

### Extension Questions {#sub:extension_questions_flow}

If you want to further your knowledge in this area you can try to answer
the following questions. The answers to these questions will require you
to think harder, and possibly look at other sources of information.

1.  Implement a Number Guesser program that will guess numbers between 1
    and 100. See the program description in
    Table [4.16](#tbl:number_guesser){reference-type="ref"
    reference="tbl:number_guesser"}

    ::: {#tbl:number_guesser}
    +-------------------------+-------------------------------------------+
    | **Program Description** |                                           |
    +:========================+:==========================================+
    | **Name**                | *Number Guesser*                          |
    +-------------------------+-------------------------------------------+
    |                         |                                           |
    +-------------------------+-------------------------------------------+
    | **Description**         | This program will work to *guess* a       |
    |                         | number that the player is thinking of     |
    |                         | between 1 and 100. This will prompt the   |
    |                         | user to think of a number, and it will    |
    |                         | then guess using the following algorithm. |
    |                         | The first guess will be 50 (the point     |
    |                         | half way between 1 and 100). The user     |
    |                         | will then enter a character to tell the   |
    |                         | computer how that value relates to the    |
    |                         | target value: 'E' for equal, 'L' for your |
    |                         | guess is less than the target, and 'G'    |
    |                         | for your guess is greater than the        |
    |                         | target.                                   |
    |                         |                                           |
    |                         | When the guess is equal, a success        |
    |                         | message is output.                        |
    |                         |                                           |
    |                         | When the guess is less than the target    |
    |                         | the computer will take another guess that |
    |                         | is half way between the guess and the     |
    |                         | pervious upper value (e.g. 1-100 = 50,    |
    |                         | 50-100 = 75, 75-100 = 87, 25-50 = 37,     |
    |                         | etc).                                     |
    |                         |                                           |
    |                         | When the guess is larger than the target  |
    |                         | the computer will take another guess that |
    |                         | is half way between the guess and the     |
    |                         | pervious lower value (e.g. 1-100 = 50,    |
    |                         | 1-50 = 25, 1-25 = 12, 50-75 = 62, etc).   |
    |                         |                                           |
    |                         | Once the number is guessed (or the        |
    |                         | computer determines the player is         |
    |                         | cheating\...) the program asks if the     |
    |                         | user wants to play again, and the process |
    |                         | is repeated or the program quits.         |
    +-------------------------+-------------------------------------------+

    : Description of the Number Guesser program.
    :::

2.  Create a SwinGame project called *Key Test*. See the description in
    Table [4.17](#tbl:key_test){reference-type="ref"
    reference="tbl:key_test"}.

    ::: {#tbl:key_test}
      **Program Description**   
      ------------------------- --------------------------------------------------------------------------------------------------------------------
      **Name**                  *Key Test*
                                
      **Description**           This SwinGame will draw a filled red rectangle in the center of the screen when the user holds down the **r** key.

      : Description of the Key Test program.
    :::

3.  Extend the *Key Test* program to draw other shapes when different
    keys are held down. For example, draw a filled circle when the user
    holds down the **c** key.

4.  Color can be represented as consisting of three components: hue,
    saturation, and brightness. The hue component represents the color,
    the saturation is how much of this color is added to the white
    point, and the white point is controlled by the brightness and moves
    from black through grey to white. An illustration of this is shown
    in Figure [4.79](#fig:color_wheel){reference-type="ref"
    reference="fig:color_wheel"}.

    ![Color wheel showing HSB
    color](./topics/control-flow/exercises/Color.png){#fig:color_wheel
    width="30%"}

    SwinGame includes a `Draw Pixel` procedure that allows you to draw a
    single pixel on the screen. We can use this to draw a color banding
    across the screen by calculating the color for each pixel using
    SwinGame's `HSB Color` function.

    The structure of this program can be broken into three blocks: a
    `Color At` function, a `My Clear Screen` procedure and a `Main`
    procedure. This structure is shown in
    Figure [4.80](#fig:my_clear_screen){reference-type="ref"
    reference="fig:my_clear_screen"}.

    ![Structure Chart for My Clear
    Screen](./topics/control-flow/exercises/MyClearScreen.pdf){#fig:my_clear_screen
    width="30%"}

    The `Color At` function will be used to calculate the color for each
    pixel on the screen. This will be passed the `x` and `y` coordinates
    of the pixel (Integers) and an `offset`[^35] (a Double value). This
    function will call SwinGame's `HSB Color` function, passing in a hue
    calculated using the equation shown below, with saturation and
    brightness set to 0.9 and 0.8 respectively, and it will return the
    resulting color.

    $$hue = \frac{\displaystyle (Offset + \frac{x + y}{Screen Width + Screen Height})}{2}$$

    The equation will cycle the hue value between 0 and 1, covering the
    full spectrum of color. The `x + y` component will alter the color
    of the pixel based on its position on the screen, while the offset
    allows the program to change the color of the first pixel as time
    passes.

    The `MyClearScreen` procedure will loop through all of the pixels
    across the screen (with x going from 0 up to (but not including)
    ScreenWidth()). This loop will select a single column of pixels on
    the screen, the pixels at the given x position. As this column has
    ScreenHeight() pixels you need an inner loop that loops from 0 to
    ScreenHeight(). Inside the inner loop the `x,y` values give you the
    coordinates of an individual pixel. You can now use the `Draw Pixel`
    procedure to draw this pixel, using the `Color At` function you
    wrote to determine its color.

    Lastly the `Main` procedure will contain the standard game loop
    provided in the SwinGame project template. Replace the code within
    the loop so that it calls `My Clear Screen` then `Refresh Screen`
    and lastly `Process Events`. Within `Main` you need to maintain an
    offset value that you increment by a fixed value each loop[^36],
    always ensuring the value is between 0 and 1. This offset is then
    passed to `My Clear Screen` when it is called.

    See if you can use this information, and program design to create a
    small screensaver program. Experiment with different values and
    loops to see what effects you can create.

# Managing Multiple Values {#cha:managing_multiple_values}

> [T]{.lettrine}[rue]{.smallcaps} magic has the power to affect not just
> one, but many targets. The secrets you will learn today will make it
> possible for your magic to be applied to many targets. The key is
> to...

Previous chapters have introduced a number programming artefacts for you
to create within your code. However, when it comes to working with data
in your programs you have been limited in the way you deal with multiple
values. This chapter introduces the concepts and practices that make it
easier to work with multiple values in your code.

When you have understood the material in this chapter you will be able
to work with multiple values more easily, allowing your programs to work
with many data values.

## Concepts Related to Managing Multiple Values {#sec:managing_multiple_values_concepts}

Data is an important part of any program. Earlier chapters have shown
how to store and work with data. These chapters covered both the s of
data you can work with, and means of storing and exchanging data using
s. So far each Variable has stored only a single value, making it
difficult to work with multiple values. This chapter introduces the
concepts needed start working more effectively with multiple values.

The chapter introduces the following **artefacts**:

-   : A kind of variable that is used to store multiple values. The
    individual values in the array are called *elements*.

-   : An existing that stores textual data.

The following **actions** are then needed to work with Arrays:

-   : A that can be used to easily repeat a block of code for each
    element of an array.

-   : Expressions can read the value from the element of an array.

-   : The assignment statement can be used to assign a value to an
    element in an array.

You may need to revise the following programming artefacts:

-   : The idea of storing data within your code.

-   : Storing data in a or .

-   : Passing data to a Function or Procedure.

The following programming terminology will also be used in this Chapter:

-   : A value used in a statement.

-   : A kind of data used in your code.

The example for this chapter is a statistics calculator, where the user
enters 10 values, and the program calculates some statistics. An example
of this program executing is shown in Figure
[5.1](#fig:simple-stats){reference-type="ref"
reference="fig:simple-stats"}.

![Statistics Calculator run from the
Terminal](./topics/arrays/images/SimpleStats.png){#fig:simple-stats
width="60%"}

### Array {#sub:array}

An array is a special kind of , one that stores multiple values instead
of a single value. The values within an array, called elements, are all
of the same .

![Arrays allow you to store multiple values in a
variable](./topics/arrays/diagrams/Array.pdf){#fig:type-decl-array
width="\\textwidth"}

#### Arrays in Memory {#ssub:arrays_in_memory}

Arrays store multiple values, with an index that provides access to the
individual elements within the array. Conceptually this can be viewed as
a that contains multiple slots (the elements) into which the values are
stored.

Many languages have **0** as the index of the first element. This
reflects how the values are actually stored in memory.
Figure [5.3](#fig:type-decl-array-idx){reference-type="ref"
reference="fig:type-decl-array-idx"} shows how an array (named `arr` in
this Figure) is stored in memory. The array is a **contiguous** area in
memory, with the elements being next to each other. You can think of the
array as starting at the first element, so you need to skip *0* elements
to access the first element. The second element is accessed by skipping
*1* element, so it has index *1*. The third element is accessed by
skipping *2* elements, so it has the index *2*, etc.

The size of each of the elements of the array can then be used to
quickly locate each element, given its index. If you have an array of
`Integer` values then these are each 32 bits (4 bytes), so the element
at index 3 is $3 \times 32bits$ (96 bits) past the start of the array.

The great thing is that you do not need to think about these details,
but knowing this should you remember that the first[^37] element of an
array is at index 0.

![Arrays occupy a contiguous area of memory, with elements next to each
other in memory
](./topics/arrays/diagrams/ArrayIndex.pdf){#fig:type-decl-array-idx
width="\\textwidth"}

### Assignment Statement (with Arrays) {#sub:assignment_statement_with_arrays_}

The assignment statement allows you to store a value in a variable. This
can now be extended to allow you to store a value in an element of an
array. To achieve this you indicate the array you want to store the
value in, as well as the index at which the value is to be stored.

![Arrays allow you to store multiple values in a
variable](./topics/arrays/diagrams/AssignmentWithArray.pdf){#fig:assignment-with-arrays
width="\\textwidth"}

#### Assigning all elements of an array {#ssub:assigning_all_elements_of_an_array}

Many languages also allow you to copy the entire contents of an array
into another array. In these cases each of the elements of one array are
copied into the elements of the destination array (left hand side of the
assignment).

![All of the elements of an array can be copied across in the assignment
statement](./topics/arrays/diagrams/AssignmentWithArray2.pdf){#fig:assignment-with-arrays2
width="\\textwidth"}

### Expressions (with Arrays) {#sub:expressions_with_arrays_}

Expressions allow you to read values from the elements of an array. To
get an elements value you must supply the name of the array, and the
index of the element you want to read.

![You can read the values back from an
array](./topics/arrays/diagrams/ExpressionWithArray.pdf){#fig:expression-with-arrays
width="\\textwidth"}

### Pass by Reference {#sub:pass_by_reference}

As with other Variables, you can pass an array to Functions and
Procedures. The only real difference is that the array can potentially
store significantly more data than other variables. When the array is
passed by value each of its elements must be passed to the Parameter.
Passing the parameter in this way means that there will be two copies of
the data in memory, which takes more time and more memory.

You should avoid passing arrays by value, and instead pass them by
reference. When passed by reference the array itself is passed across.
This gives the called Function or Procedure access to the data, but does
not require that the values be copied across.

One issue with always using pass by reference is that it allows the
called code to change the data in array you are passing across. This can
be useful, but at other times you want to pass the data across without
allowing it to be changed by the called code. Both C and Pascal allow
you to indicate that the data you are passing should be passed by
reference, but that it cannot be changed in the called code.

![Arrays should always be passed by
Reference](./topics/arrays/diagrams/ByRefByVal.pdf){#fig:array-by-ref-by-val
width="90%"}

### For Loop {#sub:for_loop}

As has been shown in previous chapters, Computers can only perform
simple actions. They cannot perform an action on all of the elements in
our arrays. For example, a computer cannot sum **all** of the values in
an array. What you need to do is think of these tasks so that they can
be performed **for *each*** value in the array. So the sum would become,
for each of the numbers in the array, add the number to a running total.
When this has been performed for each element in the array you have the
sum of all of the elements.

The for loop is a that repeats a block of code a number of times. You
can think of it like a counting loop, with it counting from a start
value to an end value. The for loop has a **control variable** that has
the number of the current loop as its value.

![The for loop can be used to loop through the elements of an
array](./topics/arrays/diagrams/For.pdf){#fig:for-loop
width="\\textwidth"}

### String {#sub:string}

Textual data is stored as an array of characters.

![Strings are textual data, stores as an array of
characters](./topics/arrays/diagrams/String.pdf){#fig:strings
width="80%"}

### Summary {#sub:arrays_summary}

This Chapter introduced a number of concepts related to working with
multiple values in code.

![Concepts covered in this
Chapter](./topics/arrays/diagrams/Summary.pdf){#fig:arrays-summary
width="80%"}

## Using these Concepts {#sec:arrays_using_these_concepts}

Arrays make it easier to work with multiple values, allowing you to have
a single variable that stores multiple values. With arrays you can start
to create programs that process larger quantities of data.

### Designing Statistics Calculator {#sub:designing_statistics_calculator}

Table [5.1](#tbl:stats-calc-prog){reference-type="ref"
reference="tbl:stats-calc-prog"} contains a description of a small
statistics programs. This program reads a number of values from the
user, and then outputs some statistics calculated from these values.
This program will make use of arrays to store the values entered, and
then calculate the required statistics.

::: {#tbl:stats-calc-prog}
  **Program Description**   
  ------------------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  **Name**                  *Statistics Calculator*
                            
  **Description**           Reads values from the user and calculates a range of statistics that are output to the Terminal. Statistics output include **mean**, **maximum**, **sum**, and **variance**.

  : Description of the Statistics Calculator program.
:::

As before, the process to design and implement this program will follow
a number of steps:

1.  Understand the problem, and get some ideas on the tasks that need to
    be performed.

2.  Choose the artefacts we will create and use

3.  Design the control flow for the procedural[^38] artefacts

4.  Map these artefacts to code

5.  Compile and run the program

### Understanding the Statistics Calculator {#sub:understanding_the_statistics_calculator}

Most of the ideas around the Statistics Calculator should be fairly
straight forward. The main thing to be checked if the equation needed to
calculate the different statistics, and then to convert these into steps
for the computer.

#### Calculating Sum {#ssub:calculating_sum}

The **sum** is the simplest of the Statistics to calculate. This
involves adding together all of the numbers in the array. The main issue
here is that the computer cannot add all of these values together, and
we must rethink our logic to express it in terms of processing *for
each* element.

Think about the way you would sum a list of numbers, this is now the
task you need to code for the Computer. What is it that you do with each
number? To think this through write a list of random numbers down, and
then calculate the sum. Do it slowly, and think about the tasks that you
are performing *for each number*.

You should have noticed that you are keeping a running total, and that
you add the value of each number from the list to that. When you have
done this for each number in the list you have the total. The Pseudocode
for this is shown in
Listing [\[plst:sum\]](#plst:sum){reference-type="ref"
reference="plst:sum"}.

There are three key things to notice about the Pseudocode in
Listing [\[plst:sum\]](#plst:sum){reference-type="ref"
reference="plst:sum"}. Firstly the is used to repeat the loop once for
each element in the array. Second, the `i` variable moves through the
valid indexes for the array. Finally, the total is used to keep the
running total throughout the code.

The for loop in will repeat its body once for each value in the array.
The `i` variable will be updated to have the *current* index value each
time the loop is repeated. Within the loop the $i^{th}$ value from the
array is accessed. This is how the for loop processes each of the values
from the array.

The `total` value keeps track of the current running total. Before the
loop its value is set to 0, ensuring that it is appropriately
initialised. In the body of the loop the current ($i^{th}$) value of the
array is added to the `total`, and the result stored back into `total`.
This means that by the end of the loop the `total` variable is now
storing the sum of all of the elements of the array.

C and Pascal differ in the amount of support they have for working with
arrays. C has very limited support, meaning that you need to do some
extra work. Pascal has more build in support for arrays, making some
common tasks easier to achieve. The main difference is that C does not
keep track of the length of an array. This means that you need to pass
the number of elements in the array along with the array to functions
and procedures that will work with this data. Pascal, on the other hand,
does keep track of the length of arrays and gives you three functions
you can use to manage this: `Low` returns the first index of the array,
`High` returns the last index of the array, `Length` returns the number
of elements in the array.

#### Calculating Mean {#ssub:calculating_mean}

The mean of a list of values is the sum of those values divided by the
number of values. In the case of the Statistics Calculator program there
is already a `Sum` function, so the `Mean` function does not need to
recalculate the sum, it can just call the `Sum` and use the result
returned.

The length of the array can be calculated in Pascal using its `Length`
function, where as C can use the `size` parameter to determine the
number of elements in the array. In both cases the basic logic is the
same, you use the `Sum` function to calculate the sum and then divide
this by the number of elements in the array.

#### Calculating Maximum {#ssub:calculating_maximum}

Calculating the largest value in the array, the maximum, will require
the logic be adjusted to use the *for each* style. How do you calculate
the largest value in a list of numbers? With a small list you are likely
to just quickly scan it and see the largest value. Think about the tasks
you need to perform, and maybe think about how you would do it for a
very long list of numbers, one that spans across many pages.

The algorithm needed to find the maximum value in an array needs to
perform an action *for each* element of the array. It needs to process
each value in isolation, ignoring the other values from the list.

The key is similar to the logic from the `Sum` function. You need to
keep a *running* maximum. This will store the *current* maximum from the
array as you loop through *each element* of the array. Like the sum this
value can be updated within the loop.

The Pseudocode for this is in
Listing [\[plst:max\]](#plst:max){reference-type="ref"
reference="plst:max"}. Notice that its basic layout if the same as the
`Sum` function in Listing [\[plst:sum\]](#plst:sum){reference-type="ref"
reference="plst:sum"}. It initialises the `max` value and then loops
through the array performing an action for each value. In this case the
action is to check if the $i^{th}$ value of the array is larger than the
running maximum in the `max` variable. When this is the case a new
maximum has been found and is stored in the `max` variable.

One of the important differences between `Maximum` and `Sum` is the
initialisation of the `max` value. In `Maximum` this cannot be
initialised to 0 as this would fail to find the maximum if all values
were negative. The maximum must be a value from the array, so it is
initialised to the first value in the array. The for loop will then
start looping from the $2^{nd}$ element, as the $1^{st}$ has already
been processed.

#### Calculating Variance {#ssub:calculating_variance}

The last statistic to calculate is the Variance. The processing for this
will be very similar to the `Sum` and `Maximum` functions, though the
actual calculation is a little more complex.
Equation [\[eq:var\]](#eq:var){reference-type="ref" reference="eq:var"}
shows how the Variance of a sample is calculated.

$$\label{eq:var}
  var(x) = \frac{\displaystyle \sum_{i=1}^{n}(x_{i} - \overline{x})^2}{n - 1}$$

In Equation [\[eq:var\]](#eq:var){reference-type="ref"
reference="eq:var"} $x$ is the array being processed, $\overline{x}$ is
the mean of $x$, $x_i$ is the value of the $i^{th}$ element of $x$, and
$n$ is the number of elements in the array. The Sigma indicates that
$x_{i} - \overline{x}$ needs to be summed for each element of $x$.

The steps to calculate the Variance are therefore:

1.  Determine the value of the mean ($\overline{x}$).

2.  Calculate $(x_{i} - \overline{x})^2$ for each element, and store
    these in a running sum (called `temp`).

3.  Divide the value (from `temp`) by the number of elements in the
    array minus one.

The matching Pseudocode for this is shown in
Listing [\[plst:variance\]](#plst:variance){reference-type="ref"
reference="plst:variance"}. In this case $x$ is the `data` array. In
Step 1 the mean ($\overline{x}$) is calculated once and stored in `avg`.
The loop starts at Step 3, and runs *for each* element of the array. The
value $(x_{i} - \overline{x})^2$ is calculated for each element in Step
4, and added to the running total stored in `temp`. The final result is
then calculated and the result returned in Step 5.

### Choosing Artefacts for Statistics Calculator {#sub:choosing_artefacts_for_statistics_calculator}

In understanding these concepts we have uncovered some Functions that
will be included in the program's design.

With the calculations thought through the design seems to be coming
together. So far we have thought through the steps needed to calculate
the output, but we have not thought about how these values will be read
into the program.

Programs can be thought of as transforming data, taking inputs and
generating outputs, as shown in
Figure [5.11](#fig:input-output-overview){reference-type="ref"
reference="fig:input-output-overview"}. So far we have examined the
processing needed to create the outputs, but we still need to consider
how the data gets into the program, the inputs.

![Programs convert Inputs to
Outputs](./topics/arrays/diagrams/ProcessingOverview.pdf){#fig:input-output-overview
width="60%"}

At the start of the program the user will need to enter the values that
will be stored in the array. This task can be coded in a
`Populate Array` procedure. This will get the user to enter all of the
values into the array. In other words it will allow the user to enter
*each value* in the array.

The logic for populating the array can be split into a `Populate Array`
procedure that calls a `Read Double` function. The `Read Double`
function will be very useful across a number of different programs, so
this may be able to be used elsewhere.

#### Reading double values from the user {#ssub:reading_double_values_from_the_user}

Figure [5.12](#fig:read-double-flow){reference-type="ref"
reference="fig:read-double-flow"} shows the flowchart for the process of
reading a double value from the user. This includes a that repeatedly
asks the user to enter a number if they value they enter is not a
number. This demonstrates a standard *validation* loop, in which you
read a value, and check that it is valid in a loop.

The C and Pascal code for this are both slightly different to the
flowchart due to different way they handle input and the features they
offer to for converting the value read to a number. Details of these are
shown alongside
Listing [\[clst:read_double\]](#clst:read_double){reference-type="ref"
reference="clst:read_double"} and
Listing [\[paslst:read_double\]](#paslst:read_double){reference-type="ref"
reference="paslst:read_double"}.

![Flowchart showing the process for reading a
double](./topics/arrays/diagrams/ReadDouble.pdf){#fig:read-double-flow
width="45%"}

#### Populating the Array {#ssub:populating_the_array}

With the logic for `Read Double` in place the next step is to determine
the steps needed in the `Populate Array` procedure. This procedure will
loop and read a value from the user for each element of the array. This
can use the `Read Double` function to get the value from the user, and
then store this in the array's elements.

A flowchart illustrating the steps in `Populate Array` is shown in
Figure [5.13](#fig:populate-array-flow){reference-type="ref"
reference="fig:populate-array-flow"}. The decision node is being used to
show the control mechanism of the for loop, counting from the lowest
index of the array to the highest index. Within the body of the loop the
two instructions build a prompt string, and then use this in the call to
`Read Double`. The result returned from `Read Double` is stored in the
current ($i^{th}$) element of the array.

Once again the C and Pascal code differ in how this is implemented,
centred on how the *prompt* is built within the loop. Pascal has built
in support for Strings, so its code is much simpler. The C code for this
requires you to coordinate the steps needed to build the text for the
prompt. The details for these are shown in the text accompanying
Listing [\[clst:populate_array\]](#clst:populate_array){reference-type="ref"
reference="clst:populate_array"} and
Listing [\[paslst:populate_array\]](#paslst:populate_array){reference-type="ref"
reference="paslst:populate_array"}.

![Flowchart showing the process for
`Populate Array`](./topics/arrays/diagrams/PopulateArray.pdf){#fig:populate-array-flow
width="60%"}

#### Where is the data stored {#ssub:where_is_the_data_stored}

The last question to remain is where will the data be stored. The array
is a kind of variable, and therefore the array could be a or a . As
Global Variables should be avoided where possible, this will be coded as
a **Local Variable** within the program's `Main` procedure. It can then
be passed from there to the other Functions and Procedures in the code.

#### Overview of Statistics Calculator's design {#ssub:overview_of_statistics_calculators_design}

That completes the logic needed to implement the Statistics Calculator
Program. The final structure is shown in
Figure [5.14](#fig:stats-calc-struct){reference-type="ref"
reference="fig:stats-calc-struct"} as a Structure Chart. Notice the
double headed arrow on `data` in the call from `Main` to
`Populate Array`. This indicates that the data parameter is passing the
values into, and getting values out of the `Populate Array` procedure.
Also see how the `data` value is passed out of `Main` to the functions
that calculate the statistics.

![Structure Chart showing the structure of the `Statistics Calculator`
program](./topics/arrays/diagrams/StatsCalcStruct.pdf){#fig:stats-calc-struct
width="\\textwidth"}

### Writing the Code for Statistics Calculator {#sub:writing_the_code_for_statistics_calculator}

The flowcharts and Pseudocode shown communicate the logic that needs to
be coded into the Functions and Procedures of this program. The
following two sections,
Section [5.3](#sec:arrays_in_c){reference-type="ref"
reference="sec:arrays_in_c"} and
Section [5.4](#sec:arrays_in_pascal){reference-type="ref"
reference="sec:arrays_in_pascal"} , contain a description of the syntax
needed to code arrays in the C and Pascal programming languages. This
information can be used to write the code for the Statistics Calculator,
and other programs.

### Compiling and Running Statistics Calculator {#sub:compiling_and_running_statistics_calculator}

When the code is finished you can compile and run the program. It is a
good idea to implement the solution a little bit at a time, compiling
and running it frequently as you progress. Try implementing the solution
using the following smaller steps, and the tests shown for each.

1.  Start by getting `Read Double` to work. In `Main` just read a single
    value and output it to the Terminal. **Tests**:

    -   Check that a number can be read correctly.

    -   Try entering text, and check the error message is shown and that
        you can enter a number the next time.

    -   Try entering multiple text values on a single line.

    -   Try entering multiple text values, one after the other.

2.  Implement `Populate Array`. Include an array in `Main`, and have its
    values read in by `Populate Array`. Print the values back to the
    Terminal so that you can check this code is working. **Tests**:

    -   Enter each of the values and check they are printed out
        correctly.

    -   Try entering text, this should be handled by `Read Double` but
        check it is working correctly with `Populate Array`.

3.  Implement the `Sum` Function. **Tests**:

    -   Test that it works with some basic values.

    -   Try all negative values.

    -   Try a mix of positive and negative values.

4.  Implement the `Mean` Function. Same tests as `Sum`.

5.  Implement the `Variance` Function. Same tests as `Sum`.

6.  Finish by implementing the `Maximum` Function. Same tests as `Sum`.

By building the code a little at a time, and running tests as you go,
you will have less code to search when you do find an issue. This makes
it easier to fix those little errors that are likely to slip into the
code from time to time.

When this iterative process is complete you should have a solution for
the Statistics Calculator. You should be able to easily change this so
that it can read in ten, a hundred, or even a thousand values from the
user. This is something that would not have been possible without using
arrays.

## Managing Multiple Values in C {#sec:arrays_in_c}

### Implementing Statistics Calculator in C {#sub:implementing_statistics_calculator_in_c}

Section [5.2](#sec:arrays_using_these_concepts){reference-type="ref"
reference="sec:arrays_using_these_concepts"} of this Chapter introduced
the Statistics Calculator. A partial implementation of this program is
shown in Listing
[\[lst:c-stats-calc\]](#lst:c-stats-calc){reference-type="ref"
reference="lst:c-stats-calc"}, with the logic in the `max` and
`variance` functions still to be implemented. This program reads a
number of values from the user into an array, and then calculates and
outputs the **sum**, **mean**, **variance**, and **maximum** value from
this data.

``` {#lst:c-stats-calc .c caption="C code for the Statistics Calculator" label="lst:c-stats-calc"}
/* stats-calc.c */

#include <stdio.h>
#include <math.h>
#include <string.h>

#define DATA_SIZE 10

// Calculate the sum of the values in the array
double sum(const double data[], int size)
{
    int i;
    double result = 0;
    
    for(i = 0; i < size; i++)
    {
        result += data[i];
    }
    
    return result;
}

// Calculate the mean of the values in the array
double mean(const double data[], int size)
{
    return sum(data, size) / size;
}

// Find the largest value in the array
double max(const double data[], int size)
{
    //todo: add logic here...
    return 0;
}

// Find the standard deviation of the values in the array
double variance(const double data[], int size)
{
    //todo: add logic here...
    return 0;
}

double read_double(const char *prompt)
{
    double result;
    
    printf("%s", prompt);
    while (scanf(" %lf", &result) != 1)
    {
        scanf("%*[^\n]");
        printf("Please enter a number.\n");
        printf("%s", prompt);
    }
    
    return result;
}

void populate_array(double data[], int size)
{
    int i;
    char prompt[17] = ""; // enough space for "Enter value 99: " + terminator
    char buffer[3] = ""; // enough space for "99" + terminator
    
    for(i = 0; i < size; i++)
    {
        // Ensure that the terminator is included in the copy
        // so that the later calls to strncat know where to
        // append their details. 
        strncpy(prompt, "Enter value ", 13); // 12 + terminator
        sprintf(buffer, "%d", (i + 1) % 100); // needs space for 3 (2 + terminator)
        strncat(prompt, buffer, 2); // takes 3 spaces, 2 + terminator
        strncat(prompt, ": ", 2); // takes 3 spaces, 2 + terminator
        
        data[i] = read_double(prompt);
    }
}

// Implements a statistics calculator. The program reads in values entered by the user
// and then calculates the sum, mean, variance, and max
int main()
{
    double data[DATA_SIZE];
    
    populate_array(data, DATA_SIZE);
    
    printf("\nCalculating statistics...\n\n");
    
    printf("Sum:        %4.2f\n", sum(data, DATA_SIZE));
    printf("Mean:       %4.2f\n", mean(data, DATA_SIZE));
    printf("Variance:   %4.2f\n", variance(data, DATA_SIZE));
    printf("Max:        %4.2f\n", max(data, DATA_SIZE));
    
    return 0;
}
```

### C Array Declaration {#sub:c_array_declaration}

C allows you to declare variables that are arrays. This is done using
the `[ ]` to denote the number of elements in the array (*n*). Indexes
will then be *0* to *n-1*.

### C Array Copying {#sub:c_array_copying}

In C you cannot use simple assignment to copy all of the elements of an
array into another array. Instead you can use the `memcpy` (memory copy)
function to perform this task for you. It copies a chunk of memory from
one location to another.

::: {#tbl:memcpy}
                         **Function Prototype**                        
  -------------------------------------------------------------------- -------------------------------------------
                                                                       
   `void *memcpy(void *destination, const void *source, size_t num )`  
                                                                       
                              **Returns**                              
                                `void *`                               Destination is returned, can be ignored.
                             **Parameter**                             **Description**
                            ` destination `                            The location where the data is copied to.
                                                                       
                               ` source `                              The data to copy.
                                                                       
                                ` num `                                The number of bytes to copy.

  : Details of the `memcpy` function
:::

The `memcpy` function needs to be told the number of bytes to copy. The
`sizeof` operator can be used to get this information for a type or
variable. The details of this operator are shown in
Figure [\[csynt:sizeof\]](#csynt:sizeof){reference-type="ref"
reference="csynt:sizeof"}.

### C For Loop {#sub:c_for_loop}

The in C can do much more than just counting, but that is its primary
purpose. You can use this to implement the logic to process each element
of an array.

The for loop itself is controlled by three aspects. The first is an
initialiser, it sets the first value for the control variable (usually
`i` if you are using it to index an array). The second part is the
condition, the body will run *while* this is true just like a . The
third part is a post loop increment, you use this to move the index to
the next value.

The standard for loop is: `for(i = 0; i < size; i++)\{...\}` . This can
be read as 'for `i` starts at 0, while `i` is less than `size`, do the
following then increment `i`'. If `size` is three then this counts 0, 1,
2. Repeating the body of the loop three times.

### C String {#sub:c_string}

C was designed to build for use with the Unix operating system. When the
language was designed string manipulation was not a high priority, and
therefore C does not have built in capabilities to perform tasks like
concatenating strings, and copying strings (i.e. assigning a string a
value after it has been declared).

Working with c-strings requires that you think about how the text is
represented in the computer.
Table [5.3](#tbl:c-string-fred){reference-type="ref"
reference="tbl:c-string-fred"} shows the characters used to store the
text value 'Fred'.

As C does not keep the length of the array there needs to be a means of
determining how long the string is. The method that C choose was to
place a **sentinel** value at the end of the string. This marks the
position in the array where the string ends. The sentinel is the `null`
character, the one with value the `0`.

::: {#tbl:c-string-fred}
  Characters:            F       r       e       d     `\0`
  -------------------- ------ ------- ------- ------- ------
  Bytes Values[^39]:    `70`   `114`   `101`   `100`   `0`

  : The characters and byte values for the c-string containing the text
  'Fred'
:::

Space characters are distinct from the `null` character.
Table [5.4](#tbl:c-string-fred-smith){reference-type="ref"
reference="tbl:c-string-fred-smith"} shows the characters involved in
storing the text 'Fred Smith'. The space character is the value 32, and
the sentinel value only appears at the end of the c-string. To store
'Fred Smith' you need an array that can store at least 11 characters.
Ten for the characters in the name, and one for the sentinel.

::: {#tbl:c-string-fred-smith}
  Characters:            F       r       e       d             S       m       i       t       h     `\0`  
  -------------------- ------ ------- ------- ------- ------ ------ ------- ------- ------- ------- ------ --
  Bytes Values[^40]:    `70`   `114`   `101`   `100`   `32`   `83`   `109`   `105`   `116`   `104`   `0`   

  : Characters for 'Fred Smith', the space has the character value 32.
:::

It is possible for an array to have more characters that are needed.
Table [5.5](#tbl:c-string-fred-null-smith){reference-type="ref"
reference="tbl:c-string-fred-null-smith"} shows an array with 11
characters that is storing the c-string 'Fred'. The `null` character at
index 4 (the $5^{th}$ character) ends the c-string and the remainder of
the data in the array will be ignored by the c-string functions.

::: {#tbl:c-string-fred-null-smith}
  Characters:            F       r       e       d     `\0`    S       m       i       t       h     `\0`  
  -------------------- ------ ------- ------- ------- ------ ------ ------- ------- ------- ------- ------ --
  Bytes Values[^41]:    `70`   `114`   `101`   `100`   `0`    `83`   `109`   `105`   `116`   `104`   `0`   

  : This would only print 'Fred', as the 0 character indicates the end
  of the c-string
:::

The code in
Listing [\[clst:test-strings\]](#clst:test-strings){reference-type="ref"
reference="clst:test-strings"} shows some examples of the main
operations you may want to perform on strings. This includes the
following actions:

-   **Initialisation**: Creating and initialising a string.

-   **Input**: Reading words, and lines, from the Terminal.

-   **Comparison**: Checking if two strings are equal. Notice that you
    also need to check the `null` value.

Other common string operations are found in
Listing [\[clst:populate_array\]](#clst:populate_array){reference-type="ref"
reference="clst:populate_array"}. These included:

-   **Copy**: Assigning one string to another, as you cannot use the
    assignment statement to achieve this in C.

-   **Concatenate**: Adding one string to the end of another.

#### Print and Scanning in Strings {#ssub:print_and_scanning_in_strings}

The `stdio.h` header also provides version of `printf` and `scanf` that
are used to write values to, and read values from strings. The `sprintf`
function writes data into a `destination` string, whereas the `sscanf`
function reads data out of a source `string`.
Table [5.6](#tbl:sprintf){reference-type="ref" reference="tbl:sprintf"}
shows the details for the `sprintf` function,
Table [5.7](#tbl:sscanf){reference-type="ref" reference="tbl:sscanf"}
shows the details for `sscanf`.

::: {#tbl:sprintf}
                   **Function Prototype**                   
  --------------------------------------------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                                            
   `int sprintf(char *destination, const char *format, …)`  
                                                            
                         **Returns**                        
                            `int`                           The number of characters written to the `destination` by `sprintf`.
                        **Parameter**                       **Description**
                       ` destination `                      The string to write the output into. **Warning:** You are responsible for ensuring there is enough space.
                                                            
                         ` format `                         The text that is to be written to the string. This text may contain format tags to include other values. This is the same as `printf`, see Figure [\[csynt:program-creation-format-string\]](#csynt:program-creation-format-string){reference-type="ref" reference="csynt:program-creation-format-string"} for the syntax of the format tag.
                                                            
                             `…`                            Optional values, must have at least as many values as format tags.

  : Parameters that must be passed to `sprintf`
:::

::: {#tbl:sscanf}
                   **Function Prototype**                   
  --------------------------------------------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                                            
   `int sscanf(const char *source, const char *format, …)`  
                                                            
                         **Returns**                        
                            `int`                           The number of values read by `sscanf`.
                        **Parameter**                       **Description**
                         ` source `                         The string from which the input is read.
                                                            
                         ` format `                         The format specifier describing what is to be read from the Terminal. This is the same as with `scanf`, see Table [3.4](#tbl:format specifiers){reference-type="ref" reference="tbl:format specifiers"}.
                                                            
                             `…`                            The variables into which the values will be read. There must be at least as many variables as format tags in the format specifier.

  : Parameters that must be passed to `sscanf`
:::

#### Example use of C-string functions {#ssub:example_use_of_c_string_functions}

The Statistics Calculator requires some string manipulation to generate
the prompt that will be shown to the user. The prompt is created from
the text '`Enter value`', the value of `i + 1`, and the text '`: `'. The
four function calls needed to achieve this are shown in
Figure [5.15](#fig:cstringops){reference-type="ref"
reference="fig:cstringops"}.

![Example usage of c-string functions from the Statistics
Calculator](./topics/arrays/images/CStringOps.pdf){#fig:cstringops
width="\\textwidth"}

### C Function (with Array Parameters) {#sub:c_fn_with_array}

In C you can only use to pass an array to a Function or Procedure. There
are two ways of passing arrays by reference in C: one uses the bracket
notation (`type name[ ]`), the other an asterisks notation
(`type *name`). The asterisks notation is more general pass by
reference, and will be covered in a later chapter in more details. The
brackets notation accomplishes the same task, and indicates that the
passed data will be an array.[^42]

The optional **`const`** operator allows you to indicate that the passed
in value will not be changed in the Function or Procedure. This is
important with strings, as if you want to pass a string literal to a
parameter it must be a `const char *`, as the literal cannot be changed.

## Managing Multiple Values in Pascal {#sec:arrays_in_pascal}

### Implementing Statistics Calculator in Pascal {#sub:implementing_statistics_calculator_in_pas}

Section [5.2](#sec:arrays_using_these_concepts){reference-type="ref"
reference="sec:arrays_using_these_concepts"} of this Chapter introduced
the Statistics Calculator. A partial implementation of this program is
shown in Listing
[\[lst:pas-stats-calc\]](#lst:pas-stats-calc){reference-type="ref"
reference="lst:pas-stats-calc"}, with the logic in the `max` and
`variance` functions still to be implemented. This program reads a
number of values from the user into an array, and then calculates and
outputs the **sum**, **mean**, **variance**, and **maximum** value from
this data.

``` {#lst:pas-stats-calc .pascal caption="Pascal code for the Statistics Calculator" label="lst:pas-stats-calc"}
program StasticsCalculator;
uses SysUtils;

const DATA_SIZE = 10;

// Calculate the Sum of the values in the array
function Sum(const data: array of Double): Double;
var
  i: Integer;
begin
    result := 0;
    
    for i := Low(data) to High(data) do
    begin
        result += data[i];
    end;
end;

// Calculate the Mean of the values in the array
function Mean(const data: array of Double): Double;
begin
    result := Sum(data) / Length(data);
end;

// Find the largest value in the array
function Max(const data: array of Double): Double;
begin
    //todo: add logic here...
    result := 0;
end;

// Find the standard deviation of the values in the array
function Variance(const data: array of Double): Double;
begin
    //todo: add logic here...
    result := 0;
end;

function ReadDouble(prompt: String): Double;
var
  line: String;
begin
    Write(prompt);
    ReadLn(line);
    while not TryStrToFloat(line, result) do
    begin
        WriteLn('Please enter a number.\n');
        Write(prompt);
        ReadLn(line);
    end;
end;

procedure PopulateArray(var data: array of Double);
var
  i: Integer;
begin
    for i := Low(data) to High(data) do
    begin
        data[i] := ReadDouble('Enter value ' + IntToStr(i) + ': ');
    end;
end;

// Implements a statistics calculator. The program reads in values entered by the user
// and then calculates the Sum, Mean, variance, and max
procedure Main();
var
  data: array [0..DATA_SIZE-1] of Double;
begin
    PopulateArray(data);
    
    WriteLn('Calculating statistics...');
    
    WriteLn('Sum:        ', Sum(data):4:2);
    WriteLn('Mean:       ', Mean(data):4:2);
    WriteLn('Variance:   ', variance(data):4:2);
    WriteLn('Max:        ', max(data):4:2);
end;

begin
    Main();
end.
```

### Pascal Array Declaration {#sub:pas_array_declaration}

Pascal allows you to declare array variables and parameters.

### Pascal For Loop {#sub:pas_for_loop}

The in Pascal can be used to implement the logic to process each element
of an array. With the for loop you specify a control variable, and the
range of values it will loop over. When the loop is started the control
variable is assigned the initial value, at the end of each loop this
value is incremented (for `to`) or decremented (for `downto`) until it
has processed all values in the indicated range.

### Pascal Array Functions {#sec:pascal_array_functions}

Pascal includes a number of functions to help you work with arrays.
These allow you to determine the length of the array, and its range of
indexes. These functions are typically used in conjuncture with the for
loop.

::: {#tbl:pas-array-fns}
             **Function Prototypes**             
  ---------------------------------------------- ------------------------------------------------
                                                 
   `function Length( arg: array of …): Integer`  
                                                 
                   **Returns**                   
                    `Integer`                    The number of elements in the `arg` array.
                  **Parameter**                  **Description**
                     ` arg `                     The array you want to get the length of.
                                                 
                                                 
    `function Low( arg: array of …): Integer`    
                                                 
                   **Returns**                   
                    `Integer`                    The lowest index in the `arg` array.
                  **Parameter**                  **Description**
                     ` arg `                     The array you want to get the lowest index of.
                                                 
                                                 
    `function High( arg: array of …): Integer`   
                                                 
                   **Returns**                   
                    `Integer`                    The last index in the `arg` array.
                  **Parameter**                  **Description**
                     ` arg `                     The array you want to get the last index of.

  : Parameters that must be passed to `Length`, `High`, and `Low`
:::

### Pascal Array Parameters {#sub:pas_array_parameters}

Pascal allows arrays to be passed as parameters to functions and
procedures. Like other parameters these can be passed by value or by
reference.

### Pascal String {#sub:pas_string}

Pascal has built in support for strings. Behind the scenes Pascal uses
an array of characters to store the text for each string. The first
element of this array stores an integer that indicates the size of the
string, and this is followed by the text characters.

::: {#tbl:pas-string-fred}
  Characters:                  F       r       e       d
  -------------------- ----- ------ ------- ------- -------
  Bytes Values[^43]:    `4`   `70`   `114`   `101`   `100`

  : The characters and byte values for the string containing the text
  'Fred' in Pascal
:::

## Understanding Arrays {#sec:understanding_arrays}

s offer a means of storing and working with a list of values in your
code. Each array has a number of elements, each of which has a value,
and can be accessed using an index. Together with the , arrays provide a
means of managing multiple values in your code. The following
illustrations show how these work in the computer, and should help you
better understand how arrays can be used within your code.

### Understanding `Populate Array` {#sub:understanding_populate_array}

Section [5.2.1](#sub:designing_statistics_calculator){reference-type="ref"
reference="sub:designing_statistics_calculator"} outlined the pseudocode
and flowcharts for a small statistics programs. This included a number
of functions and procedures that helped divide the program's code into
smaller units of work. One of these procedures was `Populate Array`,
discussed in
Section [5.2.3.2](#ssub:populating_the_array){reference-type="ref"
reference="ssub:populating_the_array"} . This procedure is responsible
for reading values from the user and using these to populate the
program's array, and the flowchart for this logic is shown in
Figure [5.16](#fig:populate-array-flow-understanding){reference-type="ref"
reference="fig:populate-array-flow-understanding"}.

![Flowchart showing the process for `Populate Array`, from
Figure [5.13](#fig:populate-array-flow){reference-type="ref"
reference="fig:populate-array-flow"}](./topics/arrays/diagrams/PopulateArray.pdf){#fig:populate-array-flow-understanding
width="50%"}

The following illustrations will show this code running to populate an
array that contains three values. This will show how the array is passed
by reference, and how the for loop works together with the array to
populate all elements.

#### Main starts, and the array is allocated space {#ssub:main_starts_and_the_array_is_allocated_space}

All local variables are allocated space on the Stack when the function
or procedure they are declared in is called. In this example the `Main`
procedure is executed and space is allocated for its `my_data` variable.
This variable is an that is used to store three `double` values. When
`Main` is loaded onto the Stack there is space allocated for three
`double` values associated with the `my_data` variable.

![When the program starts `Main` allocates space for its local
variables, including the
array](./topics/arrays/images/PopulateArray1.pdf){#fig:populate-array-vis-1
width="\\textwidth"}

#### Populate array is called, and a reference to `mydata` passed in {#ssub:populate_array_is_called_and_a_reference_to_my_data_passed_in}

`Populate Array` is called as the first step in `Main`. This is passed
the `my_data` variable (pass by reference), rather than being passed the
values from within that variable. This gives the `data` parameter access
to the memory where `my_data` is stored.

![Populate array is called, and a reference to the `my_data` array is
pass to its `data`
parameter](./topics/arrays/images/PopulateArray2.pdf){#fig:populate-array-vis-2
width="\\textwidth"}

#### Step 1 of `Populate Array` is run {#ssub:step_1_of_populate_array_is_run}

Step 1 of `Populate Array` initialises the for loop's control variable
(`i` in this case). This variable keeps track of the times the loop body
has executed, and can be used to get the *current* value from the array.

![Step 1 of `Populate Array` is called, and the for loop sets i to the
lowest index of the `data`
array](./topics/arrays/images/PopulateArray3.pdf){#fig:populate-array-vis-3
width="\\textwidth"}

#### Step 2 constructs the prompt to be shown to the user {#ssub:step_2_constructs_the_prompt_to_be_shown_to_the_user}

The user needs to be told what to enter. The `prompt` is a string that
will contain this message so that it can be passed to `Read Double`. The
value for the prompt will use the loop's control variable (the counter)
so that the user known which value they are up to.

![Step 2 builds the prompt `Enter value 1: ` which will be shown to the
user](./topics/arrays/images/PopulateArray4.pdf){#fig:populate-array-vis-4
width="\\textwidth"}

#### Step 3 reads a value from the user and stores it in the array at index 0 {#ssub:step_3_reads_a_value_from_the_user_and_stores_it_in_the_array_at_index_0}

The next step calls the `Read Double` function. This is responsible for
reading a value from the user, and returning it to the caller. The value
returned is then stored in an element of the array. The `i` variable is
read to determine the position where this value should be stored. This
means that you can think of `i` as referring to the *current* element of
the array.

![Step 3 reads a `double` value from the user and stores it in
`data[i]`](./topics/arrays/images/PopulateArray5.pdf){#fig:populate-array-vis-5
width="\\textwidth"}

#### Control returns to Step 1 as the loop body has ended {#ssub:control_returns_to_step_1_as_the_loop_body_has_ended}

At the end of the loop body the for loop performs two actions. It has
finished the first pass through the loop, so its control variable (the
counter) needs to be incremented to 1. Then it needs to jump back to
check its condition. This will determine if the loop's body is executed
again or skipped. In this case `i` is still in the defined range so the
loop's body is run again.

![At the end of the loop body *i* is incremented and control jumps back
to check the loop's
condition](./topics/arrays/images/PopulateArray6.pdf){#fig:populate-array-vis-6
width="\\textwidth"}

#### Second prompt is built asking the user to enter value 2 {#ssub:second_prompt_is_built_asking_the_user_to_enter_value_2}

Back at step 2 again, the prompt needs to be recreated. This time its
message will be '`Enter value 2: `'. The process to create this is the
same, with the value of `i + 1` being converted to a String, and the
three parts concatenated together and stored in `prompt`. This overrides
the details in the existing array, reusing the same memory to store
these values.

![Step 2 builds the prompt `Enter value 2: ` which will be shown to the
user](./topics/arrays/images/PopulateArray7.pdf){#fig:populate-array-vis-7
width="\\textwidth"}

#### `Populate Array` stores the second value read into `data[1]` {#ssub:populate_array_stores_the_second_value_read_into_data[1]}

Step 3 uses `Read Double` again to get the value to store in the second
element of the array. To find where this should be stored the computer
calculates the value of the index, reading this from the `i` variable.
The value returned from `Read Double` is then stored in the array
referred to by `data`, at index `1`.

![The second value read is stored in
`data[1]`](./topics/arrays/images/PopulateArray8.pdf){#fig:populate-array-vis-8
width="\\textwidth"}

#### `i` is incremented again, and control returns to Step 1 to determine if loop runs again {#ssub:i_is_incremented_again_and_control_returns_to_step_1_to_determine_if_loop_runs_again}

The end of the for loop's body has been reached again, so it performs
two actions: it increments its control variable (`i` in this case) and
jumps back to check its condition (step 1). The condition then
determines if the loop's body is to be executed again or skipped. In
this case `i` is still in the defined range so the loop's body is run a
third time.

![At the end of the loop body *i* is incremented and control jumps back
to check the loop's
condition](./topics/arrays/images/PopulateArray9.pdf){#fig:populate-array-vis-9
width="98%"}

#### Third prompt is built asking the user to enter value 3 {#ssub:third_prompt_is_built_asking_the_user_to_enter_value_3}

Back at step 2 for the third time. This step recreates the prompt, this
time with the message '`Enter value 3: `'. The process to create this is
the same as before, with the value of `i + 1` being converted to a
String, and the three parts concatenated together and stored in
`prompt`. Remember that this overrides the data currently in the
`prompt` array.

![Step 2 builds the prompt `Enter value 3: ` which will be shown to the
user](./topics/arrays/images/PopulateArray10.pdf){#fig:populate-array-vis-10
width="\\textwidth"}

#### `Populate Array` stores the third value read into `data[2]` {#ssub:populate_array_stores_the_third_value_read_into_data[2]}

Once again, step 3 uses `Read Double` get the value to store in the
array. In this case `i` indicates that this should be stored in the
element at index 2 (skipping 2 elements, so storing the value in the
third).

![The second value read is stored in
`data[1]`](./topics/arrays/images/PopulateArray11.pdf){#fig:populate-array-vis-11
width="\\textwidth"}

#### `i` is incremented again, and control jumps back to check the condition a fourth time {#ssub:i_is_incremented_again_and_control_jumps_back_to_check_the_condition_a_fourth_time}

The end of the for loop's body has been reached again, so it performs
two actions: it increments its control variable (`i` in this case) and
jumps back to check its condition (step 1). This time the value of `i`
is outside the range of the indexes for `data` (`0..2`, it is no longer
less than 3). This means that the loop's body should not be run again,
and control will jump past the body to the next step. As there are no
more steps in `Populate Array` it will end.

![At the end of the loop body *i* is incremented and control jumps back
to check the loop's
condition](./topics/arrays/images/PopulateArray12.pdf){#fig:populate-array-vis-12
width="\\textwidth"}

#### `Populate Array` ends, and has populated `Main`'s `mydata` array {#ssub:populate_array_ends_and_has_populated_main_s_my_data_array}

When `Populate Array` ends its space on the stack is released so that it
can be used again, and control returns to `Main`. `Populate Array` was
responsible for reading values from the user and storing these in the
array passed to it, and if you look at
Figure [5.29](#fig:populate-array-vis-13){reference-type="ref"
reference="fig:populate-array-vis-13"} you can see that this has been
achieved.

The instructions in `Populate Array` commanded the computer to read a
value from the user and store it in the current element (the $i^{th}$
element) of the array. These actions were then repeated by the *for
each* index of the array. Together the for loop and its body allow you
to define actions that must be performed on all elements in an array.

![Control returns to `Main`, and its `my_data` array has been
populated](./topics/arrays/images/PopulateArray13.pdf){#fig:populate-array-vis-13
width="\\textwidth"}

### Understanding `Sum` {#sub:understanding_sum}

Figure [5.30](#fig:sum-understanding){reference-type="ref"
reference="fig:sum-understanding"} shows the flowchart of the `Sum`
function from the Statistics Calculator program. This algorithm was
developed in the
Section [5.2.2.1](#ssub:calculating_sum){reference-type="ref"
reference="ssub:calculating_sum"}, and its Pseudocode is shown in
Listing [\[plst:sum\]](#plst:sum){reference-type="ref"
reference="plst:sum"}. The `Sum` function is responsible for calculating
the sum of all of the values in the array passed to it. This is achieved
by having a `total` variable that is initialised to 0, and then has the
value of *each element* from the array added to it.

![Flowchart showing the process for
`Sum`](./topics/arrays/diagrams/SumFlow.pdf){#fig:sum-understanding
width="50%"}

The following code will show how this function is executed on an array
with three values in it. This will continue the execution from
Section [5.5.1](#sub:understanding_populate_array){reference-type="ref"
reference="sub:understanding_populate_array"} , though the same process
would occur for any array values.

#### `Sum` is called, and passed the `my data` array {#ssub:sum_is_called_and_passed_the_values_in_my_data}

When `Sum` is called it is passed the array to read its values from.
This is passed in the same way as was done in `Populate Array`. The
difference here is that this is passed using a `const` reference, to
indicate that `Sum` is not allowed to change the data in the array. This
means that `Sum` will not compile if you update values in this array,
and provide a guarantee to the caller that their data will not change
when given to the `Sum` function.

![`Sum` is called, and it is passed the array to get its values
from](./topics/arrays/images/Sum1.pdf){#fig:sum-array-vis-1
width="\\textwidth"}

#### `total` is initialised to 0 {#ssub:total_is_initialised_to_0}

The first action in `Sum` is to set the value of `total` to 0. `total`
will be used to store the running total of the array, and it must start
at 0. Remember that the space on the stack was used before, and
therefore these variables get seemingly random values initially. It is
important to remember to always initialise the variables you are using.

![`Total` is initialised, having its value set to
0](./topics/arrays/images/Sum2.pdf){#fig:sum-array-vis-2
width="\\textwidth"}

#### For loop initialises `i` {#ssub:for_loop_initialises_i}

Step 2 of `Sum` starts the for loop that will iterate over the elements
of the `data` array. The for loop's control variable, `i`, is set to the
first index of the array and the condition checks if the loop's body
should run. As `i` is in the range `0..2`, it is less than 3, control
will jump into the loop's body making step 3 the next instruction.

![`i` is initialised by the for loop, and control jumps to the loop's
body](./topics/arrays/images/Sum3.pdf){#fig:sum-array-vis-3
width="\\textwidth"}

#### `total` is increased by the value in `data[0]` {#ssub:total_is_incremented_by_the_value_in_data[0]}

The body of the for loop reads the *current* value from the `data`
array, `data[i]`. As `i` is currently 0 this reads the first element of
the array. This reads the value 10.0, which is added to the current
`total`, 0. The resulting value is then stored back into the `total`
variable, giving it the value 10.0, calculated from the expression
`total + data[0]`, which is `0 + 10.0`.

![Total is increased by the value in
`data[0]`](./topics/arrays/images/Sum4.pdf){#fig:sum-array-vis-4
width="\\textwidth"}

#### For loop increases the value of i and runs the loop body a second time {#ssub:for_loop_increases_the_value_of_i_and_runs_the_loop_body_a_second_time}

At the end of the for loop it increments the value of its control
variable, assigning `i` the value 1, and then jumps back to check its
condition. As `i` is still in the range 0..2 the loop body will be run
again, making step 3 the next action.

![At the end of the loop body `i` is incremented, and control jumps back
to check the
condition](./topics/arrays/images/Sum5.pdf){#fig:sum-array-vis-5
width="95%"}

#### The value of `total` is increased by the value in `data[1]` {#ssub:the_value_of_total_is_increased_by_the_value_in_data[1]}

The body of the for loop reads the *current* value from the `data`
array, `data[i]`. Now `i` has the value 1 it reads the second element of
the array. This reads the value -5, which is added to the current
`total`, 10.0. The resulting value is then stored back into the `total`
variable, giving it the value 5.0, calculated from the expression
`total + data[1]`, which is `10.0 + -5.0`.

![Total is increased by the value in
`data[1]`](./topics/arrays/images/Sum6.pdf){#fig:sum-array-vis-6
width="\\textwidth"}

#### For loop increases the value of i and runs the loop body a third time {#ssub:for_loop_increases_the_value_of_i_and_runs_the_loop_body_a_third_time}

The end of the for loop has been reached again, so it increments the
value of its control variable, assigning `i` the value 2, and then jumps
back to check its condition. As `i` is still in the range 0..2 the loop
body will run a third time, making step 3 the next action.

![At the end of the loop body `i` is incremented, and control jumps back
to check the
condition](./topics/arrays/images/Sum7.pdf){#fig:sum-array-vis-7
width="95%"}

#### The value of `total` is increased by the value in `data[2]` {#ssub:the_value_of_total_is_increased_by_the_value_in_data[2]}

The body of the for loop reads the *current* value from the `data`
array, `data[i]`. Now `i` has the value 2 it reads the second element of
the array. This reads the value 17.21, which is added to the current
`total`, 5.0. The resulting value is then stored back into the `total`
variable, giving it the value 22.21, calculated from the expression
`total + data[2]`, which is `5.0 + 17.21`.

![Total is increased by the value in
`data[1]`](./topics/arrays/images/Sum8.pdf){#fig:sum-array-vis-8
width="\\textwidth"}

#### For loop increases the value of `i`, and this time the loop finishes {#ssub:for_loop_increases_the_value_of_i_and_this_time_the_loop_finishes}

The end of the for loop has been reached again, so it increments the
value of its control variable, assigning `i` the value 3, and then jumps
back to check its condition. This time `i` is no longer in the range
0..2 (it is not less than 3), so the loop body will now be skipped,
making step 4 the next action.

![At the end of the loop body `i` is incremented, and control jumps back
to check the
condition](./topics/arrays/images/Sum9.pdf){#fig:sum-array-vis-9
width="95%"}

#### `Sum` function returns the `total` to the expression in `Main` {#ssub:sum_function_returns_the_total_to_the_expression_in_main}

The end of the for loop has been reached again, so it increments the
value of its control variable, assigning `i` the value 3, and then jumps
back to check its condition. This time `i` is no longer in the range
0..2 (it is not less than 3), so the loop body will now be skipped,
making step 4 the next action.

![Step 4 indicates that the value in `total` is returned to
`Main`](./topics/arrays/images/Sum10.pdf){#fig:sum-array-vis-10
width="95%"}

#### `Main` outputs the sum to the Terminal {#ssub:main_outputs_the_sum_to_the_terminal}

`Sum` returns its value to be used in step 2 of `Main`. This step
outputs the value returned to the Terminal. So by the end of Step 2 in
`Main` the sum has been calculated, and written to the Terminal.

![Step 4 indicates that the value in `total` is returned to
`Main`](./topics/arrays/images/Sum11.pdf){#fig:sum-array-vis-11
width="95%"}

## Array Examples {#sec:array_examples}

### Bubble Game (start) {#sub:bubble_game_start_}

The bubble game has ten floating bubbles for the user to 'pop'. Each
bubble is a sprite, which gives it a location on the screen and a
movement vector. When the program run the bubbles move in random
directions, reappearing at a new random location if they go off the
screen.

![Example execution of the Bubbles
program](./topics/type-decl/examples/Bubbles.png){#fig:bubbles-img
width="80%"}

## Array Exercises {#sec:array_exercises}

### Concept Questions {#sub:array_concept_questions}

Read over the concepts in this chapter and answer the following
questions:

1.  How is an array different to a standard variable?

2.  How do arrays make it easier to work with multiple values?

3.  Why is 0 the index of the first element in an array?

4.  How many bytes in memory would an array of 10 integers require?

5.  Draw a picture to show how the 10 integer values are stored in
    memory. Indicate how these values relate to the array.

6.  How can you access an element from an array?

7.  How can you copy the contents of one array into another array?

8.  Why should you pass an array to a parameter by reference, rather
    than by value?

9.  Can you perform an action on all elements of an array?

10. How should you *rethink* actions that needs to be performed on all
    elements in an array?

11. How does the for loop work together with an array to perform an
    action on each element in an array?

12. Why is a string an array? What values are stored in the elements of
    a string?

13. Strings in C and Pascal have a single byte overhead. What is this
    overhead for? Why is it needed?

14. Can you read/write past the end of an array (i.e. reading/writing to
    the 11th element when the array only contains 10 elements)? What can
    happen if you do this?

### Code Reading Questions {#sub:code_reading_questions_array}

Use what you have learnt to read and understand the following code
samples.

1.  Read the C code in
    Listing [\[clst:all-below\]](#clst:all-below){reference-type="ref"
    reference="clst:all-below"}, or the Pascal code in
    Listing [\[plst:all-below\]](#plst:all-below){reference-type="ref"
    reference="plst:all-below"}, and answer the following questions:

    1.  What is returned by the function if it is passed \...

               **data\[ \]**       **sz** (C only)   **max**
          ----------------------- ----------------- ---------
              { 1, 2, 3, 4 }              4            10
           { -4, 6, 20, 11, 92 }          5            50
                { 2, 7, 1 }               3             5

    2.  What would be a good name for this function?

    3.  Why is the array passed in using the const modifier?

2.  Read the C code in
    Listing [\[clst:contains\]](#clst:contains){reference-type="ref"
    reference="clst:contains"}, or the Pascal code in
    Listing [\[plst:contains\]](#plst:contains){reference-type="ref"
    reference="plst:contains"}, and answer the following questions:

    1.  What does the code do?

    2.  What would be a good name for this function?

    3.  What is returned by the function if it is passed \...

               **data\[ \]**       **sz** (C only)   **val**
          ----------------------- ----------------- ---------
              { 1, 2, 3, 4 }              4             3
           { 7, 12, 20, 51, -6 }          5            10
                { 2, 7, 1 }               3             1
            { -1, -2, -3, -4 }            4            -3
           { 6, 12, 18, 24, 30 }          5            22

3.  The following code is designed to find the median (middle value) of
    a array of numbers, but does it work? Read the C code in
    Listing [\[clst:median\]](#clst:median){reference-type="ref"
    reference="clst:median"}, or the Pascal code in
    Listing [\[plst:median\]](#plst:median){reference-type="ref"
    reference="plst:median"}, and answer the following questions:

    1.  Assume that data contains the values `[1,2,3,4,5,6,7,8]`,
        execute this code by hand and show the steps involved. Explain
        any shortcuts you take.

    2.  What value is returned when the function ends?

    3.  You should have noticed that there is a bug in this program. How
        can it be fixed?

    4.  What does this program assume about the data in the array?

    5.  Can you think of a simpler solution? Explain your solution.

4.  Read the C code in
    Listing [\[clst:insert_at\]](#clst:insert_at){reference-type="ref"
    reference="clst:insert_at"}, or the Pascal code in
    Listing [\[plst:insert_at\]](#plst:insert_at){reference-type="ref"
    reference="plst:insert_at"}, and answer the following questions:

    1.  This procedure alters the array passed to it. Hand execute the
        procedure for the values shown in the following table. Record
        your workings as well as the final answer.

    2.  What does the code do?

    3.  What would be a good name for this procedure?

    4.  What would be good names for `param3` and `param4`?

              **data\[\]**      **sz** (C only)   **param3**   **param4**
          -------------------- ----------------- ------------ ------------
             { 1, 2, 3, 4 }            4              2            10
             { 8, 10, 11 }             3              1            9
           { -1, -2, -3, -4 }          4              0            0
               { 42, 42 }              2              1            73

### Code Writing Questions: Applying what you have learnt {#sub:code_writing_questions_applying_what_you_have_learnt_array}

Apply what you have learnt to the following tasks:

1.  Create a program that reads in the user's name and then outputs the
    message 'Hello ' and the users name. For example, if the user enters
    the name 'Fred' then the program outputs the message 'Hello Fred!'
    to the Terminal. See
    Table [5.10](#tbl:hello-user){reference-type="ref"
    reference="tbl:hello-user"}.

    ::: {#tbl:hello-user}
      **Program Description**   
      ------------------------- ----------------------------------------------------------------------------------------------
      **Name**                  *Hello User*
                                
      **Description**           Asks the user to enter their name, and then outputs 'Hello ' and their name to the Terminal.

      : Description of the *Hello User* program.
    :::

2.  Create a program that reads in the user's name and then determines
    if the name is a *silly name*. For example, if the user enters the
    name 'Fred' the program could output, 'Fred is an awesome name!',
    but if the user enters any other name they get a message like
    'Andrew is a silly name!'. Customise the message for your friends
    and tutor. See Table [5.11](#tbl:silly_name){reference-type="ref"
    reference="tbl:silly_name"}.

    ::: {#tbl:silly_name}
      **Program Description**   
      ------------------------- ------------------------------------------------------------------------------------------
      **Name**                  *Silly Name Test*
                                
      **Description**           Asks the user to enter their name, and then outputs a message based on the name entered.

      : Description of the *Silly Name* program.
    :::

3.  Complete the implementation of the Statistics Calculator. Then add
    the following additional functionality:

    1.  Add a **Print All** procedure to print all of the values stored
        in the array to the Terminal.

    2.  Add a **Frequency** function that calculates the frequency of a
        value in the array.

    3.  Add a **Standard Deviation** function.

    4.  Add a **Minimum** function.

4.  Implement the Bubble Game from
    Section [5.6.1](#sub:bubble_game_start_){reference-type="ref"
    reference="sub:bubble_game_start_"} . You will need to do the
    following:

    1.  Download the appropriate SwinGame template.

    2.  Extract the template into a folder called 'Bubbles'

    3.  Find a picture of a bubble that is about 30 pixels wide and
        high.

    4.  Place the bubble image in the `Resources/images` folder in your
        SwinGame project.

    5.  Implement the code from
        Section [5.6.1](#sub:bubble_game_start_){reference-type="ref"
        reference="sub:bubble_game_start_"}.

    6.  Run the game and you should something similar to
        Figure [5.43](#fig:bubble_game){reference-type="ref"
        reference="fig:bubble_game"}.

        ![The start of a bubble
        game](./topics/arrays/exercises/BubblesGame.png){#fig:bubble_game
        width="60%"}

    7.  Extend the game in one or more of the following ways:

        1.  Play a sound effect when the bubbles appear.

        2.  Add a background image.

        3.  Check if the user has clicked the bubble. If they have, play
            a pop sound effect and place the bubble.

        4.  Add a score, and give points for the number of bubbles
            popped.

        5.  Try changing the game dynamics\... for example: Start new
            bubbles at the top of the screen, have them move down the
            screen, and pop when they hit the ground, ending the game.

        6.  ...

### Extension Questions {#sub:extension_questions_array}

If you want to further your knowledge in this area you can try to answer
the following questions. The answers to these questions will require you
to think harder, and possibly look at other sources of information.

1.  Write the code to sort the values in your statistic calculator.

2.  Write a function that calculates the **Mode**, the most frequent
    value.

3.  Get your bubbles to bounce off each other. Hint: have a look at
    SwinGame physics **Collide Circles** procedure.

# Custom Data Types {#cha:more_data_types}

> [Y]{.lettrine}[ou]{.smallcaps} are progressing well! You have learnt
> to structure your spells, guide the flow of magic, and apply it to
> many targets. Now it is time to see how you can also structure the
> components that go into your spells. Fetch the snake and dragon scale,
> now turn your thoughts to their structure. Take your wand and...

Chapter [5](#cha:managing_multiple_values){reference-type="ref"
reference="cha:managing_multiple_values"} introduced the array, allowing
you to store multiple values of the same type in a single variable. This
greatly expanded the ways in which you can work with data in your code,
but there are more tools you can use to model data in your programs.

This chapter will show you how to create your own data types, allowing
you to model the entities[^44] related to your program. This means that
your code can work with more meaningful values, making it easier to
create larger and more complicated programs.

When you have understood the material in this chapter you will be able
to define your own data types to model the entities you want to work
with in your program.

## Custom Data Type Concepts {#sec:data_type_concepts}

To this point, data has been about single values that are either
numbers, text, or Boolean values. These values can be used in s and
stored in s and elements. Now as we move to creating larger and more
complicated programs you need a more effective means of modelling the
data in your code. This chapter introduces concepts that you can use to
more accurately model the data and entities associated with your
program.

This chapter introduces the following **artefacts**:

-   : a kind of type used to store one of a list of available options.

-   : a kind of type used to store multiple fields in a single composite
    value.

-   : a kind of type used to store one value of different possible
    types.

You may need to revise the following programming artefacts:

-   : The idea of storing data within your code.

-   : Allowing you to store multiple values within your code.

-   : Storing data in a or .

-   : Passing data to a Function or Procedure.

The following programming terminology will also be used in this Chapter:

-   : A value used in a statement.

-   : A kind of data used in your code.

The example program for this chapter will be Small DB, a program that
allows the user to enter a number of integer, double, and text values.

![Small DB run from the
Terminal](./topics/type-decl/images/SmallDB.png){#fig:small-db
width="70%"}

### Type (recap) {#sub:type_recap_}

This chapter is all about types, so its is important to have a good
understanding of what a type is. A type is a specification for a class
of data, describing how it is stored and interpreted.

![Type defines the size and interpretation if values in your
code](./topics/type-decl/diagrams/TypeRecap.pdf){#fig:type-recap
width="\\textwidth"}

### Program (with Type Declarations) {#sub:program_with_type_declarations_}

The Program is the overarching artefact, containing the code that
defines the other artefacts that we have been creating. When you want to
declare your own types, you do so in your program's code. This makes it
possible to model the data, in the same way as you model the actions in
your code.

![A Program can contain Type
Declarations](./topics/type-decl/diagrams/ProgramWithTypes.pdf){#fig:type-decl-program
width="\\textwidth"}

### Type Declaration {#sub:type_declaration}

Type declarations allow you to define your own types. Programming
Languages offer a range of different code structures you can use to
build your own types.

![You can declare your own Data
Types](./topics/type-decl/diagrams/TypeDecl.pdf){#fig:type-decl-type-decl
width="90%"}

#### Record {#ssub:record}

A Record (Structure in C) is a composite type whose value is made up of
a number of **fields**. Each field stores a value of a certain type. A
value of the record's type stores data for each field described in the
Record/Structure.

In your code, records can be used to model data associated with the
*things*, the *entities*, associated with your program. For example, a
financial application can have records to store `Account`, `Customer`,
and `Transaction` values. A murder mystery game may have `Player`,
`Clue`, and `Scene` values, where as a Space Invaders game would have
`Player`, `Alien`, and `Bullet` values. Each of these would be modelled
in code using a Record/Structure.

![A Record or Structure contains
Fields](./topics/type-decl/diagrams/Record.pdf){#fig:type-decl-record
width="\\textwidth"}

#### Enumeration {#ssub:enum}

An Enumeration allows you to create a type where the value is one of a
list of available options. When you declare an enumeration you are
listing the values that are available for data of this type. The example
in Figure [6.6](#fig:type-decl-enum){reference-type="ref"
reference="fig:type-decl-enum"} declares a type that can have the value
`ADD_COINS` or `REMOVE_COINS`.

![An Enumeration allows you to define related
constants](./topics/type-decl/diagrams/Enum.pdf){#fig:type-decl-enum
width="\\textwidth"}

#### Union {#ssub:union}

A Union (Variant Record in Pascal) allows you to declare a type where
the values may be one of a range of alternative types. In effect, you
can declare a type where the value may be one of a number of different
types. This is useful if you want to store different types of values at
a location in your program.

A Union often works best by accompanying it with a **tag** value. This
value then records the kind of data currently being stored at the
location in memory. A good option is to use an for the tag's type,
giving you a range of value, that describe the range of types stored.

![A Union is one type that can store one of a range of other
types](./topics/type-decl/diagrams/Union.pdf){#fig:type-decl-union
width="\\textwidth"}

### Declaring Variables (with custom types) {#sub:declaring_variables_with_custom_types_}

The custom types allow you to specify a data format. To make use of this
format you must declare variables that use this type. The type can be
used to declare s and s, allowing you to store values in this format and
pass the around between your functions and procedures.

![Some examples of how you can use your types to declare the kind of
data stored in
variables](./topics/type-decl/diagrams/VariableDeclaration.pdf){#fig:type-decl-variable-decl
width="87%"}

### Assignment Statement (with Fields and Elements) {#sub:assignment_statement_with_fields_and_elements_}

The Assignment Statement allows you to store a value in a Variable or
Array. The righthand side of the assignment is an expression that
calculates the value to be stored. The lefthand side is a variable or
array element, a place into which the value can be stored. With the
addition of the custom types you can now also store values in **fields**
of a record or union.

![You can assign values to a Record or Union's
fields](./topics/type-decl/diagrams/Assignment.pdf){#fig:type-decl-assignment
width="\\textwidth"}

#### Record Assignment {#ssub:record_assignment}

The assignment statement can be used to assign a value to a record's
fields, or to copy an existing record's values.

![You can assign an individual field or the entire record in one
assignment
statement](./topics/type-decl/diagrams/RecordAssign.pdf){#fig:record-assign
width="80%"}

#### Union Assignment {#ssub:union_assignment}

The is similar to a Record in that you can assign values to a union via
its fields or by copying another union value into the variable or array
element. The difference with the Union is that it has only a single
value, with the different fields giving you different interpretations of
that data.

![You can assign an individual field or the entire union in one
assignment
statement](./topics/type-decl/diagrams/UnionAssign.pdf){#fig:union-assign
width="80%"}

### Expression (with custom types) {#sub:expression_with_custom_types}

The types you define allow you to specify how data values can be
formatted, allowing you to declare variables that contain data in this
new format. You can then read data back from your variables in
expressions.

![An expression can read the value of a record's field, a union's field,
and from an
enumeration](./topics/type-decl/diagrams/Expression.pdf){#fig:type-decl-expression
width="\\textwidth"}

#### Record Expressions {#ssub:record_expressions}

A is a type that contains a number of fields. When using a record value
you can use either an individual fields from the record, or the record
in its entirety.

![A field of a record can be used, or the record can be used in its
entirety](./topics/type-decl/diagrams/RecordExpr.pdf){#fig:record-expr
width="80%"}

#### Union Expressions {#ssub:union_expressions}

A has multiple fields that all give access to the same piece of memory.
In effect, the union stores only *one* of the values from its available
fields. This allows you to create a type that can be used to store one
of a selection of available values.

![A field of a union can be used, or the union can be used in its
entirety](./topics/type-decl/diagrams/UnionExpr.pdf){#fig:union-expr
width="87%"}

#### Enumeration Expression {#ssub:enumeration_expression}

The is the simplest of the custom types to make use of. It defines a
list of available options for values of this type. This means that
enumerations are just like standard values.

![You interact with an Enumeration just like other simple data types
(Integers, and Doubles for
example)](./topics/type-decl/diagrams/EnumExpr.pdf){#fig:enum-expr
width="87%"}

## Using Custom Types {#sec:using_custom_types}

Designing your own data types means that your code can work with more
meaningful values. You can design the data that is stored in the program
so that it is organised in ways that will help make the processing
simpler.

### Designing Small DB {#sub:designing_small_db}

Table [6.1](#tbl:small-db-prog){reference-type="ref"
reference="tbl:small-db-prog"} contains a description of the Small DB
program that will be explored in this chapter. This is a small program
that will read, store, and output values entered by the user.

::: {#tbl:small-db-prog}
  **Program Description**   
  ------------------------- -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  **Name**                  *Small DB*
                            
  **Description**           Reads text (up to 7 characters), integer, and double values from the user, storing them within the program and then outputting their values to the Terminal along with the type being stored in the program. Each value will be stored in a row with a unique identifier that will be assigned by the system. The first value will be assigned the id 0, the second will be assigned the id 1, and so on.

  : Description of the Small DB program.
:::

As before, the process to design and implement this program will follow
a number of steps:

1.  Understand the problem, and get some ideas on the tasks that need to
    be performed.

2.  Choose the artefacts we will create and use

3.  Design the control flow for the procedural[^45] artefacts

4.  Map these artefacts to code

5.  Compile and run the program

### Understanding Small DB {#sub:understanding_small_db}

This program does not perform any complex functionality, so it does not
require much analysis to understand the tasks that need to be performed.

Data identified:

-   **Row**: has a unique identifier, and a value.

-   **Column Value**: each value in a row is either an integer, a
    double, or a text value.

Tasks to be performed:

-   **Read Row**: The program needs to be able to read a row from the
    user.

-   **Print Row**: After reading the value the program need to output
    the value to the Terminal.

### Choosing Artefacts for Small DB {#sub:choosing_artefacts_for_small_db}

The process of choosing the artefacts for a program involves determining
both the structure of the data, as well as the structure of the
functions and procedures. In many cases the structure of the data is
more important than the structure of the functionality, as getting the
data right will make the processing easier. Therefore, the first task is
to consider how the data can be structured.

The three main tools that you have for designing the structure of the
data in your program are **records**, **unions**, and **enumerations**.
A record allows you to create a composite data type that is made up of a
number of fields. The union allows you to create a type that stores one
kind of data, or another. Finally the enumeration allows you to create a
list of available options.

#### Looking for records in Small DB {#ssub:looking_for_records_in_small_db}

The most common of these is the **record**, a **struct** in C. This type
allows you to create a single composite value that is composed of a
number of related field values. This can be used to model the *entities*
in your program. When designing with records you think about the things
you want to model, and the data associated with these things.

In the case of the Small DB program there appears to be one kind of
record: the `row`. The program needs to store **row** values, where each
row has a unique id (an integer), and a data value. These two values
*together* make up a row.

This data type will also work nicely with the planned functionality for
the program. The code needs to be able to read and print `row` values.
This means that this code can accept/return `row` values. The
`Print Row` procedure will take in a `row` parameter, and print out its
details to the `Terminal`. The `Read Row` function can read values from
the user and return a `row` value to the caller.

#### Looking for unions in Small DB {#ssub:looking_for_unions_in_small_db}

The **union**[^46] is going to be less common than records, but they can
offer some useful flexibility when designing your code. The union gives
you the ability to have a type that stores one of a selection of types.
If your program requires the ability to store different types at the one
location then a union is a useful way of modelling this.

Reading the description of the Small DB program there does appear to be
the need for a union. Each `row` needs to be able to store *either* a
Integer, a Double, or a text value. Using a union it will be possible to
create a type to model this. This can be called the `Column Value` type,
and will be the union of these three values.

The great thing about a union is that it stores only one value, the one
that you assign to it. This means that it takes only the size needed to
store the largest kind of value. In our case the `Column Value` will
need space to store an Integer (4 bytes), a Double (8 bytes), or 7
Characters (8 bytes, 7 + 1 overhead). This will only need **8 bytes** of
space, as at any one time it can only have one of these values.

#### Looking for enumerations in Small DB {#ssub:looking_for_enumerations_in_small_db}

The last type to look for is the enumeration. This can be used to code a
list of available options. Reading through the description of the
program there is no really obvious list of options, but on further
analysis there may be.

Remember that the Union does not know which value you stored in it. So
you would be able to store a value in a Row, but then you would not know
which value to read back from the union. This is where an enumeration
can come in handy. You can create an enumeration that gives options for
each of the kinds of values that the union can store. In this case the
options can be `INT_VAL`, `DBL_VAL`, and `TXT_VAL`, and can be called
`Data Kind`.

The `Data Kind` enumeration allows you to declare variables that will
have one of the available options as its value. This value will need to
be stored for each `Row` in the program, so a field needs to be added to
the `Row` type. This `kind` field can then store a marker that indicates
the kind if value that is stored in the record.

This is a common pattern you will find for working with unions. It is
called a **tagged union**. The enumeration value is the **tag** and
stores an indicator of the kind of value stored in the union. This is
the model behind the implementation of unions in Pascal, but must be
manually coded in C.

#### Overall data design {#ssub:overall_data_design}

Table [6.2](#tbl:dd-small-db){reference-type="ref"
reference="tbl:dd-small-db"} shows the structures chosen for the data
Small DB program. These provide the data model that will be used by the
code to implement the program's logic. Having planned out the structure
for the data of this program, the next step will be to design its logic.

::: {#tbl:dd-small-db}
  **Data**             **Details**                                      
  -------------------- ------------------------------------------------ --------------------------------------------------------------------------------
                                                                        
  **`Row`**            A **record/struct** with the following fields:   
                       `id`                                             An **Integer** value that stores the unique id.
                       `kind`                                           A **Data Kind** value indicating the type of data being stored in this record.
                       `data`                                           Stores **Column Value** data containing the actual data.
                                                                        
  **`Data Kind`**      A **enumeration** with the following options:    
                       `INT_VAL`                                        Indicates the row is storing an Integer value.
                       `DBL_VAL`                                        Indicates that the row is storing a Double value.
                       `TXT_VAL`                                        Indicates that the row is storing a text value.
                                                                        
  **`Column Value`**   A **union** that stores one of the following:    
                       `Int Val`                                        An Integer value.
                       `Dbl Val`                                        A Double value.
                       `Txt Val`                                        A text value, with up to seven characters.

  : Data Dictionary for Small DB
:::

#### Reading a Row {#ssub:reading_a_row}

The first piece of functionality to implement can be the `Read Row`
function. This function will be responsible for reading a value from the
user, and determining if that value is an integer, double, or string and
then storing it in the row with the correct tag value.

To implement this will require the ability to check if the value in a
string is an integer or a double. These two tasks can be coded into
functions `Is Integer` and `Is Double`. Other than this the remaining
code just needs to copy values into the fields of the `Row` that will be
returned.

`Read Row` will need to accept a single parameter, `Next Id`. This will
be the value assigned to the `id` field of the `Row`, and will be passed
in as this data will be managed elsewhere. At the end of the function,
`Read Row` will return a single `Row` value. As this is a **record**, it
will contain `id`, `kind`, and `data` values.

![Flowchart for
`Read Row`](./topics/type-decl/diagrams/ReadRowFlow.pdf){#fig:read-row-flow
width="80%"}

Figure [6.16](#fig:read-row-flow){reference-type="ref"
reference="fig:read-row-flow"} shows the flowchart for the steps in the
`Read Row` function. Notice that all three fields of the `Row` are
assigned values. The `id` is assigned in the first statement, whereas
the `data` and `kind` fields are assigned values in the branches of the
if statements. All of this data is then returned when the result value
is returned.

The **union** is being used when the `data` field is assigned a value.
When the *integer* branch is taken the union is assigned a value via its
`int val` field. When the *double* branch is taken the union is assigned
a value via its `dbl val` field. When neither of these branches is
taken, the *text* value is assigned to the union via its `txt val`
field.

Finally, one of the options from the **enumeration** is stored in the
`kind` field of the record alongside the union's value. This means that
the `INT_VAL` value is stored in the `kind` field when the *integer*
branch is taken, and the `DBL_VAL` value is stored when the *double*
branch is taken, and the `TXT_VAL` value is stored when the *text* path
is taken.

Figure [6.17](#fig:read-row-data){reference-type="ref"
reference="fig:read-row-data"} shows three examples of the kinds of
values that could be the `result` of this function when it returns. In
each case there are three field values in the row. These are defined in
the `Row` record, and include the `id`, the `kind`, and the `data`.

![Examples of data that could be read into a `Row` value in
`Read Row`](./topics/type-decl/diagrams/RowDataRead.pdf){#fig:read-row-data
width="80%"}

#### Printing a Row {#ssub:printing_a_row}

Having read data into a `Row` it is now possible to output that to the
Terminal. The steps required to do this can be coded into a `Print Row`
procedure. The required steps are shown in the flowchart in
Figure [6.18](#fig:print-row-flow){reference-type="ref"
reference="fig:print-row-flow"}.

The `Print Row` procedure will take a single `Row` parameter. This
parameter will contain the data related of the row that is to be output
to the Terminal. The procedure will output the `id` value and the `data`
values from the `Row`, using the `kind` value to determine which field
to access from the `Column Value` union.

The `row`'s `id` can be output straight away as it is an Integer value.
The actual data that needs to be output depends on the kind of value
that is stored in the `Row`. A can be used to select a path based upon
the value stored in the row's `kind` field. The four paths here cater
for the three options from the `Data Kind` enumeration, plus an
additional path in case the value in `To Print`'s `kind` field does not
match[^47] one of these. This path would indicate a bug in the software,
but should be included just to be safe.

![Flowchart of the steps needed to print a `Row` to the
Terminal](./topics/type-decl/diagrams/PrintRowFlow.pdf){#fig:print-row-flow
width="\\textwidth"}

#### Overview of Small DB's design {#ssub:overview_of_small_db_s_design}

Figure [6.19](#fig:small-db-struct){reference-type="ref"
reference="fig:small-db-struct"} shows the structure chart for the
design of the Small DB program. The functionality is split between the
`Read Row` function and the `Print Row` procedure, with `Is Integer` and
`Is Double` providing useful utility functions to test the data read
from the user.

![Structure chart showing the overview of the Small DB
program](./topics/type-decl/diagrams/SmallDBStruct.pdf){#fig:small-db-struct
width="85%"}

The `Main` procedure will be responsible for storing the data read from
the user in an of `Row` values. The logic in `Main` will then loop over
this array once to read in a value *for each* element of the array,
using `Read Row` to get this value. `Main` with then loop over the array
a second time, this time calling `Print Row` *for each* element in the
array. This will allow `Main` to read in, and then print out, all of the
rows.

A `Show Intro` procedure has also been added to this design to house the
code to show the startup message to the user. This moves this code out
of the `main` procedure allowing it to focus on coordinating the tasks
involved in working with the array of `Row` values.

### Writing the Code for Small DB {#sub:writing_the_code_for_small_db}

The flowcharts and Pseudocode shown communicate the logic that needs to
be coded into the Functions and Procedures of this Program. The
following two sections,
Section [6.3](#sec:custom_types_in_c){reference-type="ref"
reference="sec:custom_types_in_c"} and
Section [6.4](#sec:custom_types_in_pas){reference-type="ref"
reference="sec:custom_types_in_pas"} , contain a description of the
syntax needed to code your own types in the C and Pascal programming
languages. This information can be used to write the code for the Small
DB program, and others.

### Compiling and Running Small DB {#sub:compiling_and_running_small_db}

Remember to code and run this one small piece after the other. For this
you could start by writing the `Print Row` procedure, and pass it values
that you hard code in the program. This will allow you to experiment
with storing different values in the fields of the record and union
without having to deal with the user input and testing functions. You
can test this by checking that the output matches what you expect based
on the values in the `Row`.

Once this is complete the next task would be to work on the `Read Row`
function, and its helpers. These have a bit more logic and will require
that you test it more carefully. Think about the kind of test data that
you can use to check each of the paths through these functions, and use
this to check your code as you progress.

Figure [6.20](#fig:small-db-using){reference-type="ref"
reference="fig:small-db-using"} shows the program in operation.

![Small DB run from the Terminal, repeated from
Figure [6.1](#fig:small-db){reference-type="ref"
reference="fig:small-db"}](./topics/type-decl/images/SmallDB.png){#fig:small-db-using
width="90%"}

## Custom Data Types in C {#sec:custom_types_in_c}

### Implementing Small DB in C {#sub:implementing_small_db_in_c}

Section [6.2](#sec:using_custom_types){reference-type="ref"
reference="sec:using_custom_types"} of this Chapter introduced the Small
DB program. A partial implementation of this program is shown in
Listing [\[lst:c-small-db\]](#lst:c-small-db){reference-type="ref"
reference="lst:c-small-db"}. The type definitions, and code are missing
the details needed to store and display double values. This program
reads a number of rows of data from the user (determined by the
`DB_SIZE` constant). Each row stores a single value, being either a
double value, an integer value, or a text value.

``` {#lst:c-small-db .c caption="C code for the Small DB program" label="lst:c-small-db"}
/* Program: small-db.c */
#include <stdio.h>
#include <strings.h>
#include <stdbool.h>
#include <stdlib.h>
#include <errno.h>
#include <limits.h>

// The number of elements in the row array
#define DB_SIZE 3

// The Column Value union. Stores either an integer, a
// double or 8 (7 + 1) characters.
typedef union {
        int     int_val;
        //todo: Add double as an option
        char    txt_val[8];
    } column_value;

// The Data Kind enumeration indicates the kind of data
// stored in a row, matches the options available in the
// Column Value union.
typedef enum {
        INT_VAL,
        //todo: Add double as an option
        TXT_VAL,
        UNK_VAL // an unknown value
    } data_kind;

// The Row record/structure. Each row contains an id
// a kind, and some data (a Column Value).
typedef struct {
        int         id;
        data_kind   kind;
        column_value   data;
    } row;

// Trim spaces from the start/end of a string (in place)
// This is passed the string to trim, and the number of characters it contains
void trim(char* text, int n)
{
    int i, j;
    int first_non_space = 0;
    
    // Get the position of the last character
    int last_char = strlen(text);
    if (last_char > n) last_char = n;
    
    // Move back one character - past the null terminator
    if (text[last_char] == '\0') last_char--;
    
    // for each character, back from the last char to the first
    for(i = last_char; i >= 0; i--)
    {
        if (text[i] == ' ') text[i] = '\0'; //replace spaces with null
        else break; // found a non-space so break out of this loop
    }
    
    // remember the new position of the last character
    last_char = i;
    
    // Search forward from the start...
    for(first_non_space = 0; first_non_space < last_char; first_non_space++)
    {
        // Break at the first character that is not a space
        if (text[first_non_space] != ' ') break;
    }
    
    if (first_non_space > 0)
    {
        // Need to copy characters back to start of text...
        // j will track from the start of the text
        j = 0; 
        
        // i will track the index of the non-white space characters
        // starting at the first_non_white space and looping
        // until it gets to the last char (include last char so <= not <)
        for(i = first_non_space; i <= last_char; i++)
        {
            text[j] = text[i];
            j++;
        }
        text[j] = '\0'; // add a null terminator to the end
    }
}

// Test if the passed in text refers to an integer
bool is_integer(const char* text)
{
    char * p;
    long val;
    
    // If the text is empty there is no integer
    if (text == NULL || *text == '\0')
      return false;
    
    // Test that it can be converted to an integer
    val = strtol (text, &p, 10); // base 10
    
    // It is an integer if all characters were used in 
    // the conversion, and there was no range error
    // and the result is in the 
    return *p == '\0' && errno != ERANGE && val <= INT_MAX && val >= INT_MIN;    
}

// Test if the passed in text refers to a double
bool is_double(const char* text)
{
    char * p;
    
    // IF the text is empty there is no double
    if (text == NULL || *text == '\0')
      return false;
    
    // Test that it converts to a double
    strtod (text, &p);
    
    // It is a double if the next character in the text 
    // after the conversion is the end of the string
    return *p == '\0';
}

// Clear anything from the input, upto the end of the current line
void clear_input()
{
    scanf("%*[^\n]"); // skip anything is not not a newline
    scanf("%*1[\n]"); // read the newline
}

// Display the intro message.
void show_intro()
{
    printf("%s%s%s%s%d%s%s%s",
        "-----------------------\n",
        "  Welcome to Small DB\n",
        "-----------------------\n",
        "Please enter ", DB_SIZE, " values.\n",
        "They can be text\n",
        "or numbers.\n");
}

// Read a row in from the user and return it. The next_id
// is the id number for the newly created row.
row read_row(int next_id)
{
    char line[16] = "", temp[2];
    row result = {0, UNK_VAL, {0}};
    
    //store the id
    result.id = next_id;
    
    // Read the value from the user into the line
    printf("Enter value: ");
    
    // Read at most 15 characrters up to a new line
    // check if only one of the two inputs is matched
    if (scanf("%15[^\n]%1[\n]", line, temp) != 2) 
    {
        // If the next character was not a newline, read
        // any remaining text and skip it
        clear_input();
    }
    
    // Remove any leading or trailing spaces
    trim(line, 16);
    
    // test int first
    if (is_integer(line))
    {
        // read the integer from the line, and store in row
        sscanf(line, "%d", &result.data.int_val);
        // store the kind in the row
        result.kind = INT_VAL;
    }
    else if (is_double(line)) // test dbl
    {
        // todo: Add handling of double...
    }
    else
    {
        // copy the text into the row (at most 7 + 1 characters)
        strncpy(result.data.txt_val, line, 7); // 7 + 1
        // store the kind in the row
        result.kind = TXT_VAL;
    }
    
    printf("Stored in row with id %d\n", result.id);
    return result;
}

// Print the row to the Terminal
void print_row(row to_print)
{
    // Print the row's id
    printf("Row with id %d: ", to_print.id);
    
    // Branch based on the kind, and output the data
    switch (to_print.kind)
    {
        case INT_VAL:
            printf(" has integer %d\n", to_print.data.int_val);
            break;
        // Add double as an option
        case TXT_VAL:
            printf(" has text '%s'\n", to_print.data.txt_val);
            break;
        default: 
            printf(" has an unknown value\n");
    }
}

// Entry point
int main()
{
    // Create array or row values
    row db_data[DB_SIZE];
    int i;
    
    show_intro();
    
    // For each row in the array
    for (i = 0; i < DB_SIZE; i++)
    {
        // Read the current row's value from the Terminal
        db_data[i] = read_row(i);
    }
    
    // For each row in the array
    for (i = 0; i < DB_SIZE; i++)
    {
        // Print the row to the Terminal
        print_row(db_data[i]);
    }
    
    return 0;
}
```

### C Type Declaration {#sub:c_type_declaration}

In C you can declare your own record/structure, union, and enumeration
types using the `typedef` declaration. It is also possible to create an
alias type declaration, in which you assign a new name to an existing
type.

### C Record/Structure Declaration {#sub:c_structure_declaration}

A record is a type that allows you to store multiple field values. In C
this is implemented using a `struct`. The struct defines a list of
fields and their types. The field declarations are similar to other
variable declarations, with you specifying the type and then the name of
the field.

### C Enumeration Declaration {#sub:c_enum_declaration}

An enumeration is a list of available options for the type. A variable
of an enumeration type can store one of these values. In C you declare
the enumeration using a `typedef`, and list the available constants
within the braces.

### C Union Declaration {#sub:c_union_declaration}

A union declaration is very similar to a record/structure declaration.
The difference is in the way these are represented in memory. The
structure stores a value for each field, whereas the union only stores a
single value, allowing you to choose which of the fields has a value.

### C Variable Declaration (with Types) {#sub:c_variable_declaration_with_types_}

In C you can declare variables from any of the types that you have
declared. The *type name* in the variable's declaration can contain the
names of the types that you declare using C's `typedef` declaration. See
.

## Custom Data Types in Pascal {#sec:custom_types_in_pas}

### Implementing Small DB in Pascal {#sub:implementing_small_db_in_pas}

Section [6.2](#sec:using_custom_types){reference-type="ref"
reference="sec:using_custom_types"} of this Chapter introduced the Small
DB program. A partial implementation of this program is shown in
Listing [\[lst:pas-small-db\]](#lst:pas-small-db){reference-type="ref"
reference="lst:pas-small-db"}. The type definitions, and code are
missing the details needed to store and display double values. This
program reads a number of rows of data from the user (determined by the
`DB_SIZE` constant). Each row stores a single value, being either a
double value, an integer value, or a text value.

``` {#lst:pas-small-db .pascal caption="Pascal code for the Small DB program" label="lst:pas-small-db"}
program SmallDb;
uses SysUtils;

// The number of elements in the row array
const DB_SIZE = 3;

type
    // The Data Kind enumeration indicates the kind of data stored in a row, matches
    // the options available in the Column Value union.
    DataKind = ( INT_VAL, DBL_VAL, TXT_VAL );
    
    ColumnValue = record // The Column Value union.
        case kind: DataKind of
            INT_VAL: ( intVal: Integer;     );  // Stores either an Integer,
            // todo: add double as an option 
            TXT_VAL: ( txtVal: String[7];   );  // 7 characters. (8 bytes)
    end;
    
    Row = record                // The Row record/structure.
        id:     Integer;        // Each row contains an id
        data:   ColumnValue;    // and a single column
    end;

procedure ShowIntro();  // Display the intro message.
begin
    WriteLn('-----------------------');
    WriteLn('  Welcome to Small DB');
    WriteLn('-----------------------');
    WriteLn('Please enter ', DB_SIZE, ' values.');
    WriteLn('They can be text');
    WriteLn('or numbers.');
end;

// Read a row in from the user and return it. 
function ReadRow(nextId: Integer): Row;
var
    line: String = '';
begin
    //store the id
    result.id := nextId;    // The nextId is the id number for the newly created row
    
    Write('Enter value: ');
    ReadLn(line);       // Read the value from the user into the line
    
    // test int first, read the Integer from the line, and store in row
    if TryStrToInt(line, result.data.intVal) then
    begin
        result.data.kind := INT_VAL;    // was an Integer, so set kind to INT_VAL
    end
    else if TryStrToFloat(line, result.data.dblVal) then // test dbl
        //todo: add double as an option...
    else
    begin
        // is not Integer or Double, so its text. Copy the text into the row
        result.data.txtVal := line;
        result.data.kind := TXT_VAL;    // was text, so set kind to TXT_VAL
    end;
    
    WriteLn('Stored in row with id ', result.Id);
end;

// Print the row to the Terminal
procedure PrintRow(toPrint: row);
begin
    WriteLn('Row with id ', toPrint.Id);    // Print the row's id
    
    // Branch based on the kind, and output the data
    case (toPrint.data.kind) of
        INT_VAL:    WriteLn(' has Integer ', toPrint.data.intVal);
        // DBL_VAL: todo: add double as an option
        TXT_VAL:    WriteLn(' has text ', toPrint.data.txtVal);
        else        WriteLn(' has an unknown value ');
    end;
end;

// Entry point
procedure Main();
var
    dbData: array[0..DB_SIZE - 1] of Row;   // Create array or Column Values
    i: Integer;
begin
    ShowIntro();
    
    // For each row in the array
    for i := Low(dbData) to High(dbData) do
        // Read the current row's value from the Terminal
        dbData[i] := ReadRow(i);
    
    // For each row in the array
    for i := Low(dbData) to High(dbData) do
        // Print the row to the Terminal
        PrintRow(dbData[i]);
end;

begin
    Main();
end.
```

### Pascal Type Declaration {#sub:pas_type_declaration}

In Pascal you can declare your own record/structure, union, and
enumeration types using a `type` declaration. It is also possible to
create an alias type declaration, in which you assign a new name to an
existing type.

### Pascal Record Declaration {#sub:pas_structure_declaration}

A record is a type that allows you to store multiple field values. In
Pascal the record defines a list of fields and their types. The field
declarations are similar to other variable declarations, with you
specifying the name and then the type of the field.

#### Pascal Variant Records (Unions) {#ssub:pascal_variant_records_unions_}

Pascal records can include a *variant* part, which stores a single value
from a list of field options. The variant part comes at the end of the
record, starting with the `case` keyword. The variant parts is matched
with a ordinal type (e.g. enumeration) that can also be stored as a
*tag* field, indicating which field option is currently storing a value.
See Listing [\[plst:test-union\]](#plst:test-union){reference-type="ref"
reference="plst:test-union"} for an example.

### Pascal Enumeration Declaration {#sub:pas_enum_declaration}

An enumeration is a list of available options for the type. A variable
of an enumeration type can store one of these values. In Pascal you
declare the enumeration by listing the available constants within
parenthesis.

## Understanding Custom Types {#sec:understanding_custom_types}

Custom data types offer you the opportunity to define how data is
organised in your program. You can create records that contain a number
of fields, unions that store a single field value, and enumerations that
define their own list of values. To help you understand these concepts
better the following illustrations demonstrate how these values are
stored in the variables in your code.

### Understanding `Read Row` {#ssub:understanding_read row}

Section [6.2.1](#sub:designing_small_db){reference-type="ref"
reference="sub:designing_small_db"} described the design of a Small DB
program. The program allows the user to enter some values that were then
stored in variables in the program, making use of records, unions, and
enumerations. The design for this program included a number of functions
and procedures, one of which was the `Read Row` function. This function
was responsible for reading a row value from the user and returning it
in a `Row` record. The flowchart for this function is shown in
Figure [6.21](#fig:read-row-flow-understanding){reference-type="ref"
reference="fig:read-row-flow-understanding"}, which is a repeat of
Figure [6.16](#fig:read-row-flow){reference-type="ref"
reference="fig:read-row-flow"}.

The following illustrations will show this code running to read in a
value from the user. This will demonstrate how the computer stores
values in record, union, and enumeration variables.

The illustrations will show the following:

1.  
2.  
3.  
4.  
5.  
6.  
7.  

![Flowchart for `Read Row`, from
Figure [6.16](#fig:read-row-flow){reference-type="ref"
reference="fig:read-row-flow"}](./topics/type-decl/diagrams/ReadRowFlow.pdf){#fig:read-row-flow-understanding
width="80%"}

#### Code is loaded for Small DB {#ssub:code_is_loaded_for_small_db}

When the program starts its code is loaded into memory and its `main`
procedure is started.

![When the program starts `Main` allocates space for its local
variables, including the
array](./topics/type-decl/images/ReadRow1.pdf){#fig:read-row-vis-1
width="90%"}

#### `Read Row` is called to read in row with id 0 {#ssub:read_row_is_called_to_read_in_row_with_id_0}

The `Read Row` function is called to read a `row` value from the user.
This will check what the user has entered and store an appropriate value
in the `Row` it returns.

![At step 2 `Main` calls `Read Row`, getting it to read in the $i^{th}$
row from the
user](./topics/type-decl/images/ReadRow2.pdf){#fig:read-row-vis-2
width="\\textwidth"}

#### Step 1 stores the value 0 in `result`'s `id` field {#ssub:step_1_stores_the_value_0_in_result_s_id_field}

![Step 1 of `Read Row` stores a value in `result`'s
id](./topics/type-decl/images/ReadRow3.pdf){#fig:read-row-vis-3
width="98%"}

#### A double value is entered by the user {#ssub:a_double_value_is_entered_by_the_user}

![A double value is entered by the user, so the code must store the
double in the
row](./topics/type-decl/images/ReadRow4.pdf){#fig:read-row-vis-4
width="98%"}

#### The double data is stored in the row {#ssub:the_double_data_is_stored_in_the_row}

![A double value is entered by the user, so the code must store the
double in the
row](./topics/type-decl/images/ReadRow5.pdf){#fig:read-row-vis-5
width="\\textwidth"}

#### The `result` row is returned to `Main` {#ssub:the_result_row_is_returned_to_main}

![The `result Row` is returned to
`Main`](./topics/type-decl/images/ReadRow6.pdf){#fig:read-row-vis-6
width="\\textwidth"}

#### This process is repeated for each element of the array {#ssub:this_process_is_repeated_for_each_element_of_the_array}

![The for loop ensures that a `Row` value is read in for each element of
the array](./topics/type-decl/images/ReadRow7.pdf){#fig:read-row-vis-7
width="92%"}

### Understanding `Print Row` {#sub:understanding_print row}

`Print Row` is the other key piece of logic in the Small DB program.
This procedure outputs the values read from the user to the Terminal. It
uses the data stored in the `Row`'s fields to determine how this value
is output, and how the data can be read. The flowchart of this logic is
shown in
Figure [6.29](#fig:print-row-flow-understanding){reference-type="ref"
reference="fig:print-row-flow-understanding"}.

![Flowchart of the steps needed to print a `Row` to the Terminal, from
Figure [6.18](#fig:print-row-flow){reference-type="ref"
reference="fig:print-row-flow"}](./topics/type-decl/diagrams/PrintRowFlow.pdf){#fig:print-row-flow-understanding
width="\\textwidth"}

Within `Main`, `Print Row` is called once for each `Row` in the
`db_data` array. The following illustrations demonstrate the third and
final call to `Print Row`.

The illustrations will show the following:

1.  
2.  

You can use these details to determine how the other data was written to
the Terminal.

#### `Print Row` is called for each element in the array {#ssub:print_row_is_called_for_each_element_in_the_array}

This illustration starts part way through the third call to `Print Row`.
At this stage the first two rows have been output to the Terminal, as
has the `id` of the third `Row`.

![`Print Row` is called for each of the `Row` elements in
`db_data`](./topics/type-decl/images/PrintRow1.pdf){#fig:print-row-vis-1
width="92%"}

#### The text value is output to the Terminal {#ssub:the_text_value_is_output_to_the_terminal}

![`Print Row` is called for each of the `Row` elements in
`db_data`](./topics/type-decl/images/PrintRow2.pdf){#fig:print-row-vis-2
width="91%"}

## Example Custom Types {#sec:example_custom_types}

### Lights {#sub:lights}

This example draws three light bulbs to the screen. These lights can be
clicked to turn them on and off. The code includes the declaration of a
record/structure and an enumeration.

![Example execution of the Lights
program](./topics/type-decl/examples/Lights.png){#fig:lights-img
width="80%"}

### Shape Drawing Program {#sub:shape_drawing_program}

The Shape Drawing program allows the user to create simple shape
drawings using circles, triangles, rectangles, and ellipses.

![Example execution of the Shape Drawing
program](./topics/type-decl/examples/ShapeDrawing.png){#fig:shape-drawing-img
width="\\textwidth"}

## Custom Type Exercises {#sec:custom_type_exercises}

### Concept Questions {#sub:concept_questions_types}

Read over the concepts in this chapter and answer the following
questions:

1.  What is a type?

2.  What is the relationship between a type and a value?

3.  When you create your own type what have you created? A value, or
    something else?

4.  Why would you want to create your own type?

5.  What are the three main kinds of type you can create?

6.  What kind of data type(s) could you create to model the following in
    a program?

    1.  An address book, containing names, phone numbers, and email
        addresses.

    2.  The kind of a '*power up*' in a game, e.g. health pack, upgrade,
        bonus, etc.

    3.  A field that is either an integer, a double, or some text.

    4.  A button that has a location on the screen, a width and height,
        and some text that is drawn on the button.

7.  What is a record? What can it be used to model?

8.  What is an enumeration? What can it be used to model?

9.  What is a union? What can it be used to model?

10. Why is it a good idea to use an enumeration in conjuncture with a
    union?

11. Explain the different ways you can store/read a value when you are
    using a record.

12. Explain the different ways you can store/read a value when you are
    using a enumeration.

13. Explain the different ways you can store/read a value when you are
    using a union.

14. Open one of your SwinGame projects and have a look in the `lib`
    folder. This folder contains a number of source code files used to
    access SwinGame functionality. Have a look in the **Types** file
    (types.c or sgTypes.pas), and examine the follow types. For each
    type write a short description of what it contains.

    1.  Rectangle

    2.  Circle

    3.  LineSegment

    4.  Triangle

    5.  Point 2D

    6.  Vector

### Code Writing Questions: Applying what you have learnt {#sub:code_writing_questions_applying_what_you_have_learnt_types}

Apply what you have learnt to the following tasks:

1.  Create a `Contact` record that stores a person's name (50
    characters), their phone number (20 characters), and email
    address (50) characters.

2.  Use the `Contact` record to create a small address book program that
    lets you enter in the details of four contacts, and then outputs
    these to the Terminal.

3.  Implement the Lights program from
    Section [6.6.1](#sub:lights){reference-type="ref"
    reference="sub:lights"}.

4.  Implement the Shape Drawing program, add the code to create Ellipse
    and Triangle shapes.

5.  Implement the Small DB program, including the support for the double
    data in the row.

### Extension Questions {#sub:extension_questions_types}

If you want to further your knowledge in this area you can try to answer
the following questions. The answers to these questions will require you
to think harder, and possibly look at other sources of information.

1.  Extend the Small DB program so that each '*row*' has three
    '*columns*'.

2.  Explore the sizes of the different data types you have created using
    the `Size Of` function: `sizeof` in C, or `SizeOf` in Pascal.

# Dynamic Memory Allocation {#cha:dynamic_memory_allocation}

> [T]{.lettrine}[here]{.smallcaps} are many places you can draw upon to
> power your spells. So far you have been limited by the constraints of
> this realm. The tools I give you now will let you stretch beyond this
> realm, and will open your mind to even greater powers. You will need
> your orb, your wand, and ...

So far data has been limited by the constraints of the Stack. With the
stack, the compiler must know how much space to allocate to each
variable ahead of time. This means you are limited to working with a
fixed number of values, whether those values are stored in a number of
variables or stored in an array. This constraint is not a problem for
small programs, but most programs will require the flexibility to work
with a variable number of data elements.

This chapter introduces the tools needed to dynamically allocate
additional memory for your program to use. With these tools you will be
able to dynamically allocate additional space for your program to use as
you need it. As memory is finite you will also see how you can release
this memory back to the computer when you no longer require it.

When you have understood the material in this chapter you will be able
to dynamically allocate memory for your program, increasing and
decreasing the number of values that you are storing.

## Dynamic Memory Allocation Concepts {#sec:dynamic_memory_allocation_concepts}

### Heap {#sub:heap}

When your program is executed it allocated memory to work with. This
memory is divided into different areas based on the kind of values that
will be stored there. Previously all of the program's data was housed on
the Stack, dynamically allocated memory is allocated into a separate
area known as the Heap. Any memory that you allocate to your program
will come from this location.

![The Heap is used to store all dynamically allocated
values](./topics/dynamic-memory/diagrams/Heap.pdf){#fig:heap
width="70%"}

#### Allocating memory on the heap {#ssub:allocating_memory_on_the_heap}

Dynamic memory allocation is performed with a couple of operations that
will be provided by the programming language you are using. These
operations allow you to do the following:

-   **Allocate Space**: You ask the Operating System to allocate you
    some space into which you want to store a certain value. The
    Operating System will then allocate you space on the Heap that is
    large enough to store the value you require.

-   **Free Allocation**: When you have finished using a piece of memory
    you have been allocated on the Heap, you can tell the operating
    system that you have finished with this memory, and that it is free
    to allocate this to some other value.

These are the two basic actions that exist for performing dynamic memory
management. Basically, you can ask for memory, and you can give it back.
Once you have been allocated space, that space will be reserved for your
use until you free that allocation. So it is important to remember to
free the memory you have been allocated when you no longer require it.

![You can ask for space, and return the space you were
allocated](./topics/dynamic-memory/diagrams/HeapAlloc.pdf){#fig:heap-alloc
width="75%"}

#### Accessing dynamically allocated memory {#ssub:accessing_dynamically_allocated_memory}

By its very nature, dynamic memory allocation must work a little
differently to the way we have been working with values so far. So far,
when you wanted to work with a value you declared a variable, or an
array. This would have been a , with its value allocated on the stack
along with the other variables you were working with in the current
function or procedure. The variable and its value were closely related,
with the value being located within the variable. With dynamic memory
allocation the values you are allocated are on the heap. This means that
their values are not bound within a variable, but exist entirely outside
of any variables that appear in your code.

One of the challenges of working with dynamically allocated memory is
that you can no longer '*see*' these values in your code. When you were
working with variables, they were in the code, you could see them and
think about the value they stored. With dynamically allocated memory you
do not have this advantage, these values will be allocated as a result
of the operations that are performed while the code is running. This is
why it is called **dynamically** allocated memory. It is *not* memory
allocated to variables, it is **memory allocated upon request**.

This raises one very important question, as illustrated in
Figure [7.3](#fig:heap-access){reference-type="ref"
reference="fig:heap-access"}:

> *If the values exist outside of variables, how do you access them?*

For this we require a new kind of data, a new . This type is used to
store a value that tells you where the data you want can be located. It
is like an address, telling you where the data can be found. This is the
type.

![How can you access these dynamically allocated
values?](./topics/dynamic-memory/diagrams/HeapAccess.pdf){#fig:heap-access
width="70%"}

### Pointer {#sub:pointer}

A Pointer is a new kind of data type, just like Integer, Double, and
Boolean. A Pointer Value is an address, a location in memory where a
value can be found. The name '*Pointer*' is very descriptive, a
*Pointer* points to a value. It tells you, 'The data I refer to is over
there\...'.

![A Pointer Value is the address of a value, in effect it *points* to a
value](./topics/dynamic-memory/diagrams/Pointer.pdf){#fig:pointer
width="\\textwidth"}

#### Using pointer to access the heap {#ssub:using_pointer_to_access_the_heap}

Pointers can be used to point to locations in the heap. When you ask the
Operating System to allocate you space on the heap, it will give you a
pointer value that *points* to the space you were allocated. You can use
this pointer to access the value at that location.

![You can use pointers to access values on the
heap](./topics/dynamic-memory/diagrams/HeapAccess2.pdf){#fig:heap_access_2
width="\\textwidth"}

#### What can a pointer point to? {#ssub:what_can_a_pointer_point_to_}

Pointers store a value that is an address of the value that it points
to. This means that you can point to *any* value in memory, regardless
of where it is. You can have pointer values that point to s, s, s,
elements, fields of s or s. One of its key ability, however, is the
ability to point to values on the .

![A Pointer can point to any value, at any location in
memory](./topics/dynamic-memory/diagrams/PointerPointing.pdf){#fig:pointer-pointing
width="80%"}

#### Where can pointer values be stored? {#ssub:where_can_pointer_values_be_stored_}

A Pointer value is the same as any other value. It can be stored in s,
s, it can be passed to a function in a , it can be returned from a , and
it can also exist on the .

Figure [7.7](#fig:pointer-values){reference-type="ref"
reference="fig:pointer-values"} shows an illustration of some values in
memory. The `start` variable is located somewhere on the stack as a
local variable. This variable is storing a pointer value that points to
a `Node`[^48] that is on the Heap. Each of the nodes on the heap are
also storing pointer values that refer to other values that are also on
the heap.

![Pointers can be stored anywhere a value can be
stored](./topics/dynamic-memory/diagrams/PointerValues.pdf){#fig:pointer-values
width="\\textwidth"}

#### How are pointers used? {#ssub:how_are_pointers_used_}

You need to be able to perform certain actions to make pointers useful.
These include:

-   You must be able to get a pointer to a value. For example, you
    should be able to get a pointer to a value stored in a Variable.

-   Once you have a Pointer value, you must be able to follow that
    pointer to its value so that you can ...

    -   Read the value it points to.

    -   Store a value at the location pointed to.

![You can get pointers to values, and you can follow pointers to
values](./topics/dynamic-memory/diagrams/PointerActions.pdf){#fig:pointer-actions
width="97%"}

#### What is the pointer value? What can you do with it? {#ssub:what_is_the_pointer_value_what_can_you_do_with_it_}

Memory is laid out as a sequence of bytes, into which values can be
stored. The bytes can be thought of as being in a long line, with each
being given numbered based on its position in that line. So the first
byte would be byte 0, the next is byte 1, the next byte 2, and so on.
This number is then the **address** of that byte. So byte 975708474 is
next to byte 975708475, which is next to byte 975708476, etc. This
number is also unique, so there is only one byte 975708474. It is this
number that is the Pointer value, the number of the byte that it is
pointing to.

Figure [7.8](#fig:pointer-actions){reference-type="ref"
reference="fig:pointer-actions"} shows an example of memory used by an
array of three values. Each value is a Double, so each one occupies 8
bytes. If the first is at address 975708474, then the second starts at
address 975708482 (975708474 + 8 bytes). This Figure also shows a
pointer, `p`, that points to this value. That means that `p` has the
value 975708474, being the address of this value, stored within it.

One feature that languages have is called **pointer arithmetic**. When
you add, or subtract, a value from a pointer the compiler will work in
terms of the kinds of values the pointer points to. So in
Figure [7.8](#fig:pointer-actions){reference-type="ref"
reference="fig:pointer-actions"} `p` is a pointer to a Double, this
means that when you add one to p you get the value 975708482, which is 1
**Double** past `p`. Therefore, `p + 2` would be 2 *doubles* past `p`,
at 975708490, and so on.

![The pointer value is the *address* of the value it points
to](./topics/dynamic-memory/diagrams/PointerArithmetic.pdf){#fig:pointer-arithmetic
width="97%"}

### Allocating Memory {#sub:allocating_memory}

With dynamic memory management, one of the tasks you can perform is to
request space from the heap. With this request the Operating System will
locate available space and allocate this to you for use in your code.
The only thing the Operating System really needs to know is how much
space do you require? It can then search for a free space of that size,
allocate this to you, and then give you a pointer to this newly
allocated area of memory.

![When requesting a memory allocation you need to specify the size you
want](./topics/dynamic-memory/diagrams/AllocateMemory-Overview.pdf){#fig:allocate-memory-overview
width="75%"}

#### Explicitly allocating memory for a single value {#ssub:explicitly_allocating_memory}

If you want to store a single value on the heap you can ask to be
allocated enough space for a single value.
Figure [7.11](#fig:allocate-memory){reference-type="ref"
reference="fig:allocate-memory"} shows a Pointer (`p`) that points to an
Integer value. If you want this value to be on the Heap you can ask to
be allocated enough space to store an integer (4 bytes). The Operating
System will then allocate you 4 bytes of space from the Heap, and give
you the address of this space.

![You can ask to be allocated enough space to store one
value](./topics/dynamic-memory/diagrams/AllocateMemory.pdf){#fig:allocate-memory
width="85%"}

#### Explicitly allocating memory for an array {#ssub:explicitly_allocating_memory_for_an_array}

Storing single values on the Heap can be useful, but often you want to
be able to allocate enough space for a number of values. Arrays on the
Stack must be of a fixed length, so this dynamic allocation allows you
to have **variable length arrays**.

![You can ask to be allocated a number of
values](./topics/dynamic-memory/diagrams/AllocateMemory-Array.pdf){#fig:allocate-memory-array
width="75%"}

#### Changing the size of a dynamically allocated array {#ssub:changing_the_size_of_a_dynamically_allocated_array}

The advantage of dynamic memory allocation is that you can change your
allocations. If you asked for an array of two values, you may later want
to be able to expand that array to three or four elements.
Alternatively, an array with twenty elements may have some data removed
and be shrunk down to only 5 elements. All of this is possible with
dynamic memory allocation. You can ask to have the memory you were
allocated changed to a different size.

![You can change the size of the allocation, growing or shrinking the
number of
element](./topics/dynamic-memory/diagrams/AllocateMemory-Array-Resize.pdf){#fig:allocate-memory-array-resize
width="75%"}

### Freeing Memory Allocations {#sub:freeing_memory_allocations}

Dynamic memory allocation requires that you manage the memory you are
allocated yourself. You ask to be allocated memory, and it is your
responsibility to tell the Operating System when you are finished with
that memory. This is one of the main challenges of working with
dynamically allocated memory. You need to take care to ensure that you
do free the memory you have been allocated when you are finished with
it, but at the same time you must make sure that you do not free the
memory while it is still needed.

![You can ask to be allocated enough space to store one
value](./topics/dynamic-memory/diagrams/FreeMem.pdf){#fig:free-memory
width="80%"}

### Issues with Pointers {#sub:issues_with_pointers}

> *With great power comes great responsibility.*

Pointers give you great flexibility in your programs, allowing you to
allocate your program more memory as you need it, and return that
allocation when you are finished with it. Conceptually this seems very
simple, but pointers are a source of many issues in programs.

#### Access Violations {#ssub:access_violations}

The first kind of error you are likely to encounter is caused by trying
to accessing memory that does not exist. This will cause your program to
crash. Figure [7.15](#fig:segfault){reference-type="ref"
reference="fig:segfault"} shows a common example where this occurs.
Trying to follow a pointer to *Nothing* will crash the program with an
access violation. This applies whether you are reading or writing to the
value at the end of the pointer. The common name for this kind of error
is a **segmentation fault**, *segfault* for short.

The only way to avoid these access violations is to **take care** with
your pointers, see
Figure [7.16](#fig:compiler-complaint){reference-type="ref"
reference="fig:compiler-complaint"}. When you start working with
pointers you need to go a little slower, and think a little more
carefully about what it is you are doing. Having a good understanding of
how these dynamic memory allocation tools work is the first step toward
achieving this.

![Trying to follow a pointer that goes nowhere is a runtime
error](./topics/dynamic-memory/diagrams/AccessViolation.pdf){#fig:segfault
width="65%"}

![To avoid access violation, take care with your pointers. From
<http://xkcd.com/371/>](./topics/dynamic-memory/images/compiler_complaint.png){#fig:compiler-complaint
width="\\textwidth"}

#### Memory Leaks {#ssub:memory_leaks}

The next error is one that will not cause your program to crash, but
will consume all of the computers memory if it is allowed to run for an
extended time. Remember that with dynamic memory allocation you are
responsible for releasing the memory back to the system. If you do not
do this there will come a time when there is no memory left to
allocate...Memory leaks are hard to detect, as they do not cause your
program to crash or generate any errors in its calculations. All that
happens is that over time it consumes more and more memory.

Once again, the only way to avoid these issues is to **take care** with
your pointers. You need to make sure that you know where the values are
allocated, and where they are released. There should be reasons why you
would the memory was allocated, and reasons why it is being released.

![If you *forget* a piece of allocated memory, it can never be
freed!](./topics/dynamic-memory/diagrams/MemoryLeak.pdf){#fig:memory_leak
width="80%"}

#### Accessing Released Memory {#ssub:accessing_released_memory}

The next error occurs when you are overly zealous about releasing
memory. You must not release memory before you are finished with it. The
problem occurs when you continue to access a value, after its memory has
been released. This is one of the most difficult problems to locate, as
it will not cause any problems initially.

Take Figure [7.18](#fig:read_unallocated){reference-type="ref"
reference="fig:read_unallocated"} as an example. This demonstrates a
case where two pointers refer to one value. It is possible to free that
value via one pointer, and then forget that the second refers to the
same location. When you read the value from `p2` later, it is *likely*
to still be `10`, so the program will continue to run as normal. The
issue will only appear later when something else is allocated to use
that piece of memory. All of a sudden the value you thought was
allocated to `p2` is now changing apparently on its own. Worst of all,
the actual cause of the bug could be hundred of lines of code away from
where the problem appears. This is what makes this kind of error very
difficult to find.

The solution, once again, is to **take care** with pointers.

![You can still read values from memory even when they are
unallocated\...](./topics/dynamic-memory/diagrams/ReadUnallocated.pdf){#fig:read_unallocated
width="75%"}

### Linked List {#sub:linked_list}

Pointers and dynamic memory allocation make it possible to store values
in new and interesting ways. One way of structuring this data is to
dynamically allocate each value, and link these together using pointers.
An illustration of this is shown in
Figure [7.19](#fig:linked-list){reference-type="ref"
reference="fig:linked-list"}.

Linked lists have the advantage of being very fast to perform insert and
delete actions, when compared with arrays. The disadvantage is an
increase in storage size to keep all the pointers, and the fact you must
loop through the nodes to access any value in the list.

![Illustration of a linked list in
memory](./topics/dynamic-memory/diagrams/LinkedList.pdf){#fig:linked-list
width="90%"}

### Summary of Dynamic Memory Allocation Concepts {#sub:dymanic_memory_summary}

This chapter has introduced a number of concepts related to working with
pointers and performing dynamic memory allocation.

![Memory management focuses on allocating memory, releasing this
allocation, pointers, and the
heap](./topics/dynamic-memory/diagrams/Summary.pdf){#fig:dynamic-memory-summary
width="90%"}

## Using Dynamic Memory Allocation {#sec:using_dynamic_memory_allocation}

Dynamic memory allocation makes it possible for you to allocate
additional space for your program to use from the . We are going to look
at two different examples of how to use dynamic memory allocation, both
of which extend the Small DB program created in
Chapter [6](#cha:more_data_types){reference-type="ref"
reference="cha:more_data_types"}. The first version will use a
dynamically allocated array to allow the program to store a variable
number of rows. Whereas, the second example will dynamically store each
row, and link them together in memory.

### Designing Small DB 2 {#sub:designing_small_db_2}

Table [7.1](#tbl:small-db-2-prog){reference-type="ref"
reference="tbl:small-db-2-prog"} contains the extended description of
the Small DB program that will be explored in this chapter. The main
change is that in Chapter [6](#cha:more_data_types){reference-type="ref"
reference="cha:more_data_types"} the program only read in a fixed number
of values. In this version the program will be able to respond to the
user wanting to add or delete data stored in the program. In effect the
program will store a *variable* number of elements, with elements being
added and removed by the user.

::: {#tbl:small-db-2-prog}
  **Program Description**   
  ------------------------- --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  **Name**                  *Small DB 2*
                            
  **Description**           Stores a number of values for the user. These values can be text (up to 7 characters), a whole number, or a real number. Each value entered is stored in a row with a unique identifier that will be assigned by the system. The first value will be assigned the id 0, the second will be assigned the id 1, and so on.
                            
                            The program will show the user a menu, and allow them to **add** new data to the program, **print** the data in the program, **delete** data from the program, or **quit**.
                            
                            **Add**: will read a value from the user and store it within the program. This data will be allocated a sequential id.
                            
                            **Print**: will print all of the values from the program, along with their types.
                            
                            **Delete**: this will ask the user to enter the id of the data they want to delete, and then search for this data in the program. If a row exists with this id, it is removed from the program.
                            
                            **Quit**: terminates the program. The menu will be repeatedly shown to the user until they decide to quit.

  : Description of the Small DB program.
:::

As before, the process to design and implement this program will follow
a number of steps:

1.  **Analyse** the problem (understand it and associated tasks).

2.  **Design** the solution, its artefacts and control flow.

3.  **Implement** the design in code.

4.  **Test** the solution, compiling and running it to check it works as
    required.

### The Analysis Phase: Understanding Small DB 2 {#sub:understanding_small_db_2}

The first task in any software development is to understand, as fully as
you can, the requirements of the program, and the associated tasks and
data. As this is an extension of the Small DB program from
Chapter [6](#cha:more_data_types){reference-type="ref"
reference="cha:more_data_types"} most of that analysis has already been
done. A number of tasks and data types were identified in the process of
, and .

To successfully implement these new features you must first think about
what is required of the new software. Then you can move on to think
about how you can achieve these goals in your code.

As identified above, the new goal is to allow the user to enter a
variable number of values. They need to be able to add and remove data
as they wish. To achieve this a menu will need to be added that allows
them to choose the action they want to perform next. From the menu the
user will be able to choose to **add**, **print**, or **delete** data or
to quit the program. These tasks give us hints about the kinds of
functions and procedures we can add to achieve this.

As well as thinking about additional tasks, you should also see if there
is additional data. Read over the description and see if you can
identify any additional data that the user may need to either enter into
the program, or get out of it.

At first glance there may not appear to be any new data needed by this
program. After all it is still allowing the user to enter the same
values that it did in
Chapter [6](#cha:more_data_types){reference-type="ref"
reference="cha:more_data_types"}. There is, however, one additional kind
of data that is hinted at in the description. There will be data
associated with the **menu**. The program is likely to need to work with
values associated with the options the user will be selecting. This can
be modelled in the code, making it a better reflect the the concepts
associated with the program.

### The Design Phase: Choosing artefacts and designing control flow {#sub:the_design_process_choosing_artefacts_for_small_db_2_and_designing_control_flow}

Once you have understood what is required, you need to set about
designing the solution. This involves choosing the artefacts to create
and use, and designing the control flow within the functions and
procedures you create. This is all about making decisions, how will you
structure this functionality? What control flow will enable this
behaviour? The many decisions you make will define the overall design of
the software.

This program already has an existing design, that needs to be extended.
The data is described in the *Data Dictionary* in
Table [6.2](#tbl:dd-small-db){reference-type="ref"
reference="tbl:dd-small-db"}. An overview of the functions and procedure
of the solution was shown in the structure chart in
Figure [6.19](#fig:small-db-struct){reference-type="ref"
reference="fig:small-db-struct"}. These are a good starting point, and
for the most part will require no changes. The new additions will build
on top of these.

#### Modelling the menu options {#ssub:modelling_the_menu_options}

Modelling the data should always be high on your priority list, so the
first task can be to think about how the menu options can be modelled.
The user needs to be able to select from **add**, **print**, **delete**,
and **quit**. At later stages there may be more options, but at this
point that is the list.

If you think back to
Chapter [6](#cha:more_data_types){reference-type="ref"
reference="cha:more_data_types"}, there are only three kinds of type you
can use to model the data associated with a program: **records**,
**unions**, and **enumerations**. These are the tools that are available
to you to model your data. A *record* has a number of values one for
each field, a *union* has one value with the type based on the field it
was stored in, a *enumeration* represents a list of options.

An **enumeration** is the obvious choice for modelling the list of
options the user can choose from the menu. We can create a `Menu Option`
type that has the values `ADD_DATA`, `PRINT_DATA`, `DELETE_DATA`, and
`QUIT`.

#### Modelling the dynamic rows {#ssub:modelling_the_dynamic_rows}

One of the first processes that needs to be designed is the process to
`Add a Row` to the data stored in the program. This can use the
`Read Row` function that already exists to get the data from the user,
so its focus is on allocating additional space for the row and
determining the row's id.

A review of the code in the current Small DB program indicates that the
row's index and its id were the same value, and `Main` was taking
advantage of this. When `Main` called `Read Row`, it passed in `i` as
the `id` for the new row. In effect, the value in `i` was being used as
the index that looped over the elements of the array as it was being
populated, and it was keeping track of the `id` value for the newly
created rows. This is no longer going to be the case as the user can now
add and delete rows. When they delete a row the index and the id will no
longer linked, so one variable cannot track both values. A `Next Row Id`
value needs to be kept somewhere.

The `Next Row Id` and the **`Rows`** data are associated. These values
can be modelled as a single `Data Store` **record**, with each
`Data Store` having fields for the `Next Row Id` and `Rows`. This will
keep the relevant information together, and allow the code to work with
these values as a group.

-   `Next Row Id` will be an integer, it will be assigned the value 0 at
    the start and have its value incremented each time a row is added.

-   `Rows` will be a dynamically allocated array of `Row` values.

Table [\[tbl:dd-small-db-2\]](#tbl:dd-small-db-2){reference-type="ref"
reference="tbl:dd-small-db-2"} shows the new data dictionary for Small
DB 2. This includes the addition of the `Menu Option` enumeration and
the `Data Store` record.

::: tabular
\|l\|l\|l\| **Data** &\
\
**`Menu Option`** &\
& `ADD_DATA` & The user wants to add a row.\
& `PRINT_DATA` & The user wants to print all rows.\
& `DELETE_DATA` & The user wants to delete a row.\
& `QUIT` & The user wants to quit.\
\
**`Row`** &\
\
**`Data Kind`** &\
\
**`Column Value`** &\
\
**`Data Store`** &\
& `Next Row Id` & The id value of the next row to be added.\
& `Rows` & A dynamically allocated array of `Row` values.\
& `Row Count` & The number of rows in the `Data Store`. (C Only)\
:::

#### Designing Small DB 2's Structure {#ssub:designing_small_db_2_s_structure}

The next step is to choose the functions and procedures that need to be
created or used to implement this program. Once again, this can build on
the structure implemented in Small DB, as shown in
Figure [6.19](#fig:small-db-struct){reference-type="ref"
reference="fig:small-db-struct"}. This included the code needed to read
a row from the user, and to output the row to the Terminal.

![Structure chart for Small DB
2](./topics/dynamic-memory/diagrams/SmallDB2Structure.pdf){#fig:small-db-2-struct
width="90%"}

Small DB 2 needs to add some additional functionality to this program.
The following list shows the tasks that need to be coded, and the
functions or procedures that will code their behaviour. This is shown in
the Structure Chart in
Figure [7.21](#fig:small-db-2-struct){reference-type="ref"
reference="fig:small-db-2-struct"}.

-   Show the menu to the user, and get the option they want to perform
    (`Get Menu Option` function).

-   Add a row to the data managed in the program (`Add a Row`
    procedure).

-   Print all of the rows (`Print All Rows` procedure).

-   Delete a row from the data managed by the program (`Delete a Row`
    procedure).

In Small DB the logic for `Print all Rows` was coded into `Main`. This
logic can be moved from there into its own procedure. The control flow
for the other functions and procedures will need to be designed.

#### Control flow for `Get Menu Option` {#ssub:control_flow_for_get_menu_option}

The code for `Get Menu Option` should be fairly simple. The basic
actions it needs to perform will be:

1.  Output the text showing the list of options to the Terminal.

2.  Read a number from the user, making sure it is in the range of the
    list of options (1 to 4).

3.  Return the value of the option selected as a `Menu Option` value.

The first part of this process will involve a sequence of output
commands, displaying the different text to the Terminal. The code to
read a number from the user will need a standard *validation* loop,
repeatedly asking them to enter a number until they enter one between 1
and 4. The final part can use a to return the correct result from the
function.

#### Control flow for `Add a Row` {#ssub:control_flow_for_add a row}

The process for adding a row will involve the following steps, as shown
in Listing [\[lst:add_row\]](#lst:add_row){reference-type="ref"
reference="lst:add_row"}:

1.  Record the next row id, and then increase the `Next Row Id` in the
    `Data Store`.

2.  Increase the memory allocated to the `Data Store`'s `Rows` (and the
    Row Count, in C), and store the `Row` read from the user into this
    newly allocated memory.

#### Control flow for `Delete a Row` {#ssub:control_flow_for_}

Deleting an element out of an array is a common task, but one that will
require you to perform all of the hard work. Remember that arrays are
*contiguous blocks of memory*, so technically you cannot delete an
element from the middle of this array. You can only remove the last
element. There are three options for how this can be achieved, as shown
in Figure [7.22](#fig:delete-illustration){reference-type="ref"
reference="fig:delete-illustration"}. All of the options involve moving
data, and then resizing the dynamically allocated array to have one
fewer element than before. The different options involve copying
different pieces of data to free the last slot so that its allocation
can be released.

1.  Only copy the last element over the element to be deleted. This is
    the fastest, but the order of the elements is not preserved. If this
    is important then you cannot use this approach.

2.  The second option is to copy the value of each element in the array
    back over the previous element. This maintains the order of the
    elements, but takes more time as you have to copy all of the values
    after the element being deleted back one spot.

3.  A faster option is to take advantage of the fact the array is stored
    as a contiguous block, and to perform a bulk move/copy of the memory
    past the element. In effect, you can copy all of the elements with
    one request and take advantage of the hardware which is optimised
    for this kind of task.

![Options for deleting an element from an
array](./topics/dynamic-memory/diagrams/DeleteIllustration.pdf){#fig:delete-illustration
width="87%"}

Figure [7.23](#fig:delete-a-row-flow){reference-type="ref"
reference="fig:delete-a-row-flow"} shows the flowchart of the process
for deleting a node from the `Rows` array using Option 2. This option
has been chosen as it helps demonstrate the use of the elements of the
array, as well as working with the dynamic memory.

The implementation of this option also demonstrates a frequently seen
pattern when working with two consecutive elements of an array. In this
case one element is being copied over another, but many algorithms will
require you to work with two elements at a time. Each time through the
loop, this code will access the $i^{th}$ element and its neighbour, the
$(i+1)^{th}$ element.[^49] This means the loop needs to go from a
starting element, to the **second last** element of the array. If `i`
looped to the *last element* of the array then `i+1` would attempt to
access an element past the end of the array.

![Flowchart describing the process for deleting a row with a given
id](./topics/dynamic-memory/diagrams/DeleteFlow.pdf){#fig:delete-a-row-flow
width="70%"}

#### Control flow for `Index of Row with Id` {#ssub:control_flow_for_index of row with id}

The `Index of Row with Id` function will search a `Data Store`'s `Rows`
for the index of the `Row` with the `Id` it is searching for. This is
used by the `Delete a Node` procedure so that it can find the index of
the row it needs to delete.

This is implemented using a standard **search pattern**. This pattern
involves looping over all of the elements in the array, and *for each*
element checking '*Is this the element I am after?*'. When a match is
found the search can end, as can the function. This can be implemented
using the appropriate statement for the language, with the function
returning the index of the row it found, in this case. However, if the
loop gets to the end of the array without finding a match, then there is
no element in the array that matches the search and the function can
return a value that indicates no match was found, in this case the index
`-1` is returned as this cannot be a valid index. The flowchart
illustrating these steps is shown in
Figure [7.24](#fig:index-of-row-flow){reference-type="ref"
reference="fig:index-of-row-flow"}.

![Flowchart describing the process for finding the index of a row with a
given
id](./topics/dynamic-memory/diagrams/IndexOfRowFlow.pdf){#fig:index-of-row-flow
width="65%"}

#### Control flow for `Main` {#ssub:control_flow_for_main}

`Main` is the last remaining procedure. The implementation of this will
require you to declare a `Data Store` variable that will be manipulated
by the other procedures previously discussed. This will need to be
initialised with no elements in its `Rows`, and other a `Next Row Id` of
0.

The control flow of Main will involve a that will repeat code until the
user chooses to quit. Within the loop `Main` can get the option the user
wants to perform by calling the `Get Menu Option` function, can then use
a to run either the `Add a Row` procedure, the `Print all Rows`
procedure, or the `Delete a Row` procedure.

### The Implementation Phase: Writing the code for Small DB 2 {#sub:writing_the_code_for_small_db_2}

The following two sections,
Section [7.3](#sec:dynamic_memory_allocation_in_c){reference-type="ref"
reference="sec:dynamic_memory_allocation_in_c"} and
Section [7.4](#sec:dynamic_memory_allocation_in_pas){reference-type="ref"
reference="sec:dynamic_memory_allocation_in_pas"} , contain a
description of the syntax needed to code dynamic memory allocation in
the C and Pascal programming languages. Use this information to write
the code for Small DB 2, and other programs.

### The Testing Phase: Compiling and running Small DB 2 {#ssub:the_testing_phase_compiling_and_running_small_db_2}

Whenever you work with dynamic memory allocation, you need to spend a
good proportion of your time checking that your solution is working.
This is now an interactive program, so you can test multiple things each
execution. The following are some of the aspects that you should test in
this program:

-   Test the basic functionality:

    -   Can you add new rows?

    -   Can you delete a row?

    -   Can you print the rows?

    -   Are you able to quit the program?

-   Test for potential issues related to memory allocation:

    -   Try deleting the first row, the last row, and a non-existent
        row.

    -   Delete all of the rows.

    -   Print when there are no rows.

    -   Delete and then add, delete all rows then add, etc.

Think about the places where you may have made a mistake, and test to
check that you can not cause the program to crash.

### Designing Linked Small DB 2 {#sub:designing_linked_list}

Arrays are only one way of dynamically allocating space for a program.
In an array the elements are allocated in a contiguous block, each
element next to the previous one. This structure is good when you want
to access an element based on its position, but operations like delete
and insert can be tricky as you need to move elements around to make or
remove space.

An alternative to the array, is to dynamically allocate space for each
value individually, and to use pointers to record their locations. The
general structure is called a **Linked List**, and we can use this to
manage the `Row`s in the linked version of the Small DB 2 program.

Figure [7.25](#fig:array-vs-linked){reference-type="ref"
reference="fig:array-vs-linked"} shows the difference between how the
rows are stored in the previous array version of Small DB 2, and the
linked version we will now explore. In the array version, each `Row` is
stored next to the previous one in an array. With this version you can
use an index value to access the middle elements of the array quickly,
but it is slower to delete or insert elements.

The linked version of Small DB 2 is made up of nodes (the `Row`s) that
each store the data and a *link* (pointer) to the `next` node in the
list. In this version the `Row`s are not stored next to each other, so
you cannot use an index to calculate the position of any one element.
Instead you need to start at the first element, and work your way
through the list one element at a time using the `next` pointer to move
from the current node to the next node of the list. Inserting or
deleting elements of the list can be much faster as the *links* can be
adjusted to introduce new elements, or remove existing one from the
list.

![Illustration of memory layout for Array and Linked versions of Small
DB
2](./topics/dynamic-memory/diagrams/ArrayVsList.pdf){#fig:array-vs-linked
width="85%"}

### The Design Place: Designing Linked Small DB 2 {#sub:the_design_place_designing_linked_small_db_2}

The linked version of the Small DB 2 program is an alternative
implementation for the same program developed in
Section [7.2.1](#sub:designing_small_db_2){reference-type="ref"
reference="sub:designing_small_db_2"}. This means that the information
from the analysis phase is still valid, and can be used to inform what
must be done in this program. Similarly, the testing strategies
developed will also be valid so when the design and implementation are
complete you can test it in the same way as the previous program.

The parts that do need to change will be the **design** and
**implementation**. The design needs to structure the data differently,
and this will impact on the structure of the code as well. The first
step, therefore, is to design the new structure for the data and then to
use this to determine the new structure for the code.

#### Modelling the links in data {#ssub:modelling_the_links_in_data}

Table [\[tbl:dd-linked-small-db-2\]](#tbl:dd-linked-small-db-2){reference-type="ref"
reference="tbl:dd-linked-small-db-2"} shows the new data dictionary for
the linked version of the Small DB 2 program. Most of the changes relate
to the `Data Store` record. This used to store all of the rows in a
dynamically allocated array. Now, in the linked version, the
`Data Store` includes two pointers: one pointing to the first `Row`, the
other pointing to the last `Row`. Only the first of these two is
actually required, but the pointer to the last `Row` will make some
tasks easier.

The `First Row` pointer, points to the first `Row` value allocated on
the heap. Tasks like `Print all Rows` will use this pointer to start
looping through the `Row`s. The `Last Row` pointer exists to make it
easier to add new `Row`s to the `Data Store`. New `Row`s are added to
the end of the list, and the `Last Row` pointer means you can get to the
end of the list without first having to loop through each node.

::: tabular
\|l\|l\|l\| **Data** &\
\
**`Row`** &\
& `Next` & A pointer to the next `Row`.\
\
**`Data Kind`** &\
\
**`Column Value`** &\
\
**`Menu Option`** &\
\
**`Data Store`** &\
& `Next Row Id` & The id value of the next row to be added.\
& `First Row` & A pointer to the first `Row` in the `Data Store`.\
& `Last Row` & A pointer to the last row in the `Data Store`.\
:::

One tricky aspect of the linked `Row`s, is that the `Row` must include a
pointer to a `Row`. In most cases the compiler requires that the thing
you are using is declared before its use. Here that is not possible, the
declaration of the row must include the point to the row which is still
being declared. Each language caters for this in its own way, the tricks
for doing this in C are shown in
Figure [\[clst:row-node\]](#clst:row-node){reference-type="ref"
reference="clst:row-node"}, the Pascal version is shown in
Figure [\[paslst:row-node\]](#paslst:row-node){reference-type="ref"
reference="paslst:row-node"}.

#### Reviewing the structure for the linked version of Small DB 2 {#ssub:reviewing_the_structure_for_the_linked_version_of_small_db_2}

The overall structure for Small DB 2 will not change much, as the basic
actions the program needs to complete remain the same. The only real
change is that the `Index of Row with ID` function is no longer
required, as indexes no longer serve as a means of accessing the `Row`
values in the `DataStore`. The updated version of the Structure Chart is
shown in
Figure [7.26](#fig:linked-small-db-2-struct){reference-type="ref"
reference="fig:linked-small-db-2-struct"}.

![Structure chart for the Linked version of Small DB
2](./topics/dynamic-memory/diagrams/LinkedSmallDB2Structure.pdf){#fig:linked-small-db-2-struct
width="90%"}

#### Adding a Row to the linked version of Small DB 2 {#ssub:adding_a_row_to_the_linked_version_of_small_db_2}

The first activity that can be examined in detail is the `Add a Row`
procedure. This will need to create a new `Row` value, and have it added
to the end of the list. To start with, let us have a look at how this
should work conceptually.

Figure [7.27](#fig:add-node-1){reference-type="ref"
reference="fig:add-node-1"} shows an illustration of the add process for
the first row. At the start the code has a single `Data Store` value,
this will be `Db Data` from within the `Main` procedure. When the
program start the data store will need to have its `First Row` and
`Last Row` fields initialised to point to *Nothing*. This indicates that
there is no first row, or last row in the `Data Store` at this stage.

When a new Row is added the first step is to allocate space for it on
the heap, and then to read the user's input into that space (using
`Read Row`). As this `Row` will be added to the end of the `Data Store`,
its `Next` field can be set to *Nothing* to indicate that there are no
more `Row` values after this one.

![The first Row becomes the start and end of the
List](./topics/dynamic-memory/diagrams/AddNode1.pdf){#fig:add-node-1
width="80%"}

The code for `Add a Row` now needs to link this new `Row` into those
already in the `Data Store`. This is where it checks to see if there is
a `Row` value currently referred to be the `last row` of the
`Data Store` value. As this refers to *Nothing* at this point, the code
takes a branch that sets the `first row` and the `last row` both to
refer to the newly created `Row` value. At this point, the new `Row` is
both the first and the last row in the `Data Store.`

Most the time when you add a new row it will not be the first one in the
list. Typically, you will need to add the row to the existing rows in
memory. This involves updating the current `last row` so that its `next`
points to the newly created row, and then you can change the
`Data Store`'s `last row` to point to it as well. This is shown in
Figure [7.28](#fig:add-node-2){reference-type="ref"
reference="fig:add-node-2"}, with the whole pseudocode shown in
Listing [\[lst:add_linked_row\]](#lst:add_linked_row){reference-type="ref"
reference="lst:add_linked_row"}.

![When a new row is added, it becomes the new last row, and the old last
node points to
it](./topics/dynamic-memory/diagrams/AddNode2.pdf){#fig:add-node-2
width="80%"}

#### Printing all rows in the linked version of Small DB 2 {#ssub:printing_all_rows_in_the_linked_version_of_small_db_2}

The code in `Add a Row` will enable you to create a list of any length.
Each row is linked in by changing the old `last row` to point to the
newly created row, and then the `last row` of the `Data Store` is
updated. This is all very nice, but now that you have the data within
the program how do you use it?

As with an array, the think that you need to determine is how to perform
some action *for each* node in the list. There is no index that you can
use to access the Nodes, so the standard is not going to be of
assistance. Instead, what you need to do is to iterate through the list
by following the `next` pointers in each `Row`.

The standard pseudocode for looping through *each* node in a list is
shown in
Listing [\[lst:iterate-linked-list\]](#lst:iterate-linked-list){reference-type="ref"
reference="lst:iterate-linked-list"}. The main idea is that you can have
a `current` node that you are processing. Then, to get to the next node
you follow `current` pointer and read the `next` field. The result is
then a pointer to the next node in the list and can be stored as the
`current` node. This process can then be repeated **while**
`current is not nothing`. This process is shown in
Figure [7.29](#fig:iterate-list){reference-type="ref"
reference="fig:iterate-list"}.

![When a new row is added, it becomes the new last row, and the old last
node points to
it](./topics/dynamic-memory/diagrams/IterateList.pdf){#fig:iterate-list
width="75%"}

#### Deleting a row in the linked version of Small DB 2 {#ssub:deleting_a_row_in_the_linked_version_of_small_db_2}

Deleting a row in the linked version of Small DB 2 will perform much
faster than the equivalent array version, as it does not need to copy
the array elements. Instead, the deletion of a `Row` is just a matter of
adjusting the links so that the row is no longer included. Though, while
it may be faster it will require more thinking and testing as it
requires some careful work with pointers.

Figure [7.30](#fig:delete-row-list){reference-type="ref"
reference="fig:delete-row-list"} shows an illustration of the actions
that need to be coded into the `Delete a Row` procedure. This code will
need to locate the Row to delete, the previous Row, as well as the next
Row. When these are located the delete code can change the previous
row's `next` field to be a pointer to the Row that follows the row that
is being deleted. With this done the Row has been removed but is still
consuming memory. So the last step of this procedure will need to
release that memory as it is no longer needed.

The code in
Listing [\[clst:delete_row_list\]](#clst:delete_row_list){reference-type="ref"
reference="clst:delete_row_list"} shows the C code for this process,
while the code in
Listing [\[paslst:delete_row_list\]](#paslst:delete_row_list){reference-type="ref"
reference="paslst:delete_row_list"} shows the matching Pascal code. One
key thing to notice is the tracking of the `prev` `Row` pointer along
with the `current`. Effectively this is a standard '*for each node*'
loop, like that used in . In this case, the `prev` pointer remembers the
last value of `current` each time through the loop. This ensures that it
will have a pointer to the previous Row when the desired Row is found.

![When a new row is added, it becomes the new last row, and the old last
node points to
it](./topics/dynamic-memory/diagrams/DeleteNode.pdf){#fig:delete-row-list
width="90%"}

#### Implementing and Testing the linked version of Small DB 2 {#ssub:implementing_and_testing_the_linked_version_of_small_db_2}

This concludes the design for the linked version of Small DB 2. As you
have done in the past you will need to work out how to code this using
the syntax diagrams and examples from hhe following two sections:
Section [7.3](#sec:dynamic_memory_allocation_in_c){reference-type="ref"
reference="sec:dynamic_memory_allocation_in_c"} and
Section [7.4](#sec:dynamic_memory_allocation_in_pas){reference-type="ref"
reference="sec:dynamic_memory_allocation_in_pas"} .

During the implementation process you should also be testing your
solution. With pointers, as with anything, it is best to write a little
and then test it. Use the same testing strategy as discussed in . This
should help you locate any memory issues with this code.

## Dynamic Memory Allocation in C {#sec:dynamic_memory_allocation_in_c}

### Small DB 2, the dynamic array version in C {#sub:small_db_2_the_dynamic_array_version}

Section [7.2](#sec:using_dynamic_memory_allocation){reference-type="ref"
reference="sec:using_dynamic_memory_allocation"}, , introduced a version
of the Small DB program with a dynamic array structure, as opposed to
the fixed array structure used to manage the rows in
Chapter [6](#cha:more_data_types){reference-type="ref"
reference="cha:more_data_types"}. The C code for the altered functions
and procedures is shown in
Listing [\[clst:dynamic-array-db\]](#clst:dynamic-array-db){reference-type="ref"
reference="clst:dynamic-array-db"}, the original version can be found in
Listing [\[lst:c-small-db\]](#lst:c-small-db){reference-type="ref"
reference="lst:c-small-db"}.

``` {#clst:dynamic-array-db .c caption="C code for the dynamic array version of Small DB, see Listing~\\ref{lst:c-small-db} for the original version of this program" label="clst:dynamic-array-db"}
/* Program: small-db-2.c - array version */
#include <stdio.h>
#include <strings.h>
#include <stdbool.h>
#include <stdlib.h>
#include <errno.h>
#include <limits.h>

// =====================
// = Type declarations =
// =====================

typedef union { /* same as original version */ } column_value;
typedef enum { /* same as original version */ } data_kind;
typedef struct { /* same as original version */ } row;

// The data store is a dynamic array of rows, keeping track
// of the number of rows in the array, and the id for the next row
typedef struct {
    int next_row_id;    // The id of the row that will be added next
    int row_count;      // The number of rows in the array
    row *rows;          // A pointer to the rows in memory
} data_store;

// The user can choose to add, delete, or print data or to quit
typedef enum {
    ADD_DATA,
    DELETE_DATA,
    PRINT_DATA,
    QUIT
} menu_option;

// ====================================
// = General Functions and Procedures =
// ====================================
void trim(char* text, int n) { /* same as original version */ }
bool is_integer(const char* text) { /* same as original version */ }
bool is_double(const char* text) { /* same as original version */ }
void clear_input() { /* same as original version */ }

// =====================================
// = Small DB Functions and Procedures =
// =====================================
row read_row(int next_id) { /* same as original version */ }
void print_row(row to_print) { /* same as original version */ }





// Show a menu to the user and get the option they select
menu_option get_menu_option()
{
    int input = 0;
    
    printf("=========================\n");
    printf("| Small DB              |\n");
    printf("=========================\n");
    printf(" 1: Add Data\n");
    printf(" 2: Print Data\n");
    printf(" 3: Delete Data\n");
    printf(" 4: Quit\n");
    printf("=========================\n");
    printf("Choose Option: ");
    
    while(scanf("%d", &input) != 1 || input < 1 || input > 4 )
    {
        clear_input();
        printf("Please enter a value between 1 and 4.\n");
        printf("Choose Option: ");
    }
    // Ensure that input is clear after menu is read.
    clear_input();
    
    switch(input)
    {
        case 1: return ADD_DATA;
        case 2: return PRINT_DATA;
        case 3: return DELETE_DATA;
        case 4: return QUIT;
        default: return QUIT;
    }
}

// Add a row to the data store
void add_a_row(data_store *db_data)
{
    int row_id = 0;
    
    if (db_data == NULL) return;
    
    // Allocate the id
    row_id = db_data->next_row_id;
    db_data->next_row_id++;
    
    // Store the data - using realloc to allocate space for the rows
    db_data->row_count++;
    db_data->rows = (row *) realloc(db_data->rows, sizeof(row) * db_data->row_count);
    db_data->rows[db_data->row_count - 1] = read_row(row_id); // last idx = n - 1
}

// Get the array index of the row with the indicated ID
int idx_of_row_with_id(const data_store *db_data, int row_id)
{
    int i;
    
    if (db_data == NULL) return -1;
    
    // Loop through each element of the array
    for(i = 0; i < db_data->row_count; i++)
    {
        // is this the one we are after?
        if (db_data->rows[i].id == row_id)
        {
            return i; // return the index found.
        }
    }
    
    // Nothing found...
    return -1;
}

// Delete a row from the data store
void delete_a_row(data_store *db_data)
{
    int row_id, i, row_index;
    
    printf("Please enter id of row to delete: ");
    scanf("%d", &row_id);
    
    // Get the index of the row (will test if db_data == NULL)
    row_index = idx_of_row_with_id(db_data, row_id);
    
    if (row_index >= 0) // a row was found to delete...
    {
        // copy all data past the row, back over the row to delete it
        for(i = row_index; i < db_data->row_count - 1; i++)
        {
            // copy data back one spot in the array (one element at a time)
            db_data->rows[i] = db_data->rows[i+1];
        }
        
        // change the row count, and resize the array
        db_data->row_count--;
        db_data->rows = realloc(db_data->rows, sizeof(row) * db_data->row_count);
    }
}

// Print all of the rows from the data store
void print_all_rows(const data_store *db_data)
{
    int i = 0;
    
    if (db_data == NULL) return;
    
    // For each row in the array
    for (i = 0; i < db_data->row_count; i++)
    {
        // Print the row to the Terminal
        print_row(db_data->rows[i]);
    }
}












// ========
// = Main =
// ========

// Entry point
int main()
{
    menu_option opt;
    data_store db_data = {0, 0, NULL}; 
    
    do
    {
        opt = get_menu_option();
        
        switch(opt)
        {
            case ADD_DATA:
                add_a_row(&db_data);
                break;
            case DELETE_DATA:
                delete_a_row(&db_data);
                break;
            case PRINT_DATA:
                print_all_rows(&db_data);
                break;
            case QUIT:
                printf("Bye.\n");
                break;
        }
    } while(opt != QUIT);
}
```

### Small DB 2, the linked version in C {#sub:small_db_2_the_linked_version}

Section [7.2](#sec:using_dynamic_memory_allocation){reference-type="ref"
reference="sec:using_dynamic_memory_allocation"}, , introduced a version
of the Small DB program with a linked structure, as opposed to the array
structure used to manage the rows in
Chapter [6](#cha:more_data_types){reference-type="ref"
reference="cha:more_data_types"}. The C code for the altered functions
and procedures is shown in
Listing [\[clst:linked-db\]](#clst:linked-db){reference-type="ref"
reference="clst:linked-db"}, the original version can be found in
Listing [\[lst:c-small-db\]](#lst:c-small-db){reference-type="ref"
reference="lst:c-small-db"}.

``` {#clst:linked-db .c caption="C code for the linked version of Small DB, see Listing~\\ref{lst:c-small-db} for the array version of this program" label="clst:linked-db"}
/* Program: small-db-2.c */

/* Includes are the same as original version */

// =====================
// = Type declarations =
// =====================

typedef enum { /* same as array version */ } menu_option;
typedef union { /* same as original version */ } column_value;
typedef enum { /* same as original version */ } data_kind;

// The Row record/structure. Each row contains an id
// a kind, and some data (a Column Value).
typedef struct row_struct {
        int                 id;
        data_kind           kind;
        column_value           data;
        struct row_struct   *next;  // Points to the next row
    } row;

// The data store is a dynamic linked list of rows, keeping track
// of the number of rows in the list, and the id for the next row
typedef struct {
        int     next_row_id;    // The id of the row that will be added next
        row     *first_row;      // A pointer to the first row
        row     *last_row;       // A pointer to the last row
    } data_store;

// ====================================
// = General Functions and Procedures =
// ====================================
void trim(char* text, int n) { /* same as original version */ }
bool is_integer(const char* text) { /* same as original version */ }
bool is_double(const char* text) { /* same as original version */ }
void clear_input()  { /* same as original version */ }

// =====================================
// = Small DB Functions and Procedures =
// =====================================

// Read a row in from the user and return it. The next_id
// is the id number for the newly created row.
row read_row(int next_id)
{
    char line[16] = "", temp[2];
    row result = {0, UNK_VAL, {0}, NULL};   // need to initialise next to point to NULL
    
    /* The remainder of this function is the same as the original version */
}

void print_row(row to_print)  { /* same as original version */ }
menu_option get_menu_option()  { /* same as array version */ }

// Add a row to the data store
void add_a_row(data_store *db_data)
{
    int row_id = 0;
    row *new_row;
    
    if (db_data == NULL) return;
    
    // Allocate the id
    row_id = db_data->next_row_id;
    db_data->next_row_id++;
    
    // Allocate space on the heap for the new row
    new_row = (row *)malloc(sizeof(row));
    
    *new_row = read_row(row_id);
    new_row->next = NULL; // there is nothing after this row
    
    if (db_data->last_row == NULL)
    {
        // The data store must be empty, new row is
        // the start and the end.
        db_data->first_row = new_row;
    }
    else
    {
        // The row come after the last row, so change then
        // current last row's next
        db_data->last_row->next = new_row;
    }
    
    // The new row is the last row in the list
    db_data->last_row = new_row;
}

// Delete a row from the data store
void delete_a_row(data_store *db_data)
{
    int row_id;
    row *current, *next, *prev;
    
    if (db_data == NULL) return;
    
    printf("Please enter id of row to delete: ");
    scanf("%d", &row_id);
    
    current = db_data->first_row;
    prev = NULL; // There is no previous for the first row
    
    while(current != NULL && current->id != row_id)
    {
        prev = current;
        current = current->next;
    }
    
    if ( current == NULL ) return; // No row found
    
    next = current->next;
    
    if ( prev == NULL )
    {
        // Deleting the first row, so change the start
        db_data->first_row = next;
    }
    else
    {
        // Skip the row that is about to be deleted
        prev->next = next;
    }
    
    if ( current == db_data->last_row )
    {
        // Last row was deleted, so update the last row of the data store
        db_data->last_row = prev;
    }
    
    // Now free the current row
    free(current);
}

// Print all of the rows from the data store
void print_all_rows(const data_store *db_data)
{
    row *current;
    
    if (db_data == NULL) return;
    
    current = db_data->first_row;       // current is the first row
    
    while(current != NULL)              // While there is a current node
    {
        print_row(*current);            // Print the row to the Terminal
        current = current->next;
    }
}

// ========
// = Main =
// ========

// Entry point
int main()
{
    menu_option opt;
    data_store db_data = {0, NULL, NULL}; // id, first, last
    
    /* The remainder of this function is the same as the array version */
}
```

### C Variable Declaration (with pointers) {#sub:c_variable_declaration_with_pointers_}

In C you can declare pointer variables. This includes s, s, and s.

#### Using pointers to emulate pass by reference in C {#ssub:using_pointers_to_emulate_pass_by_reference_in_c}

C does not have built in support for . Instead, in C you must pass a to
the variable you want passed to the function or procedure.
Listing [\[clst:simple-var-ptr\]](#clst:simple-var-ptr){reference-type="ref"
reference="clst:simple-var-ptr"} shows an examples of procedures that
accept pointers variables.

### C Pointer Operators {#sub:c_pointer_operators}

C provides a number of pointer operators that allow you to get and use
pointers.

::: {#tbl:c-ptr-operators}
  **Name**      **Operator**   **Example**         **Description**
  ------------- -------------- ------------------- ----------------------------------------------------------------
  Address Of    `&`            `&x`                Gets a pointer to the variable/field etc.
  Dereference   `*`            `*ptr`              Follow the pointer, and read the value it points to.
                `->`           `ptr->field_name`   Follow a pointer to a struct or union, and read a field value.
                `[]`           `ptr[2]`            Allows you to access a pointer as if it were an array.

  : C Pointer Operators
:::

You can get a pointer to a value using the ampersand operator (&). This
operator lets you get the address of a variable, field, etc.

### C Type Declarations (with pointers) {#sub:c_type_decl_with_ptrs}

In C you can declare custom types that make use of pointers. This
includes alias types, structs, and unions.

#### C Structure Declarations (with pointer fields) {#ssub:c_structure_declarations_with_pointer_fields_}

The fields of a structure may be a pointer.

#### C Union Declaration (with pointer fields) {#ssub:c_union_declaration_with_pointer_fields_}

The fields of a union may be pointers.

### C Memory Allocation Functions {#sub:c_memory_allocation}

C includes a number of memory allocation functions: , , , .

#### malloc {#ssub:malloc}

`malloc` is the standard memory allocation function. You tell it how
much space you want, and it allocates you that many bytes on the heap.
This is a function, that returns a pointer to the space allocated.

::: {#tbl:malloc}
      **Function Prototype**     
  ------------------------------ -----------------------------------------------
                                 
   `void *malloc(size_t size )`  
                                 
           **Returns**           
             `void *`            A pointer to the allocated space is returned.
          **Parameter**          **Description**
             ` size `            The number of bytes to allocate on the heap.

  : Details of the `malloc` function
:::

#### calloc {#ssub:calloc}

The difference between `calloc` and `malloc` is that `calloc` clears the
memory allocation. When you call `calloc` you pass it a number and a
size, and `calloc` returns you a pointer to a block of memory that is
$number \times size$ bytes.

::: {#tbl:calloc}
            **Function Prototype**            
  ------------------------------------------- -------------------------------------------------------
                                              
   `void *calloc( size_t num, size_t size )`  
                                              
                  **Returns**                 
                   `void *`                   A pointer to the allocated space is returned.
                 **Parameter**                **Description**
                    ` num `                   The number of elements to allocate to the array.
                                              
                   ` size `                   The size of each element to be allocated on the heap.

  : Details of the `calloc` function
:::

#### realloc {#ssub:realloc}

Like `malloc` and `calloc`, `realloc` allows you to allocate space from
the heap. `realloc` allows you to allocate or change (*reallocate*)
space on the heap.

::: {#tbl:realloc}
            **Function Prototype**            
  ------------------------------------------- -------------------------------------------------------
                                              
   `void *realloc( void *ptr, size_t size )`  
                                              
                  **Returns**                 
                   `void *`                   A pointer to the allocated space is returned.
                 **Parameter**                **Description**
                    ` ptr `                   The pointer to *reallocate* space for on the heap.
                                              
                   ` size `                   The size of each element to be allocated on the heap.

  : Details of the `realloc` function
:::

#### free {#ssub:free}

When you allocate memory you are responsible for freeing that memory
when you no longer require it. The `free` function allows you to do
this.

::: {#tbl:free}
   **Procedure Prototype**   
  -------------------------- -----------------------------------------------
                             
   `void free( void *ptr )`  
                             
        **Parameter**        **Description**
           ` ptr `           The pointer to the space to free on the heap.

  : Details of the `free` function
:::

## Dynamic Memory Allocation in Pascal {#sec:dynamic_memory_allocation_in_pas}

### Small DB 2, the dynamic array version in Pascal {#sub:pas-small_db_2_the_dynamic_array_version}

Section [7.2](#sec:using_dynamic_memory_allocation){reference-type="ref"
reference="sec:using_dynamic_memory_allocation"}, , introduced a version
of the Small DB program with a dynamic array structure, as opposed to
the fixed array structure used to manage the rows in
Chapter [6](#cha:more_data_types){reference-type="ref"
reference="cha:more_data_types"}. The Pascal code for the altered
functions and procedures is shown in
Listing [\[plst:dynamic-array-db\]](#plst:dynamic-array-db){reference-type="ref"
reference="plst:dynamic-array-db"}, the original version can be found in
Listing [\[lst:pas-small-db\]](#lst:pas-small-db){reference-type="ref"
reference="lst:pas-small-db"}.

``` {#plst:dynamic-array-db .pascal caption="Pascal code for the dynamic array version of Small DB, see Listing~\\ref{lst:pas-small-db} for the original version of this program" label="plst:dynamic-array-db"}
program SmallDb;
uses SysUtils;

// =====================
// = Type declarations =
// =====================

type 
    DataKind = {same as original version}
    ColumnValue = {same as original version}
    Row = {same as original version}
    
    // The data store is a dynamic linked list of rows, keeping track
    // of the number of rows in the list, and the id for the next row
    DataStore = record
        nextRowId: Integer;         // The id of the row that will be added next
        rows:      array of Row;    // A dynamic array of rows
    end;
    
    MenuOption = ( ADD_DATA, DELETE_DATA, PRINT_DATA, QUIT );


// =====================================
// = Small DB Functions and Procedures =
// =====================================

function ReadRow(nextId: Integer): Row; {same as original version}
procedure PrintRow(toPrint: row); {same as original version}

function GetMenuOption() : MenuOption;
var
    input: Integer = 0;
begin
    WriteLn('=========================');
    WriteLn('| Small DB              |');
    WriteLn('=========================');
    WriteLn(' 1: Add Data');
    WriteLn(' 2: Print Data');
    WriteLn(' 3: Delete Data');
    WriteLn(' 4: Quit');
    WriteLn('=========================');
    
    Write('Choose Option: ');
    ReadLn(input);
    
    while (input < 1) or (input > 4) do
    begin
        WriteLn('Please enter a value between 1 and 4.');
        Write('Choose Option: ');
        ReadLn(input)
    end;
    
    case input of
        1: result := ADD_DATA;
        2: result := PRINT_DATA;
        3: result := DELETE_DATA;
        4: result := QUIT;
        else result := QUIT;
    end;
end;

procedure AddRow(var dbData: DataStore);
var
    rowId: Integer = 0;
begin
    // Allocate the id
    rowId := dbData.nextRowId;
    dbData.nextRowId += 1;
    
    // Store the data
    SetLength(dbData.rows, Length(dbData.rows) + 1);
    dbData.rows[High(dbData.rows)] := ReadRow(rowId);
end;

function IndexOfRowWithID(const dbData: DataStore; rowId: Integer) : Integer;
var
    i: Integer;
begin
    // Loop through each element of the array
    for i := Low(dbData.rows) to High(dbData.rows) do
    begin
        // is this the one we are after?
        if dbData.rows[i].id = rowId then
        begin
            result := i;    // return the index found.
            exit;           // exit the function
        end;
    end;
    
    // Nothing found...
    result := -1;
end;

procedure DeleteRow(var dbData: DataStore);
var
    rowId, i, rowIndex: Integer;
begin
    WriteLn('Please enter id of row to delete: ');
    ReadLn(rowId);
    
    // Get the index of the row
    rowIndex := IndexOfRowWithID(dbData, rowId);
    
    if rowIndex >= 0 then // a row was found to delete...
    begin
        // copy all data past the row, back over the row to delete it
        for i := rowIndex to High(dbData.rows) - 1 do
        begin
            // copy data back one spot in the array (one element at a time)
            dbData.rows[i] := dbData.rows[i+1];
        end;
        
        // resize the array
        SetLength(dbData.rows, Length(dbData.rows) - 1);
    end;
end;

// Print all of the rows from the data store
procedure PrintAllRows(const dbData: DataStore);
var
    i: Integer = 0;
begin
    // For each row in the array
    for i := Low(dbData.rows) to High(dbData.rows) do
    begin
        // Print the row to the Terminal
        PrintRow(dbData.rows[i]);
    end;
end;

// ========
// = Main =
// ========

procedure Main();
var
    opt: MenuOption;
    dbData: DataStore; 
begin
    dbData.nextRowId := 0;
    SetLength(dbData.rows, 0);
    
    repeat
        opt := GetMenuOption();
        
        case opt of
            ADD_DATA:       AddRow(dbData);
            DELETE_DATA:    DeleteRow(dbData);
            PRINT_DATA:     PrintAllRows(dbData);
            QUIT:           WriteLn('Bye.');
        end;
    until opt = QUIT;
end;

begin
    Main();
end.
```

### Small DB 2, the linked version in Pascal {#sub:pas_small_db_2_the_linked_version}

Section [7.2](#sec:using_dynamic_memory_allocation){reference-type="ref"
reference="sec:using_dynamic_memory_allocation"}, , introduced a version
of the Small DB program with a linked structure, as opposed to the array
structure used to manage the rows in
Chapter [6](#cha:more_data_types){reference-type="ref"
reference="cha:more_data_types"}. The Pascal code for the altered
functions and procedures is shown in
Listing [\[plst:linked-db\]](#plst:linked-db){reference-type="ref"
reference="plst:linked-db"}, the original version can be found in
Listing [\[lst:pas-small-db\]](#lst:pas-small-db){reference-type="ref"
reference="lst:pas-small-db"}.

``` {#plst:linked-db .pascal caption="Pascal code for the linked version of Small DB, see Listing~\\ref{plst:dynamic-array-db} for the array version of this program" label="plst:linked-db"}
program SmallDb;
uses SysUtils;

type 
    MenuOption = {same as original version}
    DataKind = {same as original version}
    ColumnValue = {same as original version}
    
    // A Pointer to a row (Row must be in same type decl part)
    RowPtr = ^Row;
    
    // The Row record/structure. Each row contains an id
    // and some data (a Column Value).
    Row = record
        id: Integer;
        data: ColumnValue;
        next: RowPtr;         // The next Row in the list
    end;

    // The data store is a dynamic linked list of rows, keeping track
    // of the number of rows in the list, and the id for the next row
    DataStore = record
        nextRowId:  Integer;    // The id of the row that will be added next
        firstRow:   RowPtr;     // A pointer to the first row
        lastRow:    RowPtr;     // A pointer to the first row
    end;

// Read a row in from the user and return it. 
function ReadRow(nextId: Integer): Row;
var
    line: String = '';
begin
    //store the id
    result.id := nextId;    // The nextId is the id number for the newly created row
    result.next := nil;     // Nothing after this row... at this point
    
    {Remainder is the same as original version...}
end;

procedure PrintRow(toPrint: row); {same as original version}
function GetMenuOption() : MenuOption; {same as dynamic array version}

procedure AddRow(var dbData: DataStore);
var
    rowID: Integer = 0;
    newRow: RowPtr;
begin    
    // Allocate the id
    rowID := dbData.nextRowId;
    dbData.nextRowID += 1;
    
    // Allocate space on the heap for the new row
    New(newRow);
    
    newRow^ := ReadRow(rowID);
    newRow^.next := nil; // there is nothing after this row
    
    if dbData.lastRow = nil then
    begin
        // The data store must be empty, new row is
        // the start and the end.
        dbData.firstRow := newRow;
    end
    else
    begin
        // The row come after the last row, so change then
        // current last row's next
        dbData.lastRow^.next := newRow;
    end;
    
    // The new row is the last row in the list
    dbData.lastRow := newRow;
end;

procedure DeleteRow(var dbData: DataStore);
var
    rowId: Integer;
    current, next, prev: RowPtr;
begin    
    WriteLn('Please enter id of row to delete: ');
    ReadLn(rowId);
    
    current := dbData.firstRow;
    prev := nil; // There is no previous for the first row
    
    while (current <> nil) and (current^.id <> rowId) do
    begin
        prev := current;            // previous, is now current
        current := current^.next;   // current is... one after current
    end;
    
    if current = nil then exit; // No row found
    
    next := current^.next;          // the one after the node to delete
    
    if prev = nil then
    begin
        // Deleting the first row, so change the start
        dbData.firstRow := next;
    end
    else
    begin
        // Skip the row that is about to be deleted
        prev^.next := next; // the one before points to the one after
    end;
    
    if current = dbData.lastRow then
    begin
        // Last row was deleted, so update the last row of the data store
        dbData.lastRow := prev;
    end;
    
    // Now free the current row
    Dispose(current);
end;

// Print all of the rows from the data store
procedure PrintAllRows(const dbData: DataStore);
var
    current: RowPtr;
begin
    current := dbData.firstRow;     // current is the first row
    
    while current <> nil do         // While there is a current node
    begin
        PrintRow(current^);         // Print the row to the Terminal
        current := current^.next;
    end;
end;

// ========
// = Main =
// ========

// Entry point
procedure Main();
var
    opt: MenuOption;
    dbData: DataStore; 
begin
    dbData.nextRowId := 0;
    dbData.firstRow := nil;
    dbData.lastRow := nil;
    
    {Remainder is the same as dynamic array version...}
end;

begin
    Main();
end.
```

### Pascal Variable Declaration (with pointers) {#sub:pas_variable_declaration_with_pointers_}

In Pascal you can declare pointer variables and types. Pointer variables
can be used to declare s and s, but cannot be used as s. To pass a
pointer to a parameter you need to declare your own pointer type and use
that.

### Pascal Pointer Operators {#sub:pas_pointer_operators}

Pascal provides a number of pointer operators that allow you to get and
use pointers.

::: {#tbl:pas-ptr-operators}
  **Name**      **Operator**   **Example**   **Description**
  ------------- -------------- ------------- ------------------------------------------------------
  Address Of    `@`            `@x`          Gets a pointer to the variable/field etc.
  Dereference   `^`            `ptr^`        Follow the pointer, and read the value it points to.

  : C Pointer Operators
:::

You can get a pointer to a value using the *at* operator (@). This
operator lets you get the address of a variable, field, etc.

### Pascal Type Declarations (with pointers) {#sub:pas_type_decl_with_ptrs}

In Pascal you can declare custom types that make use of pointers.

#### Pascal Structure Declarations (with pointer fields) {#ssub:pas_structure_declarations_with_pointer_fields_}

The fields of a structure may be pointers.

### Pascal Memory Allocation Functions {#sub:pas_memory_allocation}

Pascal includes a number of memory allocation functions: , , and .

#### New {#ssub:new}

In Pascal the `New` procedure allocates space for a pointer. The amount
of memory allocated is based on the size of the type referred to by the
pointer, for example an Integer pointer is allocated enough space to
store one integer value.

::: {#tbl:new}
         **Function Prototype**         
  ------------------------------------- ------------------------------------------------------------------------------------------------
                                        
   `procedure New(var ptr: Pointer );`  
                                        
              **Parameter**             **Description**
                 ` ptr `                The pointer to allocate the space for. After the call this will point to the allocated memory.

  : Details of the `New` procedure
:::

#### Dispose {#ssub:dispose}

When you allocate memory you are responsible for freeing that memory
when you no longer require it. The `Dispose` procedure allows you to do
this.

::: {#tbl:dispose}
          **Procedure Prototype**         
  --------------------------------------- -----------------------------------------------
                                          
   `procedure Dispose( ptr : Pointer );`  
                                          
               **Parameter**              **Description**
                  ` ptr `                 The pointer to the space to free on the heap.

  : Details of the `Dispose` procedure
:::

#### Set Length {#ssub:set_length}

Pascal includes support for dynamic arrays. These are arrays where the
contents is stored on the heap, and can be dynamically resized during
execution using the `SetLength` procedure.

::: {#tbl:setlength}
                    **Procedure Prototype**                    
  ------------------------------------------------------------ ----------------------------------------------------------------------------------------
                                                               
   `procedure SetLength( arr : DynamicArray; len: Integer );`  
                                                               
                         **Parameter**                         **Description**
                            ` arr `                            The pointer to the space to free on the heap.
                                                               
                            ` len `                            The new length for the array `arr`, preserving any existing data up to the new length.

  : Details of the `SetLength` procedure
:::

## Exercises for Dynamic Memory Allocation {#sec:exercises_for_dynamic_memory_allocation}

### Concept Questions {#sub:dynamic_memory_concept_questions}

Read over the concepts in this chapter and answer the following
questions:

1.  What is the difference between the heap and the stack?

2.  Why would you want to allocate space on the heap?

3.  How can you allocate space on the heap?

4.  Why do you need to free the space you are allocated? Why do you not
    need to do this with values stored on the stack?

5.  What is a pointer?

6.  What can a pointer point to?

7.  Why do you need pointers to make use of the heap?

8.  Where can pointers be stored?

9.  How can you get a pointer to an existing value?

10. What can you do with the pointer?

11. The pointer has a value, and points to a value. What is the value of
    the pointer? How is this different to the value it points to?

12. What are the different ways you can allocate memory? Describe each,
    and explain what they can be used for.

13. What additional issues are you likely to encounter when working with
    pointers? Explain each, and how you plan to handle these issues.

### Code Reading Questions {#sub:dynamic_memory_code_reading_questions}

Use what you have learnt to read and understand the following code
samples, and answer the associated questions.

1.  Read the code for the dynamic array version of Small DB 2 (for your
    language of choice) and do the following:

    1.  Draw a picture of an empty data store that shows what it looks
        like in memory.

    2.  Draw a new picture showing how the data store will appear after
        one row is added.

    3.  Draw a new picture showing how the data store will appear after
        three rows have been added.

    4.  Explain how the Add Row code is able to add rows to the data
        store.

    5.  Explain how the Delete Row code is able to delete a row from the
        data store. Include a drawing that illustrates the process.

    6.  Explain the steps you would need to perform to add an *Insert
        Row* option for the user.

2.  Read the code for the linked version of Small DB 2 (for your
    language of choice) and do the following:

    1.  Draw a picture of an empty data store that shows what it looks
        like in memory.

    2.  Draw a new picture showing how the data store will appear after
        one row is added.

    3.  Draw a new picture showing how the data store will appear after
        three rows have been added.

    4.  Explain how the Add Row code is able to add rows to the data
        store.

    5.  Explain how the Delete Row code is able to delete a row from the
        data store. Include a drawing that illustrates the process.

    6.  Explain the steps you would need to perform to add an *Insert
        Row* option for the user.

### Code Writing Questions: Applying what you have learnt {#sub:dynamic_memory_code_writing_questions_applying_what_you_have_learnt}

Apply what you have learnt to the following tasks.

1.  Alter your address book program from
    Chapter [6](#cha:more_data_types){reference-type="ref"
    reference="cha:more_data_types"}, so that you can enter any number
    of contacts, and output their details to the Terminal. Use a small
    menu to allow the user to choose between adding a new contact,
    printing contacts, and quitting the program.

2.  Revisit your statistics program from
    Chapter [5](#cha:managing_multiple_values){reference-type="ref"
    reference="cha:managing_multiple_values"}.

    1.  Alter its implementation so that the user can enter a variable
        number of values. The program can start by asking how many
        values the user will enter, and sizing the array appropriately.

    2.  Add a loop and menu to the program so that the user can add more
        values, display statistics, or quit.

3.  Revisit your small-db program from
    Chapter [6](#cha:more_data_types){reference-type="ref"
    reference="cha:more_data_types"}.

    1.  Alter its implementation to introduce a `Data Store` type that
        contains a dynamic number of `row` values.

    2.  Introduce a menu with options to allow the user to add a row,
        delete a row, print all rows, and quit the program.

### Extension Questions {#sub:dynamic_memory_extension_questions}

If you want to further your knowledge in this area you can try to answer
the following questions. The answers to these questions will require you
to think harder, and possibly look at other sources of information.

1.  Try implementing an alternate approach to deleting a row from the
    array version of the small db program.

2.  Alter the `Row` type in your small db program to have a variable
    number of column values (rename).

3.  Compare the dynamic array and linked versions of the Small DB 2
    program. Discuss the relative advantages and disadvantages of each
    approach.

4.  Test the speed difference between the dynamic array and linked
    versions of the Small DB 2 program for the following operations:

    1.  Adding rows (test with adding 10, 100, 1000, and 10000 rows)

    2.  Inserting rows (test with inserting 10, 100, 1000, and 10000
        rows)

    3.  Deleting rows (deleting 10, 100, 1000, and 10000 rows)

# Input and Output {#cha:input_and_output}

> [Y]{.lettrine}[ou]{.smallcaps} are progressing well. You have already
> mastered most of the basics of spell and potion craft, so now we can
> turn our attention to the creation of scrolls. These magical devices
> will allow you to capture the magical energies created in your spells,
> and store them to be retrieved at a later time. Gather your parchment
> and wand, now summon the energies for your spell and ...

Over the previous chapter you have leant to create programs that
manipulate data. So far this data has only existed within the program,
with the values stored being lost when the program terminates. If you
want to be able to maintain these values between executions you need to
learn to save data to file. By saving the program's data to file you can
then load it back in when the program is restarted.

This chapter will introduce the artefacts needed to save data from your
program to file, and to load that data from file. Using this you will be
able to persist data, making it available to future executions of the
program.

When you have understood the material in this chapter you will be able
to save and load data from text and binary files.

## Input and Output Concepts {#sec:input_and_output_concepts}

### Persisting Data {#sub:persisting_data}

When a program is running it uses variables and dynamically allocated
memory to store the data it requires. These values exist within the
memory allocated to the program when it was started. When the program
ends its allocated memory is released, and the values stored within the
program are lost. If values must be *remembered* between executions then
this data must be stored outside of the program. To achieve this you can
save data from the program into files that are stored on the computer's
hard drive or solid state drive (SSD).

![When a program ends its data is gone, unless you save it to
file](./topics/file-io/diagrams/PersistData.pdf){#fig:persist-data
width="\\textwidth"}

### Interacting with Files {#sub:interacting_with_files}

Programming languages offer a number of functions and procedures that
are used to interact with files. These will allow you to save data to a
file, and load data back from the file.

![File operations include the ability to open, read, write, and close
files](./topics/file-io/diagrams/FileOps.pdf){#fig:file-ops
width="\\textwidth"}

### File Formats {#sub:file_formats}

There are two main file formats that you can work with in your program:
binary files and text files. A text file stores its data as textual
characters, whereas a binary file stores the values directly in the
file.

![Files can store *textual* or binary
data](./topics/file-io/diagrams/FileFormats.pdf){#fig:file-formats
width="90%"}

### File Structures {#sub:file_structures}

When thinking about using using files, the one aspect that you need to
spend the most time on will be the organisation of the data in the file.
You need to ensure that the data you save includes sufficient
information that it can be read back into the program at a later stage.
Some common strategies are to:

1.  Write fixed size data blocks where possible.

2.  Store meta data[^50], such as the number or size of variable data
    blocks.

3.  Alternatively, mark the end of variable sized or numbered data
    blocks with a *sentinel* values.

![You need to structure data within the file to make it possible to read
back
successfully.](./topics/file-io/diagrams/FileStructures.pdf){#fig:file-structures
width="90%"}

### Other Devices {#sub:other_devices}

The input/output operations are fairly standardised across different
device types. Saving data to a file is very similar to writing it to the
Terminal or to a network. The skills you learn with any one of these
will be transferable to other devices.

![Writing to other devices also follows similar
patterns](./topics/file-io/diagrams/OtherDevices.pdf){#fig:other-devices
width="90%"}

## Using Input and Output {#sec:using_input_and_output}

Using File Input and Output it is now possible to load and save data. As
an example we will examine how you can go about saving and loading data
in the small db program.

### Saving Data from Small DB {#ssub:saving_data_from_small_db}

The Small DB[^51] program allows the user to enter a number of *row*
values, with each row having a single column that stores a data value
(either an integer, text, or double value). At this stage the program
only keeps its data while it is executing, once it ends the data is
gone. The first step is therefore to save the data from the program into
a file.

#### Row File Format {#ssub:row_file_format}

When thinking about saving data the first task is to try to determine
how the data can be saved so that it can later be read back into the
program. The following information can help us design the structure of
the file saved from the program:

1.  There are a variable number of rows.

2.  Each row has a fixed size, when you know the kind of data it is
    storing.

In the array based version of the Small DB program the number of rows is
stored in the `data store`. Saving this data to file can be achieved by
saving the number of rows before storing the data from each row. This
will mean that when the file is loaded the program can read the number
of rows, and use this information to create enough space for these in
memory before reading them from the file.
Figure [8.6](#fig:row-file-struct){reference-type="ref"
reference="fig:row-file-struct"} shows an example of the file structure
saved from the program.

![The structure of the Small DB data
file](./topics/file-io/diagrams/RowFileStructure.pdf){#fig:row-file-struct
width="\\textwidth"}

#### Saving the Data Store {#ssub:saving_the_data_store}

The pseudocode in
Listing [\[plst:save\]](#plst:save){reference-type="ref"
reference="plst:save"} shows the steps that can be followed to save the
data from a data store into a file. Notice that the number of rows is
being saved into the file, before the row data. This will make it easier
to load the file back into memory.

The `Save` procedure calls a `Write Row to File` procedure to store each
row in the file. The pseudocode for this procedure is shown in
Listing [\[plst:save_row\]](#plst:save_row){reference-type="ref"
reference="plst:save_row"}. This code saves the `id` and `kind` values
to the file, and then uses a to ensure it saved the correct value from
the `row`'s data.

### Loading Data for Small DB {#sub:loading_data_for_small_db}

Once you can save data the logical next step will be to load that data
back into the program. The file structure is set, so all that needs to
be done is to determine the steps that need to be taken in order to load
that data back into memory.

The pseudocode for loading the data store file and reading an individual
row from file are shown in
Listing [\[plst:load\]](#plst:load){reference-type="ref"
reference="plst:load"} and
Listing [\[plst:load_row\]](#plst:load_row){reference-type="ref"
reference="plst:load_row"}. Notice how these mirror the structure of the
save procedures. The `Load` procedure open the file, and then reads the
`Next Row Id` value and `Row Count`. The `Row Count` data is used to
allocate space in memory for the data store's rows, and to determine how
many row values to read from the file.

### New Structure for Small DB {#sub:new_structure_for_small_db}

Figure [8.7](#fig:small_db_3_struct){reference-type="ref"
reference="fig:small_db_3_struct"} shows the new structure chart for
this version of the Small DB program. This shows the new `Load` and
`Save` procedures. When the program starts `Main` will load the data
from file, and then loop through performing the add, delete, and print
actions as the user desires. When the user chooses to quit `Main` will
save its `Data Store` back into the file. In this way the changes the
user makes in the program will be persisted across executions.

![The structure chart showing the functions and procedures in Small
DB](./topics/file-io/diagrams/SmallDB3Structure.pdf){#fig:small_db_3_struct
width="\\textwidth"}

### Writing Code to Load and Save Data for Small DB {#sub:writing_code_to_load_and_save_data_for_small_db}

Having completed the design for this version of the Small DB program,
the next step is to covert these ideas into code. The pseudocode shown
in this section communicate the logic that needs to be coded into the
functions and procedures of the new version of the Small DB program. The
following two sections,
Section [8.3](#sec:file_io_in_c){reference-type="ref"
reference="sec:file_io_in_c"} and
Section [8.4](#sec:file_io_in_pascal){reference-type="ref"
reference="sec:file_io_in_pascal"} , contain a description of the tools
needed to load and save data in the C and Pascal programming languages.

The good approach for implementing these additions will be to write the
code to save the data to file first. Once you have this working you can
run the program and check the file to see that the data you added was
successfully saved. When this is working correctly you can move on to
the code needed to read the values back from file.

Figure [8.8](#fig:small_db_3_running){reference-type="ref"
reference="fig:small_db_3_running"} shows the program running. Notice
that that data remains the same even after quitting and running the
program again.

![Running Small DB program twice, notice the data persists between
executions](./topics/file-io/images/SmallDBRunning.png){#fig:small_db_3_running
width="90%"}

![The contents of Small DB's data file, from
Figure [8.8](#fig:small_db_3_running){reference-type="ref"
reference="fig:small_db_3_running"}](./topics/file-io/images/DataText.png){#fig:small_db_3_data_text
width="30%"}

## Input and Output in C {#sec:file_io_in_c}

### Implementing Small DB File IO in C {#sub:implementing_small_db_file_io_in_c}

Section [8.2](#sec:using_input_and_output){reference-type="ref"
reference="sec:using_input_and_output"} presented an altered version of
the Small DB program from
Chapter [7](#cha:dynamic_memory_allocation){reference-type="ref"
reference="cha:dynamic_memory_allocation"}. The changes introduced four
new procedures used to save the programs data to file, and to reload the
data from file. The new code is presented in
Listing [\[lst:c-small-db3\]](#lst:c-small-db3){reference-type="ref"
reference="lst:c-small-db3"}.

``` {#lst:c-small-db3 .c caption="New C code for the Small DB program with file loading and saving" label="lst:c-small-db3"}
// Previous code for small db remains unchanged...

// ===========================
// = Loading and Saving Data =
// ===========================

void read_row_from_file(row *to_load, FILE *input)
{
    // Load defaults in case loading fails...
    to_load->id = 0;
    to_load->kind = INT_VAL;
    to_load->data.int_val = 0;
    
    // Read in the id and the kind (as an integer)
    fscanf(input, " %d %d ", &to_load->id, (int*)&to_load->kind);
    
    // Branch based on the kind, and output the data
    switch (to_load->kind)
    {
        case INT_VAL:
            // Read in the integer from the file
            fscanf(input, "%d\n", &to_load->data.int_val);
            break;
        // Add double as an option
        case TXT_VAL:
            // Read in the text value from the file (upto 7 characters)
            fscanf(input, "%7[^\n]\n", to_load->data.txt_val); // no & as char array
            break;
        default:
            // Dont know what the value is... set it to 0
            to_load->data.int_val = 0;
    }
}

void load(data_store *db_data, const char *filename)
{
    FILE *input;
    
    input = fopen(filename, "r");
    if ( input == NULL ) return;
    
    int i = 0;
    
    if (db_data != NULL)
    {
        // Store 0 rows as a default, in case load fails
        db_data->row_count = 0;
        
        // Save the row count...
        fscanf(input, "next id:%d ", &(db_data->next_row_id));
        fscanf(input, "rows:%d ", &(db_data->row_count));
        
        // Allocate space for rows...
        db_data->rows = (row *) realloc(db_data->rows, sizeof(row) * db_data->row_count);
        if (db_data->rows == NULL)
        {
            fclose(input);
            return;
        }
        
        // For each row in the array
        for (i = 0; i < db_data->row_count; i++)
        {
            // read the row from the file, into the space allocated for this row
            read_row_from_file(&(db_data->rows[i]), input);
        }        
    }
    
    fclose(input);
    return;
}

void save_row(row to_save, FILE *out)
{
    fprintf(out, "%d %d ", to_save.id, to_save.kind);
    
    // Branch based on the kind, and output the data
    switch (to_save.kind)
    {
        case INT_VAL:
            fprintf(out, "%d\n", to_save.data.int_val);
            break;
        // Add double as an option
        case TXT_VAL:
            fprintf(out, "%s\n", to_save.data.txt_val);
            break;
        default:
            fprintf(out, "\n"); //dont save unknown data
    }
    
}

void save(const data_store *db_data, const char *filename)
{
    FILE *out;
    int i;
    
    out = fopen(filename, "w");
    if ( out == NULL ) return;

    if (db_data != NULL)
    {
        // Save the row count...
        fprintf(out, "next id:%d\n", db_data->next_row_id);
        fprintf(out, "rows:%d\n", db_data->row_count);
        
        // For each row in the array
        for (i = 0; i < db_data->row_count; i++)
        {
            save_row(db_data->rows[i], out);
        }        
    }
    
    fclose(out);
    return;
}

// ========
// = Main =
// ========

// Entry point
int main()
{
    menu_option opt;
    data_store db_data = {0, 0, NULL}; 
    
    load(&db_data, "data.txt");
    
    do
    {
        // Code as before ...
    } while(opt != QUIT);
    
    save(&db_data, "data.txt");
    
    return 0;
}
```

### C File Type {#sub:c_file_type}

C includes a `FILE` type that is used to interact with files on the
computer. This type includes all of the information that C needs to read
and write data to a file.

### C File Functions {#sub:c_file_functions}

There are a number of functions and procedures in the **stdio.h** header
that will give you the ability to read and write data from files.

#### Opening a file {#ssub:opening_a_file}

Before you can interact with a file the first step will be to open the
file. This is done with the `fopen` function. This function will return
a FILE pointer you can then use to interact with the file.

::: tabular
\|c\|p9.5cm\|\
\
\
\
\
`FILE *` & If successful this returns a pointer to the file stream
opened, otherwise it returns NULL.\
**Parameter** & **Description**\
` filename ` & The name of the file to open. This can include a relative
or absolute path to the file.\
&\
` mode ` & Indicates the kind of operations that can be performed on the
file. Mode should be one of the following:\
&

::: {#tbl:fopen}
  **mode**   **description**
  ---------- -------------------------------------------------------------------------------------------------------------------------------
  `"r"`      Open the file for reading. The file must exist.
  `"w"`      Open the file for writing. This will create a new file, or replace an existing file.
  `"a"`      Open the file for appending data. This will append data to an existing file, or create a new file if it does not exist.
  `"r+"`     Open the file for reading, and writing. The file must exist.
  `"w+"`     Open the file for writing, and reading. This will create a new file, replacing any existing files.
  `"a+"`     Open the file for appending, and reading. This ensures that all write operations are always performed at the end of the file.

  : Details of the `fopen` function
:::

\
:::

#### Closing a file {#ssub:closing_a_file}

Once you have opened a file it is important that you also close it. The
`fclose` function can be used to close an opened file.

::: {#tbl:fclose}
      **Function Prototype**     
  ------------------------------ -------------------------------------------
                                 
   `int fclose( FILE *stream )`  
                                 
           **Returns**           
              `int`              Returns 0 when it is successfully closed.
          **Parameter**          **Description**
            ` stream `           The file to close.

  : Details of the `fclose` function
:::

#### Writing text data to file {#ssub:writing_text_data_to_file}

You can use `fprintf` to write data to a `FILE` that has been opened
with write capabilities. This works the same as `printf` and `sprintf`.

::: {#tbl:fprintf}
                   **Function Prototype**                   
  --------------------------------------------------------- --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                                            
   `int fprintf(FILE *destination, const char *format, …)`  
                                                            
                         **Returns**                        
                            `int`                           The number of characters written to the `destination` by `fprintf`.
                        **Parameter**                       **Description**
                       ` destination `                      The FILE to write the output into.
                                                            
                         ` format `                         The text that is to be written to the file. This text may contain format tags to include other values. This is the same as `printf`, see Figure [\[csynt:program-creation-format-string\]](#csynt:program-creation-format-string){reference-type="ref" reference="csynt:program-creation-format-string"} for the syntax of the format tag.
                                                            
                             `…`                            Optional values, must have at least as many values as format tags.

  : Parameters that must be passed to `fprintf`
:::

#### Reading text data from file {#ssub:reading_text_data_from_file}

Reading text data from a file is similar to reading data from the
Terminal or from a string. The `fscanf` function works in the same way
as `printf` and `sprintf`, but writes its data to a text file.

::: {#tbl:fscanf}
                **Function Prototype**                
  --------------------------------------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                                      
   `int fscanf(FILE *source, const char *format, …)`  
                                                      
                      **Returns**                     
                         `int`                        The number of values read by `fscanf`.
                     **Parameter**                    **Description**
                      ` source `                      The file from which the input is read.
                                                      
                      ` format `                      The format specifier describing what is to be read from the Terminal. This is the same as with `scanf`, see Table [3.4](#tbl:format specifiers){reference-type="ref" reference="tbl:format specifiers"}.
                                                      
                          `…`                         The variables into which the values will be read. There must be at least as many variables as format tags in the format specifier.

  : Parameters that must be passed to `fscanf`
:::

#### Writing binary data to file {#ssub:writing_binary_data_to_file}

The `fwrite` function allows you to write binary data to a file. This
requires you to pass a pointer to your data, as well as the size and
number of elements you want written.

::: {#tbl:fwrite}
                               **Function Prototype**                               
  --------------------------------------------------------------------------------- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                                                                    
   `size_t fwrite( const void *ptr, size_t size, size_t count, FILE *destination)`  
                                                                                    
                                     **Returns**                                    
                                        `int`                                       The number of elements written to the `destination` by `fwrite`. If this does not equal the `count` parameter it indicates an error occurred writing the data to the file.
                                    **Parameter**                                   **Description**
                                       ` ptr `                                      A pointer to the data to be saved to the file.
                                                                                    
                                      ` size `                                      The size of each element to be saved.
                                                                                    
                                      ` count `                                     The number of elements to be saved to the file.
                                                                                    
                                   ` destination `                                  The FILE to write the output into.
                                                                                    

  : Parameters that must be passed to `fwrite`
:::

#### Reading binary data from file {#ssub:reading_binary_data_from_file}

To read back binary data you need to use `fread`. This reads back a
block of data from the file, and stores it in memory at a location
indicated by a pointer.

::: {#tbl:fread}
                            **Function Prototype**                           
  -------------------------------------------------------------------------- ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                                                                             
   `size_t fread( void *ptr, size_t size, size_t count, FILE *destination)`  
                                                                             
                                 **Returns**                                 
                                    `int`                                    The number of elements read from the `destination` by `fprintf`. If this does not equal the `count` parameter it indicates an error occurred reading the data from the file.
                                **Parameter**                                **Description**
                                   ` ptr `                                   A pointer to the location to store the loaded data. This must be large enough to store the values loaded.
                                                                             
                                   ` size `                                  The size of each element to be loaded.
                                                                             
                                  ` count `                                  The number of elements to be loaded from the file.
                                                                             
                               ` destination `                               The FILE to read the data from.
                                                                             

  : Parameters that must be passed to `fwrite`
:::

## Input and Output in Pascal {#sec:file_io_in_pascal}

### Implementing Small DB File IO in C {#sub:implementing_small_db_file_io_in_c}

Section [8.2](#sec:using_input_and_output){reference-type="ref"
reference="sec:using_input_and_output"} presented an altered version of
the Small DB program from
Chapter [7](#cha:dynamic_memory_allocation){reference-type="ref"
reference="cha:dynamic_memory_allocation"}. The changes introduced four
new procedures used to save the programs data to file, and to reload the
data from file. The new code is presented in
Listing [\[lst:c-small-db3\]](#lst:c-small-db3){reference-type="ref"
reference="lst:c-small-db3"}.

``` {#lst:p-small-db3 .pascal caption="New Pascal code for the Small DB program with file loading and saving" label="lst:p-small-db3"}
// Previous code for small db remains unchanged...

// ===========================
// = Loading and Saving Data =
// ===========================

procedure ReadRowFromFile(var toLoad: Row; var input: Text);
begin
    // Load defaults in case Loading fails...
    toLoad.id := 0;
    toLoad.data.kind := INT_VAL;
    toLoad.data.intVal := 0;
    
    // Read in the id and the kind (as an integer)
    ReadLn(input, toLoad.id, toLoad.data.kind);
    
    // Branch based on the kind, and output the data
    case toLoad.data.kind of
        INT_VAL: ReadLn(input, toLoad.data.intVal);
        // Add double as an option
        TXT_VAL: ReadLn(input, toLoad.data.txtVal); // no & as char array
        else
            // Dont know what the value is... set it to 0
            toLoad.data.intVal := 0;
    end;
end;

procedure Load(var dbData: DataStore; filename: String);
var
    input: Text;
    i, rowCount: Integer;
    nextIdBuff: String[8]; // A buffer used to skip "Next id:" from the file
    rowsBuff: String[5]; // A buffer used to skip "rows:"
begin
    Assign(input, filename);
    Reset(input);
    
    // Save the row count...
    ReadLn(input, nextIdBuff, dbData.nextRowId); 
    ReadLn(input, rowsBuff, rowCount);
    
    // Allocate space for rows...
    SetLength(dbData.rows, rowCount);
    
    // For each row in the array
    for i := Low(dbData.rows) to High(dbData.rows) do
    begin
        // read the row from the file, into the space allocated for this row
        ReadRowFromFile(dbData.rows[i], input);
    end;        
    
    Close(input);
end;

procedure SaveRow(const toSave: Row; var output: Text);
begin
    WriteLn(output, toSave.id, ' ', toSave.data.kind);
    
    // Branch based on the kind, and output the data
    case toSave.data.kind of
        INT_VAL: WriteLn(output, toSave.data.intVal);
        // Add double as an option
        TXT_VAL: WriteLn(output, toSave.data.txtVal);
        else
            WriteLn(output, ''); //dont save unknown data
    end;
end;

procedure Save(const dbData: DataStore; filename: String);
var
    output: Text;   //Text = textfile
    i:  Integer;
begin
    Assign(output, filename);
    Rewrite(output);
    
    // Save the row count...
    WriteLn(output, 'next id:', dbData.nextRowId);
    WriteLn(output, 'rows:', Length(dbData.rows));
    
    // For each row in the array
    for i := Low(dbData.rows) to High(dbData.rows) do
    begin
        SaveRow(dbData.rows[i], output);
    end;        
    
    Close(output);
end;

procedure Main();
var
    opt: MenuOption;
    dbData: DataStore; 
begin
    dbData.nextRowId := 0;
    SetLength(dbData.rows, 0);
    Load(dbData, 'data.db');
    
    {code as before...}
    
    Save(dbData, 'data.db');
end;

begin
    Main();
end.
```

### Pascal Text Type {#sub:pas_file_type}

Pascal includes `File` and `Text` types that are used to interact with
files on the computer. The `File` type is used for binary files, the
`Text` type is used for text files.

### Pascal File Procedures {#sub:pas_file_functions}

There are a number of functions and procedures in Pascal that will give
you the ability to read and write data from files.

#### Assigning a filename {#ssub:assigning_a_filename}

Before you can interact with a file the first step will be to assign a
filename to the `Text` variable.

::: {#tbl:assign}
                    **Procedure Prototype**                   
  ----------------------------------------------------------- -----------------------------------------------------------------------------------------
                                                              
   `procedure Assign( var fileVar: Text; filename: String )`  
                                                              
                         **Parameter**                        **Description**
                          ` fileVar `                         The file variable to have its filename set
                                                              
                         ` filename `                         The name of the file to open. This can include a relative or absolute path to the file.

  : Details of the `Assign` procedure
:::

#### Opening the file to read {#ssub:opening_the_file_to_read}

The `Reset` procedure is used to reset the file cursor to the start of
the file for reading.

::: {#tbl:reset}
          **Procedure Prototype**          
  ---------------------------------------- -----------------------------------------------------------------
                                           
   `procedure Reset( var fileVar: Text )`  
                                           
               **Parameter**               **Description**
                ` fileVar `                The file to reset, after this call you can read from this file.

  : Details of the `Reset` procedure
:::

#### Opening the file to write {#ssub:opening_the_file_to_write}

You can call either `Rewrite` or `Append` to write to the file. Rewrite
deletes the old file contents, append moves to the end of the file and
adds new data there.

::: {#tbl:reset}
           **Procedure Prototype**           
  ------------------------------------------ -----------------------------------------------------------------------------------------------------------------
                                             
   `procedure Rewrite( var fileVar: Text )`  
                                             
                **Parameter**                **Description**
                 ` fileVar `                 The file to rewrite. After this call you can write data to this file, this will override the existing contents.

  : Details of the `Rewrite` procedure
:::

`Append` can also be used to open a file for write access. This will not
overwrite existing data, keeping the cursor at the end of the existing
file.

::: {#tbl:reset}
           **Procedure Prototype**          
  ----------------------------------------- -----------------------------------------------------------------------------------------------------------------------------------------
                                            
   `procedure Append( var fileVar: Text )`  
                                            
                **Parameter**               **Description**
                 ` fileVar `                The file to append data to. After this call you can write data to this file and it will appear after the existing contents of the file.

  : Details of the `Rewrite` procedure
:::

#### Closing a file {#ssub:closing_a_file}

Once you have opened a file it is important that you also close it. The
`Close` procedure can be used to close an opened file.

::: {#tbl:close}
          **Procedure Prototype**          
  ---------------------------------------- --------------------
                                           
   `procedure Close( var fileVar: Text )`  
                                           
               **Parameter**               **Description**
                ` fileVar `                The file to close.

  : Details of the `Close` procedure
:::

#### Writing text data to file {#ssub:writing_text_data_to_file}

You can use `WriteLn` to write data to a `Text` file that has been
opened with write capabilities.

::: {#tbl:FileWriteLn}
            **Procedure Prototype**           
  ------------------------------------------- -----------------------------------------
                                              
   `procedure WriteLn(destination: Text; …)`  
                                              
                 **Parameter**                **Description**
                ` destination `               The Text file to write the output into.
                                              
                      `…`                     The data to be written

  : Parameters that must be passed to `WriteLn`
:::

#### Reading text data from file {#ssub:reading_text_data_from_file}

Reading text data from a file is similar to reading data from the
Terminal or from a string.

::: {#tbl:File Readln}
         **Procedure Prototype**        
  ------------------------------------- ---------------------------------------------------
                                        
   `procedure ReadLn(source: Text; …)`  
                                        
               **Returns**              
                  `int`                 The number of values read by `fscanf`.
              **Parameter**             **Description**
               ` source `               The Text file from which the input is read.
                                        
                   `…`                  The variables into which the values will be read.

  : Parameters that must be passed to `ReadLn`
:::

## Input and Output Exercises {#sec:input_and_output_exercises}

### Concept Questions {#sub:file-io-concept_questions}

1.  Why would you want to save data to file?

2.  What are the basic operations that you can perform with files?

3.  What is the difference between a text file and a binary file?
    Explain.

4.  What challenges to saving variable sized data to file raise? How can
    these be addressed?

5.  How is reading and writing to files similar to working with the
    Terminal?

### Code Writing Questions: Applying what you have learnt {#sub:code_writing_questions_applying_what_you_have_learnt}

1.  Write two programs that save three integer values to file, one that
    saves it to a text file the other to a binary file. Execute the two
    programs and compare the files created.

2.  Write two programs that read three integer values from file, one
    that reads them from a text file the other from a binary file. Once
    the data is loaded into memory have the program output them to the
    terminal.

3.  Use the code from this chapter to implement saving data for the
    Small DB program.

4.  Revisit your statistics programs and have it save the values the
    user enters to file when the programs ends. Once this is working add
    the code to load the data from the file when the programs starts.

### Extension Questions {#sub:extension_questions}

1.  Explore the other file IO functions and procedures offered by the
    language you are using. See if you can work out how to move the
    cursor within the file (seek to a new location). Use this to move
    back and forth within a file reading individual values in response
    to user input.


[^2]: C++ and Pascal are both *imperative* programming languages. In the
    imperative paradigm a program is seen as a list of commands
    instructing the computer to perform actions.

[^3]: First Generation being Machine Code.

[^4]: Do not just copy and paste it out of the text, type it in yourself
    as this will help you learn the concepts being covered.

[^5]: Do not just copy and paste it out of the text, type it in yourself
    as this will help you learn the concepts being covered.

[^6]: This is literally translated as 'false code'; it looks like code,
    but it is not real code!

[^7]: The ; is used as shorthand to avoid having to list all of the
    characters between 'A' to 'Z'.

[^8]: Most programming languages have the same rules for identifiers.

[^9]: This example moves you into the `c:\Users\username\Documents\Code`
    folder. You need to use the /c/ to refer to the C drive.

[^10]: Expressions follow the standard mathematic order of precedence
    (BODMAS).

[^11]: The remainder after division. For example 9 modulo 3 is 0, 10
    modulo 3 is 1, 11 modulo 3 is 2 etc.

[^12]: This is technically a `char*` which denotes a reference to a
    *string* of characters.

[^13]: C does integer division for int values, rounding the value down.

[^14]: If either, or both, values are real (floating point) numbers the
    result is also a real number.

[^15]: The size in memory is determined by the number of characters
    within the string, and some overhead

[^16]: To concatenate literals you **must** tell the compiler to make
    them strings. This can be done using `string("...")` . See
    Figure [\[csynt:program-creation/typed-literal\]](#csynt:program-creation/typed-literal){reference-type="ref"
    reference="csynt:program-creation/typed-literal"}.

[^17]: Expressions follow the standard mathematic order of precedence
    (BODMAS).

[^18]: `div` performs an integer division, ignoring any remainder.

[^19]: The remainder after division. For example 9 modulo 3 is 0, 10
    modulo 3 is 1, 11 modulo 3 is 2 etc.

[^20]: The `printf` procedure in C and the `WriteLn` procedure in
    Pascal.

[^21]: In the code you will need to calculate these manually using times
    ($2^1$ = 2, $2^2$ = 2\*2, $2^3$ = 2\*2\*2, etc.)

[^22]: This means that the value will be affected by the statements that
    occurred before the expression was calculated.

[^23]: Remember the computer is unintelligent. You cannot rely upon your
    knowledge. Try to think about the information you are using and the
    steps you are performing to make sure you can capture what needs to
    be done in your code.

[^24]: In this design the `Give Change` procedure will be called once
    for each coin.

[^25]: An alternative implementation could have been to code this as a ,
    with the new change value being returned. It would then be the
    responsibility of the caller to assign this into their change
    variable.

[^26]: The **MinGW Shell** on Windows.

[^27]: Except for the percent character which is used in the Format Tag.

[^28]: This will be covered in future chapters.

[^29]: Draw boxes for Parameters, Local Variables, and Function results.

[^30]: Computer programming languages often use floating point values to
    represent real numbers. This format stores an approximation for a
    large range of values. You need to keep this in mind when thinking
    about the kind of data you will use.

[^31]: If you are using C, you will need to do this with C++. With C++
    your compiler is now g++, rather than gcc.

[^32]: Named after George Bool's Boolean logic.

[^33]: The program, and any Functions and Procedures.

[^34]: Dr Evil, Austin Powers, 1997

[^35]: Offset must have a value between 0 and 1 as the value for Hue
    must be between 0 and 1.

[^36]: Trial this with different value \< 1

[^37]: Some languages allow you to start at other values, but many use 0
    based arrays which map more closely to how the data is stored in
    memory.

[^38]: The program, and any Functions and Procedures.

[^39]: Byte values are shown as decimal.

[^40]: Byte values are shown as decimal.

[^41]: Byte values are shown as decimal.

[^42]: Which is passed by reference, as arrays are always passed by
    reference in C.

[^43]: Byte values are shown as decimal.

[^44]: A fancy way of saying the '*things*' associated with your
    program.

[^45]: The Program, and any Functions and Procedures.

[^46]: **Variant record** in Pascal

[^47]: A enumeration is stored as an Integer value, meaning it is
    possible to store other values in here.

[^48]: The `Node` would be a record type declared in the code. This type
    would contain an Integer value field named `data`, and a pointer
    field named `next`.

[^49]: An alternative version of this is to start 1 element past the
    first element you want to interact with, and use the $i^{th}$
    element and the previous element, the $(i-1)^{th}$ element.

[^50]: Meta data is data about your data.

[^51]: In this chapter we will be saving data from the array based
    version of the Small DB program, though a similar approach would be
    taken to save the linked version.
