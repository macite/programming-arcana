---
title: "Build simple sequences"
metaTitle: "Build simple sequences"
metaDescription: "Learn to build programs involving simple sequences"
---

[Sequence](03-build-simple-sequences/01-sequence) is at the heart of all imperative programming languages. This concept underpins how you approach coding, think about programs, and understand how they work. In this chapter, you will see how code runs as a sequence of machine code instructions. Using just a few simple commands, you will create some small visualisations to test out these ideas.

## Focus

Focus on getting some simple programs working and having some fun creating these programs. This chapter introduces procedure calls that allow you to run blocks of code designed for specific tasks. Procedure calls, and procedures themselves, will be expanded on in chapter 6. For now, focus on sequencing these actions, putting them together in a way that gets the computer to do what you want. 

This chapter also includes details on how your program's instructions execute. These give you the low-level details of how the computer works but are not something you need to focus on when programming. These details are likely to be something you explore when studying computer systems, but some basic understanding will help make sense of the concepts covered.

You are likely to need to continue to focus on working with the command line, building and running the programs you created, and dealing with errors that emerge. You want to start to build your confidence that you can use these tools, and deal with the errors that the compiler raises.





The chapter concludes by helping you see how to pull these concepts together to make some small programs. [Sequence in Action](03-build-simple-sequences/07-in-action.md) shows some demo programs together with details on creating [Terminal Programs](03-build-simple-sequences/07-in-action/01-terminal-programs.md), [Drawing Shapes](03-build-simple-sequences/07-in-action/02-drawing.md), [Playing Sound Effects](03-build-simple-sequences/07-in-action/03-sound-effects.md), and [Animation Basics](03-build-simple-sequences/07-in-action/04-animation-basics.md). Following this, [Sequence Demystified and Decoded](02-sequence/3-demistified)) talks you through how to approach using these concepts to design your own programs. Lastly, the [Projects](03-build-simple-sequences/09-projects.md) section provides some example projects you can use to put these ideas into practice.


:::note Learning Outcomes
At the end of **Build Simple Sequences** you should be able to:

- Design, code, compile and run programs involving sequences of instructions.
- Describe the central role of sequence in programming.
- Describe how data can be stored, used, and manipulated in programs.
- Describe the sequence of actions involved in:
  - calling a procedure.
  - assigning a value to a variable.
- Write code that includes:
  - sequences of instructions.
  - variables to store values you want to manipulate.
  - procedures to group instructions into reusable packages.
- Identify expressions within programming statements.
- Identify and distinguish between:
  - variable declarations and use.
  - procedure declarations and use.
- Read and use syntax diagrams to write valid programming statements.

:::

## Concepts

### Sequence

:::note Idea

Computers operate by performing a **sequence** of actions determined by **instructions** that it loads from memory. When one instruction finishes, the computer reads the next instruction from memory and performs its action. This process repeats over and over as the computer runs.

:::

There are two important things to take away from this simple concept:

1. Programs run their instructions in sequence.
2. Each kind of statement involves its own sequence.

#### Programs run their instructions in sequence

Programs are a list of instructions (statements). When you run a program, it will execute its code one instruction at a time.

The following picture shows the code for a small program that draws some shapes to the screen.

![An example program that draws shapes to the screen.](01-sequence/slides/slide-006.jpeg)

When you run this program, it will:

1. Open a window
2. Clear that window to white
3. Draw a filled ellipse in bright green
4. Draw a filled rectangle in gray
5. Draw a filled triangle
6. Refresh what is on the screen in the window
7. Delay for 5 seconds (5000 milliseconds)
8. End

Running this program will show you the following picture. Drawing to a window in this way uses a **painter's model**, meaning that things will layer on top of each other. Shapes drawn first will appear in the background, with anything drawn later appearing on top. Notice that the rectangle appears on top of the green ellipse (which we can only see part of).

![Output drawn by the example program.](01-sequence/SceneDrawing.png)

:::tip Think...

- What would happen if you:

  - reversed the order of the drawing instructions?
  - removed the delay from this program?
  - added more drawing instructions after the delay?

- Given the painter's model, is there a reusable sequence you can identify in this code?

:::

When designing programs, sequence will be essential to your approach. You need to be able to *sequence* the instructions you want the computer to execute in a way that gets the output you want. Remember, the computer is unintelligent. So your instructions need to be unambiguous, something a machine can perform.  Try to think through things slowly and carefully, in defined steps that run one after the other.

:::note

- Each programming language will have a set starting point. In the C/C++ language, this is `main`. When you run your program, your code will start with `main`.
- Semicolons (;) terminal each of your statements in C/C++ code. Each statement will run its steps before the computer moves to run the next statement.

:::

#### Each kind of statement involves its own sequence

When you are programming, you are usually focusing on the sequence of the steps in your program. However, when learning to program, one of the key things to understand is that each statement in your code will become more than one machine code instruction for the computer to run. To understand how programs work, you need to understand the steps involved in each of the different kinds of statements in the programming language.

![Some example programming language statements used to illustrate the sequence of steps involved in running each step of our code.](01-sequence/slides/slide-016.jpeg)

When you compile your code, the compiler reads the statements in your code and converts these to machine code. Each of the different statements has its own set of actions that it performs.

:::note

- Most programming languges support the same kinds of statements, which usually work in basically the same way.
- When learning a new kind of statement, focus on the sequence of actions it performs alongside its purpose and usage.
- Learn more about the low level details of how computers work when you study computer systems. [Elements of Computer Systems](https://www.nand2tetris.org) provides a great overview.

:::

### Program

In most software projects the top level *artefact* is a **program**, the program you are working to develop. When you create a program think about the tasks you want the program to achieve. You need to break these tasks down into the sequence of steps the computer needs to perform to achieve this.

![A program contains instructions that command the computer to perform
actions](./topics/program-creation/diagrams/BasicProgramConcept.pdf){#fig:program-creation-program
width="\\textwidth"}

C/C++ does not have an explicit program artefact. Rather, you create a program by having a function called `main` in your code. The following code shows the code you need to build a program. The code includes `main`, which defines where the program will start when executed.

```cpp
int main()
{
  // the program's instructions go here...

  return 0;
}

```

:::note

- A program is a file that can be loaded and run by the computer's operating system.
- Each program has an **entry** point defining the first instruction to be run.
- When run, the computer will execute the instructions in sequence from its first instruction.
- The program terminates when it reaches a defined endpoint, or is terminated by the operating system due to an error.
- In C/C++:
  - The compiler will create a program for you.
  - Execution will start with `main`, and run the sequence of instructions that it contains.
:::

### Procedure

The computer is unintelligent; performing anything meaningful requires many instructions. Coding all of these directly in the program would be slow and time-consuming. To avoid this, programming languages offer the capability to group instructions into **procedures**. We will look at procedures in more details in [Chapter 6](), for now we can look at the basics and expand on this later.

A procedure is a named list of instructions that is run when the procedure is called. Procedures could include any arbitrary set of instructions, but best practice is to use procedures to encapsulate the instructions needed to perform a defined task. The procedure's name should reflect the task that it performs. This helps make program code easier to read and understand.

![A procedure contains instructions to perform a task, and may need to
be passed data in order to do
this](./topics/program-creation/diagrams/Procedure.pdf){#fig:program-creation-procedure
width="90%"}

:::note

- A procedure has:
  - a name that is used to identify it.
  - a list of instructions that are run when the procedure is called.
- Procedures should perform an identifiable task, and the name should tell you what that task is.
- When you call a procedure, you will also need to pass along data that the procedure will use when it runs.

:::

### Procedure Call

A procedure call instructs the computer to run the code from a procedure. This statement uses the procedure's name to identify the procedure to run, and passes along the data that the procedure needs to function.

When a procedure call is run, the computer moves to the instructions defined within the procedure. The instructions within the procedure execute in sequence, and when the last instruction is run the computer returns back to the instructions where the procedure call occurred.

When programming, think of each procedure call as a request asking the computer to perform the identified task. For example, there is an `open_window` procedure in SplashKit that opens a window. When calling this, I would think of it as if I were asking someone to perform the task for me. When that task is finished, I would move on to the next instruction.

![A procedure calls runs a procedure, passing in values for the
procedure to
use](./topics/program-creation/diagrams/ProcedureCall.pdf){#fig:program-creation-procedure call
width="\\textwidth"}

The following code shows the shape drawing program. It includes seven procedure calls:

1. `open_window` is called as the **first** instruction in `main`. This procedure call is passed three values: the title of the Window, its width in pixels, and its height in pixels.
2. `clear_screen` is passed the color to clear the window to. `COLOR_WHITE` is one of the values provided in [SplashKit colors](https://splashkit.io/api/color/#color-white).
3. `fill_ellipse` fills an elliptical shape on the screen. The values passed to it indicate its color, top left corner (x and y) and its width and height.
4. `fill_rectangle` is similar to `fill_ellipse`, but draws a rectangle at the position indicated.
5. `fill_triangle` fills a triangle on the window, with the values representing the color and the three points of the triangle (with each point having an x and y coordinate.)
6. `refresh_screen` is used to show the latest drawing to the user. 
7. `delay` pauses execution for a number of milliseconds, enabling the user to see what is drawn before the program ends. 

```cpp
#include "splashkit.h"

int main()
{
    open_window("Shapes by ...", 800, 600);

    clear_screen(COLOR_WHITE);
    fill_ellipse(COLOR_BRIGHT_GREEN, 0, 400, 800, 400);
    fill_rectangle(COLOR_GRAY, 300, 300, 200, 200);
    fill_triangle(COLOR_RED, 250, 300, 400, 150, 550, 300);
    refresh_screen();

    delay(5000);

    return 0;
}

```

:::note

- The procedure call runs the instructions in a procedure.
- As everything runs in sequence, the procedure's instructions before your code can move to its next instruction.

:::

### Types

Everything in the computer is binary, but to make this more usable programming languages add **types** to help make sense of any data that you want to work with. Each type indicates how to interpret data associated with that value. There are three basic data types available in a programming language, as shown in Figure [2.8](#fig:program-creation-type){reference-type="ref" reference="fig:program-creation-type"}.

-   **Textual** data such as "*Fred*", "*Hello World*", "*23*", and
    "*This is text!*".
-   **Whole numbers** such as *1*, *0*, *-5*, and *37*.
-   **Real numbers** such as *0.5*, *-126.0*, *3.141516*, and *23.981*.

Splashkit build on this with some additional values, which we can start to make use of.

- **Color** for working with shape drawing.
- **Bitmaps** for working with and drawing images.
- **Sound Effects** and **Music** for playing short and streamed audio.

The shape drawing code provides examples of textual data with the title of the window being opened, whole numbers for the positions and sizes of the shares, and colors.

![A types define how values are interpreted and the operations that can
be performed on the
data.](./topics/program-creation/diagrams/Type.pdf){#fig:program-creation-type
width="90%"}

:::note
- Textual data in C/C++ is indicated by the types **string** and **char**. String values can be entered in your code using double quotes around text. For example, "Hello World". The char type is used to represent a single character, and values can be entered in your code using single quotes, e.g. 'a'.
- The **int** type is used for whole numbers in C/C++. This can be entered as a numeric value in your code. For example, 73 and -829 are both integer values.
- The **double** type is used for real numbers, also known as floating point values. These are coded as their numeric value, with whole numbers also being valid double values. For example, 73 and -829 can be used as a double value as can 3.1415, 73.0, 0.110292, and -982.08223.
:::


### Libraries

A library is a collection of reusable code artefacts. Each programming language has its own library, and your programs can make use of the code available in this library. Others can also build and distribute libraries that others can use to simplify and speed up their development work.

We will be using **SplashKit** to start building interesting programs. SplashKit provides a range of procedures and types that we can get started with now in order to build some sequences that do something more than show text on the terminal. 

Building on top of other libraries is always a great place to start. Finding and using existing libraries should be the start of most programming projects. When you use a library you dont need to start from scratch, but can build on the work of others. SplashKit itself is built on top of a number of other libraries, and if you look at those libraries you will see they build on top of other libraries.

Using libraries in C/C++ can be challenging, it is an older programming langage and does not have a nice packaging system like you will find with most newer languages. The SplashKit library has been written specifically to help with introductory programming, and it includes tools like **skm** to help you connect the library with your code.

The `#include "splashkit.h"` code at the top of the shape drawing program gives you code access to the programming artefacts written in the splashkit library. Later in the book we will look at using other libraries, and more modern languages and their packaging systems. For now, the SplashKit library will give us a range of artefacts we can use to help learn to program.

![A library contains code that can be used by your
Program](./topics/program-creation/diagrams/Library.pdf){#fig:program-creation-library
width="\\textwidth"}

:::note

- Libraries package together a range of programming artefacts that can be used by others.
- Don't reinvent the wheel... always look to see if there is a library you can use to help simplify your project.

:::

## Sequene in action

### Writing to the terminal

### Drawing shapes and images

### Playing sound effects

### Creating animations

## Using these concepts

We now have a few things in our toolbox that we can start to use to build programs:

- **Program**: we can build a program that the computer can run.
- **Procedure Call**: our program can call procedures, with main running a sequence of instructions.
- **Library**: we can make use of the SplashKit library to load images, draw shapes, and play sound effects.





## Sequence - How it works

The following details all work together to help set the scene for how sequence works:

- Each instruction in the computer is the same size. A 64bit computer has 64bit instructions.
- Values in memory are accessed in chunks of memory the same size as an instruction. So each 64bits is accessible at a unique address.
- You can think of memory as a long list of values. Each 64bit value has a unique index (its address).

Let's see how this works by stepping through a sample program on a 4bit computer.

1. Let's start with the state of the computer *before* the program is started.

   The main thing to notice at this stage is the organisation of memory. The grey numbers show the addresses, with each storing one 4bit value. For example, the value at address `14` is `0111`.

   ![When the program starts, its instructions are loaded into memory.](01-sequence/slides/slide-028.jpeg)

2. When the program starts, the operating system loads its instructions into memory. In this case, it is loaded at address **31**.

    ![When the program starts, its instructions are loaded into memory.](01-sequence/slides/slide-031.jpeg)

3. When the program is loaded, the **program counter** is assigned the address of the first instruction in memory.

    The program counter determines the instruction for the CPU to execute. In this case, the CPU runs the code `1011`.

    ![The program counter is set to the address of the first instruction.](01-sequence/slides/slide-036.jpeg)

4. After the instruction completes, the value in the program counter is increased by one.

    The program counter now refers to address `32`, and the CPU runs the code `1101`.

    ![The program counter is set to the address of the first instruction.](01-sequence/slides/slide-037.jpeg)

5. This process continues as the program runs.

:::tip think

The value in the program counter changes as the program runs. Programming languages take advantage of this capability to allow you to control the sequence of actions.

What would happen if the user of a program could, through specially crafted actions, change the value of the program counter? What would this enable them to do?

:::

## Sequence in other languages

In this book, we are learning to program using what is known as an **imperative** programming language. All imperative languages involve programs that tell the computer what to do. Languages such as Python, C#, Java, JavaScript, TypeScript, Go, Rust, and many more are imperative programming languages involving the programmer writing sequences of instructions.

**Declarative** languages are another class of programming language. When creating a declarative program, you declare what you want as output rather than coding the steps to achieve that output. These languages determine the sequence needed to get the results you requested. SQL, HTML, and CSS are some common declarative programming languages. With SQL, you write queries indicating the data you want to select from a database. HTML and CSS allow you to define the elements of a web page and how you want those elements styled. In these cases, the database or web browser interpret your requirements and sequence the instructions to produce the output you requested.

Declarative languages also include **functional programming** and **logic programming** languages. With functional programming, a program consists of nested functions that convert inputs into outputs. Lisp and Haskell are examples of functional programming languages. With logic programming, you create a database of *knowledge* made up of known facts. You can then query this database to determine the truth of given statements, and the language will use what it knows to determine (or infer) the truth of your statement.

Starting with an imperative language will help you better understand how the computer works. These languages put you in control of the *sequence* of instructions the computer runs.


