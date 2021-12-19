---
title: "Programming Arcana"
metaTitle: "Introduction to the Programming Arcana"
metaDescription: "This chapter outlines the approach to introductory programming covered in the programming arcana text."
---

Welcome to the Programming Arcana, a book about learning to program. This book will take you from knowing nothing, or little, about programming to a position where the mysteries are revealed. By the end of the material, you will be able to create programs, and you will be ready to start learning other programming languages and approaches to software development.

Each chapter introduces a programming task and its associated *arcane* knowledge. It is important to engage with each of the tasks, and to utilise the associated knowledge to ensure you understand how the task is achieved, how to use the associated concepts yourself, and how the programs covered work in the computer. Being able to create your own programs will require you to bring all of these ideas together with some creativity and perserverance.

:::note

Wiktionary defines Arcana as "specialized knowledge that is mysterious to the uninitiated.". This fits well with the idea of programming, and we think it is a cool word to describe the *magic* of programming!

:::
## Book Overview

This book is designed for people who want to gain an in-depth knowledge of software development. It assumes that you are familiar with computers and how to use them, but does not require any prior programming experience. You should feel comfortable using a computer and be prepared to start looking at how it works in more detail.

There are many ways an introductory programming book can be written. The Programming Arcana aims to focus on **programming concepts**, in particular, programming concepts that are common across many programming languages. We will explore the concepts using the C/C++ programming language initially. This language is very low level, meaning we can make use of it to understand how things work in the computer. While we are using C/C++, this is not the main focus. So do not think of this as a book about C/C++. In fact, we will not cover many part of this language, instead we focus just on the parts we need to explore the core programming concepts. At the end of this you should have all the skills and the foundational knowledge needed to learning C/C++, and many other languages. You will have learnt to program.

Chapters build upon each other. Each chapter covers a new group of concepts, that will expand your programming capabilities and enable you to create larger and more capable programs.

The layout of
each chapter is the same, with the concepts having the main focus. Each
chapter is laid out in the following order:

1.  **Concepts**: The first part of the chapter introduces the concepts
    that will be covered. This is done in a language neutral manner,
    with the focus being on how to think about the tools being
    presented. This will introduce each concept with an illustration,
    and accompany this with explanatory text.

2.  **Using the Concepts**: The next section shows how these concepts
    can be used to achieve a task. This task will try to cover all the
    concepts presented in a practical manner. This is done in a language
    neutral way, and talks about how to use the concepts to achieve a
    goal.

3.  **Languages**: The next two sections present the syntax you need to
    use these concepts in **C++** and **Pascal**. You should use this as
    a reference, and can read this alongside reading about how to use
    the concepts.

4.  **Understanding**: Following the language specific details, the next
    section explains in detail how the concepts work within the
    computer. Use this to get an understanding of how the concepts work
    in more detail. This section will show you illustrations of what is
    happening within the computer when your code is running.

5.  **Examples**: Each chapter will have at least one example showing
    you how these concepts can be used. This will include the code, and
    some explanatory text to discuss what is being presented.

6.  **Exercises**: The exercises allow you to put into practice what you
    have read about. You cannot learn to program without practice. These
    exercises are a good start, but you should try to come up with your
    own project so that you can test out these new concepts on something
    you are interested in working on.r

## Which Language? {#sec:which_language_ .unnumbered}

A programming language defines a set of rules that determine how you
write the code for your programs. Each language defines its own rules,
and so there is always the temptation to focus heavily on these details
and place the overriding concepts in second place. We believe that when
you are starting to learn to program, a good understanding of the
programming concepts is far more important than the details of the
programming language you are using. This book is not an in depth study
of either the C++ or Pascal language, it is a book about learning to
program.

To really learn these concepts well you will need to practice putting
them to use. This will require you to use a programming language. Each
chapter will provide you with enough information to put the concepts to
use in either the C++ or Pascal language. So the main question you need
to answer now is which language will you use?

Both C++ and Pascal are very capable languages. Pascal was designed as a
teaching language, which means that it does make it a little easier to
see how the concepts being covered apply to your code. C++, on the other
hand, is a commercial language designed for professionals to build
programs. This is both an advantage and disadvantage for C++. The
advantage is that the language is widely used in industry, but the
disadvantage is that it lacks the clarity that is offered by Pascal.
Remember that this is only your first programming language. A
professional software developer will know many different languages, and
by the end of this material you will be equipped to learn many new
languages.

## Formatting {#sub:formatting .unnumbered}

This book has a number of visual formatting guides. These are designed
to help you navigate through the material easily.

The language sections of each chapter also add markers to each page to
clearly mark where they start, and where they end. If this is your first
programming experience you should stick with one of these languages, so
you can skip the pages that are marked as being for the other language.

## Programming Jargon and Concept Taxonomy {#sub:concept_taxonomy .unnumbered}

Programming has a lot of its own jargon. As you learn to develop
software it is also important that you start to learn this *special
language* that software developers use to discuss their programs. You
will find that this terminology is used in many places. It is used in
programming texts, in discussions between developers, in discussion
boards, blogs, anywhere that developers are discussing software
development. Having a clear understanding of this terminology will help
you make the most of these resources.

The concepts in this book are closely linked to this programming
terminology. To help you understand each concept, we have classified
them using one of the following categories:

-   **Artefact**: An artefact is something that you can create in your
    code.

-   **Action**: Actions are things that you can *command* the computer
    to do.

-   **Term**: These are general terms, used to describe some aspect.

When you are reading about the different concepts in this book you can
use these classifications to help you think about how you may use the
knowledge you are gaining.

##### Artefacts: {#par:artefacts}

Artefacts are things that you create in your code. Programming is a very
*abstract* activity, you spend most of your time working with concepts
and ideas. You write text, code, that will create things within the
computer when your code is run.

When you are learning about a new kind of artefact come up with ways of
visualising it. It is a **thing** that you are creating with your code.
Try to picture the artefact within your code. These artefacts are the
basic building blocks that you have to work with. You need to be very
familiar with them, how they work, and what you can do with them.

##### Actions: {#par:actions}

Actions get the computer to perform a task. Your actions will be coded
within the **artefacts** that you create, and will define how artefacts
behave when they are used. The actions themselves are commands that you
issue to the computer. They are executed one at a time, and each kind of
action gets the computer to carry out certain tasks.

When you are learning a new kind of action you need to see what this
action does. To start with you should play with it, test it out, and see
if you can understand what it is getting the computer to do. As you
progress you need to start thinking about how you can sequence these
actions so that the computer performs the tasks you want it to. There
are only a very few kinds of actions, so it is by combining them that
you can get the computer to do what you want.

##### Terms: {#par:terms}

The remaining terms are words that developers use to explain concepts.
These are not things that you create, or actions that you request. These
are just words that you need to *know*.

When you are learning a new term you need to try to commit it to memory.
Memorise the terms, try to use them in sentences, explain them to
others. All of these tasks will help you understand, and remember these
terms.

## Advice {#sec:advice .unnumbered}

If you want, or need, to learn to program then you can not do this just
by reading a book, even one as magical as this. Learning to program
requires practice. This book is designed to give you the concepts you
need in order to understand how to go about creating your first
programs. To really understand these concepts you need to apply them to
the creation of your own programs.

When you are getting started, programming can appear quite daunting and
the tools you use can be unforgiving. Work through these initial
challenges, and with practice you will be able to overcome them. Once
you have some success there is nothing better than seeing a program you
created running on a computer. You have brought the machine to life,
getting it to perform a task the way you want it performed. Once you get
a program working it can become easy to get hooked and working on new
features and functions becomes a real joy. The greater the challenge the
program offers, the greater your sense of achievement when you see the
working product in operation.

Other people are the best resources to help you get over these initial
challenges. Fellow students studying this material can provide you with
support, and a chance to discuss the challenges you are facing. Teaching
staff are also a good resource when you are really stuck. If you do not
have access to anyone who can help, use discussion boards and websites.
Getting the right help will make a large difference to your learning
experience.

Remember that you will need to study this material. That is not just
reading it, but thinking and reflecting on what you have read. Try to
think about each of the concepts, and how they relate to the other
material that has been presented to you. Try to design and build your
own programs with the material you are learning. If you do think deeply
and apply the concepts to programs you create, you will eventually get
the light bulb moment when things become clear and programming can
become truly joyful.

