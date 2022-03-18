---
title: "Drawing shapes and images"
---

Get started with simple drawing operations using procedure calls and the SplashKit library.

## Pixels and Coordinates

The images you see on the computer’s screen are made from a grid of coloured dots called pixels: picture elements. The screen’s pixels are arranged in a grid consisting of a number of columns and rows. Each pixel is at a unique location (a combination of an `x` and `y` value, where `x` indicates the column and `y` the row). The color of each pixel can be changed through drawing operations that are coded into SplashKit’s drawing procedures.

An example of two rectangles, one filled and one outlined, is shown in the following image. The top left corner of the screen is at column (x) 0 and row (y) 0, and these numbers increase as you go to the right and down the screen. The call to `fill_rectangle` and `draw_rectangle` take in a color, an `x` and a `y` value, and a `width` and `height`. So the blue rectangle is filled at x 1, y 1, is 7 pixels wide, and 3 pixels high.

<img alt="Pixel locations are based on x and y locations." src="/images/articles/starter/drawing/DrawFill.png" style="width: 700px"></img>

When you position something on the screen, you provide a value for `x` to indicate the distance from the left side of the image, and another value for `y` to indicate the distance from the top of the image. The values indicate the number of pixels from the edge of the image. For example: the magenta rectangle is drawn at 10, 1. We positioned it here by giving `x` a value of 10 and `y` 1. This rectangle is drawn 10 pixels from the left of the screen, and its 1 pixel from the top.

You can play around with positioning things one the screen using the shape drawing procedures. To plan out an image you can sketch out your ideas on paper, or in an image editor, then measure the distances and sizes of the shapes you need.

## Drawing Procedures

To draw a shape with SplashKit you need to call one of the shape drawing procedures. All of the shape drawing operations in SplashKit take a number of parameter values:

* The **color** to draw the shape. 
    SplashKit has a number of colors you can use. See the list of colors starting with (Alice Blue)[/api/color#color-alice-blue]. For example, in C++ the colors are written as `COLOR_WHITE`, `COLOR_GREEN`, `COLOR_BLUE`, `COLOR_BLACK`, `COLOR_RED`, and many others. VS Code should show you a list once you type `COLOR_` then hit ctrl+space. Though this only works if you opened the project folder.
* An **x** value, representing the x position of the shape (column). 
    
    This is a number of pixels from the left edge of the screen. Larger values are further to the right.
* A **y** value, representing the y position of the shape (row). 
    
    This is a number of pixels from the top edge of the screen. Larger values are further down the screen.
* and other values for the size of the shape, these will differ depending on the kind of shape being drawn (e.g., rectangle has a width and height, as does ellipse, while circles have a radius).



See the following for examples of what you can call:

* [Clear Screen](https://splashkit.io/api/graphics#clear_screen)
* [Draw Circle](https://splashkit.io/api/graphics#draw_circle)
* [Draw Ellipse](https://splashkit.io/api/graphics#draw_ellipse)
* [Draw Line](https://splashkit.io/api/graphics#draw_line)
* [Draw Pixel](https://splashkit.io/api/graphics/#draw-pixel)
* [Draw Quad](https://splashkit.io/api/graphics#draw_quad)
* [Draw Rectangle](https://splashkit.io/api/graphics#draw_rectangle)
* [Draw Text](https://splashkit.io/api/graphics/#draw-text-no-font-no-size)
* [Draw Triangle](https://splashkit.io/api/graphics#draw_triangle)
* [Fill Circle](https://splashkit.io/api/graphics#fill_circle)
* [Fill Quad](https://splashkit.io/api/graphics#fill_quad)
* [Fill Ellipse](https://splashkit.io/api/graphics#fill_ellipse)
* [Fill Rectangle](https://splashkit.io/api/graphics#fill_rectangle)
* [Fill Triangle](https://splashkit.io/api/graphics#fill_triangle)

:::tip

**Autocomplete** is a great feature available in most code editors. With autocomplete, you can see the list of procedures and their parameters in the editor as you type. In Visual Studio Code type the start of the procedure name then press **ctrl-space** to bring up the autocomplete list. There are usually a couple of different ways to draw each shape, by pressing the up down arrows you should be able to explore this list. An example is shown below.

<img alt="Use `ctrl-space` to bring up the autocomplete for procedures you want to call." src="/images/articles/starter/drawing/AutoComplete.png" style="width: 700px"></img>

Autocomplete needs to know about your project, its code, and the related libraries you are using. To get this to work, open the **folder** that contains your project and open the files from the folder list in the editor.

:::

## Double Buffering

To draw a picture, like the house shown above, the computer executes the code to draw the individual shapes one at a time in the order they appear in the code (in **sequence**).  However, we don't often want each element to appear individually, we want to control when the updated screen should be shown to the user. In this case, we want the whole house shown all at once. 

SplashKit uses a technique called **Double Buffering** to give the programmer control of when things are shown. When double buffering, the computer first draws the shapes to a copy of the screen that is not shown to the user, and waits for a command to display this new screen to the user.  With SplashKit, call the `refresh_screen` procedure to display the new screen when you are ready. This is illustrated below.

<img alt="Illustration of double buffering, and the need to refresh screen" src="/images/articles/starter/drawing/RefreshScreen.png" style="width: 700px"></img>

## Creating a Window

In order to create a drawing, we need something we can draw on. In SplashKit you can draw onto Bitmaps and Windows. Lets start by drawing onto a Window.

Call the [open_window](/api/graphics/#open-window) procedure when you want to open a window, passing in the title, width, and height for the new window. For example,  `open_window("House Drawing", 800, 600);` will open a window titled "House Drawing" that is 800 pixels wide and 600 pixels high. The window is shown in the following image, along with a house and hill that are drawn by additional code.

<img alt="Window with dimensions illustrated" src="/images/articles/starter/drawing/NewWindow.png" style="width: 700px"></img>

Lets put this into practice now. Create a new SplashKit C++ project and give the following code a try.

1. Open a new Terminal Window, change into the directory where you want to create the project, and setup a new C++ SplashKit project.

    I used the following commands to move into the `Code` folder in my `Documents` folder, make a new *ShapeDrawing* folder using the **mkdir** command, move into this new folder, and create a new C++ project.

    ```bash
    cd /Users/andrew/Documents/Code
    mkdir ShapeDrawing
    cd ShapeDrawing
    skm new c++
    ```
    
1. Open the **folder** you created in Visual Studio Code, and open the **program.cpp** file. Update the code to appear as follows:

<%= snippet "step1", "articles/guides/starter/drawing/" %>

    :::note
    Pay attention to the following in the code:
    
    - The `#include` at the start will give you access to the SplashKit library code.
    - Your program will start with `main`, the code wrapped in braces (`{…}`) following main are the instructions that run when the program is executed.
    - Focus on the **sequence** of the instructions within `main`.
    - Make sure you can identify each of the procedure calls and, for each procedure call, the procedure it runs and the purpose of the values passed to it.
    :::

1. Compile and run the program from the terminal.

    ~~~bash
    skm clang++ program.cpp -o ShapeDrawing
    ./ShapeDrawing
    ~~~

    This will now run the program, following the instructions you have in main. You should see the window open, and the program delay for 5 seconds.

1. Change the window title to "Shapes by " and your name. For example, `"Shapes by Andrew"`.

    Switch back to the terminal to compile and run your program.

Before moving on, make sure that you can describe what each of the procedure calls is doing, and you can explain the output and how it relates to the sequence of instructions in `main`. If you are unsure, try moving the instructions around inside of main.

:::tip Try it out

What happens if you do the following? Try to to predict the outcome. Think about what will happen and why, then test it out to confirm your understanding.

- Switch the order of the call to open window and delay
- Remove the call to the delay procedure
- Move one of the procedure calls out of the braces that follow `main`
- Remove the `#include “splashkit.h”` from the start of the code

Remember to save and then recompile the program each time you make a change.

:::

## Step 2: Drawing to a Window

1. Adjust `main` so that it now draws the house shown above.

<%= snippet "step2", "articles/guides/starter/drawing/" %>

1. Build and run the program.

    For example in C++ you would use:

    ~~~bash
    skm clang++ program.cpp -o ShapeDrawing
    ./ShapeDrawing
    ~~~

    Make sure it all looks correct before moving on.

:::tip Try it out

Check your understanding of the code by considering what will happen if you do the following.

- Switch the positions of the refresh screen and delay calls
- Reverse the order of the shape drawing instructions

:::

## Creating your own drawing program

You can use the shape drawing procedures to draw any number of images. Building a small program that draws an interesting shape, pattern, or image is a good way to make sure you are able to use these tools going forward.

1. Plan out the shapes you want to draw.
2. Create a new shape drawing project, and open the folder in vscode so you can use the autocomplete features.
3. Add code to `main` to open a window, and draw your picture to the screen.
4. Compile and run your program, addressing any errors and adjusting the code to get the image to look just the way you want.
5. Grab a screenshot of your image and share it to mark you achievements.

:::tip
Make your code look neat. While it is important that your code works, it is equally important that it is readable by others. Coding conventions structure the ways we expect code to appear. Conventions change between languages and teams, but there are some general rules that apply in most circumstances:

- Code within a block (between braces in C++) should be indented to clearly show the start and end of the block.
- Names should reflect the purpose or action of the artefact.

:::

## Beyond SplashKit

Much of what you have learnt in this section about graphics and drawing operations will apply across languages, libraries, and devices. Drawing surfaces, such as windows, will give you a two dimensional grid of pixels that you can draw on. The exact procedures you need to call will change, and steps you need to take in order to get this setup and working will also change. Learning to work with 2 dimensional coordinates, colors, and sequencing drawing operations are all going to be similar whenever you work with graphics.
