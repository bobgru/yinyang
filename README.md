# Yin Yang Puzzle

This is an Elm application to interactively solve Yin-Yang puzzles, e.g. grid puzzles
with the following rules:

* All cells are filled either white or black.
* All cells of a given color are orthogonally connected.
* No 2x2 region of cells consists of a single color.

I wanted to practice solving such puzzles to get better at them, but couldn't find an app
or collection online, so decided to write one. Another motive
was to become acquainted with `elm-ui`, and to practice Elm programming in general.

As of now, there are sample games I found on `puzzling.stackexchange.com` to choose
from, where the game is selected randomly when the page is refreshed. Click left to play black.
Click right to play white. Click again to remove. The target cell for a click is highlighted
yellow. Cells part of the original puzzle configuration have a light green background.
Orthogonally connected cells are highlighted in blue when you hover over any cell within
the group. A 2x2 region of a single color is highlighted in red.

Two other error conditions are highlighted in red that are implied by the rules, to reduce eye
strain from constantly scanning for them:
* A play that results in a group of cells being surrounded by the opposite color and therefore unable
to be orthogonally connected. This hint is sometimes false in that you might be able to correctly play
the enclosed cell the same color as the enclosing cells.
* A checkerboard pattern, i.e. crossed black and white diagonals, because it leads to the previous condition.

A winning game is highlighted in orange.

Pressing "u" undoes the last move if there was one, moving it to a "redo" list. Pressing "r" replays the
next move in the redo list. A mouseclick adds a move to the undo list and clears the redo list.

Pressing "s" records a snapshot of the current board and undo list. Pressing "p" pops a snapshot off the stack
and restores the board and undo list to the previous state.

Pressing "e" toggles display of errors, so you can play without the hints if desired.

Pressing "w" toggles display of wins, so you can have the system confirm your win after you've
convinced yourself first.

There is enough functionality to practice the samples, but there are features I would like to add:
* Add visual effects to help indicate what has changed
* Select the sample from a menu
* Configure a new game interactively
* Generate a new game randomly
* Programmatically solve a game
* Provide a hint, preferably according to logical deduction
* Record elapsed time to solve
* Abstract out a general black/white grid game module