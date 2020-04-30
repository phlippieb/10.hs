# 10.hs
Haskell implementation of modified 2048

## How to play

To start the game, run

```
make run
```

(See the requirements if this doesn't work.)

**10** is a slightly modified version of [2048](https://github.com/gabrielecirulli/2048), the very smart puzzle game by Gabriele Cirulli.

The only differences are:
- In **10**, numbers grow linearly (1, 2, 3, 4, 5, etc) where in **2048**, they grow exponentially (1, 2, 4, 8, 16, etc).
- In **10**, you win and the game ends when you get to 10, where in **2048**, you win when you get to 2048, but you can keep playing.

Why the changes? Because I'm completely new to Haskell, and I didn't want to deal with string formatting, that's why.

When you start a game, you will see a 5x5 board (the size is a parameter in the source code, so you can easily tweak it).
The board is mostly empty -- empty cells are indicated by periods (`.`) -- but there will be one non-empty cell. For example:

```
. . . . .
. 1 . . .
. . . . .
. . . . .
. . . . .
```

You will also see a prompt to "enter wasd".
Choose a direction - up, down, left or right, by entering the corresponding wasd key, and hit return.
Two things will now happen.
One: the pieces on the board slide in the direction of your choice. 
And two: the computer places a new piece on the board, indicated by an asterisk.
For example, after entering `w` for up:

```
. 1 . . 1*
. . . . .
. . . . .
. . . . .
```

A third thing that will start happening pretty soon, is that when two pieces with the same value slide into each other, they merge.
For example, after entering `d` for right:

```
. . . . 2
. . . . .
. . . . .
. . 1*. .
. . . . .
```

And so on.

The goal of the game is to keep merging pieces together until you merge two `9`s, giving you a `10`.

But be careful!
If the board fills up and you have no available moves, you lose.

You will also note that the computer won't place a piece unless you actually move the pieces.
For example, if you have

```
1*. . . .
2 . . . .
. . . . .
. . . . .
. . . . .
```

and you enter `a` for left, or `w` for up, the board doesn't change, and you don't get a new piece. 
No soup for you!

Have fun.

### Requirements

You will need [Haskell Stack](https://docs.haskellstack.org/en/stable/README/).
See the link for installation instructions.

You will also need to have `random` installed:

```
build the program:
```
