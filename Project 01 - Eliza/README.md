# ELIZA LISP
## Basic Conversational Agent Written in LISP
ELIZA LISP is an AI agent capable carrying on very simple conversations using a general set of
rules. Rules define a set of keywords and patterns that the program uses to respond to text supplied
by the user.

## Requirements
ELIZA LISP was written and tested using the `clisp` implementation of Common LISP. This program is
normally available through your distro's package repository.
```
  $ sudo apt update
  $ sudo apt upgrade
  $ sudo apt install clisp
```
Once installed, the program can be ran using the `-i` argument.
```
  $ clisp -i "eliza.lisp"
```
Please note that installation instructions may differ from your distro, but the general process
should be the same regardless.

## Conversing w/ the Agent
To converse with the agent, the program must first be ran using the `clisp` interpreter. Doing this
will import the file, syntax check for errors, and allow for interaction using the command line
interpreter.

Once an interpreter session has been initialized, statements can be sent to the model using the
command `(eliza '(statement))`. A response will be selected from a dictionary of preprogrammed
responses and printed from the terminal.

## Limits of the Agent
All of the responses ELIZA LISP generates are based on preprogrammed rules, meaning there is a
finite complexity to the conversations one can have. ELIZA LISP also has no way of retaining any
information given to it, meaning it cannot react to new information based on information provided to
it beforehand.

As a result of these facts, conversations are circular in nature, regardless of the number of rules
added to the agent's dictionary.
