# oshell2

I tried to build a shell in Ocaml. It kind of worked but was really gnarly.
Instead of updating that project I am starting from scratch with a clear mind a little experience.

# Issues with Parser Combinators

I orginally wanted to use the Angstrom parser library for this project, like I did in my original attempt.
Sadly some things parse poorly under Angstrom.
Consider the following string:
`ls foo/ 2> errfle`

We would expect this to invoke the executable `ls`, with the argument list `foo/` and redirect stderr to `errfile`.
Under my Angstrom implementation however this resulted in an argument list of `foo/ 2` and a redirection of `> errfile`.
Why?

Well, the problem is parsing the various 'words' of a command. To parse a word, we want anything that is not a reserved 
char. Numbers are not reserved chars. There is nothing stopping a command or executable from having a number in the name (e.g. 0install). So we need some want to, upon seeing a number, peek ahead until we encounter ['<' '>'] (then fail to parse a word, backtracking). 

Or, using a parser generator + lexer, just clump `NUMBER? >` together into a single token. So yeah, I went from Angstrom to Menhir