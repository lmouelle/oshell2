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

# Issues with OCaml's Unix lib

I was reading [the open group spec for sh as a reference](https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#tag_18_07), and I realized something unfortunate. To support the syntax `n>&k` or `n<&k`, where we
duplicate a file descriptor to another, we need to call `Unix.dup2`. In C this is simple, just parse out `n` and `k`
into integers and then do `dup2(src, target)`. In OCaml this won't work. We parse out `n` and `k` as integers in the lexer easily enough, but `Unix.dup2` expects `file_descr` types which are abstract. There is no clear way, I have found so far, to translate from an integer to a file descriptor. So I can't do the file descriptor duplication at the moment.

# Misc issues

I'd like to be able to do something like
```bash
$foo = ls
$foo directory
```

but sadly my grammar does not accomodate that. It's hard to do that with my setup without shift/reduce conflicts all over.
How am I supposed to know if the string `$foo directory`, upon seeing the token `$foo`, is a new variable definition or
an invocation? This definitely is a known problem, but at this time I do not know the answer.

# Features I want

I'm not great at keeping READMEs current so this will get wildly out of date but here is what I have as of writing (March 2023) and what I want.

I have:
- Arbitrary redirection. So `foo < infile 2> errfile > outfile` works.
- Pipes, so I can do `ls | grep ocaml `
- I tolerate empty input, so entering an empty string is a no op
- I currently track the last exit code so `$?` can be implemented soon
- Conditionals like `true && ls` or `false || ls`

Pipes can contain unlimited redirections, conditionals can contain unlimited pipes.

I want:
- Variables
- To get that darn file redirection duplication
- Wild card expansion? Always felt kind of weird to me, not sure how I want to go about it
- Functions
- Loops
- Case

In general the closer I get to a proper `sh` implementation, the better. This will take time.