# Step 0: OPAM Housekeeping

There is a new OPAM package that we need to install for this assignment. Before installing it, update OPAM:

`$ opam update`

That command can sometimes take a very long time to complete, during which there will be no output. So don't get too concerned if it looks like the command is hanging. This especially happens on WSL1, where I've seen it take 10+ minutes.

If it afterwards prompts you to upgrade already-installed packages (which it very likely will), do so:

`$ opam upgrade`

If that tells you "Everything as up-to-date as possible [...] Nothing to do", with a lot of output in place of that "...", then don't worry: everything is fine. It's just because OCaml 5 is out now. We are not going to switch from v4 to v5 this year.

Then install ANSITerminal, which we need for the text user interface in this assignment:

`$ opam install ANSITerminal`