# zkw-subgraph

I have a directory of notes in [Obsidian](https://obsidian.md/) format 
(Commonmark but with WikiLinks-style links instead of Markdown). Those notes 
link to each other in a Wiki-ish format. Some day I might want to take some 
subset of those notes, and publish them as publicly browsable wiki. It would be 
a pain to have to manually trace the links between them in order to extract just 
the pertinent ones. Instead, it's a fairly trivial matter to take a file, parse 
the links out of it, and recurse.

In the future, I may add support for 
[backlinks](https://help.obsidian.md/How+to/Working+with+backlinks) as well.
