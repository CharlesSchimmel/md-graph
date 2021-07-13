# md-graph

Graph operations on a collection of interlinked Markdown (or VimWiki, ZimWiki, 
Zettlr, Obsidian, or Neuron) notes.

Supports regular commonmark-style links (`[title](file.md)` or `[title](file)`) 
and wikilinks (`[[file]]` or `[[file|title]]`).

## Usage
```
Usage: md-graph (-l|--library ARG) [-d|--default-extension ARG] 
                ((-o|--orphans) | (-u|--unreachable) | (-s|--subgraph ARG) | 
                  (-b|--backlink ARG))
```

* `-l, --library` - Required; folders or files to include in the graph. Folders 
    will be recursively searched.
* `-d, --default-extension` - Optional, defaults to `.md`; extension to use for 
    links that do not specify an extension
* Required, one of:
  - `-o, --orphans` - find files that are not linked to or from
  - `-u, --unreachable` - find files that are not linked to
  - `-s, --subgraph FILE` given a file, find the subgraph of its forward links
  - `-b, --backlink FILE` given a file, find its backlinks

## Installing
```
git clone https://github.com/CharlesSchimmel/md-graph
cd md-graph
stack install
```

## Features & Future Features
- [x] Backlinks
- [x] Orphans
- [x] Unreachables
- [ ] Tags
- [ ] subgraphs
    - [x] forward
    - [ ] backward
    - [ ] undirected
- [ ] Ignore...
    - Lists
    - from another subgraph
- [ ] Ignore or include static files

### Supported Formats
Because this merely parses out links, any format that uses markdown or wikilink 
style links should work. Links that are escaped or encoded in a way that is 
different from their filesystem name will not be traversed correctly.

## Background
I have a directory of notes in markdown format that link to each other like a 
Wiki. Sometimes I want to publish my notes on a given topic or group of topics 
including all of the related notes that they link to. For example, I might want 
to publish only my kombucha notes from my collection of notes on cooking. It 
would be a pain to have to manually trace the links between them in order to 
extract just the pertinent ones. This will take a root markdown file and 
traverse its links, returning all of the files necessary to complete its 
subgraph.

