# md-graph

Graph operations on a collection of interlinked Markdown (or VimWiki, ZimWiki, 
Zettlr, Obsidian, or Neuron) notes.

## Usage
```
Usage: md-graph (-l|--library ARG) [-d|--default-ext ARG] 
                ((-o|--orphans) | (-u|--unreachable) | (-s|--subgraph ARG) | 
                  (-b|--backlink ARG)) [--inc-static ARG] [--inc-nonex ARG]
  A utility for graph operations on a collection of markdown files
```

- `-l, --library` Required. The folders or files to include in the graph. 
    Folders will be recursively searched.
- Required, one of:
    - `-u, --unreachable` Find all file in the library that are not linked to
    - `-o, --orphans` Find all files in the library that are not linked to or from
    - `-s, --subgraph FILE` Given a file, find the subgraph of its forward links 
        (and its forward links' forward links, etc)
    - `-b, --backlink FILE` Given a file, find its backlinks
- Optional:
    - `-d, --default-extension` Default `md`. Extension to use for 
        links that do not specify an extension (like WikiLinks and VimWiki markdown 
        links)
    - `--inc-static=True|False` Default `True`. Includes static files in 
        the output (files that were linked to and can be resolved)
    - `--inc-nonex=True|False` Default `False`. Includes non-existant files in 
        the output (files that were linked to but can not be resolved - includes 
        HTTP links)

## Installation
```
git clone https://github.com/CharlesSchimmel/md-graph
cd md-graph
stack install
```

## Features & Future Features
- [x] Backlinks
- [x] Orphans
- [x] Subgraphs
- [x] Unreachables
- [x] Ignore or include static files
- [x] Ignore or include non-existant files
- [ ] Tags (Parsed as Backlinks)
- [ ] Ignore...
    - Lists
    - from another subgraph

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

