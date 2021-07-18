# md-graph

Graph operations on a collection of interlinked Markdown (or VimWiki, ZimWiki, 
Zettlr, Obsidian, or Neuron) notes.

## Usage
```
Usage: md-graph (-l|--library FILE|DIR) [-d|--default-ext EXT] 
                ((-o|--orphans) | (-u|--unreachable) | (-s|--subgraph NODE) | 
                  (-b|--backlinks NODE)) [--inc-static True|False] 
                [--inc-nonex True|False] [--tag-direction In|Out|Both]

  A utility for graph operations on a collection of markdown files
```

- `-l, --library` Required. The folders or files to include in the graph. 
    Folders will be recursively searched.
- Required, one of:
    - `-o, --orphans` Find files that are not linked to or from
    - `-u, --unreachable` Find files that are not linked to
    - `-s, --subgraph NODE` Given a [node](#nodes-files--tags), find its 
        subgraph: its forward links, its forward links's forward links, etc
    - `-b, --backlink NODE` Given a [node](#nodes-files--tags), find its 
        backlinks
- Optional:
    - `-d, --default-extension` Default `md`. Extension to use for 
        links that do not specify an extension (like WikiLinks and VimWiki markdown 
        links)
    - `--inc-static=True|False` Default `True`. Includes static files in 
        the output (files that were linked to and can be resolved)
    - `--inc-nonex=True|False` Default `False`. Includes non-existent files in 
        the output (files that were linked to but can not be resolved - includes 
        HTTP links)
    - `--tag-direction=In|Out|Both` Default `In`. Controls how tags are added to 
        the graph from the perspective of the tag.
        - `In`: The link is coming **in** to the tag from the file
        - `Out`: The link is from the tag **out** to the file
        - `Both`: The tag and the file **both** have links to each other.


### Nodes: Files & Tags
Arguments that take a "`NODE`" (such as `--subgraph` and `--backlink`) can be 
given either a file or a tag. Tags are given by including the hash just like 
their [file-parsed counterparts](#tag-format). The following will attempt to 
find the backlinks of the tag "my-special-tag":
`md-graph -l "my-files" --backlink #my-special-tag`.

With option `--tag-direction=Out`, you can find the subgraph of a tag.

Everything else will be parsed as a file.

## Installation
[Follow the instructions here to install stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

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
- [x] Ignore or include non-existent files
- [ ] Configurable Depths
- [ ] Tags
    - [x] As forward-linked
    - [x] As backward-linked
    - [x] As both/either
    - [ ] Include tags in output
- [ ] Ignore
    - from a list
    - from another subgraph

### Supported Formats
#### Link Formats
Because this merely parses out links, any format that uses markdown or wikilink 
style links should work. Links that are escaped or encoded in a way that is 
different from their filesystem name will not be traversed correctly.

#### Tag Format
Tags that follow the form of a hash ('#') followed by alpha-nums, dash, and 
underscores will be parsed as nodes. For example, this tag is valid: `#someSort-of_tag123`.

## Background
I have a directory of notes in markdown format that link to each other like a 
Wiki. Sometimes I want to publish my notes on a given topic or group of topics 
including all of the related notes that they link to. For example, I might want 
to publish only my kombucha notes from my collection of notes on cooking. It 
would be a pain to have to manually trace the links between them in order to 
extract just the pertinent ones. This will take a root markdown file and 
traverse its links, returning all of the files necessary to complete its 
subgraph.

