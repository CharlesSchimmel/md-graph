# md-graph

Graph operations on a collection of interlinked Markdown (or 
[VimWiki](https://github.com/vimwiki/vimwiki), [ZimWiki](https://zim-wiki.org/), 
[Zettlr](https://www.zettlr.com/), [Obsidian](https://obsidian.md/), or 
[Neuron](https://neuron.zettel.page/)) notes.

md-graph seeks to enrich an existing library of interlinked notes but not change 
how notes are written for the sake of its use. This enables interoperation with 
other note systems as well as respecting note libraries that strive to remain 
evergreen by staying simple.


## Usage
```
md-graph - A utility for graph operations on a collection of markdown files

Usage: md-graph [-l|--library DIR] [-x|--default-ext EXT] [-d|--database DB] 
                [-v|--verbosity DEBUG|INFO|ERROR|NONE] 
                [--no-scan | (-f|--scan-file FILE|DIR)] COMMAND

Available options:
  -h,--help                Show this help text
  -l,--library DIR         The parent directory for all documents.
                           (default: "./")
  -x,--default-ext EXT     Default extension to use for files linked without
                           extension (default: "md")
  -d,--database DB         Sqlite database to use. If no argument is given, an
                           in-memory database will be used for this command.
                           (default: ":memory:")
  -v,--verbosity DEBUG|INFO|ERROR|NONE
                           Modify the logging level (default: None)
  --no-scan                Don't scan or update any documents, just use what's
                           in the database.
  -f,--scan-file FILE|DIR  Scan and update only the given files or directories.

Available commands:
  subgraph                 The subgraph of a node
  backlinks                The backlinks (reverse subgraph) of a node
  unreachable              Files that are not linked to
  orphans                  Files without any links
  nonexistent              Links that cannot be resolved
  static                   Links that can be resolved but are not notes
  populate                 Don't perform any graph operations, just scan files
                           and update them in the database.
```
- `-l, --library` Optional, defaults to the current working directory. The 
    folders or files to include in the graph. Folders will be recursively 
    searched.
- `-d, --database` Optional, defaults to creating a temporary `mdgraphXXXX` 
    file.
- `-x, --default-ext` Default `md`. Extension to use for 
    links that do not specify an extension (like WikiLinks and VimWiki markdown 
    links)
- `-v,--verbosity DEBUG|INFO|ERROR|NONE` Default `NONE`.
- `--no-scan` and `-f,--scan-file FILE|DIR`. Determines whether `md-graph` 
    should scan files before performing the chosen operation. By default, 
    `md-graph` will find all files in the library and add or update any that are 
    new or changed. Multiple files may be speficied with `-f`.

### Subgraph
A subgraph is the graph of a file built by following its links, its links' 
links, etc. If `foo.md` links to `bar.md` and `baz.md`, and `bar.md` links to 
`qux.md`, the subgraph of `foo.md` will be `foo.md bar.md baz.md qux.md`

```
Usage: md-graph subgraph NODES [--inc-nonex True|False]
                        [--inc-static True|False] [--tag-direction In|Out|Both]
                        [--max-depth ARG] [--min-depth ARG]

  The subgraph of a node

Available options:
  NODES                    Nodes (files or #tags) to process
  --inc-nonex True|False   Include non-existent files in output (default: False)
  --inc-static True|False  Include static files in output (default: True)
  --tag-direction In|Out|Both
                           Change the direction of tags (default: Out)
  --max-depth ARG          How many links deep traversal should go (default: -1)
  --min-depth ARG          Only return links that are this many deep. The target
                           node is at depth 0. (default: 0)
  -h,--help                Show this help text
```
- `NODES` The target files or tags to act as the start of the subgraph.
- `--inc-static=True|False` Default `True`. Includes static files in 
    the output (files that were linked to and can be resolved)
- `--inc-nonex=True|False` Default `False`. Includes non-existent files in 
    the output (files that were linked to but can not be resolved - includes 
    HTTP links)
- `--tag-direction=In|Out|Both` Default `Out`. Controls how tags are added to 
    the graph from the perspective of the tag.
    - `In`: The link is coming **in** to the tag from the file
    - `Out`: The link is from the tag **out** to the file
    - `Both`: The tag and the file **both** have links to each other.
- `--max-depth INT` Default `-1` (infinite depth). Configures how many links deep from the target NODES a subgraph should traverse.
- `--min-depth INT` Default `0` (include the target file). Configures the 
    minimum depth a node should be to be included in the output.

### Backlinks
Backlinks are the files that link to a given file. `foo.md` is a backlink of 
`bar.md` if `foo.md` links to `bar.md`. It can also be thought of as the local 
subgraph of a file with the arrows reversed.

```
Usage: md-graph backlinks NODES [--max-depth ARG] [--min-depth ARG]

  The backlinks (reverse subgraph) of a node

Available options:
  NODES                    Nodes (files or #tags) to process
  --max-depth ARG          How many links deep traversal should go (default: 2)
  --min-depth ARG          Only return links that are this many deep. The target
                           node is at depth 0. (default: 1)
  -h,--help                Show this help text
```
- `NODES` The target files or tags to act as the start of the subgraph.
- `--max-depth INT` Default `2` (only the documents that link to the target). Configures how many links deep from the target NODES a subgraph should traverse.
- `--min-depth INT` Default `1` (do not include the target file in the output). Configures the minimum depth a node should be to be included in the output.

### Unreachable, Orphans, Nonexistent, Static
- Unreachable: find documents that are not linked _to_ (but may themselves link 
    to other documents.) If your goal is a fully hypertextual collection of 
    documents, then unreachable documents are effectively lost in your 
    collection.
- Orphans: find documents that are not linked _to_ and themselves do not link to 
    other documents.
- Nonexistent: find broken links that do not resolve to a real document. Note: 
    md-graph expects links to be file system paths. All other types of links 
    (e.g. web links) will appear as nonexistent.
- Static: find links that resolve to files but don't appear to be documents.

### Nodes: Files & Tags
Arguments that take a "`NODE`" (such as `subgraph` and `backlinks`) can be given 
either a file or a tag. Tags are given by including the hash just like their 
[file-parsed counterparts](#tag-format). The following will attempt to find the 
backlinks of the tag "my-special-tag":
`md-graph backlinks #my-special-tag`.

With option `--tag-direction=Out`, you can find the subgraph of a tag.

Everything else will be parsed as a file.

## Installation
[Follow the instructions here to install stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

```
git clone https://github.com/CharlesSchimmel/md-graph
cd md-graph
stack install
```

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
