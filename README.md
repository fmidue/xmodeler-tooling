# XModeler Tooling

## Installation

Download and install the following tools (while making sure their executables end up on the system `PATH`):

- [Haskell Tool Stack](https://haskellstack.org/)
- [Graphviz](https://graphviz.org/)

Download the code of the [xmodeler-tooling repository](https://github.com/fmidue/xmodeler-tooling), e.g., via `git clone https://github.com/fmidue/xmodeler-tooling.git` or per "Download ZIP".

Then, in a shell:
```shell
cd xmodeler-tooling
stack build
```
This can take quite a while, but you only need to do it once.
(And even if you later do it a second time, for example after a `git pull` to get any code updates from the repository, it should usually be much faster since the Haskell compiler and library dependencies will already be on your system from the first run.)

## Usage (any time after installation)

In a shell, to generate XML files to be loaded into XModeler:
```shell
cd xmodeler-tooling
stack exec generateRandomMLMs
```
The user prompts and outputs are hopefully somewhat self-explanatory.


In a shell, to read and check validity of an XML file exported from XModeler:
```shell
cd xmodeler-tooling
stack exec checkValidityOfMLM filename.xml
```


In a shell, to read-relayout-write an XModeler-XML file:
```shell
cd xmodeler-tooling
stack exec relayoutAnMLM filename.xml
```
