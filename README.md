
# Delphi Unit Dependency Scanner

The Delphi Unit Dependency Scanner (or DUDS as it has become known) parses a Delphi project or group project and builds a unit file hierarchy.

## Analysis Features

The uses hierarchy can be displayed in several different ways, searched and sorted.

The unit relationships can be exported in several formats - XML, Gephi CSV and GraphML.

## Refactoring Features

### Renaming of Units

It is possible to rename units

- one by one
- using regex expressions
- using a batch import from csv

### Extract Code from an existing unit to a new / other unit

DUDS does NOT help to extract the code - but it helps to adjust the uses lists. This is especially helpful when the change affects a lot of files (e.g. > 500) and changes must be made to units of different packages & projects. DUDS can add your target unit (new unit or existing unit) to the uses list of all files that already referenced the unit you extracted your code from.

![add unit to uses list](/Docs/media/addUnitToUses.png?raw=true)

### Remove unused units

You can use the output of a Pascal Analyzer (© [Peganza](https://www.peganza.com/)) `Uses` report to batch delete unused units.

## History

See Docs\Version History.txt

## Source

This project was originally developen by Paul Spencer Thornton and the repository was cloned from [norgepaul/DUDS](https://github.com/norgepaul/DUDS). For more history, see there.

## Parsing & Compiler Switches

Parsing of the uses lists does not respect compiler switches. This is useful for renaming across multiple projects with different switches set and not considered as a bug.
Parsing is "handmade" and implemented to only recognize uses lists. Using a tokenizer/parser like [DelphiAST](https://github.com/RomanYankovsky/DelphiAST) would leed to better compatibility / less parsing errors but it would also take away the advantage of "ignoring compiler switches".

## Requirements

DUDS is built with Delphi 10.3.3. The following 3rd party libraries are required - they can be installed via GetIt.

- VirtualTreeView
- SynEdit

## Usage

Ready to use executable is located in `bin\Win32\Release\DependencyScanner.exe`.
