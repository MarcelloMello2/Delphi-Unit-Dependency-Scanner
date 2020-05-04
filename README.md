
# Delphi Unit Dependency Scanner

The Delphi Unit Dependency Scanner (or DUDS as it has become known) parses a Delphi project or group project and builds a unit file hierarchy.

## Analysis Features

### Uses dependencies

The uses hierarchy can be displayed in several different ways, searched and sorted. Cycles are marked in the tree-view.

Unit relationships can be exported in several formats - XML, Gephi CSV and GraphML.

### Module dependencies

You can define modules (list of units or directory-paths) and allowed module dependencies. Actual module dependencies will be analyzed and dependency violations will be reported to the log.

By exporting modules to ".graphml" you can use a graphML-enabled tool like [yEd](https://www.yworks.com/products/yed) to get an overview of your modules architecture.

![modules export in yEd](/Docs/media/duds-project.modules.png?raw=true)

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

This project was originally developed by Paul Spencer Thornton and the repository was cloned from [norgepaul/DUDS](https://github.com/norgepaul/DUDS). For more history, see there.

## Parsing & Compiler Switches

Parsing of the uses lists does not respect compiler switches. This is useful for renaming across multiple projects with different switches set and not considered as a bug.
Parsing uses the lexer (NOT the parser) from [DelphiAST](https://github.com/RomanYankovsky/DelphiAST).

## Requirements

DUDS is built with Delphi 10.3.3. The following 3rd party libraries are required - they can be installed via GetIt.

- VirtualTreeView
- SynEdit

3rd party code from DelphiAST is included in the repository.

## Usage

Ready to use executable is located in `src\build\bin\Win32\Release`.
