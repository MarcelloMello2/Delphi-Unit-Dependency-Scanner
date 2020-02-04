# Delphi Unit Dependency Scanner
The Delphi Unit Dependency Scanner (or DUDS as it has become known) parses a Delphi project or group project and builds a unit file hierarchy. The hierarchy can be displayed in several different ways, searched and sorted. It is also possible to rename units
- one by one 
- using regex expressions
- using a batch import from csv.

The unit relationships can be exported in several formats - XML, Gephi CSV and GraphML.

# History
See Docs\Version History.txt

# Source
This project was originally developen by Paul Spencer Thornton and the repository was cloned from https://github.com/norgepaul/DUDS. For more history, see there.

# Parsing
Parsing of the uses lists does not respect compiler switches. This is useful for renaming across multiple projects with different switches set and not considered as a bug.

# Requirements
DUDS is built with Delphi 10.3.3. The following 3rd party libraries are required - they can be installed via GetIt.
* VirtualTreeView
* SynEdit

# Usage
Ready to use executable is located in `bin\Win32\Release\DependencyScanner.exe`.