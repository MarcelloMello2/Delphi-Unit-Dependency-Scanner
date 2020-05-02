unit duds.common.UsesParser.test;

interface

uses
  System.SysUtils, System.Classes, Generics.Collections, IOUtils,

  DUnitX.TestFramework,
  DUnitX.Assert,

  duds.common.Types,
  duds.common.UsesParser,
  duds.common.Interfaces,
  duds.common.UnitInfo,
  duds.common.UsedUnitInfo;

implementation

type

  [TestFixture]
  TUsesParserTest = class(TObject)
  private
    procedure GetUnits(code: string; var UnitInfo: IUnitInfo; UnitFileName: string
        = '');
    procedure CheckUsedUnits(UnitInfo: IUnitInfo; aExpectedUsesNames: array of
        string; aExpectedDefinedFilePaths: array of string;
        aExpectedAbsoluteFilePaths: array of string; aExpectedUsesTypes: array of
        TUsedUnitType);


  public

    [Test]
    procedure FileTypesAndUnitNames;

    [Test]
    procedure CountLinesWhileParsingForUses;

    [Test]
    procedure SimpleProgram;

    [Test]
    procedure ProgramWithPaths;

    [Test]
    procedure ProgramWithPaths_CheckPosition;

    [Test]
    procedure SimpleUnit;

    [Test]
    procedure SimpleUnitWithDottyName;

    [Test]
    procedure SimpleUnitWithDottyNamesUsingKeywords;

    [Test]
    procedure DottyNamesAndSwitches;

    [Test]
    procedure UnitWithSwitches;

    [Test]
    procedure UnitWithDoubledBodyBySwitches;

  end;


{ TUsesParserTest }

procedure TUsesParserTest.GetUnits(code: string; var UnitInfo: IUnitInfo; UnitFileName: string = '');
var
  aUsesParser : TUsesParser;
begin
  aUsesParser := TUsesParser.Create;
  try
    aUsesParser.GetUsedUnitsFromSource(UnitFileName, code, UnitInfo);
  finally
    aUsesParser.Free;
  end;
end;

procedure TUsesParserTest.FileTypesAndUnitNames;
var
  UnitInfo: IUnitInfo;
begin
  GetUnits('unit myUnit;', UnitInfo);
  Assert.AreEqual(ftPAS, UnitInfo.DelphiFileType, 'file type');
  Assert.AreEqual('myUnit', UnitInfo.DelphiUnitName, 'unit name');

  GetUnits('program myProject;', UnitInfo);
  Assert.AreEqual(ftDPR, UnitInfo.DelphiFileType, 'file type');
  Assert.AreEqual('myProject', UnitInfo.DelphiUnitName, 'unit name');

  GetUnits('library myDll;', UnitInfo);
  Assert.AreEqual(ftLIB, UnitInfo.DelphiFileType, 'file type');
  Assert.AreEqual('myDll', UnitInfo.DelphiUnitName, 'unit name');

  GetUnits('package myPackage;', UnitInfo);
  Assert.AreEqual(ftDPK, UnitInfo.DelphiFileType, 'file type');
  Assert.AreEqual('myPackage', UnitInfo.DelphiUnitName, 'unit name');
end;

procedure TUsesParserTest.CountLinesWhileParsingForUses;

  function ParseUnitsAndGetLinesOfCode(code: string): integer;
  var
    aUsesParser : TUsesParser;
    UnitInfo: IUnitInfo;
  begin
    aUsesParser := TUsesParser.Create;
    try
      aUsesParser.GetUsedUnitsFromSource('', code, UnitInfo);
      Result := aUsesParser.LinesOfCode;
    finally
      aUsesParser.Free;
    end;
  end;

var
  code: string;
begin
  code := '';
  Assert.AreEqual(0, ParseUnitsAndGetLinesOfCode(code), 'lines of code');

  // no line breaks
  code := 'program myProgram; '                    +
          'uses '                                  +
          '  unit1 in ''unit1.pas''           ,  ' +
          '  unit2 in ''..\unit2.pas''        ,  ' +
          '  unit3 in ''C:\temp\unit3.pas''   ;  ' +
          'end.';
  Assert.AreEqual(1, ParseUnitsAndGetLinesOfCode(code), 'lines of code');

  // normal line breaks, no comments, every line should count
  code := 'program myProgram; '                    + sLineBreak +
          'uses '                                  + sLineBreak +
          '  unit1 in ''unit1.pas''           ,  ' + sLineBreak +
          '  unit2 in ''..\unit2.pas''        ,  ' + sLineBreak +
          '  unit3 in ''C:\temp\unit3.pas''   ;  ' + sLineBreak +
          'end.';
  Assert.AreEqual(6, ParseUnitsAndGetLinesOfCode(code), 'lines of code');

  // normal line breaks, comments & empty lines
  code := { 1} 'program myProgram; '            + sLineBreak +
          { 2} 'uses '                          + sLineBreak +
          {  } '    '                           + sLineBreak + // <- empty line, do NOT count
          { 3} '  unit1,              '         + sLineBreak +
          {  } '  // only comment in line'      + sLineBreak + // <- only comment, do NOT count
          {  } '{$IFDEF DELPHI16}'              + sLineBreak + // <- switch only, do NOT count
          { 4} '  unit2, '                      + sLineBreak +
          {  } '{$ENDIF}'                       + sLineBreak + // <- switch only, do NOT count
          { 5} '  unit3, '                      + sLineBreak +
          { 6} '  unit4; // comment after code' + sLineBreak + // <- code & comment, count it!
          { 7} 'type               '            + sLineBreak +
          { 8} '  MyClass = class  '            + sLineBreak +
          {  } '  { multi '                     + sLineBreak + // <- comment only, do NOT count
          {  } '    line '                      + sLineBreak + // <- comment only, do NOT count
          { 9} '    comment } end; '            + sLineBreak + // <- comment & code, do NOT count
          {10} 'end.'                           + sLineBreak +
          {  } '';                                             // <- empty line after end of unit, do NOT count
  Assert.AreEqual(10, ParseUnitsAndGetLinesOfCode(code), 'lines of code');
end;

procedure TUsesParserTest.CheckUsedUnits(UnitInfo: IUnitInfo; aExpectedUsesNames: array of string;
  aExpectedDefinedFilePaths: array of string; aExpectedAbsoluteFilePaths: array of string; aExpectedUsesTypes: array of TUsedUnitType);
var
  UsedUnit: IUsedUnitInfo;
  i: Integer;
begin
  if Length(aExpectedUsesNames) <> Length(aExpectedUsesTypes) then
    raise Exception.Create('wrong usage - expected arrays should have the same length');

  if Length(aExpectedDefinedFilePaths) > 0 then
    if Length(aExpectedDefinedFilePaths) <> Length(aExpectedUsesNames) then
      raise Exception.Create('wrong usage - expected paths arrays must either be empty or have the same length as the names array');

  if Length(aExpectedAbsoluteFilePaths) > 0 then
    if Length(aExpectedAbsoluteFilePaths) <> Length(aExpectedUsesNames) then
      raise Exception.Create('wrong usage - expected absolute paths arrays must either be empty or have the same length as the names array');

  Assert.AreEqual(Length(aExpectedUsesNames), UnitInfo.UsedUnits.Count, 'number of used units');

  for i := 0 to Length(aExpectedUsesNames) - 1 do
  begin
    UsedUnit := UnitInfo.UsedUnits[i];
    Assert.AreEqual(aExpectedUsesTypes[i], UsedUnit.UsesType,       'uses type');
    Assert.AreEqual(aExpectedUsesNames[i], UsedUnit.DelphiUnitName, 'delphi unit name');

    if Length(aExpectedDefinedFilePaths) > 0 then
      Assert.AreEqual(aExpectedDefinedFilePaths[i], UsedUnit.DefinedFilePath,  'defined file path');

    if Length(aExpectedAbsoluteFilePaths) > 0 then
      Assert.AreEqual(aExpectedAbsoluteFilePaths[i], UsedUnit.Filename,  'defined absolute file path');
  end;
end;

procedure TUsesParserTest.SimpleProgram;
const
  code = 'program myProgram; uses Classes; end.';
var
  UnitInfo: IUnitInfo;
begin
  GetUnits(code, UnitInfo);
  CheckUsedUnits(UnitInfo, ['Classes'], [], [], [utImplementation]);
end;

procedure TUsesParserTest.ProgramWithPaths;
const
  code = 'program myProgram; ' +
         'uses ' +
         '  unit1 in ''unit1.pas''           ,  ' +
         '  unit2 in ''..\unit2.pas''        ,  ' +
         '  unit3 in ''C:\temp\unit3.pas''   ;  ' +
         'end.';

  folder_base     = 'C:\folder_src\program\';
  folder_src      = folder_base + 'src\';
  programFileName = folder_src + 'myProgram.dpr';
var
  UnitInfo: IUnitInfo;
begin
  GetUnits(code, UnitInfo, programFileName);
  CheckUsedUnits(UnitInfo,
    ['unit1',                  'unit2',                      'unit3'],
    ['unit1.pas',              '..\unit2.pas',               'C:\temp\unit3.pas'],
    [folder_src + 'unit1.pas', folder_base + 'unit2.pas',    'C:\temp\unit3.pas'],
    [utImplementation,         utImplementation,             utImplementation]);
end;

procedure TUsesParserTest.ProgramWithPaths_CheckPosition;
const
  code = 'program myProgram; uses my.dotty.utils in ''C:\temp\my.dotty.utils.pas''; end.';
  {                               ^                           ^
                                  |                           |
                                  .Position                   .InFilePosition
  }

var
  UnitInfo: IUnitInfo;
  UsedUnit: IUsedUnitInfo;
begin
  GetUnits(code, UnitInfo, '');

  Assert.AreEqual(1, UnitInfo.UsedUnits.Count);

  UsedUnit := UnitInfo.UsedUnits.First;
  Assert.AreEqual('my.dotty.utils', UsedUnit.DelphiUnitName);
  Assert.AreEqual(24,      UsedUnit.Position,       'position of the unit');
  Assert.AreEqual(24 + 27, UsedUnit.InFilePosition, 'position of the unit in the path');
end;

procedure TUsesParserTest.SimpleUnit;
const
  code = 'unit myUnit; ' +
         'interface ' +
         '  uses Classes; ' +
         'implementation ' +
         '  uses Windows; ' +
         'end.';
var
  UnitInfo: IUnitInfo;
begin
  GetUnits(code, UnitInfo);
  CheckUsedUnits(UnitInfo, ['Classes', 'Windows'], [], [], [utInterface, utImplementation]);
end;

procedure TUsesParserTest.SimpleUnitWithDottyName;
const
  code = 'unit my.Dotty.Utils; ' +
         'interface ' +
         '  uses System.SysUtils, System.Classes;' +
         '  type MyType = class(SuperClass)' +
         '  end;' +
         'end.';
var
  UnitInfo: IUnitInfo;
begin
  GetUnits(code, UnitInfo);
  Assert.AreEqual('my.Dotty.Utils', UnitInfo.DelphiUnitName);
  CheckUsedUnits(UnitInfo, ['System.SysUtils', 'System.Classes'], [], [], [utInterface, utInterface]);
end;

procedure TUsesParserTest.SimpleUnitWithDottyNamesUsingKeywords;
const
  code = 'unit my.Dotty.Export.Utils; ' +
         'interface ' +
         '  uses Duds.Common.Helper.IniFile, Duds.Export.Gephi;' + // 'Helper' & 'Export' are Keywords
         'implementation' +
         'end.';
var
  UnitInfo: IUnitInfo;
begin
  GetUnits(code, UnitInfo);
  Assert.AreEqual('my.Dotty.Export.Utils', UnitInfo.DelphiUnitName);
  CheckUsedUnits(UnitInfo, ['Duds.Common.Helper.IniFile', 'Duds.Export.Gephi'], [], [], [utInterface, utInterface]);
end;

procedure TUsesParserTest.UnitWithSwitches;
const
  code = 'unit myUnit; ' +
         'interface ' +
         '  {$IFDEF ABC} ' +
         '  uses Classes; ' +        // <- interface uses are doubled, both parts should be covered
         '  {$ELSE} ' +
         '  uses SuperClasses; ' +   // <- ...
         '  {$ENDIF} ' +
         'implementation ' +
         '  uses Windows; ' +
         'end.';
var
  UnitInfo: IUnitInfo;
begin
  GetUnits(code, UnitInfo);
  CheckUsedUnits(UnitInfo, ['Classes', 'SuperClasses', 'Windows'], [], [], [utInterface, utInterface, utImplementation]);
end;

procedure TUsesParserTest.DottyNamesAndSwitches;
const
code =
  'unit myGraphics;'        + sLineBreak +
  ''                        + sLineBreak +
  'interface'               + sLineBreak +
  ''                        + sLineBreak +
  'uses'                    + sLineBreak +
  '{$IFDEF DELPHI16}'       + sLineBreak +
  '  System.UITypes'        + sLineBreak +  // <- this test would fail, when lexer.UseDefines would be activated
  '  // this is a comment ' + sLineBreak +
  '{$ENDIF}'                + sLineBreak +
  '  ;';
var
  UnitInfo: IUnitInfo;
begin
  GetUnits(code, UnitInfo);
  CheckUsedUnits(UnitInfo, ['System.UITypes'], [], [], [utInterface]);

  Assert.AreEqual(5, UnitInfo.LinesOfCode);
end;

procedure TUsesParserTest.UnitWithDoubledBodyBySwitches;
const
  code = 'unit myUnit; ' +
         '  {$IFDEF ABC} ' +          // <- unit "body" is doubled due to (weird) usage of switches...
         '    interface ' +
         '      uses Classes1; ' +
         '    implementation ' +
         '      uses Windows; ' +
         '  {$ELSE} ' +               // <- ... both parts should be detected with correct usage types (interface / implementation)
         '    interface ' +
         '      uses Classes2; ' +
         '    implementation ' +
         '      uses Windows; ' +
         '  {$ENDIF} ' +
         'end.';
var
  UnitInfo: IUnitInfo;
begin
  GetUnits(code, UnitInfo);
  CheckUsedUnits(UnitInfo, ['Classes1', 'Windows', 'Classes2', 'Windows'], [], [], [utInterface, utImplementation, utInterface, utImplementation]);
end;

initialization

TDUnitX.RegisterTestFixture(TUsesParserTest);

end.
