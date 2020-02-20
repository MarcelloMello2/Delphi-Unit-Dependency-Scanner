//------------------------------------------------------------------------------
//
// This file is part of Duds.
//
//    The software is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    The software is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    To received a copy of the GNU General Public License,
//    see <http://www.gnu.org/licenses/>.
//
// The initial developer of the original code is Dontenwill AG (www.dontenwill.de)
//
//------------------------------------------------------------------------------

unit Duds.Refactoring.PascalAnalyzerUsesReportProcessor;

interface

uses
  System.SysUtils, System.Classes;

type

  //****************************************************************************
  TDelphiModule = class(TObject)
  private
    fModuleName:  string;
    fUnusedUnits: TStringList;

  public
    procedure AddUnusedUnit(const aUnitName: string);
    function UnusedUnitsAvailable: boolean;

    constructor Create;
    destructor Destroy; override;
    function UnusedUnits: TStringList;

    property ModuleName: string read fModuleName write fModuleName;
  end;

  //****************************************************************************
  TPAUsesReportProcessor = class(TObject)
  private
    FIgnoredUnits: TStringList;

    function ReduceInputToOnlyUnnecessaryUnits(const aInput: TStringList): TStringList;
    procedure WriteToOutputFilePath(
      const aFilteredInput:  TStringList;
      const aOutputFilePath: string);
    procedure AddLastModuleIfItContainsUnusedUnits(
      const aFilteredInput: TStringList;
      const aModule:        TDelphiModule);
    function IsBeginningOfNewModule(const aCurrentLine: string): boolean;
    function ExtractModelName(const aCurrentLine: string): string;
    function IsBeginningOfUnnecessaryUnit(const aCurrentLine: string): boolean;
    function ExtractUnnecessaryUnitName(const aCurrentLine: string): string;
    function IsBeginningOfProgramm(const aCurrentLine: string): boolean;
    function HasInitialization(const aCurrentLine: string): boolean;

    procedure LoadIgnoredUnits(aIgnoreUnitsFilePath: string);

  public
    constructor Create;

    function FileAsStringList(const aFilePath: string): TStringList;

    class procedure ParseOutputFileAndWriteFiltered(
      const aInputFilePath:  string;
      const aOutputFilePath: string;
      const aIgnoreUnitsFilePath: string);
  end;

implementation

function TPAUsesReportProcessor.ReduceInputToOnlyUnnecessaryUnits(const aInput: TStringList): TStringList;
var
  aCurrentLine: string;
  aModule:      TDelphiModule;
  i:            integer;
  aUnitName:    string;
begin
  aModule := nil;
  Result  := TStringList.Create;

  for i := 0 to aInput.Count - 1 do
  begin
    aCurrentLine := aInput.Strings[i];
    if IsBeginningOfNewModule(aCurrentLine) or IsBeginningOfProgramm(aCurrentLine) then
    begin
      AddLastModuleIfItContainsUnusedUnits(Result, aModule);

      if IsBeginningOfNewModule(aCurrentLine) then // do not start a module when we see the "Program"
      begin
        aModule            := TDelphiModule.Create();
        aModule.ModuleName := ExtractModelName(aCurrentLine);
      end else
        aModule := nil;
    end
    else if (aModule <> nil) and IsBeginningOfUnnecessaryUnit(aCurrentLine) and not HasInitialization(aCurrentLine) then
    begin
      aUnitName := ExtractUnnecessaryUnitName(aCurrentLine);
      if not aUnitName.IsEmpty then
        if FIgnoredUnits.IndexOf(aUnitName) = -1 then
          aModule.AddUnusedUnit(aUnitName);
    end;
  end;
end;

procedure TPAUsesReportProcessor.WriteToOutputFilePath(
  const aFilteredInput:  TStringList;
  const aOutputFilePath: string);
var
  i:              integer;
  aCurrentModule: TDelphiModule;
  aToWrite:       TStringList;
begin
  aToWrite := TStringList.Create();
  try
    for i := 0 to aFilteredInput.Count - 1 do
    begin
      aCurrentModule := TDelphiModule(aFilteredInput.Objects[i]);
      aToWrite.Add(aCurrentModule.ModuleName + '.pas');
      aToWrite.AddStrings(aCurrentModule.UnusedUnits);
      aToWrite.Add(#13#10);
    end;

    aToWrite.SaveToFile(aOutputFilePath);
  finally
    aToWrite.Free;
  end;
end;

procedure TPAUsesReportProcessor.AddLastModuleIfItContainsUnusedUnits(
  const aFilteredInput: TStringList;
  const aModule:        TDelphiModule);
begin
  if Assigned(aModule) then
  begin
    if aModule.UnusedUnitsAvailable then
      aFilteredInput.AddObject(aModule.ModuleName, aModule)
    else
      aModule.Free;
  end;
end;

function TPAUsesReportProcessor.IsBeginningOfNewModule(const aCurrentLine: string): boolean;
const
  MODULE_BEGINNING = 'Module';
begin
  Result := Copy(aCurrentLine, 0, Length(MODULE_BEGINNING)) = MODULE_BEGINNING;
end;

constructor TPAUsesReportProcessor.Create;
begin
  FIgnoredUnits := TStringList.Create;
end;


procedure TPAUsesReportProcessor.LoadIgnoredUnits(aIgnoreUnitsFilePath: string);
begin
  if FileExists(aIgnoreUnitsFilePath) then
    FIgnoredUnits.LoadFromFile(aIgnoreUnitsFilePath);
end;

function TPAUsesReportProcessor.ExtractModelName(const aCurrentLine: string): string;
const
  MODELNAME_BEGINNING = 8;
  USES_KEYWORD        = ' ';
begin
  Result := Copy(aCurrentLine, MODELNAME_BEGINNING, MaxInt);
  Result := Copy(Result, 0, Pos(USES_KEYWORD, Result) - 1);
end;

function TPAUsesReportProcessor.IsBeginningOfUnnecessaryUnit(const aCurrentLine: string): boolean;
const
  UNNECESSARY_ARROW = '  ==>';
begin
  Result := Copy(aCurrentLine, 0, Length(UNNECESSARY_ARROW)) = UNNECESSARY_ARROW;
end;

function TPAUsesReportProcessor.HasInitialization(const aCurrentLine: string): boolean;
const
  HAS_INITILIAZATION = 'has initialization';
var
  aPos: integer;
begin
  aPos   := Pos(HAS_INITILIAZATION, aCurrentLine);
  Result := (aPos > 1)
    and (aPos + Length(HAS_INITILIAZATION) < Length(aCurrentLine))
    and (aCurrentLine[aPos - 1] in ['(', ','])
    and (aCurrentLine[aPos + Length(HAS_INITILIAZATION)] in [')', ',']);
end;

function TPAUsesReportProcessor.ExtractUnnecessaryUnitName(const aCurrentLine: string): string;
const
  UNNECESSARY_UNITNAME_BEGINNING = 7;
  UNNECESSARY_UNITNAME_ENDING    = ' ';
begin
  Result := Copy(aCurrentLine, UNNECESSARY_UNITNAME_BEGINNING, MaxInt);
  Result := Copy(Result, 0, Pos(UNNECESSARY_UNITNAME_ENDING, Result) - 1);
end;

function TPAUsesReportProcessor.FileAsStringList(const aFilePath: string): TStringList;
begin
  Result := TStringList.Create;
  Result.LoadFromFile(aFilePath);
end;

class procedure TPAUsesReportProcessor.ParseOutputFileAndWriteFiltered(
  const aInputFilePath:  string;
  const aOutputFilePath: string;
  const aIgnoreUnitsFilePath: string);
var
  aFilteredInput:   TStringList;
  aInput:           TStringList;
  aPalOutputFilter: TPAUsesReportProcessor;
begin
  aPalOutputFilter := TPAUsesReportProcessor.Create();
  try
    aPalOutputFilter.LoadIgnoredUnits(aIgnoreUnitsFilePath);

    aInput := aPalOutputFilter.FileAsStringList(aInputFilePath);
    try
      aFilteredInput := aPalOutputFilter.ReduceInputToOnlyUnnecessaryUnits(aInput);
      try
        aPalOutputFilter.WriteToOutputFilePath(aFilteredInput, aOutputFilePath);
      finally
        aFilteredInput.Free;
      end;
    finally
      aInput.Free;
    end;
  finally
    aPalOutputFilter.Free;
  end;
end;

function TPAUsesReportProcessor.IsBeginningOfProgramm(const aCurrentLine: string): boolean;
const
  PROGRAM_BEGINNING = 'Program';
begin
  Result := Copy(aCurrentLine, 0, Length(PROGRAM_BEGINNING)) = PROGRAM_BEGINNING;
end;

procedure TDelphiModule.AddUnusedUnit(const aUnitName: string);
begin
  fUnusedUnits.Add(aUnitName);
end;

function TDelphiModule.UnusedUnitsAvailable: boolean;
begin
  Result := fUnusedUnits.Count > 0;
end;

constructor TDelphiModule.Create;
begin
  inherited;

  fUnusedUnits := TStringList.Create();
end;

destructor TDelphiModule.Destroy;
begin
  fUnusedUnits.Free;
  inherited;
end;

function TDelphiModule.UnusedUnits: TStringList;
begin
  Result := fUnusedUnits;
end;

end.
