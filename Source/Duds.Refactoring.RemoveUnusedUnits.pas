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

unit Duds.Refactoring.RemoveUnusedUnits;

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, System.Generics.Collections,

  Duds.Common.Types,
  Duds.Common.Strings,
  Duds.Common.Classes,
  Duds.Common.Files,
  Duds.Common.Language,
  Duds.Common.Log,
  Duds.Common.Interfaces,
  Duds.Common.Refactoring,
  Duds.Refactoring.FormatUsesHelper,
  Duds.Common.UnitInfo,
  Duds.Common.UsedUnitInfo,
  Duds.Scan.Model;

type
  TRemoveUnitsList = TObjectDictionary<String, TStringList>;

type
  TRemoveUnusedUnitsRefactoring = class(TObject)
  private
    FModel: TDudsModel;
    FOnLog: TLogProcedure;
    FDummyRun: Boolean;

    fFormatUsesHelper: TFormatUsesHelper;
    
    FUnitsProcessedSuccessfullyCount: Integer;
    FUsesRemovedCount: Integer;
    FUnitsProcessedFailureCount: Integer;    

  private
    procedure Log(const Msg: String; const Severity: Integer = LogInfo); overload;
    procedure Log(const Msg: String; const Args: array of const; const Severity: Integer); overload;

    procedure LoadWorkListFromInputFile(aWorklist: TRemoveUnitsList; const aPath: string);

    procedure RemoveUnitsFromPasFile(const aCurrentPasFileName: string; const aUnitsToRemove: TStringList);
    function RemoveUnitsFromUsesElements(
      aOriginalUsesList: TUsesList;
      aUnitsToRemove: TStringList): TUsesList;

  protected
    procedure RemoveUnitsFromSource(const aSourceLines, aUnitsToRemove: TStringList);

  public
    constructor Create();
    destructor Destroy(); override;

    procedure DeleteAllUnitsDefinedInUnusedFile(const aPath: string);

    property Model: TDudsModel    read FModel    write FModel;
    property DummyRun: Boolean    read FDummyRun write FDummyRun;
    property OnLog: TLogProcedure read FOnLog    write FOnLog;
  end;

implementation

procedure TRemoveUnusedUnitsRefactoring.Log(const Msg: String;
  const Severity: Integer);
begin
  if Assigned(FOnLog) then
    FOnLog('Removing unused units: ' + Msg, Severity);
end;

procedure TRemoveUnusedUnitsRefactoring.Log(const Msg: String;
  const Args: array of const; const Severity: Integer);
begin
  Log(Format(Msg, Args), Severity);
end;

procedure TRemoveUnusedUnitsRefactoring.LoadWorkListFromInputFile(aWorklist: TRemoveUnitsList; const aPath: string);

  function IsBeginOfNewPasFile(const aLine: string): boolean;
  begin
    Result := AnsiEndsStr('.PAS', UpperCase(aLine));
  end;

var
  aCurrentLine:        string;
  aTrimmedLine:  string;
  aUses:               TStringList;
  aCurrentPasFileName: string;
  aUnusedFile: TStringList;
  aExistingUsesList: TStringList;
  UsesEntry: string;
begin
  aUses  := nil;
  aUnusedFile := TStringList.Create;
  try
    aUnusedFile.LoadFromFile(aPath);

    for aCurrentLine in aUnusedFile do
    begin
      aTrimmedLine := Trim(aCurrentLine);
      if not aTrimmedLine.IsEmpty then
      begin
        if IsBeginOfNewPasFile(aTrimmedLine) then
        begin
          if Assigned(aUses) and (aUses.Count > 0) then
          begin
            // the input files may contain two removal list per unit because 
            // pascal analyzer outputs interface and implementation uses in separat sections
            // -> merge it here
            if aWorklist.ContainsKey(aCurrentPasFileName) then
            begin
              aExistingUsesList := aWorklist.Items[aCurrentPasFileName];

              for UsesEntry in aUses do
                aExistingUsesList.Add(UsesEntry);

              aUses.Free;
            end else
              aWorklist.Add(aCurrentPasFileName, aUses);
          end;

          aUses := TStringList.Create();

          aCurrentPasFileName := aTrimmedLine;
        end
        else
          if not Assigned(aUses) then
            raise Exception.Create('malformed input file, found uses without a module')
          else
            aUses.Add(aTrimmedLine);
      end;
    end;
  finally
    aUnusedFile.Free;
  end;

  if aUses.Count > 0 then
    aWorklist.Add(aCurrentPasFileName, aUses)
  else
    aUses.Free;
end;

procedure TRemoveUnusedUnitsRefactoring.RemoveUnitsFromSource(
  const aSourceLines:   TStringList;
  const aUnitsToRemove: TStringList);
var
  aLineNumberOfUses:  integer;
  aUsesList: TUsesList;
  aCleanedUsesList: TUsesList;
  aCleandUsesFormatted: string;
begin
  aLineNumberOfUses := 0;
  while aLineNumberOfUses >= 0 do // interface & implementation uses
  begin
    aLineNumberOfUses := fFormatUsesHelper.FindUsesLineInSource(aSourceLines, aLineNumberOfUses + 1);
    if aLineNumberOfUses >= 0 then
    begin

      aUsesList := fFormatUsesHelper.ExtractUsesFromSourceAndBuildUsesList(aSourceLines, aLineNumberOfUses);
      try
        fFormatUsesHelper.RemoveUsesListFromSource(aSourceLines, aLineNumberOfUses);

        aCleanedUsesList     := RemoveUnitsFromUsesElements(aUsesList, aUnitsToRemove);
        aCleandUsesFormatted := fFormatUsesHelper.UsesListToFormattedText(aCleanedUsesList);

        fFormatUsesHelper.InsertUsesListIntoSource(aSourceLines, aCleandUsesFormatted, aLineNumberOfUses);
      finally
        aUsesList.Free;
      end;

    end;
  end;
end;

procedure TRemoveUnusedUnitsRefactoring.RemoveUnitsFromPasFile(
  const aCurrentPasFileName:       string;
  const aUnitsToRemove: TStringList);
var
  aSourceLines: TStringList;
  aLineNumberOfUses:  integer;
  aFullPath:          string;
begin
  aSourceLines := TStringList.Create;
  try  
    if not FModel.SearchUnitByNameWithScopes(ExtractFilenameNoExt(aCurrentPasFileName), aFullPath, nil) then    
    begin
      Log('Unit %s not found in search path of project', [aCurrentPasFileName], LogError);
      Inc(FUnitsProcessedFailureCount);
    end
    else
    begin
      Log('Processing file "%s"', [aCurrentPasFileName], LogInfo);

      aSourceLines.LoadFromFile(aFullPath);
      RemoveUnitsFromSource(aSourceLines, aUnitsToRemove);
      aSourceLines.SaveToFile(aFullPath);

      Inc(FUnitsProcessedSuccessfullyCount);
    end;

  finally
    aSourceLines.Free;
  end;
end;

function TRemoveUnusedUnitsRefactoring.RemoveUnitsFromUsesElements(aOriginalUsesList: TUsesList; aUnitsToRemove: TStringList): TUsesList;
var
  i:     integer;
  aUsesElement: TUsesElement;
  aUnitCount: Integer;
  LastProcessedType,
  CurrentlyProcessingType: TUsesElementType;
begin
  Result := TUsesList.Create();

  // filter out units that should be removed
  for aUsesElement in aOriginalUsesList do
  begin
    if (aUsesElement.ElementType <> etUnit) or (aUnitsToRemove.IndexOf(aUsesElement.TextValue) < 0) then
      Result.Add(TUsesElement.Create(aUsesElement))
    else
      Inc(FUsesRemovedCount);  
  end;

  // Clean the uses elements list
  // -> eliminate single line comments when nothing follows after the last comment
  for i := Pred(Result.Count) downto 0 do
    if Result[i].ElementType = etSingleLineHeaderComment then
       Result.Delete(i)
    else
       break;

  // -> eliminate all consecutive single comment lines (keep the last one)
  LastProcessedType := etUnknown;
  for i := Pred(Result.Count) downto 0 do
  begin
    CurrentlyProcessingType := Result[i].ElementType;
    if (CurrentlyProcessingType = etSingleLineHeaderComment) and (LastProcessedType = etSingleLineHeaderComment) then
      Result.Delete(i);
    LastProcessedType := CurrentlyProcessingType;
  end;

  // clean list from potential comments, compiler directives etc. - if there are no units left
  aUnitCount := 0;
  for aUsesElement in Result do
    if aUsesElement.ElementType = etUnit then
      Inc(aUnitCount);

  if aUnitCount = 0 then
    Result.Clear;
end;

constructor TRemoveUnusedUnitsRefactoring.Create();
begin
  FUnitsProcessedSuccessfullyCount := 0;
  FUsesRemovedCount               := 0;
  FUnitsProcessedFailureCount     := 0;

  fFormatUsesHelper := TFormatUsesHelper.Create;
end;

destructor TRemoveUnusedUnitsRefactoring.Destroy();
begin
  fFormatUsesHelper.Free;
  inherited;
end;

procedure TRemoveUnusedUnitsRefactoring.DeleteAllUnitsDefinedInUnusedFile(const aPath: string);
var
  aWorkList: TRemoveUnitsList;
  aWorkItem: TPair<string, TStringList>;
begin
  Log('Loading input from file "%s"', [aPath], LogInfo);

  aWorkList := TObjectDictionary<String, TStringList>.Create([doOwnsValues]);
  try
    LoadWorkListFromInputFile(aWorkList, aPath);
    Log('Loaded removal list for %d units from input file', [aWorkList.Count], LogInfo);

    for aWorkItem in aWorkList do
      RemoveUnitsFromPasFile(aWorkItem.Key, aWorkItem.Value);  
  finally
    aWorkList.Free;
  end;

  Log('Process finished, %d files processed successfully - %d uses removed, %d files with errors', [FUnitsProcessedSuccessfullyCount, FUsesRemovedCount, FUnitsProcessedFailureCount], LogInfo);
end;

end.


