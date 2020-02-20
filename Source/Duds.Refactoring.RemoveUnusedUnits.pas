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
  Duds.Common.Parser.Pascal,
  Duds.Scan.Model;

type
  UsesListElementType = (etUnknown, etBEGIN, etUnit, etCompilerDirective, etSingleLineComment, etEND);

  TRemoveUnitsList = TObjectDictionary<String, TStringList>;

type
  TRemoveUnusedUnitsRefactoring = class(TObject)
  private
    FModel: TDudsModel;
    FOnLog: TLogProcedure;
    FDummyRun: Boolean;
    
    FUnitsProcessedSuccessfullyCount: Integer;
    FUsesRemovedCount: Integer;
    FUnitsProcessedFailureCount: Integer;    

  private const
    KEYWORD_USES            = 'USES';
    CHAR_SEMICOLON          = ';';
    INDENT_OF_ONE           = ' ';
    INDENT_OF_TWO           = '  ';
    UNIT_SEPERATOR          = ',';
    SPACE_BETWEEN_UNITS     = INDENT_OF_ONE;
    COMPILERDIRECTIVE_START = '{$';
    MAX_LINE_LENGTH         = 80;

  private

    procedure Log(const Msg: String; const Severity: Integer = LogInfo); overload;
    procedure Log(const Msg: String; const Args: array of const; const Severity: Integer); overload;
    

    procedure LoadWorkListFromInputFile(aWorklist: TRemoveUnitsList; const aPath: string);
    function IsBeginOfNewPasFile(const aLine: string): boolean;

    procedure RemoveUnitsFromPasFile(
      const aCurrentPasFileName:       string;
      const aUnitsToRemoveFromPasFile: TStringList);
    function FindUsesLine(
      const aSource: TStringList;
      const aStart:  integer = 0): integer;
    procedure InsertNewUsesList(
      const aSource, aCleanedUsesOnly: TStringList;
      const aLineNumberOfUses:         integer);
    function IsCompilerDirective(const aCandidate: string): boolean;
    function IsSingleLineComment(const aCandidate: string): boolean;
    function IsLastLineOfUses(const aCurrentUsesLine: string): boolean;
    function LoadUsesFromFileContent(
      const aUnmodifiedFileContent: TStringList;
      const aLineOfUsesBegin:       integer): TStringList;
    procedure RemoveOldUsesList(
      const aSource:           TStringList;
      const aLineNumberOfUses: integer);
    function RemoveUnitsFromUsesList(const aOriginalUsesList, aUnitsToRemove: TStringList): TStringList;
    procedure ReplaceUsesByCleanedUses(
      const aSource, aCleanedUsesOnly: TStringList;
      const aLineNumberOfUses:         integer);
    function GetElementType(const Element: string): UsesListElementType;
    function CompactAndFormattedList(const CleanedUses: TStringList): string;
  public
    constructor Create();
    destructor Destroy(); override;

    procedure DeleteAllUnitsDefinedInUnusedFile(const aPath: string);

    property Model: TDudsModel read FModel write FModel;
    property DummyRun: Boolean read FDummyRun write FDummyRun;
    property OnLog: TLogProcedure read FOnLog write FOnLog;
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

function TRemoveUnusedUnitsRefactoring.IsBeginOfNewPasFile(const aLine: string): boolean;
begin
  Result := AnsiEndsStr('.PAS', UpperCase(aLine));
end;

procedure TRemoveUnusedUnitsRefactoring.RemoveUnitsFromPasFile(
  const aCurrentPasFileName:       string;
  const aUnitsToRemoveFromPasFile: TStringList);
var
  aSourceFileContent: TStringList;
  aLineNumberOfUses:  integer;
  aUsesOnly:          TStringList;
  aFullPath:          string;
  aCleanedUsesOnly:   TStringList;
begin
  aSourceFileContent := TStringList.Create;
  try  
    if not FModel.SearchUnitByNameWithScopes(ExtractFilenameNoExt(aCurrentPasFileName), aFullPath, nil) then    
    begin
      Log('Unit %s not found in search path of project', [aCurrentPasFileName], LogError);
      Inc(FUnitsProcessedFailureCount);
    end
    else
    begin
      Log('Processing file "%s"', [aCurrentPasFileName], LogInfo);
      aLineNumberOfUses := 0;
      while aLineNumberOfUses >= 0 do // interface & implementation uses
      begin
        aSourceFileContent.LoadFromFile(aFullPath);
        aLineNumberOfUses := FindUsesLine(aSourceFileContent, aLineNumberOfUses + 1);
        if aLineNumberOfUses >= 0 then
        begin
          aUsesOnly := LoadUsesFromFileContent(aSourceFileContent, aLineNumberOfUses);
          try
            aCleanedUsesOnly := RemoveUnitsFromUsesList(aUsesOnly, aUnitsToRemoveFromPasFile);
            ReplaceUsesByCleanedUses(aSourceFileContent, aCleanedUsesOnly, aLineNumberOfUses);
            aSourceFileContent.SaveToFile(aFullPath);
          finally
            aUsesOnly.Free;
          end;
        end;
      end;
      Inc(FUnitsProcessedSuccessfullyCount);
    end;

  finally
    aSourceFileContent.Free;
  end;
end;

function TRemoveUnusedUnitsRefactoring.FindUsesLine(
  const aSource: TStringList;
  const aStart:  integer = 0): integer;
var
  aLineNumber:  integer;
  aLine:        string;
  aPos:         integer;
  aLength:      integer;
  aSpaceBefore: boolean;
  aSpaceAfter:  boolean;
begin
  Result  := -1;
  aLength := Length(KEYWORD_USES);

  for aLineNumber := aStart to aSource.Count - 1 do
  begin
    aLine := aSource[aLineNumber];
    if not IsSingleLineComment(aLine) then
    begin
      aPos  := Pos(KEYWORD_USES, aLine.ToUpper);
      if aPos > 0 then
      begin
        aSpaceBefore := (aPos = 1) or (aLine[aPos - 1] in [' ', #9, #13, #10]);
        aSpaceAfter  := (aPos + aLength > Length(aLine)) or (aLine[aPos + aLength] in [' ', #9, #13, #10]);
        if aSpaceBefore and aSpaceAfter then
        begin
          Result := aLineNumber;
          break;
        end;
      end;
    end;
  end;
end;

function TRemoveUnusedUnitsRefactoring.IsLastLineOfUses(const aCurrentUsesLine: string): boolean;
begin
  Result := AnsiEndsStr(CHAR_SEMICOLON, aCurrentUsesLine);
end;

function TRemoveUnusedUnitsRefactoring.LoadUsesFromFileContent(
  const aUnmodifiedFileContent: TStringList;
  const aLineOfUsesBegin:       integer): TStringList;
var
  i:                 integer;
  aCurrentUsesLine:  string;
  aIsLastLineOfUses: boolean;
  aSplittedUsesLine: TStringList;
  j:                 integer;
  aUsedUnit:         string;
begin
  Result := TStringList.Create;

  for i := aLineOfUsesBegin + 1 to aUnmodifiedFileContent.Count - 1 do
  begin
    aCurrentUsesLine := Trim(aUnmodifiedFileContent[i]);

    if not IsCompilerDirective(aCurrentUsesLine) and not IsSingleLineComment(aCurrentUsesLine) then
      aCurrentUsesLine := StringReplace(aCurrentUsesLine, ' ', '', [rfReplaceAll]);

    aIsLastLineOfUses := IsLastLineOfUses(aCurrentUsesLine);
    if aIsLastLineOfUses then
      aCurrentUsesLine := StringReplace(aCurrentUsesLine, CHAR_SEMICOLON, '', [rfReplaceAll]);

    if IsSingleLineComment(aCurrentUsesLine) then
      Result.Add(aCurrentUsesLine)
    else begin
      aSplittedUsesLine := TStringList.Create();
      try
        aSplittedUsesLine.Delimiter       := ',';
        aSplittedUsesLine.StrictDelimiter := True;
        aSplittedUsesLine.DelimitedText   := aCurrentUsesLine;

        for j := 0 to aSplittedUsesLine.Count - 1 do
        begin
          aUsedUnit := Trim(aSplittedUsesLine[j]);
          if (aUsedUnit <> '') then
            Result.Add(aUsedUnit);
        end;
      finally
        aSplittedUsesLine.Free;
      end;
    end;

    if aIsLastLineOfUses then
      break;
  end;
end;

function TRemoveUnusedUnitsRefactoring.RemoveUnitsFromUsesList(const aOriginalUsesList, aUnitsToRemove: TStringList): TStringList;
var
  i:            integer;
  aCurrentUses: string;
begin
  Result := TStringList.Create();

  for i := 0 to aOriginalUsesList.Count - 1 do
  begin
    aCurrentUses := Trim(aOriginalUsesList[i]);
    if aUnitsToRemove.IndexOf(aCurrentUses) < 0 then
      Result.Add(aCurrentUses)
    else
      Inc(FUsesRemovedCount);  
  end;
end;

procedure TRemoveUnusedUnitsRefactoring.ReplaceUsesByCleanedUses(
  const aSource, aCleanedUsesOnly: TStringList;
  const aLineNumberOfUses:         integer);
begin
  RemoveOldUsesList(aSource, aLineNumberOfUses);
  InsertNewUsesList(aSource, aCleanedUsesOnly, aLineNumberOfUses);
end;

procedure TRemoveUnusedUnitsRefactoring.RemoveOldUsesList(
  const aSource:           TStringList;
  const aLineNumberOfUses: integer);
var
  aIsLastLineOfUses: boolean;
begin
  while True do
  begin
    aIsLastLineOfUses := IsLastLineOfUses(aSource[aLineNumberOfUses]);
    aSource.Delete(aLineNumberOfUses);

    if aIsLastLineOfUses then
      break;
  end;
end;

procedure TRemoveUnusedUnitsRefactoring.InsertNewUsesList(
  const aSource, aCleanedUsesOnly: TStringList;
  const aLineNumberOfUses:         integer);
var
  aFirstLineAfterUsesKeyword: integer;
  aCompactUses:               string;
begin
  aCompactUses := CompactAndFormattedList(aCleanedUsesOnly);
  if aCompactUses <> '' then
  begin
    aFirstLineAfterUsesKeyword := aLineNumberOfUses + 1;
    aSource.Insert(aLineNumberOfUses, 'uses');
    aSource.Insert(aFirstLineAfterUsesKeyword, aCompactUses);
  end;
end;

function TRemoveUnusedUnitsRefactoring.IsCompilerDirective(const aCandidate: string): boolean;
begin
  Result := AnsiStartsText('{$', aCandidate);
end;

function TRemoveUnusedUnitsRefactoring.IsSingleLineComment(const aCandidate: string): boolean;
begin
  Result := AnsiStartsText('//', aCandidate);
end;

constructor TRemoveUnusedUnitsRefactoring.Create();
begin
  FUnitsProcessedSuccessfullyCount := 0;
  FUsesRemovedCount               := 0;
  FUnitsProcessedFailureCount     := 0;
end;

destructor TRemoveUnusedUnitsRefactoring.Destroy();
begin
  inherited;
end;

function TRemoveUnusedUnitsRefactoring.GetElementType(const Element: string): UsesListElementType;
begin
  if IsCompilerDirective(Element) then
    Result := etCompilerDirective
  else
    if IsSingleLineComment(Element) then
       Result := etSingleLineComment
    else
       Result := etUnit;
end;

function TRemoveUnusedUnitsRefactoring.CompactAndFormattedList(const CleanedUses: TStringList): string;

  function CalcCurrentLineLength(aCompleteText: String): Integer;
  var
    aPosOfLastLineBreak: integer;
    aLastLine: string;
  begin
     aPosOfLastLineBreak := LastDelimiter(sLineBreak, aCompleteText);
     aLastLine           := Copy(aCompleteText, aPosOfLastLineBreak + 1, MaxInt);
     Result              := Length(aLastLine);
  end;

  procedure HandleElementChange(LastType, CurrentType: UsesListElementType; CurrentElement: string);
  var
    Buf: String;
    aCurrentLineLength: Integer;
  begin
    Buf := '';
    case LastType of
      etBEGIN:
        Buf := INDENT_OF_TWO;

      etUnit:
        case CurrentType of
          etUnit:
                               begin
                                 aCurrentLineLength := CalcCurrentLineLength(Result);
                                 Buf := UNIT_SEPERATOR;
                                 if aCurrentLineLength + Length(CurrentElement) > MAX_LINE_LENGTH then
                                   Buf := Buf + sLineBreak + INDENT_OF_TWO
                                 else
                                   Buf := Buf + SPACE_BETWEEN_UNITS;
                               end;
          etCompilerDirective: Buf := sLineBreak + INDENT_OF_TWO;
          etSingleLineComment: Buf := UNIT_SEPERATOR + sLineBreak + sLineBreak + INDENT_OF_TWO;
          etEND:               Buf := '';
        end;

      etCompilerDirective:
        case CurrentType of
          etUnit:              Buf := sLineBreak + INDENT_OF_TWO + UNIT_SEPERATOR + SPACE_BETWEEN_UNITS;
          etCompilerDirective: Buf := sLineBreak + INDENT_OF_TWO;
          etSingleLineComment: Buf := sLineBreak + INDENT_OF_TWO;
          etEND:               Buf := sLineBreak + INDENT_OF_TWO;
        end;

      etSingleLineComment:
        case CurrentType of
          etUnit:              Buf := sLineBreak + INDENT_OF_TWO;
          etCompilerDirective: Buf := sLineBreak + INDENT_OF_TWO;
          etSingleLineComment: Buf := sLineBreak + INDENT_OF_TWO;
          etEND:               Buf := sLineBreak + INDENT_OF_TWO;
        end;

    end;
    Result := Result + Buf;
  end;

  procedure HandleCurrentElement(CurrentType: UsesListElementType; CurrentElement: string);
  var
    Buf: String;
  begin
    case CurrentType of
      etUnit:              Buf := CurrentElement;
      etCompilerDirective: Buf := CurrentElement;
      etSingleLineComment: Buf := CurrentElement;
      etEND:               Buf := CHAR_SEMICOLON;
    end;
    Result := Result + Buf;
  end;

var
  CurrentElement: string;
  CurrentType: UsesListElementType;
  LastType: UsesListElementType;
  UsesCount: Integer;

  LastProcessedType: UsesListElementType;
  CurrentlyProcessingType:  UsesListElementType;
  i : integer;
begin
  Result := '';

  // Clean the uses elements list
  // -> eliminate single comment lines when nothing follows
  for i := Pred(CleanedUses.Count) downto 0 do
    if GetElementType(CleanedUses[i]) = etSingleLineComment then
       CleanedUses.Delete(i)
    else
       break;   

  // -> eliminate all consecutive single comment lines (keep the last one)
  LastProcessedType := etUnknown;
  for i := Pred(CleanedUses.Count) downto 0 do  
  begin
    CurrentlyProcessingType := GetElementType(CleanedUses[i]);
    if (CurrentlyProcessingType = etSingleLineComment) and (LastProcessedType = etSingleLineComment) then
      CleanedUses.Delete(i);
    LastProcessedType := CurrentlyProcessingType;
  end;  

  // are there units left after the cleaning?
  UsesCount := 0;
  for CurrentElement in CleanedUses do
  begin
    CurrentType := GetElementType(CurrentElement);
    if CurrentType = etUnit then
      Inc(UsesCount);      
  end;           
  
  if UsesCount > 0 then
  begin
    LastType    := etBEGIN;

    // build the unit list text from the elements list
    for CurrentElement in CleanedUses do
    begin
      CurrentType := GetElementType(CurrentElement);
      HandleElementChange(LastType, CurrentType, CurrentElement);
      HandleCurrentElement(CurrentType, CurrentElement);
      LastType := CurrentType;
    end;
    CurrentType := etEND;
    HandleElementChange(LastType, CurrentType, '');
    HandleCurrentElement(CurrentType, CurrentElement);
  end;
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


