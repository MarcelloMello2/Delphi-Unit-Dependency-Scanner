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

unit Duds.Refactoring.FormatUsesHelper;

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
  TUsesElementType = (
    etUnknown,
    etBEGIN,
    etUnit,
    etCompilerDirectiveStart,
    etCompilerDirectiveEnd,
    etSingleLineComment,
    etEND);

  TUsesElement = class
  private
    fElementType: TUsesElementType;
    fTextValue: string;
  public
    constructor Create(ElementType: TUsesElementType; TextValue: string); overload;
    constructor Create(CopyFrom: TUsesElement); overload;

    property ElementType: TUsesElementType read fElementType write fElementType;
    property TextValue: string             read fTextValue   write fTextValue;
  end;

  TUsesList = TObjectList<TUsesElement>;

  // this class has no state, all methods are independent
  TFormatUsesHelper = class(TObject)
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

    function IsCompilerDirective(const aCandidate: string): boolean;
    function IsCompilerDirectiveEnd(const aCandidate: string): boolean;
    function IsSingleLineComment(const aCandidate: string): boolean;
    function IsLastLineOfUses(const aCurrentUsesLine: string): boolean;

    function StripAwayTrailingSingleLineComment(const aLine: string): string;


    function UsesListToFormattedText(const UsesList: TUsesList): string;

    function GetElementType(const Element: string): TUsesElementType;

  public


    function FindUsesLineInSource(
      const aSource: TStringList;
      const aStart:  integer = 0): integer;

    function ExtractUsesFromSourceAndBuildUsesList(
      const aSourceLines: TStringList;
      const aLineOfUsesBegin:  integer): TUsesList;

    procedure RemoveUsesListFromSource(
      const aSource:           TStringList;
      const aLineNumberOfUses: integer);

    procedure InsertUsesListIntoSource(
      const aSource: TStringList;
      const aUsesList: TUsesList;
      const aLineNumberOfUses:         integer);

  end;

implementation

{ TUsesElement }

constructor TUsesElement.Create(ElementType: TUsesElementType; TextValue: string);
begin
  fElementType := ElementType;
  fTextValue   := TextValue;
end;

constructor TUsesElement.Create(CopyFrom: TUsesElement);
begin
  fElementType := CopyFrom.ElementType;
  fTextValue   := CopyFrom.TextValue;
end;

{ TFormatUsesHelper }

function TFormatUsesHelper.FindUsesLineInSource(
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
      aLine := StripAwayTrailingSingleLineComment(aLine);
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

function TFormatUsesHelper.IsLastLineOfUses(const aCurrentUsesLine: string): boolean;
begin
  Result := AnsiEndsStr(CHAR_SEMICOLON, StripAwayTrailingSingleLineComment(aCurrentUsesLine).TrimRight);
end;

function TFormatUsesHelper.StripAwayTrailingSingleLineComment(const aLine: string): string;
var
  aPosOfCommentBegin: Integer;
begin
  if aLine.IsEmpty then
    Result := ''
  else
  begin
    aPosOfCommentBegin := Pos('//', aLine.TrimLeft);
    if aPosOfCommentBegin > 1 then // only strip, of there is content before the comment
      Result := aLine.Split(['/', '/'])[0].TrimRight
    else
      Result := aLine;
  end;
end;

function TFormatUsesHelper.ExtractUsesFromSourceAndBuildUsesList(
  const aSourceLines: TStringList;
  const aLineOfUsesBegin: integer): TUsesList;
var
  i:                 integer;
  aCurrentUsesLine:  string;
  aIsLastLineOfUses: boolean;
  aSplittedUsesLine: TStringList;
  j:                 integer;
  aUsedUnit:         string;
begin
  Result := TUsesList.Create;


  for i := aLineOfUsesBegin to aSourceLines.Count - 1 do
  begin
    aCurrentUsesLine := Trim(aSourceLines[i]);

    // in the first line, delete the 'uses' keyword
    if i = aLineOfUsesBegin then
      if aCurrentUsesLine.StartsWith(KEYWORD_USES, true) then
      begin
        aCurrentUsesLine := Trim(Copy(aCurrentUsesLine, Length(KEYWORD_USES) + 1, MaxInt));
        if aCurrentUsesLine.IsEmpty then
          Continue;
      end;

    // line has SingleLineComment at the end -> strip it away, we don't support that. Only {} comments are supported for "annotations".
    // Why? Because SingleLineComment are considered as "module group headers"
    aCurrentUsesLine := StripAwayTrailingSingleLineComment(aCurrentUsesLine);

    aIsLastLineOfUses := IsLastLineOfUses(aCurrentUsesLine);
    if aIsLastLineOfUses then
      aCurrentUsesLine := StringReplace(aCurrentUsesLine, CHAR_SEMICOLON, '', [rfReplaceAll]);

    if IsSingleLineComment(aCurrentUsesLine) then
      Result.Add(TUsesElement.Create(etSingleLineComment, aCurrentUsesLine))
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
            Result.Add(TUsesElement.Create(GetElementType(aUsedUnit), aUsedUnit));    // TODO: detect {}-style comments
        end;
      finally
        aSplittedUsesLine.Free;
      end;
    end;

    if aIsLastLineOfUses then
      break;
  end;
end;

procedure TFormatUsesHelper.RemoveUsesListFromSource(
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

procedure TFormatUsesHelper.InsertUsesListIntoSource(
  const aSource: TStringList;
  const aUsesList: TUsesList;
  const aLineNumberOfUses:         integer);
var
  aFirstLineAfterUsesKeyword: integer;
  aCompactUses:               string;
begin
  aCompactUses := UsesListToFormattedText(aUsesList);
  if aCompactUses <> '' then
  begin
    aFirstLineAfterUsesKeyword := aLineNumberOfUses + 1;
    aSource.Insert(aLineNumberOfUses, 'uses');
    aSource.Insert(aFirstLineAfterUsesKeyword, aCompactUses);
  end;
end;

function TFormatUsesHelper.IsCompilerDirective(const aCandidate: string): boolean;
begin
  Result := AnsiStartsText('{$', aCandidate.TrimLeft);
end;

function TFormatUsesHelper.IsCompilerDirectiveEnd(const aCandidate: string): boolean;
begin
  Result := AnsiStartsText('{$ENDIF}', aCandidate.TrimLeft);
end;

function TFormatUsesHelper.IsSingleLineComment(const aCandidate: string): boolean;
begin
  Result := AnsiStartsText('//', aCandidate.TrimLeft);
end;

function TFormatUsesHelper.GetElementType(const Element: string): TUsesElementType;
begin
  if IsCompilerDirective(Element) then
  begin
     if IsCompilerDirectiveEnd(Element) then
       Result := etCompilerDirectiveEnd
     else
       Result := etCompilerDirectiveStart;
  end
  else
    if IsSingleLineComment(Element) then
       Result := etSingleLineComment
    else
       Result := etUnit;
end;

function TFormatUsesHelper.UsesListToFormattedText(const UsesList: TUsesList): string;
var
  AfterFirstUnit: Boolean;
  UnitWithoutCompilerSwitchIsBefore: Boolean;
  LastInsertedUnitHasSeparator: Boolean;
  CurrentCompilerSwitchLevel: Integer;

  function CalcCurrentLineLength(aCompleteText: String): Integer;
  var
    aPosOfLastLineBreak: integer;
    aLastLine: string;
  begin
     aPosOfLastLineBreak := LastDelimiter(sLineBreak, aCompleteText);
     aLastLine           := Copy(aCompleteText, aPosOfLastLineBreak + 1, MaxInt);
     Result              := Length(aLastLine);
  end;

  procedure HandleElementChange(LastType, CurrentType: TUsesElementType; CurrentElement: string);
  var
    Buf: String;
    aCurrentLineLength: Integer;
  begin
    Buf := '';

    if CurrentType = etSingleLineComment then
      raise Exception.Create('that should not happen');

    case CurrentType of
      etCompilerDirectiveStart: Inc(CurrentCompilerSwitchLevel);
      etCompilerDirectiveEnd:   Dec(CurrentCompilerSwitchLevel);
    end;

    case LastType of
      etBEGIN:
        case CurrentType of
          etUnit:              Buf := INDENT_OF_TWO;
        end;

      etUnit:
        case CurrentType of
          etUnit:
                                   begin
                                     Buf := UNIT_SEPERATOR;
                                     LastInsertedUnitHasSeparator := true;
                                     aCurrentLineLength := CalcCurrentLineLength(Result);
                                     if aCurrentLineLength + Length(CurrentElement) > MAX_LINE_LENGTH then
                                       Buf := Buf + sLineBreak + INDENT_OF_TWO
                                     else
                                       Buf := Buf + SPACE_BETWEEN_UNITS;
                                   end;
          etCompilerDirectiveStart:
                                   begin
                                     LastInsertedUnitHasSeparator := false;
                                     Buf := Buf + sLineBreak;
                                   end;
          etCompilerDirectiveEnd:
                                   begin
                                     if not UnitWithoutCompilerSwitchIsBefore then
                                     begin
                                       Buf := Buf + UNIT_SEPERATOR;
                                       LastInsertedUnitHasSeparator := true;
                                     end
                                     else
                                       LastInsertedUnitHasSeparator := false;
                                     Buf := Buf + sLineBreak;
                                   end;

          etEND:                   Buf := '';
        end;


      etCompilerDirectiveStart:
        case CurrentType of
          etUnit:                   begin
                                      Buf := sLineBreak + INDENT_OF_TWO;
                                      if AfterFirstUnit then
                                         Buf := Buf + UNIT_SEPERATOR + SPACE_BETWEEN_UNITS;
                                    end;
          etCompilerDirectiveStart: Buf := sLineBreak;
          etCompilerDirectiveEnd:   Buf := sLineBreak;
          etEND:                    Buf := sLineBreak + INDENT_OF_TWO;
        end;

      etCompilerDirectiveEnd:
        case CurrentType of
          etUnit:                   begin
                                      Buf := sLineBreak + INDENT_OF_TWO;
                                      if AfterFirstUnit and not LastInsertedUnitHasSeparator then
                                         Buf := Buf + UNIT_SEPERATOR + SPACE_BETWEEN_UNITS;
                                    end;
          etCompilerDirectiveStart: Buf := sLineBreak;
          etCompilerDirectiveEnd:   Buf := sLineBreak;
          etEND:                    Buf := sLineBreak + INDENT_OF_TWO;
        end;

      etSingleLineComment:
        case CurrentType of
          etUnit:                   Buf := sLineBreak + INDENT_OF_TWO;
          etCompilerDirectiveStart: Buf := sLineBreak;
          etCompilerDirectiveEnd:   Buf := sLineBreak;
          etEND:                    Buf := sLineBreak + INDENT_OF_TWO;
        end;

    end;
    Result := Result + Buf;
  end;

  procedure HandleCurrentElement(CurrentType: TUsesElementType; CurrentElement: string);
  var
    Buf: String;
  begin
    case CurrentType of
      etUnit:                   begin
                                  Buf := CurrentElement;
                                  AfterFirstUnit := true;
                                  if CurrentCompilerSwitchLevel = 0 then
                                    UnitWithoutCompilerSwitchIsBefore := true;
                                end;
      etCompilerDirectiveStart: Buf := CurrentElement;
      etCompilerDirectiveEnd:   Buf := CurrentElement;
      etSingleLineComment:      Buf := CurrentElement;
      etEND:                    Buf := CHAR_SEMICOLON;
    end;
    Result := Result + Buf;
  end;

  function CalcNextNonSingleLineCommentAhead(aUsesList: TUsesList; aCurrentPosition: Integer): TUsesElementType;
  var
    i: Integer;
  begin
    Result := etEnd;
    for i := aCurrentPosition to Pred(aUsesList.Count) do
      if aUsesList[i].ElementType <> etSingleLineComment then
      begin
        Result := aUsesList[i].ElementType;
        break;
      end;
  end;

var
  CurrentElement: string;
  CurrentType: TUsesElementType;
  NextNonCommentElementAhead: TUsesElementType;
  LastType: TUsesElementType;
  i: Integer;
begin
  Result := '';
  if UsesList.Count = 0 then
    exit;

  AfterFirstUnit               := false;
  UnitWithoutCompilerSwitchIsBefore := false;
  LastInsertedUnitHasSeparator := false;
  LastType                     := etBEGIN;
  CurrentCompilerSwitchLevel   := 0;

  // build the unit list text from the elements list
  i := 0;
  while i <= Pred(UsesList.Count) do
  begin
    CurrentElement             := UsesList[i].TextValue;
    CurrentType                := UsesList[i].ElementType;

    if CurrentType = etSingleLineComment then
    begin
      NextNonCommentElementAhead := CalcNextNonSingleLineCommentAhead(UsesList, i);
      if LastType <> etBEGIN then
      begin
        HandleElementChange(LastType, NextNonCommentElementAhead, '');
        Result := Result.TrimRight;
      end;

      case LastType of
        etBEGIN:
          Result := Result + INDENT_OF_TWO;
        etUnit:
          Result := Result + sLineBreak + sLineBreak + INDENT_OF_TWO;
        etCompilerDirectiveStart:
          Result := Result + sLineBreak + INDENT_OF_TWO;
        etCompilerDirectiveEnd:
          Result := Result + sLineBreak + INDENT_OF_TWO;
      end;

    end
    else
      HandleElementChange(LastType, CurrentType, CurrentElement);

    HandleCurrentElement(CurrentType, CurrentElement);

    LastType := CurrentType;
    Inc(i);
  end;

  CurrentType := etEND;
  HandleElementChange(LastType, CurrentType, '');
  HandleCurrentElement(CurrentType, CurrentElement);
end;

end.


