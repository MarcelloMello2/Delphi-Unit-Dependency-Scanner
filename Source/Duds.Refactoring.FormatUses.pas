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

unit Duds.Refactoring.FormatUses;

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
  Duds.Scan.Model,
  Duds.Refactoring.FormatUsesHelper;
  
type
  TFormatUsesRefactoring = class
  private
    FModel: TDudsModel;
    FOnLog: TLogProcedure;
    FDummyRun: Boolean;

    fFormatUsesHelper: TFormatUsesHelper;

  private
    procedure Log(const Msg: String; const Severity: Integer = LogInfo); overload;
    procedure Log(const Msg: String; const Args: array of const; const Severity: Integer); overload;

  protected
    procedure FormatUsesInSource(const aSourceLines: TStringList);

  public
    constructor Create();
    destructor Destroy(); override;

    procedure FormatUsesInFile(const aDelphiUnitName: string);

    property Model: TDudsModel    read FModel    write FModel;
    property DummyRun: Boolean    read FDummyRun write FDummyRun;
    property OnLog: TLogProcedure read FOnLog    write FOnLog;
  end;


implementation

{ TFormatUsesRefactoring }

constructor TFormatUsesRefactoring.Create;
begin

  fFormatUsesHelper := TFormatUsesHelper.Create;
end;

destructor TFormatUsesRefactoring.Destroy();
begin
  fFormatUsesHelper.Free;
  inherited;
end;

procedure TFormatUsesRefactoring.Log(const Msg: String; const Severity: Integer);
begin
  if Assigned(FOnLog) then
    FOnLog('Removing unused units: ' + Msg, Severity);
end;

procedure TFormatUsesRefactoring.Log(const Msg: String; const Args: array of const; const Severity: Integer);
begin
  Log(Format(Msg, Args), Severity);
end;

procedure TFormatUsesRefactoring.FormatUsesInSource(const aSourceLines: TStringList);
var
  aLineNumberOfUses:  integer;
  aUsesList:          TUsesList;
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
        fFormatUsesHelper.InsertUsesListIntoSource(aSourceLines, aUsesList, aLineNumberOfUses);
      finally
        aUsesList.Free;
      end;
    end;
  end;
end;

procedure TFormatUsesRefactoring.FormatUsesInFile(const aDelphiUnitName: string);
var
  aSourceLines: TStringList;
  aFullPath:    string;
begin
  aSourceLines := TStringList.Create;
  try
    if not FModel.SearchUnitByNameWithScopes(aDelphiUnitName, aFullPath, nil) then
    begin
      Log('Unit %s not found in search path of project', [aDelphiUnitName], LogError);
    end
    else
    begin
      Log('Formatting uses of file "%s"', [aDelphiUnitName], LogInfo);

      aSourceLines.LoadFromFile(aFullPath);
      FormatUsesInSource(aSourceLines);
      if not FDummyRun then
      begin
        aSourceLines.SaveToFile(aFullPath);
        Log('Formatting uses of file "%s" DONE', [aDelphiUnitName], LogInfo);
      end
      else
        Log('DUMMY RUN - nothing changed', LogWarning);
    end;

  finally
    aSourceLines.Free;
  end;
end;

end.


