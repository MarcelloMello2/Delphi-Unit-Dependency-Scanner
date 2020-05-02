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

unit duds.refactoring.FormatUses;

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, System.Generics.Collections,

  duds.common.Types,
  duds.common.Strings,
  duds.common.Classes,
  duds.common.Files,
  duds.common.Language,
  duds.common.Log,
  duds.common.Interfaces,
  duds.common.Refactoring,
  duds.common.UnitInfo,
  duds.common.UsedUnitInfo,
  duds.analyzer.model,
  duds.refactoring.FormatUsesHelper;
  
type
  TFormatUsesRefactoring = class
  private
    FModel: TDudsModel;
    FOnLog: TLogProcedure;
    FDummyRun: Boolean;
    fProjectSettings: TProjectSettings;

    fFormatUsesHelper: TFormatUsesHelper;
    fAllowModuleGroupingWithUnknownModules: Boolean;

  private

    procedure Log(const Msg: String; const Severity: Integer = LogInfo); overload;
    procedure Log(const Msg: String; const Args: array of const; const Severity: Integer); overload;
    procedure GroupUsesByModules(aUsesList: TUsesList);

  protected
    procedure FormatUsesInSource(const aSourceLines: TStringList; UseModulesForGrouping: boolean);

  public
    constructor Create();
    destructor Destroy(); override;

    procedure FormatUsesInFile(const aDelphiUnitName: string);

    property Model: TDudsModel    read FModel    write FModel;
    property DummyRun: Boolean    read FDummyRun write FDummyRun;
    property OnLog: TLogProcedure read FOnLog    write FOnLog;
    property ProjectSettings: TProjectSettings read fProjectSettings write fProjectSettings;

    property AllowModuleGroupingWithUnknownModules: Boolean read fAllowModuleGroupingWithUnknownModules write fAllowModuleGroupingWithUnknownModules;
  end;


implementation

{ TFormatUsesRefactoring }

constructor TFormatUsesRefactoring.Create;
begin

  fFormatUsesHelper := TFormatUsesHelper.Create;
  fAllowModuleGroupingWithUnknownModules := false;
end;

destructor TFormatUsesRefactoring.Destroy();
begin
  fFormatUsesHelper.Free;
  inherited;
end;

procedure TFormatUsesRefactoring.Log(const Msg: String; const Severity: Integer);
begin
  if Assigned(FOnLog) then
    FOnLog('Formatting uses: ' + Msg, Severity);
end;

procedure TFormatUsesRefactoring.Log(const Msg: String; const Args: array of const; const Severity: Integer);
begin
  Log(Format(Msg, Args), Severity);
end;                                 

procedure TFormatUsesRefactoring.GroupUsesByModules(aUsesList: TUsesList);
var
  aUsesElement: TUsesElement;
  aUsesListBuf: TUsesList;
  aCurrentModuleName: string;
  aLastModuleName: string;  
  i: integer;
  aDelphiFile: TDelphiFile;
  Module: TModule;
  UnitsWithUnknownModules: string;
  aUnitScopes: TStringList;
begin
  // pre-checks
  if aUsesList.ContainsCompilerSwitches then
    if Assigned(FOnLog) then
    begin
      Log(StrUsesFormattingNotAvailWithCompilerSwitches, LogWarning);
      exit;
    end
    else
      raise Exception.Create(StrUsesFormattingNotAvailWithCompilerSwitches);

  // identify modules for each unit
  for aUsesElement in aUsesList do
    if aUsesElement.ElementType = etUnit then
    begin
      aUnitScopes := nil;
      if fProjectSettings <> nil then
        aUnitScopes := fProjectSettings.UnitScopeNames;
      aDelphiFile := fModel.FindParsedDelphiUnit(aUsesElement.TextValue, aUnitScopes);
      if aDelphiFile = nil then
         raise Exception.CreateFmt('could not find a parsed delphi file for uses list element "%s"', [aUsesElement.TextValue]);

      aUsesElement.Module := aDelphiFile.UnitInfo.Module;
    end;

  // any unknown modules?
  UnitsWithUnknownModules := '';
  for aUsesElement in aUsesList do
    if aUsesElement.ElementType = etUnit then
      if aUsesElement.Module = FModel.Modules.UnknownModule then
        AddToken(UnitsWithUnknownModules, aUsesElement.TextValue);
  if not UnitsWithUnknownModules.IsEmpty then
    if fAllowModuleGroupingWithUnknownModules then
      Log(StrUsesFormattingUnknownModulesWarning, [UnitsWithUnknownModules], LogWarning)
    else
      if Assigned(FOnLog) then
      begin
        Log(StrUsesFormattingNotAvailUnknownModules, [UnitsWithUnknownModules], LogWarning);
        exit;
      end
      else
        raise Exception.CreateFmt(StrUsesFormattingNotAvailUnknownModules, [UnitsWithUnknownModules]);

  // step 1: delete all existing single line comments headers, we will produce new ones
  for i:= Pred(aUsesList.Count) downto 0 do
  begin  
    aUsesElement := aUsesList[i];
    if aUsesElement.ElementType = etSingleLineHeaderComment then
      aUsesList.Delete(i);
  end;

  // security self-check: there should be only units in the list now
  for aUsesElement in aUsesList do
    if aUsesElement.ElementType <> etUnit then
       raise Exception.Create('there should be only units in the list here - algorithm expects that');

  // step 2: order uses by modules - the order is derived from the json file
  aUsesListBuf := TUsesList.Create;
  try
    // 2.1 copy to a buffer list
    for aUsesElement in aUsesList do
      aUsesListBuf.Add(TUsesElement.Create(aUsesElement));

    // 2.2 iterate all known(!) modules and insert all units from the buffer list into the orginal list
    aUsesList.Clear;
    for Module in FModel.Modules.OrderedModules  do
    begin
      if Module <> FModel.Modules.UnknownModule then
      begin
        i := 0;
        while i < aUsesListBuf.Count do
        begin
          if aUsesListBuf[i].Module = Module then
          begin
            aUsesList.Add(TUsesElement.Create(aUsesListBuf[i]));
            aUsesListBuf.Delete(i);
          end else
            Inc(i);
        end;
      end;
    end;

    // 2.3 take over left-over units, must be due to "unknown module"
    if fAllowModuleGroupingWithUnknownModules then
    begin
      for aUsesElement in aUsesListBuf do
        aUsesList.Add(TUsesElement.Create(aUsesElement));
    end else
      // security self-check: buffer must be empty now
      if aUsesListBuf.Count > 0 then
        raise Exception.Create('not all uses elements were transferred to the new list - internal error!');

  finally
    aUsesListBuf.Free;
  end;

  // step 3: insert module group headers
  aLastModuleName := '';
  i := 0;
  while i < aUsesList.Count do
  begin
    aUsesElement       := aUsesList[i];
       
    aDelphiFile := fModel.FindParsedDelphiUnit(aUsesElement.TextValue, nil); // TOOD: use scopes here!
    if aDelphiFile = nil then
       raise Exception.CreateFmt('could not find a parsed delphi file for uses list element "%s"', [aUsesElement.TextValue]);    

    if Assigned(aDelphiFile.UnitInfo.Module) then
      aCurrentModuleName := aDelphiFile.UnitInfo.Module.Name
    else
      raise Exception.CreateFmt('no module definition for file %s - internal error!?', [aDelphiFile.UnitInfo.DelphiUnitName]);
         
    if aCurrentModuleName <> aLastModuleName then
    begin
       aUsesList.Insert(i, TUsesElement.Create(etSingleLineHeaderComment, '// ' + aCurrentModuleName));
       Inc(i);
       aLastModuleName := aCurrentModuleName;
    end;

    Inc(i);
  end;

end;

procedure TFormatUsesRefactoring.FormatUsesInSource(const aSourceLines: TStringList; UseModulesForGrouping: boolean);
var
  aLineNumberOfUses:  integer;
  aUsesList:          TUsesList;
  aUsesFormatted:     string;
begin
  if UseModulesForGrouping and not Assigned(FModel) then  
    raise Exception.Create('formatting can not use grouping when there is no model provided');

  aLineNumberOfUses := 0;
  while aLineNumberOfUses >= 0 do // interface & implementation uses
  begin
    aLineNumberOfUses := fFormatUsesHelper.FindUsesLineInSource(aSourceLines, aLineNumberOfUses + 1);
    if aLineNumberOfUses >= 0 then
    begin
      aUsesList := fFormatUsesHelper.ExtractUsesFromSourceAndBuildUsesList(aSourceLines, aLineNumberOfUses);
      try
        fFormatUsesHelper.RemoveUsesListFromSource(aSourceLines, aLineNumberOfUses);

        if UseModulesForGrouping then
          GroupUsesByModules(aUsesList);
        
        aUsesFormatted := fFormatUsesHelper.UsesListToFormattedText(aUsesList);
        
        fFormatUsesHelper.InsertUsesListIntoSource(aSourceLines, aUsesFormatted, aLineNumberOfUses);
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
      FormatUsesInSource(aSourceLines, true);
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


