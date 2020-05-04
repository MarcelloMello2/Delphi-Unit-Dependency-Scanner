unit duds.analyzer.ModulesAnalyzer;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  System.JSON, IOUtils,

  duds.common.Interfaces,
  duds.common.Language,
  duds.common.Types,
  duds.common.Log,
  duds.analyzer.model,
  duds.common.Classes,
  duds.common.modules, duds.analyzer.CustomAnalyzer, duds.common.Utils;

type
  TModulesAnalyzer = class(TCustomAnalyzer)
  private
    procedure AnalyzeAllowedDependencies;
    procedure AnalyzeActualDependencies;

  protected
    function GetLogAreaName: string; override;

  public
    procedure MapUnitsToModules;
    procedure Execute;

  end;

implementation

{ TModulesAnalyzer }

function TModulesAnalyzer.GetLogAreaName: string;
begin
  Result := 'Modules Analysis';
end;

procedure TModulesAnalyzer.MapUnitsToModules;
var
  aDelphiFile: TDelphiFile;
  i: integer;
  aModule: TModule;
  UnitsTotal, UnitsMappedToModules: Integer;
begin
  fModel.Modules.ClearModulesAnalysisData;
  fModel.Modules.ReBuildFastSearchList;
  fModel.Modules.CreateUnknownModule;

  UnitsTotal := 0;
  UnitsMappedToModules := 0;
  for i := 0 to pred(fModel.DelphiFileList.Count) do
  begin
    if fCancelled then
      break;

    aDelphiFile := fModel.DelphiFileList[i];
    if (aDelphiFile.UnitInfo.DelphiFileType = ftPAS) or (not aDelphiFile.InSearchPath) then  // files that are not in search path do not get a filetype but should be '.pas'...
    begin
      Inc(UnitsTotal);

      // try to find a matching module
      if fModel.Modules.FindModuleForUnit(aDelphiFile.UnitInfo, aModule) then
        Inc(UnitsMappedToModules)
      else
        aModule := fModel.Modules.UnknownModule;

      aDelphiFile.UnitInfo.Module := aModule;

      // update the modules analysis data
      aModule.AnalysisData.NumberOfFiles := aModule.AnalysisData.NumberOfFiles + 1;
      if not aDelphiFile.InSearchPath then
        aModule.AnalysisData.NumberOfFilesNotInPath := aModule.AnalysisData.NumberOfFilesNotInPath + 1;
      aModule.AnalysisData.LinesOfCode   := aModule.AnalysisData.LinesOfCode   + aDelphiFile.UnitInfo.LinesOfCode;
    end;
  end;

  Log(StrModulesIdentified, [FormatCardinal(UnitsMappedToModules), FormatCardinal(UnitsTotal)], LogInfo);
end;

procedure TModulesAnalyzer.AnalyzeAllowedDependencies;
var
  CurrentModule: TModule;

  procedure RecursivelyAddImplicitDependencies(Dependency: TModule);
  var
    ImplicitlyDefinedDependency: TModule;
  begin
    for ImplicitlyDefinedDependency in Dependency.DefinedDependencies do
      if not CurrentModule.AllowedDependencies.Contains(ImplicitlyDefinedDependency) then
      begin
        CurrentModule.AllowedDependencies.Add(ImplicitlyDefinedDependency);
        RecursivelyAddImplicitDependencies(ImplicitlyDefinedDependency);
      end;
  end;

var
  DefinedDependency: TModule;
begin
  if fCancelled then
    exit;

  // TODO: Can cyclic module dependencies happen at all? In the module definition
  //       file, they can't since it uses a "one-pass-scan" to read it in and will
  //       fail a "dependency" cannot be resolved.

  for CurrentModule in fModel.Modules.OrderedModules do
  begin
    // add implicit dependencies
    for DefinedDependency in CurrentModule.DefinedDependencies do
      RecursivelyAddImplicitDependencies(DefinedDependency);

    // add explicit dependencies (do this after implicit to be able to warn for redundant definitions)
    for DefinedDependency in CurrentModule.DefinedDependencies do
      if CurrentModule.AllowedDependencies.Contains(DefinedDependency) then
        Log('Module "%s" defines a dependency to "%s". ' +
            'You can remove this from the module definition since this dependency is anyway implicitly allowed.',
            [CurrentModule.Name, DefinedDependency.Name], LogWarning)
      else
        CurrentModule.AllowedDependencies.Add(DefinedDependency);
  end;

end;

procedure TModulesAnalyzer.AnalyzeActualDependencies;
var
  CurrentDelphiFile: TDelphiFile;
  CurrentModule: TModule;
  UsedDelphiFile: TDelphiFile;
  UsedModule: TModule;
  UsedUnit: IUsedUnitInfo;
  IsDelphiToDelphi: Boolean;
  Is3rdPartyToDelphi: Boolean;
  Is3rdPartyTo3rdParty: Boolean;
  LogLevel: Integer;
begin
  // iterate all parsed units
  //   count cominations: "unit-module --> used-unit-module"

  for CurrentDelphiFile in fModel.DelphiFileList do
  begin
    if CurrentDelphiFile.InSearchPath then
    begin
      CurrentModule := CurrentDelphiFile.UnitInfo.Module;
      if Assigned(CurrentModule) then
      begin
        for UsedUnit in CurrentDelphiFile.UnitInfo.UsedUnits do
        begin

          // TODO ~ start ~ Reference to TDelphiFile should be saved while initial scanning - this could also be used by the formatter
          UsedDelphiFile := fModel.FindParsedDelphiUnit(UsedUnit.DelphiUnitName, fProjectSettings.UnitScopeNames);
          if Assigned(UsedDelphiFile) then
            UsedModule := UsedDelphiFile.UnitInfo.Module
          else
            raise Exception.CreateFmt('Could not find a parsed delphi file for uses "%s" of unit "%s"', [UsedUnit.DelphiUnitName, CurrentDelphiFile.UnitInfo.DelphiUnitName]); // TODO: can this ever happen?
          // TODO ~ End ~


          if CurrentModule <> UsedModule then
            if not CurrentModule.IsUnknownModule then // don't report "unknown module" => "known module"
            begin
              IsDelphiToDelphi     := (CurrentModule.Origin = moDelphi) and (UsedModule.Origin = moDelphi);
              Is3rdPartyToDelphi   := (CurrentModule.Origin = mo3rdParty) and (UsedModule.Origin = moDelphi);
              Is3rdPartyTo3rdParty := (CurrentModule.Origin = mo3rdParty) and (UsedModule.Origin = mo3rdParty);

              if UsedModule.IsUnknownModule then
                LogLevel := LogWarning
              else
                LogLevel := LogError;

              if LogLevel = LogError then
                if not IsDelphiToDelphi and not Is3rdPartyToDelphi and not Is3rdPartyTo3rdParty then
                  if not CurrentModule.AllowedDependencies.Contains(UsedModule) then
                    Log('Dependency violation: Module "%s" ==> "%s"; Unit "%s" ==> "%s"',
                         [CurrentModule.Name, UsedModule.Name, CurrentDelphiFile.UnitInfo.DelphiUnitName, UsedDelphiFile.UnitInfo.DelphiUnitName], LogLevel);
            end;

          // TODO
  //        CurrentModule.AddActualDependency(CurrentDelphiFile, UsedModule, UsedDelphiFile);


        end;
      end;
    end;
  end;

end;

procedure TModulesAnalyzer.Execute;
begin
  // step 1: Identify "allowed dependencies" for all modules
  AnalyzeAllowedDependencies;

  // step 2: Identify "actual dependencies" between modules (may contain illegal dependencies)
  // Idea: Calc "strength" = number of units using
  AnalyzeActualDependencies;

  // step 3: Reporting
  //  - "illegal dependencies"
  //  - "defined but not used dependencies"
  // TODO

end;



end.
