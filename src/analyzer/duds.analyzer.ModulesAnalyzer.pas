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
    procedure MapUnitsToModules;
    procedure AnalyzeAllowedDependencies;
    procedure AnalyzeActualDependencies;

  protected
    function GetLogAreaName: string; override;

  public
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
  fModel.Modules.ReBuildFastSearchList;

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
      if not CurrentModule.AnalysisData.AllowedDependencies.Contains(ImplicitlyDefinedDependency) then
      begin
        CurrentModule.AnalysisData.AllowedDependencies.Add(ImplicitlyDefinedDependency);
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
      if CurrentModule.AnalysisData.AllowedDependencies.Contains(DefinedDependency) then
        Log('Module "%s" defines a dependency to "%s". ' +
            'You can remove this from the module definition since this dependency is anyway implicitly allowed.',
            [CurrentModule.Name, DefinedDependency.Name], LogWarning)
      else
        CurrentModule.AnalysisData.AllowedDependencies.Add(DefinedDependency);
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


          // TODO: There should be user settings for "what to report"

          if CurrentModule <> UsedModule then // don't handle any "inner-module-dependencies"
          begin
            if not CurrentModule.IsUnknownModule then // don't report "unknown module" => "known module"
            begin
              IsDelphiToDelphi     := (CurrentModule.Origin = moDelphi)   and (UsedModule.Origin = moDelphi);
              Is3rdPartyToDelphi   := (CurrentModule.Origin = mo3rdParty) and (UsedModule.Origin = moDelphi);
              Is3rdPartyTo3rdParty := (CurrentModule.Origin = mo3rdParty) and (UsedModule.Origin = mo3rdParty);

              if UsedModule.IsUnknownModule then
                LogLevel := LogWarning
              else
                LogLevel := LogError;

              // if LogLevel = LogError then
                if not IsDelphiToDelphi and not Is3rdPartyToDelphi and not Is3rdPartyTo3rdParty then
                  if not CurrentModule.AnalysisData.AllowedDependencies.Contains(UsedModule) then
                    Log('Dependency violation: Module "%s" ==> "%s"; Unit "%s" ==> "%s"',
                         [CurrentModule.Name, UsedModule.Name, CurrentDelphiFile.UnitInfo.DelphiUnitName, UsedDelphiFile.UnitInfo.DelphiUnitName], LogLevel);

            end;

            // Save all actual dependencies between modules - What do we want to know?
            //   - Is there a dependency between A and B?
            //   - Which units cause the dependency?
            //   - How strong is the dependency between A and B? (e.g. how many units does A use from B)
            //   - Is this dependency a violation?

            // -> to get fast answers for these questions, we need to save details and sums

            // Details:
            //    (Module A, Unit x) uses (Module B, Unit y) => save these details in Module A

            // Summary
            //    (Module A) (allowed/non-allowed) uses (Module B) because of (5) units from (Module B) beeing used by (Module A)


  //        CurrentModule.AddActualDependency(CurrentDelphiFile, UsedModule, UsedDelphiFile);
          end;

        end;
      end;
    end;
  end;

end;

procedure TModulesAnalyzer.Execute;
begin

  // step 0: Clear old analysis data
  fModel.Modules.ClearModulesAnalysisData;
  fModel.Modules.CreateUnknownModule;

  // step 1: Try to map each unit to a defined module (or "unknown module)
  MapUnitsToModules;

  // step 2: Identify "allowed dependencies" for all modules
  AnalyzeAllowedDependencies;

  // step 3: Identify "actual dependencies" between modules (may contain illegal dependencies)
  // Idea: Calc "strength" = number of units using
  AnalyzeActualDependencies;

  // step 4: Reporting
  //  - "illegal dependencies"
  //  - "defined but not used dependencies"
  // TODO

end;



end.
