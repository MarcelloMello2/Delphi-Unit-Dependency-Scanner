unit duds.analyzer.Facade;

interface

uses
  System.SysUtils,
  System.Generics.Defaults,
  duds.common.log,
  duds.common.Classes,
  duds.common.Files,
  duds.analyzer.model, duds.common.modulesSerializer,
  duds.common.Language, duds.analyzer.FileScanner, duds.common.Utils,
  duds.analyzer.UnitsAnalyzer, duds.analyzer.ModulesAnalyzer,
  duds.analyzer.CustomAnalyzer;

type
  TAnalyzerFacade = class(TCustomAnalyzer)
  private
    fCurrentAnalyzer: TCustomAnalyzer;

    procedure SetCancelled(const Value: Boolean); override;
    function CreateAnalyzer<T: TCustomAnalyzer, constructor>: T;
    procedure ReleaseAnalyzer(Analyzer: TCustomAnalyzer);
  public
    procedure Execute;

  end;

implementation

{ TAnalyzerFacade }

procedure TAnalyzerFacade.SetCancelled(const Value: Boolean);
begin
  inherited;
  if Assigned(fCurrentAnalyzer) then
    fCurrentAnalyzer.Cancelled := Value;
end;

function TAnalyzerFacade.CreateAnalyzer<T>(): T;
begin
  Result := T.Create;
  fCurrentAnalyzer := Result;
end;

procedure TAnalyzerFacade.ReleaseAnalyzer(Analyzer: TCustomAnalyzer);
begin
  fCurrentAnalyzer := nil;
  Analyzer.Free;
end;

procedure TAnalyzerFacade.Execute;
var
  aFileScanner: TDudsFileScanner;
  aUnitsAnalyzer: TUnitsAnalyzer;
  aModulesAnalyzer: TModulesAnalyzer;
begin
  // step 1: read modules definition (if defined)
  if not fProjectSettings.ModulesDefinitionFile.IsEmpty then
  begin
    TModulesSerializer.ReadFromJsonFile(
      GetAbsolutePath(fProjectSettings.ModulesDefinitionFile, fProjectFilename),
      FModel.Modules);
    Log(StrModulesDefinitionLoaded, [FModel.Modules.Dictionary.Count], LogInfo);
  end;

  // step 2: scanning the disk for files (rootfiles & search paths)
  if not fCancelled then
  begin
    aFileScanner := CreateAnalyzer<TDudsFileScanner>;
    try
      aFileScanner.InitByCustomAnalyzer(Self);
      aFileScanner.ScanRootFileFolders;
      aFileScanner.ExpandAndScanSearchPaths;
    finally
      ReleaseAnalyzer(aFileScanner);
    end;
  end;

  // step 3: recursively parsing the root files for all used units
  if not fCancelled then
  begin
    aUnitsAnalyzer := CreateAnalyzer<TUnitsAnalyzer>;
    try
      aUnitsAnalyzer.InitByCustomAnalyzer(Self);
      aUnitsAnalyzer.ScanRootFilesAndBuildUsesLists;
    finally
      ReleaseAnalyzer(aUnitsAnalyzer);
    end;
  end;

  // step 4: Identify modules for each .pas unit
  //   at this point, all unit that were "seen" (by root files, search paths
  //   or parsing units) are identified
  // step 3: recursively parsing the root files for all used units
  if not fCancelled then
  begin
    aModulesAnalyzer := CreateAnalyzer<TModulesAnalyzer>;
    try
      aModulesAnalyzer.InitByCustomAnalyzer(Self);
      aModulesAnalyzer.MapUnitsToModules;
      aModulesAnalyzer.Execute;
    finally
      ReleaseAnalyzer(aModulesAnalyzer);
    end;
  end;
end;

end.
