unit duds.analyzer.Facade;

interface

uses
  System.SysUtils,
  duds.common.log,
  duds.common.Classes,
  duds.common.Files,
  duds.analyzer.model, duds.common.modulesSerializer,
  duds.common.Language, duds.analyzer.FileScanner, duds.common.Utils,
  duds.analyzer.UnitsAnalyzer, duds.analyzer.ModulesAnalyzer;

type
  TAnalyzerFacade = class
  private
    fModel: TDudsModel;
    fProjectSettings: TProjectSettings;
    fOnLog: TLogProcedure;
    fProjectFilename: string;
    fCancelled: Boolean;

    fUnitsAnalyzer: TUnitsAnalyzer;

    procedure Log(const Msg: String; const Severity: Integer = LogInfo); overload;
    procedure Log(const Msg: String; const Args: array of const; const Severity: Integer); overload;
    procedure SetCancelled(const Value: Boolean);

  public


    procedure Execute;

    property Model: TDudsModel                 read fModel           write fModel;
    property OnLog: TLogProcedure              read fOnLog           write fOnLog;
    property ProjectSettings: TProjectSettings read fProjectSettings write fProjectSettings;
    property ProjectFilename: string           read fProjectFilename write fProjectFilename;

    property Cancelled: Boolean read fCancelled write SetCancelled;

  end;

implementation

{ TAnalyzerFacade }

procedure TAnalyzerFacade.Log(const Msg: String; const Severity: Integer =
    LogInfo);
begin
  if Assigned(FOnLog) then
    FOnLog(Msg, Severity);
end;

procedure TAnalyzerFacade.Log(const Msg: String; const Args: array of const;
    const Severity: Integer);
begin
  Log(Format(Msg, Args), Severity);
end;

procedure TAnalyzerFacade.SetCancelled(const Value: Boolean);
begin
  fCancelled := Value;
  if Assigned(fUnitsAnalyzer) then
    fUnitsAnalyzer.Cancelled := Value;
end;

procedure TAnalyzerFacade.Execute;
var
  aFileScanner: TDudsFileScanner;
  aUnitsTotal: integer;
  aUnitsInModules: integer;
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
  aFileScanner := TDudsFileScanner.Create;
  try
    aFileScanner.Model           := fModel;
    aFileScanner.ProjectSettings := fProjectSettings;
    aFileScanner.ProjectFilename := fProjectFilename;
    aFileScanner.ScanRootFileFolders;
    aFileScanner.ExpandAndScanSearchPaths;
    Log(StrDFilesFound, [FormatCardinal(fModel.Files.Count)], LogInfo);
  finally
    FreeAndNil(aFileScanner);
  end;

  // step 3: recursively parsing the root files for all used units
  if not fCancelled then
  begin
    fUnitsAnalyzer := TUnitsAnalyzer.Create;
    try
      fUnitsAnalyzer.Model           := fModel;
      fUnitsAnalyzer.OnLog           := fOnLog;
      fUnitsAnalyzer.ProjectSettings := fProjectSettings;

      fUnitsAnalyzer.ScanRootFilesAndBuildUsesLists;

//      FParsedFileCount   := fUnitsAnalyzer.ParsedFileCount;
//      FScannedUsesCount  := fUnitsAnalyzer.ScannedUsesCount;
//      FDeepestScanDepth  := fUnitsAnalyzer.DeepestScanDepth;
//      FFMXFormCount      := fUnitsAnalyzer.FMXFormCount;
//      FVCLFormCount      := fUnitsAnalyzer.VCLFormCount;
//      FLineCount         := fUnitsAnalyzer.LinesOfCode;
//      FSemiCircularFiles := fUnitsAnalyzer.SemiCircularFiles;
//      FCircularFiles     := fUnitsAnalyzer.CircularFiles;
//      FFilesNotInPath    := fUnitsAnalyzer.FilesNotInPath;
    finally
      FreeAndNil(fUnitsAnalyzer);
    end;
  end;

  // step 4: Identify modules for each .pas unit
  //   at this point, all unit that were "seen" (by root files, search paths
  //   or parsing units) are identified
  TModulesAnalyzer.MapUnitsToModules(fModel, aUnitsTotal, aUnitsInModules);
  Log(StrModulesIdentified, [FormatCardinal(aUnitsInModules), FormatCardinal(aUnitsTotal)], LogInfo);
end;

end.
