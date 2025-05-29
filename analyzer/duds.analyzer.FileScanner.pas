unit duds.analyzer.FileScanner;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, IOUtils,

  duds.common.Files,
  duds.common.Types,
  duds.common.Classes,
  duds.analyzer.model,
  duds.common.Log,
  duds.common.Language, duds.analyzer.CustomAnalyzer, duds.common.Utils;

type
  TDudsFileScanner = class(TCustomAnalyzer)
  private
    procedure ScanFoldersAndAddAllDelphiFiles(Dirs: TStrings);

  protected
    function GetLogAreaName: string; override;

  public
    procedure ScanRootFileFolders;
    procedure ExpandAndScanSearchPaths;

  end;

implementation

{ TDudsFileScanner }

function TDudsFileScanner.GetLogAreaName: string;
begin
  Result := 'File Scanner';
end;

procedure TDudsFileScanner.ScanFoldersAndAddAllDelphiFiles(Dirs: TStrings);
var
  ScannedFiles: TObjectList<TFileInfo>;
  i, n: Integer;
  Extension, Dir, DelphiUnitName, ExistingFilename: String;
begin
  for n := 0 to pred(Dirs.Count) do
  begin
    Dir := Dirs[n];

    if FileExists(Dir) then
      Dir := ExtractFileDir(Dir);

    if DirectoryExists(Dir) then
    begin
      ScannedFiles := ScanFiles(Dir, '*.*', FALSE, FALSE, TRUE);
      try
        for i := 0 to pred(ScannedFiles.Count) do
        begin
          Extension := LowerCase(ExtractFileExt(ScannedFiles[i].Filename));

          if ((Extension = '.pas') or (Extension = '.dpk') or (Extension = '.dpr')) then
          begin
            DelphiUnitName := UpperCase(ExtractFilenameNoExt(ScannedFiles[i].Filename));

            // Unit found in multiple locations?
            if Model.SearchUnitByNameWithScopes(DelphiUnitName, ExistingFilename, FProjectSettings.UnitScopeNames) then
            begin
              if not SameText(ExistingFilename, ScannedFiles[i].Filename) then
                Log(StrSFoundInMultip, [ExtractFilenameNoExt(ScannedFiles[i].Filename), ExistingFilename, ScannedFiles[i].Filename], LogWarning);
            end
            else
              Model.Files.Add(DelphiUnitName, ScannedFiles[i].Filename);
          end;
        end;
      finally
        FreeAndNil(ScannedFiles);
      end;
    end;
  end;
end;

procedure TDudsFileScanner.ScanRootFileFolders;
var
  RootFile,
  RootFileAbsolutePath: string;
begin
  Log(StrScanningDSearchRootFiles, [fProjectSettings.RootFiles.Count], LogInfo);

  // step 1: calc absolute paths from root files defined in project settings
  for RootFile in FProjectSettings.RootFiles do
    // allow empty definition lines (e.g. for visual structuring)
    // allow "commenting" out of a line
    if SettingsLineIsRelevant(RootFile) then
    begin
      RootFileAbsolutePath := GetAbsolutePath(RootFile, fProjectFilename);
      if not FileExists(RootFileAbsolutePath) then
        Log(StrRootFileNotFound, [RootFileAbsolutePath], LogError)
      else
        if fModel.RootFiles.IndexOf(RootFileAbsolutePath) = -1 then
          fModel.RootFiles.Add(RootFileAbsolutePath)
        else
          Log(StrRootFileDuplicateFound, [RootFileAbsolutePath], LogError);
    end;

  // step 2: scan the root file folders
  ScanFoldersAndAddAllDelphiFiles(fModel.RootFiles);
end;

procedure TDudsFileScanner.ExpandAndScanSearchPaths;

  function HasExpandSign(aPath: string): Boolean;
  begin
    Result := aPath.TrimRight.EndsWith(ExpandSearchPathSign);
  end;

  function TrimExpandSign(aPath: string): string;
  begin
    aPath := aPath.Trim;
    if HasExpandSign(aPath) then
      Result := Copy(aPath, 1, Length(aPath) - Length(ExpandSearchPathSign))
    else
      Result := aPath;
  end;

var
  aExpandedPaths: TStringList;
  aExpandedPathsLookup: TStringList;

  procedure AddPathCheckDuplicates(aNewPath: string; aExpandingFromPath: string = '');
  begin
    aNewPath := IncludeTrailingPathDelimiter(aNewPath);
    if aExpandedPathsLookup.IndexOf(aNewPath) = -1 then
    begin
      aExpandedPaths.Add(aNewPath);
      aExpandedPathsLookup.Add(aNewPath);
    end else
      if aExpandingFromPath.IsEmpty then
        Log(StrDuplicateSearchPathFound, [aNewPath], LogError)
      else
        Log(StrDuplicateSearchPathFoundByExpanding, [aNewPath, aExpandingFromPath], LogError);
  end;

var
  j: Integer;
  DefinedPath: String;
  AbsoluteSearchPath: String;
  DoExpandPath: Boolean;
  Dir: String;
  ScannedFiles: TObjectList<TFileInfo>;
  DefinedPaths: integer;
  ExpandedPath: TFileInfo;
begin
  DefinedPaths := 0;

  aExpandedPaths       := TStringList.Create; // no sorting to keep order as defined in project settings
  aExpandedPathsLookup := TStringList.Create(TDuplicates.dupError, true, false);
  try
    for DefinedPath in fProjectSettings.SearchPaths do
    begin
      // allow empty definition lines (e.g. for visual structuring)
      // allow "commenting" out of a line
      if SettingsLineIsRelevant(DefinedPath) then
      begin
        Inc(DefinedPaths);

        DoExpandPath       := HasExpandSign(DefinedPath); // has "expand" sign?
        AbsoluteSearchPath := GetAbsolutePath(TrimExpandSign(DefinedPath), fProjectFilename);

        if not DirectoryExists(AbsoluteSearchPath) then
          Log(StrSearchPathDoesNotExist, [AbsoluteSearchPath], LogError)
        else begin
          AddPathCheckDuplicates(AbsoluteSearchPath);
          if DoExpandPath then
          begin
            ScannedFiles := ScanFiles(AbsoluteSearchPath, '*.*', TRUE, TRUE, TRUE);
            try
              for ExpandedPath in ScannedFiles do
                if ExpandedPath.IsDir then
                  AddPathCheckDuplicates(ExpandedPath.Filename, AbsoluteSearchPath);
            finally
              FreeAndNil(ScannedFiles);
            end;
          end;
        end;
      end;
    end;

    Log(StrScanningDSearchSearchPaths, [aExpandedPaths.Count, DefinedPaths], LogInfo);

    // Save the search paths (e.g. for the include file handler)
    fModel.ExpandedSearchPaths.Assign(aExpandedPaths);

    ScanFoldersAndAddAllDelphiFiles(aExpandedPaths);
  finally
    aExpandedPaths.Free;
    aExpandedPathsLookup.Free;
  end;

  Log(StrDFilesFound, [FormatCardinal(fModel.Files.Count)], LogInfo);
end;

end.
