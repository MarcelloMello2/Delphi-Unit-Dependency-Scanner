unit Duds.Scan.FileScanner;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, IOUtils,

  Duds.Common.Files,
  Duds.Common.Types,
  Duds.Common.Classes,
  Duds.Scan.Model,
  Duds.Common.Log,
  Duds.Common.Language;

type
  TDudsFileScanner = class(TObject)
  private
    fProjectFilename: string;
    fProjectSettings: TProjectSettings;
    fModel: TDudsModel;

    procedure ScanFoldersAndAddAllDelphiFiles(Dirs: TStrings);
    function GetAbsolutePath(aRelativePath: string): string;
    function IsCommentedOut(path: string): Boolean;

  public
    procedure ScanRootFileFolders;
    procedure ExpandAndScanSearchPaths;

    property Model: TDudsModel                 read fModel           write fModel;
    property ProjectSettings: TProjectSettings read fProjectSettings write fProjectSettings;
    property ProjectFilename: string           read fProjectFilename write fProjectFilename;
  end;

implementation

{ TDudsFileScanner }

function TDudsFileScanner.GetAbsolutePath(aRelativePath: string): string;
begin
  if TPath.IsRelativePath(aRelativePath) then
    Result := TPath.GetFullPath(TPath.Combine(ExtractFilePath(FProjectFilename), aRelativePath))
  else
    Result := aRelativePath;
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
                TDudsLogger.GetInstance.Log(StrSFoundInMultip, [ExtractFilenameNoExt(ScannedFiles[i].Filename),
                  ExistingFilename, ScannedFiles[i].Filename], LogWarning);
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

function TDudsFileScanner.IsCommentedOut(path: string): Boolean;
begin
  Result := path.TrimLeft.StartsWith('//');
end;

procedure TDudsFileScanner.ScanRootFileFolders;
var
  RootFile,
  RootFileAbsolutePath: string;
begin
  TDudsLogger.GetInstance.Log(StrScanningDSearchRootFiles, [fProjectSettings.RootFiles.Count]);

  // step 1: calc absolute paths from root files defined in project settings
  for RootFile in FProjectSettings.RootFiles do
    // allow empty definition lines (e.g. for visual structuring)
    // allow "commenting" out of a line
    if not RootFile.IsEmpty and not IsCommentedOut(RootFile) then
    begin
      RootFileAbsolutePath := GetAbsolutePath(RootFile);
      if not FileExists(RootFileAbsolutePath) then
        TDudsLogger.GetInstance.Log(StrRootFileNotFound, [RootFileAbsolutePath], LogError)
      else
        if fModel.RootFiles.IndexOf(RootFileAbsolutePath) = -1 then
          fModel.RootFiles.Add(RootFileAbsolutePath)
        else
          TDudsLogger.GetInstance.Log(StrRootFileDuplicateFound, [RootFileAbsolutePath], LogError);
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
        TDudsLogger.GetInstance.Log(StrDuplicateSearchPathFound, [aNewPath], LogError)
      else
        TDudsLogger.GetInstance.Log(StrDuplicateSearchPathFoundByExpanding, [aNewPath, aExpandingFromPath], LogError);
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

  aExpandedPaths       := TStringList.Create; // no sorting to keep order as defined in projet settings
  aExpandedPathsLookup := TStringList.Create(TDuplicates.dupError, true, false);
  try
    for DefinedPath in fProjectSettings.SearchPaths do
    begin
      // allow empty definition lines (e.g. for visual structuring)
      // allow "commenting" out of a line
      if not DefinedPath.IsEmpty and not IsCommentedOut(DefinedPath) then
      begin
        Inc(DefinedPaths);

        DoExpandPath       := HasExpandSign(DefinedPath); // has "expand" sign?
        AbsoluteSearchPath := GetAbsolutePath(TrimExpandSign(DefinedPath));

        if not DirectoryExists(AbsoluteSearchPath) then
          TDudsLogger.GetInstance.Log(StrSearchPathDoesNotExist, [AbsoluteSearchPath], LogError)
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

    TDudsLogger.GetInstance.Log(StrScanningDSearchSearchPaths,
      [aExpandedPaths.Count, DefinedPaths]);

    ScanFoldersAndAddAllDelphiFiles(aExpandedPaths);
  finally
    aExpandedPaths.Free;
    aExpandedPathsLookup.Free;
  end;
end;

end.
