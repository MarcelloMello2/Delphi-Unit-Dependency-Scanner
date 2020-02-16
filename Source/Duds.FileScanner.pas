unit Duds.FileScanner;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,

  Duds.Common.Files,
  Duds.Common.Types,
  Duds.Common.Classes,
  Duds.Model,
  Duds.Common.Log,
  Duds.Common.Language;

type
  TDudsFileScanner = class(TObject)
  public
    procedure LoadFilesInSearchPaths(Model: TDudsModel; ProjectSettings: TProjectSettings);

  end;

implementation

{ TDudsFileScanner }

procedure TDudsFileScanner.LoadFilesInSearchPaths(Model: TDudsModel; ProjectSettings: TProjectSettings);

  procedure ScanPasFiles(Dirs: TStrings);
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
              if Model.SearchUnitByNameWithScopes(DelphiUnitName, ExistingFilename, ProjectSettings.UnitScopeNames) then
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

  procedure ExpandAndScanSearchPaths;
  var
    aExpandedPaths: TStringList;
    i, j: Integer;
    CurrentDir: String;
    Dir: String;
    ScannedFiles: TObjectList<TFileInfo>;
  begin
    aExpandedPaths := TStringList.Create;
    try
      for i := 0 to pred(ProjectSettings.SearchPaths.Count) do
      begin
        CurrentDir := ProjectSettings.SearchPaths[i];
        if CurrentDir.EndsWith(ExpandSearchPathSign) then // has "expand" sign?
        begin
          CurrentDir := Copy(CurrentDir, 1, Length(CurrentDir) - Length(ExpandSearchPathSign));
          aExpandedPaths.Add(CurrentDir);
          ScannedFiles := ScanFiles(CurrentDir, '*.*', TRUE, TRUE, TRUE);
          try
            for j := 0 to pred(ScannedFiles.Count) do
              if ScannedFiles[j].IsDir then
                aExpandedPaths.Add(ScannedFiles[j].Filename);
          finally
            FreeAndNil(ScannedFiles);
          end;

        end
        else
          aExpandedPaths.Add(CurrentDir)
      end;

      TDudsLogger.GetInstance.Log(StrScanningDSearchSearchPaths,
        [aExpandedPaths.Count, ProjectSettings.SearchPaths.Count]);

      ScanPasFiles(aExpandedPaths);
    finally
      FreeAndNil(aExpandedPaths);
    end;
  end;

begin
  // step 1: scan root files
  TDudsLogger.GetInstance.Log(StrScanningDSearchRootFiles, [ProjectSettings.RootFiles.Count]);
  ScanPasFiles(ProjectSettings.RootFiles);

  // step 2: (expand &) scan search paths
  ExpandAndScanSearchPaths;
end;

end.
