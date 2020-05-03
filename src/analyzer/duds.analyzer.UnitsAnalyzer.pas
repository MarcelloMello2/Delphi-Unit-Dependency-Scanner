unit duds.analyzer.UnitsAnalyzer;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,

  Vcl.Forms, // for "Application.ProcessMessages"

  duds.common.Utils,
  duds.common.Files,
  duds.common.Types,
  duds.common.Interfaces,
  duds.common.Classes,
  duds.common.Log,
  duds.common.Language,
  duds.analyzer.model,

  duds.common.UnitInfo,
  duds.common.UsedUnitInfo,
  duds.common.UsesParser;

type
  TUnitsAnalyzer = class(TObject)
  private
    FModel: TDudsModel;
    FOnLog: TLogProcedure;
    fAlreadyLoggedMissingIncludes: TStringList;
    FProjectSettings: TProjectSettings;

    FScanDepth: Integer;


    FCancelled: Boolean;

    procedure Log(const Msg: String; const Severity: Integer = LogInfo); overload;
    procedure Log(const Msg: String; const Args: array of const; const Severity: Integer); overload;

    procedure AnalyzeUnitOrProjectFile(Unitname: string; IsRootFile: Boolean);
    procedure AddParsedDelphiFile(UnitInfo: IUnitInfo; IsRootFile: Boolean; InPath: Boolean);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ScanRootFilesAndBuildUsesLists;

    property Model: TDudsModel read FModel write FModel;
    property OnLog: TLogProcedure read FOnLog    write FOnLog;
    property ProjectSettings: TProjectSettings read FProjectSettings write FProjectSettings;

    property Cancelled: Boolean read FCancelled write FCancelled;

  end;

implementation

{ TUnitsAnalyzer }

constructor TUnitsAnalyzer.Create;
begin
  FScanDepth         := 0;
  fAlreadyLoggedMissingIncludes := TStringList.Create(TDuplicates.dupError, true, false);
end;

destructor TUnitsAnalyzer.Destroy;
begin
  fAlreadyLoggedMissingIncludes.Free;
  inherited;
end;

procedure TUnitsAnalyzer.Log(const Msg: String; const Severity: Integer);
begin
  if Assigned(FOnLog) then
    FOnLog('Dependency Analyzer: ' + Msg, Severity);
end;

procedure TUnitsAnalyzer.Log(const Msg: String; const Args: array of const; const Severity: Integer);
begin
  Log(Format(Msg, Args), Severity);
end;

procedure TUnitsAnalyzer.AddParsedDelphiFile(UnitInfo: IUnitInfo; IsRootFile: Boolean; InPath: Boolean);
var
  DelphiFile: TDelphiFile;
begin
  DelphiFile := FModel.CreateDelphiFile(UnitInfo.DelphiUnitName, IsRootFile);

  DelphiFile.UnitInfo := UnitInfo;

  if FScanDepth = 1 then // this is for "root" files
    DelphiFile.UsedCount := 0
  else
    DelphiFile.UsedCount := 1;

  DelphiFile.InSearchPath := InPath;
end;

procedure TUnitsAnalyzer.AnalyzeUnitOrProjectFile(Unitname: string; IsRootFile: Boolean);
var
  Filename: String;
  FoundFileInPaths: Boolean;
  Parsed: Boolean;
  UnitInfo: IUnitInfo;
  UsedUnitInfo: IUsedUnitInfo;
  DelphiFile: TDelphiFile;
  aUsesParser : TUsesParser;
begin
  Application.ProcessMessages;

  if not FCancelled then
  begin
    Inc(fModel.Stats.Units.ScannedUsesCount);
    Inc(FScanDepth);

    if FScanDepth > fModel.Stats.Units.DeepestScanDepth then
      fModel.Stats.Units.DeepestScanDepth := FScanDepth;

    // Did we already parse this file? (also try unit scopes, so that we e.g. find `System.Classes` when
    // we search for `Classes` and already parsed `System.Classes` before
    DelphiFile := FModel.FindParsedDelphiUnit(Unitname, FProjectSettings.UnitScopeNames);

    // File was not parsed already -> try to find it on disk and parse it
    if not Assigned(DelphiFile) then
    begin
      // Do we have the file on disk? (also try unit scopes here)
      FoundFileInPaths := FModel.SearchUnitByNameWithScopes(Unitname, Filename, FProjectSettings.UnitScopeNames);

      // Parse the unit
      if FoundFileInPaths then
      begin
        Parsed := FALSE;
        try
          // parse the file (.pas, .dpr, .dpk, ...) and extract the used units into `UnitInfo`
          // -> this returns `FALSE` e.g. when it is an unknown file type
          aUsesParser := TUsesParser.Create;
          try
            aUsesParser.OnLog := fOnLog;
            aUsesParser.AlreadyLoggedMissingIncludes := fAlreadyLoggedMissingIncludes;
            Parsed := aUsesParser.GetUsedUnitsFromFile(Filename, UnitInfo);
          finally
            FreeAndNil(aUsesParser);
          end;

          // when used unit have a "in '..filename.pas'" part, then those files might not be part of the
          // "known" files in the model yet => add them
          for UsedUnitInfo in UnitInfo.UsedUnits do
            if UsedUnitInfo.Filename <> '' then
              FModel.Files.AddOrSetValue(UpperCase(UsedUnitInfo.DelphiUnitName), UsedUnitInfo.Filename);

          AddParsedDelphiFile(UnitInfo, IsRootFile, TRUE);

          Inc(fModel.Stats.Units.LinesOfCode, UnitInfo.LinesOfCode);
          Inc(fModel.Stats.Units.ParsedFileCount);

          if FileExists(ChangeFileExt(UnitInfo.Filename, '.dfm')) then
            Inc(fModel.Stats.Units.VCLFormCount)
          else if FileExists(ChangeFileExt(UnitInfo.Filename, '.fmx')) then
            Inc(fModel.Stats.Units.FMXFormCount);
        except
          on e: Exception do
            TDudsLogger.GetInstance.Log(StrUnableToParseS, [Filename, e.Message], LogError);
        end;

        if Parsed then
          for UsedUnitInfo in UnitInfo.UsedUnits do
            if not FCancelled then
              AnalyzeUnitOrProjectFile(UsedUnitInfo.DelphiUnitName, FALSE);

      end else
      // we don't have the file on disk...
      begin
        Inc(fModel.Stats.Units.FilesNotInPath);

        // create an empty "parsed file" for the file that was not found on disk (still needed for the "UsedCount" counter)
        UnitInfo                := TUnitInfo.Create;
        UnitInfo.DelphiUnitName := Unitname;
        AddParsedDelphiFile(UnitInfo, IsRootFile, FALSE);
      end;
    end
    // the File was already parsed -> only increase the usage counter
    else
      DelphiFile.UsedCount := DelphiFile.UsedCount + 1;

  end;

  if FScanDepth > 0 then
    Dec(FScanDepth);
end;

procedure TUnitsAnalyzer.ScanRootFilesAndBuildUsesLists;
var
  RootFile: string;
begin
  TDudsLogger.GetInstance.Log(StrParsingFiles);

  for RootFile in fModel.RootFiles do
    AnalyzeUnitOrProjectFile(ExtractFilenameNoExt(RootFile), True);

  TDudsLogger.GetInstance.Log(StrDFilesWithATo, [FormatCardinal(FModel.ParsedDelphiFiles.Count), FormatCardinal(fModel.Stats.Units.LinesOfCode)]);
end;

end.
