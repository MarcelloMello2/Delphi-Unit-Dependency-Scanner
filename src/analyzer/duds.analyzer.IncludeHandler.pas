unit duds.analyzer.IncludeHandler;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.Ioutils,

  SimpleParser.Lexer.Types,

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
  TIncludeCacheItem = record
    FileName: string;
    Content : string;
  end;

  TIncludeCache = TDictionary<string,TIncludeCacheItem>;

  TAnalyzerIncludeHandler = class(TInterfacedObject, IIncludeHandler)
  private
    fUnitFile: string;
    fUnitFileFolder: string;
    fModel: TDudsModel;
    FOnLog: TLogProcedure;
    fIncludeCache: TIncludeCache;
    fAlreadyLoggedMissingIncludes: TStringlist;

    procedure Log(const Msg: String; const Severity: Integer = LogInfo); overload;
    procedure Log(const Msg: String; const Args: array of const; const Severity: Integer); overload;

    function FindFile(const FileName: string; RelativeToFolder: string; var FilePath: string): boolean;
    class function SafeOpenFileStream(const FileName: string; var FileStream: TStringStream; var ErrorMsg: string): boolean;
  public
    constructor Create(const FileName: string; Model: TDudsModel; OnLog: TLogProcedure; IncludeCache: TIncludeCache; AlreadyLoggedMissingIncludes: TStringList);
    function GetIncludeFileContent(const ParentFileName, FileName: string; out Content: string; out FilePath: string): Boolean;

    property OnLog: TLogProcedure read FOnLog    write FOnLog;
    property AlreadyLoggedMissingIncludes: TStringlist read fAlreadyLoggedMissingIncludes write fAlreadyLoggedMissingIncludes;
  end;

implementation

{ TAnalyzerIncludeHandler }

constructor TAnalyzerIncludeHandler.Create(const FileName: string; Model: TDudsModel; OnLog: TLogProcedure; IncludeCache: TIncludeCache; AlreadyLoggedMissingIncludes: TStringList);
begin
  inherited Create;
  fUnitFileFolder               := IncludeTrailingPathDelimiter(ExtractFilePath(FileName));
  fUnitFile                     := ExtractFileName(FileName);
  fModel                        := Model;
  FOnLog                        := OnLog;
  fIncludeCache                 := IncludeCache;
  fAlreadyLoggedMissingIncludes := AlreadyLoggedMissingIncludes;
end;

procedure TAnalyzerIncludeHandler.Log(const Msg: String; const Severity: Integer);
begin
  if Assigned(FOnLog) then
    FOnLog('Uses Parser: ' + Msg, Severity);
end;

procedure TAnalyzerIncludeHandler.Log(const Msg: String; const Args: array of const; const Severity: Integer);
begin
  Log(Format(Msg, Args), Severity);
end;

function TAnalyzerIncludeHandler.FindFile(const FileName: string; RelativeToFolder: string; var FilePath: string): boolean;

  function FilePresent(const testFile: string): boolean;
  begin
    Result := FileExists(testFile);
    if Result then
      filePath := ExpandFileName(testFile);
  end;

var
  NormalizedName : string;
begin
  Result          := false;

  NormalizedName  := fileName.DeQuotedString;
  // if there is no extension, use '.pas' (http://docwiki.embarcadero.com/RADStudio/Rio/en/Include_file_(Delphi))
  if ExtractFileExt(NormalizedName).IsEmpty then
    NormalizedName := NormalizedName + '.pas';

  // step 1: if the file name has a path - look ONLY at that location
  if not ExtractFilePath(NormalizedName).IsEmpty then
  begin
    if IsRelativePath(NormalizedName) then
      FilePath := TPath.Combine(RelativeToFolder, NormalizedName)
    else
      FilePath := NormalizedName; // = absolte file path given

    if FilePresent(FilePath) then
      Exit(True)
    else
      Exit(False);
  end;

  // - no path was given -

  // step 2: search the same directory as the current module
  if FilePresent(TPath.Combine(RelativeToFolder, NormalizedName)) then
    Exit(True);

  // step 3: search in the project root folder => not available since we are parsing multiple projects at the same time
//  if FilePresent(TPath.Combine(<project root folder>, NormalizedName)) then
//    Exit(True);

  // step 4: search in search paths
  for var SearchPath in fModel.ExpandedSearchPaths do
    if FilePresent(TPath.Combine(searchPath, NormalizedName)) then
      Exit(True);
end;

class function TAnalyzerIncludeHandler.SafeOpenFileStream(const FileName: string; var FileStream: TStringStream; var ErrorMsg: string): boolean;
var
  buf: TBytes;
  encoding: TEncoding;
  readStream: TStream;
begin
  Result := true;
  try
    readStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  except
    on E: EFCreateError do
    begin
      ErrorMsg := E.Message;
      Result := false;
    end;
    on E: EFOpenError do
    begin
      ErrorMsg := E.Message;
      Result := false;
    end;
  end;

  if Result then
    try
      SetLength(buf, 4);
      SetLength(buf, readStream.Read(buf[0], Length(buf)));
      encoding := nil;
      readStream.Position := TEncoding.GetBufferEncoding(buf, encoding);
      FileStream := TStringStream.Create('', encoding);
      FileStream.CopyFrom(readStream, readStream.Size - readStream.Position);
    finally
      FreeAndNil(readStream);
    end;
end;

function TAnalyzerIncludeHandler.GetIncludeFileContent(const ParentFileName, FileName: string; out Content: string; out FilePath: string): Boolean;
var
  errorMsg: string;
  aFileStream: TStringStream;
  fComputedName: string;
  IncludeCacheItem: TIncludeCacheItem;
  CacheKey: string;
  aRelativeToFolder: string;
begin
  aRelativeToFolder := fUnitFileFolder;
  if not ParentFileName.IsEmpty then // include inside include? -> use parent file to resolve relative paths
    aRelativeToFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParentFileName));

  if FileName.StartsWith('*.') then
    fComputedName := ChangeFileExt(fUnitFile, '') + FileName.Remove(0 { 0-based } , 1)
  else if FileName.Contains('*') then
    fComputedName := FileName.Replace('*', '', [rfReplaceAll])
  else
    fComputedName := FileName;

  CacheKey := fComputedName + #13 + aRelativeToFolder;
  if fIncludeCache.TryGetValue(CacheKey, IncludeCacheItem) then
  begin
    Content  := IncludeCacheItem.Content;
    FilePath := IncludeCacheItem.FileName;
    Exit(True);
  end;

  if not FindFile(fComputedName, aRelativeToFolder, FilePath) then
  begin
    Log('Cannot find include file "%s", defined in file "%s", source folder "%s"', [fComputedName, fUnitFile, aRelativeToFolder], LogError);
    IncludeCacheItem.FileName := '';
    IncludeCacheItem.Content  := '';
    FIncludeCache.Add(CacheKey, IncludeCacheItem);
    Exit(False);
  end;

  if fIncludeCache.TryGetValue(FilePath, IncludeCacheItem) then
  begin
    Content := IncludeCacheItem.Content;
    Exit(True);
  end;

  if not TAnalyzerIncludeHandler.SafeOpenFileStream(FilePath, aFileStream, errorMsg) then
  begin
    Log('Cannot open include file "%s": "%s"', [FilePath, errorMsg], LogError);
    Result := False;
  end
  else
    try
      Content := aFileStream.DataString;
      Result := True;
    finally
      FreeAndNil(aFileStream);
    end;

  // Add to include cache (we only arrive here, when a file path was found)
  IncludeCacheItem.FileName := FilePath;
  IncludeCacheItem.Content  := Content;
  fIncludeCache.Add(CacheKey, IncludeCacheItem);

  IncludeCacheItem.FileName := '';
  fIncludeCache.Add(FilePath, IncludeCacheItem);
end;



end.
