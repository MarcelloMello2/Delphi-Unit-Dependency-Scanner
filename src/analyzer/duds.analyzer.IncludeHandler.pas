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
  TAnalyzerIncludeHandler = class(TInterfacedObject, IIncludeHandler)
  private
    fFileName: string;
    FOnLog: TLogProcedure;
    fAlreadyLoggedMissingIncludes: TStringlist;

    procedure Log(const Msg: String; const Severity: Integer = LogInfo); overload;
    procedure Log(const Msg: String; const Args: array of const; const Severity: Integer); overload;
  public
    constructor Create(const FileName: string; OnLog: TLogProcedure; AlreadyLoggedMissingIncludes: TStringList);
    function GetIncludeFileContent(const ParentFileName, IncludeName: string; out Content: string; out FileName: string): Boolean;

    property OnLog: TLogProcedure read FOnLog    write FOnLog;
    property AlreadyLoggedMissingIncludes: TStringlist read fAlreadyLoggedMissingIncludes write fAlreadyLoggedMissingIncludes;
  end;

implementation

{ TAnalyzerIncludeHandler }

constructor TAnalyzerIncludeHandler.Create(const FileName: string; OnLog: TLogProcedure; AlreadyLoggedMissingIncludes: TStringList);
begin
  inherited Create;
  fFileName                     := FileName;
  FOnLog                        := OnLog;
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

function TAnalyzerIncludeHandler.GetIncludeFileContent(const ParentFileName, IncludeName: string; out Content, FileName: string): Boolean;
var
  IncludeFileName: string;
  FileContent: TStringList;
begin
  FileContent := TStringList.Create;
  try
    IncludeFileName := TPath.Combine(ExtractFilePath(fFileName), IncludeName.TrimStart(['''']).TrimEnd(['''']));
    if not FileExists(IncludeFileName) then
    begin
      Result := False;
      if fAlreadyLoggedMissingIncludes.IndexOf(IncludeFileName.ToLower) = -1 then
      begin
        Log('Could not resolve include in file "%s" - "%s" not found (searching only in same path)', [fFileName, IncludeName], LogWarning);
        fAlreadyLoggedMissingIncludes.Add(IncludeFileName.ToLower);
      end;
      Exit;
    end;

    FileContent.LoadFromFile(IncludeFileName);
    Content := FileContent.Text;
    FileName := IncludeFileName;

    Result := True;
  finally
    FileContent.Free;
  end;
end;



end.
