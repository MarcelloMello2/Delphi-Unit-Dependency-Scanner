unit Duds.Common.Log;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.SyncObjs,

  Duds.Common.Types,
  Duds.Common.Singleton;

const
  LogInfo = 1;
  LogWarning = 2;
  LogError = 3;
  LogDebug = 4;

type
  TLogProcedure = procedure(const Msg: String; const Severity: Integer = LogInfo) of object;

  TLogEntry = record
    Text: String;
    Severity: Integer;
  end;

  TLogEntries = TList<TLogEntry>;

  TDudsLogger = class(TDudsSingleton)
  private
    class var fInstanceCriticalSection: TCriticalSection;
    class var fInstance: TDudsSingleton;
    function GetEntry(aIndex: Cardinal): TLogEntry;
    function GetCount: Cardinal;

  var
    FLogEntries: TLogEntries;

  protected
    class function GetCriticalSection: TCriticalSection; override;
    class procedure DeleteInstanceCriticalSection; override;

  public
    class function GetInstance: TDudsLogger;
    class procedure ReleaseInstance;

    constructor Create; override;
    destructor Destroy; override;

    procedure Log(const Msg: String; const Severity: Integer = LogInfo); overload;
    procedure Log(const Msg: String; const Args: array of const; const Severity: Integer = LogInfo); overload;
    procedure Clear;

    property Count: Cardinal read GetCount;
    property LogEntries[aIndex: Cardinal]: TLogEntry read GetEntry;
  end;

implementation

constructor TDudsLogger.Create;
begin
  inherited;
  FLogEntries := TLogEntries.Create;
end;

destructor TDudsLogger.Destroy;
begin
  FreeAndNil(FLogEntries);
  inherited;
end;

class function TDudsLogger.GetInstance: TDudsLogger;
begin
  Result := TDudsLogger(inherited GetInstance(TDudsLogger, fInstance));
end;

class procedure TDudsLogger.ReleaseInstance;
begin
  inherited ReleaseInstance(fInstance);
end;

class function TDudsLogger.GetCriticalSection: TCriticalSection;
begin
  Result := TDudsSingleton.AcquireCriticalSection(fInstanceCriticalSection);
end;


class procedure TDudsLogger.DeleteInstanceCriticalSection;
begin
  FreeAndNil(fInstanceCriticalSection);
end;

procedure TDudsLogger.Log(const Msg: String; const Severity: Integer);
var
  LogEntry: TLogEntry;
begin
  LogEntry.Text := Msg;
  LogEntry.Severity := Severity;
  FLogEntries.Add(LogEntry);
end;

procedure TDudsLogger.Log(const Msg: String; const Args: Array of const; const Severity: Integer);
begin
  Log(Format(Msg, Args), Severity);
end;

procedure TDudsLogger.Clear;
begin
  FLogEntries.Clear;
end;

function TDudsLogger.GetCount: Cardinal;
begin
  Result := FLogEntries.Count;
end;

function TDudsLogger.GetEntry(aIndex: Cardinal): TLogEntry;
begin
  Result := FLogEntries[aIndex];
end;

initialization

finalization

TDudsLogger.ReleaseInstance;

end.
