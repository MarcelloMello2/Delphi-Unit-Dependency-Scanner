unit duds.analyzer.CustomAnalyzer;

interface

uses
  System.SysUtils,
  duds.common.Log, duds.common.Classes, duds.analyzer.model;

type
  TCustomAnalyzer = class(TObject)
  protected
    fModel: TDudsModel;
    fOnLog: TLogProcedure;
    fProjectSettings: TProjectSettings;
    fProjectFilename: string;
    fCancelled: Boolean;

    function SettingsLineIsRelevant(element: string): Boolean;
    function GetLogAreaName: string; virtual;
    procedure SetCancelled(const Value: Boolean); virtual;

    procedure Log(const Msg: String; const Severity: Integer = LogInfo); overload;
    procedure Log(const Msg: String; const Args: array of const; const Severity: Integer); overload;

  public
    procedure InitByCustomAnalyzer(AnotherCustomAnalyzer: TCustomAnalyzer);

    property Model: TDudsModel                 read fModel           write fModel;
    property ProjectFilename: string           read fProjectFilename write fProjectFilename;
    property ProjectSettings: TProjectSettings read fProjectSettings write fProjectSettings;
    property OnLog: TLogProcedure              read fOnLog           write fOnLog;

    property Cancelled: Boolean                read fCancelled       write SetCancelled;

  end;

implementation

procedure TCustomAnalyzer.InitByCustomAnalyzer(AnotherCustomAnalyzer: TCustomAnalyzer);
begin
  Model           := AnotherCustomAnalyzer.Model;
  OnLog           := AnotherCustomAnalyzer.OnLog;
  ProjectSettings := AnotherCustomAnalyzer.ProjectSettings;
  ProjectFilename := AnotherCustomAnalyzer.ProjectFilename;
end;

function TCustomAnalyzer.GetLogAreaName: string;
begin
  Result := '';
end;

function TCustomAnalyzer.SettingsLineIsRelevant(element: string): Boolean;
begin
  Result := not element.IsEmpty and not element.TrimLeft.StartsWith('//');
end;

procedure TCustomAnalyzer.Log(const Msg: String; const Severity: Integer);
begin
  if Assigned(fOnLog) then
  begin
    if GetLogAreaName <> '' then
      fOnLog(GetLogAreaName + ': ' + Msg, Severity)
    else
      fOnLog(Msg, Severity);
  end;
end;

procedure TCustomAnalyzer.Log(const Msg: String; const Args: array of const; const Severity: Integer);
begin
  Log(Format(Msg, Args), Severity);
end;

procedure TCustomAnalyzer.SetCancelled(const Value: Boolean);
begin
  fCancelled := Value;
end;

end.
