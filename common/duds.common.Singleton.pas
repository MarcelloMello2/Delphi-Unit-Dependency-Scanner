unit duds.common.Singleton;

interface

uses
  System.SyncObjs, System.SysUtils;

type
  TDudsSingleton = class;
  TDudsSingletonClass = class of TDudsSingleton;

  TDudsSingleton = class(TObject)
  protected
    class function GetCriticalSection: TCriticalSection; virtual; abstract;
    class procedure DeleteInstanceCriticalSection; virtual; abstract;

  public
    constructor Create; virtual;

    class function AcquireCriticalSection(
      var aSection: TCriticalSection): TCriticalSection; static;

    class function GetInstance(aClass: TDudsSingletonClass; var aInstance: TDudsSingleton): TDudsSingleton;

    class procedure ReleaseInstance(var aInstance: TDudsSingleton);

  end;

implementation

constructor TDudsSingleton.Create;
begin
  inherited;
end;

class function TDudsSingleton.AcquireCriticalSection(var aSection: TCriticalSection): TCriticalSection;
var
  aResult: TCriticalSection;
begin
  // CriticalSection bereits gesetzt? -> kein Erzeugen mehr notwendig
  if aSection = nil then
  begin
    // neue CriticalSection darf nur 1 mal erzeugt werden daher absichern
    // fCriticalSection.Enter;
    try
      // nochmals prüfen ob die CriticalSection bereits gesetzt wurde,
      // könnte nämlich mitlerweile von einem anderen Thread schon gesetzt sein
      if aSection = nil then
      begin
        // CriticalSection erst einer lokalen Variable zuweisen damit
        // die CriticalSection fertig initialisiert ist bevor sie der
        // Variable zugewiesen wird
        // siehe http://en.wikipedia.org/wiki/Double-checked_locking
        aResult := TCriticalSection.Create;
        aSection := aResult;
      end;
    finally
      // fCriticalSection.Leave;
    end;
  end;
  Result := aSection;
end;

class function TDudsSingleton.GetInstance(aClass: TDudsSingletonClass; var aInstance: TDudsSingleton): TDudsSingleton;
var
  aResult: TDudsSingleton;
begin
  if aInstance = nil then
  begin
    GetCriticalSection.Enter;
    try
      if aInstance = nil then
      begin
        aResult := aClass.Create;
        aInstance := aResult;
      end;
    finally
      GetCriticalSection.Leave;
    end;
  end;
  Result := aInstance;
end;

class procedure TDudsSingleton.ReleaseInstance(var aInstance: TDudsSingleton);
begin
  GetCriticalSection.Enter;
  try
    FreeAndNil(aInstance);
  finally
    GetCriticalSection.Leave;
  end;

  DeleteInstanceCriticalSection;
end;

end.
