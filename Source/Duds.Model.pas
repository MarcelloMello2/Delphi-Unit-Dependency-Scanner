unit Duds.Model;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,

  Duds.Common.Classes;

type
  TDudsModel = class(TObject)
  private
    FFiles: TDictionary<String, String>;
    FDelphiFiles: TObjectDictionary<String, TDelphiFile>;

  public
    constructor Create;
    destructor Destroy; override;

    function SearchUnitByNameWithScopes(
      const DelphiUnitName: String;
      var UnitFilename: String;
      UnitScopeNames: TStringList): Boolean;

    property Files: TDictionary<String, String> read FFiles;
    property DelphiFiles: TObjectDictionary<String, TDelphiFile> read FDelphiFiles;
  end;

implementation

{ TDudsModel }

constructor TDudsModel.Create;
begin
  inherited Create;
  FFiles       := TDictionary<String, String>.Create;
  FDelphiFiles := TObjectDictionary<String, TDelphiFile>.Create([doOwnsValues]);
end;

destructor TDudsModel.Destroy;
begin
  FreeAndNil(FFiles);
  FreeAndNil(FDelphiFiles);
  inherited;
end;

function TDudsModel.SearchUnitByNameWithScopes(
      const DelphiUnitName: String;
      var UnitFilename: String;
      UnitScopeNames: TStringList): Boolean;
var
  i: Integer;
  UpperDelphiUnitName: String;
begin
  UpperDelphiUnitName := UpperCase(DelphiUnitName);

  Result := FFiles.TryGetValue(UpperDelphiUnitName, UnitFilename);

  // Try the scopes
  if not Result then
  begin
    for i := 0 to pred(UnitScopeNames.Count) do
    begin
      Result := FFiles.TryGetValue(UpperCase(UnitScopeNames[i]) + '.' + UpperDelphiUnitName, UnitFilename);

      if Result then
        Break;
    end;
  end;
end;

end.
