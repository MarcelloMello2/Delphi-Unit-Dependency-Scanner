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
    FDelphiFileList: TObjectList<TDelphiFile>;

  public
    constructor Create;
    destructor Destroy; override;

    function CreateDelphiFile(const DelphiUnitName: String): TDelphiFile;
    function SearchUnitByNameWithScopes(
      const DelphiUnitName: String;
      var UnitFilename: String;
      UnitScopeNames: TStringList): Boolean;
    function IsUnitUsed(const DelphiUnitName: String; DelphiFile: TDelphiFile): Boolean;
    function FindParsedDelphiUnit(const DelphiUnitName: string): TDelphiFile;

    property Files: TDictionary<String, String> read FFiles;
    property DelphiFiles: TObjectDictionary<String, TDelphiFile> read FDelphiFiles;
    property DelphiFileList: TObjectList<TDelphiFile> read FDelphiFileList;
  end;

implementation

{ TDudsModel }

constructor TDudsModel.Create;
begin
  inherited Create;
  FFiles          := TDictionary<String, String>.Create;
  FDelphiFiles    := TObjectDictionary<String, TDelphiFile>.Create([doOwnsValues]);
  FDelphiFileList := TObjectList<TDelphiFile>.Create(FALSE);
end;

destructor TDudsModel.Destroy;
begin
  FreeAndNil(FFiles);
  FreeAndNil(FDelphiFiles);
  FreeAndNil(FDelphiFileList);
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

function TDudsModel.IsUnitUsed(const DelphiUnitName: String; DelphiFile: TDelphiFile): Boolean;
var
  i: Integer;
begin
  Result := FALSE;

  for i := 0 to pred(DelphiFile.UnitInfo.UsedUnits.Count) do
    if SameText(DelphiFile.UnitInfo.UsedUnits[i].DelphiUnitName, DelphiUnitName) then
    begin
      Result := TRUE;

      Break;
    end;
end;

function TDudsModel.FindParsedDelphiUnit(const DelphiUnitName: string): TDelphiFile;
begin
  FDelphiFiles.TryGetValue(UpperCase(DelphiUnitName), Result);
end;

function TDudsModel.CreateDelphiFile(const DelphiUnitName: String): TDelphiFile;
begin
  Result := TDelphiFile.Create;

  FDelphiFiles.Add(UpperCase(DelphiUnitName), Result);
  FDelphiFileList.Add(Result);
end;

end.
