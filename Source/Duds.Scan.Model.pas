unit Duds.Scan.Model;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,

  Duds.Common.Classes;

type
  TDudsModel = class(TObject)
  private
    FFiles: TDictionary<String, String>;
    FParsedDelphiRootFiles: TObjectList<TDelphiFile>;
    FParsedDelphiFiles: TObjectDictionary<String, TDelphiFile>;
    FDelphiFileList: TObjectList<TDelphiFile>;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function CreateDelphiFile(const DelphiUnitName: String; IsRootFile: Boolean): TDelphiFile;
    function SearchUnitByNameWithScopes(
      const DelphiUnitName: String;
      var UnitFilename: String;
      UnitScopeNames: TStringList): Boolean;
    function IsUnitUsed(const DelphiUnitName: String; DelphiFile: TDelphiFile): Boolean;
    function FindParsedDelphiUnit(const DelphiUnitName: string): TDelphiFile;

    property Files: TDictionary<String, String> read FFiles;
    property ParsedDelphiRootFiles: TObjectList<TDelphiFile> read FParsedDelphiRootFiles;
    property ParsedDelphiFiles: TObjectDictionary<String, TDelphiFile> read FParsedDelphiFiles;
    property DelphiFileList: TObjectList<TDelphiFile> read FDelphiFileList;
  end;

implementation

{ TDudsModel }

procedure TDudsModel.Clear;
begin
  FFiles.Clear;
  FParsedDelphiRootFiles.Clear;
  FParsedDelphiFiles.Clear;
  FDelphiFileList.Clear;
end;

constructor TDudsModel.Create;
begin
  inherited Create;
  FFiles                 := TDictionary<String, String>.Create;
  FParsedDelphiRootFiles := TObjectList<TDelphiFile>.Create(FALSE);
  FParsedDelphiFiles     := TObjectDictionary<String, TDelphiFile>.Create([doOwnsValues]);
  FDelphiFileList        := TObjectList<TDelphiFile>.Create(FALSE);
end;

destructor TDudsModel.Destroy;
begin
  FreeAndNil(FFiles);
  FreeAndNil(FParsedDelphiRootFiles);
  FreeAndNil(FParsedDelphiFiles);
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
    if Assigned(UnitScopeNames) then
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
  FParsedDelphiFiles.TryGetValue(UpperCase(DelphiUnitName), Result);
end;

function TDudsModel.CreateDelphiFile(const DelphiUnitName: String; IsRootFile: Boolean): TDelphiFile;
begin
  Result := TDelphiFile.Create;

  if DelphiUnitName.IsEmpty then
    raise Exception.Create('empty unit name not allowed - parser error?');

  FParsedDelphiFiles.Add(UpperCase(DelphiUnitName), Result);
  FDelphiFileList.Add(Result);

  if IsRootFile then
    FParsedDelphiRootFiles.Add(Result);
end;

end.
