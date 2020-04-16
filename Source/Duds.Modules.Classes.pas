unit Duds.Modules.Classes;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  System.JSON, IOUtils, Contnrs,

  Duds.Common.Interfaces,
  Duds.Common.Language,
  Duds.Common.Log;

type
  // key in this dictionary is the modules name (must be unique)
  TModulesList = class
  private
    fDictionary: TObjectDictionary<String, TModule>;               // this dictionary owns the TModule instances!
    fUnitsToModulesSearchlist: TObjectDictionary<string, TModule>;
    fPathsToModulesSearchlist: TObjectDictionary<string, TModule>;
    fOrderedModules: TObjectList<TModule>;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AddModule(Module: TModule);

    procedure ClearAllLists;
    procedure ReBuildFastSearchList;
    function FindModuleForUnit(UnitInfo: IUnitInfo; out Module: TModule): Boolean;

    property Dictionary: TObjectDictionary<String, TModule> read fDictionary;
    property OrderedModules: TObjectList<TModule> read fOrderedModules;

  end;

  TModulesSerializer = class
  public
    class procedure ReadFromJsonFile(aPathToFile: string; aModulesList: TModulesList);
    class procedure ReadFromJson(aContent: string; aModulesList: TModulesList);

  end;


implementation

{ TModulesSerializer }

class procedure TModulesSerializer.ReadFromJson(aContent: string;
  aModulesList: TModulesList);
var
  aJSONObject: TJSONObject;
  aModules: TJSONArray;
  aModule, aContains: TJsonObject;
  aModuleName: string;
  i, j: Integer;

  aNewModule: TModule;
  aPathsArray: TJsonArray;
  aUnitsArray: TJsonArray;
begin
  if not Assigned(aModulesList) then
    raise Exception.Create('aModulesList must be assigned');
  if aModulesList.Dictionary.Count > 0 then
    raise Exception.Create('aModulesList must be empty');


  aJSONObject := nil;
  try
    aJSONObject := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(aContent), 0) as TJSONObject;

    aModules := aJSONObject.GetValue('modules') as TJSONArray;
    for i := 0 to aModules.Count - 1 do
    begin
      aModule := aModules.Items[i] as TJsonObject;
      aModuleName := aModule.GetValue('name').AsType<string>;

      aNewModule := TModule.Create;
      aNewModule.Name := aModuleName;

      if aModule.TryGetValue<TJsonObject>('contains', aContains) then
      begin
         if aContains.TryGetValue<TJSONArray>('paths', aPathsArray) then
           for j := 0 to aPathsArray.Count - 1 do
             aNewModule.Paths.Add(IncludeTrailingPathDelimiter(aPathsArray.Items[j].Value).ToLower);

         if aContains.TryGetValue<TJSONArray>('units', aUnitsArray) then
           for j := 0 to aUnitsArray.Count - 1 do
             aNewModule.Units.Add(aUnitsArray.Items[j].Value.ToLower);
      end;

      aModulesList.AddModule(aNewModule);
    end;
  finally
    aJSONObject.Free;
  end;
end;

class procedure TModulesSerializer.ReadFromJsonFile(aPathToFile: string;
  aModulesList: TModulesList);
var
  aContent: string;
begin
  aContent := TFile.ReadAllText(aPathToFile);
  TModulesSerializer.ReadFromJson(aContent, aModulesList);
end;

{ TModulesList }

procedure TModulesList.AddModule(Module: TModule);
begin
  fDictionary.Add(Module.Name, Module);
  fOrderedModules.Add(Module);
end;

procedure TModulesList.ClearAllLists;
begin
  fUnitsToModulesSearchlist.Clear;
  fPathsToModulesSearchlist.Clear;
  fOrderedModules.Clear;
  fDictionary.Clear;
end;

constructor TModulesList.Create;
begin
  fDictionary     := TObjectDictionary<String, TModule>.Create([doOwnsValues]);

  fOrderedModules := TObjectList<TModule>.Create(false);

  fUnitsToModulesSearchlist := TObjectDictionary<string, TModule>.Create;
  fPathsToModulesSearchlist := TObjectDictionary<string, TModule>.Create;
end;

destructor TModulesList.Destroy;
begin
  fUnitsToModulesSearchlist.Free;
  fPathsToModulesSearchlist.Free;
  fOrderedModules.Free;
  fDictionary.Free;
  inherited;
end;

procedure TModulesList.ReBuildFastSearchList;
var
  aModule: TModule;
  currentUnit: string;
  currentPath: string;
  aAlreadyInModule: TModule;
begin
  // gather all units from all modules
  fUnitsToModulesSearchlist.Clear;
  for aModule in fOrderedModules do
    for currentUnit in aModule.Units do
    begin
      if fUnitsToModulesSearchlist.TryGetValue(currentUnit, aAlreadyInModule) then
         TDudsLogger.GetInstance.Log(StrAmbigousUnitToModuleMapping, [currentUnit, aAlreadyInModule.Name, aModule.Name], LogError)
      else
         fUnitsToModulesSearchlist.Add(currentUnit, aModule);
    end;

  // gather all paths from all modules
  fPathsToModulesSearchlist.Clear;
  for aModule in fOrderedModules do
    for currentPath in aModule.Paths do
    begin
      if not currentPath.IsEmpty then
        if fPathsToModulesSearchlist.TryGetValue(currentPath, aAlreadyInModule) then
           TDudsLogger.GetInstance.Log(StrAmbigousPathToModuleMapping, [currentPath, aAlreadyInModule.Name, aModule.Name], LogError)
        else
           fPathsToModulesSearchlist.Add(currentPath, aModule);
    end;
end;

function TModulesList.FindModuleForUnit(UnitInfo: IUnitInfo; out Module: TModule): Boolean;
var
  aModule: TModule;
  i: Integer;
  aUnitNameLowerCase: string;
  aPathWithOutUnitName: string;
begin
  Result := false;

  if UnitInfo.Filename.IsEmpty then // will be empty when unit is not in search path
  begin
    aUnitNameLowerCase   := UnitInfo.DelphiUnitName.ToLower;
    aPathWithOutUnitName := '';
  end else
  begin
    aUnitNameLowerCase   := ChangeFileExt(ExtractFileName(UnitInfo.Filename), '').ToLower;
    aPathWithOutUnitName := IncludeTrailingPathDelimiter(ExtractFilePath(UnitInfo.Filename)).ToLower;
  end;

  // check if unit name is defined in a module
  if fUnitsToModulesSearchlist.TryGetValue(aUnitNameLowerCase, aModule) then
  begin
     Module := aModule;
     Result := true;
     exit;
  end;

  // check if path it defined in a module
  if aPathWithOutUnitName <> '' then
    if fPathsToModulesSearchlist.TryGetValue(aPathWithOutUnitName, aModule) then
    begin
       Module := aModule;
       Result := true;
       exit;
    end;
end;



end.
