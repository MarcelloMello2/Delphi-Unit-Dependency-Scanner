unit Duds.Modules.Classes;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  System.JSON, IOUtils, Contnrs,

  Duds.Common.Types,
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
    fUnknownModule: TModule;

  public
    constructor Create;
    destructor Destroy; override;

    procedure AddModule(Module: TModule);

    procedure Clear;
    procedure ReBuildFastSearchList;
    procedure ClearModulesAnalysisData;
    function FindModuleForUnit(UnitInfo: IUnitInfo; out Module: TModule): Boolean;

    procedure CreateUnknownModule;

    property UnknownModule: TModule read fUnknownModule;

    property Dictionary: TObjectDictionary<String, TModule> read fDictionary;
    property OrderedModules: TObjectList<TModule> read fOrderedModules;

  end;

  TModulesSerializer = class
  public
    const
    cModuleOriginJSONEnumValues : array[TModuleOrigin] of string = ('undefined', 'delphi', '3rdParty', 'own');
    cModuleUsageJSONEnumValues  : array[TModuleUsage] of string = ('undefined', 'production', 'test');

    class procedure ReadFromJsonFile(aPathToFile: string; aModulesList: TModulesList);
    class procedure ReadFromJson(aContent: string; aModulesList: TModulesList);

  end;


implementation

{ TModulesSerializer }

class procedure TModulesSerializer.ReadFromJson(aContent: string;
  aModulesList: TModulesList);

  function GetModuleOriginEnum(value: string): TModuleOrigin;
  var
    i : integer;
    origin: TModuleOrigin;
  begin
    Result := moUndefined;
    for origin := Low(TModuleOrigin) to High(TModuleOrigin) do
      if cModuleOriginJSONEnumValues[origin] = value then
      begin
        Result := origin;
        break;
      end;
  end;

  function GetModuleUsageEnum(value: string): TModuleUsage;
  var
    i : integer;
    usage: TModuleUsage;
  begin
    Result := muUndefined;
    for usage := Low(TModuleUsage) to High(TModuleUsage) do
      if cModuleUsageJSONEnumValues[usage] = value then
      begin
        Result := usage;
        break;
      end;
  end;

var
  aJSONObject: TJSONObject;
  aModules: TJSONArray;
  aModule, aContains: TJsonObject;
  aModuleName: string;
  aModuleOriginString: string;
  aModuleOrigin: TModuleOrigin;
  i, j: Integer;

  aNewModule: TModule;
  aPathsArray: TJsonArray;
  aUnitsArray: TJsonArray;
  aDependenciesArray: TJsonArray;
  aDependencyModuleName: string;
  aReferencedModule: TModule;
  aModuleUsageString: string;
  aModuleUsage: TModuleUsage;
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

      // module origin (delphi, 3rdParty, own code)
      aModuleOrigin := TModuleOrigin.moUndefined;
      if aModule.TryGetValue<string>('origin', aModuleOriginString) then
        aModuleOrigin := GetModuleOriginEnum(aModuleOriginString);

      // module usage (production or test code)
      aModuleUsage := muUndefined;
      if aModule.TryGetValue<string>('usage', aModuleUsageString) then
        aModuleUsage := GetModuleUsageEnum(aModuleUsageString);

      aNewModule        := TModule.Create;
      aNewModule.Name   := aModuleName;
      aNewModule.Origin := aModuleOrigin;
      aNewModule.Usage  := aModuleUsage;

      // read "dependencies" definition
      if aModule.TryGetValue<TJsonArray>('dependencies', aDependenciesArray) then
      begin
        for j := 0 to aDependenciesArray.Count - 1 do
        begin
          aDependencyModuleName := aDependenciesArray.Items[j].Value;
          if aModulesList.Dictionary.TryGetValue(aDependencyModuleName, aReferencedModule) then
            aNewModule.Dependencies.Add(aReferencedModule)
          else
            raise Exception.CreateFmt('Module "%s" references module "%s" as dependency. Reference could not be resolved, module not found. ' +
                                      'Module must be defined before in order the be able to reference it.', [aNewModule.Name, aDependencyModuleName]);
        end;
      end;

      // read "contains" definition
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
  Module.ID := fOrderedModules.Count; // save unique id in TModule for easy usage in e.g. export to graphml
end;

procedure TModulesList.Clear;
begin
  fUnitsToModulesSearchlist.Clear;
  fPathsToModulesSearchlist.Clear;
  fUnknownModule := nil;
  fOrderedModules.Clear;
  fDictionary.Clear;
end;

constructor TModulesList.Create;
begin
  fDictionary     := TObjectDictionary<String, TModule>.Create([doOwnsValues]);

  fOrderedModules := TObjectList<TModule>.Create(false);

  fUnitsToModulesSearchlist := TObjectDictionary<string, TModule>.Create;
  fPathsToModulesSearchlist := TObjectDictionary<string, TModule>.Create;

  fUnknownModule := nil;
end;

procedure TModulesList.CreateUnknownModule;
begin
  if Assigned(fUnknownModule) then
    raise Exception.Create('unknown module already created');

  fUnknownModule      := TModule.Create;
  fUnknownModule.Name := 'unknown module';

  AddModule(fUnknownModule);
end;

destructor TModulesList.Destroy;
begin
  fUnitsToModulesSearchlist.Free;
  fPathsToModulesSearchlist.Free;
  fOrderedModules.Free;
  fDictionary.Free;
  inherited;
end;

procedure TModulesList.ClearModulesAnalysisData;
var
  aModule: TModule;
begin
  for aModule in fOrderedModules do
    aModule.AnalysisData.Clear;
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
