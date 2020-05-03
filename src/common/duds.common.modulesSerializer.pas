unit duds.common.modulesSerializer;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  System.JSON, IOUtils, Contnrs,

  duds.common.Types,
  duds.common.Interfaces,
  duds.common.Language,
  duds.common.Log,
  duds.common.files,
  duds.common.modules;

type
  TModulesSerializer = class
  public
    const
    cModuleOriginJSONEnumValues : array[TModuleOrigin] of string = ('undefined', 'delphi', '3rdParty', 'own');
    cModuleUsageJSONEnumValues  : array[TModuleUsage] of string = ('undefined', 'production', 'test');

    class procedure ReadFromJsonFile(aModulesFileName: string; aModulesList:
        TModulesDefinition);
    class procedure ReadFromJson(aModulesFileName, aContent: string; aModulesList:
        TModulesDefinition);

  end;

implementation

{ TModulesSerializer }

class procedure TModulesSerializer.ReadFromJson(aModulesFileName, aContent:
    string; aModulesList: TModulesDefinition);

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
  aPath: string;
  aUnit: string;
begin
  if not Assigned(aModulesList) then
    raise Exception.Create('aModulesList must be assigned');
  if aModulesList.Dictionary.Count > 0 then
    raise Exception.Create('aModulesList must be empty');


  aJSONObject := nil;
  try
    aJSONObject := TJSONObject.ParseJSONValue(TEncoding.UTF8.GetBytes(aContent), 0, [TJSONObject.TJSONParseOption.RaiseExc]) as TJSONObject;

    if not aJSONObject.TryGetValue<TJSONArray>('modules', aModules) then
      raise Exception.Create('"modules" element not found in json file');

    for i := 0 to aModules.Count - 1 do
    begin
      aModule     := aModules.Items[i] as TJsonObject;
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
            aNewModule.DefinedDependencies.Add(aReferencedModule)
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
           begin
             aPath := aPathsArray.Items[j].Value.ToLower;
             aPath := IncludeTrailingPathDelimiter(GetAbsolutePath(aPath, aModulesFileName)).ToLower;
             if aNewModule.Paths.IndexOf(aPath) = -1 then
               aNewModule.Paths.Add(aPath)
             else
               raise Exception.CreateFmt('Path "%s" in module "%s" is re-defined. Paths must be unique.',
                                         [aPath, aNewModule.Name]);
           end;

         if aContains.TryGetValue<TJSONArray>('units', aUnitsArray) then
           for j := 0 to aUnitsArray.Count - 1 do
           begin
             aUnit := aUnitsArray.Items[j].Value.ToLower;
             if aNewModule.Units.IndexOf(aUnit) = -1 then
               aNewModule.Units.Add(aUnit)
             else
               raise Exception.CreateFmt('Unit "%s" in module "%s" is re-defined. Units must be unique.',
                                         [aUnit, aNewModule.Name]);
           end;
      end;

      if aModulesList.Dictionary.ContainsKey(aNewModule.Name) then
        raise Exception.CreateFmt('Module "%s" is re-defined. Module names must be unique in the module definition.', [aNewModule.Name]);
      aModulesList.AddModule(aNewModule);
    end;
  finally
    aJSONObject.Free;
  end;
end;

class procedure TModulesSerializer.ReadFromJsonFile(aModulesFileName: string;
    aModulesList: TModulesDefinition);
var
  aContent: string;
begin
  try
    aContent := TFile.ReadAllText(aModulesFileName);
    TModulesSerializer.ReadFromJson(aModulesFileName, aContent, aModulesList);
  except
    on e: Exception do
    begin
     TDudsLogger.GetInstance.Log('Error while parsing the modules json file: %s', [e.Message], LogError);
      raise;
    end;
  end;
end;

end.
