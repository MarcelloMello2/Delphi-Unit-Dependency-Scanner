unit duds.common.modules;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  System.JSON, IOUtils, Contnrs,

  duds.common.Types,
  duds.common.Interfaces,
  duds.common.Language,
  duds.common.Log;

type
  // key in this dictionary is the modules name (must be unique)
  TModulesDefinition = class
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


implementation

{ TModulesDefinition }

procedure TModulesDefinition.AddModule(Module: TModule);
begin
  fDictionary.Add(Module.Name, Module);
  fOrderedModules.Add(Module);
  Module.ID := fOrderedModules.Count; // save unique id in TModule for easy usage in e.g. export to graphml
end;

procedure TModulesDefinition.Clear;
begin
  fUnitsToModulesSearchlist.Clear;
  fPathsToModulesSearchlist.Clear;
  fUnknownModule := nil;
  fOrderedModules.Clear;
  fDictionary.Clear;
end;

constructor TModulesDefinition.Create;
begin
  fDictionary     := TObjectDictionary<String, TModule>.Create([doOwnsValues]);

  fOrderedModules := TObjectList<TModule>.Create(false);

  fUnitsToModulesSearchlist := TObjectDictionary<string, TModule>.Create;
  fPathsToModulesSearchlist := TObjectDictionary<string, TModule>.Create;

  fUnknownModule := nil;
end;

procedure TModulesDefinition.CreateUnknownModule;
begin
  if Assigned(fUnknownModule) then
    raise Exception.Create('unknown module already created');

  fUnknownModule      := TModule.Create(true);
  fUnknownModule.Name := 'unknown module';

  AddModule(fUnknownModule);
end;

destructor TModulesDefinition.Destroy;
begin
  fUnitsToModulesSearchlist.Free;
  fPathsToModulesSearchlist.Free;
  fOrderedModules.Free;
  fDictionary.Free;
  inherited;
end;

procedure TModulesDefinition.ClearModulesAnalysisData;
var
  aModule: TModule;
begin
  for aModule in fOrderedModules do
    aModule.AnalysisData.Clear;
end;

procedure TModulesDefinition.ReBuildFastSearchList;
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

function TModulesDefinition.FindModuleForUnit(UnitInfo: IUnitInfo; out Module: TModule): Boolean;
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

  // priority 1: check if unit name is defined in a module
  if fUnitsToModulesSearchlist.TryGetValue(aUnitNameLowerCase, aModule) then
  begin
     Module := aModule;
     Result := true;
     exit;
  end;

  // priority 2: check if path it defined in a module
  if aPathWithOutUnitName <> '' then
    if fPathsToModulesSearchlist.TryGetValue(aPathWithOutUnitName, aModule) then
    begin
       Module := aModule;
       Result := true;
       exit;
    end;
end;



end.
