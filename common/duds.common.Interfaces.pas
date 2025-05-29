//------------------------------------------------------------------------------
//
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/
//
// Alternatively, you may redistribute this library, use and/or modify it under
// the terms of the GNU Lesser General Public License as published by the
// Free Software Foundation; either version 2.1 of the License, or (at your
// option) any later version. You may obtain a copy of the LGPL at
// http://www.gnu.org/copyleft/.
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
// the specific language governing rights and limitations under the License.
//
// Orginally released as freeware then open sourced in July 2017.
//
// The initial developer of the original code is Easy-IP AS
// (Oslo, Norway, www.easy-ip.net), written by Paul Spencer Thornton -
// paul.thornton@easy-ip.net.
//
// (C) 2017 Easy-IP AS. All Rights Reserved.
//
//------------------------------------------------------------------------------

unit duds.common.Interfaces;

interface

uses
  System.Classes, System.TypInfo, System.Generics.Collections,
  System.SysUtils,

  duds.common.Types;

type
  IUsedUnitInfo = interface
    ['{C44B91EB-DA95-49DC-91F0-E0E3B2828610}']
    function GetPosition: Integer;
    function GetInFilePosition: Integer;
    function GetDelphiUnitName: String;
    function GetUsesType: TUsedUnitType;
    function GetFilename: String;
    function GetDefinedFilePath: string;
    function GetIsInIncludeFileName: string;

    procedure SetPosition(const Value: Integer);
    procedure SetInFilePosition(const Value: Integer);
    procedure SetDelphiUnitName(const Value: String);
    procedure SetUsesType(const Value: TUsedUnitType);
    procedure SetFilename(const Value: String);
    procedure SetDefinedFilePath(const Value: String);
    procedure SetIsInIncludeFileName(const Value: String);

    procedure UpdatePosition(Offset: Integer);

    property DelphiUnitName: String read GetDelphiUnitName write SetDelphiUnitName;
    property Position: Integer read GetPosition write SetPosition;
    property IsInIncludeFileName: string read GetIsInIncludeFileName write SetIsInIncludeFileName;
    property UsesType: TUsedUnitType read GetUsesType write SetUsesType;

    property DefinedFilePath: String read GetDefinedFilePath write SetDefinedFilePath; // [optional] file path as defined in the unit, e.g. '..\myUnit.pas'
    property Filename:        String read GetFilename        write SetFilename;        // [optional] resolved, absolute file path
    property InFilePosition: Integer read GetInFilePosition  write SetInFilePosition;  // [optional] offset of the  <in '..\..\abc.pas'>   part

  end;

  TModule = class;

  IUnitInfo = interface
  ['{F8DDA5FC-AF2B-4B35-A662-F15120502477}']
    function GetDelphiUnitName: String;
    function GetDelphiFileType: TDelphiFileType;
    function GetFilename: String;
    function GetLinesOfCode: Integer;
    function GetDelphiUnitNamePosition: Integer;
    function GetUsedUnits: TList<IUsedUnitInfo>;
    function GetPreviousUnitName: String;
    function GetModule: TModule;

    procedure SetDelphiUnitName(const Value: String);
    procedure SetDelphiFileType(const Value: TDelphiFileType);
    procedure SetFilename(const Value: String);
    procedure SetLinesOfCode(const Value: Integer);
    procedure SetDelphiUnitNamePosition(const Value: Integer);
    procedure SetPreviousUnitName(const Value: String);
    procedure SetModule(const Value: TModule);

    property DelphiUnitName: String read GetDelphiUnitName write SetDelphiUnitName;
    property Filename: String read GetFilename write SetFilename;
    property LinesOfCode: Integer read GetLinesOfCode write SetLinesOfCode;
    property DelphiUnitNamePosition: Integer read GetDelphiUnitNamePosition write SetDelphiUnitNamePosition;
    property DelphiFileType: TDelphiFileType read GetDelphiFileType write SetDelphiFileType;
    property UsedUnits: TList<IUsedUnitInfo> read GetUsedUnits;
    property PreviousUnitName: String read GetPreviousUnitName write SetPreviousUnitName;
    property Module: TModule read GetModule write SetModule;
  end;

  TUsedModuleData = class // Details about how a Module B (the "used" module) is used by Module A
  private
    fDependencyViolation: Boolean;
    fIncomingEdgesCount: Integer;
    fUsingUnitsNames: TStringList;
    fUsedUnitsNames: TStringList;

  public
    constructor Create;
    destructor Destroy; override;

    property DependencyViolation: Boolean read fDependencyViolation write fDependencyViolation;
    property IncomingEdgesCount: Integer  read fIncomingEdgesCount  write fIncomingEdgesCount;

    property UsingUnitsNames: TStringList  read fUsingUnitsNames    write fUsingUnitsNames;  // Which units in A are using a unit in B

    property UsedUnitsNames: TStringList  read fUsedUnitsNames      write fUsedUnitsNames;   // Which units in B are used by a unit in A


  end;

  TModuleAnalysisData = class // pojo
  private
    fLinesOfCode: Integer;
    fNumberOfFiles: Integer;
    fNumberOfFilesNotInPath: Integer;

    fAllowedDependencies: TObjectList<TModule>;
    fActualDependencies : TObjectDictionary<TModule, TUsedModuleData>;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure AddActualDependency(UsingUnitInfo: IUnitInfo; UsedModule: TModule;
        UsedUnitInfo: IUnitInfo);


    { AllowedDependencies = (directly) defined dependencies + all indirectly defined dependencies }
    property AllowedDependencies: TObjectList<TModule>   read fAllowedDependencies write fAllowedDependencies;

    { Actual dependencies with detailled usage data }
    property ActualDependencies: TObjectDictionary<TModule, TUsedModuleData> read fActualDependencies;

    property LinesOfCode: Integer            read fLinesOfCode            write fLinesOfCode;
    property NumberOfFiles: Integer          read fNumberOfFiles          write fNumberOfFiles;
    property NumberOfFilesNotInPath: Integer read fNumberOfFilesNotInPath write fNumberOfFilesNotInPath;

  end;

  TModule = class // pojo
  private
    fID: Integer;
    fName: string;
    fIsUnknownModule: Boolean;
    fUnits: TStringList;
    fPaths: TStringList;
    fOrigin: TModuleOrigin;
    fUsage: TModuleUsage;
    fDefinedDependencies: TObjectList<TModule>;
    fAnalysisData: TModuleAnalysisData;

  protected
    function GetName: string;
    function GetUnits: TStringList;
    function GetPaths: TStringList;
    procedure SetName(const Value: string);
    procedure SetUnits(const Value: TStringList);
    procedure SetPaths(const Value: TStringList);

  public
    constructor Create(IsUnknownModule: Boolean = false);
    destructor Destroy; override;


    property ID: Integer read fID write fID;
    property Name: string read GetName write SetName;
    property IsUnknownModule: Boolean read fIsUnknownModule;
    property Origin: TModuleOrigin read fOrigin write fOrigin;
    property Usage: TModuleUsage read fUsage write fUsage;

    { dependencies - as defined in modules definition file }
    property DefinedDependencies: TObjectList<TModule> read fDefinedDependencies write fDefinedDependencies;
    property Paths: TStringList read GetPaths write SetPaths;
    property Units: TStringList read GetUnits write SetUnits;

    property AnalysisData: TModuleAnalysisData read fAnalysisData write fAnalysisData;
  end;

implementation

{ TModule }

constructor TModule.Create(IsUnknownModule: Boolean = false);
begin
  fIsUnknownModule     := IsUnknownModule;
  fDefinedDependencies := TObjectList<TModule>.Create(false);
  fUnits               := TStringList.Create(TDuplicates.dupError, true, false);
  fPaths               := TStringList.Create(TDuplicates.dupError, true, false);
  fAnalysisData        := TModuleAnalysisData.Create;
end;

destructor TModule.Destroy;
begin
  FreeAndNil(fUnits);
  FreeAndNil(fPaths);
  FreeAndNil(fDefinedDependencies);
  FreeAndNil(fAnalysisData);
  inherited;
end;

function TModule.GetName: string;
begin
  Result := fName;
end;

function TModule.GetUnits: TStringList;
begin
  Result := fUnits;
end;

function TModule.GetPaths: TStringList;
begin
  Result := fPaths;
end;

procedure TModule.SetName(const Value: string);
begin
  fName := Value;
end;

procedure TModule.SetUnits(const Value: TStringList);
begin
  fUnits := Value;
end;

procedure TModule.SetPaths(const Value: TStringList);
begin
  fPaths := Value;
end;

{ TModuleAnalysisData }

constructor TModuleAnalysisData.Create;
begin
  fAllowedDependencies      := TObjectList<TModule>.Create(false);
  fActualDependencies := TObjectDictionary<TModule, TUsedModuleData>.Create([doOwnsValues]);
end;

destructor TModuleAnalysisData.Destroy;
begin
  fAllowedDependencies.Free;
  fActualDependencies.Free;
  inherited;
end;

procedure TModuleAnalysisData.Clear;
begin
  fLinesOfCode            := 0;
  fNumberOfFiles          := 0;
  fNumberOfFilesNotInPath := 0;
  fAllowedDependencies.Clear;
  fActualDependencies.Clear;
end;

procedure TModuleAnalysisData.AddActualDependency(UsingUnitInfo: IUnitInfo; UsedModule: TModule; UsedUnitInfo: IUnitInfo);
var
  aUsedModuleData : TUsedModuleData;
begin

  if not fActualDependencies.TryGetValue(UsedModule, aUsedModuleData) then
  begin
    aUsedModuleData                     := TUsedModuleData.Create;
    aUsedModuleData.DependencyViolation := not fAllowedDependencies.Contains(UsedModule);

    fActualDependencies.Add(UsedModule, aUsedModuleData);
  end;

  // how many differnt units use the "used module"?
  aUsedModuleData.UsingUnitsNames.Add(UsingUnitInfo.DelphiUnitName);

  // how many different units of the "used modules" are being used?
  aUsedModuleData.UsedUnitsNames.Add(UsedUnitInfo.DelphiUnitName);
end;

{ TUsedModuleData }

constructor TUsedModuleData.Create;
begin
  inherited Create;
  fUsingUnitsNames := TStringList.Create(TDuplicates.dupIgnore, true, false); // <- ignore duplicates
  fUsedUnitsNames  := TStringList.Create(TDuplicates.dupIgnore, true, false); // <- ignore duplicates
end;

destructor TUsedModuleData.Destroy;
begin
  fUsingUnitsNames.Free;
  fUsedUnitsNames.Free;
  inherited Destroy;
end;

end.
