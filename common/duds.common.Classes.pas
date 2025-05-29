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

unit duds.common.Classes;

interface

uses
  System.Classes, System.IniFiles, System.SysUtils, System.Generics.Collections,

  duds.common.Helper.IniFile,
  duds.common.Interfaces,
  duds.common.Types;

type
  TNodeData = record
    ID: Integer;
    Index: Cardinal;
  end;
  PNodeData = ^TNodeData;

  TDelphiFile = class(TObject)
  strict private
    FUnitInfo: IUnitInfo;
    FUsedCount: Integer;
    FInSearchPath: Boolean;
    FBaseNode: Pointer;
  public
    constructor Create;

    property UnitInfo: IUnitInfo read FUnitInfo write FUnitInfo;
    property UsedCount: Integer read FUsedCount write FUsedCount;
    property InSearchPath: Boolean read FInSearchPath write FInSearchPath;
    property BaseTreeNode: Pointer read FBaseNode write FBaseNode;
  end;

  TNodeObject = class(TObject)
  strict private
    FDelphiFile: TDelphiFile;
    FLink: Pointer;
    FCircularReference: TCircularRelationshipType;
    FSearchTermInChildren: Boolean;
    fUnknownModuleInChildren: Boolean;
  public
    property Link: Pointer read FLink write FLink;
    property CircularReference: TCircularRelationshipType read FCircularReference write FCircularReference;
    property DelphiFile: TDelphiFile read FDelphiFile write FDelphiFile;
    property SearchTermInChildren: Boolean read FSearchTermInChildren write FSearchTermInChildren;
    property UnknownModuleInChildren: Boolean read fUnknownModuleInChildren write fUnknownModuleInChildren;
  end;

  TFileInfo = class(TObject)
  private
    FFilename: UnicodeString;
    FFilesize: Int64;
    FModifiedDate: TDateTime;
    FCreatedDate: TDateTime;
    FIsDir: Boolean;
  public
    property Filename: UnicodeString read FFilename write FFilename;
    property Filesize: Int64 read FFilesize write FFilesize;
    property ModifiedDate: TDateTime read FModifiedDate write FModifiedDate;
    property CreatedDate: TDateTime read FCreatedDate write FCreatedDate;
    property IsDir: Boolean read FIsDir write FIsDir;
  end;

  TBaseSettings = class
  protected
    procedure DoSaveToIniFile(const IniFile: TIniFile); virtual; abstract;
    procedure DoLoadFromIniFile(const IniFile: TIniFile); virtual; abstract;
  public
    procedure Clear; virtual;

    procedure SaveToFile(const Filename: String);
    function LoadFromFile(const Filename: String): Boolean;
  end;

  TProjectSettings = class(TBaseSettings)
  strict private
    FRootFiles: TStringList;
    FSearchPaths: TStringList;
    fDefines: TStringList;
    fUseDefines: Boolean;
    FUnitScopeNames: TStringList;
    FLinkUnits: Boolean;
    fModulesDefinitionFile: string;
  private
    procedure SetRootFiles(const Value: TStringList);
    procedure SetSearchPaths(const Value: TStringList);
    procedure SetUnitScopeNames(const Value: TStringList);
  protected
    procedure DoSaveToIniFile(const IniFile: TIniFile); override;
    procedure DoLoadFromIniFile(const IniFile: TIniFile); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;

    property RootFiles: TStringList read FRootFiles write SetRootFiles;
    property SearchPaths: TStringList read FSearchPaths write SetSearchPaths;
    property UseDefines: Boolean read fUseDefines write fUseDefines;
    property Defines: TStringList read fDefines write fDefines;
    property UnitScopeNames: TStringList read FUnitScopeNames write SetUnitScopeNames;
    property LinkUnits: Boolean read FLinkUnits write FLinkUnits;
    property ModulesDefinitionFile: string read fModulesDefinitionFile write fModulesDefinitionFile;

  end;

  TEnvironmentSettings = class(TBaseSettings)
  strict private
    FStatusLogHeight: Integer;
    FTreeWidth: Integer;
    FListWidth: Integer;
    FShowUnitsNotInPath: Boolean;
    FProjectFilename: String;
    FLoadLastProject: Boolean;
    FRunScanOnLoad: Boolean;
    FWindowState: Integer;
    FWindowLeft: Integer;
    FWindowTop: Integer;
    FWindowWidth: Integer;
    FWindowHeight: Integer;
    FDummyRun: Boolean;
    FRenameHistoryFiles: Boolean;
    FRenameInsertOldNameComment: Boolean;
    FRenameLowerCaseExtension: Boolean;
    FUnitPatchHeight: integer;
  protected
    procedure DoSaveToIniFile(const IniFile: TIniFile); override;
    procedure DoLoadFromIniFile(const IniFile: TIniFile); override;
  public
    constructor Create;
    destructor Destroy; override;

    property StatusLogHeight: Integer read FStatusLogHeight write FStatusLogHeight;
    property TreeWidth: Integer read FTreeWidth write FTreeWidth;
    property ListWidth: Integer read FListWidth write FListWidth;
    property ShowUnitsNotInPath: Boolean read FShowUnitsNotInPath write FShowUnitsNotInPath;
    property ProjectFilename: String read FProjectFilename write FProjectFilename;
    property LoadLastProject: Boolean read FLoadLastProject write FLoadLastProject;
    property RunScanOnLoad: Boolean read FRunScanOnLoad write FRunScanOnLoad;
    property WindowState: Integer read FWindowState write FWindowState;
    property WindowLeft: Integer read FWindowLeft write FWindowLeft;
    property WindowTop: Integer read FWindowTop write FWindowTop;
    property WindowWidth: Integer read FWindowWidth write FWindowWidth;
    property WindowHeight: Integer read FWindowHeight write FWindowHeight;
    property DummyRun: Boolean read FDummyRun write FDummyRun;
    property RenameHistoryFiles: Boolean read FRenameHistoryFiles write FRenameHistoryFiles;
    property RenameInsertOldNameComment: Boolean read FRenameInsertOldNameComment write FRenameInsertOldNameComment;
    property RenameLowerCaseExtension: Boolean read FRenameLowerCaseExtension write FRenameLowerCaseExtension;
    property UnitPatchHeight: Integer read FUnitPatchHeight write FUnitPatchHeight;
  end;

implementation

{ TDelphiFile }

constructor TDelphiFile.Create;
begin
  FUnitInfo := nil;
  FBaseNode := nil;
end;

{ TBaseSettings }

procedure TBaseSettings.Clear;
begin

end;

function TBaseSettings.LoadFromFile(const Filename: String): Boolean;
var
  IniFile: TIniFile;
begin
  Result := (Filename <> '') and (FileExists(Filename));

  if Result then
  begin
    Clear;
    IniFile := TIniFile.Create(Filename);
    try
      DoLoadFromIniFile(IniFile);
    finally
      FreeAndNil(IniFile);
    end;
  end;
end;

procedure TBaseSettings.SaveToFile(const Filename: String);
var
  IniFile: TIniFile;
begin
  if Filename <> '' then
  begin
    IniFile := TIniFile.Create(Filename);
    try
      DoSaveToIniFile(IniFile);
    finally
      FreeAndNil(IniFile);
    end;
  end;
end;


{ TProjectSettings }

procedure TProjectSettings.Clear;
begin
  fRootFiles.Clear;
  fSearchPaths.Clear;
  fUseDefines := false;
  fDefines.Clear;
  fUnitScopeNames.Clear;
  fLinkUnits             := True;
  fModulesDefinitionFile := '';
end;

constructor TProjectSettings.Create;
begin
  FRootFiles := TStringList.Create;
  FSearchPaths := TStringList.Create;
  fDefines := TStringList.Create;
  FUnitScopeNames := TStringList.Create;
  FLinkUnits := True;

  FUnitScopeNames.Sorted := TRUE;
  FUnitScopeNames.Add('Windows');
end;

destructor TProjectSettings.Destroy;
begin
  FreeAndNil(FRootFiles);
  FreeAndNil(FUnitScopeNames);
  FreeAndNil(FSearchPaths);
  FreeAndNil(fDefines);

  inherited;
end;

procedure TProjectSettings.DoLoadFromIniFile(const IniFile: TIniFile);
begin
  FRootFiles.Clear;
  FSearchPaths.Clear;
  FUnitScopeNames.Clear;

  IniFile.ReadStrings('RootFiles', FRootFiles);
  IniFile.ReadStrings('SearchPaths', FSearchPaths);
  fUseDefines := IniFile.ReadBool('Settings', 'UseDefines', fUseDefines);
  IniFile.ReadStrings('Defines', fDefines);
  IniFile.ReadStrings('UnitScopeNames', FUnitScopeNames);
  FLinkUnits := IniFile.ReadBool('Settings', 'LinkUnits', FLinkUnits);
  fModulesDefinitionFile := IniFile.ReadString('Settings', 'ModulesDefinitionFile', fModulesDefinitionFile);
end;

procedure TProjectSettings.DoSaveToIniFile(const IniFile: TIniFile);
begin
  IniFile.WriteStrings('RootFiles', FRootFiles);
  IniFile.WriteStrings('SearchPaths', FSearchPaths);
  IniFile.WriteBool('Settings', 'UseDefines', fUseDefines);
  IniFile.WriteStrings('Defines', fDefines);
  IniFile.WriteStrings('UnitScopeNames', FUnitScopeNames);
  IniFile.WriteBool('Settings', 'LinkUnits', FLinkUnits);
  IniFile.WriteString('Settings', 'ModulesDefinitionFile', fModulesDefinitionFile);
end;

procedure TProjectSettings.SetRootFiles(const Value: TStringList);
begin
  FRootFiles.Assign(Value);
end;

procedure TProjectSettings.SetSearchPaths(const Value: TStringList);
begin
  FSearchPaths.Assign(Value);
end;

procedure TProjectSettings.SetUnitScopeNames(const Value: TStringList);
begin
  FUnitScopeNames.Assign(Value);
end;

{ TEnvironmentSettings }

const
  ControlSection = 'Control';
  WindowSection = 'Window';
  ProjectSection = 'Projects';
  DialogsSection = 'Dialogs';

constructor TEnvironmentSettings.Create;
begin

end;

destructor TEnvironmentSettings.Destroy;
begin

  inherited;
end;

procedure TEnvironmentSettings.DoLoadFromIniFile(const IniFile: TIniFile);
begin
  FStatusLogHeight := IniFile.ReadInteger(ControlSection, 'StatusLogHeight', FStatusLogHeight);
  FTreeWidth := IniFile.ReadInteger(ControlSection, 'TreeWidth', FTreeWidth);
  FListWidth := IniFile.ReadInteger(ControlSection, 'ListWidth', FListWidth);

  FShowUnitsNotInPath := IniFile.ReadBool(ProjectSection, 'ShowUnitsNotInPath', FShowUnitsNotInPath);
  FProjectFilename := IniFile.ReadString(ProjectSection, 'ProjectFilename', FProjectFilename);
  FLoadLastProject := IniFile.ReadBool(ProjectSection, 'LoadLastProject', FLoadLastProject);
  FRunScanOnLoad := IniFile.ReadBool(ProjectSection, 'RunScanOnLoad', FRunScanOnLoad);

  FWindowState := IniFile.ReadInteger(WindowSection, 'WindowState', FWindowState);
  FWindowLeft := IniFile.ReadInteger(WindowSection, 'WindowLeft', FWindowLeft);
  FWindowTop := IniFile.ReadInteger(WindowSection, 'WindowTop', FWindowTop);
  FWindowWidth := IniFile.ReadInteger(WindowSection, 'WindowWidth', FWindowWidth);
  FWindowHeight := IniFile.ReadInteger(WindowSection, 'WindowHeight', FWindowHeight);
  FUnitPatchHeight := IniFile.ReadInteger(WindowSection, 'UnitPatchHeight', 19);

  FDummyRun                   := IniFile.ReadBool(DialogsSection, 'DummyRun', FDummyRun);
  FRenameHistoryFiles         := IniFile.ReadBool(DialogsSection, 'RenameHistoryFiles', FRenameHistoryFiles);
  FRenameInsertOldNameComment := IniFile.ReadBool(DialogsSection, 'RenameInsertOldNameComment', FRenameInsertOldNameComment);
  FRenameLowerCaseExtension   := IniFile.ReadBool(DialogsSection, 'RenameLowerCaseExtension', FRenameLowerCaseExtension);
end;

procedure TEnvironmentSettings.DoSaveToIniFile(const IniFile: TIniFile);
begin
  IniFile.WriteInteger(ControlSection, 'StatusLogHeight', FStatusLogHeight);
  IniFile.WriteInteger(ControlSection, 'TreeWidth', FTreeWidth);
  IniFile.WriteInteger(ControlSection, 'ListWidth', FListWidth);

  IniFile.WriteBool(ProjectSection, 'ShowUnitsNotInPath', FShowUnitsNotInPath);
  IniFile.WriteString(ProjectSection, 'ProjectFilename', FProjectFilename);
  IniFile.WriteBool(ProjectSection, 'LoadLastProject', FLoadLastProject);
  IniFile.WriteBool(ProjectSection, 'RunScanOnLoad', FRunScanOnLoad);

  IniFile.WriteInteger(WindowSection, 'WindowState', FWindowState);
  IniFile.WriteInteger(WindowSection, 'WindowLeft', FWindowLeft);
  IniFile.WriteInteger(WindowSection, 'WindowTop', FWindowTop);
  IniFile.WriteInteger(WindowSection, 'WindowWidth', FWindowWidth);
  IniFile.WriteInteger(WindowSection, 'WindowHeight', FWindowHeight);
  IniFile.WriteInteger(WindowSection, 'UnitPatchHeight', FUnitPatchHeight);

  IniFile.WriteBool(DialogsSection, 'DummyRun', FDummyRun);
  IniFile.WriteBool(DialogsSection, 'RenameHistoryFiles', FRenameHistoryFiles);
  IniFile.WriteBool(DialogsSection, 'RenameInsertOldNameComment', FRenameInsertOldNameComment);
  IniFile.WriteBool(DialogsSection, 'RenameLowerCaseExtension', FRenameLowerCaseExtension);
end;

end.
