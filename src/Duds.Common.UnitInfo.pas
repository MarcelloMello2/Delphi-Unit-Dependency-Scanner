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

unit Duds.Common.UnitInfo;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,

  Duds.Common.Utils,
  Duds.Common.Files,
  Duds.Common.Types,
  Duds.Common.Interfaces,
  Duds.Modules.Classes;

type
  TUnitInfo = class(TInterfacedObject, IUnitInfo)
  strict private
    FDelphiUnitName: String;
    FFilename: String;
    FLineCount: Integer;
    FDelphiUnitNamePosition: Integer;
    FDelphiFileType: TDelphiFileType;
    FUsedUnits: TList<IUsedUnitInfo>;
    FPreviousUnitName: String;
    fModule: TModule;
  private
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
  public
    property DelphiUnitName: String read GetDelphiUnitName write SetDelphiUnitName;
    property Filename: String read GetFilename write SetFilename;
    property LinesOfCode: Integer read GetLinesOfCode write SetLinesOfCode;
    property DelphiUnitNamePosition: Integer read GetDelphiUnitNamePosition write SetDelphiUnitNamePosition;
    property DelphiFileType: TDelphiFileType read GetDelphiFileType write SetDelphiFileType;
    property UsedUnits: TList<IUsedUnitInfo> read GetUsedUnits;
    property PreviousUnitName: String read GetPreviousUnitName write SetPreviousUnitName;
    property Module: TModule read GetModule write SetModule;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TUnitInfo }

constructor TUnitInfo.Create;
begin
  FUsedUnits := TList<IUsedUnitInfo>.Create;
end;

destructor TUnitInfo.Destroy;
begin
  FreeAndNil(FUsedUnits);

  inherited;
end;

function TUnitInfo.GetDelphiFileType: TDelphiFileType;
begin
  Result := FDelphiFileType;
end;

function TUnitInfo.GetFilename: String;
begin
  Result := FFilename;
end;

function TUnitInfo.GetLinesOfCode: Integer;
begin
  Result := FLineCount;
end;

function TUnitInfo.GetModule: TModule;
begin
  Result := fModule;
end;

function TUnitInfo.GetPreviousUnitName: String;
begin
  Result := FPreviousUnitName;
end;

function TUnitInfo.GetDelphiUnitName: String;
begin
  Result := FDelphiUnitName;
end;

function TUnitInfo.GetDelphiUnitNamePosition: Integer;
begin
  Result := FDelphiUnitNamePosition;
end;

function TUnitInfo.GetUsedUnits: TList<IUsedUnitInfo>;
begin
  Result := FUsedUnits;
end;

procedure TUnitInfo.SetDelphiFileType(const Value: TDelphiFileType);
begin
  FDelphiFileType := Value;
end;

procedure TUnitInfo.SetFilename(const Value: String);
begin
  FFilename := Value;
end;

procedure TUnitInfo.SetLinesOfCode(const Value: Integer);
begin
  FLineCount := Value;
end;

procedure TUnitInfo.SetModule(const Value: TModule);
begin
  fModule := Value;
end;

procedure TUnitInfo.SetPreviousUnitName(const Value: String);
begin
  FPreviousUnitName := Value;
end;

procedure TUnitInfo.SetDelphiUnitName(const Value: String);
begin
  FDelphiUnitName := Value;
end;

procedure TUnitInfo.SetDelphiUnitNamePosition(const Value: Integer);
begin
  FDelphiUnitNamePosition := Value;
end;

end.
