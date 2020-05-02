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

unit duds.common.UsedUnitInfo;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,

  duds.common.Utils,
  duds.common.Files,
  duds.common.Types,
  duds.common.Interfaces,
  duds.common.modules;

type
  TUsedUnitInfo = class(TInterfacedObject, IUsedUnitInfo)
  strict private
    FDelphiUnitName: String;
    FPosition: Integer;
    fIsInIncludeFileName: string;
    FUsesType: TUsedUnitType;

    fDefinedFilePath: String;
    FFilename: String;
    FInFilePosition: Integer;
  private
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
  public
    procedure UpdatePosition(Offset: Integer);

    property DelphiUnitName: String read GetDelphiUnitName write SetDelphiUnitName;
    property Position: Integer read GetPosition write SetPosition;
    property IsInIncludeFileName: string read GetIsInIncludeFileName write SetIsInIncludeFileName;

    property UsesType: TUsedUnitType read GetUsesType write SetUsesType;

    property DefinedFilePath: String read GetDefinedFilePath write SetDefinedFilePath;
    property Filename: String read GetFilename write SetFilename;
    property InFilePosition: Integer read GetInFilePosition write SetInFilePosition;
  end;

implementation

{ TUnitInfo }

function TUsedUnitInfo.GetFilename: String;
begin
  Result := FFilename;
end;

function TUsedUnitInfo.GetDefinedFilePath: string;
begin
  Result := fDefinedFilePath;
end;

function TUsedUnitInfo.GetInFilePosition: Integer;
begin
  Result := FInFilePosition;
end;

function TUsedUnitInfo.GetIsInIncludeFileName: string;
begin
  Result := fIsInIncludeFileName;
end;

function TUsedUnitInfo.GetPosition: Integer;
begin
  Result := FPosition;
end;

function TUsedUnitInfo.GetDelphiUnitName: String;
begin
  Result := FDelphiUnitName;
end;

function TUsedUnitInfo.GetUsesType: TUsedUnitType;
begin
  Result := FUsesType;
end;

procedure TUsedUnitInfo.SetInFilePosition(const Value: Integer);
begin
  FInFilePosition := Value;
end;

procedure TUsedUnitInfo.SetIsInIncludeFileName(const Value: String);
begin
  fIsInIncludeFileName := Value;
end;

procedure TUsedUnitInfo.SetPosition(const Value: Integer);
begin
  FPosition := Value;
end;

procedure TUsedUnitInfo.SetDelphiUnitName(const Value: String);
begin
  FDelphiUnitName := Value;
end;

procedure TUsedUnitInfo.SetFilename(const Value: String);
begin
  FFilename := Value;
end;

procedure TUsedUnitInfo.SetDefinedFilePath(const Value: String);
begin
  fDefinedFilePath := Value;
end;

procedure TUsedUnitInfo.SetUsesType(const Value: TUsedUnitType);
begin
  FUsesType := Value;
end;

procedure TUsedUnitInfo.UpdatePosition(Offset: Integer);
begin
  if Position > 0 then
    Position := Position + Offset;

  if InFilePosition > 0 then
    InFilePosition := InFilePosition + Offset;
end;

end.
