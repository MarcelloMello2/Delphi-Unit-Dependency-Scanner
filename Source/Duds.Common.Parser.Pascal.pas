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

unit Duds.Common.Parser.Pascal;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,

  Duds.Common.Utils,
  Duds.Common.Files,
  Duds.Common.Types,
  Duds.Common.Interfaces,
  Duds.Common.Parser.Pascal.Tokeniser,
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
    function GetLineCount: Integer;
    function GetDelphiUnitNamePosition: Integer;
    function GetUsedUnits: TList<IUsedUnitInfo>;
    function GetPreviousUnitName: String;
    function GetModule: TModule;

    procedure SetDelphiUnitName(const Value: String);
    procedure SetDelphiFileType(const Value: TDelphiFileType);
    procedure SetFilename(const Value: String);
    procedure SetLineCount(const Value: Integer);
    procedure SetDelphiUnitNamePosition(const Value: Integer);
    procedure SetPreviousUnitName(const Value: String);
    procedure SetModule(const Value: TModule);
  public
    property DelphiUnitName: String read GetDelphiUnitName write SetDelphiUnitName;
    property Filename: String read GetFilename write SetFilename;
    property LineCount: Integer read GetLineCount write SetLineCount;
    property DelphiUnitNamePosition: Integer read GetDelphiUnitNamePosition write SetDelphiUnitNamePosition;
    property DelphiFileType: TDelphiFileType read GetDelphiFileType write SetDelphiFileType;
    property UsedUnits: TList<IUsedUnitInfo> read GetUsedUnits;
    property PreviousUnitName: String read GetPreviousUnitName write SetPreviousUnitName;
    property Module: TModule read GetModule write SetModule;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TUsedUnitInfo = class(TInterfacedObject, IUsedUnitInfo)
  strict private
    FDelphiUnitName: String;
    FPosition: Integer;
    FInFilePosition: Integer;
    FUsesType: TUsedUnitType;
    FFilename: String;
  private
    function GetPosition: Integer;
    function GetInFilePosition: Integer;
    function GetDelphiUnitName: String;
    function GetUsesType: TUsedUnitType;

    procedure SetPosition(const Value: Integer);
    procedure SetInFilePosition(const Value: Integer);
    procedure SetDelphiUnitName(const Value: String);
    procedure SetUsesType(const Value: TUsedUnitType);
    procedure SetFilename(const Value: String);
    function GetFilename: String;
  public
    procedure UpdatePosition(Offset: Integer);

    property DelphiUnitName: String read GetDelphiUnitName write SetDelphiUnitName;
    property Position: Integer read GetPosition write SetPosition;
    property InFilePosition: Integer read GetInFilePosition write SetInFilePosition;
    property UsesType: TUsedUnitType read GetUsesType write SetUsesType;
    property Filename: String read GetFilename write SetFilename;
  end;

  TUsedUnits = class(TList<IUsedUnitInfo>);

  TPascalUnitExtractor = class(TComponent)
  private
    FTokeniser: TPascalTokeniser;
    FTokeniserInFile: TPascalTokeniser;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetUsedUnits(const UnitFileName: String; var UnitInfo: IUnitInfo): Boolean;
  end;

const
  DelphiFileTypeStrings: Array[TDelphiFileType] of String = (
    'Unknown',
    'Pascal',
    'Program',
    'Package',
    'Library'
  );

  UsesTypeStrings: Array[TUsedUnitType] of String = (
    'Interface',
    'Implementation',
    'Contains'
  );

implementation

resourcestring
  StrUnsupportedFileType = 'Unsupported file type "%s"';

constructor TPascalUnitExtractor.Create(AOwner: TComponent);
begin
  inherited;

  FTokeniser := TPascalTokeniser.Create;
  FTokeniserInFile := TPascalTokeniser.Create;
end;

destructor TPascalUnitExtractor.Destroy;
begin
  FreeAndNil(FTokeniser);
  FreeAndNil(FTokeniserInFile);

  inherited;
end;

function TPascalUnitExtractor.GetUsedUnits(const UnitFileName: String; var UnitInfo: IUnitInfo): Boolean;

  function StripQuotes(const Value: String): String;
  begin
    Result := Trim(Value);

    while (Result.Length > 0) and
          (CharInSet(Result[1], ['''', '"'])) do
      Result := copy(Result, 2, MaxInt);

    while (Result.Length > 0) and
          (CharInSet(Result[Result.Length], ['''', '"'])) do
      Result := copy(Result, 1, Result.Length - 1);

    Result := Trim(Result);
  end;

  procedure ExtractUnits(UsesType: TUsedUnitType);
  var
    Delimiter, UnitName: String;
    UsedUnitInfo: IUsedUnitInfo;
    PosOfUnitNameInPath, InFilePos: Integer;
  begin
    while not FTokeniser.Eof do
    begin
      FTokeniser.Next([',',';'], Delimiter);

      if not FTokeniser.Eof then
      begin
        UsedUnitInfo := TUsedUnitInfo.Create;
        UnitInfo.UsedUnits.Add(UsedUnitInfo);

        FTokeniserInFile.Code := FTokeniser.Token.Text;

        InFilePos := 0;

        // when there are no further tokens, it must be a uses without a file declaration "in 'abc.pas'"
        if FTokeniserInFile.Eof then
        begin
          UnitName := FTokeniser.Token.Text;
        end
        else
        begin
          // Get the real unit text
          UnitName := FTokeniserInFile.Token.Text;

          FTokeniserInFile.Next;

          // Is the token 'in' a file?
          if SameText(FTokeniserInFile.Token.Text, 'in') then
          begin
            // Find the text between '...' -> it should be the filename / absolute filepath / relative filepath
            //   originally, this used 'FTokeniserInFile.Next()' but that crashes with spaces in paths
            FTokeniserInFile.Next(['''']);
            FTokeniserInFile.Next(['''']);

            PosOfUnitNameInPath := pos(LowerCase(UnitName + '.pas'), LowerCase(FTokeniserInFile.Token.Text));

            if PosOfUnitNameInPath > 0 then
            begin
              UsedUnitInfo.Filename := ExpandFileNameRelBaseDir(StripQuotes(FTokeniserInFile.Token.Text), ExtractFilePath(UnitFilename));

              InFilePos := FTokeniser.Token.Position + FTokeniserInFile.Token.Position + PosOfUnitNameInPath - 2 - 1;
            end else
            begin
              // error -> create a "fake unit name" that will cause an error in the oupt because it cannot be found on the disk
              UsedUnitInfo.Filename := ExpandFileNameRelBaseDir(UnitName + '.failed to parse "in file" path - please check source', ExtractFilePath(UnitFilename));
              InFilePos             := 0;
            end;
          end;
        end;

        UsedUnitInfo.DelphiUnitName := Trim(UnitName);
        UsedUnitInfo.Position       := FTokeniser.Token.Position - 1;
        UsedUnitInfo.InFilePosition := InFilePos;
        UsedUnitInfo.UsesType       := UsesType;
      end;

      if Delimiter = ';' then
        Break;
    end;
  end;

const
  InterfaceStr = 'interface';
  ImplementationStr = 'implementation';
  UsesStr = 'uses';
  ContainsStr = 'contains';
  UnitStr = 'unit';
  ProgramStr = 'program';
  PackageStr = 'package';
  LibraryStr = 'Library';
var
  UnitStrings: TStringList;
  Extension: String;
begin
  Result := TRUE;

  UnitInfo := TUnitInfo.Create;
  UnitInfo.Filename := UnitFileName;

  Extension := ExtractFileExt(UnitFilename);

  UnitStrings := TStringList.Create;
  try
    UnitStrings.LoadFromFile(UnitFilename);

    UnitInfo.LineCount := UnitStrings.Count;

    FTokeniser.Code := UnitStrings.Text;
  finally
    FreeAndNil(UnitStrings);
  end;

  if SameText(UnitStr, FTokeniser.Token.Text) then
    UnitInfo.DelphiFileType := ftPAS else
  if SameText(ProgramStr, FTokeniser.Token.Text) then
    UnitInfo.DelphiFileType := ftDPR else
  if SameText(PackageStr, FTokeniser.Token.Text) then
    UnitInfo.DelphiFileType := ftDPK else
  if SameText(LibraryStr, FTokeniser.Token.Text) then
    UnitInfo.DelphiFileType := ftLIB
  else
  begin
    UnitInfo.DelphiFileType := ftUnknown;

    Exit(FALSE);
  end;

  // Move to the unit name
  FTokeniser.Next;

  UnitInfo.DelphiUnitName := FTokeniser.Token.Text;

  if SameText(UnitInfo.DelphiUnitName, ExtractFilenameNoExt(UnitFilename)) then
  begin
    // This should always be the case as a unit name should match the filename
    UnitInfo.DelphiUnitNamePosition := FTokeniser.Token.Position - 1;
  end;

  case UnitInfo.DelphiFileType of
    ftPAS:
      begin
        if FTokeniser.FindNextToken(InterfaceStr) then
        begin
          if FTokeniser.FindNextToken([UsesStr, ImplementationStr]) then
          begin
            if SameText(FTokeniser.Token.Text, UsesStr)  then
            begin
              ExtractUnits(utInterface);

              if not FTokeniser.FindNextToken(ImplementationStr) then
                Exit;
            end;

            if FTokeniser.FindNextToken(UsesStr) then
              ExtractUnits(utImplementation);
          end;
        end;
      end;

    ftDPR,
    ftLIB:
      begin
        if FTokeniser.FindNextToken(UsesStr) then
          ExtractUnits(utImplementation);
      end;

    ftDPK:
      begin
        if FTokeniser.FindNextToken(ContainsStr) then
          ExtractUnits(utContains);
      end;
    // Unknown file type
    else
      Result := FALSE;
  end;
end;

{ TUnitInfo }

function TUsedUnitInfo.GetFilename: String;
begin
  Result := FFilename;
end;

function TUsedUnitInfo.GetInFilePosition: Integer;
begin
  Result := FInFilePosition;
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

function TUnitInfo.GetLineCount: Integer;
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

procedure TUnitInfo.SetLineCount(const Value: Integer);
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
