unit duds.common.UsesParser;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, IOUtils,

  duds.common.Utils,
  duds.common.Files,
  duds.common.Types,
  duds.common.Interfaces,
  duds.common.UnitInfo,
  duds.common.UsedUnitInfo,
  duds.common.Log,
  duds.common.Language,
  duds.common.modules,

  SimpleParser.Lexer,
  SimpleParser.Lexer.Types;

type
  TUsesLexer = class(TmwPasLex)
  private
    fCurrentUsesType: TUsedUnitType;

    fLoC_Count: integer;
    fLoC_HasCodeInLine: Boolean;
    fLoC_ReachedTheEnd: Boolean;

    function GetLinesOfCode: integer;
  public
    constructor Create;

    procedure ProceedToFileType;
    procedure ProceedToNextIdentifier;

    function ProceedToNextContains: Boolean;
    function ProceedToNextUses: Boolean;

    procedure Next;

    property CurrentUsesType: TUsedUnitType read fCurrentUsesType;
    property LinesOfCode: integer read GetLinesOfCode; // this is only defined after a complete lexing until the end of the file (ptNull)
  end;

  TUsesParser = class
  private
    fIncludeHandler: IIncludeHandler;
    fUsesLexer: TUsesLexer;
    FOnLog: TLogProcedure;

    procedure Log(const Msg: String; const Severity: Integer = LogInfo); overload;
    procedure Log(const Msg: String; const Args: array of const; const Severity: Integer); overload;

    function GetDataString(StringStream: TStringStream): string;

    function ConsumeDottyIdentifier(aLexer: TUsesLexer): string;
    procedure ReadUses(UnitInfo: IUnitInfo; UsesType: TUsedUnitType);

    function GetLinesOfCode: integer;
  public
    constructor Create;
    destructor Destroy; override;

    function GetUsedUnitsFromSource(const UnitFileName, Source: String; var UnitInfo: IUnitInfo): Boolean;
    function GetUsedUnitsFromFile(const UnitFileName: String; var UnitInfo: IUnitInfo): Boolean;

    property UsesLexer: TUsesLexer read fUsesLexer;
    property OnLog: TLogProcedure read FOnLog    write FOnLog;
    property IncludeHandler: IIncludeHandler read fIncludeHandler write fIncludeHandler;
    property LinesOfCode: integer read GetLinesOfCode;

  end;

implementation

{ TUsesLexer }

constructor TUsesLexer.Create;
begin
  inherited Create;
  fCurrentUsesType   := utUnknown;
  fLoC_Count         := 0;
  fLoC_HasCodeInLine := false;
  fLoC_ReachedTheEnd := false;
end;

// next with counting "lines of code"
function TUsesLexer.GetLinesOfCode: integer;
begin
  if fLoC_ReachedTheEnd then
    Result := fLoC_Count
  else
    Result := 0;
end;

// hides ".Next" of base class, implements "real lines of code" counting
procedure TUsesLexer.Next;
begin
  inherited Next;

  case TokenID of
    ptCRLF :
      begin
        if fLoC_HasCodeInLine then
          Inc(fLoC_Count); // ptCRLFco would occur for crlf inside a comment => don't count
        fLoC_HasCodeInLine := false;
      end;
    ptNull :
      begin
        if fLoC_HasCodeInLine then
          Inc(fLoC_Count);
        fLoC_ReachedTheEnd := true;
      end
  else
    if not fLoC_HasCodeInLine then
      if not IsJunk then
        fLoC_HasCodeInLine := true;
  end;
end;

procedure TUsesLexer.ProceedToFileType;
begin
  // get the next token until we arrive at the start of a file
  // -> this skips any comments etc. before the start of the file
  while not (GenID in [ptProgram, ptPackage, ptLibrary, ptUnit, ptNull]) do
    Next;
end;

procedure TUsesLexer.ProceedToNextIdentifier;
begin
  while not (GenID in [ptIdentifier, ptNull]) do
    Next;
end;

function TUsesLexer.ProceedToNextContains: Boolean;
begin
  while not (GenID in [ptContains, ptNull]) do
    Next;
  Result := GenID = ptContains;
end;

function TUsesLexer.ProceedToNextUses: Boolean;
begin
  while not (GenID in [ptUses, ptNull]) do
  begin
    Next;
    case GenID of
      ptInterface:      fCurrentUsesType := utInterface;
      ptImplementation: fCurrentUsesType := utImplementation;
    end;
  end;
  Result := GenID = ptUses;
end;

{ TUsesParser }

function TUsesParser.ConsumeDottyIdentifier(aLexer: TUsesLexer): string;
begin
  Result := aLexer.Token;

  // look ahead, maybe the identifiert is dotty
  // -> consume all 'ptPoint' and 'ptIdentifier' tokens

  aLexer.InitAhead; // this proceeds to the next non-junk token automatically
  if aLexer.AheadTokenID = ptPoint then
  begin
    aLexer.Next;
    while (aLexer.TokenID in [ptIdentifier, ptPoint]) do
    begin
      Result := Result + aLexer.Token;
      aLexer.Next;
    end;
  end;
end;

procedure TUsesParser.ReadUses(UnitInfo: IUnitInfo; UsesType: TUsedUnitType);
var
  UsedUnitInfo: IUsedUnitInfo;
  PosOfUnitNameInPath: integer;
  PathToken: string;
  UnitNameWithExtension: string;
begin
  fUsesLexer.Next;

  while not (fUsesLexer.GenID in [ptSemiColon, ptNull]) do
  begin
    case fUsesLexer.GenId of
      ptIdentifier:
        begin
          UsedUnitInfo                     := TUsedUnitInfo.Create;
          UsedUnitInfo.Position            := fUsesLexer.TokenPos;
          UsedUnitInfo.IsInIncludeFileName := fUsesLexer.FileName; // .FileName is empty, when we are NOT in an inlcude file
          UsedUnitInfo.DelphiUnitName      := ConsumeDottyIdentifier(fUsesLexer);
          UsedUnitInfo.UsesType            := UsesType;
          UnitInfo.UsedUnits.Add(UsedUnitInfo);

          // after consuming the (dotty)name, we may already have reached the end of this uses entry
          if fUsesLexer.GenID in [ptComma, ptSemiColon] then
            Continue;

          // look ahead, maybe there is a path definition for the unit
          //    e.g. "myUnit in '..\someFolder\myUnit.pas'"
          //                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

          // we use the "ahead" lexer here to not move the cursor of the main lexer
          fUsesLexer.InitAhead;
          while not (fUsesLexer.AheadGenID in [ptIn, ptComma, ptSemiColon, ptNull]) do
            fUsesLexer.AheadNext;
          // Did we find the 'in' token?
          if fUsesLexer.AheadGenID = ptIn then
          begin
            // ... then move to the following '<path>' token
            while not (fUsesLexer.AheadGenID in [ptStringConst, ptComma, ptSemiColon, ptNull]) do
              fUsesLexer.AheadNext;
            // did we find the string const token?
            if fUsesLexer.AheadGenID = ptStringConst then
            begin
              PathToken                    := fUsesLexer.AheadToken;
              UsedUnitInfo.DefinedFilePath := PathToken.TrimStart(['''']).TrimEnd(['''']);
              UsedUnitInfo.FileName        := ExpandFileNameRelBaseDir(UsedUnitInfo.DefinedFilePath, ExtractFilePath(UnitInfo.Filename)); // absolute path

              PathToken             := PathToken.TrimEnd(['''']);
              UnitNameWithExtension := UsedUnitInfo.DelphiUnitName + '.pas';
              if PathToken.EndsWith(UnitNameWithExtension, true) then
              begin
                PosOfUnitNameInPath          := Length(PathToken) - Length(UnitNameWithExtension);
                UsedUnitInfo.InFilePosition  := fUsesLexer.AheadLex.TokenPos + PosOfUnitNameInPath;  // start position of the unit name in the path string const
              end
              else
              begin
                UsedUnitInfo.InFilePosition  := 0;
                Log('unit name "%s" not found in unit path "%s"', [UnitNameWithExtension, UsedUnitInfo.DefinedFilePath], LogError);
              end;

            end;
          end;
        end;
    end;

    // Go to the next non-junk token - we don't use fUsesLexer.NextNoJunk, because that would skip our "Lines of Code" counting mechanism
    // Hint: Code inside inactive compiler switches is considered  as "Junk" also
    repeat
      fUsesLexer.Next;
    until not fUsesLexer.IsJunk;
  end;
end;

constructor TUsesParser.Create;
begin
  fUsesLexer            := TUsesLexer.Create;

  // For parsing dependencies we typically want to respect switches -
  // For parsing a unit e.g. to reformat uses - we want to see all tokens, so do not activate compiler switch logic
  // .UseDefines should be set by the "user" of the TUsesParser
  fUsesLexer.UseDefines := false;

  fIncludeHandler       := nil;
end;

destructor TUsesParser.Destroy;
begin
  fUsesLexer.Free;
  inherited;
end;

procedure TUsesParser.Log(const Msg: String; const Severity: Integer);
begin
  if Assigned(FOnLog) then
    FOnLog('Uses Parser: ' + Msg, Severity);
end;

procedure TUsesParser.Log(const Msg: String; const Args: array of const; const Severity: Integer);
begin
  Log(Format(Msg, Args), Severity);
end;

function TUsesParser.GetDataString(StringStream: TStringStream): string;
var
  Encoding: TEncoding;
begin
  // try to read a bom from the buffer to create the correct encoding
  // but only if the encoding is still the default encoding
  if StringStream.Encoding = TEncoding.Default then
  begin
    Encoding := nil;
    TEncoding.GetBufferEncoding(StringStream.Bytes, Encoding);
    Result := Encoding.GetString(StringStream.Bytes, Length(Encoding.GetPreamble), StringStream.Size);
  end
  else
    Result := StringStream.Encoding.GetString(StringStream.Bytes, 0, StringStream.Size);
end;

function TUsesParser.GetLinesOfCode: integer;
begin
  Result := fUsesLexer.LinesOfCode;
end;

function TUsesParser.GetUsedUnitsFromSource(const UnitFileName, Source: String; var UnitInfo: IUnitInfo): Boolean;
begin
  Result := True;

  UnitInfo          := TUnitInfo.Create;
  UnitInfo.Filename := UnitFileName;

  fUsesLexer.Origin := Source;

  // assign the include handler
  fUsesLexer.IncludeHandler := IncludeHandler;

  // step 1: determine file type by file content -> "uses", "program", "package", ...
  fUsesLexer.ProceedToFileType;
  case fUsesLexer.GenID of
    ptProgram:
      UnitInfo.DelphiFileType := ftDPR;
    ptPackage:
      UnitInfo.DelphiFileType := ftDPK;
    ptLibrary:
      UnitInfo.DelphiFileType := ftLIB;
    ptUnit:
      UnitInfo.DelphiFileType := ftPAS
  else
    UnitInfo.DelphiFileType := ftUnknown;

    Exit(False);
  end;

  // step 2: read file name from content
  fUsesLexer.ProceedToNextIdentifier;
  if fUsesLexer.GenID = ptNull then
    raise Exception.Create('file name identifier not found');

  UnitInfo.DelphiUnitNamePosition := fUsesLexer.TokenPos;
  UnitInfo.DelphiUnitName         := ConsumeDottyIdentifier(fUsesLexer);

  case UnitInfo.DelphiFileType of
    ftPAS:
      begin
        while fUsesLexer.ProceedToNextUses do
          ReadUses(UnitInfo, fUsesLexer.CurrentUsesType);
      end;

    ftDPR:
      begin
        while fUsesLexer.ProceedToNextUses do
          ReadUses(UnitInfo, utImplementation);
      end;

    ftDPK:
      begin
        while fUsesLexer.ProceedToNextContains do
          ReadUses(UnitInfo, utContains);
      end;
  end;

  // "lines of code" gets calculated while parsing, so we can only receive it down here
  UnitInfo.LinesOfCode := fUsesLexer.LinesOfCode;
end;

function TUsesParser.GetUsedUnitsFromFile(const UnitFileName: String; var UnitInfo: IUnitInfo): Boolean;
var
  StringStream : TStringStream;
begin
  StringStream := TStringStream.Create;
  try
    StringStream.LoadFromFile(UnitFileName);

    Result := GetUsedUnitsFromSource(UnitFileName, GetDataString(StringStream), UnitInfo);
  finally
    StringStream.Free;
  end;
end;

end.
