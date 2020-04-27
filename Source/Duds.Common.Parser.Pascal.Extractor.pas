unit Duds.Common.Parser.Pascal.Extractor;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,

  Duds.Common.Utils,
  Duds.Common.Files,
  Duds.Common.Types,
  Duds.Common.Interfaces,
  Duds.Common.Parser.Pascal.Tokeniser,
  Duds.Common.Parser.Pascal,
  Duds.Modules.Classes;

type

  TPascalUnitExtractor = class(TComponent)
  private
    FTokeniser: TPascalTokeniser;
    FTokeniserInFile: TPascalTokeniser;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetUsedUnits(const UnitFileName: String; var UnitInfo: IUnitInfo): Boolean;
  end;

implementation

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

end.
