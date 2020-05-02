unit Duds.Common.UsesParser.SimpleLexer.test;

interface

uses
  System.SysUtils, System.Classes, Generics.Collections, IOUtils,

  DUnitX.TestFramework,
  DUnitX.Assert,

  SimpleParser.Lexer, SimpleParser.Lexer.Types;

implementation

type

  // these test were made to understand the usage of the lexer
  [TestFixture]
  TDudsDelphiAstLexerTest = class(TObject)
  private
    procedure CheckTokenIDs(code: string; aExpectedTokenIDs: array of TptTokenKind; aLexer: TmwPasLex = nil);
    procedure CheckTokenTexts(code: string; aExpectedTokenTexts: array of string; aLexer: TmwPasLex = nil);
    procedure GetAllTokens(code: string; out aTokenIDs: TList<TptTokenKind>; out aTokenTexts: TList<string>;
      aLexer: TmwPasLex = nil);
  public

    [test]
    procedure TokensOfEmptyUnit;

    [test]
    procedure CommentsAndCrlfs;

    [test]
    procedure CompilerSwitchInUsesClause;

    [test]
    procedure IncludeHandler;

  end;

type
  TTestIncludeHandler = class(TInterfacedObject, IIncludeHandler)
  private
    FContent: string;
  public
    constructor Create(const Content: string);
    function GetIncludeFileContent(const ParentFileName, IncludeName: string; out Content: string;
      out FileName: string): Boolean;
  end;

  { TTestIncludeHandler }

constructor TTestIncludeHandler.Create(const Content: string);
begin
  inherited Create;
  FContent := Content;
end;

function TTestIncludeHandler.GetIncludeFileContent(const ParentFileName, IncludeName: string; out Content: string;
  out FileName: string): Boolean;
begin
  Result := True;
  Content := FContent;
  FileName := IncludeName;
end;

  { TDudsDelphiAstLexerTest }

procedure TDudsDelphiAstLexerTest.GetAllTokens(code: string; out aTokenIDs: TList<TptTokenKind>;
  out aTokenTexts: TList<string>; aLexer: TmwPasLex = nil);
var
  aUsedLexer: TmwPasLex;
begin
  if aLexer = nil then
    aUsedLexer := TmwPasLex.Create
  else
    aUsedLexer := aLexer;
  try
    aUsedLexer.Origin := code;

    aTokenIDs := TList<TptTokenKind>.Create;
    aTokenTexts := TList<string>.Create;

    aTokenIDs.Add(aUsedLexer.TokenID);
    aTokenTexts.Add(aUsedLexer.Token);
    repeat
      aUsedLexer.Next;
      aTokenIDs.Add(aUsedLexer.TokenID);
      aTokenTexts.Add(aUsedLexer.Token);
    until aUsedLexer.TokenID = ptNull;

  finally
    if aLexer = nil then
      aUsedLexer.Free;
  end;
end;

procedure TDudsDelphiAstLexerTest.CheckTokenIDs(code: string; aExpectedTokenIDs: array of TptTokenKind;
  aLexer: TmwPasLex = nil);
var
  aActualTokenIDs: TList<TptTokenKind>;
  aActualTokenTexts: TList<string>;
  i: integer;
begin
  GetAllTokens(code, aActualTokenIDs, aActualTokenTexts, aLexer);
  try
    Assert.AreEqual(Length(aExpectedTokenIDs), aActualTokenIDs.Count, 'number of tokens');

    for i := 0 to aActualTokenIDs.Count - 1 do
      Assert.AreEqual(aExpectedTokenIDs[i], aActualTokenIDs[i], 'token #' + IntToStr(i));
  finally
    aActualTokenIDs.Free;
    aActualTokenTexts.Free;
  end;
end;

procedure TDudsDelphiAstLexerTest.CheckTokenTexts(code: string; aExpectedTokenTexts: array of string;
  aLexer: TmwPasLex = nil);
var
  aActualTokenIDs: TList<TptTokenKind>;
  aActualTokenTexts: TList<string>;
  i: integer;
begin
  GetAllTokens(code, aActualTokenIDs, aActualTokenTexts, aLexer);
  try
    Assert.AreEqual(Length(aExpectedTokenTexts), aActualTokenTexts.Count, 'number of tokens');

    for i := 0 to aActualTokenTexts.Count - 1 do
      Assert.AreEqual(aExpectedTokenTexts[i], aActualTokenTexts[i], 'token #' + IntToStr(i));
  finally
    aActualTokenIDs.Free;
    aActualTokenTexts.Free;
  end;
end;

procedure TDudsDelphiAstLexerTest.TokensOfEmptyUnit;
const
  code = 'unit test; interface implementation end.';
begin
  CheckTokenIDs(code, [ptUnit, ptSpace, ptIdentifier, ptSemiColon, ptSpace, ptInterface, ptSpace, ptImplementation,
    ptSpace, ptEnd, ptPoint, ptNull]);
end;

procedure TDudsDelphiAstLexerTest.CommentsAndCrlfs;
const
  code = '// single line comment' + sLineBreak +
         'unit abc;' + sLineBreak +
         '{ multi ' + sLineBreak +
         '  line'  + sLineBreak +
         '  comment }';
begin
  CheckTokenIDs(code, [ptSlashesComment, ptCRLF, ptUnit, ptSpace, ptIdentifier, ptSemiColon, ptCRLF, ptBorComment, ptNull]);
end;

procedure TDudsDelphiAstLexerTest.CompilerSwitchInUsesClause;
const
  code = 'uses SysUtils {$IFDEF TEST}, Classes {$ELSE}, Windows {$ENDIF}';
begin
  CheckTokenIDs(code, [ptUses, ptSpace, ptIdentifier, ptSpace, ptIfDefDirect, ptComma, ptSpace, ptIdentifier, ptSpace,
    ptElseDirect, ptComma, ptSpace, ptIdentifier, ptSpace, ptEndIfDirect, ptNull]);
  CheckTokenTexts(code, ['uses', ' ', 'SysUtils', ' ', '{$IFDEF TEST}', ',', ' ', 'Classes', ' ', '{$ELSE}', ',', ' ',
    'Windows', ' ', '{$ENDIF}', '']);
end;


procedure TDudsDelphiAstLexerTest.IncludeHandler;
const
  code = 'uses {$INCLUDE myUsesIncludeFile};';
var
  aLexer: TmwPasLex;
begin
  aLexer := TmwPasLex.Create;
  try
    aLexer.IncludeHandler := TTestIncludeHandler.Create('Classes, SysUtils');

    CheckTokenIDs(code,
      [ptUses, ptSpace, ptIdentifier, ptComma, ptSpace, ptIdentifier,
       ptCRLF {<- this line break is inserted by include handler},
       ptSemiColon, ptNull], aLexer);

    //
    Assert.AreEqual(ptNull, aLexer.TokenID, 'token id after end of stream');

    // filename of the lexer is empty, as long we are in the "main" file
    Assert.AreEqual('', aLexer.FileName, 'lexer filename');

  finally
    aLexer.Free;
  end;

end;

initialization

TDUnitX.RegisterTestFixture(TDudsDelphiAstLexerTest);

end.
