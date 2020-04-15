//------------------------------------------------------------------------------
//
// This file is part of Duds.
//
//    The software is free software: you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    The software is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    To received a copy of the GNU General Public License,
//    see <http://www.gnu.org/licenses/>.
//
// The initial developer of the original code is Dontenwill AG (www.dontenwill.de)
//
//------------------------------------------------------------------------------

unit Duds.Refactoring.FormatUses.test;

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, System.Generics.Collections,

  DUnitX.TestFramework,
  DUnitX.Assert,
  
  Duds.Refactoring.FormatUses;

implementation

type
  TFormatUsesRefactoringHack = class(TFormatUsesRefactoring)
  end;

type
  [TestFixture]
  TFormatUsesRefactoringTest = class(TObject)
  private
    FRefactoring : TFormatUsesRefactoringHack;

    procedure FormatAndCheckResult(aInputCode, aExpectedCode: string);

  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure OneUsesWithNewLine;

    [Test]
    procedure OneUsesWithoutNewLine;

  end;

procedure TFormatUsesRefactoringTest.Setup;
begin
  FRefactoring := TFormatUsesRefactoringHack.Create;
end;

procedure TFormatUsesRefactoringTest.TearDown;
begin
  FreeAndNil(FRefactoring);
end;

procedure TFormatUsesRefactoringTest.FormatAndCheckResult(aInputCode, aExpectedCode: string);
var
  aSourceLines: TStringList;
  aExpectedCodeStrings: TStringList;
begin
  aSourceLines    := TStringList.Create;
  aExpectedCodeStrings := TStringList.Create;
  try
    aSourceLines.Text         := aInputCode;
    aExpectedCodeStrings.Text := aExpectedCode;
    FRefactoring.FormatUsesInSource(aSourceLines);
    Assert.AreEqual(aExpectedCodeStrings.Text, aSourceLines.Text);
  finally
    FreeAndNil(aSourceLines);
    FreeAndNil(aExpectedCodeStrings);
  end;
end;

procedure TFormatUsesRefactoringTest.OneUsesWithNewLine;
begin
 FormatAndCheckResult(

     'unit demo'         + sLineBreak +
     'interface'         + sLineBreak +
     'uses'              + sLineBreak +  // <- new line after "uses" keyword
     '     OneUnit;'     + sLineBreak +  // <- indent is 4
     'implementation'    + sLineBreak +
     'end.'
 ,

     'unit demo'         + sLineBreak +
     'interface'         + sLineBreak +
     'uses'              + sLineBreak +
     '  OneUnit;'        + sLineBreak +  // <- indent should be 2
     'implementation'    + sLineBreak +
     'end.'
  )
end;


procedure TFormatUsesRefactoringTest.OneUsesWithoutNewLine;
begin
 FormatAndCheckResult(

     'unit demo'         + sLineBreak +
     'interface'         + sLineBreak +
     'uses OneUnit;'     + sLineBreak + // <- NO new line after "uses" keyword
     'implementation'    + sLineBreak +
     'end.'
 ,

     'unit demo'         + sLineBreak +
     'interface'         + sLineBreak +
     'uses'              + sLineBreak +
     '  OneUnit;'        + sLineBreak +  // <- indent should be 2
     'implementation'    + sLineBreak +
     'end.'
  )
end;

initialization
  TDUnitX.RegisterTestFixture(TFormatUsesRefactoringTest);

end.