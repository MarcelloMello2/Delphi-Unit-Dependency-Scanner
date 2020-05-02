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

unit Duds.Refactoring.FormatUsesHelper.test;

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, System.Generics.Collections,

  DUnitX.TestFramework,
  DUnitX.Assert,
  
  Duds.Refactoring.FormatUsesHelper;

implementation

type
  [TestFixture]
  TFormatUsesHelperTest = class(TObject)
  private
    FFormatHelper : TFormatUsesHelper;
    procedure BuildUsesListAndCheckList(aCode: string; ExpectedElementTypes: array of TUsesElementType; ExpectedValues: array of string);


  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure FindUsesLine;

    [Test]
    procedure BuildUsesList_TwoUsesOnly;

    [Test]
    procedure BuildUsesList_WithSingleLineComments;

    [Test]
    procedure BuildUsesList_WithSingleLineCommentAfterUnit;

    [Test]
    procedure BuildUsesList_WithSimpleCompilerDirective;

  end;

procedure TFormatUsesHelperTest.Setup;
begin
  FFormatHelper := TFormatUsesHelper.Create;
end;

procedure TFormatUsesHelperTest.TearDown;
begin
  FreeAndNil(FFormatHelper);
end;

procedure TFormatUsesHelperTest.FindUsesLine;
var
  aCode: string;
  aInputCodeStrings: TStringList;
  aLine: Integer;
begin

  aCode :=
     'implementation' + sLineBreak +
                        sLineBreak +
     ' // just a comment, should not be touched' + sLineBreak +
     ' // uses UnitOne, UnitThree'               + sLineBreak +
                                                   sLineBreak +
     'uses'           + sLineBreak +
     '  UnitOne, UnitTwo, '       + sLineBreak +
     '  // a comment in between ' + sLineBreak +
     '  UnitThree;              ' + sLineBreak +

     'function Foo...';

  aInputCodeStrings    := TStringList.Create;
  try
    aInputCodeStrings.Text := aCode;

    aLine := FFormatHelper.FindUsesLineInSource(aInputCodeStrings);
    Assert.AreEqual(5, aLine);
  finally
    FreeAndNil(aInputCodeStrings);
  end;
end;

procedure TFormatUsesHelperTest.BuildUsesListAndCheckList(aCode: string; ExpectedElementTypes: array of TUsesElementType; ExpectedValues: array of string);
var
  aLine: integer;
  aUsesList: TUsesList;
  aCodeStrings: TStringList;
  I: Integer;
  aUsesElement: TUsesElement;
begin
  aCodeStrings    := TStringList.Create;
  try
    aCodeStrings.Text := aCode;

    aLine    := FFormatHelper.FindUsesLineInSource(aCodeStrings);
    aUsesList := FFormatHelper.ExtractUsesFromSourceAndBuildUsesList(aCodeStrings, aLine);
    try
      Assert.AreEqual(Length(ExpectedElementTypes), Length(ExpectedValues), 'wrong test usage - both array should have the same length');

      Assert.AreEqual(Length(ExpectedElementTypes), aUsesList.Count, 'length of uses list');

      for i := 0 to pred(aUsesList.Count) do
      begin
        aUsesElement := aUsesList[i];
        Assert.AreEqual(ExpectedElementTypes[i], aUsesElement.ElementType,         'wrong element type');
        Assert.AreEqual(ExpectedValues[i],       aUsesElement.TextValue,    false, 'wrong text value of element');
      end;

    finally
      aUsesList.Free;
    end;
  finally
    FreeAndNil(aCodeStrings);
  end;
end;

procedure TFormatUsesHelperTest.BuildUsesList_TwoUsesOnly;
begin
  BuildUsesListAndCheckList(

    'unit demo'            + sLineBreak +
    'interface'            + sLineBreak +
    'uses '                + sLineBreak +
    '  SysUtils, Classes;' + sLineBreak +
    'implementation'       + sLineBreak +
    'end.',

    [etUnit, etUnit],

    ['SysUtils', 'Classes']

    )
end;

procedure TFormatUsesHelperTest.BuildUsesList_WithSingleLineComments;
begin
  BuildUsesListAndCheckList(

    'unit demo'            + sLineBreak +
    'interface'            + sLineBreak +
    'uses '                + sLineBreak +
    '  // MyComment'       + sLineBreak +  // <- this should be put into the list
    '  SysUtils;'          + sLineBreak +
    'implementation'       + sLineBreak +
    'end.',

    [etSingleLineHeaderComment, etUnit],

    ['// MyComment', 'SysUtils']

    )
end;

procedure TFormatUsesHelperTest.BuildUsesList_WithSingleLineCommentAfterUnit;
begin
  BuildUsesListAndCheckList(

    'unit demo'                           + sLineBreak +
    'interface'                           + sLineBreak +
    'uses '                               + sLineBreak +
    '  SysUtils // MyCommentAfterTheUnit' + sLineBreak +   // <- this should be deleted since it does not stand alone
    '  // MyHeaderCommentAtTheEnd'        + sLineBreak +   // <- this should be kept
    ';'                                   + sLineBreak +
    'implementation'                      + sLineBreak +
    'end.',

    [etUnit, etSingleLineHeaderComment],

    ['SysUtils', '// MyHeaderCommentAtTheEnd']

    )
end;

procedure TFormatUsesHelperTest.BuildUsesList_WithSimpleCompilerDirective;
begin
  BuildUsesListAndCheckList(

    'unit demo'                           + sLineBreak +
    'interface'                           + sLineBreak +
    'uses '                               + sLineBreak +
    '  {$IFDEF MYSWITCH}'                 + sLineBreak +
    '  SysUtils'                          + sLineBreak +
    '  {$ENDIF}'                          + sLineBreak +
    '  , Classes;'                        + sLineBreak +
    'implementation'                      + sLineBreak +
    'end.',

    [etCompilerDirectiveStart, etUnit, etCompilerDirectiveEnd, etUnit],

    ['{$IFDEF MYSWITCH}', 'SysUtils', '{$ENDIF}', 'Classes']

    )
end;

initialization
  TDUnitX.RegisterTestFixture(TFormatUsesHelperTest);

end.