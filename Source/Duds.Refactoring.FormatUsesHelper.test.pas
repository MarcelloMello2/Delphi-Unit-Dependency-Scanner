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
    FRefactoring : TFormatUsesHelper;

  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestFindUsesLine;

  end;

procedure TFormatUsesHelperTest.Setup;
begin
  FRefactoring := TFormatUsesHelper.Create;
end;

procedure TFormatUsesHelperTest.TearDown;
begin
  FreeAndNil(FRefactoring);
end;

procedure TFormatUsesHelperTest.TestFindUsesLine;
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

    aLine := FRefactoring.FindUsesLine(aInputCodeStrings);
    Assert.AreEqual(5, aLine);
  finally
    FreeAndNil(aInputCodeStrings);
  end;
end;


initialization
  TDUnitX.RegisterTestFixture(TFormatUsesHelperTest);

end.