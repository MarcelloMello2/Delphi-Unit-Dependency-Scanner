unit Duds.Refactoring.RemoveUnusedUnits.test;

interface

uses
  System.SysUtils, System.Classes,

  DUnitX.TestFramework,
  DUnitX.Assert,

  Duds.Refactoring.RemoveUnusedUnits;

type
  TRemoveUnusedUnitsRefactoringHack = class(TRemoveUnusedUnitsRefactoring)
  end;

type
  [TestFixture]
  TDudsRefactoringRemoveUnusedUnitsTest = class(TObject)
  private
    FRefactoring : TRemoveUnusedUnitsRefactoringHack;

    procedure RemoveUnitsAndCheckResult(aInputCode, aExpectedCode: string;
      aUnitsToRemove: array of string);


  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestUsesListReplacement_SingleLineCommentAfterEndOfList;

    [Test]
    procedure TestUsesListReplacement_NoUsesLeft;

    [Test]
    procedure TestUsesListReplacement_PreserveSingleLineCommentsInBetween;

    [Test]
    procedure TestUsesListReplacement_CommentBeforeCompilerSwitch;

    [Test]
    procedure TestUsesListReplacement_CompilerSwitchForFirstUnit;

    [Test]
    procedure TestUsesListReplacement_CompilerSwitchInBetween;

    [Test]
    procedure TestUsesListReplacement_CompilerSwitchAtTheEnd;

  end;

implementation

procedure TDudsRefactoringRemoveUnusedUnitsTest.Setup;
begin
  FRefactoring := TRemoveUnusedUnitsRefactoringHack.Create;
end;

procedure TDudsRefactoringRemoveUnusedUnitsTest.TearDown;
begin
  FreeAndNil(FRefactoring);
end;

procedure TDudsRefactoringRemoveUnusedUnitsTest.RemoveUnitsAndCheckResult(aInputCode, aExpectedCode: string; aUnitsToRemove: array of string);
var
  aRemoveUnits: TStringList;
  aSourceLines: TStringList;
  aExpectedCodeStrings: TStringList;
  UnitToRemove: String;
begin
  aRemoveUnits := TStringList.Create;
  try
    for UnitToRemove in aUnitsToRemove do
       aRemoveUnits.Add(UnitToRemove);

    aSourceLines    := TStringList.Create;
    aExpectedCodeStrings := TStringList.Create;
    try
      aSourceLines.Text := aInputCode;
      aExpectedCodeStrings.Text := aExpectedCode;
      FRefactoring.RemoveUnitsFromSource(aSourceLines, aRemoveUnits);
      Assert.AreEqual(aExpectedCodeStrings.Text, aSourceLines.Text);
    finally
      FreeAndNil(aSourceLines);
      FreeAndNil(aExpectedCodeStrings);
    end;
  finally
    FreeAndNil(aRemoveUnits);
  end;
end;

procedure TDudsRefactoringRemoveUnusedUnitsTest.TestUsesListReplacement_PreserveSingleLineCommentsInBetween;
begin
 RemoveUnitsAndCheckResult(

     'implementation'    + sLineBreak +
                           sLineBreak +
     'uses'              + sLineBreak +
     '      // Delphi '  + sLineBreak +      // <- misaligned comment
     '  UnitOne, '       + sLineBreak +
     '// 3rd-Party  '    + sLineBreak +      // <- there should be a new line before a comment line
     '  UnitToRemove, '  + sLineBreak +      // <- this unit should be remove
     '  UnitTwo;      '                      // <- spaces after ;

 ,

     'implementation'   + sLineBreak +
                          sLineBreak +
     'uses'             + sLineBreak +
     '  // Delphi'      + sLineBreak +
     '  UnitOne,'       + sLineBreak +
                          sLineBreak +
     '  // 3rd-Party'   + sLineBreak +
     '  UnitTwo;'

 , ['UnitToRemove']);
end;

procedure TDudsRefactoringRemoveUnusedUnitsTest.TestUsesListReplacement_CommentBeforeCompilerSwitch;
begin
 RemoveUnitsAndCheckResult(

     'implementation'    + sLineBreak +
                           sLineBreak +
     'uses'              + sLineBreak +
     '      // Delphi '  + sLineBreak +
     '  UnitOne '       + sLineBreak +
     '// 3rd-Party  '    + sLineBreak +
     '  {$IFDEF MYSWITCH} '                    + sLineBreak +
     '  , UnitOne '                             + sLineBreak +
     '  {$ENDIF}'                              + sLineBreak +
     '  , UnitTwo;      '

 ,

     'implementation'   + sLineBreak +
                          sLineBreak +
     'uses'             + sLineBreak +
     '  // Delphi'      + sLineBreak +
     '  UnitOne'        + sLineBreak +
                          sLineBreak +
     '  // 3rd-Party'   + sLineBreak +
     '{$IFDEF MYSWITCH}'+ sLineBreak +
     '  , UnitOne'      + sLineBreak +
     '{$ENDIF}'         + sLineBreak +
     '  , UnitTwo;'

 , ['UnitToRemove']);
end;

procedure TDudsRefactoringRemoveUnusedUnitsTest.TestUsesListReplacement_SingleLineCommentAfterEndOfList;
begin
 RemoveUnitsAndCheckResult(

     'implementation' + sLineBreak +
                        sLineBreak +
     'uses'           + sLineBreak +
     '  UnitOne, UnitToRemove, UnitTwo; // Comment at the end of the unit list that will be dropped' + sLineBreak +
                                                               sLineBreak +
     'function Foo...'

 ,

     'implementation' + sLineBreak +
                        sLineBreak +
     'uses'           + sLineBreak +
     '  UnitOne, UnitTwo;' + sLineBreak +
                        sLineBreak +
     'function Foo...'

 , ['UnitToRemove']);
end;

procedure TDudsRefactoringRemoveUnusedUnitsTest.TestUsesListReplacement_NoUsesLeft;
begin
 RemoveUnitsAndCheckResult(

     'implementation' + sLineBreak +
                        sLineBreak +
     ' // just a comment, should not be touched' + sLineBreak +
     ' // uses UnitOne, UnitThree'               + sLineBreak +
                                                   sLineBreak +
     'uses'           + sLineBreak +
     '  UnitOne, UnitTwo, '       + sLineBreak +
     '  // a comment in between ' + sLineBreak +
     '  UnitThree;              ' + sLineBreak +

     'function Foo...'
 ,

     'implementation' + sLineBreak +
                        sLineBreak +
     ' // just a comment, should not be touched' + sLineBreak +
     ' // uses UnitOne, UnitThree'               + sLineBreak +
                                                   sLineBreak +
     'function Foo...'

 , ['UnitOne', 'UnitTwo', 'UnitThree']);
end;

procedure TDudsRefactoringRemoveUnusedUnitsTest.TestUsesListReplacement_CompilerSwitchForFirstUnit;
begin

RemoveUnitsAndCheckResult(
     '// some comment before '                 + sLineBreak +
     'uses'                                    + sLineBreak +
     '  {$IFDEF MYSWITCH} '                    + sLineBreak +
     '  UnitOne, '                             + sLineBreak +
     '  {$ENDIF}'                              + sLineBreak +
     '  UnitToRemove, UnitTwo;'

 ,
     '// some comment before '                 + sLineBreak +
     'uses'                                   + sLineBreak +
     '{$IFDEF MYSWITCH}'                      + sLineBreak +
     '  UnitOne,'                             + sLineBreak +
     '{$ENDIF}'                               + sLineBreak +
     '  UnitTwo;'

 , ['UnitToRemove']);
end;

procedure TDudsRefactoringRemoveUnusedUnitsTest.TestUsesListReplacement_CompilerSwitchInBetween;
begin

RemoveUnitsAndCheckResult(
     '// some comment before'                  + sLineBreak +
     'uses'                                   + sLineBreak +
     '  UnitOne, UnitToRemove1,'              + sLineBreak +
     '{$IFDEF MYSWITCH}'                      + sLineBreak +
     '  UnitTwo, UnitToRemove2,'              + sLineBreak +
     '{$ENDIF}'                               + sLineBreak +
     '  UnitToRemove3, UnitThree;'

 ,
     '// some comment before'                  + sLineBreak +
     'uses'                                   + sLineBreak +
     '  UnitOne'                              + sLineBreak +
     '{$IFDEF MYSWITCH}'                      + sLineBreak +
     '  , UnitTwo'                            + sLineBreak +
     '{$ENDIF}'                               + sLineBreak +
     '  , UnitThree;'

 , ['UnitToRemove1', 'UnitToRemove2', 'UnitToRemove3']);
end;

procedure TDudsRefactoringRemoveUnusedUnitsTest.TestUsesListReplacement_CompilerSwitchAtTheEnd;
begin

RemoveUnitsAndCheckResult(
     '// some comment before'                  + sLineBreak +
     'uses'                                   + sLineBreak +
     '  UnitOne, UnitToRemove1'               + sLineBreak +
     '{$IFDEF MYSWITCH}'                      + sLineBreak +
     '  , UnitTwo, UnitToRemove2'             + sLineBreak +
     '{$ENDIF}'                               + sLineBreak +
     '  ;'

 ,
     '// some comment before'                 + sLineBreak +
     'uses'                                   + sLineBreak +
     '  UnitOne'                              + sLineBreak +
     '{$IFDEF MYSWITCH}'                      + sLineBreak +
     '  , UnitTwo'                            + sLineBreak +
     '{$ENDIF}'                               + sLineBreak +
     '  ;'

 , ['UnitToRemove1', 'UnitToRemove2']);
end;

initialization
  TDUnitX.RegisterTestFixture(TDudsRefactoringRemoveUnusedUnitsTest);
end.
