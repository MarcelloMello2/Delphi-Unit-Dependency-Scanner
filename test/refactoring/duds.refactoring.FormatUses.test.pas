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

unit duds.refactoring.FormatUses.test;

interface

uses
  System.Classes, System.SysUtils, System.StrUtils, System.Generics.Collections,
  System.JSON,

  DUnitX.TestFramework,
  DUnitX.Assert,
  duds.analyzer.model,
  duds.common.Classes,
  duds.common.modules,
  duds.common.modulesSerializer,
  duds.analyzer.ModulesAnalyzer,
  duds.common.UnitInfo,
  duds.common.UsedUnitInfo,
  duds.common.Types,

  duds.refactoring.FormatUses;

implementation

type
  TFormatUsesRefactoringHack = class(TFormatUsesRefactoring)
  end;

type

  [TestFixture]
  TBaseFormatUsesRefactoringTest = class(TObject)
  private
    procedure FormatAndCheckResult(aInputCode, aExpectedCode: string; UseModulesForGrouping: Boolean = false; Model: TDudsModel = nil);

  end;

  [TestFixture]
  TFormatUsesRefactoringTestWithoutModel = class(TBaseFormatUsesRefactoringTest)
  public

    [test]
    procedure OneUsesWithNewLine;

    [test]
    procedure OneUsesWithoutNewLine;

  end;

  [TestFixture]
  TFormatUsesRefactoringTestWithModel = class(TBaseFormatUsesRefactoringTest)
  private
    fModel: TDudsModel;

    procedure LoadMockDelphiFiles;
    procedure LoadMockModulesDefinition;

    procedure FormatAndCheckResult_GroupByModules(aInputCode, aExpectedCode: string);

  public
    [Setup]
    procedure Setup;

    [TearDown]
    procedure TearDown;

    [Test]
    procedure OneUsesWithoutComment;

    [Test]
    procedure MultipleModulesMixed;

  end;

{ TBaseFormatUsesRefactoringTest }

procedure TBaseFormatUsesRefactoringTest.FormatAndCheckResult(aInputCode, aExpectedCode: string; UseModulesForGrouping: Boolean = false; Model: TDudsModel = nil);
var
  aSourceLines: TStringList;
  aExpectedCodeStrings: TStringList;
  aRefactoring : TFormatUsesRefactoringHack;
begin
  if UseModulesForGrouping and (Model = nil) then
    raise Exception.Create('wrong usage - pass a model when using "UseModulesForGrouping"');


  aRefactoring       := TFormatUsesRefactoringHack.Create;
  aRefactoring.Model := Model;
  aSourceLines := TStringList.Create;
  aExpectedCodeStrings := TStringList.Create;
  try
    aSourceLines.Text := aInputCode;
    aExpectedCodeStrings.Text := aExpectedCode;
    aRefactoring.FormatUsesInSource(aSourceLines, UseModulesForGrouping);
    Assert.AreEqual(aExpectedCodeStrings.Text, aSourceLines.Text);
  finally
    FreeAndNil(aSourceLines);
    FreeAndNil(aExpectedCodeStrings);
    FreeAndNil(aRefactoring);
  end;
end;

{ TFormatUsesRefactoringTestWithoutModel }

procedure TFormatUsesRefactoringTestWithoutModel.OneUsesWithNewLine;
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

procedure TFormatUsesRefactoringTestWithoutModel.OneUsesWithoutNewLine;
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

{ TFormatUsesRefactoringTestWithModel }

procedure TFormatUsesRefactoringTestWithModel.Setup;
var
  aModulesAnalyzer: TModulesAnalyzer;
begin
  fModel := TDudsModel.Create;

  LoadMockModulesDefinition;
  LoadMockDelphiFiles;

  aModulesAnalyzer := TModulesAnalyzer.Create;
  try
    aModulesAnalyzer.Model := fModel;
    aModulesAnalyzer.MapUnitsToModules;
  finally
    aModulesAnalyzer.Free;
  end;
end;

procedure TFormatUsesRefactoringTestWithModel.TearDown;
begin

  FreeAndNil(fModel);
end;

const
  cDelphiRtl_ModuleName    = 'delphi.rtl';
  cDelphiRtl_Unit_SysUtils = 'SysUtils';
  cDelphiRtl_Unit_Classes  = 'Classes';

  c3rdParty_Ciphers_ModuleName  = '3rdParty.ciphers';
  c3rdParty_Ciphers_Unit1       = 'cipherUtils1';
  c3rdParty_Ciphers_Unit2       = 'cipherUtils2';

  cCoreCommon_Module     = 'core.common';
  cCoreCommon_Unit_Text  = 'myTextUtils';
  cCoreCommon_Unit_Array = 'myArrayUtils';

  cAllUnits : array[0..5] of string =
    (cDelphiRtl_Unit_SysUtils, cDelphiRtl_Unit_Classes,
     c3rdParty_Ciphers_Unit1, c3rdParty_Ciphers_Unit2,
     cCoreCommon_Unit_Text, cCoreCommon_Unit_Array);

procedure TFormatUsesRefactoringTestWithModel.LoadMockModulesDefinition;

  procedure AddModuleToJson(modules: TJsonArray; cModuleName: string; Units: array of string);
  var
    module: TjsonObject;
    moduleContains: TJSONObject;
    moduleUnits: TJsonArray;
    i: integer;
  begin
    module := TJsonObject.Create;
    module.AddPair('name', cModuleName);

    moduleContains := TJSONObject.Create;

    moduleUnits := TJSonArray.Create;
    for i := 0 to Length(Units) - 1 do
      moduleUnits.Add(Units[i]);

    moduleContains.AddPair('units', moduleUnits);
    module.AddPair('contains', moduleContains);

    modules.add(module);
  end;

var
  jsonObj: TJsonObject;
  modules: TJsonArray;
begin
  jsonObj := TJSONObject.Create;
  try
    modules := TJsonArray.Create;
    jsonObj.AddPair('modules', modules);

    AddModuleToJson(modules, cDelphiRtl_ModuleName,        [cDelphiRtl_Unit_SysUtils, cDelphiRtl_Unit_Classes]);
    AddModuleToJson(modules, c3rdParty_Ciphers_ModuleName, [c3rdParty_Ciphers_Unit1, c3rdParty_Ciphers_Unit2]);
    AddModuleToJson(modules, cCoreCommon_Module,           [cCoreCommon_Unit_Text, cCoreCommon_Unit_Array]);

    TModulesSerializer.ReadFromJson('', jsonObj.ToJSON, fModel.Modules);
  finally
    jsonObj.Free;
  end;
end;

procedure TFormatUsesRefactoringTestWithModel.FormatAndCheckResult_GroupByModules(
  aInputCode, aExpectedCode: string);
begin
  FormatAndCheckResult(aInputCode, aExpectedCode, true, fModel);
end;

procedure TFormatUsesRefactoringTestWithModel.LoadMockDelphiFiles;

  procedure AddDelphiFile(DelphiUnitname: string);
  var
    aDelphiFile: TDelphiFile;
    aUnitInfo: TUnitInfo;
  begin
    aUnitInfo                        := TUnitInfo.Create;
    aUnitInfo.Filename               := ''; // no filename under real conditions mean => unit not found in search path
    aUnitInfo.LinesOfCode            := 0;
    aUnitInfo.DelphiFileType         := ftPAS;
    aUnitInfo.DelphiUnitName         := DelphiUnitname;
    aUnitInfo.DelphiUnitNamePosition := 0;

    aDelphiFile              := fModel.CreateDelphiFile(aUnitInfo.DelphiUnitName, false);
    aDelphiFile.UnitInfo     := aUnitInfo;
    aDelphiFile.InSearchPath := not aUnitInfo.Filename.IsEmpty;
  end;

var
  i: integer;
begin
  for I := 0 to Length(cAllUnits) - 1 do
    AddDelphiFile(cAllUnits[i]);
end;

procedure TFormatUsesRefactoringTestWithModel.OneUsesWithoutComment;
begin
  FormatAndCheckResult_GroupByModules(

     'unit demo'         + sLineBreak +
     'interface'         + sLineBreak +
     'uses'              + sLineBreak +
     '  SysUtils;'       + sLineBreak +
     'implementation'    + sLineBreak +
     'end.'
 ,

     'unit demo'         + sLineBreak +
     'interface'         + sLineBreak +
     'uses'              + sLineBreak +
     '  // delphi.rtl'   + sLineBreak + // <- this module header should be inserted
     '  SysUtils;'       + sLineBreak +
     'implementation'    + sLineBreak +
     'end.'
  )
end;

procedure TFormatUsesRefactoringTestWithModel.MultipleModulesMixed;
begin
  FormatAndCheckResult_GroupByModules(

     'unit demo'         + sLineBreak +
     'interface'         + sLineBreak +
     'uses'              + sLineBreak +
     '  myArrayUtils, Classes, cipherUtils2, SysUtils, cipherUtils1, myTextUtils;' + sLineBreak +     // <- units from different module, order is mixed
     'implementation'    + sLineBreak +
     'end.'
 ,

     'unit demo'         + sLineBreak +
     'interface'         + sLineBreak +
     'uses'              + sLineBreak +
     '  // delphi.rtl'                + sLineBreak + // <- this module header should be inserted
     '  Classes, SysUtils,'           + sLineBreak + //    <- original order shall be preserved!
                                        sLineBreak +
     '  // 3rdParty.ciphers'          + sLineBreak + // <- this module header should be inserted
     '  cipherUtils2, cipherUtils1,'  + sLineBreak + //    <- original order shall be preserved!
                                        sLineBreak +
     '  // core.common'               + sLineBreak + // <- this module header should be inserted
     '  myArrayUtils, myTextUtils;'   + sLineBreak + //    <- original order shall be preserved!
     'implementation'                 + sLineBreak +
     'end.'
  )
end;

initialization

TDUnitX.RegisterTestFixture(TFormatUsesRefactoringTestWithoutModel);
TDUnitX.RegisterTestFixture(TFormatUsesRefactoringTestWithModel);

end.
