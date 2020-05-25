program duds.test;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ENDIF }
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  DUnitX.TestFramework,
  SimpleParser.Lexer                                 in '..\src\3rdParty\DelphiAST\Source\SimpleParser\SimpleParser.Lexer.pas',
  SimpleParser.Lexer.Types                           in '..\src\3rdParty\DelphiAST\Source\SimpleParser\SimpleParser.Lexer.Types.pas',
  duds.common.UnitInfo                               in '..\src\common\duds.common.UnitInfo.pas',
  duds.common.UsedUnitInfo                           in '..\src\common\duds.common.UsedUnitInfo.pas',
  duds.common.UsesParser                             in '..\src\common\duds.common.UsesParser.pas',
  duds.common.Interfaces                             in '..\src\common\duds.common.Interfaces.pas',
  duds.common.Types                                  in '..\src\common\duds.common.Types.pas',
  duds.common.Language                               in '..\src\common\duds.common.Language.pas',
  duds.common.Files                                  in '..\src\common\duds.common.Files.pas',
  duds.common.Strings                                in '..\src\common\duds.common.Strings.pas',
  duds.common.Helper.IniFile                         in '..\src\common\duds.common.Helper.IniFile.pas',
  duds.common.Classes                                in '..\src\common\duds.common.Classes.pas',
  duds.common.Utils                                  in '..\src\common\duds.common.Utils.pas',
  duds.common.Singleton                              in '..\src\common\duds.common.Singleton.pas',
  duds.common.Refactoring                            in '..\src\common\duds.common.Refactoring.pas',
  duds.common.Log                                    in '..\src\common\duds.common.Log.pas',
  duds.common.Delphi                                 in '..\src\common\duds.common.Delphi.pas',
  duds.common.modules                                in '..\src\common\duds.common.modules.pas',
  duds.common.modulesSerializer                      in '..\src\common\duds.common.modulesSerializer.pas',
  duds.analyzer.CustomAnalyzer                       in '..\src\analyzer\duds.analyzer.CustomAnalyzer.pas',  
  duds.analyzer.Stats                                in '..\src\analyzer\duds.analyzer.Stats.pas',
  duds.analyzer.model                                in '..\src\analyzer\duds.analyzer.model.pas',
  duds.analyzer.FileScanner                          in '..\src\analyzer\duds.analyzer.FileScanner.pas',
  duds.analyzer.IncludeHandler                       in '..\src\analyzer\duds.analyzer.IncludeHandler.pas',
  duds.analyzer.UnitsAnalyzer                        in '..\src\analyzer\duds.analyzer.UnitsAnalyzer.pas',
  duds.analyzer.ModulesAnalyzer                      in '..\src\analyzer\duds.analyzer.ModulesAnalyzer.pas',
  duds.analyzer.Facade                               in '..\src\analyzer\duds.analyzer.Facade.pas',
  duds.export.units.Gephi                            in '..\src\export\duds.export.units.Gephi.pas',
  duds.export.units.GraphML                          in '..\src\export\duds.export.units.GraphML.pas',
  duds.export.modules.CSV                            in '..\src\export\duds.export.modules.CSV.pas',
  duds.export.modules.GraphML                        in '..\src\export\duds.export.modules.GraphML.pas',
  duds.refactoring.RenameUnit                        in '..\src\refactoring\duds.refactoring.RenameUnit.pas',
  duds.refactoring.AddUnitToUses                     in '..\src\refactoring\duds.refactoring.AddUnitToUses.pas',
  duds.refactoring.FormatUsesHelper                  in '..\src\refactoring\duds.refactoring.FormatUsesHelper.pas',
  duds.refactoring.FormatUses                        in '..\src\refactoring\duds.refactoring.FormatUses.pas',
  duds.refactoring.PascalAnalyzerUsesReportProcessor in '..\src\refactoring\duds.refactoring.PascalAnalyzerUsesReportProcessor.pas',
  duds.refactoring.RemoveUnusedUnits                 in '..\src\refactoring\duds.refactoring.RemoveUnusedUnits.pas',

  duds.common.UsesParser.SimpleLexer.test            in '..\test\common\duds.common.UsesParser.SimpleLexer.test.pas',
  duds.common.UsesParser.test                        in '..\test\common\duds.common.UsesParser.test.pas',
  duds.common.ModulesSerializer.test                 in '..\test\common\duds.common.ModulesSerializer.test.pas',
  duds.analyzer.ModulesAnalyzer.test                 in 'analyzer\duds.analyzer.ModulesAnalyzer.test.pas',
  duds.refactoring.FormatUsesHelper.test             in '..\test\refactoring\duds.refactoring.FormatUsesHelper.test.pas',
  duds.refactoring.FormatUses.test                   in '..\test\refactoring\duds.refactoring.FormatUses.test.pas',
  duds.refactoring.RemoveUnusedUnits.test            in '..\test\refactoring\duds.refactoring.RemoveUnusedUnits.test.pas';

var
  runner : ITestRunner;
  results : IRunResults;
  logger : ITestLogger;
  nunitLogger : ITestLogger;
begin
{$IFDEF TESTINSIGHT}
  TestInsight.DUnitX.RunRegisteredTests;
  exit;
{$ENDIF}
  try
    //Check command line options, will exit if invalid
    TDUnitX.CheckCommandLine;
    //Create the test runner
    runner := TDUnitX.CreateRunner;
    //Tell the runner to use RTTI to find Fixtures
    runner.UseRTTI := True;
    //tell the runner how we will log things
    //Log to the console window
    logger := TDUnitXConsoleLogger.Create(true);
    runner.AddLogger(logger);
    //Generate an NUnit compatible XML File
    nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
    runner.AddLogger(nunitLogger);
    runner.FailsOnNoAsserts := False; //When true, Assertions must be made during tests;

    //Run tests
    results := runner.Execute;
    if not results.AllPassed then
      System.ExitCode := EXIT_ERRORS;

    {$IFNDEF CI}
    //We don't want this happening when running under CI.
    if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then
    begin
      System.Write('Done.. press <Enter> key to quit.');
      System.Readln;
    end;
    {$ENDIF}
  except
    on E: Exception do
      System.Writeln(E.ClassName, ': ', E.Message);
  end;
end.
