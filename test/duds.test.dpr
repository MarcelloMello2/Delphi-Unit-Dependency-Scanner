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
  SimpleParser.Lexer                                 in '..\src\3rdParty\DelphiAST\source\SimpleParser\SimpleParser.Lexer.pas',
  SimpleParser.Lexer.Types                           in '..\src\3rdParty\DelphiAST\source\SimpleParser\SimpleParser.Lexer.Types.pas',
  Duds.Common.UsesParser.SimpleLexer.test            in '..\test\Duds.Common.UsesParser.SimpleLexer.test.pas',
  Duds.Common.UnitInfo                               in '..\src\Duds.Common.UnitInfo.pas',
  Duds.Common.UsedUnitInfo                           in '..\src\Duds.Common.UsedUnitInfo.pas',
  Duds.Common.UsesParser                             in '..\src\Duds.Common.UsesParser.pas',
  Duds.Common.UsesParser.test                        in '..\test\Duds.Common.UsesParser.test.pas',
  Duds.Common.Interfaces                             in '..\src\Duds.Common.Interfaces.pas',
  Duds.Common.Types                                  in '..\src\Duds.Common.Types.pas',
  Duds.Common.Language                               in '..\src\Duds.Common.Language.pas',
  Duds.Common.Files                                  in '..\src\Duds.Common.Files.pas',
  Duds.Common.Strings                                in '..\src\Duds.Common.Strings.pas',
  Duds.Common.Helper.IniFile                         in '..\src\Duds.Common.Helper.IniFile.pas',
  Duds.Common.Classes                                in '..\src\Duds.Common.Classes.pas',
  Duds.Common.Utils                                  in '..\src\Duds.Common.Utils.pas',
  Duds.Common.Singleton                              in '..\src\Duds.Common.Singleton.pas',
  Duds.Common.Refactoring                            in '..\src\Duds.Common.Refactoring.pas',
  Duds.Common.Log                                    in '..\src\Duds.Common.Log.pas',
  Duds.Common.Delphi                                 in '..\src\Duds.Common.Delphi.pas',
  Duds.Scan.Model                                    in '..\src\Duds.Scan.Model.pas',
  Duds.Scan.FileScanner                              in '..\src\Duds.Scan.FileScanner.pas',
  Duds.Scan.DependencyAnalyzer                       in '..\src\Duds.Scan.DependencyAnalyzer.pas',
  Duds.Refactoring.RenameUnit                        in '..\src\Duds.Refactoring.RenameUnit.pas',
  Duds.Refactoring.AddUnitToUses                     in '..\src\Duds.Refactoring.AddUnitToUses.pas',
  Duds.Export.Gephi                                  in '..\src\Duds.Export.Gephi.pas',
  Duds.Export.GraphML                                in '..\src\Duds.Export.GraphML.pas',
  Duds.Modules.Classes                               in '..\src\Duds.Modules.Classes.pas',
  Duds.Modules.Classes.test                          in '..\test\Duds.Modules.Classes.test.pas',
  Duds.Modules.Analyzer                              in '..\src\Duds.Modules.Analyzer.pas',
  Duds.Refactoring.FormatUsesHelper                  in '..\src\Duds.Refactoring.FormatUsesHelper.pas',
  Duds.Refactoring.FormatUsesHelper.test             in '..\test\Duds.Refactoring.FormatUsesHelper.test.pas',
  Duds.Refactoring.FormatUses                        in '..\src\Duds.Refactoring.FormatUses.pas',
  Duds.Refactoring.FormatUses.test                   in '..\test\Duds.Refactoring.FormatUses.test.pas',
  Duds.Refactoring.PascalAnalyzerUsesReportProcessor in '..\src\Duds.Refactoring.PascalAnalyzerUsesReportProcessor.pas',
  Duds.Refactoring.RemoveUnusedUnits                 in '..\src\Duds.Refactoring.RemoveUnusedUnits.pas',
  Duds.Refactoring.RemoveUnusedUnits.test            in '..\test\Duds.Refactoring.RemoveUnusedUnits.test.pas';

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
