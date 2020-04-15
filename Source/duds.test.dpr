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
  Duds.Common.Parser.Pascal,
  Duds.Common.Parser.Pascal.Tokeniser,
  Duds.Common.Interfaces,
  Duds.Common.Types,
  Duds.Common.Language,
  Duds.Common.Files,
  Duds.Common.Strings,
  Duds.Common.Helper.IniFile,
  Duds.Common.Classes,
  Duds.Common.Utils,
  Duds.Common.Singleton,
  Duds.Common.Refactoring,
  Duds.Common.Log,
  Duds.Common.Delphi,
  Duds.Scan.Model,
  Duds.Scan.FileScanner,
  Duds.Scan.DependencyAnalyzer,
  Duds.Refactoring.RenameUnit,
  Duds.Refactoring.AddUnitToUses,
  Duds.Export.Gephi,
  Duds.Export.GraphML,
  Duds.Modules.Classes,
  Duds.Modules.Classes.test,
  Duds.Refactoring.FormatUsesHelper,
  Duds.Refactoring.FormatUsesHelper.test,
  Duds.Refactoring.PascalAnalyzerUsesReportProcessor,
  Duds.Refactoring.RemoveUnusedUnits,
  Duds.Refactoring.RemoveUnusedUnits.test;

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
