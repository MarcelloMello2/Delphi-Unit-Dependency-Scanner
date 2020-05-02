program duds;

uses
  Vcl.Forms,
  SimpleParser.Lexer       in '3rdParty\DelphiAST\Source\SimpleParser\SimpleParser.Lexer.pas',
  SimpleParser.Lexer.Types in '3rdParty\DelphiAST\Source\SimpleParser\SimpleParser.Lexer.Types.pas',
  Duds.Common.UnitInfo,
  Duds.Common.UsedUnitInfo,
  Duds.Common.UsesParser,
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
  Duds.Modules.Analyzer,
  Duds.Modules.Export.CSV,
  Duds.Modules.Export.GraphML,
  Duds.Refactoring.FormatUsesHelper,
  Duds.Refactoring.FormatUses,
  Duds.Refactoring.PascalAnalyzerUsesReportProcessor,
  Duds.Refactoring.RemoveUnusedUnits,
  Duds.Vcl.HourGlass,
  Duds.Vcl.Utils,
  Duds.Vcl.VirtualTreeview,
  Duds.Vcl.Form.Main {frmMain},
  Duds.Vcl.Form.FindReplace {frmSearchAndReplace},
  Duds.Vcl.Form.Settings {frmDependencyScannerSetting},
  Duds.Vcl.Form.Rename {frmRenameUnit},
  Duds.Vcl.Form.AddUnitToUses {frmAddNewUnit};

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Delphi Unit Dependency Scanner';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
