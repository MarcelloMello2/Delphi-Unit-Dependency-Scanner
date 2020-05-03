program duds;

uses
  Vcl.Forms,
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
  duds.analyzer.Stats                                in '..\src\analyzer\duds.analyzer.Stats.pas',
  duds.analyzer.model                                in '..\src\analyzer\duds.analyzer.model.pas',
  duds.analyzer.FileScanner                          in '..\src\analyzer\duds.analyzer.FileScanner.pas',
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
  duds.gui.HourGlass                                 in '..\src\gui\duds.gui.HourGlass.pas',
  duds.gui.Utils                                     in '..\src\gui\duds.gui.Utils.pas',
  duds.gui.VirtualTreeview                           in '..\src\gui\duds.gui.VirtualTreeview.pas',
  duds.gui.forms.Main                                in '..\src\gui\duds.gui.forms.Main.pas',
  duds.gui.forms.Settings                            in '..\src\gui\duds.gui.forms.Settings.pas',
  duds.gui.forms.SearchAndReplaceUnitName            in '..\src\gui\duds.gui.forms.SearchAndReplaceUnitName.pas',
  duds.gui.forms.RenameUnit                          in '..\src\gui\duds.gui.forms.RenameUnit.pas',
  duds.gui.forms.AddUnitToUses                       in '..\src\gui\duds.gui.forms.AddUnitToUses.pas';

{$R *.res}

var
  frmMain: TfrmMain;
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
