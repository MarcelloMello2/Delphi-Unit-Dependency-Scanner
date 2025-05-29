// -----------------------------------------------------------------------------
//
// The contents of this file are subject to the Mozilla Public License
// Version 2.0 (the "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
// http://www.mozilla.org/MPL/
//
// Alternatively, you may redistribute this library, use and/or modify it under
// the terms of the GNU Lesser General Public License as published by the
// Free Software Foundation; either version 2.1 of the License, or (at your
// option) any later version. You may obtain a copy of the LGPL at
// http://www.gnu.org/copyleft/.
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
// the specific language governing rights and limitations under the License.
//
// Orginally released as freeware then open sourced in July 2017.
//
// The initial developer of the original code is Easy-IP AS
// (Oslo, Norway, www.easy-ip.net), written by Paul Spencer Thornton -
// paul.thornton@easy-ip.net.
//
// (C) 2017 Easy-IP AS. All Rights Reserved.
//
// -----------------------------------------------------------------------------

unit duds.gui.forms.Main;

interface

uses
  System.SysUtils, System.Variants, System.Classes, System.ImageList,
  System.Actions, System.DateUtils,
  System.Generics.Collections, System.UITypes, System.IOUtils,

  Xml.XMLDoc, Xml.XMLIntf,

  Winapi.Windows, Winapi.Messages, Winapi.ShellAPI,
  System.Win.Registry,

  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.ActnList, Vcl.ToolWin, Vcl.ActnMan,
  Vcl.ImgList, Vcl.ActnCtrls, Vcl.PlatformDefaultStyleActnCtrls, Vcl.Mask,
  Vcl.Menus,

  VirtualTrees,
  VirtualTrees.Types,

  SynEditHighlighter, SynHighlighterPas, SynEdit,

  duds.common.Types,
  duds.common.Classes,
  duds.common.Files,
  duds.common.Strings,
  duds.common.Utils,
  duds.common.Interfaces,
  duds.common.UnitInfo,
  duds.common.UsedUnitInfo,
  duds.common.Refactoring,
  duds.common.Language,
  duds.common.Log,
  duds.common.modules,
  duds.common.modulesSerializer,
  duds.analyzer.model,
  duds.analyzer.FileScanner,
  duds.analyzer.UnitsAnalyzer,
  duds.analyzer.ModulesAnalyzer,
  duds.analyzer.Facade,
  duds.export.modules.CSV,
  duds.export.modules.GraphML,
  duds.export.units.Gephi,
  duds.export.units.GraphML,
  duds.refactoring.FormatUses,
  duds.refactoring.RenameUnit,
  duds.refactoring.AddUnitToUses,
  duds.refactoring.PascalAnalyzerUsesReportProcessor,
  duds.refactoring.RemoveUnusedUnits,
  duds.gui.HourGlass,
  duds.gui.Utils,
  duds.gui.VirtualTreeview, SynEditCodeFolding, VirtualTrees.BaseAncestorVCL,
  VirtualTrees.BaseTree, VirtualTrees.AncestorVCL;

type
  TfrmMain = class(TForm)
    SynPasSyn1: TSynPasSyn;
    ActionManager1: TActionManager;
    actStartScan: TAction;
    actShowUnitsNotInPath: TAction;
    actSaveChanges: TAction;
    actRename: TAction;
    actApplyRenameList: TAction;
    popTree: TPopupMenu;
    Rename1: TMenuItem;
    ShowUnitsnotinPath1: TMenuItem;
    N1: TMenuItem;
    actExpandAll: TAction;
    actExpand: TAction;
    actCollapseAll: TAction;
    actCollapse: TAction;
    N3: TMenuItem;
    Expand1: TMenuItem;
    ExpandAll1: TMenuItem;
    Collapse1: TMenuItem;
    CollapseAll1: TMenuItem;
    N4: TMenuItem;
    actStopScan: TAction;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    actSettings: TAction;
    Settings1: TMenuItem;
    N5: TMenuItem;
    actExit: TAction;
    Exit1: TMenuItem;
    Scan1: TMenuItem;
    View1: TMenuItem;
    Scan2: TMenuItem;
    Stop1: TMenuItem;
    ShowUnitsnotinPath2: TMenuItem;
    N6: TMenuItem;
    ExpandAll2: TMenuItem;
    CollapseAll2: TMenuItem;
    Edit1: TMenuItem;
    Rename2: TMenuItem;
    SaveChanges1: TMenuItem;
    N7: TMenuItem;
    actLoadProject: TAction;
    actSaveProject: TAction;
    actSaveProjectAs: TAction;
    LoadProject1: TMenuItem;
    SaveProject1: TMenuItem;
    SaveProjectAs1: TMenuItem;
    N2: TMenuItem;
    N8: TMenuItem;
    openDialog_Project: TOpenDialog;
    saveDialog_Project: TSaveDialog;
    OpenDialogMultipleRenames: TOpenDialog;
    ActionToolBar1: TActionToolBar;
    pnlBackground: TPanel;
    pnlMain: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    pcView: TPageControl;
    tabTree: TTabSheet;
    Splitter2: TSplitter;
    pnlTree: TPanel;
    Panel5: TPanel;
    pcSource: TPageControl;
    tabParentFile: TTabSheet;
    memParentFile: TSynEdit;
    tabSelectedFile: TTabSheet;
    memSelectedFile: TSynEdit;
    tabList: TTabSheet;
    Splitter3: TSplitter;
    pnlList: TPanel;
    Panel10: TPanel;
    pnlLog: TPanel;
    Panel9: TPanel;
    pcList: TPageControl;
    tabUsedBy: TTabSheet;
    tabSource: TTabSheet;
    memListFile: TSynEdit;
    tmrClose: TTimer;
    tabUsesList: TTabSheet;
    actNewProject: TAction;
    actCloseProject: TAction;
    New1: TMenuItem;
    N10: TMenuItem;
    Close1: TMenuItem;
    N9: TMenuItem;
    actSearchAndReplace: TAction;
    SearchandReplace1: TMenuItem;
    tmrLoaded: TTimer;
    actShowFile: TAction;
    ShowfileinWindowsExplorer1: TMenuItem;
    N11: TMenuItem;
    actSaveToXML: TAction;
    saveDialog_Units_XML: TSaveDialog;
    actSaveToGephiCSV: TAction;
    N12: TMenuItem;
    SavetoXML1: TMenuItem;
    SavetoGephiCSV1: TMenuItem;
    saveDialog_Units_GephiCSV: TSaveDialog;
    actExportUnitsToGraphML: TAction;
    actExportUnitsToGraphMLCurrentModule: TAction;
    saveDialog_Units_GraphML: TSaveDialog;
    edtSearchTree: TEdit;
    edtSearchList: TEdit;
    edtSearchUsedByList: TEdit;
    edtSearchUsesList: TEdit;
    vtUnitsList: TVirtualStringTree;
    vtUnitsTree: TVirtualStringTree;
    vtUsedByUnits: TVirtualStringTree;
    vtUsesUnits: TVirtualStringTree;
    vtStats: TVirtualStringTree;
    ImageList1: TImageList;
    vtLog: TVirtualStringTree;
    SearchandReplace2: TMenuItem;
    N13: TMenuItem;
    actSaveCircularRefs: TAction;
    Savecircularreference1: TMenuItem;
    RichEditUnitPath: TRichEdit;
    PanelFooter: TPanel;
    Splitter4: TSplitter;
    actAddUnitToUses: TAction;
    Applymultiplerenamesdefinedincsv1: TMenuItem;
    Addunittouseslistinallfilesthatcurrentlyusethisunit1: TMenuItem;
    N14: TMenuItem;
    Addunittouseslistinallfilesthatcurrentlyusethisunit2: TMenuItem;
    actRefactoringsDropDown: TAction;
    actExportDropDown: TAction;
    actRemoveUnusedUnitsProcessPalOutput: TAction;
    actRemoveUnUsedUnits: TAction;
    actFormatUsesOfFile: TAction;
    Refactorings1: TMenuItem;
    Formatusesofthisfile1: TMenuItem;
    Formatusesofthisfile2: TMenuItem;
    N15: TMenuItem;
    actExportModulesToGraphML: TAction;
    saveDialog_Modules_GraphML: TSaveDialog;
    Modules: TTabSheet;
    vtModules: TVirtualStringTree;
    actExportModulesToCSV: TAction;
    saveDialog_Modules_CSV: TSaveDialog;
    actShowOnlyUnknownModules: TAction;

    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);

    procedure vtCommonHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure vtCommonGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);

    procedure vtUnitsTreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure vtUnitsTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vtUnitsTreePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure vtUnitsTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
        Node: PVirtualNode; Column: TColumnIndex; const Text: string; const
        CellRect: TRect; var DefaultDraw: Boolean);      
    procedure vtUnitsTreeBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
      ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
    procedure vtUnitsTreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vtUnitsTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
      Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
    procedure vtUnitsTreeDblClick(Sender: TObject);
    procedure vtUnitsTreeFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode;
      OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure vtUnitsTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure edtSearchTreeChange(Sender: TObject);
    procedure edtSearchTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure edtSearchTreeKeyPress(Sender: TObject; var Key: Char);

    procedure vtUnitsListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var CellText: string);
    procedure vtUnitsListFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode;
      OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
    procedure vtUnitsListFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure vtUnitsListSearchComparison(Sender: TObject; Node: PVirtualNode; const SearchTerms: TStrings;
      var IsMatch: Boolean);
    procedure vtUnitsListDblClick(Sender: TObject);
    procedure vtUnitsListPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure vtUnitsListCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
      var Result: Integer);
    procedure vtUnitsListDrawText(Sender: TBaseVirtualTree; TargetCanvas:
        TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string;
        const CellRect: TRect; var DefaultDraw: Boolean);
    procedure edtSearchListChange(Sender: TObject);

    procedure vtModulesDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
        Node: PVirtualNode; Column: TColumnIndex; const Text: string; const
        CellRect: TRect; var DefaultDraw: Boolean);
    procedure vtModulesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
        Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

    procedure vtLogGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vtLogGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: TImageIndex);

    procedure vtStatsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure vtStatsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);

    procedure actStartScanExecute(Sender: TObject);
    procedure actStopScanExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actSettingsExecute(Sender: TObject);
    procedure actLoadProjectExecute(Sender: TObject);
    procedure actSaveProjectExecute(Sender: TObject);
    procedure actSaveProjectAsExecute(Sender: TObject);
    procedure actNewProjectExecute(Sender: TObject);
    procedure actCloseProjectExecute(Sender: TObject);

    procedure actShowUnitsNotInPathExecute(Sender: TObject);
    procedure actShowOnlyUnknownModulesExecute(Sender: TObject);

    procedure ActionManager1Update(Action: TBasicAction; var Handled: Boolean);

    procedure actCollapseAllExecute(Sender: TObject);
    procedure actCollapseExecute(Sender: TObject);
    procedure actExpandAllExecute(Sender: TObject);
    procedure actExpandExecute(Sender: TObject);

    procedure memListFileChange(Sender: TObject);
    procedure memParentFileChange(Sender: TObject);
    procedure memSelectedFileChange(Sender: TObject);
    procedure actSaveChangesExecute(Sender: TObject);

    procedure tmrCloseTimer(Sender: TObject);
    procedure tmrLoadedTimer(Sender: TObject);

    procedure edtSearchUsedByListEditChange(Sender: TObject);
    procedure edtSearchUsesListEditChange(Sender: TObject);
    procedure actSearchAndReplaceExecute(Sender: TObject);

    procedure actShowFileExecute(Sender: TObject);

    procedure actRenameExecute(Sender: TObject);
    procedure actApplyRenameListExecute(Sender: TObject);
    procedure actAddUnitToUsesExecute(Sender: TObject);
    procedure actRemoveUnUsedUnitsExecute(Sender: TObject);
    procedure actRemoveUnusedUnitsProcessPalOutputExecute(Sender: TObject);
    procedure actFormatUsesOfFileExecute(Sender: TObject);

    procedure actSaveToXMLExecute(Sender: TObject);
    procedure actSaveToGephiCSVExecute(Sender: TObject);
    procedure actExportUnitsToGraphMLExecute(Sender: TObject);
    procedure actExportUnitsToGraphMLCurrentModuleExecute(Sender: TObject);
    procedure actSaveCircularRefsExecute(Sender: TObject);
    procedure actExportModulesToCSVExecute(Sender: TObject);
    procedure actExportModulesToGraphMLExecute(Sender: TObject);

  private
    FModel: TDudsModel;
    FStats: TStringList; // TODO: this should be moved inside fModel.Stats.Units
    fAnalyzerFacade: TAnalyzerFacade;
    FTreeNodeObjects: TObjectList<TNodeObject>;
    FSearchText: String;
    FStartTime: TDateTime;
    fCancelled: Boolean;
    FBusy: Boolean;
    FpnlLogHeight: Integer;
    FEnvironmentSettings: TEnvironmentSettings;
    FProjectSettings: TProjectSettings;
    fProjectFilename: String;
    FLoadLastProject: Boolean;
    FModified: Boolean;
    FClosing: Boolean;
    FShowSearchTermParents: Boolean;
    FRunScanOnLoad: Boolean;

    procedure FillGUIFromModel;
    procedure SearchUnitsTree(const SearchText: String; FromFirstNode: Boolean);
    function UnitsTreeIsSearchHitNode(Node: PVirtualNode): Boolean;
    function GetNodePath(Node: PVirtualNode): String;
    procedure SetNodePathRichEdit(Node: PVirtualNode; ARichEdit: TRichEdit);
    procedure SetNodesVisibilityForAllNodes;
    procedure SetNodeVisibility(VT: TVirtualStringTree; Node: PVirtualNode; DelphiFile: TDelphiFile);
    function GetLinkedNode(Node: PVirtualNode): PVirtualNode;
    procedure UpdateTreeControls(Node: PVirtualNode);
    procedure UpdateStats;
    procedure UpdateListControls(Node: PVirtualNode);
    procedure ShowHideControls;
    procedure LoadSettings;
    procedure SaveSettings;
    function LoadProjectSettings(const Filename: String = ''; RunScanAfterLoad: Boolean = TRUE): Boolean;
    procedure SaveProjectSettings(const Filename: String = '');
    function GetSettingsFilename: String;
    procedure ExpandAll;
    procedure UpdateControls;
    procedure SetFormCaption;
    procedure SetModified(const Value: Boolean);
    function CheckSaveProject: Boolean;
    function SaveProject: Boolean;
    function SaveProjectAs: Boolean;
    function RenameDelphiFileWithDialog(RenameType: TRenameType): Boolean; overload;
    function RenameDelphiFileWithDialog(const DelphiFile: TDelphiFile; RenameType: TRenameType): Boolean; overload;
    procedure ApplyMultipleRenames(aCsvFilename: String; DummyRun, RenameHistoryFiles, InsertOldNameComment,
      RenameLowerCaseExtension: Boolean);
    procedure RenameDelphiFile(const aClearLog: Boolean; const SearchString, ReplaceString: String;
      const DummyRun, RenameHistoryFiles, ExactMatch, InsertOldNameComment,
      LowerCaseExtension: Boolean);
    procedure SearchUnitsList(VT: TVirtualStringTree; const SearchText: String);
    procedure SearchUnitsListChildList(VT: TVirtualStringTree; const SearchText: String; UsedBy: Boolean);
    procedure ClearGUIAndModelAndCloseControls;
    function CheckNotRunning: Boolean;
    procedure ResetSettings;
    function GetFocusedDelphiFile: TDelphiFile;
    procedure ExportToXML(const VT: TVirtualStringTree; const Filename: String);

    procedure Log(const Msg: String; const Severity: Integer); overload;
    procedure Log(const Msg: String; const Args: array of const; const Severity: Integer); overload;

    function GetID(const Node: PVirtualNode): Integer;
    procedure SetID(const Node: PVirtualNode; const ID: Integer);
    function GetFocusedID(const VT: TVirtualStringTree): Integer;
    function GetNodeIndex(const Node: PVirtualNode): Integer;
    procedure UpdateLogEntries;
    procedure ClearLog;
    procedure FixDPI;
    function CompareTwoModulesTreeNodes(aDelphiFile1,
      aDelphiFile2: TDelphiFile): Integer;
    procedure SetupUnitsListsColumns(tree: TVirtualStringTree);
    function GetModuleFromModuleNode(Node: PVirtualNode): TModule;

    property Modified: Boolean read FModified write SetModified;
  end;

implementation

uses
  duds.gui.forms.RenameUnit,
  duds.gui.forms.Settings,
  duds.gui.forms.SearchAndReplaceUnitName,
  duds.gui.forms.AddUnitToUses;

const
  cNotInSearchpathForegroundColor = clGray;

  cUnknownModuleBackgroundColor   = $000021DB;
  cUnknownModuleForegroundColor   = $00FAFAFA;

  cSearchHitMarkColor = $008CFFFF;

{$R *.dfm}

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FClosing then
    ClearGUIAndModelAndCloseControls
  else
  begin
    CanClose := FALSE;

    if CheckSaveProject then
    begin
      actStopScan.Execute;

      tmrClose.Enabled := TRUE;
    end;
  end;
end;

function TfrmMain.CheckSaveProject: Boolean;
begin
  Result := not Modified;

  if not Result then
  begin
    case MessageDlg(StrTheProjectHasBeen, mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        Result := SaveProject;
      mrNo:
        Result := TRUE;
      mrCancel:
        Abort;
    end;
  end;
end;

function TfrmMain.GetID(const Node: PVirtualNode): Integer;
var
  NodeData: PNodeData;
begin
  NodeData := Node.GetData;

  if Assigned(NodeData) then
  begin
    Result := NodeData.ID;
  end
  else
  begin
    raise Exception.Create('No node data found');
  end;
end;

procedure TfrmMain.SetID(const Node: PVirtualNode; const ID: Integer);
var
  NodeData: PNodeData;
begin
  NodeData := Node.GetData;

  if Assigned(NodeData) then
  begin
    NodeData.ID := ID;
    NodeData.Index := Node.Index;
  end;
end;

const
  cvtUnitsList_ColumnIdx_Unit              = 0;
  cvtUnitsList_ColumnIdx_FileType          = 1;
  cvtUnitsList_ColumnIdx_LoC               = 2;
  cvtUnitsList_ColumnIdx_UsedByUnits_Count = 3;
  cvtUnitsList_ColumnIdx_UsedUnits_Count   = 4;
  cvtUnitsList_ColumnIdx_FileName          = 5;
  cvtUnitsList_ColumnIdx_Module            = 6;

procedure TfrmMain.SetupUnitsListsColumns(tree: TVirtualStringTree);
var
  aColumn: TVirtualTreeColumn;
begin
  tree.Header.Columns.Clear;

  aColumn           := tree.Header.Columns.Add; // Column Index 0
  aColumn.Text      := 'Unit';
  aColumn.Width     := 300;

  aColumn           := tree.Header.Columns.Add; // Column Index 1
  aColumn.Text      := 'File Type';

  aColumn           := tree.Header.Columns.Add; // Column Index 2
  aColumn.Text      := 'LoC';
  aColumn.Alignment := taRightJustify;

  aColumn           := tree.Header.Columns.Add; // Column Index 3
  aColumn.Text      := 'Used By Units';
  aColumn.Alignment := taRightJustify;

  aColumn           := tree.Header.Columns.Add; // Column Index 4
  aColumn.Text      := 'Used Units';
  aColumn.Alignment := taRightJustify;

  aColumn           := tree.Header.Columns.Add; // Column Index 5
  aColumn.Text      := 'Filename';

  aColumn           := tree.Header.Columns.Add; // Column Index 6
  aColumn.Text      := 'Module';
  aColumn.Position  := 5; // bring before 'Filename'
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  pcSource.ActivePageIndex := 0;
  pcView.ActivePageIndex := 0;
  pcList.ActivePageIndex := 0;

  // Units tree
  vtUnitsTree.NodeDataSize := SizeOf(TNodeData);

  // Units lists (3x)
  vtUnitsList.NodeDataSize   := SizeOf(TNodeData);
  SetupUnitsListsColumns(vtUnitsList);
  vtUsedByUnits.NodeDataSize := SizeOf(TNodeData);
  SetupUnitsListsColumns(vtUsedByUnits);
  vtUsesUnits.NodeDataSize   := SizeOf(TNodeData);
  SetupUnitsListsColumns(vtUsesUnits);


  vtStats.NodeDataSize := SizeOf(TNodeData);
  vtModules.NodeDataSize := SizeOf(TNodeData);

  FModel := TDudsModel.Create;
  fAnalyzerFacade  := nil;
  FTreeNodeObjects := TObjectList<TNodeObject>.Create(TRUE);
  FStats := TStringList.Create;

  FEnvironmentSettings := TEnvironmentSettings.Create;
  FProjectSettings := TProjectSettings.Create;

  UpdateTreeControls(nil);
  UpdateListControls(nil);

  FLoadLastProject       := TRUE;
  FShowSearchTermParents := True;
  FRunScanOnLoad         := TRUE;

  FixDPI;

  LoadSettings;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  SaveSettings;

  FreeAndNil(FModel);
  FreeAndNil(FTreeNodeObjects);
  FreeAndNil(FStats);
  FreeAndNil(FEnvironmentSettings);
  FreeAndNil(FProjectSettings);
end;

procedure TfrmMain.Log(const Msg: String; const Severity: Integer);
begin
  TDudsLogger.GetInstance.Log(Msg, Severity);
  UpdateLogEntries;
end;

procedure TfrmMain.Log(const Msg: String; const Args: array of const; const
    Severity: Integer);
begin
  Log(Format(Msg, Args), Severity);
end;

function TfrmMain.LoadProjectSettings(const Filename: String; RunScanAfterLoad: Boolean): Boolean;
begin
  Result := FALSE;

  if FProjectSettings.LoadFromFile(Filename) then
  begin
    if (fProjectFilename <> '') and (RunScanAfterLoad) and (FRunScanOnLoad) and (FProjectSettings.RootFiles.Count > 0)
    then
      actStartScan.Execute;

    SetFormCaption;

    Result := TRUE;
  end;
end;

function TfrmMain.GetSettingsFilename: String;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetDocumentsPath) + 'DUDS\config.ini';
end;

procedure TfrmMain.LoadSettings;
begin
  if FEnvironmentSettings.LoadFromFile(GetSettingsFilename) then
  begin
    pnlLog.Height                 := FEnvironmentSettings.StatusLogHeight;
    pnlTree.Width                 := FEnvironmentSettings.TreeWidth;
    pnlList.Width                 := FEnvironmentSettings.ListWidth;
    actShowUnitsNotInPath.Checked := FEnvironmentSettings.ShowUnitsNotInPath;

    FLoadLastProject              := FEnvironmentSettings.LoadLastProject;
    fProjectFilename              := FEnvironmentSettings.ProjectFilename;
    FRunScanOnLoad                := FEnvironmentSettings.RunScanOnLoad;

    Left                          := FEnvironmentSettings.WindowLeft;
    Top                           := FEnvironmentSettings.WindowTop;
    Width                         := FEnvironmentSettings.WindowWidth;
    Height                        := FEnvironmentSettings.WindowHeight;
    PanelFooter.Height            := FEnvironmentSettings.UnitPatchHeight;

    WindowState                   := TWindowState(FEnvironmentSettings.WindowState);
  end;
end;

procedure TfrmMain.memListFileChange(Sender: TObject);
begin
  memListFile.Modified := TRUE;
end;

procedure TfrmMain.memParentFileChange(Sender: TObject);
begin
  memParentFile.Modified := TRUE;
end;

procedure TfrmMain.memSelectedFileChange(Sender: TObject);
begin
  memSelectedFile.Modified := TRUE;
end;

procedure TfrmMain.edtSearchListChange(Sender: TObject);
begin
  SearchUnitsList(vtUnitsList, edtSearchList.Text);
end;

procedure TfrmMain.ClearLog;
begin
  TDudsLogger.GetInstance.Clear;
  UpdateLogEntries;
end;

procedure TfrmMain.edtSearchUsedByListEditChange(Sender: TObject);
begin
  SearchUnitsListChildList(vtUsedByUnits, edtSearchUsedByList.Text, TRUE);
end;

procedure TfrmMain.edtSearchUsesListEditChange(Sender: TObject);
begin
  SearchUnitsListChildList(vtUsesUnits, edtSearchUsesList.Text, FALSE);
end;

function TfrmMain.GetFocusedID(const VT: TVirtualStringTree): Integer;
begin
  Result := GetID(VT.FocusedNode);
end;

// search detail-list "Uses Unit" or "Used by Unit"
procedure TfrmMain.SearchUnitsListChildList(VT: TVirtualStringTree; const SearchText: String; UsedBy: Boolean);
var
  Node: PVirtualNode;
  LowerSearchText, UsesOrUsedUnitName: String;
  UsesOrUsedDelphiFile, DelphiFile: TDelphiFile;
  InSearchPathOrShowAll,
  UnitMatchesList, UnitMatchesSearchTerm: Boolean;
begin
  VT.BeginUpdate;
  try
    LowerSearchText := LowerCase(SearchText);

    Node := VT.GetFirst;

    while Node <> nil do
    begin
      DelphiFile := FModel.DelphiFileList[GetID(Node)];

      if UsedBy then
        UnitMatchesList := FModel.IsUnitUsed(
          FModel.DelphiFileList[GetFocusedID(vtUnitsList)].UnitInfo.DelphiUnitName,
          DelphiFile)
      else
        UnitMatchesList := FModel.IsUnitUsed(
          DelphiFile.UnitInfo.DelphiUnitName,
          FModel.DelphiFileList[GetFocusedID(vtUnitsList)]);

      InSearchPathOrShowAll := DelphiFile.InSearchPath or (not DelphiFile.InSearchPath and
        actShowUnitsNotInPath.Checked);
      UnitMatchesSearchTerm := ((LowerSearchText = '') or ((pos(LowerSearchText, LowerCase(DelphiFile.UnitInfo.DelphiUnitName)) <> 0)));

      VT.IsVisible[Node] := UnitMatchesSearchTerm and InSearchPathOrShowAll and UnitMatchesList;

      Node := Node.NextSibling;
    end;

    VT.Invalidate;
  finally
    VT.EndUpdate;
  end;
end;

procedure TfrmMain.SaveProjectSettings(const Filename: String);
begin
  if Filename <> '' then
  begin
    FProjectSettings.SaveToFile(Filename);

    Modified := FALSE;
  end;
end;

procedure TfrmMain.SaveSettings;
begin
  FEnvironmentSettings.StatusLogHeight    := pnlLog.Height;
  FEnvironmentSettings.TreeWidth          := pnlTree.Width;
  FEnvironmentSettings.ListWidth          := pnlList.Width;
  FEnvironmentSettings.ShowUnitsNotInPath := actShowUnitsNotInPath.Checked;
  FEnvironmentSettings.LoadLastProject    := FLoadLastProject;
  FEnvironmentSettings.ProjectFilename    := fProjectFilename;
  FEnvironmentSettings.RunScanOnLoad      := FRunScanOnLoad;

  FEnvironmentSettings.WindowLeft         := Left;
  FEnvironmentSettings.WindowTop          := Top;
  FEnvironmentSettings.WindowWidth        := Width;
  FEnvironmentSettings.WindowHeight       := Height;
  FEnvironmentSettings.UnitPatchHeight    := PanelFooter.Height;

  FEnvironmentSettings.WindowState        := Integer(WindowState);

  ForceDirectories(ExtractFileDir(GetSettingsFilename));
  FEnvironmentSettings.SaveToFile(GetSettingsFilename);
end;

procedure TfrmMain.vtLogGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  if (Kind in [ikNormal, ikSelected]) and (Column <= 0) then
  begin
    case TDudsLogger.GetInstance.LogEntries[Node.Index].Severity of
      LogWarning:
        ImageIndex := 4;
      LogError:
        ImageIndex := 5;
    else
      ImageIndex := 3;
    end;
  end;
end;

procedure TfrmMain.vtLogGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
begin
  CellText := TDudsLogger.GetInstance.LogEntries[Node.Index].Text;
end;

procedure TfrmMain.vtStatsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  Stat: String;
begin
  Stat := FStats[Node.Index];

  case Column of
    0:
      CellText := Copy(Stat, 1, pos('=', Stat) - 1);
    1:
      CellText := Copy(Stat, pos('=', Stat) + 1, MaxInt);
  end;
end;

procedure TfrmMain.vtStatsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);
begin
  if Column = 0 then
    TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
end;

procedure TfrmMain.vtUnitsTreeBeforeItemErase(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  ItemRect: TRect; var ItemColor: TColor; var EraseAction: TItemEraseAction);
begin
  if UnitsTreeIsSearchHitNode(Node) then
  begin
    EraseAction := eaColor;
    ItemColor   := cSearchHitMarkColor;
  end
  else if (FTreeNodeObjects[GetID(Node)].SearchTermInChildren) then
  begin
    EraseAction := eaColor;
    ItemColor   := $00CCFFCC;
  end;
end;

function TfrmMain.CompareTwoModulesTreeNodes(aDelphiFile1, aDelphiFile2: TDelphiFile): Integer;
var
  aText1,
  aText2: string;
begin
  aText1 := '';
  aText2 := '';

  if aDelphiFile1.UnitInfo.Module <> nil then
    aText1 := aDelphiFile1.UnitInfo.Module.Name;

  if aDelphiFile2.UnitInfo.Module <> nil then
    aText2 := aDelphiFile2.UnitInfo.Module.Name;

  Result := CompareStr(aText1, aText2);
end;

procedure TfrmMain.vtUnitsTreeCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);
var
  NodeObject1, NodeObject2: TNodeObject;
begin
  NodeObject1 := FTreeNodeObjects[GetID(Node1)];
  NodeObject2 := FTreeNodeObjects[GetID(Node2)];

  case Column of
    0:
      Result := CompareStr(NodeObject1.DelphiFile.UnitInfo.DelphiUnitName,
        NodeObject2.DelphiFile.UnitInfo.DelphiUnitName);
    1:
      Result := CompareStr(NodeObject1.DelphiFile.UnitInfo.PreviousUnitName,
        NodeObject2.DelphiFile.UnitInfo.PreviousUnitName);
    2:
      Result := CompareStr(DelphiFileTypeStrings[NodeObject1.DelphiFile.UnitInfo.DelphiFileType],
        DelphiFileTypeStrings[NodeObject2.DelphiFile.UnitInfo.DelphiFileType]);
    3:
      Result := CompareInteger(NodeObject1.DelphiFile.UnitInfo.LinesOfCode, NodeObject2.DelphiFile.UnitInfo.LinesOfCode);
    4:
      Result := CompareInteger(NodeObject1.DelphiFile.UsedCount, NodeObject2.DelphiFile.UsedCount);
    5:
      Result := CompareInteger(NodeObject1.DelphiFile.UnitInfo.UsedUnits.Count,
        NodeObject2.DelphiFile.UnitInfo.UsedUnits.Count);
    6:
      Result := 0;
    7:
      Result := CompareInteger(Integer(FTreeNodeObjects[GetID(Node1)].CircularReference),
        Integer(FTreeNodeObjects[GetID(Node2)].CircularReference));
    8:
      Result := CompareBoolean(FTreeNodeObjects[GetID(Node1)].Link <> nil, FTreeNodeObjects[GetID(Node2)].Link <> nil);
    9:
      Result := CompareStr(NodeObject1.DelphiFile.UnitInfo.Filename, NodeObject2.DelphiFile.UnitInfo.Filename);
    10:
      Result := CompareTwoModulesTreeNodes(NodeObject1.DelphiFile, NodeObject2.DelphiFile);
  end;

end;

procedure TfrmMain.vtUnitsTreeDblClick(Sender: TObject);
begin
  if vtUnitsTree.FocusedNode <> nil then
    vtUnitsTree.SelectNodeEx(GetLinkedNode(vtUnitsTree.FocusedNode));
end;

procedure TfrmMain.vtUnitsTreeFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  UpdateTreeControls(Node);
end;

procedure TfrmMain.vtUnitsTreeFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode;
  OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
begin
  if memParentFile.Modified then
    case MessageDlg(Format(StrSHasBeenModifi, [tabParentFile.Caption]), mtWarning, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        memParentFile.Lines.SaveToFile(FTreeNodeObjects[GetID(OldNode.Parent)].DelphiFile.UnitInfo.Filename);
      mrCancel:
        begin
          Allowed := FALSE;

          Exit;
        end;
    end;

  if memSelectedFile.Modified then
    case MessageDlg(Format(StrSHasBeenModifi, [tabSelectedFile.Caption]), mtWarning, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        memSelectedFile.Lines.SaveToFile(FTreeNodeObjects[GetID(OldNode)].DelphiFile.UnitInfo.Filename);
      mrCancel:
        Allowed := FALSE;
    end;
end;

procedure TfrmMain.UpdateTreeControls(Node: PVirtualNode);
var
  ParentFileInfo: IUnitInfo;
  UsedUnitInfo: IUsedUnitInfo;
  ParentFileNameToShow: string;
begin
  SetNodePathRichEdit(Node, RichEditUnitPath);

  tabParentFile.TabVisible := (Node <> nil) and (Node.Parent <> vtUnitsTree.RootNode);
  tabSelectedFile.TabVisible := (Node <> nil) and (FTreeNodeObjects[GetID(Node)].DelphiFile.InSearchPath);

  if Node <> nil then
  begin
    pcSource.Visible := TRUE;

    if (FTreeNodeObjects[GetID(Node)].DelphiFile.InSearchPath) and
      (FileExists(FTreeNodeObjects[GetID(Node)].DelphiFile.UnitInfo.Filename)) then
    begin
      tabSelectedFile.Caption := ExtractFileName(FTreeNodeObjects[GetID(Node)].DelphiFile.UnitInfo.Filename);
      memSelectedFile.Lines.LoadFromFile(FTreeNodeObjects[GetID(Node)].DelphiFile.UnitInfo.Filename);
      memSelectedFile.Modified := FALSE;
    end;

    if (Node.Parent <> vtUnitsTree.RootNode) and (FileExists(FTreeNodeObjects[GetID(Node.Parent)].DelphiFile.UnitInfo.Filename))
    then
    begin
      ParentFileInfo       := FTreeNodeObjects[GetID(Node.Parent)].DelphiFile.UnitInfo;
      ParentFileNameToShow := ParentFileInfo.Filename;

      UsedUnitInfo := ParentFileInfo.UsedUnits[GetNodeIndex(Node)];

      if not UsedUnitInfo.IsInIncludeFileName.IsEmpty then
        ParentFileNameToShow := UsedUnitInfo.IsInIncludeFileName;

      tabParentFile.Caption := ExtractFileName(ParentFileNameToShow);
      memParentFile.Lines.LoadFromFile(ParentFileNameToShow);

      memParentFile.SelStart := UsedUnitInfo.Position;
      if UsedUnitInfo.InFilePosition > 0 then
      begin
        // memParentFile.SelStart  := UsedUnitInfo.InFilePosition; // uncomment this line to visualize unitname recognition in the "in file" part
      end;
      memParentFile.SelLength := Length(UsedUnitInfo.DelphiUnitName);

      memParentFile.Modified := FALSE;

      if tabSelectedFile.TabVisible then
        pcSource.ActivePageIndex := 1
      else
        pcSource.ActivePageIndex := 0;
    end;
  end
  else
    pcSource.Visible := FALSE;
end;

function TfrmMain.GetNodePath(Node: PVirtualNode): String;
begin
  Result := '';

  while (Node <> nil) and (Node <> vtUnitsTree.RootNode) do
  begin
    if Result <> '' then
      Result := ' -> ' + Result;

    Result := FTreeNodeObjects[GetID(Node)].DelphiFile.UnitInfo.DelphiUnitName + Result;

    Node := Node.Parent;
  end;
end;

procedure TfrmMain.SetNodePathRichEdit(Node: PVirtualNode; ARichEdit: TRichEdit);
var
  LStartNodeFile: string;
  LParentNodeFile: string;
  LArrow: string;
begin
  ARichEdit.Clear;

  if Node <> nil then
    LStartNodeFile := FTreeNodeObjects[GetID(Node)].DelphiFile.UnitInfo.DelphiUnitName;

  LArrow := '';

  while (Node <> nil) and (Node <> vtUnitsTree.RootNode) do
  begin
    LParentNodeFile := FTreeNodeObjects[GetID(Node)].DelphiFile.UnitInfo.DelphiUnitName;

    RichEditUnitPath.SelStart := 0;

    if AnsiSameText(LStartNodeFile, LParentNodeFile) then
      RichEditUnitPath.SelAttributes.Style := [fsBold]
    else
      RichEditUnitPath.SelAttributes.Style := [];

    RichEditUnitPath.SelText := LParentNodeFile + LArrow;
    LArrow := ' -> ';

    Node := Node.Parent;
  end;
end;

procedure TfrmMain.vtUnitsTreeGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  if (Kind in [ikNormal, ikSelected]) and (Column = 0) then
  begin
    case FTreeNodeObjects[GetID(Node)].CircularReference of
      crNone:
        ImageIndex := 0;
      crSemiCircular:
        ImageIndex := 1;
      crCircular:
        ImageIndex := 2;
    end;
  end;
end;

procedure TfrmMain.vtUnitsTreeGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TNodeData);
end;

function TfrmMain.GetLinkedNode(Node: PVirtualNode): PVirtualNode;
begin
  if Node = nil then
    Result := nil
  else
  begin
    Result := FTreeNodeObjects[GetID(Node)].Link;

    if Result = nil then
      Result := Node;
  end;
end;

function TfrmMain.GetNodeIndex(const Node: PVirtualNode): Integer;
var
  NodeData: PNodeData;
begin
  NodeData := Node.GetData;

  if Assigned(NodeData) then
  begin
    Result := NodeData.Index;
  end
  else
  begin
    raise Exception.Create('No node data found');
  end;
end;

procedure TfrmMain.vtUnitsTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  UnitInfo: IUnitInfo;
  DelphiFile: TDelphiFile;
  ParentUnitFilename: String;
begin
  DelphiFile := FTreeNodeObjects[GetID(Node)].DelphiFile;
  UnitInfo := DelphiFile.UnitInfo;

  if TextType = ttStatic then
  begin
    CellText := '';

    if (Node.Parent <> Sender.RootNode) and (Column = 0) and
      (GetNodeIndex(Node) < FTreeNodeObjects[GetID(Node.Parent)].DelphiFile.UnitInfo.UsedUnits.Count) then
    begin
      ParentUnitFilename := FTreeNodeObjects[GetID(Node.Parent)].DelphiFile.UnitInfo.UsedUnits[GetNodeIndex(Node)
        ].DelphiUnitName;

      if not SameText(ParentUnitFilename, UnitInfo.DelphiUnitName) then
        CellText := '(' + ParentUnitFilename + ')';
    end;
  end
  else
  begin
    CellText := '-';

    case Column of
      0:
        CellText := UnitInfo.DelphiUnitName;

      1:
        CellText := UnitInfo.PreviousUnitName;

      2:
        if DelphiFile.InSearchPath then
          CellText := DelphiFileTypeStrings[UnitInfo.DelphiFileType];

      3:
        if UnitInfo.LinesOfCode > 0 then
          CellText := IntToStr(UnitInfo.LinesOfCode);

      4:
        CellText := IntToStr(DelphiFile.UsedCount);

      5:
        if DelphiFile.UnitInfo.UsedUnits.Count > 0 then
          CellText := IntToStr(DelphiFile.UnitInfo.UsedUnits.Count);

      6:
        if (DelphiFile.InSearchPath) and (Sender.GetNodeLevel(Node) > 0) and (FTreeNodeObjects[GetID(Node)] <> nil) and
          (GetNodeIndex(Node) < FTreeNodeObjects[GetID(Node.Parent)].DelphiFile.UnitInfo.UsedUnits.Count) then
          CellText := UsesTypeStrings[FTreeNodeObjects[GetID(Node.Parent)].DelphiFile.UnitInfo.UsedUnits[GetNodeIndex(Node)
            ].UsesType];

      7:
        CellText := CircularRelationshipTypeDescriptions[FTreeNodeObjects[GetID(Node)].CircularReference];

      8:
        if FTreeNodeObjects[GetID(Node)].Link <> nil then
          CellText := StrYes
        else
          CellText := StrNo;

      9:
        CellText := UnitInfo.Filename;

      10:
        if Assigned(UnitInfo.Module) then
          CellText := UnitInfo.Module.Name;
    end;
  end;
end;

procedure TfrmMain.vtCommonHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
begin
  inherited;

  // Sort the list
  ShowHourGlass;
  try
    if Sender.SortColumn = HitInfo.Column then
    Begin
      if Sender.SortDirection = sdAscending then
        Sender.SortDirection := sdDescending
      else
        Sender.SortDirection := sdAscending;
    end
    else
    begin
      Sender.SortColumn := HitInfo.Column;
      Sender.SortDirection := sdAscending;
    end;

//    Sender.Treeview.Sort(nil, Sender.SortColumn, Sender.SortDirection);

    Sender.Treeview.Invalidate;
  finally
    HideHourGlass;
  end;
end;

procedure TfrmMain.vtUnitsListDblClick(Sender: TObject);
begin
  if TVirtualStringTree(Sender).FocusedNode <> nil then
  begin
    pcView.ActivePage := tabTree;

    vtUnitsTree.SelectNodeEx(FModel.DelphiFileList[GetFocusedID(TVirtualStringTree(Sender))].BaseTreeNode);
    vtUnitsTree.SetFocus;
  end;
end;

procedure TfrmMain.vtUnitsListFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  UpdateListControls(Node);
end;

procedure TfrmMain.UpdateListControls(Node: PVirtualNode);
var
  DelphiFile: TDelphiFile;
begin
  pcList.Visible := vtUnitsList.FocusedNode <> nil;

  if Node <> nil then
  begin
    DelphiFile := FModel.DelphiFileList[GetFocusedID(vtUnitsList)];

    if (DelphiFile.InSearchPath) and (FileExists(DelphiFile.UnitInfo.Filename)) then
    begin
      memListFile.Lines.LoadFromFile(DelphiFile.UnitInfo.Filename);
      memListFile.Modified := FALSE;
    end;

    SearchUnitsListChildList(vtUsedByUnits, edtSearchUsedByList.Text, TRUE);
    SearchUnitsListChildList(vtUsesUnits, edtSearchUsesList.Text, FALSE);

    vtUsedByUnits.AutoFitColumns;
    vtUsesUnits.AutoFitColumns;
  end;
end;

procedure TfrmMain.vtUnitsListFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode;
  OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
begin
  if memListFile.Modified then
    case MessageDlg(Format(StrSHasBeenModifi, [FModel.DelphiFileList[GetFocusedID(vtUnitsList)].UnitInfo.Filename]),
      mtWarning, [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        memListFile.Lines.SaveToFile(FModel.DelphiFileList[GetFocusedID(vtUnitsList)].UnitInfo.Filename);
      mrCancel:
        Allowed := FALSE;
    end;
end;

procedure TfrmMain.vtCommonGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
begin
  if (Kind in [ikNormal, ikSelected]) and (Column = 0) then
  begin
    ImageIndex := 0;
  end;
end;

procedure TfrmMain.vtUnitsListCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex;
  var Result: Integer);
var
  idx1, idx2: Integer;
  DelphiFile1, DelphiFile2: TDelphiFile;
begin
  idx1 := GetID(Node1);
  idx2 := GetID(Node2);

  if (idx1 <> -1) and (idx2 <> -1) then
  begin
    DelphiFile1 := FModel.DelphiFileList[idx1];
    DelphiFile2 := FModel.DelphiFileList[idx2];

    case Column of
      cvtUnitsList_ColumnIdx_Unit:
        Result := CompareStr(DelphiFile1.UnitInfo.DelphiUnitName, DelphiFile2.UnitInfo.DelphiUnitName);

      cvtUnitsList_ColumnIdx_FileType:
        Result := CompareStr(DelphiFileTypeStrings[DelphiFile1.UnitInfo.DelphiFileType],
          DelphiFileTypeStrings[DelphiFile2.UnitInfo.DelphiFileType]);

      cvtUnitsList_ColumnIdx_LoC:
        Result := CompareInteger(DelphiFile1.UnitInfo.LinesOfCode, DelphiFile2.UnitInfo.LinesOfCode);

      cvtUnitsList_ColumnIdx_UsedByUnits_Count:
        Result := CompareInteger(DelphiFile1.UsedCount, DelphiFile2.UsedCount);

      cvtUnitsList_ColumnIdx_UsedUnits_Count:
        Result := CompareInteger(DelphiFile1.UnitInfo.UsedUnits.Count, DelphiFile2.UnitInfo.UsedUnits.Count);

      cvtUnitsList_ColumnIdx_FileName:
        Result := CompareStr(DelphiFile1.UnitInfo.Filename, DelphiFile2.UnitInfo.Filename);

      cvtUnitsList_ColumnIdx_Module:
        Result := CompareTwoModulesTreeNodes(DelphiFile1, DelphiFile2);
    end;
  end;
end;

procedure TfrmMain.vtUnitsListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  UnitInfo: IUnitInfo;
  DelphiFile: TDelphiFile;
begin
  CellText := '-';

  DelphiFile := FModel.DelphiFileList[GetID(Node)];
  UnitInfo   := DelphiFile.UnitInfo;

  case Column of
    cvtUnitsList_ColumnIdx_Unit:
      CellText := UnitInfo.DelphiUnitName;

    cvtUnitsList_ColumnIdx_FileType:
      if DelphiFile.InSearchPath then
        CellText := DelphiFileTypeStrings[UnitInfo.DelphiFileType];

    cvtUnitsList_ColumnIdx_LoC:
      CellText := IntToStr(UnitInfo.LinesOfCode);

    cvtUnitsList_ColumnIdx_UsedByUnits_Count:
      CellText := IntToStr(DelphiFile.UsedCount);

    cvtUnitsList_ColumnIdx_UsedUnits_Count:
      CellText := IntToStr(DelphiFile.UnitInfo.UsedUnits.Count);

    cvtUnitsList_ColumnIdx_FileName:
      CellText := UnitInfo.Filename;

    cvtUnitsList_ColumnIdx_Module:
      if Assigned(UnitInfo.Module) then
        CellText := UnitInfo.Module.Name;
  end;
end;

procedure TfrmMain.vtUnitsListDrawText(Sender: TBaseVirtualTree;
    TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const
    Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  aDelphiFile: TDelphiFile;
begin
  // highlight "unknown modules"
  if Column = cvtUnitsList_ColumnIdx_Module then
    if not Sender.Selected[Node] then
    begin
      aDelphiFile := FModel.DelphiFileList[GetID(Node)];
      if Assigned(aDelphiFile.UnitInfo.Module) and aDelphiFile.UnitInfo.Module.IsUnknownModule then
      begin
        TargetCanvas.Brush.Color := cUnknownModuleBackgroundColor;
        TargetCanvas.Font.Color  := cUnknownModuleForegroundColor;
      end;
    end;
end;

procedure TfrmMain.vtUnitsListPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);
begin
  if (not Sender.Selected[Node]) and (not FModel.DelphiFileList[GetID(Node)].InSearchPath) then
    TargetCanvas.Font.Color := cNotInSearchpathForegroundColor;
end;

procedure TfrmMain.vtUnitsListSearchComparison(Sender: TObject; Node: PVirtualNode; const SearchTerms: TStrings;
  var IsMatch: Boolean);
var
  i: Integer;
begin
  IsMatch := TRUE;

  for i := 0 to pred(SearchTerms.Count) do
    if (pos(LowerCase(SearchTerms[i]), LowerCase(FModel.DelphiFileList[GetID(Node)].UnitInfo.DelphiUnitName)) = 0) or
      ((not actShowUnitsNotInPath.Checked) and (not FModel.DelphiFileList[GetID(Node)].InSearchPath)) then
    begin
      IsMatch := FALSE;

      Break;
    end;
end;

procedure TfrmMain.vtUnitsTreePaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType);
var
  aDelphiFile: TDelphiFile;
begin
  if TextType = ttStatic then
  begin
    if not Sender.Selected[Node] then
      TargetCanvas.Font.Color := clGray;
  end
  else
  begin
    aDelphiFile := FTreeNodeObjects[GetID(Node)].DelphiFile;
    if (Column = 0) and (Sender.FocusedNode = Node) and (UnitsTreeIsSearchHitNode(Node)) then
      TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];

    if not Sender.Selected[Node] then
    begin
      if not aDelphiFile.InSearchPath then
        TargetCanvas.Font.Color := cNotInSearchpathForegroundColor
      else if FTreeNodeObjects[GetID(GetLinkedNode(Node))].DelphiFile.UnitInfo.DelphiFileType = ftUnknown then
        TargetCanvas.Font.Color := clRed;
    end;

    // highlight "linked" nodes
    if Column = 0 then
    begin
      if FTreeNodeObjects[GetID(Node)].Link <> nil then
      begin
        if not Sender.Selected[Node] then
        begin
          if aDelphiFile.InSearchPath then
            TargetCanvas.Font.Color := clBlue
          else
            TargetCanvas.Font.Color := $00FEC9B1;
        end;

        TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsUnderline];
      end;
    end;
  end;
end;

procedure TfrmMain.vtUnitsTreeDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  aDelphiFile: TDelphiFile;
begin
  // highlight "unknown modules"
  if Column = 10 then // Column = 'Modules'
    if not Sender.Selected[Node] then
    begin
      aDelphiFile := FTreeNodeObjects[GetID(Node)].DelphiFile;
      if Assigned(aDelphiFile.UnitInfo.Module) and aDelphiFile.UnitInfo.Module.IsUnknownModule then
      begin
        TargetCanvas.Brush.Color := cUnknownModuleBackgroundColor;
        TargetCanvas.Font.Color  := cUnknownModuleForegroundColor;
      end;
    end;
end;

procedure TfrmMain.actCloseProjectExecute(Sender: TObject);
begin
  if (CheckNotRunning) and (CheckSaveProject) then
  begin
    fProjectFilename := '';

    ClearGUIAndModelAndCloseControls;
  end;
end;

procedure TfrmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.actExpandExecute(Sender: TObject);
begin
  vtUnitsTree.ExpandAll(vtUnitsTree.FocusedNode);
end;

procedure TfrmMain.actExpandAllExecute(Sender: TObject);
begin
  ExpandAll;
end;

procedure TfrmMain.ExpandAll;
var
  OldCursor: TCursor;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    vtUnitsTree.ExpandAll(nil);
//    vtUnitsTree.AutoFitColumns(False, smaUseColumnOption, 2, 10);
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TfrmMain.actCollapseAllExecute(Sender: TObject);
var
  OldCursor: TCursor;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    vtUnitsTree.CollapseAll(nil);
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TfrmMain.actCollapseExecute(Sender: TObject);
begin
  vtUnitsTree.Expanded[vtUnitsTree.FocusedNode] := FALSE;
end;

procedure TfrmMain.ActionManager1Update(Action: TBasicAction; var Handled: Boolean);
begin
  UpdateControls;

  Handled := TRUE;
end;

procedure TfrmMain.actSaveCircularRefsExecute(Sender: TObject);
var
  Node: PVirtualNode;
  LStrs: TStringList;
begin
  vtUnitsTree.BeginUpdate;
  LStrs := TStringList.Create;
  try
    LStrs.Add('Kind;Unit;Ref');
    Node := vtUnitsTree.GetFirst;

    while Node <> nil do
    begin
      if FTreeNodeObjects[GetID(Node)].DelphiFile.InSearchPath then
      begin
        if FTreeNodeObjects[GetID(Node)].CircularReference in [crSemiCircular, crCircular] then
        begin
          LStrs.Add(CircularRelationshipTypeDescriptions[FTreeNodeObjects[GetID(Node)].CircularReference] + ';' +
            FTreeNodeObjects[GetID(Node.Parent)].DelphiFile.UnitInfo.DelphiUnitName + ';' + FTreeNodeObjects[GetID(Node)
            ].DelphiFile.UnitInfo.DelphiUnitName);
        end;
      end;

      Node := vtUnitsTree.GetNext(Node);
    end;

    if vtUnitsTree.FocusedNode <> nil then
      vtUnitsTree.ScrollIntoView(vtUnitsTree.FocusedNode, TRUE);
  finally
    if saveDialog_Units_GephiCSV.Execute then
      LStrs.SaveToFile(saveDialog_Units_GephiCSV.Filename);
    LStrs.Free;
    vtUnitsTree.EndUpdate;
  end;
end;

procedure TfrmMain.UpdateControls;
var
  DelphiFile: TDelphiFile;
begin
  DelphiFile := GetFocusedDelphiFile;

  actShowUnitsNotInPath.Enabled     := (not FBusy) and (not actShowOnlyUnknownModules.Checked);
  actShowOnlyUnknownModules.Enabled := not FBusy;

  actSaveChanges.Enabled            := (not FBusy) and ((memParentFile.Modified) or (memSelectedFile.Modified) or (memListFile.Modified));

  actRename.Enabled                 := (not FBusy) and (DelphiFile <> nil) and (DelphiFile.InSearchPath);

  actApplyRenameList.Enabled        := actRename.Enabled;
  actAddUnitToUses.Enabled          := actRename.Enabled;

  actSearchAndReplace.Enabled       := (not FBusy) and (vtUnitsTree.RootNodeCount > 0);

  actExpandAll.Enabled              := (not FBusy) and (pcView.ActivePage = tabTree) and (vtUnitsTree.RootNodeCount > 0);
  actCollapseAll.Enabled            := (not FBusy) and (pcView.ActivePage = tabTree) and (vtUnitsTree.RootNodeCount > 0);
  actExpand.Enabled                 := (not FBusy) and (vtUnitsTree.FocusedNode <> nil);
  actCollapse.Enabled               := (not FBusy) and (vtUnitsTree.FocusedNode <> nil);
  actLoadProject.Enabled            := not FBusy;
  actSaveProject.Enabled            := (not FBusy) and (fProjectFilename <> '');
  actSaveProjectAs.Enabled          := not FBusy;
  actSettings.Enabled               := not FBusy;
  actCloseProject.Enabled           := (pnlMain.Visible) or (fProjectFilename <> '');
  actShowFile.Enabled               := DelphiFile <> nil;
  actSaveToXML.Enabled              := not FBusy;
  actSaveToGephiCSV.Enabled         := not FBusy;
  actExportUnitsToGraphML.Enabled              := not FBusy;
  actExportUnitsToGraphMLCurrentModule.Enabled := not FBusy and (vtModules.FocusedNode <> nil);
  actSaveCircularRefs.Enabled       := not FBusy;
end;

procedure TfrmMain.actLoadProjectExecute(Sender: TObject);
begin
  if openDialog_Project.Execute and CheckSaveProject then
  begin
    if LoadProjectSettings(openDialog_Project.Filename, False) then
    begin
      fProjectFilename := openDialog_Project.Filename;

      SetFormCaption;
    end;
  end;
end;

procedure TfrmMain.actNewProjectExecute(Sender: TObject);
begin
  if CheckNotRunning and CheckSaveProject then
  begin
    fProjectFilename := '';

    ClearGUIAndModelAndCloseControls;

    ResetSettings;

    actSettings.Execute;
  end;
end;

procedure TfrmMain.ResetSettings;
begin
  fProjectFilename := '';
  FProjectSettings.Clear;
end;

function TfrmMain.CheckNotRunning: Boolean;
begin
  Result := not FBusy;

  if not Result then
    MessageDlg(StrPleaseStopTheCurr, mtInformation, [mbOK], 0);
end;

procedure TfrmMain.UpdateLogEntries;
begin
  vtLog.RootNodeCount := TDudsLogger.GetInstance.Count;
  vtLog.Invalidate;

  if not vtLog.Focused then
    vtLog.ScrollIntoView(vtLog.GetLast, FALSE);
end;

procedure TfrmMain.ClearGUIAndModelAndCloseControls;
begin
  vtUnitsTree.Clear;
  vtUnitsList.Clear;
  vtUsedByUnits.Clear;
  vtUsesUnits.Clear;
  vtStats.Clear;

//  vtUnitsTree.Header.Columns[1].Options := vtUnitsTree.Header.Columns[1].Options - [coVisible];

  FModel.Clear;
  FStats.Clear;
  TDudsLogger.GetInstance.Clear;
  UpdateLogEntries;

  pnlMain.Visible := FALSE;

  SetFormCaption;
end;

procedure TfrmMain.SetFormCaption;
begin
  Caption := StrUnitDependencyScan;

  if fProjectFilename <> '' then
    Caption := Caption + ' - ' + ExtractFilenameNoExt(fProjectFilename);

  if Modified then
    Caption := Caption + '*';
end;

procedure TfrmMain.SetModified(const Value: Boolean);
begin
  FModified := Value;

  SetFormCaption;
end;

procedure TfrmMain.actRenameExecute(Sender: TObject);
begin
  RenameDelphiFileWithDialog(rtRename);
end;

procedure TfrmMain.actSearchAndReplaceExecute(Sender: TObject);
begin
  RenameDelphiFileWithDialog(rtSearchAndReplace);
end;

procedure TfrmMain.actApplyRenameListExecute(Sender: TObject);
var
  aFileName: String;
  ShowDlgResult: Boolean;
begin
  if (OpenDialogMultipleRenames.Execute) then
  begin
    aFileName := OpenDialogMultipleRenames.Filename;

    with TfrmRenameUnitName.Create(Self) do
      try
        Width := Width + 300;
        Label1.Caption := 'Selected file';
        edtNewName.Text := aFileName;
        edtNewName.Enabled := FALSE;
        Caption := 'Rename multiple files';
        infoRenameCSV.Visible := TRUE;

        chkDummyRun.Checked := FEnvironmentSettings.DummyRun;
        chkRenameHistoryFiles.Checked := FEnvironmentSettings.RenameHistoryFiles;
        chkInsertOldNameComment.Checked := FEnvironmentSettings.RenameInsertOldNameComment;
        chkInsertOldNameComment.Caption := 'Insert "{renamed from ' + 'oldname' + '.pas} comment"';
        chkRenameLowerCaseExtension.Checked := FEnvironmentSettings.RenameLowerCaseExtension;

        ShowDlgResult := ShowModal = mrOK;

        if ShowDlgResult then
        begin

          FEnvironmentSettings.DummyRun := chkDummyRun.Checked;
          FEnvironmentSettings.RenameHistoryFiles := chkRenameHistoryFiles.Checked;
          FEnvironmentSettings.RenameInsertOldNameComment := chkInsertOldNameComment.Checked;
          FEnvironmentSettings.RenameLowerCaseExtension := chkRenameLowerCaseExtension.Checked;

          ApplyMultipleRenames(aFileName, chkDummyRun.Checked, chkRenameHistoryFiles.Checked,
            chkInsertOldNameComment.Checked, chkRenameLowerCaseExtension.Checked);
        end;
      finally
        Release;
      end;
  end;
end;

procedure TfrmMain.actAddUnitToUsesExecute(Sender: TObject);
var
  ShowDlgResult: Boolean;
  AddUnitToUsesRefactoring: TAddUnitToUsesRefactoring;
begin
  if GetFocusedDelphiFile <> nil then
  begin
    with TfrmAddNewUnit.Create(Self) do
      try
        edtNewName.Text := '';
        edtNewName.Enabled := TRUE;
        Label1.Caption := 'Add uses to all units that currently use "' +
          GetFocusedDelphiFile.UnitInfo.DelphiUnitName + '"';
        Caption := Label1.Caption;

        ShowDlgResult := ShowModal = mrOK;

        if ShowDlgResult then
        begin
          ClearLog();
          try
            AddUnitToUsesRefactoring := TAddUnitToUsesRefactoring.Create;
            try
              AddUnitToUsesRefactoring.Model := FModel;
              AddUnitToUsesRefactoring.AddUnitToUses(chkDummyRun.Checked, GetFocusedDelphiFile, edtNewName.Text, mem_OnlyApplyToUnits.Lines);

              // the refactoring changed the model, so we need to rebuild the gui from the model
              FillGUIFromModel;
            finally
              FreeAndNil(AddUnitToUsesRefactoring);
            end;
          finally
            UpdateLogEntries;
          end;
        end;

      finally
        Free;
      end;
  end;
end;

procedure TfrmMain.actFormatUsesOfFileExecute(Sender: TObject);
var
  aFormatUsesRefactoring : TFormatUsesRefactoring;
begin
  if GetFocusedDelphiFile <> nil then
  begin
      aFormatUsesRefactoring := TFormatUsesRefactoring.Create;
      try
        aFormatUsesRefactoring.Model    := FModel;
        aFormatUsesRefactoring.OnLog    := Self.Log;
        aFormatUsesRefactoring.DummyRun := false;
        aFormatUsesRefactoring.ProjectSettings := FProjectSettings;
        aFormatUsesRefactoring.AllowModuleGroupingWithUnknownModules := true;

        aFormatUsesRefactoring.FormatUsesInFile(GetFocusedDelphiFile.UnitInfo.DelphiUnitName);

      finally
        FreeAndNil(aFormatUsesRefactoring);
      end;
  end;
end;

procedure TfrmMain.actRemoveUnusedUnitsProcessPalOutputExecute(Sender: TObject);
begin
  try
    TPAUsesReportProcessor.ParseOutputFileAndWriteFiltered('./Uses.txt', './Unused.txt', './IgnoreUnits.txt');
  except
    on e: Exception do
      Log(e.Message, LogError);
  end;
end;

procedure TfrmMain.actRemoveUnUsedUnitsExecute(Sender: TObject);
var
  RemoveUnusedUnitsRefactoring: TRemoveUnusedUnitsRefactoring;
begin
  try
    RemoveUnusedUnitsRefactoring := TRemoveUnusedUnitsRefactoring.Create;
    try
      RemoveUnusedUnitsRefactoring.Model := FModel;
      RemoveUnusedUnitsRefactoring.OnLog := Self.Log;
      RemoveUnusedUnitsRefactoring.DeleteAllUnitsDefinedInUnusedFile('.\Unused.txt');
    finally
      FreeAndNil(RemoveUnusedUnitsRefactoring);
    end;
  except
    on e: Exception do
      Log(e.Message, LogError);
  end;
end;

function TfrmMain.GetFocusedDelphiFile: TDelphiFile;
begin
  if (vtUnitsTree.Focused) and (vtUnitsTree.FocusedNode <> nil) then
    Result := FTreeNodeObjects[GetID(vtUnitsTree.FocusedNode)].DelphiFile
  else

    if (vtUnitsList.Focused) and (vtUnitsList.FocusedNode <> nil) then
    Result := FModel.DelphiFileList[GetFocusedID(vtUnitsList)]
  else

    if (vtUsedByUnits.Focused) and (vtUsedByUnits.FocusedNode <> nil) then
    Result := FModel.DelphiFileList[GetFocusedID(vtUsedByUnits)]
  else

    if (vtUsesUnits.Focused) and (vtUsesUnits.FocusedNode <> nil) then
    Result := FModel.DelphiFileList[GetFocusedID(vtUsesUnits)]
  else
    Result := nil;
end;

function TfrmMain.RenameDelphiFileWithDialog(RenameType: TRenameType): Boolean;
begin
  Result := RenameDelphiFileWithDialog(GetFocusedDelphiFile, RenameType);
end;

function TfrmMain.RenameDelphiFileWithDialog(const DelphiFile: TDelphiFile; RenameType: TRenameType): Boolean;
begin
  Result := FALSE;

  case RenameType of
    rtRename:
      begin
        if DelphiFile <> nil then
        begin
          With TfrmRenameUnitName.Create(Self) do
            try
              edtNewName.Text := DelphiFile.UnitInfo.DelphiUnitName;
              Caption := Format(StrRenameS, [edtNewName.Text]);

              chkDummyRun.Checked := FEnvironmentSettings.DummyRun;
              chkRenameHistoryFiles.Checked := FEnvironmentSettings.RenameHistoryFiles;
              chkInsertOldNameComment.Checked := FEnvironmentSettings.RenameInsertOldNameComment;
              chkInsertOldNameComment.Caption := 'Insert "{renamed from ' + DelphiFile.UnitInfo.DelphiUnitName +
                '.pas} comment"';
              chkRenameLowerCaseExtension.Checked := FEnvironmentSettings.RenameLowerCaseExtension;

              Result := ShowModal = mrOK;

              if Result then
              begin
                FEnvironmentSettings.DummyRun := chkDummyRun.Checked;
                FEnvironmentSettings.RenameHistoryFiles := chkRenameHistoryFiles.Checked;
                FEnvironmentSettings.RenameInsertOldNameComment := chkInsertOldNameComment.Checked;
                FEnvironmentSettings.RenameLowerCaseExtension := chkRenameLowerCaseExtension.Checked;

                RenameDelphiFile(TRUE, DelphiFile.UnitInfo.DelphiUnitName, edtNewName.Text,
                  chkDummyRun.Checked, chkRenameHistoryFiles.Checked, TRUE,
                  chkInsertOldNameComment.Checked, chkRenameLowerCaseExtension.Checked);
              end;
            finally
              Release;
            end;
        end;
      end;

    rtSearchAndReplace:
      begin
        With TfrmSearchAndReplaceUnitName.Create(Self) do
          try
            if DelphiFile <> nil then
            begin
              edtSearch.Text := DelphiFile.UnitInfo.DelphiUnitName;
              edtReplace.Text := DelphiFile.UnitInfo.DelphiUnitName;
              edtTest.Text := DelphiFile.UnitInfo.DelphiUnitName;
            end;

            chkDummyRun.Checked := FEnvironmentSettings.DummyRun;
            chkRenameHistoryFiles.Checked := FEnvironmentSettings.RenameHistoryFiles;

            Result := ShowModal = mrOK;

            if Result then
            begin
              FEnvironmentSettings.DummyRun := chkDummyRun.Checked;
              FEnvironmentSettings.RenameHistoryFiles := chkRenameHistoryFiles.Checked;

              RenameDelphiFile(TRUE, edtSearch.Text, edtReplace.Text,
                chkDummyRun.Checked, chkRenameHistoryFiles.Checked, FALSE, FALSE, FALSE);
            end;
          finally
            Release;
          end;
      end;
  end;

  if Result then
//    vtUnitsTree.Header.Columns[1].Options := vtUnitsTree.Header.Columns[1].Options + [coVisible];
end;

procedure TfrmMain.ApplyMultipleRenames(aCsvFilename: String; DummyRun, RenameHistoryFiles, InsertOldNameComment,
  RenameLowerCaseExtension: Boolean);
var
  aRenameFileStrings: TStringList;
  i: Integer;
  semicolonPos: Integer;
  aLine: String;
  OldUnitName, NewUnitName: String;
  renameCount: Integer;
  OldFile, NewFile: TDelphiFile;
  PreCheckOk: Boolean;
  aRenameUnitsList: TDictionary<String, String>;
begin

  aRenameUnitsList := TDictionary<String, String>.Create;
  try
    if FileExists(aCsvFilename) then
    begin
      ClearLog;

      PreCheckOk := TRUE;

      aRenameFileStrings := TStringList.Create;
      try
        aRenameFileStrings.LoadFromFile(aCsvFilename);
        for i := 0 to pred(aRenameFileStrings.Count) do
        begin
          aLine := aRenameFileStrings[i];
          semicolonPos := pos(';', aLine);
          OldUnitName := Copy(aLine, 1, semicolonPos - 1);
          NewUnitName := Copy(aLine, semicolonPos + 1, Length(aLine));

          // "old name" Unit exists?
          OldFile := FModel.FindParsedDelphiUnit(OldUnitName, nil);
          if not Assigned(OldFile) then
          begin
            Log(StrUnableToRenameS, [OldUnitName], LogError);
            PreCheckOk := FALSE;
          end;

          // "new name" unit does NOT exist already?
          NewFile := FModel.FindParsedDelphiUnit(NewUnitName, nil);
          if Assigned(NewFile) then
          begin
            Log(StrUnableToRenameToNewName, [OldUnitName, NewUnitName], LogError);
            PreCheckOk := FALSE;
          end;

          aRenameUnitsList.Add(OldUnitName, NewUnitName);
        end;
      finally
        FreeAndNil(aRenameFileStrings);
      end;

      renameCount := 0;
      if PreCheckOk then
      begin
        for OldUnitName in aRenameUnitsList.Keys do
        begin
          NewUnitName := aRenameUnitsList[OldUnitName];

          Log(StrStartBatchRename, [OldUnitName, NewUnitName], LogInfo);
          Application.ProcessMessages;

          RenameDelphiFile(FALSE, OldUnitName, NewUnitName, DummyRun, RenameHistoryFiles, TRUE,
            InsertOldNameComment, RenameLowerCaseExtension);
          Inc(renameCount);
        end;
      end;

      Log(StrBatchRenameResult, [renameCount], LogInfo);
    end
    else
      Log(StrUnableToFindFile, [aCsvFilename], LogWarning);
  finally
    FreeAndNil(aRenameUnitsList);
  end;
end;

procedure TfrmMain.RenameDelphiFile(const aClearLog: Boolean; const SearchString, ReplaceString: String;
  const DummyRun, RenameHistoryFiles, ExactMatch, InsertOldNameComment,
  LowerCaseExtension: Boolean);
var
  RenameRefactoring: TDudsRenameRefacotring;
begin
  if aClearLog then
    ClearLog();
  try
    RenameRefactoring := TDudsRenameRefacotring.Create;
    try
      RenameRefactoring.Model := FModel;
      RenameRefactoring.RenameDelphiFile(
        SearchString, ReplaceString,
        DummyRun, RenameHistoryFiles, ExactMatch,
        InsertOldNameComment, LowerCaseExtension);
    finally
      FreeAndNil(RenameRefactoring);
    end;
  finally
    UpdateLogEntries;
    UpdateTreeControls(vtUnitsTree.FocusedNode);
    UpdateListControls(vtUnitsList.FocusedNode);

    SearchUnitsList(vtUnitsList, edtSearchList.Text);

    vtUsedByUnits.Invalidate;
  end;
end;

procedure TfrmMain.ExportToXML(const VT: TVirtualStringTree; const Filename: String);
var
  XMLDoc: TXMLDocument;
  Count: Integer;

  procedure ProcessTreeItem(const ParentNode: PVirtualNode; const ParentXMLNode: IXMLNode);
  var
    Node: PVirtualNode;
    XMLNode: IXMLNode;
    NodeObject: TNodeObject;
    DelphiFile: TDelphiFile;
  begin
    if ParentNode <> nil then
    begin
      Node := ParentNode;

      while Node <> nil do
      begin
        if VT.IsVisible[Node] then
        begin
          DelphiFile := nil;

          if VT = vtUnitsList then
          begin
            DelphiFile := FModel.DelphiFileList[GetID(Node)];

            NodeObject := nil;
          end
          else
          begin
            NodeObject := FTreeNodeObjects[GetID(Node)];

            if NodeObject <> nil then
              DelphiFile := NodeObject.DelphiFile;
          end;

          if DelphiFile <> nil then
          begin
            XMLNode := ParentXMLNode.AddChild('Unit');
            XMLNode.Attributes['Name'] := DelphiFile.UnitInfo.DelphiUnitName;
            XMLNode.Attributes['UsedCount'] := DelphiFile.UsedCount.ToString;
            XMLNode.Attributes['InSearchPath'] := DelphiFile.InSearchPath;
            XMLNode.Attributes['LineCount'] := DelphiFile.UnitInfo.LinesOfCode;
            XMLNode.Attributes['UnitNameCharPosition'] := DelphiFile.UnitInfo.DelphiUnitNamePosition;
            XMLNode.Attributes['FileType'] := DelphiFileTypeStrings[DelphiFile.UnitInfo.DelphiFileType];

            if NodeObject <> nil then
            begin
              XMLNode.Attributes['IsLink'] := NodeObject.Link <> nil;
              XMLNode.Attributes['CircularReferenceType'] := CircularRelationshipTypeDescriptions
                [NodeObject.CircularReference];
            end;

            XMLNode.Attributes['Filename'] := DelphiFile.UnitInfo.Filename;

            Inc(Count);
          end;

          if Node.FirstChild <> nil then
            ProcessTreeItem(Node.FirstChild, XMLNode);
        end;

        Node := Node.NextSibling;
      end;
    end;
  end;

begin
  ShowHourGlass;
  try
    Count := 0;

    XMLDoc := TXMLDocument.Create(nil);

    XMLDoc.Active := TRUE;

    ProcessTreeItem(VT.GetFirst, XMLDoc.AddChild('DUDS'));

    XMLDoc.SaveToFile(Filename);
  finally
    HideHourGlass;
  end;
end;

procedure TfrmMain.FillGUIFromModel;

  function SearchForCircularRelationshipByWalkingUpParents(TreeNode: PVirtualNode; UsageType: TUsedUnitType; const DelphiUnitName: String): TCircularRelationshipType;
  var
    i: Integer;
  begin
    Result := crNone;

    TreeNode := TreeNode.Parent;

    if (TreeNode <> nil) and (TreeNode <> vtUnitsTree.RootNode) then
    begin
      while (TreeNode <> nil) and (TreeNode <> vtUnitsTree.RootNode) do
      begin
        if SameText(FTreeNodeObjects[GetID(TreeNode)].DelphiFile.UnitInfo.DelphiUnitName, DelphiUnitName) then
        begin
          if UsageType = utInterface then
            Result := crCircular
          else
            Result := crSemiCircular;

          Break;
        end
        else
          TreeNode := TreeNode.Parent;
      end;
    end;

  end;

  procedure MarkAllParentsContainingUnknownModule(node: PVirtualNode);
  var
    ParentStepNode: PVirtualNode;
  begin
    ParentStepNode := Node.Parent;
    while ParentStepNode <> vtUnitsTree.RootNode do
    begin
      if FTreeNodeObjects[GetID(ParentStepNode)].UnknownModuleInChildren then
        Break
      else
        FTreeNodeObjects[GetID(ParentStepNode)].UnknownModuleInChildren := True;

      ParentStepNode := ParentStepNode.Parent;
    end;
  end;

  procedure AddTreeNode(Parent: PVirtualNode; UsageType: TUsedUnitType; UnitOrProjectFileName: string);
  var
    DelphiFile: TDelphiFile;
    NodeObject: TNodeObject;
    NodeObjectID: Cardinal;
    TreeNode: PVirtualNode;
    UsedUnitInfo: IUsedUnitInfo;
    UnitWasAddedForTheFirstTime: Boolean;
  begin
    DelphiFile := FModel.FindParsedDelphiUnit(UnitOrProjectFileName, FProjectSettings.UnitScopeNames);
    if Assigned(DelphiFile) then
    begin
      NodeObject              := TNodeObject.Create;
      NodeObject.DelphiFile   := DelphiFile;

      NodeObjectID := FTreeNodeObjects.Add(NodeObject);
      TreeNode     := vtUnitsTree.AddChild(Parent);
      SetID(TreeNode, NodeObjectID);

      // the first time we add a tree node for a unit, we remember that node to be the "base node"
      UnitWasAddedForTheFirstTime := not Assigned(DelphiFile.BaseTreeNode);
      if UnitWasAddedForTheFirstTime then
         DelphiFile.BaseTreeNode := TreeNode;

      // update the "UnknownModuleInChildren" information for all parents
      if Assigned(DelphiFile.UnitInfo.Module) and DelphiFile.UnitInfo.Module.IsUnknownModule then
        MarkAllParentsContainingUnknownModule(TreeNode);

      // Calc Circular Ref by walking up the tree
      if Parent = nil then
         NodeObject.CircularReference := crNone
      else
         NodeObject.CircularReference := SearchForCircularRelationshipByWalkingUpParents(TreeNode, UsageType, DelphiFile.UnitInfo.DelphiUnitName);

      case NodeObject.CircularReference of
        crSemiCircular:
          Inc(fModel.Stats.Units.SemiCircularFiles);
        crCircular:
          Inc(fModel.Stats.Units.CircularFiles);
      end;

      // Recursively add used units as tree nodes
      // -> But add children of a unit only once in the tree. If the unit was already
      //    added, provide a "jump-link" to the first occurence. This keeps the tree smaller.
      if UnitWasAddedForTheFirstTime then
      begin
        if NodeObject.CircularReference = crNone then
          for UsedUnitInfo in DelphiFile.UnitInfo.UsedUnits do
            AddTreeNode(TreeNode, UsedUnitInfo.UsesType, UsedUnitInfo.DelphiUnitName)
      end
      else
        // when the unit occurs for the second, third, ... time, just link back to the "base node"
        if FProjectSettings.LinkUnits then
          NodeObject.Link := DelphiFile.BaseTreeNode;

      SetNodeVisibility(vtUnitsTree, TreeNode, DelphiFile);
    end;
  end;

  procedure FillTreeAndCalcCircularReferences;
  var
    DelphiFile, RootFile: TDelphiFile;
  begin
    // Reset stats
    fModel.Stats.Units.SemiCircularFiles := 0;
    fModel.Stats.Units.CircularFiles     := 0;

    // Reset "Base Nodes" (to be able to re-run this method without re-building the model
    for DelphiFile in FModel.DelphiFileList do
      DelphiFile.BaseTreeNode := nil;

    // Add root nodes to the tree and then recursively...
    for RootFile in FModel.ParsedDelphiRootFiles do
      AddTreeNode(nil, utInterface, RootFile.UnitInfo.DelphiUnitName);
  end;

  procedure FillLists;
  var
    Node: PVirtualNode;
    i: Integer;
  begin
    for i := 0 to pred(FModel.DelphiFileList.Count) do
    begin

      Node := vtUnitsList.AddChild(nil);
      SetID(Node, i);

      SetNodeVisibility(vtUnitsList, Node, FModel.DelphiFileList[i]);

      Node := vtUsedByUnits.AddChild(nil);
      SetID(Node, i);

      Node := vtUsesUnits.AddChild(nil);
      SetID(Node, i);
    end;

  end;

  procedure FillModules;
  var
    Node: PVirtualNode;
    module: TModule;
  begin
    for module in fModel.Modules.OrderedModules do
    begin
      Node := vtModules.AddChild(nil);
      SetID(Node, module.ID);
    end;
  end;

begin
  vtUnitsTree.Clear;
  vtUnitsList.Clear;
  vtUsedByUnits.Clear;
  vtUsesUnits.Clear;
  vtModules.Clear;
  memParentFile.Clear;
  memSelectedFile.Clear;

  Log(StrTransferingToGUI, LogInfo);

  vtUnitsTree.BeginUpdate;
  vtUnitsList.BeginUpdate;
  vtUsedByUnits.BeginUpdate;
  vtUsesUnits.BeginUpdate;
  vtModules.BeginUpdate;
  try
    FillTreeAndCalcCircularReferences;
    FillLists;
    FillModules;

    if vtUnitsTree.FocusedNode = nil then
      vtUnitsTree.SelectNodeEx(vtUnitsTree.GetFirst);

    if vtUnitsList.FocusedNode = nil then
      vtUnitsList.SelectNodeEx(vtUnitsList.GetFirst);

    UpdateStats;
  finally
//    vtUnitsTree.AutoFitColumns(False, smaUseColumnOption, 2, 10);
//    vtUnitsList.AutoFitColumns(False, smaUseColumnOption, 1, 6);
    vtModules.AutoFitColumns;

    vtUnitsTree.EndUpdate;
    vtUnitsList.EndUpdate;
    vtUsedByUnits.EndUpdate;
    vtUsesUnits.EndUpdate;
    vtModules.EndUpdate;

    UpdateListControls(vtUnitsList.FocusedNode);
  end;
end;

procedure TfrmMain.actStartScanExecute(Sender: TObject);
begin
  if FProjectSettings.RootFiles.Count = 0 then
  begin
    if MessageDlg(StrYouNeedToAddAtLeastOne, mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      actSettings.Execute
  end
  else
  begin
    ClearGUIAndModelAndCloseControls;
    FStartTime := now;
    fCancelled := False;

    actStartScan.Visible := FALSE;
    actStopScan.Visible := TRUE;
    actStopScan.Enabled := TRUE;

    FBusy := TRUE;
    try
      UpdateControls;
      ShowHideControls;
      pnlMain.Visible := TRUE;

      RichEditUnitPath.Clear;
      RichEditUnitPath.SelText := 'Loading...';

      Refresh;

      // step 1: load modules, scan files, analyze dependencies etc. (non-visual part)
      fAnalyzerFacade := TAnalyzerFacade.Create;
      try
        fAnalyzerFacade.Model           := fModel;
        fAnalyzerFacade.OnLog           := Self.Log;
        fAnalyzerFacade.ProjectSettings := fProjectSettings;
        fAnalyzerFacade.ProjectFilename := fProjectFilename;

        fAnalyzerFacade.Execute;
      finally
        FreeAndNil(fAnalyzerFacade);
      end;

      // step 2: fill gui trees & stats
      if not fCancelled then
        FillGUIFromModel;
    finally
      FBusy := FALSE;

      actStartScan.Visible := TRUE;
      actStopScan.Visible := FALSE;

      ShowHideControls;
      UpdateLogEntries;

      UpdateTreeControls(vtUnitsTree.FocusedNode);
    end;
  end;
end;

procedure TfrmMain.ShowHideControls;
begin
  pcView.Visible := not FBusy;
  Splitter1.Visible := not FBusy;

  if FBusy then
  begin
    FpnlLogHeight := pnlLog.Height;
    pnlLog.Align := alClient;
  end
  else
  begin
    pnlLog.Align := alBottom;
    pnlLog.Height := FpnlLogHeight;
    Splitter1.Top := pnlLog.Top - 1;
  end;
end;

procedure TfrmMain.actStopScanExecute(Sender: TObject);
begin
  fCancelled := True;
  if Assigned(fAnalyzerFacade) then
     fAnalyzerFacade.Cancelled := fCancelled;

  actStopScan.Enabled := FALSE;
  Log('Progress cancelled by user', LogError);
end;

procedure TfrmMain.actSaveChangesExecute(Sender: TObject);
begin
  if memParentFile.Modified then
    memParentFile.Lines.SaveToFile(FTreeNodeObjects[GetID(vtUnitsTree.FocusedNode.Parent)].DelphiFile.UnitInfo.Filename);

  if memSelectedFile.Modified then
    memSelectedFile.Lines.SaveToFile(FTreeNodeObjects[GetID(vtUnitsTree.FocusedNode.Parent)].DelphiFile.UnitInfo.Filename);

  if memListFile.Modified then
    memListFile.Lines.SaveToFile(FModel.DelphiFileList[GetFocusedID(vtUnitsList)].UnitInfo.Filename);

  memParentFile.Modified := FALSE;
  memSelectedFile.Modified := FALSE;
  memListFile.Modified := FALSE;
end;

procedure TfrmMain.actSaveProjectAsExecute(Sender: TObject);
begin
  SaveProjectAs;
end;

procedure TfrmMain.actSaveProjectExecute(Sender: TObject);
begin
  SaveProject;
end;

procedure TfrmMain.actSaveToGephiCSVExecute(Sender: TObject);
begin
  saveDialog_Units_GephiCSV.FileName := ChangeFileExt(fProjectFilename, '.units.gephi.csv');

  if saveDialog_Units_GephiCSV.Execute then
    ExportToGephi(FModel, actShowUnitsNotInPath.Checked, saveDialog_Units_GephiCSV.Filename);
end;

procedure TfrmMain.actExportUnitsToGraphMLExecute(Sender: TObject);
begin
  saveDialog_Units_GraphML.FileName := ChangeFileExt(fProjectFilename, '.units.graphml');

  if saveDialog_Units_GraphML.Execute then
    ExportUnitsToGraphML(FModel, actShowUnitsNotInPath.Checked, saveDialog_Units_GraphML.Filename);
end;

procedure TfrmMain.actExportUnitsToGraphMLCurrentModuleExecute(Sender: TObject);

  function CharOnly(aChars, aValue: string): string;
  var
    i: LongInt;
  begin
    Result := '';
    for i  := 1 to Length(aValue) do
      if Pos(aValue[i], aChars) > 0 then
        Result := Result + aValue[i];
  end;

var
  aModule: TModule;
  aModuleNameForFileName: string;
begin
  if vtModules.FocusedNode <> nil then
  begin
    aModule := GetModuleFromModuleNode(vtModules.FocusedNode);

    aModuleNameForFileName := CharOnly('abcdefghijklmnopqrstuvwxyz_.1234567890', aModule.Name.ToLower);

    saveDialog_Units_GraphML.FileName := ChangeFileExt(fProjectFilename, aModuleNameForFileName + '.units.graphml');

    if saveDialog_Units_GraphML.Execute then
      ExportUnitsToGraphML(FModel, actShowUnitsNotInPath.Checked, saveDialog_Units_GraphML.Filename, aModule);
  end;
end;

procedure TfrmMain.actSaveToXMLExecute(Sender: TObject);
begin
  saveDialog_Units_XML.FileName := ChangeFileExt(fProjectFilename, '.units.xml');

  if saveDialog_Units_XML.Execute then
  begin
    if pcView.ActivePage = tabTree then
      ExportToXML(vtUnitsTree, saveDialog_Units_XML.Filename)
    else
      ExportToXML(vtUnitsList, saveDialog_Units_XML.Filename)
  end;
end;

procedure TfrmMain.actExportModulesToCSVExecute(Sender: TObject);
var
  aExportModulesToCSV: TExportModulesToCSV;
begin
  saveDialog_Modules_CSV.FileName := ChangeFileExt(fProjectFilename, '.modules.csv');

  if saveDialog_Modules_CSV.Execute then
  begin
    aExportModulesToCSV := TExportModulesToCSV.Create;
    try
      aExportModulesToCSV.Model           := FModel;
      aExportModulesToCSV.OnLog           := Self.Log;;
      aExportModulesToCSV.ProjectSettings := FProjectSettings;
      aExportModulesToCSV.FileName        := saveDialog_Modules_CSV.FileName;

      aExportModulesToCSV.ExportModules;
    finally
      aExportModulesToCSV.Free;
    end;
  end;
end;

procedure TfrmMain.actExportModulesToGraphMLExecute(Sender: TObject);
var
  aExportModulesToGraphML: TExportModulesToGraphML;
begin
  saveDialog_Modules_GraphML.FileName := ChangeFileExt(fProjectFilename, '.modules.graphml');

  if saveDialog_Modules_GraphML.Execute then
  begin
    aExportModulesToGraphML := TExportModulesToGraphML.Create;
    try
      aExportModulesToGraphML.Model           := FModel;
      aExportModulesToGraphML.OnLog           := Self.Log;;
      aExportModulesToGraphML.ProjectSettings := FProjectSettings;
      aExportModulesToGraphML.FileName        := saveDialog_Modules_GraphML.FileName;
      aExportModulesToGraphML.OriginsToExport := [moUndefined, mo3rdParty, moOwn]; // TODO: This should be a user setting

      aExportModulesToGraphML.ExportModules;
    finally
      aExportModulesToGraphML.Free;
    end;
  end;
end;

function TfrmMain.SaveProject: Boolean;
begin
  if fProjectFilename = '' then
    Result := SaveProjectAs
  else
  begin
    SaveProjectSettings(fProjectFilename);
    Result := TRUE;
  end;
end;

function TfrmMain.SaveProjectAs: Boolean;
begin
  Result := saveDialog_Project.Execute;

  if Result then
  begin
    fProjectFilename := saveDialog_Project.Filename;
    SaveProjectSettings(fProjectFilename);
  end;
end;

procedure TfrmMain.actSettingsExecute(Sender: TObject);
begin
  With TfrmSettings.Create(Self) do
    try
      SaveProjectSettings;

      if Execute(FEnvironmentSettings, FProjectSettings) then
      begin
        LoadProjectSettings('', FALSE);

        Modified := TRUE;
      end;
    finally
      Release;
    end;
end;

procedure TfrmMain.actShowFileExecute(Sender: TObject);
var
  DelphiFile: TDelphiFile;
begin
  DelphiFile := GetFocusedDelphiFile;

  if DelphiFile <> nil then
    ShellExecute(Handle, 'OPEN', PChar('explorer.exe'), PChar('/select, "' + DelphiFile.UnitInfo.Filename + '"'), nil,
      SW_NORMAL);
end;

// search list "Unit list"
procedure TfrmMain.SearchUnitsList(VT: TVirtualStringTree; const SearchText: String);
var
  Node: PVirtualNode;
  LowerSearchText: String;
  DelphiFile: TDelphiFile;
  InSearchPathOrShowAll,
  UnitMatchesSearchTerm: Boolean;
begin
  VT.BeginUpdate;
  try
    LowerSearchText := LowerCase(SearchText);

    Node := VT.GetFirst;

    while Node <> nil do
    begin
      DelphiFile := FModel.DelphiFileList[GetID(Node)];

      InSearchPathOrShowAll := DelphiFile.InSearchPath or (not DelphiFile.InSearchPath and actShowUnitsNotInPath.Checked);
      UnitMatchesSearchTerm := (LowerSearchText = '')
                               or ((pos(LowerSearchText, LowerCase(DelphiFile.UnitInfo.DelphiUnitName)) <> 0))
                               or ((DelphiFile.UnitInfo.Module <> nil) and (Pos(LowerSearchText, LowerCase(DelphiFile.UnitInfo.Module.Name)) > 0));

      VT.IsVisible[Node] :=  UnitMatchesSearchTerm and InSearchPathOrShowAll;

      Node := Node.NextSibling;
    end;

    VT.Invalidate;
  finally
    VT.EndUpdate;
  end;
end;

procedure TfrmMain.SearchUnitsTree(const SearchText: String; FromFirstNode: Boolean);
var
  Node, EndNode, ParentStepNode: PVirtualNode;
begin
  vtUnitsTree.BeginUpdate;
  try
    FSearchText := LowerCase(SearchText);

    if (vtUnitsTree.FocusedNode = nil) or (FromFirstNode) then
    begin
      Node := vtUnitsTree.GetFirst;

      EndNode := nil;
    end
    else
    begin
      Node := vtUnitsTree.GetNext(vtUnitsTree.FocusedNode);

      if Node = nil then
        Node := vtUnitsTree.GetFirst;

      EndNode := vtUnitsTree.GetPrevious(Node);
    end;

    while Node <> EndNode do
    begin
      if UnitsTreeIsSearchHitNode(Node) then
      begin
        if vtUnitsTree.IsVisible[Node] then
        begin
          vtUnitsTree.SelectNodeEx(Node, TRUE, TRUE);

          if not FShowSearchTermParents then
            Break;
        end;

        if FShowSearchTermParents then
        begin
          ParentStepNode := Node.Parent;

          while ParentStepNode <> vtUnitsTree.RootNode do
          begin
            if FTreeNodeObjects[GetID(ParentStepNode)].SearchTermInChildren then
              Break
            else
              FTreeNodeObjects[GetID(ParentStepNode)].SearchTermInChildren := TRUE;

            ParentStepNode := ParentStepNode.Parent;
          end;
        end;
      end
      else
        FTreeNodeObjects[GetID(Node)].SearchTermInChildren := FALSE;

      Node := vtUnitsTree.GetNext(Node);

      if (Node = nil) and (EndNode <> nil) then
        Node := vtUnitsTree.GetFirst;
    end;

    vtUnitsTree.Invalidate;
  finally
    vtUnitsTree.EndUpdate;
  end;
end;

function TfrmMain.UnitsTreeIsSearchHitNode(Node: PVirtualNode): Boolean;
var
  aUnitInfo: IUnitInfo;
begin
  Result := false;

  if Assigned(Node) then
  begin
    aUnitInfo := FTreeNodeObjects[GetID(Node)].DelphiFile.UnitInfo;
    Result :=    (Pos(FSearchText, LowerCase(aUnitInfo.DelphiUnitName)) > 0)
              or ((aUnitInfo.Module <> nil) and (Pos(FSearchText, LowerCase(aUnitInfo.Module.Name)) > 0));
  end;
end;

procedure TfrmMain.SetNodeVisibility(VT: TVirtualStringTree; Node: PVirtualNode; DelphiFile: TDelphiFile);
var
  aIsUnknownModule: Boolean;
  aVisible: Boolean;
begin
  if (Node <> nil) and (DelphiFile <> nil) then
  begin
    aVisible := true;
    if actShowOnlyUnknownModules.Checked then
    begin
      aIsUnknownModule := (Assigned(DelphiFile.UnitInfo.Module) and DelphiFile.UnitInfo.Module.IsUnknownModule) or FTreeNodeObjects[GetID(Node)].UnknownModuleInChildren;
      aVisible         := aIsUnknownModule;
    end
    else
      if not actShowUnitsNotInPath.Checked then
        aVisible := DelphiFile.InSearchPath;

    VT.IsVisible[Node] := aVisible;
  end;
end;

procedure TfrmMain.SetNodesVisibilityForAllNodes; // TODO: This seems to be doubled logic, compare with Search-Methods for tree and list...
var
  Node: PVirtualNode;
  OldCursor: TCursor;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    // Update the units tree
    vtUnitsTree.BeginUpdate;
    try
      Node := vtUnitsTree.GetFirst;
      while Node <> nil do
      begin
        SetNodeVisibility(vtUnitsTree, Node, FTreeNodeObjects[GetID(Node)].DelphiFile);
        Node := vtUnitsTree.GetNext(Node);
      end;
      if vtUnitsTree.FocusedNode <> nil then
        vtUnitsTree.ScrollIntoView(vtUnitsTree.FocusedNode, TRUE);
//      vtUnitsTree.AutoFitColumns(False, smaUseColumnOption, 2, 10);
    finally
      vtUnitsTree.EndUpdate;
    end;

    // Update the units list
    vtUnitsList.BeginUpdate;
    try
      Node := vtUnitsList.GetFirst;
      while Node <> nil do
      begin
        SetNodeVisibility(vtUnitsList, Node, FModel.DelphiFileList[GetID(Node)]);
        Node := Node.NextSibling;
      end;
      if vtUnitsList.FocusedNode <> nil then
        vtUnitsList.ScrollIntoView(vtUnitsList.FocusedNode, TRUE);
//      vtUnitsList.AutoFitColumns(False, smaUseColumnOption, 1, 6);
    finally
      vtUnitsList.EndUpdate;
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TfrmMain.actShowOnlyUnknownModulesExecute(Sender: TObject);
begin
  // When this filter is activated, the "not in path" must be activated too, otherwise one could
  // not see the "unknown module" units that are "not in path"
  if actShowOnlyUnknownModules.Checked then
    actShowUnitsNotInPath.Checked := true;

  SetNodesVisibilityForAllNodes;
end;

procedure TfrmMain.actShowUnitsNotInPathExecute(Sender: TObject);
begin
  SetNodesVisibilityForAllNodes;
end;

procedure TfrmMain.tmrCloseTimer(Sender: TObject);
begin
  tmrClose.Enabled := FALSE;

  if not FBusy then
  begin
    FClosing := TRUE;

    Close;
  end;
end;

procedure TfrmMain.tmrLoadedTimer(Sender: TObject);
begin
  tmrLoaded.Enabled := FALSE;

  if FLoadLastProject then
    if not LoadProjectSettings(fProjectFilename) then
    begin
      fProjectFilename := '';
      ClearGUIAndModelAndCloseControls;
    end;

  Show;
end;

procedure TfrmMain.FixDPI;

  procedure ScaleVT(const VT: TVirtualStringTree);
  begin
    VT.DefaultNodeHeight := ScaleDimension(VT.DefaultNodeHeight, PixelsPerInch);
    VT.Header.Height    := ScaleDimension(VT.Header.Height, PixelsPerInch);
  end;

begin
  ScaleVT(vtUnitsTree);
  ScaleVT(vtUnitsList);
  ScaleVT(vtUsedByUnits);
  ScaleVT(vtUsesUnits);
  ScaleVT(vtModules);

  RichEditUnitPath.Height := ScaleDimension(RichEditUnitPath.Height, PixelsPerInch);
end;

procedure TfrmMain.edtSearchTreeChange(Sender: TObject);
begin
  SearchUnitsTree(edtSearchTree.Text, TRUE);
end;

procedure TfrmMain.edtSearchTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = 13 then
  begin
    Key := 00;

    if edtSearchTree.Text <> '' then
      SearchUnitsTree(edtSearchTree.Text, FALSE);
  end;
end;

procedure TfrmMain.edtSearchTreeKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    Key := #00;
end;

procedure TfrmMain.UpdateStats;
var
  Index: Integer;

  procedure AddStat(const StatName: String; StatValue: Variant);
  begin
    if Index > FStats.Count - 1 then
      FStats.Add('');

    FStats[Index] := StatName + '=' + VarToStr(StatValue);

    Inc(Index);
  end;

begin
  Index := 0;

  AddStat(StrTime,                          SecondsToTimeString(SecondsBetween(now, FStartTime)));
  AddStat(StrSearchPathFiles,               FormatCardinal(FModel.Files.Count));
  AddStat(StrTotalUsedUnitsAndProjectFiles, FormatCardinal(FModel.ParsedDelphiFiles.Count));
  AddStat(StrNotInSearchPathFiles,          FormatCardinal(fModel.Stats.Units.FilesNotInPath));
  AddStat(StrParsedFiles,                   FormatCardinal(fModel.Stats.Units.ParsedFileCount));
  AddStat(StrScannedUsesCount,              FormatCardinal(fModel.Stats.Units.ScannedUsesCount));
  AddStat(StrDeepestScanDepth,              FormatCardinal(fModel.Stats.Units.DeepestScanDepth));
  AddStat(StrTotalLinesOfCode,              FormatCardinal(fModel.Stats.Units.LinesOfCode));

  AddStat(StrFCircularFiles,                FormatCardinal(fModel.Stats.Units.CircularFiles));
  AddStat(StrSemiCircularFiles,             FormatCardinal(fModel.Stats.Units.SemiCircularFiles));

  AddStat(StrVCLFormCount,                  FormatCardinal(fModel.Stats.Units.VCLFormCount));
  AddStat(StrFMXFormCount,                  FormatCardinal(fModel.Stats.Units.FMXFormCount));

  while Index < FStats.Count do
    FStats.Delete(pred(FStats.Count));

  vtStats.RootNodeCount := FStats.Count;
  vtStats.Invalidate;
end;

procedure TfrmMain.vtModulesGetText(Sender: TBaseVirtualTree; Node:
    PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText:
    string);
var
  aModule: TModule;
begin
  aModule := GetModuleFromModuleNode(Node);

  CellText := '';

  if TextType <> ttStatic then
  begin

    case Column of
      0:
        CellText := aModule.Name;
      1:
        if aModule.Origin <> moUndefined then
          CellText := TModulesSerializer.cModuleOriginJSONEnumValues[aModule.Origin];
      2:
        if aModule.Usage <> muUndefined then
          CellText := TModulesSerializer.cModuleUsageJSONEnumValues[aModule.Usage];

      3:
        if aModule.AnalysisData.NumberOfFiles > 0 then
          CellText := FormatCardinal(aModule.AnalysisData.NumberOfFiles);

      4:
        if aModule.AnalysisData.NumberOfFilesNotInPath > 0 then
          CellText := FormatCardinal(aModule.AnalysisData.NumberOfFilesNotInPath);

      5:
        if aModule.AnalysisData.LinesOfCode > 0 then
          CellText := FormatCardinal(aModule.AnalysisData.LinesOfCode);
    end;
  end;
end;

function TfrmMain.GetModuleFromModuleNode(Node: PVirtualNode): TModule;
begin
  Result := fModel.Modules.OrderedModules[GetID(Node) - 1]; // ID beginnt mit 1, ObjectList mit 0
end;

procedure TfrmMain.vtModulesDrawText(Sender: TBaseVirtualTree; TargetCanvas:
    TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string;
    const CellRect: TRect; var DefaultDraw: Boolean);
var
  aModule: TModule;
begin
  aModule := GetModuleFromModuleNode(Node);

  // highlight "unknown modules"
  if Column = 0 then // Column = 'name'
    if not Sender.Selected[Node] then
    begin
      if aModule.IsUnknownModule then
      begin
        TargetCanvas.Brush.Color := cUnknownModuleBackgroundColor;
        TargetCanvas.Font.Color  := cUnknownModuleForegroundColor;
      end;
    end;
end;

end.
